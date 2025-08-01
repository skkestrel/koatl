use std::{borrow::Cow, fmt::Display};

use chumsky::span::SimpleSpan;

pub type Indirect<T> = Box<T>;

pub trait FromIndirect<T> {
    fn extract(self) -> T;
}

impl<T: Clone> FromIndirect<T> for Indirect<T> {
    fn extract(self) -> T {
        *self
    }
}

pub trait IntoIndirect<T> {
    fn indirect(self) -> Indirect<T>;
}

impl<T> IntoIndirect<T> for Indirect<T> {
    fn indirect(self) -> Indirect<T> {
        self
    }
}

impl<T> IntoIndirect<T> for T {
    fn indirect(self) -> Indirect<T> {
        Box::new(self)
    }
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Mod,
    MatMul,
    Div,
    Exp,

    Lt,
    Leq,
    Gt,
    Geq,
    Eq,
    Neq,
    Is,
    Nis,

    And,
    Or,

    Coalesce,
    Pipe,
}

#[derive(Debug, Copy, Clone)]
pub enum UnaryOp {
    Inv,
    Pos,
    Neg,
    Not,
    Bind,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Ident<'a>(pub Cow<'a, str>);

impl<'a> Display for Ident<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<'a> From<Cow<'a, str>> for Ident<'a> {
    fn from(value: Cow<'a, str>) -> Self {
        Ident(value)
    }
}

pub type SIdent<'a> = Spanned<Ident<'a>>;

#[derive(Debug, Clone)]
pub enum Literal<'a> {
    Num(Cow<'a, str>),
    Str(Cow<'a, str>),
    Bool(bool),
    None,
}

pub type SLiteral<'a> = Spanned<Literal<'a>>;

#[derive(Debug, Clone)]
pub enum ImportList<'a> {
    // ident, alias
    Leaves(Vec<(SIdent<'a>, Option<SIdent<'a>>)>),
    Star,
}

#[derive(Debug, Clone)]
pub struct ImportStmt<'a> {
    pub trunk: Vec<SIdent<'a>>,
    pub imports: ImportList<'a>,

    // number of dots prepending the trunk
    pub level: usize,

    pub reexport: bool,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DeclType {
    Export,
    Global,
    Let,
    Const,
}

#[derive(Debug, Clone)]
pub enum Stmt<'a, TNode, TPattern> {
    Module,
    Decl(Vec<SIdent<'a>>, DeclType),
    Assign(TNode, TNode, Option<DeclType>),
    Expr(TNode),

    Return(TNode),
    While(TNode, TNode),
    For(TPattern, TNode, TNode),
    Import(ImportStmt<'a>),
    Try(TNode, Vec<MatchCase<TNode, TPattern>>, Option<TNode>),
    Assert(TNode, Option<TNode>),
    Raise(Option<TNode>),

    Break,
    Continue,
}

#[derive(Debug, Clone)]
pub struct FmtExpr<'a, TExpr> {
    pub expr: TExpr,
    pub fmt: Option<Ident<'a>>,
}

#[derive(Debug, Clone)]
pub enum ListItem<TExpr> {
    Item(TExpr),
    Spread(TExpr),
}

#[derive(Debug, Clone)]
pub enum MappingItem<'a, TExpr> {
    Ident(SIdent<'a>),
    Item(TExpr, TExpr),
    Spread(TExpr),
}

#[derive(Debug, Clone)]
pub enum CallItem<'a, TExpr> {
    Arg(TExpr),
    Kwarg(SIdent<'a>, TExpr),
    ArgSpread(TExpr),
    KwargSpread(TExpr),
}

#[derive(Debug, Clone)]
pub enum ArgDefItem<'a, TExpr, TPattern> {
    Arg(TPattern, Option<TExpr>),
    ArgSpread(SIdent<'a>),
    KwargSpread(SIdent<'a>),
}

#[derive(Debug, Clone)]
pub struct MatchCase<TExpr, TPattern> {
    pub pattern: Option<TPattern>,
    pub guard: Option<TExpr>,
    pub body: TExpr,
}

#[derive(Debug, Clone)]
pub enum Expr<'a, TExpr, TPattern> {
    Literal(SLiteral<'a>),
    Ident(SIdent<'a>),
    Placeholder,
    Tuple(Vec<ListItem<TExpr>>),
    List(Vec<ListItem<TExpr>>),
    Mapping(Vec<MappingItem<'a, TExpr>>),
    Slice(Option<TExpr>, Option<TExpr>, Option<TExpr>),

    Unary(UnaryOp, TExpr),
    Binary(BinaryOp, TExpr, TExpr),

    Await(TExpr),
    Yield(TExpr),
    YieldFrom(TExpr),

    If(TExpr, TExpr, Option<TExpr>),
    Match(TExpr, Vec<MatchCase<TExpr, TPattern>>),
    Matches(TExpr, TPattern),
    Class(Vec<CallItem<'a, TExpr>>, TExpr),

    Call(TExpr, Vec<CallItem<'a, TExpr>>),
    Subscript(TExpr, Vec<ListItem<TExpr>>),
    RawAttribute(TExpr, SIdent<'a>),
    ScopedAttribute(TExpr, TExpr),
    Attribute(TExpr, SIdent<'a>),

    MappedCall(TExpr, Vec<CallItem<'a, TExpr>>),
    MappedSubscript(TExpr, Vec<ListItem<TExpr>>),
    MappedRawAttribute(TExpr, SIdent<'a>),
    MappedScopedAttribute(TExpr, TExpr),
    MappedAttribute(TExpr, SIdent<'a>),

    Checked(TExpr, Option<TPattern>),

    Fn(Vec<ArgDefItem<'a, TExpr, TPattern>>, TExpr),
    Fstr(Spanned<String>, Vec<(FmtExpr<'a, TExpr>, Spanned<String>)>),

    Decorated(TExpr, TExpr),

    Block(Vec<SStmt<'a>>),
}

// Patterns

#[derive(Debug, Clone)]
pub enum PatternSequenceItem<'a, TPattern> {
    Item(TPattern),
    Spread(Option<SIdent<'a>>),
}

#[derive(Debug, Clone)]
pub enum PatternMappingItem<'a, TExpr, TPattern> {
    Ident(SIdent<'a>),
    Item(TExpr, TPattern),
    Spread(Option<SIdent<'a>>),
}

#[derive(Debug, Clone)]
pub enum PatternClassItem<'a, TPattern> {
    Item(TPattern),
    Kw(SIdent<'a>, TPattern),
}

#[derive(Debug, Clone)]
pub enum Pattern<'a, TExpr, TPattern> {
    Capture(Option<SIdent<'a>>),
    Value(TExpr),
    As(TPattern, SIdent<'a>),
    Or(Vec<TPattern>),
    Literal(SLiteral<'a>),
    Sequence(Vec<PatternSequenceItem<'a, TPattern>>),
    Mapping(Vec<PatternMappingItem<'a, TExpr, TPattern>>),
    Class(TExpr, Vec<PatternClassItem<'a, TPattern>>),
}

// Spanned types

pub type Span = SimpleSpan<usize, ()>;

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub span: Span,
    pub value: T,
}

pub trait Spannable<T> {
    fn spanned(self, span: Span) -> Spanned<T>;
}

impl<T> Spannable<T> for T {
    fn spanned(self, span: Span) -> Spanned<T> {
        Spanned { span, value: self }
    }
}

pub type SPatternInner<'a> = Pattern<'a, Indirect<SExpr<'a>>, Indirect<SPattern<'a>>>;

#[derive(Debug, Clone)]
pub struct SPattern<'a> {
    pub value: SPatternInner<'a>,
    pub span: Span,
}

impl<'a> SPatternInner<'a> {
    pub fn spanned(self, span: Span) -> SPattern<'a> {
        SPattern { value: self, span }
    }
}

pub type SExprInner<'a> = Expr<'a, Indirect<SExpr<'a>>, Indirect<SPattern<'a>>>;

#[derive(Debug, Clone)]
pub struct SExpr<'a> {
    pub value: SExprInner<'a>,
    pub span: Span,
}

impl<'a> SExprInner<'a> {
    pub fn spanned(self, span: Span) -> SExpr<'a> {
        SExpr { value: self, span }
    }
}

pub type SStmtInner<'a> = Stmt<'a, Indirect<SExpr<'a>>, Indirect<SPattern<'a>>>;

impl<'a> SStmtInner<'a> {
    pub fn spanned(self, span: Span) -> SStmt<'a> {
        SStmt { value: self, span }
    }
}

#[derive(Debug, Clone)]
pub struct SStmt<'a> {
    pub value: SStmtInner<'a>,
    pub span: Span,
}

pub type SListItem<'a> = ListItem<Indirect<SExpr<'a>>>;
pub type SMappingItem<'a> = MappingItem<'a, Indirect<SExpr<'a>>>;
pub type SMatchCase<'a> = MatchCase<Indirect<SExpr<'a>>, Indirect<SPattern<'a>>>;
pub type SCallItem<'a> = CallItem<'a, Indirect<SExpr<'a>>>;
pub type SArgDefItem<'a> = ArgDefItem<'a, Indirect<SExpr<'a>>, Indirect<SPattern<'a>>>;
pub type SFmtExpr<'a> = FmtExpr<'a, Indirect<SExpr<'a>>>;
