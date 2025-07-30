use std::{borrow::Cow, rc::Rc};

use chumsky::span::SimpleSpan;

pub type Span = SimpleSpan<usize, ()>;
pub type Spanned<T> = (T, Span);
pub type Indirect<T> = Rc<T>;

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

impl<'a> From<Cow<'a, str>> for Ident<'a> {
    fn from(value: Cow<'a, str>) -> Self {
        Ident(value)
    }
}

pub type SIdent<'a> = Spanned<Ident<'a>>;

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

// TODO should these be cows
#[derive(Debug, Clone)]
pub enum Stmt<'a> {
    Module,
    Decl(Vec<SIdent<'a>>, DeclType),
    Assign(Indirect<SExpr<'a>>, Indirect<SExpr<'a>>, Option<DeclType>),
    Expr(SExpr<'a>),

    Return(SExpr<'a>),
    While(SExpr<'a>, SExpr<'a>),
    For(SPattern<'a>, SExpr<'a>, SExpr<'a>),
    Import(ImportStmt<'a>),
    Try(SExpr<'a>, Vec<Indirect<MatchCase<'a>>>, Option<SExpr<'a>>),
    Assert(SExpr<'a>, Option<SExpr<'a>>),
    Raise(Option<SExpr<'a>>),
    Break,
    Continue,
    Err,
}

pub type SStmt<'a> = Spanned<Stmt<'a>>;

#[derive(Debug, Clone)]
pub enum Literal<'a> {
    Num(Cow<'a, str>),
    Str(Cow<'a, str>),
    Bool(bool),
    None,
}

pub type SLiteral<'a> = Spanned<Literal<'a>>;

#[derive(Debug, Clone)]
pub struct FmtExpr<'a> {
    pub expr: SExpr<'a>,
    pub fmt: Option<Ident<'a>>,
}

pub type SFmtExpr<'a> = Spanned<FmtExpr<'a>>;

#[derive(Debug, Clone)]
pub enum ListItem<'a> {
    Item(SExpr<'a>),
    Spread(SExpr<'a>),
}

#[derive(Debug, Clone)]
pub enum MappingItem<'a> {
    Ident(SIdent<'a>),
    Item(SExpr<'a>, SExpr<'a>),
    Spread(SExpr<'a>),
}

#[derive(Debug, Clone)]
pub enum CallItem<'a> {
    Arg(SExpr<'a>),
    Kwarg(SIdent<'a>, SExpr<'a>),
    ArgSpread(SExpr<'a>),
    KwargSpread(SExpr<'a>),
}

pub type SCallItem<'a> = Spanned<CallItem<'a>>;

#[derive(Debug, Clone)]
pub enum ArgDefItem<'a> {
    Arg(ISPattern<'a>, Option<SExpr<'a>>),
    ArgSpread(SIdent<'a>),
    KwargSpread(SIdent<'a>),
}

pub type SArgItem<'a> = Spanned<ArgDefItem<'a>>;

#[derive(Debug, Clone)]
pub struct MatchCase<'src> {
    pub pattern: Option<Indirect<SPattern<'src>>>,
    pub guard: Option<SExpr<'src>>,
    pub body: SExpr<'src>,
}

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Literal(SLiteral<'a>),
    Ident(SIdent<'a>),
    Placeholder,
    Tuple(Vec<ListItem<'a>>),
    List(Vec<ListItem<'a>>),
    Mapping(Vec<MappingItem<'a>>),
    Slice(
        Option<Indirect<SExpr<'a>>>,
        Option<Indirect<SExpr<'a>>>,
        Option<Indirect<SExpr<'a>>>,
    ),

    Unary(UnaryOp, Indirect<SExpr<'a>>),
    Binary(BinaryOp, Indirect<SExpr<'a>>, Indirect<SExpr<'a>>),

    Await(Indirect<SExpr<'a>>),
    Yield(Indirect<SExpr<'a>>),
    YieldFrom(Indirect<SExpr<'a>>),

    If(
        Indirect<SExpr<'a>>,
        Indirect<SExpr<'a>>,
        Option<Indirect<SExpr<'a>>>,
    ),
    Match(Indirect<SExpr<'a>>, Vec<Indirect<MatchCase<'a>>>),
    Matches(Indirect<SExpr<'a>>, ISPattern<'a>),
    Class(Vec<SCallItem<'a>>, Indirect<SExpr<'a>>),

    Call(Indirect<SExpr<'a>>, Vec<SCallItem<'a>>),
    Subscript(Indirect<SExpr<'a>>, Vec<ListItem<'a>>),
    RawAttribute(Indirect<SExpr<'a>>, SIdent<'a>),
    ScopedAttribute(Indirect<SExpr<'a>>, Indirect<SExpr<'a>>),
    Attribute(Indirect<SExpr<'a>>, SIdent<'a>),

    MappedCall(Indirect<SExpr<'a>>, Vec<SCallItem<'a>>),
    MappedSubscript(Indirect<SExpr<'a>>, Vec<ListItem<'a>>),
    MappedRawAttribute(Indirect<SExpr<'a>>, SIdent<'a>),
    MappedScopedAttribute(Indirect<SExpr<'a>>, Indirect<SExpr<'a>>),
    MappedAttribute(Indirect<SExpr<'a>>, SIdent<'a>),

    Checked(Indirect<SExpr<'a>>, Option<ISPattern<'a>>),

    Fn(Vec<ArgDefItem<'a>>, Indirect<SExpr<'a>>),
    Fstr(Spanned<String>, Vec<(SFmtExpr<'a>, Spanned<String>)>),

    Decorated(Indirect<SExpr<'a>>, Indirect<SExpr<'a>>),

    Block(Vec<SStmt<'a>>),
}

pub type SExpr<'a> = Spanned<Expr<'a>>;

#[derive(Debug, Clone)]
pub enum PatternSequenceItem<'a> {
    Item(ISPattern<'a>),
    Spread(Option<SIdent<'a>>),
}

#[derive(Debug, Clone)]
pub enum PatternMappingItem<'a> {
    Ident(SIdent<'a>),
    Item(SExpr<'a>, ISPattern<'a>),
    Spread(Option<SIdent<'a>>),
}

#[derive(Debug, Clone)]
pub enum PatternClassItem<'a> {
    Item(ISPattern<'a>),
    Kw(SIdent<'a>, ISPattern<'a>),
}

#[derive(Debug, Clone)]
pub enum Pattern<'a> {
    Capture(Option<SIdent<'a>>),
    Value(SExpr<'a>),
    As(ISPattern<'a>, SIdent<'a>),
    Or(Vec<ISPattern<'a>>),
    Literal(SLiteral<'a>),
    Sequence(Vec<PatternSequenceItem<'a>>),
    Mapping(Vec<PatternMappingItem<'a>>),
    Class(SExpr<'a>, Vec<PatternClassItem<'a>>),
}

pub type SPattern<'a> = Spanned<Pattern<'a>>;
pub type ISPattern<'a> = Indirect<SPattern<'a>>;
