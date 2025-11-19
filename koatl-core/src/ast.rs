use std::{borrow::Cow, fmt::Display};

pub use koatl_parser::cst::{BinaryOp, Spannable, Spanned, UnaryOp};
pub use koatl_parser::lexer::Span;

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
    Float(Cow<'a, str>),
    Int(Cow<'a, str>),
    IntHex(Cow<'a, str>),
    IntOct(Cow<'a, str>),
    IntBin(Cow<'a, str>),

    Str(Cow<'a, str>),
    Bool(bool),
    None,
}

pub type SLiteral<'a> = Spanned<Literal<'a>>;

#[derive(Debug, Clone)]
pub enum ImportLeaf<'a> {
    Multi(Vec<ImportTree<'a>>),
    Single(SIdent<'a>, Option<SIdent<'a>>),
    This(Option<SIdent<'a>>),
    Star,
}

#[derive(Debug, Clone)]
pub struct ImportTree<'a> {
    pub trunk: Vec<SIdent<'a>>,
    pub leaf: Spanned<ImportLeaf<'a>>,

    // number of dots prepending the trunk
    pub level: usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DeclType {
    Export,
    Global,
    Let,
    Const,
}

#[derive(Debug, Clone)]
pub enum Stmt<'a, TTree: Tree> {
    Decl(Vec<SIdent<'a>>, DeclType),
    Assign(TTree::Expr, TTree::Expr, Option<BinaryOp>),
    PatternAssign(TTree::Pattern, TTree::Expr, Option<DeclType>),

    Expr(TTree::Expr),

    Return(TTree::Expr),
    While(TTree::Expr, TTree::Expr),
    For(TTree::Pattern, TTree::Expr, TTree::Expr),
    Import(ImportTree<'a>, bool),
    Raise(Option<TTree::Expr>),

    Break,
    Continue,
}

#[derive(Debug, Clone)]
pub struct FmtExpr<TTree: Tree> {
    pub expr: TTree::Expr,
    pub fmt: Option<TTree::Expr>,
}

#[derive(Debug, Clone)]
pub enum ListItem<TTree: Tree> {
    Item(TTree::Expr),
    Spread(TTree::Expr),
}

#[derive(Debug, Clone)]
pub enum MappingItem<TTree: Tree> {
    Ident(TTree::Expr),
    Item(TTree::Expr, TTree::Expr),
    Spread(TTree::Expr),
}

#[derive(Debug, Clone)]
pub enum CallItem<'a, TTree: Tree> {
    Arg(TTree::Expr),
    Kwarg(SIdent<'a>, TTree::Expr),
    ArgSpread(TTree::Expr),
    KwargSpread(TTree::Expr),
}

#[derive(Debug, Clone)]
pub enum ArgDefItem<'a, TTree: Tree> {
    Arg(TTree::Pattern, Option<TTree::Expr>),
    ArgSpread(SIdent<'a>),
    KwargSpread(SIdent<'a>),
    PosOnlyMarker,
    KwOnlyMarker,
}

#[derive(Debug, Clone)]
pub struct MatchCase<TTree: Tree> {
    pub pattern: TTree::Pattern,
    pub guard: Option<TTree::Expr>,
    pub body: TTree::Expr,
}

pub trait Tree {
    type Expr;
    type Pattern;
    type Stmt;
}

#[derive(Debug, Clone)]
pub enum Expr<'a, TTree: Tree> {
    Literal(SLiteral<'a>),
    Ident(SIdent<'a>),
    Tuple(Vec<ListItem<TTree>>),
    List(Vec<ListItem<TTree>>),
    Mapping(Vec<MappingItem<TTree>>),
    Slice(
        Option<TTree::Expr>,
        Option<TTree::Expr>,
        Option<TTree::Expr>,
    ),

    Unary(UnaryOp, TTree::Expr),
    Binary(BinaryOp, TTree::Expr, TTree::Expr),

    Await(TTree::Expr),
    Yield(TTree::Expr),
    YieldFrom(TTree::Expr),
    Memo(TTree::Expr, bool),

    If(TTree::Expr, TTree::Expr, Option<TTree::Expr>),
    Match(TTree::Expr, Vec<MatchCase<TTree>>),
    Matches(TTree::Expr, TTree::Pattern),
    Class(Vec<CallItem<'a, TTree>>, TTree::Expr),

    With(TTree::Pattern, TTree::Expr, TTree::Expr),

    Call(TTree::Expr, Vec<CallItem<'a, TTree>>),
    Subscript(TTree::Expr, Vec<ListItem<TTree>>),
    RawAttribute(TTree::Expr, SIdent<'a>),
    ScopedAttribute(TTree::Expr, TTree::Expr),
    Attribute(TTree::Expr, SIdent<'a>),
    MaybeAttribute(TTree::Expr, SIdent<'a>),

    MappedCall(TTree::Expr, Vec<CallItem<'a, TTree>>),
    MappedSubscript(TTree::Expr, Vec<ListItem<TTree>>),
    MappedRawAttribute(TTree::Expr, SIdent<'a>),
    MappedScopedAttribute(TTree::Expr, TTree::Expr),
    MappedAttribute(TTree::Expr, SIdent<'a>),
    MappedMaybeAttribute(TTree::Expr, SIdent<'a>),

    Try(TTree::Expr, Vec<MatchCase<TTree>>, Option<TTree::Expr>),
    Checked(TTree::Expr, Option<TTree::Pattern>),

    Block(Vec<TTree::Stmt>),

    Fn(Vec<ArgDefItem<'a, TTree>>, TTree::Expr),
    Fstr(Spanned<String>, Vec<(FmtExpr<TTree>, Spanned<String>)>),

    // these are removed during desugaring
    Decorated(TTree::Expr, TTree::Expr),
    Placeholder,
}

// Patterns

#[derive(Debug, Clone)]
pub enum PatternSequenceItem<'a, TTree: Tree> {
    Item(TTree::Pattern),
    Spread(Option<SIdent<'a>>),
}

#[derive(Debug, Clone)]
pub enum PatternMappingItem<'a, TTree: Tree> {
    Ident(SIdent<'a>),
    Item(TTree::Expr, TTree::Pattern),
    Spread(Option<SIdent<'a>>),
}

#[derive(Debug, Clone)]
pub enum PatternClassItem<'a, TTree: Tree> {
    Item(TTree::Pattern),
    Kw(SIdent<'a>, TTree::Pattern),
}

#[derive(Debug, Clone)]
pub enum Pattern<'a, TTree: Tree> {
    Capture(Option<SIdent<'a>>),
    Value(TTree::Expr),
    As(TTree::Pattern, Option<SIdent<'a>>),
    Or(Vec<TTree::Pattern>),
    Literal(SLiteral<'a>),
    Sequence(Vec<PatternSequenceItem<'a, TTree>>),
    Mapping(Vec<PatternMappingItem<'a, TTree>>),
    Class(TTree::Expr, Vec<PatternClassItem<'a, TTree>>),
}

// Spanned types

#[derive(Debug, Clone)]
pub struct STree<'src> {
    phantom: std::marker::PhantomData<&'src ()>,
}

impl<'src> Tree for STree<'src> {
    type Expr = Indirect<SExpr<'src>>;
    type Pattern = Indirect<SPattern<'src>>;
    type Stmt = Indirect<SStmt<'src>>;
}

pub type SPatternInner<'a> = Pattern<'a, STree<'a>>;

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

pub type SExprInner<'a> = Expr<'a, STree<'a>>;

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

pub type SStmtInner<'a> = Stmt<'a, STree<'a>>;

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

pub type SListItem<'a> = ListItem<STree<'a>>;
pub type SMappingItem<'a> = MappingItem<STree<'a>>;
pub type SMatchCase<'a> = MatchCase<STree<'a>>;
pub type SCallItem<'a> = CallItem<'a, STree<'a>>;
pub type SArgDefItem<'a> = ArgDefItem<'a, STree<'a>>;
pub type SFmtExpr<'a> = FmtExpr<STree<'a>>;
