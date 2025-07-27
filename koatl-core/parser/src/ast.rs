use std::borrow::Cow;

use chumsky::span::SimpleSpan;

pub type Span = SimpleSpan<usize, ()>;
pub type Spanned<T> = (T, Span);

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

pub type Ident<'a> = Cow<'a, str>;
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
pub enum AssignModifier {
    Export,
    Global,
    Nonlocal,
}

// TODO should these be cows
#[derive(Debug, Clone)]
pub enum Stmt<'a> {
    Module,
    Assign(SExpr<'a>, SExpr<'a>, Vec<AssignModifier>),
    Expr(SExpr<'a>),

    Return(SExpr<'a>),
    While(SExpr<'a>, SExpr<'a>),
    For(SPattern<'a>, SExpr<'a>, SExpr<'a>),
    Import(ImportStmt<'a>),
    Try(SExpr<'a>, Vec<MatchCase<'a>>, Option<SExpr<'a>>),
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
    pub block: SExpr<'a>,
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
    Arg(SPattern<'a>, Option<SExpr<'a>>),
    ArgSpread(SIdent<'a>),
    KwargSpread(SIdent<'a>),
}

pub type SArgItem<'a> = Spanned<ArgDefItem<'a>>;

#[derive(Debug, Clone)]
pub struct MatchCase<'src> {
    pub pattern: Option<SPattern<'src>>,
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
        Option<Box<SExpr<'a>>>,
        Option<Box<SExpr<'a>>>,
        Option<Box<SExpr<'a>>>,
    ),

    Unary(UnaryOp, Box<SExpr<'a>>),
    Binary(BinaryOp, Box<SExpr<'a>>, Box<SExpr<'a>>),

    Await(Box<SExpr<'a>>),
    Yield(Box<SExpr<'a>>),
    YieldFrom(Box<SExpr<'a>>),

    If(Box<SExpr<'a>>, Box<SExpr<'a>>, Option<Box<SExpr<'a>>>),
    Match(Box<SExpr<'a>>, Vec<MatchCase<'a>>),
    Matches(Box<SExpr<'a>>, Box<SPattern<'a>>),
    Class(Vec<SCallItem<'a>>, Box<SExpr<'a>>),

    Call(Box<SExpr<'a>>, Vec<SCallItem<'a>>),
    Subscript(Box<SExpr<'a>>, Vec<ListItem<'a>>),
    RawAttribute(Box<SExpr<'a>>, SIdent<'a>),
    ScopedAttribute(Box<SExpr<'a>>, Box<SExpr<'a>>),
    Attribute(Box<SExpr<'a>>, SIdent<'a>),

    MappedCall(Box<SExpr<'a>>, Vec<SCallItem<'a>>),
    MappedSubscript(Box<SExpr<'a>>, Vec<ListItem<'a>>),
    MappedRawAttribute(Box<SExpr<'a>>, SIdent<'a>),
    MappedScopedAttribute(Box<SExpr<'a>>, Box<SExpr<'a>>),
    MappedAttribute(Box<SExpr<'a>>, SIdent<'a>),

    Checked(Box<SExpr<'a>>, Option<Box<SPattern<'a>>>),

    Fn(Vec<ArgDefItem<'a>>, Box<SExpr<'a>>),
    Fstr(Spanned<String>, Vec<(SFmtExpr<'a>, Spanned<String>)>),

    Decorated(Box<SExpr<'a>>, Box<SExpr<'a>>),

    Block(Vec<SStmt<'a>>),
}

pub type SExpr<'a> = Spanned<Expr<'a>>;

#[derive(Debug, Clone)]
pub enum PatternSequenceItem<'a> {
    Item(SPattern<'a>),
    Spread(Option<SIdent<'a>>),
}

#[derive(Debug, Clone)]
pub enum PatternMappingItem<'a> {
    Ident(SIdent<'a>),
    Item(SExpr<'a>, SPattern<'a>),
    Spread(Option<SIdent<'a>>),
}

#[derive(Debug, Clone)]
pub enum PatternClassItem<'a> {
    Item(SPattern<'a>),
    Kw(SIdent<'a>, SPattern<'a>),
}

#[derive(Debug, Clone)]
pub enum Pattern<'a> {
    Capture(Option<SIdent<'a>>),
    Value(SExpr<'a>),
    As(Box<SPattern<'a>>, SIdent<'a>),
    Or(Vec<SPattern<'a>>),
    Literal(SLiteral<'a>),
    Sequence(Vec<PatternSequenceItem<'a>>),
    Mapping(Vec<PatternMappingItem<'a>>),
    Class(SExpr<'a>, Vec<PatternClassItem<'a>>),
}

pub type SPattern<'a> = Spanned<Pattern<'a>>;
