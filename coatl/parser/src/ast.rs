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

    Coalesce,
    Pipe,
}

#[derive(Debug, Copy, Clone)]
pub enum UnaryOp {
    Inv,
    Pos,
    Neg,
    Yield,
    YieldFrom,
}

pub type Ident<'a> = &'a str;
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
}

#[derive(Debug, Clone)]
pub struct ExceptHandler<'a> {
    pub typ: Option<SExpr<'a>>,
    pub name: Option<SIdent<'a>>,
    pub body: SBlock<'a>,
}

// TODO should these be cows
#[derive(Debug, Clone)]
pub enum Stmt<'a> {
    Global(Vec<SIdent<'a>>),
    Nonlocal(Vec<SIdent<'a>>),
    Assign(SExpr<'a>, SExpr<'a>),
    Return(SExpr<'a>),
    Expr(SExpr<'a>),
    While(SExpr<'a>, SBlock<'a>),
    For(SExpr<'a>, SExpr<'a>, SBlock<'a>),
    Import(ImportStmt<'a>),
    Try(SBlock<'a>, Vec<ExceptHandler<'a>>, Option<SBlock<'a>>),
    Assert(SExpr<'a>, Option<SExpr<'a>>),
    Raise(SExpr<'a>),
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
    pub block: SBlock<'a>,
    pub fmt: Option<&'a str>,
}

pub type SFmtExpr<'a> = Spanned<FmtExpr<'a>>;

#[derive(Debug, Clone)]
pub enum ListItem<'a> {
    Item(SExpr<'a>),
    Spread(SExpr<'a>),
}

#[derive(Debug, Clone)]
pub enum MappingItem<'a> {
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
pub enum ArgItem<'a> {
    Arg(SIdent<'a>),
    DefaultArg(SIdent<'a>, SExpr<'a>),
    ArgSpread(SIdent<'a>),
    KwargSpread(SIdent<'a>),
}

pub type SArgItem<'a> = Spanned<ArgItem<'a>>;

#[derive(Debug, Clone)]
pub enum Block<'a> {
    Stmts(Vec<SStmt<'a>>),
    Expr(SExpr<'a>),
}

pub type SBlock<'a> = Spanned<Block<'a>>;

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Literal(SLiteral<'a>),
    Ident(SIdent<'a>),
    Unary(UnaryOp, Box<SExpr<'a>>),
    Binary(BinaryOp, Box<SExpr<'a>>, Box<SExpr<'a>>),

    List(Vec<ListItem<'a>>),
    Mapping(Vec<MappingItem<'a>>),
    Slice(
        Option<Box<SExpr<'a>>>,
        Option<Box<SExpr<'a>>>,
        Option<Box<SExpr<'a>>>,
    ),

    If(Box<SExpr<'a>>, Box<SBlock<'a>>, Option<Box<SBlock<'a>>>),
    Match(Box<SExpr<'a>>, Vec<(SExpr<'a>, Box<SBlock<'a>>)>),
    Class(Vec<SCallItem<'a>>, Box<SBlock<'a>>),

    Call(Box<SExpr<'a>>, Vec<SCallItem<'a>>),
    Subscript(Box<SExpr<'a>>, Vec<ListItem<'a>>),
    Attribute(Box<SExpr<'a>>, SIdent<'a>),
    Then(Box<SExpr<'a>>, Box<SExpr<'a>>),

    Fn(Vec<ArgItem<'a>>, Box<SBlock<'a>>),
    Fstr(Spanned<String>, Vec<(SFmtExpr<'a>, Spanned<String>)>),

    Block(Box<SBlock<'a>>),
}

pub type SExpr<'a> = Spanned<Expr<'a>>;
