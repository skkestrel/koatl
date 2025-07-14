use std::borrow::Cow;

use parser::ast::Span;

pub type PyIdent<'a> = Cow<'a, str>;

#[derive(Debug, Clone)]
pub struct PyImportAlias<'a> {
    pub name: PyIdent<'a>,
    pub as_name: Option<PyIdent<'a>>,
}

#[derive(Debug, Clone)]
pub struct PyExceptHandler<'a> {
    pub typ: Option<SPyExpr<'a>>,
    pub name: Option<PyIdent<'a>>,
    pub body: Vec<SPyStmt<'a>>,
}

#[derive(Debug, Clone)]
pub struct PyMatchCase<'a> {
    pub pattern: SPyExpr<'a>,
    pub body: Vec<SPyStmt<'a>>,
}

#[derive(Debug, Clone)]
pub enum PyStmt<'a> {
    Expr(SPyExpr<'a>),
    If(SPyExpr<'a>, SPyStmts<'a>, Option<SPyStmts<'a>>),
    Match(SPyExpr<'a>, Vec<PyMatchCase<'a>>),
    Assign(SPyExpr<'a>, SPyExpr<'a>),
    Return(SPyExpr<'a>),
    Raise(SPyExpr<'a>),
    Assert(SPyExpr<'a>, Option<SPyExpr<'a>>),
    Global(Vec<PyIdent<'a>>),
    Nonlocal(Vec<PyIdent<'a>>),
    Import(PyImportAlias<'a>),
    ImportFrom(PyIdent<'a>, Vec<PyImportAlias<'a>>),
    FnDef(PyIdent<'a>, Vec<PyArgDefItem<'a>>, SPyStmts<'a>),
    ClassDef(PyIdent<'a>, Vec<PyCallItem<'a>>, SPyStmts<'a>),
    While(SPyExpr<'a>, SPyStmts<'a>),
    For(PyIdent<'a>, SPyExpr<'a>, SPyStmts<'a>),
    Try(SPyStmts<'a>, Vec<PyExceptHandler<'a>>, Option<SPyStmts<'a>>),
    Break,
    Continue,
}

pub type SPyStmt<'a> = PySpanned<PyStmt<'a>>;
pub type SPyStmts<'a> = Vec<SPyStmt<'a>>;

#[derive(Debug, Clone)]
pub enum PyBinaryOp {
    Add,
    Sub,
    Mult,
    Div,
    Mod,
    Pow,

    And,
    Or,

    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,
    Is,
    Nis,
}

#[derive(Debug, Clone)]
pub enum PyUnaryOp {
    Not,
    Neg,
    Pos,
    Inv,
}

#[derive(Debug, Clone)]
pub enum PyNameCtx {
    Load,
    Store,
}

#[derive(Debug, Clone)]
pub enum PyCallItem<'a> {
    Arg(SPyExpr<'a>),
    Kwarg(PyIdent<'a>, SPyExpr<'a>),
    ArgSpread(SPyExpr<'a>),
    KwargSpread(SPyExpr<'a>),
}

#[derive(Debug, Clone)]
pub enum PyArgDefItem<'a> {
    Arg(PyIdent<'a>, Option<SPyExpr<'a>>),
    ArgSpread(PyIdent<'a>),
    KwargSpread(PyIdent<'a>),
}

#[derive(Debug, Clone)]
pub enum PyTupleItem<'a> {
    Item(SPyExpr<'a>),
    Spread(SPyExpr<'a>),
}

#[derive(Debug, Clone)]
pub enum PyDictItem<'a> {
    Item(SPyExpr<'a>, SPyExpr<'a>),
    Spread(SPyExpr<'a>),
}

#[derive(Debug, Clone)]
pub enum PyFstrPart<'a> {
    Str(PyIdent<'a>),
    Expr(SPyExpr<'a>, PyIdent<'a>),
}

#[derive(Debug, Clone)]
pub enum PyExpr<'a> {
    Literal(PyLiteral<'a>),
    Fstr(Vec<PyFstrPart<'a>>),
    Name(PyIdent<'a>),

    Binary(PyBinaryOp, Box<SPyExpr<'a>>, Box<SPyExpr<'a>>),
    Unary(PyUnaryOp, Box<SPyExpr<'a>>),
    Call(Box<SPyExpr<'a>>, Vec<PyCallItem<'a>>),
    Attribute(Box<SPyExpr<'a>>, PyIdent<'a>),
    Subscript(Box<SPyExpr<'a>>, Box<SPyExpr<'a>>),

    Tuple(Vec<PyTupleItem<'a>>),
    Dict(Vec<PyDictItem<'a>>),
    Slice(
        Option<Box<SPyExpr<'a>>>,
        Option<Box<SPyExpr<'a>>>,
        Option<Box<SPyExpr<'a>>>,
    ),

    Yield(Box<SPyExpr<'a>>),
    YieldFrom(Box<SPyExpr<'a>>),
}

pub type SPyExpr<'a> = PySpanned<PyExpr<'a>>;

#[derive(Debug, Clone)]
pub struct PySpanned<T> {
    pub value: T,
    pub tl_span: Span,
    pub py_span: Option<PySpan>,
}

#[derive(Debug, Clone)]
pub struct PySpan {
    pub start_line: usize,
    pub start_col: usize,
    pub end_line: usize,
    pub end_col: usize,
}

impl PySpan {
    pub fn inline(start_col: usize, end_col: usize) -> Self {
        Self {
            start_line: 0,
            start_col,
            end_line: 0,
            end_col,
        }
    }
}

impl<T> From<(T, Span)> for PySpanned<T> {
    fn from((value, tl_span): (T, Span)) -> Self {
        PySpanned {
            value,
            tl_span,
            py_span: None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum PyLiteral<'a> {
    Num(PyIdent<'a>),
    Str(PyIdent<'a>),
    Bool(bool),
    None,
}
