use std::borrow::Cow;

use koatl_parser::lexer::Span;

pub type PyToken<'a> = Cow<'a, str>;

#[derive(Debug, Clone, PartialEq)]
pub enum PyAccessCtx {
    Load,
    Store,
    Del,
}

#[derive(Debug, Clone)]
pub struct PyImportAlias<'a> {
    pub name: PyToken<'a>,
    pub as_name: Option<PyToken<'a>>,
}

#[derive(Debug, Clone)]
pub struct PyExceptHandler<'a> {
    pub typ: Option<SPyExpr<'a>>,
    pub name: Option<PyToken<'a>>,
    pub body: PyBlock<'a>,
}

#[derive(Debug, Clone)]
pub struct PyMatchCase<'a> {
    pub guard: Option<SPyExpr<'a>>,
    pub pattern: SPyPattern<'a>,
    pub body: PyBlock<'a>,
}

#[derive(Debug, Clone)]
pub struct PyDecorators<'a>(pub Vec<SPyExpr<'a>>);

impl<'a> PyDecorators<'a> {
    pub fn new() -> Self {
        PyDecorators(Vec::new())
    }

    pub fn push(&mut self, decorator: SPyExpr<'a>) {
        self.0.push(decorator);
    }
}

#[derive(Debug, Clone)]
pub struct PyFnDef<'a> {
    pub name: PyToken<'a>,
    pub args: PyArgList<'a>,
    pub body: PyBlock<'a>,
    pub decorators: PyDecorators<'a>,
    pub async_: bool,
}

#[derive(Debug, Clone)]
pub struct PyClassDef<'a> {
    pub name: PyToken<'a>,
    pub bases: Vec<PyCallItem<'a>>,
    pub body: PyBlock<'a>,
    pub decorators: PyDecorators<'a>,
}

#[derive(Debug, Clone)]
pub enum PyStmt<'a> {
    Expr(SPyExpr<'a>),
    With(Vec<(SPyExpr<'a>, Option<SPyExpr<'a>>)>, PyBlock<'a>),
    If(SPyExpr<'a>, PyBlock<'a>, Option<PyBlock<'a>>),
    Match(SPyExpr<'a>, Vec<PyMatchCase<'a>>),
    Assign(SPyExpr<'a>, SPyExpr<'a>, Option<PyBinaryOp>),
    Return(SPyExpr<'a>),
    Raise(Option<SPyExpr<'a>>),
    Assert(SPyExpr<'a>, Option<SPyExpr<'a>>),
    Global(Vec<PyToken<'a>>),
    Nonlocal(Vec<PyToken<'a>>),
    Import(Vec<PyImportAlias<'a>>),
    ImportFrom(Option<PyToken<'a>>, Vec<PyImportAlias<'a>>, usize),
    FnDef(PyFnDef<'a>),
    ClassDef(PyClassDef<'a>),
    While(SPyExpr<'a>, PyBlock<'a>),
    For(SPyExpr<'a>, SPyExpr<'a>, PyBlock<'a>),
    Try(PyBlock<'a>, Vec<PyExceptHandler<'a>>, Option<PyBlock<'a>>),
    Del(Vec<SPyExpr<'a>>),
    Break,
    Continue,
    Pass,
}

pub type SPyStmt<'a> = PySpanned<PyStmt<'a>>;

#[derive(Debug, Clone)]
pub struct PyBlock<'a>(pub Vec<SPyStmt<'a>>);

impl<'a> PyBlock<'a> {
    pub fn new() -> Self {
        PyBlock(Vec::new())
    }

    pub fn push(&mut self, stmt: SPyStmt<'a>) {
        self.0.push(stmt);
    }

    pub fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = SPyStmt<'a>>,
    {
        self.0.extend(iter);
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<'a> From<PyBlock<'a>> for Vec<SPyStmt<'a>> {
    fn from(block: PyBlock<'a>) -> Self {
        block.0
    }
}

impl<'a> IntoIterator for PyBlock<'a> {
    type Item = SPyStmt<'a>;
    type IntoIter = std::vec::IntoIter<SPyStmt<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> PyBlock<'a> {
    pub fn iter(&self) -> std::slice::Iter<SPyStmt<'a>> {
        self.0.iter()
    }
}

impl<'a> std::iter::FromIterator<SPyStmt<'a>> for PyBlock<'a> {
    fn from_iter<I: IntoIterator<Item = SPyStmt<'a>>>(iter: I) -> Self {
        PyBlock(iter.into_iter().collect())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PyBinaryOp {
    Add,
    Sub,
    Mult,
    Div,
    Mod,
    FloorDiv,
    Pow,
    MatMult,

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
    In,
    Nin,

    BitAnd,
    BitOr,
    BitXor,
    LShift,
    RShift,
}

#[derive(Debug, Clone)]
pub enum PyUnaryOp {
    Not,
    Neg,
    Pos,
    Inv,
}

#[derive(Debug, Clone)]
pub enum PyCallItem<'a> {
    Arg(SPyExpr<'a>),
    Kwarg(PyToken<'a>, SPyExpr<'a>),
    ArgSpread(SPyExpr<'a>),
    KwargSpread(SPyExpr<'a>),
}

#[derive(Debug, Clone)]
pub struct PyArgList<'a> {
    /// Position-only arguments (before `/` marker)
    pub posonlyargs: Vec<(PyToken<'a>, Option<SPyExpr<'a>>)>,
    /// Regular positional or keyword arguments
    pub args: Vec<(PyToken<'a>, Option<SPyExpr<'a>>)>,
    /// Variadic positional argument (*args)
    pub vararg: Option<PyToken<'a>>,
    /// Keyword-only arguments (after `*` or `*args`)
    pub kwonlyargs: Vec<(PyToken<'a>, Option<SPyExpr<'a>>)>,
    /// Variadic keyword argument (**kwargs)
    pub kwarg: Option<PyToken<'a>>,
}

impl<'a> PyArgList<'a> {
    pub fn empty() -> Self {
        PyArgList {
            posonlyargs: vec![],
            args: vec![],
            vararg: None,
            kwonlyargs: vec![],
            kwarg: None,
        }
    }

    pub fn simple_args(args: Vec<(PyToken<'a>, Option<SPyExpr<'a>>)>) -> Self {
        PyArgList {
            posonlyargs: vec![],
            args,
            vararg: None,
            kwonlyargs: vec![],
            kwarg: None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum PyListItem<'a> {
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
    Str(PyToken<'a>),
    Expr(SPyExpr<'a>, Option<SPyExpr<'a>>),
}

#[derive(Debug, Clone)]
pub enum PyExpr<'a> {
    Literal(PyLiteral<'a>),
    Fstr(Vec<PyFstrPart<'a>>),
    Name(PyToken<'a>, PyAccessCtx),

    Binary(PyBinaryOp, Box<SPyExpr<'a>>, Box<SPyExpr<'a>>),
    Unary(PyUnaryOp, Box<SPyExpr<'a>>),
    Call(Box<SPyExpr<'a>>, Vec<PyCallItem<'a>>),
    Attribute(Box<SPyExpr<'a>>, PyToken<'a>, PyAccessCtx),
    Subscript(Box<SPyExpr<'a>>, Box<SPyExpr<'a>>, PyAccessCtx),

    IfExpr(Box<SPyExpr<'a>>, Box<SPyExpr<'a>>, Box<SPyExpr<'a>>),
    Lambda(PyArgList<'a>, Box<SPyExpr<'a>>),

    List(Vec<PyListItem<'a>>, PyAccessCtx),
    Tuple(Vec<PyListItem<'a>>, PyAccessCtx),
    Dict(Vec<PyDictItem<'a>>),
    Slice(
        Option<Box<SPyExpr<'a>>>,
        Option<Box<SPyExpr<'a>>>,
        Option<Box<SPyExpr<'a>>>,
    ),

    Await(Box<SPyExpr<'a>>),
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

pub type PySpan = Span;

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
    Int(PyToken<'a>),
    IntBin(PyToken<'a>),
    IntOct(PyToken<'a>),
    IntHex(PyToken<'a>),
    Float(PyToken<'a>),
    Str(PyToken<'a>),
    Bool(bool),
    None,
}

#[derive(Debug, Clone)]
pub enum PyPatternSequenceItem<'a> {
    Item(SPyPattern<'a>),
    Spread(Option<PyToken<'a>>),
}

#[derive(Debug, Clone)]
pub enum PyPattern<'a> {
    Value(SPyExpr<'a>),
    Singleton(PyLiteral<'a>),
    As(Option<Box<SPyPattern<'a>>>, Option<PyToken<'a>>),
    Or(Vec<SPyPattern<'a>>),
    Sequence(Vec<PyPatternSequenceItem<'a>>),
    Mapping(Vec<(SPyExpr<'a>, SPyPattern<'a>)>, Option<PyToken<'a>>),
    Class(
        SPyExpr<'a>,
        Vec<SPyPattern<'a>>,
        Vec<(PyToken<'a>, SPyPattern<'a>)>,
    ),
}

pub type SPyPattern<'a> = PySpanned<PyPattern<'a>>;
