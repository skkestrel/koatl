use crate::lexer::{SToken, Span};

#[derive(Debug, Clone)]
pub struct ListingItem<T, TTree: Tree> {
    pub item: T,
    pub separator: Option<TTree::Token>,
    pub newline: Option<TTree::Token>,
}

#[derive(Debug, Clone)]
pub struct Listing<T, TTree: Tree> {
    pub begin: TTree::Token,
    pub indent: Option<TTree::Token>,
    pub items: Vec<ListingItem<T, TTree>>,
    pub dedent: Option<TTree::Token>,
    pub newline: Option<TTree::Token>,
    pub end: TTree::Token,
}

#[derive(Debug, Clone)]
pub enum ImportLeaf<TTree: Tree> {
    Multi(Listing<ImportTree<TTree>, TTree>),
    Single(TTree::Token, Option<TTree::Token>),
    This(TTree::Token, Option<TTree::Token>),
    Star,
}

#[derive(Debug, Clone)]
pub struct ImportTree<TTree: Tree> {
    pub trunk: Vec<TTree::Token>,
    pub leaf: Spanned<ImportLeaf<TTree>>,
    pub level: usize,
}

#[derive(Debug, Clone)]
pub enum Stmt<TTree: Tree> {
    // modifier, identifiers
    Decl(TTree::Token, Vec<TTree::Token>),

    // modifier, lhs, =, rhs
    PatternAssign(
        Option<TTree::Token>,
        TTree::Pattern,
        TTree::Token,
        TTree::Expr,
    ),

    // lhs, op, =, rhs
    Assign(TTree::Expr, Option<TTree::Token>, TTree::Token, TTree::Expr),

    // while, cond, :, body
    While(TTree::Token, TTree::Expr, TTree::Token, TTree::Expr),

    // for, cursor, in, iterable, :, body
    For(
        TTree::Token,
        TTree::Pattern,
        TTree::Token,
        TTree::Expr,
        TTree::Token,
        TTree::Expr,
    ),

    // export, import, tree
    Import(Option<TTree::Token>, TTree::Token, ImportTree<TTree>),

    // try, expr, :, cases, (finally, :, expr)?
    Try(
        TTree::Token,
        TTree::Expr,
        TTree::Token,
        Vec<MatchCase<TTree>>,
        Option<(TTree::Token, TTree::Token, TTree::Expr)>,
    ),

    Break(TTree::Token),
    Continue(TTree::Token),
    Return(TTree::Token, TTree::Expr),
    Raise(TTree::Token, Option<TTree::Expr>),
    Expr(TTree::Expr),
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
pub enum CallItem<TTree: Tree> {
    Arg(TTree::Expr),
    Kwarg(TTree::Token, TTree::Expr),
    ArgSpread(TTree::Expr),
    KwargSpread(TTree::Expr),
}

#[derive(Debug, Clone)]
pub enum ArgDefItem<TTree: Tree> {
    Arg(TTree::Pattern, Option<TTree::Expr>),
    ArgSpread(TTree::Token),
    KwargSpread(TTree::Token),
}

#[derive(Debug, Clone)]
pub struct MatchCase<TTree: Tree> {
    pub pattern: Option<TTree::Pattern>,
    pub guard: Option<TTree::Expr>,
    pub body: TTree::Expr,
}

pub trait Tree {
    type Expr;
    type Pattern;
    type Stmt;
    type Token;
}

#[derive(Debug, Clone)]
pub enum Expr<TTree: Tree> {
    Literal(TTree::Token),
    Ident(TTree::Token),
    Tuple(Listing<ListItem<TTree>, TTree>),
    List(Listing<ListItem<TTree>, TTree>),
    Mapping(Listing<MappingItem<TTree>, TTree>),
    Slice(
        Option<TTree::Expr>,
        Option<TTree::Expr>,
        Option<TTree::Expr>,
    ),

    Unary(TTree::Token, TTree::Expr),
    Binary(TTree::Token, TTree::Expr, TTree::Expr),

    Await(TTree::Token, TTree::Expr),
    Yield(TTree::Token, TTree::Expr),
    YieldFrom(TTree::Token, TTree::Token, TTree::Expr),
    Memo(
        TTree::Token,
        TTree::Token,
        Option<TTree::Token>,
        TTree::Expr,
    ),

    If(TTree::Expr, TTree::Expr, Option<TTree::Expr>),

    Match(TTree::Expr, Vec<MatchCase<TTree>>),
    Matches(TTree::Expr, TTree::Pattern),
    Class(Vec<CallItem<TTree>>, TTree::Expr),

    With(TTree::Pattern, TTree::Expr, TTree::Expr),

    Call(TTree::Expr, Vec<CallItem<TTree>>),
    Subscript(TTree::Expr, Vec<ListItem<TTree>>),
    RawAttribute(TTree::Expr, TTree::Token),
    ScopedAttribute(TTree::Expr, TTree::Expr),
    Attribute(TTree::Expr, TTree::Token),

    MappedCall(TTree::Expr, Vec<CallItem<TTree>>),
    MappedSubscript(TTree::Expr, Vec<ListItem<TTree>>),
    MappedRawAttribute(TTree::Expr, TTree::Token),
    MappedScopedAttribute(TTree::Expr, TTree::Expr),
    MappedAttribute(TTree::Expr, TTree::Token),

    Checked(TTree::Expr, Option<TTree::Pattern>),

    Block(Vec<TTree::Stmt>),

    Fn(Vec<ArgDefItem<TTree>>, TTree::Expr),
    Fstr(Spanned<String>, Vec<(FmtExpr<TTree>, Spanned<String>)>),

    // these are removed during desugaring
    Decorated(TTree::Expr, TTree::Expr),
    Placeholder,
}

// Patterns

#[derive(Debug, Clone)]
pub enum PatternSequenceItem<TTree: Tree> {
    Item(TTree::Pattern),
    Spread(Option<TTree::Token>),
}

#[derive(Debug, Clone)]
pub enum PatternMappingItem<TTree: Tree> {
    Ident(TTree::Token),
    Item(TTree::Expr, TTree::Pattern),
    Spread(Option<TTree::Token>),
}

#[derive(Debug, Clone)]
pub enum PatternClassItem<TTree: Tree> {
    Item(TTree::Pattern),
    Kw(TTree::Token, TTree::Pattern),
}

#[derive(Debug, Clone)]
pub enum Pattern<TTree: Tree> {
    Capture(Option<TTree::Token>),
    Value(TTree::Expr),
    As(TTree::Pattern, TTree::Token),
    Or(Vec<TTree::Pattern>),
    Literal(TTree::Token),
    Sequence(Vec<PatternSequenceItem<TTree>>),
    Mapping(Vec<PatternMappingItem<TTree>>),
    Class(TTree::Expr, Vec<PatternClassItem<TTree>>),
}

// Spans

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

// Spanned types

#[derive(Debug, Clone)]
pub struct STree<'src: 'tok, 'tok> {
    _phantom: std::marker::PhantomData<&'src ()>,
    _phantom2: std::marker::PhantomData<&'tok ()>,
}

impl<'src: 'tok, 'tok> Tree for STree<'src, 'tok> {
    type Expr = Box<SExpr<'src, 'tok>>;
    type Pattern = Box<SPattern<'src, 'tok>>;
    type Stmt = Box<SStmt<'src, 'tok>>;
    type Token = &'tok SToken<'src>;
}

pub type SPatternInner<'src, 'tok> = Pattern<STree<'src, 'tok>>;

#[derive(Debug, Clone)]
pub struct SPattern<'src, 'tok> {
    pub value: SPatternInner<'src, 'tok>,
    pub span: Span,
}

impl<'src, 'tok> SPatternInner<'src, 'tok> {
    pub fn spanned(self, span: Span) -> SPattern<'src, 'tok> {
        SPattern { value: self, span }
    }
}

pub type SExprInner<'src, 'tok> = Expr<STree<'src, 'tok>>;

#[derive(Debug, Clone)]
pub struct SExpr<'src, 'tok> {
    pub value: SExprInner<'src, 'tok>,
    pub span: Span,
}

impl<'src, 'tok> SExpr<'src, 'tok> {
    pub fn boxed(self) -> Box<SExpr<'src, 'tok>> {
        Box::new(self)
    }
}

impl<'src, 'tok> SExprInner<'src, 'tok> {
    pub fn spanned(self, span: Span) -> SExpr<'src, 'tok> {
        SExpr { value: self, span }
    }
}

pub type SStmtInner<'src, 'tok> = Stmt<STree<'src, 'tok>>;

impl<'src, 'tok> SStmtInner<'src, 'tok> {
    pub fn spanned(self, span: Span) -> SStmt<'src, 'tok> {
        SStmt { value: self, span }
    }
}

#[derive(Debug, Clone)]
pub struct SStmt<'src, 'tok> {
    pub value: SStmtInner<'src, 'tok>,
    pub span: Span,
}

pub type SListItem<'src, 'tok> = ListItem<STree<'src, 'tok>>;
pub type SMappingItem<'src, 'tok> = MappingItem<STree<'src, 'tok>>;
pub type SMatchCase<'src, 'tok> = MatchCase<STree<'src, 'tok>>;
pub type SCallItem<'src, 'tok> = CallItem<STree<'src, 'tok>>;
pub type SArgDefItem<'src, 'tok> = ArgDefItem<STree<'src, 'tok>>;
pub type SFmtExpr<'src, 'tok> = FmtExpr<STree<'src, 'tok>>;
