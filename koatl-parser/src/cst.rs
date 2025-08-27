use crate::lexer::{SToken, Span};

pub trait Tree {
    type Expr: std::fmt::Debug + Clone;
    type Pattern: std::fmt::Debug + Clone;
    type Stmt: std::fmt::Debug + Clone;
    type Token: std::fmt::Debug + Clone;
}

#[derive(Debug, Clone)]
pub struct ListingItem<T, TTree: Tree> {
    pub item: T,
    pub separator: Option<TTree::Token>,
    pub newline: Option<TTree::Token>,
}

#[derive(Debug, Clone)]
pub enum Listing<T, TTree: Tree> {
    Block {
        begin: TTree::Token,
        indent: TTree::Token,
        items: Vec<ListingItem<T, TTree>>,
        dedent: TTree::Token,
        newline: Option<TTree::Token>,
        end: TTree::Token,
    },

    Inline {
        begin: TTree::Token,
        items: Vec<ListingItem<T, TTree>>,
        end: TTree::Token,
    },

    Open {
        items: Vec<ListingItem<T, TTree>>,
    },
}

#[derive(Debug, Clone)]
pub enum ImportLeaf<TTree: Tree> {
    Multi(Listing<ImportTree<TTree>, TTree>),
    Single {
        name: TTree::Token,
        // as, name
        alias: Option<(TTree::Token, TTree::Token)>,
    },
    This {
        dot: TTree::Token,
        // as, name
        alias: Option<(TTree::Token, TTree::Token)>,
    },
    Star {
        star: TTree::Token,
    },
}

#[derive(Debug, Clone)]
pub struct ImportTree<TTree: Tree> {
    pub dots: Vec<TTree::Token>,
    // ident, dot
    pub trunk: Vec<(TTree::Token, TTree::Token)>,
    pub leaf: Spanned<ImportLeaf<TTree>>,
}

#[derive(Debug, Clone)]
pub enum Stmt<TTree: Tree> {
    Decl {
        modifier: TTree::Token,
        names: Vec<(TTree::Token, Option<TTree::Token>)>, // ident, comma
    },

    PatternAssign {
        modifier: Option<TTree::Token>,
        lhs: TTree::Pattern,
        eq: TTree::Token,
        rhs: TTree::Expr,
    },

    Assign {
        lhs: TTree::Expr,
        op: Option<BinaryOp>,
        eq: TTree::Token,
        rhs: TTree::Expr,
    },

    While {
        while_kw: TTree::Token,
        cond: TTree::Expr,
        body: TTree::Expr,
    },

    For {
        for_kw: TTree::Token,
        pattern: TTree::Pattern,
        in_kw: TTree::Token,
        iter: TTree::Expr,
        body: TTree::Expr,
    },

    Import {
        export: Option<TTree::Token>,
        import: TTree::Token,
        tree: ImportTree<TTree>,
    },

    Try {
        try_kw: TTree::Token,
        expr: TTree::Expr,
        cases: Vec<ExceptCase<TTree>>,
        finally: Option<(TTree::Token, TTree::Expr)>,
    },

    Break {
        break_kw: TTree::Token,
    },
    Continue {
        continue_kw: TTree::Token,
    },
    Return {
        return_kw: TTree::Token,
        expr: TTree::Expr,
    },
    Raise {
        raise_kw: TTree::Token,
        expr: Option<TTree::Expr>,
    },
    Expr {
        expr: TTree::Expr,
    },
}

#[derive(Debug, Clone)]
pub struct FmtExpr<TTree: Tree> {
    pub expr: TTree::Expr,
    pub fmt: Option<(TTree::Token, TTree::Expr)>,
}

#[derive(Debug, Clone)]
pub enum ListItem<TTree: Tree> {
    Item {
        expr: TTree::Expr,
    },
    Spread {
        star: TTree::Token,
        expr: TTree::Expr,
    },
}

#[derive(Debug, Clone)]
pub enum MappingKey<TTree: Tree> {
    Ident {
        token: TTree::Token,
    },
    Literal {
        token: TTree::Token,
    },
    Expr {
        lparen: TTree::Token,
        key: TTree::Expr,
        rparen: TTree::Token,
    },
}

#[derive(Debug, Clone)]
pub enum MappingItem<TTree: Tree> {
    Ident {
        ident: TTree::Token,
    },
    Item {
        key: MappingKey<TTree>,
        colon: TTree::Token,
        value: TTree::Expr,
    },
    Spread {
        stars: TTree::Token,
        expr: TTree::Expr,
    },
}

#[derive(Debug, Clone)]
pub enum CallItem<TTree: Tree> {
    Arg {
        expr: TTree::Expr,
    },
    Kwarg {
        name: TTree::Token,
        eq: TTree::Token,
        expr: TTree::Expr,
    },
    ArgSpread {
        star: TTree::Token,
        expr: TTree::Expr,
    },
    KwargSpread {
        stars: TTree::Token, // **
        expr: TTree::Expr,
    },
}

#[derive(Debug, Clone)]
pub enum ArgDefItem<TTree: Tree> {
    Arg {
        pattern: TTree::Pattern,
        default: Option<(TTree::Token, TTree::Expr)>, // =, expr
    },
    ArgSpread {
        star: TTree::Token,
        name: TTree::Token,
    },
    KwargSpread {
        stars: TTree::Token, // **
        name: TTree::Token,
    },
}

#[derive(Debug, Clone)]
pub struct MatchCase<TTree: Tree> {
    pub pattern: TTree::Pattern,
    pub guard: Option<(TTree::Token, TTree::Expr)>,
    pub arrow: TTree::Token,
    pub body: TTree::Expr,
}

#[derive(Debug, Clone)]
pub struct ExceptCase<TTree: Tree> {
    pub except: TTree::Token,
    pub pattern: TTree::Pattern,
    pub guard: Option<(TTree::Token, TTree::Expr)>,
    pub arrow: TTree::Token,
    pub body: TTree::Expr,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    MatMul,
    Div,
    Exp,

    FloorDiv,
    Mod,

    In,
    Nin,
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

#[derive(Debug, Clone)]
pub enum TupleKind<TTree: Tree> {
    Unit(TTree::Token, TTree::Token),
    Listing(Vec<ListingItem<ListItem<TTree>, TTree>>),
}

#[derive(Debug, Clone)]
pub enum BlockKind<TTree: Tree> {
    Regular {
        colon: Option<TTree::Token>,
        indent: TTree::Token,
        body: Vec<TTree::Stmt>,
        dedent: TTree::Token,
    },

    Bare {
        body: Vec<TTree::Stmt>,
    },

    Inline {
        colon: Option<TTree::Token>,
        stmt: TTree::Stmt,
    },
}

#[derive(Debug, Clone)]
pub enum Expr<TTree: Tree> {
    Block {
        kind: BlockKind<TTree>,
    },

    Literal {
        token: TTree::Token,
    },
    Ident {
        token: TTree::Token,
    },
    Tuple {
        kind: TupleKind<TTree>,
    },
    List {
        listing: Listing<ListItem<TTree>, TTree>,
    },
    Mapping {
        listing: Listing<MappingItem<TTree>, TTree>,
    },

    Slice {
        start: Option<TTree::Expr>,
        dots: TTree::Token,
        stop: Option<TTree::Expr>,
        step_dots: Option<TTree::Token>,
        step: Option<TTree::Expr>,
    },

    Unary {
        op: TTree::Token,
        op_kind: UnaryOp,
        expr: TTree::Expr,
    },
    Binary {
        lhs: TTree::Expr,
        not: Option<TTree::Token>,
        op: TTree::Token,
        op_kind: BinaryOp,
        rhs: TTree::Expr,
    },

    Await {
        await_kw: TTree::Token,
        expr: TTree::Expr,
    },
    Yield {
        yield_kw: TTree::Token,
        expr: TTree::Expr,
    },
    YieldFrom {
        yield_kw: TTree::Token,
        from_kw: TTree::Token,
        expr: TTree::Expr,
    },
    Memo {
        async_kw: Option<TTree::Token>,
        memo_kw: TTree::Token,
        colon: Option<TTree::Token>,
        expr: TTree::Expr,
    },

    If {
        cond: TTree::Expr,
        then_kw: TTree::Token,
        then: TTree::Expr,
        else_clause: Option<(TTree::Token, TTree::Expr)>,
    },

    Match {
        scrutinee: TTree::Expr,
        match_kw: TTree::Token,
        colon: Option<TTree::Token>,
        indent: TTree::Token,
        cases: Vec<MatchCase<TTree>>,
        dedent: TTree::Token,
    },

    Matches {
        lhs: TTree::Expr,
        matches_kw: TTree::Token,
        not_kw: Option<TTree::Token>,
        pattern: TTree::Pattern,
    },
    Class {
        class_kw: TTree::Token,
        args: Option<Listing<CallItem<TTree>, TTree>>,
        body: TTree::Expr,
    },

    With {
        with_kw: TTree::Token,
        pattern: TTree::Pattern,
        eq: TTree::Token,
        value: TTree::Expr,
        body: TTree::Expr,
    },

    Call {
        expr: TTree::Expr,
        question: Option<TTree::Token>,
        args: Listing<CallItem<TTree>, TTree>,
    },
    MethodCall {
        expr: TTree::Expr,
        question: Option<TTree::Token>,
        dot: TTree::Token,
        method: TTree::Token,
        args: Listing<CallItem<TTree>, TTree>,
    },
    Subscript {
        expr: TTree::Expr,
        question: Option<TTree::Token>,
        indices: Listing<ListItem<TTree>, TTree>,
    },
    RawAttribute {
        expr: TTree::Expr,
        question: Option<TTree::Token>,
        double_colon: TTree::Token,
        attr: TTree::Token,
    },
    ScopedAttribute {
        expr: TTree::Expr,
        question: Option<TTree::Token>,
        dot: TTree::Token,
        lparen: TTree::Token,
        rhs: TTree::Expr,
        rparen: TTree::Token,
    },
    Attribute {
        expr: TTree::Expr,
        question: Option<TTree::Token>,
        dot: TTree::Token,
        attr: TTree::Token,
    },

    Checked {
        try_kw: TTree::Token,
        expr: TTree::Expr,
        except_kw: Option<TTree::Token>,
        pattern: Option<TTree::Pattern>,
    },

    Fn {
        arg: TTree::Pattern,
        arrow: TTree::Token,
        body: TTree::Expr,
    },

    ParenthesizedFn {
        args: Listing<ArgDefItem<TTree>, TTree>,
        arrow: TTree::Token,
        body: TTree::Expr,
    },

    Fstr {
        begin: TTree::Token,
        parts: Vec<(FmtExpr<TTree>, TTree::Token)>,
    },

    // these are removed during desugaring
    Decorated {
        expr: TTree::Expr,
        ampersand: TTree::Token,
        decorator: TTree::Expr,
    },
    Placeholder {
        token: TTree::Token,
    },

    Parenthesized {
        lparen: TTree::Token,
        expr: TTree::Expr,
        rparen: TTree::Token,
    },

    Error,
}

// Patterns

#[derive(Debug, Clone)]
pub enum PatternSequenceItem<TTree: Tree> {
    Item {
        pattern: TTree::Pattern,
    },
    Spread {
        star: TTree::Token,
        name: TTree::Token,
    },
}

#[derive(Debug, Clone)]
pub enum PatternMappingItem<TTree: Tree> {
    Ident {
        name: TTree::Token,
    },
    Item {
        key: TTree::Expr,
        colon: TTree::Token,
        pattern: TTree::Pattern,
    },
    Spread {
        stars: TTree::Token,
        name: TTree::Token,
    },
}

#[derive(Debug, Clone)]
pub enum PatternClassItem<TTree: Tree> {
    Item {
        pattern: TTree::Pattern,
    },
    Kw {
        name: TTree::Token,
        eq: TTree::Token,
        pattern: TTree::Pattern,
    },
}

#[derive(Debug, Clone)]
pub enum Pattern<TTree: Tree> {
    Capture {
        name: TTree::Token,
    },
    Value {
        dot: TTree::Token,
        expr: TTree::Expr,
    },
    As {
        pattern: TTree::Pattern,
        as_kw: TTree::Token,
        name: TTree::Token,
    },
    Or {
        head: TTree::Pattern,
        rest: Vec<(TTree::Token, TTree::Pattern)>,
    },
    Literal {
        token: TTree::Token,
    },
    Sequence {
        listing: Listing<PatternSequenceItem<TTree>, TTree>,
    },
    TupleSequence {
        listing: Listing<PatternSequenceItem<TTree>, TTree>,
    },
    Mapping {
        listing: Listing<PatternMappingItem<TTree>, TTree>,
    },
    Class {
        expr: TTree::Expr,
        items: Listing<PatternClassItem<TTree>, TTree>,
    },
    Parenthesized {
        lparen: TTree::Token,
        pattern: TTree::Pattern,
        rparen: TTree::Token,
    },
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

impl<'src, 'tok> SPattern<'src, 'tok> {
    pub fn boxed(self) -> Box<SPattern<'src, 'tok>> {
        Box::new(self)
    }
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

pub type SFusedBlock<'src, 'tok> = BlockKind<STree<'src, 'tok>>;
pub type SListing<'src, 'tok, T> = Listing<T, STree<'src, 'tok>>;
pub type SListItem<'src, 'tok> = ListItem<STree<'src, 'tok>>;
pub type SMappingItem<'src, 'tok> = MappingItem<STree<'src, 'tok>>;
pub type SMatchCase<'src, 'tok> = MatchCase<STree<'src, 'tok>>;
pub type SCallItem<'src, 'tok> = CallItem<STree<'src, 'tok>>;
pub type SArgDefItem<'src, 'tok> = ArgDefItem<STree<'src, 'tok>>;
pub type SFmtExpr<'src, 'tok> = FmtExpr<STree<'src, 'tok>>;
