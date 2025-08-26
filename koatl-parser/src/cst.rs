use crate::lexer::{SToken, Span, Token};

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
    Single {
        name: TTree::Token,
        // as, name
        alias: Option<(TTree::Token, TTree::Token)>,
    },
    This {
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
    // modifier, identifiers with commas
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
        op: Option<TTree::Token>,
        eq: TTree::Token,
        rhs: TTree::Expr,
    },

    While {
        while_kw: TTree::Token,
        cond: TTree::Expr,
        colon: TTree::Token,
        body: TTree::Expr,
    },

    For {
        for_kw: TTree::Token,
        pattern: TTree::Pattern,
        in_kw: TTree::Token,
        iter: TTree::Expr,
        colon: TTree::Token,
        body: TTree::Expr,
    },

    // export?, import, tree
    Import {
        export: Option<TTree::Token>,
        import: TTree::Token,
        tree: ImportTree<TTree>,
    },

    // try, expr, colon, cases, (finally, colon, expr)?
    Try {
        try_kw: TTree::Token,
        expr: TTree::Expr,
        colon: TTree::Token,
        cases: Vec<MatchCase<TTree>>,
        finally_clause: Option<(TTree::Token, TTree::Token, TTree::Expr)>,
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
    pub excl: Option<TTree::Token>,
    pub fmt: Option<TTree::Expr>,
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
pub enum MappingItem<TTree: Tree> {
    Ident {
        ident: TTree::Token,
    },
    Item {
        key: TTree::Expr,
        colon: TTree::Token,
        value: TTree::Expr,
    },
    Spread {
        stars: TTree::Token, // **
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
    pub pattern: Option<TTree::Pattern>,
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
    Fused {
        starter: TTree::Token,
        indent: TTree::Token,
        body: Vec<TTree::Stmt>,
        dedent: TTree::Token,
    },

    Program {
        body: Vec<TTree::Stmt>,
    },
}

#[derive(Debug, Clone)]
pub enum Expr<TTree: Tree> {
    Block(BlockKind<TTree>),

    Literal(TTree::Token),
    Ident(TTree::Token),
    Tuple(TupleKind<TTree>),
    List(Listing<ListItem<TTree>, TTree>),
    Mapping(Listing<MappingItem<TTree>, TTree>),

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
        cases: Vec<MatchCase<TTree>>,
    },

    Matches {
        lhs: TTree::Expr,
        matches_kw: TTree::Token,
        not_kw: Option<TTree::Token>,
        pattern: TTree::Pattern,
    },
    Class {
        class_kw: TTree::Token,
        lparen: Option<TTree::Token>,
        args: Vec<CallItem<TTree>>,
        rparen: Option<TTree::Token>,
        colon: TTree::Token,
        body: TTree::Expr,
    },

    With {
        with_kw: TTree::Token,
        pattern: TTree::Pattern,
        eq: TTree::Token,
        value: TTree::Expr,
        colon: TTree::Token,
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
        args: Listing<ArgDefItem<TTree>, TTree>,
        arrow: TTree::Token,
        body: TTree::Expr,
    },

    ParenthesizedFn {
        args: Listing<ArgDefItem<TTree>, TTree>,
        arrow: TTree::Token,
        body: TTree::Expr,
    },

    Fstr {
        begin: Spanned<String>,
        parts: Vec<(FmtExpr<TTree>, Spanned<String>)>,
    },

    // these are removed during desugaring
    Decorated {
        expr: TTree::Expr,
        ampersand: TTree::Token,
        decorator: TTree::Expr,
    },
    Placeholder(TTree::Token),

    // Grouping expressions
    Parenthesized {
        lparen: TTree::Token,
        expr: TTree::Expr,
        rparen: TTree::Token,
    },
}

// Patterns

#[derive(Debug, Clone)]
pub enum PatternSequenceItem<TTree: Tree> {
    Item {
        pattern: TTree::Pattern,
    },
    Spread {
        star: TTree::Token,
        name: Option<TTree::Token>,
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
        stars: TTree::Token, // **
        name: Option<TTree::Token>,
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

impl<'src, 'tok> SExpr<'src, 'tok> {
    pub fn simple_fmt(&self) -> String {
        self.value.simple_fmt()
    }
}

impl<'src, 'tok> SExprInner<'src, 'tok> {
    pub fn simple_fmt(&self) -> String {
        match self {
            Expr::Literal(token) => token.simple_fmt(),
            Expr::Ident(token) => token.simple_fmt(),
            Expr::Placeholder(token) => token.simple_fmt(),
            Expr::Binary {
                lhs,
                not,
                op,
                op_kind: _,
                rhs,
            } => {
                let not_str = if let Some(not_token) = not {
                    format!("{} ", not_token.simple_fmt())
                } else {
                    String::new()
                };

                format!(
                    "({} {}{} {})",
                    lhs.simple_fmt(),
                    not_str,
                    op.simple_fmt(),
                    rhs.simple_fmt()
                )
            }
            Expr::Unary {
                op_kind: _,
                op,
                expr,
            } => {
                format!("({} {})", op.token, expr.simple_fmt())
            }
            Expr::Call {
                expr,
                question,
                args,
            } => {
                let question_str = if question.is_some() { "?" } else { "" };
                let args_str = args
                    .items
                    .iter()
                    .map(|item| item.item.simple_fmt())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}{}({})", expr.simple_fmt(), question_str, args_str)
            }
            Expr::List(listing) => {
                let items_str = listing
                    .items
                    .iter()
                    .map(|item| item.item.simple_fmt())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{}]", items_str)
            }
            Expr::Tuple(kind) => match kind {
                TupleKind::Unit(..) => format!("()"),
                TupleKind::Listing(items) => {
                    if items.len() == 1 {
                        return format!("({},)", items[0].item.simple_fmt());
                    }

                    let items_str = items
                        .iter()
                        .map(|item| item.item.simple_fmt())
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("({})", items_str)
                }
            },
            Expr::If {
                cond,
                then_kw: _,
                then,
                else_clause,
            } => {
                let else_str = if let Some((_, expr)) = else_clause {
                    format!(" else {}", expr.simple_fmt())
                } else {
                    String::new()
                };
                format!(
                    "{} then {}{}",
                    cond.simple_fmt(),
                    then.simple_fmt(),
                    else_str
                )
            }
            Expr::Parenthesized { expr, .. } => expr.simple_fmt(),
            Expr::Subscript {
                expr,
                question,
                indices,
            } => {
                let question_str = if question.is_some() { "?" } else { "" };
                let indices_str = indices
                    .items
                    .iter()
                    .map(|item| item.item.simple_fmt())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}{}[{}]", expr.simple_fmt(), question_str, indices_str)
            }
            Expr::RawAttribute {
                expr,
                question,
                attr,
                ..
            } => {
                let question_str = if question.is_some() { "?" } else { "" };
                format!(
                    "{}{}::{}",
                    expr.simple_fmt(),
                    question_str,
                    attr.simple_fmt()
                )
            }
            Expr::ScopedAttribute {
                expr,
                question,
                rhs,
                ..
            } => {
                let question_str = if question.is_some() { "?" } else { "" };
                format!("{}{}.{}", expr.simple_fmt(), question_str, rhs.simple_fmt())
            }
            Expr::Attribute {
                expr,
                question,
                attr,
                ..
            } => {
                let question_str = if question.is_some() { "?" } else { "" };
                format!(
                    "{}{}.{}",
                    expr.simple_fmt(),
                    question_str,
                    attr.simple_fmt()
                )
            }
            Expr::Slice {
                start,
                dots: _,
                stop: end,
                step_dots: _,
                step,
            } => {
                let start_str = start.as_ref().map(|s| s.simple_fmt()).unwrap_or_default();
                let end_str = end.as_ref().map(|e| e.simple_fmt()).unwrap_or_default();

                if let Some(step) = step {
                    format!("({}..{}..{})", start_str, end_str, step.simple_fmt())
                } else {
                    format!("({}..{})", start_str, end_str)
                }
            }
            Expr::Block(block) => match block {
                BlockKind::Fused { body, .. } => {
                    let stmts_str = body
                        .iter()
                        .map(|stmt| stmt.value.simple_fmt())
                        .collect::<Vec<_>>()
                        .join("; ");
                    format!("Block({})", stmts_str)
                }
                BlockKind::Program { body, .. } => body
                    .iter()
                    .map(|stmt| stmt.value.simple_fmt())
                    .collect::<Vec<_>>()
                    .join("\n"),
            },
            _ => format!("{:?}", self).chars().take(50).collect::<String>() + "...",
        }
    }
}

impl<'src, 'tok> SStmt<'src, 'tok> {
    pub fn simple_fmt(&self) -> String {
        self.value.simple_fmt()
    }
}

impl<'src, 'tok> SStmtInner<'src, 'tok> {
    pub fn simple_fmt(&self) -> String {
        match self {
            Stmt::Expr { expr } => expr.simple_fmt(),
            Stmt::Assign { lhs, rhs, .. } => {
                format!("{} = {}", lhs.simple_fmt(), rhs.simple_fmt())
            }
            _ => format!("{:?}", self).chars().take(30).collect::<String>() + "...",
        }
    }
}

impl<'src, 'tok> ListItem<STree<'src, 'tok>> {
    pub fn simple_fmt(&self) -> String {
        match self {
            ListItem::Item { expr } => expr.simple_fmt(),
            ListItem::Spread { expr, .. } => format!("*{}", expr.simple_fmt()),
        }
    }
}

impl<'src, 'tok> CallItem<STree<'src, 'tok>> {
    pub fn simple_fmt(&self) -> String {
        match self {
            CallItem::Arg { expr } => expr.simple_fmt(),
            CallItem::Kwarg { name, expr, .. } => {
                format!("{}={}", name.simple_fmt(), expr.simple_fmt())
            }
            CallItem::ArgSpread { expr, .. } => format!("*{}", expr.simple_fmt()),
            CallItem::KwargSpread { expr, .. } => format!("**{}", expr.simple_fmt()),
        }
    }
}

impl<'src, 'tok> SPattern<'src, 'tok> {
    pub fn simple_fmt(&self) -> String {
        self.value.simple_fmt()
    }
}

impl<'src, 'tok> Pattern<STree<'src, 'tok>> {
    pub fn simple_fmt(&self) -> String {
        match self {
            Pattern::Capture { name } => name.simple_fmt(),
            Pattern::Value { expr, .. } => {
                format!(".{}", expr.simple_fmt())
            }
            Pattern::As { pattern, name, .. } => {
                format!("{} as {}", pattern.simple_fmt(), name.simple_fmt())
            }
            Pattern::Or { head, rest } => {
                let mut parts = vec![head.simple_fmt()];
                for (_, pattern) in rest {
                    parts.push(pattern.simple_fmt());
                }
                parts.join(" | ")
            }
            Pattern::Literal { token } => token.simple_fmt(),
            Pattern::Sequence { listing } => {
                let items_str = listing
                    .items
                    .iter()
                    .map(|item| item.item.simple_fmt())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{}]", items_str)
            }
            Pattern::TupleSequence { listing } => {
                let items_str = listing
                    .items
                    .iter()
                    .map(|item| item.item.simple_fmt())
                    .collect::<Vec<_>>()
                    .join(", ");
                if listing.items.len() == 1 {
                    format!("({},)", items_str)
                } else {
                    format!("({})", items_str)
                }
            }
            Pattern::Mapping { listing } => {
                let items_str = listing
                    .items
                    .iter()
                    .map(|item| item.item.simple_fmt())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{{{}}}", items_str)
            }
            Pattern::Class { expr, items, .. } => {
                let items_str = items
                    .items
                    .iter()
                    .map(|item| item.item.simple_fmt())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", expr.simple_fmt(), items_str)
            }
            Pattern::Parenthesized { pattern, .. } => {
                format!("({})", pattern.simple_fmt())
            }
        }
    }
}

impl<'src, 'tok> PatternSequenceItem<STree<'src, 'tok>> {
    pub fn simple_fmt(&self) -> String {
        match self {
            PatternSequenceItem::Item { pattern } => pattern.simple_fmt(),
            PatternSequenceItem::Spread { name, .. } => {
                if let Some(name) = name {
                    format!("*{}", name.simple_fmt())
                } else {
                    "*".to_string()
                }
            }
        }
    }
}

impl<'src, 'tok> PatternMappingItem<STree<'src, 'tok>> {
    pub fn simple_fmt(&self) -> String {
        match self {
            PatternMappingItem::Ident { name } => name.simple_fmt(),
            PatternMappingItem::Item { key, pattern, .. } => {
                format!("{}: {}", key.simple_fmt(), pattern.simple_fmt())
            }
            PatternMappingItem::Spread { name, .. } => {
                if let Some(name) = name {
                    format!("**{}", name.simple_fmt())
                } else {
                    "**".to_string()
                }
            }
        }
    }
}

impl<'src, 'tok> PatternClassItem<STree<'src, 'tok>> {
    pub fn simple_fmt(&self) -> String {
        match self {
            PatternClassItem::Item { pattern } => pattern.simple_fmt(),
            PatternClassItem::Kw { name, pattern, .. } => {
                format!("{}={}", name.simple_fmt(), pattern.simple_fmt())
            }
        }
    }
}

impl<'src> SToken<'src> {
    pub fn simple_fmt(&self) -> String {
        self.token.simple_fmt()
    }
}

impl<'src> Token<'src> {
    pub fn simple_fmt(&self) -> String {
        match self {
            Token::Ident(s) => s.to_string(),
            Token::Num(s) => s.to_string(),
            Token::Str(s) => format!("\"{}\"", s),
            Token::Bool(b) => b.to_string(),
            Token::None => "None".to_string(),
            Token::Symbol(s) => s.to_string(),
            Token::Kw(s) => s.to_string(),
            _ => format!("{:?}", self),
        }
    }
}
