use crate::lexer::{SToken, Span, Token, py_escape_fstr};

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

// Format

pub trait SimpleFmt {
    fn simple_fmt(&self) -> String;
}

impl<'src, 'tok> SimpleFmt for SExpr<'src, 'tok> {
    fn simple_fmt(&self) -> String {
        self.value.simple_fmt()
    }
}

impl<'src, 'tok> SimpleFmt for SExprInner<'src, 'tok> {
    fn simple_fmt(&self) -> String {
        match self {
            Expr::Literal { token } => token.simple_fmt(),
            Expr::Ident { token } => token.simple_fmt(),
            Expr::Placeholder { token } => token.simple_fmt(),
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
                format!(
                    "{}{}({})",
                    expr.simple_fmt(),
                    question_str,
                    args.simple_fmt()
                )
            }
            Expr::List { listing } => listing.simple_fmt(),
            Expr::Tuple { kind } => match kind {
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
                format!(
                    "{}{}[{}]",
                    expr.simple_fmt(),
                    question_str,
                    indices.simple_fmt()
                )
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
            Expr::Block { kind } => match kind {
                BlockKind::Regular { body, .. } => {
                    let stmts_str = body
                        .iter()
                        .map(|stmt| stmt.value.simple_fmt())
                        .collect::<Vec<_>>()
                        .join("; ");
                    format!("Block({})", stmts_str)
                }
                BlockKind::Inline { stmt, .. } => stmt.value.simple_fmt(),
                BlockKind::Bare { body, .. } => body
                    .iter()
                    .map(|stmt| stmt.value.simple_fmt())
                    .collect::<Vec<_>>()
                    .join("\n"),
            },
            Expr::Mapping { listing } => listing.simple_fmt(),
            Expr::Await { expr, .. } => {
                format!("(await {})", expr.simple_fmt())
            }
            Expr::Yield { expr, .. } => {
                format!("(yield {})", expr.simple_fmt())
            }
            Expr::YieldFrom { expr, .. } => {
                format!("(yield from {})", expr.simple_fmt())
            }
            Expr::Memo { async_kw, expr, .. } => {
                let async_str = if async_kw.is_some() { "async " } else { "" };
                format!("({}memo {})", async_str, expr.simple_fmt())
            }
            Expr::Match {
                scrutinee, cases, ..
            } => {
                let cases_str = cases
                    .iter()
                    .map(|case| case.simple_fmt())
                    .collect::<Vec<_>>()
                    .join("; ");
                format!("({} match: {})", scrutinee.simple_fmt(), cases_str)
            }
            Expr::Matches {
                lhs,
                not_kw,
                pattern,
                ..
            } => {
                let not_str = if not_kw.is_some() { " not" } else { "" };
                format!(
                    "({}{} matches {})",
                    lhs.simple_fmt(),
                    not_str,
                    pattern.simple_fmt()
                )
            }
            Expr::Class { args, body, .. } => {
                let args_str = args
                    .iter()
                    .map(|arg| arg.simple_fmt())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("class{} {})", args_str, body.simple_fmt())
            }
            Expr::With {
                pattern,
                value,
                body,
                ..
            } => {
                format!(
                    "with {} = {}: {}",
                    pattern.simple_fmt(),
                    value.simple_fmt(),
                    body.simple_fmt()
                )
            }
            Expr::MethodCall {
                expr,
                question,
                method,
                args,
                ..
            } => {
                let question_str = if question.is_some() { "?" } else { "" };
                format!(
                    "{}{}.{}({})",
                    expr.simple_fmt(),
                    question_str,
                    method.simple_fmt(),
                    args.simple_fmt()
                )
            }
            Expr::Checked {
                expr,
                except_kw,
                pattern,
                ..
            } => {
                let except_str = if let Some(pattern) = pattern {
                    format!(" except {}", pattern.simple_fmt())
                } else if except_kw.is_some() {
                    " except".to_string()
                } else {
                    String::new()
                };
                format!("try {}{}", expr.simple_fmt(), except_str)
            }
            Expr::Fn { arg, body, .. } => {
                format!("{} => {}", arg.simple_fmt(), body.simple_fmt())
            }
            Expr::ParenthesizedFn { args, body, .. } => {
                format!("{} => {}", args.simple_fmt(), body.simple_fmt())
            }
            Expr::Fstr { begin, parts } => {
                if parts.is_empty() {
                    format!("f\"{}\"", begin.token.simple_fmt())
                } else {
                    let parts_str = parts
                        .iter()
                        .map(|(fmt_expr, cont)| {
                            format!("{{{}}}{}", fmt_expr.simple_fmt(), cont.simple_fmt())
                        })
                        .collect::<Vec<_>>()
                        .join("");
                    format!("f\"{}{}\"", begin.token.simple_fmt(), parts_str)
                }
            }
            Expr::Decorated {
                expr, decorator, ..
            } => {
                format!("{} & {}", expr.simple_fmt(), decorator.simple_fmt())
            }
            Expr::Error => "<error>".to_string(),
        }
    }
}

impl<'src, 'tok> SimpleFmt for SStmt<'src, 'tok> {
    fn simple_fmt(&self) -> String {
        self.value.simple_fmt()
    }
}

impl<'src, 'tok> SimpleFmt for SStmtInner<'src, 'tok> {
    fn simple_fmt(&self) -> String {
        match self {
            Stmt::Expr { expr } => expr.simple_fmt(),
            Stmt::Assign { lhs, rhs, op, .. } => {
                let op_str = if let Some(op) = op {
                    format!(" {}= ", format!("{:?}", op).to_lowercase())
                } else {
                    " = ".to_string()
                };
                format!("{}{}{}", lhs.simple_fmt(), op_str, rhs.simple_fmt())
            }
            Stmt::PatternAssign {
                lhs, rhs, modifier, ..
            } => {
                let mod_str = if let Some(modifier) = modifier {
                    format!("{} ", modifier.simple_fmt())
                } else {
                    String::new()
                };
                format!("{}{} = {}", mod_str, lhs.simple_fmt(), rhs.simple_fmt())
            }
            Stmt::Decl { modifier, names } => {
                let names_str = names
                    .iter()
                    .map(|(name, _)| name.simple_fmt())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{} {}", modifier.simple_fmt(), names_str)
            }
            Stmt::While { cond, body, .. } => {
                format!("while {}: {}", cond.simple_fmt(), body.simple_fmt())
            }
            Stmt::For {
                pattern,
                iter,
                body,
                ..
            } => {
                format!(
                    "for {} in {}: {}",
                    pattern.simple_fmt(),
                    iter.simple_fmt(),
                    body.simple_fmt()
                )
            }
            Stmt::Try {
                expr,
                cases,
                finally: finally_clause,
                ..
            } => {
                let cases_str = cases
                    .iter()
                    .map(|case| {
                        format!(
                            "except {} {} {}",
                            case.pattern.simple_fmt(),
                            case.guard
                                .as_ref()
                                .map(|x| " if ".to_string() + &x.1.simple_fmt())
                                .unwrap_or_default(),
                            case.body.simple_fmt()
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("; ");

                let finally_str = if let Some((_, finally_expr)) = finally_clause {
                    format!("; finally: {}", finally_expr.simple_fmt())
                } else {
                    String::new()
                };

                format!("try {}: {}{}", expr.simple_fmt(), cases_str, finally_str)
            }
            Stmt::Break { .. } => "break".to_string(),
            Stmt::Continue { .. } => "continue".to_string(),
            Stmt::Return { expr, .. } => {
                format!("return {}", expr.simple_fmt())
            }
            Stmt::Raise { expr, .. } => {
                if let Some(expr) = expr {
                    format!("raise {}", expr.simple_fmt())
                } else {
                    "raise".to_string()
                }
            }
            Stmt::Import { export, tree, .. } => {
                let export_str = if export.is_some() { "export " } else { "" };
                format!("{}import {}", export_str, tree.simple_fmt())
            }
        }
    }
}

impl<'src, 'tok> SimpleFmt for ImportTree<STree<'src, 'tok>> {
    fn simple_fmt(&self) -> String {
        let dots_str = self.dots.iter().map(|_| "..").collect::<Vec<_>>().join("");
        let trunk_str = self
            .trunk
            .iter()
            .map(|(ident, _)| ident.simple_fmt())
            .collect::<Vec<_>>()
            .join(".");
        let leaf_str = self.leaf.value.simple_fmt();

        if !trunk_str.is_empty() {
            format!("{}{}.{}", dots_str, trunk_str, leaf_str)
        } else {
            format!("{}{}", dots_str, leaf_str)
        }
    }
}

impl<'src, 'tok> SimpleFmt for ImportLeaf<STree<'src, 'tok>> {
    fn simple_fmt(&self) -> String {
        match self {
            ImportLeaf::Multi(listing) => listing.simple_fmt(),
            ImportLeaf::Single { name, alias } => {
                if let Some((_, alias_name)) = alias {
                    format!("{} as {}", name.simple_fmt(), alias_name.simple_fmt())
                } else {
                    name.simple_fmt()
                }
            }
            ImportLeaf::This { alias } => {
                if let Some((_, alias_name)) = alias {
                    format!("this as {}", alias_name.simple_fmt())
                } else {
                    "this".to_string()
                }
            }
            ImportLeaf::Star { .. } => "*".to_string(),
        }
    }
}

impl<'src, 'tok> SimpleFmt for ListItem<STree<'src, 'tok>> {
    fn simple_fmt(&self) -> String {
        match self {
            ListItem::Item { expr } => expr.simple_fmt(),
            ListItem::Spread { expr, .. } => format!("*{}", expr.simple_fmt()),
        }
    }
}

impl<'src, 'tok> SimpleFmt for CallItem<STree<'src, 'tok>> {
    fn simple_fmt(&self) -> String {
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

impl<'src, 'tok> SimpleFmt for MappingKey<STree<'src, 'tok>> {
    fn simple_fmt(&self) -> String {
        match self {
            MappingKey::Ident { token } => token.simple_fmt(),
            MappingKey::Literal { token } => token.simple_fmt(),
            MappingKey::Expr { key, .. } => format!("({})", key.simple_fmt()),
        }
    }
}

impl<'src, 'tok> SimpleFmt for MappingItem<STree<'src, 'tok>> {
    fn simple_fmt(&self) -> String {
        match self {
            MappingItem::Ident { ident } => ident.simple_fmt(),
            MappingItem::Item { key, value, .. } => {
                format!("{}: {}", key.simple_fmt(), value.simple_fmt())
            }
            MappingItem::Spread { expr, .. } => {
                format!("**{}", expr.simple_fmt())
            }
        }
    }
}

impl<'src, 'tok> SimpleFmt for MatchCase<STree<'src, 'tok>> {
    fn simple_fmt(&self) -> String {
        let guard_str = if let Some((_, guard_expr)) = &self.guard {
            format!(" if {}", guard_expr.simple_fmt())
        } else {
            String::new()
        };

        format!(
            "{}{} => {}",
            self.pattern.simple_fmt(),
            guard_str,
            self.body.simple_fmt()
        )
    }
}

impl<'src, 'tok> SimpleFmt for ArgDefItem<STree<'src, 'tok>> {
    fn simple_fmt(&self) -> String {
        match self {
            ArgDefItem::Arg { pattern, default } => {
                if let Some((_, default_expr)) = default {
                    format!("{}={}", pattern.simple_fmt(), default_expr.simple_fmt())
                } else {
                    pattern.simple_fmt()
                }
            }
            ArgDefItem::ArgSpread { name, .. } => {
                format!("*{}", name.simple_fmt())
            }
            ArgDefItem::KwargSpread { name, .. } => {
                format!("**{}", name.simple_fmt())
            }
        }
    }
}

impl<'src, 'tok> SimpleFmt for FmtExpr<STree<'src, 'tok>> {
    fn simple_fmt(&self) -> String {
        let expr_str = self.expr.simple_fmt();
        if let Some((_, fmt)) = &self.fmt {
            format!("{}!{}", expr_str, fmt.simple_fmt())
        } else {
            expr_str
        }
    }
}

impl<'src, 'tok> SimpleFmt for SPattern<'src, 'tok> {
    fn simple_fmt(&self) -> String {
        self.value.simple_fmt()
    }
}

impl<'src, 'tok> SimpleFmt for Pattern<STree<'src, 'tok>> {
    fn simple_fmt(&self) -> String {
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
            Pattern::Sequence { listing } => listing.simple_fmt(),
            Pattern::TupleSequence { listing } => listing.simple_fmt(),
            Pattern::Mapping { listing } => listing.simple_fmt(),
            Pattern::Class { expr, items, .. } => {
                format!("{}({})", expr.simple_fmt(), items.simple_fmt())
            }
            Pattern::Parenthesized { pattern, .. } => {
                format!("({})", pattern.simple_fmt())
            }
        }
    }
}

impl<'src, 'tok> SimpleFmt for PatternSequenceItem<STree<'src, 'tok>> {
    fn simple_fmt(&self) -> String {
        match self {
            PatternSequenceItem::Item { pattern } => pattern.simple_fmt(),
            PatternSequenceItem::Spread { name, .. } => {
                format!("*{}", name.simple_fmt())
            }
        }
    }
}

impl<'src, 'tok> SimpleFmt for PatternMappingItem<STree<'src, 'tok>> {
    fn simple_fmt(&self) -> String {
        match self {
            PatternMappingItem::Ident { name } => name.simple_fmt(),
            PatternMappingItem::Item { key, pattern, .. } => {
                format!("{}: {}", key.simple_fmt(), pattern.simple_fmt())
            }
            PatternMappingItem::Spread { name, .. } => {
                format!("**{}", name.simple_fmt())
            }
        }
    }
}

impl<'src, 'tok> SimpleFmt for PatternClassItem<STree<'src, 'tok>> {
    fn simple_fmt(&self) -> String {
        match self {
            PatternClassItem::Item { pattern } => pattern.simple_fmt(),
            PatternClassItem::Kw { name, pattern, .. } => {
                format!("{}={}", name.simple_fmt(), pattern.simple_fmt())
            }
        }
    }
}

impl<'src, 'tok, T> SListing<'src, 'tok, T> {
    pub fn simple_fmt(&self) -> String
    where
        T: SimpleFmt,
    {
        let (begin, items, end) = match self {
            SListing::Block {
                begin, items, end, ..
            }
            | SListing::Inline { begin, items, end } => {
                (begin.simple_fmt(), items, end.simple_fmt())
            }
            SListing::Open { items } => ("(".to_string(), items, ")".to_string()),
        };

        format!(
            "{}{}{}",
            begin,
            items
                .iter()
                .map(|item| format!(
                    "{}{} ",
                    item.item.simple_fmt(),
                    item.separator
                        .map(|x| x.simple_fmt())
                        .unwrap_or("".to_string())
                ))
                .collect::<Vec<_>>()
                .join(""),
            end
        )
    }
}

impl<'src> SimpleFmt for SToken<'src> {
    fn simple_fmt(&self) -> String {
        self.token.simple_fmt()
    }
}

impl<'src> SimpleFmt for Token<'src> {
    fn simple_fmt(&self) -> String {
        match self {
            Token::Ident(s) => s.to_string(),
            Token::Num(s) => s.to_string(),
            Token::Str(s) => format!("\"{}\"", s),
            Token::Bool(b) => b.to_string(),
            Token::None => "None".to_string(),
            Token::Symbol(s) => s.to_string(),
            Token::Kw(s) => s.to_string(),
            Token::FstrBegin(s) => py_escape_fstr(s),
            Token::FstrContinue(s) => py_escape_fstr(s),
            _ => format!("{:?}", self),
        }
    }
}
