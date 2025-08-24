// use std::{borrow::Cow, fmt::Display};

// use crate::ast::{FromIndirect, Indirect, IntoIndirect};
// use chumsky::span::SimpleSpan;

// use crate::lexer::SToken;

// #[derive(Debug, Clone)]
// pub enum ImportLeaf<'a> {
//     Multi(Vec<ImportTree<'a>>),
//     Single(SToken<'a>, Option<SToken<'a>>),
//     This(SToken<'a>, Option<SToken<'a>>),
//     Star,
// }

// #[derive(Debug, Clone)]
// pub struct ImportTree<'a> {
//     pub trunk: Vec<SToken<'a>>,
//     pub leaf: Spanned<ImportLeaf<'a>>,
//     pub level: usize,
// }

// #[derive(Debug, Clone)]
// pub enum Stmt<'a, TTree: Tree> {
//     // modifier, identifiers
//     Decl(SToken<'a>, Vec<SToken<'a>>),

//     // modifier, lhs, =, rhs
//     PatternAssign(Option<SToken<'a>>, TTree::Pattern, SToken<'a>, TTree::Expr),

//     // lhs, op, =, rhs
//     Assign(TTree::Expr, Option<SToken<'a>>, SToken<'a>, TTree::Expr),

//     // while, cond, :, body
//     While(SToken<'a>, TTree::Expr, SToken<'a>, TTree::Expr),

//     // for, cursor, in, iterable, :, body
//     For(
//         SToken<'a>,
//         TTree::Pattern,
//         SToken<'a>,
//         TTree::Expr,
//         SToken<'a>,
//         TTree::Expr,
//     ),

//     // export, import, tree
//     Import(Option<SToken<'a>>, SToken<'a>, ImportTree<'a>),

//     // try, expr, :, cases, (finally, :, expr)?
//     Try(
//         SToken<'a>,
//         TTree::Expr,
//         SToken<'a>,
//         Vec<MatchCase<TTree>>,
//         Option<(SToken<'a>, SToken<'a>, TTree::Expr)>,
//     ),

//     Break(SToken<'a>),
//     Continue(SToken<'a>),
//     Return(SToken<'a>, TTree::Expr),
//     Raise(SToken<'a>, Option<TTree::Expr>),
//     Expr(TTree::Expr),
// }

// #[derive(Debug, Clone)]
// pub struct FmtExpr<TTree: Tree> {
//     pub expr: TTree::Expr,
//     pub fmt: Option<TTree::Expr>,
// }

// #[derive(Debug, Clone)]
// pub enum ListItem<TTree: Tree> {
//     Item(TTree::Expr),
//     Spread(TTree::Expr),
// }

// #[derive(Debug, Clone)]
// pub enum MappingItem<TTree: Tree> {
//     Ident(TTree::Expr),
//     Item(TTree::Expr, TTree::Expr),
//     Spread(TTree::Expr),
// }

// #[derive(Debug, Clone)]
// pub enum CallItem<'a, TTree: Tree> {
//     Arg(TTree::Expr),
//     Kwarg(SIdent<'a>, TTree::Expr),
//     ArgSpread(TTree::Expr),
//     KwargSpread(TTree::Expr),
// }

// #[derive(Debug, Clone)]
// pub enum ArgDefItem<'a, TTree: Tree> {
//     Arg(TTree::Pattern, Option<TTree::Expr>),
//     ArgSpread(SIdent<'a>),
//     KwargSpread(SIdent<'a>),
// }

// #[derive(Debug, Clone)]
// pub struct MatchCase<TTree: Tree> {
//     pub pattern: Option<TTree::Pattern>,
//     pub guard: Option<TTree::Expr>,
//     pub body: TTree::Expr,
// }

// pub trait Tree {
//     type Expr;
//     type Pattern;
//     type Stmt;
// }

// #[derive(Debug, Clone)]
// pub enum Expr<'a, TTree: Tree> {
//     Literal(SLiteral<'a>),
//     Ident(SIdent<'a>),
//     Tuple(Vec<ListItem<TTree>>),
//     List(Vec<ListItem<TTree>>),
//     Mapping(Vec<MappingItem<TTree>>),
//     Slice(
//         Option<TTree::Expr>,
//         Option<TTree::Expr>,
//         Option<TTree::Expr>,
//     ),

//     Unary(UnaryOp, TTree::Expr),
//     Binary(BinaryOp, TTree::Expr, TTree::Expr),

//     Await(TTree::Expr),
//     Yield(TTree::Expr),
//     YieldFrom(TTree::Expr),
//     Memo(TTree::Expr, bool),

//     If(TTree::Expr, TTree::Expr, Option<TTree::Expr>),
//     Match(TTree::Expr, Vec<MatchCase<TTree>>),
//     Matches(TTree::Expr, TTree::Pattern),
//     Class(Vec<CallItem<'a, TTree>>, TTree::Expr),

//     With(TTree::Pattern, TTree::Expr, TTree::Expr),

//     Call(TTree::Expr, Vec<CallItem<'a, TTree>>),
//     Subscript(TTree::Expr, Vec<ListItem<TTree>>),
//     RawAttribute(TTree::Expr, SIdent<'a>),
//     ScopedAttribute(TTree::Expr, TTree::Expr),
//     Attribute(TTree::Expr, SIdent<'a>),

//     MappedCall(TTree::Expr, Vec<CallItem<'a, TTree>>),
//     MappedSubscript(TTree::Expr, Vec<ListItem<TTree>>),
//     MappedRawAttribute(TTree::Expr, SIdent<'a>),
//     MappedScopedAttribute(TTree::Expr, TTree::Expr),
//     MappedAttribute(TTree::Expr, SIdent<'a>),

//     Checked(TTree::Expr, Option<TTree::Pattern>),

//     Block(Vec<TTree::Stmt>),

//     Fn(Vec<ArgDefItem<'a, TTree>>, TTree::Expr),
//     Fstr(Spanned<String>, Vec<(FmtExpr<TTree>, Spanned<String>)>),

//     // these are removed during desugaring
//     Decorated(TTree::Expr, TTree::Expr),
//     Placeholder,
// }

// // Patterns

// #[derive(Debug, Clone)]
// pub enum PatternSequenceItem<'a, TTree: Tree> {
//     Item(TTree::Pattern),
//     Spread(Option<SIdent<'a>>),
// }

// #[derive(Debug, Clone)]
// pub enum PatternMappingItem<'a, TTree: Tree> {
//     Ident(SIdent<'a>),
//     Item(TTree::Expr, TTree::Pattern),
//     Spread(Option<SIdent<'a>>),
// }

// #[derive(Debug, Clone)]
// pub enum PatternClassItem<'a, TTree: Tree> {
//     Item(TTree::Pattern),
//     Kw(SIdent<'a>, TTree::Pattern),
// }

// #[derive(Debug, Clone)]
// pub enum Pattern<'a, TTree: Tree> {
//     Capture(Option<SIdent<'a>>),
//     Value(TTree::Expr),
//     As(TTree::Pattern, SIdent<'a>),
//     Or(Vec<TTree::Pattern>),
//     Literal(SLiteral<'a>),
//     Sequence(Vec<PatternSequenceItem<'a, TTree>>),
//     Mapping(Vec<PatternMappingItem<'a, TTree>>),
//     Class(TTree::Expr, Vec<PatternClassItem<'a, TTree>>),
// }

// // Spanned types

// #[derive(Debug, Clone)]
// pub struct STree<'src> {
//     phantom: std::marker::PhantomData<&'src ()>,
// }

// impl<'src> Tree for STree<'src> {
//     type Expr = Indirect<SExpr<'src>>;
//     type Pattern = Indirect<SPattern<'src>>;
//     type Stmt = Indirect<SStmt<'src>>;
// }

// pub type Span = SimpleSpan<usize, ()>;

// #[derive(Debug, Clone)]
// pub struct Spanned<T> {
//     pub span: Span,
//     pub value: T,
// }

// pub trait Spannable<T> {
//     fn spanned(self, span: Span) -> Spanned<T>;
// }

// impl<T> Spannable<T> for T {
//     fn spanned(self, span: Span) -> Spanned<T> {
//         Spanned { span, value: self }
//     }
// }

// pub type SPatternInner<'a> = Pattern<'a, STree<'a>>;

// #[derive(Debug, Clone)]
// pub struct SPattern<'a> {
//     pub value: SPatternInner<'a>,
//     pub span: Span,
//     pub leading_trivia: Vec<crate::lexer::Trivium<'a>>,
//     pub trailing_trivia: Vec<crate::lexer::Trivium<'a>>,
// }

// impl<'a> SPatternInner<'a> {
//     pub fn spanned(self, span: Span) -> SPattern<'a> {
//         SPattern {
//             value: self,
//             span,
//             leading_trivia: Vec::new(),
//             trailing_trivia: Vec::new(),
//         }
//     }

//     pub fn spanned_with_trivia(
//         self,
//         span: Span,
//         leading_trivia: Vec<crate::lexer::Trivium<'a>>,
//         trailing_trivia: Vec<crate::lexer::Trivium<'a>>,
//     ) -> SPattern<'a> {
//         SPattern {
//             value: self,
//             span,
//             leading_trivia,
//             trailing_trivia,
//         }
//     }
// }

// pub type SExprInner<'a> = Expr<'a, STree<'a>>;

// #[derive(Debug, Clone)]
// pub struct SExpr<'a> {
//     pub value: SExprInner<'a>,
//     pub span: Span,
//     pub leading_trivia: Vec<crate::lexer::Trivium<'a>>,
//     pub trailing_trivia: Vec<crate::lexer::Trivium<'a>>,
// }

// impl<'a> SExprInner<'a> {
//     pub fn spanned(self, span: Span) -> SExpr<'a> {
//         SExpr {
//             value: self,
//             span,
//             leading_trivia: Vec::new(),
//             trailing_trivia: Vec::new(),
//         }
//     }

//     pub fn spanned_with_trivia(
//         self,
//         span: Span,
//         leading_trivia: Vec<crate::lexer::Trivium<'a>>,
//         trailing_trivia: Vec<crate::lexer::Trivium<'a>>,
//     ) -> SExpr<'a> {
//         SExpr {
//             value: self,
//             span,
//             leading_trivia,
//             trailing_trivia,
//         }
//     }
// }

// pub type SStmtInner<'a> = Stmt<'a, STree<'a>>;

// impl<'a> SStmtInner<'a> {
//     pub fn spanned(self, span: Span) -> SStmt<'a> {
//         SStmt {
//             value: self,
//             span,
//             leading_trivia: Vec::new(),
//             trailing_trivia: Vec::new(),
//         }
//     }

//     pub fn spanned_with_trivia(
//         self,
//         span: Span,
//         leading_trivia: Vec<crate::lexer::Trivium<'a>>,
//         trailing_trivia: Vec<crate::lexer::Trivium<'a>>,
//     ) -> SStmt<'a> {
//         SStmt {
//             value: self,
//             span,
//             leading_trivia,
//             trailing_trivia,
//         }
//     }
// }

// #[derive(Debug, Clone)]
// pub struct SStmt<'a> {
//     pub value: SStmtInner<'a>,
//     pub span: Span,
//     pub leading_trivia: Vec<crate::lexer::Trivium<'a>>,
//     pub trailing_trivia: Vec<crate::lexer::Trivium<'a>>,
// }

// pub type SListItem<'a> = ListItem<STree<'a>>;
// pub type SMappingItem<'a> = MappingItem<STree<'a>>;
// pub type SMatchCase<'a> = MatchCase<STree<'a>>;
// pub type SCallItem<'a> = CallItem<'a, STree<'a>>;
// pub type SArgDefItem<'a> = ArgDefItem<'a, STree<'a>>;
// pub type SFmtExpr<'a> = FmtExpr<STree<'a>>;
