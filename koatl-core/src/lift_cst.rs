use std::borrow::Cow;

use crate::ast;
use crate::ast::{Indirect, IntoIndirect};
use koatl_parser::cst::{Spannable, Spanned};
use koatl_parser::lexer::SToken;
use koatl_parser::{Span, Token, cst};

trait STokenExt<'src> {
    fn lift_as_ident(&self) -> Spanned<ast::Ident<'src>>;
    fn lift_as_literal(&self) -> Spanned<ast::Literal<'src>>;
    fn lift_as_decl_modifier(&self) -> ast::DeclType;
    fn lift_as_capture(&self) -> Option<Spanned<ast::Ident<'src>>>;
}

fn lift_fstr_fmt<'src, 'tok>(
    fmt: &cst::FmtSpec<cst::STree<'src, 'tok>>,
) -> Indirect<ast::SExpr<'src>> {
    // TODO the span here is wrong
    lift_fstr(&fmt.head, &fmt.parts)
        .spanned(Span::new(fmt.head.span.start..fmt.head.span.end))
        .indirect()
}

fn lift_fstr<'src, 'tok>(
    begin: &SToken<'src>,
    parts: &[(cst::FmtExpr<cst::STree<'src, 'tok>>, &'tok SToken<'src>)],
) -> ast::Expr<'src, ast::STree<'src>> {
    let begin_str = match &begin.token {
        Token::FstrInner(_, s) => s.clone(),
        _ => panic!("Expected FstrInner token"),
    };
    let fmt_parts = parts
        .iter()
        .map(|(fmt_expr, cont)| {
            let cont_str = match &cont.token {
                Token::FstrInner(_, s) => s.clone(),
                _ => panic!("Expected FstrContinue token"),
            };

            (
                ast::FmtExpr {
                    expr: fmt_expr.expr.lift(),
                    fmt: fmt_expr.fmt.as_ref().map(lift_fstr_fmt),
                },
                cont_str.spanned(cont.span),
            )
        })
        .collect();

    ast::Expr::Fstr(begin_str.spanned(begin.span), fmt_parts)
}

impl<'src> STokenExt<'src> for SToken<'src> {
    fn lift_as_ident(&self) -> Spanned<ast::Ident<'src>> {
        match &self.token {
            Token::Ident(s) => ast::Ident(Cow::Borrowed(s)).spanned(self.span),
            _ => panic!("Expected identifier token, got {:?}", self.token),
        }
    }

    fn lift_as_capture(&self) -> Option<Spanned<ast::Ident<'src>>> {
        match &self.token {
            Token::Ident(s) => {
                if *s == "_" {
                    None
                } else {
                    Some(ast::Ident(Cow::Borrowed(s)).spanned(self.span))
                }
            }
            _ => panic!("Expected identifier token, got {:?}", self.token),
        }
    }

    fn lift_as_literal(&self) -> Spanned<ast::Literal<'src>> {
        match &self.token {
            Token::Int(n) => ast::Literal::Int(Cow::Borrowed(n)),
            Token::IntHex(n) => ast::Literal::IntHex(Cow::Borrowed(n)),
            Token::IntOct(n) => ast::Literal::IntOct(Cow::Borrowed(n)),
            Token::IntBin(n) => ast::Literal::IntBin(Cow::Borrowed(n)),
            Token::Float(n) => ast::Literal::Float(Cow::Borrowed(n)),
            Token::Str(_, s) => ast::Literal::Str(s.clone().into()),
            Token::Bool(b) => ast::Literal::Bool(*b),
            Token::None => ast::Literal::None,
            _ => panic!("Expected literal token, got {:?}", self.token),
        }
        .spanned(self.span)
    }

    fn lift_as_decl_modifier(&self) -> ast::DeclType {
        match &self.token {
            Token::Kw(k) => match k.as_ref() {
                "let" => ast::DeclType::Let,
                "const" => ast::DeclType::Const,
                "global" => ast::DeclType::Global,
                "export" => ast::DeclType::Export,
                _ => panic!("Unknown declaration modifier: {}", k),
            },
            _ => panic!("Expected keyword token for declaration modifier"),
        }
    }
}

trait Lift<T> {
    fn lift(&self) -> T;
}

impl<'src, 'tok, T, U> Lift<Vec<U>> for cst::SListing<'src, 'tok, T>
where
    T: Lift<U>,
{
    fn lift(&self) -> Vec<U> {
        match self {
            cst::Listing::Block { items, .. } | cst::Listing::Inline { items, .. } => {
                items.iter().map(|item| item.item.lift()).collect()
            }
        }
    }
}

impl<'src, 'tok> Lift<ast::ListItem<ast::STree<'src>>> for cst::ListItem<cst::STree<'src, 'tok>> {
    fn lift(&self) -> ast::ListItem<ast::STree<'src>> {
        match self {
            cst::ListItem::Item { expr } => ast::ListItem::Item(expr.lift()),
            cst::ListItem::Spread { expr, .. } => ast::ListItem::Spread(expr.lift()),
        }
    }
}

impl<'src, 'tok> Lift<ast::CallItem<'src, ast::STree<'src>>>
    for cst::CallItem<cst::STree<'src, 'tok>>
{
    fn lift(&self) -> ast::CallItem<'src, ast::STree<'src>> {
        match self {
            cst::CallItem::Arg { expr } => ast::CallItem::Arg(expr.lift()),
            cst::CallItem::Kwarg { name, expr, .. } => {
                ast::CallItem::Kwarg(name.lift_as_ident(), expr.lift())
            }
            cst::CallItem::ArgSpread { expr, .. } => ast::CallItem::ArgSpread(expr.lift()),
            cst::CallItem::KwargSpread { expr, .. } => ast::CallItem::KwargSpread(expr.lift()),
        }
    }
}

impl<'src, 'tok> Lift<ast::MappingItem<ast::STree<'src>>>
    for cst::MappingItem<cst::STree<'src, 'tok>>
{
    fn lift(&self) -> ast::MappingItem<ast::STree<'src>> {
        match self {
            cst::MappingItem::Ident { ident } => ast::MappingItem::Ident(
                ast::Expr::Ident(ident.lift_as_ident())
                    .spanned(ident.span)
                    .indirect(),
            ),
            cst::MappingItem::Item { key, value, .. } => {
                let key_expr = match &key.value {
                    cst::MappingKey::Unit { .. } => ast::Expr::Tuple(vec![]),
                    cst::MappingKey::ParenthesizedBlock { body, .. } => body.lift().value,
                    cst::MappingKey::Ident { token } => ast::Expr::Literal(
                        ast::Literal::Str(token.lift_as_ident().value.0).spanned(token.span),
                    ),
                    cst::MappingKey::Literal { token } => {
                        ast::Expr::Literal(token.lift_as_literal())
                    }
                    cst::MappingKey::Fstr { head, parts, .. } => lift_fstr(head, parts),
                    cst::MappingKey::Expr { key, .. } => key.lift().value,
                };
                ast::MappingItem::Item(key_expr.spanned(key.span).indirect(), value.lift())
            }
            cst::MappingItem::Spread { expr, .. } => ast::MappingItem::Spread(expr.lift()),
        }
    }
}

impl<'src, 'tok> Lift<Indirect<ast::SExpr<'src>>> for cst::SExpr<'src, 'tok> {
    fn lift(&self) -> Indirect<ast::SExpr<'src>> {
        let expr = match &self.value {
            cst::Expr::Literal { token } => ast::Expr::Literal(token.lift_as_literal()),
            cst::Expr::Ident { token } => ast::Expr::Ident(token.lift_as_ident()),
            cst::Expr::Placeholder { .. } => ast::Expr::Placeholder,
            cst::Expr::Binary {
                lhs, rhs, op_kind, ..
            } => ast::Expr::Binary(*op_kind, lhs.lift(), rhs.lift()),
            cst::Expr::Unary { op_kind, expr, .. } => ast::Expr::Unary(*op_kind, expr.lift()),
            cst::Expr::List { listing } => ast::Expr::List(listing.lift()),
            cst::Expr::Tuple { kind } => {
                let items = match kind {
                    cst::TupleKind::Unit(..) => vec![],
                    cst::TupleKind::Listing(items) => {
                        items.iter().map(|item| item.item.lift()).collect()
                    }
                };
                ast::Expr::Tuple(items)
            }
            cst::Expr::Try {
                body,
                cases,
                finally,
                ..
            } => {
                let exception_cases: Vec<ast::MatchCase<ast::STree<'src>>> =
                    cases.iter().map(|case| case.lift()).collect();
                let finally_expr = finally.as_ref().map(|(_, expr)| expr.lift());
                ast::Expr::Try(body.lift(), exception_cases, finally_expr)
            }
            cst::Expr::If {
                cond,
                body,
                else_clause,
                ..
            }
            | cst::Expr::ClassicIf {
                cond,
                body,
                else_clause,
                ..
            } => {
                let else_expr = else_clause.as_ref().map(|(_, expr)| expr.lift());
                ast::Expr::If(cond.lift(), body.lift(), else_expr)
            }
            cst::Expr::Parenthesized { expr, .. } => return expr.lift(),
            cst::Expr::Slice {
                start, stop, step, ..
            } => ast::Expr::Slice(
                start.as_ref().map(|e| e.lift()),
                stop.as_ref().map(|e| e.lift()),
                step.as_ref().map(|e| e.lift()),
            ),
            cst::Expr::ParenthesizedBlock { body, .. } => body.lift().value,
            cst::Expr::Mapping { listing } => ast::Expr::Mapping(listing.lift()),
            cst::Expr::Await { expr, .. } => ast::Expr::Await(expr.lift()),
            cst::Expr::Yield { expr, from_kw, .. } => {
                if from_kw.is_none() {
                    ast::Expr::Yield(expr.lift())
                } else {
                    ast::Expr::YieldFrom(expr.lift())
                }
            }
            cst::Expr::Match {
                scrutinee, cases, ..
            }
            | cst::Expr::ClassicMatch {
                scrutinee, cases, ..
            } => {
                let cases_list = cases.iter().map(|case| case.lift()).collect();
                ast::Expr::Match(scrutinee.lift(), cases_list)
            }
            cst::Expr::Matches {
                lhs,
                pattern,
                not_kw,
                ..
            } => {
                if not_kw.is_none() {
                    ast::Expr::Matches(lhs.lift(), pattern.lift())
                } else {
                    ast::Expr::Unary(
                        ast::UnaryOp::Not,
                        ast::Expr::Matches(lhs.lift(), pattern.lift())
                            .spanned(self.span)
                            .indirect(),
                    )
                }
            }
            cst::Expr::Fn { arg, body, .. } => {
                ast::Expr::Fn(vec![ast::ArgDefItem::Arg(arg.lift(), None)], body.lift())
            }
            cst::Expr::ParenthesizedFn { args, body, .. } => {
                let args_list = match args {
                    cst::Listing::Block { items, .. } | cst::Listing::Inline { items, .. } => {
                        items.iter().map(|item| item.item.lift()).collect()
                    }
                };
                ast::Expr::Fn(args_list, body.lift())
            }
            cst::Expr::Fstr { head, parts, .. } => lift_fstr(head, parts),
            cst::Expr::Decorated {
                expr, decorator, ..
            } => ast::Expr::Decorated(expr.lift(), decorator.lift()),
            cst::Expr::Memo { async_kw, body, .. } => {
                ast::Expr::Memo(body.lift(), async_kw.is_some())
            }
            cst::Expr::Class {
                class_kw: _,
                args,
                body,
            } => {
                let class_args = args.as_ref().map_or(vec![], |args| args.lift());
                ast::Expr::Class(class_args, body.lift())
            }
            cst::Expr::With {
                pattern,
                value,
                body,
                ..
            } => ast::Expr::With(pattern.lift(), value.lift(), body.lift()),
            cst::Expr::Subscript {
                expr,
                indices,
                question,
            } => {
                if question.is_some() {
                    ast::Expr::MappedSubscript(expr.lift(), indices.lift())
                } else {
                    ast::Expr::Subscript(expr.lift(), indices.lift())
                }
            }
            cst::Expr::Attribute {
                expr,
                attr,
                question,
                ..
            } => {
                if question.is_some() {
                    ast::Expr::MappedAttribute(expr.lift(), attr.lift_as_ident())
                } else {
                    ast::Expr::Attribute(expr.lift(), attr.lift_as_ident())
                }
            }
            cst::Expr::MaybeAttribute {
                expr,
                attr,
                question,
                ..
            } => {
                if question.is_some() {
                    ast::Expr::MappedMaybeAttribute(expr.lift(), attr.lift_as_ident())
                } else {
                    ast::Expr::MaybeAttribute(expr.lift(), attr.lift_as_ident())
                }
            }
            cst::Expr::Call {
                expr,
                args,
                question,
            } => {
                if question.is_some() {
                    ast::Expr::MappedCall(expr.lift(), args.lift())
                } else {
                    ast::Expr::Call(expr.lift(), args.lift())
                }
            }
            cst::Expr::MethodCall {
                expr,
                question,
                method,
                args,
                ..
            } => {
                if question.is_some() {
                    ast::Expr::MappedCall(
                        ast::Expr::MappedAttribute(expr.lift(), method.lift_as_ident())
                            .spanned(method.span)
                            .indirect(),
                        args.lift(),
                    )
                } else {
                    ast::Expr::Call(
                        ast::Expr::Attribute(expr.lift(), method.lift_as_ident())
                            .spanned(method.span)
                            .indirect(),
                        args.lift(),
                    )
                }
            }
            cst::Expr::RawAttribute {
                expr,
                question,
                attr,
                ..
            } => {
                if question.is_some() {
                    ast::Expr::MappedRawAttribute(expr.lift(), attr.lift_as_ident())
                } else {
                    ast::Expr::RawAttribute(expr.lift(), attr.lift_as_ident())
                }
            }
            cst::Expr::ScopedAttribute {
                expr,
                question,
                rhs,
                ..
            } => {
                if question.is_some() {
                    ast::Expr::MappedScopedAttribute(expr.lift(), rhs.lift())
                } else {
                    ast::Expr::ScopedAttribute(expr.lift(), rhs.lift())
                }
            }
            cst::Expr::Checked { expr, pattern, .. } => {
                ast::Expr::Checked(expr.lift(), pattern.as_ref().map(|p| p.lift()))
            }
        };
        expr.spanned(self.span).indirect()
    }
}

impl<'src, 'tok> Lift<Indirect<ast::SPattern<'src>>> for cst::SPattern<'src, 'tok> {
    fn lift(&self) -> Indirect<ast::SPattern<'src>> {
        let pattern = match &self.value {
            cst::Pattern::Capture { name } => ast::Pattern::Capture(name.lift_as_capture()),
            cst::Pattern::Value { expr, .. } => ast::Pattern::Value(expr.lift()),
            cst::Pattern::As { pattern, name, .. } => {
                ast::Pattern::As(pattern.lift(), name.lift_as_capture())
            }
            cst::Pattern::Or { head, rest } => {
                let mut patterns = vec![head.lift()];
                patterns.extend(rest.iter().map(|(_, pattern)| pattern.lift()));
                ast::Pattern::Or(patterns)
            }
            cst::Pattern::Literal { token } => ast::Pattern::Literal(token.lift_as_literal()),
            cst::Pattern::Sequence { listing } => {
                let items = match listing {
                    cst::Listing::Block { items, .. } | cst::Listing::Inline { items, .. } => {
                        items.iter().map(|item| item.item.lift()).collect()
                    }
                };
                ast::Pattern::Sequence(items)
            }
            cst::Pattern::TupleSequence { kind } => {
                let items = match kind {
                    cst::PatternTupleSequenceKind::Unit { .. } => vec![],
                    cst::PatternTupleSequenceKind::Listing(items) => {
                        items.iter().map(|item| item.item.lift()).collect()
                    }
                };
                ast::Pattern::Sequence(items)
            }
            cst::Pattern::Mapping { listing } => {
                let items = match listing {
                    cst::Listing::Block { items, .. } | cst::Listing::Inline { items, .. } => {
                        items.iter().map(|item| item.item.lift()).collect()
                    }
                };
                ast::Pattern::Mapping(items)
            }
            cst::Pattern::Class { expr, items, .. } => {
                let class_items = match items {
                    cst::Listing::Block { items, .. } | cst::Listing::Inline { items, .. } => {
                        items.iter().map(|item| item.item.lift()).collect()
                    }
                };
                ast::Pattern::Class(expr.lift(), class_items)
            }
            cst::Pattern::Parenthesized { pattern, .. } => return pattern.lift(),
        };
        pattern.spanned(self.span).indirect()
    }
}

impl<'src, 'tok> Lift<Indirect<ast::SStmt<'src>>> for cst::SStmt<'src, 'tok> {
    fn lift(&self) -> Indirect<ast::SStmt<'src>> {
        let stmt = match &self.value {
            cst::Stmt::Expr { expr } => ast::Stmt::Expr(expr.lift()),
            cst::Stmt::Assign { lhs, rhs, op, .. } => {
                ast::Stmt::Assign(lhs.lift(), rhs.lift(), op.clone())
            }
            cst::Stmt::PatternAssign {
                lhs, rhs, modifier, ..
            } => {
                let mod_kind = modifier.as_ref().map(|token| token.lift_as_decl_modifier());
                ast::Stmt::PatternAssign(lhs.lift(), rhs.lift(), mod_kind)
            }
            cst::Stmt::Decl { modifier, names } => {
                let name_list = names.iter().map(|(name, _)| name.lift_as_ident()).collect();
                ast::Stmt::Decl(name_list, modifier.lift_as_decl_modifier())
            }
            cst::Stmt::While { cond, body, .. } => ast::Stmt::While(cond.lift(), body.lift()),
            cst::Stmt::For {
                pattern,
                iter,
                body,
                ..
            } => ast::Stmt::For(pattern.lift(), iter.lift(), body.lift()),
            cst::Stmt::Break { .. } => ast::Stmt::Break,
            cst::Stmt::Continue { .. } => ast::Stmt::Continue,
            cst::Stmt::Return { expr, .. } => ast::Stmt::Return(expr.lift()),
            cst::Stmt::Raise { expr, .. } => ast::Stmt::Raise(expr.as_ref().map(|e| e.lift())),
            cst::Stmt::Import { tree, export, .. } => {
                ast::Stmt::Import(tree.lift(), export.is_some())
            }
            cst::Stmt::Error { .. } => ast::Stmt::Expr(
                ast::Expr::Ident(ast::Ident("_ERROR_".into()).spanned(self.span))
                    .spanned(self.span)
                    .indirect(),
            ),
        };
        stmt.spanned(self.span).indirect()
    }
}

impl<'src, 'tok> Lift<ast::MatchCase<ast::STree<'src>>>
    for cst::ExceptCase<cst::STree<'src, 'tok>>
{
    fn lift(&self) -> ast::MatchCase<ast::STree<'src>> {
        ast::MatchCase {
            pattern: self.pattern.lift(),
            guard: self.guard.as_ref().map(|(_, expr)| expr.lift()),
            body: self.body.lift(),
        }
    }
}

impl<'src, 'tok> Lift<Indirect<ast::SExpr<'src>>> for cst::SStmts<'src, 'tok> {
    fn lift(&self) -> Indirect<ast::SExpr<'src>> {
        let stmts = self
            .value
            .iter()
            .map(|stmt: &Box<cst::SStmt>| -> Indirect<ast::SStmt> { (*stmt).lift() })
            .collect();

        ast::Expr::Block(stmts).spanned(self.span).indirect()
    }
}

impl<'src, 'tok> Lift<Indirect<ast::SExpr<'src>>> for cst::SStmt<'src, 'tok> {
    fn lift(&self) -> Indirect<ast::SExpr<'src>> {
        ast::Expr::Block(vec![self.lift()])
            .spanned(self.span)
            .indirect()
    }
}

impl<'src, 'tok> Lift<Indirect<ast::SExpr<'src>>> for cst::InducedBlock<cst::STree<'src, 'tok>> {
    fn lift(&self) -> Indirect<ast::SExpr<'src>> {
        match self {
            cst::InducedBlock::Block { body, .. } => body.lift(),
            cst::InducedBlock::Inline { stmt, .. } => stmt.lift(),
        }
    }
}

impl<'src, 'tok> Lift<ast::MatchCase<ast::STree<'src>>> for cst::MatchCase<cst::STree<'src, 'tok>> {
    fn lift(&self) -> ast::MatchCase<ast::STree<'src>> {
        ast::MatchCase {
            pattern: self.pattern.lift(),
            guard: self.guard.as_ref().map(|(_, expr)| expr.lift()),
            body: self.body.lift(),
        }
    }
}

impl<'src, 'tok> Lift<ast::ArgDefItem<'src, ast::STree<'src>>>
    for cst::ArgDefItem<cst::STree<'src, 'tok>>
{
    fn lift(&self) -> ast::ArgDefItem<'src, ast::STree<'src>> {
        match self {
            cst::ArgDefItem::Arg { pattern, default } => ast::ArgDefItem::Arg(
                pattern.lift(),
                default.as_ref().map(|(_, expr)| expr.lift()),
            ),
            cst::ArgDefItem::ArgSpread { name, .. } => {
                ast::ArgDefItem::ArgSpread(name.lift_as_ident())
            }
            cst::ArgDefItem::KwargSpread { name, .. } => {
                ast::ArgDefItem::KwargSpread(name.lift_as_ident())
            }
            cst::ArgDefItem::PosOnlyMarker { .. } => ast::ArgDefItem::PosOnlyMarker,
            cst::ArgDefItem::KwOnlyMarker { .. } => ast::ArgDefItem::KwOnlyMarker,
        }
    }
}

// Helper functions for lifting complex structures

impl<'src, 'tok> Lift<ast::PatternSequenceItem<'src, ast::STree<'src>>>
    for cst::PatternSequenceItem<cst::STree<'src, 'tok>>
{
    fn lift(&self) -> ast::PatternSequenceItem<'src, ast::STree<'src>> {
        match self {
            cst::PatternSequenceItem::Item { pattern } => {
                ast::PatternSequenceItem::Item(pattern.lift())
            }
            cst::PatternSequenceItem::Spread { name, .. } => {
                ast::PatternSequenceItem::Spread(name.lift_as_capture())
            }
        }
    }
}

impl<'src, 'tok> Lift<ast::PatternMappingItem<'src, ast::STree<'src>>>
    for cst::PatternMappingItem<cst::STree<'src, 'tok>>
{
    fn lift(&self) -> ast::PatternMappingItem<'src, ast::STree<'src>> {
        match self {
            cst::PatternMappingItem::Ident { name } => {
                ast::PatternMappingItem::Ident(name.lift_as_ident())
            }
            cst::PatternMappingItem::Item { key, pattern, .. } => {
                let expr = match &key.value {
                    cst::PatternMappingKey::Unit { .. } => ast::Expr::Tuple(vec![]),
                    cst::PatternMappingKey::Ident { token } => ast::Expr::Literal(
                        ast::Literal::Str(token.lift_as_ident().value.0).spanned(token.span),
                    ),
                    cst::PatternMappingKey::Literal { token } => {
                        ast::Expr::Literal(token.lift_as_literal())
                    }
                    cst::PatternMappingKey::Expr { key, .. } => key.lift().value,
                };

                ast::PatternMappingItem::Item(expr.spanned(key.span).indirect(), pattern.lift())
            }
            cst::PatternMappingItem::Spread { name, .. } => {
                ast::PatternMappingItem::Spread(name.lift_as_capture())
            }
        }
    }
}

impl<'src, 'tok> Lift<ast::PatternClassItem<'src, ast::STree<'src>>>
    for cst::PatternClassItem<cst::STree<'src, 'tok>>
{
    fn lift(&self) -> ast::PatternClassItem<'src, ast::STree<'src>> {
        match self {
            cst::PatternClassItem::Item { pattern } => ast::PatternClassItem::Item(pattern.lift()),
            cst::PatternClassItem::Kw { name, pattern, .. } => {
                ast::PatternClassItem::Kw(name.lift_as_ident(), pattern.lift())
            }
        }
    }
}

impl<'src, 'tok> Lift<ast::ImportTree<'src>> for cst::ImportTree<cst::STree<'src, 'tok>> {
    fn lift(&self) -> ast::ImportTree<'src> {
        ast::ImportTree {
            level: self.dots.value.iter().map(|(_, x)| x).sum(),
            trunk: self
                .trunk
                .iter()
                .map(|(ident, _)| ident.lift_as_ident())
                .collect(),
            leaf: self.leaf.value.lift().spanned(self.leaf.span),
        }
    }
}

impl<'src, 'tok> Lift<ast::ImportLeaf<'src>> for cst::ImportLeaf<cst::STree<'src, 'tok>> {
    fn lift(&self) -> ast::ImportLeaf<'src> {
        match self {
            cst::ImportLeaf::Multi(listing) => ast::ImportLeaf::Multi(listing.lift()),
            cst::ImportLeaf::Single { name, alias } => ast::ImportLeaf::Single(
                name.lift_as_ident(),
                alias.as_ref().map(|(_, name)| name.lift_as_ident()),
            ),
            cst::ImportLeaf::This { alias, .. } => {
                ast::ImportLeaf::This(alias.as_ref().map(|(_, name)| name.lift_as_ident()))
            }
            cst::ImportLeaf::Star { .. } => ast::ImportLeaf::Star,
        }
    }
}

pub fn lift_cst<'src, 'tok>(cst: &cst::SStmts<'src, 'tok>) -> Indirect<ast::SExpr<'src>> {
    cst.lift()
}
