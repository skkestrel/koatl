use std::collections::HashMap;

use crate::ast::*;

use crate::{
    resolve_scopes::ResolveState,
    types::Type,
    util::{RefHash, TlResult},
};

#[allow(dead_code)]
pub struct InferenceCtx<'src, 'ast> {
    resolve_state: &'ast ResolveState<'src>,
    pub types: HashMap<RefHash, Type>,
}

trait SStmtExt<'src, 'ast> {
    fn traverse(&'ast self, ctx: &mut InferenceCtx<'src, '_>) -> Type;
}

impl<'src, 'ast> SStmtExt<'src, 'ast> for Indirect<SStmt<'src>> {
    fn traverse(&'ast self, ctx: &mut InferenceCtx<'src, '_>) -> Type {
        match &self.value {
            Stmt::Expr(expr) => expr.traverse(ctx),
            Stmt::PatternAssign(_lhs, rhs, _decl_type) => {
                rhs.traverse(ctx);
                Type::NoReturn
            }
            Stmt::Assign(lhs, rhs, _op) => {
                lhs.traverse(ctx);
                rhs.traverse(ctx);
                Type::NoReturn
            }
            Stmt::While(cond, body) => {
                cond.traverse(ctx);
                body.traverse(ctx);
                Type::NoReturn
            }
            Stmt::For(_pattern, iter, body) => {
                iter.traverse(ctx);
                body.traverse(ctx);
                Type::NoReturn
            }
            Stmt::Raise(..) | Stmt::Return(..) | Stmt::Break | Stmt::Continue => Type::Bottom,
            Stmt::Import(..) => Type::NoReturn,
            Stmt::Decl(..) => Type::NoReturn,
        }
    }
}

trait SExprExt<'src, 'ast> {
    fn traverse(&'ast self, ctx: &mut InferenceCtx<'src, '_>) -> Type;
}

impl<'src, 'ast> SExprExt<'src, 'ast> for Indirect<SExpr<'src>> {
    fn traverse(&'ast self, ctx: &mut InferenceCtx<'src, '_>) -> Type {
        let typ = match &self.value {
            Expr::Block(block) => 'block: {
                let mut typ = Type::NoReturn;

                for stmt in block {
                    typ = stmt.traverse(ctx);

                    if typ == Type::Bottom {
                        break 'block typ;
                    }
                }

                typ
            }
            Expr::With(_pat, value, body) => {
                value.traverse(ctx);
                body.traverse(ctx)
            }
            Expr::If(cond, then_, else_) => {
                cond.traverse(ctx);
                let then_type = then_.traverse(ctx);
                let else_type = if let Some(else_) = else_ {
                    else_.traverse(ctx)
                } else {
                    Type::NoReturn
                };

                if then_type == Type::Bottom && else_type == Type::Bottom {
                    Type::Bottom
                } else if then_type == Type::NoReturn || else_type == Type::NoReturn {
                    Type::NoReturn
                } else {
                    Type::Any
                }
            }
            Expr::Tuple(items) => {
                for item in items {
                    match item {
                        ListItem::Item(expr) => expr.traverse(ctx),
                        ListItem::Spread(expr) => expr.traverse(ctx),
                    };
                }
                Type::Any
            }
            Expr::List(items) => {
                for item in items {
                    match item {
                        ListItem::Item(expr) => expr.traverse(ctx),
                        ListItem::Spread(expr) => expr.traverse(ctx),
                    };
                }
                Type::Any
            }
            Expr::Mapping(items) => {
                for item in items {
                    match item {
                        MappingItem::Item(key, value) => {
                            key.traverse(ctx);
                            value.traverse(ctx);
                        }
                        MappingItem::Spread(expr) => {
                            expr.traverse(ctx);
                        }
                        MappingItem::Ident(expr) => {
                            expr.traverse(ctx);
                        }
                    };
                }
                Type::Any
            }
            Expr::Slice(a, b, c) => {
                if let Some(expr) = a {
                    expr.traverse(ctx);
                }
                if let Some(expr) = b {
                    expr.traverse(ctx);
                }
                if let Some(expr) = c {
                    expr.traverse(ctx);
                }
                Type::Any
            }
            Expr::Unary(_op, expr) => {
                expr.traverse(ctx);
                Type::Any
            }
            Expr::Binary(_op, a, b) => {
                a.traverse(ctx);
                b.traverse(ctx);
                Type::Any
            }
            Expr::Await(expr) | Expr::Yield(expr) | Expr::Memo(expr, _) | Expr::YieldFrom(expr) => {
                expr.traverse(ctx);
                Type::Any
            }
            Expr::Try(body, cases, finally) => {
                body.traverse(ctx);
                for case in cases {
                    case.body.traverse(ctx);
                }
                if let Some(finally) = finally {
                    finally.traverse(ctx);
                }
                Type::Any
            }
            Expr::Match(subject, cases) => {
                subject.traverse(ctx);

                let mut is_bottom: bool = true;
                let mut default_case: bool = false;

                for case in cases {
                    let body_typ = case.body.traverse(ctx);
                    if body_typ != Type::Bottom {
                        is_bottom = false;
                    }

                    let meta = ctx
                        .resolve_state
                        .patterns
                        .get(&case.pattern.as_ref().into())
                        .unwrap();

                    if meta.default {
                        default_case = true;
                    }
                }

                if is_bottom && default_case {
                    Type::Bottom
                } else {
                    Type::Any
                }
            }
            Expr::Matches(subject, _pattern) => {
                subject.traverse(ctx);
                Type::Any
            }
            Expr::Class(bases, body) => {
                for base in bases {
                    match base {
                        CallItem::Arg(expr) => expr.traverse(ctx),
                        CallItem::Kwarg(_kw, expr) => expr.traverse(ctx),
                        CallItem::ArgSpread(expr) => expr.traverse(ctx),
                        CallItem::KwargSpread(expr) => expr.traverse(ctx),
                    };
                }
                body.traverse(ctx);
                Type::Any
            }
            Expr::Call(expr, items) | Expr::MappedCall(expr, items) => {
                expr.traverse(ctx);
                for item in items {
                    match item {
                        CallItem::Arg(expr) => expr.traverse(ctx),
                        CallItem::Kwarg(_kw, expr) => expr.traverse(ctx),
                        CallItem::ArgSpread(expr) => expr.traverse(ctx),
                        CallItem::KwargSpread(expr) => expr.traverse(ctx),
                    };
                }
                Type::Any
            }
            Expr::Subscript(expr, items) | Expr::MappedSubscript(expr, items) => {
                expr.traverse(ctx);
                for item in items {
                    match item {
                        ListItem::Item(expr) => expr.traverse(ctx),
                        ListItem::Spread(expr) => expr.traverse(ctx),
                    };
                }
                Type::Any
            }
            Expr::RawAttribute(expr, _)
            | Expr::MappedRawAttribute(expr, _)
            | Expr::Attribute(expr, _)
            | Expr::MappedAttribute(expr, _)
            | Expr::MaybeAttribute(expr, _)
            | Expr::MappedMaybeAttribute(expr, _) => {
                expr.traverse(ctx);
                Type::Any
            }
            Expr::ScopedAttribute(expr, other) | Expr::MappedScopedAttribute(expr, other) => {
                expr.traverse(ctx);
                other.traverse(ctx);
                Type::Any
            }
            Expr::Checked(expr, _pattern) => {
                expr.traverse(ctx);
                Type::Any
            }
            Expr::Fn(args, body) => {
                for arg in args {
                    match arg {
                        ArgDefItem::Arg(_expr, default) => {
                            if let Some(default) = default {
                                default.traverse(ctx);
                            }
                        }
                        _ => {}
                    };
                }
                body.traverse(ctx);
                Type::Any
            }
            Expr::Fstr(_str, items) => {
                for (expr, _) in items {
                    expr.expr.traverse(ctx);
                }
                Type::Any
            }
            Expr::Literal(..) | Expr::Ident(..) => Type::Any,
            Expr::Decorated(_, _) => panic!("Decorated expressions should not be traversed"),
            Expr::Placeholder => panic!("Placeholder expression should not be traversed"),
        };

        ctx.types.insert(self.as_ref().into(), typ.clone());

        return typ;
    }
}

pub fn infer<'src, 'ast>(
    _src: &'src str,
    ast: &'ast Indirect<SExpr<'src>>,
    resolve_state: &'ast ResolveState<'src>,
) -> TlResult<InferenceCtx<'src, 'ast>> {
    let mut ctx = InferenceCtx {
        resolve_state,
        types: HashMap::new(),
    };

    ast.traverse(&mut ctx);

    Ok(ctx)
}
