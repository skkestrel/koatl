use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
};

use crate::{
    inference::InferenceCtx,
    py::{ast::*, util::PyAstBuilder},
    resolve_scopes::{
        Declaration, DeclarationKey, FnInfo, PatternInfo, ResolveState, Scope, ScopeKey,
    },
    types::Type,
    util::{LineColCache, RefHash, TlErrBuilder, TlErrs, TlResult},
};
use once_cell::sync::Lazy;
use parser::ast::*;
use slotmap::SlotMap;

static PY_KWS: &[&str] = &[
    "and", "as", "assert", "break", "class", "continue", "def", "del", "elif", "else", "except",
    "finally", "for", "from", "global", "if", "import", "in", "is", "lambda", "match", "nonlocal",
    "not", "or", "pass", "raise", "return", "try", "while", "with", "yield", "await", "async",
];

static PY_KWS_SET: Lazy<HashSet<String>> = Lazy::new(|| {
    PY_KWS
        .iter()
        .map(|&s| s.to_string())
        .collect::<HashSet<_>>()
});

trait IdentExt<'src> {
    fn escape(&self) -> PyIdent<'src>;
}

impl<'src> IdentExt<'src> for Ident<'src> {
    fn escape(&self) -> PyIdent<'src> {
        if PY_KWS_SET.contains(self.0.as_ref()) {
            format!("{}_", self.0).into()
        } else {
            self.0.clone()
        }
    }
}

#[derive(Debug, Clone)]
struct PyDecl<'src> {
    ident: PyIdent<'src>,
}

#[allow(dead_code)]
struct TlCtx<'src, 'ast> {
    source: &'src str,
    filename: &'src str,
    line_cache: LineColCache,
    export_stars: Vec<PyIdent<'src>>,

    ident_counts: HashMap<Ident<'src>, usize>,
    py_decls: HashMap<DeclarationKey, PyDecl<'src>>,

    functions: &'ast HashMap<RefHash, FnInfo>,
    patterns: &'ast HashMap<RefHash, PatternInfo>,
    resolutions: &'ast HashMap<RefHash, DeclarationKey>,
    memo_captures: &'ast HashMap<RefHash, FnInfo>,

    scopes: &'ast SlotMap<ScopeKey, Scope>,
    declarations: &'ast SlotMap<DeclarationKey, Declaration<'src>>,

    types: &'ast HashMap<RefHash, Type>,
}

impl<'src, 'ast> TlCtx<'src, 'ast> {
    fn new(
        source: &'src str,
        filename: &'src str,
        resolve_state: &'ast ResolveState<'src>,
        inference: &'ast InferenceCtx<'src, 'ast>,
    ) -> TlResult<Self> {
        Ok(TlCtx {
            source,
            filename,
            export_stars: Vec::new(),
            line_cache: LineColCache::new(source),

            ident_counts: HashMap::new(),
            py_decls: HashMap::new(),

            functions: &resolve_state.functions,
            patterns: &resolve_state.patterns,
            resolutions: &resolve_state.resolutions,
            memo_captures: &resolve_state.memo_captures,

            scopes: &resolve_state.scopes,
            declarations: &resolve_state.declarations,

            types: &inference.types,
        })
    }

    fn linecol(&self, cursor: usize) -> (usize, usize) {
        self.line_cache.linecol(cursor)
    }

    fn create_aux_var(&self, typ: &str, cursor: usize) -> PyIdent<'src> {
        let (line, col) = self.linecol(cursor);
        format!("_{}_l{}c{}", typ, line, col).into()
    }

    fn fn_info(&self, expr: &SExpr<'src>) -> TlResult<&'ast FnInfo> {
        let Expr::Fn(..) = &expr.value else {
            return Err(simple_err("Internal: expected an Expr::Fn", expr.span));
        };

        self.functions
            .get(&expr.into())
            .ok_or_else(|| simple_err("Internal: Unresolved function", expr.span))
    }

    fn pattern_info(&self, pattern: &SPattern<'src>) -> TlResult<&'ast PatternInfo> {
        self.patterns
            .get(&pattern.into())
            .ok_or_else(|| simple_err("Internal: Unresolved pattern", pattern.span))
    }

    fn decl_py_ident(&mut self, decl_key: DeclarationKey) -> TlResult<PyIdent<'src>> {
        let decl = &self.declarations[decl_key];
        let scope = &self.scopes[decl.scope];

        if scope.is_class || scope.is_global || decl.is_fn_arg {
            return Ok(decl.name.escape());
        }

        let entry = self.py_decls.entry(decl_key).or_insert_with(|| {
            let base_ident = decl.name.escape();
            let count = self.ident_counts.entry(decl.name.clone()).or_default();
            *count += 1;

            let ident = format!("let_{}_{}", base_ident, count);

            PyDecl {
                ident: ident.into(),
            }
        });

        Ok(entry.ident.clone())
    }

    fn py_ident(&mut self, expr: &SExpr<'src>) -> TlResult<PyIdent<'src>> {
        let Expr::Ident(_) = &expr.value else {
            return Err(simple_err("Internal: expected an Expr::Ident", expr.span));
        };

        let decl = self
            .resolutions
            .get(&expr.into())
            .ok_or_else(|| simple_err("Internal: Unresolved identifier", expr.span))?;

        self.decl_py_ident(*decl)
    }
}

struct WithPre<'src, T> {
    pre: PyBlock<'src>,
    value: T,
}

impl<'src> PyBlock<'src> {
    fn bind<T>(&mut self, v: WithPre<'src, T>) -> T {
        self.extend(v.pre);
        v.value
    }
}

enum PyBlockExpr<'src> {
    Nothing,
    Never,
    Expr(SPyExpr<'src>),
}

type SPyExprWithPre<'src> = WithPre<'src, SPyExpr<'src>>;
type PyBlockExprWithPre<'src> = WithPre<'src, PyBlockExpr<'src>>;

trait SPyExprWithPreExt<'src> {
    fn drop_expr<'ast>(self, ctx: &mut TlCtx<'src, 'ast>) -> PyBlock<'src>;
}

impl<'src> SPyExprWithPreExt<'src> for SPyExprWithPre<'src> {
    fn drop_expr<'ast>(self, _ctx: &mut TlCtx<'src, 'ast>) -> PyBlock<'src> {
        let mut block = self.pre;

        match self.value.value {
            PyExpr::Literal(..) | PyExpr::Ident(..) => {}
            _ => {
                let span = self.value.tl_span;
                block.push((PyStmt::Expr(self.value), span).into());
            }
        }

        block
    }
}

impl<'src> SPyExprWithPreExt<'src> for PyBlockExprWithPre<'src> {
    fn drop_expr<'ast>(self, ctx: &mut TlCtx<'src, 'ast>) -> PyBlock<'src> {
        if let PyBlockExpr::Expr(expr) = self.value {
            SPyExprWithPre {
                pre: self.pre,
                value: expr,
            }
            .drop_expr(ctx)
        } else {
            self.pre
        }
    }
}

trait BlockExt<'src> {
    fn transform<'ast>(
        &'ast self,
        ctx: &mut TlCtx<'src, 'ast>,
    ) -> TlResult<PyBlockExprWithPre<'src>>;
}

impl<'src> BlockExt<'src> for [Indirect<SStmt<'src>>] {
    fn transform<'ast>(
        &'ast self,
        ctx: &mut TlCtx<'src, 'ast>,
    ) -> TlResult<PyBlockExprWithPre<'src>> {
        if self.is_empty() {
            return Ok(WithPre {
                pre: PyBlock::new(),
                value: PyBlockExpr::Nothing,
            });
        }

        let mut pre = PyBlock::new();
        let mut errs = Vec::new();
        let mut ok = true;

        let mut handle_stmt = |stmt: &'ast SStmt<'src>| {
            match stmt.transform(ctx) {
                Ok(transformed) => {
                    pre.extend(transformed);
                }
                Err(e) => {
                    errs.extend(e.0);
                    ok = false;
                }
            };
        };

        let mut iter = self.iter();

        for stmt in iter.by_ref().take(self.len() - 1) {
            handle_stmt(stmt);
        }

        let final_stmt = iter.next().unwrap();
        let mut value = PyBlockExpr::Nothing;

        match &final_stmt.value {
            Stmt::Expr(expr) => match expr.transform(ctx) {
                Ok(expr_with_aux) => {
                    value = PyBlockExpr::Expr(pre.bind(expr_with_aux));
                }
                Err(e) => {
                    errs.extend(e.0);
                    ok = false;
                }
            },
            Stmt::Raise(..) | Stmt::Return(..) | Stmt::Break | Stmt::Continue => {
                handle_stmt(final_stmt);
                value = PyBlockExpr::Never;
            }
            _ => {
                handle_stmt(final_stmt);
            }
        }

        if ok {
            Ok(WithPre { pre, value })
        } else {
            Err(TlErrs(errs))
        }
    }
}

fn destructure_list<'src, 'ast>(
    ctx: &mut TlCtx<'src, 'ast>,
    target: &'ast SExpr<'src>,
    items: &'ast [SListItem<'src>],
    modifier: Option<DeclType>,
) -> TlResult<DestructureBindings<'src>> {
    let cursor_var = ctx.create_aux_var("des_curs", target.span.start);

    // a, b, *c = cursor_var

    let a = PyAstBuilder::new(target.span);
    let mut post = PyBlock::new();

    let mut lhs_items = vec![];
    let mut seen_spread = false;

    for item in items.iter() {
        match item {
            ListItem::Item(expr) => {
                let item_bindings = destructure(ctx, expr, modifier)?;
                lhs_items.push(PyListItem::Item(item_bindings.assign_to));
                post.extend(item_bindings.post);
            }
            ListItem::Spread(expr) => {
                if seen_spread {
                    return Err(simple_err(
                        "Destructuring assignment with multiple spreads is not allowed",
                        target.span,
                    ));
                }
                seen_spread = true;

                let item_bindings = destructure(ctx, expr, modifier)?;
                lhs_items.push(PyListItem::Spread(item_bindings.assign_to));
                post.extend(item_bindings.post);
            }
        }
    }

    post.0.insert(
        0,
        a.assign(
            a.list(lhs_items, PyAccessCtx::Store),
            a.load_ident(cursor_var.clone()),
        ),
    );

    Ok(DestructureBindings {
        post,
        assign_to: a.ident(cursor_var, PyAccessCtx::Store),
    })
}

fn destructure_tuple<'src, 'ast>(
    ctx: &mut TlCtx<'src, 'ast>,
    target: &'ast SExpr<'src>,
    items: &'ast [SListItem<'src>],
    modifier: Option<DeclType>,
) -> TlResult<DestructureBindings<'src>> {
    let cursor_var = ctx.create_aux_var("des_curs", target.span.start);
    let tuple_var = ctx.create_aux_var("des_tuple", target.span.start);
    let len_var = ctx.create_aux_var("des_len", target.span.start);

    // list_var = tuple(cursor_var)
    // len_var = len(list_var)

    let a = PyAstBuilder::new(target.span);

    let mut stmts = PyBlock(vec![
        a.assign(
            a.ident(tuple_var.clone(), PyAccessCtx::Store),
            a.call(
                a.load_ident("tuple"),
                vec![a.call_arg(a.load_ident(cursor_var.clone()))],
            ),
        ),
        a.assign(
            a.ident(len_var.clone(), PyAccessCtx::Store),
            a.call(
                a.load_ident("len"),
                vec![a.call_arg(a.load_ident(tuple_var.clone()))],
            ),
        ),
    ]);

    let mut post_stmts = vec![];

    // a = list_var[0]
    // b = list_var[1]
    // c = list_var[i:len_var-n_single_spreads_left]

    let mut seen_spread = false;
    let mut i = 0;

    for item in items.iter() {
        match item {
            ListItem::Item(expr) => {
                let item_bindings = destructure(ctx, expr, modifier)?;
                post_stmts.extend(item_bindings.post);

                stmts.push(
                    a.assign(
                        item_bindings.assign_to,
                        a.subscript(
                            a.load_ident(tuple_var.clone()),
                            a.num(
                                (if seen_spread {
                                    -((items.len() - i - 1) as isize)
                                } else {
                                    i as isize
                                })
                                .to_string(),
                            ),
                            PyAccessCtx::Load,
                        ),
                    ),
                );
                i += 1;
            }
            ListItem::Spread(expr) => {
                if seen_spread {
                    return Err(simple_err(
                        "Destructuring assignment with multiple spreads is not allowed",
                        target.span,
                    ));
                }
                seen_spread = true;

                // TODO restrict to only idents
                let item_bindings = destructure(ctx, expr, modifier)?;
                post_stmts.extend(item_bindings.post);

                stmts.push(a.assign(
                    item_bindings.assign_to,
                    a.subscript(
                        a.load_ident(tuple_var.clone()),
                        a.slice(
                            Some(a.num(i.to_string())),
                            Some(a.binary(
                                PyBinaryOp::Sub,
                                a.load_ident(len_var.clone()),
                                a.num((items.len() - 1 - i).to_string()),
                            )),
                            None,
                        ),
                        PyAccessCtx::Load,
                    ),
                ));
            }
        }
    }

    stmts.extend(post_stmts);

    Ok(DestructureBindings {
        post: stmts,
        assign_to: a.ident(cursor_var, PyAccessCtx::Store),
    })
}

fn destructure_mapping<'src, 'ast>(
    ctx: &mut TlCtx<'src, 'ast>,
    target: &'ast SExpr<'src>,
    items: &'ast [SMappingItem<'src>],
    modifier: Option<DeclType>,
) -> TlResult<DestructureBindings<'src>> {
    let cursor_var = ctx.create_aux_var("des_curs", target.span.start);
    let dict_var = ctx.create_aux_var("des_dict", target.span.start);

    // dict_var = dict(cursor_var)
    let a = PyAstBuilder::new(target.span);
    let mut stmts = PyBlock(vec![a.assign(
        a.ident(dict_var.clone(), PyAccessCtx::Store),
        a.call(
            a.tl_builtin("unpack_record"),
            vec![a.call_arg(a.load_ident(cursor_var.clone()))],
        ),
    )]);

    let mut post_stmts = PyBlock::new();

    // a = dict_var.pop(a_key)
    // b = dict_var.pop(b_key)
    // c = dict_var

    let mut spread_var = None;
    for item in items.iter() {
        match item {
            MappingItem::Ident(ident) => {
                let lhs = post_stmts.bind(ident.transform_store(ctx)?);

                let Expr::Ident(ident) = &ident.value else {
                    return Err(simple_err("Internal error: Expected ident", ident.span));
                };

                stmts.push(a.assign(
                    lhs,
                    a.call(
                        a.attribute(a.load_ident(dict_var.clone()), "pop", PyAccessCtx::Load),
                        vec![a.call_arg(a.str(ident.value.escape()))],
                    ),
                ))
            }
            MappingItem::Item(key, expr) => {
                let key_node = post_stmts.bind(key.transform(ctx)?);

                let item_bindings = destructure(ctx, expr, modifier)?;
                post_stmts.extend(item_bindings.post);

                stmts.push(a.assign(
                    item_bindings.assign_to,
                    a.call(
                        a.attribute(a.load_ident(dict_var.clone()), "pop", PyAccessCtx::Load),
                        vec![a.call_arg(key_node)],
                    ),
                ));
            }
            MappingItem::Spread(expr) => {
                if spread_var.is_some() {
                    return Err(simple_err(
                        "Destructuring assignment with multiple spreads is not allowed",
                        target.span,
                    ));
                }

                spread_var = Some(expr);
            }
        }
    }

    if let Some(spread_var) = spread_var {
        // TODO restrict to only idents
        let item_bindings = destructure(ctx, spread_var, modifier)?;

        post_stmts.extend(item_bindings.post);

        stmts.push(a.assign(item_bindings.assign_to, a.load_ident(dict_var.clone())));
    }

    stmts.extend(post_stmts);

    Ok(DestructureBindings {
        post: stmts,
        assign_to: a.ident(cursor_var, PyAccessCtx::Store),
    })
}

struct DestructureBindings<'a> {
    assign_to: SPyExpr<'a>,
    post: PyBlock<'a>,
}

fn destructure<'src, 'ast>(
    ctx: &mut TlCtx<'src, 'ast>,
    target: &'ast SExpr<'src>,
    modifier: Option<DeclType>,
) -> TlResult<DestructureBindings<'src>> {
    let mut post = PyBlock::new();

    let assign_to: SPyExpr<'src>;

    match &target.value {
        Expr::Ident(..) | Expr::RawAttribute(..) | Expr::Attribute(..) | Expr::Subscript(..) => {
            let target_node = match &target.value {
                Expr::Ident(..) => target.transform_store(ctx)?,
                Expr::RawAttribute(..) | Expr::Subscript(..) => {
                    if modifier.is_some() {
                        return Err(simple_err(
                            "Only identifiers allowed in this destructuring",
                            target.span,
                        ));
                    }
                    target.transform_store(ctx)?
                }
                Expr::Attribute(lhs, ext) => {
                    if modifier.is_some() {
                        return Err(simple_err(
                            "Only identifiers allowed in this destructuring",
                            target.span,
                        ));
                    }

                    // we need to replace an extension with a regular attribute
                    // when assigning to it

                    let mut pre = PyBlock::new();
                    let lhs = pre.bind(lhs.transform(ctx)?);

                    SPyExprWithPre {
                        pre,
                        value: (
                            PyExpr::Attribute(
                                Box::new(lhs),
                                ext.clone().value.escape(),
                                PyAccessCtx::Store,
                            ),
                            target.span,
                        )
                            .into(),
                    }
                }
                _ => {
                    panic!();
                }
            };

            post.extend(target_node.pre);
            assign_to = target_node.value;
        }
        Expr::List(items) => {
            let bindings = destructure_list(ctx, target, items, modifier)?;

            post.extend(bindings.post);
            assign_to = bindings.assign_to;
        }
        Expr::Tuple(items) => {
            let bindings = destructure_tuple(ctx, target, items, modifier)?;

            post.extend(bindings.post);
            assign_to = bindings.assign_to;
        }
        Expr::Mapping(items) => {
            let bindings = destructure_mapping(ctx, target, items, modifier)?;

            post.extend(bindings.post);
            assign_to = bindings.assign_to;
        }
        _ => {
            return Err(simple_err("Assignment target is not allowed", target.span));
        }
    };

    Ok(DestructureBindings { post, assign_to })
}

fn simple_err(msg: impl Into<String>, span: Span) -> TlErrs {
    TlErrBuilder::new().message(msg.into()).span(span).build()
}

fn transform_assignment<'src, 'ast>(
    ctx: &mut TlCtx<'src, 'ast>,
    lhs: &'ast SExpr<'src>,
    rhs: &'ast SExpr<'src>,
    modifier: Option<DeclType>,
    span: &Span,
) -> TlResult<PyBlock<'src>> {
    let mut stmts = PyBlock::new();
    let a = PyAstBuilder::new(*span);

    if let Expr::Ident(_) = &lhs.value {
        let mut decorators: Vec<&SExpr> = vec![];
        let mut cur_node = rhs;

        loop {
            match &cur_node.value {
                Expr::Binary(BinaryOp::Pipe, left, right) => {
                    cur_node = left;
                    decorators.push(right);
                }
                Expr::Decorated(deco, right) => {
                    cur_node = right;
                    decorators.push(deco);
                }
                Expr::Call(left, right) => {
                    if right.len() != 1 {
                        break;
                    }

                    match &right[0] {
                        CallItem::Arg(arg) => {
                            cur_node = arg;
                            decorators.push(left);
                        }
                        _ => {
                            break;
                        }
                    }
                }
                _ => {
                    break;
                }
            }
        }

        let lhs_ident = ctx.py_ident(lhs)?;

        let py_decorators = || -> TlResult<_> {
            Ok(PyDecorators(
                decorators
                    .into_iter()
                    .map(|x| {
                        let t = x.transform(ctx)?;
                        stmts.extend(t.pre);
                        Ok(t.value)
                    })
                    .collect::<TlResult<_>>()?,
            ))
        };

        if let Expr::Fn(arglist, body) = &cur_node.value {
            let decorators = py_decorators()?;

            let fn_def = make_fn_def(
                ctx,
                lhs_ident,
                FnDef::TlFnDef(cur_node, arglist, body),
                decorators,
                span,
            )?;

            // TODO give fn and class defs a better __name__ than the mangled ident

            return Ok(fn_def);
        } else if let Expr::Class(bases, body) = &cur_node.value {
            let decorators = py_decorators()?;

            let cls_def = make_class_def(ctx, lhs_ident, &bases, &body, decorators, span)?;

            return Ok(cls_def);
        };
    };

    let value_node = rhs.transform(ctx)?;
    stmts.extend(value_node.pre);

    let destructure = destructure(ctx, lhs, modifier)?;
    stmts.push(a.assign(destructure.assign_to, value_node.value));
    stmts.extend(destructure.post);

    Ok(stmts)
}

fn transform_if_matches_not_never_expr<'src, 'ast>(
    ctx: &mut TlCtx<'src, 'ast>,
    subject: &'ast SExpr<'src>,
    pattern_neg: &'ast SPattern<'src>,
    pattern_meta: &'ast PatternInfo,
    then_block: &'ast SExpr<'src>,
    else_block: Option<&'ast SExpr<'src>>,
    span: &Span,
) -> TlResult<SPyExprWithPre<'src>> {
    let mut pre = PyBlock::new();
    let subject = pre.bind(subject.transform(ctx)?);

    let a = PyAstBuilder::new(*span);
    let var = ctx.create_aux_var("if_matches_not", span.start);

    let span = then_block.span;

    let typ = ctx
        .types
        .get(&then_block.into())
        .ok_or_else(|| simple_err("Internal: No type for then block", span))?;

    if *typ != Type::Bottom {
        return Err(simple_err(
            "then block of 'if ... matches not ...' with named captures must have type Never (raise, return, continue, break)",
            span,
        ));
    }

    let on_no_match = then_block.transform(ctx)?.drop_expr(ctx);

    let pattern_span = pattern_neg.span;
    let pattern = pre.bind(pattern_neg.transform(ctx, pattern_meta)?);

    let on_match = if let Some(else_block) = else_block {
        let mut py_else = PyBlock::new();
        let py_else_expr = py_else.bind(else_block.transform(ctx)?);
        py_else.push(a.assign(a.ident(var.clone(), PyAccessCtx::Store), py_else_expr));
        py_else
    } else {
        PyBlock(vec![
            a.assign(a.ident(var.clone(), PyAccessCtx::Store), a.none()),
        ])
    };

    let mut cases = vec![a.match_case(pattern, None, on_match)];

    if !pattern_meta.default {
        cases.push(a.match_case(
            (PyPattern::As(None, None), pattern_span).into(),
            None,
            on_no_match,
        ))
    }

    pre.push(a.match_(subject, cases));

    Ok(SPyExprWithPre {
        value: a.ident(var, PyAccessCtx::Load),
        pre,
    })
}

fn transform_if_expr<'src, 'ast>(
    ctx: &mut TlCtx<'src, 'ast>,
    cond: &'ast SExpr<'src>,
    then_block: &'ast SExpr<'src>,
    else_block: Option<&'ast SExpr<'src>>,
    span: &Span,
) -> TlResult<SPyExprWithPre<'src>> {
    if let Expr::Unary(UnaryOp::Not, expr) = &cond.value {
        if let Expr::Matches(expr, pattern) = &expr.value {
            let info = ctx.pattern_info(pattern)?;

            if !info.decls.is_empty() {
                return transform_if_matches_not_never_expr(
                    ctx, expr, pattern, &info, then_block, else_block, span,
                );
            }
        }
    }

    let mut pre = PyBlock::new();
    let cond = pre.bind(cond.transform(ctx)?);
    let a = PyAstBuilder::new(*span);

    let ret_varname = ctx.create_aux_var("ifexp", span.start);
    let store_ret_var = a.ident(ret_varname.clone(), PyAccessCtx::Store);
    let load_ret_var = a.ident(ret_varname.clone(), PyAccessCtx::Load);

    let mut py_then = PyBlock::new();
    let py_then_expr = py_then.bind(then_block.transform(ctx)?);
    py_then.push(a.assign(store_ret_var.clone(), py_then_expr));

    let py_else = if let Some(else_block) = else_block {
        let mut py_else = PyBlock::new();
        let py_else_expr = py_else.bind(else_block.transform(ctx)?);
        py_else.push(a.assign(store_ret_var.clone(), py_else_expr));
        py_else
    } else {
        PyBlock(vec![a.assign(store_ret_var.clone(), a.none())])
    };

    pre.push(a.if_(cond, py_then, Some(py_else)));

    Ok(SPyExprWithPre {
        value: load_ret_var,
        pre,
    })
}

trait SPatternExt<'src, 'ast> {
    fn transform(
        &'ast self,
        ctx: &mut TlCtx<'src, 'ast>,
        info: &PatternInfo,
    ) -> TlResult<WithPre<'src, SPyPattern<'src>>>;
}

impl<'src, 'ast> SPatternExt<'src, 'ast> for SPattern<'src> {
    fn transform(
        &'ast self,
        ctx: &mut TlCtx<'src, 'ast>,
        info: &PatternInfo,
    ) -> TlResult<WithPre<'src, SPyPattern<'src>>> {
        // TODO avoid python syntaxerror by verifying that all branches in Or bind the same name
        // also check for no patterns after default pattern

        let mut pre = PyBlock::new();

        let pattern = &self.value;
        let span = self.span;

        fn capture_slot<'src, 'ast>(
            ctx: &mut TlCtx<'src, 'ast>,
            ident: &SIdent<'src>,
            info: &PatternInfo,
        ) -> TlResult<Option<PyIdent<'src>>> {
            if ident.value.0 == "_" {
                Ok(None)
            } else {
                let found = info
                    .decls
                    .iter()
                    .find(|d| ctx.declarations[**d].name == ident.value)
                    .ok_or_else(|| simple_err("Internal: unresolved capture slot", ident.span))?;

                Ok(Some(ctx.decl_py_ident(*found)?))
            }
        }

        fn maybe_capture_slot<'src, 'ast>(
            ctx: &mut TlCtx<'src, 'ast>,
            ident: &Option<SIdent<'src>>,
            info: &PatternInfo,
        ) -> TlResult<Option<PyIdent<'src>>> {
            if let Some(ident) = ident {
                capture_slot(ctx, ident, info)
            } else {
                Ok(None)
            }
        }

        let transformed = match pattern {
            Pattern::As(pattern, ident) => PyPattern::As(
                Some(Box::new(pre.bind(pattern.transform(ctx, info)?))),
                capture_slot(ctx, ident, info)?,
            ),
            Pattern::Literal(literal) => match literal.value {
                Literal::Num(..) | Literal::Str(..) => {
                    PyPattern::Value((PyExpr::Literal(literal.value.transform(ctx)?), span).into())
                }
                Literal::Bool(..) | Literal::None => {
                    PyPattern::Singleton(literal.value.transform(ctx)?)
                }
            },
            Pattern::Capture(v) => PyPattern::As(None, maybe_capture_slot(ctx, v, info)?),
            Pattern::Value(v) => {
                let v_node = pre.bind(v.transform(ctx)?);

                match v_node.value {
                    PyExpr::Literal(..) | PyExpr::Attribute(..) => PyPattern::Value(v_node),
                    _ => {
                        let var = ctx.create_aux_var("mproxy", span.start);
                        let a = PyAstBuilder::new(span);
                        pre.push(a.assign(
                            a.ident(var.clone(), PyAccessCtx::Store),
                            a.call(a.load_ident("Record"), vec![a.call_kwarg("value", v_node)]),
                        ));

                        PyPattern::Value(a.attribute(a.load_ident(var), "value", PyAccessCtx::Load))
                    }
                }
            }
            Pattern::Or(items) => {
                let items_nodes = items
                    .iter()
                    .map(|x| Ok(pre.bind(x.transform(ctx, info)?)))
                    .collect::<TlResult<Vec<_>>>()?;
                PyPattern::Or(items_nodes)
            }
            Pattern::Sequence(items) => {
                let mut seen_spread = false;
                let mut items_nodes = vec![];

                for item in items {
                    let node = match item {
                        PatternSequenceItem::Item(item) => {
                            PyPatternSequenceItem::Item(pre.bind(item.transform(ctx, info)?))
                        }
                        PatternSequenceItem::Spread(item) => {
                            if seen_spread {
                                return Err(simple_err(
                                    "Destructuring assignment with multiple spreads is not allowed",
                                    span,
                                ));
                            }
                            seen_spread = true;
                            PyPatternSequenceItem::Spread(maybe_capture_slot(ctx, item, info)?)
                        }
                    };

                    items_nodes.push(node);
                }

                PyPattern::Sequence(items_nodes)
            }
            Pattern::Mapping(items) => {
                let mut kvps = vec![];
                let mut spread = None;
                for item in items {
                    match item {
                        PatternMappingItem::Ident(ident) => {
                            if ident.value.0 == "_" {
                                return Err(simple_err("Record matching '_' is not allowed", span));
                            }

                            kvps.push((
                                (PyExpr::Literal(PyLiteral::Str(ident.value.escape())), span)
                                    .into(),
                                (PyPattern::As(None, capture_slot(ctx, ident, info)?), span).into(),
                            ));
                        }
                        PatternMappingItem::Item(key, value) => {
                            let key_node = key.transform(ctx)?;
                            pre.extend(key_node.pre);

                            let value_node = value.transform(ctx, info)?;
                            pre.extend(value_node.pre);

                            kvps.push((key_node.value, value_node.value));
                        }
                        PatternMappingItem::Spread(value) => {
                            if spread.is_some() {
                                return Err(simple_err(
                                    "Destructuring assignment with multiple spreads is not allowed",
                                    span,
                                ));
                            }

                            spread = maybe_capture_slot(ctx, value, info)?;
                        }
                    }
                }

                PyPattern::Mapping(kvps, spread)
            }
            Pattern::Class(cls, items) => {
                let mut values = vec![];
                let mut kvps = vec![];

                for item in items {
                    match item {
                        PatternClassItem::Item(value) => {
                            if !kvps.is_empty() {
                                return Err(simple_err(
                                    "Keyword patterns must come after positional patterns",
                                    span,
                                ));
                            }
                            values.push(pre.bind(value.transform(ctx, info)?));
                        }
                        PatternClassItem::Kw(key, value) => {
                            kvps.push((key.value.escape(), pre.bind(value.transform(ctx, info)?)));
                        }
                    }
                }

                let cls_node = pre.bind(cls.transform(ctx)?);

                PyPattern::Class(cls_node, values, kvps)
            }
        };

        Ok(WithPre {
            pre,
            value: (transformed, span).into(),
        })
    }
}

/**
 * Creates a matcher that throws an error if the pattern does not match.
 */
fn create_throwing_matcher<'src, 'ast>(
    ctx: &mut TlCtx<'src, 'ast>,
    pattern: &'ast SPattern<'src>,
    pattern_meta: &'ast PatternInfo,
) -> TlResult<(PyBlock<'src>, PyIdent<'src>)> {
    if let Pattern::Capture(Some(ident)) = &pattern.value {
        if pattern_meta.decls.len() != 1 {
            return Err(simple_err(
                "Internal: expected exactly one decl",
                pattern.span,
            ));
        }

        if ident.value.0 != "_" {
            let decl = pattern_meta.decls.iter().next().unwrap();
            return Ok((PyBlock::new(), ctx.decl_py_ident(*decl)?));
        }
    }

    let cursor = ctx.create_aux_var("matcher", pattern.span.start);

    let a = PyAstBuilder::new(pattern.span);
    let success = PyBlock(vec![a.pass()]);
    let fail = PyBlock(vec![a.raise(Some(a.call(
        a.load_ident("MatchError"),
        vec![a.call_arg(a.fstr(vec![
            a.fstr_str("failed to match value "),
            a.fstr_expr(a.load_ident(cursor.clone()), None),
            a.fstr_str(format!(
                " to pattern {}",
                &ctx.source[pattern.span.start..pattern.span.end]
            )),
        ]))],
    )))]);

    Ok((
        create_binary_matcher(
            ctx,
            a.load_ident(cursor.clone()),
            pattern,
            pattern_meta,
            success,
            fail,
        )?,
        cursor.clone().into(),
    ))
}

/**
 * Creates a basic match expression that either matches or doesn't.
 */
fn create_binary_matcher<'src, 'ast>(
    ctx: &mut TlCtx<'src, 'ast>,
    subject: SPyExpr<'src>,
    pattern: &'ast SPattern<'src>,
    pattern_meta: &'ast PatternInfo,
    on_success: PyBlock<'src>,
    on_fail: PyBlock<'src>,
) -> TlResult<PyBlock<'src>> {
    let mut post = PyBlock::new();
    let a = PyAstBuilder::new(pattern.span);
    let pattern_node = pattern.transform(ctx, pattern_meta)?;

    post.extend(pattern_node.pre);

    let mut cases = vec![PyMatchCase {
        pattern: pattern_node.value,
        guard: None,
        body: on_success,
    }];

    if !pattern_meta.default {
        cases.push(PyMatchCase {
            pattern: (PyPattern::As(None, None), pattern.span).into(),
            guard: None,
            body: on_fail,
        });
    }

    post.push(a.match_(subject, cases));

    Ok(post)
}

enum MatchSubject<'src, 'ast> {
    Py(SPyExpr<'src>),
    Tl(&'ast SExpr<'src>),
}

fn transform_match_expr<'src, 'ast>(
    ctx: &mut TlCtx<'src, 'ast>,
    subject: MatchSubject<'src, 'ast>,
    cases: &'ast [SMatchCase<'src>],
    fill_default_case: bool,
    span: &Span,
) -> TlResult<(SPyExprWithPre<'src>, bool)> {
    let mut pre = PyBlock::new();
    let subject = match subject {
        MatchSubject::Tl(subject) => pre.bind(subject.transform(ctx)?),
        MatchSubject::Py(subject) => subject,
    };

    let a = PyAstBuilder::new(*span);

    let ret_varname = ctx.create_aux_var("matchexp", span.start);
    let load_ret_var: SPyExpr = (
        PyExpr::Ident(ret_varname.clone().into(), PyAccessCtx::Load),
        *span,
    )
        .into();
    let store_ret_var: SPyExpr = (
        PyExpr::Ident(ret_varname.clone().into(), PyAccessCtx::Store),
        *span,
    )
        .into();

    let mut py_cases = vec![];
    let mut has_default_case = false;

    for case in cases {
        if has_default_case {
            return Err(simple_err(
                "Previous default case makes remaining cases unreachable",
                case.body.span,
            ));
        }

        let (pattern, default) = if let Some(pattern) = &case.pattern {
            let info = ctx.pattern_info(pattern)?;
            (pre.bind(pattern.transform(ctx, info)?), info.default)
        } else {
            ((PyPattern::As(None, None), *span).into(), true)
        };

        if case.guard.is_none() {
            if default {
                has_default_case = true;
            }
        }

        let guard = if let Some(guard) = &case.guard {
            Some(pre.bind(guard.transform(ctx)?))
        } else {
            None
        };

        let py_block = case.body.transform(ctx)?;
        let mut block_stmts = py_block.pre;

        block_stmts.push(a.assign(store_ret_var.clone(), py_block.value));

        py_cases.push(PyMatchCase {
            pattern,
            guard,
            body: block_stmts,
        });
    }

    if !has_default_case && fill_default_case {
        py_cases.push(PyMatchCase {
            pattern: (PyPattern::As(None, None), *span).into(),
            guard: None,
            body: PyBlock(vec![a.assign(store_ret_var.clone(), a.none())]),
        });

        has_default_case = true;
    }

    pre.push((PyStmt::Match(subject, py_cases), *span).into());

    Ok((
        SPyExprWithPre {
            value: load_ret_var,
            pre,
        },
        has_default_case,
    ))
}

fn make_class_def<'src, 'ast>(
    ctx: &mut TlCtx<'src, 'ast>,
    name: Cow<'src, str>,
    bases: &'ast [SCallItem<'src>],
    body: &'ast SExpr<'src>,
    decorators: PyDecorators<'src>,
    span: &Span,
) -> TlResult<PyBlock<'src>> {
    let mut stmts = PyBlock::new();
    let mut py_bases: Vec<PyCallItem<'src>> = vec![];

    for base in bases {
        let call_item: PyCallItem<'src> = match &base {
            CallItem::Arg(expr) => {
                let base_node = expr.transform(ctx)?;
                stmts.extend(base_node.pre);
                PyCallItem::Arg(base_node.value)
            }
            CallItem::Kwarg(name, expr) => {
                let expr_node = expr.transform(ctx)?;
                stmts.extend(expr_node.pre);
                PyCallItem::Kwarg(name.value.escape(), expr_node.value)
            }
            _ => {
                return Err(simple_err(
                    "spread args are not allowed in class bases",
                    *span,
                ));
            }
        };

        py_bases.push(call_item);
    }

    let mut py_body = body.transform(ctx)?.drop_expr(ctx);

    if py_body.is_empty() {
        py_body.push((PyStmt::Pass, *span).into());
    }

    stmts.push(
        (
            PyStmt::ClassDef(PyClassDef {
                name,
                bases: py_bases,
                body: py_body,
                decorators,
            }),
            *span,
        )
            .into(),
    );

    Ok(stmts)
}

struct PyArgList<'src> {
    pre: PyBlock<'src>,
    post: PyBlock<'src>,
    items: Vec<PyArgDefItem<'src>>,
}

fn make_arglist<'src, 'ast>(
    ctx: &mut TlCtx<'src, 'ast>,
    args: &'ast [SArgDefItem<'src>],
    info: &FnInfo,
) -> TlResult<PyArgList<'src>> {
    let mut pre = PyBlock::new();
    let mut post = PyBlock::new();

    let mut args_vec = vec![];

    for arg in args {
        let arg = match arg {
            ArgDefItem::Arg(arg_pattern, default) => {
                let default = if let Some(default) = default {
                    Some(pre.bind(default.transform(ctx)?))
                } else {
                    None
                };

                let meta = ctx.pattern_info(arg_pattern)?;
                let (matcher, cursor) = create_throwing_matcher(ctx, arg_pattern, &meta)?;
                post.extend(matcher);
                PyArgDefItem::Arg(cursor, default)
            }
            ArgDefItem::ArgSpread(name) => {
                let decl = info
                    .arg_names
                    .iter()
                    .find(|key| ctx.declarations[**key].name == name.value)
                    .ok_or_else(|| simple_err("Internal: missing arg name", name.span))?;

                let decl_value = &ctx.declarations[*decl];

                if decl_value.name.0 == "_" {
                    // need to create a unique name to prevent python complaining
                    PyArgDefItem::ArgSpread(ctx.create_aux_var("_", name.span.start))
                } else {
                    PyArgDefItem::ArgSpread(ctx.decl_py_ident(*decl)?)
                }
            }
            ArgDefItem::KwargSpread(name) => {
                let decl = info
                    .arg_names
                    .iter()
                    .find(|key| ctx.declarations[**key].name == name.value)
                    .ok_or_else(|| simple_err("Internal: missing arg name", name.span))?;

                let decl_value = &ctx.declarations[*decl];

                if decl_value.name.0 == "_" {
                    PyArgDefItem::KwargSpread(ctx.create_aux_var("_", name.span.start))
                } else {
                    PyArgDefItem::KwargSpread(ctx.decl_py_ident(*decl)?)
                }
            }
        };
        args_vec.push(arg);
    }

    Ok(PyArgList {
        pre,
        post,
        items: args_vec,
    })
}

struct PartialPyFnDef<'a> {
    body: PyBlock<'a>,
    decorators: PyDecorators<'a>,
    args: Vec<PyArgDefItem<'a>>,
    async_: bool,
}

enum FnDef<'src, 'ast> {
    /**
     * Args, body, is_do, is_async
     */
    PyFnDef(Vec<PyArgDefItem<'src>>, PyBlock<'src>, bool, bool),

    // Expr::Fn, args, body
    TlFnDef(
        &'ast SExpr<'src>,
        &'ast [SArgDefItem<'src>],
        &'ast SExpr<'src>,
    ),
}

/**
 * This function uses lifted_decls to store future declarations to make recursion work.
 */
fn prepare_py_fn<'src, 'ast>(
    ctx: &mut TlCtx<'src, 'ast>,
    def: FnDef<'src, 'ast>,
    span: &Span,
) -> TlResult<WithPre<'src, PartialPyFnDef<'src>>> {
    let mut pre = PyBlock::new();
    let mut py_body = PyBlock::new();
    let mut decorators = PyDecorators(vec![]);

    let a = PyAstBuilder::new(*span);
    let mut async_ = false;
    let args;

    match def {
        FnDef::PyFnDef(args_, body, is_do, is_async) => {
            args = args_;

            if is_async {
                async_ = true;
            }

            if is_do {
                decorators.push(a.tl_builtin("do"));
            }

            py_body.extend(body);
        }
        FnDef::TlFnDef(expr, arglist, body) => {
            let fn_info = ctx.fn_info(expr)?;

            let PyArgList {
                pre: arg_pre,
                post,
                items: args_,
            } = make_arglist(ctx, arglist, fn_info)?;

            args = args_;

            pre.extend(arg_pre);
            py_body.extend(post);

            let body = body.transform(ctx)?;

            if fn_info.is_async {
                async_ = true;
            }

            if fn_info.is_do {
                decorators.push(a.tl_builtin("do"));
            }

            let mut nonlocals = vec![];
            let mut globals = vec![];

            for capture in fn_info.captures.iter() {
                let scope = &ctx.scopes[ctx.declarations[*capture].scope];
                if scope.is_global || scope.is_class {
                    globals.push(ctx.decl_py_ident(*capture)?);
                } else {
                    nonlocals.push(ctx.decl_py_ident(*capture)?);
                }
            }

            if !nonlocals.is_empty() {
                py_body.push(a.nonlocal(nonlocals.iter().map(|x| x.clone()).collect()));
            }
            if !globals.is_empty() {
                py_body.push(a.global(globals.iter().map(|x| x.clone()).collect()));
            }

            py_body.extend(body.pre);
            py_body.push(a.return_(body.value));
        }
    }

    Ok(WithPre {
        pre,
        value: PartialPyFnDef {
            body: py_body,
            args,
            decorators,
            async_,
        },
    })
}

fn make_fn_exp<'src, 'ast>(
    ctx: &mut TlCtx<'src, 'ast>,
    def: FnDef<'src, 'ast>,
    span: &Span,
) -> TlResult<SPyExprWithPre<'src>> {
    let mut pre = PyBlock::new();
    let PartialPyFnDef {
        body,
        args,
        decorators,
        async_,
    } = pre.bind(prepare_py_fn(ctx, def, span)?);
    let a = PyAstBuilder::new(*span);

    if body.0.len() == 1 {
        // TODO maybe refactor prepare_py_fn to return body_stmts as PyExprWithPre instead of pattern matching Return

        if let PyStmt::Return(_) = &body.0[0].value {
            let PyStmt::Return(expr) = body.0.into_iter().next().unwrap().value else {
                return Err(simple_err(
                    "Internal error: Expected a single return statement in function body",
                    *span,
                ));
            };

            let mut inner = a.lambda(args, expr);

            for deco in decorators.0.into_iter().rev() {
                inner = a.call(deco, vec![a.call_arg(inner)]);
            }

            return Ok(SPyExprWithPre { value: inner, pre });
        }
    }

    let name = ctx.create_aux_var("fnexp", span.start);
    pre.push(
        (
            PyStmt::FnDef(PyFnDef {
                name: name.clone().into(),
                args: args,
                body,
                decorators: decorators,
                async_,
            }),
            *span,
        )
            .into(),
    );

    Ok(SPyExprWithPre {
        value: (PyExpr::Ident(name.into(), PyAccessCtx::Load), *span).into(),
        pre,
    })
}

fn make_fn_def<'src, 'ast>(
    ctx: &mut TlCtx<'src, 'ast>,
    name: Cow<'src, str>,
    def: FnDef<'src, 'ast>,
    mut decorators: PyDecorators<'src>,
    span: &Span,
) -> TlResult<PyBlock<'src>> {
    let mut pre = PyBlock::new();
    let PartialPyFnDef {
        body,
        args,
        decorators: inner_decorators,
        async_,
    } = pre.bind(prepare_py_fn(ctx, def, span)?);

    decorators.0.extend(inner_decorators.0);

    pre.push(
        (
            PyStmt::FnDef(PyFnDef {
                name: name.into(),
                args,
                body,
                decorators,
                async_,
            }),
            *span,
        )
            .into(),
    );

    Ok(pre)
}

fn transform_call_items<'src, 'ast>(
    ctx: &mut TlCtx<'src, 'ast>,
    args: &'ast [SCallItem<'src>],
    span: &Span,
) -> TlResult<WithPre<'src, Vec<PyCallItem<'src>>>> {
    let mut started_kwargs = false;
    let mut call_items = vec![];

    let mut pre = PyBlock::new();

    for arg in args {
        match &arg {
            CallItem::Arg(expr) => {
                if started_kwargs {
                    return Err(simple_err("Cannot have args after kwargs", *span));
                }

                let e = pre.bind(expr.transform(ctx)?);
                call_items.push(PyCallItem::Arg(e));
            }
            CallItem::Kwarg(name, expr) => {
                started_kwargs = true;
                let e = pre.bind(expr.transform(ctx)?);
                call_items.push(PyCallItem::Kwarg(name.value.escape(), e));
            }
            CallItem::ArgSpread(expr) => {
                if started_kwargs {
                    return Err(simple_err("Cannot have arg spread after kwargs", *span));
                }

                let e = pre.bind(expr.transform(ctx)?);
                call_items.push(PyCallItem::ArgSpread(e));
            }
            CallItem::KwargSpread(expr) => {
                started_kwargs = true;
                let e = pre.bind(expr.transform(ctx)?);
                call_items.push(PyCallItem::KwargSpread(e));
            }
        };
    }

    Ok(WithPre {
        pre,
        value: call_items,
    })
}

fn transform_subscript_items<'src, 'ast>(
    ctx: &mut TlCtx<'src, 'ast>,
    indices: &'ast [SListItem<'src>],
    span: &Span,
) -> TlResult<WithPre<'src, SPyExpr<'src>>> {
    let mut pre = PyBlock::new();

    let single_item = if indices.len() == 1 {
        match &indices[0] {
            ListItem::Item(item) => Some(item),
            ListItem::Spread(_) => None,
        }
    } else {
        None
    };

    let subscript_expr = if let Some(single_item) = single_item {
        pre.bind(single_item.transform(ctx)?)
    } else {
        (
            PyExpr::Tuple(
                indices
                    .into_iter()
                    .map(|i| match i {
                        ListItem::Item(expr) => {
                            Ok(PyListItem::Item(pre.bind(expr.transform(ctx)?)))
                        }
                        ListItem::Spread(expr) => {
                            Ok(PyListItem::Spread(pre.bind(expr.transform(ctx)?)))
                        }
                    })
                    .collect::<TlResult<Vec<_>>>()?,
                PyAccessCtx::Load,
            ),
            *span,
        )
            .into()
    };

    Ok(WithPre {
        pre,
        value: subscript_expr,
    })
}

fn transform_postfix_expr<'src, 'ast>(
    ctx: &mut TlCtx<'src, 'ast>,
    expr: &'ast SExpr<'src>,
    access_ctx: PyAccessCtx,
) -> TlResult<SPyExprWithPre<'src>> {
    let (mapped, lhs_node) = match &expr.value {
        Expr::RawAttribute(obj, _) => (false, obj),
        Expr::Subscript(obj, _) => (false, obj),
        Expr::Call(obj, _) => (false, obj),
        Expr::ScopedAttribute(obj, _) => (false, obj),
        Expr::Attribute(obj, _) => (false, obj),
        Expr::MappedRawAttribute(obj, _) => (true, obj),
        Expr::MappedSubscript(obj, _) => (true, obj),
        Expr::MappedCall(obj, _) => (true, obj),
        Expr::MappedScopedAttribute(obj, _) => (true, obj),
        Expr::MappedAttribute(obj, _) => (true, obj),
        _ => {
            return Err(simple_err(
                "Internal error: Postfix expressions can only be attributes, subscripts, calls, or extensions",
                expr.span,
            ));
        }
    };

    if let Expr::Attribute(..) | Expr::RawAttribute(..) | Expr::Subscript(..) = &expr.value {
    } else {
        if access_ctx != PyAccessCtx::Load {
            return Err(simple_err("Illegal assignment target", expr.span));
        }
    }

    let mut pre = PyBlock::new();
    let a = PyAstBuilder::new(expr.span);

    let lhs = if mapped {
        pre.bind(lhs_node.transform_lifted(ctx)?)
    } else {
        pre.bind(lhs_node.transform(ctx)?)
    };

    let guard_if_expr = |expr| {
        a.if_expr(
            a.call(a.tl_builtin("ok"), vec![a.call_arg(lhs.clone())]),
            expr,
            lhs.clone(),
        )
    };

    let node = match &expr.value {
        Expr::Call(_, list) => {
            let t = pre.bind(transform_call_items(ctx, &list, &expr.span)?);
            a.call(lhs, t)
        }
        Expr::MappedCall(_, list) => {
            let t = pre.bind(transform_call_items(ctx, &list, &expr.span)?);
            guard_if_expr(a.call(lhs.clone(), t))
        }
        Expr::Subscript(_, list) => {
            let t = pre.bind(transform_subscript_items(ctx, &list, &expr.span)?);
            a.subscript(lhs, t, access_ctx)
        }
        Expr::MappedSubscript(_, list) => {
            let t = pre.bind(transform_subscript_items(ctx, &list, &expr.span)?);
            guard_if_expr(a.subscript(lhs.clone(), t, access_ctx))
        }
        Expr::RawAttribute(_, attr) => a.attribute(lhs, attr.value.escape(), access_ctx),
        Expr::MappedRawAttribute(_, attr) => {
            guard_if_expr(a.attribute(lhs.clone(), attr.value.escape(), PyAccessCtx::Load))
        }
        Expr::ScopedAttribute(_, rhs) => {
            let t = pre.bind(rhs.transform(ctx)?);
            a.call(
                a.tl_builtin("partial"),
                vec![PyCallItem::Arg(t), PyCallItem::Arg(lhs)],
            )
        }
        Expr::MappedScopedAttribute(_, rhs) => {
            let t = pre.bind(rhs.transform(ctx)?);
            guard_if_expr(a.call(
                a.tl_builtin("partial"),
                vec![PyCallItem::Arg(t), PyCallItem::Arg(lhs.clone())],
            ))
        }
        Expr::Attribute(_, rhs) => a.call(
            a.tl_builtin("vget"),
            vec![a.call_arg(lhs), a.call_arg(a.str(rhs.value.escape()))],
        ),
        Expr::MappedAttribute(_, rhs) => guard_if_expr(a.call(
            a.tl_builtin("vget"),
            vec![
                a.call_arg(lhs.clone()),
                a.call_arg(a.str(rhs.value.escape())),
            ],
        )),
        _ => {
            return Err(simple_err(
                "Internal error: Postfix expressions can only be attributes, subscripts, calls, or extensions",
                expr.span,
            ));
        }
    };

    Ok(SPyExprWithPre { value: node, pre })
}

fn matching_except_handler<'src, 'ast>(
    ctx: &mut TlCtx<'src, 'ast>,
    var_name: PyIdent<'src>,
    handlers: &'ast [SMatchCase<'src>],
    span: &Span,
) -> TlResult<PyExceptHandler<'src>> {
    let a = PyAstBuilder::new(*span);
    let mut block = PyBlock::new();

    let (mut match_stmt, has_default) = transform_match_expr(
        ctx,
        MatchSubject::Py(a.load_ident(var_name.clone())),
        handlers,
        false,
        span,
    )?;

    if let PyStmt::Match(_, cases) = &mut match_stmt.pre.0.last_mut().unwrap().value {
        if !has_default {
            cases.push(PyMatchCase {
                pattern: (PyPattern::As(None, None), *span).into(),
                guard: None,
                body: PyBlock(vec![a.raise(None)]),
            });
        }
    } else {
        return Err(simple_err(
            "Internal error: Expected a match statement",
            *span,
        ));
    }

    block.extend(match_stmt.pre);

    Ok(PyExceptHandler {
        typ: None,
        name: Some(var_name),
        body: block,
    })
}

trait ListItemsExt<'src> {
    fn transform<'ast>(
        &'ast self,
        ctx: &mut TlCtx<'src, 'ast>,
    ) -> TlResult<WithPre<'src, Vec<PyListItem<'src>>>>;
}

impl<'src> ListItemsExt<'src> for Vec<SListItem<'src>> {
    fn transform<'ast>(
        &'ast self,
        ctx: &mut TlCtx<'src, 'ast>,
    ) -> TlResult<WithPre<'src, Vec<PyListItem<'src>>>> {
        let mut pre = PyBlock::new();
        let mut items = vec![];

        for expr in self {
            let e = match expr {
                ListItem::Spread(expr) => expr.transform(ctx)?,
                ListItem::Item(expr) => expr.transform(ctx)?,
            };
            pre.extend(e.pre);
            items.push(match expr {
                ListItem::Spread(_) => PyListItem::Spread(e.value),
                ListItem::Item(_) => PyListItem::Item(e.value),
            });
        }

        Ok(WithPre { pre, value: items })
    }
}

trait LiteralExt<'src> {
    fn transform<'ast>(&'ast self, ctx: &TlCtx<'src, 'ast>) -> TlResult<PyLiteral<'src>>;
}

impl<'src> LiteralExt<'src> for Literal<'src> {
    fn transform<'ast>(&'ast self, _ctx: &TlCtx<'src, 'ast>) -> TlResult<PyLiteral<'src>> {
        let value = match self {
            Literal::Num(num) => PyLiteral::Num(num.to_owned()),
            Literal::Str(s) => PyLiteral::Str(s.to_owned()),
            Literal::Bool(b) => PyLiteral::Bool(*b),
            Literal::None => PyLiteral::None,
        };

        Ok(value)
    }
}

trait SStmtExt<'src> {
    fn transform<'ast>(&'ast self, ctx: &mut TlCtx<'src, 'ast>) -> TlResult<PyBlock<'src>>;
}

impl<'src> SStmtExt<'src> for SStmt<'src> {
    fn transform<'ast>(&'ast self, ctx: &mut TlCtx<'src, 'ast>) -> TlResult<PyBlock<'src>> {
        let mut pre = PyBlock::new();
        let stmt = &self.value;
        let span = self.span;
        let a = PyAstBuilder::new(span);

        match &stmt {
            Stmt::Expr(expr) => {
                let expr = pre.bind(expr.transform(ctx)?);

                if let PyExpr::Ident(_, PyAccessCtx::Load) = &expr.value {
                    // this is a no-op, so we can skip it
                } else {
                    pre.push(a.expr(expr));
                }
            }
            Stmt::Assert(expr, msg) => {
                let expr = pre.bind(expr.transform(ctx)?);

                let msg = if let Some(msg) = msg {
                    Some(pre.bind(msg.transform(ctx)?))
                } else {
                    None
                };

                pre.push(a.assert(expr, msg));
            }
            Stmt::Return(expr) => {
                let expr = pre.bind(expr.transform(ctx)?);
                pre.push(a.return_(expr));
            }
            Stmt::Decl(..) => {
                // no-op
            }
            Stmt::Assign(target, value, modifier) => {
                let binding_stmts = transform_assignment(ctx, target, value, *modifier, &span)?;

                pre.extend(binding_stmts);
            }
            Stmt::Raise(expr) => {
                if let Some(expr) = expr {
                    let expr = pre.bind(expr.transform(ctx)?);
                    pre.push(a.raise(Some(expr)));
                } else {
                    pre.push(a.raise(Some(a.call(a.tl_builtin("Exception"), vec![]))));
                }
            }
            Stmt::For(target, iter, body) => {
                let iter = pre.bind(iter.transform(ctx)?);

                let mut py_body = PyBlock::new();
                let pattern_info = ctx.pattern_info(target)?;

                let (matcher, cursor) = create_throwing_matcher(ctx, target, pattern_info)?;
                py_body.extend(matcher);
                py_body.extend(body.transform(ctx)?.drop_expr(ctx));

                pre.push(a.for_(
                    a.ident(cursor.clone(), PyAccessCtx::Store),
                    a.call(
                        a.tl_builtin("vget"),
                        vec![a.call_arg(iter), a.call_arg(a.str("iter"))],
                    ),
                    py_body,
                ));
            }
            Stmt::While(cond, body) => {
                let cond = cond.transform(ctx)?;

                let body_block = body.transform(ctx)?.drop_expr(ctx);

                let cond: SPyExpr<'src> = if cond.pre.is_empty() {
                    cond.value
                } else {
                    // TODO check if inside a function

                    // if fn_ctx.is_async {
                    //     // TODO revisit this!
                    //     return Err(TlErrBuilder::default()
                    //         .message("Await is not allowed in this complex loop condition")
                    //         .span(span)
                    //         .build_errs());
                    // }

                    // if fn_ctx.is_do {
                    //     return Err(TlErrBuilder::default()
                    //         .message("Binding is not allowed in this complex loop condition")
                    //         .span(span)
                    //         .build_errs());
                    // }

                    let aux_fn = pre.bind(make_fn_exp(
                        ctx,
                        FnDef::PyFnDef(vec![], cond.pre, false, false),
                        &span,
                    )?);

                    a.call(aux_fn, vec![])
                };

                pre.push((PyStmt::While(cond, body_block), span).into());
            }
            Stmt::Try(body, excepts, finally) => {
                let body_block = body.transform(ctx)?.drop_expr(ctx);

                let finally_block = if let Some(finally) = finally {
                    Some(finally.transform(ctx)?.drop_expr(ctx))
                } else {
                    None
                };

                let var_name = ctx.create_aux_var("e", span.start);

                let excepts = matching_except_handler(ctx, var_name.into(), excepts, &span)?;

                pre.push(a.try_(body_block, vec![excepts], finally_block));
            }
            Stmt::Break => pre.push(a.break_()),
            Stmt::Continue => pre.push(a.continue_()),
            Stmt::Import(import_stmt) => {
                let mut aliases = vec![];

                let base_module = import_stmt
                    .trunk
                    .iter()
                    .map(|ident| ident.value.0.as_ref())
                    .collect::<Vec<_>>()
                    .join(".");

                match &import_stmt.imports {
                    ImportList::Star => {
                        aliases.push(PyImportAlias {
                            name: "*".into(),
                            as_name: None,
                        });
                    }
                    ImportList::Leaves(imports) => {
                        for (ident, alias) in imports {
                            aliases.push(PyImportAlias {
                                name: ident.value.escape(),
                                as_name: alias.as_ref().map(|a| a.value.escape()),
                            });
                        }
                    }
                }

                if !import_stmt.trunk.is_empty() {
                    if import_stmt.level == 0 {
                        pre.push(a.import(vec![
                            a.import_alias(import_stmt.trunk[0].value.escape(), None),
                        ]))
                    }
                    pre.push(a.import_from(Some(base_module.into()), aliases, import_stmt.level));
                } else if import_stmt.level != 0 {
                    pre.push(a.import_from(None, aliases, import_stmt.level));
                } else {
                    pre.push(a.import(aliases));
                };
            }
            Stmt::Module => {
                return Err(simple_err(
                    "Module statements are not allowed in the transform phase",
                    span,
                ));
            }
        };

        Ok(pre)
    }
}

trait SExprExt<'src, 'ast> {
    fn transform(&'ast self, ctx: &mut TlCtx<'src, 'ast>) -> TlResult<SPyExprWithPre<'src>>;

    /**
     * Transforms
     * expr
     * to
     * x = expr
     * x
     *
     * to avoid evaluating expr multiple times
     */
    fn transform_lifted(&'ast self, ctx: &mut TlCtx<'src, 'ast>) -> TlResult<SPyExprWithPre<'src>>;

    fn transform_store(&'ast self, ctx: &mut TlCtx<'src, 'ast>) -> TlResult<SPyExprWithPre<'src>>;

    fn transform_full(
        &'ast self,
        ctx: &mut TlCtx<'src, 'ast>,
        py_ctx: PyAccessCtx,
    ) -> TlResult<SPyExprWithPre<'src>>;
}

impl<'src, 'ast> SExprExt<'src, 'ast> for SExpr<'src> {
    fn transform(&'ast self, ctx: &mut TlCtx<'src, 'ast>) -> TlResult<SPyExprWithPre<'src>> {
        self.transform_full(ctx, PyAccessCtx::Load)
    }

    fn transform_lifted(&'ast self, ctx: &mut TlCtx<'src, 'ast>) -> TlResult<SPyExprWithPre<'src>> {
        let mut pre = PyBlock::new();
        let value = pre.bind(self.transform(ctx)?);
        let a = PyAstBuilder::new(self.span);

        let expr = match self.value {
            Expr::Ident(..) | Expr::Literal(..) => value,
            _ => {
                let temp_var = ctx.create_aux_var("tmp", self.span.start);

                pre.push(a.assign(a.ident(temp_var.clone(), PyAccessCtx::Store), value));

                a.ident(temp_var, PyAccessCtx::Load)
            }
        };

        Ok(SPyExprWithPre { value: expr, pre })
    }

    fn transform_store(&'ast self, ctx: &mut TlCtx<'src, 'ast>) -> TlResult<SPyExprWithPre<'src>> {
        self.transform_full(ctx, PyAccessCtx::Store)
    }

    fn transform_full(
        &'ast self,
        ctx: &mut TlCtx<'src, 'ast>,
        access_ctx: PyAccessCtx,
    ) -> TlResult<SPyExprWithPre<'src>> {
        let expr = &self.value;
        let span = self.span;
        let a = PyAstBuilder::new(span);

        let mut pre = PyBlock::<'src>::new();

        match &expr {
            Expr::RawAttribute(..) | Expr::Subscript(..) | Expr::Ident(..) => {}
            _ => {
                if access_ctx != PyAccessCtx::Load {
                    return Err(simple_err(
                        "Expression context must be Load for this expression",
                        span,
                    ));
                }
            }
        }

        let value: SPyExpr<'src> = match &expr {
            Expr::Checked(expr, pattern) => {
                let var_name = ctx.create_aux_var("chk", span.start);

                // exception variable doesn't leave the except slope, so rebind it to chk
                let err_name = ctx.create_aux_var("e", span.start);

                let mut try_body = PyBlock::new();

                let expr = try_body.bind(expr.transform(ctx)?);

                try_body.push(a.assign(a.ident(var_name.clone(), PyAccessCtx::Store), expr));

                let mut cases = vec![];

                if let Some(pattern) = pattern {
                    let info = ctx.pattern_info(pattern)?;

                    cases.push(PyMatchCase {
                        pattern: pre.bind(pattern.transform(ctx, info)?),
                        guard: None,
                        body: PyBlock(vec![a.assign(
                            a.ident(var_name.clone(), PyAccessCtx::Store),
                            a.load_ident(err_name.clone()),
                        )]),
                    });

                    if !info.default {
                        cases.push(PyMatchCase {
                            pattern: (PyPattern::As(None, None), span).into(),
                            guard: None,
                            body: PyBlock(vec![a.raise(None)]),
                        });
                    }
                } else {
                    cases.push(PyMatchCase {
                        pattern: (PyPattern::As(None, None), span).into(),
                        guard: None,
                        body: PyBlock(vec![a.assign(
                            a.ident(var_name.clone(), PyAccessCtx::Store),
                            a.load_ident(err_name.clone()),
                        )]),
                    });
                }

                let handler = PyExceptHandler {
                    typ: None,
                    name: Some(err_name.clone()),
                    body: PyBlock(vec![a.match_(a.load_ident(err_name), cases)]),
                };

                pre.push(a.try_(try_body, vec![handler], None));

                a.load_ident(var_name)
            }
            Expr::Placeholder => return Err(simple_err("Internal: <placeholder>", span)),
            Expr::Fn(arglist, body) => pre.bind(make_fn_exp(
                ctx,
                FnDef::TlFnDef(self, arglist, body),
                &span,
            )?),
            Expr::Class(bases, body) => {
                let name: Cow<_> = ctx.create_aux_var("clsexp", span.start).into();

                pre.extend(make_class_def(
                    ctx,
                    name.clone(),
                    bases,
                    body,
                    PyDecorators::new(),
                    &span,
                )?);

                a.load_ident(name)
            }
            Expr::Decorated(deco, expr) => a.call(
                pre.bind(deco.transform(ctx)?),
                vec![PyCallItem::Arg(pre.bind(expr.transform(ctx)?))],
            ),
            Expr::Literal(lit) => a.literal(lit.value.transform(ctx)?),
            Expr::Ident(_) => {
                let ident = ctx.py_ident(self);
                a.ident(ident?, access_ctx)
            }
            Expr::RawAttribute(..)
            | Expr::MappedRawAttribute(..)
            | Expr::Call(..)
            | Expr::MappedCall(..)
            | Expr::Subscript(..)
            | Expr::MappedSubscript(..)
            | Expr::ScopedAttribute(..)
            | Expr::MappedScopedAttribute(..)
            | Expr::Attribute(..)
            | Expr::MappedAttribute(..) => pre.bind(transform_postfix_expr(ctx, self, access_ctx)?),
            Expr::If(cond, then_block, else_block) => pre.bind(transform_if_expr(
                ctx,
                cond,
                then_block,
                else_block.as_ref().map(|x| x.as_ref()),
                &span,
            )?),
            Expr::Block(block) => {
                let block = pre.bind(block.transform(ctx)?);

                match block {
                    PyBlockExpr::Expr(expr) => expr,
                    PyBlockExpr::Nothing | PyBlockExpr::Never => a.none(),
                }
            }
            Expr::Match(subject, cases) => pre
                .bind(transform_match_expr(ctx, MatchSubject::Tl(subject), cases, true, &span)?.0),
            Expr::Matches(subject, pattern) => {
                let subject = pre.bind(subject.transform(ctx)?);

                let a = PyAstBuilder::new(span);
                let var = ctx.create_aux_var("matches", span.start);

                let meta = ctx.pattern_info(pattern)?;

                let matcher = create_binary_matcher(
                    ctx,
                    subject,
                    pattern,
                    &meta,
                    PyBlock(vec![
                        a.assign(a.ident(var.clone(), PyAccessCtx::Store), a.bool(true)),
                    ]),
                    PyBlock(vec![a.assign(
                        a.ident(var.clone(), PyAccessCtx::Store),
                        a.bool(false),
                    )]),
                )?;

                pre.extend(matcher);

                a.load_ident(var.clone())
            }
            Expr::Binary(op, lhs, rhs) => 'block: {
                let (lhs, rhs) = match op {
                    BinaryOp::Coalesce => {
                        let lhs = lhs.transform_lifted(ctx)?;
                        let rhs = rhs.transform(ctx)?;

                        (lhs, rhs)
                    }
                    _ => (lhs.transform(ctx)?, rhs.transform(ctx)?),
                };

                let lhs = pre.bind(lhs);
                let rhs = pre.bind(rhs);

                let py_op = match op {
                    BinaryOp::Add => PyBinaryOp::Add,
                    BinaryOp::Sub => PyBinaryOp::Sub,
                    BinaryOp::Mul => PyBinaryOp::Mult,
                    BinaryOp::Div => PyBinaryOp::Div,
                    BinaryOp::Mod => PyBinaryOp::Mod,
                    BinaryOp::Exp => PyBinaryOp::Pow,
                    BinaryOp::MatMul => PyBinaryOp::MatMult,

                    BinaryOp::And => PyBinaryOp::And,
                    BinaryOp::Or => PyBinaryOp::Or,

                    BinaryOp::Lt => PyBinaryOp::Lt,
                    BinaryOp::Gt => PyBinaryOp::Gt,
                    BinaryOp::Leq => PyBinaryOp::Leq,
                    BinaryOp::Geq => PyBinaryOp::Geq,
                    BinaryOp::Eq => PyBinaryOp::Eq,
                    BinaryOp::Neq => PyBinaryOp::Neq,
                    BinaryOp::Is => PyBinaryOp::Is,
                    BinaryOp::Nis => PyBinaryOp::Nis,

                    BinaryOp::Pipe => break 'block a.call(rhs, vec![PyCallItem::Arg(lhs)]),
                    BinaryOp::Coalesce => {
                        break 'block a.if_expr(
                            a.call(a.tl_builtin("ok"), vec![a.call_arg(lhs.clone())]),
                            lhs,
                            rhs,
                        );
                    }
                };

                a.binary(py_op, lhs, rhs)
            }
            Expr::Memo(expr) => {
                let memo_captures =
                    ctx.memo_captures
                        .get(&expr.as_ref().into())
                        .ok_or_else(|| {
                            simple_err(
                                "Internal error: Memo expression not found in memo captures",
                                expr.span,
                            )
                        })?;

                let py_expr = expr.transform(ctx)?;
                let mut py_body = PyBlock::new();

                let mut nonlocals = vec![];
                let mut globals = vec![];

                for capture in memo_captures.captures.iter() {
                    if ctx.scopes[ctx.declarations[*capture].scope].is_global {
                        globals.push(ctx.decl_py_ident(*capture)?);
                    } else {
                        nonlocals.push(ctx.decl_py_ident(*capture)?);
                    }
                }

                if !nonlocals.is_empty() {
                    py_body.push(a.nonlocal(nonlocals.iter().map(|x| x.clone()).collect()));
                }
                if !globals.is_empty() {
                    py_body.push(a.global(globals.iter().map(|x| x.clone()).collect()));
                }

                py_body.extend(py_expr.pre);
                py_body.push(a.return_(py_expr.value));

                let callback = pre.bind(make_fn_exp(
                    ctx,
                    FnDef::PyFnDef(vec![], py_body, false, false),
                    &span,
                )?);

                let linecol = ctx.line_cache.linecol(span.start);

                a.yield_(a.call(
                    a.tl_builtin("memo"),
                    vec![
                        PyCallItem::Arg(
                            a.str(
                                format!("{}:{}:{}", ctx.filename, linecol.0, linecol.1)
                            )
                        ),
                        PyCallItem::Arg(
                            a.tuple(
                                nonlocals
                                    .iter()
                                    .map(|x| a.list_item(a.load_ident(x.clone())))
                                    .collect(),
                                PyAccessCtx::Load,
                            ),
                        ),
                        PyCallItem::Arg(callback),
                    ],
                ))
            }
            Expr::Await(expr) => a.await_(pre.bind(expr.transform(ctx)?)),
            Expr::Yield(expr) => a.yield_(pre.bind(expr.transform(ctx)?)),
            Expr::YieldFrom(expr) => a.yield_from(a.call(
                a.tl_builtin("vget"),
                vec![
                    a.call_arg(pre.bind(expr.transform(ctx)?)),
                    a.call_arg(a.str("iter")),
                ],
            )),
            Expr::Unary(op, expr) => 'block: {
                let expr = pre.bind(expr.transform(ctx)?);

                let py_op = match op {
                    UnaryOp::Neg => PyUnaryOp::Neg,
                    UnaryOp::Pos => PyUnaryOp::Pos,
                    UnaryOp::Inv => PyUnaryOp::Inv,
                    UnaryOp::Not => PyUnaryOp::Not,
                    UnaryOp::Bind => {
                        break 'block a.yield_(expr);
                    }
                };

                a.unary(py_op, expr)
            }
            Expr::List(exprs) => a.list(pre.bind(exprs.transform(ctx)?), PyAccessCtx::Load),
            Expr::Tuple(exprs) => a.tuple(pre.bind(exprs.transform(ctx)?), PyAccessCtx::Load),
            Expr::Mapping(items) => {
                let mut dict_items = vec![];

                for item in items {
                    match item {
                        MappingItem::Ident(expr) => {
                            let Expr::Ident(id) = &expr.value else {
                                return Err(simple_err(
                                    "Internal error: Expected an identifier in a mapping item",
                                    expr.span,
                                ));
                            };

                            dict_items.push(PyDictItem::Item(
                                a.str(id.value.escape()),
                                a.load_ident(ctx.py_ident(expr)?),
                            ));
                        }
                        MappingItem::Item(key, value) => {
                            let key = pre.bind(key.transform(ctx)?);
                            let value = pre.bind(value.transform(ctx)?);

                            dict_items.push(PyDictItem::Item(key, value));
                        }
                        MappingItem::Spread(expr) => {
                            let expr = pre.bind(expr.transform(ctx)?);

                            dict_items.push(PyDictItem::Spread(expr));
                        }
                    }
                }

                a.call(a.load_ident("Record"), vec![a.call_arg(a.dict(dict_items))])
            }
            Expr::Slice(start, end, step) => {
                let start_node = start
                    .as_ref()
                    .map(|e| e.as_ref().transform(ctx))
                    .transpose()?;
                let end_node = end
                    .as_ref()
                    .map(|e| e.as_ref().transform(ctx))
                    .transpose()?;
                let step_node = step
                    .as_ref()
                    .map(|e| e.as_ref().transform(ctx))
                    .transpose()?;

                let mut get = |x: Option<SPyExprWithPre<'src>>| {
                    let expr = if let Some(x) = x {
                        pre.bind(x)
                    } else {
                        a.none()
                    };

                    PyCallItem::Arg(expr)
                };

                a.call(
                    a.tl_builtin("slice"),
                    vec![get(start_node), get(end_node), get(step_node)],
                )
            }
            Expr::Fstr(begin, parts) => {
                let mut nodes = Vec::new();

                nodes.push(PyFstrPart::Str(begin.value.clone().into()));

                for (fmt_expr, str_part) in parts {
                    // TODO format specifiers?
                    let expr = pre.bind(fmt_expr.expr.transform(ctx)?);

                    nodes.push(PyFstrPart::Expr(expr, None));
                    nodes.push(PyFstrPart::Str(str_part.value.clone().into()));
                }

                a.fstr(nodes)
            }
        };

        Ok(SPyExprWithPre { value, pre })
    }
}

pub struct TransformOutput<'src> {
    pub py_block: PyBlock<'src>,
    pub exports: Vec<PyIdent<'src>>,
    pub module_star_exports: Vec<PyIdent<'src>>,
}

pub fn transform_ast<'src, 'ast>(
    source: &'src str,
    filename: &'src str,
    block: &'ast SExpr<'src>,
    resolve_state: &'ast ResolveState<'src>,
    inference: &'ast InferenceCtx<'src, 'ast>,
) -> TlResult<TransformOutput<'src>> {
    let mut ctx = TlCtx::new(source, filename, resolve_state, inference)?;

    let mut py_block = PyBlock::new();
    let expr = py_block.bind(block.transform(&mut ctx)?);
    py_block.0.push((PyStmt::Expr(expr), block.span).into());

    let mut exports = Vec::new();

    for decl in &resolve_state.scopes[resolve_state.root_scope].locals {
        let decl = &resolve_state.declarations[*decl];

        if decl.is_exported {
            exports.push(decl.name.escape())
        }
    }

    Ok(TransformOutput {
        py_block,
        exports,
        module_star_exports: resolve_state
            .export_stars
            .iter()
            .map(|x| x.value.0.clone())
            .collect::<Vec<_>>(),
    })
}
