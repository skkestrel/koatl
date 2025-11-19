use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
};

use crate::ast::*;
use crate::{
    inference::InferenceCtx,
    py::{ast::*, ast_builder::PyAstBuilder},
    resolve_scopes::{
        Declaration, DeclarationKey, FnInfo, PatternInfo, ResolveState, Scope, ScopeKey,
    },
    types::Type,
    util::{LineColCache, RefHash, TlErrBuilder, TlErrs, TlResult},
};
use once_cell::sync::Lazy;
use slotmap::SlotMap;
use std::hash::{Hash, Hasher};

static PY_KWS: &[&str] = &[
    "and", "as", "assert", "break", "class", "continue", "def", "del", "elif", "else", "except",
    "finally", "for", "from", "global", "if", "import", "in", "is", "lambda", "nonlocal", "not",
    "or", "pass", "raise", "return", "try", "while", "with", "yield", "await", "async",
];

static PY_KWS_SET: Lazy<HashSet<String>> = Lazy::new(|| {
    PY_KWS
        .iter()
        .map(|&s| s.to_string())
        .collect::<HashSet<_>>()
});

trait IdentExt<'src> {
    fn escape(&self) -> PyToken<'src>;
}

impl<'src> IdentExt<'src> for Ident<'src> {
    fn escape(&self) -> PyToken<'src> {
        if PY_KWS_SET.contains(self.0.as_ref()) {
            format!("{}_", self.0).into()
        } else {
            self.0.clone()
        }
    }
}

#[derive(Debug, Clone)]
struct PyDecl<'src> {
    ident: PyToken<'src>,
}

#[allow(dead_code)]
struct TlCtx<'src, 'ast> {
    source: &'src str,
    filename: &'src str,
    line_cache: LineColCache,
    export_stars: Vec<PyToken<'src>>,

    ident_counts: HashMap<Ident<'src>, usize>,
    py_decls: HashMap<DeclarationKey, PyDecl<'src>>,

    functions: &'ast HashMap<RefHash, FnInfo>,
    patterns: &'ast HashMap<RefHash, PatternInfo>,
    resolutions: &'ast HashMap<RefHash, DeclarationKey>,
    memo_fninfo: &'ast HashMap<RefHash, FnInfo>,
    mapped_fninfo: &'ast HashMap<RefHash, FnInfo>,
    coal_fninfo: &'ast HashMap<RefHash, FnInfo>,

    scopes: &'ast SlotMap<ScopeKey, Scope>,
    declarations: &'ast SlotMap<DeclarationKey, Declaration<'src>>,

    types: &'ast HashMap<RefHash, Type>,

    target_version: (usize, usize),
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
            memo_fninfo: &resolve_state.memo_fninfo,
            mapped_fninfo: &resolve_state.mapped_fninfo,
            coal_fninfo: &resolve_state.coal_fninfo,

            scopes: &resolve_state.scopes,
            declarations: &resolve_state.declarations,

            types: &inference.types,
            target_version: (3, 12),
        })
    }

    fn linecol(&self, cursor: usize) -> (usize, usize) {
        self.line_cache.linecol(cursor)
    }

    fn create_aux_var(&self, typ: &str, cursor: usize) -> PyToken<'src> {
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

    fn decl_py_ident(&mut self, decl_key: DeclarationKey) -> TlResult<PyToken<'src>> {
        let decl = &self.declarations[decl_key];
        let scope = &self.scopes[decl.scope];

        // Some idents must not be mangled.
        if scope.is_class || scope.is_global || decl.is_fn_arg || decl.is_import {
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

    fn py_ident(&mut self, expr: &SExpr<'src>) -> TlResult<PyToken<'src>> {
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

type SPyExprWithPre<'src> = WithPre<'src, SPyExpr<'src>>;

trait SPyExprWithPreExt<'src> {
    fn drop_expr<'ast>(self, ctx: &mut TlCtx<'src, 'ast>) -> PyBlock<'src>;
}

impl<'src> SPyExprWithPreExt<'src> for SPyExprWithPre<'src> {
    fn drop_expr<'ast>(self, _ctx: &mut TlCtx<'src, 'ast>) -> PyBlock<'src> {
        let mut block = self.pre;

        match self.value.value {
            PyExpr::Literal(..) | PyExpr::Name(..) => {}
            _ => {
                let span = self.value.tl_span;
                block.push((PyStmt::Expr(self.value), span).into());
            }
        }

        block
    }
}

fn simple_err(msg: impl Into<String>, span: Span) -> TlErrs {
    TlErrBuilder::new().message(msg.into()).span(span).build()
}

fn deduplicate<'src, 'ast>(
    ctx: &mut TlCtx<'src, 'ast>,
    expr: SPyExpr<'src>,
    span: Span,
) -> TlResult<SPyExprWithPre<'src>> {
    let var_name = ctx.create_aux_var("exp", span.start);
    let a = PyAstBuilder::new(span);
    let mut pre = PyBlock::new();

    let expr = match expr.value {
        PyExpr::Name(id, _) => a.load_ident(id),
        x => {
            pre.push(a.assign(
                a.ident(var_name.clone(), PyAccessCtx::Store),
                (x, expr.tl_span).into(),
            ));
            a.load_ident(var_name.clone())
        }
    };

    Ok(SPyExprWithPre { value: expr, pre })
}

trait BlockExt<'src> {
    fn transform<'ast>(
        &'ast self,
        ctx: &mut TlCtx<'src, 'ast>,
        span: &Span,
    ) -> TlResult<SPyExprWithPre<'src>>;
}

impl<'src> BlockExt<'src> for [Indirect<SStmt<'src>>] {
    fn transform<'ast>(
        &'ast self,
        ctx: &mut TlCtx<'src, 'ast>,
        span: &Span,
    ) -> TlResult<SPyExprWithPre<'src>> {
        let mut value = (PyExpr::Literal(PyLiteral::None), *span).into();

        if self.is_empty() {
            return Ok(SPyExprWithPre {
                pre: PyBlock::new(),
                value,
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

        match &final_stmt.value {
            Stmt::Expr(expr) => match expr.transform(ctx) {
                Ok(expr_with_aux) => {
                    value = pre.bind(expr_with_aux);
                }
                Err(e) => {
                    errs.extend(e.0);
                    ok = false;
                }
            },
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

fn transform_assignment<'src, 'ast>(
    ctx: &mut TlCtx<'src, 'ast>,
    lhs: &'ast SPattern<'src>,
    rhs: &'ast SExpr<'src>,
    span: &Span,
) -> TlResult<PyBlock<'src>> {
    let mut stmts = PyBlock::new();
    let meta = ctx.pattern_info(lhs)?;
    let a = PyAstBuilder::new(*span);

    if let Pattern::Capture(..) = &lhs.value {
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

        let lhs_ident = match meta.decls.as_slice() {
            [first] => ctx.decl_py_ident(*first),
            _ => Err(simple_err("Internal: No unique identifier for lhs", *span)),
        }?;

        let py_decorators = || -> TlResult<_> {
            Ok(PyDecorators(
                decorators
                    .into_iter()
                    .map(|x| Ok(stmts.bind(x.transform(ctx)?)))
                    .collect::<TlResult<_>>()?,
            ))
        };

        if let Expr::Fn(arglist, body) = &cur_node.value {
            let decorators = py_decorators()?;

            stmts.extend(make_fn_def(
                ctx,
                lhs_ident,
                FnDef::TlFnDef(cur_node, arglist, body),
                decorators,
                span,
            )?);

            // TODO give fn and class defs a better __name__ than the mangled ident

            return Ok(stmts);
        } else if let Expr::Class(bases, body) = &cur_node.value {
            let decorators = py_decorators()?;

            stmts.extend(make_class_def(
                ctx, lhs_ident, &bases, &body, decorators, span,
            )?);

            return Ok(stmts);
        };
    };

    let value_node = rhs.transform(ctx)?;
    stmts.extend(value_node.pre);

    let (matcher, cursor) = create_throwing_matcher(ctx, lhs, meta)?;
    stmts.push(a.assign(a.ident(cursor, PyAccessCtx::Store), value_node.value));
    stmts.extend(matcher);

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
            "then block of 'if ... not matches ...' with named captures must have type Never (raise, return, continue, break)",
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
        let mut pre = PyBlock::new();

        let pattern = &self.value;
        let span = self.span;

        fn capture_slot<'src, 'ast>(
            ctx: &mut TlCtx<'src, 'ast>,
            ident: &SIdent<'src>,
            info: &PatternInfo,
        ) -> TlResult<Option<PyToken<'src>>> {
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
        ) -> TlResult<Option<PyToken<'src>>> {
            if let Some(ident) = ident {
                capture_slot(ctx, ident, info)
            } else {
                Ok(None)
            }
        }

        let transformed = match pattern {
            Pattern::As(pattern, ident) => match ident {
                Some(ident) => PyPattern::As(
                    Some(Box::new(pre.bind(pattern.transform(ctx, info)?))),
                    capture_slot(ctx, ident, info)?,
                ),
                None => pre.bind(pattern.transform(ctx, info)?).value,
            },
            Pattern::Literal(literal) => match literal.value {
                Literal::Int(..)
                | Literal::IntBin(..)
                | Literal::IntOct(..)
                | Literal::IntHex(..)
                | Literal::Float(..)
                | Literal::Str(..) => {
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
                            a.call(
                                a.tl_builtin("record_literal"),
                                vec![a.call_arg(a.dict(vec![a.dict_item(a.str("value"), v_node)]))],
                            ),
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
) -> TlResult<(PyBlock<'src>, PyToken<'src>)> {
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
        a.tl_builtin("MatchError"),
        vec![a.call_arg(a.fstr(vec![
            a.fstr_str("failed to match value of type "),
            a.fstr_expr(
                a.call(
                    a.tl_builtin("type"),
                    vec![a.call_arg(a.load_ident(cursor.clone()))],
                ),
                None,
            ),
            a.fstr_str(format!(
                " to pattern \"{}\"",
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
        PyExpr::Name(ret_varname.clone().into(), PyAccessCtx::Load),
        *span,
    )
        .into();
    let store_ret_var: SPyExpr = (
        PyExpr::Name(ret_varname.clone().into(), PyAccessCtx::Store),
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

        let info = ctx.pattern_info(&case.pattern)?;
        let pattern = pre.bind(case.pattern.transform(ctx, info)?);
        let default = info.default;

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

struct PyArgListWithPre<'src> {
    pre: PyBlock<'src>,
    post: PyBlock<'src>,
    arglist: PyArgList<'src>,
}

fn make_arglist<'src, 'ast>(
    ctx: &mut TlCtx<'src, 'ast>,
    args: &'ast [SArgDefItem<'src>],
    info: &FnInfo,
) -> TlResult<PyArgListWithPre<'src>> {
    // Validate Python argument syntax rules
    {
        let mut seen_posonly_marker: Option<Span> = None;
        let mut seen_kwonly_marker: Option<Span> = None;
        let mut seen_vararg: Option<Span> = None;
        let mut seen_kwarg = false;
        let mut seen_default = false;
        let mut after_kwonly_marker = false;

        for arg in args.iter() {
            match arg {
                ArgDefItem::Arg(arg_pattern, default) => {
                    if after_kwonly_marker {
                        // Keyword-only args can have any default pattern
                        continue;
                    }

                    if default.is_some() {
                        seen_default = true;
                    } else if seen_default {
                        return Err(simple_err(
                            "Non-default argument follows default argument",
                            arg_pattern.span,
                        ));
                    }
                }
                ArgDefItem::PosOnlyMarker => {
                    // Get a span - we'll use the first arg's span as a fallback
                    let span = args
                        .first()
                        .and_then(|a| match a {
                            ArgDefItem::Arg(p, _) => Some(p.span),
                            ArgDefItem::ArgSpread(n) | ArgDefItem::KwargSpread(n) => Some(n.span),
                            _ => None,
                        })
                        .unwrap_or(Span { start: 0, end: 0 });

                    if seen_posonly_marker.is_some() {
                        return Err(simple_err(
                            "Only one position-only marker (/) is allowed",
                            span,
                        ));
                    }
                    if seen_kwonly_marker.is_some() || seen_vararg.is_some() {
                        return Err(simple_err(
                            "Position-only marker (/) must come before * or *args",
                            span,
                        ));
                    }
                    seen_posonly_marker = Some(span);
                    // Reset for regular args section
                    seen_default = false;
                }
                ArgDefItem::KwOnlyMarker => {
                    let span = args
                        .first()
                        .and_then(|a| match a {
                            ArgDefItem::Arg(p, _) => Some(p.span),
                            ArgDefItem::ArgSpread(n) | ArgDefItem::KwargSpread(n) => Some(n.span),
                            _ => None,
                        })
                        .unwrap_or(Span { start: 0, end: 0 });

                    if seen_kwonly_marker.is_some() {
                        return Err(simple_err(
                            "Only one keyword-only marker (*) is allowed",
                            span,
                        ));
                    }
                    if seen_vararg.is_some() {
                        return Err(simple_err("Cannot have both *args and bare * marker", span));
                    }
                    seen_kwonly_marker = Some(span);
                    after_kwonly_marker = true;
                    seen_default = false;
                }
                ArgDefItem::ArgSpread(name) => {
                    if seen_vararg.is_some() {
                        return Err(simple_err("Only one *args is allowed", name.span));
                    }
                    if seen_kwonly_marker.is_some() {
                        return Err(simple_err(
                            "Cannot have both *args and bare * marker",
                            name.span,
                        ));
                    }
                    seen_vararg = Some(name.span);
                    after_kwonly_marker = true;
                    seen_default = false;
                }
                ArgDefItem::KwargSpread(name) => {
                    if seen_kwarg {
                        return Err(simple_err("Only one **kwargs is allowed", name.span));
                    }
                    seen_kwarg = true;
                }
            }
        }
    }

    let mut pre = PyBlock::new();
    let mut post = PyBlock::new();

    let mut posonlyargs = vec![];
    let mut regular_args = vec![];
    let mut vararg = None;
    let mut kwonlyargs = vec![];
    let mut kwarg = None;

    // Track state transitions: posonly -> normal -> kwonly
    // States: 0 = position-only, 1 = normal, 2 = keyword-only
    let mut state = 0;

    for arg in args {
        match arg {
            ArgDefItem::Arg(arg_pattern, default) => {
                let default = if let Some(default) = default {
                    Some(pre.bind(default.transform(ctx)?))
                } else {
                    None
                };

                let meta = ctx.pattern_info(arg_pattern)?;
                let (matcher, cursor) = create_throwing_matcher(ctx, arg_pattern, &meta)?;
                post.extend(matcher);

                // Place argument in appropriate category based on state
                match state {
                    0 => posonlyargs.push((cursor, default)),
                    1 => regular_args.push((cursor, default)),
                    _ => kwonlyargs.push((cursor, default)),
                }
            }
            ArgDefItem::PosOnlyMarker => {
                // Transition from position-only to regular args
                state = 1;
            }
            ArgDefItem::KwOnlyMarker => {
                // Bare * marker - transition to keyword-only without vararg
                state = 2;
            }
            ArgDefItem::ArgSpread(name) => {
                let decl = info
                    .arg_names
                    .iter()
                    .find(|key| ctx.declarations[**key].name == name.value)
                    .ok_or_else(|| simple_err("Internal: missing arg name", name.span))?;

                let decl_value = &ctx.declarations[*decl];

                let var_name = if decl_value.name.0 == "_" {
                    // need to create a unique name to prevent python complaining
                    ctx.create_aux_var("_", name.span.start)
                } else {
                    ctx.decl_py_ident(*decl)?
                };

                vararg = Some(var_name);
                // Transition to keyword-only state after *args
                state = 2;
            }
            ArgDefItem::KwargSpread(name) => {
                let decl = info
                    .arg_names
                    .iter()
                    .find(|key| ctx.declarations[**key].name == name.value)
                    .ok_or_else(|| simple_err("Internal: missing arg name", name.span))?;

                let decl_value = &ctx.declarations[*decl];

                let kwarg_name = if decl_value.name.0 == "_" {
                    ctx.create_aux_var("_", name.span.start)
                } else {
                    ctx.decl_py_ident(*decl)?
                };

                kwarg = Some(kwarg_name);
            }
        };
    }

    Ok(PyArgListWithPre {
        pre,
        post,
        arglist: PyArgList {
            posonlyargs,
            args: regular_args,
            vararg,
            kwonlyargs,
            kwarg,
        },
    })
}

struct PartialPyFnDef<'a> {
    body: SPyExprWithPre<'a>,
    decorators: PyDecorators<'a>,
    args: PyArgList<'a>,
    async_: bool,
}

enum FnDef<'src, 'ast> {
    /**
     * Args, body, is_do, is_async
     */
    PyFnDef(PyArgList<'src>, SPyExprWithPre<'src>, bool, bool),

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
    let mut py_body_pre = PyBlock::new();
    let py_body_expr: SPyExpr;
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

            py_body_pre.extend(body.pre);
            py_body_expr = body.value;
        }
        FnDef::TlFnDef(expr, arglist, body) => {
            let fn_info = ctx.fn_info(expr)?;

            let PyArgListWithPre {
                pre: arg_pre,
                post,
                arglist: args_,
            } = make_arglist(ctx, arglist, fn_info)?;

            args = args_;

            pre.extend(arg_pre);
            py_body_pre.extend(post);

            let body = body.transform(ctx)?;

            if fn_info.is_async {
                async_ = true;
            }

            if fn_info.is_do {
                decorators.push(a.tl_builtin("do"));
            }

            let (py_bindings, _, _) = py_fn_bindings(ctx, fn_info, *span)?;

            py_body_pre.extend(py_bindings);
            py_body_expr = py_body_pre.bind(body);
        }
    }

    Ok(WithPre {
        pre,
        value: PartialPyFnDef {
            body: WithPre {
                pre: py_body_pre,
                value: py_body_expr,
            },
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

    if body.pre.is_empty() && !async_ {
        // TODO maybe refactor prepare_py_fn to return body_stmts as PyExprWithPre instead of pattern matching Return

        let mut inner = a.lambda(args, body.value);

        for deco in decorators.0.into_iter().rev() {
            inner = a.call(deco, vec![a.call_arg(inner)]);
        }

        return Ok(SPyExprWithPre {
            pre: PyBlock::new(),
            value: inner,
        });
    }

    let body = {
        let mut block = body.pre;
        block.push(a.return_(body.value));
        block
    };

    let name = ctx.create_aux_var("fnexp", span.start);
    pre.push(
        (
            PyStmt::FnDef(PyFnDef {
                name: name.clone().into(),
                args: args,
                body: body,
                decorators: decorators,
                async_,
            }),
            *span,
        )
            .into(),
    );

    Ok(SPyExprWithPre {
        value: (PyExpr::Name(name.into(), PyAccessCtx::Load), *span).into(),
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
    let a = PyAstBuilder::new(*span);
    let PartialPyFnDef {
        body,
        args,
        decorators: inner_decorators,
        async_,
    } = pre.bind(prepare_py_fn(ctx, def, span)?);

    decorators.0.extend(inner_decorators.0);

    let body = {
        let mut block = body.pre;
        block.push(a.return_(body.value));
        block
    };

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
        Expr::MaybeAttribute(obj, _) => (false, obj),
        Expr::MappedRawAttribute(obj, _) => (true, obj),
        Expr::MappedSubscript(obj, _) => (true, obj),
        Expr::MappedCall(obj, _) => (true, obj),
        Expr::MappedScopedAttribute(obj, _) => (true, obj),
        Expr::MappedAttribute(obj, _) => (true, obj),
        Expr::MappedMaybeAttribute(obj, _) => (true, obj),
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

    let a = PyAstBuilder::new(expr.span);

    let make_expr = |ctx, lhs| -> TlResult<_> {
        let mut inner_pre = PyBlock::new();
        let node = match &expr.value {
            Expr::Call(_, list) | Expr::MappedCall(_, list) => {
                let t = inner_pre.bind(transform_call_items(ctx, &list, &expr.span)?);
                a.call(lhs, t)
            }
            Expr::Subscript(_, list) | Expr::MappedSubscript(_, list) => {
                let t = inner_pre.bind(transform_subscript_items(ctx, &list, &expr.span)?);
                a.subscript(lhs, t, access_ctx)
            }
            Expr::ScopedAttribute(_, rhs) | Expr::MappedScopedAttribute(_, rhs) => {
                let t = inner_pre.bind(rhs.transform(ctx)?);
                a.call(t, vec![PyCallItem::Arg(lhs)])
            }
            Expr::RawAttribute(_, attr) | Expr::MappedRawAttribute(_, attr) => {
                a.attribute(lhs, attr.value.escape(), access_ctx)
            }
            Expr::Attribute(_, rhs) | Expr::MappedAttribute(_, rhs) => {
                if access_ctx == PyAccessCtx::Load {
                    a.call(
                        a.tl_builtin("vget"),
                        vec![a.call_arg(lhs), a.call_arg(a.str(rhs.value.escape()))],
                    )
                } else {
                    a.attribute(lhs, rhs.value.escape(), access_ctx)
                }
            }
            Expr::MaybeAttribute(_, rhs) | Expr::MappedMaybeAttribute(_, rhs) => {
                let temp_var = ctx.create_aux_var("attr", expr.span.start);

                inner_pre.push(a.try_(
                    PyBlock(vec![a.assign(
                        a.ident(temp_var.clone(), PyAccessCtx::Store),
                        a.call(
                            a.tl_builtin("vget"),
                            vec![a.call_arg(lhs), a.call_arg(a.str(rhs.value.escape()))],
                        ),
                    )]),
                    vec![PyExceptHandler {
                        typ: None,
                        name: None,
                        body: PyBlock(vec![
                            a.assign(a.ident(temp_var.clone(), PyAccessCtx::Store), a.none()),
                        ]),
                    }],
                    None,
                ));

                a.load_ident(temp_var)
            }
            _ => {
                panic!()
            }
        };

        Ok(SPyExprWithPre {
            value: node,
            pre: inner_pre,
        })
    };

    let mut pre = PyBlock::new();

    if mapped {
        let arg_name = ctx.create_aux_var("maparg", expr.span.start);
        let py_expr = make_expr(ctx, a.load_ident(arg_name.clone()))?;

        let fn_info = ctx.mapped_fninfo.get(&expr.into()).unwrap();

        let inner_fn = pre.bind(make_fn_exp(
            ctx,
            FnDef::PyFnDef(
                PyArgList::simple_args(vec![(arg_name, None)]),
                py_expr,
                false,
                fn_info.is_async,
            ),
            &expr.span,
        )?);

        let mut call = a.call(
            a.tl_builtin("op_map"),
            vec![
                a.call_arg(pre.bind(lhs_node.transform(ctx)?)),
                a.call_arg(inner_fn),
            ],
        );

        if fn_info.is_async {
            call = a.await_(call);
        }
        if fn_info.is_do || fn_info.is_generator {
            call = a.yield_from(call);
        }

        Ok(SPyExprWithPre { value: call, pre })
    } else {
        let lhs = pre.bind(lhs_node.transform(ctx)?);
        let expr = pre.bind(make_expr(ctx, lhs)?);
        Ok(SPyExprWithPre { value: expr, pre })
    }
}

fn matching_except_handler<'src, 'ast>(
    ctx: &mut TlCtx<'src, 'ast>,
    var_name: PyToken<'src>,
    handlers: &'ast [SMatchCase<'src>],
    assign_match_value_to: Option<PyToken<'src>>,
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
    if let Some(var_name) = assign_match_value_to {
        block.push(a.assign(a.ident(var_name, PyAccessCtx::Store), match_stmt.value));
    }

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
            Literal::Int(num) => PyLiteral::Int(num.to_owned()),
            Literal::IntBin(num) => PyLiteral::IntBin(num.to_owned()),
            Literal::IntOct(num) => PyLiteral::IntOct(num.to_owned()),
            Literal::IntHex(num) => PyLiteral::IntHex(num.to_owned()),
            Literal::Float(num) => PyLiteral::Float(num.to_owned()),
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

                if let PyExpr::Name(_, PyAccessCtx::Load) = &expr.value {
                    // this is a no-op, so we can skip it
                } else {
                    pre.push(a.expr(expr));
                }
            }
            Stmt::Return(expr) => {
                let expr = pre.bind(expr.transform(ctx)?);
                pre.push(a.return_(expr));
            }
            Stmt::Decl(..) => {
                // no-op
            }
            Stmt::PatternAssign(lhs, rhs, _decl_type) => {
                let binding_stmts = transform_assignment(ctx, lhs, rhs, &span)?;

                pre.extend(binding_stmts);
            }
            Stmt::Assign(lhs, rhs, op) => 'block: {
                if let Some(BinaryOp::Coalesce | BinaryOp::Pipe) = op {
                    let lhs = pre.bind(lhs.transform_store(ctx)?);

                    // TODO this is a bit hacky. We need to get separate the root node
                    // from the rest of the expression to avoid double evaluation
                    let (safe_lhs_store, safe_lhs_load) = match lhs.value {
                        PyExpr::Name(id, _) => (
                            a.ident(id.clone(), PyAccessCtx::Store),
                            a.load_ident(id.clone()),
                        ),
                        PyExpr::Attribute(left, right, _) => {
                            let left_span = left.tl_span;
                            let dedup = pre.bind(deduplicate(ctx, *left, left_span)?);
                            (
                                a.attribute(dedup.clone(), right.clone(), PyAccessCtx::Store),
                                a.attribute(dedup, right, PyAccessCtx::Load),
                            )
                        }
                        PyExpr::Subscript(left, right, _) => {
                            let left_span = left.tl_span;
                            let dedup = pre.bind(deduplicate(ctx, *left, left_span)?);
                            let dedup_right = pre.bind(deduplicate(ctx, *right, left_span)?);
                            (
                                a.subscript(dedup.clone(), dedup_right.clone(), PyAccessCtx::Store),
                                a.subscript(dedup, dedup_right, PyAccessCtx::Load),
                            )
                        }
                        _ => panic!(),
                    };

                    let new_rhs = match op {
                        Some(BinaryOp::Pipe) => a.call(
                            pre.bind(rhs.transform(ctx)?),
                            vec![a.call_arg(safe_lhs_load)],
                        ),
                        Some(BinaryOp::Coalesce) => {
                            pre.bind(create_coalesce(ctx, safe_lhs_load, rhs, span)?)
                        }
                        _ => panic!(),
                    };

                    pre.push(a.assign(safe_lhs_store, new_rhs));

                    break 'block;
                }

                let py_op = match op {
                    Some(op) => Some(map_py_binary_op(*op, span)?),
                    None => None,
                };

                let rhs = pre.bind(rhs.transform(ctx)?);
                let lhs = pre.bind(lhs.transform_store(ctx)?);

                pre.push(a.assign_modified(lhs, rhs, py_op))
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
                let py_cond = cond.transform(ctx)?;

                let body_block = body.transform(ctx)?.drop_expr(ctx);

                if py_cond.pre.is_empty() {
                    pre.push(a.while_(py_cond.value, body_block));
                } else {
                    let mut new_body_block = PyBlock::new();
                    new_body_block.extend(py_cond.pre);
                    new_body_block.push(a.if_(
                        a.not(py_cond.value),
                        PyBlock(vec![a.break_()]),
                        None,
                    ));
                    new_body_block.extend(body_block);

                    pre.push(a.while_(a.bool(true), new_body_block));
                }
            }
            Stmt::Break => pre.push(a.break_()),
            Stmt::Continue => pre.push(a.continue_()),
            Stmt::Import(tree, reexport) => {
                fn traverse_import_tree<'src>(
                    tree: &ImportTree<'src>,
                    mut trunk_accum: Vec<SIdent<'src>>,
                    mut level: usize,
                    reexport: bool,
                ) -> TlResult<PyBlock<'src>> {
                    let to_drain = std::cmp::min(tree.level, trunk_accum.len());
                    trunk_accum.truncate(trunk_accum.len() - to_drain);
                    level += tree.level - to_drain;

                    trunk_accum.extend(tree.trunk.iter().cloned());

                    let base_module = trunk_accum
                        .iter()
                        .map(|ident| ident.value.0.as_ref())
                        .collect::<Vec<_>>()
                        .join(".");

                    let a = PyAstBuilder::new(tree.leaf.span);

                    match &tree.leaf.value {
                        ImportLeaf::Star => Ok(PyBlock(vec![a.import_from(
                            Some(base_module.into()),
                            vec![a.import_alias("*", None)],
                            level,
                        )])),
                        ImportLeaf::This(alias) => {
                            if trunk_accum.len() == 0 {
                                Err(simple_err(
                                    "Cannot use `this` import without a base module",
                                    tree.leaf.span,
                                ))
                            } else {
                                let base_module = trunk_accum[..trunk_accum.len() - 1]
                                    .iter()
                                    .map(|ident| ident.value.0.as_ref())
                                    .collect::<Vec<_>>()
                                    .join(".");

                                let aliases = vec![a.import_alias(
                                    trunk_accum.last().unwrap().value.escape(),
                                    alias.as_ref().map(|a| a.value.escape()),
                                )];

                                if trunk_accum.len() == 1 && level == 0 {
                                    Ok(PyBlock(vec![a.import(aliases)]))
                                } else {
                                    Ok(PyBlock(vec![a.import_from(
                                        Some(base_module.into()),
                                        aliases,
                                        level,
                                    )]))
                                }
                            }
                        }
                        ImportLeaf::Single(name, alias) => {
                            let aliases = vec![a.import_alias(
                                name.value.escape(),
                                alias.as_ref().map(|a| a.value.escape()),
                            )];

                            if trunk_accum.len() == 0 && level == 0 {
                                Ok(PyBlock(vec![a.import(aliases)]))
                            } else {
                                Ok(PyBlock(vec![a.import_from(
                                    Some(base_module.into()),
                                    aliases,
                                    level,
                                )]))
                            }
                        }
                        ImportLeaf::Multi(leaves) => {
                            let mut stmts = PyBlock::new();

                            for leaf in leaves {
                                stmts.extend(traverse_import_tree(
                                    leaf,
                                    trunk_accum.clone(),
                                    level,
                                    reexport,
                                )?);
                            }

                            Ok(stmts)
                        }
                    }
                }

                if tree.level == 0 {
                    if let Some(tl) = tree.trunk.first() {
                        pre.push(a.import(vec![a.import_alias(tl.value.escape(), None)]));
                    }
                }

                pre.extend(traverse_import_tree(tree, vec![], 0, *reexport)?);
            }
        };

        Ok(pre)
    }
}

trait SExprExt<'src, 'ast> {
    fn transform(&'ast self, ctx: &mut TlCtx<'src, 'ast>) -> TlResult<SPyExprWithPre<'src>>;

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
            Expr::Attribute(..)
            | Expr::RawAttribute(..)
            | Expr::Subscript(..)
            | Expr::Ident(..) => {}
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

                try_body.push(a.assign(
                    a.ident(var_name.clone(), PyAccessCtx::Store),
                    a.call(a.tl_builtin("Ok"), vec![a.call_arg(expr)]),
                ));

                let mut cases = vec![];

                if let Some(pattern) = pattern {
                    let info = ctx.pattern_info(pattern)?;

                    cases.push(PyMatchCase {
                        pattern: pre.bind(pattern.transform(ctx, info)?),
                        guard: None,
                        body: PyBlock(vec![a.assign(
                            a.ident(var_name.clone(), PyAccessCtx::Store),
                            a.call(
                                a.tl_builtin("Err"),
                                vec![a.call_arg(a.load_ident(err_name.clone()))],
                            ),
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
                            a.call(
                                a.tl_builtin("Err"),
                                vec![a.call_arg(a.load_ident(err_name.clone()))],
                            ),
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
            | Expr::MappedAttribute(..)
            | Expr::MaybeAttribute(..)
            | Expr::MappedMaybeAttribute(..) => {
                pre.bind(transform_postfix_expr(ctx, self, access_ctx)?)
            }
            Expr::With(pattern, value, body) => {
                let temp_var = ctx.create_aux_var("with", span.start);
                let value = pre.bind(value.transform(ctx)?);

                let info = ctx.pattern_info(pattern)?;
                let (matcher, cursor) = create_throwing_matcher(ctx, pattern, &info)?;

                let mut body_block = PyBlock::new();
                body_block.extend(matcher);
                let body = body.transform(ctx)?;
                body_block.extend(body.pre);
                body_block
                    .push(a.assign(a.ident(temp_var.clone(), PyAccessCtx::Store), body.value));

                pre.push(a.with(
                    vec![(value, Some(a.ident(cursor, PyAccessCtx::Store)))],
                    body_block,
                ));

                a.load_ident(temp_var)
            }
            Expr::Try(body, excepts, finally) => {
                let temp_var = ctx.create_aux_var("try", span.start);

                let t_body = body.transform(ctx)?;
                let mut py_body = t_body.pre;
                py_body.push(a.assign(a.ident(temp_var.clone(), PyAccessCtx::Store), t_body.value));

                let finally_block = if let Some(finally) = finally {
                    Some(finally.transform(ctx)?.drop_expr(ctx))
                } else {
                    None
                };

                let var_name = ctx.create_aux_var("e", span.start);

                let excepts = matching_except_handler(
                    ctx,
                    var_name.into(),
                    excepts,
                    Some(temp_var.clone()),
                    &span,
                )?;

                pre.push(a.try_(py_body, vec![excepts], finally_block));

                a.load_ident(temp_var)
            }
            Expr::If(cond, then_block, else_block) => pre.bind(transform_if_expr(
                ctx,
                cond,
                then_block,
                else_block.as_ref().map(|x| x.as_ref()),
                &span,
            )?),
            Expr::Block(block) => pre.bind(block.transform(ctx, &span)?),
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
                if let BinaryOp::Coalesce = op {
                    let py_lhs = pre.bind(lhs.transform(ctx)?);
                    let call = pre.bind(create_coalesce(ctx, py_lhs, rhs, span)?);
                    break 'block call;
                } else if let BinaryOp::And | BinaryOp::Or = op {
                    // handle short-circuiting manually

                    let is_and = *op == BinaryOp::And;

                    let py_lhs = pre.bind(lhs.transform(ctx)?);
                    let py_rhs = rhs.transform(ctx)?;

                    if py_rhs.pre.is_empty() {
                        // no pre, so we can just use regular python binary
                        break 'block a.binary(
                            if is_and {
                                PyBinaryOp::And
                            } else {
                                PyBinaryOp::Or
                            },
                            py_lhs,
                            py_rhs.value,
                        );
                    }

                    // rhs has pre, so we need to manually short-circuit
                    let temp_var = ctx.create_aux_var("bool", span.start);
                    let (short_condition, short_value) = if is_and {
                        (a.not(py_lhs), a.bool(false))
                    } else {
                        (py_lhs, a.bool(true))
                    };

                    let mut else_block = PyBlock::new();
                    let rhs = else_block.bind(py_rhs);
                    else_block.push(a.assign(a.ident(temp_var.clone(), PyAccessCtx::Store), rhs));

                    pre.push(a.if_(
                        short_condition,
                        PyBlock(vec![a.assign(
                            a.ident(temp_var.clone(), PyAccessCtx::Store),
                            short_value,
                        )]),
                        Some(else_block),
                    ));

                    break 'block a.load_ident(temp_var);
                }

                let (lhs, rhs) = (lhs.transform(ctx)?, rhs.transform(ctx)?);

                let lhs = pre.bind(lhs);
                let rhs = pre.bind(rhs);

                let py_op = match op {
                    BinaryOp::Pipe => break 'block a.call(rhs, vec![PyCallItem::Arg(lhs)]),
                    BinaryOp::Coalesce | BinaryOp::And | BinaryOp::Or => {
                        panic!()
                    }
                    _ => map_py_binary_op(*op, span)?,
                };

                a.binary(py_op, lhs, rhs)
            }
            Expr::Memo(expr, is_async) => {
                let memo_captures =
                    ctx.memo_fninfo.get(&expr.as_ref().into()).ok_or_else(|| {
                        simple_err(
                            "Internal error: Memo expression not found in memo captures",
                            expr.span,
                        )
                    })?;

                if memo_captures.is_generator {
                    return Err(simple_err("Cannot yield inside a memo", expr.span));
                }

                if memo_captures.is_async {
                    return Err(simple_err("Cannot await inside a memo", expr.span));
                }

                let py_expr = expr.transform(ctx)?;
                let mut py_body = PyBlock::new();

                let (py_bindings, _nonlocals, _globals) = py_fn_bindings(ctx, memo_captures, span)?;

                let deps_set = memo_captures
                    .indirect_captures
                    .iter()
                    .chain(memo_captures.captures.iter())
                    .collect::<HashSet<_>>();

                let mut nonglobal_deps = vec![];

                for capture in deps_set {
                    let decl = &ctx.declarations[*capture];
                    let decl_scope = &ctx.scopes[decl.scope];
                    let py_name = ctx.decl_py_ident(*capture)?;

                    if !decl_scope.is_global {
                        nonglobal_deps.push(py_name.clone());
                    }
                }

                py_body.extend(py_bindings);
                let py_expr = py_body.bind(py_expr);

                let callback = pre.bind(make_fn_exp(
                    ctx,
                    FnDef::PyFnDef(
                        PyArgList::empty(),
                        WithPre {
                            pre: py_body,
                            value: py_expr,
                        },
                        memo_captures.is_do,
                        memo_captures.is_async,
                    ),
                    &span,
                )?);

                let linecol = ctx.line_cache.linecol(span.start);

                let memo_call = a.call(
                    if *is_async {
                        a.tl_builtin("async_memo_value")
                    } else {
                        a.tl_builtin("memo_value")
                    },
                    vec![
                        PyCallItem::Arg(a.str(format!(
                            "{}:{}:{}:{:08x}",
                            ctx.filename,
                            linecol.0,
                            linecol.1,
                            {
                                let mut hasher = std::collections::hash_map::DefaultHasher::new();
                                ctx.source[span.start..span.end].hash(&mut hasher);
                                hasher.finish() as u32
                            }
                        ))),
                        PyCallItem::Arg(
                            a.tuple(
                                nonglobal_deps
                                    .into_iter()
                                    .map(|x| a.list_item(a.load_ident(x)))
                                    .collect(),
                                PyAccessCtx::Load,
                            ),
                        ),
                        PyCallItem::Arg(callback),
                    ],
                );

                a.yield_(memo_call)
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

                a.call(
                    a.tl_builtin("record_literal"),
                    vec![a.call_arg(a.dict(dict_items))],
                )
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
                    // Nested strings are not allowed in f-strings pre 3.12,
                    // so lift the expression part out
                    let expr = if ctx.target_version >= (3, 12) {
                        pre.bind(fmt_expr.expr.transform(ctx)?)
                    } else {
                        let e = pre.bind(fmt_expr.expr.transform(ctx)?);
                        pre.bind(deduplicate(ctx, e, fmt_expr.expr.span)?)
                    };

                    let fmt = fmt_expr
                        .fmt
                        .as_ref()
                        .map(|x| -> TlResult<_> { Ok(pre.bind(x.transform(ctx)?)) })
                        .transpose()?;

                    nodes.push(PyFstrPart::Expr(expr, fmt));
                    nodes.push(PyFstrPart::Str(str_part.value.clone().into()));
                }

                a.fstr(nodes)
            }
        };

        Ok(SPyExprWithPre { value, pre })
    }
}

fn create_coalesce<'src, 'ast>(
    ctx: &mut TlCtx<'src, 'ast>,
    py_lhs: PySpanned<PyExpr<'src>>,
    rhs: &'ast SExpr<'src>,
    span: Span,
) -> TlResult<SPyExprWithPre<'src>> {
    let a = PyAstBuilder::new(span);
    let mut pre = PyBlock::new();

    let fn_info = ctx.coal_fninfo.get(&rhs.into()).unwrap();

    let py_body = rhs.transform(ctx)?;

    let inner_fn = pre.bind(make_fn_exp(
        ctx,
        FnDef::PyFnDef(PyArgList::empty(), py_body, false, fn_info.is_async),
        &span,
    )?);

    let mut call = a.call(
        a.tl_builtin("op_coal"),
        vec![a.call_arg(py_lhs), a.call_arg(inner_fn)],
    );

    if fn_info.is_async {
        call = a.await_(call);
    }
    if fn_info.is_do || fn_info.is_generator {
        call = a.yield_from(call);
    }

    Ok(SPyExprWithPre { value: call, pre })
}

fn map_py_binary_op(op: BinaryOp, span: Span) -> TlResult<PyBinaryOp> {
    Ok(match op {
        BinaryOp::Add => PyBinaryOp::Add,
        BinaryOp::Sub => PyBinaryOp::Sub,
        BinaryOp::Mul => PyBinaryOp::Mult,
        BinaryOp::Div => PyBinaryOp::Div,
        BinaryOp::FloorDiv => PyBinaryOp::FloorDiv,
        BinaryOp::Mod => PyBinaryOp::Mod,
        BinaryOp::Exp => PyBinaryOp::Pow,
        BinaryOp::MatMul => PyBinaryOp::MatMult,

        BinaryOp::Lt => PyBinaryOp::Lt,
        BinaryOp::Gt => PyBinaryOp::Gt,
        BinaryOp::Leq => PyBinaryOp::Leq,
        BinaryOp::Geq => PyBinaryOp::Geq,
        BinaryOp::Eq => PyBinaryOp::Eq,
        BinaryOp::Neq => PyBinaryOp::Neq,
        BinaryOp::Is => PyBinaryOp::Is,
        BinaryOp::Nis => PyBinaryOp::Nis,
        BinaryOp::In => PyBinaryOp::In,
        BinaryOp::Nin => PyBinaryOp::Nin,

        BinaryOp::BitAnd => PyBinaryOp::BitAnd,
        BinaryOp::BitOr => PyBinaryOp::BitOr,
        BinaryOp::BitXor => PyBinaryOp::BitXor,
        BinaryOp::LShift => PyBinaryOp::LShift,
        BinaryOp::RShift => PyBinaryOp::RShift,

        _ => return Err(simple_err("Internal error: Unsupported binary op", span)),
    })
}

fn py_fn_bindings<'src, 'ast>(
    ctx: &mut TlCtx<'src, 'ast>,
    memo_captures: &FnInfo,
    span: Span,
) -> TlResult<(PyBlock<'src>, Vec<PyToken<'src>>, Vec<PyToken<'src>>)> {
    let a = PyAstBuilder::new(span);

    let mut nonlocals = vec![];
    let mut globals = vec![];
    let mut py_body = PyBlock::new();

    for capture in memo_captures.captures.iter() {
        let mut scope = &ctx.scopes[ctx.declarations[*capture].scope];
        loop {
            let Some(parent) = scope.parent else {
                break;
            };

            if scope.is_fn || scope.is_class || scope.is_global {
                break;
            }

            scope = &ctx.scopes[parent];
        }

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

    Ok((py_body, nonlocals, globals))
}

pub struct TransformOptions {
    pub interactive: bool,
    pub target_version: (usize, usize),
}

pub struct TransformOutput<'src> {
    pub py_block: PyBlock<'src>,
    pub exports: Vec<PyToken<'src>>,
    pub module_star_exports: Vec<PyToken<'src>>,
}

pub fn transform_ast<'src, 'ast>(
    source: &'src str,
    filename: &'src str,
    block: &'ast SExpr<'src>,
    resolve_state: &'ast ResolveState<'src>,
    inference: &'ast InferenceCtx<'src, 'ast>,
    options: TransformOptions,
) -> TlResult<TransformOutput<'src>> {
    let mut ctx = TlCtx::new(source, filename, resolve_state, inference)?;
    ctx.target_version = options.target_version;

    let py_block = if options.interactive {
        let mut py_value = block.transform(&mut ctx)?;
        let span = py_value.value.tl_span;
        py_value
            .pre
            .push((PyStmt::Expr(py_value.value), span).into());
        py_value.pre
    } else {
        block.transform(&mut ctx)?.drop_expr(&mut ctx)
    };

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
