use std::borrow::Cow;

use crate::{
    linecol::LineColCache,
    py::{ast::*, util::PyAstBuilder},
};
use parser::{ast::*, util::AstBuilder};

#[derive(Debug)]
pub struct TfErr {
    pub message: String,
    pub span: Option<Span>,
}

#[derive(Debug)]
pub struct TfErrs(pub Vec<TfErr>);

impl TfErrs {
    pub fn new() -> Self {
        TfErrs(vec![])
    }

    pub fn extend(&mut self, other: TfErrs) {
        self.0.extend(other.0);
    }
}

#[derive(Default)]
pub struct TfErrBuilder {
    message: String,
    span: Option<Span>,
}

impl TfErrBuilder {
    pub fn span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn message<S: Into<String>>(mut self, message: S) -> Self {
        self.message = message.into();
        self
    }

    pub fn build(self) -> TfErr {
        TfErr {
            message: self.message,
            span: self.span,
        }
    }

    pub fn build_errs(self) -> TfErrs {
        TfErrs(vec![self.build()])
    }
}

pub type TfResult<T> = Result<T, TfErrs>;

#[allow(dead_code)]
struct TfCtx<'src> {
    source: &'src str,
    exports: Vec<PyIdent<'src>>,
    module_star_exports: Vec<PyIdent<'src>>,

    line_cache: LineColCache,
    placeholder_ctx_stack: Vec<PlaceholderCtx>,
}

impl<'src> TfCtx<'src> {
    fn new(source: &'src str) -> TfResult<Self> {
        Ok(TfCtx {
            source,
            line_cache: LineColCache::new(source),
            exports: Vec::new(),
            module_star_exports: Vec::new(),
            placeholder_ctx_stack: Vec::new(),
        })
    }

    fn linecol(&self, cursor: usize) -> (usize, usize) {
        self.line_cache.linecol(cursor)
    }

    fn temp_var_name(&self, typ: &str, cursor: usize) -> String {
        let (line, col) = self.linecol(cursor);
        format!("__tl_{}_l{}c{}", typ, line, col)
    }
}

struct WithPre<'src, T> {
    pre: PyBlock<'src>,
    value: T,
}

type SPyExprWithPre<'src> = WithPre<'src, SPyExpr<'src>>;

trait SBlockExt<'src> {
    fn transform<'ast>(&'ast self, ctx: &mut TfCtx<'src>) -> TfResult<SPyExprWithPre<'src>>;

    fn transform_with_final_stmt<'ast>(
        &'ast self,
        ctx: &mut TfCtx<'src>,
    ) -> TfResult<PyBlock<'src>>;

    fn transform_with_depth<'ast>(
        &'ast self,
        ctx: &mut TfCtx<'src>,
        is_top_level: bool,
    ) -> TfResult<SPyExprWithPre<'src>>;
}

impl<'src> SBlockExt<'src> for SBlock<'src> {
    fn transform<'ast>(&'ast self, ctx: &mut TfCtx<'src>) -> TfResult<SPyExprWithPre<'src>> {
        self.transform_with_depth(ctx, false)
    }

    fn transform_with_final_stmt<'ast>(
        &'ast self,
        ctx: &mut TfCtx<'src>,
    ) -> TfResult<PyBlock<'src>> {
        let mut t = self.transform(ctx)?;
        match t.value.value {
            PyExpr::Literal(..) | PyExpr::Ident(..) => {}
            _ => {
                t.pre.push((PyStmt::Expr(t.value), self.1).into());
            }
        }
        Ok(t.pre)
    }

    fn transform_with_depth<'ast>(
        &'ast self,
        ctx: &mut TfCtx<'src>,
        is_top_level: bool,
    ) -> TfResult<SPyExprWithPre<'src>> {
        let (block, _span) = self;

        match block {
            Block::Stmts(stmts) => {
                if stmts.is_empty() {
                    return Ok(WithPre {
                        pre: PyBlock::new(),
                        value: (PyExpr::Literal(PyLiteral::None), self.1).into(),
                    });
                }

                let mut py_stmts = PyBlock::new();
                let mut errs = Vec::new();
                let mut ok = true;

                let mut handle_stmt = |stmt: &SStmt<'src>| {
                    match stmt.transform_with_depth(ctx, is_top_level) {
                        Ok(transformed) => {
                            py_stmts.extend(transformed);
                        }
                        Err(e) => {
                            errs.extend(e.0);
                            ok = false;
                        }
                    };
                };

                let mut iter = stmts.iter();

                for stmt in iter.by_ref().take(stmts.len() - 1) {
                    handle_stmt(stmt);
                }

                let final_stmt = iter.next().unwrap();
                let mut final_expr = None;

                match &final_stmt.0 {
                    Stmt::Expr(expr) => match expr.transform_with_placeholder_guard(ctx) {
                        Ok(expr_with_aux) => {
                            py_stmts.extend(expr_with_aux.pre);
                            final_expr = Some(expr_with_aux.value);
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
                    Ok(WithPre {
                        pre: py_stmts,
                        value: final_expr
                            .unwrap_or((PyExpr::Literal(PyLiteral::None), self.1).into()),
                    })
                } else {
                    Err(TfErrs(errs))
                }
            }
            Block::Expr(sexpr) => sexpr.transform(ctx),
        }
    }
}

fn destructure_list<'src, 'ast>(
    ctx: &mut TfCtx<'src>,
    target: &'ast SExpr<'src>,
    items: &'ast [ListItem<'src>],
    decl_only: bool,
) -> TfResult<DestructureBindings<'src>> {
    let cursor_var = ctx.temp_var_name("des_curs", target.1.start);

    // a, b, *c = cursor_var

    let a = PyAstBuilder::new(target.1);
    let mut post_stmts = PyBlock::new();

    let mut lhs_items = vec![];
    let mut decls = vec![];
    let mut seen_spread = false;

    for item in items.iter() {
        match item {
            ListItem::Item(expr) => {
                let item_bindings = destructure(ctx, expr, decl_only)?;
                lhs_items.push(PyListItem::Item(item_bindings.assign_to));
                post_stmts.extend(item_bindings.post_stmts);
                decls.extend(item_bindings.declarations);
            }
            ListItem::Spread(expr) => {
                if seen_spread {
                    return Err(TfErrBuilder::default()
                        .message("Destructuring assignment with multiple spreads is not allowed")
                        .span(target.1)
                        .build_errs());
                }
                seen_spread = true;

                let item_bindings = destructure(ctx, expr, decl_only)?;
                lhs_items.push(PyListItem::Spread(item_bindings.assign_to));
                post_stmts.extend(item_bindings.post_stmts);
                decls.extend(item_bindings.declarations);
            }
        }
    }

    let mut stmts = PyBlock::new();
    stmts.push(a.assign(
        a.list(lhs_items, PyAccessCtx::Store),
        a.load_ident(cursor_var.clone()),
    ));

    stmts.extend(post_stmts);

    Ok(DestructureBindings {
        post_stmts: stmts,
        assign_to: a.ident(cursor_var, PyAccessCtx::Store),
        declarations: decls,
    })
}

fn destructure_tuple<'src, 'ast>(
    ctx: &mut TfCtx<'src>,
    target: &'ast SExpr<'src>,
    items: &'ast [ListItem<'src>],
    decl_only: bool,
) -> TfResult<DestructureBindings<'src>> {
    let cursor_var = ctx.temp_var_name("des_curs", target.1.start);
    let tuple_var = ctx.temp_var_name("des_tuple", target.1.start);
    let len_var = ctx.temp_var_name("des_len", target.1.start);

    // list_var = tuple(cursor_var)
    // len_var = len(list_var)

    let a = PyAstBuilder::new(target.1);

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
    let mut decls = vec![];

    // a = list_var[0]
    // b = list_var[1]
    // c = list_var[i:len_var-n_single_spreads_left]

    let mut seen_spread = false;
    let mut i = 0;

    for item in items.iter() {
        match item {
            ListItem::Item(expr) => {
                let item_bindings = destructure(ctx, expr, decl_only)?;
                post_stmts.extend(item_bindings.post_stmts);
                decls.extend(item_bindings.declarations);

                stmts.push(
                    a.assign(
                        item_bindings.assign_to,
                        a.subscript(
                            a.load_ident(tuple_var.clone()),
                            a.num(
                                (if seen_spread {
                                    -((items.len() - i - 1) as i32)
                                } else {
                                    i as i32
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
                    return Err(TfErrBuilder::default()
                        .message("Destructuring assignment with multiple spreads is not allowed")
                        .span(target.1)
                        .build_errs());
                }
                seen_spread = true;

                let item_bindings = destructure(ctx, expr, true)?;
                post_stmts.extend(item_bindings.post_stmts);
                decls.extend(item_bindings.declarations);

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
        post_stmts: stmts,
        assign_to: a.ident(cursor_var, PyAccessCtx::Store),
        declarations: decls,
    })
}

fn destructure_mapping<'src, 'ast>(
    ctx: &mut TfCtx<'src>,
    target: &'ast SExpr<'src>,
    items: &'ast [MappingItem<'src>],
    decl_only: bool,
) -> TfResult<DestructureBindings<'src>> {
    let cursor_var = ctx.temp_var_name("des_curs", target.1.start);
    let dict_var = ctx.temp_var_name("des_dict", target.1.start);

    // dict_var = dict(cursor_var)
    let a = PyAstBuilder::new(target.1);
    let mut stmts = PyBlock(vec![a.assign(
        a.ident(dict_var.clone(), PyAccessCtx::Store),
        a.call(
            a.load_ident("dict"),
            vec![a.call_arg(a.load_ident(cursor_var.clone()))],
        ),
    )]);

    let mut post_stmts = vec![];
    let mut decls = vec![];

    // a = dict_var.pop(a_key)
    // b = dict_var.pop(b_key)
    // c = dict_var

    let mut spread_var = None;
    for item in items.iter() {
        match item {
            MappingItem::Item(key, expr) => {
                let item_bindings = destructure(ctx, expr, decl_only)?;
                let key_node = key.transform(ctx)?;
                post_stmts.extend(key_node.pre);
                post_stmts.extend(item_bindings.post_stmts);
                decls.extend(item_bindings.declarations);

                stmts.push(a.assign(
                    item_bindings.assign_to,
                    a.call(
                        a.attribute(a.load_ident(dict_var.clone()), "pop", PyAccessCtx::Load),
                        vec![a.call_arg(key_node.value)],
                    ),
                ));
            }
            MappingItem::Spread(expr) => {
                if spread_var.is_some() {
                    return Err(TfErrBuilder::default()
                        .message("Destructuring assignment with multiple spreads is not allowed")
                        .span(target.1)
                        .build_errs());
                }

                spread_var = Some(expr);
            }
        }
    }

    if let Some(spread_var) = spread_var {
        let item_bindings = destructure(ctx, spread_var, true)?;

        post_stmts.extend(item_bindings.post_stmts);
        decls.extend(item_bindings.declarations);

        stmts.push(a.assign(item_bindings.assign_to, a.load_ident(dict_var.clone())));
    }

    stmts.extend(post_stmts);

    Ok(DestructureBindings {
        post_stmts: stmts,
        assign_to: a.ident(cursor_var, PyAccessCtx::Store),
        declarations: decls,
    })
}

struct DestructureBindings<'a> {
    assign_to: SPyExpr<'a>,
    post_stmts: PyBlock<'a>,
    declarations: Vec<PyIdent<'a>>,
}

fn destructure<'src, 'ast>(
    ctx: &mut TfCtx<'src>,
    target: &'ast SExpr<'src>,
    decl_only: bool,
) -> TfResult<DestructureBindings<'src>> {
    let mut post_stmts = PyBlock::new();
    let mut decls = Vec::<PyIdent<'src>>::new();

    let assign_to: SPyExpr<'src>;

    match &target.0 {
        Expr::Ident(..) | Expr::Attribute(..) | Expr::Subscript(..) => {
            match &target.0 {
                Expr::Ident(id) => {
                    decls.push(id.0.to_owned().into());
                }
                Expr::Attribute(..) | Expr::Subscript(..) => {
                    if decl_only {
                        return Err(TfErrBuilder::default()
                            .message("Only identifiers allowed in this destructuring")
                            .span(target.1)
                            .build_errs());
                    }
                }
                _ => {
                    panic!();
                }
            }

            let target_node = target.transform_with_access(ctx, PyAccessCtx::Store)?;
            post_stmts.extend(target_node.pre);

            assign_to = target_node.value;
        }
        Expr::List(items) => {
            let bindings = destructure_list(ctx, target, items, decl_only)?;

            post_stmts.extend(bindings.post_stmts);
            decls.extend(bindings.declarations);
            assign_to = bindings.assign_to;
        }
        Expr::Tuple(items) => {
            let bindings = destructure_tuple(ctx, target, items, decl_only)?;

            post_stmts.extend(bindings.post_stmts);
            decls.extend(bindings.declarations);
            assign_to = bindings.assign_to;
        }
        Expr::Mapping(items) => {
            let bindings = destructure_mapping(ctx, target, items, decl_only)?;

            post_stmts.extend(bindings.post_stmts);
            decls.extend(bindings.declarations);
            assign_to = bindings.assign_to;
        }
        _ => {
            return Err(TfErrBuilder::default()
                .message("Assignment target is not allowed")
                .span(target.1)
                .build_errs());
        }
    };

    Ok(DestructureBindings {
        post_stmts,
        assign_to,
        declarations: decls,
    })
}

fn get_scope_modifier<'a>(
    mods: &'a Vec<AssignModifier>,
    is_top_level: bool,
    span: &Span,
) -> TfResult<Option<&'a AssignModifier>> {
    let scope_modifier = mods
        .iter()
        .filter(|m| {
            matches!(
                m,
                AssignModifier::Export | AssignModifier::Global | AssignModifier::Nonlocal
            )
        })
        .collect::<Vec<_>>();

    if scope_modifier.len() > 1 {
        return Err(TfErrBuilder::default()
            .message("Only one scope modifier is allowed in an assignment")
            .span(*span)
            .build_errs());
    }

    let scope_modifier = scope_modifier.first().map(|x| *x);

    if !is_top_level && scope_modifier.is_some_and(|x| *x == AssignModifier::Export) {
        return Err(TfErrBuilder::default()
            .message("Export modifier is only allowed at the top level")
            .span(*span)
            .build_errs());
    }

    Ok(scope_modifier)
}

fn get_scope_modifying_statements<'a>(
    ctx: &mut TfCtx<'a>,
    scope_modifier: Option<&AssignModifier>,
    decls: Vec<PyIdent<'a>>,
    span: &Span,
) -> TfResult<PyBlock<'a>> {
    let mut stmts = PyBlock::new();

    if let Some(scope_modifier) = scope_modifier {
        match scope_modifier {
            AssignModifier::Export => ctx.exports.extend(decls),
            AssignModifier::Global => {
                // exports are implemented by lifting into global scope

                stmts.push((PyStmt::Global(decls), *span).into());
            }

            AssignModifier::Nonlocal => {
                stmts.push((PyStmt::Nonlocal(decls), *span).into());
            }
        }
    }

    Ok(stmts)
}

fn transform_assignment<'src, 'ast>(
    ctx: &mut TfCtx<'src>,
    lhs: &'ast SExpr<'src>,
    rhs: &'ast SExpr<'src>,
    scope_modifier: Option<&AssignModifier>,
    span: &Span,
) -> TfResult<(PyBlock<'src>, Vec<PyIdent<'src>>)> {
    let mut stmts = PyBlock::new();
    if let Expr::Ident((ident, _ident_span)) = &lhs.0 {
        let mut decorators = vec![];
        let mut cur_node = &rhs.0;

        loop {
            match cur_node {
                Expr::Then(left, right) => {
                    cur_node = &left.0;
                    decorators.push(right);
                }
                Expr::Binary(BinaryOp::Pipe, left, right) => {
                    cur_node = &left.0;
                    decorators.push(right);
                }
                Expr::Call(left, right) => {
                    if right.len() != 1 {
                        break;
                    }

                    match &right[0].0 {
                        CallItem::Arg(arg) => {
                            cur_node = &arg.0;
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

        let py_decorators = || {
            Ok(PyDecorators(
                decorators
                    .into_iter()
                    .map(|x| {
                        let t = x.transform_with_placeholder_guard(ctx)?;
                        stmts.extend(t.pre);
                        Ok(t.value)
                    })
                    .collect::<TfResult<_>>()?,
            ))
        };

        if let Expr::Fn(arglist, body) = &cur_node {
            let decorators = py_decorators()?;
            return Ok((
                make_fn_def(
                    ctx,
                    ident.clone(),
                    FnDefArgs::ArgList(arglist),
                    FnDefBody::Block(body),
                    decorators,
                    span,
                )?,
                vec![ident.clone()],
            ));
        } else if let Expr::Class(bases, body) = &cur_node {
            let decorators = py_decorators()?;
            return Ok((
                make_class_def(ctx, ident.clone(), &bases, &body, decorators, span)?,
                vec![ident.clone()],
            ));
        };
    };

    let value_node = rhs.transform_with_placeholder_guard(ctx)?;
    stmts.extend(value_node.pre);

    let decl_only = scope_modifier.is_some();
    let destructure = destructure(ctx, lhs, decl_only)?;

    stmts.push(
        (
            PyStmt::Assign(destructure.assign_to, value_node.value),
            lhs.1,
        )
            .into(),
    );
    stmts.extend(destructure.post_stmts);

    Ok((stmts, destructure.declarations))
}

trait SStmtExt<'src> {
    fn transform_with_depth<'ast>(
        &'ast self,
        ctx: &mut TfCtx<'src>,
        top_level: bool,
    ) -> TfResult<PyBlock<'src>>;
}

impl<'src> SStmtExt<'src> for SStmt<'src> {
    fn transform_with_depth<'ast>(
        &'ast self,
        ctx: &mut TfCtx<'src>,
        top_level: bool,
    ) -> TfResult<PyBlock<'src>> {
        let (stmt, span) = self;
        let a = PyAstBuilder::new(*span);

        match &stmt {
            Stmt::Expr(expr) => {
                let expr = expr.transform_with_placeholder_guard(ctx)?;
                let mut stmts = expr.pre;
                stmts.push((PyStmt::Expr(expr.value), *span).into());

                Ok(stmts)
            }
            Stmt::Assert(expr, msg) => {
                let expr_node = expr.transform_with_placeholder_guard(ctx)?;
                let msg = msg
                    .as_ref()
                    .map(|x| x.transform_with_placeholder_guard(ctx))
                    .transpose()?;

                let mut stmts = expr_node.pre;
                let mut msg_node = None;
                if let Some(msg) = msg {
                    stmts.extend(msg.pre);
                    msg_node = Some(msg.value);
                }

                stmts.push((PyStmt::Assert(expr_node.value, msg_node), *span).into());

                Ok(stmts)
            }
            Stmt::Return(expr) => {
                let value = expr.transform_with_placeholder_guard(ctx)?;
                let mut stmts = value.pre;

                stmts.push((PyStmt::Return(value.value), *span).into());

                Ok(stmts)
            }
            Stmt::Assign(target, value, modifiers) => {
                let scope_modifier = get_scope_modifier(modifiers, top_level, span)?;

                let (binding_stmts, decls): (PyBlock, Vec<PyIdent>) =
                    transform_assignment(ctx, target, value, scope_modifier, span)?;

                let mut stmts = PyBlock::new();

                stmts.extend(get_scope_modifying_statements(
                    ctx,
                    scope_modifier,
                    decls,
                    span,
                )?);
                stmts.extend(binding_stmts);

                Ok(stmts)
            }
            Stmt::Raise(expr) => {
                let mut stmts = PyBlock::new();
                let expr_node = expr
                    .as_ref()
                    .map(|x| {
                        let t = x.transform(ctx)?;
                        stmts.extend(t.pre);
                        Ok(t.value)
                    })
                    .transpose()?;

                stmts.push((PyStmt::Raise(expr_node), *span).into());

                Ok(stmts)
            }
            Stmt::For(target, iter, body) => {
                let iter_node: SPyExprWithPre<'src> = iter.transform_with_placeholder_guard(ctx)?;
                let aux_stmts = iter_node.pre;

                let mut block = aux_stmts;
                let mut body_block = PyBlock::new();

                let (matcher, cursor) = create_throwing_matcher(ctx, target)?;
                body_block.extend(matcher);
                body_block.extend(body.transform_with_final_stmt(ctx)?);

                block.push(a.for_(
                    a.ident(cursor.clone(), PyAccessCtx::Store),
                    a.call(
                        a.load_ident("__vtable_lookup"),
                        vec![
                            a.call_arg(iter_node.value),
                            a.call_arg(a.literal(PyLiteral::Str("iter".into()))),
                        ],
                    ),
                    body_block,
                ));

                Ok(block)
            }
            Stmt::While(cond, body) => {
                let cond_node = cond.transform_with_placeholder_guard(ctx)?;
                let body_block = body.transform_with_final_stmt(ctx)?;

                let mut stmts = PyBlock::new();

                let cond: SPyExpr<'src> = if cond_node.pre.is_empty() {
                    cond_node.value
                } else {
                    let aux_fn = make_fn_exp(
                        ctx,
                        FnDefArgs::PyArgList(vec![]),
                        FnDefBody::PyStmts(cond_node.pre),
                        span,
                    )?;

                    stmts.extend(aux_fn.pre);

                    (PyExpr::Call(Box::new(aux_fn.value), vec![]), *span).into()
                };

                stmts.push((PyStmt::While(cond, body_block), *span).into());

                Ok(stmts)
            }
            Stmt::Try(body, excepts, finally) => {
                let body_block = body.transform_with_final_stmt(ctx)?;
                let finally_block = finally
                    .as_ref()
                    .map(|f| f.transform_with_final_stmt(ctx))
                    .transpose()?;

                let mut stmts = PyBlock::new();

                let excepts = matching_except_handler(ctx, excepts, span)?;

                stmts.push((PyStmt::Try(body_block, vec![excepts], finally_block), *span).into());

                Ok(stmts)
            }
            Stmt::Break => Ok(PyBlock(vec![(PyStmt::Break, *span).into()])),
            Stmt::Continue => Ok(PyBlock(vec![(PyStmt::Continue, *span).into()])),
            Stmt::Import(import_stmt) => {
                let mut aliases = vec![];

                let base_module = import_stmt
                    .trunk
                    .iter()
                    .map(|ident| ident.0.as_ref())
                    .collect::<Vec<_>>()
                    .join(".");

                let full_module = ".".repeat(import_stmt.level) + &base_module;

                if import_stmt.reexport {
                    if !top_level {
                        return Err(TfErrBuilder::default()
                            .message("Re-exporting imports is only allowed at the top level")
                            .span(*span)
                            .build_errs());
                    }
                }

                match &import_stmt.imports {
                    ImportList::Star => {
                        aliases.push(PyImportAlias {
                            name: "*".into(),
                            as_name: None,
                        });

                        if import_stmt.reexport {
                            ctx.module_star_exports.push(full_module.into());
                        }
                    }
                    ImportList::Leaves(imports) => {
                        for (ident, alias) in imports {
                            aliases.push(PyImportAlias {
                                name: ident.0.clone(),
                                as_name: alias.as_ref().map(|a| a.0.clone()),
                            });
                        }

                        if import_stmt.reexport {
                            // alias else orig_name
                            ctx.exports.extend(
                                aliases
                                    .iter()
                                    .map(|x| x.as_name.clone().unwrap_or(x.name.clone())),
                            );
                        }
                    }
                }

                let py_import: SPyStmt = if !import_stmt.trunk.is_empty() {
                    a.import_from(Some(base_module.into()), aliases, import_stmt.level)
                } else if import_stmt.level != 0 {
                    a.import_from(None, aliases, import_stmt.level)
                } else {
                    a.import(aliases)
                };

                Ok(PyBlock(vec![py_import]))
            }
            Stmt::Err => Err(TfErrBuilder::default()
                .message("unexpected statement error (should have been caught in lexer)".to_owned())
                .span(*span)
                .build_errs()),
            Stmt::Module => Err(TfErrBuilder::default()
                .message("Module statements are not allowed in the transform phase".to_owned())
                .span(*span)
                .build_errs()),
        }
    }
}

fn transform_if_expr<'src, 'ast>(
    ctx: &mut TfCtx<'src>,
    cond: &'ast SExpr<'src>,
    then_block: &'ast SBlock<'src>,
    else_block: &'ast Option<Box<SBlock<'src>>>,
    span: &Span,
) -> TfResult<SPyExprWithPre<'src>> {
    let cond = cond.transform(ctx)?;
    let mut aux_stmts = cond.pre;
    let a = PyAstBuilder::new(*span);

    let ret_varname = ctx.temp_var_name("ifexp", span.start);
    let store_ret_var: SPyExpr = (
        PyExpr::Ident(ret_varname.clone().into(), PyAccessCtx::Store),
        *span,
    )
        .into();
    let load_ret_var: SPyExpr = (
        PyExpr::Ident(ret_varname.clone().into(), PyAccessCtx::Load),
        *span,
    )
        .into();

    let t = then_block.transform(ctx)?;
    let mut then_block_ast = t.pre;
    then_block_ast.push(a.assign(store_ret_var.clone(), t.value));

    let mut else_block = else_block
        .as_ref()
        .map(|else_block| {
            let t = else_block.transform(ctx)?;
            let mut else_block_ast = t.pre;
            else_block_ast.push(a.assign(store_ret_var.clone(), t.value));
            Ok(else_block_ast)
        })
        .transpose()?;

    if else_block.is_none() {
        else_block = Some(PyBlock(vec![
            a.assign(store_ret_var.clone(), a.literal(PyLiteral::None)),
        ]));
    }

    aux_stmts.push((PyStmt::If(cond.value, then_block_ast, else_block), *span).into());

    Ok(SPyExprWithPre {
        value: load_ret_var,
        pre: aux_stmts,
    })
}

fn bind_pre<'src, 'a, T>(pre: &'a mut PyBlock<'src>, v: WithPre<'src, T>) -> T {
    pre.extend(v.pre);
    v.value
}

trait SPatternExt<'src> {
    fn transform<'ast>(
        &'ast self,
        ctx: &mut TfCtx<'src>,
    ) -> TfResult<WithPre<'src, SPyPattern<'src>>>;
}

impl<'src> SPatternExt<'src> for SPattern<'src> {
    fn transform<'ast>(
        &'ast self,
        ctx: &mut TfCtx<'src>,
    ) -> TfResult<WithPre<'src, SPyPattern<'src>>> {
        // TODO avoid python syntaxerror by verifying that all branches in Or bind the same name
        // also check for no patterns after default pattern

        let mut pre = PyBlock::new();
        let mut attach = |v| bind_pre(&mut pre, v);

        let (pattern, span) = self;
        let capture_slot = |v: &Option<SIdent<'src>>| {
            v.clone()
                .and_then(|x| if x.0 == "_" { None } else { Some(x.0) })
        };

        let transformed = match pattern {
            Pattern::As(pattern, ident) => PyPattern::As(
                Some(Box::new(attach(pattern.transform(ctx)?))),
                Some(ident.0.clone()),
            ),
            Pattern::Literal(literal) => match literal.0 {
                Literal::Num(..) | Literal::Str(..) => {
                    PyPattern::Value((PyExpr::Literal(literal.0.transform(ctx)?), *span).into())
                }
                Literal::Bool(..) | Literal::None => {
                    PyPattern::Singleton(literal.0.transform(ctx)?)
                }
            },
            Pattern::Capture(v) => {
                if v.as_ref()
                    .is_some_and(|x| char::is_uppercase(x.0.chars().nth(0).unwrap_or('_')))
                {
                    return Err(TfErrBuilder::default()
                        .message("Capture patterns must start with a lowercase letter; to match a type, add '()'")
                        .span(*span)
                        .build_errs());
                }

                PyPattern::As(None, capture_slot(v))
            }
            Pattern::Value(v) => {
                let v_node = bind_pre(&mut pre, v.transform(ctx)?);

                match v_node.value {
                    PyExpr::Literal(..) | PyExpr::Attribute(..) => PyPattern::Value(v_node),
                    _ => {
                        let var = ctx.temp_var_name("mproxy", span.start);
                        let a = PyAstBuilder::new(*span);
                        pre.push(a.assign(
                            a.ident(var.clone(), PyAccessCtx::Store),
                            a.call(a.load_ident("__match_proxy"), vec![a.call_arg(v_node)]),
                        ));

                        PyPattern::Value(a.attribute(a.load_ident(var), "value", PyAccessCtx::Load))
                    }
                }
            }
            Pattern::Or(items) => {
                let items_nodes = items
                    .iter()
                    .map(|x| Ok(attach(x.transform(ctx)?)))
                    .collect::<TfResult<Vec<_>>>()?;
                PyPattern::Or(items_nodes)
            }
            Pattern::Sequence(items) => {
                let mut seen_spread = false;

                let items_nodes = items
                        .iter()
                        .map(|x| {
                            Ok(match x {
                                PatternSequenceItem::Item(item) => {
                                    PyPatternSequenceItem::Item(attach(item.transform(ctx)?))
                                }
                                PatternSequenceItem::Spread(item) => {
                                    if seen_spread {
                                        return Err(TfErrBuilder::default()
                                            .message("Destructuring assignment with multiple spreads is not allowed")
                                            .span(*span)
                                            .build_errs());
                                    }
                                    seen_spread = true;
                                    PyPatternSequenceItem::Spread(capture_slot(item))
                                }
                            })
                        })
                        .collect::<TfResult<Vec<_>>>()?;
                PyPattern::Sequence(items_nodes)
            }
            Pattern::Mapping(items) => {
                let mut kvps = vec![];
                let mut spread = None;
                for item in items {
                    match item {
                        PatternMappingItem::Item(key, value) => {
                            let value_node = value.transform(ctx)?;
                            pre.extend(value_node.pre);

                            let key_node = key.transform(ctx)?;
                            pre.extend(key_node.pre);

                            kvps.push((key_node.value, value_node.value));
                        }
                        PatternMappingItem::Spread(value) => {
                            if spread.is_some() {
                                return Err(TfErrBuilder::default()
                                        .message("Destructuring assignment with multiple spreads is not allowed")
                                        .span(*span)
                                        .build_errs());
                            }
                            spread = capture_slot(value);
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
                                return Err(TfErrBuilder::default()
                                    .message("Keyword patterns must come after positional patterns")
                                    .span(*span)
                                    .build_errs());
                            }
                            values.push(attach(value.transform(ctx)?));
                        }
                        PatternClassItem::Kw(key, value) => {
                            kvps.push((key.0.clone(), attach(value.transform(ctx)?)));
                        }
                    }
                }

                let cls_node = bind_pre(&mut pre, cls.transform(ctx)?);

                PyPattern::Class(cls_node, values, kvps)
            }
        };

        Ok(WithPre {
            pre,
            value: (transformed, *span).into(),
        })
    }
}

fn create_throwing_matcher<'src, 'ast>(
    ctx: &mut TfCtx<'src>,
    pattern: &'ast SPattern<'src>,
) -> TfResult<(PyBlock<'src>, PyIdent<'src>)> {
    if let Pattern::Capture(Some(id)) = &pattern.0 {
        return Ok((PyBlock::new(), id.0.clone()));
    }

    let cursor = ctx.temp_var_name("matcher", pattern.1.start);

    let a = PyAstBuilder::new(pattern.1);
    let success = PyBlock(vec![a.pass()]);
    let fail = PyBlock(vec![a.raise(Some(a.call(
        a.load_ident("MatchError"),
        vec![a.call_arg(a.fstr(vec![
            a.fstr_str("failed to match value "),
            a.fstr_expr(a.load_ident(cursor.clone()), None),
            a.fstr_str(format!(
                " to pattern {}",
                &ctx.source[pattern.1.start..pattern.1.end]
            )),
        ]))],
    )))]);

    Ok((
        create_matcher(ctx, a.load_ident(cursor.clone()), pattern, success, fail)?,
        cursor.clone().into(),
    ))
}

fn create_matcher<'src, 'ast>(
    ctx: &mut TfCtx<'src>,
    subject: SPyExpr<'src>,
    pattern: &'ast SPattern<'src>,
    on_success: PyBlock<'src>,
    on_fail: PyBlock<'src>,
) -> TfResult<PyBlock<'src>> {
    if is_default_pattern(pattern) {
        return Ok(on_success);
    }

    let a = PyAstBuilder::new(pattern.1);
    let pattern_node = pattern.transform(ctx)?;
    let mut post = PyBlock::new();

    post.push(a.match_(
        subject,
        vec![
            PyMatchCase {
                pattern: pattern_node.value,
                guard: None,
                body: on_success,
            },
            PyMatchCase {
                pattern: (PyPattern::As(None, None), pattern.1).into(),
                guard: None,
                body: on_fail,
            },
        ],
    ));

    Ok(post)
}

fn transform_match_expr<'src, 'ast>(
    ctx: &mut TfCtx<'src>,
    subject: &'ast SExpr<'src>,
    cases: &'ast [MatchCase<'src>],
    span: &Span,
) -> TfResult<(SPyExprWithPre<'src>, bool)> {
    let subject = subject.transform_with_placeholder_guard(ctx)?;
    let mut pre = subject.pre;
    let a = PyAstBuilder::new(*span);

    let ret_varname = ctx.temp_var_name("matchexp", span.start);
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
        if case.pattern.as_ref().is_none_or(is_default_pattern) && case.guard.is_none() {
            has_default_case = true;
        }

        // TODO verify binding same names

        let pattern = if let Some(pattern) = &case.pattern {
            bind_pre(&mut pre, pattern.transform(ctx)?)
        } else {
            (PyPattern::As(None, None), *span).into()
        };

        let guard = if let Some(guard) = &case.guard {
            Some(bind_pre(
                &mut pre,
                guard.transform_with_placeholder_guard(ctx)?,
            ))
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

    if !has_default_case {
        py_cases.push(PyMatchCase {
            pattern: (PyPattern::As(None, None), *span).into(),
            guard: None,
            body: PyBlock(vec![
                a.assign(store_ret_var.clone(), a.literal(PyLiteral::None)),
            ]),
        });

        has_default_case = true;
    }

    pre.push((PyStmt::Match(subject.value, py_cases), *span).into());

    Ok((
        SPyExprWithPre {
            value: load_ret_var,
            pre,
        },
        has_default_case,
    ))
}

fn make_class_def<'src, 'ast>(
    ctx: &mut TfCtx<'src>,
    name: Cow<'src, str>,
    bases: &'ast Vec<SCallItem<'src>>,
    body: &'ast Box<SBlock<'src>>,
    decorators: PyDecorators<'src>,
    span: &Span,
) -> TfResult<PyBlock<'src>> {
    let mut stmts = PyBlock::new();
    let mut bases_nodes: Vec<PyCallItem<'src>> = vec![];

    let mut block = body.transform_with_final_stmt(ctx)?;

    for base in bases {
        let call_item: PyCallItem<'src> = match &base.0 {
            CallItem::Arg(expr) => {
                let base_node = expr.transform_with_placeholder_guard(ctx)?;
                stmts.extend(base_node.pre);
                PyCallItem::Arg(base_node.value)
            }
            CallItem::Kwarg(name, expr) => {
                let expr_node = expr.transform_with_placeholder_guard(ctx)?;
                stmts.extend(expr_node.pre);
                PyCallItem::Kwarg(name.0.clone(), expr_node.value)
            }
            _ => {
                return Err(TfErrBuilder::default()
                    .message("spread args are not allowed in class bases")
                    .span(*span)
                    .build_errs());
            }
        };

        bases_nodes.push(call_item);
    }

    if block.is_empty() {
        block.push((PyStmt::Pass, *span).into());
    }

    stmts.push(
        (
            PyStmt::ClassDef(name, bases_nodes, block, decorators),
            *span,
        )
            .into(),
    );

    Ok(stmts)
}

enum FnDefBody<'src, 'ast> {
    PyStmts(PyBlock<'src>),
    Block(&'ast SBlock<'src>),
}

enum FnDefArgs<'src, 'ast> {
    ArgList(&'ast [ArgDefItem<'src>]),
    PyArgList(Vec<PyArgDefItem<'src>>),
}

fn make_arglist<'src, 'ast>(
    ctx: &mut TfCtx<'src>,
    arglist: FnDefArgs<'src, 'ast>,
) -> TfResult<(PyBlock<'src>, PyBlock<'src>, Vec<PyArgDefItem<'src>>)> {
    let mut pre = PyBlock::new();
    let mut post = PyBlock::new();

    let args = match arglist {
        FnDefArgs::ArgList(args) => {
            let mut args_vec = vec![];
            for arg in args {
                let arg = match arg {
                    ArgDefItem::Arg(arg, default) => {
                        let default = if let Some(default) = default {
                            let t = default.transform_with_placeholder_guard(ctx)?;
                            pre.extend(t.pre);
                            Some(t.value)
                        } else {
                            None
                        };

                        let (matcher, cursor) = create_throwing_matcher(ctx, arg)?;
                        post.extend(matcher);
                        PyArgDefItem::Arg(cursor, default)
                    }
                    ArgDefItem::ArgSpread(name) => PyArgDefItem::ArgSpread(name.0.clone()),
                    ArgDefItem::KwargSpread(name) => PyArgDefItem::KwargSpread(name.0.clone()),
                };
                args_vec.push(arg);
            }
            args_vec
        }
        FnDefArgs::PyArgList(args) => args,
    };

    Ok((pre, post, args))
}

fn prepare_py_fn<'src, 'ast>(
    ctx: &mut TfCtx<'src>,
    arglist: FnDefArgs<'src, 'ast>,
    body: FnDefBody<'src, 'ast>,
    span: &Span,
) -> TfResult<(PyBlock<'src>, PyBlock<'src>, Vec<PyArgDefItem<'src>>)> {
    let mut aux_stmts = PyBlock::new();
    let mut body_stmts = PyBlock::new();

    let (pre, post, args) = make_arglist(ctx, arglist)?;
    aux_stmts.extend(pre);
    body_stmts.extend(post);

    body_stmts.extend(match body {
        FnDefBody::PyStmts(stmts) => stmts,
        FnDefBody::Block(block) => {
            let block = block.transform(ctx)?;
            let mut stmts = block.pre;

            stmts.push((PyStmt::Return(block.value), *span).into());

            stmts
        }
    });

    Ok((aux_stmts, body_stmts, args))
}

fn make_fn_exp<'src, 'ast>(
    ctx: &mut TfCtx<'src>,
    arglist: FnDefArgs<'src, 'ast>,
    body: FnDefBody<'src, 'ast>,
    span: &Span,
) -> TfResult<SPyExprWithPre<'src>> {
    let (mut aux_stmts, body_stmts, args) = prepare_py_fn(ctx, arglist, body, span)?;

    if body_stmts.0.len() == 1 {
        // TODO maybe refactor prepare_py_fn to return body_stmts as PyExprWithPre
        if let PyStmt::Return(expr) = &body_stmts.0[0].value {
            return Ok(SPyExprWithPre {
                value: (PyExpr::Lambda(args, Box::new(expr.clone())), *span).into(),
                pre: PyBlock::new(),
            });
        }
    }

    let name = ctx.temp_var_name("fnexp", span.start);
    aux_stmts.push(
        (
            PyStmt::FnDef(name.clone().into(), args, body_stmts, PyDecorators::new()),
            *span,
        )
            .into(),
    );
    Ok(SPyExprWithPre {
        value: (PyExpr::Ident(name.into(), PyAccessCtx::Load), *span).into(),
        pre: aux_stmts,
    })
}

fn make_fn_def<'src, 'ast>(
    ctx: &mut TfCtx<'src>,
    name: Cow<'src, str>,
    arglist: FnDefArgs<'src, 'ast>,
    body: FnDefBody<'src, 'ast>,
    decorators: PyDecorators<'src>,
    span: &Span,
) -> TfResult<PyBlock<'src>> {
    let (mut aux_stmts, body_stmts, args) = prepare_py_fn(ctx, arglist, body, span)?;
    aux_stmts.push(
        (
            PyStmt::FnDef(name.into(), args, body_stmts, decorators),
            *span,
        )
            .into(),
    );
    Ok(aux_stmts)
}

fn transform_call_items<'src, 'ast>(
    ctx: &mut TfCtx<'src>,
    args: &'ast [SCallItem<'src>],
    span: &Span,
) -> TfResult<(PyBlock<'src>, Vec<PyCallItem<'src>>)> {
    let mut started_kwargs = false;
    let mut call_items = vec![];
    let mut aux_stmts = PyBlock::new();

    for arg in args {
        match &arg.0 {
            CallItem::Arg(expr) => {
                if started_kwargs {
                    return Err(TfErrBuilder::default()
                        .message("Cannot have args after kwargs")
                        .span(*span)
                        .build_errs());
                }

                let e = expr.transform_with_deep_placeholder_guard(ctx)?;
                aux_stmts.extend(e.pre);
                call_items.push(PyCallItem::Arg(e.value));
            }
            CallItem::Kwarg((name, _name_span), expr) => {
                started_kwargs = true;
                let e = expr.transform_with_deep_placeholder_guard(ctx)?;
                aux_stmts.extend(e.pre);
                call_items.push(PyCallItem::Kwarg(name.clone(), e.value));
            }
            CallItem::ArgSpread(expr) => {
                if started_kwargs {
                    return Err(TfErrBuilder::default()
                        .message("Cannot have arg spread after kwargs")
                        .span(*span)
                        .build_errs());
                }

                let e = expr.transform_with_deep_placeholder_guard(ctx)?;
                aux_stmts.extend(e.pre);
                call_items.push(PyCallItem::ArgSpread(e.value));
            }
            CallItem::KwargSpread(expr) => {
                started_kwargs = true;
                let e = expr.transform_with_deep_placeholder_guard(ctx)?;
                aux_stmts.extend(e.pre);
                call_items.push(PyCallItem::KwargSpread(e.value));
            }
        };
    }

    Ok((aux_stmts, call_items))
}

fn transform_subscript_items<'src, 'ast>(
    ctx: &mut TfCtx<'src>,
    indices: &'ast [ListItem<'src>],
    span: &Span,
) -> TfResult<(PyBlock<'src>, SPyExpr<'src>)> {
    let mut aux_stmts = PyBlock::new();

    let single_item = if indices.len() == 1 {
        match &indices[0] {
            ListItem::Item(item) => Some(item),
            ListItem::Spread(_) => None,
        }
    } else {
        None
    };

    let subscript_expr = if let Some(single_item) = single_item {
        let e = single_item.transform_with_deep_placeholder_guard(ctx)?;
        aux_stmts.extend(e.pre);
        e.value
    } else {
        (
            PyExpr::Tuple(
                indices
                    .into_iter()
                    .map(|i| match i {
                        ListItem::Item(expr) => {
                            let e = expr.transform_with_deep_placeholder_guard(ctx)?;
                            aux_stmts.extend(e.pre);
                            Ok(PyListItem::Item(e.value))
                        }
                        ListItem::Spread(expr) => {
                            let e = expr.transform_with_deep_placeholder_guard(ctx)?;
                            aux_stmts.extend(e.pre);
                            Ok(PyListItem::Spread(e.value))
                        }
                    })
                    .collect::<TfResult<Vec<_>>>()?,
                PyAccessCtx::Load,
            ),
            *span,
        )
            .into()
    };

    Ok((aux_stmts, subscript_expr))
}

struct PlaceholderCtx {
    activated: bool,
    span: Span,
}

impl PlaceholderCtx {
    fn new(span: Span) -> Self {
        Self {
            activated: false,
            span,
        }
    }

    fn var_name<'src>(&self, ctx: &TfCtx<'src>) -> Cow<'src, str> {
        ctx.temp_var_name("ph", self.span.start).into()
    }
}

fn placeholder_guard<'src, F>(
    ctx: &mut TfCtx<'src>,
    span: &Span,
    f: F,
) -> TfResult<SPyExprWithPre<'src>>
where
    F: FnOnce(&mut TfCtx<'src>) -> TfResult<SPyExprWithPre<'src>>,
{
    ctx.placeholder_ctx_stack.push(PlaceholderCtx::new(*span));
    let inner_expr = f(ctx)?;
    let popped = ctx.placeholder_ctx_stack.pop().unwrap();

    if popped.activated {
        let var_name = popped.var_name(ctx);

        let mut body = PyBlock::new();
        body.extend(inner_expr.pre);
        body.push((PyStmt::Return(inner_expr.value), *span).into());

        let fn_exp = make_fn_exp(
            ctx,
            FnDefArgs::PyArgList(vec![PyArgDefItem::Arg(var_name, None)]),
            FnDefBody::PyStmts(body),
            span,
        )?;

        Ok(SPyExprWithPre {
            value: fn_exp.value,
            pre: fn_exp.pre,
        })
    } else {
        Ok(inner_expr)
    }
}

fn transform_placeholder<'src>(
    ctx: &mut TfCtx<'src>,
    span: &Span,
    access_ctx: PyAccessCtx,
) -> TfResult<SPyExprWithPre<'src>> {
    unsafe {
        let raw_ctx = ctx as *mut TfCtx<'src>;
        let ph_ctx = ctx.placeholder_ctx_stack.last_mut().ok_or_else(|| {
            TfErrBuilder::default()
                .message("Placeholder expression outside of placeholder context")
                .span(*span)
                .build_errs()
        })?;

        ph_ctx.activated = true;
        let var_name = ph_ctx.var_name(&*raw_ctx);

        Ok(SPyExprWithPre {
            value: (PyExpr::Ident(var_name, access_ctx), *span).into(),
            pre: PyBlock::new(),
        })
    }
}

fn transform_postfix_expr<'src, 'ast>(
    ctx: &mut TfCtx<'src>,
    expr: &'ast SExpr<'src>,
    access_ctx: PyAccessCtx,
) -> TfResult<SPyExprWithPre<'src>> {
    let mut aux = PyBlock::new();
    let (lift_lhs, lhs_node) = match &expr.0 {
        Expr::Attribute(obj, _) => (false, obj),
        Expr::Subscript(obj, _) => (false, obj),
        Expr::Call(obj, _) => (false, obj),
        Expr::Then(obj, _) => (false, obj),
        Expr::Extension(obj, _) => (false, obj),
        Expr::MappedAttribute(obj, _) => (true, obj),
        Expr::MappedSubscript(obj, _) => (true, obj),
        Expr::MappedCall(obj, _) => (true, obj),
        Expr::MappedThen(obj, _) => (true, obj),
        Expr::MappedExtension(obj, _) => (true, obj),
        _ => {
            return Err(TfErrBuilder::default()
                .message("Internal error: Postfix expressions can only be attributes, subscripts, calls, or extensions")
                .span(expr.1)
                .build_errs());
        }
    };

    if lift_lhs && access_ctx != PyAccessCtx::Load {
        return Err(TfErrBuilder::default()
            .message("Internal error: Cannot use null-coalescing in a non-Load context")
            .span(expr.1)
            .build_errs());
    }

    let lhs = if lift_lhs {
        let t = lhs_node.transform_lifted(ctx)?;
        aux.extend(t.pre);
        t.value
    } else {
        let t = lhs_node.transform(ctx)?;
        aux.extend(t.pre);
        t.value
    };

    placeholder_guard(ctx, &expr.1, |ctx| {
        let a = PyAstBuilder::new(expr.1);

        let guard_if_expr = |e| {
            a.if_expr(
                a.call(a.load_ident("__coalesces"), vec![a.call_arg(lhs.clone())]),
                lhs.clone(),
                e,
            )
        };

        let node = match &expr.0 {
            Expr::Call(_, list) => {
                let t = transform_call_items(ctx, &list, &expr.1)?;
                aux.extend(t.0);
                a.call(lhs, t.1)
            }
            Expr::MappedCall(_, list) => {
                let t = transform_call_items(ctx, &list, &expr.1)?;
                aux.extend(t.0);
                guard_if_expr(a.call(lhs.clone(), t.1))
            }
            Expr::Subscript(_, list) => {
                let t = transform_subscript_items(ctx, &list, &expr.1)?;
                aux.extend(t.0);
                a.subscript(lhs, t.1, access_ctx)
            }
            Expr::MappedSubscript(_, list) => {
                let t = transform_subscript_items(ctx, &list, &expr.1)?;
                aux.extend(t.0);
                guard_if_expr(a.subscript(lhs.clone(), t.1, access_ctx))
            }
            Expr::Attribute(_, attr) => a.attribute(lhs, attr.0.clone(), access_ctx),
            Expr::MappedAttribute(_, attr) => {
                guard_if_expr(a.attribute(lhs.clone(), attr.0.clone(), access_ctx))
            }
            Expr::Then(_, rhs) => {
                let rhs_node = rhs.transform_with_placeholder_guard(ctx)?;
                aux.extend(rhs_node.pre);
                a.call(rhs_node.value, vec![PyCallItem::Arg(lhs)])
            }
            Expr::MappedThen(_, rhs) => {
                let rhs_node = rhs.transform_with_placeholder_guard(ctx)?;
                aux.extend(rhs_node.pre);
                guard_if_expr(a.call(rhs_node.value, vec![PyCallItem::Arg(lhs.clone())]))
            }
            Expr::Extension(_, rhs) => a.call(
                a.load_ident("__vtable_lookup"),
                vec![
                    a.call_arg(lhs),
                    a.call_arg(a.literal(PyLiteral::Str(rhs.0.clone()))),
                ],
            ),
            Expr::MappedExtension(_, rhs) => guard_if_expr(a.call(
                a.load_ident("__vtable_lookup"),
                vec![
                    a.call_arg(lhs.clone()),
                    a.call_arg(a.literal(PyLiteral::Str(rhs.0.clone()))),
                ],
            )),
            _ => {
                return Err(TfErrBuilder::default()
                .message("Internal error: Postfix expressions can only be attributes, subscripts, calls, or extensions")
                .span(expr.1)
                .build_errs());
            }
        };

        Ok(SPyExprWithPre {
            value: node,
            pre: aux,
        })
    })
}

fn is_default_pattern<'src>(pattern: &SPattern<'src>) -> bool {
    match &pattern.0 {
        Pattern::Capture(_) => true,
        Pattern::As(pattern, _) => is_default_pattern(pattern),
        Pattern::Or(items) => items.iter().any(is_default_pattern),
        _ => false,
    }
}

fn matching_except_handler<'src>(
    ctx: &mut TfCtx<'src>,
    handlers: &Vec<MatchCase<'src>>,
    span: &Span,
) -> TfResult<PyExceptHandler<'src>> {
    let a = PyAstBuilder::new(*span);
    let b = AstBuilder::new(*span);
    let mut block = PyBlock::new();

    let (mut match_stmt, has_default) = transform_match_expr(ctx, &b.ident("__e"), handlers, span)?;

    if let PyStmt::Match(_, cases) = &mut match_stmt.pre.0.last_mut().unwrap().value {
        if !has_default {
            cases.push(PyMatchCase {
                pattern: (PyPattern::As(None, None), *span).into(),
                guard: None,
                body: PyBlock(vec![a.raise(None)]),
            });
        }
    } else {
        return Err(TfErrBuilder::default()
            .message("Internal error: Expected a match statement")
            .span(*span)
            .build_errs());
    }

    block.extend(match_stmt.pre);

    Ok(PyExceptHandler {
        typ: None,
        name: Some("__e".into()),
        body: block,
    })
}

trait ListItemsExt<'src> {
    fn transform<'ast>(
        &'ast self,
        ctx: &mut TfCtx<'src>,
    ) -> TfResult<(Vec<PyListItem<'src>>, PyBlock<'src>)>;
}

impl<'src> ListItemsExt<'src> for Vec<ListItem<'src>> {
    fn transform<'ast>(
        &'ast self,
        ctx: &mut TfCtx<'src>,
    ) -> TfResult<(Vec<PyListItem<'src>>, PyBlock<'src>)> {
        let mut aux_stmts = PyBlock::new();
        let mut items = vec![];

        for expr in self {
            let e = match expr {
                ListItem::Spread(expr) => expr.transform_with_deep_placeholder_guard(ctx)?,
                ListItem::Item(expr) => expr.transform_with_deep_placeholder_guard(ctx)?,
            };
            aux_stmts.extend(e.pre);
            items.push(match expr {
                ListItem::Spread(_) => PyListItem::Spread(e.value),
                ListItem::Item(_) => PyListItem::Item(e.value),
            });
        }

        Ok((items, aux_stmts))
    }
}

trait LiteralExt<'src> {
    fn transform<'ast>(&'ast self, ctx: &mut TfCtx<'src>) -> TfResult<PyLiteral<'src>>;
}

impl<'src> LiteralExt<'src> for Literal<'src> {
    fn transform<'ast>(&'ast self, _ctx: &mut TfCtx<'src>) -> TfResult<PyLiteral<'src>> {
        let value = match self {
            Literal::Num(num) => PyLiteral::Num(num.to_owned()),
            Literal::Str(s) => PyLiteral::Str(s.to_owned()),
            Literal::Bool(b) => PyLiteral::Bool(*b),
            Literal::None => PyLiteral::None,
        };

        Ok(value)
    }
}

trait SExprExt<'src> {
    fn transform<'ast>(&'ast self, ctx: &mut TfCtx<'src>) -> TfResult<SPyExprWithPre<'src>>;

    fn transform_lifted<'ast>(&'ast self, ctx: &mut TfCtx<'src>) -> TfResult<SPyExprWithPre<'src>>;

    /**
     * Transforms the expression, setting a placeholder guard so that
     * expr($, ...) will transform into x => expr(x, ...)
     */
    fn transform_with_placeholder_guard<'ast>(
        &'ast self,
        ctx: &mut TfCtx<'src>,
    ) -> TfResult<SPyExprWithPre<'src>>;

    /**
     * Transforms the expression, setting a placeholder guard one level deeper
     * so that
     * $ will transform into x => parent_expr(x)
     * and
     * expr will transform into transform_with_placeholder_guard(expr)
     */
    fn transform_with_deep_placeholder_guard<'ast>(
        &'ast self,
        ctx: &mut TfCtx<'src>,
    ) -> TfResult<SPyExprWithPre<'src>>;

    fn transform_with_access<'ast>(
        &'ast self,
        ctx: &mut TfCtx<'src>,
        py_ctx: PyAccessCtx,
    ) -> TfResult<SPyExprWithPre<'src>>;
}

impl<'src> SExprExt<'src> for SExpr<'src> {
    fn transform<'ast>(&'ast self, ctx: &mut TfCtx<'src>) -> TfResult<SPyExprWithPre<'src>> {
        self.transform_with_access(ctx, PyAccessCtx::Load)
    }

    /**
     * Transforms
     * expr
     * to
     * x = expr
     * x
     */
    fn transform_lifted<'ast>(&'ast self, ctx: &mut TfCtx<'src>) -> TfResult<SPyExprWithPre<'src>> {
        let mut aux_stmts = PyBlock::new();
        let value = self.transform(ctx)?;
        aux_stmts.extend(value.pre);

        // TODO skip if value is already ident

        let expr = match self.0 {
            Expr::Ident(..) => value.value,
            _ => {
                let temp_var = ctx.temp_var_name("tmp", self.1.start);

                aux_stmts.push(
                    (
                        PyStmt::Assign(
                            (
                                PyExpr::Ident(temp_var.clone().into(), PyAccessCtx::Store),
                                self.1,
                            )
                                .into(),
                            value.value,
                        ),
                        self.1,
                    )
                        .into(),
                );

                (PyExpr::Ident(temp_var.into(), PyAccessCtx::Load), self.1).into()
            }
        };

        Ok(SPyExprWithPre {
            value: expr,
            pre: aux_stmts,
        })
    }

    fn transform_with_placeholder_guard<'ast>(
        &'ast self,
        ctx: &mut TfCtx<'src>,
    ) -> TfResult<SPyExprWithPre<'src>> {
        placeholder_guard(ctx, &self.1, |ctx| self.transform(ctx))
    }

    fn transform_with_deep_placeholder_guard<'ast>(
        &'ast self,
        ctx: &mut TfCtx<'src>,
    ) -> TfResult<SPyExprWithPre<'src>> {
        match &self.0 {
            Expr::Placeholder => transform_placeholder(ctx, &self.1, PyAccessCtx::Load),
            _ => self.transform_with_placeholder_guard(ctx),
        }
    }

    fn transform_with_access<'ast>(
        &'ast self,
        ctx: &mut TfCtx<'src>,
        access_ctx: PyAccessCtx,
    ) -> TfResult<SPyExprWithPre<'src>> {
        let (expr, span) = self;

        match &expr {
            Expr::Attribute(..) | Expr::Subscript(..) | Expr::Ident(..) => {}
            _ => {
                if access_ctx != PyAccessCtx::Load {
                    return Err(TfErrBuilder::default()
                        .message("Expression context must be Load for this expression")
                        .span(*span)
                        .build_errs());
                }
            }
        }

        match &expr {
            Expr::Checked(expr, pattern) => {
                let a = PyAstBuilder::new(*span);
                let b = AstBuilder::new(*span);

                let t = expr.transform(ctx)?;
                let var_name = ctx.temp_var_name("chk", span.start);

                let mut try_body = t.pre;
                try_body.push(a.assign(a.ident(var_name.clone(), PyAccessCtx::Store), t.value));

                let mut catch_body = PyBlock::new();
                catch_body.push(a.assign(
                    a.ident(var_name.clone(), PyAccessCtx::Store),
                    a.load_ident("__e"),
                ));

                let mut stmts = PyBlock::new();

                let handler = MatchCase {
                    pattern: pattern.as_ref().map(|x| (**x).clone()),
                    guard: None,
                    body: b.stmts_block(vec![b.assign(b.ident(var_name.clone()), b.ident("__e"))]),
                };

                let except_handler = matching_except_handler(ctx, &vec![handler], span)?;
                stmts.push(a.try_(try_body, vec![except_handler], None));

                Ok(SPyExprWithPre {
                    pre: stmts,
                    value: a.load_ident(var_name),
                })
            }
            Expr::Placeholder => transform_placeholder(ctx, span, access_ctx),
            Expr::Fn(arglist, body) => make_fn_exp(
                ctx,
                FnDefArgs::ArgList(arglist),
                FnDefBody::Block(body),
                span,
            ),
            Expr::Class(bases, body) => {
                let name = Cow::<'src, str>::Owned(ctx.temp_var_name("clsexp", span.start));
                let aux_stmts =
                    make_class_def(ctx, name.clone(), bases, body, PyDecorators::new(), span)?;

                Ok(SPyExprWithPre {
                    value: (PyExpr::Ident(name, PyAccessCtx::Load), *span).into(),
                    pre: aux_stmts,
                })
            }
            Expr::Literal((lit, span)) => Ok(SPyExprWithPre {
                value: (PyExpr::Literal(lit.transform(ctx)?), *span).into(),
                pre: PyBlock::new(),
            }),
            Expr::Ident((ident, span)) => Ok(SPyExprWithPre {
                value: (PyExpr::Ident(ident.clone(), access_ctx), *span).into(),
                pre: PyBlock::new(),
            }),
            Expr::Attribute(..)
            | Expr::MappedAttribute(..)
            | Expr::Call(..)
            | Expr::MappedCall(..)
            | Expr::Subscript(..)
            | Expr::MappedSubscript(..)
            | Expr::Then(..)
            | Expr::MappedThen(..)
            | Expr::Extension(..)
            | Expr::MappedExtension(..) => transform_postfix_expr(ctx, self, access_ctx),
            Expr::If(cond, then_block, else_block) => {
                transform_if_expr(ctx, cond, then_block, else_block, span)
            }
            Expr::Block(block) => block.transform(ctx),
            Expr::Match(subject, cases) => Ok(transform_match_expr(ctx, subject, cases, span)?.0),
            Expr::Matches(subject, pattern) => {
                let mut block = PyBlock::new();
                let subject_t = subject.transform(ctx)?;
                block.extend(subject_t.pre);

                let a = PyAstBuilder::new(*span);
                let var = ctx.temp_var_name("matches", span.start);

                let matcher = create_matcher(
                    ctx,
                    subject_t.value,
                    pattern,
                    PyBlock(vec![a.assign(
                        a.ident(var.clone(), PyAccessCtx::Store),
                        a.literal(PyLiteral::Bool(true)),
                    )]),
                    PyBlock(vec![a.assign(
                        a.ident(var.clone(), PyAccessCtx::Store),
                        a.literal(PyLiteral::Bool(false)),
                    )]),
                )?;

                block.extend(matcher);

                Ok(SPyExprWithPre {
                    value: a.load_ident(var.clone()),
                    pre: block,
                })
            }
            Expr::Binary(op, lhs, rhs) => {
                let (lhs, rhs) = match op {
                    BinaryOp::Pipe => {
                        let lhs = lhs.transform_with_placeholder_guard(ctx)?;
                        let rhs = rhs.transform_with_placeholder_guard(ctx)?;

                        (lhs, rhs)
                    }
                    BinaryOp::Coalesce => {
                        let lhs = lhs.transform_lifted(ctx)?;
                        let rhs = rhs.transform(ctx)?;

                        (lhs, rhs)
                    }
                    _ => (lhs.transform(ctx)?, rhs.transform(ctx)?),
                };

                let mut aux_stmts = lhs.pre;
                aux_stmts.extend(rhs.pre);

                let py_op = match op {
                    BinaryOp::Add => PyBinaryOp::Add,
                    BinaryOp::Sub => PyBinaryOp::Sub,
                    BinaryOp::Mul => PyBinaryOp::Mult,
                    BinaryOp::Div => PyBinaryOp::Div,
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

                    BinaryOp::Pipe => {
                        return Ok(SPyExprWithPre {
                            value: (
                                PyExpr::Call(Box::new(rhs.value), vec![PyCallItem::Arg(lhs.value)]),
                                *span,
                            )
                                .into(),
                            pre: aux_stmts,
                        });
                    }

                    BinaryOp::Coalesce => {
                        let a = PyAstBuilder::new(*span);

                        let expr = a.if_expr(
                            a.call(
                                a.load_ident("__coalesces"),
                                vec![a.call_arg(lhs.value.clone())],
                            ),
                            rhs.value,
                            lhs.value,
                        );

                        return Ok(SPyExprWithPre {
                            value: expr,
                            pre: aux_stmts,
                        });
                    }
                };

                return Ok(SPyExprWithPre {
                    value: (
                        PyExpr::Binary(py_op, Box::new(lhs.value), Box::new(rhs.value)),
                        *span,
                    )
                        .into(),
                    pre: aux_stmts,
                });
            }
            Expr::Unary(op, expr) => {
                let expr = expr.transform(ctx)?;
                let aux_stmts = expr.pre;

                let py_op = match op {
                    UnaryOp::Neg => PyUnaryOp::Neg,
                    UnaryOp::Pos => PyUnaryOp::Pos,
                    UnaryOp::Inv => PyUnaryOp::Inv,
                    UnaryOp::Yield => {
                        return Ok(SPyExprWithPre {
                            value: (PyExpr::Yield(Box::new(expr.value)), *span).into(),
                            pre: aux_stmts,
                        });
                    }
                    UnaryOp::YieldFrom => {
                        return Ok(SPyExprWithPre {
                            value: (PyExpr::YieldFrom(Box::new(expr.value)), *span).into(),
                            pre: aux_stmts,
                        });
                    }
                };

                return Ok(SPyExprWithPre {
                    value: (PyExpr::Unary(py_op, Box::new(expr.value)), *span).into(),
                    pre: aux_stmts,
                });
            }
            Expr::List(exprs) => {
                return placeholder_guard(ctx, span, |ctx| {
                    let (items, aux_stmts) = exprs.transform(ctx)?;

                    return Ok(SPyExprWithPre {
                        value: (PyExpr::List(items, PyAccessCtx::Load), *span).into(),
                        pre: aux_stmts,
                    });
                });
            }
            Expr::Tuple(exprs) => {
                return placeholder_guard(ctx, span, |ctx| {
                    let (items, aux_stmts) = exprs.transform(ctx)?;

                    return Ok(SPyExprWithPre {
                        value: (PyExpr::Tuple(items, PyAccessCtx::Load), *span).into(),
                        pre: aux_stmts,
                    });
                });
            }
            Expr::Mapping(items) => {
                return placeholder_guard(ctx, span, |ctx| {
                    let mut aux_stmts = PyBlock::new();
                    let mut dict_items = vec![];

                    for item in items {
                        match item {
                            MappingItem::Item(key, value) => {
                                let key = key.transform_with_deep_placeholder_guard(ctx)?;
                                let value = value.transform_with_deep_placeholder_guard(ctx)?;

                                aux_stmts.extend(key.pre);
                                aux_stmts.extend(value.pre);

                                dict_items.push(PyDictItem::Item(key.value, value.value));
                            }
                            MappingItem::Spread(expr) => {
                                let e = expr.transform_with_deep_placeholder_guard(ctx)?;
                                aux_stmts.extend(e.pre);

                                dict_items.push(PyDictItem::Spread(e.value));
                            }
                        }
                    }

                    return Ok(SPyExprWithPre {
                        value: (PyExpr::Dict(dict_items), *span).into(),
                        pre: aux_stmts,
                    });
                });
            }
            Expr::Slice(start, end, step) => {
                return placeholder_guard(ctx, span, |ctx| {
                    let start_node = start
                        .as_ref()
                        .map(|e| e.as_ref().transform_with_deep_placeholder_guard(ctx))
                        .transpose()?;
                    let end_node = end
                        .as_ref()
                        .map(|e| e.as_ref().transform_with_deep_placeholder_guard(ctx))
                        .transpose()?;
                    let step_node = step
                        .as_ref()
                        .map(|e| e.as_ref().transform_with_deep_placeholder_guard(ctx))
                        .transpose()?;

                    let mut aux_stmts = PyBlock::new();

                    let mut get = |x: Option<SPyExprWithPre<'src>>| {
                        let expr = if let Some(x) = x {
                            aux_stmts.extend(x.pre);
                            x.value
                        } else {
                            (PyExpr::Literal(PyLiteral::None), *span).into()
                        };

                        PyCallItem::Arg(expr)
                    };

                    return Ok(SPyExprWithPre {
                        value: (
                            PyExpr::Call(
                                Box::new(
                                    (PyExpr::Ident("slice".into(), PyAccessCtx::Load), *span)
                                        .into(),
                                ),
                                vec![get(start_node), get(end_node), get(step_node)],
                            ),
                            *span,
                        )
                            .into(),
                        pre: aux_stmts,
                    });
                });
            }
            Expr::Fstr(begin, parts) => {
                return placeholder_guard(ctx, span, |ctx| {
                    let mut aux_stmts = PyBlock::new();
                    let mut nodes = Vec::new();

                    nodes.push(PyFstrPart::Str(begin.0.clone().into()));

                    for (fmt_expr, str_part) in parts {
                        // TODO format specifiers?
                        let block_node = fmt_expr.0.block.transform(ctx)?;
                        aux_stmts.extend(block_node.pre);

                        nodes.push(PyFstrPart::Expr(block_node.value, None));
                        nodes.push(PyFstrPart::Str(str_part.0.clone().into()));
                    }

                    let expr = (PyExpr::Fstr(nodes), *span).into();
                    return Ok(SPyExprWithPre {
                        value: expr,
                        pre: aux_stmts,
                    });
                });
            }
        }
    }
}

pub struct TransformOutput<'src> {
    pub py_block: PyBlock<'src>,
    pub exports: Vec<PyIdent<'src>>,
    pub module_star_exports: Vec<PyIdent<'src>>,
}

pub fn transform_ast<'src, 'ast>(
    source: &'src str,
    block: &'ast SBlock<'src>,
) -> TfResult<TransformOutput<'src>> {
    let mut ctx = TfCtx::new(source)?;
    let mut stmts = block.transform_with_depth(&mut ctx, true)?;
    let span = stmts.value.tl_span;

    stmts.pre.push((PyStmt::Expr(stmts.value), span).into());

    Ok(TransformOutput {
        py_block: stmts.pre,
        exports: ctx.exports,
        module_star_exports: ctx.module_star_exports,
    })
}
