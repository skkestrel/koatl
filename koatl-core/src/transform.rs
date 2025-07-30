use std::{borrow::Cow, collections::HashSet};

use crate::{
    linecol::LineColCache,
    py::{ast::*, util::PyAstBuilder},
};
use once_cell::sync::Lazy;
use parser::{ast::*, util::AstBuilder};

#[derive(Debug)]
pub struct TfErr {
    pub message: String,
    pub span: Option<Span>,
    pub contexts: Vec<(String, Span)>,
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
    contexts: Vec<(String, Span)>,
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
            contexts: self.contexts,
        }
    }

    pub fn context(mut self, message: impl Into<String>, span: Span) -> Self {
        self.contexts.push((message.into(), span));
        self
    }

    pub fn build_errs(self) -> TfErrs {
        TfErrs(vec![self.build()])
    }
}

pub type TfResult<T> = Result<T, TfErrs>;

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
        ctx.create_aux_var("ph", self.span.start).into()
    }
}

struct FnCtx<'src> {
    is_async: bool,
    is_do: bool,

    globals: HashSet<PyIdent<'src>>,
    nonlocals: HashSet<PyIdent<'src>>,
}

impl FnCtx<'_> {
    fn new() -> Self {
        Self {
            is_async: false,
            is_do: false,
            globals: HashSet::new(),
            nonlocals: HashSet::new(),
        }
    }
}

trait FnCtxStackExt {
    fn set_async(&mut self, allow_top_level_await: bool, span: &Span) -> TfResult<()>;
    fn set_do(&mut self, span: &Span) -> TfResult<()>;
}

impl<'src> FnCtxStackExt for Vec<FnCtx<'src>> {
    fn set_async(&mut self, allow_top_level_await: bool, span: &Span) -> TfResult<()> {
        if let Some(fn_ctx) = self.last_mut() {
            fn_ctx.is_async = true;
        } else if !allow_top_level_await {
            return Err(TfErrBuilder::default()
                .message("Await is only allowed in a function context")
                .span(*span)
                .build_errs());
        }

        Ok(())
    }

    fn set_do(&mut self, span: &Span) -> TfResult<()> {
        if let Some(fn_ctx) = self.last_mut() {
            fn_ctx.is_do = true;
        } else {
            return Err(TfErrBuilder::default()
                .message("Bind operator is only allowed in a function context")
                .span(*span)
                .build_errs());
        }

        Ok(())
    }
}

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

trait SIdentExt<'src> {
    fn escape(&self) -> PyIdent<'src>;

    fn transform(&self, ctx: &TfCtx<'src>) -> PyIdent<'src>;

    // When an ident appears on the lhs, this should be called before transform
    // so that the local decl can be prepared for transform to find later.
    fn prepare_binding(&self, ctx: &mut TfCtx<'src>) -> TfResult<PyIdent<'src>>;
}

impl<'src> SIdentExt<'src> for SIdent<'src> {
    fn escape(&self) -> PyIdent<'src> {
        self.0.escape()
    }

    fn transform(&self, ctx: &TfCtx<'src>) -> PyIdent<'src> {
        if let Some(found) = ctx.scope_ctx_stack.find_decl(self) {
            return found.decl.py_ident.clone();
        }
        return self.escape();
    }

    fn prepare_binding(&self, ctx: &mut TfCtx<'src>) -> TfResult<PyIdent<'src>> {
        let scope = ctx.scope_ctx_stack.last_mut().ok_or_else(|| {
            TfErrBuilder::default()
                .message("Internal error: expected a scope")
                .span(self.1)
                .build_errs()
        })?;

        if scope.is_class_scope || scope.is_global_scope {
            // at class or global scope, binding can happen without 'let'

            // prepare a declaration so transform() knows what to do later

            if !scope.locals.iter().rev().any(|d| d.ident == self.0) {
                scope.locals.push(Declaration {
                    ident: self.0.clone(),
                    py_ident: self.escape(),
                    loc: self.1,
                    const_: false,
                    const_assigned: None,
                    typ: if scope.is_global_scope {
                        PyDeclType::Global
                    } else {
                        PyDeclType::Local
                    },
                });
            }

            return Ok(self.escape());
        }

        if let Some(found) = ctx.scope_ctx_stack.find_decl_mut(self) {
            if found.decl.const_ {
                if let Some(assigned) = found.decl.const_assigned {
                    return Err(TfErrBuilder::default()
                        .message("Can only assign to a constant once")
                        .span(self.1)
                        .context("Declared here", found.decl.loc)
                        .context("Assigned here", assigned)
                        .build_errs());
                }

                found.decl.const_assigned = Some(self.1);
            }

            if found.py_local {
                return Ok(found.decl.py_ident.clone());
            }

            // it was in a different python scope.
            // need to either declare it as global or nonlocal

            match found.decl.typ {
                PyDeclType::GlobalCapture | PyDeclType::Global => {
                    ctx.fn_ctx_stack
                        .last_mut()
                        .unwrap()
                        .globals
                        .insert(found.decl.py_ident.clone());
                }
                PyDeclType::NonlocalCapture | PyDeclType::Local => {
                    ctx.fn_ctx_stack
                        .last_mut()
                        .unwrap()
                        .nonlocals
                        .insert(found.decl.py_ident.clone());
                }
            }

            return Ok(found.decl.py_ident.clone());
        }

        Err(TfErrBuilder::default()
            .message("Undeclared identifier; either declare with 'let' or mark as 'global'.")
            .span(self.1)
            .build_errs())
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
enum PyDeclType {
    Local,
    Global,
    GlobalCapture,
    NonlocalCapture,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
struct Declaration<'src> {
    ident: Ident<'src>,
    py_ident: PyIdent<'src>,
    loc: Span,
    typ: PyDeclType,
    const_: bool,
    const_assigned: Option<Span>,
}

#[derive(Debug)]
struct ScopeCtx<'src> {
    locals: Vec<Declaration<'src>>,
    is_class_scope: bool,
    is_global_scope: bool,
    is_py_fn_scope: bool,

    /**
     * This enables special treatment for defining
     * recursive functions; lifted_decls are added to the
     * containing scope when processing the function body.
     */
    lifted_decls: Vec<Declaration<'src>>,

    /**
     * this is a really messed up hack to handle Python's
     * weird class scoping rules -
     * method bodies TWO scopes lower
     * are able to see the class definition
     */
    lifted_class_decls: Vec<Declaration<'src>>,
}

impl<'src> ScopeCtx<'src> {
    fn new() -> Self {
        Self {
            locals: Vec::new(),
            lifted_decls: Vec::new(),
            lifted_class_decls: Vec::new(),
            is_py_fn_scope: false,
            is_class_scope: false,
            is_global_scope: false,
        }
    }
}

#[derive(Debug)]
#[allow(dead_code)]
struct ScopeSearchResultMut<'slf, 'src> {
    decl: &'slf mut Declaration<'src>,

    local: bool,
    py_local: bool,
}

#[derive(Debug)]
struct ScopeSearchResult<'slf, 'src> {
    decl: &'slf Declaration<'src>,
}

trait ScopeCtxStackExt<'src> {
    fn find_decl<'slf>(&'slf self, ident: &SIdent<'src>) -> Option<ScopeSearchResult<'slf, 'src>>;
    fn find_decl_mut<'slf>(
        &'slf mut self,
        ident: &SIdent<'src>,
    ) -> Option<ScopeSearchResultMut<'slf, 'src>>;
}

impl<'src> ScopeCtxStackExt<'src> for Vec<ScopeCtx<'src>> {
    fn find_decl<'slf>(&'slf self, ident: &SIdent<'src>) -> Option<ScopeSearchResult<'slf, 'src>> {
        for scope in self.iter().rev() {
            if let Some(decl) = scope.locals.iter().rev().find(|d| d.ident == ident.0) {
                return Some(ScopeSearchResult { decl });
            }
        }

        None
    }

    fn find_decl_mut<'slf>(
        &'slf mut self,
        ident: &SIdent<'src>,
    ) -> Option<ScopeSearchResultMut<'slf, 'src>> {
        let mut crossed_tl_scope = false;
        let mut crossed_py_scope = false;

        for scope in self.iter_mut().rev() {
            if let Some(decl) = scope.locals.iter_mut().rev().find(|d| d.ident == ident.0) {
                return Some(ScopeSearchResultMut {
                    decl,
                    local: !crossed_tl_scope,
                    py_local: !crossed_py_scope,
                });
            }

            if scope.is_py_fn_scope {
                crossed_py_scope = true;
            }

            crossed_tl_scope = true;
        }

        None
    }
}

fn create_declaration<'src>(
    ctx: &mut TfCtx<'src>,
    ident: &SIdent<'src>,
    modifier: DeclType,
) -> TfResult<Option<Declaration<'src>>> {
    match modifier {
        DeclType::Global => {
            if ctx.scope_ctx_stack.len() == 1 {
                return Err(TfErrBuilder::default()
                    .message("Global declaration is not allowed at the module level")
                    .span(ident.1)
                    .build_errs());
            }

            if !ctx.fn_ctx_stack.is_empty() {
                let fn_scope = ctx.fn_ctx_stack.last_mut().unwrap();
                fn_scope.globals.insert(ident.escape());
            }

            return Ok(Some(Declaration {
                ident: ident.0.clone(),
                py_ident: ident.escape(),
                const_: false,
                const_assigned: None,
                loc: ident.1,
                typ: PyDeclType::GlobalCapture,
            }));
        }
        DeclType::Export => {
            if ctx.scope_ctx_stack.len() > 1 {
                return Err(TfErrBuilder::default()
                    .message("Export declaration is only allowed at the module level")
                    .span(ident.1)
                    .build_errs());
            }

            ctx.exports.push(ident.escape());

            return Ok(None);
        }
        DeclType::Let | DeclType::Const => {
            let Some(last_ctx) = ctx.scope_ctx_stack.last_mut() else {
                return Err(TfErrBuilder::default()
                    .message("Internal error: expected a scope")
                    .span(ident.1)
                    .build_errs());
            };

            let is_const = modifier == DeclType::Const;

            if last_ctx.is_class_scope || last_ctx.is_global_scope {
                let global = last_ctx.is_global_scope;

                if let Some(found) = ctx.scope_ctx_stack.find_decl(ident) {
                    return Err(TfErrBuilder::default()
                        .message("Cannot shadow declarations in a class or global scope")
                        .span(ident.1)
                        .context("Declared here", found.decl.loc)
                        .build_errs());
                }

                if is_const {
                    return Err(TfErrBuilder::default()
                        .message("Cannot declare a constant in a class or global scope")
                        .span(ident.1)
                        .build_errs());
                }

                // global scope.
                // add a declaration so that inner scopes know how to capture it

                return Ok(Some(Declaration {
                    ident: ident.clone().0,
                    py_ident: ident.escape(),
                    loc: ident.1,
                    const_: is_const,
                    const_assigned: None,
                    typ: if global {
                        PyDeclType::Global
                    } else {
                        PyDeclType::Local
                    },
                }));
            }

            let cur_scope = ctx.scope_ctx_stack.last().unwrap();

            // need to make sure variable is completely shadowed, so construct a unique identifier
            let py_ident = format!(
                "let_{}_{}_{}",
                ident.0.0,
                cur_scope.locals.len(),
                ctx.scope_id_counter
            )
            .into();

            let new_decl = Declaration {
                ident: ident.0.clone(),
                py_ident,
                loc: ident.1,
                const_: is_const,
                const_assigned: None,
                typ: PyDeclType::Local,
            };

            return Ok(Some(new_decl));
        }
    }
}

fn declare_var<'src>(
    ctx: &mut TfCtx<'src>,
    ident: &SIdent<'src>,
    modifier: DeclType,
) -> TfResult<()> {
    if let Some(decl) = create_declaration(ctx, ident, modifier)? {
        ctx.scope_ctx_stack
            .last_mut()
            .ok_or_else(|| {
                TfErrBuilder::default()
                    .message("Internal error: expected a scope")
                    .span(ident.1)
                    .build_errs()
            })?
            .locals
            .push(decl);
    }

    Ok(())
}

#[allow(dead_code)]
struct TfCtx<'src> {
    source: &'src str,
    exports: Vec<PyIdent<'src>>,
    module_star_exports: Vec<PyIdent<'src>>,

    allow_top_level_await: bool,

    placeholder_ctx_stack: Vec<PlaceholderCtx>,
    fn_ctx_stack: Vec<FnCtx<'src>>,
    scope_ctx_stack: Vec<ScopeCtx<'src>>,
    scope_id_counter: usize,

    line_cache: LineColCache,
}

impl<'src> TfCtx<'src> {
    fn new(source: &'src str) -> TfResult<Self> {
        Ok(TfCtx {
            allow_top_level_await: false,
            source,
            line_cache: LineColCache::new(source),
            exports: Vec::new(),
            module_star_exports: Vec::new(),

            scope_id_counter: 0,
            placeholder_ctx_stack: Vec::new(),
            fn_ctx_stack: Vec::new(),
            scope_ctx_stack: Vec::new(),
        })
    }

    fn linecol(&self, cursor: usize) -> (usize, usize) {
        self.line_cache.linecol(cursor)
    }

    fn create_aux_var(&self, typ: &str, cursor: usize) -> PyIdent<'src> {
        let (line, col) = self.linecol(cursor);
        format!("_{}_l{}c{}", typ, line, col).into()
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
    fn drop_expr(self, ctx: &mut TfCtx<'src>) -> PyBlock<'src>;
}

impl<'src> SPyExprWithPreExt<'src> for SPyExprWithPre<'src> {
    fn drop_expr(self, _ctx: &mut TfCtx<'src>) -> PyBlock<'src> {
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
    fn drop_expr(self, ctx: &mut TfCtx<'src>) -> PyBlock<'src> {
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
    fn transform<'ast>(&'ast self, ctx: &mut TfCtx<'src>) -> TfResult<PyBlockExprWithPre<'src>>;
}

impl<'src> BlockExt<'src> for [SStmt<'src>] {
    fn transform<'ast>(&'ast self, ctx: &mut TfCtx<'src>) -> TfResult<PyBlockExprWithPre<'src>> {
        if self.is_empty() {
            return Ok(WithPre {
                pre: PyBlock::new(),
                value: PyBlockExpr::Nothing,
            });
        }

        let mut pre = PyBlock::new();
        let mut errs = Vec::new();
        let mut ok = true;

        let mut handle_stmt = |stmt: &SStmt<'src>| {
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

        match &final_stmt.0 {
            Stmt::Expr(expr) => match expr.transform_with_placeholder_guard(ctx) {
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
            Err(TfErrs(errs))
        }
    }
}

fn destructure_list<'src, 'ast>(
    ctx: &mut TfCtx<'src>,
    target: &'ast SExpr<'src>,
    items: &'ast [ListItem<'src>],
    modifier: Option<DeclType>,
) -> TfResult<DestructureBindings<'src>> {
    let cursor_var = ctx.create_aux_var("des_curs", target.1.start);

    // a, b, *c = cursor_var

    let a = PyAstBuilder::new(target.1);
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
                    return Err(TfErrBuilder::default()
                        .message("Destructuring assignment with multiple spreads is not allowed")
                        .span(target.1)
                        .build_errs());
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
    ctx: &mut TfCtx<'src>,
    target: &'ast SExpr<'src>,
    items: &'ast [ListItem<'src>],
    modifier: Option<DeclType>,
) -> TfResult<DestructureBindings<'src>> {
    let cursor_var = ctx.create_aux_var("des_curs", target.1.start);
    let tuple_var = ctx.create_aux_var("des_tuple", target.1.start);
    let len_var = ctx.create_aux_var("des_len", target.1.start);

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
                    return Err(TfErrBuilder::default()
                        .message("Destructuring assignment with multiple spreads is not allowed")
                        .span(target.1)
                        .build_errs());
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
    ctx: &mut TfCtx<'src>,
    target: &'ast SExpr<'src>,
    items: &'ast [MappingItem<'src>],
    modifier: Option<DeclType>,
) -> TfResult<DestructureBindings<'src>> {
    let cursor_var = ctx.create_aux_var("des_curs", target.1.start);
    let dict_var = ctx.create_aux_var("des_dict", target.1.start);

    // dict_var = dict(cursor_var)
    let a = PyAstBuilder::new(target.1);
    let mut stmts = PyBlock(vec![a.assign(
        a.ident(dict_var.clone(), PyAccessCtx::Store),
        a.call(
            a.tl_builtin("unpack_record"),
            vec![a.call_arg(a.load_ident(cursor_var.clone()))],
        ),
    )]);

    let mut post_stmts = vec![];

    // a = dict_var.pop(a_key)
    // b = dict_var.pop(b_key)
    // c = dict_var

    let mut spread_var = None;
    for item in items.iter() {
        match item {
            MappingItem::Ident(ident) => stmts.push(a.assign(
                a.ident(ident.prepare_binding(ctx)?, PyAccessCtx::Store),
                a.call(
                    a.attribute(a.load_ident(dict_var.clone()), "pop", PyAccessCtx::Load),
                    vec![a.call_arg(a.literal(PyLiteral::Str(ident.prepare_binding(ctx)?)))],
                ),
            )),
            MappingItem::Item(key, expr) => {
                let item_bindings = destructure(ctx, expr, modifier)?;
                let key_node = key.transform(ctx)?;
                post_stmts.extend(key_node.pre);
                post_stmts.extend(item_bindings.post);

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

fn destructure_decls<'src, 'ast>(
    ctx: &mut TfCtx<'src>,
    target: &'ast SExpr<'src>,
    modifier: DeclType,
) -> TfResult<Vec<Declaration<'src>>> {
    let mut decls = vec![];

    match &target.0 {
        Expr::Ident(ident) => {
            if let Some(decl) = create_declaration(ctx, ident, modifier)? {
                decls.push(decl);
            }
        }
        Expr::RawAttribute(..) | Expr::Attribute(..) | Expr::Subscript(..) => {}
        Expr::List(items) | Expr::Tuple(items) => {
            for item in items {
                match item {
                    ListItem::Item(expr) => {
                        decls.extend(destructure_decls(ctx, expr, modifier)?);
                    }
                    ListItem::Spread(expr) => {
                        decls.extend(destructure_decls(ctx, expr, modifier)?);
                    }
                }
            }
        }
        Expr::Mapping(items) => {
            for item in items {
                match item {
                    MappingItem::Ident(ident) => {
                        if let Some(decl) = create_declaration(ctx, ident, modifier)? {
                            decls.push(decl);
                        }
                    }
                    MappingItem::Item(_, expr) => {
                        decls.extend(destructure_decls(ctx, expr, modifier)?);
                    }
                    MappingItem::Spread(expr) => {
                        decls.extend(destructure_decls(ctx, expr, modifier)?);
                    }
                }
            }
        }
        _ => {}
    };

    Ok(decls)
}

fn destructure<'src, 'ast>(
    ctx: &mut TfCtx<'src>,
    target: &'ast SExpr<'src>,
    modifier: Option<DeclType>,
) -> TfResult<DestructureBindings<'src>> {
    let mut post = PyBlock::new();

    let assign_to: SPyExpr<'src>;

    match &target.0 {
        Expr::Ident(..) | Expr::RawAttribute(..) | Expr::Attribute(..) | Expr::Subscript(..) => {
            let target_node = match &target.0 {
                Expr::Ident(ident) => {
                    ident.prepare_binding(ctx)?;
                    target.transform_with_access(ctx, PyAccessCtx::Store)?
                }
                Expr::RawAttribute(..) | Expr::Subscript(..) => {
                    if modifier.is_some() {
                        return Err(TfErrBuilder::default()
                            .message("Only identifiers allowed in this destructuring")
                            .span(target.1)
                            .build_errs());
                    }
                    target.transform_with_access(ctx, PyAccessCtx::Store)?
                }
                Expr::Attribute(lhs, ext) => {
                    if modifier.is_some() {
                        return Err(TfErrBuilder::default()
                            .message("Only identifiers allowed in this destructuring")
                            .span(target.1)
                            .build_errs());
                    }

                    // we need to replace an extension with a regular attribute
                    // when assigning to it

                    let new_target = (Expr::RawAttribute(lhs.clone(), ext.clone()), target.1);

                    new_target.transform_with_access(ctx, PyAccessCtx::Store)?
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
            return Err(TfErrBuilder::default()
                .message("Assignment target is not allowed")
                .span(target.1)
                .build_errs());
        }
    };

    Ok(DestructureBindings { post, assign_to })
}

fn transform_assignment<'src, 'ast>(
    ctx: &mut TfCtx<'src>,
    lhs: &'ast SExpr<'src>,
    rhs: &'ast SExpr<'src>,
    modifier: Option<DeclType>,
    span: &Span,
) -> TfResult<PyBlock<'src>> {
    let mut stmts = PyBlock::new();
    let a = PyAstBuilder::new(*span);

    if !ctx.scope_ctx_stack.last().unwrap().lifted_decls.is_empty() {
        return Err(TfErrBuilder::default()
            .message("Internal error: lifted decls should be empty at this point")
            .span(lhs.1)
            .build_errs());
    }

    if !ctx
        .scope_ctx_stack
        .last()
        .unwrap()
        .lifted_class_decls
        .is_empty()
    {
        return Err(TfErrBuilder::default()
            .message("Internal error: lifted class decls should be empty at this point")
            .span(lhs.1)
            .build_errs());
    }

    let decls = if let Some(modifier) = modifier {
        destructure_decls(ctx, lhs, modifier)?
    } else {
        vec![]
    };

    // mark lifted decls so they can be seen by function bodies (for recursion to work)
    ctx.scope_ctx_stack
        .last_mut()
        .unwrap()
        .lifted_decls
        .extend(decls);

    if let Expr::Ident(ident) = &lhs.0 {
        let mut decorators: Vec<&SExpr> = vec![];
        let mut cur_node = &rhs.0;

        loop {
            match cur_node {
                Expr::Binary(BinaryOp::Pipe, left, right) => {
                    cur_node = &left.0;
                    decorators.push(right);
                }
                Expr::Decorated(deco, right) => {
                    cur_node = &right.0;
                    decorators.push(deco);
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

            // commit the lifted decls in advance since this is a simple definition
            let scope = ctx.scope_ctx_stack.last_mut().unwrap();
            scope.locals.extend(scope.lifted_decls.drain(..));

            let py_ident = ident.prepare_binding(ctx)?;

            let fn_def = make_fn_def(
                ctx,
                py_ident.clone(),
                FnDef::TlFnDef(arglist, body),
                decorators,
                span,
            )?;

            // TODO give fn and class defs a better __name__ than the mangled ident

            return Ok(fn_def);
        } else if let Expr::Class(bases, body) = &cur_node {
            let decorators = py_decorators()?;

            // unlike functions, the class body should not know about the lifted decl yet
            // (except for inside member function body)
            // so we need to do some juggling here...

            // first let the binding know about the decl: move lifted to locals
            let scope = ctx.scope_ctx_stack.last_mut().unwrap();
            let n_lifted = scope.lifted_decls.len();
            scope.locals.extend(scope.lifted_decls.drain(..));

            let py_ident = ident.prepare_binding(ctx)?;

            // now hide it from the class def: move back to lifted
            let scope = ctx.scope_ctx_stack.last_mut().unwrap();
            scope
                .lifted_decls
                .extend(scope.locals.drain(scope.locals.len() - n_lifted..));

            // but make it available to the class methods: move to lifted classdefs
            scope
                .lifted_class_decls
                .extend(scope.lifted_decls.drain(..));

            let cls_def = make_class_def(ctx, py_ident.clone(), &bases, &body, decorators, span)?;

            // and finally commit to the current scope
            let scope = ctx.scope_ctx_stack.last_mut().unwrap();
            scope.locals.extend(scope.lifted_class_decls.drain(..));

            return Ok(cls_def);
        };
    };

    let value_node = rhs.transform_with_placeholder_guard(ctx)?;
    stmts.extend(value_node.pre);

    // now commit the lifted decls so that the destructure has a target
    let scope = ctx.scope_ctx_stack.last_mut().unwrap();
    scope.locals.extend(scope.lifted_decls.drain(..));

    let destructure = destructure(ctx, lhs, modifier)?;
    stmts.push(a.assign(destructure.assign_to, value_node.value));
    stmts.extend(destructure.post);

    Ok(stmts)
}

fn transform_if_matches_expr<'src, 'ast>(
    ctx: &mut TfCtx<'src>,
    subject: &'ast SExpr<'src>,
    pattern: &'ast SPattern<'src>,
    then_block: &'ast SExpr<'src>,
    else_block: Option<&'ast SExpr<'src>>,
    span: &Span,
) -> TfResult<SPyExprWithPre<'src>> {
    let mut pre = PyBlock::new();
    let subject = pre.bind(subject.transform(ctx)?);

    let a = PyAstBuilder::new(*span);
    let var = ctx.create_aux_var("if_matches", span.start);
    let meta = pattern.preprocess()?;

    let (pattern, on_success) = scoped(ctx, ScopeCtx::new(), |ctx| {
        for capture in &meta.captures {
            declare_var(ctx, &(capture.clone(), pattern.1), DeclType::Let)?;
        }

        // pattern needs to know about the declared vars above
        let pattern = pre.bind(pattern.transform(ctx)?);

        let mut py_then = PyBlock::new();
        let py_then_expr = py_then.bind(then_block.transform_expecting_new_block_scope(ctx)?);
        py_then.push(a.assign(a.ident(var.clone(), PyAccessCtx::Store), py_then_expr));

        Ok((pattern, py_then))
    })?
    .0;

    let pattern_span = pattern.tl_span;

    let on_fail = if let Some(else_block) = else_block {
        scoped(ctx, ScopeCtx::new(), |ctx| {
            let mut py_else = PyBlock::new();
            let py_else_expr = py_else.bind(else_block.transform_expecting_new_block_scope(ctx)?);
            py_else.push(a.assign(a.ident(var.clone(), PyAccessCtx::Store), py_else_expr));
            Ok(py_else)
        })?
        .0
    } else {
        PyBlock(vec![a.assign(
            a.ident(var.clone(), PyAccessCtx::Store),
            a.literal(PyLiteral::None),
        )])
    };

    let mut cases = vec![a.match_case(pattern, None, on_success)];

    if !meta.default {
        cases.push(a.match_case(
            (PyPattern::As(None, None), pattern_span).into(),
            None,
            on_fail,
        ))
    }

    pre.push(a.match_(subject, cases));

    Ok(SPyExprWithPre {
        value: a.ident(var, PyAccessCtx::Load),
        pre,
    })
}

fn transform_if_expr<'src, 'ast>(
    ctx: &mut TfCtx<'src>,
    cond: &'ast SExpr<'src>,
    then_block: &'ast SExpr<'src>,
    else_block: Option<&'ast SExpr<'src>>,
    span: &Span,
) -> TfResult<SPyExprWithPre<'src>> {
    if let Expr::Matches(expr, pattern) = &cond.0 {
        return transform_if_matches_expr(ctx, expr, pattern, then_block, else_block, span);
    }

    let mut pre = PyBlock::new();
    let cond = pre.bind(cond.transform(ctx)?);
    let a = PyAstBuilder::new(*span);

    let ret_varname = ctx.create_aux_var("ifexp", span.start);
    let store_ret_var = a.ident(ret_varname.clone(), PyAccessCtx::Store);
    let load_ret_var = a.ident(ret_varname.clone(), PyAccessCtx::Load);

    let py_then = scoped(ctx, ScopeCtx::new(), |ctx| {
        let mut py_then = PyBlock::new();
        let py_then_expr = py_then.bind(then_block.transform_expecting_new_block_scope(ctx)?);
        py_then.push(a.assign(store_ret_var.clone(), py_then_expr));
        Ok(py_then)
    })?
    .0;

    let py_else = if let Some(else_block) = else_block {
        scoped(ctx, ScopeCtx::new(), |ctx| {
            let mut py_else = PyBlock::new();
            let py_else_expr = py_else.bind(else_block.transform_expecting_new_block_scope(ctx)?);
            py_else.push(a.assign(store_ret_var.clone(), py_else_expr));
            Ok(py_else)
        })?
        .0
    } else {
        PyBlock(vec![
            a.assign(store_ret_var.clone(), a.literal(PyLiteral::None)),
        ])
    };

    pre.push(a.if_(cond, py_then, Some(py_else)));

    Ok(SPyExprWithPre {
        value: load_ret_var,
        pre,
    })
}

#[derive(Debug)]
struct PatternInfo<'src> {
    default: bool,
    captures: Vec<Ident<'src>>,
}

trait SPatternExt<'src> {
    fn preprocess(&self) -> TfResult<PatternInfo<'src>>;

    fn transform<'ast>(
        &'ast self,
        ctx: &mut TfCtx<'src>,
    ) -> TfResult<WithPre<'src, SPyPattern<'src>>>;
}

impl<'src> SPatternExt<'src> for SPattern<'src> {
    fn preprocess(&self) -> TfResult<PatternInfo<'src>> {
        let default = match &self.0 {
            Pattern::Capture(cap) => {
                // Make sure that capture patterns do not start with an uppercase letter to
                // prevent unexpectedly shadowing a type

                if cap
                    .as_ref()
                    .is_some_and(|x| char::is_uppercase(x.0.0.chars().nth(0).unwrap_or('_')))
                {
                    return Err(TfErrBuilder::default()
                                .message(
                                    "Capture patterns must start with a lowercase letter; to match a type, add '()'",
                                )
                                .span(self.1)
                                .build_errs());
                }

                let mut captures = vec![];
                if let Some(cap) = cap {
                    captures.push(cap.0.clone());
                }

                PatternInfo {
                    default: true,
                    captures,
                }
            }
            Pattern::As(pattern, name) => {
                let mut meta = pattern.preprocess()?;
                meta.captures.push(name.0.clone());
                meta
            }
            Pattern::Or(items) => {
                let mut iter = items.iter();
                let initial = iter.next().ok_or_else(|| {
                    TfErrBuilder::default()
                        .message("Or pattern must have at least one item")
                        .span(self.1)
                        .build_errs()
                })?;

                let mut initial = initial.preprocess()?;

                for item in iter {
                    if initial.default {
                        return Err(TfErrBuilder::default()
                            .message("Default pattern makes remaining patterns unreachable")
                            .span(item.1)
                            .build_errs())?;
                    }

                    let meta = item.preprocess()?;
                    initial.default |= meta.default;

                    if meta.captures != initial.captures {
                        return Err(TfErrBuilder::default()
                            .message("Or patterns must bind the same names")
                            .span(item.1)
                            .build_errs());
                    }
                }

                initial
            }
            Pattern::Value(..) | Pattern::Literal(..) => PatternInfo {
                default: false,
                captures: vec![],
            },
            Pattern::Sequence(items) => {
                let mut captures = vec![];
                for item in items {
                    match item {
                        PatternSequenceItem::Item(inner) => {
                            let meta = inner.preprocess()?;
                            captures.extend(meta.captures);
                        }
                        PatternSequenceItem::Spread(Some(inner)) => {
                            captures.push(inner.0.clone());
                        }
                        PatternSequenceItem::Spread(None) => {}
                    }
                }

                PatternInfo {
                    default: false,
                    captures,
                }
            }
            Pattern::Mapping(items) => {
                let mut captures = vec![];
                for item in items {
                    match item {
                        PatternMappingItem::Ident(id) => captures.push(id.0.clone()),
                        PatternMappingItem::Item(_key, value) => {
                            let meta = value.preprocess()?;
                            captures.extend(meta.captures);
                        }
                        PatternMappingItem::Spread(Some(id)) => {
                            captures.push(id.0.clone());
                        }
                        PatternMappingItem::Spread(None) => {}
                    }
                }

                PatternInfo {
                    default: false,
                    captures,
                }
            }
            Pattern::Class(_, items) => {
                let mut captures = vec![];

                for item in items {
                    match item {
                        PatternClassItem::Item(inner) => {
                            let meta = inner.preprocess()?;
                            captures.extend(meta.captures);
                        }
                        PatternClassItem::Kw(_, inner) => {
                            let meta = inner.preprocess()?;
                            captures.extend(meta.captures);
                        }
                    }
                }

                PatternInfo {
                    default: false,
                    captures,
                }
            }
        };

        Ok(default)
    }

    fn transform<'ast>(
        &'ast self,
        ctx: &mut TfCtx<'src>,
    ) -> TfResult<WithPre<'src, SPyPattern<'src>>> {
        // TODO avoid python syntaxerror by verifying that all branches in Or bind the same name
        // also check for no patterns after default pattern

        let mut pre = PyBlock::new();

        let (pattern, span) = self;

        fn capture_slot<'src>(
            ctx: &TfCtx<'src>,
            v: &Option<SIdent<'src>>,
        ) -> Option<PyIdent<'src>> {
            v.clone().and_then(|x| {
                if x.0.0 == "_" {
                    None
                } else {
                    Some(x.transform(ctx))
                }
            })
        }

        let transformed = match pattern {
            Pattern::As(pattern, ident) => PyPattern::As(
                Some(Box::new(pre.bind(pattern.transform(ctx)?))),
                Some(ident.transform(ctx).clone()),
            ),
            Pattern::Literal(literal) => match literal.0 {
                Literal::Num(..) | Literal::Str(..) => {
                    PyPattern::Value((PyExpr::Literal(literal.0.transform(ctx)?), *span).into())
                }
                Literal::Bool(..) | Literal::None => {
                    PyPattern::Singleton(literal.0.transform(ctx)?)
                }
            },
            Pattern::Capture(v) => PyPattern::As(None, capture_slot(ctx, v)),
            Pattern::Value(v) => {
                let v_node = pre.bind(v.transform(ctx)?);

                match v_node.value {
                    PyExpr::Literal(..) | PyExpr::Attribute(..) => PyPattern::Value(v_node),
                    _ => {
                        let var = ctx.create_aux_var("mproxy", span.start);
                        let a = PyAstBuilder::new(*span);
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
                    .map(|x| Ok(pre.bind(x.transform(ctx)?)))
                    .collect::<TfResult<Vec<_>>>()?;
                PyPattern::Or(items_nodes)
            }
            Pattern::Sequence(items) => {
                let mut seen_spread = false;
                let mut items_nodes = vec![];

                for item in items {
                    let node = match item {
                        PatternSequenceItem::Item(item) => {
                            PyPatternSequenceItem::Item(pre.bind(item.transform(ctx)?))
                        }
                        PatternSequenceItem::Spread(item) => {
                            if seen_spread {
                                return Err(TfErrBuilder::default()
                                            .message("Destructuring assignment with multiple spreads is not allowed")
                                            .span(*span)
                                            .build_errs());
                            }
                            seen_spread = true;
                            PyPatternSequenceItem::Spread(capture_slot(ctx, item))
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
                            if ident.0.0 == "_" {
                                return Err(TfErrBuilder::default()
                                    .message("Record matching '_' is not allowed")
                                    .span(*span)
                                    .build_errs());
                            }

                            kvps.push((
                                (PyExpr::Literal(PyLiteral::Str(ident.transform(ctx))), *span)
                                    .into(),
                                (PyPattern::As(None, Some(ident.transform(ctx))), *span).into(),
                            ));
                        }
                        PatternMappingItem::Item(key, value) => {
                            let key_node = key.transform(ctx)?;
                            pre.extend(key_node.pre);

                            let value_node = value.transform(ctx)?;
                            pre.extend(value_node.pre);

                            kvps.push((key_node.value, value_node.value));
                        }
                        PatternMappingItem::Spread(value) => {
                            if spread.is_some() {
                                return Err(TfErrBuilder::default()
                                        .message("Destructuring assignment with multiple spreads is not allowed")
                                        .span(*span)
                                        .build_errs());
                            }

                            spread = capture_slot(ctx, value);
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
                            values.push(pre.bind(value.transform(ctx)?));
                        }
                        PatternClassItem::Kw(key, value) => {
                            kvps.push((key.escape(), pre.bind(value.transform(ctx)?)));
                        }
                    }
                }

                let cls_node = pre.bind(cls.transform(ctx)?);

                PyPattern::Class(cls_node, values, kvps)
            }
        };

        Ok(WithPre {
            pre,
            value: (transformed, *span).into(),
        })
    }
}

/**
 * Creates a matcher that throws an error if the pattern does not match.
 */
fn create_throwing_matcher<'src, 'ast>(
    ctx: &mut TfCtx<'src>,
    pattern: &'ast SPattern<'src>,
    pattern_meta: &'ast PatternInfo<'src>,
) -> TfResult<(PyBlock<'src>, PyIdent<'src>)> {
    if let Pattern::Capture(Some(id)) = &pattern.0 {
        return Ok((PyBlock::new(), id.transform(ctx)));
    }

    let cursor = ctx.create_aux_var("matcher", pattern.1.start);

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
    ctx: &mut TfCtx<'src>,
    subject: SPyExpr<'src>,
    pattern: &'ast SPattern<'src>,
    pattern_meta: &'ast PatternInfo<'src>,
    on_success: PyBlock<'src>,
    on_fail: PyBlock<'src>,
) -> TfResult<PyBlock<'src>> {
    let mut post = PyBlock::new();
    let a = PyAstBuilder::new(pattern.1);
    let pattern_node = pattern.transform(ctx)?;

    post.extend(pattern_node.pre);

    let mut cases = vec![PyMatchCase {
        pattern: pattern_node.value,
        guard: None,
        body: on_success,
    }];

    if !pattern_meta.default {
        cases.push(PyMatchCase {
            pattern: (PyPattern::As(None, None), pattern.1).into(),
            guard: None,
            body: on_fail,
        });
    }

    post.push(a.match_(subject, cases));

    Ok(post)
}

fn transform_match_expr<'src, 'ast>(
    ctx: &mut TfCtx<'src>,
    subject: &'ast SExpr<'src>,
    cases: &[Indirect<MatchCase<'src>>],
    fill_default_case: bool,
    span: &Span,
) -> TfResult<(SPyExprWithPre<'src>, bool)> {
    let subject = subject.transform(ctx)?;
    let mut pre = subject.pre;
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
            return Err(TfErrBuilder::default()
                .message("Previous default case makes remaining cases unreachable")
                .span(case.body.1)
                .build_errs());
        }

        let meta = if let Some(pattern) = &case.pattern {
            pattern.preprocess()?
        } else {
            PatternInfo {
                default: true,
                captures: vec![],
            }
        };

        if case.guard.is_none() {
            if meta.default {
                has_default_case = true;
            }
        }

        scoped(ctx, ScopeCtx::new(), |ctx| {
            for capture in &meta.captures {
                declare_var(ctx, &(capture.clone(), case.body.1), DeclType::Let)?;
            }

            let pattern = if let Some(pattern) = &case.pattern {
                pre.bind(pattern.transform(ctx)?)
            } else {
                (PyPattern::As(None, None), *span).into()
            };

            let guard = if let Some(guard) = &case.guard {
                Some(pre.bind(guard.transform(ctx)?))
            } else {
                None
            };

            let py_block = case.body.transform_expecting_new_block_scope(ctx)?;
            let mut block_stmts = py_block.pre;

            block_stmts.push(a.assign(store_ret_var.clone(), py_block.value));

            py_cases.push(PyMatchCase {
                pattern,
                guard,
                body: block_stmts,
            });

            Ok(())
        })?;
    }

    if !has_default_case && fill_default_case {
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
    bases: &'ast [SCallItem<'src>],
    body: &'ast SExpr<'src>,
    decorators: PyDecorators<'src>,
    span: &Span,
) -> TfResult<PyBlock<'src>> {
    let mut stmts = PyBlock::new();
    let mut py_bases: Vec<PyCallItem<'src>> = vec![];

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
                PyCallItem::Kwarg(name.transform(ctx), expr_node.value)
            }
            _ => {
                return Err(TfErrBuilder::default()
                    .message("spread args are not allowed in class bases")
                    .span(*span)
                    .build_errs());
            }
        };

        py_bases.push(call_item);
    }

    let mut scope = ScopeCtx::new();
    scope.is_class_scope = true;

    let py_body = scoped(ctx, scope, |ctx| {
        let mut py_body = body
            .transform_expecting_new_block_scope(ctx)?
            .drop_expr(ctx);

        if py_body.is_empty() {
            py_body.push((PyStmt::Pass, *span).into());
        }

        Ok(py_body)
    })?
    .0;

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

/**
 * Prepares the argument list for a function.
 *
 * Caution: this function pushes a new scope onto the stack, but only if it succeeds.
 */
#[allow(non_snake_case)]
fn make_arglist_CAUTION<'src, 'ast>(
    ctx: &mut TfCtx<'src>,
    args: &'ast [ArgDefItem<'src>],
) -> TfResult<PyArgList<'src>> {
    let mut pre = PyBlock::new();
    let mut post = PyBlock::new();

    let make_decl = |capture: Ident<'src>, loc| -> Declaration<'src> {
        Declaration {
            ident: capture.clone(),
            py_ident: capture.escape(),
            const_: false,
            const_assigned: None,
            loc,
            typ: PyDeclType::Local,
        }
    };

    let mut args_vec = vec![];
    let mut defaults = vec![];

    // need to process defaults first so that they resolve to the outer scope
    for arg in args {
        defaults.push(match arg {
            ArgDefItem::Arg(_, Some(default)) => {
                Some(pre.bind(default.transform_with_placeholder_guard(ctx)?))
            }
            _ => None,
        })
    }

    ctx.scope_id_counter += 1;
    ctx.scope_ctx_stack.push(ScopeCtx::new());
    ctx.scope_ctx_stack.last_mut().unwrap().is_py_fn_scope = true;

    let mut rest = || {
        for (i, arg) in args.iter().enumerate() {
            let arg = match arg {
                ArgDefItem::Arg(arg_pattern, _) => {
                    // TODO avoid this clone
                    let default = defaults[i].clone();

                    let meta = arg_pattern.preprocess()?;

                    let scope_ctx = ctx.scope_ctx_stack.last_mut().unwrap();
                    for capture in &meta.captures {
                        if scope_ctx.locals.iter().any(|d| d.ident.0 == capture.0) {
                            return Err(TfErrBuilder::default()
                                .message("Duplicate argument name in function definition")
                                .span(arg_pattern.1)
                                .build_errs());
                        }

                        scope_ctx
                            .locals
                            .push(make_decl(capture.clone(), arg_pattern.1));
                    }

                    // TODO need to declare variables prior to matching
                    let (matcher, cursor) = create_throwing_matcher(ctx, arg_pattern, &meta)?;
                    post.extend(matcher);
                    PyArgDefItem::Arg(cursor, default)
                }
                ArgDefItem::ArgSpread(name) => {
                    let scope_ctx = ctx.scope_ctx_stack.last_mut().unwrap();
                    let decl = make_decl(name.0.clone(), name.1);
                    let py_ident = decl.py_ident.clone();
                    scope_ctx.locals.push(decl);
                    PyArgDefItem::ArgSpread(py_ident)
                }
                ArgDefItem::KwargSpread(name) => {
                    let scope_ctx = ctx.scope_ctx_stack.last_mut().unwrap();
                    let decl = make_decl(name.0.clone(), name.1);
                    let py_ident = decl.py_ident.clone();
                    scope_ctx.locals.push(decl);
                    PyArgDefItem::KwargSpread(py_ident)
                }
            };
            args_vec.push(arg);
        }

        Ok(())
    };

    let result = rest();
    if let Err(e) = result {
        ctx.scope_ctx_stack.pop();
        return Err(e);
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
    // Args, body, is_do, is_async
    PyFnDef(Vec<PyArgDefItem<'src>>, PyBlock<'src>, bool, bool),

    TlFnDef(&'ast [ArgDefItem<'src>], &'ast SExpr<'src>),
}

#[allow(dead_code)]
fn debug_scopes(ctx: &TfCtx) -> String {
    let mut s = String::new();

    for scope in &ctx.scope_ctx_stack {
        let mut locals = String::new();
        for decl in &scope.locals {
            locals.push_str(&format!("{}: {}, ", decl.ident.0, decl.py_ident));
        }

        let mut lifted = String::new();
        for decl in &scope.lifted_decls {
            lifted.push_str(&format!("{}: {}, ", decl.ident.0, decl.py_ident));
        }

        s.push_str(&format!(
            "SCOPE[ locals=[{}] lifted=[{}] ] ",
            locals, lifted
        ));
    }

    s
}

/**
 * This function uses lifted_decls to store future declarations to make recursion work.
 */
fn prepare_py_fn<'src, 'ast>(
    ctx: &mut TfCtx<'src>,
    def: FnDef<'src, 'ast>,
    span: &Span,
) -> TfResult<WithPre<'src, PartialPyFnDef<'src>>> {
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
        FnDef::TlFnDef(arglist, body) => {
            let PyArgList {
                pre: arg_pre,
                post,
                items: args_,
            } = make_arglist_CAUTION(ctx, arglist)?;

            args = args_;

            pre.extend(arg_pre);
            py_body.extend(post);

            let result = fn_scoped(ctx, FnCtx::new(), |ctx| {
                let n_scopes = ctx.scope_ctx_stack.len();

                // first we need to make sure that the class definition
                // is visible to functions,
                // but they should also be shadowed by class properties

                let n_classdef_lifted = if n_scopes >= 3 {
                    let classdef_scope = &mut ctx.scope_ctx_stack[n_scopes - 3];
                    let n = classdef_scope.lifted_class_decls.len();
                    classdef_scope
                        .locals
                        .extend(classdef_scope.lifted_class_decls.drain(..));
                    n
                } else {
                    0
                };

                // we need to temporarily lift declarations in the PREVIOUS scope
                // so that they can be seen by the function body
                let scope = &mut ctx.scope_ctx_stack[n_scopes - 2];

                let n_lifted = scope.lifted_decls.len();
                scope.locals.extend(scope.lifted_decls.drain(..));

                let body = body.transform_expecting_new_block_scope(ctx)?;

                // put the lifted decls back
                let scope = &mut ctx.scope_ctx_stack[n_scopes - 2];
                scope
                    .lifted_decls
                    .extend(scope.locals.drain(scope.locals.len() - n_lifted..));

                // and same for the lifted classdef decls
                if n_classdef_lifted > 0 {
                    let scope = &mut ctx.scope_ctx_stack[n_scopes - 3];
                    scope
                        .lifted_class_decls
                        .extend(scope.locals.drain(scope.locals.len() - n_classdef_lifted..));
                }

                Ok(body)
            });

            ctx.scope_ctx_stack.pop();

            let (body, fn_ctx) = result?;

            if fn_ctx.is_async {
                async_ = true;
            }

            if fn_ctx.is_do {
                decorators.push(a.tl_builtin("do"));
            }

            if !fn_ctx.nonlocals.is_empty() {
                py_body.push(a.nonlocal(fn_ctx.nonlocals.iter().map(|x| x.clone()).collect()));
            }
            if !fn_ctx.globals.is_empty() {
                py_body.push(a.global(fn_ctx.globals.iter().map(|x| x.clone()).collect()));
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
    ctx: &mut TfCtx<'src>,
    def: FnDef<'src, 'ast>,
    span: &Span,
) -> TfResult<SPyExprWithPre<'src>> {
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
                return Err(TfErrBuilder::default()
                    .message("Internal error: Expected a single return statement in function body")
                    .span(*span)
                    .build_errs());
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
    ctx: &mut TfCtx<'src>,
    name: Cow<'src, str>,
    def: FnDef<'src, 'ast>,
    mut decorators: PyDecorators<'src>,
    span: &Span,
) -> TfResult<PyBlock<'src>> {
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

fn fn_scoped<'src, F, O>(
    ctx: &mut TfCtx<'src>,
    scope: FnCtx<'src>,
    f: F,
) -> TfResult<(O, FnCtx<'src>)>
where
    F: FnOnce(&mut TfCtx<'src>) -> TfResult<O>,
{
    ctx.fn_ctx_stack.push(scope);
    let result = f(ctx);
    let scope_ctx = ctx.fn_ctx_stack.pop().unwrap();
    result.map(|value| (value, scope_ctx))
}

fn scoped<'src, F, O>(
    ctx: &mut TfCtx<'src>,
    scope: ScopeCtx<'src>,
    f: F,
) -> TfResult<(O, ScopeCtx<'src>)>
where
    F: FnOnce(&mut TfCtx<'src>) -> TfResult<O>,
{
    ctx.scope_id_counter += 1;
    ctx.scope_ctx_stack.push(scope);
    let result = f(ctx);
    let scope_ctx = ctx.scope_ctx_stack.pop().unwrap();
    result.map(|value| (value, scope_ctx))
}

fn placeholder_guard<'src, F>(
    ctx: &mut TfCtx<'src>,
    span: &Span,
    f: F,
) -> TfResult<SPyExprWithPre<'src>>
where
    F: FnOnce(&mut TfCtx<'src>) -> TfResult<SPyExprWithPre<'src>>,
{
    let fn_ctx = FnCtx::new();

    ctx.placeholder_ctx_stack.push(PlaceholderCtx::new(*span));
    ctx.fn_ctx_stack.push(fn_ctx);

    let inner_expr = match f(ctx) {
        Err(e) => {
            ctx.placeholder_ctx_stack.pop();
            ctx.fn_ctx_stack.pop();
            return Err(e);
        }
        Ok(expr) => expr,
    };

    let placeholder_ctx = ctx.placeholder_ctx_stack.pop().unwrap();
    let fn_ctx = ctx.fn_ctx_stack.pop().unwrap();

    if placeholder_ctx.activated {
        if fn_ctx.is_async {
            return Err(TfErrBuilder::default()
                .message("Await is not allowed as part of a placeholder-expression")
                .span(*span)
                .build_errs());
        }

        let var_name = placeholder_ctx.var_name(ctx);

        let mut body = PyBlock::new();
        let inner_expr = body.bind(inner_expr);

        body.push((PyStmt::Return(inner_expr), *span).into());

        let fn_exp = make_fn_exp(
            ctx,
            FnDef::PyFnDef(
                vec![PyArgDefItem::Arg(var_name, None)],
                body,
                fn_ctx.is_do,
                false,
            ),
            span,
        )?;

        Ok(fn_exp)
    } else {
        if fn_ctx.is_async {
            // this potential placeholder ctx generated an async ctx,
            // but wasn't actually a placeholder,
            // so we forward it down to the next fn_ctx
            ctx.fn_ctx_stack
                .set_async(ctx.allow_top_level_await, span)?;
        }

        if fn_ctx.is_do {
            // same as above
            ctx.fn_ctx_stack.set_do(span)?;
        }

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

fn transform_call_items<'src, 'ast>(
    ctx: &mut TfCtx<'src>,
    args: &'ast [SCallItem<'src>],
    span: &Span,
) -> TfResult<WithPre<'src, Vec<PyCallItem<'src>>>> {
    let mut started_kwargs = false;
    let mut call_items = vec![];

    let mut pre = PyBlock::new();

    for arg in args {
        match &arg.0 {
            CallItem::Arg(expr) => {
                if started_kwargs {
                    return Err(TfErrBuilder::default()
                        .message("Cannot have args after kwargs")
                        .span(*span)
                        .build_errs());
                }

                let e = pre.bind(expr.transform_with_deep_placeholder_guard(ctx)?);
                call_items.push(PyCallItem::Arg(e));
            }
            CallItem::Kwarg(name, expr) => {
                started_kwargs = true;
                let e = pre.bind(expr.transform_with_deep_placeholder_guard(ctx)?);
                call_items.push(PyCallItem::Kwarg(name.transform(ctx), e));
            }
            CallItem::ArgSpread(expr) => {
                if started_kwargs {
                    return Err(TfErrBuilder::default()
                        .message("Cannot have arg spread after kwargs")
                        .span(*span)
                        .build_errs());
                }

                let e = pre.bind(expr.transform_with_deep_placeholder_guard(ctx)?);
                call_items.push(PyCallItem::ArgSpread(e));
            }
            CallItem::KwargSpread(expr) => {
                started_kwargs = true;
                let e = pre.bind(expr.transform_with_deep_placeholder_guard(ctx)?);
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
    ctx: &mut TfCtx<'src>,
    indices: &'ast [ListItem<'src>],
    span: &Span,
) -> TfResult<WithPre<'src, SPyExpr<'src>>> {
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
                    .collect::<TfResult<Vec<_>>>()?,
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
    ctx: &mut TfCtx<'src>,
    expr: &'ast SExpr<'src>,
    access_ctx: PyAccessCtx,
) -> TfResult<SPyExprWithPre<'src>> {
    let (lift_lhs, lhs_node) = match &expr.0 {
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
            return Err(TfErrBuilder::default()
                .message("Internal error: Postfix expressions can only be attributes, subscripts, calls, or extensions")
                .span(expr.1)
                .build_errs());
        }
    };

    if let Expr::Attribute(..) | Expr::RawAttribute(..) = &expr.0 {
    } else {
        if access_ctx != PyAccessCtx::Load {
            return Err(TfErrBuilder::default()
                .message("Internal error: Cannot use null-coalescing in a non-Load context")
                .span(expr.1)
                .build_errs());
        }
    }

    let mut pre = PyBlock::new();
    let a = PyAstBuilder::new(expr.1);

    let lhs = if lift_lhs {
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

    let node = match &expr.0 {
        Expr::Call(_, list) => {
            let t = pre.bind(transform_call_items(ctx, &list, &expr.1)?);
            a.call(lhs, t)
        }
        Expr::MappedCall(_, list) => {
            let t = pre.bind(transform_call_items(ctx, &list, &expr.1)?);
            guard_if_expr(a.call(lhs.clone(), t))
        }
        Expr::Subscript(_, list) => {
            let t = pre.bind(transform_subscript_items(ctx, &list, &expr.1)?);
            a.subscript(lhs, t, access_ctx)
        }
        Expr::MappedSubscript(_, list) => {
            let t = pre.bind(transform_subscript_items(ctx, &list, &expr.1)?);
            guard_if_expr(a.subscript(lhs.clone(), t, access_ctx))
        }
        Expr::RawAttribute(_, attr) => a.attribute(lhs, attr.escape(), access_ctx),
        Expr::MappedRawAttribute(_, attr) => {
            guard_if_expr(a.attribute(lhs.clone(), attr.escape(), PyAccessCtx::Load))
        }
        Expr::ScopedAttribute(_, rhs) => {
            let t = pre.bind(rhs.transform_with_placeholder_guard(ctx)?);
            a.call(
                a.tl_builtin("partial"),
                vec![PyCallItem::Arg(t), PyCallItem::Arg(lhs)],
            )
        }
        Expr::MappedScopedAttribute(_, rhs) => {
            let t = pre.bind(rhs.transform_with_placeholder_guard(ctx)?);
            guard_if_expr(a.call(
                a.tl_builtin("partial"),
                vec![PyCallItem::Arg(t), PyCallItem::Arg(lhs.clone())],
            ))
        }
        Expr::Attribute(_, rhs) => a.call(
            a.tl_builtin("vget"),
            vec![
                a.call_arg(lhs),
                a.call_arg(a.literal(PyLiteral::Str(rhs.escape()))),
            ],
        ),
        Expr::MappedAttribute(_, rhs) => guard_if_expr(a.call(
            a.tl_builtin("vget"),
            vec![
                a.call_arg(lhs.clone()),
                a.call_arg(a.literal(PyLiteral::Str(rhs.escape()))),
            ],
        )),
        _ => {
            return Err(TfErrBuilder::default()
                .message("Internal error: Postfix expressions can only be attributes, subscripts, calls, or extensions")
                .span(expr.1)
                .build_errs());
        }
    };

    Ok(SPyExprWithPre { value: node, pre })
}

fn matching_except_handler<'src>(
    ctx: &mut TfCtx<'src>,
    var_name: PyIdent<'src>,
    handlers: &[Indirect<MatchCase<'src>>],
    span: &Span,
) -> TfResult<PyExceptHandler<'src>> {
    let a = PyAstBuilder::new(*span);
    let b = AstBuilder::new(*span);
    let mut block = PyBlock::new();

    let (mut match_stmt, has_default) =
        transform_match_expr(ctx, &b.ident(var_name.clone()), handlers, false, span)?;

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
        name: Some(var_name),
        body: block,
    })
}

trait ListItemsExt<'src> {
    fn transform<'ast>(
        &'ast self,
        ctx: &mut TfCtx<'src>,
    ) -> TfResult<WithPre<'src, Vec<PyListItem<'src>>>>;
}

impl<'src> ListItemsExt<'src> for Vec<ListItem<'src>> {
    fn transform<'ast>(
        &'ast self,
        ctx: &mut TfCtx<'src>,
    ) -> TfResult<WithPre<'src, Vec<PyListItem<'src>>>> {
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
    fn transform<'ast>(&'ast self, ctx: &TfCtx<'src>) -> TfResult<PyLiteral<'src>>;
}

impl<'src> LiteralExt<'src> for Literal<'src> {
    fn transform<'ast>(&'ast self, _ctx: &TfCtx<'src>) -> TfResult<PyLiteral<'src>> {
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
    fn transform<'ast>(&'ast self, ctx: &mut TfCtx<'src>) -> TfResult<PyBlock<'src>>;
}

impl<'src> SStmtExt<'src> for SStmt<'src> {
    fn transform<'ast>(&'ast self, ctx: &mut TfCtx<'src>) -> TfResult<PyBlock<'src>> {
        let mut pre = PyBlock::new();
        let (stmt, span) = self;
        let a = PyAstBuilder::new(*span);

        let is_top_level = ctx.fn_ctx_stack.is_empty();

        match &stmt {
            Stmt::Expr(expr) => {
                let expr = pre.bind(expr.transform_with_placeholder_guard(ctx)?);
                pre.push(a.expr(expr));
            }
            Stmt::Assert(expr, msg) => {
                let expr = pre.bind(expr.transform_with_placeholder_guard(ctx)?);

                let msg = if let Some(msg) = msg {
                    Some(pre.bind(msg.transform_with_placeholder_guard(ctx)?))
                } else {
                    None
                };

                pre.push(a.assert(expr, msg));
            }
            Stmt::Return(expr) => {
                let expr = pre.bind(expr.transform_with_placeholder_guard(ctx)?);
                pre.push(a.return_(expr));
            }
            Stmt::Decl(idents, modifier) => {
                for ident in idents {
                    declare_var(ctx, ident, *modifier)?;
                }
            }
            Stmt::Assign(target, value, modifier) => {
                let binding_stmts = transform_assignment(ctx, target, value, *modifier, span)?;

                pre.extend(binding_stmts);
            }
            Stmt::Raise(expr) => {
                if let Some(expr) = expr {
                    let expr = pre.bind(expr.transform_with_placeholder_guard(ctx)?);
                    pre.push(a.raise(Some(expr)));
                } else {
                    pre.push(a.raise(None));
                }
            }
            Stmt::For(target, iter, body) => {
                let iter = pre.bind(iter.transform_with_placeholder_guard(ctx)?);

                let mut py_body = PyBlock::new();

                scoped(ctx, ScopeCtx::new(), |ctx| {
                    let meta = target.preprocess()?;

                    let (matcher, cursor) = create_throwing_matcher(ctx, target, &meta)?;
                    py_body.extend(matcher);
                    py_body.extend(
                        body.transform_expecting_new_block_scope(ctx)?
                            .drop_expr(ctx),
                    );

                    pre.push(a.for_(
                        a.ident(cursor.clone(), PyAccessCtx::Store),
                        a.call(
                            a.tl_builtin("vget"),
                            vec![
                                a.call_arg(iter),
                                a.call_arg(a.literal(PyLiteral::Str("iter".into()))),
                            ],
                        ),
                        py_body,
                    ));

                    Ok(())
                })?;
            }
            Stmt::While(cond, body) => {
                let (cond_node, fn_ctx) = fn_scoped(ctx, FnCtx::new(), |ctx| {
                    cond.transform_with_placeholder_guard(ctx)
                })?;

                let body_block = scoped(ctx, ScopeCtx::new(), |ctx| {
                    Ok(body
                        .transform_expecting_new_block_scope(ctx)?
                        .drop_expr(ctx))
                })?
                .0;

                let cond: SPyExpr<'src> = if cond_node.pre.is_empty() {
                    cond_node.value
                } else {
                    if fn_ctx.is_async {
                        // TODO revisit this!
                        return Err(TfErrBuilder::default()
                            .message("Await is not allowed in this complex loop condition")
                            .span(*span)
                            .build_errs());
                    }

                    if fn_ctx.is_do {
                        return Err(TfErrBuilder::default()
                            .message("Binding is not allowed in this complex loop condition")
                            .span(*span)
                            .build_errs());
                    }

                    let aux_fn = pre.bind(make_fn_exp(
                        ctx,
                        FnDef::PyFnDef(vec![], cond_node.pre, false, false),
                        span,
                    )?);

                    a.call(aux_fn, vec![])
                };

                pre.push((PyStmt::While(cond, body_block), *span).into());
            }
            Stmt::Try(body, excepts, finally) => {
                let body_block = scoped(ctx, ScopeCtx::new(), |ctx| {
                    Ok(body
                        .transform_expecting_new_block_scope(ctx)?
                        .drop_expr(ctx))
                })?
                .0;

                let finally_block = if let Some(finally) = finally {
                    Some(
                        scoped(ctx, ScopeCtx::new(), |ctx| {
                            Ok(finally
                                .transform_expecting_new_block_scope(ctx)?
                                .drop_expr(ctx))
                        })?
                        .0,
                    )
                } else {
                    None
                };

                let var_name = ctx.create_aux_var("e", span.start);

                let excepts = matching_except_handler(ctx, var_name.into(), excepts, span)?;

                pre.push(a.try_(body_block, vec![excepts], finally_block));
            }
            Stmt::Break => pre.push(a.break_()),
            Stmt::Continue => pre.push(a.continue_()),
            Stmt::Import(import_stmt) => {
                let mut aliases = vec![];

                let base_module = import_stmt
                    .trunk
                    .iter()
                    .map(|ident| ident.0.0.as_ref())
                    .collect::<Vec<_>>()
                    .join(".");

                let full_module = ".".repeat(import_stmt.level) + &base_module;

                if import_stmt.reexport {
                    if !is_top_level {
                        return Err(TfErrBuilder::default()
                            .message("Re-exporting imports is not allowed inside functions")
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
                                name: ident.escape(),
                                as_name: alias.as_ref().map(|a| a.escape()),
                            });
                        }

                        if import_stmt.reexport {
                            // alias else orig_name
                            let export_aliases: Vec<_> = aliases
                                .iter()
                                .map(|x| x.as_name.as_ref().unwrap_or(&x.name).clone())
                                .collect();

                            ctx.exports.extend(export_aliases);
                        }
                    }
                }

                if !import_stmt.trunk.is_empty() {
                    if import_stmt.level == 0 {
                        pre.push(
                            a.import(vec![a.import_alias(import_stmt.trunk[0].escape(), None)]),
                        )
                    }
                    pre.push(a.import_from(Some(base_module.into()), aliases, import_stmt.level));
                } else if import_stmt.level != 0 {
                    pre.push(a.import_from(None, aliases, import_stmt.level));
                } else {
                    pre.push(a.import(aliases));
                };
            }
            Stmt::Err => {
                return Err(TfErrBuilder::default()
                    .message(
                        "unexpected statement error (should have been caught in lexer)".to_owned(),
                    )
                    .span(*span)
                    .build_errs());
            }
            Stmt::Module => {
                return Err(TfErrBuilder::default()
                    .message("Module statements are not allowed in the transform phase".to_owned())
                    .span(*span)
                    .build_errs());
            }
        };

        Ok(pre)
    }
}

trait SExprExt<'src> {
    fn transform<'ast>(&'ast self, ctx: &mut TfCtx<'src>) -> TfResult<SPyExprWithPre<'src>>;

    /**
     * Transforms
     * expr
     * to
     * x = expr
     * x
     *
     * to avoid evaluating expr multiple times
     */
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

    fn transform_expecting_new_block_scope<'ast>(
        &'ast self,
        ctx: &mut TfCtx<'src>,
    ) -> TfResult<SPyExprWithPre<'src>>;

    fn transform_full<'ast>(
        &'ast self,
        ctx: &mut TfCtx<'src>,
        py_ctx: PyAccessCtx,
        create_new_block_scope: bool,
    ) -> TfResult<SPyExprWithPre<'src>>;
}

impl<'src> SExprExt<'src> for SExpr<'src> {
    fn transform<'ast>(&'ast self, ctx: &mut TfCtx<'src>) -> TfResult<SPyExprWithPre<'src>> {
        self.transform_with_access(ctx, PyAccessCtx::Load)
    }

    fn transform_lifted<'ast>(&'ast self, ctx: &mut TfCtx<'src>) -> TfResult<SPyExprWithPre<'src>> {
        let mut pre = PyBlock::new();
        let value = pre.bind(self.transform(ctx)?);
        let a = PyAstBuilder::new(self.1);

        let expr = match self.0 {
            Expr::Ident(..) | Expr::Literal(..) => value,
            _ => {
                let temp_var = ctx.create_aux_var("tmp", self.1.start);

                pre.push(a.assign(a.ident(temp_var.clone(), PyAccessCtx::Store), value));

                a.ident(temp_var, PyAccessCtx::Load)
            }
        };

        Ok(SPyExprWithPre { value: expr, pre })
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
        py_ctx: PyAccessCtx,
    ) -> TfResult<SPyExprWithPre<'src>> {
        self.transform_full(ctx, py_ctx, true)
    }

    fn transform_expecting_new_block_scope<'ast>(
        &'ast self,
        ctx: &mut TfCtx<'src>,
    ) -> TfResult<SPyExprWithPre<'src>> {
        self.transform_full(ctx, PyAccessCtx::Load, false)
    }

    fn transform_full<'ast>(
        &'ast self,
        ctx: &mut TfCtx<'src>,
        access_ctx: PyAccessCtx,
        create_new_block_scope: bool,
    ) -> TfResult<SPyExprWithPre<'src>> {
        let (expr, span) = self;
        let a = PyAstBuilder::new(*span);

        let mut pre = PyBlock::<'src>::new();

        match &expr {
            Expr::RawAttribute(..) | Expr::Subscript(..) | Expr::Ident(..) => {}
            _ => {
                if access_ctx != PyAccessCtx::Load {
                    return Err(TfErrBuilder::default()
                        .message("Expression context must be Load for this expression")
                        .span(*span)
                        .build_errs());
                }
            }
        }

        let value: SPyExpr<'src> = match &expr {
            Expr::Checked(expr, pattern) => {
                let b = AstBuilder::new(*span);

                let var_name = ctx.create_aux_var("chk", span.start);

                // exception variable doesn't leave the except slope, so rebind it to chk
                let err_name = ctx.create_aux_var("e", span.start);

                // TODO avoid declaring variable here by constructing the output directly
                // in the python ast
                declare_var(ctx, &(Ident(var_name.clone()), expr.1), DeclType::Let)?;
                declare_var(ctx, &(Ident(err_name.clone()), expr.1), DeclType::Let)?;

                let try_body =
                    b.block_expr(vec![b.assign(b.ident(var_name.clone()), expr.clone())]);

                let mut handlers = vec![];
                if let Some(pattern) = pattern {
                    handlers.push(Indirect::new(MatchCase {
                        pattern: Some(pattern.clone()),
                        guard: None,
                        body: b.block_expr(vec![
                            b.assign(b.ident(var_name.clone()), b.ident(err_name.clone())),
                        ]),
                    }));
                } else {
                    handlers.push(Indirect::new(MatchCase {
                        pattern: None,
                        guard: None,
                        body: b.block_expr(vec![
                            b.assign(b.ident(var_name.clone()), b.ident(err_name.clone())),
                        ]),
                    }));
                }

                let py_err_name = (Ident(err_name.clone()), *span).transform(ctx);

                let except_handler = matching_except_handler(ctx, py_err_name, &handlers, span)?;

                pre.push(a.try_(
                    try_body.transform(ctx)?.drop_expr(ctx),
                    vec![except_handler],
                    None,
                ));

                pre.bind(b.ident(var_name).transform(ctx)?)
            }
            Expr::Placeholder => pre.bind(transform_placeholder(ctx, span, access_ctx)?),
            Expr::Fn(arglist, body) => {
                pre.bind(make_fn_exp(ctx, FnDef::TlFnDef(arglist, body), span)?)
            }
            Expr::Class(bases, body) => {
                let name: Cow<_> = ctx.create_aux_var("clsexp", span.start).into();

                pre.extend(make_class_def(
                    ctx,
                    name.clone(),
                    bases,
                    body,
                    PyDecorators::new(),
                    span,
                )?);

                a.load_ident(name)
            }
            Expr::Decorated(deco, expr) => a.call(
                pre.bind(deco.transform(ctx)?),
                vec![PyCallItem::Arg(pre.bind(expr.transform(ctx)?))],
            ),
            Expr::Literal(lit) => a.literal(lit.0.transform(ctx)?),
            Expr::Ident(ident) => a.ident(ident.transform(ctx), access_ctx),
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
                span,
            )?),
            Expr::Block(block) => {
                let block = if create_new_block_scope {
                    pre.bind(scoped(ctx, ScopeCtx::new(), |ctx| block.transform(ctx))?.0)
                } else {
                    pre.bind(block.transform(ctx)?)
                };

                match block {
                    PyBlockExpr::Expr(expr) => expr,
                    PyBlockExpr::Nothing | PyBlockExpr::Never => a.none(),
                }
            }
            Expr::Match(subject, cases) => {
                pre.bind(transform_match_expr(ctx, subject, cases, true, span)?.0)
            }
            Expr::Matches(subject, pattern) => {
                let subject = pre.bind(subject.transform(ctx)?);

                let a = PyAstBuilder::new(*span);
                let var = ctx.create_aux_var("matches", span.start);

                let meta = pattern.preprocess()?;
                if meta.captures.len() > 0 {
                    return Err(TfErrBuilder::default()
                        .message("Capturing non-'_' names in a 'matches' condition is only allowed in 'if ... matches ...'.")
                        .span(*span)
                        .build_errs());
                }

                let matcher = create_binary_matcher(
                    ctx,
                    subject,
                    pattern,
                    &meta,
                    PyBlock(vec![a.assign(
                        a.ident(var.clone(), PyAccessCtx::Store),
                        a.literal(PyLiteral::Bool(true)),
                    )]),
                    PyBlock(vec![a.assign(
                        a.ident(var.clone(), PyAccessCtx::Store),
                        a.literal(PyLiteral::Bool(false)),
                    )]),
                )?;

                pre.extend(matcher);

                a.load_ident(var.clone())
            }
            Expr::Binary(op, lhs, rhs) => 'block: {
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
            Expr::Await(expr) => {
                ctx.fn_ctx_stack
                    .set_async(ctx.allow_top_level_await, span)?;
                a.await_(pre.bind(expr.transform(ctx)?))
            }
            Expr::Yield(expr) => a.yield_(pre.bind(expr.transform(ctx)?)),
            Expr::YieldFrom(expr) => a.yield_from(a.call(
                a.tl_builtin("vget"),
                vec![
                    a.call_arg(pre.bind(expr.transform(ctx)?)),
                    a.call_arg(a.literal(PyLiteral::Str("iter".into()))),
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
                        ctx.fn_ctx_stack.set_do(span)?;
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
                        MappingItem::Ident(id) => {
                            dict_items.push(PyDictItem::Item(
                                a.literal(PyLiteral::Str(id.escape())),
                                a.load_ident(id.transform(ctx)),
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

                nodes.push(PyFstrPart::Str(begin.0.clone().into()));

                for (fmt_expr, str_part) in parts {
                    // TODO format specifiers?
                    let expr = pre.bind(fmt_expr.0.expr.transform(ctx)?);

                    nodes.push(PyFstrPart::Expr(expr, None));
                    nodes.push(PyFstrPart::Str(str_part.0.clone().into()));
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
    block: &'ast [SStmt<'src>],
    allow_await: bool,
) -> TfResult<TransformOutput<'src>> {
    let mut ctx = TfCtx::new(source)?;
    ctx.allow_top_level_await = allow_await;

    let mut py_block = PyBlock::new();
    let mut scope = ScopeCtx::new();
    scope.is_global_scope = true;

    let block = py_block.bind(scoped(&mut ctx, scope, |ctx| block.transform(ctx))?.0);

    if !ctx.scope_ctx_stack.is_empty() {
        return Err(TfErrBuilder::default()
            .message("Internal error: Scope stack is not empty after transformation")
            .build_errs());
    }

    if let PyBlockExpr::Expr(value) = block {
        let span = value.tl_span;
        py_block.push((PyStmt::Expr(value), span).into());
    }

    Ok(TransformOutput {
        py_block,
        exports: ctx.exports,
        module_star_exports: ctx.module_star_exports,
    })
}
