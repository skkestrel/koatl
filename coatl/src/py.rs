use parser::*;
use pyo3::{
    call::PyCallArgs,
    prelude::*,
    types::{PyDict, PyList, PyString},
};

use crate::ast::AstBuilder;

pub struct TlErr {
    pub message: String,
    pub py_err: Option<PyErr>,
    pub span: Option<Span>,
}

pub struct TlErrs(pub Vec<TlErr>);

impl TlErrs {
    pub fn new() -> Self {
        TlErrs(vec![])
    }

    pub fn extend(&mut self, other: TlErrs) {
        self.0.extend(other.0);
    }
}

#[derive(Default)]
pub struct TlErrBuilder {
    message: String,
    py_err: Option<PyErr>,
    span: Option<Span>,
}

impl TlErrBuilder {
    pub fn py_err(mut self, py_err: PyErr) -> Self {
        self.py_err = Some(py_err);
        self
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn message<S: Into<String>>(mut self, message: S) -> Self {
        self.message = message.into();
        self
    }

    fn optional<T>(self, option: Option<T>, f: impl FnOnce(Self, T) -> Self) -> Self {
        if let Some(value) = option {
            f(self, value)
        } else {
            self
        }
    }

    pub fn build(mut self) -> TlErr {
        if self.message.is_empty() && self.py_err.is_some() {
            self.message = "Python error".to_owned();
        }

        TlErr {
            message: self.message,
            py_err: self.py_err,
            span: self.span,
        }
    }

    pub fn build_errs(self) -> TlErrs {
        TlErrs(vec![self.build()])
    }
}

impl From<PyErr> for TlErrs {
    fn from(err: PyErr) -> Self {
        TlErrBuilder::default().py_err(err).build_errs()
    }
}

pub type TlResult<T> = Result<T, TlErrs>;

#[allow(dead_code)]
enum PyAccessCtx {
    Load,
    Store,
    Del,
}

struct TlCtx<'py> {
    py: Python<'py>,
    source: &'py str,
    ast_module: Bound<'py, PyModule>,
    constant_node: Bound<'py, PyAny>,
}

impl<'py> TlCtx<'py> {
    fn new(py: Python<'py>, source: &'py str) -> TlResult<Self> {
        let ast_module = py.import("ast")?;
        let constant_node = ast_module.getattr("Constant")?;

        Ok(TlCtx {
            py,
            source,
            ast_module,
            constant_node,
        })
    }

    fn linecol(&self, cursor: usize) -> (usize, usize) {
        let line = self.source[..cursor].lines().count();
        let col = self.source[..cursor]
            .lines()
            .last()
            .map_or(0, |line| line.len());

        (line, col)
    }

    fn temp_var_name(&self, typ: &str, cursor: usize) -> String {
        let (line, col) = self.linecol(cursor);
        format!("__tl_{}_l{}c{}", typ, line, col)
    }

    fn method1_unbound<A>(
        &self,
        method: &str,
        args: A,
        err_span: Option<&Span>,
    ) -> TlResult<PyObject>
    where
        A: PyCallArgs<'py>,
    {
        self.ast_module
            .call_method1(method, args)
            .map_err(|e| {
                TlErrBuilder::default()
                    .message(format!("Failed to call method {}: {}", method, e))
                    .py_err(e)
                    .optional(err_span, |builder, span| builder.span(span.clone()))
                    .build_errs()
            })
            .map(|obj| obj.unbind())
    }

    fn name_ctx(&self, ctx: PyAccessCtx) -> TlResult<Bound<'py, PyAny>> {
        Ok(self.ast_module.getattr(match ctx {
            PyAccessCtx::Load => "Load",
            PyAccessCtx::Store => "Store",
            PyAccessCtx::Del => "Del",
        })?)
    }

    fn name(&self, name: &str, ctx: PyAccessCtx, err_span: Option<&Span>) -> TlResult<PyObject> {
        self.method1_unbound("Name", (name, self.name_ctx(ctx)?), err_span)
    }

    fn constant<T: pyo3::IntoPyObject<'py>>(&self, value: T) -> TlResult<PyObject> {
        Ok(self.constant_node.call1((value,))?.unbind())
    }
}

struct PyBlock {
    stmts: PyStmts,
    final_expr: Option<PyExpr>,
}

fn transpile_block_with_final_stmt<'py>(ast: &TlCtx<'py>, block: &SBlock) -> TlResult<PyStmts> {
    let (block, span) = block;

    match block {
        Block::Stmts(stmts) => {
            let mut errs = vec![];
            let mut t_stmts = vec![];

            let mut ok = true;

            for stmt in stmts {
                match transpile_stmt(ast, stmt) {
                    Ok(transpiled) => {
                        t_stmts.extend(transpiled);
                    }
                    Err(e) => {
                        errs.extend(e.0);
                        ok = false;
                    }
                }
            }

            if ok {
                return Ok(t_stmts);
            } else {
                return Err(TlErrs(errs));
            }
        }
        Block::Expr(sexpr) => {
            let t = transpile_expr(ast, &sexpr)?;

            let mut stmts = t.aux_stmts;
            stmts.push(ast.method1_unbound("Expr", (t.expr,), Some(span))?);

            Ok(stmts)
        }
    }
}

fn transpile_block_with_final_expr<'py>(ast: &TlCtx<'py>, block: &SBlock) -> TlResult<PyBlock> {
    let (block, span) = block;

    match block {
        Block::Stmts(stmts) => {
            if stmts.is_empty() {
                return Ok(PyBlock {
                    stmts: vec![],
                    final_expr: None,
                });
            }

            let mut t_stmts = Vec::new();
            let mut errs = Vec::new();
            let mut ok = true;

            let mut final_expr = None;
            let mut iter = stmts.iter();

            for stmt in iter.by_ref().take(stmts.len() - 1) {
                match transpile_stmt(ast, stmt) {
                    Ok(transpiled) => {
                        t_stmts.extend(transpiled);
                    }
                    Err(e) => {
                        errs.extend(e.0);
                        ok = false;
                    }
                }
            }

            let final_stmt = iter.next().unwrap();

            match &final_stmt.0 {
                Stmt::Expr(expr) => match transpile_expr(ast, &expr) {
                    Ok(expr_with_aux) => {
                        t_stmts.extend(expr_with_aux.aux_stmts);
                        final_expr = Some(expr_with_aux.expr);
                    }
                    Err(e) => {
                        errs.extend(e.0);
                        ok = false;
                    }
                },
                _ => match transpile_stmt(ast, final_stmt) {
                    Ok(transpiled) => {
                        t_stmts.extend(transpiled);
                    }
                    Err(e) => {
                        errs.extend(e.0);
                        ok = false;
                    }
                },
            }

            if ok {
                Ok(PyBlock {
                    stmts: t_stmts,
                    final_expr: final_expr,
                })
            } else {
                Err(TlErrs(errs))
            }
        }
        Block::Expr(sexpr) => {
            let t = transpile_expr(ast, &sexpr)?;

            Ok(PyBlock {
                stmts: t.aux_stmts,
                final_expr: Some(t.expr),
            })
        }
    }
}

fn make_lvalue<'py>(ast: &TlCtx<'py>, node: &PyObject, span: &Span) -> TlResult<()> {
    node.getattr(ast.py, "ctx").map_err(|e| {
        TlErrBuilder::default()
            .message(format!("Assignment target is not an lvalue"))
            .py_err(e)
            .span(*span)
            .build_errs()
    })?;

    node.setattr(ast.py, "ctx", ast.name_ctx(PyAccessCtx::Load)?)?;

    Ok(())
}

#[allow(dead_code)]
fn generate_list_destructure_ast<'py, 'ast>(
    ast: &TlCtx<'py>,
    target: &SExpr,
    items: &'ast [ListItem],
) -> TlResult<(SBlock<'ast>, SExpr<'ast>)> {
    let cursor_var = ast.temp_var_name("des_curs", target.1.start);
    let list_var = ast.temp_var_name("des_list", target.1.start);
    let len_var = ast.temp_var_name("des_len", target.1.start);

    // list_var = list(cursor_var)
    // len_var = len(list_var)

    let a = AstBuilder::new(target.1);
    let mut stmts = vec![
        a.assign(
            a.ident(list_var.clone()),
            a.call(
                a.ident("tuple"),
                vec![a.call_arg(a.ident(cursor_var.clone()))],
            ),
        ),
        a.assign(
            a.ident(len_var.clone()),
            a.call(a.ident("len"), vec![a.call_arg(a.ident(list_var.clone()))]),
        ),
    ];

    // a = list_var[0]
    // b = list_var[1]
    // c = list_var[i:len_var-n_single_spreads]

    let mut seen_spread = false;
    let mut i = 0;

    for item in items.iter() {
        match item {
            ListItem::Item(expr) => {
                stmts.push(a.assign(
                    expr.clone(),
                    a.subscript(
                        a.ident(list_var.clone()),
                        vec![
                                        a.list_item(
                                            a.num(
                                                (if seen_spread {
                                                    -((items.len() - i - 1) as i32)
                                                } else {
                                                    i as i32
                                                })
                                                .to_string(),
                                            ),
                                        ),
                                    ],
                    ),
                ));
                i += 1;
            }
            ListItem::Spread(expr) => {
                if seen_spread {
                    return Err(TlErrBuilder::default()
                        .message("Destructuring assignment with multiple spreads is not allowed")
                        .span(target.1)
                        .build_errs());
                }

                seen_spread = true;

                stmts.push(a.assign(
                    expr.clone(),
                    a.subscript(
                        a.ident(list_var.clone()),
                        vec![a.list_item(a.slice(
                            Some(a.num(i.to_string())),
                            Some(a.binary(
                                BinaryOp::Sub,
                                a.ident(len_var.clone()),
                                a.num((items.len() - 2).to_string()),
                            )),
                            None,
                        ))],
                    ),
                ));
            }
        }
    }

    Ok((a.stmts_block(stmts), a.ident(cursor_var).clone()))
}

fn generate_mapping_destructure_ast<'py, 'ast>(
    ast: &TlCtx<'py>,
    target: &SExpr,
    items: &'ast [MappingItem],
) -> TlResult<(SBlock<'ast>, SExpr<'ast>)> {
    let cursor_var = ast.temp_var_name("des_curs", target.1.start);
    let dict_var = ast.temp_var_name("des_dict", target.1.start);

    // dict_var = dict(cursor_var)
    let a = AstBuilder::new(target.1);
    let mut stmts = vec![a.assign(
        a.ident(dict_var.clone()),
        a.call(
            a.ident("dict"),
            vec![a.call_arg(a.ident(cursor_var.clone()))],
        ),
    )];

    // a = dict_var.pop(a_key)
    // b = dict_var.pop(b_key)
    // c = dict_var

    let mut spread_var = None;
    for item in items.iter() {
        match item {
            MappingItem::Item(key, expr) => {
                stmts.push(a.assign(
                    expr.clone(),
                    a.call(
                        a.attribute(a.ident(dict_var.clone()), "pop"),
                        vec![a.call_arg(key.clone())],
                    ),
                ));
            }
            MappingItem::Spread(expr) => {
                if spread_var.is_some() {
                    return Err(TlErrBuilder::default()
                        .message("Destructuring assignment with multiple spreads is not allowed")
                        .span(target.1)
                        .build_errs());
                }

                spread_var = Some(expr.clone());
            }
        }
    }

    if let Some(spread_var) = spread_var {
        stmts.push(a.assign(spread_var, a.ident(dict_var.clone())));
    }

    Ok((a.stmts_block(stmts), a.ident(cursor_var).clone()))
}

struct DestructureBindings {
    assign_to: PyObject,
    post_stmts: PyStmts,
}

fn make_destructure_bindings<'py>(
    ast: &TlCtx<'py>,
    target: &SExpr,
    decl_only: bool,
) -> TlResult<DestructureBindings> {
    let mut post_stmts = vec![];

    let assign_to: PyObject = match &target.0 {
        Expr::Ident(..) | Expr::Attribute(..) | Expr::Subscript(..) => {
            if decl_only {
                match &target.0 {
                    Expr::Ident(_) => {}
                    _ => {
                        return Err(TlErrBuilder::default()
                            .message("Destructuring assignment target must be an identifier")
                            .span(target.1)
                            .build_errs());
                    }
                }
            }

            let target_node = transpile_expr(ast, target)?;
            post_stmts.extend(target_node.aux_stmts);

            let lhs = target_node.expr;
            make_lvalue(ast, &lhs, &target.1)?;

            lhs
        }
        Expr::List(items) => {
            /*
            Python destructure (destructures everything into a list)
            let mut py_targets = vec![];
            let des_target = ast.temp_var_name("des", target.1.start);

            let mut post_post_stmts = vec![];

            for i in items {
                match i {
                    ListItem::Item(expr) => {
                        let binding = make_destructure_bindings(ast, expr, decl_only)?;
                        post_post_stmts.extend(binding.post_stmts);
                        py_targets.push(binding.assign_to);
                    }
                    ListItem::Spread(expr) => {
                        let binding = make_destructure_bindings(ast, expr, decl_only)?;
                        post_post_stmts.extend(binding.post_stmts);
                        py_targets.push(ast.method1_unbound(
                            "Starred",
                            (binding.assign_to,),
                            Some(&target.1),
                        )?);
                    }
                }
            }

            post_stmts.push(ast.method1_unbound(
                "Assign",
                (
                    [ast.method1_unbound("Tuple", (py_targets,), Some(&target.1))?],
                    ast.name(&des_target, PyAccessCtx::Load, Some(&target.1))?,
                ),
                Some(&target.1),
            )?);

            post_stmts.extend(post_post_stmts);

            ast.name(&des_target, PyAccessCtx::Store, Some(&target.1))?
            */

            // Native coatl AST

            let (block, cursor) = generate_list_destructure_ast(ast, target, items)?;

            let stmts = transpile_block_with_final_stmt(ast, &block)?;
            let curs = transpile_expr(ast, &cursor)?;

            post_stmts.extend(stmts);
            post_stmts.extend(curs.aux_stmts);

            curs.expr
        }
        Expr::Mapping(items) => {
            let (block, cursor) = generate_mapping_destructure_ast(ast, target, items)?;

            let stmts = transpile_block_with_final_stmt(ast, &block)?;
            let curs = transpile_expr(ast, &cursor)?;

            post_stmts.extend(stmts);
            post_stmts.extend(curs.aux_stmts);

            curs.expr
        }
        _ => {
            return Err(TlErrBuilder::default()
                .message("Destructuring assignment target is not allowed")
                .span(target.1)
                .build_errs());
        }
    };

    Ok(DestructureBindings {
        post_stmts,
        assign_to,
    })
}

fn transpile_stmt<'py, 'src>(ast: &TlCtx<'py>, stmt: &SStmt) -> TlResult<PyStmts> {
    let (stmt, span) = stmt;

    match stmt {
        Stmt::Expr(expr) => match &expr.0 {
            Expr::If(cond, then_block, else_block) => {
                transpile_if_stmt(ast, cond, then_block, else_block, span)
            }
            Expr::Match(subject, cases) => transpile_match_stmt(ast, subject, cases, span),
            _ => {
                let expr = transpile_expr(ast, &expr)?;
                let mut stmts = expr.aux_stmts;
                stmts.push(ast.method1_unbound("Expr", (expr.expr,), Some(span))?);

                Ok(stmts)
            }
        },
        Stmt::Assert(expr, msg) => {
            let expr_node = transpile_expr(ast, &expr)?;
            let msg = msg.as_ref().map(|x| transpile_expr(ast, &x)).transpose()?;

            let mut stmts = expr_node.aux_stmts;
            let mut msg_node = None;
            if let Some(msg) = msg {
                stmts.extend(msg.aux_stmts);
                msg_node = Some(msg.expr);
            }

            stmts.push(ast.method1_unbound("Assert", (expr_node.expr, msg_node), Some(span))?);

            Ok(stmts)
        }
        Stmt::Return(expr) => {
            let value = transpile_expr(ast, &expr)?;
            let mut stmts = value.aux_stmts;

            stmts.push(ast.method1_unbound("Return", (value.expr,), Some(span))?);

            Ok(stmts)
        }
        Stmt::Assign(target, value) => {
            if let (Expr::Ident((ident, ident_span)), Expr::Fn(arglist, body)) =
                (&target.0, &value.0)
            {
                return make_fndef_stmts(ast, ident, &arglist, FnDefBody::Block(&body), span);
            }

            if let (Expr::Ident((ident, ident_span)), Expr::Class(bases, body)) =
                (&target.0, &value.0)
            {
                return make_classdef_stmts(ast, ident, &bases, &body, span);
            }

            let value_node = transpile_expr(ast, &value)?;
            let mut stmts = value_node.aux_stmts;
            let destructure = make_destructure_bindings(ast, target, false)?;
            stmts.push(ast.method1_unbound(
                "Assign",
                ([destructure.assign_to], value_node.expr),
                Some(&target.1),
            )?);
            stmts.extend(destructure.post_stmts);

            Ok(stmts)
        }
        Stmt::Global(names) => {
            let names: Vec<_> = names
                .iter()
                .map(|name| ast.name(&name.0, PyAccessCtx::Store, Some(span)))
                .collect::<TlResult<_>>()?;

            Ok(vec![ast.method1_unbound("Global", (names,), Some(span))?])
        }
        Stmt::Nonlocal(names) => {
            let names: Vec<_> = names
                .iter()
                .map(|name| ast.name(&name.0, PyAccessCtx::Store, Some(span)))
                .collect::<TlResult<_>>()?;

            Ok(vec![ast.method1_unbound(
                "Nonlocal",
                (names,),
                Some(span),
            )?])
        }
        Stmt::Raise(expr) => {
            let expr_node = transpile_expr(ast, expr)?;
            let mut stmts = expr_node.aux_stmts;

            stmts.push(ast.method1_unbound("Raise", (expr_node.expr,), Some(span))?);

            Ok(stmts)
        }
        Stmt::For(target, iter, body) => {
            if let Expr::Ident((ident, _)) = &target.0 {
                let iter_node = transpile_expr(ast, iter)?;
                let aux_stmts = iter_node.aux_stmts;

                let body_block = transpile_block_with_final_stmt(ast, body)?;

                Ok(vec![ast.method1_unbound(
                    "For",
                    (
                        ast.name(ident, PyAccessCtx::Store, Some(span))?,
                        iter_node.expr,
                        body_block,
                        PyList::empty(ast.py),
                    ),
                    Some(span),
                )?])
            } else {
                Err(TlErrBuilder::default()
                    .message("for loop target must be an identifier".to_owned())
                    .span(*span)
                    .build_errs())
            }
        }
        Stmt::While(cond, body) => {
            let cond_node = transpile_expr(ast, cond)?;
            let body_block = transpile_block_with_final_stmt(ast, body)?;

            let mut stmts = vec![];

            let cond = if cond_node.aux_stmts.is_empty() {
                cond_node.expr
            } else {
                let cond_name = ast.temp_var_name("whilecond", span.start);
                let aux_fn = make_fndef_stmts(
                    ast,
                    &cond_name,
                    &vec![],
                    FnDefBody::Stmts(cond_node.aux_stmts),
                    span,
                )?;
                stmts.extend(aux_fn);

                ast.method1_unbound(
                    "Call",
                    (
                        ast.name(&cond_name, PyAccessCtx::Load, Some(span))?,
                        PyList::empty(ast.py),
                        PyList::empty(ast.py),
                    ),
                    Some(span),
                )?
            };

            stmts.push(ast.method1_unbound(
                "While",
                (cond, body_block, PyList::empty(ast.py)),
                Some(span),
            )?);

            Ok(stmts)
        }
        Stmt::Try(body, excepts, finally) => {
            let body_block = transpile_block_with_final_stmt(ast, body)?;
            let finally_block = finally
                .as_ref()
                .map(|f| transpile_block_with_final_stmt(ast, f))
                .transpose()?;

            let mut stmts = vec![];
            let mut excepts_ast = vec![];

            for except in excepts {
                let type_node = if let Some(typ) = &except.typ {
                    let expr = transpile_expr(ast, typ)?;
                    stmts.extend(expr.aux_stmts);
                    expr.expr
                } else {
                    ast.name("BaseException", PyAccessCtx::Load, Some(span))?
                };

                let ident_node = if let Some(ident) = &except.name {
                    PyString::new(ast.py, &ident.0).into_any()
                } else {
                    ast.py.None().bind(ast.py).clone()
                };

                let body_block = transpile_block_with_final_stmt(ast, body)?;

                let except_ast = ast.method1_unbound(
                    "ExceptHandler",
                    (type_node, ident_node, body_block),
                    Some(span),
                )?;

                excepts_ast.push(except_ast);
            }

            stmts.push(ast.method1_unbound(
                "Try",
                (
                    body_block,
                    excepts_ast,
                    PyList::empty(ast.py),
                    finally_block,
                ),
                Some(span),
            )?);

            Ok(stmts)
        }
        Stmt::Break => Ok(vec![ast.method1_unbound("Break", (), Some(span))?]),
        Stmt::Continue => Ok(vec![ast.method1_unbound("Continue", (), Some(span))?]),
        Stmt::Import(import_stmt) => {
            let mut aliases = vec![];
            let mut base_module = None;

            if !import_stmt.trunk.is_empty() {
                base_module = Some(
                    import_stmt
                        .trunk
                        .iter()
                        .map(|ident| ident.0.as_ref())
                        .collect::<Vec<_>>()
                        .join("."),
                );
            }

            if import_stmt.star {
                aliases.push(ast.method1_unbound("alias", ("*", ast.py.None()), Some(span))?);
            } else {
                for (ident, alias) in &import_stmt.leaves {
                    aliases.push(ast.method1_unbound(
                        "alias",
                        (&ident.0, alias.as_ref().map(|a| a.0.as_ref())),
                        Some(span),
                    )?);
                }
            }

            if let Some(base_module) = base_module {
                Ok(vec![ast.method1_unbound(
                    "ImportFrom",
                    (base_module, aliases, 0),
                    Some(span),
                )?])
            } else {
                Ok(vec![ast.method1_unbound(
                    "Import",
                    (aliases,),
                    Some(span),
                )?])
            }
        }
        Stmt::Err => Err(TlErrBuilder::default()
            .message("unexpected statement error (should have been caught in lexer)".to_owned())
            .span(*span)
            .build_errs()),
    }
}

type PyStmts = Vec<PyObject>;
type PyExpr = PyObject;

struct PyExprWithAux {
    expr: PyObject,
    aux_stmts: PyStmts,
}

fn transpile_if_stmt<'py>(
    ast: &TlCtx<'py>,
    cond: &SExpr,
    then_block: &SBlock,
    else_block: &Option<Box<SBlock>>,
    span: &Span,
) -> TlResult<PyStmts> {
    let mut stmts = vec![];

    let cond = transpile_expr(ast, cond)?;
    stmts.extend(cond.aux_stmts.into_iter());

    let then_block = transpile_block_with_final_stmt(ast, then_block)?;
    let then_block_ast = then_block;

    let mut else_block_ast = None;
    if let Some(else_block) = else_block {
        let else_block = transpile_block_with_final_stmt(ast, else_block)?;

        else_block_ast = Some(else_block);
    }

    Ok(vec![ast.method1_unbound(
        "If",
        (cond.expr, then_block_ast, else_block_ast),
        Some(span),
    )?])
}

fn transpile_if_expr<'py>(
    ast: &TlCtx<'py>,
    cond: &SExpr,
    then_block: &SBlock,
    else_block: &Option<Box<SBlock>>,
    span: &Span,
) -> TlResult<PyExprWithAux> {
    let cond = transpile_expr(ast, cond)?;
    let mut aux_stmts = cond.aux_stmts;

    let ret_varname = ast.temp_var_name("ifexp", span.start);
    let ret_var = ast.name(&ret_varname, PyAccessCtx::Store, Some(span))?;

    let PyBlock { stmts, final_expr } = transpile_block_with_final_expr(ast, then_block)?;
    let mut then_block_ast = stmts;

    if let Some(final_expr) = final_expr {
        then_block_ast.push(ast.method1_unbound("Assign", ([&ret_var], final_expr), Some(span))?);
    } else {
        return Err(TlErrBuilder::default()
            .message("then block must have a final expression")
            .span(then_block.1)
            .build_errs());
    }

    let else_block = else_block.as_ref().ok_or_else(|| {
        TlErrBuilder::default()
            .message("else block is required in an if-expr")
            .span(*span)
            .build_errs()
    })?;

    let PyBlock { stmts, final_expr } = transpile_block_with_final_expr(ast, else_block)?;
    let mut else_block_ast = stmts;
    if let Some(final_expr) = final_expr {
        else_block_ast.push(ast.method1_unbound("Assign", ([&ret_var], final_expr), Some(span))?);
    } else {
        return Err(TlErrBuilder::default()
            .message("else block must have a final expression")
            .span(else_block.1)
            .build_errs());
    }

    aux_stmts.push(ast.method1_unbound(
        "If",
        (cond.expr, then_block_ast, else_block_ast),
        Some(span),
    )?);

    Ok(PyExprWithAux {
        expr: ast.name(&ret_varname, PyAccessCtx::Load, Some(span))?,
        aux_stmts,
    })
}

fn transpile_match_stmt<'py>(
    ast: &TlCtx<'py>,
    subject: &SExpr,
    cases: &[(SExpr, Box<SBlock>)],
    span: &Span,
) -> TlResult<PyStmts> {
    let subject = transpile_expr(ast, subject)?;
    let mut aux_stmts = subject.aux_stmts;

    let mut py_cases = vec![];
    for (pattern, block) in cases {
        let pattern = transpile_expr(ast, pattern)?;
        aux_stmts.extend(pattern.aux_stmts);

        let py_block = transpile_block_with_final_stmt(ast, block)?;

        py_cases.push(ast.method1_unbound(
            "match_case",
            (pattern.expr, ast.py.None(), py_block),
            Some(&block.1),
        )?);
    }

    Ok(vec![ast.method1_unbound(
        "Match",
        (subject.expr, py_cases),
        Some(span),
    )?])
}

fn transpile_match_expr<'py>(
    ast: &TlCtx<'py>,
    subject: &SExpr,
    cases: &[(SExpr, Box<SBlock>)],
    span: &Span,
) -> TlResult<PyExprWithAux> {
    let subject = transpile_expr(ast, subject)?;
    let mut aux_stmts = subject.aux_stmts;

    let ret_varname = ast.temp_var_name("matchexp", span.start);
    let ret_var = ast.name(&ret_varname, PyAccessCtx::Store, Some(span))?;

    let mut py_cases = vec![];
    for (pattern, block) in cases {
        let pattern = transpile_expr(ast, pattern)?;
        aux_stmts.extend(pattern.aux_stmts);

        let py_block = transpile_block_with_final_expr(ast, &*block)?;
        let mut block_stmts = py_block.stmts;

        block_stmts.push(ast.method1_unbound(
            "Assign",
            ([&ret_var], py_block.final_expr),
            Some(&block.1),
        )?);

        py_cases.push(ast.method1_unbound(
            "match_case",
            (pattern.expr, ast.py.None(), block_stmts),
            Some(&block.1),
        )?);
    }

    aux_stmts.push(ast.method1_unbound("Match", (subject.expr, py_cases), Some(span))?);

    Ok(PyExprWithAux {
        expr: ast.name(&ret_varname, PyAccessCtx::Load, Some(span))?,
        aux_stmts,
    })
}

fn make_classdef_stmts<'py>(
    ast: &TlCtx<'py>,
    name: &str,
    bases: &Vec<SCallItem>,
    body: &Box<SBlock>,
    span: &Span,
) -> TlResult<PyStmts> {
    let mut stmts = vec![];
    let mut bases_nodes = vec![];
    let mut keywords_nodes = vec![];

    let block = transpile_block_with_final_stmt(ast, body)?;

    for base in bases {
        match &base.0 {
            CallItem::Arg(expr) => {
                let base_node = transpile_expr(ast, &expr)?;
                stmts.extend(base_node.aux_stmts);
                bases_nodes.push(base_node.expr);
            }
            CallItem::Kwarg(name, expr) => {
                let expr_node = transpile_expr(ast, &expr)?;
                stmts.extend(expr_node.aux_stmts);
                keywords_nodes.push((name.0.as_ref(), expr_node.expr));
            }
            _ => {
                Err(TlErrBuilder::default()
                    .message("spread args are not allowed in class bases")
                    .span(*span)
                    .build_errs())?;
            }
        }
    }

    stmts.push(ast.method1_unbound(
        "ClassDef",
        (
            name,
            bases_nodes,
            keywords_nodes,
            block,
            PyList::empty(ast.py), // decorator_list
            PyList::empty(ast.py), // type_params
        ),
        Some(span),
    )?);

    Ok(stmts)
}

enum FnDefBody<'a> {
    Stmts(PyStmts),
    Block(&'a SBlock<'a>),
}

fn make_fndef_stmts<'py>(
    ast: &TlCtx<'py>,
    name: &str,
    arglist: &[ArgItem],
    body: FnDefBody,
    span: &Span,
) -> TlResult<PyStmts> {
    let empty = || Vec::<PyObject>::new();
    let posonly = empty();
    let mut args = empty();
    let mut vararg = empty();
    let kwonly = empty();
    let mut kwarg = empty();
    let kw_defaults = empty();
    let mut defaults = empty();

    let arg_node = |name: &str| ast.method1_unbound("arg", (name, ast.py.None()), Some(span));

    let mut aux_stmts = empty();

    for arg in arglist {
        match arg {
            ArgItem::Arg(name) => {
                args.push(arg_node(name.0.as_ref())?);
                defaults.push(ast.py.None());
            }
            ArgItem::DefaultArg(name, default) => {
                let mut expr = transpile_expr(ast, default)?;
                aux_stmts.append(&mut expr.aux_stmts);

                args.push(arg_node(name.0.as_ref())?);
                defaults.push(expr.expr);
            }
            ArgItem::ArgSpread(name) => {
                vararg.push(arg_node(name.0.as_ref())?);
                if vararg.len() > 1 {
                    return Err(TlErrBuilder::default()
                        .message("only one vararg is allowed")
                        .span(*span)
                        .build_errs());
                }
            }
            ArgItem::KwargSpread(name) => {
                kwarg.push(arg_node(name.0.as_ref())?);
                if kwarg.len() > 1 {
                    return Err(TlErrBuilder::default()
                        .message("only one kwarg is allowed")
                        .span(*span)
                        .build_errs());
                }
            }
        }
    }

    let args = ast.method1_unbound(
        "arguments",
        (posonly, args, vararg, kwonly, kw_defaults, kwarg, defaults),
        Some(span),
    )?;

    let body_stmts = match body {
        FnDefBody::Stmts(stmts) => stmts,
        FnDefBody::Block(block) => {
            let block = transpile_block_with_final_expr(ast, block)?;
            let mut stmts = block.stmts;

            if let Some(final_expr) = block.final_expr {
                stmts.push(ast.method1_unbound("Return", (final_expr,), Some(span))?);
            };

            stmts
        }
    };

    let decorators = empty();

    let mut stmts = aux_stmts;
    stmts.push(ast.method1_unbound(
        "FunctionDef",
        (&name, args, body_stmts, decorators),
        Some(span),
    )?);

    Ok(stmts)
}

fn transpile_expr<'py>(ast: &TlCtx<'py>, expr: &SExpr) -> TlResult<PyExprWithAux> {
    let no_aux = |expr| PyExprWithAux {
        expr,
        aux_stmts: vec![],
    };

    let (expr, span) = expr;

    match expr {
        Expr::Fn(arglist, body) => {
            let name = ast.temp_var_name("fnexp", span.start);
            let aux_stmts = make_fndef_stmts(ast, &name, arglist, FnDefBody::Block(body), span)?;

            Ok(PyExprWithAux {
                expr: ast.name(&name, PyAccessCtx::Load, Some(span))?,
                aux_stmts,
            })
        }
        Expr::Class(bases, body) => {
            let name = ast.temp_var_name("classexp", span.start);
            let aux_stmts = make_classdef_stmts(ast, &name, bases, body, span)?;

            Ok(PyExprWithAux {
                expr: ast.name(&name, PyAccessCtx::Load, Some(span))?,
                aux_stmts,
            })
        }
        Expr::Literal((lit, span)) => {
            let value = match lit {
                Literal::Num(num) => match num.parse::<i128>() {
                    Ok(i) => Ok(ast.constant(i)?),
                    Err(_) => match num.parse::<f64>() {
                        Ok(f) => Ok(ast.constant(f)?),
                        Err(_) => Err(TlErrBuilder::default()
                            .message("failed to parse number as int or float")
                            .span(*span)
                            .build_errs()),
                    },
                },
                Literal::Str(s) => ast.constant(s.to_string()),
            }?;

            Ok(no_aux(value))
        }
        Expr::Ident((ident, span)) => {
            let name = ast.name(ident, PyAccessCtx::Load, Some(span))?;
            Ok(no_aux(name))
        }
        Expr::Attribute(obj, attr) => {
            let obj = transpile_expr(ast, obj)?;

            Ok(PyExprWithAux {
                expr: ast.method1_unbound(
                    "Attribute",
                    (obj.expr, attr.0.as_ref(), ast.name_ctx(PyAccessCtx::Load)?),
                    Some(span),
                )?,
                aux_stmts: obj.aux_stmts,
            })
        }
        Expr::Call(obj, args) => {
            let obj = transpile_expr(ast, obj)?;
            let mut aux_stmts = obj.aux_stmts;

            let mut args_nodes = vec![];
            let mut keywords_nodes = vec![];

            for arg in args {
                match &arg.0 {
                    CallItem::Arg(expr) => {
                        let e = transpile_expr(ast, &expr)?;
                        aux_stmts.extend(e.aux_stmts);
                        args_nodes.push(e.expr);
                    }
                    CallItem::Kwarg((name, name_span), expr) => {
                        let e = transpile_expr(ast, &expr)?;
                        aux_stmts.extend(e.aux_stmts);
                        keywords_nodes.push(ast.method1_unbound(
                            "keyword",
                            (name, e.expr),
                            Some(span),
                        )?);
                    }
                    CallItem::ArgSpread(expr) => {
                        let e = transpile_expr(ast, &expr)?;
                        aux_stmts.extend(e.aux_stmts);
                        args_nodes.push(ast.method1_unbound("Starred", (e.expr,), Some(span))?);
                    }
                    CallItem::KwargSpread(expr) => {
                        let e = transpile_expr(ast, &expr)?;
                        aux_stmts.extend(e.aux_stmts);
                        keywords_nodes.push(ast.method1_unbound(
                            "keyword",
                            (ast.py.None(), e.expr),
                            Some(span),
                        )?);
                    }
                };
            }

            Ok(PyExprWithAux {
                expr: ast.method1_unbound(
                    "Call",
                    (obj.expr, args_nodes, keywords_nodes),
                    Some(span),
                )?,
                aux_stmts,
            })
        }
        Expr::Pipe(lhs, rhs) => {
            let lhs = transpile_expr(ast, lhs)?;
            let mut rhs = transpile_expr(ast, rhs)?;

            let mut aux_stmts = lhs.aux_stmts;
            aux_stmts.append(&mut rhs.aux_stmts);

            Ok(PyExprWithAux {
                expr: ast.method1_unbound(
                    "Call",
                    (rhs.expr, [lhs.expr], PyList::empty(ast.py)),
                    Some(span),
                )?,
                aux_stmts,
            })
        }
        Expr::Subscript(obj, indices) => {
            let obj = transpile_expr(ast, obj)?;
            let mut aux_stmts = obj.aux_stmts;

            let indices: Vec<_> = indices
                .into_iter()
                .map(|index| match index {
                    ListItem::Spread(expr) => {
                        let e = transpile_expr(ast, expr)?;
                        aux_stmts.extend(e.aux_stmts);
                        Ok(ast.method1_unbound("Starred", (e.expr,), Some(span))?)
                    }
                    ListItem::Item(expr) => match &expr.0 {
                        Expr::Slice(start, stop, step) => {
                            let mut get = |e: &Option<Box<SExpr>>| -> TlResult<PyObject> {
                                if let Some(e) = e {
                                    let e = transpile_expr(ast, e)?;
                                    aux_stmts.extend(e.aux_stmts);
                                    Ok(e.expr)
                                } else {
                                    Ok(ast.py.None())
                                }
                            };

                            Ok(ast.method1_unbound(
                                "Slice",
                                (get(start)?, get(stop)?, get(step)?),
                                Some(&expr.1),
                            )?)
                        }
                        _ => {
                            let e = transpile_expr(ast, expr)?;
                            aux_stmts.extend(e.aux_stmts);
                            Ok(e.expr)
                        }
                    },
                })
                .collect::<TlResult<Vec<_>>>()?;

            if indices.len() == 1 {
                return Ok(PyExprWithAux {
                    expr: ast.method1_unbound(
                        "Subscript",
                        (obj.expr, &indices[0], ast.name_ctx(PyAccessCtx::Load)?),
                        Some(span),
                    )?,
                    aux_stmts,
                });
            } else {
                Ok(PyExprWithAux {
                    expr: ast.method1_unbound(
                        "Subscript",
                        (
                            obj.expr,
                            ast.method1_unbound(
                                "Tuple",
                                (indices, ast.name_ctx(PyAccessCtx::Load)?),
                                Some(span),
                            )?,
                            ast.name_ctx(PyAccessCtx::Load)?,
                        ),
                        Some(span),
                    )?,
                    aux_stmts,
                })
            }
        }
        Expr::If(cond, then_block, else_block) => {
            transpile_if_expr(ast, cond, then_block, else_block, span)
        }
        Expr::Block(block) => {
            let PyBlock { stmts, final_expr } = transpile_block_with_final_expr(ast, block)?;

            Ok(PyExprWithAux {
                expr: final_expr.ok_or_else(|| {
                    TlErrBuilder::default()
                        .message("block-expression must have a final expression")
                        .span(block.1)
                        .build_errs()
                })?,
                aux_stmts: stmts,
            })
        }
        Expr::Match(subject, cases) => transpile_match_expr(ast, subject, cases, span),
        Expr::Yield(expr) => {
            let value = transpile_expr(ast, expr)?;

            Ok(PyExprWithAux {
                expr: ast.method1_unbound("Yield", (value.expr,), Some(span))?,
                aux_stmts: value.aux_stmts,
            })
        }
        Expr::YieldFrom(expr) => {
            let value = transpile_expr(ast, expr)?;

            Ok(PyExprWithAux {
                expr: ast.method1_unbound("YieldFrom", (value.expr,), Some(span))?,
                aux_stmts: value.aux_stmts,
            })
        }
        Expr::Binary(op, lhs, rhs) => {
            let lhs = transpile_expr(ast, lhs)?;
            let mut rhs = transpile_expr(ast, rhs)?;

            let mut aux_stmts = lhs.aux_stmts;
            aux_stmts.append(&mut rhs.aux_stmts);

            let py_op = match op {
                BinaryOp::Add => Some("Add"),
                BinaryOp::Sub => Some("Sub"),
                BinaryOp::Mul => Some("Mult"),
                BinaryOp::Div => Some("Div"),
                BinaryOp::Mod => Some("Mod"),
                BinaryOp::Exp => Some("Pow"),
                BinaryOp::MatMul => Some("MatMult"),
                _ => None,
            };

            let py_cmp_op = match op {
                BinaryOp::Lt => Some("Lt"),
                BinaryOp::Gt => Some("Gt"),
                BinaryOp::Leq => Some("LtE"),
                BinaryOp::Geq => Some("GtE"),
                BinaryOp::Eq => Some("Eq"),
                BinaryOp::Neq => Some("NotEq"),
                BinaryOp::Is => Some("Is"),
                BinaryOp::Nis => Some("IsNot"),
                _ => None,
            };

            if let Some(py_op) = py_op {
                return Ok(PyExprWithAux {
                    expr: ast.method1_unbound(
                        "BinOp",
                        (lhs.expr, ast.method1_unbound(py_op, (), None)?, rhs.expr),
                        Some(span),
                    )?,
                    aux_stmts,
                });
            } else if let Some(py_op) = py_cmp_op {
                return Ok(PyExprWithAux {
                    expr: ast.method1_unbound(
                        "Compare",
                        (
                            lhs.expr,
                            [ast.method1_unbound(py_op, (), None)?],
                            [rhs.expr],
                        ),
                        Some(span),
                    )?,
                    aux_stmts,
                });
            } else {
                return Err(TlErrBuilder::default()
                    .message(format!("Unsupported binary operator: {:?}", op))
                    .span(*span)
                    .build_errs());
            }
        }
        Expr::Unary(op, expr) => {
            let expr = transpile_expr(ast, expr)?;
            let aux_stmts = expr.aux_stmts;

            let py_op = match op {
                UnaryOp::Neg => "USub",
                UnaryOp::Pos => "UAdd",
                UnaryOp::Inv => "Invert",
                UnaryOp::Await => todo!(),
            };

            return Ok(PyExprWithAux {
                expr: ast.method1_unbound(
                    "UnaryOp",
                    (ast.method1_unbound(py_op, (), None)?, expr.expr),
                    Some(span),
                )?,
                aux_stmts,
            });
        }
        Expr::List(exprs) => {
            let mut aux_stmts = vec![];
            let mut items = vec![];

            for expr in exprs {
                match expr {
                    ListItem::Spread(expr) => {
                        let e = transpile_expr(ast, expr)?;
                        aux_stmts.extend(e.aux_stmts);
                        items.push(ast.method1_unbound("Starred", (e.expr,), Some(span))?);
                    }
                    ListItem::Item(expr) => {
                        let e = transpile_expr(ast, expr)?;
                        aux_stmts.extend(e.aux_stmts);
                        items.push(e.expr);
                    }
                }
            }

            return Ok(PyExprWithAux {
                expr: ast.method1_unbound("Tuple", (items,), Some(span))?,
                aux_stmts,
            });
        }
        Expr::Mapping(items) => {
            let mut aux_stmts = vec![];
            let mut keys = vec![];
            let mut values = vec![];

            for item in items {
                match item {
                    MappingItem::Item(key, value) => {
                        let key = transpile_expr(ast, key)?;
                        let value = transpile_expr(ast, value)?;

                        aux_stmts.extend(key.aux_stmts);
                        aux_stmts.extend(value.aux_stmts);

                        keys.push(key.expr);
                        values.push(value.expr);
                    }
                    MappingItem::Spread(expr) => {
                        let e = transpile_expr(ast, expr)?;
                        aux_stmts.extend(e.aux_stmts);

                        keys.push(ast.py.None());
                        values.push(e.expr);
                    }
                }
            }

            return Ok(PyExprWithAux {
                expr: ast.method1_unbound("Dict", (keys, values), Some(span))?,
                aux_stmts,
            });
        }
        Expr::Slice(start, end, step) => {
            let start = start
                .as_ref()
                .map(|e| transpile_expr(ast, e.as_ref()))
                .transpose()?;
            let end = end
                .as_ref()
                .map(|e| transpile_expr(ast, e.as_ref()))
                .transpose()?;
            let step = step
                .as_ref()
                .map(|e| transpile_expr(ast, e.as_ref()))
                .transpose()?;

            let mut aux_stmts = vec![];

            let mut get = |x: Option<PyExprWithAux>| {
                if let Some(x) = x {
                    aux_stmts.extend(x.aux_stmts);
                    Ok(x.expr)
                } else {
                    ast.constant(ast.py.None())
                }
            };

            return Ok(PyExprWithAux {
                expr: ast.method1_unbound(
                    "Call",
                    (
                        ast.name("slice", PyAccessCtx::Load, Some(span))?,
                        [get(start)?, get(end)?, get(step)?],
                        Vec::<PyObject>::new(),
                    ),
                    Some(span),
                )?,
                aux_stmts,
            });
        }
        Expr::Fstr(begin, parts) => {
            let mut aux_stmts = Vec::new();
            let mut nodes = Vec::new();
            nodes.push(ast.constant(&begin.0)?);

            for (fmt_expr, str_part) in parts {
                // TODO format specifiers?
                let block_node = transpile_block_with_final_expr(ast, &fmt_expr.0.block)?;
                aux_stmts.extend(block_node.stmts);

                let expr_node = block_node.final_expr.ok_or_else(|| {
                    TlErrBuilder::default()
                        .message("f-string expression must have a final expression")
                        .span(fmt_expr.1)
                        .build_errs()
                })?;

                nodes.push(ast.method1_unbound(
                    "FormattedValue",
                    (expr_node, -1),
                    Some(&fmt_expr.1),
                )?);

                nodes.push(ast.constant(&str_part.0)?);
            }

            let expr = ast.method1_unbound("JoinedStr", (nodes,), Some(span))?;
            return Ok(PyExprWithAux { expr, aux_stmts });
        }
    }
}

pub fn transpile(source: &str, block: &SBlock) -> TlResult<String> {
    Python::with_gil(move |py| {
        let ast = TlCtx::new(py, source)?;
        let span = &block.1;

        let stmts = transpile_block_with_final_stmt(&ast, block)?;

        let root_node = ast.method1_unbound("Module", (stmts, PyList::empty(py)), Some(span))?;

        ast.ast_module
            .call_method1("fix_missing_locations", (&root_node,))?;

        let dump_args = PyDict::new(ast.py);
        dump_args.set_item("indent", 4)?;

        let dump = ast
            .ast_module
            .call_method("dump", (&root_node,), Some(&dump_args))?
            .extract::<String>()?;

        // println!("{}", dump);

        let source = ast
            .ast_module
            .call_method1("unparse", (root_node,))?
            .extract()?;

        return Ok(source);
    })
}
