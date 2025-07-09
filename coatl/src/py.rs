use parser::*;
use pyo3::{
    call::PyCallArgs,
    prelude::*,
    types::{PyDict, PyList},
};

pub struct TranspileErr {
    pub message: String,
    pub py_err: Option<PyErr>,
    pub span: Option<Span>,
}

// TODO: translate span to line and col number in errors and temporary var names

pub enum TlErr {
    PyErr(PyErr),
    TranspileErr(TranspileErr),
}

impl From<PyErr> for TlErr {
    fn from(err: PyErr) -> Self {
        TlErr::PyErr(err)
    }
}

pub type TlResult<T> = Result<T, TlErr>;

#[allow(dead_code)]
enum PyAccessCtx {
    Load,
    Store,
    Del,
}

struct TlCtx<'py> {
    py: Python<'py>,
    ast_module: Bound<'py, PyModule>,
    constant_node: Bound<'py, PyAny>,
}

impl<'py> TlCtx<'py> {
    fn new(py: Python<'py>) -> TlResult<Self> {
        let ast_module = py.import("ast")?;
        let constant_node = ast_module.getattr("Constant")?;

        Ok(TlCtx {
            py,
            ast_module,
            constant_node,
        })
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
                TlErr::TranspileErr(TranspileErr {
                    message: format!("Failed to call method {}: {}", method, e),
                    py_err: Some(e),
                    span: err_span.cloned(),
                })
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
            let mut result = Vec::new();

            for stmt in stmts {
                let stmts_vec = transpile_stmt(ast, stmt)?;
                result.extend(stmts_vec);
            }

            Ok(result)
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

            let mut result = Vec::new();
            let mut final_expr = None;

            let mut iter = stmts.iter();

            for stmt in iter.by_ref().take(stmts.len() - 1) {
                let stmts_vec = transpile_stmt(ast, stmt)?;
                result.extend(stmts_vec);
            }

            let final_stmt = iter.next().unwrap();

            match &final_stmt.0 {
                Stmt::Expr(expr) => {
                    let expr_with_aux = transpile_expr(ast, &expr)?;
                    result.extend(expr_with_aux.aux_stmts);
                    final_expr = Some(expr_with_aux.expr);
                }
                _ => result.extend(transpile_stmt(ast, final_stmt)?),
            }

            Ok(PyBlock {
                stmts: result,
                final_expr: final_expr,
            })
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

fn transpile_stmt<'py>(ast: &TlCtx<'py>, stmt: &SStmt) -> TlResult<PyStmts> {
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
                return transpile_fn_def(ast, &arglist, &body, ident, span);
            }

            // TODO allow destructuring in assign and for loop and fn def

            let mut target_node = transpile_expr(ast, target)?;
            let value_node = transpile_expr(ast, value)?;

            let mut stmts = value_node.aux_stmts;
            stmts.append(&mut target_node.aux_stmts);

            let lhs = target_node.expr;

            lhs.getattr(ast.py, "ctx").map_err(|e| {
                TlErr::TranspileErr(TranspileErr {
                    message: format!("Assignment target is not an lvalue: {}", e),
                    py_err: Some(e),
                    span: Some(target.1),
                })
            })?;

            lhs.setattr(ast.py, "ctx", ast.name_ctx(PyAccessCtx::Load)?)?;

            stmts.push(ast.method1_unbound("Assign", ([lhs], value_node.expr), Some(&target.1))?);

            Ok(stmts)
        }
        Stmt::Global(names) => {
            let names: Vec<_> = names
                .iter()
                .map(|name| ast.name(name.0, PyAccessCtx::Store, Some(span)))
                .collect::<TlResult<_>>()?;

            Ok(vec![ast.method1_unbound("Global", (names,), Some(span))?])
        }
        Stmt::Nonlocal(names) => {
            let names: Vec<_> = names
                .iter()
                .map(|name| ast.name(name.0, PyAccessCtx::Store, Some(span)))
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
                Err(TlErr::TranspileErr(TranspileErr {
                    message: "for loop target must be an identifier".to_owned(),
                    py_err: None,
                    span: Some(*span),
                }))
            }
        }
        Stmt::While(cond, body) => {
            unimplemented!();
        }
        Stmt::Break => Ok(vec![ast.method1_unbound("Break", (), Some(span))?]),
        Stmt::Continue => Ok(vec![ast.method1_unbound("Continue", (), Some(span))?]),
        Stmt::Import(module) => {
            unimplemented!();
        }
        Stmt::Err => Err(TlErr::TranspileErr(TranspileErr {
            message: "unexpected error in statement".to_owned(),
            py_err: None,
            span: Some(*span),
        })),
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

    let ret_varname = &format!("__tl{}", span.start);
    let ret_var = ast.name(ret_varname, PyAccessCtx::Store, Some(span))?;

    let PyBlock { stmts, final_expr } = transpile_block_with_final_expr(ast, then_block)?;
    let mut then_block_ast = stmts;

    if let Some(final_expr) = final_expr {
        then_block_ast.push(ast.method1_unbound("Assign", ([&ret_var], final_expr), Some(span))?);
    } else {
        return Err(TlErr::TranspileErr(TranspileErr {
            message: "then block must have a final expression".to_owned(),
            py_err: None,
            span: Some(*span),
        }));
    }

    let else_block = else_block.as_ref().ok_or_else(|| {
        TlErr::TranspileErr(TranspileErr {
            message: "else block is required in an if-expr".to_owned(),
            py_err: None,
            span: Some(*span),
        })
    })?;

    let PyBlock { stmts, final_expr } = transpile_block_with_final_expr(ast, else_block)?;
    let mut else_block_ast = stmts;
    if let Some(final_expr) = final_expr {
        else_block_ast.push(ast.method1_unbound("Assign", ([&ret_var], final_expr), Some(span))?);
    } else {
        return Err(TlErr::TranspileErr(TranspileErr {
            message: "else block must have a final expression".to_owned(),
            py_err: None,
            span: Some(*span),
        }));
    }

    aux_stmts.push(ast.method1_unbound(
        "If",
        (cond.expr, then_block_ast, else_block_ast),
        Some(span),
    )?);

    Ok(PyExprWithAux {
        expr: ast.name(ret_varname, PyAccessCtx::Load, Some(span))?,
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

    let ret_varname = &format!("__tl{}", span.start);
    let ret_var = ast.name(ret_varname, PyAccessCtx::Store, Some(span))?;

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
        expr: ast.name(ret_varname, PyAccessCtx::Load, Some(span))?,
        aux_stmts,
    })
}

fn make_fn_node<'py>(
    ast: &TlCtx<'py>,
    arglist: &[ArgItem],
    body: &SBlock,
    span: &Span,
) -> TlResult<(Vec<PyObject>, PyObject, Vec<PyObject>, Vec<PyObject>)> {
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
                args.push(arg_node(name.0)?);
                defaults.push(ast.py.None());
            }
            ArgItem::DefaultArg(name, default) => {
                let mut expr = transpile_expr(ast, default)?;
                aux_stmts.append(&mut expr.aux_stmts);

                args.push(arg_node(name.0)?);
                defaults.push(expr.expr);
            }
            ArgItem::ArgSpread(name) => {
                vararg.push(arg_node(name.0)?);
                if vararg.len() > 1 {
                    return Err(TlErr::TranspileErr(TranspileErr {
                        message: "only one vararg is allowed".to_owned(),
                        py_err: None,
                        span: None,
                    }));
                }
            }
            ArgItem::KwargSpread(name) => {
                kwarg.push(arg_node(name.0)?);
                if kwarg.len() > 1 {
                    return Err(TlErr::TranspileErr(TranspileErr {
                        message: "only one kwarg is allowed".to_owned(),
                        py_err: None,
                        span: None,
                    }));
                }
            }
        }
    }

    let args = ast.method1_unbound(
        "arguments",
        (posonly, args, vararg, kwonly, kw_defaults, kwarg, defaults),
        Some(span),
    )?;

    let block = transpile_block_with_final_expr(ast, body)?;
    let mut body_stmts = block.stmts;

    if let Some(final_expr) = block.final_expr {
        body_stmts.push(ast.method1_unbound("Return", (final_expr,), Some(span))?);
    };

    let decorators = empty();

    Ok((aux_stmts, args, body_stmts, decorators))
}

fn transpile_fn_expr<'py>(
    ast: &TlCtx<'py>,
    arglist: &[ArgItem],
    body: &SBlock,
    span: &Span,
) -> TlResult<PyExprWithAux> {
    let name = format!("__tl{}", span.start);
    let (aux_stmts, args, body_stmts, decorators) = make_fn_node(ast, arglist, body, span)?;

    let mut aux_stmts = aux_stmts;

    aux_stmts.push(ast.method1_unbound(
        "FunctionDef",
        (&name, args, body_stmts, decorators),
        Some(span),
    )?);

    Ok(PyExprWithAux {
        expr: ast.name(&name, PyAccessCtx::Load, Some(span))?,
        aux_stmts,
    })
}

fn transpile_fn_def<'py>(
    ast: &TlCtx<'py>,
    arglist: &[ArgItem],
    body: &SBlock,
    name: &str,
    span: &Span,
) -> TlResult<PyStmts> {
    let (aux_stmts, args, body_stmts, decorators) = make_fn_node(ast, arglist, body, span)?;
    let mut stmts = aux_stmts;
    stmts.push(ast.method1_unbound(
        "FunctionDef",
        (name, args, body_stmts, decorators),
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
        Expr::Fn(arglist, body) => transpile_fn_expr(ast, arglist, body, span),
        Expr::Literal((lit, span)) => {
            let value = match lit {
                Literal::Num(num) => ast.constant(num.parse::<i128>().map_err(|_| {
                    TlErr::TranspileErr(TranspileErr {
                        message: "int parse fail".to_owned(),
                        py_err: None,
                        span: Some(*span),
                    })
                })?),
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
                expr: ast.method1_unbound("Attribute", (obj.expr, attr.0), Some(span))?,
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
                return Err(TlErr::TranspileErr(TranspileErr {
                    message: format!("Unsupported binary operator: {:?}", op),
                    py_err: None,
                    span: Some(*span),
                }));
            }
        }
        Expr::Unary(op, expr) => {
            let expr = transpile_expr(ast, expr)?;
            let aux_stmts = expr.aux_stmts;

            let py_op = match op {
                UnaryOp::Neg => "USub",
                UnaryOp::Pos => "UAdd",
                UnaryOp::Inv => "Invert",
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
                        let value = transpile_block_with_final_expr(ast, value)?;

                        aux_stmts.extend(key.aux_stmts);
                        aux_stmts.extend(value.stmts);

                        keys.push(key.expr);
                        values.push(value.final_expr.unwrap_or_else(|| ast.py.None()));
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
        Expr::FmtStr(parts) => {
            unimplemented!();
        }
        Expr::Class(name, bases, body) => {
            unimplemented!();
        }
    }
}

pub fn transpile(block: &SBlock) -> TlResult<String> {
    Python::with_gil(move |py| {
        let ast = TlCtx::new(py)?;
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
