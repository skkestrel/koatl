use parser::*;
use pyo3::{
    call::PyCallArgs,
    prelude::*,
    types::{PyDict, PyList},
};

pub struct TranspileErr {
    pub message: String,
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
enum NameCtx {
    Load,
    Store,
    Del,
}

struct PyAst<'py> {
    py: Python<'py>,
    ast_module: Bound<'py, PyModule>,
    constant_node: Bound<'py, PyAny>,
}

impl<'py> PyAst<'py> {
    fn new(py: Python<'py>) -> TlResult<Self> {
        let ast_module = py.import("ast")?;
        let constant_node = ast_module.getattr("Constant")?;

        Ok(PyAst {
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
            .map_err(TlErr::from)
            .map(|obj| obj.unbind())
    }

    fn name_ctx(&self, ctx: NameCtx) -> TlResult<Bound<'py, PyAny>> {
        Ok(self.ast_module.getattr(match ctx {
            NameCtx::Load => "Load",
            NameCtx::Store => "Store",
            NameCtx::Del => "Del",
        })?)
    }

    fn name(&self, name: &str, ctx: NameCtx, err_span: Option<&Span>) -> TlResult<PyObject> {
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

fn transpile_block<'py>(ast: &PyAst<'py>, block: &SBlock) -> TlResult<PyBlock> {
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

fn transpile_stmt<'py>(ast: &PyAst<'py>, stmt: &SStmt) -> TlResult<PyStmts> {
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

            let mut target_node = transpile_expr(ast, target)?;
            let value_node = transpile_expr(ast, value)?;

            let mut stmts = value_node.aux_stmts;
            stmts.append(&mut target_node.aux_stmts);

            let lhs = target_node.expr;

            lhs.setattr(ast.py, "ctx", ast.name_ctx(NameCtx::Load)?)
                .map_err(|e| {
                    TlErr::TranspileErr(TranspileErr {
                        message: format!("Failed to set context for assignment: {}", e),
                        span: Some(target.1),
                    })
                })?;

            stmts.push(ast.method1_unbound("Assign", ([lhs], value_node.expr), Some(&target.1))?);

            Ok(stmts)
        }

        _ => Ok(vec![ast.constant("[statement]")?]),
    }
}

type PyStmts = Vec<PyObject>;
type PyExpr = PyObject;

struct PyExprWithAux {
    expr: PyObject,
    aux_stmts: PyStmts,
}

fn transpile_if_stmt<'py>(
    ast: &PyAst<'py>,
    cond: &SExpr,
    then_block: &SBlock,
    else_block: &Option<Box<SBlock>>,
    span: &Span,
) -> TlResult<PyStmts> {
    let mut stmts = vec![];

    let cond = transpile_expr(ast, cond)?;
    stmts.extend(cond.aux_stmts.into_iter());

    let then_block = transpile_block(ast, then_block)?;
    let mut then_block_ast = then_block.stmts;
    if let Some(final_expr) = then_block.final_expr {
        then_block_ast.push(ast.method1_unbound("Expr", (final_expr,), Some(span))?);
    }

    let mut else_block_ast = None;
    if let Some(else_block) = else_block {
        let else_block = transpile_block(ast, else_block)?;
        let mut block = else_block.stmts;

        if let Some(final_expr) = else_block.final_expr {
            block.push(ast.method1_unbound("Expr", (final_expr,), Some(span))?);
        }

        else_block_ast = Some(block);
    }

    Ok(vec![ast.method1_unbound(
        "If",
        (cond.expr, then_block_ast, else_block_ast),
        Some(span),
    )?])
}

fn transpile_if_expr<'py>(
    ast: &PyAst<'py>,
    cond: &SExpr,
    then_block: &SBlock,
    else_block: &Option<Box<SBlock>>,
    span: &Span,
) -> TlResult<PyExprWithAux> {
    let cond = transpile_expr(ast, cond)?;
    let mut aux_stmts = cond.aux_stmts;

    let ret_varname = &format!("__tl{}", span.start);
    let ret_var = ast.name(ret_varname, NameCtx::Store, Some(span))?;

    let PyBlock { stmts, final_expr } = transpile_block(ast, then_block)?;
    let mut then_block_ast = stmts;

    if let Some(final_expr) = final_expr {
        then_block_ast.push(ast.method1_unbound("Assign", ([&ret_var], final_expr), Some(span))?);
    } else {
        return Err(TlErr::TranspileErr(TranspileErr {
            message: "then block must have a final expression".to_owned(),
            span: Some(*span),
        }));
    }

    let else_block = else_block.as_ref().ok_or_else(|| {
        TlErr::TranspileErr(TranspileErr {
            message: "else block is required in an if-expr".to_owned(),
            span: Some(*span),
        })
    })?;

    let PyBlock { stmts, final_expr } = transpile_block(ast, else_block)?;
    let mut else_block_ast = stmts;
    if let Some(final_expr) = final_expr {
        else_block_ast.push(ast.method1_unbound("Assign", ([&ret_var], final_expr), Some(span))?);
    } else {
        return Err(TlErr::TranspileErr(TranspileErr {
            message: "else block must have a final expression".to_owned(),
            span: Some(*span),
        }));
    }

    aux_stmts.push(ast.method1_unbound(
        "If",
        (cond.expr, then_block_ast, else_block_ast),
        Some(span),
    )?);

    Ok(PyExprWithAux {
        expr: ast.name(ret_varname, NameCtx::Load, Some(span))?,
        aux_stmts,
    })
}

fn transpile_match_stmt<'py>(
    ast: &PyAst<'py>,
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

        let py_block = transpile_block(ast, block)?;
        let block_stmts = py_block.stmts;

        py_cases.push(ast.method1_unbound(
            "match_case",
            (pattern.expr, ast.py.None(), block_stmts),
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
    ast: &PyAst<'py>,
    subject: &SExpr,
    cases: &[(SExpr, Box<SBlock>)],
    span: &Span,
) -> TlResult<PyExprWithAux> {
    let subject = transpile_expr(ast, subject)?;
    let mut aux_stmts = subject.aux_stmts;

    let ret_varname = &format!("__tl{}", span.start);
    let ret_var = ast.name(ret_varname, NameCtx::Store, Some(span))?;

    let mut py_cases = vec![];
    for (pattern, block) in cases {
        let pattern = transpile_expr(ast, pattern)?;
        aux_stmts.extend(pattern.aux_stmts);

        let py_block = transpile_block(ast, &*block)?;
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
        expr: ast.name(ret_varname, NameCtx::Load, Some(span))?,
        aux_stmts,
    })
}

fn make_fn_node<'py>(
    ast: &PyAst<'py>,
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
                        span: None,
                    }));
                }
            }
            ArgItem::KwargSpread(name) => {
                kwarg.push(arg_node(name.0)?);
                if kwarg.len() > 1 {
                    return Err(TlErr::TranspileErr(TranspileErr {
                        message: "only one kwarg is allowed".to_owned(),
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

    let block = transpile_block(ast, body)?;
    let mut body_stmts = block.stmts;

    if let Some(final_expr) = block.final_expr {
        body_stmts.push(ast.method1_unbound("Return", (final_expr,), Some(span))?);
    };

    let decorators = empty();

    Ok((aux_stmts, args, body_stmts, decorators))
}

fn transpile_fn_expr<'py>(
    ast: &PyAst<'py>,
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
        expr: ast.name(&name, NameCtx::Load, Some(span))?,
        aux_stmts,
    })
}

fn transpile_fn_def<'py>(
    ast: &PyAst<'py>,
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

fn transpile_expr<'py>(ast: &PyAst<'py>, expr: &SExpr) -> TlResult<PyExprWithAux> {
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
                        span: Some(*span),
                    })
                })?),
                Literal::Str(s) => ast.constant(s.to_string()),
            }?;

            Ok(no_aux(value))
        }
        Expr::Ident((ident, span)) => {
            let name = ast.name(ident, NameCtx::Load, Some(span))?;
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
                .map(|index| {
                    let e = transpile_expr(ast, index)?;
                    aux_stmts.extend(e.aux_stmts);
                    Ok(e.expr)
                })
                .collect::<TlResult<Vec<_>>>()?;

            Ok(PyExprWithAux {
                expr: ast.method1_unbound("Subscript", (obj.expr, indices), Some(span))?,
                aux_stmts,
            })
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
                        let value = transpile_block(ast, value)?;

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
        _ => Ok(no_aux(ast.constant("[expression]")?)),
    }
}

pub fn transpile(block: &SBlock) -> TlResult<String> {
    Python::with_gil(move |py| {
        let ast = PyAst::new(py)?;
        let span = &block.1;

        let block = transpile_block(&ast, block)?;
        let mut stmts = block.stmts;
        stmts.push(ast.method1_unbound("Expr", (block.final_expr,), Some(span))?);

        let root_node = ast.method1_unbound("Module", (stmts, PyList::empty(py)), Some(span))?;

        ast.ast_module
            .call_method1("fix_missing_locations", (&root_node,))?;

        let dump_args = PyDict::new(ast.py);
        dump_args.set_item("indent", 4)?;

        let dump = ast
            .ast_module
            .call_method("dump", (&root_node,), Some(&dump_args))?
            .extract::<String>()?;

        println!("{}", dump);

        let source = ast
            .ast_module
            .call_method1("unparse", (root_node,))?
            .extract()?;

        println!();

        return Ok(source);
    })
}
