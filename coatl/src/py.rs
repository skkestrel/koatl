use std::borrow::Cow;

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
        err_span: Option<Span>,
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

    fn name(&self, name: &str, ctx: NameCtx, err_span: Option<Span>) -> TlResult<PyObject> {
        self.method1_unbound("Name", (name, self.name_ctx(ctx)?), err_span)
    }

    fn constant<T: pyo3::IntoPyObject<'py>>(&self, value: T) -> TlResult<PyObject> {
        Ok(self.constant_node.call1((value, self.py.None()))?.unbind())
    }
}

fn transpile_block<'py>(ast: &PyAst<'py>, block: SBlock) -> TlResult<PyStmts> {
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
            let t = transpile_expr(ast, sexpr)?;
            let mut stmts = t.aux_stmts;
            stmts.push(ast.method1_unbound("Expr", (t.expr,), Some(span))?);

            Ok(stmts)
        }
    }
}

fn transpile_stmt<'py>(ast: &PyAst<'py>, stmt: SStmt) -> TlResult<PyStmts> {
    let (stmt, span) = stmt;

    match stmt {
        Stmt::Expr(expr) => {
            let expr = transpile_expr(ast, expr)?;
            let mut stmts = expr.aux_stmts;
            stmts.push(ast.method1_unbound("Expr", (expr.expr,), Some(span))?);

            Ok(stmts)
        }
        Stmt::Return(expr) => {
            let value = transpile_expr(ast, expr)?;
            let mut stmts = value.aux_stmts;

            stmts.push(ast.method1_unbound("Return", (value.expr,), Some(span))?);

            Ok(stmts)
        }

        Stmt::Assign(target, value) => {
            let target_span = target.1;
            let mut target_node = transpile_expr(ast, target)?;
            let value_node = transpile_expr(ast, value)?;

            let mut stmts = value_node.aux_stmts;
            stmts.append(&mut target_node.aux_stmts);

            let lhs = target_node.expr;

            lhs.setattr(ast.py, "ctx", ast.name_ctx(NameCtx::Load)?)
                .map_err(|e| {
                    TlErr::TranspileErr(TranspileErr {
                        message: format!("Failed to set context for assignment: {}", e),
                        span: Some(target_span),
                    })
                })?;

            stmts.push(ast.method1_unbound(
                "Assign",
                ([lhs], value_node.expr),
                Some(target_span),
            )?);

            Ok(stmts)
        }

        _ => Ok(vec![ast.constant("[statement]")?]),
    }
}

type PyStmts = Vec<PyObject>;

struct PyExprWithAux {
    expr: PyObject,
    aux_stmts: PyStmts,
}

fn transpile_fn<'py>(
    ast: &PyAst<'py>,
    func: SExpr,
    name: Option<Cow<str>>,
) -> TlResult<PyExprWithAux> {
    let (Expr::Fn(arglist, body), span) = func else {
        return Err(TlErr::TranspileErr(TranspileErr {
            message: "expected a function expression".to_owned(),
            span: Some(func.1),
        }));
    };

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

    let body_ast = match *body {
        (Block::Stmts(stmts), span) => transpile_block(ast, (Block::Stmts(stmts), span))?,
        (Block::Expr(expr), span) => {
            let mut expr = transpile_expr(ast, expr)?;
            aux_stmts.append(&mut expr.aux_stmts);

            vec![ast.method1_unbound("Return", (expr.expr,), Some(span))?]
        }
    };

    let name = name.unwrap_or(Cow::from(format!("__tl{}", span.start)));
    let decorators = empty();

    aux_stmts.push(ast.method1_unbound(
        "FunctionDef",
        (&name, args, body_ast, decorators),
        Some(span),
    )?);

    Ok(PyExprWithAux {
        expr: ast.name(&name, NameCtx::Load, Some(span))?,
        aux_stmts,
    })
}

fn transpile_expr<'py>(ast: &PyAst<'py>, expr: SExpr) -> TlResult<PyExprWithAux> {
    let no_aux = |expr| PyExprWithAux {
        expr,
        aux_stmts: vec![],
    };

    let (expr, span) = expr;

    match expr {
        Expr::Fn(arglist, body) => transpile_fn(ast, (Expr::Fn(arglist, body), span), None),
        Expr::Literal((lit, span)) => {
            let value = match lit {
                Literal::Num(num) => ast.constant(num.parse::<i128>().map_err(|_| {
                    TlErr::TranspileErr(TranspileErr {
                        message: "int parse fail".to_owned(),
                        span: Some(span),
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
            let obj = transpile_expr(ast, *obj)?;

            Ok(PyExprWithAux {
                expr: ast.method1_unbound("Attribute", (obj.expr, attr.0), Some(span))?,
                aux_stmts: obj.aux_stmts,
            })
        }
        Expr::Call(obj, args) => {
            let obj = transpile_expr(ast, *obj)?;
            let mut aux_stmts = obj.aux_stmts;

            let mut args_nodes = vec![];
            let mut keywords_nodes = vec![];

            for arg in args {
                match arg.0 {
                    CallItem::Arg(expr) => {
                        let e = transpile_expr(ast, expr)?;
                        aux_stmts.extend(e.aux_stmts);
                        args_nodes.push(e.expr);
                    }
                    CallItem::Kwarg((name, name_span), expr) => {
                        let e = transpile_expr(ast, expr)?;
                        aux_stmts.extend(e.aux_stmts);
                        keywords_nodes.push(ast.method1_unbound(
                            "keyword",
                            (name, e.expr),
                            Some(span),
                        )?);
                    }
                    CallItem::ArgSpread(expr) => {
                        let e = transpile_expr(ast, expr)?;
                        aux_stmts.extend(e.aux_stmts);
                        args_nodes.push(ast.method1_unbound("Starred", (e.expr,), Some(span))?);
                    }
                    CallItem::KwargSpread(expr) => {
                        let e = transpile_expr(ast, expr)?;
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
            let lhs = transpile_expr(ast, *lhs)?;
            let mut rhs = transpile_expr(ast, *rhs)?;

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
            let obj = transpile_expr(ast, *obj)?;
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
            let cond = transpile_expr(ast, *cond)?;
            let mut aux_stmts = cond.aux_stmts;

            let ret_varname = &format!("__tl{}", span.start);
            let ret_var = ast.name(ret_varname, NameCtx::Store, Some(span))?;

            let mut then_block = transpile_block(ast, *then_block)?;
            let mut else_block = match else_block {
                Some(else_block) => transpile_block(ast, *else_block)?,
                None => vec![],
            };

            if !then_block.is_empty() {
                let last_stmt = then_block.last().unwrap().bind(ast.py);

                if last_stmt.is_instance(&ast.ast_module.getattr("Expr")?)? {
                    *then_block.last_mut().unwrap() = ast.method1_unbound(
                        "Assign",
                        ([&ret_var], last_stmt.getattr("value")?),
                        Some(span),
                    )?;
                } else {
                    *then_block.last_mut().unwrap() =
                        ast.method1_unbound("Assign", ([&ret_var], ast.py.None()), Some(span))?;
                }
            }

            if !else_block.is_empty() {
                let last_stmt = else_block.last().unwrap().bind(ast.py);

                if last_stmt.is_instance(&ast.ast_module.getattr("Expr")?)? {
                    *else_block.last_mut().unwrap() = ast.method1_unbound(
                        "Assign",
                        ([&ret_var], last_stmt.getattr("value")?),
                        Some(span),
                    )?;
                } else {
                    *else_block.last_mut().unwrap() =
                        ast.method1_unbound("Assign", ([&ret_var], ast.py.None()), Some(span))?;
                }
            }

            aux_stmts.push(ast.method1_unbound(
                "If",
                (cond.expr, then_block, else_block),
                Some(span),
            )?);

            Ok(PyExprWithAux {
                expr: ast.name(ret_varname, NameCtx::Load, Some(span))?,
                aux_stmts,
            })
        }
        Expr::Yield(expr) => {
            let value = transpile_expr(ast, *expr)?;

            Ok(PyExprWithAux {
                expr: ast.method1_unbound("Yield", (value.expr,), Some(span))?,
                aux_stmts: value.aux_stmts,
            })
        }
        Expr::YieldFrom(expr) => {
            let value = transpile_expr(ast, *expr)?;

            Ok(PyExprWithAux {
                expr: ast.method1_unbound("YieldFrom", (value.expr,), Some(span))?,
                aux_stmts: value.aux_stmts,
            })
        }
        Expr::Binary(op, lhs, rhs) => {
            let lhs = transpile_expr(ast, *lhs)?;
            let mut rhs = transpile_expr(ast, *rhs)?;

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
                    span: Some(span),
                }));
            }
        }
        Expr::Unary(op, expr) => {
            let expr = transpile_expr(ast, *expr)?;
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
        _ => Ok(no_aux(ast.constant("[expression]")?)),
    }
}

pub fn transpile(block: SBlock) -> TlResult<String> {
    Python::with_gil(move |py| {
        let ast = PyAst::new(py)?;
        let span = block.1;

        let root_node = ast.method1_unbound(
            "Module",
            (transpile_block(&ast, block)?, PyList::empty(py)),
            Some(span),
        )?;

        ast.ast_module
            .call_method1("fix_missing_locations", (&root_node,))?;

        let dump_args = PyDict::new(ast.py);
        dump_args.set_item("indent", 4)?;

        let dump = ast
            .ast_module
            .call_method("dump", (&root_node,), Some(&dump_args))?
            .extract::<String>()?;

        let source = ast
            .ast_module
            .call_method1("unparse", (root_node,))?
            .extract()?;

        println!("{}", dump);
        println!();

        return Ok(source);
    })
}
