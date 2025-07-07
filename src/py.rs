use pyo3::{call::PyCallArgs, prelude::*, types::PyDict};

use crate::parser::*;

pub enum TlErr {
    PyErr(PyErr),
    Other(String),
}

impl From<PyErr> for TlErr {
    fn from(err: PyErr) -> Self {
        TlErr::PyErr(err)
    }
}

pub type TlResult<T> = Result<T, TlErr>;

struct PyAst<'py> {
    py: Python<'py>,
    ast_module: Bound<'py, PyModule>,
    constant_node: Bound<'py, PyAny>,
}

enum NameCtx {
    Load,
    Store,
    Del,
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

    fn method1_unbound<A>(&self, method: &str, args: A) -> TlResult<PyObject>
    where
        A: PyCallArgs<'py>,
    {
        self.ast_module
            .call_method1(method, args)
            .map_err(TlErr::from)
            .map(|obj| obj.unbind())
    }

    fn name(&self, name: &str, ctx: NameCtx) -> TlResult<PyObject> {
        let ctx = match ctx {
            NameCtx::Load => "Load",
            NameCtx::Store => "Store",
            NameCtx::Del => "Del",
        };

        self.method1_unbound("Name", (name, self.ast_module.getattr(ctx)?))
    }

    fn module(&self, value: Vec<PyObject>) -> TlResult<PyObject> {
        self.method1_unbound("Module", (value, Vec::<PyObject>::new()))
    }

    fn constant<T: pyo3::IntoPyObject<'py>>(&self, value: T) -> TlResult<PyObject> {
        Ok(self.constant_node.call1((value, self.py.None()))?.unbind())
    }
}

fn transpile_block<'py>(ast: &PyAst<'py>, block: Block) -> TlResult<PyStmts> {
    match block {
        Block::Stmts(stmts) => {
            let mut result = Vec::new();
            for stmt in stmts {
                let stmts_vec = transpile_stmt(ast, stmt)?;
                result.extend(stmts_vec);
            }
            Ok(result)
        }
        Block::Expr(expr) => {
            panic!();
        }
    }
}

fn transpile_stmt<'py>(ast: &PyAst<'py>, stmt: Stmt) -> TlResult<PyStmts> {
    match stmt {
        Stmt::Expr(expr) => {
            let expr = transpile_expr(ast, expr)?;
            let mut stmts = expr.aux_stmts;
            stmts.push(ast.method1_unbound("Expr", (expr.expr,))?);

            Ok(stmts)
        }
        Stmt::Return(expr) => {
            let value = transpile_expr(ast, expr)?;
            let mut stmts = value.aux_stmts;

            stmts.push(ast.method1_unbound("Return", (value.expr,))?);

            Ok(stmts)
        }
        Stmt::Assign(target, value) => {
            let value = transpile_expr(ast, value)?;
            let mut stmts = value.aux_stmts;

            match target {
                Decl::Single(name) => {
                    let targets = vec![ast.name(name.ident.0, NameCtx::Store)?];

                    stmts.push(ast.method1_unbound("Assign", (targets, value.expr))?);
                }
                Decl::Unpack(names) => {
                    panic!();
                }
            }

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

fn transpile_expr<'py>(ast: &PyAst<'py>, expr: Expr) -> TlResult<PyExprWithAux> {
    match expr {
        Expr::Fn(arglist, body) => {
            let empty = || Vec::<PyObject>::new();
            let posonly = empty();
            let mut args = empty();
            let mut vararg = empty();
            let kwonly = empty();
            let mut kwarg = empty();
            let kw_defaults = empty();
            let mut defaults = empty();

            let arg_node = |name: &str| ast.method1_unbound("arg", (name, ast.py.None()));

            let mut aux_stmts = empty();

            for arg in arglist {
                match arg {
                    ArgItem::Arg(name) => {
                        args.push(arg_node(name.ident.0)?);
                        defaults.push(ast.py.None());
                    }
                    ArgItem::DefaultArg(name, default) => {
                        let mut expr = transpile_expr(ast, default)?;
                        aux_stmts.append(&mut expr.aux_stmts);

                        args.push(arg_node(name.ident.0)?);
                        defaults.push(expr.expr);
                    }
                    ArgItem::ArgSpread(name) => {
                        vararg.push(arg_node(name.ident.0)?);
                        if vararg.len() > 1 {
                            return Err(TlErr::Other("only one vararg is allowed".to_owned()));
                        }
                    }
                    ArgItem::KwargSpread(name) => {
                        kwarg.push(arg_node(name.ident.0)?);
                        if kwarg.len() > 1 {
                            return Err(TlErr::Other("only one kwarg is allowed".to_owned()));
                        }
                    }
                }
            }

            let args = ast.method1_unbound(
                "arguments",
                (posonly, args, vararg, kwonly, kw_defaults, kwarg, defaults),
            )?;

            let body = match body {
                Block::Stmts(stmts) => transpile_block(ast, Block::Stmts(stmts))?,
                Block::Expr(expr) => {
                    let mut expr = transpile_expr(ast, *expr)?;
                    aux_stmts.append(&mut expr.aux_stmts);

                    vec![ast.method1_unbound("Return", (expr.expr,))?]
                }
            };

            let name = format!("__tl{}", 0);
            let decorators = empty();

            aux_stmts.push(ast.method1_unbound("FunctionDef", (&name, args, body, decorators))?);

            Ok(PyExprWithAux {
                expr: ast.name(&name, NameCtx::Load)?,
                aux_stmts,
            })
        }
        _ => Ok(PyExprWithAux {
            expr: ast.constant("[expression]")?,
            aux_stmts: vec![],
        }),
    }
}

pub fn transpile(block: Block) -> TlResult<String> {
    Python::with_gil(move |py| {
        let ast = PyAst::new(py)?;

        let root_node = ast.module(transpile_block(&ast, block)?)?;

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

        return Ok(source);
    })
}

// use pyo3::ffi::c_str;
// use pyo3::types::IntoPyDict;
// fn main() -> TlResult<()> {
//     Python::with_gil(|py| {
//         let sys = py.import("sys")?;
//         let version: String = sys.getattr("version")?.extract()?;

//         let locals = [("os", py.import("os")?)].into_py_dict(py)?;
//         let code = c_str!("os.getenv('USER') or os.getenv('USERNAME') or 'Unknown'");
//         let user: String = py.eval(code, None, Some(&locals))?.extract()?;

//         println!("Hello {}, I'm Python {}", user, version);
//         Ok(())
//     })
// }
