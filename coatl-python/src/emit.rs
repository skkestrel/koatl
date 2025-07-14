use coatl::py::ast::*;
use parser::ast::*;

use pyo3::{
    call::PyCallArgs,
    ffi::c_str,
    prelude::*,
    types::{PyDict, PyList, PyString},
};

#[derive(Debug)]
pub struct PyTlErr {
    pub message: String,
    pub py_err: Option<PyErr>,
    pub span: Option<Span>,
}

impl From<PyErr> for PyTlErr {
    fn from(err: PyErr) -> Self {
        PyTlErr {
            message: err.to_string(),
            py_err: Some(err),
            span: None,
        }
    }
}

pub type PyTlResult<T> = Result<T, PyTlErr>;

struct PyCtx<'py, 'src> {
    py: Python<'py>,
    source: &'src str,
    ast_module: Bound<'py, PyModule>,
}

impl<'py, 'src> PyCtx<'py, 'src> {
    fn new(py: Python<'py>, source: &'src str) -> PyTlResult<Self> {
        let ast_module = py.import("ast")?;

        Ok(PyCtx {
            py,
            source,
            ast_module,
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

    fn ast_cls<A>(&self, name: &str, args: A) -> PyTlResult<PyObject>
    where
        A: PyCallArgs<'py>,
    {
        let node = self.ast_module.call_method1(name, args)?;

        Ok(node.unbind())
    }

    fn ast_node<A>(&self, name: &str, args: A, span: &Span) -> PyTlResult<PyObject>
    where
        A: PyCallArgs<'py>,
    {
        let node = self.ast_module.call_method1(name, args)?;
        let (line, col) = self.linecol(span.start);
        node.setattr("lineno", line)?;
        node.setattr("col_offset", col)?;
        let (line, col) = self.linecol(span.end);
        node.setattr("end_lineno", line)?;
        node.setattr("end_col_offset", col)?;

        Ok(node.unbind())
    }

    fn ast_node_unspanned<A>(&self, name: &str, args: A) -> PyTlResult<PyObject>
    where
        A: PyCallArgs<'py>,
    {
        let node = self.ast_module.call_method1(name, args)?;

        Ok(node.unbind())
    }
}

trait PyBlockExt<'src> {
    fn emit_py<'py>(&self, ctx: &PyCtx<'py, 'src>) -> PyTlResult<Py<PyList>>;
}

impl<'src> PyBlockExt<'src> for PyBlock<'src> {
    fn emit_py<'py>(&self, ctx: &PyCtx<'py, 'src>) -> PyTlResult<Py<PyList>> {
        let stmts: Vec<_> = self
            .0
            .iter()
            .map(|stmt| stmt.emit_py(ctx))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(PyList::new(ctx.py, stmts)?.unbind())
    }
}

trait PyStmtExt<'src> {
    fn emit_py<'py>(&self, ctx: &PyCtx<'py, 'src>) -> PyTlResult<PyObject>;
}

impl<'src> PyStmtExt<'src> for SPyStmt<'src> {
    fn emit_py<'py>(&self, ctx: &PyCtx<'py, 'src>) -> PyTlResult<PyObject> {
        match &self.value {
            PyStmt::Expr(expr) => ctx.ast_node("Expr", (expr.emit_py(ctx)?,), &self.tl_span),
            PyStmt::If(py_spanned, py_block, py_block1) => todo!(),
            PyStmt::Match(py_spanned, py_match_cases) => todo!(),
            PyStmt::Assign(py_spanned, py_spanned1) => todo!(),
            PyStmt::Return(py_spanned) => todo!(),
            PyStmt::Raise(py_spanned) => todo!(),
            PyStmt::Assert(py_spanned, py_spanned1) => todo!(),
            PyStmt::Global(cows) => todo!(),
            PyStmt::Nonlocal(cows) => todo!(),
            PyStmt::Import(py_import_alias) => todo!(),
            PyStmt::ImportFrom(cow, items) => todo!(),
            PyStmt::FnDef(cow, py_arg_def_items, py_block) => todo!(),
            PyStmt::ClassDef(cow, py_call_items, py_block) => todo!(),
            PyStmt::While(py_spanned, py_block) => todo!(),
            PyStmt::For(cow, py_spanned, py_block) => todo!(),
            PyStmt::Try(py_block, py_except_handlers, py_block1) => todo!(),
            PyStmt::Break => todo!(),
            PyStmt::Continue => todo!(),
        }
    }
}

trait PyNameCtxExt {
    fn emit_py(&self, ctx: &PyCtx<'_, '_>) -> PyTlResult<PyObject>;
}

impl PyNameCtxExt for PyNameCtx {
    fn emit_py(&self, ctx: &PyCtx<'_, '_>) -> PyTlResult<PyObject> {
        Ok(ctx.ast_cls(
            match self {
                PyNameCtx::Load => "Load",
                PyNameCtx::Store => "Store",
                PyNameCtx::Del => "Del",
            },
            (),
        )?)
    }
}

trait PyExprExt<'src> {
    fn emit_py<'py>(&self, ctx: &PyCtx<'py, 'src>) -> PyTlResult<PyObject>;
}

impl<'src> PyExprExt<'src> for SPyExpr<'src> {
    fn emit_py<'py>(&self, ctx: &PyCtx<'py, 'src>) -> PyTlResult<PyObject> {
        Ok(match &self.value {
            PyExpr::Literal(lit) => match lit {
                PyLiteral::Num(num) => match num.parse::<i128>() {
                    Ok(i) => ctx.ast_node("Constant", (i,), &self.tl_span)?,
                    Err(_) => match num.parse::<f64>() {
                        Ok(f) => ctx.ast_node("Constant", (f,), &self.tl_span)?,
                        Err(_) => {
                            return Err(PyTlErr {
                                message: format!("Invalid number literal: {}", num),
                                py_err: None,
                                span: Some(self.tl_span),
                            })
                        }
                    },
                },
                PyLiteral::Bool(b) => ctx.ast_node("Constant", (b,), &self.tl_span)?,
                PyLiteral::Str(s) => ctx.ast_node("Constant", (s,), &self.tl_span)?,
                PyLiteral::None => ctx.ast_node("Constant", (ctx.py.None(),), &self.tl_span)?,
            },
            PyExpr::Ident(ident, c) => {
                ctx.ast_node("Name", (ident, c.emit_py(ctx)?), &self.tl_span)?
            }
            PyExpr::Binary(op, left, right) => {
                let py_op_str = match op {
                    PyBinaryOp::Add => Some("Add"),
                    PyBinaryOp::Sub => Some("Sub"),
                    PyBinaryOp::Mult => Some("Mult"),
                    PyBinaryOp::Div => Some("Div"),
                    PyBinaryOp::Mod => Some("Mod"),
                    PyBinaryOp::Pow => Some("Pow"),
                    _ => None,
                };

                if let Some(py_op_str) = py_op_str {
                    return ctx.ast_node(
                        "BinOp",
                        (
                            left.emit_py(ctx)?,
                            ctx.ast_cls(py_op_str, ())?,
                            right.emit_py(ctx)?,
                        ),
                        &self.tl_span,
                    );
                }

                let py_cmp_op = match op {
                    PyBinaryOp::Lt => Some("Lt"),
                    PyBinaryOp::Gt => Some("Gt"),
                    PyBinaryOp::Leq => Some("LtE"),
                    PyBinaryOp::Geq => Some("GtE"),
                    PyBinaryOp::Eq => Some("Eq"),
                    PyBinaryOp::Neq => Some("NotEq"),
                    PyBinaryOp::Is => Some("Is"),
                    PyBinaryOp::Nis => Some("IsNot"),
                    _ => None,
                };

                if let Some(py_cmp_op) = py_cmp_op {
                    return Ok(ctx.ast_node(
                        "Compare",
                        (
                            left.emit_py(ctx)?,
                            [ctx.ast_cls(py_cmp_op, ())?],
                            [right.emit_py(ctx)?],
                        ),
                        &self.tl_span,
                    )?);
                }

                return Err(PyTlErr {
                    message: format!("Unsupported binary operator: {:?}", op),
                    py_err: None,
                    span: Some(self.tl_span),
                });
            }
            PyExpr::Fstr(py_fstr_parts) => todo!(),
            PyExpr::Unary(py_unary_op, py_spanned) => todo!(),
            PyExpr::Call(py_spanned, py_call_items) => todo!(),
            PyExpr::Attribute(py_spanned, cow, py_name_ctx) => todo!(),
            PyExpr::Subscript(py_spanned, py_spanned1, py_name_ctx) => todo!(),
            PyExpr::Tuple(py_tuple_items) => todo!(),
            PyExpr::Dict(py_dict_items) => todo!(),
            PyExpr::Slice(py_spanned, py_spanned1, py_spanned2) => todo!(),
            PyExpr::Yield(py_spanned) => todo!(),
            PyExpr::YieldFrom(py_spanned) => todo!(),
        })
    }
}

pub fn emit_py(block: &PyBlock, source: &str) -> PyTlResult<PyObject> {
    Python::with_gil(move |py| {
        let ctx = PyCtx::new(py, source)?;
        let stmts = block.emit_py(&ctx)?;

        let root_node = ctx.ast_node_unspanned("Module", (stmts, PyList::empty(py)))?;

        ctx.ast_module
            .call_method1("fix_missing_locations", (&root_node,))?;

        let dump_args = PyDict::new(ctx.py);
        dump_args.set_item("indent", 4)?;

        // let dump = ctx
        //     .ast_module
        //     .call_method("dump", (&root_node,), Some(&dump_args))?
        //     .extract::<String>()?;

        Ok(root_node)
    })
}
