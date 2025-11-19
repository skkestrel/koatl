use koatl_core::{py::ast::*, util::LineColCache, Span};

use pyo3::{
    call::PyCallArgs,
    prelude::*,
    types::{PyBool, PyDict, PyList, PyNone},
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

#[allow(dead_code)]
struct PyCtx<'py, 'src> {
    py: Python<'py>,
    source: &'src str,
    line_cache: LineColCache,
    ast_module: Bound<'py, PyModule>,
}

impl<'py, 'src> PyCtx<'py, 'src> {
    fn new(py: Python<'py>, source: &'src str) -> PyTlResult<Self> {
        let ast_module = py.import("ast")?;

        Ok(Self {
            py,
            source,
            ast_module,
            line_cache: LineColCache::new(source),
        })
    }

    fn linecol(&self, cursor: usize) -> (usize, usize) {
        self.line_cache.linecol(cursor)
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
        let node = self
            .ast_module
            .call_method1(name, args)
            .map_err(|e| PyTlErr {
                message: format!("Failed to create AST node {}: {}", name, e),
                py_err: Some(e),
                span: Some(*span),
            })?;

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

trait PyArgDefExt<'src> {
    fn emit_py<'py>(&self, ctx: &PyCtx<'py, 'src>) -> PyTlResult<PyObject>;
}
impl<'src> PyArgDefExt<'src> for PyArgList<'src> {
    fn emit_py<'py>(&self, ctx: &PyCtx<'py, 'src>) -> PyTlResult<PyObject> {
        let mut py_posonlyargs = Vec::new();
        let mut py_args = Vec::new();
        let mut py_kwonlyargs = Vec::new();
        let mut py_defaults = Vec::new();
        let mut py_kw_defaults = Vec::new();
        let mut vararg = None;
        let mut kwarg = None;

        // Process position-only arguments
        for (arg_name, default) in &self.posonlyargs {
            let arg_ast = ctx.ast_cls("arg", (arg_name.as_ref(), ctx.py.None()))?;
            py_posonlyargs.push(arg_ast);
            if let Some(default_expr) = default {
                py_defaults.push(default_expr.emit_py(ctx)?);
            }
        }

        // Process regular arguments
        for (arg_name, default) in &self.args {
            let arg_ast = ctx.ast_cls("arg", (arg_name.as_ref(), ctx.py.None()))?;
            py_args.push(arg_ast);
            if let Some(default_expr) = default {
                py_defaults.push(default_expr.emit_py(ctx)?);
            }
        }

        // Process *args
        if let Some(arg_name) = &self.vararg {
            vararg = Some(ctx.ast_cls("arg", (arg_name.as_ref(), ctx.py.None()))?);
        }

        // Process keyword-only arguments
        for (arg_name, default) in &self.kwonlyargs {
            let arg_ast = ctx.ast_cls("arg", (arg_name.as_ref(), ctx.py.None()))?;
            py_kwonlyargs.push(arg_ast);
            if let Some(default_expr) = default {
                py_kw_defaults.push(default_expr.emit_py(ctx)?);
            } else {
                py_kw_defaults.push(ctx.py.None());
            }
        }

        // Process **kwargs
        if let Some(arg_name) = &self.kwarg {
            kwarg = Some(ctx.ast_cls("arg", (arg_name.as_ref(), ctx.py.None()))?);
        }

        ctx.ast_cls(
            "arguments",
            (
                py_posonlyargs, // posonlyargs
                py_args,        // args
                vararg,         // vararg
                py_kwonlyargs,  // kwonlyargs
                py_kw_defaults, // kw_defaults
                kwarg,          // kwarg
                py_defaults,    // defaults
            ),
        )
    }
}

trait PyDecoratorsExt<'src> {
    fn emit_py<'py>(&self, ctx: &PyCtx<'py, 'src>) -> PyTlResult<PyObject>;
}
impl<'src> PyDecoratorsExt<'src> for PyDecorators<'src> {
    fn emit_py<'py>(&self, ctx: &PyCtx<'py, 'src>) -> PyTlResult<PyObject> {
        let mut decorators = Vec::new();
        for decorator in &self.0 {
            let decorator_ast = decorator.emit_py(ctx)?;
            decorators.push(decorator_ast);
        }

        Ok(PyList::new(ctx.py, decorators)?.unbind().into_any())
    }
}

trait PyPatternExt<'src> {
    fn emit_py<'py>(&self, ctx: &PyCtx<'py, 'src>) -> PyTlResult<PyObject>;
}

impl<'src> PyPatternExt<'src> for SPyPattern<'src> {
    fn emit_py<'py>(&self, ctx: &PyCtx<'py, 'src>) -> PyTlResult<PyObject> {
        Ok(match &self.value {
            PyPattern::As(pattern, ident) => {
                let pattern_ast = pattern.as_ref().map(|x| x.emit_py(ctx)).transpose()?;

                ctx.ast_node("MatchAs", (pattern_ast, ident.as_ref()), &self.tl_span)?
            }
            PyPattern::Value(value) => {
                ctx.ast_node("MatchValue", (value.emit_py(ctx)?,), &self.tl_span)?
            }
            PyPattern::Singleton(value) => {
                let arg = match &value {
                    PyLiteral::Bool(b) => PyBool::new(ctx.py, *b).as_any().clone().unbind(),
                    PyLiteral::None => PyNone::get(ctx.py).as_any().clone().unbind(),
                    _ => {
                        return Err(PyTlErr {
                            message: format!("Unsupported singleton literal: {:?}", value),
                            py_err: None,
                            span: Some(self.tl_span),
                        });
                    }
                };

                ctx.ast_node("MatchSingleton", (arg,), &self.tl_span)?
            }
            PyPattern::Class(cls, items, kws) => {
                let mut items_ast = Vec::new();
                for item in items {
                    items_ast.push(item.emit_py(ctx)?);
                }
                let mut keys = vec![];
                let mut values = vec![];
                for (key, value) in kws {
                    keys.push(key);
                    values.push(value.emit_py(ctx)?);
                }
                ctx.ast_node(
                    "MatchClass",
                    (cls.emit_py(&ctx)?, items_ast, keys, values),
                    &self.tl_span,
                )?
            }
            PyPattern::Sequence(items) => {
                let mut items_ast = Vec::new();
                for item in items {
                    match item {
                        PyPatternSequenceItem::Item(item) => {
                            items_ast.push(item.emit_py(ctx)?);
                        }
                        PyPatternSequenceItem::Spread(name) => {
                            items_ast.push(ctx.ast_node("MatchStar", (name,), &self.tl_span)?);
                        }
                    }
                }
                ctx.ast_node("MatchSequence", (items_ast,), &self.tl_span)?
            }
            PyPattern::Mapping(items, rest) => {
                let mut keys = vec![];
                let mut values = vec![];

                for (key, value) in items {
                    keys.push(key.emit_py(ctx)?);
                    values.push(value.emit_py(ctx)?);
                }

                ctx.ast_node("MatchMapping", (keys, values, rest), &self.tl_span)?
            }
            PyPattern::Or(patterns) => {
                let mut patterns_ast = Vec::new();
                for pattern in patterns {
                    patterns_ast.push(pattern.emit_py(ctx)?);
                }
                ctx.ast_node("MatchOr", (patterns_ast,), &self.tl_span)?
            }
        })
    }
}

trait PyStmtExt<'src> {
    fn emit_py<'py>(&self, ctx: &PyCtx<'py, 'src>) -> PyTlResult<PyObject>;
}

impl<'src> PyStmtExt<'src> for SPyStmt<'src> {
    fn emit_py<'py>(&self, ctx: &PyCtx<'py, 'src>) -> PyTlResult<PyObject> {
        match &self.value {
            PyStmt::With(bindings, body) => {
                let mut context_exprs = Vec::new();
                for (binding, alias) in bindings {
                    let binding_ast = binding.emit_py(ctx)?;
                    context_exprs.push(ctx.ast_node(
                        "withitem",
                        (
                            binding_ast,
                            alias.as_ref().map(|x| x.emit_py(ctx)).transpose()?,
                        ),
                        &self.tl_span,
                    )?)
                }
                let body_ast = body.emit_py(ctx)?;
                ctx.ast_node("With", (context_exprs, body_ast), &self.tl_span)
            }
            PyStmt::Expr(expr) => ctx.ast_node("Expr", (expr.emit_py(ctx)?,), &self.tl_span),
            PyStmt::If(cond, then_block, else_block) => {
                let cond_ast = cond.emit_py(ctx)?;
                let then_ast = then_block.emit_py(ctx)?;
                let else_ast = else_block
                    .as_ref()
                    .map(|b| b.emit_py(ctx))
                    .transpose()?
                    .unwrap_or_else(|| PyList::empty(ctx.py).unbind());
                ctx.ast_node("If", (cond_ast, then_ast, else_ast), &self.tl_span)
            }
            PyStmt::Match(subject, cases) => {
                let subject_ast = subject.emit_py(ctx)?;
                let cases_ast: Result<Vec<_>, _> = cases
                    .iter()
                    .map(|case| {
                        let pattern_ast = case.pattern.emit_py(ctx)?;
                        let guard_ast = case.guard.as_ref().map(|x| x.emit_py(ctx)).transpose()?;
                        let body_ast = case.body.emit_py(ctx)?;
                        ctx.ast_cls("match_case", (pattern_ast, guard_ast, body_ast))
                    })
                    .collect();
                ctx.ast_node("Match", (subject_ast, cases_ast?), &self.tl_span)
            }
            PyStmt::Assign(target, value, op) => {
                let target_ast = target.emit_py(ctx)?;
                let value_ast = value.emit_py(ctx)?;

                if let Some(op) = op {
                    let py_op = match op {
                        PyBinaryOp::Add => "Add",
                        PyBinaryOp::Sub => "Sub",
                        PyBinaryOp::Mult => "Mult",
                        PyBinaryOp::Div => "Div",
                        _ => {
                            return Err(PyTlErr {
                                message: format!(
                                    "Unsupported augmented assignment operator: {:?}",
                                    op
                                ),
                                py_err: None,
                                span: Some(self.tl_span),
                            });
                        }
                    };

                    ctx.ast_node(
                        "AugAssign",
                        (target_ast, ctx.ast_cls(py_op, ())?, value_ast),
                        &self.tl_span,
                    )
                } else {
                    ctx.ast_node("Assign", ([target_ast], value_ast), &self.tl_span)
                }
            }
            PyStmt::Return(expr) => {
                let expr_ast = expr.emit_py(ctx)?;
                ctx.ast_node("Return", (expr_ast,), &self.tl_span)
            }
            PyStmt::Raise(expr) => {
                let expr_ast = expr.as_ref().map(|e| e.emit_py(ctx)).transpose()?;
                ctx.ast_node("Raise", (expr_ast,), &self.tl_span)
            }
            PyStmt::Assert(test, msg) => {
                let test_ast = test.emit_py(ctx)?;
                let msg_ast = msg.as_ref().map(|m| m.emit_py(ctx)).transpose()?;
                ctx.ast_node("Assert", (test_ast, msg_ast), &self.tl_span)
            }
            PyStmt::Global(names) => {
                let names_list: Vec<&str> = names.iter().map(|n| n.as_ref()).collect();
                ctx.ast_node("Global", (names_list,), &self.tl_span)
            }
            PyStmt::Nonlocal(names) => {
                let names_list: Vec<&str> = names.iter().map(|n| n.as_ref()).collect();
                ctx.ast_node("Nonlocal", (names_list,), &self.tl_span)
            }
            PyStmt::Del(exprs) => {
                let py_exprs = exprs
                    .iter()
                    .map(|e| e.emit_py(ctx))
                    .collect::<PyTlResult<Vec<_>>>()?;

                ctx.ast_node("Delete", (py_exprs,), &self.tl_span)
            }
            PyStmt::Import(aliases) => {
                let py_aliases = aliases
                    .iter()
                    .map(|alias| ctx.ast_cls("alias", (&alias.name, alias.as_name.as_deref())))
                    .collect::<PyTlResult<Vec<_>>>()?;

                ctx.ast_node("Import", (py_aliases,), &self.tl_span)
            }
            PyStmt::ImportFrom(module, items, level) => {
                let py_aliases = items
                    .iter()
                    .map(|item| ctx.ast_cls("alias", (&item.name, item.as_name.as_deref())))
                    .collect::<PyTlResult<Vec<_>>>()?;

                ctx.ast_node(
                    "ImportFrom",
                    (module.as_ref(), py_aliases, level),
                    &self.tl_span,
                )
            }
            PyStmt::FnDef(fndef) => {
                let arguments = fndef.args.emit_py(ctx)?;
                let body_ast = fndef.body.emit_py(ctx)?;
                let decorators = fndef.decorators.emit_py(ctx)?;

                if fndef.async_ {
                    ctx.ast_node(
                        "AsyncFunctionDef",
                        (fndef.name.as_ref(), arguments, body_ast, decorators),
                        &self.tl_span,
                    )
                } else {
                    ctx.ast_node(
                        "FunctionDef",
                        (fndef.name.as_ref(), arguments, body_ast, decorators),
                        &self.tl_span,
                    )
                }
            }
            PyStmt::ClassDef(PyClassDef {
                name,
                bases,
                body,
                decorators,
            }) => {
                let mut bases_ast = Vec::new();
                let mut keywords_ast = Vec::new();

                for base in bases {
                    match base {
                        PyCallItem::Arg(expr) => {
                            bases_ast.push(expr.emit_py(ctx)?);
                        }
                        PyCallItem::Kwarg(kw_name, expr) => {
                            let keyword =
                                ctx.ast_cls("keyword", (kw_name.as_ref(), expr.emit_py(ctx)?))?;
                            keywords_ast.push(keyword);
                        }
                        _ => {
                            return Err(PyTlErr {
                                message: "Unsupported call item in class definition".to_string(),
                                py_err: None,
                                span: Some(self.tl_span),
                            });
                        }
                    }
                }

                let body_ast = body.emit_py(ctx)?;
                let decorators = decorators.emit_py(ctx)?;
                ctx.ast_node(
                    "ClassDef",
                    (name.as_ref(), bases_ast, keywords_ast, body_ast, decorators),
                    &self.tl_span,
                )
            }
            PyStmt::While(cond, body) => {
                let cond_ast = cond.emit_py(ctx)?;
                let body_ast = body.emit_py(ctx)?;
                let orelse = Vec::<PyObject>::new();
                ctx.ast_node("While", (cond_ast, body_ast, orelse), &self.tl_span)
            }
            PyStmt::For(target, iter, body) => {
                let target_ast = target.emit_py(ctx)?;
                let iter_ast = iter.emit_py(ctx)?;
                let body_ast = body.emit_py(ctx)?;
                let orelse = Vec::<PyObject>::new();
                ctx.ast_node(
                    "For",
                    (target_ast, iter_ast, body_ast, orelse),
                    &self.tl_span,
                )
            }
            PyStmt::Try(body, handlers, finally_block) => {
                let body_ast = body.emit_py(ctx)?;
                let handlers_ast: Result<Vec<_>, _> = handlers
                    .iter()
                    .map(|handler| {
                        let typ_ast = if let Some(typ) = &handler.typ {
                            typ.emit_py(ctx)?
                        } else {
                            PySpanned::from((
                                PyExpr::Name("Exception".into(), PyAccessCtx::Load),
                                self.tl_span,
                            ))
                            .emit_py(ctx)?
                        };

                        let name_ast: Option<String> = handler.name.as_deref().map(Into::into);
                        let handler_body_ast = handler.body.emit_py(ctx)?;
                        ctx.ast_cls("ExceptHandler", (typ_ast, name_ast, handler_body_ast))
                    })
                    .collect();

                let finally_ast = finally_block
                    .as_ref()
                    .map(|f| f.emit_py(ctx))
                    .transpose()?
                    .unwrap_or(PyList::empty(ctx.py).unbind());

                ctx.ast_node(
                    "Try",
                    (body_ast, handlers_ast?, PyList::empty(ctx.py), finally_ast),
                    &self.tl_span,
                )
            }
            PyStmt::Pass => ctx.ast_node("Pass", (), &self.tl_span),
            PyStmt::Break => ctx.ast_node("Break", (), &self.tl_span),
            PyStmt::Continue => ctx.ast_node("Continue", (), &self.tl_span),
        }
    }
}

trait PyNameCtxExt {
    fn emit_py(&self, ctx: &PyCtx<'_, '_>) -> PyTlResult<PyObject>;
}

impl PyNameCtxExt for PyAccessCtx {
    fn emit_py(&self, ctx: &PyCtx<'_, '_>) -> PyTlResult<PyObject> {
        Ok(ctx.ast_cls(
            match self {
                PyAccessCtx::Load => "Load",
                PyAccessCtx::Store => "Store",
                PyAccessCtx::Del => "Del",
            },
            (),
        )?)
    }
}

trait PyLiteralExt<'src> {
    fn emit_py<'py>(&self, ctx: &PyCtx<'py, 'src>, span: &Span) -> PyTlResult<PyObject>;
}

impl<'src> PyLiteralExt<'src> for PyLiteral<'src> {
    fn emit_py<'py>(&self, ctx: &PyCtx<'py, 'src>, span: &Span) -> PyTlResult<PyObject> {
        Ok(match self {
            PyLiteral::Int(num) => {
                let num = num.replace('_', "");
                match num.parse::<i128>() {
                    Ok(i) => ctx.ast_node("Constant", (i,), span)?,
                    Err(_) => {
                        return Err(PyTlErr {
                            message: format!("Invalid integer literal: {}", num),
                            py_err: None,
                            span: Some(*span),
                        });
                    }
                }
            }
            PyLiteral::IntBin(num) => {
                let num = num.replace('_', "");
                // Keep the 0b prefix for Python compatibility
                match i128::from_str_radix(&num[2..], 2) {
                    Ok(i) => ctx.ast_node("Constant", (i,), span)?,
                    Err(_) => {
                        return Err(PyTlErr {
                            message: format!("Invalid binary literal: {}", num),
                            py_err: None,
                            span: Some(*span),
                        });
                    }
                }
            }
            PyLiteral::IntOct(num) => {
                let num = num.replace('_', "");
                // Keep the 0o prefix for Python compatibility
                match i128::from_str_radix(&num[2..], 8) {
                    Ok(i) => ctx.ast_node("Constant", (i,), span)?,
                    Err(_) => {
                        return Err(PyTlErr {
                            message: format!("Invalid octal literal: {}", num),
                            py_err: None,
                            span: Some(*span),
                        });
                    }
                }
            }
            PyLiteral::IntHex(num) => {
                let num = num.replace('_', "");
                // Keep the 0x prefix for Python compatibility
                match i128::from_str_radix(&num[2..], 16) {
                    Ok(i) => ctx.ast_node("Constant", (i,), span)?,
                    Err(_) => {
                        return Err(PyTlErr {
                            message: format!("Invalid hexadecimal literal: {}", num),
                            py_err: None,
                            span: Some(*span),
                        });
                    }
                }
            }
            PyLiteral::Float(num) => {
                let num = num.replace('_', "");
                match num.parse::<f64>() {
                    Ok(f) => ctx.ast_node("Constant", (f,), span)?,
                    Err(_) => {
                        return Err(PyTlErr {
                            message: format!("Invalid float literal: {}", num),
                            py_err: None,
                            span: Some(*span),
                        });
                    }
                }
            }
            PyLiteral::Bool(b) => ctx.ast_node("Constant", (b,), span)?,
            PyLiteral::Str(s) => ctx.ast_node("Constant", (s,), span)?,
            PyLiteral::None => ctx.ast_node("Constant", (ctx.py.None(),), span)?,
        })
    }
}

trait PyExprExt<'src> {
    fn emit_py<'py>(&self, ctx: &PyCtx<'py, 'src>) -> PyTlResult<PyObject>;
}

impl<'src> PyExprExt<'src> for SPyExpr<'src> {
    fn emit_py<'py>(&self, ctx: &PyCtx<'py, 'src>) -> PyTlResult<PyObject> {
        Ok(match &self.value {
            PyExpr::Literal(lit) => lit.emit_py(ctx, &self.tl_span)?,
            PyExpr::Name(ident, c) => {
                ctx.ast_node("Name", (ident, c.emit_py(ctx)?), &self.tl_span)?
            }
            PyExpr::Binary(op, left, right) => {
                let py_bin_op = match op {
                    PyBinaryOp::Add => Some("Add"),
                    PyBinaryOp::Sub => Some("Sub"),
                    PyBinaryOp::Mult => Some("Mult"),
                    PyBinaryOp::Div => Some("Div"),
                    PyBinaryOp::FloorDiv => Some("FloorDiv"),
                    PyBinaryOp::Mod => Some("Mod"),
                    PyBinaryOp::Pow => Some("Pow"),
                    _ => None,
                };

                if let Some(py_bin_op) = py_bin_op {
                    return ctx.ast_node(
                        "BinOp",
                        (
                            left.emit_py(ctx)?,
                            ctx.ast_cls(py_bin_op, ())?,
                            right.emit_py(ctx)?,
                        ),
                        &self.tl_span,
                    );
                }

                let py_bool_op = match op {
                    PyBinaryOp::And => Some("And"),
                    PyBinaryOp::Or => Some("Or"),
                    _ => None,
                };

                if let Some(py_bool_op) = py_bool_op {
                    return ctx.ast_node(
                        "BoolOp",
                        (
                            ctx.ast_cls(py_bool_op, ())?,
                            [left.emit_py(ctx)?, right.emit_py(ctx)?],
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
                    PyBinaryOp::In => Some("In"),
                    PyBinaryOp::Nin => Some("NotIn"),
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
            PyExpr::Fstr(parts) => {
                let mut values = Vec::new();
                for part in parts {
                    match part {
                        PyFstrPart::Str(s) => {
                            values.push(ctx.ast_node("Constant", (s.as_ref(),), &self.tl_span)?);
                        }
                        PyFstrPart::Expr(expr, _format_spec) => {
                            let expr_ast = expr.emit_py(ctx)?;
                            values.push(ctx.ast_node(
                                "FormattedValue",
                                (expr_ast, -1, ctx.py.None()),
                                &self.tl_span,
                            )?);
                        }
                    }
                }
                ctx.ast_node("JoinedStr", (values,), &self.tl_span)?
            }
            PyExpr::Unary(op, expr) => {
                let expr_ast = expr.emit_py(ctx)?;
                let op_str = match op {
                    PyUnaryOp::Not => "Not",
                    PyUnaryOp::Neg => "USub",
                    PyUnaryOp::Pos => "UAdd",
                    PyUnaryOp::Inv => "Invert",
                };
                ctx.ast_node(
                    "UnaryOp",
                    (ctx.ast_cls(op_str, ())?, expr_ast),
                    &self.tl_span,
                )?
            }
            PyExpr::Call(func, args) => {
                let func_ast = func.emit_py(ctx)?;
                let mut py_args = Vec::new();
                let mut py_keywords = Vec::new();

                for arg in args {
                    match arg {
                        PyCallItem::Arg(expr) => {
                            py_args.push(expr.emit_py(ctx)?);
                        }
                        PyCallItem::Kwarg(name, expr) => {
                            let keyword =
                                ctx.ast_cls("keyword", (name.as_ref(), expr.emit_py(ctx)?))?;
                            py_keywords.push(keyword);
                        }
                        PyCallItem::ArgSpread(expr) => {
                            let starred = ctx.ast_node(
                                "Starred",
                                (expr.emit_py(ctx)?, ctx.ast_cls("Load", ())?),
                                &self.tl_span,
                            )?;
                            py_args.push(starred);
                        }
                        PyCallItem::KwargSpread(expr) => {
                            let keyword =
                                ctx.ast_cls("keyword", (ctx.py.None(), expr.emit_py(ctx)?))?;
                            py_keywords.push(keyword);
                        }
                    }
                }

                ctx.ast_node("Call", (func_ast, py_args, py_keywords), &self.tl_span)?
            }
            PyExpr::Attribute(obj, attr, ctx_) => {
                let obj_ast = obj.emit_py(ctx)?;
                ctx.ast_node(
                    "Attribute",
                    (obj_ast, attr.as_ref(), ctx_.emit_py(ctx)?),
                    &self.tl_span,
                )?
            }
            PyExpr::Subscript(obj, index, ctx_) => {
                let obj_ast = obj.emit_py(ctx)?;
                let index_ast = index.emit_py(ctx)?;
                ctx.ast_node(
                    "Subscript",
                    (obj_ast, index_ast, ctx_.emit_py(ctx)?),
                    &self.tl_span,
                )?
            }
            PyExpr::List(items, access) => {
                let mut elts = Vec::new();
                for item in items {
                    match item {
                        PyListItem::Item(expr) => {
                            elts.push(expr.emit_py(ctx)?);
                        }
                        PyListItem::Spread(expr) => {
                            let starred = ctx.ast_node(
                                "Starred",
                                (expr.emit_py(ctx)?, access.emit_py(ctx)?),
                                &self.tl_span,
                            )?;
                            elts.push(starred);
                        }
                    }
                }
                ctx.ast_node("List", (elts, access.emit_py(ctx)?), &self.tl_span)?
            }
            PyExpr::Tuple(items, access) => {
                let mut elts = Vec::new();
                for item in items {
                    match item {
                        PyListItem::Item(expr) => {
                            elts.push(expr.emit_py(ctx)?);
                        }
                        PyListItem::Spread(expr) => {
                            let starred = ctx.ast_node(
                                "Starred",
                                (expr.emit_py(ctx)?, access.emit_py(ctx)?),
                                &self.tl_span,
                            )?;
                            elts.push(starred);
                        }
                    }
                }
                ctx.ast_node("Tuple", (elts, access.emit_py(ctx)?), &self.tl_span)?
            }
            PyExpr::Dict(items) => {
                let mut keys = Vec::new();
                let mut values = Vec::new();
                for item in items {
                    match item {
                        PyDictItem::Item(key, value) => {
                            keys.push(key.emit_py(ctx)?);
                            values.push(value.emit_py(ctx)?);
                        }
                        PyDictItem::Spread(expr) => {
                            keys.push(ctx.py.None().into());
                            values.push(expr.emit_py(ctx)?);
                        }
                    }
                }
                ctx.ast_node("Dict", (keys, values), &self.tl_span)?
            }
            PyExpr::Slice(start, stop, step) => {
                let start_ast = start.as_ref().map(|e| e.emit_py(ctx)).transpose()?;
                let stop_ast = stop.as_ref().map(|e| e.emit_py(ctx)).transpose()?;
                let step_ast = step.as_ref().map(|e| e.emit_py(ctx)).transpose()?;
                ctx.ast_node("Slice", (start_ast, stop_ast, step_ast), &self.tl_span)?
            }
            PyExpr::Await(expr) => {
                let expr_ast = expr.emit_py(ctx)?;
                ctx.ast_node("Await", (expr_ast,), &self.tl_span)?
            }
            PyExpr::Yield(expr) => {
                let expr_ast = expr.emit_py(ctx)?;
                ctx.ast_node("Yield", (expr_ast,), &self.tl_span)?
            }
            PyExpr::YieldFrom(expr) => {
                let expr_ast = expr.emit_py(ctx)?;
                ctx.ast_node("YieldFrom", (expr_ast,), &self.tl_span)?
            }
            PyExpr::IfExpr(cond, if_, else_) => {
                let cond_ast = cond.emit_py(ctx)?;
                let if_ast = if_.emit_py(ctx)?;
                let else_ast = else_.emit_py(ctx)?;
                ctx.ast_node("IfExp", (cond_ast, if_ast, else_ast), &self.tl_span)?
            }
            PyExpr::Lambda(args, body) => ctx.ast_node(
                "Lambda",
                (args.emit_py(ctx)?, body.emit_py(ctx)?),
                &self.tl_span,
            )?,
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
