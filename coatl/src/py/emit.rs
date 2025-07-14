use parser::ast::Span;

use crate::{py::ast::*, transform::TfResult};

const LOW_PREC: f32 = -1.0;
const HIGH_PREC: f32 = 100.0;

pub struct EmitCtx {
    pub indentation: usize,
    pub source: String,
}

impl EmitCtx {
    pub fn emit_spanned<T>(&mut self, value: &mut PySpanned<T>, text: &str) -> () {
        value.py_span = Some(self.emit(text));
    }

    pub fn emit(&mut self, text: &str) -> Span {
        let start = self.source.len();
        self.source.push_str(text);
        let end = self.source.len();

        Span {
            context: (),
            start,
            end,
        }
    }

    pub fn emit_indent(&mut self) -> Span {
        self.emit(&"  ".repeat(self.indentation))
    }

    pub fn emit_endl(&mut self) -> Span {
        self.emit("\n")
    }
}

impl PyImportAlias<'_> {
    pub fn emit_to(&mut self, ctx: &mut EmitCtx) -> () {
        if let Some(as_name) = &self.as_name {
            ctx.emit(&format!("{} as {}", self.name, as_name));
        } else {
            ctx.emit(&self.name);
        };
    }
}

impl PyCallItem<'_> {
    pub fn emit_to(&mut self, ctx: &mut EmitCtx) -> TfResult<()> {
        match self {
            PyCallItem::Arg(expr) => {
                expr.emit_to(ctx, LOW_PREC)?;
            }
            PyCallItem::Kwarg(name, expr) => {
                ctx.emit(name);
                ctx.emit("=");
                expr.emit_to(ctx, LOW_PREC)?;
            }
            PyCallItem::ArgSpread(expr) => {
                ctx.emit("*");
                expr.emit_to(ctx, HIGH_PREC)?;
            }
            PyCallItem::KwargSpread(expr) => {
                ctx.emit("**");
                expr.emit_to(ctx, HIGH_PREC)?;
            }
        }

        Ok(())
    }
}

impl PyArgDefItem<'_> {
    pub fn emit_to(&mut self, ctx: &mut EmitCtx) -> TfResult<()> {
        match self {
            PyArgDefItem::Arg(name, default) => {
                if let Some(default) = default {
                    ctx.emit(name);
                    ctx.emit("=");
                    default.emit_to(ctx, LOW_PREC)?;
                } else {
                    ctx.emit(name);
                }
            }
            PyArgDefItem::ArgSpread(name) => {
                ctx.emit(&format!("*{}", name));
            }
            PyArgDefItem::KwargSpread(name) => {
                ctx.emit(&format!("**{}", name));
            }
        }

        Ok(())
    }
}

impl PyUnaryOp {
    pub fn precedence(&self) -> f32 {
        match self {
            PyUnaryOp::Not => 0.0,
            PyUnaryOp::Neg | PyUnaryOp::Pos | PyUnaryOp::Inv => 4.0,
        }
    }

    pub fn emit_to(&mut self, ctx: &mut EmitCtx) {
        ctx.emit(match self {
            PyUnaryOp::Not => "not ",
            PyUnaryOp::Neg => "-",
            PyUnaryOp::Pos => "+",
            PyUnaryOp::Inv => "~",
        });
    }
}

impl PyBinaryOp {
    pub fn precedence(&self) -> f32 {
        match self {
            PyBinaryOp::Eq
            | PyBinaryOp::Neq
            | PyBinaryOp::Lt
            | PyBinaryOp::Leq
            | PyBinaryOp::Gt
            | PyBinaryOp::Geq
            | PyBinaryOp::Is
            | PyBinaryOp::Nis
            | PyBinaryOp::And
            | PyBinaryOp::Or => 0.0,
            PyBinaryOp::Add | PyBinaryOp::Sub => 1.0,
            PyBinaryOp::Mult | PyBinaryOp::Div | PyBinaryOp::Mod => 2.0,
            PyBinaryOp::Pow => 3.0,
        }
    }

    pub fn emit_to(&mut self, ctx: &mut EmitCtx) {
        ctx.emit(match self {
            PyBinaryOp::Add => "+",
            PyBinaryOp::Sub => "-",
            PyBinaryOp::Mult => "*",
            PyBinaryOp::Div => "/",
            PyBinaryOp::Mod => "%",
            PyBinaryOp::Pow => "**",
            PyBinaryOp::Eq => "==",
            PyBinaryOp::Neq => "!=",
            PyBinaryOp::Lt => "<",
            PyBinaryOp::Leq => "<=",
            PyBinaryOp::Gt => ">",
            PyBinaryOp::Geq => ">=",
            PyBinaryOp::Is => "is",
            PyBinaryOp::Nis => "is not",
            PyBinaryOp::And => "and",
            PyBinaryOp::Or => "or",
        });
    }
}

impl SPyExpr<'_> {
    pub fn emit_to(&mut self, ctx: &mut EmitCtx, prec: f32) -> TfResult<()> {
        self.emit_sided_to(ctx, prec, true)
    }

    pub fn emit_sided_to(
        &mut self,
        ctx: &mut EmitCtx,
        parent_precendence: f32,
        lhs: bool,
    ) -> TfResult<()> {
        let mut require_paren = false;
        let span_start = ctx.source.len();

        let mut set_prec = |prec| {
            if lhs && prec < parent_precendence {
                require_paren = true;
            }

            if !lhs && prec <= parent_precendence {
                require_paren = true;
            }

            if require_paren {
                ctx.emit("(");
            }
        };

        match &mut self.value {
            PyExpr::Name(id) => {
                ctx.emit(&id);
            }
            PyExpr::Tuple(items) => {
                ctx.emit("(");
                for (i, item) in items.iter_mut().enumerate() {
                    if i > 0 {
                        ctx.emit(", ");
                    }
                    match item {
                        PyTupleItem::Item(expr) => expr.emit_to(ctx, LOW_PREC)?,
                        PyTupleItem::Spread(expr) => {
                            ctx.emit("*");
                            expr.emit_to(ctx, HIGH_PREC)?;
                        }
                    }
                }
                if items.len() == 1 {
                    ctx.emit(",");
                }
                ctx.emit(")");
            }
            PyExpr::Dict(items) => {
                ctx.emit("{");
                for (i, item) in items.iter_mut().enumerate() {
                    if i > 0 {
                        ctx.emit(", ");
                    }
                    match item {
                        PyDictItem::Item(key, value) => {
                            key.emit_to(ctx, HIGH_PREC)?;
                            ctx.emit(": ");
                            value.emit_to(ctx, LOW_PREC)?;
                        }
                        PyDictItem::Spread(expr) => {
                            ctx.emit("**");
                            expr.emit_to(ctx, HIGH_PREC)?;
                        }
                    }
                }
                ctx.emit("}");
            }
            PyExpr::Binary(op, left, right) => {
                let prec = op.precedence();
                set_prec(prec);

                left.emit_sided_to(ctx, prec, true)?;
                ctx.emit(" ");
                op.emit_to(ctx);
                ctx.emit(" ");
                right.emit_sided_to(ctx, prec, false)?;
            }
            PyExpr::Unary(op, expr) => {
                let prec = op.precedence();
                set_prec(prec);

                op.emit_to(ctx);
                expr.emit_to(ctx, prec)?;
            }
            PyExpr::Call(func, args) => {
                let prec = 20.;
                set_prec(prec);

                func.emit_to(ctx, prec)?;
                ctx.emit("(");
                for (i, arg) in args.iter_mut().enumerate() {
                    if i > 0 {
                        ctx.emit(", ");
                    }
                    arg.emit_to(ctx)?;
                }
                ctx.emit(")");
            }
            PyExpr::Attribute(obj, attr) => {
                let prec = 20.;
                set_prec(prec);

                obj.emit_to(ctx, prec)?;
                ctx.emit(".");
                ctx.emit(attr);
            }
            PyExpr::Subscript(obj, index) => {
                let prec = 20.;
                set_prec(prec);

                obj.emit_to(ctx, prec)?;
                ctx.emit("[");
                index.emit_to(ctx, LOW_PREC)?;
                ctx.emit("]");
            }
            PyExpr::Yield(expr) => {
                set_prec(-0.5);
                ctx.emit("yield ");
                expr.emit_to(ctx, LOW_PREC)?;
            }
            PyExpr::YieldFrom(expr) => {
                set_prec(-0.5); // TODO is this precedence correct?
                ctx.emit("yield from ");
                expr.emit_to(ctx, LOW_PREC)?;
            }
            PyExpr::Literal(literal) => match literal {
                PyLiteral::Num(num) => {
                    set_prec(15.); // lower than attribute
                    ctx.emit(&num.to_string());
                }
                PyLiteral::Str(s) => {
                    ctx.emit("\"");
                    ctx.emit(s.as_ref());
                    ctx.emit("\"");
                }
                PyLiteral::Bool(b) => {
                    ctx.emit(if *b { "True" } else { "False" });
                }
                PyLiteral::None => {
                    ctx.emit("None");
                }
            },
            PyExpr::Fstr(fstr_parts) => {
                ctx.emit("f\"");
                for part in fstr_parts.iter_mut() {
                    match part {
                        PyFstrPart::Str(s) => {
                            ctx.emit(s);
                        }
                        PyFstrPart::Expr(expr, _ident) => {
                            ctx.emit("{");
                            expr.emit_sided_to(ctx, HIGH_PREC, true)?;
                            ctx.emit("}");
                        }
                    }
                }
                ctx.emit("\"");
            }
            PyExpr::Slice(start, stop, step) => {
                if let Some(start) = start {
                    start.emit_to(ctx, LOW_PREC)?;
                }
                ctx.emit(":");
                if let Some(stop) = stop {
                    stop.emit_to(ctx, LOW_PREC)?;
                }
                if let Some(step) = step {
                    ctx.emit(":");
                    step.emit_to(ctx, LOW_PREC)?;
                }
            }
        };

        if require_paren {
            ctx.emit(")");
        }

        let span_end = ctx.source.len();

        self.py_span = Some(Span {
            context: (),
            start: span_start,
            end: span_end,
        });

        Ok(())
    }
}

impl PyBlock<'_> {
    pub fn emit_to(&mut self, ctx: &mut EmitCtx, delta_indentation: i32) -> TfResult<()> {
        let old_indentation = ctx.indentation;
        ctx.indentation = (ctx.indentation as i32 + delta_indentation) as usize;

        for stmt in self.0.iter_mut() {
            stmt.emit_to(ctx)?;
        }

        ctx.indentation = old_indentation;
        Ok(())
    }
}

impl SPyStmt<'_> {
    pub fn emit_to(&mut self, ctx: &mut EmitCtx) -> TfResult<()> {
        let start_span = ctx.source.len();

        match &mut self.value {
            PyStmt::Expr(expr) => {
                ctx.emit_indent();
                expr.emit_to(ctx, LOW_PREC)?;
                ctx.emit_endl();
            }
            PyStmt::Assign(target, value) => {
                ctx.emit_indent();
                target.emit_to(ctx, LOW_PREC)?;
                ctx.emit(" = ");
                value.emit_to(ctx, LOW_PREC)?;
                ctx.emit_endl();
            }
            PyStmt::Return(expr) => {
                ctx.emit_indent();
                ctx.emit("return ");
                expr.emit_to(ctx, LOW_PREC)?;
                ctx.emit_endl();
            }
            PyStmt::If(cond, body, orelse) => {
                ctx.emit_indent();
                ctx.emit("if ");
                cond.emit_to(ctx, LOW_PREC)?;
                ctx.emit(":");
                ctx.emit_endl();
                body.emit_to(ctx, 1)?;
                if let Some(orelse) = orelse {
                    ctx.emit_indent();
                    ctx.emit("else:");
                    ctx.emit_endl();
                    orelse.emit_to(ctx, 1)?;
                }
            }
            PyStmt::Raise(expr) => {
                ctx.emit_indent();
                ctx.emit("raise ");
                expr.emit_to(ctx, LOW_PREC)?;
                ctx.emit_endl();
            }
            PyStmt::Assert(expr, msg) => {
                ctx.emit_indent();
                ctx.emit("assert ");
                expr.emit_to(ctx, LOW_PREC)?;
                if let Some(msg) = msg {
                    ctx.emit(", ");
                    msg.emit_to(ctx, LOW_PREC)?;
                }
                ctx.emit_endl();
            }
            PyStmt::Global(names) => {
                ctx.emit_indent();
                ctx.emit("global ");
                for (i, name) in names.iter().enumerate() {
                    if i > 0 {
                        ctx.emit(", ");
                    }
                    ctx.emit(name);
                }
                ctx.emit_endl();
            }
            PyStmt::Nonlocal(names) => {
                ctx.emit_indent();
                ctx.emit("nonlocal ");
                for (i, name) in names.iter().enumerate() {
                    if i > 0 {
                        ctx.emit(", ");
                    }
                    ctx.emit(name);
                }
                ctx.emit_endl();
            }
            PyStmt::Import(name) => {
                ctx.emit_indent();
                ctx.emit("import ");
                name.emit_to(ctx);
                ctx.emit_endl();
            }
            PyStmt::ImportFrom(module, aliases) => {
                ctx.emit_indent();
                ctx.emit("from ");
                ctx.emit(&module);
                ctx.emit(" import ");
                if aliases.is_empty() {
                    ctx.emit("*");
                } else {
                    for (i, alias) in aliases.iter_mut().enumerate() {
                        if i > 0 {
                            ctx.emit(", ");
                        }
                        alias.emit_to(ctx);
                    }
                }
                ctx.emit_endl();
            }
            PyStmt::FnDef(name, args, body) => {
                ctx.emit_indent();
                ctx.emit("def ");
                ctx.emit(&name);
                ctx.emit("(");
                for (i, arg) in args.iter_mut().enumerate() {
                    if i > 0 {
                        ctx.emit(", ");
                    }
                    arg.emit_to(ctx)?;
                }
                ctx.emit("):");
                ctx.emit_endl();
                body.emit_to(ctx, 1)?;
            }
            PyStmt::ClassDef(name, bases, body) => {
                ctx.emit_indent();
                ctx.emit("class ");
                ctx.emit(&name);
                ctx.emit("(");
                for (i, base) in bases.iter_mut().enumerate() {
                    if i > 0 {
                        ctx.emit(", ");
                    }
                    base.emit_to(ctx)?;
                }
                ctx.emit("):");
                ctx.emit_endl();
                body.emit_to(ctx, 1)?;
            }
            PyStmt::While(cond, body) => {
                ctx.emit_indent();
                ctx.emit("while ");
                cond.emit_to(ctx, LOW_PREC)?;
                ctx.emit(":");
                ctx.emit_endl();
                body.emit_to(ctx, 1)?;
            }
            PyStmt::For(target, iter, body) => {
                ctx.emit_indent();
                ctx.emit("for ");
                ctx.emit(&target);
                ctx.emit(" in ");
                iter.emit_to(ctx, LOW_PREC)?;
                ctx.emit(":");
                ctx.emit_endl();
                body.emit_to(ctx, 1)?;
            }
            PyStmt::Try(body, handlers, finally) => {
                ctx.emit_indent();
                ctx.emit("try:");
                ctx.emit_endl();
                body.emit_to(ctx, 1)?;
                for handler in handlers {
                    ctx.emit_indent();
                    ctx.emit("except:"); // TODO specific cases
                    ctx.emit_endl();
                    handler.body.emit_to(ctx, 1)?;
                }
                if let Some(finally) = finally {
                    ctx.emit_indent();
                    ctx.emit("finally:");
                    ctx.emit_endl();
                    finally.emit_to(ctx, 1)?;
                }
            }
            PyStmt::Break => {
                ctx.emit_indent();
                ctx.emit("break");
                ctx.emit_endl();
            }
            PyStmt::Continue => {
                ctx.emit_indent();
                ctx.emit("continue");
                ctx.emit_endl();
            }
            PyStmt::Match(py_spanned, py_match_cases) => {
                ctx.emit_indent();
                ctx.emit("match ");
                py_spanned.emit_to(ctx, LOW_PREC)?;
                ctx.emit(":");
                ctx.emit_endl();
                for case in py_match_cases {
                    ctx.emit_indent();
                    ctx.emit("case ");
                    case.pattern.emit_to(ctx, LOW_PREC)?;
                    ctx.emit(":");
                    ctx.emit_endl();
                    case.body.emit_to(ctx, 1)?;
                }
            }
        }

        let end_span = ctx.source.len();
        self.py_span = Some(Span {
            context: (),
            start: start_span,
            end: end_span,
        });

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use parser::ast::Span;

    use crate::py::util::PyAstBuilder;

    use super::*;

    const DUMMY_SPAN: Span = Span {
        context: (),
        start: 0,
        end: 0,
    };

    #[test]
    fn test_expr_to_source() {
        let a = PyAstBuilder::new(DUMMY_SPAN);

        let mut expr: SPyExpr = a.binary(
            PyBinaryOp::Mult,
            a.ident("x"),
            a.binary(PyBinaryOp::Add, a.ident("y"), a.ident("z")),
        );

        let mut ctx = EmitCtx {
            indentation: 0,
            source: String::new(),
        };

        expr.emit_to(&mut ctx, LOW_PREC);
        assert_eq!(ctx.source, "x * (y + z)");
    }
}
