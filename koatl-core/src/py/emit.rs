use std::collections::HashMap;

use parser::{
    ast::Span,
    lexer::{escape_fstr, escape_str},
};

use crate::{py::ast::*, transform::TfResult};

const LOW_PREC: f32 = -100.0;
const HIGH_PREC: f32 = 100.0;

pub struct EmitCtx {
    pub indentation: usize,
    pub source: String,
    pub source_line_map: HashMap<usize, Span>,
    pub lineno: usize,
}

impl EmitCtx {
    pub fn new() -> Self {
        EmitCtx {
            lineno: 1,
            indentation: 0,
            source: String::new(),
            source_line_map: HashMap::new(),
        }
    }

    pub fn record_source_map(&mut self, span: Span) -> () {
        self.source_line_map.insert(self.lineno, span);
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

    pub fn emit_escaped_str(&mut self, text: &str) -> Span {
        let start = self.source.len();
        self.source.push_str(&escape_str(text));
        let end = self.source.len();

        Span {
            context: (),
            start,
            end,
        }
    }

    pub fn emit_escaped_fstr(&mut self, text: &str) -> Span {
        let start = self.source.len();
        self.source.push_str(&escape_fstr(text));
        let end = self.source.len();

        Span {
            context: (),
            start,
            end,
        }
    }

    fn emit_indent(&mut self) -> Span {
        self.emit(&"  ".repeat(self.indentation))
    }

    fn emit_endl(&mut self) -> Span {
        self.lineno += 1;
        self.emit("\n")
    }
}

impl PyImportAlias<'_> {
    fn emit_to(&mut self, ctx: &mut EmitCtx) -> () {
        if let Some(as_name) = &self.as_name {
            ctx.emit(&format!("{} as {}", self.name, as_name));
        } else {
            ctx.emit(&self.name);
        };
    }
}

impl PyCallItem<'_> {
    fn emit_to(&mut self, ctx: &mut EmitCtx) -> TfResult<()> {
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
    fn emit_to(&mut self, ctx: &mut EmitCtx) -> TfResult<()> {
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

    fn emit_to(&mut self, ctx: &mut EmitCtx) {
        ctx.emit(match self {
            PyUnaryOp::Not => "not ",
            PyUnaryOp::Neg => "-",
            PyUnaryOp::Pos => "+",
            PyUnaryOp::Inv => "~",
        });
    }
}

impl SPyPattern<'_> {
    pub fn emit_to(&mut self, ctx: &mut EmitCtx) -> TfResult<()> {
        let span_start = ctx.source.len();

        match &mut self.value {
            PyPattern::As(pattern, ident) => {
                if let Some(pattern) = pattern {
                    pattern.emit_to(ctx)?;
                } else {
                    ctx.emit("_");
                }
                if let Some(ident) = ident {
                    ctx.emit(" as ");
                    ctx.emit(ident);
                }
            }
            PyPattern::Or(patterns) => {
                ctx.emit("(");
                for (i, item) in patterns.iter_mut().enumerate() {
                    if i > 0 {
                        ctx.emit(" | ");
                    }
                    ctx.emit("(");
                    item.emit_to(ctx)?;
                    ctx.emit(")");
                }
                ctx.emit(")");
            }
            PyPattern::Sequence(items) => {
                ctx.emit("[");
                for (i, item) in items.iter_mut().enumerate() {
                    if i > 0 {
                        ctx.emit(", ");
                    }
                    match item {
                        PyPatternSequenceItem::Item(pat) => {
                            pat.emit_to(ctx)?;
                        }
                        PyPatternSequenceItem::Spread(ident) => {
                            ctx.emit("*");
                            ctx.emit(ident.as_ref().map(|x| x.as_ref()).unwrap_or("_"));
                        }
                    }
                }
                ctx.emit("]");
            }
            PyPattern::Mapping(items, spread) => {
                ctx.emit("{");
                for (key, value) in items {
                    key.emit_to(ctx, LOW_PREC)?;
                    ctx.emit(": ");
                    value.emit_to(ctx)?;
                    ctx.emit(", ");
                }
                if let Some(spread) = spread {
                    ctx.emit("**");
                    ctx.emit(spread);
                }
                ctx.emit("}");
            }
            PyPattern::Singleton(literal) => {
                literal.emit_to(ctx, 0.)?;
            }
            PyPattern::Value(v) => v.emit_to(ctx, 0.)?,
            PyPattern::Class(cls, items, kws) => {
                // never emit parens
                cls.emit_to(ctx, LOW_PREC)?;
                ctx.emit("(");
                for item in items {
                    item.emit_to(ctx)?;
                    ctx.emit(", ");
                }
                for (key, value) in kws {
                    ctx.emit(key);
                    ctx.emit("=");
                    value.emit_to(ctx)?;
                }
                ctx.emit(")");
            }
        };

        let span_end = ctx.source.len();

        self.py_span = Some(Span {
            context: (),
            start: span_start,
            end: span_end,
        });

        Ok(())
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
            PyBinaryOp::Mult | PyBinaryOp::Div | PyBinaryOp::Mod | PyBinaryOp::MatMult => 2.0,
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
            PyBinaryOp::MatMult => "@",
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

impl PyLiteral<'_> {
    pub fn emit_to(&mut self, ctx: &mut EmitCtx, prec: f32) -> TfResult<()> {
        match self {
            PyLiteral::Num(num) => {
                if prec > 15. {
                    ctx.emit("(");
                    ctx.emit(&num.to_string());
                    ctx.emit(")");
                } else {
                    ctx.emit(&num.to_string());
                }
            }
            PyLiteral::Str(s) => {
                ctx.emit("\"");
                ctx.emit_escaped_str(s);
                ctx.emit("\"");
            }
            PyLiteral::Bool(b) => {
                ctx.emit(if *b { "True" } else { "False" });
            }
            PyLiteral::None => {
                ctx.emit("None");
            }
        };

        Ok(())
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
            PyExpr::Ident(id, _ctx) => {
                ctx.emit(&id);
            }
            PyExpr::Tuple(items, _ctx) => {
                ctx.emit("(");
                for (i, item) in items.iter_mut().enumerate() {
                    if i > 0 {
                        ctx.emit(", ");
                    }
                    match item {
                        PyListItem::Item(expr) => expr.emit_to(ctx, LOW_PREC)?,
                        PyListItem::Spread(expr) => {
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
            PyExpr::List(items, _ctx) => {
                ctx.emit("[");
                for (i, item) in items.iter_mut().enumerate() {
                    if i > 0 {
                        ctx.emit(", ");
                    }
                    match item {
                        PyListItem::Item(expr) => expr.emit_to(ctx, LOW_PREC)?,
                        PyListItem::Spread(expr) => {
                            ctx.emit("*");
                            expr.emit_to(ctx, HIGH_PREC)?;
                        }
                    }
                }
                ctx.emit("]");
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

                let right_assoc = *op == PyBinaryOp::Pow;

                left.emit_sided_to(ctx, prec, true ^ right_assoc)?;
                ctx.emit(" ");
                op.emit_to(ctx);
                ctx.emit(" ");
                right.emit_sided_to(ctx, prec, false ^ right_assoc)?;
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
            PyExpr::Attribute(obj, attr, _ctx) => {
                let prec = 20.;
                set_prec(prec);

                obj.emit_to(ctx, prec)?;
                ctx.emit(".");
                ctx.emit(attr);
            }
            PyExpr::Subscript(obj, index, _ctx) => {
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
            PyExpr::Literal(literal) => literal.emit_to(ctx, parent_precendence)?,
            PyExpr::Fstr(fstr_parts) => {
                ctx.emit("f\"");
                for part in fstr_parts.iter_mut() {
                    match part {
                        PyFstrPart::Str(s) => {
                            ctx.emit_escaped_fstr(s);
                        }
                        PyFstrPart::Expr(expr, fmt) => {
                            ctx.emit("{");
                            expr.emit_sided_to(ctx, HIGH_PREC, true)?;
                            if let Some(fmt) = fmt {
                                ctx.emit(":");
                                ctx.emit(fmt);
                            }
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
            PyExpr::IfExpr(cond, if_, else_) => {
                // TODO precedence here?
                ctx.emit("(");
                if_.emit_to(ctx, LOW_PREC)?;
                ctx.emit(" if ");
                cond.emit_to(ctx, LOW_PREC)?;
                ctx.emit(" else ");
                else_.emit_to(ctx, LOW_PREC)?;
                ctx.emit(")");
            }
            PyExpr::Lambda(args, body) => {
                ctx.emit("(lambda ");
                ctx.emit("");
                for (i, arg) in args.iter_mut().enumerate() {
                    if i > 0 {
                        ctx.emit(", ");
                    }
                    arg.emit_to(ctx)?;
                }
                ctx.emit(": ");
                body.emit_to(ctx, HIGH_PREC)?;
                ctx.emit(")");
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
        ctx.record_source_map(self.tl_span);

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
                if let Some(expr) = expr {
                    expr.emit_to(ctx, LOW_PREC)?;
                }
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
            PyStmt::Del(exprs) => {
                ctx.emit_indent();
                ctx.emit("del ");
                for (i, expr) in exprs.iter_mut().enumerate() {
                    if i > 0 {
                        ctx.emit(", ");
                    }
                    expr.emit_to(ctx, LOW_PREC)?;
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
            PyStmt::Import(names) => {
                ctx.emit_indent();
                ctx.emit("import ");
                for (i, name) in names.iter_mut().enumerate() {
                    if i > 0 {
                        ctx.emit(", ");
                    }
                    name.emit_to(ctx);
                }
                ctx.emit_endl();
            }
            PyStmt::ImportFrom(module, aliases, level) => {
                ctx.emit_indent();
                ctx.emit("from ");
                ctx.emit(&".".repeat(*level));
                if let Some(module) = module {
                    ctx.emit(&module);
                }
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
            PyStmt::FnDef(name, args, body, decorators) => {
                for d in &mut decorators.0 {
                    ctx.emit_indent();
                    ctx.emit("@");
                    d.emit_to(ctx, HIGH_PREC)?;
                    ctx.emit_endl();
                }

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
            PyStmt::ClassDef(name, bases, body, decorators) => {
                for d in &mut decorators.0 {
                    ctx.emit_indent();
                    ctx.emit("@");
                    d.emit_to(ctx, HIGH_PREC)?;
                    ctx.emit_endl();
                }

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
                target.emit_to(ctx, LOW_PREC)?;
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
                    ctx.emit("except ");

                    if let Some(typ) = &mut handler.typ {
                        typ.emit_to(ctx, LOW_PREC)?;
                    } else {
                        ctx.emit("Exception");
                    }

                    if let Some(name) = &handler.name {
                        ctx.emit(" as ");
                        ctx.emit(name);
                    }

                    ctx.emit(":");
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
            PyStmt::Pass => {
                ctx.emit_indent();
                ctx.emit("pass");
                ctx.emit_endl();
            }
            PyStmt::Match(py_spanned, py_match_cases) => {
                ctx.emit_indent();
                ctx.emit("match ");
                py_spanned.emit_to(ctx, LOW_PREC)?;
                ctx.emit(":");
                ctx.emit_endl();
                ctx.indentation += 1;
                for case in py_match_cases {
                    ctx.emit_indent();
                    ctx.emit("case ");
                    case.pattern.emit_to(ctx)?;
                    if case.guard.is_some() {
                        ctx.emit(" if ");
                        case.guard.as_mut().unwrap().emit_to(ctx, LOW_PREC)?;
                    }
                    ctx.emit(":");
                    ctx.emit_endl();
                    case.body.emit_to(ctx, 1)?;
                }
                ctx.indentation -= 1;
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
            a.load_ident("x"),
            a.binary(PyBinaryOp::Add, a.load_ident("y"), a.load_ident("z")),
        );

        let mut ctx = EmitCtx::new();

        expr.emit_to(&mut ctx, LOW_PREC).unwrap();
        assert_eq!(ctx.source, "x * (y + z)");
    }
}
