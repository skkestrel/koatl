use crate::{py::ast::*, transform::TlResult};

const LOW_PREC: f32 = -1.0;
const HIGH_PREC: f32 = 100.0;

pub struct EmitCtx {
    pub indentation: usize,
}

impl EmitCtx {
    pub fn indent(&self, s: &str) -> String {
        format!("{}{}", "  ".repeat(self.indentation), s)
    }

    pub fn indent_endl(&self, s: &str) -> String {
        format!("{}{}\n", "  ".repeat(self.indentation), s)
    }
}

impl PyImportAlias<'_> {
    pub fn to_source(&self) -> String {
        if let Some(as_name) = &self.as_name {
            format!("{} as {}", self.name, as_name)
        } else {
            self.name.to_string()
        }
    }
}

impl PyCallItem<'_> {
    pub fn to_source(&self) -> TlResult<String> {
        match self {
            PyCallItem::Arg(expr) => expr.to_source_lhs(LOW_PREC),
            PyCallItem::Kwarg(name, expr) => {
                Ok(format!("{}={}", name, expr.to_source_lhs(LOW_PREC)?))
            }
            PyCallItem::ArgSpread(expr) => Ok(format!("*{}", expr.to_source_lhs(HIGH_PREC)?)),
            PyCallItem::KwargSpread(expr) => Ok(format!("**{}", expr.to_source_lhs(HIGH_PREC)?)),
        }
    }
}

impl PyArgDefItem<'_> {
    pub fn to_source(&self) -> TlResult<String> {
        match self {
            PyArgDefItem::Arg(name, default) => {
                if let Some(default) = default {
                    Ok(format!("{}={}", name, default.to_source_lhs(LOW_PREC)?))
                } else {
                    Ok(name.to_string())
                }
            }
            PyArgDefItem::ArgSpread(name) => Ok(format!("*{}", name)),
            PyArgDefItem::KwargSpread(name) => Ok(format!("**{}", name)),
        }
    }
}

impl PyUnaryOp {
    pub fn precedence(&self) -> f32 {
        match self {
            PyUnaryOp::Not => 0.0,
            PyUnaryOp::Neg | PyUnaryOp::Pos | PyUnaryOp::Inv => 4.0,
        }
    }

    pub fn to_source(&self) -> &'static str {
        match self {
            PyUnaryOp::Not => "not ",
            PyUnaryOp::Neg => "-",
            PyUnaryOp::Pos => "+",
            PyUnaryOp::Inv => "~",
        }
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

    pub fn to_source(&self) -> &'static str {
        match self {
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
        }
    }
}

impl SPyExpr<'_> {
    pub fn to_source(&self) -> TlResult<(String, f32)> {
        let mut result = String::new();
        let mut precedence = HIGH_PREC;

        match &self.value {
            PyExpr::Name(id) => {
                result.push_str(&id);
            }
            PyExpr::Tuple(items) => {
                let item_str: Vec<String> = items
                    .iter()
                    .map(|item| match item {
                        PyTupleItem::Item(expr) => expr.to_source_lhs(LOW_PREC),
                        PyTupleItem::Spread(expr) => {
                            Ok(format!("*{}", expr.to_source_lhs(HIGH_PREC)?))
                        }
                    })
                    .collect::<TlResult<_>>()?;

                if item_str.len() == 1 {
                    result.push_str(&format!("({},)", item_str[0]));
                } else {
                    result.push_str(&format!("({})", item_str.join(", ")));
                }
            }
            PyExpr::Dict(items) => {
                let item_str: Vec<String> = items
                    .iter()
                    .map(|item| match item {
                        PyDictItem::Item(key, value) => Ok(format!(
                            "{}: {}",
                            key.to_source_lhs(HIGH_PREC)?,
                            value.to_source_rhs(LOW_PREC)?
                        )),
                        PyDictItem::Spread(expr) => {
                            Ok(format!("**{}", expr.to_source_lhs(HIGH_PREC)?))
                        }
                    })
                    .collect::<TlResult<_>>()?;

                result.push_str(&format!("{{{}}}", item_str.join(", ")));
            }
            PyExpr::Binary(op, left, right) => {
                precedence = op.precedence();
                result.push_str(&format!(
                    "{} {} {}",
                    left.to_source_lhs(precedence)?,
                    op.to_source(),
                    right.to_source_rhs(precedence)?
                ));
            }
            PyExpr::Unary(op, expr) => {
                precedence = op.precedence();
                let s = expr.to_source()?;

                result.push_str(&format!(
                    "{}{}",
                    expr.to_source_lhs(precedence)?,
                    op.to_source()
                ));
            }
            PyExpr::Call(func, args) => {
                precedence = 20.;

                let mut arg_str: Vec<String> = vec![];
                for arg in args {
                    arg_str.push(arg.to_source()?);
                }

                result.push_str(&format!(
                    "{}({})",
                    func.to_source_lhs(precedence)?,
                    arg_str.join(", "),
                ));
            }
            PyExpr::Attribute(obj, attr) => {
                precedence = 20.;

                result.push_str(&format!("{}.{}", obj.to_source_lhs(precedence)?, attr));
            }
            PyExpr::Subscript(obj, index) => {
                precedence = 20.;

                result.push_str(&format!(
                    "{}[{}]",
                    obj.to_source_lhs(precedence)?,
                    index.to_source_rhs(LOW_PREC)?,
                ));
            }
            PyExpr::Yield(expr) => {
                result.push_str("yield ");
                result.push_str(&expr.to_source_rhs(precedence)?);
            }
            PyExpr::YieldFrom(expr) => {
                result.push_str("yield from ");
                result.push_str(&expr.to_source_rhs(precedence)?);
            }
            PyExpr::Literal(literal) => match literal {
                PyLiteral::Num(num) => {
                    precedence = 15.; // lower than attribute
                    result.push_str(&num.to_string())
                }
                PyLiteral::Str(s) => {
                    result.push('"');
                    result.push_str(s.as_ref());
                    result.push('"');
                }
                PyLiteral::Bool(b) => result.push_str(if *b { "True" } else { "False" }),
                PyLiteral::None => result.push_str("None"),
            },
            PyExpr::Fstr(fstr_parts) => {
                result.push_str("f\"");
                for part in fstr_parts {
                    match part {
                        PyFstrPart::Str(s) => result.push_str(s),
                        PyFstrPart::Expr(expr, ident) => {
                            result.push('{');
                            result.push_str(&expr.to_source_lhs(HIGH_PREC)?);
                            result.push('}');
                        }
                    }
                }
                result.push('"');
            }
            PyExpr::Slice(start, stop, step) => {
                let start_str = if let Some(start) = start {
                    start.to_source_lhs(LOW_PREC)?
                } else {
                    String::new()
                };
                let stop_str = if let Some(stop) = stop {
                    stop.to_source_lhs(LOW_PREC)?
                } else {
                    String::new()
                };
                let step_str = if let Some(step) = step {
                    format!(":{}", step.to_source_lhs(LOW_PREC)?)
                } else {
                    String::new()
                };
                result.push_str(&format!("{}:{}{}", start_str, stop_str, step_str));
            }
        }

        Ok((result, precedence))
    }

    pub fn to_source_lhs(&self, precedence: f32) -> TlResult<String> {
        let result = self.to_source()?;
        if result.1 < precedence {
            Ok(format!("({})", result.0))
        } else {
            Ok(result.0)
        }
    }

    pub fn to_source_rhs(&self, precedence: f32) -> TlResult<String> {
        let result = self.to_source()?;
        if result.1 <= precedence {
            Ok(format!("({})", result.0))
        } else {
            Ok(result.0)
        }
    }
}

pub fn block_to_source(
    block: &[SPyStmt],
    ctx: &mut EmitCtx,
    delta_indentation: i32,
) -> TlResult<String> {
    let old_indentation = ctx.indentation;
    ctx.indentation = (ctx.indentation as i32 + delta_indentation) as usize;

    let mut result = String::new();
    for stmt in block {
        result.push_str(&stmt.to_source(ctx)?);
    }

    ctx.indentation = old_indentation;
    Ok(result)
}

impl SPyStmt<'_> {
    pub fn to_source(&self, ctx: &mut EmitCtx) -> TlResult<String> {
        let res = match &self.value {
            PyStmt::Expr(expr) => ctx.indent_endl(&expr.to_source_lhs(LOW_PREC)?),
            PyStmt::Assign(target, value) => {
                let value_str = value.to_source_lhs(LOW_PREC)?;
                ctx.indent_endl(&format!(
                    "{} = {}",
                    target.to_source_lhs(LOW_PREC)?,
                    value_str
                ))
            }
            PyStmt::Return(expr) => {
                let expr_str = expr.to_source_lhs(LOW_PREC)?;
                ctx.indent_endl(&format!("return {}", expr_str))
            }
            PyStmt::If(cond, body, orelse) => {
                let mut s = String::new();

                s.push_str(&ctx.indent_endl(&format!("if {}:", cond.to_source_lhs(LOW_PREC)?)));
                s.push_str(&block_to_source(body, ctx, 1)?);

                if let Some(orelse) = orelse {
                    s.push_str(&ctx.indent_endl(&"else:"));
                    s.push_str(&block_to_source(orelse, ctx, 1)?);
                }

                s
            }
            PyStmt::Raise(expr) => {
                let expr_str = expr.to_source_lhs(LOW_PREC)?;
                ctx.indent_endl(&format!("raise {}", expr_str))
            }
            PyStmt::Assert(expr, msg) => {
                let expr_str = expr.to_source_lhs(LOW_PREC)?;
                let msg_str = if let Some(msg) = msg {
                    format!(", {}", msg.to_source_lhs(LOW_PREC)?)
                } else {
                    String::new()
                };
                ctx.indent_endl(&format!("assert {}{}", expr_str, msg_str))
            }
            PyStmt::Global(names) => {
                let names_str: Vec<String> = names.iter().map(|name| name.to_string()).collect();
                ctx.indent_endl(&format!("global {}", names_str.join(", ")))
            }
            PyStmt::Nonlocal(names) => {
                let names_str: Vec<String> = names.iter().map(|name| name.to_string()).collect();
                ctx.indent_endl(&format!("nonlocal {}", names_str.join(", ")))
            }
            PyStmt::Import(name) => ctx.indent_endl(&format!("import {}", name.to_source())),
            PyStmt::ImportFrom(module, aliases) => {
                let aliases_str: Vec<String> =
                    aliases.iter().map(|alias| alias.to_source()).collect();

                if aliases.is_empty() {
                    ctx.indent_endl(&format!("from {} import *", module))
                } else {
                    ctx.indent_endl(&format!(
                        "from {} import {}",
                        module,
                        aliases_str.join(", ")
                    ))
                }
            }
            PyStmt::FnDef(name, args, body) => {
                let mut s = String::new();
                let args_str: Vec<String> = args
                    .iter()
                    .map(|arg| arg.to_source())
                    .collect::<TlResult<_>>()?;

                s.push_str(&ctx.indent_endl(&format!("def {}({}):", name, args_str.join(", "))));
                s.push_str(&block_to_source(body, ctx, 1)?);
                s
            }
            PyStmt::ClassDef(name, bases, body) => {
                let mut s = String::new();
                let bases_str: Vec<String> = bases
                    .iter()
                    .map(|base| base.to_source())
                    .collect::<TlResult<_>>()?;

                s.push_str(&ctx.indent_endl(&format!("class {}({}):", name, bases_str.join(", "))));
                s.push_str(&block_to_source(body, ctx, 1)?);
                s
            }
            PyStmt::While(cond, body) => {
                let mut s = String::new();
                s.push_str(&ctx.indent_endl(&format!("while {}:", cond.to_source_lhs(LOW_PREC)?)));
                s.push_str(&block_to_source(body, ctx, 1)?);
                s
            }
            PyStmt::For(target, iter, body) => {
                let mut s = String::new();
                s.push_str(&ctx.indent_endl(&format!(
                    "for {} in {}:",
                    target,
                    iter.to_source_lhs(LOW_PREC)?
                )));
                s.push_str(&block_to_source(body, ctx, 1)?);
                s
            }
            PyStmt::Try(body, handlers, finally) => {
                let mut s = String::new();
                s.push_str(&ctx.indent_endl("try:"));
                s.push_str(&block_to_source(body, ctx, 1)?);

                for handler in handlers {
                    // This would need handler.to_source() implementation
                    s.push_str(&ctx.indent_endl("except:"));
                    // Handler body would go here
                }

                if let Some(finally) = finally {
                    s.push_str(&ctx.indent_endl("finally:"));
                    s.push_str(&block_to_source(finally, ctx, 1)?);
                }
                s
            }
            PyStmt::Break => ctx.indent_endl("break"),
            PyStmt::Continue => ctx.indent_endl("continue"),
            PyStmt::Match(py_spanned, py_match_cases) => {
                let mut s = String::new();
                s.push_str(
                    &ctx.indent_endl(&format!("match {}:", py_spanned.to_source_lhs(LOW_PREC)?)),
                );

                for case in py_match_cases {
                    s.push_str(
                        &ctx.indent_endl(&format!(
                            "case {}:",
                            case.pattern.to_source_lhs(LOW_PREC)?
                        )),
                    );
                    s.push_str(&block_to_source(&case.body, ctx, 1)?);
                }

                s
            }
        };

        Ok(res)
    }
}

#[cfg(test)]
mod tests {
    use parser::ast::Span;

    use super::*;

    const DUMMY_SPAN: Span = Span {
        context: (),
        start: 0,
        end: 0,
    };

    #[test]
    fn test_expr_to_source() {
        let expr: SPyExpr = (
            PyExpr::Binary(
                PyBinaryOp::Mult,
                Box::new((PyExpr::Name("x".into()), DUMMY_SPAN).into()),
                Box::new(
                    (
                        PyExpr::Binary(
                            PyBinaryOp::Add,
                            Box::new((PyExpr::Name("y".into()), DUMMY_SPAN).into()),
                            Box::new((PyExpr::Name("z".into()), DUMMY_SPAN).into()),
                        ),
                        DUMMY_SPAN,
                    )
                        .into(),
                ),
            ),
            DUMMY_SPAN,
        )
            .into();

        assert_eq!(expr.to_source_lhs(LOW_PREC).unwrap(), "x * (y + z)");
    }
}
