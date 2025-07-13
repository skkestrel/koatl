use crate::{py::TlResult, py_ast::*};

const LOW_PREC: f32 = -1.0;
const HIGH_PREC: f32 = 100.0;

struct GenCtx {
    indentation: usize,
}

impl PyCallItem<'_> {
    pub fn to_source(&self) -> TlResult<String> {
        match self {
            PyCallItem::Arg(expr) => expr.to_source_lhs(LOW_PREC),
            PyCallItem::Kwarg(name, expr) => {
                Ok(format!("{}={}", name, expr.to_source_lhs(LOW_PREC)?))
            }
            PyCallItem::ArgSpread(expr) => Ok(format!("*{}", expr.to_source_lhs(LOW_PREC)?)),
            PyCallItem::KwargSpread(expr) => Ok(format!("**{}", expr.to_source_lhs(LOW_PREC)?)),
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
            PyExpr::Name(id, _) => {
                result.push_str(&id);
            }
            PyExpr::Tuple(items) => {
                let item_str: Vec<String> = items
                    .iter()
                    .map(|item| item.to_source_lhs(LOW_PREC))
                    .collect::<TlResult<_>>()?;

                result.push_str(&format!("({})", item_str.join(", ")));
            }
            PyExpr::Dict(items) => {
                let item_str: Vec<String> = items
                    .iter()
                    .map(|(key, value)| {
                        Ok(format!(
                            "{}: {}",
                            key.to_source_lhs(HIGH_PREC)?,
                            value.to_source_rhs(LOW_PREC)?
                        ))
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
                    arg_str.join(","),
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
                Box::new((PyExpr::Name("x".into(), PyNameCtx::Load), DUMMY_SPAN).into()),
                Box::new(
                    (
                        PyExpr::Binary(
                            PyBinaryOp::Add,
                            Box::new(
                                (PyExpr::Name("y".into(), PyNameCtx::Load), DUMMY_SPAN).into(),
                            ),
                            Box::new(
                                (PyExpr::Name("z".into(), PyNameCtx::Load), DUMMY_SPAN).into(),
                            ),
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
