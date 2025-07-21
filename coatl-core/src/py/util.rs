#![allow(unused_variables, dead_code)]

use crate::py::ast::*;
use parser::ast::Span;

pub struct PyAstBuilder {
    pub span: Span,
}

impl PyAstBuilder {
    pub fn new(span: Span) -> Self {
        PyAstBuilder { span }
    }

    // Statement builders
    pub fn expr<'src>(&self, expr: SPyExpr<'src>) -> SPyStmt<'src> {
        (PyStmt::Expr(expr), self.span).into()
    }

    pub fn assign<'src>(&self, target: SPyExpr<'src>, value: SPyExpr<'src>) -> SPyStmt<'src> {
        (PyStmt::Assign(target, value), self.span).into()
    }

    pub fn return_<'src>(&self, expr: SPyExpr<'src>) -> SPyStmt<'src> {
        (PyStmt::Return(expr), self.span).into()
    }

    pub fn raise<'src>(&self, expr: Option<SPyExpr<'src>>) -> SPyStmt<'src> {
        (PyStmt::Raise(expr), self.span).into()
    }

    pub fn assert<'src>(&self, expr: SPyExpr<'src>, msg: Option<SPyExpr<'src>>) -> SPyStmt<'src> {
        (PyStmt::Assert(expr, msg), self.span).into()
    }

    pub fn global<'src>(&self, names: Vec<impl Into<PyIdent<'src>>>) -> SPyStmt<'src> {
        let names = names.into_iter().map(|name| name.into()).collect();
        (PyStmt::Global(names), self.span).into()
    }

    pub fn nonlocal<'src>(&self, names: Vec<impl Into<PyIdent<'src>>>) -> SPyStmt<'src> {
        let names = names.into_iter().map(|name| name.into()).collect();
        (PyStmt::Nonlocal(names), self.span).into()
    }

    pub fn import<'src>(&self, aliases: Vec<PyImportAlias<'src>>) -> SPyStmt<'src> {
        (PyStmt::Import(aliases), self.span).into()
    }

    pub fn import_from<'src>(
        &self,
        module: impl Into<Option<PyIdent<'src>>>,
        aliases: Vec<PyImportAlias<'src>>,
        level: usize,
    ) -> SPyStmt<'src> {
        (PyStmt::ImportFrom(module.into(), aliases, level), self.span).into()
    }

    pub fn fn_def<'src>(
        &self,
        name: impl Into<PyIdent<'src>>,
        args: Vec<PyArgDefItem<'src>>,
        body: PyBlock<'src>,
    ) -> SPyStmt<'src> {
        (
            PyStmt::FnDef(name.into(), args, body, PyDecorators::new()),
            self.span,
        )
            .into()
    }

    pub fn class_def<'src>(
        &self,
        name: impl Into<PyIdent<'src>>,
        bases: Vec<PyCallItem<'src>>,
        body: PyBlock<'src>,
    ) -> SPyStmt<'src> {
        (
            PyStmt::ClassDef(name.into(), bases, body, PyDecorators::new()),
            self.span,
        )
            .into()
    }

    pub fn del<'src>(&self, targets: Vec<SPyExpr<'src>>) -> SPyStmt<'src> {
        (PyStmt::Del(targets), self.span).into()
    }

    pub fn while_<'src>(&self, test: SPyExpr<'src>, body: PyBlock<'src>) -> SPyStmt<'src> {
        (PyStmt::While(test, body), self.span).into()
    }

    pub fn for_<'src>(
        &self,
        target: SPyExpr<'src>,
        iter: SPyExpr<'src>,
        body: PyBlock<'src>,
    ) -> SPyStmt<'src> {
        (PyStmt::For(target, iter, body), self.span).into()
    }

    pub fn if_<'src>(
        &self,
        test: SPyExpr<'src>,
        body: PyBlock<'src>,
        orelse: Option<PyBlock<'src>>,
    ) -> SPyStmt<'src> {
        (PyStmt::If(test, body, orelse), self.span).into()
    }

    pub fn match_<'src>(
        &self,
        subject: SPyExpr<'src>,
        cases: Vec<PyMatchCase<'src>>,
    ) -> SPyStmt<'src> {
        (PyStmt::Match(subject, cases), self.span).into()
    }

    pub fn try_<'src>(
        &self,
        body: PyBlock<'src>,
        handlers: Vec<PyExceptHandler<'src>>,
        finally: Option<PyBlock<'src>>,
    ) -> SPyStmt<'src> {
        (PyStmt::Try(body, handlers, finally), self.span).into()
    }

    pub fn break_<'src>(&self) -> SPyStmt<'src> {
        (PyStmt::Break, self.span).into()
    }

    pub fn continue_<'src>(&self) -> SPyStmt<'src> {
        (PyStmt::Continue, self.span).into()
    }

    pub fn pass<'src>(&self) -> SPyStmt<'src> {
        (PyStmt::Pass, self.span).into()
    }

    // Expression builders
    pub fn if_expr<'src>(
        &self,
        test: SPyExpr<'src>,
        body: SPyExpr<'src>,
        orelse: SPyExpr<'src>,
    ) -> SPyExpr<'src> {
        (
            PyExpr::IfExpr(Box::new(test), Box::new(body), Box::new(orelse)),
            self.span,
        )
            .into()
    }

    pub fn ident<'src>(&self, name: impl Into<PyIdent<'src>>, ctx: PyAccessCtx) -> SPyExpr<'src> {
        (PyExpr::Ident(name.into(), ctx), self.span).into()
    }

    pub fn load_ident<'src>(&self, name: impl Into<PyIdent<'src>>) -> SPyExpr<'src> {
        self.ident(name, PyAccessCtx::Load)
    }

    pub fn literal<'src>(&self, lit: PyLiteral<'src>) -> SPyExpr<'src> {
        (PyExpr::Literal(lit), self.span).into()
    }

    pub fn num<'src>(&self, value: impl Into<PyIdent<'src>>) -> SPyExpr<'src> {
        (PyExpr::Literal(PyLiteral::Num(value.into())), self.span).into()
    }

    pub fn str<'src>(&self, value: impl Into<PyIdent<'src>>) -> SPyExpr<'src> {
        (PyExpr::Literal(PyLiteral::Str(value.into())), self.span).into()
    }

    pub fn bool<'src>(&self, value: bool) -> SPyExpr<'src> {
        (PyExpr::Literal(PyLiteral::Bool(value)), self.span).into()
    }

    pub fn none<'src>(&self) -> SPyExpr<'src> {
        (PyExpr::Literal(PyLiteral::None), self.span).into()
    }

    pub fn fstr<'src>(&self, parts: Vec<PyFstrPart<'src>>) -> SPyExpr<'src> {
        (PyExpr::Fstr(parts), self.span).into()
    }

    pub fn binary<'src>(
        &self,
        op: PyBinaryOp,
        left: SPyExpr<'src>,
        right: SPyExpr<'src>,
    ) -> SPyExpr<'src> {
        (
            PyExpr::Binary(op, Box::new(left), Box::new(right)),
            self.span,
        )
            .into()
    }

    pub fn unary<'src>(&self, op: PyUnaryOp, operand: SPyExpr<'src>) -> SPyExpr<'src> {
        (PyExpr::Unary(op, Box::new(operand)), self.span).into()
    }

    pub fn call<'src>(&self, func: SPyExpr<'src>, args: Vec<PyCallItem<'src>>) -> SPyExpr<'src> {
        (PyExpr::Call(Box::new(func), args), self.span).into()
    }

    pub fn attribute<'src>(
        &self,
        value: SPyExpr<'src>,
        attr: impl Into<PyIdent<'src>>,
        ctx: PyAccessCtx,
    ) -> SPyExpr<'src> {
        (
            PyExpr::Attribute(Box::new(value), attr.into(), ctx),
            self.span,
        )
            .into()
    }

    pub fn subscript<'src>(
        &self,
        value: SPyExpr<'src>,
        slice: SPyExpr<'src>,
        ctx: PyAccessCtx,
    ) -> SPyExpr<'src> {
        (
            PyExpr::Subscript(Box::new(value), Box::new(slice), ctx),
            self.span,
        )
            .into()
    }

    pub fn tuple<'src>(&self, items: Vec<PyListItem<'src>>, access: PyAccessCtx) -> SPyExpr<'src> {
        (PyExpr::Tuple(items, access), self.span).into()
    }

    pub fn list<'src>(&self, items: Vec<PyListItem<'src>>, access: PyAccessCtx) -> SPyExpr<'src> {
        (PyExpr::List(items, access), self.span).into()
    }

    pub fn dict<'src>(&self, items: Vec<PyDictItem<'src>>) -> SPyExpr<'src> {
        (PyExpr::Dict(items), self.span).into()
    }

    pub fn slice<'src>(
        &self,
        start: Option<SPyExpr<'src>>,
        stop: Option<SPyExpr<'src>>,
        step: Option<SPyExpr<'src>>,
    ) -> SPyExpr<'src> {
        (
            PyExpr::Slice(start.map(Box::new), stop.map(Box::new), step.map(Box::new)),
            self.span,
        )
            .into()
    }

    pub fn yield_<'src>(&self, value: SPyExpr<'src>) -> SPyExpr<'src> {
        (PyExpr::Yield(Box::new(value)), self.span).into()
    }

    pub fn yield_from<'src>(&self, value: SPyExpr<'src>) -> SPyExpr<'src> {
        (PyExpr::YieldFrom(Box::new(value)), self.span).into()
    }

    pub fn lambda<'src>(
        &self,
        args: Vec<PyArgDefItem<'src>>,
        body: SPyExpr<'src>,
    ) -> SPyExpr<'src> {
        (PyExpr::Lambda(args, Box::new(body)), self.span).into()
    }

    // Utility builders for call items
    pub fn call_arg<'src>(&self, expr: SPyExpr<'src>) -> PyCallItem<'src> {
        PyCallItem::Arg(expr)
    }

    pub fn call_kwarg<'src>(
        &self,
        name: impl Into<PyIdent<'src>>,
        value: SPyExpr<'src>,
    ) -> PyCallItem<'src> {
        PyCallItem::Kwarg(name.into(), value)
    }

    pub fn call_arg_spread<'src>(&self, expr: SPyExpr<'src>) -> PyCallItem<'src> {
        PyCallItem::ArgSpread(expr)
    }

    pub fn call_kwarg_spread<'src>(&self, expr: SPyExpr<'src>) -> PyCallItem<'src> {
        PyCallItem::KwargSpread(expr)
    }

    // Utility builders for argument definitions
    pub fn arg_def<'src>(
        &self,
        name: impl Into<PyIdent<'src>>,
        default: Option<SPyExpr<'src>>,
    ) -> PyArgDefItem<'src> {
        PyArgDefItem::Arg(name.into(), default)
    }

    pub fn arg_def_spread<'src>(&self, name: impl Into<PyIdent<'src>>) -> PyArgDefItem<'src> {
        PyArgDefItem::ArgSpread(name.into())
    }

    pub fn kwarg_def_spread<'src>(&self, name: impl Into<PyIdent<'src>>) -> PyArgDefItem<'src> {
        PyArgDefItem::KwargSpread(name.into())
    }

    // Utility builders for tuple items
    pub fn tuple_item<'src>(&self, expr: SPyExpr<'src>) -> PyListItem<'src> {
        PyListItem::Item(expr)
    }

    pub fn tuple_spread<'src>(&self, expr: SPyExpr<'src>) -> PyListItem<'src> {
        PyListItem::Spread(expr)
    }

    // Utility builders for dict items
    pub fn dict_item<'src>(&self, key: SPyExpr<'src>, value: SPyExpr<'src>) -> PyDictItem<'src> {
        PyDictItem::Item(key, value)
    }

    pub fn dict_spread<'src>(&self, expr: SPyExpr<'src>) -> PyDictItem<'src> {
        PyDictItem::Spread(expr)
    }

    // Utility builders for f-string parts
    pub fn fstr_str<'src>(&self, text: impl Into<PyIdent<'src>>) -> PyFstrPart<'src> {
        PyFstrPart::Str(text.into())
    }

    pub fn fstr_expr<'src>(
        &self,
        expr: SPyExpr<'src>,
        format_spec: impl Into<Option<PyIdent<'src>>>,
    ) -> PyFstrPart<'src> {
        PyFstrPart::Expr(expr, format_spec.into())
    }

    // Utility builders for import aliases
    pub fn import_alias<'src>(
        &self,
        name: impl Into<PyIdent<'src>>,
        as_name: Option<impl Into<PyIdent<'src>>>,
    ) -> PyImportAlias<'src> {
        PyImportAlias {
            name: name.into(),
            as_name: as_name.map(|n| n.into()),
        }
    }

    // Utility builders for exception handlers
    pub fn except_handler<'src>(
        &self,
        typ: Option<SPyExpr<'src>>,
        name: Option<impl Into<PyIdent<'src>>>,
        body: PyBlock<'src>,
    ) -> PyExceptHandler<'src> {
        PyExceptHandler {
            typ,
            name: name.map(|n| n.into()),
            body,
        }
    }

    // Utility builders for match cases
    pub fn match_case<'src>(
        &self,
        pattern: SPyPattern<'src>,
        guard: Option<SPyExpr<'src>>,
        body: PyBlock<'src>,
    ) -> PyMatchCase<'src> {
        PyMatchCase {
            pattern,
            guard,
            body,
        }
    }

    // Convenience methods for common operations
    pub fn add<'src>(&self, left: SPyExpr<'src>, right: SPyExpr<'src>) -> SPyExpr<'src> {
        self.binary(PyBinaryOp::Add, left, right)
    }

    pub fn sub<'src>(&self, left: SPyExpr<'src>, right: SPyExpr<'src>) -> SPyExpr<'src> {
        self.binary(PyBinaryOp::Sub, left, right)
    }

    pub fn mult<'src>(&self, left: SPyExpr<'src>, right: SPyExpr<'src>) -> SPyExpr<'src> {
        self.binary(PyBinaryOp::Mult, left, right)
    }

    pub fn div<'src>(&self, left: SPyExpr<'src>, right: SPyExpr<'src>) -> SPyExpr<'src> {
        self.binary(PyBinaryOp::Div, left, right)
    }

    pub fn mod_<'src>(&self, left: SPyExpr<'src>, right: SPyExpr<'src>) -> SPyExpr<'src> {
        self.binary(PyBinaryOp::Mod, left, right)
    }

    pub fn pow<'src>(&self, left: SPyExpr<'src>, right: SPyExpr<'src>) -> SPyExpr<'src> {
        self.binary(PyBinaryOp::Pow, left, right)
    }

    pub fn eq<'src>(&self, left: SPyExpr<'src>, right: SPyExpr<'src>) -> SPyExpr<'src> {
        self.binary(PyBinaryOp::Eq, left, right)
    }

    pub fn neq<'src>(&self, left: SPyExpr<'src>, right: SPyExpr<'src>) -> SPyExpr<'src> {
        self.binary(PyBinaryOp::Neq, left, right)
    }

    pub fn lt<'src>(&self, left: SPyExpr<'src>, right: SPyExpr<'src>) -> SPyExpr<'src> {
        self.binary(PyBinaryOp::Lt, left, right)
    }

    pub fn leq<'src>(&self, left: SPyExpr<'src>, right: SPyExpr<'src>) -> SPyExpr<'src> {
        self.binary(PyBinaryOp::Leq, left, right)
    }

    pub fn gt<'src>(&self, left: SPyExpr<'src>, right: SPyExpr<'src>) -> SPyExpr<'src> {
        self.binary(PyBinaryOp::Gt, left, right)
    }

    pub fn geq<'src>(&self, left: SPyExpr<'src>, right: SPyExpr<'src>) -> SPyExpr<'src> {
        self.binary(PyBinaryOp::Geq, left, right)
    }

    pub fn is<'src>(&self, left: SPyExpr<'src>, right: SPyExpr<'src>) -> SPyExpr<'src> {
        self.binary(PyBinaryOp::Is, left, right)
    }

    pub fn is_not<'src>(&self, left: SPyExpr<'src>, right: SPyExpr<'src>) -> SPyExpr<'src> {
        self.binary(PyBinaryOp::Nis, left, right)
    }

    pub fn and<'src>(&self, left: SPyExpr<'src>, right: SPyExpr<'src>) -> SPyExpr<'src> {
        self.binary(PyBinaryOp::And, left, right)
    }

    pub fn or<'src>(&self, left: SPyExpr<'src>, right: SPyExpr<'src>) -> SPyExpr<'src> {
        self.binary(PyBinaryOp::Or, left, right)
    }

    pub fn not<'src>(&self, operand: SPyExpr<'src>) -> SPyExpr<'src> {
        self.unary(PyUnaryOp::Not, operand)
    }

    pub fn neg<'src>(&self, operand: SPyExpr<'src>) -> SPyExpr<'src> {
        self.unary(PyUnaryOp::Neg, operand)
    }

    pub fn pos<'src>(&self, operand: SPyExpr<'src>) -> SPyExpr<'src> {
        self.unary(PyUnaryOp::Pos, operand)
    }

    pub fn inv<'src>(&self, operand: SPyExpr<'src>) -> SPyExpr<'src> {
        self.unary(PyUnaryOp::Inv, operand)
    }
}
