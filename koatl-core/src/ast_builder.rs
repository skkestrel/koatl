use std::borrow::Cow;

use crate::ast::*;

pub struct AstBuilder {
    pub span: Span,
}

impl AstBuilder {
    pub fn new(span: Span) -> Self {
        AstBuilder { span }
    }

    // Statement builders

    pub fn ident<'src>(&self, name: impl Into<Ident<'src>>) -> SExpr<'src> {
        Expr::Ident(name.into().spanned(self.span)).spanned(self.span)
    }

    pub fn expr<'src>(&self, expr: impl IntoIndirect<SExpr<'src>>) -> SStmt<'src> {
        Stmt::Expr(expr.indirect()).spanned(self.span)
    }

    pub fn assign<'src>(
        &self,
        target: impl Into<Indirect<SExpr<'src>>>,
        value: impl Into<Indirect<SExpr<'src>>>,
    ) -> SStmt<'src> {
        Stmt::Assign(target.into(), value.into(), None).spanned(self.span)
    }

    pub fn assign_modified<'src>(
        &self,
        target: impl Into<Indirect<SExpr<'src>>>,
        value: impl Into<Indirect<SExpr<'src>>>,
        aug_op: Option<BinaryOp>,
    ) -> SStmt<'src> {
        Stmt::Assign(target.into(), value.into(), aug_op).spanned(self.span)
    }

    pub fn return_<'src>(&self, expr: impl IntoIndirect<SExpr<'src>>) -> SStmt<'src> {
        Stmt::Return(expr.indirect()).spanned(self.span)
    }

    pub fn while_<'src>(
        &self,
        test: impl IntoIndirect<SExpr<'src>>,
        body: impl IntoIndirect<SExpr<'src>>,
    ) -> SStmt<'src> {
        Stmt::While(test.indirect(), body.indirect()).spanned(self.span)
    }

    pub fn for_<'src>(
        &self,
        target: impl IntoIndirect<SPattern<'src>>,
        iter: impl IntoIndirect<SExpr<'src>>,
        body: impl IntoIndirect<SExpr<'src>>,
    ) -> SStmt<'src> {
        Stmt::For(target.indirect(), iter.indirect(), body.indirect()).spanned(self.span)
    }

    pub fn import<'src>(&self, import: ImportTree<'src>, reexport: bool) -> SStmt<'src> {
        Stmt::Import(import, reexport).spanned(self.span)
    }

    pub fn try_<'src>(
        &self,
        body: impl IntoIndirect<SExpr<'src>>,
        handlers: Vec<SMatchCase<'src>>,
        orelse: Option<impl IntoIndirect<SExpr<'src>>>,
    ) -> SExpr<'src> {
        Expr::Try(body.indirect(), handlers, orelse.map(|o| o.indirect())).spanned(self.span)
    }

    pub fn raise<'src>(&self, expr: Option<impl IntoIndirect<SExpr<'src>>>) -> SStmt<'src> {
        Stmt::Raise(expr.map(|e| e.indirect())).spanned(self.span)
    }

    pub fn break_<'src>(&self) -> SStmt<'src> {
        Stmt::Break.spanned(self.span)
    }

    pub fn continue_<'src>(&self) -> SStmt<'src> {
        Stmt::Continue.spanned(self.span)
    }

    // Expression builders
    pub fn literal<'src>(&self, lit: Literal<'src>) -> SExpr<'src> {
        Expr::Literal(lit.spanned(self.span)).spanned(self.span)
    }

    pub fn unary<'src>(&self, op: UnaryOp, operand: impl IntoIndirect<SExpr<'src>>) -> SExpr<'src> {
        Expr::Unary(op, operand.indirect()).spanned(self.span)
    }

    pub fn binary<'src>(
        &self,
        op: BinaryOp,
        left: impl IntoIndirect<SExpr<'src>>,
        right: impl IntoIndirect<SExpr<'src>>,
    ) -> SExpr<'src> {
        Expr::Binary(op, left.indirect(), right.indirect()).spanned(self.span)
    }

    pub fn list<'src>(&self, items: Vec<SListItem<'src>>) -> SExpr<'src> {
        Expr::List(items).spanned(self.span)
    }

    pub fn mapping<'src>(&self, items: Vec<SMappingItem<'src>>) -> SExpr<'src> {
        Expr::Mapping(items).spanned(self.span)
    }

    pub fn slice<'src>(
        &self,
        start: Option<impl IntoIndirect<SExpr<'src>>>,
        stop: Option<impl IntoIndirect<SExpr<'src>>>,
        step: Option<impl IntoIndirect<SExpr<'src>>>,
    ) -> SExpr<'src> {
        Expr::Slice(
            start.map(|x| x.indirect()),
            stop.map(|x| x.indirect()),
            step.map(|x| x.indirect()),
        )
        .spanned(self.span)
    }

    pub fn if_<'src>(
        &self,
        test: impl IntoIndirect<SExpr<'src>>,
        body: impl IntoIndirect<SExpr<'src>>,
        orelse: Option<impl IntoIndirect<SExpr<'src>>>,
    ) -> SExpr<'src> {
        Expr::If(
            test.indirect(),
            body.indirect(),
            orelse.map(|x| x.indirect()),
        )
        .spanned(self.span)
    }

    pub fn match_<'src>(&self, subject: SExpr<'src>, cases: Vec<SMatchCase<'src>>) -> SExpr<'src> {
        Expr::Match(subject.indirect(), cases).spanned(self.span)
    }

    pub fn class<'src>(&self, bases: Vec<SCallItem<'src>>, body: SExpr<'src>) -> SExpr<'src> {
        Expr::Class(bases, body.indirect()).spanned(self.span)
    }

    pub fn call<'src>(
        &self,
        func: impl IntoIndirect<SExpr<'src>>,
        args: Vec<SCallItem<'src>>,
    ) -> SExpr<'src> {
        Expr::Call(func.indirect(), args).spanned(self.span)
    }

    pub fn subscript<'src>(&self, value: SExpr<'src>, slice: Vec<SListItem<'src>>) -> SExpr<'src> {
        Expr::Subscript(value.indirect(), slice).spanned(self.span)
    }

    pub fn attribute<'src>(&self, value: SExpr<'src>, attr: impl Into<Ident<'src>>) -> SExpr<'src> {
        Expr::RawAttribute(value.indirect(), attr.into().spanned(self.span)).spanned(self.span)
    }

    pub fn then<'src>(&self, left: SExpr<'src>, right: SExpr<'src>) -> SExpr<'src> {
        Expr::ScopedAttribute(left.indirect(), right.indirect()).spanned(self.span)
    }

    pub fn function<'src>(&self, args: Vec<SArgDefItem<'src>>, body: SExpr<'src>) -> SExpr<'src> {
        Expr::Fn(args, body.indirect()).spanned(self.span)
    }

    pub fn fstring<'src>(
        &self,
        prefix: impl Into<String>,
        parts: Vec<(SFmtExpr<'src>, impl Into<String>)>,
    ) -> SExpr<'src> {
        Expr::Fstr(
            prefix.into().spanned(self.span),
            parts
                .into_iter()
                .map(|(x, s)| (x, s.into().spanned(self.span)))
                .collect(),
        )
        .spanned(self.span)
    }

    pub fn block_expr<'src>(&self, block: Vec<impl IntoIndirect<SStmt<'src>>>) -> SExpr<'src> {
        Expr::Block(block.into_iter().map(|x| x.indirect()).collect()).spanned(self.span)
    }

    // Literal builders
    pub fn int<'src>(&self, value: impl Into<Cow<'src, str>>) -> SExpr<'src> {
        Expr::Literal(Literal::Int(value.into()).spanned(self.span)).spanned(self.span)
    }

    pub fn str<'src>(&self, value: impl Into<Cow<'src, str>>) -> SExpr<'src> {
        Expr::Literal(Literal::Str(value.into()).spanned(self.span)).spanned(self.span)
    }

    // Utility builders for list/mapping items
    pub fn list_item<'src>(&self, expr: SExpr<'src>) -> SListItem<'src> {
        ListItem::Item(expr.indirect())
    }

    pub fn list_spread<'src>(&self, expr: SExpr<'src>) -> SListItem<'src> {
        ListItem::Spread(expr.indirect())
    }

    pub fn mapping_item<'src>(&self, key: SExpr<'src>, value: SExpr<'src>) -> SMappingItem<'src> {
        MappingItem::Item(key.indirect(), value.indirect())
    }

    pub fn mapping_spread<'src>(&self, expr: SExpr<'src>) -> SMappingItem<'src> {
        MappingItem::Spread(expr.indirect())
    }

    // Call item builders
    pub fn call_arg<'src>(&self, expr: SExpr<'src>) -> SCallItem<'src> {
        CallItem::Arg(expr.indirect())
    }

    // Argument item builders
    pub fn arg<'src>(&self, arg: SPattern<'src>) -> SArgDefItem<'src> {
        ArgDefItem::Arg(arg.indirect(), None)
    }

    pub fn import_star<'src>(
        &self,
        trunk: Vec<impl Into<Ident<'src>>>,
        level: usize,
    ) -> ImportTree<'src> {
        ImportTree {
            trunk: trunk
                .into_iter()
                .map(|t| t.into().spanned(self.span))
                .collect(),
            leaf: ImportLeaf::Star.spanned(self.span),
            level,
        }
    }
}
