use std::{borrow::Cow, rc::Rc};

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
        (Expr::Ident((name.into(), self.span)), self.span)
    }

    pub fn expr<'src>(&self, expr: SExpr<'src>) -> SStmt<'src> {
        (Stmt::Expr(expr), self.span)
    }

    pub fn assign<'src>(
        &self,
        target: impl Into<Indirect<SExpr<'src>>>,
        value: impl Into<Indirect<SExpr<'src>>>,
    ) -> SStmt<'src> {
        (Stmt::Assign(target.into(), value.into(), None), self.span)
    }

    pub fn assign_modified<'src>(
        &self,
        target: impl Into<Indirect<SExpr<'src>>>,
        value: impl Into<Indirect<SExpr<'src>>>,
        modifier: DeclType,
    ) -> SStmt<'src> {
        (
            Stmt::Assign(target.into(), value.into(), Some(modifier)),
            self.span,
        )
    }

    pub fn return_<'src>(&self, expr: SExpr<'src>) -> SStmt<'src> {
        (Stmt::Return(expr), self.span)
    }

    pub fn assert<'src>(&self, expr: SExpr<'src>, msg: Option<SExpr<'src>>) -> SStmt<'src> {
        (Stmt::Assert(expr, msg), self.span)
    }

    pub fn while_<'src>(&self, test: SExpr<'src>, body: SExpr<'src>) -> SStmt<'src> {
        (Stmt::While(test, body), self.span)
    }

    pub fn for_<'src>(
        &self,
        target: SPattern<'src>,
        iter: SExpr<'src>,
        body: SExpr<'src>,
    ) -> SStmt<'src> {
        (Stmt::For(target, iter, body), self.span)
    }

    pub fn import<'src>(&self, import: ImportStmt<'src>) -> SStmt<'src> {
        (Stmt::Import(import), self.span)
    }

    pub fn try_<'src>(
        &self,
        body: SExpr<'src>,
        handlers: Vec<Indirect<MatchCase<'src>>>,
        orelse: Option<SExpr<'src>>,
    ) -> SStmt<'src> {
        (Stmt::Try(body, handlers, orelse), self.span)
    }

    pub fn raise<'src>(&self, expr: Option<SExpr<'src>>) -> SStmt<'src> {
        (Stmt::Raise(expr), self.span)
    }

    pub fn break_<'src>(&self) -> SStmt<'src> {
        (Stmt::Break, self.span)
    }

    pub fn continue_<'src>(&self) -> SStmt<'src> {
        (Stmt::Continue, self.span)
    }

    // Expression builders
    pub fn literal<'src>(&self, lit: Literal<'src>) -> SExpr<'src> {
        (Expr::Literal((lit, self.span)), self.span)
    }

    pub fn unary<'src>(&self, op: UnaryOp, operand: SExpr<'src>) -> SExpr<'src> {
        (Expr::Unary(op, operand.into()), self.span)
    }

    pub fn binary<'src>(&self, op: BinaryOp, left: SExpr<'src>, right: SExpr<'src>) -> SExpr<'src> {
        (Expr::Binary(op, left.into(), right.into()), self.span)
    }

    pub fn list<'src>(&self, items: Vec<ListItem<'src>>) -> SExpr<'src> {
        (Expr::List(items), self.span)
    }

    pub fn mapping<'src>(&self, items: Vec<MappingItem<'src>>) -> SExpr<'src> {
        (Expr::Mapping(items), self.span)
    }

    pub fn slice<'src>(
        &self,
        start: Option<SExpr<'src>>,
        stop: Option<SExpr<'src>>,
        step: Option<SExpr<'src>>,
    ) -> SExpr<'src> {
        (
            Expr::Slice(start.map(Rc::new), stop.map(Rc::new), step.map(Rc::new)),
            self.span,
        )
    }

    pub fn if_<'src>(
        &self,
        test: SExpr<'src>,
        body: SExpr<'src>,
        orelse: Option<SExpr<'src>>,
    ) -> SExpr<'src> {
        (
            Expr::If(test.into(), body.into(), orelse.map(Rc::new)),
            self.span,
        )
    }

    pub fn match_<'src>(
        &self,
        subject: SExpr<'src>,
        cases: Vec<Indirect<MatchCase<'src>>>,
    ) -> SExpr<'src> {
        (Expr::Match(subject.into(), cases), self.span)
    }

    pub fn class<'src>(&self, bases: Vec<SCallItem<'src>>, body: SExpr<'src>) -> SExpr<'src> {
        (Expr::Class(bases, body.into()), self.span)
    }

    pub fn call<'src>(&self, func: SExpr<'src>, args: Vec<SCallItem<'src>>) -> SExpr<'src> {
        (Expr::Call(func.into(), args), self.span)
    }

    pub fn subscript<'src>(&self, value: SExpr<'src>, slice: Vec<ListItem<'src>>) -> SExpr<'src> {
        (Expr::Subscript(value.into(), slice), self.span)
    }

    pub fn attribute<'src>(&self, value: SExpr<'src>, attr: impl Into<Ident<'src>>) -> SExpr<'src> {
        (
            Expr::RawAttribute(value.into(), (attr.into(), self.span)),
            self.span,
        )
    }

    pub fn then<'src>(&self, left: SExpr<'src>, right: SExpr<'src>) -> SExpr<'src> {
        (Expr::ScopedAttribute(left.into(), right.into()), self.span)
    }

    pub fn function<'src>(&self, args: Vec<ArgDefItem<'src>>, body: SExpr<'src>) -> SExpr<'src> {
        (Expr::Fn(args, body.into()), self.span)
    }

    pub fn fstring<'src>(
        &self,
        prefix: Spanned<String>,
        parts: Vec<(SFmtExpr<'src>, Spanned<String>)>,
    ) -> SExpr<'src> {
        (Expr::Fstr(prefix, parts), self.span)
    }

    pub fn block_expr<'src>(&self, block: Vec<SStmt<'src>>) -> SExpr<'src> {
        (Expr::Block(block), self.span)
    }

    // Literal builders
    pub fn num<'src>(&self, value: impl Into<Cow<'src, str>>) -> SExpr<'src> {
        (
            Expr::Literal((Literal::Num(value.into()), self.span)),
            self.span,
        )
    }

    pub fn str<'src>(&self, value: impl Into<Cow<'src, str>>) -> SExpr<'src> {
        (
            Expr::Literal((Literal::Str(value.into()), self.span)),
            self.span,
        )
    }

    // Utility builders for list/mapping items
    pub fn list_item<'src>(&self, expr: SExpr<'src>) -> ListItem<'src> {
        ListItem::Item(expr)
    }

    pub fn list_spread<'src>(&self, expr: SExpr<'src>) -> ListItem<'src> {
        ListItem::Spread(expr)
    }

    pub fn mapping_item<'src>(&self, key: SExpr<'src>, value: SExpr<'src>) -> MappingItem<'src> {
        MappingItem::Item(key, value)
    }

    pub fn mapping_spread<'src>(&self, expr: SExpr<'src>) -> MappingItem<'src> {
        MappingItem::Spread(expr)
    }

    // Call item builders
    pub fn call_arg<'src>(&self, expr: SExpr<'src>) -> SCallItem<'src> {
        (CallItem::Arg(expr), self.span)
    }

    pub fn call_kwarg<'src>(
        &self,
        name: impl Into<Ident<'src>>,
        value: SExpr<'src>,
    ) -> SCallItem<'src> {
        (CallItem::Kwarg((name.into(), self.span), value), self.span)
    }

    pub fn call_arg_spread<'src>(&self, expr: SExpr<'src>) -> SCallItem<'src> {
        (CallItem::ArgSpread(expr), self.span)
    }

    pub fn call_kwarg_spread<'src>(&self, expr: SExpr<'src>) -> SCallItem<'src> {
        (CallItem::KwargSpread(expr), self.span)
    }

    // Argument item builders
    pub fn arg<'src>(&self, arg: SPattern<'src>) -> SArgItem<'src> {
        (ArgDefItem::Arg(arg.into(), None), self.span)
    }

    pub fn default_arg<'src>(&self, arg: SPattern<'src>, default: SExpr<'src>) -> SArgItem<'src> {
        (ArgDefItem::Arg(arg.into(), Some(default)), self.span)
    }

    pub fn arg_spread<'src>(&self, name: impl Into<Ident<'src>>) -> SArgItem<'src> {
        (ArgDefItem::ArgSpread((name.into(), self.span)), self.span)
    }

    pub fn kwarg_spread<'src>(&self, name: impl Into<Ident<'src>>) -> SArgItem<'src> {
        (ArgDefItem::KwargSpread((name.into(), self.span)), self.span)
    }

    // Format expression builder
    pub fn fmt_expr<'src>(
        &self,
        block: SExpr<'src>,
        fmt: Option<impl Into<Ident<'src>>>,
    ) -> SFmtExpr<'src> {
        (
            FmtExpr {
                expr: block,
                fmt: fmt.map(|f| f.into()),
            },
            self.span,
        )
    }

    pub fn import_star<'src>(
        &self,
        trunk: Vec<impl Into<Ident<'src>>>,
        level: usize,
    ) -> ImportStmt<'src> {
        ImportStmt {
            trunk: trunk.into_iter().map(|t| (t.into(), self.span)).collect(),
            imports: ImportList::Star,
            level,
            reexport: false,
        }
    }

    pub fn import_<'src>(
        &self,
        trunk: Vec<impl Into<Ident<'src>>>,
        leaves: Vec<(impl Into<Ident<'src>>, Option<impl Into<Ident<'src>>>)>,
        level: usize,
    ) -> ImportStmt<'src> {
        ImportStmt {
            trunk: trunk.into_iter().map(|t| (t.into(), self.span)).collect(),
            imports: ImportList::Leaves(
                leaves
                    .into_iter()
                    .map(|(name, alias)| {
                        (
                            (name.into(), self.span),
                            alias.map(|a| (a.into(), self.span)),
                        )
                    })
                    .collect(),
            ),
            level,
            reexport: false,
        }
    }
}
