use parser::*;

pub struct AstBuilder {
    pub span: Span,
}

impl AstBuilder {
    pub fn new(span: Span) -> Self {
        AstBuilder { span }
    }

    // Statement builders
    pub fn expr<'src>(&self, expr: SExpr<'src>) -> SStmt<'src> {
        (Stmt::Expr(expr), self.span)
    }

    pub fn assign<'src>(&self, target: SExpr<'src>, value: SExpr<'src>) -> SStmt<'src> {
        (Stmt::Assign(target, value), self.span)
    }

    pub fn return_<'src>(&self, expr: SExpr<'src>) -> SStmt<'src> {
        (Stmt::Return(expr), self.span)
    }

    pub fn global<'src>(&self, names: Vec<SIdent<'src>>) -> SStmt<'src> {
        (Stmt::Global(names), self.span)
    }

    pub fn nonlocal<'src>(&self, names: Vec<SIdent<'src>>) -> SStmt<'src> {
        (Stmt::Nonlocal(names), self.span)
    }

    pub fn while_<'src>(&self, test: SExpr<'src>, body: SBlock<'src>) -> SStmt<'src> {
        (Stmt::While(test, body), self.span)
    }

    pub fn for_<'src>(
        &self,
        target: SExpr<'src>,
        iter: SExpr<'src>,
        body: SBlock<'src>,
    ) -> SStmt<'src> {
        (Stmt::For(target, iter, body), self.span)
    }

    pub fn import<'src>(&self, import: ImportStmt<'src>) -> SStmt<'src> {
        (Stmt::Import(import), self.span)
    }

    pub fn try_<'src>(
        &self,
        body: SBlock<'src>,
        handlers: Vec<ExceptHandler<'src>>,
        orelse: Option<SBlock<'src>>,
    ) -> SStmt<'src> {
        (Stmt::Try(body, handlers, orelse), self.span)
    }

    pub fn raise<'src>(&self, expr: SExpr<'src>) -> SStmt<'src> {
        (Stmt::Raise(expr), self.span)
    }

    pub fn break_<'src>(&self) -> SStmt<'src> {
        (Stmt::Break, self.span)
    }

    pub fn continue_<'src>(&self) -> SStmt<'src> {
        (Stmt::Continue, self.span)
    }

    // Expression builders
    pub fn literal<'src>(&self, lit: SLiteral<'src>) -> SExpr<'src> {
        (Expr::Literal(lit), self.span)
    }

    pub fn ident<'src>(&self, name: SIdent<'src>) -> SExpr<'src> {
        (Expr::Ident(name), self.span)
    }

    pub fn unary<'src>(&self, op: UnaryOp, operand: SExpr<'src>) -> SExpr<'src> {
        (Expr::Unary(op, Box::new(operand)), self.span)
    }

    pub fn binary<'src>(&self, op: BinaryOp, left: SExpr<'src>, right: SExpr<'src>) -> SExpr<'src> {
        (Expr::Binary(op, Box::new(left), Box::new(right)), self.span)
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
            Expr::Slice(start.map(Box::new), stop.map(Box::new), step.map(Box::new)),
            self.span,
        )
    }

    pub fn if_<'src>(
        &self,
        test: SExpr<'src>,
        body: SBlock<'src>,
        orelse: Option<SBlock<'src>>,
    ) -> SExpr<'src> {
        (
            Expr::If(Box::new(test), Box::new(body), orelse.map(Box::new)),
            self.span,
        )
    }

    pub fn match_<'src>(
        &self,
        subject: SExpr<'src>,
        cases: Vec<(SExpr<'src>, Box<SBlock<'src>>)>,
    ) -> SExpr<'src> {
        (Expr::Match(Box::new(subject), cases), self.span)
    }

    pub fn class<'src>(&self, bases: Vec<SCallItem<'src>>, body: SBlock<'src>) -> SExpr<'src> {
        (Expr::Class(bases, Box::new(body)), self.span)
    }

    pub fn call<'src>(&self, func: SExpr<'src>, args: Vec<SCallItem<'src>>) -> SExpr<'src> {
        (Expr::Call(Box::new(func), args), self.span)
    }

    pub fn subscript<'src>(&self, value: SExpr<'src>, slice: Vec<ListItem<'src>>) -> SExpr<'src> {
        (Expr::Subscript(Box::new(value), slice), self.span)
    }

    pub fn attribute<'src>(&self, value: SExpr<'src>, attr: SIdent<'src>) -> SExpr<'src> {
        (Expr::Attribute(Box::new(value), attr), self.span)
    }

    pub fn pipe<'src>(&self, left: SExpr<'src>, right: SExpr<'src>) -> SExpr<'src> {
        (Expr::Pipe(Box::new(left), Box::new(right)), self.span)
    }

    pub fn yield_<'src>(&self, value: SExpr<'src>) -> SExpr<'src> {
        (Expr::Yield(Box::new(value)), self.span)
    }

    pub fn yield_from<'src>(&self, value: SExpr<'src>) -> SExpr<'src> {
        (Expr::YieldFrom(Box::new(value)), self.span)
    }

    pub fn function<'src>(&self, args: Vec<ArgItem<'src>>, body: SBlock<'src>) -> SExpr<'src> {
        (Expr::Fn(args, Box::new(body)), self.span)
    }

    pub fn fstring<'src>(
        &self,
        prefix: Spanned<String>,
        parts: Vec<(SFmtExpr<'src>, Spanned<String>)>,
    ) -> SExpr<'src> {
        (Expr::Fstr(prefix, parts), self.span)
    }

    pub fn block_expr<'src>(&self, block: SBlock<'src>) -> SExpr<'src> {
        (Expr::Block(Box::new(block)), self.span)
    }

    // Block builders
    pub fn stmts_block<'src>(&self, stmts: Vec<SStmt<'src>>) -> SBlock<'src> {
        (Block::Stmts(stmts), self.span)
    }

    pub fn expr_block<'src>(&self, expr: SExpr<'src>) -> SBlock<'src> {
        (Block::Expr(expr), self.span)
    }

    // Literal builders
    pub fn num<'src>(&self, value: &'src str) -> SLiteral<'src> {
        (Literal::Num(value), self.span)
    }

    pub fn str<'src>(&self, value: String) -> SLiteral<'src> {
        (Literal::Str(value), self.span)
    }

    // Utility builders for list/mapping items
    pub fn list_item<'src>(&self, expr: SExpr<'src>) -> ListItem<'src> {
        ListItem::Item(expr)
    }

    pub fn list_spread<'src>(&self, expr: SExpr<'src>) -> ListItem<'src> {
        ListItem::Spread(expr)
    }

    pub fn mapping_item<'src>(&self, key: SExpr<'src>, value: SBlock<'src>) -> MappingItem<'src> {
        MappingItem::Item(key, value)
    }

    pub fn mapping_spread<'src>(&self, expr: SExpr<'src>) -> MappingItem<'src> {
        MappingItem::Spread(expr)
    }

    // Call item builders
    pub fn call_arg<'src>(&self, expr: SExpr<'src>) -> SCallItem<'src> {
        (CallItem::Arg(expr), self.span)
    }

    pub fn call_kwarg<'src>(&self, name: SIdent<'src>, value: SExpr<'src>) -> SCallItem<'src> {
        (CallItem::Kwarg(name, value), self.span)
    }

    pub fn call_arg_spread<'src>(&self, expr: SExpr<'src>) -> SCallItem<'src> {
        (CallItem::ArgSpread(expr), self.span)
    }

    pub fn call_kwarg_spread<'src>(&self, expr: SExpr<'src>) -> SCallItem<'src> {
        (CallItem::KwargSpread(expr), self.span)
    }

    // Argument item builders
    pub fn arg<'src>(&self, name: SIdent<'src>) -> SArgItem<'src> {
        (ArgItem::Arg(name), self.span)
    }

    pub fn default_arg<'src>(&self, name: SIdent<'src>, default: SExpr<'src>) -> SArgItem<'src> {
        (ArgItem::DefaultArg(name, default), self.span)
    }

    pub fn arg_spread<'src>(&self, name: SIdent<'src>) -> SArgItem<'src> {
        (ArgItem::ArgSpread(name), self.span)
    }

    pub fn kwarg_spread<'src>(&self, name: SIdent<'src>) -> SArgItem<'src> {
        (ArgItem::KwargSpread(name), self.span)
    }

    // Format expression builder
    pub fn fmt_expr<'src>(&self, block: SBlock<'src>, fmt: Option<&'src str>) -> SFmtExpr<'src> {
        (FmtExpr { block, fmt }, self.span)
    }

    // Exception handler builder
    pub fn except_handler<'src>(
        &self,
        typ: Option<SExpr<'src>>,
        name: Option<SIdent<'src>>,
        body: SBlock<'src>,
    ) -> ExceptHandler<'src> {
        ExceptHandler { typ, name, body }
    }

    // Import statement builder
    pub fn import_stmt<'src>(
        &self,
        trunk: Vec<SIdent<'src>>,
        leaves: Vec<(SIdent<'src>, Option<SIdent<'src>>)>,
        star: bool,
    ) -> ImportStmt<'src> {
        ImportStmt {
            trunk,
            leaves,
            star,
        }
    }

    // Spanned identifier helper
    pub fn ident_<'src>(&self, name: &'src str) -> SIdent<'src> {
        (name, self.span)
    }
}
