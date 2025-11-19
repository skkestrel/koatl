use crate::{Token, cst::*, lexer::SToken};

pub trait SimpleFmt {
    fn simple_fmt(&self) -> String;
}

impl<'src, 'tok> SimpleFmt for SExpr<'src, 'tok> {
    fn simple_fmt(&self) -> String {
        self.value.simple_fmt()
    }
}

impl<'src, 'tok> SimpleFmt for SExprInner<'src, 'tok> {
    fn simple_fmt(&self) -> String {
        match self {
            Expr::Literal { token } => token.simple_fmt(),
            Expr::Ident { token } => token.simple_fmt(),
            Expr::Placeholder { token } => token.simple_fmt(),
            Expr::Binary {
                lhs,
                not,
                op,
                op_kind: _,
                rhs,
            } => {
                let not_str = if let Some(not_token) = not {
                    format!("{} ", not_token.simple_fmt())
                } else {
                    String::new()
                };

                format!(
                    "({} {}{} {})",
                    lhs.simple_fmt(),
                    not_str,
                    op.simple_fmt(),
                    rhs.simple_fmt()
                )
            }
            Expr::Unary {
                op_kind: _,
                op,
                expr,
            } => {
                format!("({} {})", op.token, expr.simple_fmt())
            }
            Expr::Call {
                expr,
                question,
                args,
            } => {
                let question_str = if question.is_some() { "?" } else { "" };
                format!(
                    "{}{}({})",
                    expr.simple_fmt(),
                    question_str,
                    args.simple_fmt()
                )
            }
            Expr::List { listing } => listing.simple_fmt(),
            Expr::Tuple { kind } => match kind {
                TupleKind::Unit(..) => format!("()"),
                TupleKind::Listing(items) => {
                    if items.len() == 1 {
                        return format!("({},)", items[0].item.simple_fmt());
                    }

                    let items_str = items
                        .iter()
                        .map(|item| item.item.simple_fmt())
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("({})", items_str)
                }
            },
            Expr::Try {
                body,
                cases,
                finally: finally_clause,
                ..
            } => {
                let cases_str = cases
                    .iter()
                    .map(|case| {
                        format!(
                            "except {} {} {}",
                            case.pattern.simple_fmt(),
                            case.guard
                                .as_ref()
                                .map(|x| " if ".to_string() + &x.1.simple_fmt())
                                .unwrap_or_default(),
                            case.body.simple_fmt()
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("; ");

                let finally_str = if let Some((_, finally_expr)) = finally_clause {
                    format!("; finally: {}", finally_expr.simple_fmt())
                } else {
                    String::new()
                };

                format!("(try {} \n{}{})", body.simple_fmt(), cases_str, finally_str)
            }
            Expr::ClassicIf {
                if_kw: _,
                cond,
                body,
                else_clause,
            } => {
                let else_str = if let Some((_, expr)) = else_clause {
                    format!(" else {}", expr.simple_fmt())
                } else {
                    String::new()
                };
                format!(
                    "if {} then {}{}",
                    cond.simple_fmt(),
                    body.simple_fmt(),
                    else_str
                )
            }
            Expr::If {
                cond,
                then_kw: _,
                body,
                else_clause,
            } => {
                let else_str = if let Some((_, expr)) = else_clause {
                    format!(" else {}", expr.simple_fmt())
                } else {
                    String::new()
                };
                format!(
                    "{} then {}{}",
                    cond.simple_fmt(),
                    body.simple_fmt(),
                    else_str
                )
            }
            Expr::Parenthesized { expr, .. } => expr.simple_fmt(),
            Expr::Subscript {
                expr,
                question,
                indices,
            } => {
                let question_str = if question.is_some() { "?" } else { "" };
                format!(
                    "{}{}[{}]",
                    expr.simple_fmt(),
                    question_str,
                    indices.simple_fmt()
                )
            }
            Expr::RawAttribute {
                expr,
                question,
                attr,
                ..
            } => {
                let question_str = if question.is_some() { "?" } else { "" };
                format!(
                    "{}{}::{}",
                    expr.simple_fmt(),
                    question_str,
                    attr.simple_fmt()
                )
            }
            Expr::ScopedAttribute {
                expr,
                question,
                rhs,
                ..
            } => {
                let question_str = if question.is_some() { "?" } else { "" };
                format!("{}{}.{}", expr.simple_fmt(), question_str, rhs.simple_fmt())
            }
            Expr::Attribute {
                expr,
                question,
                attr,
                ..
            } => {
                let question_str = if question.is_some() { "?" } else { "" };
                format!(
                    "{}{}.{}",
                    expr.simple_fmt(),
                    question_str,
                    attr.simple_fmt()
                )
            }
            Expr::MaybeAttribute {
                expr,
                question,
                attr,
                ..
            } => {
                let question_str = if question.is_some() { "?" } else { "" };
                format!(
                    "{}{}.?{}",
                    expr.simple_fmt(),
                    question_str,
                    attr.simple_fmt()
                )
            }
            Expr::Slice {
                start,
                dots: _,
                stop: end,
                step_dots: _,
                step,
            } => {
                let start_str = start.as_ref().map(|s| s.simple_fmt()).unwrap_or_default();
                let end_str = end.as_ref().map(|e| e.simple_fmt()).unwrap_or_default();

                if let Some(step) = step {
                    format!("({}..{}..{})", start_str, end_str, step.simple_fmt())
                } else {
                    format!("({}..{})", start_str, end_str)
                }
            }
            Expr::ParenthesizedBlock { body, .. } => {
                format!("Block({})", body.simple_fmt())
            }
            Expr::Mapping { listing } => listing.simple_fmt(),
            Expr::Await { expr, .. } => {
                format!("(await {})", expr.simple_fmt())
            }
            Expr::Yield { expr, from_kw, .. } => {
                format!(
                    "(yield {}{})",
                    expr.simple_fmt(),
                    from_kw.map(|_| " from").unwrap_or_default()
                )
            }
            Expr::Memo { async_kw, body, .. } => {
                let async_str = if async_kw.is_some() { "async " } else { "" };
                format!("({}memo {})", async_str, body.simple_fmt())
            }
            Expr::Match {
                scrutinee, cases, ..
            } => {
                let cases_str = cases
                    .iter()
                    .map(|case| case.simple_fmt())
                    .collect::<Vec<_>>()
                    .join("; ");
                format!("({} match: {})", scrutinee.simple_fmt(), cases_str)
            }
            Expr::ClassicMatch {
                scrutinee, cases, ..
            } => {
                let cases_str = cases
                    .iter()
                    .map(|case| case.simple_fmt())
                    .collect::<Vec<_>>()
                    .join("; ");
                format!("(match {}: {})", scrutinee.simple_fmt(), cases_str)
            }
            Expr::Matches {
                lhs,
                not_kw,
                pattern,
                ..
            } => {
                let not_str = if not_kw.is_some() { " not" } else { "" };
                format!(
                    "({}{} matches {})",
                    lhs.simple_fmt(),
                    not_str,
                    pattern.simple_fmt()
                )
            }
            Expr::Class { args, body, .. } => {
                let args_str = args
                    .iter()
                    .map(|arg| arg.simple_fmt())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("class{} {})", args_str, body.simple_fmt())
            }
            Expr::With {
                pattern,
                value,
                body,
                ..
            } => {
                format!(
                    "with {} = {}: {}",
                    pattern.simple_fmt(),
                    value.simple_fmt(),
                    body.simple_fmt()
                )
            }
            Expr::MethodCall {
                expr,
                question,
                method,
                args,
                ..
            } => {
                let question_str = if question.is_some() { "?" } else { "" };
                format!(
                    "{}{}.{}({})",
                    expr.simple_fmt(),
                    question_str,
                    method.simple_fmt(),
                    args.simple_fmt()
                )
            }
            Expr::Checked {
                expr,
                except_kw,
                pattern,
                ..
            } => {
                let except_str = if let Some(pattern) = pattern {
                    format!(" except {}", pattern.simple_fmt())
                } else if except_kw.is_some() {
                    " except".to_string()
                } else {
                    String::new()
                };
                format!("try {}{}", expr.simple_fmt(), except_str)
            }
            Expr::Fn { arg, body, .. } => {
                format!("{} => {}", arg.simple_fmt(), body.simple_fmt())
            }
            Expr::ParenthesizedFn { args, body, .. } => {
                format!("{} => {}", args.simple_fmt(), body.simple_fmt())
            }
            Expr::Fstr {
                begin,
                head,
                parts,
                end,
            } => {
                let mut result = begin.token.simple_fmt();
                result.push_str(&head.token.simple_fmt());

                for (fmt_expr, inner) in parts {
                    result.push_str(&format!("{{{}}}", fmt_expr.simple_fmt()));
                    result.push_str(&inner.token.simple_fmt());
                }

                result.push_str(&end.token.simple_fmt());
                result
            }
            Expr::Decorated {
                expr, decorator, ..
            } => {
                format!("{} & {}", expr.simple_fmt(), decorator.simple_fmt())
            }
        }
    }
}

impl<'src, 'tok> SimpleFmt for SStmt<'src, 'tok> {
    fn simple_fmt(&self) -> String {
        self.value.simple_fmt()
    }
}

impl<'src, 'tok> SimpleFmt for SStmtInner<'src, 'tok> {
    fn simple_fmt(&self) -> String {
        match self {
            Stmt::Expr { expr } => expr.simple_fmt(),
            Stmt::Assign { lhs, rhs, op, .. } => {
                let op_str = if let Some(op) = op {
                    format!(" {}= ", format!("{:?}", op).to_lowercase())
                } else {
                    " = ".to_string()
                };
                format!("{}{}{}", lhs.simple_fmt(), op_str, rhs.simple_fmt())
            }
            Stmt::PatternAssign {
                lhs, rhs, modifier, ..
            } => {
                let mod_str = if let Some(modifier) = modifier {
                    format!("{} ", modifier.simple_fmt())
                } else {
                    String::new()
                };
                format!("{}{} = {}", mod_str, lhs.simple_fmt(), rhs.simple_fmt())
            }
            Stmt::Decl { modifier, names } => {
                let names_str = names
                    .iter()
                    .map(|(name, _)| name.simple_fmt())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{} {}", modifier.simple_fmt(), names_str)
            }
            Stmt::While { cond, body, .. } => {
                format!("while {}: {}", cond.simple_fmt(), body.simple_fmt())
            }
            Stmt::For {
                pattern,
                iter,
                body,
                ..
            } => {
                format!(
                    "for {} in {}: {}",
                    pattern.simple_fmt(),
                    iter.simple_fmt(),
                    body.simple_fmt()
                )
            }
            Stmt::Break { .. } => "break".to_string(),
            Stmt::Continue { .. } => "continue".to_string(),
            Stmt::Return { expr, .. } => {
                format!("return {}", expr.simple_fmt())
            }
            Stmt::Raise { expr, .. } => {
                if let Some(expr) = expr {
                    format!("raise {}", expr.simple_fmt())
                } else {
                    "raise".to_string()
                }
            }
            Stmt::Import { export, tree, .. } => {
                let export_str = if export.is_some() { "export " } else { "" };
                format!("{}import {}", export_str, tree.simple_fmt())
            }
            Stmt::Error { .. } => "/* parse error */".to_string(),
        }
    }
}

impl<'src, 'tok> SimpleFmt for ImportTree<STree<'src, 'tok>> {
    fn simple_fmt(&self) -> String {
        let dots_str = self
            .dots
            .value
            .iter()
            .map(|(x, _)| x.token.simple_fmt())
            .collect::<Vec<_>>()
            .join("");
        let trunk_str = self
            .trunk
            .iter()
            .map(|(ident, _)| ident.simple_fmt() + ".")
            .collect::<Vec<_>>()
            .join("");
        let leaf_str = self.leaf.value.simple_fmt();

        if !trunk_str.is_empty() {
            format!("{}{}.{}", dots_str, trunk_str, leaf_str)
        } else {
            format!("{}{}", dots_str, leaf_str)
        }
    }
}

impl<'src, 'tok> SimpleFmt for ImportLeaf<STree<'src, 'tok>> {
    fn simple_fmt(&self) -> String {
        match self {
            ImportLeaf::Multi(listing) => listing.simple_fmt(),
            ImportLeaf::Single { name, alias } => {
                if let Some((_, alias_name)) = alias {
                    format!("{} as {}", name.simple_fmt(), alias_name.simple_fmt())
                } else {
                    name.simple_fmt()
                }
            }
            ImportLeaf::This { alias, .. } => {
                if let Some((_, alias_name)) = alias {
                    format!("this as {}", alias_name.simple_fmt())
                } else {
                    "this".to_string()
                }
            }
            ImportLeaf::Star { .. } => "*".to_string(),
        }
    }
}

impl<'src, 'tok> SimpleFmt for ListItem<STree<'src, 'tok>> {
    fn simple_fmt(&self) -> String {
        match self {
            ListItem::Item { expr } => expr.simple_fmt(),
            ListItem::Spread { expr, .. } => format!("*{}", expr.simple_fmt()),
        }
    }
}

impl<'src, 'tok> SimpleFmt for CallItem<STree<'src, 'tok>> {
    fn simple_fmt(&self) -> String {
        match self {
            CallItem::Arg { expr } => expr.simple_fmt(),
            CallItem::Kwarg { name, expr, .. } => {
                format!("{}={}", name.simple_fmt(), expr.simple_fmt())
            }
            CallItem::ArgSpread { expr, .. } => format!("*{}", expr.simple_fmt()),
            CallItem::KwargSpread { expr, .. } => format!("**{}", expr.simple_fmt()),
        }
    }
}

impl<'src, 'tok> SimpleFmt for MappingKey<STree<'src, 'tok>> {
    fn simple_fmt(&self) -> String {
        match self {
            MappingKey::Ident { token } => token.simple_fmt(),
            MappingKey::Unit { .. } => "()".to_string(),
            MappingKey::Literal { token } => token.simple_fmt(),
            MappingKey::Expr { key, .. } => format!("({})", key.simple_fmt()),
            MappingKey::ParenthesizedBlock { body, .. } => {
                format!("Block({})", body.simple_fmt())
            }
            MappingKey::Fstr {
                begin,
                head,
                parts,
                end,
            } => {
                let mut result = begin.simple_fmt();
                result.push_str(&head.simple_fmt());
                for (fmt_expr, inner) in parts {
                    result.push_str(&format!("{{{}}}", fmt_expr.simple_fmt()));
                    result.push_str(&inner.simple_fmt());
                }
                result.push_str(&end.simple_fmt());
                result
            }
        }
    }
}

impl<'src, 'tok> SimpleFmt for PatternMappingKey<STree<'src, 'tok>> {
    fn simple_fmt(&self) -> String {
        match self {
            PatternMappingKey::Ident { token } => token.simple_fmt(),
            PatternMappingKey::Unit { .. } => "()".to_string(),
            PatternMappingKey::Literal { token } => token.simple_fmt(),
            PatternMappingKey::Expr { key, .. } => format!("({})", key.simple_fmt()),
        }
    }
}

impl<'src, 'tok> SimpleFmt for MappingItem<STree<'src, 'tok>> {
    fn simple_fmt(&self) -> String {
        match self {
            MappingItem::Ident { ident } => ident.simple_fmt(),
            MappingItem::Item { key, value, .. } => {
                format!("{}: {}", key.value.simple_fmt(), value.simple_fmt())
            }
            MappingItem::Spread { expr, .. } => {
                format!("**{}", expr.simple_fmt())
            }
        }
    }
}

impl<'src, 'tok> SimpleFmt for MatchCase<STree<'src, 'tok>> {
    fn simple_fmt(&self) -> String {
        let guard_str = if let Some((_, guard_expr)) = &self.guard {
            format!(" if {}", guard_expr.simple_fmt())
        } else {
            String::new()
        };

        format!(
            "{}{} => {}",
            self.pattern.simple_fmt(),
            guard_str,
            self.body.simple_fmt()
        )
    }
}

impl<'src, 'tok> SimpleFmt for ArgDefItem<STree<'src, 'tok>> {
    fn simple_fmt(&self) -> String {
        match self {
            ArgDefItem::Arg { pattern, default } => {
                if let Some((_, default_expr)) = default {
                    format!("{}={}", pattern.simple_fmt(), default_expr.simple_fmt())
                } else {
                    pattern.simple_fmt()
                }
            }
            ArgDefItem::ArgSpread { name, .. } => {
                format!("*{}", name.simple_fmt())
            }
            ArgDefItem::KwargSpread { name, .. } => {
                format!("**{}", name.simple_fmt())
            }
            ArgDefItem::PosOnlyMarker { .. } => "/".to_string(),
            ArgDefItem::KwOnlyMarker { .. } => "*".to_string(),
        }
    }
}

impl<'src, 'tok> SimpleFmt for SStmts<'src, 'tok> {
    fn simple_fmt(&self) -> String {
        self.value
            .iter()
            .map(|stmt| stmt.value.simple_fmt())
            .collect::<Vec<_>>()
            .join("; ")
    }
}

impl<'src, 'tok> SimpleFmt for InducedBlock<STree<'src, 'tok>> {
    fn simple_fmt(&self) -> String {
        let stmts = match &self {
            InducedBlock::Block { body, .. } => body,
            InducedBlock::Inline { stmt, .. } => &vec![stmt.clone()].spanned(stmt.span),
        };

        format!("Block({})", stmts.simple_fmt())
    }
}

impl<'src, 'tok> SimpleFmt for FmtExpr<STree<'src, 'tok>> {
    fn simple_fmt(&self) -> String {
        if let Some(_) = &self.fmt {
            format!("{}!...", self.expr.simple_fmt())
        } else {
            self.expr.simple_fmt()
        }
    }
}

impl<'src, 'tok> SimpleFmt for SPattern<'src, 'tok> {
    fn simple_fmt(&self) -> String {
        self.value.simple_fmt()
    }
}

impl<'src, 'tok> SimpleFmt for Pattern<STree<'src, 'tok>> {
    fn simple_fmt(&self) -> String {
        match self {
            Pattern::Capture { name } => name.simple_fmt(),
            Pattern::Value { expr, .. } => {
                format!(".{}", expr.simple_fmt())
            }
            Pattern::As { pattern, name, .. } => {
                format!("{} as {}", pattern.simple_fmt(), name.simple_fmt())
            }
            Pattern::Or { head, rest } => {
                let mut parts = vec![head.simple_fmt()];
                for (_, pattern) in rest {
                    parts.push(pattern.simple_fmt());
                }
                parts.join(" | ")
            }
            Pattern::Literal { token } => token.simple_fmt(),
            Pattern::Sequence { listing } => listing.simple_fmt(),
            Pattern::TupleSequence { kind } => match kind {
                PatternTupleSequenceKind::Unit(_, _) => "()".to_string(),
                PatternTupleSequenceKind::Listing(items) => items
                    .iter()
                    .map(|item| item.item.simple_fmt())
                    .collect::<Vec<_>>()
                    .join(", "),
            },
            Pattern::Mapping { listing } => listing.simple_fmt(),
            Pattern::Class { expr, items, .. } => {
                format!("{}({})", expr.simple_fmt(), items.simple_fmt())
            }
            Pattern::Parenthesized { pattern, .. } => {
                format!("({})", pattern.simple_fmt())
            }
        }
    }
}

impl<'src, 'tok> SimpleFmt for PatternSequenceItem<STree<'src, 'tok>> {
    fn simple_fmt(&self) -> String {
        match self {
            PatternSequenceItem::Item { pattern } => pattern.simple_fmt(),
            PatternSequenceItem::Spread { name, .. } => {
                format!("*{}", name.simple_fmt())
            }
        }
    }
}

impl<'src, 'tok> SimpleFmt for PatternMappingItem<STree<'src, 'tok>> {
    fn simple_fmt(&self) -> String {
        match self {
            PatternMappingItem::Ident { name } => name.simple_fmt(),
            PatternMappingItem::Item { key, pattern, .. } => {
                format!("{}: {}", key.value.simple_fmt(), pattern.simple_fmt())
            }
            PatternMappingItem::Spread { name, .. } => {
                format!("**{}", name.simple_fmt())
            }
        }
    }
}

impl<'src, 'tok> SimpleFmt for PatternClassItem<STree<'src, 'tok>> {
    fn simple_fmt(&self) -> String {
        match self {
            PatternClassItem::Item { pattern } => pattern.simple_fmt(),
            PatternClassItem::Kw { name, pattern, .. } => {
                format!("{}={}", name.simple_fmt(), pattern.simple_fmt())
            }
        }
    }
}

impl<'src, 'tok, T> SListing<'src, 'tok, T> {
    pub fn simple_fmt(&self) -> String
    where
        T: SimpleFmt,
    {
        let (begin, items, end) = match self {
            SListing::Block {
                begin, items, end, ..
            }
            | SListing::Inline {
                begin, items, end, ..
            } => (begin.simple_fmt(), items, end.simple_fmt()),
        };

        format!(
            "{}{}{}",
            begin,
            items
                .iter()
                .map(|item| format!(
                    "{}{} ",
                    item.item.simple_fmt(),
                    item.separator
                        .map(|x| x.simple_fmt())
                        .unwrap_or("".to_string())
                ))
                .collect::<Vec<_>>()
                .join(""),
            end
        )
    }
}

impl<'src> SimpleFmt for SToken<'src> {
    fn simple_fmt(&self) -> String {
        self.token.simple_fmt()
    }
}

impl<'src> SimpleFmt for Token<'src> {
    fn simple_fmt(&self) -> String {
        match self {
            Token::Ident(s) => s.to_string(),
            Token::Int(s) => s.to_string(),
            Token::Str(orig, _) => orig.to_string(),
            Token::FstrBegin(s) => s.to_string(),
            Token::FstrEnd(s) => s.to_string(),
            Token::VerbatimFstrBegin(s) => s.to_string(),
            Token::VerbatimFstrEnd(s) => s.to_string(),
            Token::FstrInner(s, _) => s.to_string(),
            Token::Bool(b) => b.to_string(),
            Token::None => "None".to_string(),
            Token::Symbol(s) => s.to_string(),
            Token::Kw(s) => s.to_string(),
            _ => format!("{:?}", self),
        }
    }
}
