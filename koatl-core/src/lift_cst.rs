use std::borrow::Cow;

use crate::ast;
use crate::ast::{Indirect, IntoIndirect};
use koatl_parser::cst::{Spannable, Spanned};
use koatl_parser::lexer::SToken;
use koatl_parser::{Token, cst};

trait STokenExt<'src> {
    fn lift(&self) -> Spanned<ast::Ident<'src>>;
}

impl<'src> STokenExt<'src> for SToken<'src> {
    fn lift(&self) -> Spanned<ast::Ident<'src>> {
        match &self.token {
            Token::Ident(s) => return ast::Ident(Cow::Borrowed(s)).spanned(self.span),
            _ => panic!(),
        }
    }
}

trait SExprExt<'src> {
    fn lift(&self) -> Indirect<ast::SExpr<'src>>;
}

impl<'src, 'tok> SExprExt<'src> for cst::SExpr<'src, 'tok> {
    fn lift(&self) -> Indirect<ast::SExpr<'src>> {
        match &self.value {
            cst::Expr::Attribute { expr, attr, .. } => {
                ast::Expr::Attribute(expr.lift(), attr.lift())
            }
            _ => panic!(),
        }
        .spanned(self.span)
        .indirect()
    }
}

pub fn lift_cst<'src, 'tok>(cst: &cst::SExpr<'src, 'tok>) -> Indirect<ast::SExpr<'src>> {
    cst.lift()
}
