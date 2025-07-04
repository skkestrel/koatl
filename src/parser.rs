#![allow(dead_code)]

use crate::lexer::*;
use chumsky::{input::ValueInput, prelude::*};

#[derive(Debug, Copy, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Mod,
    MatMul,
    Div,
    Exp,
    Lt,
    Leq,
    Gt,
    Geq,
    Eq,
    Neq,
}

#[derive(Debug, Copy, Clone)]
pub enum UnaryOp {
    Inv,
    Pos,
    Neg,
}

#[derive(Debug)]
pub struct Ident<'a>(&'a str);

#[derive(Debug)]
pub enum Stmt<'a> {
    Global(Vec<Ident<'a>>),
    Nonlocal(Vec<Ident<'a>>),
    Assign(Ident<'a>, Expr<'a>),
    Return(Expr<'a>),
    Expr(Expr<'a>),
}

#[derive(Debug)]
pub enum Literal<'a> {
    Num(&'a str),
    Str(&'a str),
}

#[derive(Debug)]
pub struct FmtExpr<'a> {
    pub expr: Expr<'a>,
    pub fmt: Option<&'a str>,
}

#[derive(Debug)]
pub enum ListItem<'a> {
    Item(Expr<'a>),
    Spread(Expr<'a>),
}

#[derive(Debug)]
pub enum MappingItem<'a> {
    Item(Expr<'a>, Expr<'a>),
    Spread(Expr<'a>),
}

#[derive(Debug)]
pub enum CallItem<'a> {
    Arg(Expr<'a>),
    Kwarg(Ident<'a>, Expr<'a>),
    ArgSpread(Expr<'a>),
    KwargSpread(Expr<'a>),
}

#[derive(Debug)]
pub enum ArgItem<'a> {
    Arg(Ident<'a>),
    DefaultArg(Ident<'a>, Expr<'a>),
    ArgSpread(Ident<'a>),
    KwargSpread(Ident<'a>),
}

#[derive(Debug)]
pub enum Expr<'a> {
    Literal(Literal<'a>),
    Placeholder,
    Ident(Ident<'a>),
    Unary(UnaryOp, Box<Expr<'a>>),
    Binary(BinaryOp, Box<Expr<'a>>, Box<Expr<'a>>),
    List(Vec<ListItem<'a>>),
    Mapping(Vec<MappingItem<'a>>),
    Call(Box<Expr<'a>>, Vec<CallItem<'a>>),
    Attribute(Box<Expr<'a>>, Ident<'a>),
    Index(Box<Expr<'a>>, Vec<Expr<'a>>),
    Pipe(Box<Expr<'a>>, Box<Expr<'a>>),

    Fn(Vec<ArgItem<'a>>, Vec<Stmt<'a>>),
    FmtStr(Vec<Box<FmtExpr<'a>>>),
}

pub fn parser<'tokens, 'src: 'tokens, TInput>()
-> impl Parser<'tokens, TInput, Stmt<'src>, extra::Err<Rich<'tokens, Token<'src>, Span>>>
where
    TInput: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    let expr = recursive(|expr| {
        let literal = select! {
            Token::Num(s) => Expr::Literal(Literal::Num(s)),
            Token::Str(s) => Expr::Literal(Literal::Str(s)),
        }
        .labelled("literal");

        let just_symbol = |s: &'static str| just(Token::Symbol(s));

        let ident = select! {
            Token::Ident(s) => Ident(s),
        }
        .labelled("identifier");

        let placeholder = just_symbol("$")
            .map(|_| Expr::Placeholder)
            .labelled("placeholder");

        let separator = choice((
            just_symbol(",")
                .padded_by(just(Token::Separator).repeated())
                .to(()),
            just(Token::Separator).repeated().at_least(1).to(()),
        ))
        .labelled("separator");

        let list = just(Token::Separator)
            .repeated()
            .ignore_then(choice((
                just_symbol("*")
                    .ignore_then(expr.clone())
                    .map(ListItem::Spread),
                expr.clone().map(ListItem::Item),
            )))
            .separated_by(separator.clone())
            .allow_trailing()
            .collect()
            .map(Expr::List)
            .delimited_by(just_symbol("["), just_symbol("]"))
            .labelled("list");

        let mapping = just(Token::Separator)
            .repeated()
            .ignore_then(choice((
                just_symbol("**")
                    .ignore_then(expr.clone())
                    .map(MappingItem::Spread),
                expr.clone()
                    .then_ignore(just_symbol(":"))
                    .then(expr.clone())
                    .map(|(key, value)| MappingItem::Item(key, value)),
            )))
            .separated_by(separator.clone())
            .allow_trailing()
            .collect()
            .map(Expr::Mapping)
            .delimited_by(just(Token::Symbol("[")), just(Token::Symbol("]")))
            .labelled("mapping");

        let call_list = just(Token::Separator)
            .repeated()
            .ignore_then(choice((
                just_symbol("*")
                    .ignore_then(expr.clone())
                    .map(CallItem::ArgSpread),
                just_symbol("**")
                    .ignore_then(expr.clone())
                    .map(CallItem::KwargSpread),
                expr.clone().map(CallItem::Arg),
                ident
                    .clone()
                    .then_ignore(just_symbol("="))
                    .then(expr.clone())
                    .map(|(key, value)| CallItem::Kwarg(key, value)),
            )))
            .separated_by(separator.clone())
            .allow_trailing()
            .collect()
            .delimited_by(just(Token::Symbol("(")), just(Token::Symbol(")")))
            .labelled("arg-list");

        let atom = choice((
            literal,
            placeholder,
            ident.map(Expr::Ident),
            list,
            mapping,
            expr.delimited_by(just(Token::Symbol("(")), just(Token::Symbol(")"))),
        ));

        let call = atom.foldl(call_list.repeated(), |expr, args| {
            Expr::Call(Box::new(expr), args)
        });

        let item_list = just(Token::Separator)
            .repeated()
            .ignore_then(choice((expr.clone().map(CallItem::Arg),)))
            .separated_by(separator.clone())
            .allow_trailing()
            .delimited_by(just(Token::Symbol("(")), just(Token::Symbol(")")))
            .labelled("arg-list");

        let attribute = call.foldl(
            just_symbol(".").ignore_then(ident).repeated(),
            |expr, attr| Expr::Attribute(Box::new(expr), attr),
        );

        // let unary_op = select! {
        //     Token::Symbol("+") => UnaryOp::Pos,
        //     Token::Symbol("-") => UnaryOp::Neg,
        //     Token::Symbol("~") => UnaryOp::Inv,
        // };

        // let unary = unary_op
        //     .repeated()
        //     .fold(atom, |op, rhs| Expr::Unary(op, Box::new(rhs)));

        // let pipe = unary
        //     .then_ignore(just_symbol("|>").then(unary))
        //     .map(|(lhs, rhs)| Expr::Pipe(Box::new(lhs), Box::new(rhs)));

        attribute
    })
    .labelled("expression");

    just(Token::BeginBlock)
        .ignore_then(expr.map(Stmt::Expr))
        .then_ignore(just(Token::EndBlock))
}
