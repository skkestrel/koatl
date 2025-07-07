#![allow(dead_code)]

use crate::lexer::*;
use chumsky::{extra::ParserExtra, input::ValueInput, prelude::*};

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
pub struct Ident<'a>(pub &'a str);

#[derive(Debug)]
pub struct SingleDecl<'a> {
    pub ident: Ident<'a>,
    pub typ: Option<Expr<'a>>,
}

#[derive(Debug)]
pub enum Decl<'a> {
    Single(SingleDecl<'a>),
    Unpack(Box<Decl<'a>>),
}

#[derive(Debug)]
pub enum Stmt<'a> {
    Global(Vec<Ident<'a>>),
    Nonlocal(Vec<Ident<'a>>),
    Assign(Decl<'a>, Expr<'a>),
    Return(Expr<'a>),
    Expr(Expr<'a>),
    While(Expr<'a>, Block<'a>),
    For(Decl<'a>, Expr<'a>, Block<'a>),
    Import(Vec<Ident<'a>>),
    Raise(Expr<'a>),
    Break,
    Continue,
    Err,
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
    Arg(SingleDecl<'a>),
    DefaultArg(SingleDecl<'a>, Expr<'a>),
    ArgSpread(SingleDecl<'a>),
    KwargSpread(SingleDecl<'a>),
}

#[derive(Debug)]
pub enum Block<'a> {
    Stmts(Vec<Stmt<'a>>),
    Expr(Box<Expr<'a>>),
}

#[derive(Debug)]
pub enum Expr<'a> {
    Literal(Literal<'a>),
    Ident(Ident<'a>),
    Unary(UnaryOp, Box<Expr<'a>>),
    Binary(BinaryOp, Box<Expr<'a>>, Box<Expr<'a>>),
    List(Vec<ListItem<'a>>),
    Mapping(Vec<MappingItem<'a>>),

    If(Box<Expr<'a>>, Block<'a>, Option<Block<'a>>),
    Match(Box<Expr<'a>>, Vec<(Expr<'a>, Block<'a>)>),
    Class(Ident<'a>, Vec<CallItem<'a>>, Block<'a>),

    Call(Box<Expr<'a>>, Vec<CallItem<'a>>),
    Attribute(Box<Expr<'a>>, Ident<'a>),
    Pipe(Box<Expr<'a>>, Box<Expr<'a>>),
    Index(Box<Expr<'a>>, Vec<Expr<'a>>),

    Fn(Vec<ArgItem<'a>>, Block<'a>),
    FmtStr(Vec<Box<FmtExpr<'a>>>),
}

fn enumeration<'tokens, 'src: 'tokens, I, O, E, ItemParser>(
    item_parser: ItemParser,
) -> impl Parser<'tokens, I, Vec<O>, E> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
    E: ParserExtra<'tokens, I, Error = Rich<'tokens, Token<'src>, Span>>,
    ItemParser: Parser<'tokens, I, O, E> + Clone,
{
    just(Token::Continuation)
        .repeated()
        .ignore_then(item_parser)
        .separated_by(choice((
            just(Token::Symbol(","))
                .padded_by(just(Token::Continuation).repeated())
                .ignored(),
            just(Token::Continuation).repeated().at_least(1).ignored(),
        )))
        .allow_trailing()
        .collect()
        .labelled("enumeration")
}

pub trait ParserExt<'tokens, 'src: 'tokens, I, O, E>: Parser<'tokens, I, O, E>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
    E: ParserExtra<'tokens, I>,
{
    fn pad_cont(self) -> impl Parser<'tokens, I, O, E> + Clone + Sized
    where
        Self: Sized + Clone,
    {
        self.padded_by(just(Token::Continuation).repeated())
    }
}

impl<'tokens, 'src: 'tokens, I, O, E, P> ParserExt<'tokens, 'src, I, O, E> for P
where
    P: Parser<'tokens, I, O, E> + Sized,
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
    E: ParserExtra<'tokens, I>,
{
}

pub fn parser<'tokens, 'src: 'tokens, TInput>()
-> impl Parser<'tokens, TInput, Block<'src>, extra::Err<Rich<'tokens, Token<'src>, Span>>>
where
    TInput: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    let just_symbol = |s: &'static str| just(Token::Symbol(s));

    let mut stmt = chumsky::recursive::Recursive::declare();
    let mut expr = chumsky::recursive::Recursive::declare();

    let block = stmt
        .clone()
        .repeated()
        .collect()
        .delimited_by(just_symbol("{"), just_symbol("}"))
        .map(Block::Stmts);

    let block_or_expr = choice((
        block.clone(),
        expr.clone().map(|x| Block::Expr(Box::new(x))),
    ));

    let single_decl = select! {
        Token::Ident(s) => SingleDecl { ident: Ident(s), typ: None },
    };

    let multi_decl = single_decl.clone().map(Decl::Single);

    let literal = select! {
        Token::Num(s) => Expr::Literal(Literal::Num(s)),
        Token::Str(s) => Expr::Literal(Literal::Str(s)),
    }
    .labelled("literal");

    let ident = select! {
        Token::Ident(s) => Ident(s),
    }
    .labelled("identifier");

    let list = enumeration(choice((
        just_symbol("*")
            .ignore_then(expr.clone())
            .map(ListItem::Spread),
        expr.clone().map(ListItem::Item),
    )))
    .map(Expr::List)
    .delimited_by(just_symbol("["), just_symbol("]"))
    .labelled("list");

    let mapping = enumeration(choice((
        just_symbol("**")
            .ignore_then(expr.clone())
            .map(MappingItem::Spread),
        expr.clone()
            .then_ignore(just_symbol(":"))
            .then(expr.clone())
            .map(|(key, value)| MappingItem::Item(key, value)),
    )))
    .map(Expr::Mapping)
    .delimited_by(just(Token::Symbol("[")), just(Token::Symbol("]")))
    .labelled("mapping");

    let atom = choice((
        literal,
        ident.map(Expr::Ident),
        list,
        mapping,
        expr.clone()
            .delimited_by(just(Token::Symbol("(")), just(Token::Symbol(")"))),
    ))
    .pad_cont();

    let call = enumeration(choice((
        just_symbol("*")
            .ignore_then(expr.clone())
            .map(CallItem::ArgSpread),
        just_symbol("**")
            .ignore_then(expr.clone())
            .map(CallItem::KwargSpread),
        ident
            .clone()
            .then_ignore(just_symbol("="))
            .then(expr.clone())
            .map(|(key, value)| CallItem::Kwarg(key, value)),
        expr.clone().map(CallItem::Arg),
    )))
    .delimited_by(just(Token::Symbol("(")), just(Token::Symbol(")")))
    .labelled("call");

    let arg_list = enumeration(choice((
        just_symbol("*")
            .ignore_then(single_decl.clone())
            .map(|x| ArgItem::ArgSpread(x)),
        just_symbol("**")
            .ignore_then(single_decl.clone())
            .map(ArgItem::KwargSpread),
        single_decl
            .clone()
            .then_ignore(just_symbol("="))
            .then(expr.clone())
            .map(|(key, value)| ArgItem::DefaultArg(key, value)),
        single_decl.clone().map(ArgItem::Arg),
    )))
    .labelled("arg-list");

    let getitem = enumeration(choice((expr.clone(),)))
        .delimited_by(just(Token::Symbol("[")), just(Token::Symbol("]")))
        .labelled("getitem");

    let attribute = just_symbol(".").ignore_then(ident);

    let then = just_symbol(".").ignore_then(
        expr.clone()
            .delimited_by(just_symbol("("), just_symbol(")")),
    );

    enum Postfix<'a> {
        Call(Vec<CallItem<'a>>),
        GetItem(Vec<Expr<'a>>),
        Then(Expr<'a>),
        Attribute(Ident<'a>),
    }

    let postfix = atom.foldl(
        choice((
            call.map(Postfix::Call),
            getitem.map(Postfix::GetItem),
            attribute.map(Postfix::Attribute),
            then.map(Postfix::Then),
        ))
        .repeated(),
        |expr, op: Postfix<'src>| match op {
            Postfix::Call(args) => Expr::Call(Box::new(expr), args),
            Postfix::GetItem(args) => Expr::Index(Box::new(expr), args),
            Postfix::Attribute(attr) => Expr::Attribute(Box::new(expr), attr),
            Postfix::Then(rhs) => Expr::Pipe(Box::new(expr), Box::new(rhs)),
        },
    );

    let unary = select! {
        Token::Symbol("+") => UnaryOp::Pos,
        Token::Symbol("-") => UnaryOp::Neg,
        Token::Symbol("~") => UnaryOp::Inv,
    }
    .repeated()
    .foldr(postfix, |op, rhs| Expr::Unary(op, Box::new(rhs)));

    let if_ = just(Token::Kw("if"))
        .pad_cont()
        .ignore_then(group((
            expr.clone().then_ignore(just_symbol(";").or_not()),
            block_or_expr.clone(),
            group((
                one_of([Token::Continuation, Token::Eol]).or_not(),
                just(Token::Kw("else")),
                just_symbol(";").or_not(),
            ))
            .ignore_then(block_or_expr.clone())
            .or_not(),
        )))
        .map(|(cond, if_, else_)| Expr::If(Box::new(cond), if_, else_));

    let case_ = expr
        .clone()
        .then_ignore(just_symbol(";"))
        .then(block_or_expr.clone())
        .then_ignore(just(Token::Eol))
        .map(|(pattern, body)| (pattern, body));

    let match_ = just(Token::Kw("match"))
        .pad_cont()
        .ignore_then(expr.clone())
        .then_ignore(just_symbol(";"))
        .then(
            case_
                .repeated()
                .collect()
                .delimited_by(just_symbol("{"), just_symbol("}")),
        )
        .map(|(scrutinee, cases)| Expr::Match(Box::new(scrutinee), cases));

    let fn_ = just(Token::Symbol("\\"))
        .pad_cont()
        .ignore_then(arg_list)
        .then_ignore(just_symbol(";"))
        .then(block_or_expr.clone())
        .map(|(args, body)| Expr::Fn(args, body))
        .labelled("function definition");

    expr.define(choice((unary, if_, match_, fn_)).labelled("expression"));

    let expr_stmt = expr
        .clone()
        .map(Stmt::Expr)
        .then_ignore(just(Token::Eol))
        .labelled("expression statement");

    let assign_stmt = multi_decl
        .pad_cont()
        .then_ignore(just_symbol("=").pad_cont())
        .then(expr.clone())
        .map(|(decl, expr)| Stmt::Assign(decl, expr))
        .then_ignore(just(Token::Eol))
        .labelled("assignment statement");

    let while_stmt = just(Token::Kw("while"))
        .pad_cont()
        .ignore_then(expr.clone())
        .then_ignore(just_symbol(";").or_not())
        .then(block.clone())
        .map(|(cond, body)| Stmt::While(cond, body))
        .labelled("while statement");

    let for_stmt = just(Token::Kw("for"))
        .pad_cont()
        .ignore_then(group((
            multi_decl
                .clone()
                .pad_cont()
                .then_ignore(just_symbol("in").pad_cont()),
            expr.clone().then_ignore(just_symbol(";").or_not()),
            block.clone(),
        )))
        .map(|(decl, iter, body)| Stmt::For(decl, iter, body))
        .labelled("for statement");

    stmt.define(choice((expr_stmt, assign_stmt, while_stmt, for_stmt)).labelled("statement"));

    stmt.repeated()
        .collect::<Vec<_>>()
        .map(Block::Stmts)
        .delimited_by(just_symbol("{"), just_symbol("}").then(just(Token::Eol)))
}
