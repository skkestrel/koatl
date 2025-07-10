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

pub type Ident<'a> = &'a str;
pub type SIdent<'a> = Spanned<&'a str>;

#[derive(Debug)]
pub struct ImportStmt<'a> {
    pub trunk: Vec<SIdent<'a>>,
    pub leaves: Vec<(SIdent<'a>, Option<SIdent<'a>>)>,
    pub star: bool,
}

#[derive(Debug)]
pub struct ExceptHandler<'a> {
    pub typ: Option<SExpr<'a>>,
    pub name: Option<SIdent<'a>>,
    pub body: SBlock<'a>,
}

#[derive(Debug)]
pub enum Stmt<'a> {
    Global(Vec<SIdent<'a>>),
    Nonlocal(Vec<SIdent<'a>>),
    Assign(SExpr<'a>, SExpr<'a>),
    Return(SExpr<'a>),
    Expr(SExpr<'a>),
    While(SExpr<'a>, SBlock<'a>),
    For(SExpr<'a>, SExpr<'a>, SBlock<'a>),
    Import(ImportStmt<'a>),
    Try(SBlock<'a>, Vec<ExceptHandler<'a>>, Option<SBlock<'a>>),
    Raise(SExpr<'a>),
    Break,
    Continue,
    Err,
}

pub type SStmt<'a> = Spanned<Stmt<'a>>;

#[derive(Debug)]
pub enum Literal<'a> {
    Num(&'a str),
    Str(&'a str),
}

pub type SLiteral<'a> = Spanned<Literal<'a>>;

#[derive(Debug)]
pub struct FmtExpr<'a> {
    pub expr: SExpr<'a>,
    pub fmt: Option<&'a str>,
}

pub type SFmtExpr<'a> = Spanned<FmtExpr<'a>>;

#[derive(Debug)]
pub enum ListItem<'a> {
    Item(SExpr<'a>),
    Spread(SExpr<'a>),
}

#[derive(Debug)]
pub enum MappingItem<'a> {
    Item(SExpr<'a>, SBlock<'a>),
    Spread(SExpr<'a>),
}

#[derive(Debug)]
pub enum CallItem<'a> {
    Arg(SExpr<'a>),
    Kwarg(SIdent<'a>, SExpr<'a>),
    ArgSpread(SExpr<'a>),
    KwargSpread(SExpr<'a>),
}

pub type SCallItem<'a> = Spanned<CallItem<'a>>;

#[derive(Debug)]
pub enum ArgItem<'a> {
    Arg(SIdent<'a>),
    DefaultArg(SIdent<'a>, SExpr<'a>),
    ArgSpread(SIdent<'a>),
    KwargSpread(SIdent<'a>),
}

pub type SArgItem<'a> = Spanned<ArgItem<'a>>;

#[derive(Debug)]
pub enum Block<'a> {
    Stmts(Vec<SStmt<'a>>),
    Expr(SExpr<'a>),
}

pub type SBlock<'a> = Spanned<Block<'a>>;

#[derive(Debug)]
pub enum Expr<'a> {
    Literal(SLiteral<'a>),
    Ident(SIdent<'a>),
    Unary(UnaryOp, Box<SExpr<'a>>),
    Binary(BinaryOp, Box<SExpr<'a>>, Box<SExpr<'a>>),

    List(Vec<ListItem<'a>>),
    Mapping(Vec<MappingItem<'a>>),
    Slice(
        Option<Box<SExpr<'a>>>,
        Option<Box<SExpr<'a>>>,
        Option<Box<SExpr<'a>>>,
    ),

    If(Box<SExpr<'a>>, Box<SBlock<'a>>, Option<Box<SBlock<'a>>>),
    Match(Box<SExpr<'a>>, Vec<(SExpr<'a>, Box<SBlock<'a>>)>),
    Class(Vec<SCallItem<'a>>, Box<SBlock<'a>>),

    Call(Box<SExpr<'a>>, Vec<SCallItem<'a>>),
    Subscript(Box<SExpr<'a>>, Vec<ListItem<'a>>),
    Attribute(Box<SExpr<'a>>, SIdent<'a>),
    Pipe(Box<SExpr<'a>>, Box<SExpr<'a>>),

    Yield(Box<SExpr<'a>>),
    YieldFrom(Box<SExpr<'a>>),

    Fn(Vec<ArgItem<'a>>, Box<SBlock<'a>>),
    FmtStr(Vec<Box<SFmtExpr<'a>>>),
}

pub type SExpr<'a> = Spanned<Expr<'a>>;

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

    fn spanned(self) -> impl Parser<'tokens, I, Spanned<O>, E> + Clone + Sized
    where
        Self: Sized + Clone,
    {
        self.map_with(|x, e| (x, e.span()))
    }

    fn delimited_by_with_eol(
        self,
        start: impl Parser<'tokens, I, Token<'src>, E> + Clone,
        end: impl Parser<'tokens, I, Token<'src>, E> + Clone,
    ) -> impl Parser<'tokens, I, O, E> + Clone
    where
        Self: Sized + Clone,
    {
        self.delimited_by(start, just(Token::Eol).or_not().then(end))
    }
}

impl<'tokens, 'src: 'tokens, I, O, E, P> ParserExt<'tokens, 'src, I, O, E> for P
where
    P: Parser<'tokens, I, O, E> + Sized,
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
    E: ParserExtra<'tokens, I>,
{
}

const START_BLOCK: Token = Token::Symbol(":");

pub fn parser<'tokens, 'src: 'tokens, TInput>()
-> impl Parser<'tokens, TInput, SBlock<'src>, extra::Err<Rich<'tokens, Token<'src>, Span>>>
where
    TInput: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    let just_symbol = |s: &'static str| just(Token::Symbol(s));

    let mut stmt = chumsky::recursive::Recursive::declare();
    let mut sexpr = chumsky::recursive::Recursive::declare();
    let sstmt = stmt.clone().spanned();

    let block = sstmt
        .clone()
        .repeated()
        .collect()
        .delimited_by(just_symbol("{"), just_symbol("}"))
        .map(Block::Stmts)
        .boxed();

    let sblock = block.clone().spanned().boxed();

    let block_or_expr = choice((block.clone(), sexpr.clone().map(Block::Expr))).boxed();
    let sblock_or_expr = block_or_expr.clone().spanned().boxed();

    let literal = select! {
        Token::Num(s) => Literal::Num(s),
        Token::VerbatimStr(s) => Literal::Str(s),
    }
    .spanned()
    .map(Expr::Literal)
    .labelled("literal");

    let ident = select! {
        Token::Ident(s) => s,
    }
    .spanned()
    .labelled("identifier");

    let list = enumeration(choice((
        just_symbol("*")
            .ignore_then(sexpr.clone())
            .map(ListItem::Spread),
        sexpr.clone().map(ListItem::Item),
    )))
    .delimited_by_with_eol(just_symbol("["), just_symbol("]"))
    .map(Expr::List)
    .labelled("list")
    .boxed();

    let mapping = enumeration(choice((
        just_symbol("**")
            .ignore_then(sexpr.clone())
            .map(MappingItem::Spread),
        sexpr
            .clone()
            .then_ignore(just(START_BLOCK))
            .then(sblock_or_expr.clone())
            .map(|(key, value)| MappingItem::Item(key, value)),
    )))
    .delimited_by_with_eol(just(Token::Symbol("[")), just(Token::Symbol("]")))
    .map(Expr::Mapping)
    .labelled("mapping")
    .boxed();

    let atom = choice((
        literal.spanned(),
        ident.clone().map(Expr::Ident).spanned(),
        list.spanned(),
        mapping.spanned(),
        sexpr
            .clone()
            .delimited_by_with_eol(just(Token::Symbol("(")), just(Token::Symbol(")"))),
    ))
    .boxed();

    enum Postfix<'a> {
        Call(Vec<SCallItem<'a>>),
        Subscript(Vec<ListItem<'a>>),
        Then(SExpr<'a>),
        Attribute(SIdent<'a>),
    }

    let call = enumeration(
        choice((
            just_symbol("*")
                .ignore_then(sexpr.clone())
                .map(CallItem::ArgSpread),
            just_symbol("**")
                .ignore_then(sexpr.clone())
                .map(CallItem::KwargSpread),
            ident
                .clone()
                .then_ignore(just_symbol("="))
                .then(sexpr.clone())
                .map(|(key, value)| CallItem::Kwarg(key, value)),
            sexpr.clone().map(CallItem::Arg),
        ))
        .spanned()
        .boxed(),
    )
    .delimited_by_with_eol(just(Token::Symbol("(")), just(Token::Symbol(")")))
    .map(Postfix::Call)
    .labelled("call")
    .boxed();

    let subscript = enumeration(
        choice((
            just_symbol("*")
                .ignore_then(sexpr.clone())
                .map(ListItem::Spread),
            sexpr.clone().map(ListItem::Item),
        ))
        .boxed(),
    )
    .delimited_by_with_eol(just(Token::Symbol("[")), just(Token::Symbol("]")))
    .map(Postfix::Subscript)
    .labelled("subscript")
    .boxed();

    let attribute = just_symbol(".")
        .pad_cont()
        .ignore_then(ident.clone())
        .map(Postfix::Attribute)
        .labelled("attr");

    let then = just_symbol(".")
        .pad_cont()
        .ignore_then(
            sexpr
                .clone()
                .delimited_by_with_eol(just_symbol("("), just_symbol(")"))
                .map(Postfix::Then),
        )
        .labelled("then")
        .boxed();

    let postfix = atom
        .clone()
        .foldl_with(
            choice((call, subscript, attribute, then)).repeated(),
            |expr, op, e| -> SExpr {
                (
                    match op {
                        Postfix::Call(args) => Expr::Call(Box::new(expr), args),
                        Postfix::Subscript(args) => Expr::Subscript(Box::new(expr), args),
                        Postfix::Attribute(attr) => Expr::Attribute(Box::new(expr), attr),
                        Postfix::Then(rhs) => Expr::Pipe(Box::new(expr), Box::new(rhs)),
                    },
                    e.span(),
                )
            },
        )
        .boxed();

    let unary = select! {
        Token::Symbol("+") => UnaryOp::Pos,
        Token::Symbol("-") => UnaryOp::Neg,
        Token::Symbol("~") => UnaryOp::Inv,
    }
    .repeated()
    .foldr_with(postfix, |op: UnaryOp, rhs: SExpr, e| {
        (Expr::Unary(op, Box::new(rhs)), e.span())
    })
    .boxed();

    fn make_binary_op<'tokens, 'src: 'tokens, I, POp, PArg>(
        arg: PArg,
        op: POp,
    ) -> impl Parser<'tokens, I, SExpr<'src>, extra::Err<Rich<'tokens, Token<'src>, Span>>> + Clone
    where
        PArg: Parser<'tokens, I, SExpr<'src>, extra::Err<Rich<'tokens, Token<'src>, Span>>> + Clone,
        POp: Parser<'tokens, I, BinaryOp, extra::Err<Rich<'tokens, Token<'src>, Span>>> + Clone,
        I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
    {
        arg.clone()
            .foldl_with(op.then(arg).repeated(), |lhs, (op, rhs), e| {
                (Expr::Binary(op, Box::new(lhs), Box::new(rhs)), e.span())
            })
    }

    let binary0 = make_binary_op(
        unary,
        select! {
            Token::Symbol("**") => BinaryOp::Exp,
        },
    );

    let binary1 = make_binary_op(
        binary0,
        select! {
            Token::Symbol("*") => BinaryOp::Mul,
            Token::Symbol("/") => BinaryOp::Div,
            Token::Symbol("%") => BinaryOp::Mod,
            Token::Symbol("@") => BinaryOp::MatMul,
        },
    );

    let binary2 = make_binary_op(
        binary1,
        select! {
            Token::Symbol("+") => BinaryOp::Add,
            Token::Symbol("-") => BinaryOp::Sub,
        },
    );

    let binary3 = make_binary_op(
        binary2,
        select! {
            Token::Symbol("<") => BinaryOp::Lt,
            Token::Symbol("<=") => BinaryOp::Leq,
            Token::Symbol(">") => BinaryOp::Gt,
            Token::Symbol(">=") => BinaryOp::Geq,
            Token::Symbol("==") => BinaryOp::Eq,
            Token::Symbol("!=") => BinaryOp::Neq,
        },
    );

    let binary = binary3.boxed();

    let slice0 = just_symbol("..")
        .ignore_then(binary.clone().or_not())
        .then(just_symbol("..").ignore_then(binary.clone()).or_not())
        .map(|(e1, e2)| Expr::Slice(None, e1.map(Box::new), e2.map(Box::new)))
        .spanned()
        .labelled("slice")
        .boxed();

    let slice1 = binary
        .clone()
        .then_ignore(just_symbol(".."))
        .then(binary.clone().or_not())
        .then(just_symbol("..").ignore_then(binary.clone()).or_not())
        .map(|((e0, e1), e2)| Expr::Slice(Some(Box::new(e0)), e1.map(Box::new), e2.map(Box::new)))
        .spanned()
        .labelled("slice")
        .boxed();

    let if_ = just(Token::Kw("if"))
        .pad_cont()
        .ignore_then(group((
            sexpr.clone().then_ignore(just(START_BLOCK)),
            sblock_or_expr.clone(),
            group((
                one_of([Token::Continuation, Token::Eol]).or_not(),
                just(Token::Kw("else")),
                just(START_BLOCK),
            ))
            .ignore_then(sblock_or_expr.clone())
            .or_not(),
        )))
        .map(|(cond, if_, else_)| Expr::If(Box::new(cond), Box::new(if_), else_.map(Box::new)))
        .spanned()
        .labelled("if")
        .boxed();

    let case_ = sexpr
        .clone()
        .then_ignore(just(START_BLOCK))
        .then(sblock_or_expr.clone())
        .then_ignore(just(Token::Eol))
        .map(|(pattern, body)| (pattern, Box::new(body)))
        .labelled("match-case")
        .boxed();

    let match_ = just(Token::Kw("match"))
        .pad_cont()
        .ignore_then(sexpr.clone())
        .then_ignore(just(START_BLOCK))
        .then(
            case_
                .repeated()
                .collect()
                .delimited_by_with_eol(just_symbol("{"), just_symbol("}")),
        )
        .map(|(scrutinee, cases)| Expr::Match(Box::new(scrutinee), cases))
        .spanned()
        .labelled("match")
        .boxed();

    let class_ = just(Token::Kw("class"))
        .pad_cont()
        .ignore_then(
            enumeration(
                choice((
                    ident
                        .clone()
                        .then_ignore(just_symbol("="))
                        .then(sexpr.clone())
                        .map(|(key, value)| CallItem::Kwarg(key, value)),
                    sexpr.clone().map(CallItem::Arg),
                ))
                .spanned()
                .boxed(),
            )
            .delimited_by_with_eol(just(Token::Symbol("(")), just(Token::Symbol(")")))
            .or_not(),
        )
        .then_ignore(just(START_BLOCK))
        .then(sblock.clone())
        .map(|(arglist, block)| Expr::Class(arglist.unwrap_or_else(|| Vec::new()), Box::new(block)))
        .spanned()
        .labelled("class")
        .boxed();

    let arg_list = enumeration(choice((
        just_symbol("*")
            .ignore_then(ident.clone())
            .map(|x| ArgItem::ArgSpread(x)),
        just_symbol("**")
            .ignore_then(ident.clone())
            .map(ArgItem::KwargSpread),
        ident
            .clone()
            .then_ignore(just_symbol("="))
            .then(sexpr.clone())
            .map(|(key, value)| ArgItem::DefaultArg(key, value)),
        ident.clone().map(ArgItem::Arg),
    )))
    .labelled("arg-list")
    .boxed();

    let fn_ = choice((
        arg_list
            .clone()
            .delimited_by_with_eol(just_symbol("("), just_symbol(")")),
        ident.clone().map(|x| vec![ArgItem::Arg(x)]),
    ))
    .pad_cont()
    .then_ignore(just_symbol("=>"))
    .then(sblock_or_expr.clone())
    .map(|(args, body)| Expr::Fn(args, Box::new(body)))
    .spanned()
    .labelled("function definition")
    .boxed();

    let yield_ = just(Token::Kw("yield"))
        .ignore_then(just_symbol("*").or_not())
        .pad_cont()
        .then(sexpr.clone())
        .map(|(star, expr)| {
            if star.is_some() {
                Expr::YieldFrom(Box::new(expr))
            } else {
                Expr::Yield(Box::new(expr))
            }
        })
        .spanned()
        .labelled("yield")
        .boxed();

    sexpr.define(
        choice((slice0, slice1, fn_, class_, if_, match_, yield_, binary))
            .labelled("expression")
            .boxed(),
    );

    let expr_stmt = sexpr
        .clone()
        .then_ignore(just(Token::Eol))
        .map(Stmt::Expr)
        .labelled("expression statement")
        .boxed();

    let assign_stmt = sexpr
        .clone()
        .then_ignore(just_symbol("=").pad_cont())
        .then(sexpr.clone())
        .map(|(lhs, rhs)| Stmt::Assign(lhs, rhs))
        .then_ignore(just(Token::Eol))
        .labelled("assignment statement")
        .boxed();

    let while_stmt = just(Token::Kw("while"))
        .pad_cont()
        .ignore_then(sexpr.clone())
        .then_ignore(just(START_BLOCK))
        .then(sblock.clone())
        .map(|(cond, body)| Stmt::While(cond, body))
        .then_ignore(just(Token::Eol))
        .labelled("while statement")
        .boxed();

    let except_block = one_of([Token::Continuation, Token::Eol])
        .then(just(Token::Kw("except")))
        .pad_cont()
        .ignore_then(
            group((
                choice((sexpr.clone().map(Some), just_symbol("*").map(|_| None))),
                just(Token::Kw("as"))
                    .pad_cont()
                    .ignore_then(ident.clone())
                    .or_not(),
            ))
            .or_not(),
        )
        .then(just(START_BLOCK).ignore_then(sblock.clone()))
        .map(|(opt, body)| {
            if let Some((typ, name)) = opt {
                ExceptHandler { typ, name, body }
            } else {
                ExceptHandler {
                    typ: None,
                    name: None,
                    body,
                }
            }
        })
        .labelled("except block")
        .boxed();

    let finally_block = one_of([Token::Continuation, Token::Eol])
        .then(just(Token::Kw("finally")))
        .then(just(START_BLOCK))
        .ignore_then(sblock.clone())
        .labelled("finally block")
        .boxed();

    let try_stmt = just(Token::Kw("try"))
        .then(just(START_BLOCK))
        .ignore_then(group((
            sblock_or_expr.clone(),
            except_block.repeated().collect(),
            finally_block.or_not(),
        )))
        .map(|(body, excepts, finally)| Stmt::Try(body, excepts, finally))
        .then_ignore(just(Token::Eol))
        .labelled("try statement")
        .boxed();

    let for_stmt = just(Token::Kw("for"))
        .pad_cont()
        .ignore_then(group((
            sexpr.clone().then_ignore(just(Token::Kw("in")).pad_cont()),
            sexpr.clone().then_ignore(just(START_BLOCK)),
            sblock.clone(),
        )))
        .then_ignore(just(Token::Eol))
        .map(|(decl, iter, body)| Stmt::For(decl, iter, body))
        .labelled("for statement")
        .boxed();

    let return_stmt = just(Token::Kw("return"))
        .pad_cont()
        .ignore_then(sexpr.clone())
        .then_ignore(just(Token::Eol))
        .map(Stmt::Return)
        .labelled("return statement")
        .boxed();

    let raise_stmt = just(Token::Kw("raise"))
        .pad_cont()
        .ignore_then(sexpr.clone())
        .then_ignore(just(Token::Eol))
        .map(Stmt::Raise)
        .labelled("raise statement")
        .boxed();

    let break_stmt = just(Token::Kw("break"))
        .pad_cont()
        .ignore_then(just(Token::Eol))
        .map(|_| Stmt::Break)
        .labelled("break statement")
        .boxed();

    let continue_stmt = just(Token::Kw("continue"))
        .pad_cont()
        .ignore_then(just(Token::Eol))
        .map(|_| Stmt::Continue)
        .labelled("continue statement")
        .boxed();

    enum ImportLeaves<'a> {
        Multiple(Vec<(SIdent<'a>, Option<SIdent<'a>>)>),
        SingleAlias(SIdent<'a>),
        Star,
    }

    let import_stmt = just(Token::Kw("import"))
        .pad_cont()
        .ignore_then(
            ident
                .clone()
                .pad_cont()
                .separated_by(just(Token::Symbol(".")).pad_cont())
                .at_least(1)
                .collect()
                .pad_cont()
                .boxed()
                .then(
                    choice((
                        just(Token::Symbol(".")).ignore_then(
                            enumeration(
                                ident.clone().pad_cont().then(
                                    just(Token::Kw("as"))
                                        .pad_cont()
                                        .ignore_then(ident.clone())
                                        .or_not(),
                                ),
                            )
                            .delimited_by_with_eol(just_symbol("("), just_symbol(")"))
                            .map(ImportLeaves::Multiple),
                        ),
                        just(Token::Symbol("."))
                            .then(just(Token::Symbol("*")))
                            .map(|_| ImportLeaves::Star),
                        just(Token::Kw("as"))
                            .pad_cont()
                            .ignore_then(ident.clone())
                            .map(|x| ImportLeaves::SingleAlias(x)),
                    ))
                    .boxed()
                    .or_not(),
                ),
        )
        .map(
            |(mut trunk, leaves): (Vec<SIdent>, Option<ImportLeaves>)| -> Stmt {
                match leaves {
                    Some(ImportLeaves::Multiple(leaves)) => Stmt::Import(ImportStmt {
                        trunk,
                        leaves,
                        star: false,
                    }),
                    Some(ImportLeaves::SingleAlias(alias)) => {
                        if let Some(leaf) = trunk.pop() {
                            Stmt::Import(ImportStmt {
                                trunk,
                                leaves: vec![(leaf, Some(alias))],
                                star: false,
                            })
                        } else {
                            panic!("trunk should not be empty here")
                        }
                    }
                    Some(ImportLeaves::Star) => Stmt::Import(ImportStmt {
                        trunk,
                        leaves: vec![],
                        star: true,
                    }),
                    None => {
                        if let Some(leaf) = trunk.pop() {
                            Stmt::Import(ImportStmt {
                                trunk,
                                leaves: vec![(leaf, None)],
                                star: false,
                            })
                        } else {
                            panic!("trunk should not be empty here")
                        }
                    }
                }
            },
        )
        .then_ignore(just(Token::Eol))
        .labelled("import statement")
        .boxed();

    stmt.define(
        choice((
            expr_stmt,
            assign_stmt,
            while_stmt,
            for_stmt,
            return_stmt,
            raise_stmt,
            break_stmt,
            continue_stmt,
            import_stmt,
            try_stmt,
        ))
        .labelled("statement")
        .boxed(),
    );

    stmt.spanned()
        .repeated()
        .collect::<Vec<_>>()
        .map(Block::Stmts)
        .spanned()
        .delimited_by(just_symbol("{"), just_symbol("}").then(just(Token::Eol)))
        .boxed()
}

pub fn parse_tokens<'tokens, 'src: 'tokens>(
    src: &'src str,
    tokens: &'src TokenList<'src>,
) -> (Option<SBlock<'src>>, Vec<Rich<'tokens, Token<'src>, Span>>) {
    parser()
        .parse(
            tokens
                .0
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_output_errors()
}
