#![allow(dead_code)]

use std::borrow::Cow;

use crate::ast::*;
use crate::lexer::*;
use chumsky::{extra::ParserExtra, input::ValueInput, prelude::*};

fn enumeration<'tokens, 'src: 'tokens, I, O: 'tokens, E, ItemParser>(
    item_parser: ItemParser,
) -> impl Parser<'tokens, I, Vec<O>, E> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
    E: ParserExtra<'tokens, I, Error = Rich<'tokens, Token<'src>, Span>>,
    ItemParser: Parser<'tokens, I, O, E> + Clone + 'tokens,
{
    choice((
        item_parser
            .clone()
            .separated_by(choice((
                just(Token::Symbol(","))
                    .then(just(Token::Eol).or_not())
                    .ignored(),
                just(Token::Symbol(","))
                    .or_not()
                    .then(just(Token::Eol))
                    .ignored(),
            )))
            .allow_trailing()
            .collect()
            .delimited_by(
                just(Token::Symbol("BEGIN_BLOCK")),
                just(Token::Symbol("END_BLOCK")),
            )
            .labelled("block enumeration"),
        item_parser
            .separated_by(just(Token::Symbol(",")))
            .allow_trailing()
            .collect()
            .labelled("inline enumeration"),
    ))
    .labelled("enumeration")
    .as_context()
    .boxed()
}

pub trait ParserExt<'tokens, 'src: 'tokens, I, O, E>: Parser<'tokens, I, O, E>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
    E: ParserExtra<'tokens, I>,
{
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
    let mut inline_stmt = chumsky::recursive::Recursive::declare();
    let mut atom = chumsky::recursive::Recursive::declare();
    let mut postfix = chumsky::recursive::Recursive::declare();
    let mut unary = chumsky::recursive::Recursive::declare();
    let mut sexpr = chumsky::recursive::Recursive::declare();

    let stmts = stmt
        .clone()
        .repeated()
        .collect::<Vec<_>>()
        .map(Block::Stmts)
        .spanned()
        .labelled("statement-list")
        .boxed();

    let block = stmts
        .clone()
        .delimited_by(just_symbol("BEGIN_BLOCK"), just_symbol("END_BLOCK"))
        .boxed();

    let block_or_expr = choice((block.clone(), sexpr.clone().map(Block::Expr).spanned())).boxed();

    let block_or_inline_stmt = choice((
        block.clone(),
        inline_stmt
            .clone()
            .map(|x| Block::Stmts(vec![x]))
            .spanned()
            .boxed(),
    ))
    .boxed();

    let literal = select! {
        Token::Num(s) => Literal::Num(Cow::Borrowed(s)),
        Token::Str(s) => Literal::Str(Cow::Owned(s)),
        Token::Bool(s) => Literal::Bool(s),
        Token::None => Literal::None
    }
    .spanned()
    .map(Expr::Literal)
    .labelled("literal")
    .spanned()
    .boxed();

    let ident = select! {
        Token::Ident(s) => s,
    }
    .spanned()
    .labelled("identifier")
    .boxed();

    let placeholder = select! {
        Token::Symbol("$") => Expr::Placeholder,
    }
    .spanned()
    .labelled("placeholder")
    .boxed();

    let list_item = choice((
        just_symbol("*")
            .ignore_then(unary.clone())
            .map(ListItem::Spread),
        sexpr.clone().map(ListItem::Item),
    ))
    .boxed();

    let list = enumeration(list_item.clone())
        .delimited_by_with_eol(just_symbol("["), just_symbol("]"))
        .map(Expr::List)
        .labelled("list")
        .as_context()
        .spanned()
        .boxed();

    let nary_list = group((
        list_item.clone(),
        just_symbol(",")
            .ignore_then(list_item)
            .repeated()
            .collect::<Vec<_>>(),
        just_symbol(",").to(1).or_not(),
    ))
    .map_with(|(first, rest, last_comma), e| -> SExpr {
        let mut items = Vec::<ListItem>::new();

        match first {
            ListItem::Item(expr) if rest.is_empty() && last_comma.is_none() => {
                return expr;
            }
            ListItem::Item(..) => {
                items.push(first);
            }
            ListItem::Spread(..) => {
                items.push(first);
            }
        }
        items.extend(rest);

        (Expr::List(items), e.span())
    })
    .labelled("nary-list")
    .as_context()
    .boxed();

    let binding_target = nary_list.clone();

    let mapping = enumeration(choice((
        just_symbol("**")
            .ignore_then(unary.clone())
            .map(MappingItem::Spread),
        sexpr
            .clone()
            .then_ignore(just(START_BLOCK))
            .then(sexpr.clone())
            .map(|(key, value)| MappingItem::Item(key, value)),
    )))
    .delimited_by_with_eol(just(Token::Symbol("[")), just(Token::Symbol("]")))
    .map(Expr::Mapping)
    .labelled("mapping")
    .as_context()
    .spanned()
    .boxed();

    let fstr_begin = select! {
        Token::FstrBegin(s) => s,
    };
    let fstr_continue = select! {
        Token::FstrContinue(s) => s,
    };

    let fstr = fstr_begin
        .spanned()
        .then(
            block_or_expr
                .clone()
                .spanned()
                .then(fstr_continue.spanned())
                .map(|(block, cont)| {
                    (
                        (
                            FmtExpr {
                                block: block.0,
                                fmt: None,
                            },
                            block.1,
                        ),
                        cont,
                    )
                })
                .repeated()
                .collect::<Vec<_>>(),
        )
        .map(|(begin, parts)| Expr::Fstr(begin, parts))
        .spanned()
        .labelled("f-string")
        .boxed();

    atom.define(
        choice((
            ident.clone().map(Expr::Ident).spanned(),
            literal,
            placeholder,
            list.clone(),
            mapping,
            fstr,
            block_or_inline_stmt
                .clone()
                .map(|b| Expr::Block(Box::new(b)))
                .delimited_by_with_eol(just(Token::Symbol("(")), just(Token::Symbol(")")))
                .spanned(),
        ))
        .labelled("atom")
        .boxed(),
    );

    enum Postfix<'a> {
        Call(Vec<SCallItem<'a>>),
        Subscript(Vec<ListItem<'a>>),
        Then(SExpr<'a>),
        Attribute(SIdent<'a>),
    }

    let call = enumeration(
        choice((
            just_symbol("*")
                .ignore_then(unary.clone())
                .map(CallItem::ArgSpread),
            just_symbol("**")
                .ignore_then(unary.clone())
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
    .labelled("argument-list")
    .as_context()
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
    .as_context()
    .boxed();

    let attribute = just_symbol(".")
        .ignore_then(ident.clone())
        .map(Postfix::Attribute)
        .labelled("attr");

    let then = just_symbol(".")
        .ignore_then(
            sexpr
                .clone()
                .delimited_by_with_eol(just_symbol("("), just_symbol(")"))
                .map(Postfix::Then),
        )
        .labelled("generalized-attr")
        .boxed();

    postfix.define(
        atom.clone()
            .foldl_with(
                just_symbol("?")
                    .to(1)
                    .or_not()
                    .then(choice((call, subscript, attribute, then)))
                    .repeated(),
                |expr, (coal, op), e| -> SExpr {
                    (
                        if coal.is_none() {
                            match op {
                                Postfix::Call(args) => Expr::Call(Box::new(expr), args),
                                Postfix::Subscript(args) => Expr::Subscript(Box::new(expr), args),
                                Postfix::Attribute(attr) => Expr::Attribute(Box::new(expr), attr),
                                Postfix::Then(rhs) => Expr::Then(Box::new(expr), Box::new(rhs)),
                            }
                        } else {
                            match op {
                                Postfix::Call(args) => Expr::MappedCall(Box::new(expr), args),
                                Postfix::Subscript(args) => {
                                    Expr::MappedSubscript(Box::new(expr), args)
                                }
                                Postfix::Attribute(attr) => {
                                    Expr::MappedAttribute(Box::new(expr), attr)
                                }
                                Postfix::Then(rhs) => {
                                    Expr::MappedThen(Box::new(expr), Box::new(rhs))
                                }
                            }
                        },
                        e.span(),
                    )
                },
            )
            .labelled("postfix")
            .boxed(),
    );

    unary.define(
        select! {
            Token::Symbol("@") => UnaryOp::Yield,
            Token::Symbol("@@") => UnaryOp::YieldFrom,
            Token::Symbol("+") => UnaryOp::Pos,
            Token::Symbol("-") => UnaryOp::Neg,
            Token::Symbol("~") => UnaryOp::Inv,
        }
        .repeated()
        .foldr_with(postfix, |op: UnaryOp, rhs: SExpr, e| {
            (Expr::Unary(op, Box::new(rhs)), e.span())
        })
        .labelled("unary")
        .boxed(),
    );

    fn make_binary_op<'tokens, 'src: 'tokens, I, POp, PArg>(
        arg: PArg,
        op: POp,
        right_assoc: bool,
    ) -> impl Parser<'tokens, I, SExpr<'src>, extra::Err<Rich<'tokens, Token<'src>, Span>>> + Clone
    where
        PArg: Parser<'tokens, I, SExpr<'src>, extra::Err<Rich<'tokens, Token<'src>, Span>>>
            + Clone
            + 'tokens,
        POp: Parser<'tokens, I, BinaryOp, extra::Err<Rich<'tokens, Token<'src>, Span>>>
            + Clone
            + 'tokens,
        I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
    {
        if !right_assoc {
            arg.clone()
                .foldl_with(op.then(arg).repeated(), |lhs, (op, rhs), e| {
                    (Expr::Binary(op, Box::new(lhs), Box::new(rhs)), e.span())
                })
                .boxed()
        } else {
            recursive(|bin| {
                arg.clone()
                    .then(op.then(bin.or(arg.clone())).or_not())
                    .map_with(|(lhs, matched), e| {
                        if let Some((op, rhs)) = matched {
                            (Expr::Binary(op, Box::new(lhs), Box::new(rhs)), e.span())
                        } else {
                            lhs
                        }
                    })
            })
            .boxed()
        }
    }

    let binary0 = make_binary_op(
        unary.clone(),
        select! {
            Token::Symbol("**") => BinaryOp::Exp,
        },
        true,
    );

    let binary1 = make_binary_op(
        binary0,
        select! {
            Token::Symbol("*") => BinaryOp::Mul,
            Token::Symbol("/") => BinaryOp::Div,
            Token::Symbol("%") => BinaryOp::Mod,
            Token::Symbol("@") => BinaryOp::MatMul,
        },
        false,
    );

    let binary2 = make_binary_op(
        binary1,
        select! {
            Token::Symbol("+") => BinaryOp::Add,
            Token::Symbol("-") => BinaryOp::Sub,
        },
        false,
    );

    let binary3 = make_binary_op(
        binary2.clone(),
        select! {
            Token::Symbol("<") => BinaryOp::Lt,
            Token::Symbol("<=") => BinaryOp::Leq,
            Token::Symbol(">") => BinaryOp::Gt,
            Token::Symbol(">=") => BinaryOp::Geq,
            Token::Symbol("==") => BinaryOp::Eq,
            Token::Symbol("<>") => BinaryOp::Neq,
            Token::Symbol("===") => BinaryOp::Is,
            Token::Symbol("<=>") => BinaryOp::Nis,
        },
        false,
    );

    let checked_ = just(Token::Kw("try"))
        .ignore_then(binary2.clone())
        .then(
            just(Token::Kw("except"))
                .ignore_then(list.or(unary))
                .or_not(),
        )
        .map(|(expr, typs)| Expr::Checked(Box::new(expr), typs.map(Box::new)))
        .spanned()
        .labelled("checked")
        .boxed();

    let binary4 = make_binary_op(
        binary3.or(checked_),
        select! {
            Token::Symbol("??") => BinaryOp::Coalesce,
        },
        false,
    );

    let slice0 = group((
        binary4.clone(),
        just_symbol("..")
            .ignore_then(binary4.clone().or_not())
            .or_not(),
        just_symbol("..")
            .ignore_then(binary4.clone().or_not())
            .or_not(),
    ))
    .map_with(|(lhs, a, b), e| {
        if a.is_none() && b.is_none() {
            lhs
        } else {
            (
                Expr::Slice(
                    Some(Box::new(lhs)),
                    a.flatten().map(Box::new),
                    b.flatten().map(Box::new),
                ),
                e.span(),
            )
        }
    })
    .labelled("slice")
    .boxed();

    let slice1 = just_symbol("..")
        .ignore_then(binary4.clone().or_not())
        .then(
            just_symbol("..")
                .ignore_then(binary4.clone().or_not())
                .or_not(),
        )
        .map(|(e1, e2)| Expr::Slice(None, e1.map(Box::new), e2.flatten().map(Box::new)))
        .spanned()
        .labelled("slice")
        .boxed();

    let slices = choice((slice0, slice1));

    let arg_list = enumeration(choice((
        just_symbol("*")
            .ignore_then(ident.clone())
            .map(|x| ArgDefItem::ArgSpread(x)),
        just_symbol("**")
            .ignore_then(ident.clone())
            .map(ArgDefItem::KwargSpread),
        sexpr
            .clone()
            .then(just_symbol("=").ignore_then(sexpr.clone()).or_not())
            .map(|(key, value)| ArgDefItem::Arg(key, value)),
    )))
    .labelled("argument-def-list")
    .boxed();

    let if_ = slices
        .clone()
        .then(
            group((
                just(Token::Kw("then"))
                    .then(just(START_BLOCK).or_not())
                    .ignore_then(block_or_inline_stmt.clone()),
                just(Token::Eol)
                    .or_not()
                    .then(just(Token::Kw("else")))
                    .then(just(START_BLOCK).or_not())
                    .ignore_then(block_or_inline_stmt.clone())
                    .or_not(),
            ))
            .or_not(),
        )
        .map_with(|(cond, if_cases), e| {
            if let Some((if_, else_)) = if_cases {
                (
                    Expr::If(Box::new(cond), Box::new(if_), else_.map(Box::new)),
                    e.span(),
                )
            } else {
                cond
            }
        });

    let mut fn_ = chumsky::recursive::Recursive::declare();
    let fn_body = just_symbol("=>")
        .ignore_then(choice((
            block_or_inline_stmt.clone(),
            fn_.clone().map(|x| Block::Expr(x)).spanned().boxed(),
        )))
        .boxed();

    let unary_fn = if_
        .clone()
        .then(fn_body.clone().or_not())
        .map_with(|(x, body), e| {
            if let Some(body) = body {
                (
                    Expr::Fn(vec![ArgDefItem::Arg(x, None)], Box::new(body)),
                    e.span(),
                )
            } else {
                x
            }
        })
        .labelled("unary-fn")
        .as_context()
        .boxed();

    let nary_fn = arg_list
        .clone()
        .delimited_by_with_eol(just_symbol("("), just_symbol(")"))
        .then(fn_body)
        .map(|(args, body)| Expr::Fn(args, Box::new(body)))
        .spanned()
        .labelled("nary-fn")
        .as_context()
        .boxed();

    fn_.define(choice((nary_fn, unary_fn)).labelled("fn-or-binary").boxed());

    let binary6 = make_binary_op(
        fn_,
        select! {
            Token::Symbol("|") => BinaryOp::Pipe,
        },
        false,
    );

    let binary = binary6.boxed();

    let classic_if = just(Token::Kw("if"))
        .ignore_then(group((
            sexpr.clone().then_ignore(just(START_BLOCK)),
            block_or_inline_stmt.clone(),
            group((
                just(Token::Eol).or_not(),
                just(Token::Kw("else")),
                just(START_BLOCK),
            ))
            .ignore_then(block_or_inline_stmt.clone())
            .or_not(),
        )))
        .map(|(cond, if_, else_)| Expr::If(Box::new(cond), Box::new(if_), else_.map(Box::new)))
        .spanned()
        .labelled("if")
        .boxed();

    let case_ = sexpr
        .clone()
        .then_ignore(just(START_BLOCK))
        .then(block_or_inline_stmt.clone())
        .then_ignore(just(Token::Eol))
        .map(|(pattern, body)| (pattern, Box::new(body)))
        .labelled("match-case")
        .boxed();

    let match_ = just(Token::Kw("match"))
        .ignore_then(sexpr.clone())
        .then_ignore(just(START_BLOCK))
        .then(
            case_
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by_with_eol(just_symbol("BEGIN_BLOCK"), just_symbol("END_BLOCK")),
        )
        .map(|(scrutinee, cases)| Expr::Match(Box::new(scrutinee), cases))
        .spanned()
        .labelled("match")
        .boxed();

    let class_ = just(Token::Kw("class"))
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
        .then(block_or_inline_stmt.clone())
        .map(|(arglist, block)| Expr::Class(arglist.unwrap_or_else(|| Vec::new()), Box::new(block)))
        .spanned()
        .labelled("class")
        .boxed();

    sexpr.define(
        choice((class_, classic_if, match_, binary))
            .labelled("expression")
            .as_context()
            .boxed(),
    );

    let expr_stmt = nary_list
        .clone()
        .map(Stmt::Expr)
        .labelled("expression statement")
        .boxed();

    let assign_stmt = group((
        choice((
            just(Token::Kw("export")).to(AssignModifier::Export),
            just(Token::Kw("global")).to(AssignModifier::Global),
            just(Token::Kw("nonlocal")).to(AssignModifier::Nonlocal),
        ))
        .repeated()
        .collect()
        .boxed(),
        binding_target.clone().then_ignore(just_symbol("=")),
        nary_list.clone(),
    ))
    .map(|(modifiers, lhs, rhs)| Stmt::Assign(lhs, rhs, modifiers))
    .labelled("assignment statement")
    .boxed();

    let inline_assign_stmt = group((sexpr.clone().then_ignore(just_symbol("=")), sexpr.clone()))
        .map(|(lhs, rhs)| Stmt::Assign(lhs, rhs, vec![]))
        .labelled("inline assignment statement")
        .boxed();

    let while_stmt = just(Token::Kw("while"))
        .ignore_then(sexpr.clone())
        .then_ignore(just(START_BLOCK))
        .then(block_or_inline_stmt.clone())
        .map(|(cond, body)| Stmt::While(cond, body))
        .labelled("while statement")
        .boxed();

    let except_block = just(Token::Eol)
        .then(just(Token::Kw("except")))
        .ignore_then(
            group((
                choice((sexpr.clone().map(Some), just_symbol("*").map(|_| None))),
                just(Token::Kw("as")).ignore_then(ident.clone()).or_not(),
            ))
            .or_not(),
        )
        .boxed()
        .then(just(START_BLOCK).ignore_then(block_or_inline_stmt.clone()))
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

    let finally_block = one_of([Token::Eol])
        .then(just(Token::Kw("finally")))
        .then(just(START_BLOCK))
        .ignore_then(block_or_inline_stmt.clone())
        .labelled("finally block")
        .boxed();

    let try_stmt = just(Token::Kw("try"))
        .then(just(START_BLOCK))
        .ignore_then(group((
            block_or_inline_stmt.clone(),
            except_block.repeated().collect(),
            finally_block.or_not(),
        )))
        .map(|(body, excepts, finally)| Stmt::Try(body, excepts, finally))
        .labelled("try statement")
        .boxed();

    let for_stmt = just(Token::Kw("for"))
        .ignore_then(group((
            binding_target.clone().then_ignore(just(Token::Kw("in"))),
            sexpr.clone().then_ignore(just(START_BLOCK)),
            block_or_inline_stmt.clone(),
        )))
        .map(|(decl, iter, body)| Stmt::For(decl, iter, body))
        .labelled("for statement")
        .boxed();

    let return_stmt = just(Token::Kw("return"))
        .ignore_then(nary_list.clone())
        .map(Stmt::Return)
        .labelled("return statement")
        .boxed();

    let inline_return_stmt = just(Token::Kw("return"))
        .ignore_then(sexpr.clone())
        .map(Stmt::Return)
        .labelled("inline return statement")
        .boxed();

    let assert_stmt = just(Token::Kw("assert"))
        .ignore_then(sexpr.clone())
        .then(just_symbol(",").ignore_then(sexpr.clone()).or_not())
        .map(|(x, y)| Stmt::Assert(x, y))
        .labelled("assert statement")
        .boxed();

    let raise_stmt = just(Token::Kw("raise"))
        .ignore_then(nary_list.clone())
        .map(Stmt::Raise)
        .labelled("raise statement")
        .boxed();

    let inline_raise_stmt = just(Token::Kw("raise"))
        .ignore_then(sexpr.clone())
        .map(Stmt::Raise)
        .labelled("inline raise statement")
        .boxed();

    let break_stmt = just(Token::Kw("break"))
        .map(|_| Stmt::Break)
        .labelled("break statement")
        .boxed();

    let continue_stmt = just(Token::Kw("continue"))
        .map(|_| Stmt::Continue)
        .labelled("continue statement")
        .boxed();

    let import_stmt = just(Token::Kw("export"))
        .to(1)
        .or_not()
        .then_ignore(just(Token::Kw("import")))
        .then(group((
            just_symbol(".").repeated().count(),
            ident
                .clone()
                .then_ignore(just_symbol("."))
                .repeated()
                .collect()
                .boxed(),
            choice((
                enumeration(
                    ident
                        .clone()
                        .then(just(Token::Kw("as")).ignore_then(ident.clone()).or_not()),
                )
                .delimited_by_with_eol(just_symbol("("), just_symbol(")"))
                .map(ImportList::Leaves)
                .boxed(),
                just(Token::Symbol("*")).map(|_| ImportList::Star),
                ident
                    .clone()
                    .then(just(Token::Kw("as")).ignore_then(ident.clone()).or_not())
                    .map(|x| ImportList::Leaves(vec![x]))
                    .boxed(),
            ))
            .boxed(),
        )))
        .map(|(reexport, (level, trunk, import_list))| -> Stmt {
            Stmt::Import(ImportStmt {
                trunk,
                imports: import_list,
                level,
                reexport: reexport.is_some(),
            })
        })
        .labelled("import statement")
        .boxed();

    let module_stmt = just(Token::Kw("module")).map(|_| Stmt::Module);

    stmt.define(
        choice((
            expr_stmt.clone().then_ignore(just(Token::Eol)),
            assign_stmt.then_ignore(just(Token::Eol)),
            //
            module_stmt.then_ignore(just(Token::Eol)),
            while_stmt.clone().then_ignore(just(Token::Eol)),
            for_stmt.clone().then_ignore(just(Token::Eol)),
            return_stmt.then_ignore(just(Token::Eol)),
            assert_stmt.then_ignore(just(Token::Eol)),
            raise_stmt.then_ignore(just(Token::Eol)),
            break_stmt.clone().then_ignore(just(Token::Eol)),
            continue_stmt.clone().then_ignore(just(Token::Eol)),
            import_stmt.then_ignore(just(Token::Eol)),
            try_stmt.then_ignore(just(Token::Eol)),
        ))
        .labelled("statement")
        .spanned()
        .boxed(),
    );

    inline_stmt.define(
        choice((
            expr_stmt,
            inline_assign_stmt,
            //
            while_stmt,
            for_stmt,
            inline_return_stmt,
            inline_raise_stmt,
            break_stmt,
            continue_stmt,
        ))
        .labelled("inline-statement")
        .spanned()
        .boxed(),
    );

    stmts.labelled("program")
}

pub fn parse_tokens<'tokens, 'src: 'tokens>(
    src: &'src str,
    tokens: &'tokens TokenList<'src>,
) -> (Option<SBlock<'src>>, Vec<Rich<'tokens, Token<'src>, Span>>) {
    parser()
        .parse(
            tokens
                .0
                .as_slice()
                // convert the span type with map
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_output_errors()
}
