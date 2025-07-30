#![allow(dead_code)]

use std::borrow::Cow;
use std::rc::Rc;

use crate::ast::*;
use crate::lexer::*;
use chumsky::{extra::ParserExtra, input::ValueInput, prelude::*};

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

fn enumeration<'tokens, 'src: 'tokens, I, O: 'tokens, OS: 'tokens, E, ItemParser, SepParser>(
    item_parser: ItemParser,
    optional_separator: SepParser,
) -> impl Parser<'tokens, I, Vec<O>, E> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
    E: ParserExtra<'tokens, I, Error = Rich<'tokens, Token<'src>, Span>>,
    ItemParser: Parser<'tokens, I, O, E> + Clone + 'tokens,
    SepParser: Parser<'tokens, I, OS, E> + Clone + 'tokens,
{
    choice((
        item_parser
            .clone()
            .separated_by(choice((
                optional_separator
                    .clone()
                    .then(just(Token::Eol).or_not())
                    .ignored(),
                optional_separator
                    .clone()
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
            .separated_by(optional_separator)
            .allow_trailing()
            .collect()
            .labelled("inline enumeration"),
    ))
    .labelled("enumeration")
    .boxed()
}

const START_BLOCK: Token = Token::Symbol(":");
type TExtra<'tokens, 'src> = extra::Err<Rich<'tokens, Token<'src>, Span>>;

pub fn symbol<'tokens, 'src: 'tokens, I>(
    symbol: &'static str,
) -> impl Parser<'tokens, I, Token<'src>, TExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    just(Token::Symbol(symbol))
}

pub fn match_pattern<'tokens, 'src: 'tokens, TInput, PIdent, PQualIdent, PExpr, PLiteral>(
    ident: PIdent,
    qualified_ident: PQualIdent,
    sexpr: PExpr,
    literal: PLiteral,
) -> (
    impl Parser<'tokens, TInput, SPattern<'src>, TExtra<'tokens, 'src>> + Clone,
    impl Parser<'tokens, TInput, SPattern<'src>, TExtra<'tokens, 'src>> + Clone,
)
where
    TInput: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
    PIdent: Parser<'tokens, TInput, SIdent<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
    PQualIdent: Parser<'tokens, TInput, SExpr<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
    PExpr: Parser<'tokens, TInput, SExpr<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
    PLiteral: Parser<'tokens, TInput, SLiteral<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
{
    let mut pattern =
        Recursive::<chumsky::recursive::Indirect<TInput, SPattern, TExtra>>::declare();

    let literal_pattern = literal.clone().map(Pattern::Literal).spanned().boxed();

    fn to_wildcard<'src>(id: SIdent<'src>) -> Option<SIdent<'src>> {
        if id.0.0 == "_" { None } else { Some(id) }
    }

    let value_pattern = symbol(".")
        .to(1)
        .or_not()
        .then(qualified_ident.clone())
        .try_map(|(q, value), _e| {
            Ok(if let Expr::RawAttribute(..) = value.0 {
                Pattern::Value(value)
            } else if q.is_some() {
                Pattern::Value(value)
            } else if let Expr::Ident(id) = value.0 {
                Pattern::Capture(to_wildcard(id))
            } else {
                return Err(Rich::custom(
                    value.1,
                    "Internal error: value pattern must be an identifier or attribute",
                ));
            })
        })
        .spanned()
        .boxed();

    let group_pattern = pattern
        .clone()
        .delimited_by_with_eol(symbol("("), symbol(")"))
        .boxed();

    let sequence_item = choice((
        symbol("*")
            .ignore_then(ident.clone())
            .map(|x| PatternSequenceItem::Spread(to_wildcard(x))),
        pattern.clone().map(|x| PatternSequenceItem::Item(x.into())),
    ))
    .boxed();

    let sequence_pattern = enumeration(sequence_item.clone(), symbol(","))
        .map(Pattern::Sequence)
        .delimited_by_with_eol(symbol("["), symbol("]"))
        .spanned()
        .boxed();

    let sequence_pattern2 = enumeration(sequence_item.clone(), symbol(","))
        .map(Pattern::Sequence)
        .delimited_by_with_eol(symbol("("), symbol(")"))
        .spanned()
        .boxed();

    let nary_sequence_pattern = sequence_item
        .separated_by(symbol(","))
        .at_least(1)
        .collect::<Vec<_>>()
        .then(symbol(",").to(0).or_not())
        .map_with(|(items, last_comma), e| {
            if items.len() == 1 && last_comma.is_none() {
                let item = items.into_iter().next().unwrap();
                if let PatternSequenceItem::Item(inner) = item {
                    Rc::unwrap_or_clone(inner)
                } else {
                    (Pattern::Sequence(vec![item]), e.span())
                }
            } else {
                (Pattern::Sequence(items), e.span())
            }
        })
        .boxed();

    let mapping_item = choice((
        symbol("**")
            .ignore_then(ident.clone())
            .map(|x| PatternMappingItem::Spread(to_wildcard(x))),
        choice((
            ident
                .clone()
                .map(|(s, span)| Expr::Literal((Literal::Str(s.0), span)))
                .spanned(),
            literal.clone().map(Expr::Literal).spanned(),
            sexpr
                .clone()
                .delimited_by_with_eol(symbol("("), symbol(")")),
        ))
        .then_ignore(symbol(":"))
        .then(pattern.clone())
        .map(|(key, value)| PatternMappingItem::Item(key, value.into())),
        ident.clone().map(PatternMappingItem::Ident),
    ));

    let mapping_pattern = enumeration(mapping_item, symbol(","))
        .map(Pattern::Mapping)
        .delimited_by_with_eol(symbol("{"), symbol("}"))
        .spanned()
        .boxed();

    let class_item = choice((
        ident
            .clone()
            .then_ignore(symbol("="))
            .then(pattern.clone())
            .map(|(key, value)| PatternClassItem::Kw(key, value.into())),
        pattern.clone().map(|x| PatternClassItem::Item(x.into())),
    ))
    .boxed();

    let class_pattern = qualified_ident
        .clone()
        .then(
            class_item
                .separated_by(symbol(","))
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by_with_eol(symbol("("), symbol(")")),
        )
        .map(|(a, b)| Pattern::Class(a, b))
        .spanned()
        .boxed();

    let closed_pattern = choice((
        literal_pattern,
        class_pattern,
        value_pattern.clone(),
        group_pattern,
        sequence_pattern,
        sequence_pattern2,
        mapping_pattern,
    ))
    .boxed();

    let or_pattern = closed_pattern
        .clone()
        .map(Indirect::new)
        .separated_by(symbol("|").then(just(Token::Eol).or_not()))
        .at_least(1)
        .allow_leading()
        .collect::<Vec<_>>()
        .map_with(|x, e| {
            if x.len() == 1 {
                Rc::unwrap_or_clone(x.into_iter().next().unwrap())
            } else {
                (Pattern::Or(x), e.span())
            }
        });

    let as_pattern = or_pattern
        .then(just(Token::Kw("as")).ignore_then(ident.clone()).or_not())
        .map_with(|(pattern, as_ident), e| {
            if let Some(as_ident) = as_ident {
                (Pattern::As(pattern.into(), as_ident), e.span())
            } else {
                pattern
            }
        })
        .boxed();

    pattern.define(
        choice((
            as_pattern,
            value_pattern,
            closed_pattern.clone(),
            symbol("_").map(|_| Pattern::Capture(None)).spanned(),
        ))
        .labelled("pattern"),
    );

    (
        closed_pattern.labelled("pattern").as_context(),
        nary_sequence_pattern.labelled("pattern").as_context(),
    )
}

pub fn match_expr<'tokens, 'src: 'tokens, TInput, PLHS, PGuard, PPattern, PBody>(
    lhs: PLHS,
    case_guard: PGuard,
    nary_pattern: PPattern,
    expr_or_inline_stmt_or_block: PBody,
) -> (
    impl Parser<'tokens, TInput, SExpr<'src>, TExtra<'tokens, 'src>> + Clone,
    impl Parser<'tokens, TInput, Vec<Indirect<MatchCase<'src>>>, TExtra<'tokens, 'src>> + Clone,
)
where
    TInput: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
    PLHS: Parser<'tokens, TInput, SExpr<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
    PGuard: Parser<'tokens, TInput, SExpr<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
    PPattern: Parser<'tokens, TInput, SPattern<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
    PBody: Parser<'tokens, TInput, SExpr<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
{
    let case_ = group((
        nary_pattern.clone(),
        just(Token::Kw("if")).ignore_then(case_guard).or_not(),
        symbol("=>").ignore_then(expr_or_inline_stmt_or_block.clone()),
    ))
    .map(|(pattern, guard, body)| MatchCase {
        pattern: Some(pattern.into()),
        guard,
        body,
    })
    .labelled("match-case")
    .as_context()
    .boxed();

    let default_case = just(Token::Ident("default"))
        .then(just(START_BLOCK).or_not())
        .ignore_then(expr_or_inline_stmt_or_block.clone())
        .map(|x| MatchCase {
            pattern: None,
            guard: None,
            body: x,
        })
        .boxed();

    let cases = choice((
        just(Token::Kw("else"))
            .or_not()
            .ignore_then(case_.clone())
            .then_ignore(just(Token::Eol))
            .map(Indirect::new)
            .repeated()
            .collect::<Vec<_>>()
            .then(default_case.clone().then_ignore(just(Token::Eol)).or_not())
            .delimited_by(
                just(Token::Symbol("BEGIN_BLOCK")),
                just(Token::Symbol("END_BLOCK")),
            ),
        case_
            .map(Indirect::new)
            .separated_by(just(Token::Kw("else")))
            .allow_trailing()
            .collect::<Vec<_>>()
            .then(default_case.or_not()),
    ))
    .map(|(mut cases, default)| {
        cases.extend(default.into_iter().map(Indirect::new));
        cases
    })
    .boxed();

    let match_ = lhs
        .then(
            just(Token::Kw("match"))
                .then(just(START_BLOCK).or_not())
                .ignore_then(cases.clone())
                .or_not(),
        )
        .map_with(|(scrutinee, cases), e| {
            if let Some(cases) = cases {
                (Expr::Match(Indirect::new(scrutinee), cases), e.span())
            } else {
                scrutinee
            }
        })
        .labelled("match")
        .boxed();

    (match_.labelled("match-expression").boxed(), cases)
}

pub fn function<'tokens, 'src: 'tokens, TInput, PBody, PIdent, PExpr, PPattern>(
    block_or_inline_stmt: PBody,
    ident: PIdent,
    expr: PExpr,
    pattern: PPattern,
) -> impl Parser<'tokens, TInput, SExpr<'src>, TExtra<'tokens, 'src>> + Clone
where
    TInput: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
    PBody: Parser<'tokens, TInput, SExpr<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
    PIdent: Parser<'tokens, TInput, SIdent<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
    PExpr: Parser<'tokens, TInput, SExpr<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
    PPattern: Parser<'tokens, TInput, SPattern<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
{
    recursive(|fn_| {
        let fn_body = symbol("=>")
            .ignore_then(choice((
                block_or_inline_stmt.clone(),
                // should these be switched?
                fn_.clone().boxed(),
            )))
            .boxed();

        let uni_fn = pattern
            .clone()
            .then(fn_body.clone())
            .map(|(x, body)| Expr::Fn(vec![ArgDefItem::Arg(x.into(), None)], Indirect::new(body)))
            .spanned()
            .labelled("uni-fn")
            .boxed();

        let arg_list = enumeration(
            choice((
                symbol("*")
                    .ignore_then(ident.clone())
                    .map(|x| ArgDefItem::ArgSpread(x)),
                symbol("**")
                    .ignore_then(ident.clone())
                    .map(ArgDefItem::KwargSpread),
                pattern
                    .clone()
                    .then(symbol("=").ignore_then(expr.clone()).or_not())
                    .map(|(key, value)| ArgDefItem::Arg(key.into(), value)),
            )),
            symbol(","),
        )
        .labelled("argument-def-list")
        .boxed();

        let multi_fn = arg_list
            .clone()
            .delimited_by_with_eol(symbol("("), symbol(")"))
            .then(fn_body)
            .map(|(args, body)| Expr::Fn(args, Indirect::new(body)))
            .spanned()
            .labelled("multi-fn")
            .as_context()
            .boxed();

        choice((multi_fn, uni_fn)).labelled("fn")
    })
}

pub fn parser<'tokens, 'src: 'tokens, TInput>()
-> impl Parser<'tokens, TInput, Vec<SStmt<'src>>, TExtra<'tokens, 'src>> + Clone
where
    TInput: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    let mut stmt = Recursive::<chumsky::recursive::Indirect<TInput, SStmt, TExtra>>::declare();
    let mut inline_stmt =
        Recursive::<chumsky::recursive::Indirect<TInput, SStmt, TExtra>>::declare();
    let mut atom = Recursive::<chumsky::recursive::Indirect<TInput, SExpr, TExtra>>::declare();
    let mut expr = Recursive::<chumsky::recursive::Indirect<TInput, SExpr, TExtra>>::declare();
    let mut unary = Recursive::<chumsky::recursive::Indirect<TInput, SExpr, TExtra>>::declare();
    let mut cases = Recursive::<chumsky::recursive::Indirect<TInput, _, TExtra>>::declare();
    let mut below_pipe =
        Recursive::<chumsky::recursive::Indirect<TInput, SExpr, TExtra>>::declare();

    let stmts = stmt
        .clone()
        .repeated()
        .collect::<Vec<SStmt>>()
        .labelled("statement-list")
        .boxed();

    let block = stmts
        .clone()
        .delimited_by(symbol("BEGIN_BLOCK"), symbol("END_BLOCK"))
        .map(Expr::Block)
        .spanned()
        .boxed();

    let expr_or_block = choice((block.clone(), expr.clone())).boxed();

    let expr_or_inline_stmt_or_block = choice((
        block.clone(),
        inline_stmt
            .clone()
            .map_with(|x, e| match x.0 {
                Stmt::Expr(x) => x,
                _ => (Expr::Block(vec![x]), e.span()),
            })
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
    .boxed();

    let literal_expr = literal
        .clone()
        .map(Expr::Literal)
        .labelled("literal")
        .spanned()
        .boxed();

    let ident = select! {
        Token::Ident(s) => Ident(Cow::Borrowed(s)),
    }
    .spanned()
    .labelled("identifier")
    .boxed();

    let ident_expr = ident
        .clone()
        .map(Expr::Ident)
        .spanned()
        .labelled("identifier-expression")
        .boxed();

    let placeholder = select! {
        Token::Symbol("$") => Expr::Placeholder,
    }
    .spanned()
    .labelled("placeholder")
    .boxed();

    let list_item = choice((
        symbol("*").ignore_then(expr.clone()).map(ListItem::Spread),
        expr.clone().map(ListItem::Item),
    ))
    .boxed();

    let list = enumeration(list_item.clone(), symbol(","))
        .delimited_by_with_eol(symbol("["), symbol("]"))
        .map(Expr::List)
        .labelled("list")
        .as_context()
        .spanned()
        .boxed();

    let nary_tuple = group((
        list_item.clone(),
        symbol(",")
            .ignore_then(list_item)
            .repeated()
            .collect::<Vec<_>>(),
        symbol(",").to(1).or_not(),
    ))
    .try_map_with(
        |(first, rest, last_comma), e| -> Result<SExpr, Rich<'tokens, Token<'src>, Span>> {
            let mut items = Vec::<ListItem>::new();

            match first {
                ListItem::Item(expr) if rest.is_empty() && last_comma.is_none() => {
                    return Ok(expr);
                }
                // ListItem::Spread(expr) if rest.is_empty() && last_comma.is_none() => {
                //     // should this be an error?

                //     return Err(Rich::custom(
                //         first.1,
                //         "Spread operator must be in a list or tuple",
                //     ));
                // }
                ListItem::Item(..) => {
                    items.push(first);
                }
                ListItem::Spread(..) => {
                    items.push(first);
                }
            }
            items.extend(rest);

            Ok((Expr::Tuple(items), e.span()))
        },
    )
    .labelled("nary-tuple")
    .boxed();
    // .memoized();

    let mapping = enumeration(
        choice((
            symbol("**")
                .ignore_then(expr.clone())
                .map(MappingItem::Spread),
            choice((
                ident
                    .clone()
                    .map(|(s, span)| Expr::Literal((Literal::Str(s.0), span)))
                    .spanned(),
                literal_expr.clone(),
                expr.clone().delimited_by_with_eol(symbol("("), symbol(")")),
            ))
            .then_ignore(symbol(":"))
            .then(expr.clone())
            .map(|(key, value)| MappingItem::Item(key, value)),
            ident.clone().map(MappingItem::Ident),
        )),
        symbol(","),
    )
    .delimited_by_with_eol(just(Token::Symbol("{")), just(Token::Symbol("}")))
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
            expr_or_block
                .clone()
                .spanned()
                .then(fstr_continue.spanned())
                .map(|(block, cont)| {
                    (
                        (
                            FmtExpr {
                                expr: block.0,
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

    let class_ = just(Token::Kw("class"))
        .ignore_then(
            enumeration(
                choice((
                    ident
                        .clone()
                        .then_ignore(symbol("="))
                        .then(expr.clone())
                        .map(|(key, value)| CallItem::Kwarg(key, value)),
                    expr.clone().map(CallItem::Arg),
                ))
                .spanned()
                .boxed(),
                symbol(","),
            )
            .delimited_by_with_eol(just(Token::Symbol("(")), just(Token::Symbol(")")))
            .or_not(),
        )
        .then_ignore(just(START_BLOCK))
        .then(expr_or_inline_stmt_or_block.clone())
        .map(|(arglist, block)| {
            Expr::Class(arglist.unwrap_or_else(|| Vec::new()), Indirect::new(block))
        })
        .spanned()
        .labelled("class")
        .boxed();

    let classic_if = just(Token::Kw("if"))
        .ignore_then(group((
            expr.clone().then_ignore(just(START_BLOCK)),
            expr_or_inline_stmt_or_block.clone(),
            group((
                just(Token::Eol).or_not(),
                just(Token::Kw("else")),
                just(START_BLOCK).or_not(),
            ))
            .ignore_then(expr_or_inline_stmt_or_block.clone())
            .or_not(),
        )))
        .map(|(cond, if_, else_)| {
            Expr::If(
                Indirect::new(cond),
                Indirect::new(if_),
                else_.map(Indirect::new),
            )
        })
        .spanned()
        .labelled("if")
        .boxed();

    let qualified_ident = ident_expr
        .clone()
        .foldl_with(
            symbol(".").ignore_then(ident.clone()).repeated(),
            |lhs, rhs, e| (Expr::RawAttribute(Indirect::new(lhs), rhs), e.span()),
        )
        .boxed();

    let (closed_pattern, nary_pattern) = match_pattern(
        ident.clone(),
        qualified_ident.clone(),
        expr.clone(),
        literal.clone(),
    );

    let classic_match = just(Token::Kw("match"))
        .ignore_then(expr.clone())
        .then_ignore(just(START_BLOCK))
        .then(cases.clone())
        .map(|(scrutinee, cases)| Expr::Match(Indirect::new(scrutinee), cases))
        .spanned()
        .labelled("classic-match")
        .as_context()
        .boxed();

    let tuple = choice((
        symbol("(")
            .then(symbol(")"))
            .map(|_| Expr::Tuple(vec![]))
            .spanned(),
        nary_tuple
            .clone()
            .delimited_by_with_eol(just(Token::Symbol("(")), just(Token::Symbol(")"))),
    ))
    .boxed();

    enum ControlKw {
        Await,
        Yield,
        YieldFrom,
    }

    let control_kw = choice((
        just(Token::Kw("await")).map(|_| ControlKw::Await),
        just(Token::Kw("yield"))
            .ignore_then(just(Token::Ident("from")).or_not())
            .map(|star| {
                if star.is_some() {
                    ControlKw::YieldFrom
                } else {
                    ControlKw::Yield
                }
            }),
    ))
    .repeated()
    .at_least(1)
    .foldr_with(expr.clone(), |lhs, expr, e| {
        let expr = match lhs {
            ControlKw::Await => Expr::Await(Indirect::new(expr)),
            ControlKw::Yield => Expr::Yield(Indirect::new(expr)),
            ControlKw::YieldFrom => Expr::YieldFrom(Indirect::new(expr)),
        };
        (expr, e.span())
    })
    .labelled("control-expression");

    atom.define(
        choice((
            ident_expr.clone(),
            classic_if,
            classic_match,
            control_kw,
            class_,
            literal_expr.clone(),
            placeholder,
            list.clone(),
            mapping,
            fstr,
            tuple.clone(),
            block
                .clone()
                .delimited_by_with_eol(symbol("("), symbol(")")),
        ))
        .labelled("atom"),
    );

    enum Postfix<'a> {
        Call(Vec<SCallItem<'a>>),
        Subscript(Vec<ListItem<'a>>),
        Attribute(SIdent<'a>),
        ScopedAttribute(SExpr<'a>),
        ScopedAttributeCall(SExpr<'a>, Vec<SCallItem<'a>>),
        RawAttribute(SIdent<'a>),
    }

    let call_args = enumeration(
        choice((
            symbol("*")
                .ignore_then(expr.clone())
                .map(CallItem::ArgSpread),
            symbol("**")
                .ignore_then(expr.clone())
                .map(CallItem::KwargSpread),
            ident
                .clone()
                .then_ignore(symbol("="))
                .then(expr.clone())
                .map(|(key, value)| CallItem::Kwarg(key, value)),
            expr.clone().map(CallItem::Arg),
        ))
        .spanned()
        .boxed(),
        symbol(","),
    )
    .delimited_by_with_eol(just(Token::Symbol("(")), just(Token::Symbol(")")));

    let call = call_args
        .clone()
        .map(Postfix::Call)
        .labelled("argument-list")
        .boxed();

    let subscript = enumeration(
        choice((
            symbol("*").ignore_then(expr.clone()).map(ListItem::Spread),
            expr.clone().map(ListItem::Item),
        ))
        .boxed(),
        symbol(","),
    )
    .delimited_by_with_eol(just(Token::Symbol("[")), just(Token::Symbol("]")))
    .map(Postfix::Subscript)
    .labelled("subscript")
    .boxed();

    let attribute = symbol(".")
        .ignore_then(ident.clone())
        .map(Postfix::Attribute)
        .labelled("extension-attribute");

    let then = symbol(".")
        .ignore_then(expr.clone().delimited_by_with_eol(symbol("("), symbol(")")))
        .then(call_args.or_not())
        .map(|(rhs, args)| {
            if let Some(args) = args {
                Postfix::ScopedAttributeCall(rhs, args)
            } else {
                Postfix::ScopedAttribute(rhs)
            }
        })
        .labelled("then-attribute")
        .boxed();

    let raw_attr = symbol("!")
        .ignore_then(ident.clone())
        .map(Postfix::RawAttribute)
        .labelled("raw-attribute")
        .boxed();

    let postfix = atom
        .clone()
        .foldl_with(
            symbol("?")
                .to(1)
                .or_not()
                .then(choice((call, subscript, attribute, then, raw_attr)))
                .repeated(),
            |expr, (coal, op), e| -> SExpr {
                (
                    if coal.is_none() {
                        match op {
                            Postfix::Call(args) => Expr::Call(Indirect::new(expr), args),
                            Postfix::Subscript(args) => Expr::Subscript(Indirect::new(expr), args),
                            Postfix::RawAttribute(attr) => {
                                Expr::RawAttribute(Indirect::new(expr), attr)
                            }
                            Postfix::ScopedAttribute(rhs) => {
                                Expr::ScopedAttribute(Indirect::new(expr), Indirect::new(rhs))
                            }
                            Postfix::ScopedAttributeCall(rhs, args) => Expr::Call(
                                // TODO optimize this case in the AST by skipping partial application
                                Indirect::new((
                                    Expr::ScopedAttribute(Indirect::new(expr), Indirect::new(rhs)),
                                    e.span(),
                                )),
                                args,
                            ),
                            Postfix::Attribute(rhs) => Expr::Attribute(Indirect::new(expr), rhs),
                        }
                    } else {
                        match op {
                            Postfix::Call(args) => Expr::MappedCall(Indirect::new(expr), args),
                            Postfix::Subscript(args) => {
                                Expr::MappedSubscript(Indirect::new(expr), args)
                            }
                            Postfix::RawAttribute(attr) => {
                                Expr::MappedRawAttribute(Indirect::new(expr), attr)
                            }
                            Postfix::ScopedAttribute(rhs) => {
                                Expr::MappedScopedAttribute(Indirect::new(expr), Indirect::new(rhs))
                            }
                            Postfix::ScopedAttributeCall(rhs, args) => Expr::Call(
                                Indirect::new((
                                    Expr::MappedScopedAttribute(
                                        Indirect::new(expr),
                                        Indirect::new(rhs),
                                    ),
                                    e.span(),
                                )),
                                args,
                            ),
                            Postfix::Attribute(rhs) => {
                                Expr::MappedAttribute(Indirect::new(expr), rhs)
                            }
                        }
                    },
                    e.span(),
                )
            },
        )
        .labelled("postfix")
        .boxed();

    let decorator = postfix
        .clone()
        .then(symbol("&").ignore_then(expr.clone()).or_not())
        .map_with(|(lhs, rhs), e| {
            if let Some(rhs) = rhs {
                (
                    Expr::Decorated(Indirect::new(lhs), Indirect::new(rhs)),
                    e.span(),
                )
            } else {
                lhs
            }
        })
        .labelled("decorator")
        .boxed();

    let mut checked = Recursive::<chumsky::recursive::Indirect<TInput, SExpr, TExtra>>::declare();

    unary.define(
        select! {
            Token::Symbol("@") => UnaryOp::Bind,
            Token::Symbol("+") => UnaryOp::Pos,
            Token::Symbol("-") => UnaryOp::Neg,
            Token::Symbol("~") => UnaryOp::Inv,
        }
        .repeated()
        .foldr_with(
            choice((decorator, checked.clone())),
            |op: UnaryOp, rhs: SExpr, e| (Expr::Unary(op, Indirect::new(rhs)), e.span()),
        )
        .labelled("unary-expression")
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
                    (
                        Expr::Binary(op, Indirect::new(lhs), Indirect::new(rhs)),
                        e.span(),
                    )
                })
                .boxed()
        } else {
            recursive(|bin| {
                arg.clone()
                    .then(op.then(bin.or(arg.clone())).or_not())
                    .map_with(|(lhs, matched), e| {
                        if let Some((op, rhs)) = matched {
                            (
                                Expr::Binary(op, Indirect::new(lhs), Indirect::new(rhs)),
                                e.span(),
                            )
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

    let mut not_or_try =
        Recursive::<chumsky::recursive::Indirect<TInput, SExpr, TExtra>>::declare();

    let not = just(Token::Kw("not"))
        .ignore_then(not_or_try.clone())
        .map_with(|expr, e| (Expr::Unary(UnaryOp::Not, Indirect::new(expr)), e.span()))
        .boxed();

    checked.define(
        just(Token::Kw("try"))
            .ignore_then(not_or_try.clone())
            .then(
                just(Token::Kw("except"))
                    .ignore_then(closed_pattern.clone())
                    .or_not(),
            )
            .map(|(expr, typs)| Expr::Checked(Indirect::new(expr), typs.map(Indirect::new)))
            .spanned()
            .labelled("checked")
            .boxed(),
    );

    let fn_ = function(
        expr_or_inline_stmt_or_block.clone(),
        ident.clone(),
        expr.clone(),
        closed_pattern.clone(),
    );

    not_or_try.define(choice((not, checked, binary3.clone())));

    let binary4 = make_binary_op(
        choice((fn_, not_or_try)),
        select! {
            Token::Symbol("??") => BinaryOp::Coalesce,
        },
        false,
    );

    let slice0 = group((
        binary4.clone(),
        symbol("..").ignore_then(binary4.clone().or_not()).or_not(),
        symbol("..").ignore_then(binary4.clone().or_not()).or_not(),
    ))
    .map_with(|(lhs, a, b), e| {
        if a.is_none() && b.is_none() {
            lhs
        } else {
            (
                Expr::Slice(
                    Some(Indirect::new(lhs)),
                    a.flatten().map(Indirect::new),
                    b.flatten().map(Indirect::new),
                ),
                e.span(),
            )
        }
    })
    .labelled("slice")
    .boxed();

    let slice1 = symbol("..")
        .ignore_then(binary4.clone().or_not())
        .then(symbol("..").ignore_then(binary4.clone().or_not()).or_not())
        .map(|(e1, e2)| Expr::Slice(None, e1.map(Indirect::new), e2.flatten().map(Indirect::new)))
        .spanned()
        .labelled("slice")
        .boxed();

    let slices = choice((slice0, slice1));

    let (match_, cases_) = match_expr(
        slices,
        binary3,
        nary_pattern.clone(),
        expr_or_inline_stmt_or_block.clone(),
    );

    cases.define(cases_);

    let matches = match_
        .then(
            just(Token::Ident("matches"))
                .ignore_then(just(Token::Kw("not")).to(0).or_not())
                .then(closed_pattern.clone())
                .or_not(),
        )
        .map_with(|(expr, matches_part), e| {
            if let Some((not, pattern)) = matches_part {
                if not.is_some() {
                    (
                        Expr::Unary(
                            UnaryOp::Not,
                            Indirect::new((
                                Expr::Matches(Indirect::new(expr), Indirect::new(pattern)),
                                e.span(),
                            )),
                        ),
                        e.span(),
                    )
                } else {
                    (
                        Expr::Matches(Indirect::new(expr), Indirect::new(pattern)),
                        e.span(),
                    )
                }
            } else {
                expr
            }
        });

    let binary5 = make_binary_op(
        matches,
        select! {
            Token::Kw("and") => BinaryOp::And,
            Token::Kw("or") => BinaryOp::Or,
        },
        false,
    );

    let if_ = binary5
        .then(
            group((
                just(Token::Kw("then"))
                    .then(just(START_BLOCK).or_not())
                    .ignore_then(expr_or_inline_stmt_or_block.clone()),
                just(Token::Eol)
                    .or_not()
                    .then(just(Token::Kw("else")))
                    .then(just(START_BLOCK).or_not())
                    .ignore_then(expr_or_inline_stmt_or_block.clone())
                    .or_not(),
            ))
            .or_not(),
        )
        .map_with(|(cond, if_cases), e| {
            if let Some((if_, else_)) = if_cases {
                (
                    Expr::If(
                        Indirect::new(cond),
                        Indirect::new(if_),
                        else_.map(Indirect::new),
                    ),
                    e.span(),
                )
            } else {
                cond
            }
        });

    below_pipe.define(if_);

    let binary6 = make_binary_op(
        below_pipe.clone(),
        select! {
            Token::Symbol("|") => BinaryOp::Pipe,
        },
        false,
    );

    expr.define(
        binary6.labelled("expression").as_context().boxed(), // .memoized(),
    );

    // Statements

    let decl_mod = choice((
        just(Token::Kw("export")).to(DeclType::Export),
        just(Token::Kw("global")).to(DeclType::Global),
        just(Token::Kw("let")).to(DeclType::Let),
        just(Token::Kw("const")).to(DeclType::Const),
    ));

    let decl_stmt = decl_mod
        .clone()
        .then(ident.clone().separated_by(symbol(",")).collect())
        .map(|(decl, idents)| Stmt::Decl(idents, decl))
        .boxed();

    let assign_stmt = group((
        decl_mod.clone().or_not(),
        nary_tuple.clone(),
        symbol("=").ignore_then(nary_tuple.clone()),
    ))
    .map(|(decl, lhs, rhs)| Stmt::Assign(lhs.into(), rhs.into(), decl))
    .boxed();

    let inline_assign_stmt = group((
        decl_mod.clone().or_not(),
        expr.clone(),
        symbol("=").ignore_then(expr.clone()),
    ))
    .map(|(decl, lhs, rhs)| Stmt::Assign(lhs.into(), rhs.into(), decl))
    .boxed();

    let expr_stmt = nary_tuple.clone().map(Stmt::Expr).boxed();

    let inline_expr_stmt = expr.clone().map(Stmt::Expr).boxed();

    let while_stmt = just(Token::Kw("while"))
        .ignore_then(expr.clone())
        .then_ignore(just(START_BLOCK))
        .then(expr_or_inline_stmt_or_block.clone())
        .map(|(cond, body)| Stmt::While(cond, body))
        .labelled("while statement")
        .boxed();

    let except_block = just(Token::Eol)
        .then(just(Token::Kw("except")))
        .ignore_then(nary_pattern.clone().or_not())
        .boxed()
        .then(just(START_BLOCK).ignore_then(expr_or_inline_stmt_or_block.clone()))
        .map(|(pattern, body)| MatchCase {
            pattern: pattern.map(Indirect::new),
            guard: None,
            body,
        })
        .labelled("except block")
        .boxed();

    let finally_block = one_of([Token::Eol])
        .then(just(Token::Kw("finally")))
        .then(just(START_BLOCK))
        .ignore_then(expr_or_inline_stmt_or_block.clone())
        .labelled("finally block")
        .boxed();

    let try_stmt = just(Token::Kw("try"))
        .then(just(START_BLOCK))
        .ignore_then(group((
            expr_or_inline_stmt_or_block.clone(),
            except_block.map(Indirect::new).repeated().collect(),
            finally_block.or_not(),
        )))
        .map(|(body, excepts, finally)| Stmt::Try(body, excepts, finally))
        .labelled("try statement")
        .boxed();

    let for_stmt = just(Token::Kw("for"))
        .ignore_then(group((
            nary_pattern.clone().then_ignore(just(Token::Kw("in"))),
            expr.clone().then_ignore(just(START_BLOCK)),
            expr_or_inline_stmt_or_block.clone(),
        )))
        .map(|(decl, iter, body)| Stmt::For(decl, iter, body))
        .labelled("for statement")
        .boxed();

    let return_stmt = just(Token::Kw("return"))
        .ignore_then(nary_tuple.clone())
        .map(Stmt::Return)
        .labelled("return statement")
        .boxed();

    let inline_return_stmt = just(Token::Kw("return"))
        .ignore_then(expr.clone())
        .map(Stmt::Return)
        .labelled("inline return statement")
        .boxed();

    let assert_stmt = just(Token::Ident("assert"))
        .ignore_then(expr.clone())
        .then(symbol(",").ignore_then(expr.clone()).or_not())
        .map(|(x, y)| Stmt::Assert(x, y))
        .labelled("assert statement")
        .boxed();

    let raise_stmt = just(Token::Kw("raise"))
        .ignore_then(nary_tuple.clone().or_not())
        .map(Stmt::Raise)
        .labelled("raise statement")
        .boxed();

    let inline_raise_stmt = just(Token::Kw("raise"))
        .ignore_then(expr.clone().or_not())
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
            symbol(".").repeated().count(),
            ident
                .clone()
                .then_ignore(symbol("."))
                .repeated()
                .collect()
                .boxed(),
            choice((
                enumeration(
                    ident
                        .clone()
                        .then(just(Token::Kw("as")).ignore_then(ident.clone()).or_not()),
                    symbol(","),
                )
                .delimited_by_with_eol(symbol("("), symbol(")"))
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
            decl_stmt.then_ignore(just(Token::Eol)),
            assign_stmt.then_ignore(just(Token::Eol)),
            expr_stmt.then_ignore(just(Token::Eol)),
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
            inline_assign_stmt,
            inline_expr_stmt,
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
) -> (
    Option<Vec<SStmt<'src>>>,
    Vec<Rich<'tokens, Token<'src>, Span>>,
) {
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
