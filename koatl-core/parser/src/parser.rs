#![allow(dead_code)]

use std::borrow::Cow;

use crate::ast::*;
use crate::lexer::*;
use crate::parser_error::TriviaRich;
use chumsky::input::BorrowInput;
use chumsky::label::LabelError;
use chumsky::prelude::*;

const START_BLOCK: Token = Token::Symbol(":");
type TErr<'tokens, 'src> = TriviaRich<'tokens, 'src, Span>;
type TExtra<'tokens, 'src> = extra::Err<TErr<'tokens, 'src>>;

fn token<'tokens, 'src: 'tokens, I>(
    token: Token<'src>,
) -> impl Parser<'tokens, I, &'tokens SToken<'src>, TExtra<'tokens, 'src>> + Clone
where
    I: BorrowInput<'tokens, Token = SToken<'src>, Span = Span>,
{
    custom(move |input| {
        let start = input.cursor();

        let next: Option<&SToken<'src>> = input.next_ref();

        if let Some(found) = next {
            if found.token == token {
                return Ok(found);
            }
        }

        Err(LabelError::<I, _>::expected_found(
            vec![format!("{}", token)],
            next.map(|x| x.into()),
            input.span_since(&start),
        ))
    })
}

pub fn symbol<'tokens, 'src: 'tokens, I>(
    symbol: &'static str,
) -> impl Parser<'tokens, I, &'tokens SToken<'src>, TExtra<'tokens, 'src>> + Clone
where
    I: BorrowInput<'tokens, Token = SToken<'src>, Span = Span>,
{
    token(Token::Symbol(symbol))
}

pub fn parse_tokens<'tokens, 'src: 'tokens>(
    src: &'src str,
    tokens: &'tokens TokenList<'src>,
) -> (Option<SExpr<'src>>, Vec<TriviaRich<'tokens, 'src>>) {
    let input = tokens
        .0
        .as_slice()
        .map((src.len()..src.len()).into(), |tok| (tok, &tok.span));

    let p = token(Token::Indent)
        .ignore_then(symbol("-").map(|sym| {
            Expr::Unary(
                UnaryOp::Neg,
                Expr::Literal(
                    Literal::Str(
                        format!("{:?} {:?}", sym.leading_trivia, sym.trailing_trivia).into(),
                    )
                    .spanned(sym.span),
                )
                .spanned(sym.span)
                .indirect(),
            )
            .spanned(sym.span)
        }))
        .then_ignore(token(Token::Eol))
        .then_ignore(token(Token::Dedent));

    p.parse(input).into_output_errors()
}
