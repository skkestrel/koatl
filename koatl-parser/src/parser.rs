#![allow(dead_code)]

use std::borrow::Cow;
use std::ops::Range;

use chumsky::span::Span;

use crate::cst::*;
use crate::lexer::*;
use crate::parser_error::TriviaRich;

const START_BLOCK: Token = Token::Symbol(":");

#[derive(Debug, Clone)]
enum ErrMsg<'a> {
    Expected(Cow<'a, str>),
    Custom(Cow<'a, str>),
}

#[derive(Debug, Clone)]
struct ParseErr<'a> {
    span: Range<usize>,
    message: ErrMsg<'a>,
}

trait Parser<'src, 'tok, O>: Clone + Fn(&mut ParseCtx<'src, 'tok>) -> TlResult<'src, O> {}
impl<'src, 'tok, O, F> Parser<'src, 'tok, O> for F where
    F: Clone + Fn(&mut ParseCtx<'src, 'tok>) -> TlResult<'src, O>
{
}

impl<'a> ParseErr<'a> {
    fn expected(exp: impl Into<Cow<'a, str>>, span: Range<usize>) -> ParseErr<'a> {
        ParseErr {
            span: span.start()..span.end(),
            message: ErrMsg::Expected(exp.into()),
        }
    }
}

type TlResult<'a, T> = Result<T, ParseErr<'a>>;

struct ParseCtx<'src: 'tok, 'tok> {
    src: &'src str,
    input: &'tok [SToken<'src>],
    cursor: usize,
}

macro_rules! first_of {
    ($ctx:expr, $name:literal, $($parser:expr),+) => {
        'first_of: {
            let start = $ctx.save();
            let mut furthest = $ctx.save();
            let mut furthest_err = ParseErr::expected($name, start..$ctx.cursor+1);

            $(
                let before = $ctx.save();
                match $parser($ctx) {
                    Ok(result) => break 'first_of Ok(result),
                    Err(err) => {
                        if $ctx.save() > furthest {
                            furthest = $ctx.save();
                            furthest_err = err;
                        }
                    }
                }
                $ctx.rewind(before);
            )+

            $ctx.rewind(furthest);

            Err(furthest_err)
        }
    };
}

macro_rules! optional {
    ($ctx:expr, $name:literal, $parser:expr) => {{
        let start = $ctx.cursor;
        Ok(match $parser($ctx) {
            Ok(result) => Some(result),
            Err(_) => {
                $ctx.rewind(start);
                None
            }
        })
    }};
}

impl<'src: 'tok, 'tok> ParseCtx<'src, 'tok> {
    fn save(&self) -> usize {
        self.cursor
    }

    fn rewind(&mut self, cursor: usize) {
        self.cursor = cursor;
    }

    fn peek_token(&mut self) -> Option<&'tok SToken<'src>> {
        self.input.get(self.cursor)
    }

    fn next(&mut self) {
        self.cursor += 1;
    }

    fn next_token(&mut self) -> Option<&'tok SToken<'src>> {
        let token = self.input.get(self.cursor)?;
        self.cursor += 1;
        Some(token)
    }

    fn token(&mut self, token: &Token<'src>) -> TlResult<'src, &'tok SToken<'src>> {
        let start = self.cursor;

        let next: Option<&SToken<'src>> = self.peek_token();

        if let Some(found) = next {
            if found.token == *token {
                self.next();
                return Ok(found);
            }
        }

        Err(ParseErr::expected(
            format!("{}", token),
            start..self.cursor + 1,
        ))
    }

    fn symbol(&mut self, sym: &'static str) -> TlResult<'src, &'tok SToken<'src>> {
        return self
            .token(&Token::Symbol(sym))
            .map_err(|e| ParseErr::expected(sym, e.span));
    }

    fn ident(&mut self) -> TlResult<'src, &'tok SToken<'src>> {
        let start = self.cursor;

        let next: Option<&SToken<'src>> = self.peek_token();

        if let Some(found) = next {
            if let Token::Ident(_) = found.token {
                self.next();
                return Ok(found);
            }
        }

        Err(ParseErr::expected("identifier", start..self.cursor + 1))
    }

    fn literal(&mut self) -> TlResult<'src, &'tok SToken<'src>> {
        let start = self.cursor;

        let next: Option<&SToken<'src>> = self.peek_token();

        if let Some(found) = next {
            if let Token::Num(_) | Token::Str(_) | Token::Bool(_) | Token::None = found.token {
                self.next();
                return Ok(found);
            }
        }

        Err(ParseErr::expected("identifier", start..self.cursor + 1))
    }

    fn listing<O>(
        &mut self,
        begin: &'static str,
        end: &'static str,
        optional_separator: Token<'src>,
        item_parser: impl Parser<'src, 'tok, O>,
    ) -> TlResult<'src, SListing<'src, 'tok, O>> {
        let item_parser2 = item_parser.clone();

        let block_body = |ctx: &mut Self| {
            let begin_tok = ctx.symbol(begin)?;
            let indent = ctx.token(&Token::Indent)?;

            let mut acc = vec![];
            loop {
                let before_item = ctx.save();
                if let Ok(item) = item_parser(ctx) {
                    let mut separator = None;
                    let before_sep = ctx.save();

                    if let Ok(sep) = ctx.token(&optional_separator) {
                        separator = Some(sep);
                    } else {
                        ctx.rewind(before_sep);
                    }

                    let mut newline = None;
                    let before_newl = ctx.save();

                    if let Ok(newl) = ctx.token(&Token::Eol) {
                        newline = Some(newl);
                    } else {
                        ctx.rewind(before_newl);
                    }

                    acc.push(ListingItem {
                        item,
                        separator,
                        newline,
                    });

                    if newline.is_none() && separator.is_none() {
                        break;
                    }
                } else {
                    println!("rewind {} {}", ctx.cursor, before_item);
                    ctx.rewind(before_item);
                    break;
                }
            }

            let dedent = ctx.token(&Token::Dedent)?;
            let newline = optional!(ctx, "newline", |ctx: &mut Self| ctx.token(&Token::Eol))?;

            let end_tok = ctx.symbol(end)?;

            Ok((begin_tok, Some(indent), acc, Some(dedent), newline, end_tok))
        };

        let inline_body = |ctx: &mut Self| {
            let mut acc = vec![];

            let begin_tok = ctx.symbol(begin)?;

            loop {
                let before_item = ctx.save();

                if let Ok(item) = item_parser2(ctx) {
                    let mut separator = None;
                    let before_sep = ctx.save();

                    if let Ok(sep) = ctx.token(&optional_separator) {
                        separator = Some(sep);
                    } else {
                        ctx.rewind(before_sep);
                    }

                    acc.push(ListingItem {
                        item,
                        separator,
                        newline: None,
                    });

                    if separator.is_none() {
                        break;
                    }
                } else {
                    ctx.rewind(before_item);
                    break;
                }
            }

            let end_tok = ctx.symbol(end)?;

            Ok((begin_tok, None, acc, None, None, end_tok))
        };

        let (begin, indent, items, dedent, newline, end) =
            first_of!(self, "listing body", block_body, inline_body)?;

        Ok(Listing {
            begin,
            indent,
            items,
            dedent,
            newline,
            end,
        })
    }

    fn atom(&mut self) -> TlResult<'src, SExpr<'src, 'tok>> {
        let start = self.cursor;

        let name = |ctx: &mut Self| ctx.ident().map(|token| Expr::Ident { token });
        let literal = |ctx: &mut Self| ctx.literal().map(|token| Expr::Literal { token });

        let atom = first_of!(self, "atomic expression", name, literal)?;
        Ok(atom.spanned(Span::new((), start..self.cursor)))
    }
}

pub fn parse_tokens<'src: 'tok, 'tok>(
    src: &'src str,
    tokens: &'tok TokenList<'src>,
) -> (Option<SExpr<'src, 'tok>>, Vec<TriviaRich<'tok, 'src>>) {
    let mut ctx = ParseCtx {
        src,
        input: &tokens.0,
        cursor: 0,
    };

    ctx.token(&Token::Indent);
    let listing = ctx.listing("[", "]", Token::Symbol(","), |ctx| {
        Ok(ListItem::Item {
            expr: ctx.atom()?.boxed(),
        })
    });
    ctx.token(&Token::Eol);
    ctx.token(&Token::Dedent);

    match listing {
        Ok(listing) => (
            Some(Expr::List { listing }.spanned(Span::new((), 0..0))),
            vec![],
        ),
        Err(err) => {
            println!("Parse error: {:#?}", err);
            println!("Tokens: {}", tokens);

            (
                None,
                vec![TriviaRich::custom(
                    Span::new(
                        (),
                        tokens.0[err.span.start].span.start..tokens.0[err.span.end - 1].span.end,
                    ),
                    format!("{:?}", err.message),
                )],
            )
        }
    }
}
