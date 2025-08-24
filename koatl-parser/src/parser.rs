#![allow(dead_code)]

use crate::cst::*;
use crate::lexer::*;
use crate::parser_error::TriviaRich;
use chumsky::input::BorrowInput;
use chumsky::label::LabelError;
use chumsky::prelude::*;

const START_BLOCK: Token = Token::Symbol(":");
type TErr<'src, 'tok> = TriviaRich<'tok, 'src, Span>;
type TExtra<'src, 'tok> = extra::Err<TErr<'src, 'tok>>;

fn token<'src: 'tok, 'tok, I>(
    token: Token<'src>,
) -> impl Parser<'tok, I, &'tok SToken<'src>, TExtra<'src, 'tok>> + Clone
where
    I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
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

pub fn symbol<'src: 'tok, 'tok, I>(
    symbol: &'static str,
) -> impl Parser<'tok, I, &'tok SToken<'src>, TExtra<'src, 'tok>> + Clone
where
    I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
{
    token(Token::Symbol(symbol))
}

pub fn kw<'src: 'tok, 'tok, I>(
    symbol: &'static str,
) -> impl Parser<'tok, I, &'tok SToken<'src>, TExtra<'src, 'tok>> + Clone
where
    I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
{
    token(Token::Kw(symbol))
}

pub fn ident<'src: 'tok, 'tok, I>(
    symbol: &'static str,
) -> impl Parser<'tok, I, &'tok SToken<'src>, TExtra<'src, 'tok>> + Clone
where
    I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
{
    token(Token::Ident(symbol))
}

fn listing<'src: 'tok, 'tok, I, O: 'tok, ItemParser>(
    begin: &'static str,
    end: &'static str,
    optional_separator: Token<'src>,
    item_parser: ItemParser,
) -> impl Parser<'tok, I, Listing<O, STree<'src, 'tok>>, TExtra<'src, 'tok>> + Clone
where
    I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
    ItemParser: Parser<'tok, I, O, TExtra<'src, 'tok>> + Clone + 'tok,
{
    let item_parser1 = item_parser;
    let item_parser2 = item_parser1.clone();

    let sep_parser1 = token(optional_separator);
    let sep_parser2 = sep_parser1.clone();

    choice((
        group((
            symbol(begin),
            token(Token::Indent),
            custom(move |input| {
                let mut acc = vec![];
                loop {
                    let before_item = input.save();
                    if let Ok(item) = input.parse(&item_parser1) {
                        let mut sep = None;
                        let before_sep = input.save();

                        if let Ok(sep_) = input.parse(&sep_parser1) {
                            sep = Some(sep_);
                        } else {
                            input.rewind(before_sep);
                        }

                        let mut newl = None;
                        let before_newl = input.save();

                        if let Ok(newl_) = input.parse(token(Token::Eol)) {
                            newl = Some(newl_);
                        } else {
                            input.rewind(before_newl);
                        }

                        if newl.is_none() && sep.is_none() {
                            break;
                        }

                        acc.push(ListingItem {
                            item,
                            separator: sep,
                            newline: newl,
                        });
                    } else {
                        input.rewind(before_item);
                        break;
                    }
                }

                Ok(acc)
            }),
            token(Token::Dedent),
            token(Token::Eol).or_not(),
            symbol(end),
        ))
        .map(|(begin, indent, items, dedent, newline, end)| Listing {
            begin,
            indent: Some(indent),
            items,
            newline,
            dedent: Some(dedent),
            end,
        })
        .labelled("block enumeration"),
        group((
            symbol(begin),
            custom(move |input| {
                let mut acc = vec![];
                loop {
                    let before_item = input.save();
                    if let Ok(item) = input.parse(&item_parser2) {
                        let mut sep = None;
                        let before_sep = input.save();

                        if let Ok(sep_) = input.parse(&sep_parser2) {
                            sep = Some(sep_);
                        } else {
                            input.rewind(before_sep);
                        }

                        if sep.is_none() {
                            break;
                        }

                        acc.push(ListingItem {
                            item,
                            separator: sep,
                            newline: None,
                        });
                    } else {
                        input.rewind(before_item);
                        break;
                    }
                }

                Ok(acc)
            }),
            symbol(end),
        ))
        .map(|(begin, items, end)| Listing {
            begin,
            indent: None,
            items,
            newline: None,
            dedent: None,
            end,
        })
        .labelled("inline-enumeration"),
    ))
    .labelled("enumeration")
    .boxed()
}

pub fn parse_tokens<'src: 'tok, 'tok>(
    src: &'src str,
    tokens: &'tok TokenList<'src>,
) -> (Option<SExpr<'src, 'tok>>, Vec<TriviaRich<'tok, 'src>>) {
    let input = tokens
        .0
        .as_slice()
        .map((src.len()..src.len()).into(), |tok| (tok, &tok.span));

    let p = group((
        token(Token::Indent),
        listing(
            "[",
            "]",
            Token::Symbol(","),
            ident("asdf").map_with(|x, e| ListItem::Item(Expr::Ident(x).spanned(e.span()).boxed())),
        )
        .map_with(|x, e| Expr::List(x).spanned(e.span())),
        token(Token::Eol),
        token(Token::Dedent),
    ))
    .map(|(_, x, _, _)| x);

    p.parse(input).into_output_errors()
}
