#![allow(dead_code)]

use std::borrow::Cow;
use std::ops::Range;

use crate::cst::*;
use crate::lexer::*;
use crate::parser_error::TriviaRich;
use crate::simple_fmt::SimpleFmt;

const START_BLOCK: Token = Token::Symbol(":");

#[derive(Debug, Clone)]
enum ErrMsg<'a> {
    Unexpected,
    Expected(Cow<'a, str>),
    Custom(Cow<'a, str>),
}

#[derive(Debug, Clone)]
struct ParseErr<'a> {
    span: Range<usize>,
    message: ErrMsg<'a>,
}

trait Parser<'src, 'tok, O>: Fn(&mut ParseCtx<'src, 'tok>) -> ParseResult<O> {}
impl<'src, 'tok, O, F> Parser<'src, 'tok, O> for F where
    F: Clone + Fn(&mut ParseCtx<'src, 'tok>) -> ParseResult<O>
{
}

impl<'a> ParseErr<'a> {
    fn unexpected(span: Range<usize>) -> ParseErr<'a> {
        ParseErr {
            span,
            message: ErrMsg::Unexpected,
        }
    }

    fn expected(exp: impl Into<Cow<'a, str>>, span: Range<usize>) -> ParseErr<'a> {
        ParseErr {
            span,
            message: ErrMsg::Expected(exp.into()),
        }
    }

    fn custom(msg: impl Into<Cow<'a, str>>, span: Range<usize>) -> ParseErr<'a> {
        ParseErr {
            span,
            message: ErrMsg::Custom(msg.into()),
        }
    }
}

type ParseResult<T> = Result<T, ()>;

struct ParseCtx<'src: 'tok, 'tok> {
    src: &'src str,
    input: &'tok [SToken<'src>],
    cursor: usize,

    errors: Vec<ParseErr<'src>>,

    cur_error: Option<(usize, ParseErr<'src>)>,
}

macro_rules! first_of {
    ($ctx:expr, $name:literal, $($parser:expr),+) => {
        'first_of: {
            // Set a default error if none of the choices make progress
            $ctx.set_error($ctx.cursor, ParseErr::expected($name, $ctx.cursor..$ctx.cursor+1));

            $(
                let before = $ctx.save();
                match $ctx.parse($parser) {
                    Ok(result) => break 'first_of Ok(result),
                    Err(()) => {}
                }
                $ctx.rewind(before);
            )+

            Err(())
        }
    };
}

macro_rules! optional {
    ($ctx:expr, $parser:expr) => {{
        let start = $ctx.cursor;
        Ok(match $ctx.parse($parser) {
            Ok(result) => Some(result),
            Err(_) => {
                $ctx.rewind(start);
                None
            }
        })
    }};
}

enum ExprPrec {
    CaseGuard,
    All,
    Tryable,
}

pub fn true_span(start: usize, end: usize, input: &[SToken]) -> Span {
    let start = if start < input.len() {
        input[start].span.start
    } else {
        input.last().map_or(0, |t| t.span.end)
    };

    let end = if end <= input.len() {
        input[end.saturating_sub(1)].span.end
    } else {
        input.last().map_or(0, |t| t.span.end)
    };

    Span::from(start..end)
}

impl<'src: 'tok, 'tok> ParseCtx<'src, 'tok> {
    fn save(&self) -> usize {
        self.cursor
    }

    fn rewind(&mut self, cursor: usize) {
        self.cursor = cursor;
    }

    fn peek_token(&self) -> Option<&'tok SToken<'src>> {
        self.input.get(self.cursor)
    }

    fn next(&mut self) {
        self.cursor += 1;
    }

    fn set_error(&mut self, cursor: usize, err: ParseErr<'src>) {
        if let Some((furthest_cursor, _)) = &self.cur_error {
            if cursor > *furthest_cursor {
                self.cur_error = Some((cursor, err));
            }
        } else {
            self.cur_error = Some((cursor, err));
        }
    }

    fn take_error(&mut self) -> (usize, ParseErr<'src>) {
        self.cur_error
            .take()
            .unwrap_or((0, ParseErr::expected("unknown error", 0..0)))
    }

    fn span_from(&self, start: usize) -> Span {
        true_span(start, self.cursor, self.input)
    }

    fn next_token(&mut self) -> Option<&'tok SToken<'src>> {
        let token = self.input.get(self.cursor)?;
        self.cursor += 1;
        Some(token)
    }

    fn parse<O>(&mut self, f: impl Fn(&mut Self) -> ParseResult<O>) -> ParseResult<O> {
        f(self)
    }

    // Parsers

    fn token(&mut self, token: &Token<'src>) -> ParseResult<&'tok SToken<'src>> {
        let start = self.cursor;

        let next: Option<&SToken<'src>> = self.next_token();

        if let Some(found) = next {
            if found.token == *token {
                return Ok(found);
            }
        }

        Err(self.set_error(
            start,
            ParseErr::expected(format!("{}", token), start..self.cursor),
        ))
    }

    fn symbol(&mut self, sym: &'static str) -> ParseResult<&'tok SToken<'src>> {
        self.token(&Token::Symbol(sym))
    }

    fn keyword(&mut self, kw: &'static str) -> ParseResult<&'tok SToken<'src>> {
        self.token(&Token::Kw(kw))
    }

    fn ident(&mut self, ident: &'static str) -> ParseResult<&'tok SToken<'src>> {
        self.token(&Token::Ident(ident))
    }

    fn any_ident(&mut self) -> ParseResult<&'tok SToken<'src>> {
        let start = self.cursor;

        let next: Option<&SToken<'src>> = self.next_token();

        if let Some(found) = next {
            if let Token::Ident(_) = found.token {
                return Ok(found);
            }
        }

        Err(self.set_error(start, ParseErr::expected("identifier", start..self.cursor)))
    }

    fn literal(&mut self) -> ParseResult<&'tok SToken<'src>> {
        let start = self.cursor;

        let next: Option<&SToken<'src>> = self.next_token();

        if let Some(found) = next {
            if let Token::Num(_) | Token::Str(_) | Token::Bool(_) | Token::None = found.token {
                return Ok(found);
            }
        }

        Err(self.set_error(start, ParseErr::expected("literal", start..self.cursor)))
    }

    fn listing<O: std::fmt::Debug>(
        &mut self,
        begin: &'static str,
        end: &'static str,
        optional_separator: Token<'src>,
        item_parser: impl Parser<'src, 'tok, O>,
    ) -> ParseResult<SListing<'src, 'tok, O>> {
        let block_body = |ctx: &mut Self| {
            let begin = ctx.symbol(begin)?;

            let indent = ctx.token(&Token::Indent)?;

            let mut acc = vec![];

            loop {
                let parsed = optional!(ctx, |ctx: &mut Self| {
                    let item = ctx.parse(&item_parser)?;
                    let separator =
                        optional!(ctx, |ctx: &mut Self| ctx.token(&optional_separator))?;
                    let newline = optional!(ctx, |ctx: &mut Self| ctx.token(&Token::Eol))?;

                    Ok((item, separator, newline))
                })?;

                let Some((item, separator, newline)) = parsed else {
                    break;
                };

                acc.push(ListingItem {
                    item,
                    separator,
                    newline,
                });

                if newline.is_none() && separator.is_none() {
                    break;
                }
            }

            let dedent = ctx.token(&Token::Dedent)?;
            let newline = optional!(ctx, |ctx: &mut Self| ctx.token(&Token::Eol))?;

            let end = ctx.symbol(end)?;

            Ok(Listing::Block {
                begin,
                indent,
                items: acc,
                newline,
                dedent,
                end,
            })
        };

        let inline_body = |ctx: &mut Self| {
            let begin = ctx.symbol(begin)?;
            let mut acc = vec![];

            loop {
                let parsed = optional!(ctx, |ctx: &mut Self| {
                    let item = ctx.parse(&item_parser)?;
                    let separator =
                        optional!(ctx, |ctx: &mut Self| ctx.token(&optional_separator))?;

                    Ok((item, separator))
                })?;

                let Some((item, separator)) = parsed else {
                    break;
                };

                acc.push(ListingItem {
                    item,
                    separator,
                    newline: None,
                });

                if separator.is_none() {
                    break;
                }
            }

            let end = ctx.symbol(end)?;

            Ok(Listing::Inline {
                begin,
                items: acc,
                end,
            })
        };

        let listing = first_of!(self, "listing", block_body, inline_body)?;

        Ok(listing)
    }

    // Patterns

    fn qualified_ident(&mut self) -> ParseResult<SExpr<'src, 'tok>> {
        let start = self.cursor;
        let mut expr = Expr::Ident {
            token: self.any_ident()?,
        }
        .spanned(self.span_from(start));

        loop {
            let parsed = optional!(self, |ctx| {
                let dot = ctx.symbol(".")?;
                let attr = ctx.any_ident()?;
                Ok((dot, attr))
            })?;

            let Some((dot, attr)) = parsed else {
                break;
            };

            expr = Expr::Attribute {
                expr: expr.boxed(),
                question: None,
                dot,
                attr,
            }
            .spanned(self.span_from(start));
        }

        Ok(expr)
    }

    fn or_pattern(&mut self) -> ParseResult<SPattern<'src, 'tok>> {
        let start = self.cursor;
        let first = self.atom_pattern()?;

        let mut alts = vec![];

        loop {
            let parsed = optional!(self, |ctx| {
                let pipe = ctx.symbol("|")?;
                let pattern = ctx.atom_pattern()?;
                Ok((pipe, pattern))
            })?;

            let Some((pipe, pattern)) = parsed else {
                break;
            };

            alts.push((pipe, pattern.boxed()));
        }

        if !alts.is_empty() {
            return Ok(Pattern::Or {
                head: first.boxed(),
                rest: alts,
            }
            .spanned(self.span_from(start)));
        }

        Ok(first)
    }

    fn open_pattern(&mut self) -> ParseResult<SPattern<'src, 'tok>> {
        self.set_error(
            self.cursor,
            ParseErr::expected("open pattern", self.cursor..self.cursor + 1),
        );

        let before_star = self.cursor;

        let item = self.pattern_sequence_item()?;

        self.set_error(
            self.cursor,
            ParseErr::unexpected(self.cursor..self.cursor + 1),
        );

        let comma = optional!(self, |ctx: &mut Self| ctx.symbol(","))?;

        if comma.is_none() {
            match item {
                PatternSequenceItem::Item { pattern } => return Ok(*pattern),
                PatternSequenceItem::Spread { .. } => {
                    return Err(self.set_error(
                        before_star,
                        ParseErr::custom(
                            "Spread is not allowed outside of tuples",
                            before_star..before_star + 1,
                        ),
                    ));
                }
            }
        }

        let mut acc = vec![ListingItem {
            item,
            separator: comma,
            newline: None,
        }];

        loop {
            self.set_error(
                self.cursor,
                ParseErr::expected("pattern", self.cursor..self.cursor + 1),
            );

            let Some(item) = optional!(self, Self::pattern_sequence_item)? else {
                break;
            };

            let comma = optional!(self, |ctx: &mut Self| ctx.symbol(","))?;

            acc.push(ListingItem {
                item,
                separator: comma,
                newline: None,
            });

            if comma.is_none() {
                break;
            }
        }

        Ok(Pattern::Sequence {
            listing: Listing::Open { items: acc },
        }
        .spanned(self.span_from(before_star)))
    }

    fn as_pattern(&mut self) -> ParseResult<SPattern<'src, 'tok>> {
        let start = self.cursor;
        let pattern = self.or_pattern()?;

        let parsed = optional!(self, |ctx| {
            let as_kw = ctx.keyword("as")?;
            let name = ctx.any_ident()?;
            Ok((as_kw, name))
        })?;

        if let Some((as_kw, name)) = parsed {
            return Ok(Pattern::As {
                pattern: pattern.boxed(),
                as_kw,
                name,
            }
            .spanned(self.span_from(start)));
        }

        Ok(pattern)
    }

    fn atom_pattern(&mut self) -> ParseResult<SPattern<'src, 'tok>> {
        first_of!(
            self,
            "atom pattern",
            |ctx| ctx.literal_pattern(),
            |ctx| ctx.class_pattern(),
            |ctx| ctx.capture_pattern(),
            |ctx| ctx.value_pattern(),
            |ctx| ctx.parenthesized_pattern(),
            |ctx| ctx.sequence_pattern(),
            |ctx| ctx.tuple_sequence_pattern(),
            |ctx| ctx.mapping_pattern()
        )
    }

    fn literal_pattern(&mut self) -> ParseResult<SPattern<'src, 'tok>> {
        let start = self.cursor;
        let token = self.literal()?;
        Ok(Pattern::Literal { token }.spanned(self.span_from(start)))
    }

    fn capture_pattern(&mut self) -> ParseResult<SPattern<'src, 'tok>> {
        let start = self.cursor;
        let name = self.any_ident()?;
        Ok(Pattern::Capture { name }.spanned(self.span_from(start)))
    }

    fn value_pattern(&mut self) -> ParseResult<SPattern<'src, 'tok>> {
        let start = self.cursor;
        let dot = self.symbol(".")?;
        let expr = self.expr()?.boxed();
        Ok(Pattern::Value { dot, expr }.spanned(self.span_from(start)))
    }

    fn parenthesized_pattern(&mut self) -> ParseResult<SPattern<'src, 'tok>> {
        let start = self.cursor;
        let lparen = self.symbol("(")?;
        let pattern = self.open_pattern()?.boxed();
        let rparen = self.symbol(")")?;
        Ok(Pattern::Parenthesized {
            lparen,
            pattern,
            rparen,
        }
        .spanned(self.span_from(start)))
    }

    fn sequence_pattern(&mut self) -> ParseResult<SPattern<'src, 'tok>> {
        let start = self.cursor;
        let listing = self.listing("[", "]", Token::Symbol(","), |ctx| {
            ctx.pattern_sequence_item()
        })?;
        Ok(Pattern::Sequence { listing }.spanned(self.span_from(start)))
    }

    fn tuple_sequence_pattern(&mut self) -> ParseResult<SPattern<'src, 'tok>> {
        let start = self.cursor;
        let listing = self.listing("(", ")", Token::Symbol(","), |ctx| {
            ctx.pattern_sequence_item()
        })?;
        Ok(Pattern::TupleSequence { listing }.spanned(self.span_from(start)))
    }

    fn mapping_pattern(&mut self) -> ParseResult<SPattern<'src, 'tok>> {
        let start = self.cursor;
        let listing = self.listing("{", "}", Token::Symbol(","), |ctx| {
            ctx.pattern_mapping_item()
        })?;
        Ok(Pattern::Mapping { listing }.spanned(self.span_from(start)))
    }

    fn class_pattern(&mut self) -> ParseResult<SPattern<'src, 'tok>> {
        let start = self.cursor;
        let cls = self.qualified_ident()?;
        let items = self.listing("(", ")", Token::Symbol(","), |ctx| ctx.pattern_class_item())?;
        Ok(Pattern::Class {
            expr: cls.boxed(),
            items,
        }
        .spanned(self.span_from(start)))
    }

    fn pattern_sequence_item(&mut self) -> ParseResult<PatternSequenceItem<STree<'src, 'tok>>> {
        first_of!(
            self,
            "pattern sequence item",
            |ctx| {
                let star = ctx.symbol("*")?;
                let name = ctx.any_ident()?;
                Ok(PatternSequenceItem::Spread { star, name })
            },
            |ctx| {
                let pattern = ctx.as_pattern()?;
                Ok(PatternSequenceItem::Item {
                    pattern: pattern.boxed(),
                })
            }
        )
    }

    fn pattern_mapping_item(&mut self) -> ParseResult<PatternMappingItem<STree<'src, 'tok>>> {
        first_of!(
            self,
            "pattern mapping item",
            |ctx| {
                let stars = ctx.symbol("**")?;
                let name = ctx.any_ident()?;
                Ok(PatternMappingItem::Spread { stars, name })
            },
            |ctx| {
                let key = first_of!(
                    ctx,
                    "mapping key",
                    |ctx| {
                        let name = ctx.any_ident()?;
                        // Convert ident to string literal expression
                        let token = name; // Reuse the ident token as a string literal
                        Ok(Expr::Literal { token }.spanned(name.span))
                    },
                    |ctx| {
                        let token = ctx.literal()?;
                        Ok(Expr::Literal { token }.spanned(token.span))
                    },
                    |ctx| {
                        let _lparen = ctx.symbol("(")?;
                        let expr = ctx.expr()?;
                        let _rparen = ctx.symbol(")")?;
                        Ok(expr) // Return the parenthesized expression
                    }
                )?;
                let colon = ctx.symbol(":")?;
                let pattern = ctx.as_pattern()?;
                Ok(PatternMappingItem::Item {
                    key: key.boxed(),
                    colon,
                    pattern: pattern.boxed(),
                })
            },
            |ctx| {
                let name = ctx.any_ident()?;
                Ok(PatternMappingItem::Ident { name })
            }
        )
    }

    fn pattern_class_item(&mut self) -> ParseResult<PatternClassItem<STree<'src, 'tok>>> {
        first_of!(
            self,
            "pattern class item",
            |ctx| {
                let name = ctx.any_ident()?;
                let eq = ctx.symbol("=")?;
                let pattern = ctx.as_pattern()?;
                Ok(PatternClassItem::Kw {
                    name,
                    eq,
                    pattern: pattern.boxed(),
                })
            },
            |ctx| {
                let pattern = ctx.as_pattern()?;
                Ok(PatternClassItem::Item {
                    pattern: pattern.boxed(),
                })
            }
        )
    }

    // Expressions

    fn atom(&mut self) -> ParseResult<SExpr<'src, 'tok>> {
        let start = self.cursor;

        let literal_expr = |ctx: &mut Self| {
            let token = ctx.literal()?;
            Ok(Expr::Literal { token }.spanned(ctx.span_from(start)))
        };

        let ident_expr = |ctx: &mut Self| {
            let token = ctx.any_ident()?;
            Ok(Expr::Ident { token }.spanned(ctx.span_from(start)))
        };

        let placeholder = |ctx: &mut Self| {
            let token = ctx.symbol("$")?;
            Ok(Expr::Placeholder { token }.spanned(ctx.span_from(start)))
        };

        let list = |ctx: &mut Self| {
            let listing = ctx.listing("[", "]", Token::Symbol(","), |ctx| {
                let star = optional!(ctx, |ctx: &mut Self| ctx.symbol("*"))?;
                let expr = ctx.expr()?.boxed();

                if let Some(star) = star {
                    Ok(ListItem::Spread { star, expr })
                } else {
                    Ok(ListItem::Item { expr })
                }
            })?;
            Ok(Expr::List { listing }.spanned(ctx.span_from(start)))
        };

        let mapping = Self::mapping_expr;

        let fstr = Self::fstr_expr;

        let parenthesized = |ctx: &mut Self| {
            let lparen = ctx.symbol("(")?;

            let empty_tuple = |ctx: &mut Self| {
                let rparen = ctx.symbol(")")?;
                Ok(Expr::Tuple {
                    kind: TupleKind::Unit(lparen, rparen),
                }
                .spanned(ctx.span_from(start)))
            };

            let parenthesized_expr = |ctx: &mut Self| {
                let expr = ctx.open_expr()?;
                let _newl = optional!(ctx, |ctx: &mut Self| ctx.token(&Token::Eol))?;
                let rparen = ctx.symbol(")")?;
                Ok(Expr::Parenthesized {
                    lparen,
                    expr: expr.boxed(),
                    rparen,
                }
                .spanned(ctx.span_from(start)))
            };

            first_of!(
                ctx,
                "parenthesized expression",
                empty_tuple,
                parenthesized_expr
            )
        };

        let checked_expr = Self::checked_expr;

        let class_expr = Self::class_expr;

        first_of!(
            self,
            "atom",
            checked_expr,
            class_expr,
            literal_expr,
            ident_expr,
            placeholder,
            list,
            mapping,
            fstr,
            parenthesized
        )
    }

    fn checked_expr(&mut self) -> ParseResult<SExpr<'src, 'tok>> {
        let start = self.cursor;

        let try_kw = self.keyword("try")?;
        let expr = self.expr_with_prec(ExprPrec::Tryable)?;
        let except_clause = optional!(self, |ctx: &mut Self| {
            let except_kw = ctx.keyword("except")?;
            let pattern = optional!(ctx, |ctx: &mut Self| ctx.atom_pattern())?;
            Ok((except_kw, pattern))
        })?;

        let (except_kw, pattern) = if let Some((except_kw, pattern)) = except_clause {
            (Some(except_kw), pattern.map(|p| p.boxed()))
        } else {
            (None, None)
        };

        Ok(Expr::Checked {
            try_kw,
            expr: expr.boxed(),
            except_kw,
            pattern,
        }
        .spanned(self.span_from(start)))
    }

    fn class_expr(&mut self) -> ParseResult<SExpr<'src, 'tok>> {
        let start = self.cursor;

        let class_kw = self.keyword("class")?;
        let args = optional!(self, |ctx: &mut Self| {
            ctx.listing("(", ")", Token::Symbol(","), |ctx| {
                let expr = ctx.atom()?;
                Ok(CallItem::Arg { expr: expr.boxed() })
            })
        })?;

        let body = self.colon_block()?;

        Ok(Expr::Class {
            class_kw,
            args,
            body: body.boxed(),
        }
        .spanned(self.span_from(start)))
    }

    fn mapping_expr(&mut self) -> ParseResult<SExpr<'src, 'tok>> {
        let start = self.cursor;
        let listing = self.listing("{", "}", Token::Symbol(","), |ctx| {
            first_of!(
                ctx,
                "mapping item",
                |ctx| {
                    let stars = ctx.symbol("**")?;
                    let expr = ctx.expr()?;
                    Ok(MappingItem::Spread {
                        stars,
                        expr: expr.boxed(),
                    })
                },
                |ctx| {
                    let key = first_of!(
                        ctx,
                        "mapping key",
                        |ctx| {
                            let name = ctx.any_ident()?;
                            Ok(MappingKey::Ident { token: name })
                        },
                        |ctx| {
                            let token = ctx.literal()?;
                            Ok(MappingKey::Literal { token })
                        },
                        |ctx| {
                            let lparen = ctx.symbol("(")?;
                            let key = ctx.expr()?;
                            let rparen = ctx.symbol(")")?;
                            Ok(MappingKey::Expr {
                                lparen,
                                key: key.boxed(),
                                rparen,
                            })
                        }
                    )?;
                    let colon = ctx.symbol(":")?;
                    let value = ctx.expr()?;
                    Ok(MappingItem::Item {
                        key,
                        colon,
                        value: value.boxed(),
                    })
                },
                |ctx| {
                    let ident = ctx.any_ident()?;
                    Ok(MappingItem::Ident { ident })
                }
            )
        })?;
        Ok(Expr::Mapping { listing }.spanned(self.span_from(start)))
    }

    fn fstr_expr(&mut self) -> ParseResult<SExpr<'src, 'tok>> {
        let start = self.cursor;

        let begin = 'block: {
            let next = self.next_token();
            if let Some(token) = next {
                if let Token::FstrBegin(_) = token.token {
                    break 'block token;
                }
            }

            return Err(self.set_error(
                start,
                ParseErr::expected("f-string begin", start..self.cursor),
            ));
        };

        let mut parts = Vec::new();

        loop {
            let Some(expr) = optional!(self, |ctx| ctx.generic_block(false))? else {
                break;
            };

            // Optional format specification after !
            let fmt = optional!(self, |ctx: &mut Self| {
                let excl = ctx.symbol("!")?;
                let fmt_expr = ctx.expr()?.boxed();
                Ok((excl, fmt_expr))
            })?;

            let fmt_expr = FmtExpr {
                expr: expr.boxed(),
                fmt,
            };

            let cont = 'block: {
                let next = self.next_token();
                if let Some(token) = next {
                    if let Token::FstrContinue(_) = token.token {
                        break 'block token;
                    }
                }

                return Err(self.set_error(
                    start,
                    ParseErr::expected("f-string continue", start..self.cursor),
                ));
            };

            parts.push((fmt_expr, cont));
        }

        Ok(Expr::Fstr { begin, parts }.spanned(self.span_from(start)))
    }

    fn symbol_table<T: Clone>(
        &mut self,
        symbols: &[(&str, T)],
    ) -> ParseResult<(&'tok SToken<'src>, T)> {
        let start = self.cursor;

        let next: Option<&SToken<'src>> = self.next_token();

        if let Some(found) = next {
            if let Token::Symbol(sym) = found.token {
                if let Some(matched) = symbols.iter().find(|(s, _)| *s == sym) {
                    return Ok((found, matched.1.clone()));
                }
            }
        }

        Err(self.set_error(
            start,
            ParseErr::expected("specific symbol", start..self.cursor),
        ))
    }

    fn binary_op(
        &mut self,
        precedence: u8,
    ) -> ParseResult<(Option<&'tok SToken<'src>>, &'tok SToken<'src>, BinaryOp)> {
        match precedence {
            11 => {
                let op = self.symbol("|")?;
                Ok((None, op, BinaryOp::Pipe))
            }
            10 => {
                let op = self.keyword("or")?;
                Ok((None, op, BinaryOp::Or))
            }
            9 => {
                let op = self.keyword("and")?;
                Ok((None, op, BinaryOp::And))
            }
            8 => {
                let op = self.symbol("??")?;
                Ok((None, op, BinaryOp::Coalesce))
            }
            7 => {
                let res = first_of!(
                    self,
                    "binary op",
                    |ctx| {
                        let not = ctx.keyword("not")?;
                        let in_ = ctx.keyword("in")?;
                        Ok((Some(not), in_, BinaryOp::Nin))
                    },
                    |ctx| {
                        let in_ = ctx.keyword("in")?;
                        Ok((None, in_, BinaryOp::In))
                    },
                    |ctx| {
                        let (tok, kind) = ctx.symbol_table(&[
                            ("<", BinaryOp::Lt),
                            ("<=", BinaryOp::Leq),
                            (">", BinaryOp::Gt),
                            (">=", BinaryOp::Geq),
                            ("==", BinaryOp::Eq),
                            ("<>", BinaryOp::Neq),
                            ("===", BinaryOp::Is),
                            ("<=>", BinaryOp::Nis),
                        ])?;

                        Ok((None, tok, kind))
                    }
                )?;

                Ok(res)
            }
            6 => Err(()), // Bitwise or
            5 => Err(()), // Bitwise xor
            4 => Err(()), // Bitwise and
            3 => Err(()), // Bit shift
            2 => self
                .symbol_table(&[("+", BinaryOp::Add), ("-", BinaryOp::Sub)])
                .map(|(tok, kind)| (None, tok, kind)),
            1 => self
                .symbol_table(&[
                    ("*", BinaryOp::Mul),
                    ("/", BinaryOp::Div),
                    ("//", BinaryOp::FloorDiv),
                    ("%", BinaryOp::Mod),
                    ("@", BinaryOp::MatMul),
                ])
                .map(|(tok, kind)| (None, tok, kind)),
            0 => {
                let op = self.symbol("**")?;
                Ok((None, op, BinaryOp::Exp))
            }
            _ => panic!(),
        }
    }

    fn binary_expr(
        &mut self,
        prec: u8,
        next_level: &impl Parser<'src, 'tok, SExpr<'src, 'tok>>,
    ) -> ParseResult<SExpr<'src, 'tok>> {
        let start = self.cursor;
        let mut lhs = next_level(self)?;

        loop {
            let before_op = self.save();

            self.set_error(
                self.cursor,
                ParseErr::expected("binary operation", self.cursor..self.cursor + 1),
            );

            match self.binary_op(prec) {
                Ok((not_token, op, op_kind)) => {
                    let is_right_associative = op.token == Token::Symbol("**");

                    let rhs = if is_right_associative {
                        first_of!(
                            self,
                            "right-assoc",
                            |ctx| ctx.binary_expr(prec, next_level),
                            |ctx| ctx.parse(next_level)
                        )?
                    } else {
                        self.parse(next_level)?
                    };

                    lhs = Expr::Binary {
                        lhs: lhs.boxed(),
                        not: not_token,
                        op,
                        op_kind,
                        rhs: rhs.boxed(),
                    }
                    .spanned(self.span_from(start));
                }
                Err(_) => {
                    self.rewind(before_op);
                    break;
                }
            }
        }

        Ok(lhs)
    }

    fn token_to_unary_op(token: &Token) -> Option<UnaryOp> {
        match token {
            Token::Symbol("+") => Some(UnaryOp::Pos),
            Token::Symbol("-") => Some(UnaryOp::Neg),
            Token::Symbol("~") => Some(UnaryOp::Inv),
            Token::Symbol("@") => Some(UnaryOp::Bind),
            Token::Kw("not") => Some(UnaryOp::Not),
            _ => None,
        }
    }

    fn unary_expr(
        &mut self,
        postfix: &impl Parser<'src, 'tok, SExpr<'src, 'tok>>,
    ) -> ParseResult<SExpr<'src, 'tok>> {
        let start = self.cursor;

        if let Some(token) = self.peek_token() {
            if matches!(
                token.token,
                Token::Symbol("+")
                    | Token::Symbol("-")
                    | Token::Symbol("~")
                    | Token::Symbol("@")
                    | Token::Kw("not")
            ) {
                let op = self.next_token().unwrap();
                let op_kind = Self::token_to_unary_op(&op.token).unwrap();
                let expr = self.unary_expr(postfix)?;
                return Ok(Expr::Unary {
                    op,
                    op_kind,
                    expr: expr.boxed(),
                }
                .spanned(self.span_from(start)));
            }
        }

        self.parse(postfix)
    }

    fn postfix_expr(
        &mut self,
        atom: &impl Parser<'src, 'tok, SExpr<'src, 'tok>>,
    ) -> ParseResult<SExpr<'src, 'tok>> {
        let mut expr = self.parse(atom)?;

        enum Postfix<'src, 'tok> {
            Call {
                args: SListing<'src, 'tok, SCallItem<'src, 'tok>>,
            },
            MethodCall {
                dot: &'tok SToken<'src>,
                method: &'tok SToken<'src>,
                args: SListing<'src, 'tok, SCallItem<'src, 'tok>>,
            },
            Subscript {
                indices: SListing<'src, 'tok, SListItem<'src, 'tok>>,
            },
            RawAttribute {
                double_colon: &'tok SToken<'src>,
                attr: &'tok SToken<'src>,
            },
            ScopedAttribute {
                dot: &'tok SToken<'src>,
                lparen: &'tok SToken<'src>,
                rhs: SExpr<'src, 'tok>,
                rparen: &'tok SToken<'src>,
            },
            Attribute {
                dot: &'tok SToken<'src>,
                attr: &'tok SToken<'src>,
            },
            Decorator {
                ampersand: &'tok SToken<'src>,
                decorator: SExpr<'src, 'tok>,
            },
        }

        loop {
            let start = self.cursor;

            self.set_error(
                self.cursor,
                ParseErr::unexpected(self.cursor..self.cursor + 1),
            );

            let question = optional!(self, |ctx: &mut Self| ctx.symbol("?"))?;

            let call = |ctx: &mut Self| {
                let args = ctx.listing("(", ")", Token::Symbol(","), |ctx| ctx.call_item())?;
                Ok(Postfix::Call { args })
            };

            let subscript = |ctx: &mut Self| {
                let indices = ctx.listing("[", "]", Token::Symbol(","), |ctx| {
                    let star = optional!(ctx, |ctx: &mut Self| ctx.symbol("*"))?;
                    let expr = ctx.expr()?.boxed();

                    if let Some(star) = star {
                        Ok(ListItem::Spread { star, expr })
                    } else {
                        Ok(ListItem::Item { expr })
                    }
                })?;
                Ok(Postfix::Subscript { indices })
            };

            let raw_attribute = |ctx: &mut Self| {
                let double_colon = ctx.symbol("::")?;
                let attr = ctx.any_ident()?;
                Ok(Postfix::RawAttribute { double_colon, attr })
            };

            let decorator = |ctx: &mut Self| {
                let ampersand = ctx.symbol("&")?;
                let decorator = ctx.expr()?;
                Ok(Postfix::Decorator {
                    ampersand,
                    decorator,
                })
            };

            let dot_attribute = |ctx: &mut Self| {
                let dot = ctx.symbol(".")?;

                first_of!(
                    ctx,
                    "attribute",
                    |ctx| {
                        let method = ctx.any_ident()?;
                        let args =
                            ctx.listing("(", ")", Token::Symbol(","), |ctx| ctx.call_item())?;
                        Ok(Postfix::MethodCall { dot, method, args })
                    },
                    |ctx| {
                        let attr = ctx.any_ident()?;
                        Ok(Postfix::Attribute { dot, attr })
                    },
                    |ctx| {
                        let lparen = ctx.symbol("(")?;
                        let rhs = ctx.expr()?;
                        let rparen = ctx.symbol(")")?;
                        Ok(Postfix::ScopedAttribute {
                            dot,
                            lparen,
                            rhs,
                            rparen,
                        })
                    }
                )
            };

            expr = match first_of!(
                self,
                "postfix",
                call,
                subscript,
                raw_attribute,
                decorator,
                dot_attribute
            ) {
                Ok(item) => match item {
                    Postfix::Call { args } => Expr::Call {
                        expr: expr.boxed(),
                        question,
                        args,
                    }
                    .spanned(self.span_from(start)),
                    Postfix::MethodCall { dot, method, args } => Expr::MethodCall {
                        expr: expr.boxed(),
                        question,
                        dot,
                        method,
                        args,
                    }
                    .spanned(self.span_from(start)),
                    Postfix::Subscript { indices } => Expr::Subscript {
                        expr: expr.boxed(),
                        question,
                        indices,
                    }
                    .spanned(self.span_from(start)),
                    Postfix::RawAttribute { double_colon, attr } => Expr::RawAttribute {
                        expr: expr.boxed(),
                        question,
                        double_colon,
                        attr,
                    }
                    .spanned(self.span_from(start)),
                    Postfix::ScopedAttribute {
                        dot,
                        lparen,
                        rhs,
                        rparen,
                    } => Expr::ScopedAttribute {
                        expr: expr.boxed(),
                        question,
                        dot,
                        lparen,
                        rhs: rhs.boxed(),
                        rparen,
                    }
                    .spanned(self.span_from(start)),
                    Postfix::Attribute { dot, attr } => Expr::Attribute {
                        expr: expr.boxed(),
                        question,
                        dot,
                        attr,
                    }
                    .spanned(self.span_from(start)),
                    Postfix::Decorator {
                        ampersand,
                        decorator,
                    } => {
                        if question.is_some() {
                            return Err(self.set_error(
                                start,
                                ParseErr::custom(
                                    "Decorator cannot be used with ? operator",
                                    start..self.cursor,
                                ),
                            ));
                        }
                        Expr::Decorated {
                            expr: expr.boxed(),
                            ampersand,
                            decorator: decorator.boxed(),
                        }
                        .spanned(self.span_from(start))
                    }
                },
                Err(_) => {
                    break;
                }
            }
        }

        Ok(expr)
    }

    fn slice_expr(
        &mut self,
        next_level: &impl Parser<'src, 'tok, SExpr<'src, 'tok>>,
    ) -> ParseResult<SExpr<'src, 'tok>> {
        let has_start = |ctx: &mut Self| {
            let start_cursor = ctx.cursor;

            let expr = ctx.parse(next_level)?;
            let stop = optional!(ctx, |ctx: &mut Self| {
                let dots = ctx.symbol("..")?;
                let expr = optional!(ctx, next_level)?;
                Ok((dots, expr))
            })?;
            let step = optional!(ctx, |ctx: &mut Self| {
                let dots = ctx.symbol("..")?;
                let expr = optional!(ctx, next_level)?;
                Ok((dots, expr))
            })?;

            if let Some((dots, stop)) = stop {
                if let Some((step_dots, step)) = step {
                    return Ok(Expr::Slice {
                        start: Some(expr.boxed()),
                        dots,
                        stop: stop.map(|x| x.boxed()),
                        step_dots: Some(step_dots),
                        step: step.map(|x| x.boxed()),
                    }
                    .spanned(ctx.span_from(start_cursor)));
                }

                return Ok(Expr::Slice {
                    start: Some(expr.boxed()),
                    dots,
                    stop: stop.map(|x| x.boxed()),
                    step_dots: None,
                    step: None,
                }
                .spanned(ctx.span_from(start_cursor)));
            }

            return Ok(expr);
        };

        let no_start = |ctx: &mut Self| {
            let dots = ctx.symbol("..")?;
            let stop = optional!(ctx, next_level)?;
            let step = optional!(ctx, |ctx: &mut Self| {
                let dots = ctx.symbol("..")?;
                let expr = optional!(ctx, next_level)?;
                Ok((dots, expr))
            })?;

            if let Some((step_dots, step)) = step {
                Ok(Expr::Slice {
                    start: None,
                    dots,
                    stop: stop.map(|x| x.boxed()),
                    step_dots: Some(step_dots),
                    step: step.map(|x| x.boxed()),
                }
                .spanned(ctx.span_from(dots.span.start)))
            } else {
                Ok(Expr::Slice {
                    start: None,
                    dots,
                    stop: stop.map(|x| x.boxed()),
                    step_dots: None,
                    step: None,
                }
                .spanned(ctx.span_from(dots.span.start)))
            }
        };

        first_of!(self, "slice", has_start, no_start)
    }

    fn match_expr(
        &mut self,
        next_level: &impl Parser<'src, 'tok, SExpr<'src, 'tok>>,
    ) -> ParseResult<SExpr<'src, 'tok>> {
        let start = self.cursor;

        let mut expr = next_level(self)?;

        let parsed = optional!(self, |ctx: &mut Self| {
            let match_kw = ctx.ident("match")?;
            let colon = optional!(ctx, |ctx: &mut Self| ctx.symbol(":"))?;
            let indent = ctx.token(&Token::Indent)?;
            let cases = ctx.match_cases()?;
            let dedent = ctx.token(&Token::Dedent)?;
            Ok((match_kw, colon, indent, cases, dedent))
        })?;

        if let Some((match_kw, colon, indent, cases, dedent)) = parsed {
            expr = Expr::Match {
                scrutinee: expr.boxed(),
                match_kw,
                colon,
                indent,
                cases,
                dedent,
            }
            .spanned(self.span_from(start));
        }

        Ok(expr)
    }

    fn with_expr(
        &mut self,
        next_level: &impl Parser<'src, 'tok, SExpr<'src, 'tok>>,
    ) -> ParseResult<SExpr<'src, 'tok>> {
        let start = self.cursor;

        let with_variant = optional!(self, |ctx: &mut Self| {
            let with_kw = ctx.keyword("with")?;
            let pattern = ctx.open_pattern()?;
            let eq = ctx.symbol("=")?;
            let value = ctx.parse(next_level)?;
            let body = ctx.colon_block()?;

            Ok(Expr::With {
                with_kw,
                pattern: pattern.boxed(),
                eq,
                value: value.boxed(),
                body: body.boxed(),
            }
            .spanned(ctx.span_from(start)))
        })?;

        if let Some(with_expr) = with_variant {
            Ok(with_expr)
        } else {
            next_level(self)
        }
    }

    fn control_expr(
        &mut self,
        next_level: &impl Parser<'src, 'tok, SExpr<'src, 'tok>>,
    ) -> ParseResult<SExpr<'src, 'tok>> {
        let start = self.cursor;

        let await_variant = optional!(self, |ctx: &mut Self| {
            let await_kw = ctx.keyword("await")?;
            let expr = ctx.parse(next_level)?;

            Ok(Expr::Await {
                await_kw,
                expr: expr.boxed(),
            }
            .spanned(ctx.span_from(start)))
        })?;

        if let Some(await_expr) = await_variant {
            return Ok(await_expr);
        }

        let yield_variant = optional!(self, |ctx: &mut Self| {
            let yield_kw = ctx.keyword("yield")?;
            let from_kw = optional!(ctx, |ctx: &mut Self| ctx.keyword("from"))?;
            let expr = ctx.parse(next_level)?;

            if let Some(from_kw) = from_kw {
                Ok(Expr::YieldFrom {
                    yield_kw,
                    from_kw,
                    expr: expr.boxed(),
                }
                .spanned(ctx.span_from(start)))
            } else {
                Ok(Expr::Yield {
                    yield_kw,
                    expr: expr.boxed(),
                }
                .spanned(ctx.span_from(start)))
            }
        })?;

        if let Some(yield_expr) = yield_variant {
            return Ok(yield_expr);
        }

        next_level(self)
    }

    fn memo_expr(
        &mut self,
        next_level: &impl Parser<'src, 'tok, SExpr<'src, 'tok>>,
    ) -> ParseResult<SExpr<'src, 'tok>> {
        let start = self.cursor;

        let memo_variant = optional!(self, |ctx: &mut Self| {
            let async_kw = optional!(ctx, |ctx: &mut Self| ctx.keyword("async"))?;
            let memo_kw = ctx.keyword("memo")?;
            let colon = optional!(ctx, |ctx: &mut Self| ctx.symbol(":"))?;
            let expr = ctx.colon_block()?;

            Ok(Expr::Memo {
                async_kw,
                memo_kw,
                colon,
                expr: expr.boxed(),
            }
            .spanned(ctx.span_from(start)))
        })?;

        if let Some(memo_expr) = memo_variant {
            Ok(memo_expr)
        } else {
            next_level(self)
        }
    }

    fn expr_with_prec(&mut self, level: ExprPrec) -> ParseResult<SExpr<'src, 'tok>> {
        self.set_error(
            self.cursor,
            ParseErr::expected("expression", self.cursor..self.cursor + 1),
        );

        let atom = |ctx: &mut Self| ctx.atom();
        let postfix = |ctx: &mut Self| ctx.postfix_expr(&atom);
        let unary = |ctx: &mut Self| ctx.unary_expr(&postfix);
        let control = |ctx: &mut Self| ctx.control_expr(&unary);
        let binary0 = |ctx: &mut Self| ctx.binary_expr(0, &control);
        let binary1 = |ctx: &mut Self| ctx.binary_expr(1, &binary0);
        let binary2 = |ctx: &mut Self| ctx.binary_expr(2, &binary1);
        let binary3 = |ctx: &mut Self| ctx.binary_expr(3, &binary2);
        let binary4 = |ctx: &mut Self| ctx.binary_expr(4, &binary3);
        let binary5 = |ctx: &mut Self| ctx.binary_expr(5, &binary4);
        let binary6 = |ctx: &mut Self| ctx.binary_expr(6, &binary5);
        let binary7 = |ctx: &mut Self| ctx.binary_expr(7, &binary6);
        let binary8 = |ctx: &mut Self| ctx.binary_expr(8, &binary7);
        let slice = |ctx: &mut Self| ctx.slice_expr(&binary8);
        let matches = |ctx: &mut Self| ctx.matches_expr(&slice);
        let binary9 = |ctx: &mut Self| ctx.binary_expr(9, &matches);
        let binary10 = |ctx: &mut Self| ctx.binary_expr(10, &binary9);
        let memo = |ctx: &mut Self| ctx.memo_expr(&binary10);
        let with_ = |ctx: &mut Self| ctx.with_expr(&memo);
        let if_ = |ctx: &mut Self| ctx.if_expr(&with_);
        let match_ = |ctx: &mut Self| ctx.match_expr(&if_);
        let function = |ctx: &mut Self| ctx.function_expr(&match_);
        let binary11 = |ctx: &mut Self| ctx.binary_expr(11, &function);

        match level {
            ExprPrec::All => binary11(self),
            ExprPrec::CaseGuard => if_(self),
            ExprPrec::Tryable => binary7(self),
        }
    }

    fn expr(&mut self) -> ParseResult<SExpr<'src, 'tok>> {
        self.expr_with_prec(ExprPrec::All)
    }

    fn match_cases(&mut self) -> ParseResult<Vec<MatchCase<STree<'src, 'tok>>>> {
        let mut cases = Vec::new();

        loop {
            if let Some(tok) = self.peek_token() {
                if tok.token == Token::Dedent {
                    break;
                }
            }

            let pattern = self.as_pattern()?;

            let guard = optional!(self, |ctx: &mut Self| {
                let if_kw = ctx.keyword("if")?;
                let guard_expr = ctx.expr()?;
                Ok((if_kw, guard_expr))
            })?;

            let (arrow, body) = self.arrow_block()?;
            self.token(&Token::Eol)?;

            cases.push(MatchCase {
                pattern: pattern.boxed(),
                guard: guard.map(|(kw, expr)| (kw, expr.boxed())),
                arrow,
                body: body.boxed(),
            });
        }

        Ok(cases)
    }

    fn generic_block(&mut self, require_colon: bool) -> ParseResult<SExpr<'src, 'tok>> {
        let block = |ctx: &mut Self| {
            let start = ctx.cursor;

            let colon = if require_colon {
                Some(ctx.symbol(":")?)
            } else {
                None
            };

            let indent = ctx.token(&Token::Indent)?;
            let body = ctx.stmts()?;
            let dedent = ctx.token(&Token::Dedent)?;

            Ok(Expr::Block {
                kind: BlockKind::Regular {
                    colon,
                    indent,
                    body,
                    dedent,
                },
            }
            .spanned(ctx.span_from(start)))
        };

        let inline_stmt = |ctx: &mut Self| {
            let start = ctx.cursor;
            let colon = if require_colon {
                optional!(ctx, |ctx: &mut Self| ctx.symbol(":"))?
            } else {
                None
            };

            let stmt = ctx.stmt(true)?;

            Ok(Expr::Block {
                kind: BlockKind::Inline {
                    colon,
                    stmt: Box::new(stmt),
                },
            }
            .spanned(ctx.span_from(start)))
        };

        first_of!(self, "expression or block", block, inline_stmt)
    }

    fn colon_block(&mut self) -> ParseResult<SExpr<'src, 'tok>> {
        self.generic_block(true)
    }

    fn arrow_block(&mut self) -> ParseResult<(&'tok SToken<'src>, SExpr<'src, 'tok>)> {
        let arrow = self.symbol("=>")?;
        let block = self.generic_block(false)?;
        Ok((arrow, block))
    }

    fn if_expr(
        &mut self,
        next_level: &impl Parser<'src, 'tok, SExpr<'src, 'tok>>,
    ) -> ParseResult<SExpr<'src, 'tok>> {
        let start = self.cursor;

        let mut expr = next_level(self)?;

        let parsed = optional!(self, |ctx: &mut Self| {
            let then_kw = ctx.keyword("then")?;
            let then = ctx.colon_block()?.boxed();

            let else_clause = optional!(ctx, |ctx: &mut Self| {
                optional!(ctx, |ctx| ctx.token(&Token::Eol))?;
                let else_kw = ctx.keyword("else")?;
                let else_body = ctx.colon_block()?;
                Ok((else_kw, else_body.boxed()))
            })?;

            Ok((then_kw, then, else_clause))
        })?;

        if let Some((then_kw, then, else_clause)) = parsed {
            expr = Expr::If {
                cond: expr.boxed(),
                then_kw,
                then,
                else_clause,
            }
            .spanned(self.span_from(start));
        }

        Ok(expr)
    }

    fn matches_expr(
        &mut self,
        next_level: &impl Parser<'src, 'tok, SExpr<'src, 'tok>>,
    ) -> ParseResult<SExpr<'src, 'tok>> {
        let start = self.cursor;
        let mut expr = next_level(self)?;

        let parsed = optional!(self, |ctx: &mut Self| {
            let matches_kw = ctx.ident("matches")?;
            let not_kw = optional!(ctx, |ctx: &mut Self| ctx.keyword("not"))?;
            let pattern = ctx.as_pattern()?;
            Ok((matches_kw, not_kw, pattern))
        })?;

        if let Some((matches_kw, not_kw, pattern)) = parsed {
            expr = Expr::Matches {
                lhs: expr.boxed(),
                matches_kw,
                not_kw,
                pattern: pattern.boxed(),
            }
            .spanned(self.span_from(start));
        }

        Ok(expr)
    }

    fn function_expr(
        &mut self,
        next_level: &impl Parser<'src, 'tok, SExpr<'src, 'tok>>,
    ) -> ParseResult<SExpr<'src, 'tok>> {
        let start = self.cursor;

        let parenthesized_fn = |ctx: &mut Self| {
            let args = ctx.listing("(", ")", Token::Symbol(","), |ctx| {
                first_of!(
                    ctx,
                    "argument definition",
                    |ctx| {
                        let stars = ctx.symbol("**")?;
                        let name = ctx.any_ident()?;
                        Ok(ArgDefItem::KwargSpread { stars, name })
                    },
                    |ctx| {
                        let star = ctx.symbol("*")?;
                        let name = ctx.any_ident()?;
                        Ok(ArgDefItem::ArgSpread { star, name })
                    },
                    |ctx| {
                        let pattern = ctx.atom_pattern()?;
                        let default = optional!(ctx, |ctx: &mut Self| {
                            let eq = ctx.symbol("=")?;
                            let expr = ctx.expr()?;
                            Ok((eq, expr.boxed()))
                        })?;
                        Ok(ArgDefItem::Arg {
                            pattern: pattern.boxed(),
                            default,
                        })
                    }
                )
            })?;
            let arrow = ctx.symbol("=>")?;
            let body = ctx.generic_block(false)?;

            Ok(Expr::ParenthesizedFn {
                args,
                arrow,
                body: body.boxed(),
            }
            .spanned(ctx.span_from(start)))
        };

        let unary_fn = |ctx: &mut Self| {
            let pattern = ctx.atom_pattern()?;
            let arrow = ctx.symbol("=>")?;
            let body = ctx.generic_block(false)?;

            Ok(Expr::Fn {
                arg: pattern.boxed(),
                arrow,
                body: body.boxed(),
            }
            .spanned(ctx.span_from(start)))
        };

        first_of!(self, "function", parenthesized_fn, unary_fn, next_level)
    }

    fn open_expr(&mut self) -> ParseResult<SExpr<'src, 'tok>> {
        self.set_error(
            self.cursor,
            ParseErr::expected("open expression", self.cursor..self.cursor + 1),
        );

        let before_star = self.cursor;

        let star = optional!(self, |ctx: &mut Self| ctx.symbol("*"))?;
        let expr = self.expr()?;
        let comma = optional!(self, |ctx: &mut Self| ctx.symbol(","))?;

        if comma.is_none() {
            if let Some(_) = star {
                return Err(self.set_error(
                    before_star,
                    ParseErr::custom(
                        "Spread is not allowed outside of tuples",
                        before_star..before_star + 1,
                    ),
                ));
            }

            return Ok(expr);
        }

        let mut acc = vec![ListingItem {
            item: match star {
                Some(star) => ListItem::Spread {
                    star,
                    expr: expr.boxed(),
                },
                None => ListItem::Item { expr: expr.boxed() },
            },
            separator: comma,
            newline: None,
        }];

        loop {
            self.set_error(
                self.cursor,
                ParseErr::expected("expression or spread", self.cursor..self.cursor + 1),
            );

            let parsed = optional!(self, |ctx: &mut Self| {
                let star = optional!(ctx, |ctx: &mut Self| ctx.symbol("*"))?;
                let expr = ctx.expr()?.boxed();
                let comma = optional!(ctx, |ctx: &mut Self| ctx.symbol(","))?;

                Ok((star, expr, comma))
            })?;

            let Some((star, expr, comma)) = parsed else {
                break;
            };

            acc.push(ListingItem {
                item: match star {
                    Some(star) => ListItem::Spread { star, expr },
                    None => ListItem::Item { expr },
                },
                separator: comma,
                newline: None,
            });

            if comma.is_none() {
                break;
            }
        }

        Ok(Expr::Tuple {
            kind: TupleKind::Listing(acc),
        }
        .spanned(self.span_from(before_star)))
    }

    fn call_item(&mut self) -> ParseResult<SCallItem<'src, 'tok>> {
        let kwarg_spread = |ctx: &mut Self| {
            let stars = ctx.symbol("**")?;
            let expr = ctx.expr()?;
            Ok(CallItem::KwargSpread {
                stars,
                expr: expr.boxed(),
            })
        };

        let arg_spread = |ctx: &mut Self| {
            let star = ctx.symbol("*")?;
            let expr = ctx.expr()?;
            Ok(CallItem::ArgSpread {
                star,
                expr: expr.boxed(),
            })
        };

        let kwarg = |ctx: &mut Self| {
            let name = ctx.any_ident()?;
            let eq = ctx.symbol("=")?;
            let expr = ctx.expr()?;
            Ok(CallItem::Kwarg {
                name,
                eq,
                expr: expr.boxed(),
            })
        };

        let positional_arg = |ctx: &mut Self| {
            let expr = ctx.expr()?;
            Ok(CallItem::Arg { expr: expr.boxed() })
        };

        first_of!(
            self,
            "call item",
            kwarg_spread,
            arg_spread,
            kwarg,
            positional_arg
        )
    }

    // Statements

    fn import_stmt(&mut self) -> ParseResult<SStmtInner<'src, 'tok>> {
        let export = optional!(self, |ctx: &mut Self| ctx.keyword("export"))?;
        let import = self.keyword("import")?;
        let tree = self.import_tree()?;

        Ok(Stmt::Import {
            export,
            import,
            tree,
        })
    }

    fn import_tree(&mut self) -> ParseResult<ImportTree<STree<'src, 'tok>>> {
        // Parse dots (relative imports)
        let mut dots = Vec::new();
        loop {
            if let Some(dot) = optional!(self, |ctx: &mut Self| ctx.symbol("."))? {
                dots.push(dot);
            } else if let Some(double_dot) = optional!(self, |ctx: &mut Self| ctx.symbol(".."))? {
                dots.push(double_dot);
            } else {
                break;
            }
        }

        let mut trunk = Vec::new();
        loop {
            let Some((ident, dot)) = optional!(self, |ctx| {
                let ident = ctx.any_ident()?;
                let dot = ctx.symbol(".")?;
                Ok((ident, dot))
            })?
            else {
                break;
            };

            trunk.push((ident, dot));
        }

        let leaf_start = self.cursor;
        let leaf = first_of!(
            self,
            "import leaf",
            |ctx| {
                let listing = ctx.listing("(", ")", Token::Symbol(","), |ctx| ctx.import_tree())?;
                Ok(ImportLeaf::Multi(listing))
            },
            |ctx| {
                let name = ctx.any_ident()?;
                let alias = optional!(ctx, |ctx: &mut Self| {
                    let as_kw = ctx.keyword("as")?;
                    let alias_name = ctx.any_ident()?;
                    Ok((as_kw, alias_name))
                })?;
                Ok(ImportLeaf::Single { name, alias })
            },
            |ctx| {
                let star = ctx.symbol("*")?;
                Ok(ImportLeaf::Star { star })
            },
            |ctx| {
                let dot = ctx.symbol(".")?;
                let alias = optional!(ctx, |ctx: &mut Self| {
                    let as_kw = ctx.keyword("as")?;
                    let alias_name = ctx.any_ident()?;
                    Ok((as_kw, alias_name))
                })?;
                Ok(ImportLeaf::This { dot, alias })
            }
        )?;

        Ok(ImportTree {
            dots,
            trunk,
            leaf: leaf.spanned(self.span_from(leaf_start)),
        })
    }

    fn decl_modifier(&mut self) -> ParseResult<&'tok SToken<'src>> {
        first_of!(
            self,
            "declaration modifier",
            |ctx| ctx.keyword("export"),
            |ctx| ctx.keyword("global"),
            |ctx| ctx.keyword("let"),
            |ctx| ctx.keyword("const")
        )
    }

    fn decl_stmt(&mut self) -> ParseResult<SStmtInner<'src, 'tok>> {
        let modifier = self.decl_modifier()?;

        let mut names = Vec::new();
        let first_name = self.any_ident()?;
        names.push((first_name, None));

        while let Some(comma) = optional!(self, |ctx: &mut Self| ctx.symbol(","))? {
            let name = self.any_ident()?;
            if let Some(last) = names.last_mut() {
                last.1 = Some(comma);
            }
            names.push((name, None));
        }

        Ok(Stmt::Decl { modifier, names })
    }

    fn stmt(&mut self, inline: bool) -> ParseResult<SStmt<'src, 'tok>> {
        let start = self.cursor;

        let expr = if inline { Self::expr } else { Self::open_expr };

        let pattern = if inline {
            Self::atom_pattern
        } else {
            Self::open_pattern
        };

        let expr_stmt = |ctx: &mut Self| {
            Ok(Stmt::Expr {
                expr: ctx.parse(expr)?.boxed(),
            })
        };

        let pattern_debug = |ctx: &mut Self| {
            let debug = ctx.ident("debug")?;
            ctx.ident("pattern")?;
            let pat = ctx.parse(&pattern)?;
            println!("Parsed pattern: {}", pat.simple_fmt());
            Ok(Stmt::Break { break_kw: debug })
        };

        let pattern_assign = |ctx: &mut Self| {
            let modifier = optional!(ctx, Self::decl_modifier)?;
            let pat = ctx.parse(&pattern)?;
            let eq = ctx.symbol("=")?;
            let expr = ctx.parse(&expr)?;

            Ok(Stmt::PatternAssign {
                modifier,
                lhs: pat.boxed(),
                eq,
                rhs: expr.boxed(),
            })
        };

        let assign = |ctx: &mut Self| -> ParseResult<SStmtInner<'src, 'tok>> {
            let lhs = ctx.parse(&expr)?;

            let parsed = optional!(ctx, |ctx: &mut Self| {
                let aug_assign = |ctx: &mut Self| {
                    let (tok, kind) = ctx.symbol_table(&[
                        ("+=", BinaryOp::Add),
                        ("-=", BinaryOp::Sub),
                        ("*=", BinaryOp::Mul),
                        ("/=", BinaryOp::Div),
                        ("//=", BinaryOp::FloorDiv),
                        ("%=", BinaryOp::Mod),
                        ("@=", BinaryOp::MatMul),
                        ("**=", BinaryOp::Exp),
                        ("|=", BinaryOp::Pipe),
                        ("??=", BinaryOp::Coalesce),
                    ])?;
                    Ok((tok, Some(kind)))
                };

                let reg_assign = |ctx: &mut Self| {
                    let eq = ctx.symbol("=")?;
                    Ok((eq, None))
                };

                let (tok, kind) = first_of!(ctx, "assignment operator", aug_assign, reg_assign)?;
                let rhs = ctx.parse(&expr)?;

                Ok((tok, kind, rhs))
            })?;

            if let Some((tok, kind, rhs)) = parsed {
                return Ok(Stmt::Assign {
                    lhs: lhs.boxed(),
                    op: kind,
                    eq: tok,
                    rhs: rhs.boxed(),
                });
            }

            Ok(Stmt::Expr { expr: lhs.boxed() })
        };

        let while_stmt = |ctx: &mut Self| {
            let while_kw = ctx.keyword("while")?;
            let cond = ctx.expr()?;
            let body = ctx.colon_block()?;

            Ok(Stmt::While {
                while_kw,
                cond: cond.boxed(),
                body: body.boxed(),
            })
        };

        let for_stmt = |ctx: &mut Self| {
            let for_kw = ctx.keyword("for")?;
            let pattern = ctx.open_pattern()?;
            let in_kw = ctx.keyword("in")?;
            let iter = ctx.expr()?;
            let body = ctx.colon_block()?;

            Ok(Stmt::For {
                for_kw,
                pattern: pattern.boxed(),
                in_kw,
                iter: iter.boxed(),
                body: body.boxed(),
            })
        };

        let try_stmt = |ctx: &mut Self| {
            let try_kw = ctx.keyword("try")?;
            let expr = ctx.colon_block()?;

            let cases = {
                let mut acc = vec![];
                loop {
                    let Some(except) = optional!(ctx, |ctx| ctx.keyword("except"))? else {
                        break;
                    };

                    let pattern = ctx.open_pattern()?;

                    let guard = optional!(ctx, |ctx: &mut Self| {
                        let if_kw = ctx.keyword("if")?;
                        let guard_expr = ctx.expr()?;
                        Ok((if_kw, guard_expr))
                    })?;

                    let (arrow, body) = ctx.arrow_block()?;

                    acc.push(ExceptCase {
                        except,
                        pattern: pattern.boxed(),
                        guard: guard.map(|(kw, expr)| (kw, expr.boxed())),
                        arrow,
                        body: body.boxed(),
                    });
                }
                acc
            };

            let finally = optional!(ctx, |ctx: &mut Self| {
                let finally_kw = ctx.keyword("finally")?;
                let expr = ctx.colon_block()?;
                Ok((finally_kw, expr.boxed()))
            })?;

            Ok(Stmt::Try {
                try_kw,
                expr: expr.boxed(),
                cases,
                finally,
            })
        };

        let break_stmt = |ctx: &mut Self| {
            let break_kw = ctx.keyword("break")?;
            Ok(Stmt::Break { break_kw })
        };

        let continue_stmt = |ctx: &mut Self| {
            let continue_kw = ctx.keyword("continue")?;
            Ok(Stmt::Continue { continue_kw })
        };

        let return_stmt = |ctx: &mut Self| {
            let return_kw = ctx.keyword("return")?;
            let expr = ctx.expr()?;
            Ok(Stmt::Return {
                return_kw,
                expr: expr.boxed(),
            })
        };

        let raise_stmt = |ctx: &mut Self| {
            let raise_kw = ctx.keyword("raise")?;
            let expr = optional!(ctx, |ctx: &mut Self| ctx.expr())?;
            Ok(Stmt::Raise {
                raise_kw,
                expr: expr.map(|e| e.boxed()),
            })
        };

        let stmt = first_of!(
            self,
            "statement",
            pattern_debug,
            pattern_assign,
            Self::import_stmt,
            Self::decl_stmt,
            assign,
            expr_stmt,
            while_stmt,
            for_stmt,
            try_stmt,
            break_stmt,
            continue_stmt,
            return_stmt,
            raise_stmt
        )?;

        if !inline {
            let _ = self.token(&Token::Eol)?;
        }

        Ok(stmt.spanned(self.span_from(start)))
    }

    fn stmts(&mut self) -> ParseResult<Vec<Box<SStmt<'src, 'tok>>>> {
        let mut stmts = Vec::new();

        'outer: loop {
            if self.peek_token().is_none() {
                break 'outer;
            }

            if matches!(self.peek_token(), Some(tok) if tok.token == Token::Dedent) {
                break 'outer;
            }

            let before = self.save();

            if let Ok(stmt) = self.stmt(false) {
                stmts.push(Box::new(stmt));
            } else {
                let error = self.take_error().1;
                self.errors.push(error);

                // Recover
                self.rewind(before);
                let mut indent_level = 0;
                loop {
                    match self.peek_token() {
                        Some(tok) if tok.token == Token::Eol => {
                            if indent_level == 0 {
                                self.next();
                                break;
                            }
                        }
                        Some(tok) if tok.token == Token::Dedent => {
                            indent_level -= 1;
                            if indent_level < 0 {
                                break 'outer;
                            }
                        }
                        Some(tok) if tok.token == Token::Indent => {
                            indent_level += 1;
                        }
                        None => break 'outer,
                        _ => {}
                    }
                    self.next();
                }
            }
        }

        Ok(stmts)
    }

    fn program(&mut self) -> ParseResult<SExpr<'src, 'tok>> {
        let start = self.cursor;

        let body = self.stmts()?;

        if self.cursor != self.input.len() {
            return Err(self.set_error(
                self.cursor,
                ParseErr::custom(
                    "Unexpected tokens after program end",
                    self.cursor..self.input.len(),
                ),
            ));
        }

        Ok(Expr::Block {
            kind: BlockKind::Bare { body },
        }
        .spanned(self.span_from(start)))
    }
}

pub fn parse_tokens<'src: 'tok, 'tok>(
    src: &'src str,
    tokens: &'tok TokenList<'src>,
) -> (Option<SExpr<'src, 'tok>>, Vec<TriviaRich<'tok, 'src>>) {
    if tokens.0.is_empty() {
        return (
            None,
            vec![TriviaRich::custom((0..0).into(), "No tokens to parse")],
        );
    }

    let mut ctx = ParseCtx {
        src,
        input: &tokens.0,
        cursor: 0,
        errors: vec![],
        cur_error: None,
    };

    let expr = match ctx.program() {
        Ok(expr) => Some(expr),
        Err(()) => {
            let err = ctx.take_error().1;
            ctx.errors.push(err);
            None
        }
    };

    (
        expr,
        ctx.errors
            .iter()
            .map(|err| {
                TriviaRich::custom(
                    true_span(err.span.start, err.span.end, ctx.input),
                    format!("{:?}", err.message),
                )
            })
            .collect(),
    )
}
