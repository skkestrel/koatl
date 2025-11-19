#![allow(dead_code)]

use std::borrow::Cow;

use crate::cst::*;
use crate::lexer::*;

const START_BLOCK: Token = Token::Symbol(":");

#[derive(Debug, Clone)]
enum ErrMsg<'a> {
    Unexpected,
    UnmatchedDelimiter,
    Trailing,
    Expected(Cow<'a, str>),
    Custom(Cow<'a, str>),
}

#[derive(Debug, Clone)]
struct ParseErr<'a> {
    index: usize,
    message: ErrMsg<'a>,
}

trait Parser<'src, 'tok, O>: Fn(&mut ParseCtx<'src, 'tok>) -> ParseResult<O> {}
impl<'src, 'tok, O, F> Parser<'src, 'tok, O> for F where
    F: Clone + Fn(&mut ParseCtx<'src, 'tok>) -> ParseResult<O>
{
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
            $ctx.set_error($ctx.cursor, ErrMsg::Expected($name.into()));

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
    Checkable,
    Inline,
}

pub fn true_span(start: usize, end: usize, input: &[SToken]) -> Span {
    let true_start = if start < input.len() {
        input[start].span.start
    } else {
        input.last().map_or(0, |t| t.span.end)
    };

    let true_end = if end == start {
        true_start
    } else if end <= input.len() {
        input[end.saturating_sub(1)].span.end
    } else {
        input.last().map_or(0, |t| t.span.end)
    };

    Span::new(true_start..true_end)
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

    fn set_error(&mut self, cursor: usize, err: ErrMsg<'src>) {
        // println!("set_error {} {:?}", cursor, err);

        if let Some((furthest_cursor, _)) = &self.cur_error {
            if cursor > *furthest_cursor {
                self.cur_error = Some((
                    cursor,
                    ParseErr {
                        message: err,
                        index: cursor,
                    },
                ));
            }
        } else {
            self.cur_error = Some((
                cursor,
                ParseErr {
                    message: err,
                    index: cursor,
                },
            ));
        }
    }

    fn take_error(&mut self) -> (usize, ParseErr<'src>) {
        self.cur_error.take().unwrap_or((
            0,
            ParseErr {
                message: ErrMsg::Custom("unknown error".into()),
                index: 0,
            },
        ))
    }

    fn span_from(&self, start: usize) -> Span {
        let span = true_span(start, self.cursor, self.input);
        if span.start > span.end {
            panic!("Invalid span")
        }
        span
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

        Err(self.set_error(start, ErrMsg::Expected(format!("{}", token).into())))
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

        Err(self.set_error(start, ErrMsg::Expected("identifier".into())))
    }

    fn literal(&mut self) -> ParseResult<&'tok SToken<'src>> {
        let start = self.cursor;

        let next: Option<&SToken<'src>> = self.next_token();

        if let Some(found) = next {
            if let Token::Int(_)
            | Token::IntBin(_)
            | Token::IntHex(_)
            | Token::IntOct(_)
            | Token::Float(_)
            | Token::Str(..)
            | Token::Bool(_)
            | Token::None = found.token
            {
                return Ok(found);
            }
        }

        Err(self.set_error(start, ErrMsg::Expected("literal".into())))
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

            let newline = optional!(ctx, |ctx: &mut Self| ctx.token(&Token::Eol))?;
            let end = ctx.symbol(end)?;

            Ok(Listing::Inline {
                begin,
                items: acc,
                newline,
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

        self.set_error(self.cursor, ErrMsg::Unexpected);

        loop {
            let parsed = optional!(self, |ctx| {
                let colon_or_dot = first_of!(ctx, ":: or .", |ctx| ctx.symbol("."), |ctx| ctx
                    .symbol("::"))?;
                let attr = ctx.any_ident()?;
                Ok((colon_or_dot, attr))
            })?;

            let Some((double_colon, attr)) = parsed else {
                break;
            };

            expr = Expr::RawAttribute {
                expr: expr.boxed(),
                question: None,
                double_colon,
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
            self.set_error(self.cursor, ErrMsg::Unexpected);

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
        self.set_error(self.cursor, ErrMsg::Expected("open pattern".into()));

        let before_star = self.cursor;

        let item = self.pattern_sequence_item()?;

        self.set_error(self.cursor, ErrMsg::Unexpected);

        let comma = optional!(self, |ctx: &mut Self| ctx.symbol(","))?;

        if comma.is_none() {
            match item {
                PatternSequenceItem::Item { pattern } => return Ok(*pattern),
                PatternSequenceItem::Spread { .. } => {
                    return Err(self.set_error(
                        before_star,
                        ErrMsg::Custom("Spread is not allowed outside of tuples".into()),
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
            self.set_error(self.cursor, ErrMsg::Expected("pattern".into()));

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

        Ok(Pattern::TupleSequence {
            kind: PatternTupleSequenceKind::Listing(acc),
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
        let expr = self.qualified_ident()?.boxed();
        Ok(Pattern::Value { dot, expr }.spanned(self.span_from(start)))
    }

    fn parenthesized_pattern(&mut self) -> ParseResult<SPattern<'src, 'tok>> {
        let start = self.cursor;
        let lparen = self.symbol("(")?;

        let empty_unit = |ctx: &mut Self| {
            let rparen = ctx.symbol(")")?;
            Ok(Pattern::TupleSequence {
                kind: PatternTupleSequenceKind::Unit(lparen, rparen),
            })
        };

        let parenthesized_pattern = |ctx: &mut Self| {
            let pattern = ctx.open_pattern()?.boxed();
            let rparen = ctx.symbol(")")?;
            Ok(Pattern::Parenthesized {
                lparen,
                pattern,
                rparen,
            })
        };

        Ok(first_of!(
            self,
            "parenthesized pattern",
            empty_unit,
            parenthesized_pattern
        )?
        .spanned(self.span_from(start)))
    }

    fn sequence_pattern(&mut self) -> ParseResult<SPattern<'src, 'tok>> {
        let start = self.cursor;
        let listing = self.listing("[", "]", Token::Symbol(","), |ctx| {
            ctx.pattern_sequence_item()
        })?;
        Ok(Pattern::Sequence { listing }.spanned(self.span_from(start)))
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
            "mapping item",
            |ctx| {
                let stars = ctx.symbol("**")?;
                let name = ctx.any_ident()?;
                Ok(PatternMappingItem::Spread { stars, name })
            },
            |ctx| {
                let start = ctx.cursor;
                let key = first_of!(
                    ctx,
                    "mapping key",
                    |ctx| {
                        let token = ctx.any_ident()?;
                        Ok(PatternMappingKey::Ident { token })
                    },
                    |ctx| {
                        let token = ctx.literal()?;
                        Ok(PatternMappingKey::Literal { token })
                    },
                    |ctx| {
                        let _ = ctx.fstr()?;

                        Err(ctx.set_error(
                            ctx.cursor,
                            ErrMsg::Custom(
                                "f-strings are not allowed as mapping-pattern keys".into(),
                            ),
                        ))
                    },
                    |ctx| {
                        let lparen = ctx.symbol("(")?;
                        let rparen = ctx.symbol(")")?;
                        Ok(PatternMappingKey::Unit { lparen, rparen })
                    },
                    |ctx| {
                        let lparen = ctx.symbol("(")?;
                        let key = ctx.expr()?.boxed();
                        let rparen = ctx.symbol(")")?;
                        Ok(PatternMappingKey::Expr {
                            lparen,
                            key,
                            rparen,
                        })
                    }
                )?;
                let colon = ctx.symbol(":")?;
                let pattern = ctx.as_pattern()?;
                Ok(PatternMappingItem::Item {
                    key: key.spanned(ctx.span_from(start)),
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
                })
            };

            let block = |ctx: &mut Self| {
                let indent = ctx.token(&Token::Indent)?;
                let body = ctx.stmts()?;
                let dedent = ctx.token(&Token::Dedent)?;
                optional!(ctx, |ctx| ctx.token(&Token::Eol))?;
                let rparen = ctx.symbol(")")?;

                Ok(Expr::ParenthesizedBlock {
                    lparen,
                    indent,
                    body,
                    dedent,
                    rparen,
                })
            };

            let parenthesized_expr = |ctx: &mut Self| {
                let expr = ctx.open_expr()?;
                let _newl = optional!(ctx, |ctx: &mut Self| ctx.token(&Token::Eol))?;
                let rparen = ctx.symbol(")")?;
                Ok(Expr::Parenthesized {
                    lparen,
                    expr: expr.boxed(),
                    rparen,
                })
            };

            Ok(first_of!(
                ctx,
                "parenthesized expression",
                empty_tuple,
                block,
                parenthesized_expr
            )?
            .spanned(ctx.span_from(start)))
        };

        let try_expr = Self::try_expr;
        let checked_expr = Self::checked_expr;
        let class_expr = Self::class_expr;
        let classic_if = Self::classic_if;
        let classic_match = Self::classic_match;

        first_of!(
            self,
            "atom",
            checked_expr,
            class_expr,
            try_expr,
            classic_if,
            classic_match,
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

        let check_kw = self.ident("check")?;
        let expr = self.expr_with_prec(ExprPrec::Checkable)?;
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
            check_kw,
            expr: expr.boxed(),
            except_kw,
            pattern,
        }
        .spanned(self.span_from(start)))
    }

    fn classic_if(&mut self) -> ParseResult<SExpr<'src, 'tok>> {
        let start = self.cursor;

        let if_kw = self.keyword("if")?;
        let cond = self.expr()?.boxed();

        let body = self.colon_block()?;

        let else_clause = optional!(self, |ctx: &mut Self| {
            optional!(ctx, |ctx| ctx.token(&Token::Eol))?;
            let else_kw = ctx.keyword("else")?;
            let else_body = ctx.colon_block()?;
            Ok((else_kw, else_body))
        })?;

        Ok(Expr::ClassicIf {
            if_kw,
            cond,
            body,
            else_clause,
        }
        .spanned(self.span_from(start)))
    }

    fn classic_match(&mut self) -> ParseResult<SExpr<'src, 'tok>> {
        let start = self.cursor;

        let match_kw = self.ident("match")?;
        let scrutinee = self.expr()?.boxed();
        let colon = self.symbol(":")?;
        let indent = self.token(&Token::Indent)?;
        let cases = self.match_cases()?;
        let dedent = self.token(&Token::Dedent)?;

        Ok(Expr::Match {
            scrutinee,
            match_kw,
            colon,
            indent,
            cases,
            dedent,
        }
        .spanned(self.span_from(start)))
    }

    fn class_expr(&mut self) -> ParseResult<SExpr<'src, 'tok>> {
        let start = self.cursor;

        let class_kw = self.keyword("class")?;

        let args = optional!(self, |ctx: &mut Self| {
            ctx.listing("(", ")", Token::Symbol(","), |ctx| Ok(ctx.call_item()?))
        })?;

        let body = self.colon_block()?;

        Ok(Expr::Class {
            class_kw,
            args,
            body,
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
                    let start = ctx.cursor;
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
                            let (begin, head, parts, end) = ctx.fstr()?;
                            Ok(MappingKey::Fstr {
                                begin,
                                head,
                                parts,
                                end,
                            })
                        },
                        |ctx| {
                            let lparen = ctx.symbol("(")?;
                            let rparen = ctx.symbol(")")?;
                            Ok(MappingKey::Unit { lparen, rparen })
                        },
                        |ctx| {
                            let lparen = ctx.symbol("(")?;
                            let indent = ctx.token(&Token::Indent)?;
                            let body = ctx.stmts()?;
                            let dedent = ctx.token(&Token::Dedent)?;
                            optional!(ctx, |ctx| ctx.token(&Token::Eol))?;
                            let rparen = ctx.symbol(")")?;

                            Ok(MappingKey::ParenthesizedBlock {
                                lparen,
                                indent,
                                body,
                                dedent,
                                rparen,
                            })
                        },
                        |ctx| {
                            let lparen = ctx.symbol("(")?;
                            let key = ctx.open_expr()?;
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
                        key: key.spanned(ctx.span_from(start)),
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

    fn fstr(
        &mut self,
    ) -> ParseResult<(
        &'tok SToken<'src>,
        &'tok SToken<'src>,
        Vec<(SFmtExpr<'src, 'tok>, &'tok SToken<'src>)>,
        &'tok SToken<'src>,
    )> {
        let start = self.cursor;

        let begin = 'block: {
            let next = self.next_token();
            if let Some(token) = next {
                if let Token::FstrBegin(_) | Token::VerbatimFstrBegin(_) = token.token {
                    break 'block token;
                }
            }

            return Err(self.set_error(start, ErrMsg::Expected("f-string begin".into())));
        };

        let (head, parts) = self.fstr_inner()?;

        let end = 'block: {
            let next = self.next_token();
            if let Some(token) = next {
                if let Token::FstrEnd(_) | Token::VerbatimFstrEnd(_) = token.token {
                    break 'block token;
                }
            }

            return Err(self.set_error(start, ErrMsg::Expected("f-string end".into())));
        };

        Ok((begin, head, parts, end))
    }

    fn fstr_inner(
        &mut self,
    ) -> ParseResult<(
        &'tok SToken<'src>,
        Vec<(SFmtExpr<'src, 'tok>, &'tok SToken<'src>)>,
    )> {
        // First, get the head (initial FstrInner content)
        let head = 'block: {
            let next = self.next_token();
            if let Some(token) = next {
                if let Token::FstrInner(..) = token.token {
                    break 'block token;
                }
            }
            return Err(self.set_error(
                self.cursor,
                ErrMsg::Expected("f-string head content".into()),
            ));
        };

        let mut parts = Vec::new();

        loop {
            let Some(expr) = optional!(self, |ctx| {
                let expr = ctx.open_expr()?.boxed();
                Ok(expr)
            })?
            else {
                break;
            };

            let fmt = optional!(self, |ctx: &mut Self| {
                let sep = ctx.symbol("%")?;
                let (head, parts) = ctx.fstr_inner()?;
                Ok(FmtSpec { sep, head, parts })
            })?;

            let fmt_expr = FmtExpr { expr, fmt };

            let inner_content = 'block: {
                let next = self.next_token();
                if let Some(token) = next {
                    if let Token::FstrInner(..) = token.token {
                        break 'block token;
                    }
                }
                return Err(self.set_error(
                    self.cursor,
                    ErrMsg::Expected("f-string content after expression".into()),
                ));
            };

            parts.push((fmt_expr, inner_content));
        }

        Ok((head, parts))
    }

    fn fstr_expr(&mut self) -> ParseResult<SExpr<'src, 'tok>> {
        let start = self.cursor;

        let (begin, head, parts, end) = self.fstr()?;

        Ok(Expr::Fstr {
            begin,
            head,
            parts,
            end,
        }
        .spanned(self.span_from(start)))
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

        Err(self.set_error(start, ErrMsg::Expected("specific symbol".into())))
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
                let op = self.symbol("??")?;
                Ok((None, op, BinaryOp::Coalesce))
            }
            9 => {
                let op = self.keyword("or")?;
                Ok((None, op, BinaryOp::Or))
            }
            8 => {
                let op = self.keyword("and")?;
                Ok((None, op, BinaryOp::And))
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
            6 => self
                .symbol_table(&[("||", BinaryOp::BitOr)])
                .map(|(tok, kind)| (None, tok, kind)),
            5 => self
                .symbol_table(&[("^^", BinaryOp::BitXor)])
                .map(|(tok, kind)| (None, tok, kind)),
            4 => self
                .symbol_table(&[("&&", BinaryOp::BitAnd)])
                .map(|(tok, kind)| (None, tok, kind)),
            3 => self
                .symbol_table(&[("<<", BinaryOp::LShift), (">>", BinaryOp::RShift)])
                .map(|(tok, kind)| (None, tok, kind)),
            2 => self
                .symbol_table(&[("+", BinaryOp::Add), ("-", BinaryOp::Sub)])
                .map(|(tok, kind)| (None, tok, kind)),
            1 => self
                .symbol_table(&[
                    ("*", BinaryOp::Mul),
                    ("/", BinaryOp::Div),
                    ("//", BinaryOp::FloorDiv),
                    ("%%", BinaryOp::Mod),
                    ("@@", BinaryOp::MatMul),
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

            self.set_error(self.cursor, ErrMsg::Expected("binary operation".into()));

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
                Token::Symbol("+") | Token::Symbol("-") | Token::Symbol("~") | Token::Symbol("@")
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
            MaybeAttribute {
                question: &'tok SToken<'src>,
                dot: &'tok SToken<'src>,
                attr: &'tok SToken<'src>,
            },
            Attribute {
                dot: &'tok SToken<'src>,
                attr: &'tok SToken<'src>,
            },
            Decorator {
                op: &'tok SToken<'src>,
                decorator: SExpr<'src, 'tok>,
            },
        }

        loop {
            let start = self.cursor;

            self.set_error(self.cursor, ErrMsg::Unexpected);

            let question = optional!(self, |ctx: &mut Self| ctx.symbol("?"))?;

            self.set_error(
                self.cursor,
                ErrMsg::Expected("attribute, call, or index".into()),
            );

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
                let op = ctx.symbol("!")?;
                let decorator = ctx.expr()?;
                Ok(Postfix::Decorator { op, decorator })
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
                        let question = ctx.symbol("?")?;
                        let attr = ctx.any_ident()?;
                        Ok(Postfix::MaybeAttribute {
                            question,
                            dot,
                            attr,
                        })
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
                    },
                    Postfix::MethodCall { dot, method, args } => Expr::MethodCall {
                        expr: expr.boxed(),
                        question,
                        dot,
                        method,
                        args,
                    },
                    Postfix::Subscript { indices } => Expr::Subscript {
                        expr: expr.boxed(),
                        question,
                        indices,
                    },
                    Postfix::RawAttribute { double_colon, attr } => Expr::RawAttribute {
                        expr: expr.boxed(),
                        question,
                        double_colon,
                        attr,
                    },
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
                    },
                    Postfix::Attribute { dot, attr } => Expr::Attribute {
                        expr: expr.boxed(),
                        question,
                        dot,
                        attr,
                    },
                    Postfix::MaybeAttribute {
                        question: question2,
                        dot,
                        attr,
                    } => Expr::MaybeAttribute {
                        expr: expr.boxed(),
                        question,
                        dot,
                        question2,
                        attr,
                    },
                    Postfix::Decorator { op, decorator } => {
                        if question.is_some() {
                            return Err(self.set_error(
                                start,
                                ErrMsg::Custom("Decorator cannot be used with ? operator".into()),
                            ));
                        }
                        Expr::Decorated {
                            expr: expr.boxed(),
                            op,
                            decorator: decorator.boxed(),
                        }
                    }
                }
                .spanned(self.span_from(start)),
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
        let start = self.cursor;

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
                .spanned(ctx.span_from(start)))
            } else {
                Ok(Expr::Slice {
                    start: None,
                    dots,
                    stop: stop.map(|x| x.boxed()),
                    step_dots: None,
                    step: None,
                }
                .spanned(ctx.span_from(start)))
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
            let colon = ctx.symbol(":")?;
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
                body,
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
            let from_kw = optional!(ctx, |ctx: &mut Self| ctx.ident("from"))?;
            let expr = ctx.parse(next_level)?;

            Ok(Expr::Yield {
                yield_kw,
                from_kw,
                expr: expr.boxed(),
            }
            .spanned(ctx.span_from(start)))
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
            let async_kw = optional!(ctx, |ctx: &mut Self| ctx.ident("async"))?;
            let memo_kw = ctx.ident("memo")?;
            let body = ctx.colon_block()?;

            Ok(Expr::Memo {
                async_kw,
                memo_kw,
                body,
            }
            .spanned(ctx.span_from(start)))
        })?;

        if let Some(memo_expr) = memo_variant {
            Ok(memo_expr)
        } else {
            next_level(self)
        }
    }

    fn not_expr(
        &mut self,
        next_level: &impl Parser<'src, 'tok, SExpr<'src, 'tok>>,
    ) -> ParseResult<SExpr<'src, 'tok>> {
        let start = self.cursor;

        let not_variant = optional!(self, |ctx: &mut Self| {
            let not_kw = ctx.keyword("not")?;
            let expr = ctx.not_expr(next_level)?;

            Ok(Expr::Unary {
                op: not_kw,
                op_kind: UnaryOp::Not,
                expr: expr.boxed(),
            }
            .spanned(ctx.span_from(start)))
        })?;

        if let Some(not_expr) = not_variant {
            Ok(not_expr)
        } else {
            next_level(self)
        }
    }

    fn expr_with_prec(&mut self, level: ExprPrec) -> ParseResult<SExpr<'src, 'tok>> {
        self.set_error(self.cursor, ErrMsg::Expected("expression".into()));

        let atom = |ctx: &mut Self| ctx.atom();
        let postfix = |ctx: &mut Self| ctx.postfix_expr(&atom);
        let unary = |ctx: &mut Self| ctx.unary_expr(&postfix);
        let control = |ctx: &mut Self| {
            ctx.set_error(ctx.cursor, ErrMsg::Expected("expression".into()));
            ctx.control_expr(&unary)
        };
        let binary0 = |ctx: &mut Self| ctx.binary_expr(0, &control);
        let binary1 = |ctx: &mut Self| ctx.binary_expr(1, &binary0);
        let binary2 = |ctx: &mut Self| ctx.binary_expr(2, &binary1);
        let binary3 = |ctx: &mut Self| ctx.binary_expr(3, &binary2);
        let binary4 = |ctx: &mut Self| ctx.binary_expr(4, &binary3);
        let binary5 = |ctx: &mut Self| ctx.binary_expr(5, &binary4);
        let binary6 = |ctx: &mut Self| ctx.binary_expr(6, &binary5);
        let binary7 = |ctx: &mut Self| ctx.binary_expr(7, &binary6);
        let not_expr = |ctx: &mut Self| ctx.not_expr(&binary7);

        // Matches needs to be above "and" and "or" so we can write "a matches 1 and b matches 2 ..."
        let matches = |ctx: &mut Self| ctx.matches_expr(&not_expr);

        let binary8 = |ctx: &mut Self| ctx.binary_expr(8, &matches);
        let binary9 = |ctx: &mut Self| ctx.binary_expr(9, &binary8);
        let slice = |ctx: &mut Self| ctx.slice_expr(&binary9);
        let binary10 = |ctx: &mut Self| ctx.binary_expr(10, &slice);
        let memo = |ctx: &mut Self| ctx.memo_expr(&binary10);
        let with_ = |ctx: &mut Self| ctx.with_expr(&memo);
        let if_ = |ctx: &mut Self| ctx.if_expr(&with_);

        let match_ = |ctx: &mut Self| ctx.match_expr(&if_);
        let function = |ctx: &mut Self| ctx.function_expr();
        let function_or_match =
            |ctx: &mut Self| first_of!(ctx, "function or match", &function, &match_);

        let binary11 = |ctx: &mut Self| ctx.binary_expr(11, &function_or_match);

        match level {
            ExprPrec::All => binary11(self),
            ExprPrec::CaseGuard => binary10(self),
            // Checkable needs to be above matches so we can write "check a matches ..."
            ExprPrec::Checkable => not_expr(self),
            ExprPrec::Inline => function_or_match(self),
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

            let pattern = self.open_pattern()?;

            let guard = optional!(self, |ctx: &mut Self| {
                let if_kw = ctx.keyword("if")?;
                let guard_expr = ctx.expr_with_prec(ExprPrec::CaseGuard)?;
                Ok((if_kw, guard_expr))
            })?;

            let body = self.arrow_block(false)?;
            self.token(&Token::Eol)?;

            cases.push(MatchCase {
                pattern: pattern.boxed(),
                guard: guard.map(|(kw, expr)| (kw, expr.boxed())),
                body,
            });
        }

        Ok(cases)
    }

    fn colon_block(&mut self) -> ParseResult<InducedBlock<STree<'src, 'tok>>> {
        let block = |ctx: &mut Self| {
            let colon = ctx.symbol(":")?;

            let indent = ctx.token(&Token::Indent)?;
            let body = ctx.stmts()?;
            let dedent = ctx.token(&Token::Dedent)?;

            Ok(InducedBlock::Block {
                inducer: colon,
                indent,
                body,
                dedent,
            })
        };

        let inline_stmt = |ctx: &mut Self| {
            let colon = optional!(ctx, |ctx: &mut Self| ctx.symbol(":"))?;

            let stmt = ctx.stmt(true)?;

            Ok(InducedBlock::Inline {
                inducer: colon,
                stmt: Box::new(stmt),
            })
        };

        first_of!(self, "expression or block", block, inline_stmt)
    }

    fn arrow_block(
        &mut self,
        parse_inline_stmt_on_inline_block: bool,
    ) -> ParseResult<InducedBlock<STree<'src, 'tok>>> {
        let arrow = self.symbol("=>")?;

        let block = |ctx: &mut Self| {
            let indent = ctx.token(&Token::Indent)?;
            let body = ctx.stmts()?;
            let dedent = ctx.token(&Token::Dedent)?;

            Ok(InducedBlock::Block {
                inducer: arrow,
                indent,
                body,
                dedent,
            })
        };

        let inline_stmt = |ctx: &mut Self| {
            let stmt = ctx.stmt(parse_inline_stmt_on_inline_block)?;

            Ok(InducedBlock::Inline {
                inducer: Some(arrow),
                stmt: Box::new(stmt),
            })
        };

        first_of!(self, "expression or block", block, inline_stmt)
    }

    fn if_expr(
        &mut self,
        next_level: &impl Parser<'src, 'tok, SExpr<'src, 'tok>>,
    ) -> ParseResult<SExpr<'src, 'tok>> {
        let start = self.cursor;

        let mut expr = next_level(self)?;

        let parsed = optional!(self, |ctx: &mut Self| {
            let then_kw = ctx.keyword("then")?;
            let then = ctx.colon_block()?;

            let else_clause = optional!(ctx, |ctx: &mut Self| {
                optional!(ctx, |ctx| ctx.token(&Token::Eol))?;
                let else_kw = ctx.keyword("else")?;
                let else_body = ctx.colon_block()?;
                Ok((else_kw, else_body))
            })?;

            Ok((then_kw, then, else_clause))
        })?;

        if let Some((then_kw, body, else_clause)) = parsed {
            expr = Expr::If {
                cond: expr.boxed(),
                then_kw,
                body,
                else_clause,
            }
            .spanned(self.span_from(start));
        }

        Ok(expr)
    }

    fn try_expr(&mut self) -> ParseResult<SExpr<'src, 'tok>> {
        let start = self.cursor;
        let try_kw = self.keyword("try")?;
        let body = self.colon_block()?;

        let cases = {
            let mut acc = vec![];
            loop {
                let Some(except) = optional!(self, |ctx| {
                    ctx.token(&Token::Eol)?;
                    ctx.keyword("except")
                })?
                else {
                    break;
                };

                let pattern = self.open_pattern()?;

                self.set_error(self.cursor, ErrMsg::Expected("guard or =>".into()));

                let guard = optional!(self, |ctx: &mut Self| {
                    let if_kw = ctx.keyword("if")?;
                    let guard_expr = ctx.expr_with_prec(ExprPrec::CaseGuard)?;
                    Ok((if_kw, guard_expr))
                })?;

                let body = self.arrow_block(false)?;

                acc.push(ExceptCase {
                    except,
                    pattern: pattern.boxed(),
                    guard: guard.map(|(kw, expr)| (kw, expr.boxed())),
                    body,
                });
            }
            acc
        };

        let finally = optional!(self, |ctx: &mut Self| {
            optional!(ctx, |ctx| ctx.token(&Token::Eol))?;
            let finally_kw = ctx.keyword("finally")?;
            let body = ctx.colon_block()?;
            Ok((finally_kw, body))
        })?;

        Ok(Expr::Try {
            try_kw,
            body,
            cases,
            finally,
        }
        .spanned(self.span_from(start)))
    }

    fn matches_expr(
        &mut self,
        next_level: &impl Parser<'src, 'tok, SExpr<'src, 'tok>>,
    ) -> ParseResult<SExpr<'src, 'tok>> {
        let start = self.cursor;
        let mut expr = next_level(self)?;

        let parsed = optional!(self, |ctx: &mut Self| {
            let not_kw = optional!(ctx, |ctx: &mut Self| ctx.keyword("not"))?;
            let matches_kw = ctx.ident("matches")?;
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

    fn function_expr(&mut self) -> ParseResult<SExpr<'src, 'tok>> {
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
                    },
                    |ctx| {
                        let star = ctx.symbol("*")?;
                        Ok(ArgDefItem::KwOnlyMarker { star })
                    },
                    |ctx| {
                        let slash = ctx.symbol("/")?;
                        Ok(ArgDefItem::PosOnlyMarker { slash })
                    }
                )
            })?;
            let body = ctx.arrow_block(true)?;

            Ok(Expr::ParenthesizedFn { args, body }.spanned(ctx.span_from(start)))
        };

        let unary_fn = |ctx: &mut Self| {
            let pattern = ctx.atom_pattern()?;
            let body = ctx.arrow_block(true)?;

            Ok(Expr::Fn {
                arg: pattern.boxed(),
                body,
            }
            .spanned(ctx.span_from(start)))
        };

        first_of!(self, "function", parenthesized_fn, unary_fn)
    }

    fn open_expr(&mut self) -> ParseResult<SExpr<'src, 'tok>> {
        self.set_error(self.cursor, ErrMsg::Expected("open expression".into()));

        let before_star = self.cursor;

        let star = optional!(self, |ctx: &mut Self| ctx.symbol("*"))?;
        let expr = self.expr()?;
        let comma = optional!(self, |ctx: &mut Self| ctx.symbol(","))?;

        if comma.is_none() {
            if let Some(_) = star {
                return Err(self.set_error(
                    before_star,
                    ErrMsg::Custom("Spread is not allowed outside of tuples".into()),
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
            self.set_error(self.cursor, ErrMsg::Expected("expression or spread".into()));

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
        let start = self.cursor;
        let mut dots = Vec::new();

        self.set_error(self.cursor, ErrMsg::Expected("import path".into()));

        loop {
            let Some(tok) = self.peek_token() else {
                break;
            };

            match tok.token {
                Token::Symbol(s) if s == "." => dots.push((tok, 1)),
                Token::Symbol(s) if s == ".." => dots.push((tok, 2)),
                _ => break,
            }
            self.next_token();
        }
        let dots_span = self.span_from(start);

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
                let listing = ctx.listing("(", ")", Token::Symbol(","), |ctx| {
                    first_of!(
                        ctx,
                        "import path",
                        Self::import_tree,
                        Self::import_this_tree
                    )
                })?;
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
            }
        )?;

        Ok(ImportTree {
            dots: dots.spanned(dots_span),
            trunk,
            leaf: leaf.spanned(self.span_from(leaf_start)),
        })
    }

    fn import_this_tree(&mut self) -> ParseResult<ImportTree<STree<'src, 'tok>>> {
        let dots_span = self.span_from(self.cursor);

        let start = self.cursor;

        let dot = self.symbol(".")?;
        let alias = optional!(self, |ctx: &mut Self| {
            let as_kw = ctx.keyword("as")?;
            let alias_name = ctx.any_ident()?;
            Ok((as_kw, alias_name))
        })?;

        Ok(ImportTree {
            dots: vec![].spanned(dots_span),
            trunk: vec![],
            leaf: ImportLeaf::This { dot, alias }.spanned(self.span_from(start)),
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

        let expr = if inline {
            |ctx: &mut Self| ctx.expr_with_prec(ExprPrec::Inline)
        } else {
            Self::open_expr
        };

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
                body,
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
                body,
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
            let expr = ctx.parse(expr)?;
            Ok(Stmt::Return {
                return_kw,
                expr: expr.boxed(),
            })
        };

        let raise_stmt = |ctx: &mut Self| {
            let raise_kw = ctx.keyword("raise")?;
            let expr = optional!(ctx, expr)?;
            Ok(Stmt::Raise {
                raise_kw,
                expr: expr.map(|e| e.boxed()),
            })
        };

        let stmt = first_of!(
            self,
            "statement",
            pattern_assign,
            Self::import_stmt,
            Self::decl_stmt,
            assign,
            expr_stmt,
            while_stmt,
            for_stmt,
            break_stmt,
            continue_stmt,
            return_stmt,
            raise_stmt
        )?;

        Ok(stmt.spanned(self.span_from(start)))
    }

    fn stmts(&mut self) -> ParseResult<SStmts<'src, 'tok>> {
        let start = self.cursor;
        let mut stmts = Vec::new();

        'outer: loop {
            if self.peek_token().is_none() {
                break 'outer;
            }

            if matches!(self.peek_token(), Some(tok) if tok.token == Token::Dedent) {
                break 'outer;
            }

            let before = self.save();

            if let Ok(stmt) = (|ctx: &mut Self| -> ParseResult<SStmt> {
                let stmt = ctx.stmt(false)?;
                ctx.token(&Token::Eol)?;
                Ok(stmt)
            })(self)
            {
                stmts.push(Box::new(stmt));
            } else {
                // Recover
                self.rewind(before);

                let mut delim_stack = vec![];

                loop {
                    let span = self.span_from(before);

                    let err_stmt = Box::new(
                        Stmt::Error {
                            raw: &self.src[span.start..span.end],
                        }
                        .spanned(span),
                    );

                    match self.peek_token() {
                        Some(tok) if tok.token == Token::Eol => {
                            if delim_stack.is_empty() {
                                // Successfully recovered
                                let error = self.take_error().1;
                                self.errors.push(error);

                                stmts.push(err_stmt);
                                self.next();

                                break;
                            }
                        }
                        Some(tok)
                            if tok.token == Token::Indent
                                || tok.token == Token::Symbol("(")
                                || tok.token == Token::Symbol("[")
                                || tok.token == Token::Symbol("{") =>
                        {
                            delim_stack.push(tok.token.clone());
                        }
                        Some(tok) => {
                            if let Some(expected) = match tok.token {
                                Token::Dedent => Some(Token::Indent),
                                Token::Symbol(")") => Some(Token::Symbol("(".into())),
                                Token::Symbol("]") => Some(Token::Symbol("[".into())),
                                Token::Symbol("}") => Some(Token::Symbol("{".into())),
                                _ => None,
                            } {
                                let Some(delim) = delim_stack.pop() else {
                                    stmts.push(err_stmt);
                                    break 'outer;
                                };

                                if delim != expected {
                                    return Err(());
                                }
                            }
                        }
                        None => {
                            stmts.push(err_stmt);
                            return Err(());
                        }
                    }
                    self.next();
                }
            }
        }

        Ok(stmts.spanned(self.span_from(start)))
    }

    fn program(&mut self) -> ParseResult<SStmts<'src, 'tok>> {
        let body = self.stmts()?;

        if self.cursor != self.input.len() {
            return Err(self.set_error(self.cursor, ErrMsg::Trailing));
        }

        Ok(body)
    }
}

pub fn parse_tokens<'src: 'tok, 'tok>(
    src: &'src str,
    tokens: &'tok TokenList<'src>,
) -> (Option<SStmts<'src, 'tok>>, Vec<(Span, String)>) {
    if tokens.0.is_empty() {
        return (Some(vec![].spanned(Span::new(0..0))), vec![]);
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
                (
                    true_span(err.index, err.index + 1, ctx.input),
                    format!(
                        "{:?}. Found: {}",
                        err.message,
                        tokens
                            .0
                            .get(err.index)
                            .map(|x| format!("{:?}", x.token))
                            .unwrap_or("Eof".into())
                    ),
                )
            })
            .collect(),
    )
}
