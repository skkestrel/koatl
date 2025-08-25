#![allow(dead_code)]

use std::borrow::Cow;
use std::ops::Range;

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

trait Parser<'src, 'tok, O>: Clone + Fn(&mut ParseCtx<'src, 'tok>) -> ParseResult<O> {}
impl<'src, 'tok, O, F> Parser<'src, 'tok, O> for F where
    F: Clone + Fn(&mut ParseCtx<'src, 'tok>) -> ParseResult<O>
{
}

impl<'a> ParseErr<'a> {
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

    error: Option<(usize, ParseErr<'src>)>,
}

macro_rules! first_of {
    ($ctx:expr, $name:literal, $($parser:expr),+) => {
        'first_of: {
            // Set a default error if none of the choices make progress
            $ctx.set_error($ctx.cursor, ParseErr::expected($name, $ctx.cursor..$ctx.cursor+1));

            $(
                let before = $ctx.save();
                match $ctx.parse($parser) {
                    Ok(result) => {
                        $ctx.take_error();
                        break 'first_of Ok(result);
                    }
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

    fn span(&self, start: usize, end: usize) -> Span {
        let start = self.input[start].span.start;

        let end = if end <= self.input.len() {
            self.input[end.saturating_sub(1)].span.end
        } else {
            self.src.len()
        };

        Span::from(start..end)
    }

    fn set_error(&mut self, cursor: usize, err: ParseErr<'src>) {
        if let Some((furthest_cursor, _)) = &self.error {
            if cursor >= *furthest_cursor {
                self.error = Some((cursor, err));
            }
        } else {
            self.error = Some((cursor, err));
        }
    }

    fn take_error(&mut self) -> (usize, ParseErr<'src>) {
        self.error
            .take()
            .unwrap_or((0, ParseErr::expected("unknown error", 0..0)))
    }

    fn span_from(&self, start: usize) -> Span {
        self.span(start, self.cursor)
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

        self.set_error(
            start,
            ParseErr::expected(format!("{}", token), start..self.cursor),
        );

        Err(())
    }

    fn symbol(&mut self, sym: &'static str) -> ParseResult<&'tok SToken<'src>> {
        self.token(&Token::Symbol(sym))
    }

    fn keyword(&mut self, kw: &'static str) -> ParseResult<&'tok SToken<'src>> {
        self.token(&Token::Kw(kw))
    }

    fn ident(&mut self) -> ParseResult<&'tok SToken<'src>> {
        let start = self.cursor;

        let next: Option<&SToken<'src>> = self.next_token();

        if let Some(found) = next {
            if let Token::Ident(_) = found.token {
                return Ok(found);
            }
        }

        self.set_error(start, ParseErr::expected("identifier", start..self.cursor));

        Err(())
    }

    fn literal(&mut self) -> ParseResult<&'tok SToken<'src>> {
        let start = self.cursor;

        let next: Option<&SToken<'src>> = self.next_token();

        if let Some(found) = next {
            if let Token::Num(_) | Token::Str(_) | Token::Bool(_) | Token::None = found.token {
                return Ok(found);
            }
        }

        self.set_error(start, ParseErr::expected("literal", start..self.cursor));

        Err(())
    }

    fn listing<O: std::fmt::Debug>(
        &mut self,
        begin: &'static str,
        end: &'static str,
        optional_separator: Token<'src>,
        item_parser: impl Parser<'src, 'tok, O>,
    ) -> ParseResult<SListing<'src, 'tok, O>> {
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
                    ctx.rewind(before_item);
                    break;
                }
            }

            let dedent = ctx.token(&Token::Dedent)?;
            let newline = optional!(ctx, |ctx: &mut Self| ctx.token(&Token::Eol))?;

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

    // Expressions

    fn atom(&mut self) -> ParseResult<SExpr<'src, 'tok>> {
        let start = self.cursor;

        let literal_expr = |ctx: &mut Self| {
            let token = ctx.literal()?;
            Ok(Expr::Literal { token }.spanned(ctx.span_from(start)))
        };

        let ident_expr = |ctx: &mut Self| {
            let token = ctx.ident()?;
            Ok(Expr::Ident { token }.spanned(ctx.span_from(start)))
        };

        let placeholder = |ctx: &mut Self| {
            let dollar = ctx.symbol("$")?;
            Ok(Expr::Placeholder { dollar }.spanned(ctx.span_from(start)))
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

        let parenthesized = |ctx: &mut Self| {
            let lparen = ctx.symbol("(")?;

            // Check for empty tuple
            if ctx.symbol(")").is_ok() {
                let rparen = &ctx.input[ctx.cursor - 1];
                let listing = Listing {
                    begin: lparen,
                    indent: None,
                    items: vec![],
                    dedent: None,
                    newline: None,
                    end: rparen,
                };
                return Ok(Expr::Tuple { listing }.spanned(ctx.span_from(start)));
            }

            let expr = ctx.expr()?;
            let rparen = ctx.symbol(")")?;
            Ok(Expr::Parenthesized {
                lparen,
                expr: expr.boxed(),
                rparen,
            }
            .spanned(ctx.span_from(start)))
        };

        first_of!(
            self,
            "atom",
            literal_expr,
            ident_expr,
            placeholder,
            list,
            parenthesized
        )
    }

    fn binary_expr(&mut self, min_prec: u8) -> ParseResult<SExpr<'src, 'tok>> {
        let mut lhs = self.unary_expr()?;

        while let Some(op_token) = self.peek_token() {
            let (prec, right_assoc) = self.binary_op_precedence(&op_token.token);
            if prec < min_prec {
                break;
            }

            let op = self.next_token().unwrap();
            let next_min_prec = if right_assoc { prec } else { prec + 1 };
            let rhs = self.binary_expr(next_min_prec)?;

            let span = lhs.span.start..rhs.span.end;
            lhs = Expr::Binary {
                lhs: lhs.boxed(),
                op,
                rhs: rhs.boxed(),
            }
            .spanned(Span::from(span));
        }

        Ok(lhs)
    }

    fn unary_expr(&mut self) -> ParseResult<SExpr<'src, 'tok>> {
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
                let expr = self.unary_expr()?;
                return Ok(Expr::Unary {
                    op,
                    expr: expr.boxed(),
                }
                .spanned(self.span_from(start)));
            }
        }

        self.postfix_expr()
    }

    fn postfix_expr(&mut self) -> ParseResult<SExpr<'src, 'tok>> {
        let mut expr = self.atom()?;

        enum Postfix<'src, 'tok> {
            Call {
                args: SListing<'src, 'tok, SCallItem<'src, 'tok>>,
            },
        }

        loop {
            let start = self.cursor;

            let question = optional!(self, |ctx: &mut Self| ctx.symbol("?"))?;

            let call = |ctx: &mut Self| {
                let args = ctx.listing("[", "]", Token::Symbol(","), |ctx| ctx.call_item())?;

                Ok(Postfix::Call { args })
            };

            expr = match first_of!(self, "postfix", call) {
                Ok(item) => match item {
                    Postfix::Call { args } => Expr::Call {
                        expr: expr.boxed(),
                        question,
                        args,
                    }
                    .spanned(self.span_from(start)),
                },
                Err(_) => {
                    break;
                }
            }
        }

        Ok(expr)
    }

    fn expr(&mut self) -> ParseResult<SExpr<'src, 'tok>> {
        self.atom()
    }

    fn call_item(&mut self) -> ParseResult<SCallItem<'src, 'tok>> {
        let start = self.cursor;

        // Try **expr (kwarg spread)
        if self.symbol("**").is_ok() {
            let stars = &self.input[start];
            let expr = self.expr()?;
            return Ok(CallItem::KwargSpread {
                stars,
                expr: expr.boxed(),
            });
        }

        // Try *expr (arg spread)
        if self.symbol("*").is_ok() {
            let star = &self.input[start];
            let expr = self.expr()?;
            return Ok(CallItem::ArgSpread {
                star,
                expr: expr.boxed(),
            });
        }

        // Try name=expr (keyword argument)
        let checkpoint = self.cursor;
        if let Ok(name) = self.ident() {
            if self.symbol("=").is_ok() {
                let eq = &self.input[self.cursor - 1];
                let expr = self.expr()?;
                return Ok(CallItem::Kwarg {
                    name,
                    eq,
                    expr: expr.boxed(),
                });
            }
        }
        self.rewind(checkpoint);

        // Default: expr (positional argument)
        let expr = self.expr()?;
        Ok(CallItem::Arg { expr: expr.boxed() })
    }

    fn binary_op_precedence(&self, token: &Token<'src>) -> (u8, bool) {
        match token {
            Token::Symbol("**") => (8, true), // right associative
            Token::Symbol("*") | Token::Symbol("/") | Token::Symbol("//") | Token::Symbol("%") => {
                (7, false)
            }
            Token::Symbol("+") | Token::Symbol("-") => (6, false),
            Token::Symbol("<")
            | Token::Symbol("<=")
            | Token::Symbol(">")
            | Token::Symbol(">=")
            | Token::Symbol("==")
            | Token::Symbol("!=")
            | Token::Symbol("===")
            | Token::Symbol("!==") => (5, false),
            Token::Kw("and") => (4, false),
            Token::Kw("or") => (3, false),
            Token::Symbol("|") => (2, false),
            _ => (0, false),
        }
    }

    fn stmt(&mut self) -> ParseResult<SStmt<'src, 'tok>> {
        let start = self.cursor;

        let expr = self.expr()?;
        let newl = self.token(&Token::Eol)?;

        Ok(Stmt::Expr { expr: expr.boxed() }.spanned(self.span_from(start)))
    }

    fn block(&mut self) -> ParseResult<SExpr<'src, 'tok>> {
        let indent = self.token(&Token::Indent)?;

        let mut stmts = Vec::new();

        loop {
            let before = self.save();
            if let Ok(stmt) = self.stmt() {
                stmts.push(Box::new(stmt));
            } else {
                self.rewind(before);
                break;
            }
        }

        let dedent = self.token(&Token::Dedent)?;

        Ok(SExpr {
            value: Expr::Block {
                starter: None,
                indent,
                stmts,
                dedent,
            },
            span: Span::from(indent.span.start..dedent.span.end),
        })
    }

    fn program(&mut self) -> ParseResult<SExpr<'src, 'tok>> {
        Ok(self.block()?)
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
        error: None,
    };

    match ctx.program() {
        Ok(expr) => (Some(expr), vec![]),
        Err(()) => {
            let err = ctx.take_error().1;

            println!("Parse error: {:#?}", err);
            println!("Tokens: {}", tokens);

            (
                None,
                vec![TriviaRich::custom(
                    ctx.span(err.span.start, err.span.end),
                    format!("{:?}", err.message),
                )],
            )
        }
    }
}
