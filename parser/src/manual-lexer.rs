#![allow(dead_code)]

use chumsky::{
    input::{InputRef, StrInput},
    prelude::*,
};
use std::fmt;

pub type Span = SimpleSpan<usize, ()>;
pub type Spanned<T> = (T, Span);

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'src> {
    Ident(&'src str),
    Bool(bool),
    Num(&'src str),
    Str(&'src str),
    Kw(&'src str),
    Symbol(&'src str),

    FstringBegin(&'src str),
    FstringMiddle(&'src str),

    Eol,
    Continuation,
}

#[derive(Debug)]
pub struct TokenList<'src>(pub Vec<Spanned<Token<'src>>>);

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Bool(x) => write!(f, "<literal {x}>"),
            Token::Num(n) => write!(f, "<literal {n}>"),
            Token::Str(s) => write!(f, "<literal {s}>"),
            Token::Symbol(s) => write!(f, "{s}"),
            Token::Ident(s) => write!(f, "{s}"),
            Token::Kw(s) => write!(f, "<{s}>"),
            Token::Eol => write!(f, "<eol>"),
            Token::Continuation => write!(f, "<cont>"),
            Token::FstringBegin(s) => write!(f, "<f_begin {s}>"),
            Token::FstringMiddle(s) => write!(f, "<f_middle {s}>"),
        }
    }
}

impl<'src> std::iter::FromIterator<Spanned<Token<'src>>> for TokenList<'src> {
    fn from_iter<T: IntoIterator<Item = Spanned<Token<'src>>>>(iter: T) -> Self {
        TokenList(iter.into_iter().collect())
    }
}

impl fmt::Display for TokenList<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, (token, span)) in self.0.iter().enumerate() {
            write!(f, "{token}")?;
            if i < self.0.len() - 1 {
                write!(f, " ")?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct IndentLevel {
    pub level: usize,
    pub start_cursor: usize,
}

#[derive(Debug, Clone, Default)]
pub enum NextLineExpectation {
    #[default]
    BeginInput,
    None,
    NewBlock,
}

pub struct TokenizeState<'src> {
    pub indents: Vec<IndentLevel>,
    pub context: &'src str,
    pub next_line_expectation: NextLineExpectation,
}

pub type TOutput<'src> = TokenList<'src>;
pub type TError<'src> = Rich<'src, char, Span>;
pub type TExtra<'src> = extra::Full<TError<'src>, (), ()>;

type TResult<'src, T> = Result<T, TError<'src>>;

trait StrInputExt<'src> {
    fn try_parse<TOut>(
        &mut self,
        parse_fn: impl FnOnce(&mut Self) -> TResult<'src, TOut>,
    ) -> TResult<'src, TOut>;
    fn parse_indentation(&mut self) -> TResult<'src, Spanned<usize>>;
    fn parse_number(&mut self) -> TResult<'src, Spanned<Token<'src>>>;

    fn tokenize_input(&mut self) -> TResult<'src, TokenList<'src>>;
}

impl<'src, TInput> StrInputExt<'src> for InputRef<'src, '_, TInput, TExtra<'src>>
where
    TInput: StrInput<'src, Token = char, Span = SimpleSpan, Slice = &'src str>,
{
    fn try_parse<TOut>(
        &mut self,
        parse_fn: impl FnOnce(&mut Self) -> TResult<'src, TOut>,
    ) -> TResult<'src, TOut> {
        let start = self.save();

        match parse_fn(self) {
            Ok(result) => Ok(result),
            Err(e) => {
                self.rewind(start);
                Err(e)
            }
        }
    }

    fn parse_indentation(&mut self) -> TResult<'src, Spanned<usize>> {
        let start = self.cursor();
        let mut indent_level = 0;

        while let Some(c) = self.peek() {
            if c != ' ' && c != '\t' {
                break;
            }

            indent_level += 1;
            self.next();
        }

        Ok((indent_level, self.span_since(&start)))
    }

    fn parse_number(&mut self) -> TResult<'src, Spanned<Token<'src>>> {
        let start = self.cursor();

        let c = self.peek();

        if c.is_none_or(|c| !(c.is_ascii_digit() || c == '.')) {
            return Err(Rich::custom(self.span_since(&start), "expected a number"));
        };

        let mut after_dot = false;

        while let Some(c) = self.peek() {
            if c == '.' {
                if after_dot {
                    return Err(Rich::custom(
                        self.span_since(&start),
                        "unexpected second dot",
                    ));
                }
                after_dot = true;
            } else if !(c.is_ascii_digit() || c == '_') {
                break;
            }
            self.next();
        }

        Ok((
            Token::Num(self.slice_since(&start..)),
            self.span_since(&start),
        ))
    }

    fn tokenize_input(&mut self) -> TResult<'src, TokenList<'src>> {
        let mut tokens = vec![];

        let res = self.try_parse(StrInputExt::parse_indentation);
        println!("parse_indentation: {:?}", res);
        let res = self.try_parse(|input| input.parse_number());
        println!("parse_number: {:?}", res);

        Ok(TokenList(tokens))
    }
}

fn lexer<'src, TInput>() -> impl Parser<'src, TInput, TOutput<'src>, TExtra<'src>>
where
    TInput: StrInput<'src, Token = char, Span = SimpleSpan, Slice = &'src str>,
{
    custom(|input| input.tokenize_input())
}

pub fn tokenize<'src>(s: &'src str) -> (Option<TokenList<'src>>, Vec<TError<'src>>) {
    let output = lexer()
        .parse(s.map_span(|s| Span::new(s.context, s.start()..s.end())))
        .into_output_errors();

    output
}
