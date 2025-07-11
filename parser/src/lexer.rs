#![allow(dead_code, unused_imports)]

use chumsky::{
    input::{Cursor, InputRef, StrInput},
    prelude::*,
    recursive::Indirect,
};
use std::fmt;

pub type Span = SimpleSpan<usize, ()>;
pub type Spanned<T> = (T, Span);

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'src> {
    Ident(&'src str),
    Bool(bool),
    Num(&'src str),
    Str(&'src str),
    Kw(&'src str),
    Symbol(&'src str),

    FstrBegin(&'src str),
    FstrContinue(&'src str),

    Eol,
    Continuation,
}

#[derive(Debug)]
pub struct TokenList<'src>(pub Vec<Spanned<Token<'src>>>);

impl<'src> TokenList<'src> {
    pub fn concat(a: TokenList<'src>, b: TokenList<'src>) -> Self {
        let mut result = a;
        result.0.extend(b.0);
        result
    }
}

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
            Token::FstrBegin(s) => write!(f, "<f_begin {s}>"),
            Token::FstrContinue(s) => write!(f, "<f_continue {s}>"),
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

pub type TError<'src> = Rich<'src, char, Span>;

#[derive(Debug, Copy, Clone, PartialEq, Default)]
enum NextLineExpectation {
    #[default]
    BeginInput,
    None,
    NewBlock,
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum IndentAction {
    Unknown,
    BeginInput,
    None,
    BeginBlock,
    Continuation,
}

#[derive(Debug, Clone)]
struct Context {
    current_indent: usize,
    next_line_expectation: NextLineExpectation,
    indent_action: IndentAction,
}

type TResult<'src, T> = Result<T, TError<'src>>;
type TExtra<'src> = extra::Full<TError<'src>, (), Context>;
type TOutput<'src> = TokenList<'src>;

fn lexer<'src, TInput>() -> impl Parser<'src, TInput, TokenList<'src>, extra::Err<TError<'src>>>
where
    TInput: StrInput<'src, Token = char, Span = SimpleSpan, Slice = &'src str>,
{
    let mut block = chumsky::recursive::Recursive::<Indirect<TInput, TOutput, TExtra>>::declare();

    let indentation = custom(|input: &mut InputRef<TInput, TExtra>| {
        let start = input.cursor();
        let mut indent_level = 0;

        while let Some(c) = input.peek() {
            if c != ' ' && c != '\t' {
                break;
            }

            indent_level += 1;
            input.next();
        }

        println!("indentation: {indent_level} {:?}", input.ctx());

        let action = match input.ctx().next_line_expectation {
            NextLineExpectation::BeginInput => IndentAction::BeginInput,
            NextLineExpectation::NewBlock => {
                if indent_level < input.ctx().current_indent {
                    return Err(Rich::custom(
                        input.span_since(&start),
                        "expected an indented block",
                    ));
                }

                IndentAction::BeginBlock
            }
            NextLineExpectation::None => {
                if indent_level < input.ctx().current_indent {
                    return Err(Rich::custom(
                        input.span_since(&start),
                        "unexpected dedentation",
                    ));
                } else if indent_level > input.ctx().current_indent {
                    indent_level = input.ctx().current_indent;
                    IndentAction::Continuation
                } else {
                    IndentAction::None
                }
            }
        };

        Ok(Context {
            current_indent: indent_level,
            next_line_expectation: NextLineExpectation::None,
            indent_action: action,
        })
    });

    let num = text::int::<TInput, TExtra>(10)
        .then(just('.').then(text::digits(10)).or_not())
        .to_slice()
        .map(Token::Num)
        .boxed();

    let symbol = choice((
        just("=>"),
        just(".."),
        just("=="),
        just("!="),
        just("<="),
        just(">="),
        just("//"),
        just("**"),
        one_of("+-*/%|&$:=,.()[]<>").to_slice(),
    ))
    .map(Token::Symbol);

    static KEYWORDS: &[&str] = &[
        "if", "then", "else", "match", "import", "as", "class", "while", "for", "in", "break",
        "continue", "with", "yield", "global", "nonlocal", "return", "raise", "try", "except",
        "finally", "and", "or", "not",
    ];

    let ident = text::ascii::ident::<TInput, TExtra>()
        .and_is(just("f\"").not())
        .map(|ident: &str| {
            if KEYWORDS.contains(&ident) {
                Token::Kw(ident)
            } else {
                Token::Ident(ident)
            }
        });

    let non_semantic = group((
        text::inline_whitespace(),
        just('#').then(any().and_is(text::newline().not())).or_not(),
    ))
    .ignored();

    let expect_new_block = just("=>")
        .or(just(":"))
        .then_ignore(non_semantic.clone())
        .then(text::newline())
        .ignored();

    let line_end_handler = choice((
        expect_new_block.ignore_then(block.clone()),
        text::newline().map(|_| TokenList(vec![])),
    ))
    .boxed();

    let line_semantic = choice((num, symbol, ident))
        .and_is(expect_new_block.not())
        .then_ignore(non_semantic.clone())
        .map_with(|x, e| (x, e.span()))
        .repeated()
        .collect::<Vec<_>>()
        .map(|x| TokenList(x))
        .then(line_end_handler.clone())
        .map(|(a, b)| TokenList::concat(a, b))
        .boxed();

    // tokens.define(
    //     unit.then_ignore(text::inline_whitespace())
    //         .map_with(|token, e| (token, e.span()))
    //         .repeated()
    //         .collect::<Vec<_>>()
    //         .map(|x: Vec<_>| TokenList(x))
    //         .map_err(|e: Error| {
    //             Rich::custom(
    //                 *e.span(),
    //                 format!(
    //                     "unexpected {:?}",
    //                     e.found().map_or("EOF".to_string(), |x| x.to_string())
    //                 ),
    //             )
    //         }),
    // );

    let line = indentation.then_with_ctx(line_semantic).map(|(ctx, b)| {
        println!("then_with_ctx: {ctx:?}, {b:?}");
        b
    });

    block.define(line.repeated().collect::<Vec<_>>().map(|x| {
        let mut l = TokenList(Vec::new());
        for tokens in x {
            l.0.extend(tokens.0);
        }
        l
    }));

    block.with_ctx(Context {
        current_indent: 0,
        next_line_expectation: NextLineExpectation::BeginInput,
        indent_action: IndentAction::Unknown,
    })
}

pub fn tokenize<'src>(s: &'src str) -> (Option<TokenList<'src>>, Vec<TError<'src>>) {
    lexer().parse(s).into_output_errors()
}
