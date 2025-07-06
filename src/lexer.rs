use chumsky::{
    input::{Cursor, InputRef, StrInput},
    prelude::*,
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

    Eol,
    Continuation,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Bool(x) => write!(f, "{x}"),
            Token::Num(n) => write!(f, "{n}"),
            Token::Str(s) => write!(f, "{s}"),
            Token::Symbol(s) => write!(f, "{s}"),
            Token::Ident(s) => write!(f, "{s}"),
            Token::Kw(s) => write!(f, "<{s}>"),
            Token::Eol => write!(f, "<eol>"),
            Token::Continuation => write!(f, "<cont>"),
        }
    }
}

#[derive(Debug)]
pub enum LexerLine<'src> {
    Line(&'src str, Span, Option<Token<'src>>),
    IndentError(&'static str, Span),
    BeginBlock(Span),
    EndBlock(Span, Token<'src>),
}

struct LineCursors<'src, 'parse> {
    line_start: Cursor<'src, 'parse, &'src str>,
    text_start: Cursor<'src, 'parse, &'src str>,
    text_end: Cursor<'src, 'parse, &'src str>,
    line_end: Cursor<'src, 'parse, &'src str>,
}

fn parse_line<'src, 'parse, 'input>(
    input: &'input mut InputRef<'src, 'parse, &'src str, extra::Err<Rich<'src, char, Span>>>,
) -> Option<LineCursors<'src, 'parse>> {
    let line_start = input.cursor();

    if input.peek().is_none() {
        return None;
    }

    while let Some(chr) = input.peek() {
        if chr != ' ' && chr != '\t' {
            break;
        }
        input.next();
    }

    let text_start = input.cursor();
    let mut text_end = text_start.clone();

    while let Some(char) = input.peek() {
        if char == '\n' {
            input.next();
            break;
        } else if char == '\r' {
            input.next();
            if input.peek() == Some('\n') {
                input.next();
            }
            break;
        } else if char == ' ' || char == '\t' {
            input.next();
        } else {
            input.next();
            text_end = input.cursor();
        }
    }

    let line_end = input.cursor();

    Some(LineCursors {
        line_start: line_start,
        text_start: text_start,
        text_end: text_end,
        line_end: line_end,
    })
}

fn semantic_whitespace<'src>()
-> impl Parser<'src, &'src str, Vec<LexerLine<'src>>, extra::Err<Rich<'src, char, Span>>> {
    custom(|input| {
        struct IndentationLevel<'src> {
            indentation: &'src str,
            start_cursor: usize,
            is_continuation: bool,
        }

        let mut lines = Vec::<LexerLine>::new();
        let mut indents = Vec::<IndentationLevel>::new();
        let mut expecting_block = true;

        while let Some(LineCursors {
            line_start,
            text_start,
            text_end,
            line_end,
        }) = parse_line(input)
        {
            let text: &str = input.slice(&text_start..&text_end);
            let indent: &str = input.slice(&line_start..&text_start);

            if text.is_empty() {
                continue;
            }

            let mut found = false;
            for (i, block) in indents.iter().enumerate() {
                if indent == block.indentation {
                    for _ in i + 1..indents.len() {
                        let popped = indents.pop();

                        if !popped.unwrap().is_continuation {
                            lines.push(LexerLine::EndBlock(
                                Span::new((), *line_start.inner()..*text_start.inner()),
                                Token::Eol,
                            ));
                        }
                    }

                    found = true;
                    break;
                }
            }

            if !found {
                if indents.is_empty() || indent.starts_with(indents.last().unwrap().indentation) {
                    if expecting_block {
                        lines.push(LexerLine::BeginBlock(Span::new(
                            (),
                            *line_start.inner()..*text_start.inner(),
                        )));
                    }

                    indents.push(IndentationLevel {
                        indentation: indent,
                        start_cursor: *line_start.inner(),
                        is_continuation: !expecting_block,
                    });
                } else {
                    lines.push(LexerLine::IndentError(
                        "unaligned indentation",
                        Span::new((), *line_start.inner()..*text_start.inner()),
                    ));

                    continue;
                }
            } else {
                if expecting_block {
                    lines.push(LexerLine::IndentError(
                        "expected indented block",
                        Span::new((), *line_start.inner()..*text_start.inner()),
                    ));
                }
            }

            if indents.last().unwrap().is_continuation {
                match lines.pop().unwrap() {
                    LexerLine::Line(a, b, _) => {
                        lines.push(LexerLine::Line(a, b, Some(Token::Continuation)));
                    }
                    LexerLine::EndBlock(a, _) => {
                        lines.push(LexerLine::EndBlock(a, Token::Continuation));
                    }
                    LexerLine::BeginBlock(_) => {
                        panic!();
                    }
                    LexerLine::IndentError(_, _) => (),
                }
            }

            expecting_block = text.ends_with(';');

            lines.push(LexerLine::Line(
                text,
                Span::new((), *text_start.inner()..*text_end.inner()),
                if expecting_block {
                    None
                } else {
                    Some(Token::Eol)
                },
            ));
        }

        while !indents.is_empty() {
            if !indents.pop().unwrap().is_continuation {
                lines.push(LexerLine::EndBlock(
                    Span::new((), *input.cursor().inner()..*input.cursor().inner()),
                    Token::Eol,
                ));
            }
        }

        Ok(lines)
    })
}

pub struct TokenList<'src>(pub Vec<Spanned<Token<'src>>>);

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

pub type LexerError<'src> = Rich<'src, char, Span>;

fn line_lexer<'src, TInput>()
-> impl Parser<'src, TInput, TokenList<'src>, extra::Err<LexerError<'src>>>
where
    TInput: StrInput<'src, Token = char, Span = SimpleSpan, Slice = &'src str>,
{
    let num = text::int(10)
        .then(just('.').then(text::digits(10)).or_not())
        .to_slice()
        .map(Token::Num);

    let str_ = just('"')
        .ignore_then(none_of('"').repeated().to_slice())
        .then_ignore(just('"'))
        .map(Token::Str);

    let symbol = choice((
        just("=="),
        just("!="),
        just("<="),
        just(">="),
        just("//"),
        just("**"),
        one_of("+-*/%|&$:;=,.()[]<>").to_slice(),
    ))
    .map(Token::Symbol);

    static KEYWORDS: &[&str] = &[
        "fn", "if", "else", "match", "import", "class", "while", "for", "break", "continue",
        "with", "yield", "global", "nonlocal", "return", "raise", "try", "except", "finally",
        "and", "or", "not",
    ];

    let ident = text::ascii::ident().map(|ident: &str| {
        if KEYWORDS.contains(&ident) {
            Token::Kw(ident)
        } else {
            Token::Ident(ident)
        }
    });

    let token = choice((num, str_, symbol, ident));

    token
        .then_ignore(text::whitespace())
        .map_with(|token, e| (token, e.span()))
        .repeated()
        .collect::<Vec<_>>()
        .map(|x: Vec<_>| TokenList(x))
        .map_err(|e: LexerError| {
            Rich::custom(
                *e.span(),
                format!(
                    "unexpected {:?}",
                    e.found().map_or("EOF".to_string(), |x| x.to_string())
                ),
            )
        })
}

pub fn tokenize<'src>(s: &'src str) -> (TokenList<'src>, Vec<LexerError<'src>>) {
    let lines = semantic_whitespace().parse(s).unwrap();

    let mut tokens = TokenList(Vec::new());
    let mut errs = Vec::new();

    for l in lines {
        match l {
            LexerLine::Line(txt, span, end_token) => {
                let (line_tokens, mut line_errs) = line_lexer()
                    .parse(txt.map_span(move |s| {
                        Span::new(s.context, span.start + s.start()..span.start + s.end())
                    }))
                    .into_output_errors();

                if let Some(mut tok) = line_tokens {
                    tokens.0.append(&mut tok.0);

                    if let Some(end_token) = end_token {
                        tokens.0.push((end_token, span));
                    }
                }

                errs.append(&mut line_errs);
            }
            LexerLine::IndentError(reason, span) => {
                errs.push(Rich::custom(span, reason));
            }
            LexerLine::BeginBlock(span) => {
                tokens.0.push((Token::Symbol("{"), span));
            }
            LexerLine::EndBlock(span, end_token) => {
                tokens.0.push((Token::Symbol("}"), span));
                tokens.0.push((end_token, span));
            }
        }
    }

    (tokens, errs)
}
