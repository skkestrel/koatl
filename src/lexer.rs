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

    BeginBlock,
    EndBlock,
    Eol,
    Separator,
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
            Token::BeginBlock => write!(f, "{{"),
            Token::EndBlock => write!(f, "}}"),
            Token::Eol => write!(f, "<EOL>"),
            Token::Separator => write!(f, "<SEP>"),
        }
    }
}

#[derive(Debug)]
pub enum LexerStmt<'src> {
    Stmt(&'src str, Span),
    IndentError(&'static str, Span),
    BeginBlock(Span),
    EndBlock(Span),
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
-> impl Parser<'src, &'src str, Vec<LexerStmt<'src>>, extra::Err<Rich<'src, char, Span>>> {
    custom(|input| {
        let mut lines = Vec::<LexerStmt>::new();
        let mut block_indents = Vec::<&str>::new();

        let mut stmt_start = None;
        let mut stmt_end = None;

        let mut prev_line_start_index = None;

        let mut continuation_indent: Option<&str> = None;

        let mut expecting_new_block = true;

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

            if expecting_new_block {
                expecting_new_block = false;

                if let Some(prev_indent) = block_indents.last() {
                    if !indent.starts_with(prev_indent) || indent.len() <= prev_indent.len() {
                        lines.push(LexerStmt::IndentError(
                            "expected indented block",
                            Span::new((), prev_line_start_index.unwrap()..*line_end.inner()),
                        ));
                        continue;
                    }
                }

                lines.push(LexerStmt::BeginBlock(Span::new(
                    (),
                    *line_start.inner()..*text_start.inner(),
                )));

                block_indents.push(indent);
                stmt_start = Some(text_start);
                stmt_end = Some(text_end);
                continuation_indent = None;
            } else {
                while indent.len() < block_indents.last().unwrap().len() {
                    lines.push(LexerStmt::EndBlock(Span::new(
                        (),
                        *line_start.inner()..*line_start.inner(),
                    )));
                    block_indents.pop();

                    if block_indents.is_empty() {
                        lines.push(LexerStmt::IndentError(
                            "unexpected dedent",
                            Span::new((), prev_line_start_index.unwrap()..*line_end.inner()),
                        ));

                        return Ok(lines);
                    }
                }

                if !indent.starts_with(block_indents.last().unwrap()) {
                    lines.push(LexerStmt::IndentError(
                        "unaligned indentation",
                        Span::new((), prev_line_start_index.unwrap()..*line_end.inner()),
                    ));

                    continue;
                }

                if indent == *block_indents.last().unwrap() {
                    let stmt_start_ref = stmt_start.as_ref().unwrap();
                    let stmt_end_ref = stmt_end.as_ref().unwrap();
                    let full_txt = input.slice(stmt_start_ref..stmt_end_ref);

                    lines.push(LexerStmt::Stmt(
                        &full_txt,
                        Span::new((), *stmt_start_ref.inner()..*stmt_end_ref.inner()),
                    ));

                    stmt_start = Some(text_start);
                    stmt_end = Some(text_end);
                    continuation_indent = None;
                } else if let Some(continuation) = continuation_indent {
                    if indent != continuation {
                        lines.push(LexerStmt::IndentError(
                            "continuation line must match previous indent",
                            Span::new((), prev_line_start_index.unwrap()..*line_end.inner()),
                        ));
                        continue;
                    }

                    stmt_end = Some(text_end);
                } else {
                    continuation_indent = Some(indent);
                    stmt_end = Some(text_end);
                }
            }

            if text.ends_with(':') {
                let stmt_start_ref = stmt_start.as_ref().unwrap();
                let stmt_end_ref = stmt_end.as_ref().unwrap();
                let full_txt = input.slice(stmt_start_ref..stmt_end_ref);

                lines.push(LexerStmt::Stmt(
                    &full_txt[..full_txt.len() - 1],
                    Span::new((), *stmt_start_ref.inner()..*stmt_end_ref.inner() - 1),
                ));

                expecting_new_block = true;
            }

            prev_line_start_index = Some(*line_start.inner());
        }

        if let Some(start) = stmt_start {
            if let Some(end) = stmt_end {
                lines.push(LexerStmt::Stmt(
                    input.slice(&start..&end),
                    Span::new((), *start.inner()..*end.inner()),
                ));
            }
        }

        while !block_indents.is_empty() {
            lines.push(LexerStmt::EndBlock(Span::new(
                (),
                *input.cursor().inner()..*input.cursor().inner(),
            )));
            block_indents.pop();
        }

        Ok(lines)
    })
}

pub type LexerOutput<'src> = Vec<Spanned<Token<'src>>>;
pub type LexerError<'src> = Rich<'src, char, Span>;

fn line_lexer<'src, TInput>()
-> impl Parser<'src, TInput, LexerOutput<'src>, extra::Err<LexerError<'src>>>
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
        just("<"),
        just(">"),
        just("<="),
        just(">="),
        //
        just("//"),
        just("**"),
        just("+"),
        just("-"),
        just("*"),
        just("/"),
        just("%"),
        //
        just(".."),
        just("$"),
        just(":"),
        just("="),
        just(","),
        just("."),
        just("("),
        just(")"),
        just("["),
        just("]"),
    ))
    .to_slice()
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

    let token = choice((
        num,
        str_,
        symbol,
        ident,
        text::newline().to(Token::Separator),
    ));

    token
        .map_err(|e: LexerError| {
            Rich::custom(
                *e.span(),
                format!(
                    "unexpected {:?}",
                    e.found().map_or("EOF".to_string(), |x| x.to_string())
                ),
            )
        })
        .map_with(|token, e| (token, e.span()))
        .then_ignore(text::inline_whitespace())
        .repeated()
        .collect()
}

pub fn tokenize<'src>(s: &'src str) -> (Vec<Spanned<Token<'src>>>, Vec<Rich<'src, char, Span>>) {
    let lines = semantic_whitespace().parse(s).unwrap();

    let mut tokens = Vec::new();
    let mut errs = Vec::new();

    for l in lines {
        match l {
            LexerStmt::Stmt(txt, span) => {
                let (line_tokens, mut line_errs) = line_lexer()
                    .parse(txt.map_span(move |s| {
                        Span::new(s.context, span.start + s.start()..span.start + s.end())
                    }))
                    .into_output_errors();

                if let Some(mut tok) = line_tokens {
                    tokens.append(&mut tok);
                }

                errs.append(&mut line_errs);
            }
            LexerStmt::IndentError(reason, span) => {
                errs.push(Rich::custom(span, reason));
            }
            LexerStmt::BeginBlock(span) => {
                tokens.push((Token::BeginBlock, span));
            }
            LexerStmt::EndBlock(span) => {
                tokens.push((Token::EndBlock, span));
            }
        }
    }

    (tokens, errs)
}
