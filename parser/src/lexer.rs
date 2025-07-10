#![allow(dead_code)]

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

    FstringBegin(&'src str),
    FstringMiddle(&'src str),

    Eol,
    Continuation,
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
            Token::FstringBegin(s) => write!(f, "<f_begin {s}>"),
            Token::FstringMiddle(s) => write!(f, "<f_middle {s}>"),
        }
    }
}

#[derive(Debug)]
struct TokenizeLineInfo<'src> {
    text: &'src str,
    span: Span,
    end_token: Option<Token<'src>>,
    starts_as_fstring: bool,
    starts_as_verbatim_fstring: bool,
}

#[derive(Debug)]
enum LexerLine<'src> {
    Line(TokenizeLineInfo<'src>),
    IndentError(&'static str, Span),
    BeginBlock(Span),
    EndBlock(Span, Token<'src>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NextLineExpectation {
    None,
    ExpectNewBlock,
    ExpectFstringBlock,
    ExpectVerbatimFstringBlock,
}

struct LineInfo<'src, 'parse> {
    line_start: Cursor<'src, 'parse, &'src str>,
    text_start: Cursor<'src, 'parse, &'src str>,
    text_end: Cursor<'src, 'parse, &'src str>,
    line_end: Cursor<'src, 'parse, &'src str>,

    next_line: NextLineExpectation,
}

enum LineParseErr {
    UnterminatedString,
    UnterminatedVerbatimString,
    UnterminatedFstringExpr,
    UnmatchedFstringBrace,
    NoNestedStringsInFstring,
    Eof,
}

fn parse_line<'src, 'parse, 'input>(
    input: &'input mut InputRef<'src, 'parse, &'src str, extra::Err<Rich<'src, char, Span>>>,
    initial_in_fstring: bool,
    initial_in_verbatim_fstring: bool,
) -> Result<LineInfo<'src, 'parse>, LineParseErr> {
    let line_start = input.cursor();

    if input.peek().is_none() {
        return Err(LineParseErr::Eof);
    }

    while let Some(chr) = input.peek() {
        if chr != ' ' && chr != '\t' {
            break;
        }
        input.next();
    }

    let text_start = input.cursor();
    let mut text_end = text_start.clone();
    let mut quotes_in_a_row = 0;

    let mut in_regular_string = initial_in_fstring;
    let mut in_verbatim_string = initial_in_verbatim_fstring;

    let mut is_fstring = initial_in_fstring || initial_in_verbatim_fstring;
    let mut in_fstring_expr = false;

    // when resuming parsing an fstring, make sure that it begins with a }
    if is_fstring {
        if let Some(chr) = input.peek() {
            if chr != '}' {
                return Err(LineParseErr::UnmatchedFstringBrace);
            } else {
                input.next();

                if let Some(chr) = input.peek() {
                    if chr == '}' {
                        return Err(LineParseErr::UnmatchedFstringBrace);
                    }
                }
            }
        } else {
            return Err(LineParseErr::UnmatchedFstringBrace);
        }
    }

    let mut in_comment = false;
    let mut prev_char = '\0';
    let mut prev_prev_char = '\0';
    let mut next_line_expectation = NextLineExpectation::None;

    while let Some(char) = input.peek() {
        if is_fstring {
            if char == '{' && prev_char != '{' {
                if in_fstring_expr {
                    return Err(LineParseErr::UnmatchedFstringBrace);
                }

                in_fstring_expr = true;
            }
            if char == '}' && prev_char != '}' {
                if !in_fstring_expr {
                    return Err(LineParseErr::UnmatchedFstringBrace);
                }

                in_fstring_expr = false;
            }
        }

        if !in_verbatim_string || in_fstring_expr {
            let mut handle_endl = || {
                if in_regular_string && !in_fstring_expr {
                    return Err(LineParseErr::UnterminatedString);
                }

                if in_fstring_expr {
                    if !(prev_char == '{' && prev_prev_char != '{') {
                        return Err(LineParseErr::UnterminatedFstringExpr);
                    }

                    if in_verbatim_string {
                        next_line_expectation = NextLineExpectation::ExpectVerbatimFstringBlock;
                    } else {
                        next_line_expectation = NextLineExpectation::ExpectFstringBlock;
                    }
                }

                Ok(())
            };

            if char == '\n' {
                input.next();
                handle_endl()?;
                break;
            }

            if char == '\r' {
                input.next();
                if input.peek() == Some('\n') {
                    input.next();
                    handle_endl()?;
                }
                break;
            }
        }

        if char == ' ' || char == '\t' {
            input.next();
        } else {
            if !in_comment {
                if char == ':' || (char == '=' && prev_char == '>') {
                    next_line_expectation = NextLineExpectation::ExpectNewBlock;
                }

                if prev_char != '"' {
                    quotes_in_a_row = 0;
                }

                if char == '"' {
                    if quotes_in_a_row == 0 {
                        is_fstring = prev_char == 'f' || prev_char == 't';
                    }

                    quotes_in_a_row += 1;

                    if quotes_in_a_row == 3 {
                        in_verbatim_string = !in_verbatim_string;
                        in_regular_string = false;
                        quotes_in_a_row = 0;
                    } else {
                        in_regular_string = !in_regular_string;
                    }
                } else {
                    quotes_in_a_row = 0;
                }
            }

            if char == '#' && !in_verbatim_string && !in_regular_string {
                in_comment = true;
            }

            input.next();

            if !in_comment {
                text_end = input.cursor();
            }
        }

        prev_prev_char = prev_char;
        prev_char = char;
    }

    let line_end = input.cursor();

    Ok(LineInfo {
        line_start: line_start,
        text_start: text_start,
        text_end: text_end,
        line_end: line_end,
        next_line: next_line_expectation,
    })
}

fn semantic_whitespace<'src>()
-> impl Parser<'src, &'src str, Vec<LexerLine<'src>>, extra::Err<Rich<'src, char, Span>>> {
    custom(|input| {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        enum IndentationLevelType {
            Block,
            Continuation,
            FstringExpr,
            VerbatimFstringExpr,
        }

        struct IndentationLevel<'src> {
            indentation: &'src str,
            start_cursor: usize,
            typ: IndentationLevelType,
        }

        let mut lines = Vec::<LexerLine>::new();
        let mut indents = Vec::<IndentationLevel>::new();
        let mut next_line_expectation = NextLineExpectation::ExpectNewBlock;

        let mut next_parse_line_in_fstring = false;
        let mut next_parse_line_in_verbatim_fstring = false;

        loop {
            let parse_line_in_fstring = next_parse_line_in_fstring;
            let parse_line_in_verbatim_fstring = next_parse_line_in_verbatim_fstring;

            let parsed = parse_line(input, parse_line_in_fstring, parse_line_in_verbatim_fstring);

            println!("{parse_line_in_fstring:?}, {parse_line_in_verbatim_fstring:?}");

            next_parse_line_in_fstring = false;
            next_parse_line_in_verbatim_fstring = false;

            match parsed {
                Ok(LineInfo {
                    line_start,
                    text_start,
                    text_end,
                    line_end,
                    next_line: next_next_line,
                }) => {
                    let text: &str = input.slice(&text_start..&text_end);
                    let indent: &str = input.slice(&line_start..&text_start);

                    if text.is_empty() {
                        continue;
                    }

                    let mut found = false;
                    for (i, block) in indents.iter().enumerate() {
                        if indent == block.indentation {
                            for _ in i + 1..indents.len() {
                                let popped_type = indents.pop().unwrap().typ;

                                match popped_type {
                                    IndentationLevelType::FstringExpr => {
                                        next_parse_line_in_fstring = true;
                                    }
                                    IndentationLevelType::VerbatimFstringExpr => {
                                        next_parse_line_in_verbatim_fstring = true;
                                    }
                                    IndentationLevelType::Block => {
                                        lines.push(LexerLine::EndBlock(
                                            Span::new((), *line_start.inner()..*text_start.inner()),
                                            Token::Eol,
                                        ));
                                    }
                                    IndentationLevelType::Continuation => (),
                                }
                            }

                            found = true;
                            break;
                        }
                    }

                    if !found {
                        if indents.is_empty()
                            || indent.starts_with(indents.last().unwrap().indentation)
                        {
                            if next_line_expectation != NextLineExpectation::None {
                                lines.push(LexerLine::BeginBlock(Span::new(
                                    (),
                                    *line_start.inner()..*text_start.inner(),
                                )));
                            }

                            indents.push(IndentationLevel {
                                indentation: indent,
                                start_cursor: *line_start.inner(),
                                typ: match next_line_expectation {
                                    NextLineExpectation::ExpectNewBlock => {
                                        IndentationLevelType::Block
                                    }
                                    NextLineExpectation::ExpectFstringBlock => {
                                        IndentationLevelType::FstringExpr
                                    }
                                    NextLineExpectation::ExpectVerbatimFstringBlock => {
                                        IndentationLevelType::VerbatimFstringExpr
                                    }
                                    NextLineExpectation::None => IndentationLevelType::Continuation,
                                },
                            });
                        } else {
                            lines.push(LexerLine::IndentError(
                                "unaligned indentation",
                                Span::new((), *line_start.inner()..*text_start.inner()),
                            ));

                            continue;
                        }
                    } else {
                        if next_line_expectation != NextLineExpectation::None {
                            lines.push(LexerLine::IndentError(
                                "expected indented block",
                                Span::new((), *line_start.inner()..*text_start.inner()),
                            ));
                        }
                    }

                    // rewrite the line-end token if its a continuation
                    if indents.last().unwrap().typ == IndentationLevelType::Continuation {
                        match lines.pop().unwrap() {
                            LexerLine::Line(mut l) => {
                                l.end_token = Some(Token::Continuation);
                                lines.push(LexerLine::Line(l));
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

                    lines.push(LexerLine::Line(TokenizeLineInfo {
                        text: text,
                        span: Span::new((), *line_start.inner()..*line_end.inner()),
                        end_token: if next_next_line == NextLineExpectation::None {
                            Some(Token::Eol)
                        } else {
                            None
                        },
                        starts_as_fstring: parse_line_in_fstring,
                        starts_as_verbatim_fstring: parse_line_in_verbatim_fstring,
                    }));

                    next_line_expectation = next_next_line;
                }
                Err(LineParseErr::Eof) => break,
                Err(LineParseErr::UnterminatedString) => {
                    return Err(Rich::custom(
                        Span::new((), *input.cursor().inner() - 1..*input.cursor().inner()),
                        "unterminated string",
                    ));
                }
                Err(LineParseErr::UnterminatedVerbatimString) => {
                    return Err(Rich::custom(
                        Span::new((), *input.cursor().inner() - 1..*input.cursor().inner()),
                        "unterminated verbatim string",
                    ));
                }
                Err(LineParseErr::UnterminatedFstringExpr) => {
                    return Err(Rich::custom(
                        Span::new((), *input.cursor().inner() - 1..*input.cursor().inner()),
                        "unterminated fstring expr",
                    ));
                }
                Err(LineParseErr::UnmatchedFstringBrace) => {
                    return Err(Rich::custom(
                        Span::new((), *input.cursor().inner() - 1..*input.cursor().inner()),
                        "unmatched fstring brace",
                    ));
                }
                Err(LineParseErr::NoNestedStringsInFstring) => {
                    return Err(Rich::custom(
                        Span::new((), *input.cursor().inner() - 1..*input.cursor().inner()),
                        "no nested strings in fstring",
                    ));
                }
            }
        }

        while !indents.is_empty() {
            match indents.pop().unwrap().typ {
                IndentationLevelType::Continuation => (),
                IndentationLevelType::Block => {
                    lines.push(LexerLine::EndBlock(
                        Span::new((), *input.cursor().inner()..*input.cursor().inner()),
                        Token::Eol,
                    ));
                }
                IndentationLevelType::FstringExpr => {
                    return Err(Rich::custom(
                        Span::new((), *input.cursor().inner()..*input.cursor().inner()),
                        "unmatched fstring brace",
                    ));
                }
                IndentationLevelType::VerbatimFstringExpr => {
                    return Err(Rich::custom(
                        Span::new((), *input.cursor().inner()..*input.cursor().inner()),
                        "unmatched fstring brace",
                    ));
                }
            }
        }

        Ok(lines)
    })
}

#[derive(Debug)]
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
pub type TExtra<'src> = extra::Err<LexerError<'src>>;

fn line_lexer<'src, TInput>(
    starts_as_fstring: bool,
    starts_as_verbatim_fstring: bool,
) -> impl Parser<'src, TInput, TokenList<'src>, TExtra<'src>>
where
    TInput: StrInput<'src, Token = char, Span = SimpleSpan, Slice = &'src str>,
{
    let mut tokens =
        chumsky::recursive::Recursive::<Indirect<TInput, TokenList, TExtra>>::declare();

    let num = text::int(10)
        .then(just('.').then(text::digits(10)).or_not())
        .to_slice()
        .map(Token::Num);

    let str_ = just("\"").ignore_then(custom(|input| {
        let mut start = input.cursor();
        let mut quotes = 1;

        while input.peek() == Some('"') && quotes < 3 {
            quotes += 1;
            input.next();
        }

        if quotes == 2 {
            return Ok(Token::Str(input.slice(&start..&start)));
        }

        start = input.cursor();

        let is_verbatim = quotes == 3;
        quotes = 0;

        let mut end = input.cursor();
        let mut prev_char = '\0';
        while let Some(chr) = input.peek() {
            if prev_char != '"' {
                quotes = 0;
                end = input.cursor();
            }

            if chr == '"' {
                quotes += 1;
            }

            if (quotes == 3 && is_verbatim) || (quotes == 1 && !is_verbatim) {
                break;
            }

            input.next();

            prev_char = chr;
        }

        if input.peek().is_none() {
            return Err(Rich::custom(
                Span::new((), *start.inner()..*end.inner()),
                "unterminated regular string",
            ));
        }

        input.next();

        Ok(Token::Str(input.slice(&start..&end)))
    }));

    let fstr_bare = just("f\"").ignore_then(custom(|input| {
        let start = input.cursor();

        while let Some(chr) = input.peek() {
            println!("fstr_bare: {chr:?}");

            if chr == '"' {
                input.next();
                return Ok(Token::FstringBegin(input.slice(&start..&input.cursor())));
            }

            if chr == '{' {
                input.next();
                if input.peek() != Some('{') {
                    return Err(Rich::custom(
                        Span::new((), *start.inner()..*input.cursor().inner()),
                        "expected no {",
                    ));
                }
                input.next();
            }

            input.next();
        }

        Err(Rich::custom(
            Span::new((), *start.inner()..*input.cursor().inner()),
            "unterminated fstring",
        ))
    }));

    let fstr_begin = just("f\"").ignore_then(custom(|input| {
        let start = input.cursor();

        while let Some(chr) = input.peek() {
            println!("fstr_begin: {chr:?}");

            if chr == '"' {
                input.next();
                return Err(Rich::custom(
                    Span::new((), *start.inner()..*input.cursor().inner()),
                    "expected a {",
                ));
            }

            if chr == '{' {
                let end = input.cursor();

                input.next();
                if input.peek() != Some('{') {
                    return Ok(Token::FstringBegin(input.slice(&start..&end)));
                }
                input.next();
            }

            input.next();
        }

        Err(Rich::custom(
            Span::new((), *start.inner()..*input.cursor().inner()),
            "unterminated fstring",
        ))
    }));

    let fstr_middle = just("}").ignore_then(custom(|input| {
        let start = input.cursor();

        while let Some(chr) = input.peek() {
            println!("fstr_middle: {chr:?}");

            if chr == '"' {
                input.next();
                return Err(Rich::custom(
                    Span::new((), *start.inner()..*input.cursor().inner()),
                    "expected a {",
                ));
            }

            if chr == '{' {
                let end = input.cursor();
                input.next();
                if input.peek() != Some('{') {
                    return Ok(Token::FstringMiddle(input.slice(&start..&end)));
                }
                input.next();
            }

            input.next();
        }

        Err(Rich::custom(
            Span::new((), *start.inner()..*input.cursor().inner()),
            "unterminated fstring",
        ))
    }));

    let fstr_end = just("}")
        .ignore_then(custom(|input| {
            let start = input.cursor();

            while let Some(chr) = input.peek() {
                println!("fstr_end: {chr:?}");

                if chr == '"' {
                    let end = input.cursor();
                    input.next();
                    return Ok(Token::FstringMiddle(input.slice(&start..&end)));
                }

                if chr == '{' {
                    input.next();
                    if input.peek() != Some('{') {
                        return Err(Rich::custom(
                            Span::new((), *start.inner()..*input.cursor().inner()),
                            "expected no more {",
                        ));
                    }
                    input.next();
                }

                input.next();
            }

            Err(Rich::custom(
                Span::new((), *start.inner()..*input.cursor().inner()),
                "unterminated fstring-end",
            ))
        }))
        .map_with(|end, e| (end, e.span()));

    let fstr = group((
        fstr_begin.map_with(|begin, e| (begin, e.span())),
        text::whitespace().ignore_then(tokens.clone()),
        fstr_middle
            .map_with(|mid, e| (mid, e.span()))
            .then(text::whitespace().ignore_then(tokens.clone()))
            .map(|(a, mut b)| {
                b.0.insert(0, a);
                b
            })
            .repeated()
            .collect::<Vec<_>>(),
        fstr_end,
    ))
    .map(|(begin, first_tokens, parts, end)| {
        println!("{begin:?}, {first_tokens:?}, {parts:?}, {end:?}");
        let mut result = vec![begin];
        result.extend(first_tokens.0);
        for part in parts {
            result.extend(part.0);
        }
        result.push(end);
        TokenList(result)
    })
    .or(fstr_bare.map_with(|begin, e| TokenList(vec![(begin, e.span())])))
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

    let unit = choice((num, str_, symbol, ident)).boxed();

    tokens.define(
        choice((
            fstr,
            fstr_begin
                .then_ignore(end())
                .map_with(|begin, e| TokenList(vec![(begin, e.span())])),
            unit.then_ignore(text::whitespace())
                .map_with(|token, e| (TokenList(vec![(token, e.span())]))),
        ))
        .repeated()
        .collect::<Vec<_>>()
        .map(|x: Vec<_>| TokenList(x.into_iter().flat_map(|x| x.0).collect()))
        .map_err(|e: LexerError| {
            Rich::custom(
                *e.span(),
                format!(
                    "unexpected {:?}",
                    e.found().map_or("EOF".to_string(), |x| x.to_string())
                ),
            )
        }),
    );

    if starts_as_fstring {
        fstr_end
            .then(tokens)
            .map(|(end, mut tokens)| {
                tokens.0.insert(0, end);
                tokens
            })
            .boxed()
    } else {
        tokens.boxed()
    }
}

pub fn tokenize<'src>(s: &'src str) -> (TokenList<'src>, Vec<LexerError<'src>>) {
    let lines = match semantic_whitespace().parse(s).into_result() {
        Ok(lines) => lines,
        Err(err) => {
            return (TokenList(Vec::new()), err);
        }
    };

    println!("{lines:?}");

    let mut tokens = TokenList(Vec::new());
    let mut errs = Vec::new();

    for l in lines {
        match l {
            LexerLine::Line(line_info) => {
                let TokenizeLineInfo {
                    text,
                    span,
                    end_token,
                    starts_as_fstring,
                    starts_as_verbatim_fstring,
                } = line_info;

                let (line_tokens, mut line_errs) =
                    line_lexer(starts_as_fstring, starts_as_verbatim_fstring)
                        .parse(text.map_span(move |s| {
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
                tokens.0.push((Token::Symbol("BEGIN_BLOCK"), span));
            }
            LexerLine::EndBlock(span, end_token) => {
                tokens.0.push((Token::Symbol("END_BLOCK"), span));
                tokens.0.push((end_token, span));
            }
        }
    }

    (tokens, errs)
}
