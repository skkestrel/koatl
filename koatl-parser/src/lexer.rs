#![allow(unused_variables, dead_code)]

pub type Span = SimpleSpan<usize, ()>;

use chumsky::{
    input::{Cursor, InputRef, StrInput},
    label::LabelError,
    prelude::*,
};
use std::{
    collections::HashSet,
    fmt::{self},
};

#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum Token<'src> {
    Ident(&'src str),
    None,
    Bool(bool),

    Str(String),
    FstrBegin(String),
    FstrContinue(String),

    Num(&'src str),
    Kw(&'src str),

    Symbol(&'src str),

    Indent,
    Dedent,
    Eol,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TriviumType {
    Newline,
    Whitespace,
    LineComment,
    BlockComment,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Trivium<'src> {
    pub span: Span,
    pub typ: TriviumType,
    pub value: &'src str,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SToken<'src> {
    pub span: Span,
    pub token: Token<'src>,
    pub leading_trivia: Vec<Trivium<'src>>,
    pub trailing_trivia: Vec<Trivium<'src>>,
}

impl<'src> SToken<'src> {
    pub fn new(token: Token<'src>, span: Span, leading_trivia: Vec<Trivium<'src>>) -> Self {
        SToken {
            span,
            token,
            leading_trivia,
            trailing_trivia: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct TokenList<'src>(pub Vec<SToken<'src>>);

impl<'src> IntoIterator for TokenList<'src> {
    type Item = SToken<'src>;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::None => write!(f, "<none>"),
            Token::Bool(x) => write!(f, "<literal {x}>"),
            Token::Num(n) => write!(f, "<literal {n}>"),
            Token::Str(s) => write!(f, "<literal {s}>"),
            Token::Symbol(s) => write!(f, "{s}"),
            Token::Ident(s) => write!(f, "{s}"),
            Token::Kw(s) => write!(f, "<{s}>"),
            Token::FstrBegin(s) => write!(f, "<f_begin {s}>"),
            Token::FstrContinue(s) => write!(f, "<f_middle {s}>"),
            Token::Indent => write!(f, "<indent>"),
            Token::Dedent => write!(f, "<dedent>"),
            Token::Eol => write!(f, "<eol>"),
        }
    }
}

impl<'src> std::iter::FromIterator<SToken<'src>> for TokenList<'src> {
    fn from_iter<T: IntoIterator<Item = SToken<'src>>>(iter: T) -> Self {
        TokenList(iter.into_iter().collect())
    }
}

impl fmt::Display for TokenList<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, stoken) in self.0.iter().enumerate() {
            write!(f, "{}", stoken.token)?;
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

#[derive(Debug, Clone, PartialEq)]
pub enum ParseBlockMode {
    BeginInput,
    NewBlock,
    Continuation,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseBlockEndType {
    Delimiter,
    EolOrEof,
}

pub type TOutput<'src> = TokenList<'src>;
pub type TError<'src> = Rich<'src, char, Span>;
pub type TExtra<'src> = extra::Full<TError<'src>, (), ()>;

type TResult<'src, T> = Result<T, TError<'src>>;

pub fn py_escape_str(s: &str) -> String {
    let mut escaped_string = String::new();

    // Iterate over each character in the input string.
    for c in s.chars() {
        match c {
            // Handle common escape sequences
            '\0' => escaped_string.push_str("\\0"), // Null character
            '\n' => escaped_string.push_str("\\n"),
            '\r' => escaped_string.push_str("\\r"),
            '\t' => escaped_string.push_str("\\t"),
            '\"' => escaped_string.push_str("\\\""),
            '\\' => escaped_string.push_str("\\\\"),

            // Handle all other ASCII control characters (0x00 to 0x1F) using hex escapes
            c if (c as u32) < 32 => {
                escaped_string.push_str(&format!("\\x{:02x}", c as u32));
            }

            // For all other characters, simply append them to the new string.
            _ => escaped_string.push(c),
        }
    }

    escaped_string
}

pub fn py_escape_fstr(s: &str) -> String {
    py_escape_str(s)
        .chars()
        .map(|c| match c {
            '{' => "{{".to_string(),
            '}' => "}}".to_string(),
            _ => c.to_string(),
        })
        .collect()
}

// TODO don't duplicate this with parser below?
pub fn is_valid_ident(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }

    if !s
        .chars()
        .next()
        .map_or(false, |c| c.is_ascii_alphabetic() || c == '_')
    {
        return false;
    }

    s.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
}

struct TokenizeCtx<'src: 'parse, 'parse, 'input, TInput>
where
    TInput: StrInput<'src, Token = char, Span = SimpleSpan, Slice = &'src str>,
{
    input: &'input mut InputRef<'src, 'parse, TInput, TExtra<'src>>,
    keywords: HashSet<String>,
    keep_trivia: bool,
    unassigned_trivia: Vec<Trivium<'src>>,
}

impl<'src: 'parse, 'parse, 'input, TInput> TokenizeCtx<'src, 'parse, 'input, TInput>
where
    TInput: StrInput<'src, Token = char, Span = SimpleSpan, Slice = &'src str>,
{
    fn new(input: &'input mut InputRef<'src, 'parse, TInput, TExtra<'src>>) -> Self {
        static KEYWORDS: &[&str] = &[
            "if", "then", "else", "import", "export", "as", "class", "while", "for", "in", "break",
            "continue", "with", "yield", "global", "return", "raise", "try", "except", "finally",
            "and", "or", "not", "await", "let", "const", "with",
        ];

        let keywords = HashSet::<String>::from_iter(KEYWORDS.iter().map(|s| s.to_string()));

        TokenizeCtx {
            input,
            keywords,
            keep_trivia: false,
            unassigned_trivia: Vec::new(),
        }
    }

    fn cursor(&self) -> Cursor<'src, 'parse, TInput> {
        self.input.cursor()
    }

    fn ucursor(&self) -> usize {
        *self.cursor().inner()
    }

    fn peek(&mut self) -> Option<char> {
        self.input.peek()
    }

    fn next(&mut self) -> Option<char> {
        self.input.next()
    }

    fn span_since(&mut self, start: &Cursor<'src, 'parse, TInput>) -> Span {
        self.input.span_since(start)
    }

    fn slice_since(&mut self, range: &Cursor<'src, 'parse, TInput>) -> &'src str {
        self.input.slice_since(range..)
    }

    fn try_parse<TOut>(
        &mut self,
        parse_fn: impl FnOnce(&mut Self) -> TResult<'src, TOut>,
    ) -> TResult<'src, TOut> {
        let start = self.input.save();

        match parse_fn(self) {
            Ok(result) => Ok(result),
            Err(e) => {
                self.input.rewind(start);
                Err(e)
            }
        }
    }

    fn look_ahead<TOut>(
        &mut self,
        parse_fn: impl FnOnce(&mut Self) -> TResult<'src, TOut>,
    ) -> TResult<'src, TOut> {
        let start = self.input.save();

        match parse_fn(self) {
            Ok(r) => {
                self.input.rewind(start);
                Ok(r)
            }
            Err(e) => {
                self.input.rewind(start);
                Err(e)
            }
        }
    }

    fn parse_indentation(&mut self) -> TResult<'src, (usize, Span, Vec<Trivium<'src>>)> {
        let start = self.cursor();
        let mut indent_level: usize = 0;

        if self.peek().is_none() {
            return Err(Rich::custom(
                self.span_since(&start),
                "expected indentation, not eof",
            ));
        }

        while let Some(c) = self.peek() {
            if c != ' ' && c != '\t' {
                break;
            }

            indent_level += 1;
            self.next();
        }

        Ok((indent_level, self.span_since(&start), vec![]))
    }

    fn parse_ident_or_token(&mut self) -> TResult<'src, (Token<'src>, Span)> {
        let start = self.cursor();

        let c = self.peek();
        if c.is_none_or(|c| !c.is_ascii_alphabetic() && c != '_') {
            return Err(Rich::custom(self.span_since(&start), "expected identifier"));
        }

        while let Some(c) = self.peek() {
            if c.is_ascii_alphanumeric() || c == '_' {
                self.next();
            } else {
                break;
            }
        }

        let ident = self.slice_since(&start);
        let span = self.span_since(&start);

        let token = if self.keywords.contains(ident) {
            Token::Kw(ident)
        } else if ident == "True" {
            Token::Bool(true)
        } else if ident == "False" {
            Token::Bool(false)
        } else if ident == "None" {
            Token::None
        } else {
            Token::Ident(ident)
        };

        Ok((token, span))
    }

    fn parse_symbol(&mut self) -> TResult<'src, (Token<'src>, Span)> {
        const POLYGRAMS: &[&str] = &[
            "+=", "-=", "*=", "/=", "|=", "??=", "===", "<=>", "=>", "..", "==", "<>", "<=", ">=",
            "//", "**", "??", ".=", "::", "||", "&&", "^^", ">>", "<<",
        ];
        const MONOGRAMS: &str = "[](){}<>.,;:!?@$%^&*+-=|\\/`~";

        let saved = self.input.save();
        let start = self.cursor();
        for _ in 0..3 {
            self.next();
        }

        let sl = self.slice_since(&start);
        for polygram in POLYGRAMS {
            if sl.starts_with(polygram) {
                self.input.rewind(saved);
                for _ in 0..polygram.len() {
                    self.next();
                }
                return Ok((Token::Symbol(polygram), self.span_since(&start)));
            }
        }
        for monogram in MONOGRAMS.chars() {
            if sl.starts_with(monogram) {
                self.input.rewind(saved);
                self.next();
                let symbol = self.slice_since(&start);
                return Ok((Token::Symbol(symbol), self.span_since(&start)));
            }
        }

        Err(Rich::custom(self.span_since(&start), "expected a symbol"))
    }

    fn parse_number(&mut self) -> TResult<'src, (Token<'src>, Span, bool)> {
        let start = self.cursor();

        let c = self.peek();

        if c.is_none_or(|c| !(c.is_ascii_digit() || c == '.')) {
            return Err(Rich::custom(self.span_since(&start), "expected a number"));
        };

        let mut digits_before_dot = false;
        let mut digits_after_dot = false;
        let mut after_dot = false;

        while let Some(c) = self.peek() {
            if self.look_ahead(|x| x.parse_seq("..")).is_ok() {
                break;
            }

            if c == '.' {
                after_dot = true;
            } else {
                if !(c.is_ascii_digit() || c == '_') {
                    break;
                }

                if after_dot && c.is_ascii_digit() {
                    digits_after_dot = true;
                }

                if !after_dot && c.is_ascii_digit() {
                    digits_before_dot = true;
                }
            }

            self.next();
        }

        if !digits_before_dot && !digits_after_dot {
            return Err(Rich::custom(
                self.span_since(&start),
                "expected at least one digit",
            ));
        }

        let span = self.span_since(&start);
        let num_str = self.slice_since(&start);

        Ok((Token::Num(num_str), span, after_dot))
    }

    fn parse_newline(&mut self) -> TResult<'src, Trivium<'src>> {
        let start = self.cursor();
        let mut err = false;

        match self.next() {
            Some('\r') => {
                if self.next() != Some('\n') {
                    err = true;
                }
            }
            Some('\n') => {}
            None => {
                err = true;
            }
            _ => {
                err = true;
            }
        }

        if err {
            return Err(Rich::custom(self.span_since(&start), "expected newline"));
        } else {
            Ok(Trivium {
                span: self.span_since(&start),
                typ: TriviumType::Newline,
                value: self.slice_since(&start),
            })
        }
    }

    fn parse_newline_or_eof(&mut self) -> TResult<'src, Vec<Trivium<'src>>> {
        if self.peek().is_none() {
            return Ok(vec![]);
        }

        if let Ok(tr) = self.try_parse(TokenizeCtx::parse_newline) {
            if self.keep_trivia {
                return Ok(vec![tr]);
            }
            return Ok(vec![]);
        }

        return Err(Rich::custom(
            self.span_since(&self.cursor()),
            "expected newline or end of file",
        ));
    }

    fn parse_whitespace(&mut self) -> TResult<'src, Trivium<'src>> {
        let start = self.cursor();
        let mut found_one = false;

        while let Some(c) = self.peek() {
            if !c.is_whitespace() {
                break;
            }

            if c == '\n' || self.look_ahead(|ctx| ctx.parse_seq("\r\n")).is_ok() {
                break;
            }

            found_one = true;
            self.next();
        }

        if !found_one {
            return Err(Rich::custom(self.span_since(&start), "expected whitespace"));
        }

        Ok(Trivium {
            span: self.span_since(&start),
            typ: TriviumType::Whitespace,
            value: self.slice_since(&start),
        })
    }

    fn parse_empty_line(&mut self) -> TResult<'src, Vec<Trivium<'src>>> {
        if self.peek().is_none() {
            return Err(Rich::custom(
                self.span_since(&self.cursor()),
                "expected empty line, found eof",
            ));
        }

        let mut trivia = vec![];
        if let Ok(t) = self.try_parse(|x| x.parse_whitespace()) {
            trivia.push(t);
        }

        trivia.extend(self.try_parse(TokenizeCtx::parse_newline_or_eof)?);

        Ok(trivia)
    }

    fn parse_line_comment(&mut self) -> TResult<'src, Trivium<'src>> {
        let start = self.cursor();

        if self.next() != Some('#') {
            return Err(Rich::custom(
                self.span_since(&start),
                "expected line comment",
            ));
        }

        while self.look_ahead(TokenizeCtx::parse_newline_or_eof).is_err() {
            self.next();
        }

        Ok(Trivium {
            span: self.span_since(&start),
            typ: TriviumType::LineComment,
            value: self.slice_since(&start),
        })
    }

    fn parse_block_comment(&mut self) -> TResult<'src, Trivium<'src>> {
        let start = self.cursor();

        self.parse_seq("#-")?;

        while let Some(_) = self.peek() {
            if self.try_parse(|ctx| ctx.parse_seq("-#")).is_ok() {
                return Ok(Trivium {
                    span: self.span_since(&start),
                    typ: TriviumType::BlockComment,
                    value: self.slice_since(&start),
                });
            }

            // nesting
            if self.try_parse(|ctx| ctx.parse_block_comment()).is_ok() {
                continue;
            }

            self.next();
        }

        return Err(Rich::custom(
            self.span_since(&start),
            "unterminated block comment",
        ));
    }

    fn parse_trivia(&mut self) -> TResult<'src, Vec<Trivium<'src>>> {
        let mut trivia = Vec::new();

        while self.peek().is_some() {
            if let Ok(t) = self.try_parse(|x| x.parse_whitespace()) {
                trivia.push(t);
                continue;
            }

            if let Ok(t) = self.try_parse(|x| x.parse_block_comment()) {
                trivia.push(t);
                continue;
            }

            if let Ok(t) = self.try_parse(|x| x.parse_line_comment()) {
                trivia.push(t);
                continue;
            }

            break;
        }

        if self.keep_trivia {
            Ok(trivia)
        } else {
            Ok(vec![])
        }
    }

    fn parse_char(&mut self, c: char) -> TResult<'src, ()> {
        let start = self.cursor();

        if self.next() != Some(c) {
            return Err(Rich::custom(
                self.span_since(&start),
                format!("expected '{c}'"),
            ));
        }

        Ok(())
    }

    fn parse_seq(&mut self, seq: &str) -> TResult<'src, ()> {
        let start = self.cursor();

        for c in seq.chars() {
            if self.next() != Some(c) {
                return Err(Rich::custom(
                    self.span_since(&start),
                    format!("expected '{c}'"),
                ));
            }
        }

        Ok(())
    }

    fn parse_escaped_char(&mut self) -> TResult<'src, char> {
        let start = self.cursor();

        match self.next() {
            Some('\\') => {
                if let Some(next) = self.next() {
                    match next {
                        'n' => return Ok('\n'),
                        't' => return Ok('\t'),
                        'r' => return Ok('\r'),
                        c => {
                            return Ok(c);
                        }
                    }
                }

                Err(Rich::custom(self.span_since(&start), "unterminated escape"))
            }
            Some(c) => Ok(c),
            None => Err(Rich::custom(self.span_since(&start), "unterminated string")),
        }
    }

    fn parse_fstr_inner(
        &mut self,
        ending_char: char,
        ending_char_count: u32,
        verbatim: bool,
    ) -> TResult<'src, TokenList<'src>> {
        let mut marker = self.cursor();
        let mut tokens = vec![];
        let mut current_str = String::new();

        let mut seen_quotes = 0;
        let mut ending_marker = self.input.save();
        loop {
            if !verbatim && self.try_parse(|x| x.parse_newline()).is_ok() {
                return Err(Rich::custom(
                    self.span_since(&marker),
                    "unterminated fstring",
                ));
            }

            let Some(peeked) = self.peek() else {
                return Err(Rich::custom(
                    self.span_since(&marker),
                    "unterminated fstring",
                ));
            };

            let mut save_marker = false;

            if peeked == ending_char {
                seen_quotes += 1;

                if seen_quotes == ending_char_count {
                    let span = self.span_since(&marker);
                    if tokens.len() == 0 {
                        tokens.push(SToken::new(
                            Token::FstrBegin(current_str),
                            span,
                            Vec::new(), // First token trivia will be assigned later.
                        ));
                    } else {
                        tokens.push(SToken::new(
                            Token::FstrContinue(current_str),
                            span,
                            Vec::new(),
                        ));
                    }

                    self.input.rewind(ending_marker);

                    return Ok(TokenList(tokens));
                }
            } else {
                for _ in 0..seen_quotes {
                    current_str.push(ending_char);
                }
                seen_quotes = 0;
                save_marker = true;
            }

            if self.try_parse(|x| x.parse_seq("{{")).is_ok() {
                current_str.push('{');
            } else if self.try_parse(|x| x.parse_seq("}}")).is_ok() {
                current_str.push('}');
            } else if self.try_parse(|x| x.parse_char('}')).is_ok() {
                return Err(Rich::custom(self.span_since(&marker), "unexpected '}'"));
            } else if self.try_parse(|x| x.parse_char('{')).is_ok() {
                let span = self.span_since(&marker);
                let trivia = self.parse_trivia()?;

                let mut new_token = if tokens.len() == 0 {
                    SToken::new(Token::FstrBegin(current_str), span, Vec::new())
                } else {
                    SToken::new(Token::FstrContinue(current_str), span, Vec::new())
                };

                new_token.trailing_trivia = trivia;
                tokens.push(new_token);
                current_str = String::new();

                let (inner_tokens, _, inner_span) =
                    self.try_parse(|x| x.parse_block(0, ParseBlockMode::BeginInput))?;

                self.parse_indentation()?;

                marker = self.cursor();
                let mut format_tokens = vec![];
                if self.try_parse(|x| x.parse_seq("!")).is_ok() {
                    format_tokens.push(SToken::new(
                        Token::Symbol("!"),
                        self.span_since(&marker),
                        Vec::new(),
                    ));
                    format_tokens.extend(self.parse_fstr_inner('}', 1, verbatim)?);
                }

                self.parse_seq("}")?;

                tokens.push(SToken::new(
                    Token::Indent,
                    Span::new(inner_span.context, inner_span.start..inner_span.start),
                    Vec::new(),
                ));
                tokens.extend(inner_tokens.0);
                tokens.push(SToken::new(
                    Token::Dedent,
                    Span::new(inner_span.context, inner_span.end..inner_span.end),
                    Vec::new(),
                ));
                tokens.extend(format_tokens);

                marker = self.cursor();
            } else {
                let next_char = if verbatim {
                    self.next().ok_or_else(|| {
                        Rich::custom(self.span_since(&marker), "unterminated verbatim fstring")
                    })?
                } else {
                    self.parse_escaped_char()?
                };

                if save_marker {
                    current_str.push(next_char);
                }
            }

            if save_marker {
                ending_marker = self.input.save();
            }
        }
    }

    fn parse_regular_str(&mut self, ch: char) -> TResult<'src, (Token<'src>, Span)> {
        let start = self.cursor();
        let mut quote_count = 0;

        let mut verbatim = self.try_parse(|x| x.parse_char('r')).is_ok();

        while self.try_parse(|x| x.parse_char(ch)).is_ok() {
            quote_count += 1;
        }

        if quote_count == 2 {
            let span = self.span_since(&start);
            return Ok((Token::Str(String::new()), span));
        }

        verbatim |= quote_count >= 3;

        let mut seen_quotes = 0;
        let mut s = String::new();
        loop {
            if !verbatim && self.try_parse(|x| x.parse_newline()).is_ok() {
                return Err(Rich::custom(self.span_since(&start), "unterminated string"));
            }

            let Some(peeked) = self.peek() else {
                return Err(Rich::custom(self.span_since(&start), "unterminated string"));
            };

            let mut push_next = false;
            if peeked == ch {
                seen_quotes += 1;

                if seen_quotes == quote_count {
                    self.next();
                    let span = self.span_since(&start);
                    return Ok((Token::Str(s), span));
                }
            } else {
                for _ in 0..seen_quotes {
                    s.push(ch);
                }
                seen_quotes = 0;
                push_next = true;
            }

            let next_char = if verbatim {
                self.next().unwrap()
            } else {
                self.parse_escaped_char()?
            };

            if push_next {
                s.push(next_char);
            }
        }
    }

    fn parse_fstr(&mut self, ch: char) -> TResult<'src, TokenList<'src>> {
        let start = self.cursor();
        let mut verbatim = self.try_parse(|x| x.parse_seq("r")).is_ok();

        self.parse_seq("f")?;
        let mut quote_count = 0;

        while self.try_parse(|x| x.parse_char(ch)).is_ok() {
            quote_count += 1;
        }

        if quote_count == 2 {
            let span = self.span_since(&start);
            return Ok(TokenList(vec![SToken::new(
                Token::FstrBegin(String::new()),
                span,
                Vec::new(),
            )]));
        }

        verbatim |= quote_count >= 3;

        let inner = self.parse_fstr_inner(ch, quote_count, verbatim)?;

        let start = self.cursor();

        let mut close_quote_count = 0;
        while self.try_parse(|x| x.parse_char(ch)).is_ok() {
            close_quote_count += 1;
        }

        if quote_count != close_quote_count {
            return Err(Rich::custom(
                self.span_since(&start),
                format!(
                    "expected {} closing quotes, found {}",
                    quote_count, close_quote_count
                ),
            ));
        }

        return Ok(inner);
    }

    fn parse_str_start(&mut self) -> TResult<'src, ()> {
        let start = self.cursor();

        let _ = self.try_parse(|x| x.parse_ident_or_token());
        self.parse_char('"')?;

        Ok(())
    }

    fn parse_str(&mut self) -> TResult<'src, TokenList<'src>> {
        if self.look_ahead(|x| x.parse_seq("rf\"")).is_ok() {
            let tokens = self.parse_fstr('"')?;
            return Ok(tokens);
        }

        if self.look_ahead(|x| x.parse_seq("f\"")).is_ok() {
            let tokens = self.parse_fstr('"')?;
            return Ok(tokens);
        }

        if self.look_ahead(|x| x.parse_seq("r\"")).is_ok() {
            let (token, span) = self.parse_regular_str('"')?;
            return Ok(TokenList(vec![SToken::new(token, span, Vec::new())]));
        }

        if self.look_ahead(|x| x.parse_seq("\"")).is_ok() {
            let (token, span) = self.parse_regular_str('"')?;
            return Ok(TokenList(vec![SToken::new(token, span, Vec::new())]));
        }

        Err(Rich::custom(
            self.span_since(&self.cursor()),
            "expected string start",
        ))
    }

    fn parse_block(
        &mut self,
        current_indent: usize,
        mode: ParseBlockMode,
    ) -> TResult<'src, (TokenList<'src>, ParseBlockEndType, Span)> {
        let start = self.cursor();

        let mut tokens = vec![];
        let mut line_tokens = vec![];

        let mut block_indent = None;
        let mut cur_block_unassigned_trivia = std::mem::take(&mut self.unassigned_trivia);

        const OPEN_DELIMS: &[char] = &['{', '[', '('];
        const CLOSE_DELIMS: &[char] = &['}', ']', ')'];
        let mut delim_stack = vec![];
        let mut expect_new_block = false;

        let break_type = 'lines: loop {
            if expect_new_block {
                expect_new_block = false;

                let new_block = self
                    .try_parse(|x| x.parse_block(block_indent.unwrap(), ParseBlockMode::NewBlock));

                let (new_block_tokens, new_block_break_type, new_block_span) = new_block?;

                line_tokens.push(SToken::new(
                    Token::Indent,
                    Span::new(
                        new_block_span.context,
                        new_block_span.start..new_block_span.start,
                    ),
                    Vec::new(),
                ));
                line_tokens.extend(new_block_tokens.0);
                line_tokens.push(SToken::new(
                    Token::Dedent,
                    Span::new(
                        new_block_span.context,
                        new_block_span.end..new_block_span.end,
                    ),
                    Vec::new(),
                ));

                if new_block_break_type == ParseBlockEndType::EolOrEof {
                    continue 'lines;
                }
            } else {
                // prune empty lines
                while let Ok(empty_line_trivia) =
                    self.try_parse(|ctx| TokenizeCtx::parse_empty_line(ctx))
                {
                    cur_block_unassigned_trivia.extend(empty_line_trivia);
                }

                let before_indentation = self.input.save();

                let Ok((line_indent_level, line_indent_span, line_indent_trivia)) =
                    self.try_parse(|x| x.parse_indentation())
                else {
                    if block_indent.is_none() {
                        return Err(Rich::custom(
                            self.span_since(&self.cursor()),
                            "expected an indented block",
                        ));
                    }

                    // EOF
                    break ParseBlockEndType::EolOrEof;
                };

                cur_block_unassigned_trivia.extend(line_indent_trivia);

                if let Some(block_indent) = block_indent {
                    if line_indent_level > block_indent {
                        self.input.rewind(before_indentation);

                        // handle continuation
                        let (mut continuation_tokens, continuation_break_type, _span) = self
                            .try_parse(|x| {
                                x.parse_block(block_indent, ParseBlockMode::Continuation)
                            })?;

                        if let Some(first_token) = continuation_tokens.0.first_mut() {
                            first_token
                                .leading_trivia
                                .extend(cur_block_unassigned_trivia.drain(..));

                            line_tokens.extend(continuation_tokens.0);
                        }

                        if continuation_break_type == ParseBlockEndType::EolOrEof {
                            continue 'lines;
                        }
                    } else if line_indent_level < block_indent {
                        self.input.rewind(before_indentation);

                        break ParseBlockEndType::EolOrEof;
                    } else if !line_tokens.is_empty() {
                        tokens.extend(line_tokens.drain(..));

                        if mode != ParseBlockMode::Continuation {
                            tokens.push(SToken::new(
                                Token::Eol,
                                self.span_since(&self.cursor()),
                                Vec::new(),
                            ));
                        }
                    }
                } else {
                    match mode {
                        ParseBlockMode::BeginInput => {}
                        ParseBlockMode::NewBlock => {
                            if line_indent_level <= current_indent {
                                return Err(Rich::custom(
                                    line_indent_span,
                                    "expected an indented block",
                                ));
                            }
                        }
                        ParseBlockMode::Continuation => {
                            if line_indent_level <= current_indent {
                                return Err(Rich::custom(
                                    line_indent_span,
                                    "expected an indented continuation",
                                ));
                            }
                        }
                    }
                    block_indent = Some(line_indent_level);
                }
            }

            cur_block_unassigned_trivia.extend(self.parse_trivia()?);

            loop {
                // over tokens in line
                if self.look_ahead(TokenizeCtx::parse_str_start).is_ok() {
                    let toks = self.parse_str()?;
                    line_tokens.extend(toks.0);
                } else {
                    let tok;

                    let start_curs = self.cursor();
                    let saved = self.input.save();

                    if let Ok((token, span, after_dot)) = self.try_parse(|ctx| ctx.parse_number()) {
                        if after_dot && self.peek() == Some('.') {
                            return Err(Rich::custom(
                                self.span_since(&self.cursor()),
                                "unexpected '.'",
                            ));
                        }
                        tok = SToken::new(token, span, vec![]);
                    } else if let Ok((token, span)) =
                        self.try_parse(|ctx| ctx.parse_ident_or_token())
                    {
                        tok = SToken::new(token, span, vec![]);
                    } else if let Ok((token, span)) = self.try_parse(|ctx| ctx.parse_symbol()) {
                        tok = SToken::new(token, span, vec![]);
                    } else {
                        break;
                    }

                    if let Token::Symbol("!") = &tok.token {
                        // Format specifier delimiter; end block.
                        self.input.rewind(saved);
                        break 'lines ParseBlockEndType::Delimiter;
                    }

                    if let Token::Symbol(s) = &tok.token {
                        let char = s.chars().next().unwrap_or('\0');
                        if OPEN_DELIMS.contains(&char) {
                            delim_stack.push((char, tok.span));
                        } else if let Some(i) = CLOSE_DELIMS.iter().position(|&c| c == char) {
                            let corresponding_open = OPEN_DELIMS[i];

                            if let Some((open_delim_char, span)) = delim_stack.pop() {
                                if open_delim_char != corresponding_open {
                                    let mut err = TError::custom(
                                        self.span_since(&start_curs),
                                        format!(
                                            "unmatched delimiter '{}', found '{}'",
                                            open_delim_char, s
                                        ),
                                    );

                                    // why?
                                    <chumsky::error::Rich<'_, char> as LabelError<
                                        '_,
                                        TInput,
                                        &str,
                                    >>::in_context(
                                        &mut err, "here", span
                                    );

                                    return Err(err);
                                }
                            } else {
                                self.input.rewind(saved);
                                break 'lines ParseBlockEndType::Delimiter;
                            }
                        }
                    }

                    line_tokens.push(tok);
                }

                let trailing_trivia = self.parse_trivia()?;
                let Some(last_token) = line_tokens.last_mut() else {
                    panic!();
                };
                last_token.trailing_trivia.extend(trailing_trivia);
            }

            if let Some(tok) = line_tokens.first_mut() {
                tok.leading_trivia = {
                    let mut vec = std::mem::take(&mut cur_block_unassigned_trivia);
                    vec.extend(tok.leading_trivia.drain(..));
                    vec
                };
            }

            if let Some(last_token) = line_tokens.last() {
                if let Token::Symbol(s) = last_token.token {
                    if s == "=>"
                        || s == ":"
                        || OPEN_DELIMS.contains(&s.chars().next().unwrap_or('\0'))
                    {
                        expect_new_block = true;
                    }
                }
            }

            let Ok(newline_trivium) = self.try_parse(|x| x.parse_newline_or_eof()) else {
                return Err(Rich::custom(
                    self.span_since(&self.cursor()),
                    format!("unexpected '{}'", self.peek().unwrap()),
                ));
            };

            if let Some(last_token) = line_tokens.last_mut() {
                last_token.trailing_trivia.extend(newline_trivium);
            } else {
                cur_block_unassigned_trivia.extend(newline_trivium);
            };
        };

        if !line_tokens.is_empty() {
            tokens.extend(line_tokens.drain(..));

            if mode != ParseBlockMode::Continuation {
                tokens.push(SToken::new(
                    Token::Eol,
                    self.span_since(&self.cursor()),
                    Vec::new(),
                ));
            }
        }

        /*
         * empty block is possible with some weird continuation layouts:
         *
         * x = [
         *      1
         *      2
         *   ] then ...
         *
         *   ^ this is a continuation that immediately ends due to ]
         */
        let block_span = if let Some(last_block_token) = tokens.last_mut() {
            Span::new((), *start.inner()..last_block_token.span.end)
        } else {
            Span::new((), *start.inner()..*start.inner())
        };

        let len = tokens.len();
        if len >= 2 {
            // we want to avoid putting trivia on an eol token, so put it on the second last one
            tokens[len - 2]
                .trailing_trivia
                .extend(cur_block_unassigned_trivia.drain(..));
        } else {
            self.unassigned_trivia
                .extend(cur_block_unassigned_trivia.drain(..));
        }

        Ok((TokenList(tokens), break_type, block_span))
    }

    fn tokenize_input(&mut self) -> TResult<'src, TokenList<'src>> {
        if let None = self.peek() {
            return Ok(TokenList(vec![]));
        }

        let mut tokens = TokenList(vec![]);

        let (block_tokens, _, _) = self.parse_block(0, ParseBlockMode::BeginInput)?;
        tokens.0.extend(block_tokens.0);

        Ok(tokens)
    }
}

fn lexer<'src, TInput>(keep_trivia: bool) -> impl Parser<'src, TInput, TOutput<'src>, TExtra<'src>>
where
    TInput: StrInput<'src, Token = char, Span = SimpleSpan, Slice = &'src str>,
{
    custom(move |input| {
        let mut ctx = TokenizeCtx::new(input);
        if keep_trivia {
            ctx.keep_trivia = true;
        }
        ctx.tokenize_input()
    })
}

pub fn tokenize<'src>(
    s: &'src str,
    keep_trivia: bool,
) -> (Option<TokenList<'src>>, Vec<TError<'src>>) {
    let output = lexer(keep_trivia)
        .parse(s.map_span(|s| Span::new(s.context, s.start()..s.end())))
        .into_output_errors();

    output
}

#[cfg(test)]
mod tests {
    use super::*;

    fn match_trivia(expected: &Vec<Trivium>, actual: &Vec<Trivium>) -> bool {
        if expected.len() != actual.len() {
            return false;
        }

        for (a, b) in expected.iter().zip(actual.iter()) {
            if a.typ != b.typ || a.value != b.value {
                return false;
            }
        }

        true
    }

    fn assert_has_token_with_trivia(
        token_list: &TokenList,
        token: Token,
        leading_trivia: Vec<Trivium>,
        trailing_trivia: Vec<Trivium>,
    ) {
        let mut cands = vec![];

        for t in token_list.0.iter() {
            if t.token == token {
                if match_trivia(&leading_trivia, &t.leading_trivia)
                    && match_trivia(&trailing_trivia, &t.trailing_trivia)
                {
                    return;
                }

                cands.push(t);
            }
        }

        for token in token_list.0.iter() {
            println!("{:?}", token);
        }

        assert!(
            false,
            "Expected token '{}' not found. Candidates:\n{}",
            token,
            cands
                .iter()
                .map(|t| format!("{:?}", t))
                .collect::<Vec<_>>()
                .join("\n")
        );
    }

    fn simple_tokenize(source: &str) -> TokenList {
        let (tokens, errors) = tokenize(source, true);
        if !errors.is_empty() {
            assert!(false, "Errors during tokenization: {:?}", errors);
        }
        assert!(tokens.is_some());

        tokens.unwrap()
    }

    fn simple_trivium(typ: TriviumType, value: &str) -> Trivium {
        Trivium {
            span: Span::new((), 0..0),
            typ,
            value,
        }
    }

    fn newline_trivium() -> Trivium<'static> {
        simple_trivium(TriviumType::Newline, "\n")
    }

    fn whitespace_trivium(value: &str) -> Trivium {
        simple_trivium(TriviumType::Whitespace, value)
    }

    fn comment_trivium(value: &str) -> Trivium {
        simple_trivium(TriviumType::LineComment, value)
    }

    fn block_comment_trivium(value: &str) -> Trivium {
        simple_trivium(TriviumType::BlockComment, value)
    }

    #[test]
    fn test_basic_trivia() {
        let source = "  x   =    42  \n";
        let token_list = simple_tokenize(source);

        assert_has_token_with_trivia(
            &token_list,
            Token::Ident("x"),
            vec![], // Leading trivia here is interpreted as indent.
            vec![whitespace_trivium("   ")],
        );

        assert_has_token_with_trivia(
            &token_list,
            Token::Symbol("="),
            vec![],
            vec![whitespace_trivium("    ")],
        );

        assert_has_token_with_trivia(
            &token_list,
            Token::Num("42"),
            vec![],
            vec![whitespace_trivium("  "), newline_trivium()],
        );
    }

    #[test]
    fn test_basic_line_comment_trivia() {
        let source = "x = 42  # This is a comment\ny = 10";
        let token_list = simple_tokenize(source);

        assert_has_token_with_trivia(
            &token_list,
            Token::Ident("x"),
            vec![],
            vec![whitespace_trivium(" ")],
        );

        assert_has_token_with_trivia(
            &token_list,
            Token::Num("42"),
            vec![],
            vec![
                whitespace_trivium("  "),
                comment_trivium("# This is a comment"),
                newline_trivium(),
            ],
        );

        assert_has_token_with_trivia(
            &token_list,
            Token::Ident("y"),
            vec![],
            vec![whitespace_trivium(" ")],
        );
    }

    #[test]
    fn test_block_comment_trivia() {
        let source = "x = #- block comment -# 42\n";
        let token_list = simple_tokenize(source);

        assert_has_token_with_trivia(
            &token_list,
            Token::Ident("x"),
            vec![],
            vec![whitespace_trivium(" ")],
        );

        assert_has_token_with_trivia(
            &token_list,
            Token::Symbol("="),
            vec![],
            vec![
                whitespace_trivium(" "),
                block_comment_trivium("#- block comment -#"),
                whitespace_trivium(" "),
            ],
        );

        assert_has_token_with_trivia(
            &token_list,
            Token::Num("42"),
            vec![],
            vec![newline_trivium()],
        );
    }

    #[test]
    fn test_block_comment_trivia_2() {
        let source = r#"#- Header comment -#
x = 42  #- Block comment -#"#;
        let token_list = simple_tokenize(source);

        assert_has_token_with_trivia(
            &token_list,
            Token::Ident("x"),
            vec![
                block_comment_trivium("#- Header comment -#"),
                newline_trivium(),
            ],
            vec![whitespace_trivium(" ")],
        );

        assert_has_token_with_trivia(
            &token_list,
            Token::Num("42"),
            vec![],
            vec![
                whitespace_trivium("  "),
                block_comment_trivium("#- Block comment -#"),
            ],
        );
    }

    #[test]
    fn test_basic_mixed_trivia() {
        let source = "  #- Start comment -#\n  x = 42";
        let token_list = simple_tokenize(source);

        assert_has_token_with_trivia(
            &token_list,
            Token::Ident("x"),
            vec![
                block_comment_trivium("#- Start comment -#"),
                newline_trivium(),
            ],
            vec![simple_trivium(TriviumType::Whitespace, " ")],
        );
    }

    #[test]
    fn test_indentation_trivia() {
        let source = "if condition:\n    x  =  42";
        let token_list = simple_tokenize(source);

        assert_has_token_with_trivia(&token_list, Token::Ident("condition"), vec![], vec![]);
        assert_has_token_with_trivia(
            &token_list,
            Token::Symbol(":"),
            vec![],
            vec![newline_trivium()],
        );

        assert_has_token_with_trivia(
            &token_list,
            Token::Ident("x"),
            vec![],
            vec![simple_trivium(TriviumType::Whitespace, "  ")],
        );

        assert_has_token_with_trivia(
            &token_list,
            Token::Symbol("="),
            vec![],
            vec![simple_trivium(TriviumType::Whitespace, "  ")],
        );
    }

    #[test]
    fn test_trailing_comment_trivia() {
        let source = "if condition #-test-#:\n    x  =  42";
        let token_list = simple_tokenize(source);

        assert_has_token_with_trivia(
            &token_list,
            Token::Ident("condition"),
            vec![],
            vec![whitespace_trivium(" "), block_comment_trivium("#-test-#")],
        );

        assert_has_token_with_trivia(
            &token_list,
            Token::Symbol(":"),
            vec![],
            vec![newline_trivium()],
        );
    }

    #[test]
    fn test_trailing_comment_trivia_2() {
        let source = "if condition: # test \n    x  =  42";
        let token_list = simple_tokenize(source);

        assert_has_token_with_trivia(
            &token_list,
            Token::Symbol(":"),
            vec![],
            vec![
                whitespace_trivium(" "),
                comment_trivium("# test "),
                newline_trivium(),
            ],
        );
    }

    #[test]
    fn test_consecutive_comments() {
        let source = r#"
#- Comment 1 -#
#- Comment 2 -#
#- Comment 3 -#
x = 42
"#;
        let token_list = simple_tokenize(source);

        assert_has_token_with_trivia(
            &token_list,
            Token::Ident("x"),
            vec![
                newline_trivium(),
                block_comment_trivium("#- Comment 1 -#"),
                newline_trivium(),
                block_comment_trivium("#- Comment 2 -#"),
                newline_trivium(),
                block_comment_trivium("#- Comment 3 -#"),
                newline_trivium(),
            ],
            vec![whitespace_trivium(" ")],
        );
    }

    #[test]
    fn test_string_literal_trivia() {
        let source = r#"
#- comment before string -# "hello world" # Comment after string
"#;
        let token_list = simple_tokenize(source);

        assert_has_token_with_trivia(
            &token_list,
            Token::Str("hello world".into()),
            vec![
                newline_trivium(),
                block_comment_trivium("#- comment before string -#"),
                whitespace_trivium(" "),
            ],
            vec![
                whitespace_trivium(" "),
                comment_trivium("# Comment after string"),
                newline_trivium(),
            ],
        );
    }

    #[test]
    fn test_fstring_literal_trivia() {
        let source = r#"
#- comment before string -# f"hello world" # Comment after string
"#;
        let token_list = simple_tokenize(source);

        assert_has_token_with_trivia(
            &token_list,
            Token::FstrBegin("hello world".into()),
            vec![
                newline_trivium(),
                block_comment_trivium("#- comment before string -#"),
                whitespace_trivium(" "),
            ],
            vec![
                whitespace_trivium(" "),
                comment_trivium("# Comment after string"),
                newline_trivium(),
            ],
        );
    }

    #[test]
    fn test_block_end_comment_trivia() {
        let source = r#"
if condition:
    # before
    x = 1 # after
    # end
# after if"#;

        let token_list = simple_tokenize(source);

        assert_has_token_with_trivia(
            &token_list,
            Token::Ident("x"),
            vec![comment_trivium("# before"), newline_trivium()],
            vec![whitespace_trivium(" ")],
        );

        assert_has_token_with_trivia(
            &token_list,
            Token::Num("1"),
            vec![],
            vec![
                whitespace_trivium(" "),
                comment_trivium("# after"),
                newline_trivium(),
                comment_trivium("# end"),
                newline_trivium(),
            ],
        );

        assert_has_token_with_trivia(
            &token_list,
            Token::Dedent,
            vec![],
            vec![comment_trivium("# after if")],
        );
    }

    #[test]
    fn test_empty_lines_trivia() {
        let source = r#"
x
y


z
# end
"#;

        let token_list = simple_tokenize(source);

        assert_has_token_with_trivia(
            &token_list,
            Token::Ident("x"),
            vec![newline_trivium()],
            vec![newline_trivium()],
        );

        assert_has_token_with_trivia(
            &token_list,
            Token::Ident("y"),
            vec![],
            vec![newline_trivium()],
        );

        assert_has_token_with_trivia(
            &token_list,
            Token::Ident("z"),
            vec![newline_trivium(), newline_trivium()],
            vec![
                newline_trivium(),
                comment_trivium("# end"),
                newline_trivium(),
            ],
        );
    }
}
