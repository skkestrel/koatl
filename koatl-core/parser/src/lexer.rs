#![allow(unused_variables, dead_code)]

use chumsky::{
    input::{Cursor, InputRef, StrInput},
    prelude::*,
};
use std::{collections::HashSet, fmt};

use crate::ast::{Span, Spanned};

#[derive(Debug, Clone, PartialEq)]
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

    Eol,
}

#[derive(Debug)]
pub struct TokenList<'src>(pub Vec<Spanned<Token<'src>>>);

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
            Token::Eol => write!(f, "<eol>"),
            Token::FstrBegin(s) => write!(f, "<f_begin {s}>"),
            Token::FstrContinue(s) => write!(f, "<f_middle {s}>"),
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

#[derive(Debug, Clone, Default, PartialEq)]
pub enum NewBlockType {
    #[default]
    BeginInput,
    NewBlock,
    Continuation,
}

pub type TOutput<'src> = TokenList<'src>;
pub type TError<'src> = Rich<'src, char, Span>;
pub type TExtra<'src> = extra::Full<TError<'src>, (), ()>;

type TResult<'src, T> = Result<T, TError<'src>>;

pub fn escape_str(s: &str) -> String {
    // TODO
    s.chars()
        .map(|c| match c {
            '\n' => "\\n".to_string(),
            '"' => "\\\"".to_string(),
            _ => c.to_string(),
        })
        .collect()
}

pub fn escape_fstr(s: &str) -> String {
    // TODO
    s.chars()
        .map(|c| match c {
            '{' => "{{".to_string(),
            '}' => "}}".to_string(),
            '\n' => "\\n".to_string(),
            '"' => "\\\"".to_string(),
            _ => c.to_string(),
        })
        .collect()
}

// TODO don't duplicate this with parser below
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
}

impl<'src: 'parse, 'parse, 'input, TInput> TokenizeCtx<'src, 'parse, 'input, TInput>
where
    TInput: StrInput<'src, Token = char, Span = SimpleSpan, Slice = &'src str>,
{
    fn new(input: &'input mut InputRef<'src, 'parse, TInput, TExtra<'src>>) -> Self {
        static KEYWORDS: &[&str] = &[
            "if", "then", "else", "import", "export", "as", "class", "while", "for", "in", "break",
            "continue", "with", "yield", "global", "nonlocal", "return", "raise", "try", "except",
            "finally", "and", "or", "not", "assert", "def", "lambda",
        ];

        let keywords = HashSet::<String>::from_iter(KEYWORDS.iter().map(|s| s.to_string()));

        TokenizeCtx { input, keywords }
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

    fn parse_indentation(&mut self) -> TResult<'src, Spanned<usize>> {
        let start = self.cursor();
        let mut indent_level = 0;

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

        self.parse_nonsemantic()?;
        Ok((indent_level, self.span_since(&start)))
    }

    fn parse_ident_or_token(&mut self) -> TResult<'src, Spanned<Token<'src>>> {
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
        if self.keywords.contains(ident) {
            return Ok((Token::Kw(ident), self.span_since(&start)));
        }

        if ident == "True" {
            return Ok((Token::Bool(true), self.span_since(&start)));
        } else if ident == "False" {
            return Ok((Token::Bool(false), self.span_since(&start)));
        } else if ident == "None" {
            return Ok((Token::None, self.span_since(&start)));
        }

        Ok((
            Token::Ident(self.slice_since(&start)),
            self.span_since(&start),
        ))
    }

    fn parse_symbol(&mut self) -> TResult<'src, Spanned<Token<'src>>> {
        const POLYGRAMS: &[&str] = &[
            "===", "<=>", "=>", "..", "==", "<>", "<=", ">=", "//", "**", "??", "@@", ".=",
        ];
        const MONOGRAMS: &str = "[]()<>.,;:!?@$%^&*+-=|\\/`~";

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
                return Ok((
                    Token::Symbol(self.slice_since(&start)),
                    self.span_since(&start),
                ));
            }
        }

        Err(Rich::custom(self.span_since(&start), "expected a symbol"))
    }

    fn parse_number(&mut self) -> TResult<'src, (Spanned<Token<'src>>, bool)> {
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

        Ok((
            (
                Token::Num(self.slice_since(&start)),
                self.span_since(&start),
            ),
            after_dot,
        ))
    }

    fn parse_block_comment_begin(&mut self) -> TResult<'src, ()> {
        let start = self.cursor();

        if self.next() != Some('#') || self.next() != Some('-') {
            return Err(Rich::custom(
                self.span_since(&start),
                "expected block comment start",
            ));
        }

        Ok(())
    }

    fn parse_block_comment_end(&mut self) -> TResult<'src, ()> {
        let start = self.cursor();

        if self.next() != Some('-') || self.next() != Some('#') {
            return Err(Rich::custom(
                self.span_since(&start),
                "expected block comment end",
            ));
        }

        Ok(())
    }

    fn parse_block_comment(&mut self) -> TResult<'src, ()> {
        let start = self.cursor();

        self.parse_block_comment_begin()?;

        while let Some(_) = self.peek() {
            if self.try_parse(|x| x.parse_block_comment_end()).is_ok() {
                return Ok(());
            }

            if self
                .look_ahead(TokenizeCtx::parse_block_comment_begin)
                .is_ok()
            {
                self.parse_block_comment()?;
            }

            self.next();
        }

        return Err(Rich::custom(
            self.span_since(&start),
            "unterminated block comment",
        ));
    }

    fn parse_newline(&mut self) -> TResult<'src, ()> {
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
            Ok(())
        }
    }

    fn parse_newline_or_eof(&mut self) -> TResult<'src, ()> {
        if self.peek().is_none() {
            return Ok(());
        }

        if self.try_parse(TokenizeCtx::parse_newline).is_ok() {
            return Ok(());
        }

        return Err(Rich::custom(
            self.span_since(&self.cursor()),
            "expected newline or end of file",
        ));
    }

    fn parse_nonsemantic(&mut self) -> TResult<'src, ()> {
        while let Some(c) = self.peek() {
            if c == ' ' || c == '\t' {
                self.next();
            } else if self
                .look_ahead(TokenizeCtx::parse_block_comment_begin)
                .is_ok()
            {
                self.parse_block_comment()?;
            } else if c == '#' {
                while self.look_ahead(TokenizeCtx::parse_newline_or_eof).is_err() {
                    self.next();
                }
            } else {
                break;
            }
        }

        Ok(())
    }

    fn parse_empty_line(&mut self) -> TResult<'src, ()> {
        if self.peek().is_none() {
            return Err(Rich::custom(
                self.span_since(&self.cursor()),
                "expected empty line, not eof",
            ));
        }

        loop {
            self.parse_nonsemantic()?;

            if self.try_parse(TokenizeCtx::parse_newline_or_eof).is_ok() {
                return Ok(());
            } else {
                return Err(Rich::custom(
                    self.span_since(&self.cursor()),
                    "expected empty line",
                ));
            }
        }
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

    fn parse_regular_str(&mut self) -> TResult<'src, Spanned<Token<'src>>> {
        let start = self.cursor();

        self.parse_seq("\"")?;

        let mut s = String::new();
        loop {
            if self.try_parse(|x| x.parse_seq("\"")).is_ok() {
                return Ok((Token::Str(s), self.span_since(&start)));
            }

            if self.try_parse(|x| x.parse_newline()).is_ok() {
                return Err(Rich::custom(self.span_since(&start), "unterminated string"));
            }

            s.push(self.parse_escaped_char()?);
        }
    }

    fn parse_verbatim_str(&mut self) -> TResult<'src, Spanned<Token<'src>>> {
        let start = self.cursor();

        self.parse_seq("\"\"\"")?;

        let mut s = String::new();
        loop {
            if self.try_parse(|x| x.parse_seq("\"\"\"")).is_ok() {
                return Ok((Token::Str(s), self.span_since(&start)));
            }

            s.push(self.next().ok_or_else(|| {
                Rich::custom(self.span_since(&start), "unterminated verbatim string")
            })?);
        }
    }

    fn parse_fstr(&mut self, verbatim: bool) -> TResult<'src, TokenList<'src>> {
        let mut marker = self.cursor();

        if verbatim {
            self.parse_seq("f\"\"\"")?;
        } else {
            self.parse_seq("f\"")?;
        }

        let mut tokens = vec![];
        let mut current_str = String::new();

        loop {
            if self
                .try_parse(|x| x.parse_seq(if verbatim { "\"\"\"" } else { "\"" }))
                .is_ok()
            {
                if tokens.len() == 0 {
                    tokens.push((Token::FstrBegin(current_str), self.span_since(&marker)));
                } else {
                    tokens.push((Token::FstrContinue(current_str), self.span_since(&marker)));
                }

                return Ok(TokenList(tokens));
            }

            if !verbatim && self.try_parse(|x| x.parse_newline()).is_ok() {
                return Err(Rich::custom(
                    self.span_since(&marker),
                    "unterminated fstring",
                ));
            }

            if self.try_parse(|x| x.parse_seq("{{")).is_ok() {
                current_str.push('{');
                continue;
            }

            if self.try_parse(|x| x.parse_seq("}}")).is_ok() {
                current_str.push('}');
                continue;
            } else if self.try_parse(|x| x.parse_seq("}")).is_ok() {
                return Err(Rich::custom(self.span_since(&marker), "unexpected '}'"));
            }

            if self.try_parse(|x| x.parse_seq("{")).is_ok() {
                if tokens.len() == 0 {
                    tokens.push((Token::FstrBegin(current_str), self.span_since(&marker)));
                } else {
                    tokens.push((Token::FstrContinue(current_str), self.span_since(&marker)));
                }
                current_str = String::new();

                self.parse_nonsemantic()?;
                let _ = self.try_parse(|x| x.parse_newline());

                let (expr, expr_span) =
                    self.try_parse(|x| x.parse_block(0, NewBlockType::BeginInput, false))?;

                self.try_parse(|x| x.parse_seq("}"))?;

                tokens.push((
                    Token::Symbol("BEGIN_BLOCK"),
                    Span::new(expr_span.context, expr_span.start..expr_span.start),
                ));
                tokens.extend(expr.0);
                tokens.push((
                    Token::Eol,
                    Span::new(expr_span.context, expr_span.end..expr_span.end),
                ));
                tokens.push((
                    Token::Symbol("END_BLOCK"),
                    Span::new(expr_span.context, expr_span.end..expr_span.end),
                ));

                marker = self.cursor();

                continue;
            }

            if verbatim {
                current_str.push(self.next().ok_or_else(|| {
                    Rich::custom(self.span_since(&marker), "unterminated verbatim fstring")
                })?);
            } else {
                current_str.push(self.parse_escaped_char()?);
            }
        }
    }

    fn parse_str_start(&mut self) -> TResult<'src, ()> {
        let start = self.cursor();

        if self.try_parse(|x| x.parse_seq("f\"")).is_ok() {
            return Ok(());
        }

        if self.try_parse(|x| x.parse_seq("\"")).is_ok() {
            return Ok(());
        }

        Err(Rich::custom(
            self.span_since(&start),
            "expected string start",
        ))
    }

    fn parse_str(&mut self) -> TResult<'src, TokenList<'src>> {
        if self.look_ahead(|x| x.parse_seq("\"\"\"")).is_ok() {
            let token = self.parse_verbatim_str()?;
            return Ok(TokenList(vec![token]));
        }

        if self.look_ahead(|x| x.parse_seq("\"")).is_ok() {
            let token = self.parse_regular_str()?;
            return Ok(TokenList(vec![token]));
        }

        if self.look_ahead(|x| x.parse_seq("f\"\"\"")).is_ok() {
            let tokens = self.parse_fstr(true)?;
            return Ok(tokens);
        }

        if self.look_ahead(|x| x.parse_seq("f\"")).is_ok() {
            let tokens = self.parse_fstr(false)?;
            return Ok(tokens);
        }

        Err(Rich::custom(
            self.span_since(&self.cursor()),
            "expected string start",
        ))
    }

    fn parse_block(
        &mut self,
        current_indent: usize,
        block_type: NewBlockType,
        close_brace_err: bool,
    ) -> TResult<'src, Spanned<TokenList<'src>>> {
        let mut tokens = vec![];

        // TODO should parse_empty_line be part of parse_indentation
        while self.try_parse(TokenizeCtx::parse_empty_line).is_ok() {}

        let (indent_level, indent_span) =
            self.try_parse(|x| x.parse_indentation()).map_err(|_| {
                Rich::custom(
                    self.span_since(&self.cursor()),
                    "expected indentation at the beginning of parse_block",
                )
            })?;

        match block_type {
            NewBlockType::BeginInput => {}
            NewBlockType::NewBlock => {
                if indent_level <= current_indent {
                    return Err(Rich::custom(indent_span, "expected new block indentation"));
                }
            }
            NewBlockType::Continuation => {
                if indent_level <= current_indent {
                    return Err(Rich::custom(indent_span, "expected continuation"));
                }
            }
        }

        loop {
            let mut has_token = false;
            let mut expect_new_block = false;

            loop {
                if self.look_ahead(TokenizeCtx::parse_str_start).is_ok() {
                    let toks = self.parse_str()?;
                    tokens.extend(toks.0);

                    has_token = true;
                    expect_new_block = false;
                } else {
                    let tok;

                    if let Ok((token, after_dot)) = self.try_parse(TokenizeCtx::parse_number) {
                        if after_dot && self.peek() == Some('.') {
                            return Err(Rich::custom(
                                self.span_since(&self.cursor()),
                                "unexpected '.'",
                            ));
                        }

                        tok = token;
                    } else if let Ok(token) = self.try_parse(TokenizeCtx::parse_ident_or_token) {
                        tok = token;
                    } else if let Ok(token) = self.try_parse(TokenizeCtx::parse_symbol) {
                        tok = token;
                    } else {
                        break;
                    }

                    expect_new_block = tok.0 == Token::Symbol("=>")
                        || tok.0 == Token::Symbol(":")
                        || tok.0 == Token::Symbol("{")
                        || tok.0 == Token::Symbol("[")
                        || tok.0 == Token::Symbol("(");

                    tokens.push(tok);
                    has_token = true;
                }

                self.parse_nonsemantic()?;
            }

            if self.peek().is_none() {
                // reached eof
                break;
            }

            if self.peek() == Some('}') {
                if close_brace_err {
                    return Err(Rich::custom(
                        self.span_since(&self.cursor()),
                        "unexpected '}' outside fstr",
                    ));
                } else {
                    break;
                }
            }

            if self.try_parse(|x| x.parse_newline()).is_err() {
                return Err(Rich::custom(
                    self.span_since(&self.cursor()),
                    format!("unexpected '{}'", self.peek().unwrap()),
                ));
            }

            if has_token {
                if expect_new_block {
                    let new_block = self.try_parse(|x| {
                        x.parse_block(indent_level, NewBlockType::NewBlock, close_brace_err)
                    });

                    if let Ok((new_block, new_block_span)) = new_block {
                        tokens.push((
                            Token::Symbol("BEGIN_BLOCK"),
                            Span::new(
                                new_block_span.context,
                                new_block_span.start..new_block_span.start,
                            ),
                        ));
                        tokens.extend(new_block.0);

                        let end_span = Span::new(
                            new_block_span.context,
                            new_block_span.end..new_block_span.end,
                        );
                        tokens.push((Token::Eol, end_span));
                        tokens.push((Token::Symbol("END_BLOCK"), end_span));
                    } else if let Err(e) = new_block {
                        while self.try_parse(TokenizeCtx::parse_newline_or_eof).is_err() {
                            self.next();
                        }

                        return Err(e);
                    }
                }
            }

            while self.try_parse(TokenizeCtx::parse_empty_line).is_ok() {}

            if let Ok((cur_indent_level, cur_indent_span)) =
                self.look_ahead(|x| x.parse_indentation())
            {
                if cur_indent_level > indent_level {
                    // handle continuation

                    let (new_block, new_block_span) = self.try_parse(|x| {
                        x.parse_block(indent_level, NewBlockType::Continuation, close_brace_err)
                    })?;

                    tokens.extend(new_block.0);
                }
            }

            if let Ok((cur_indent_level, cur_indent_span)) =
                self.look_ahead(|x| x.parse_indentation())
            {
                if cur_indent_level < indent_level {
                    break;
                }

                self.parse_indentation()?;

                if block_type != NewBlockType::Continuation {
                    tokens.push((Token::Eol, self.span_since(&self.cursor())));
                }
            } else {
                break;
            }
        }

        let Some(last_block_token) = tokens.last() else {
            return Err(Rich::custom(indent_span, "empty block"));
        };

        let block_span = Span::new(
            indent_span.context,
            indent_span.start..last_block_token.1.end,
        );

        Ok((TokenList(tokens), block_span))
    }

    fn tokenize_input(&mut self) -> TResult<'src, TokenList<'src>> {
        let (mut tokens, span) = self.parse_block(0, NewBlockType::BeginInput, true)?;
        tokens.0.push((Token::Eol, self.span_since(&self.cursor())));
        Ok(tokens)
    }
}

fn lexer<'src, TInput>() -> impl Parser<'src, TInput, TOutput<'src>, TExtra<'src>>
where
    TInput: StrInput<'src, Token = char, Span = SimpleSpan, Slice = &'src str>,
{
    custom(|input| {
        let mut ctx = TokenizeCtx::new(input);
        ctx.tokenize_input()
    })
}

pub fn tokenize<'src>(s: &'src str) -> (Option<TokenList<'src>>, Vec<TError<'src>>) {
    let output = lexer()
        .parse(s.map_span(|s| Span::new(s.context, s.start()..s.end())))
        .into_output_errors();

    output
}
