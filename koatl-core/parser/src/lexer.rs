#![allow(unused_variables, dead_code)]

use chumsky::{
    input::{Cursor, InputRef, StrInput},
    label::LabelError,
    prelude::*,
};
use std::{
    collections::HashSet,
    fmt::{self},
};

use crate::ast::{Span, Spannable, Spanned};

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

    Indent,
    Dedent,
    Eol,
}

#[derive(Debug)]
pub struct TokenList<'src>(pub Vec<Spanned<Token<'src>>>);

impl<'src> IntoIterator for TokenList<'src> {
    type Item = Spanned<Token<'src>>;
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

impl<'src> std::iter::FromIterator<Spanned<Token<'src>>> for TokenList<'src> {
    fn from_iter<T: IntoIterator<Item = Spanned<Token<'src>>>>(iter: T) -> Self {
        TokenList(iter.into_iter().collect())
    }
}

impl fmt::Display for TokenList<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, stoken) in self.0.iter().enumerate() {
            write!(f, "{}", stoken.value)?;
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

pub fn py_escape_str(s: &str) -> String {
    let mut escaped_string = String::new();

    // Iterate over each character in the input string.
    for c in s.chars() {
        match c {
            // Handle common escape sequences
            '\n' => escaped_string.push_str("\\n"),
            '\r' => escaped_string.push_str("\\r"),
            '\t' => escaped_string.push_str("\\t"),
            '\"' => escaped_string.push_str("\\\""),
            '\\' => escaped_string.push_str("\\\\"),

            // Handle additional escape sequences
            '\0' => escaped_string.push_str("\\0"), // Null character
            '\x08' => escaped_string.push_str("\\b"), // Backspace
            '\x0c' => escaped_string.push_str("\\f"), // Form feed

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
    // TODO
    py_escape_str(s)
        .chars()
        .map(|c| match c {
            '{' => "{{".to_string(),
            '}' => "}}".to_string(),
            _ => c.to_string(),
        })
        .collect()
}

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
            "continue", "with", "yield", "global", "return", "raise", "try", "except", "finally",
            "and", "or", "not", "await", "let", "const", "with",
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

        self.parse_nonsemantic()?;
        Ok(indent_level.spanned(self.span_since(&start)))
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
            return Ok(Token::Kw(ident).spanned(self.span_since(&start)));
        }

        if ident == "True" {
            return Ok(Token::Bool(true).spanned(self.span_since(&start)));
        } else if ident == "False" {
            return Ok(Token::Bool(false).spanned(self.span_since(&start)));
        } else if ident == "None" {
            return Ok(Token::None.spanned(self.span_since(&start)));
        }

        Ok(Token::Ident(self.slice_since(&start)).spanned(self.span_since(&start)))
    }

    fn parse_symbol(&mut self) -> TResult<'src, Spanned<Token<'src>>> {
        const POLYGRAMS: &[&str] = &[
            "+=", "-=", "*=", "/=", "|=", "??=", "===", "<=>", "=>", "..", "==", "<>", "<=", ">=",
            "//", "**", "??", ".=", "::",
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
                return Ok(Token::Symbol(polygram).spanned(self.span_since(&start)));
            }
        }
        for monogram in MONOGRAMS.chars() {
            if sl.starts_with(monogram) {
                self.input.rewind(saved);
                self.next();
                return Ok(Token::Symbol(self.slice_since(&start)).spanned(self.span_since(&start)));
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
            Token::Num(self.slice_since(&start)).spanned(self.span_since(&start)),
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
                return Ok(Token::Str(s).spanned(self.span_since(&start)));
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
                return Ok(Token::Str(s).spanned(self.span_since(&start)));
            }

            s.push(self.next().ok_or_else(|| {
                Rich::custom(self.span_since(&start), "unterminated verbatim string")
            })?);
        }
    }

    fn parse_fstr_inner(&mut self, ending: &str, verbatim: bool) -> TResult<'src, TokenList<'src>> {
        let mut marker = self.cursor();
        let mut tokens = vec![];
        let mut current_str = String::new();

        loop {
            if self.look_ahead(|x| x.parse_seq(ending)).is_ok() {
                if tokens.len() == 0 {
                    tokens.push(Token::FstrBegin(current_str).spanned(self.span_since(&marker)));
                } else {
                    tokens.push(Token::FstrContinue(current_str).spanned(self.span_since(&marker)));
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
                    tokens.push(Token::FstrBegin(current_str).spanned(self.span_since(&marker)));
                } else {
                    tokens.push(Token::FstrContinue(current_str).spanned(self.span_since(&marker)));
                }
                current_str = String::new();

                self.parse_nonsemantic()?;
                let _ = self.try_parse(|x| x.parse_newline());
                let sexpr = self.try_parse(|x| x.parse_block(0, NewBlockType::BeginInput))?;
                let _ = self.try_parse(|x| x.parse_newline());

                self.parse_indentation()?;

                marker = self.cursor();
                let mut format_tokens = vec![];
                if self.try_parse(|x| x.parse_seq("!")).is_ok() {
                    format_tokens.push(Token::Symbol("!").spanned(self.span_since(&marker)));
                    format_tokens.extend(self.parse_fstr_inner("}", verbatim)?);
                }

                self.parse_seq("}")?;

                tokens.push(Token::Indent.spanned(Span::new(
                    sexpr.span.context,
                    sexpr.span.start..sexpr.span.start,
                )));
                tokens.extend(sexpr.value);
                tokens.push(Token::Eol.spanned(Span::new(
                    sexpr.span.context,
                    sexpr.span.end..sexpr.span.end,
                )));
                tokens.push(Token::Dedent.spanned(Span::new(
                    sexpr.span.context,
                    sexpr.span.end..sexpr.span.end,
                )));
                tokens.extend(format_tokens);

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

    fn parse_fstr(&mut self, verbatim: bool) -> TResult<'src, TokenList<'src>> {
        if verbatim {
            self.parse_seq("f\"\"\"")?;
            let inner = self.parse_fstr_inner("\"\"\"", true)?;
            self.parse_seq("\"\"\"")?;
            return Ok(inner);
        } else {
            self.parse_seq("f\"")?;
            let inner = self.parse_fstr_inner("\"", false)?;
            self.parse_seq("\"")?;
            return Ok(inner);
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
    ) -> TResult<'src, Spanned<TokenList<'src>>> {
        let mut tokens = vec![];

        // TODO should parse_empty_line be part of parse_indentation?
        while self.try_parse(TokenizeCtx::parse_empty_line).is_ok() {}

        let indent_level = self.try_parse(|x| x.parse_indentation()).map_err(|_| {
            Rich::custom(
                self.span_since(&self.cursor()),
                "expected indentation at the beginning of parse_block",
            )
        })?;

        match block_type {
            NewBlockType::BeginInput => {}
            NewBlockType::NewBlock => {
                if indent_level.value <= current_indent {
                    return Err(Rich::custom(
                        indent_level.span,
                        "expected new block indentation",
                    ));
                }
            }
            NewBlockType::Continuation => {
                if indent_level.value <= current_indent {
                    return Err(Rich::custom(indent_level.span, "expected continuation"));
                }
            }
        }

        const OPEN_DELIMS: &[char] = &['{', '[', '('];
        const CLOSE_DELIMS: &[char] = &['}', ']', ')'];
        let mut delim_stack = vec![];

        loop {
            // over lines
            let mut expect_new_block = false;
            let mut end_block = false;

            loop {
                // over tokens in line
                if self.look_ahead(TokenizeCtx::parse_str_start).is_ok() {
                    let toks = self.parse_str()?;
                    tokens.extend(toks.0);
                } else {
                    let tok;

                    let start_curs = self.cursor();
                    let saved = self.input.save();

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

                    if let Token::Symbol("!") = &tok.value {
                        // Format specifier delimiter; end block.
                        self.input.rewind(saved);
                        end_block = true;
                        break;
                    }

                    if let Token::Symbol(s) = &tok.value {
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
                                // end the block - eol is emitted by caller
                                self.input.rewind(saved);
                                end_block = true;
                                break;
                            }
                        }
                    }

                    tokens.push(tok);
                }

                expect_new_block = false;

                if let Some(last_token) = tokens.last() {
                    if let Token::Symbol(s) = last_token.value {
                        if s == "=>"
                            || s == ":"
                            || OPEN_DELIMS.contains(&s.chars().next().unwrap_or('\0'))
                        {
                            expect_new_block = true;
                        }
                    }
                }

                self.parse_nonsemantic()?;
            }

            if end_block || self.peek().is_none() {
                break;
            }

            let before_newline = self.input.save();
            let eol_span = self.span_since(&self.cursor());

            if self.try_parse(|x| x.parse_newline()).is_err() {
                // make sure that there aren't any unexpected characters
                return Err(Rich::custom(
                    self.span_since(&self.cursor()),
                    format!("unexpected '{}'", self.peek().unwrap()),
                ));
            }

            if expect_new_block {
                let new_block =
                    self.try_parse(|x| x.parse_block(indent_level.value, NewBlockType::NewBlock));

                if let Ok(new_block) = new_block {
                    tokens.push(Token::Indent.spanned(Span::new(
                        new_block.span.context,
                        new_block.span.start..new_block.span.start,
                    )));
                    tokens.extend(new_block.value);

                    let end_span = Span::new(
                        new_block.span.context,
                        new_block.span.end..new_block.span.end,
                    );

                    // don't push another eol if the last token is already eol (edge case fix)
                    if tokens.last().is_none_or(|t| t.value != Token::Eol) {
                        tokens.push(Token::Eol.spanned(end_span));
                    }

                    tokens.push(Token::Dedent.spanned(end_span));

                    // continue in order to catch trailing characters after the block
                    continue;
                } else if let Err(e) = new_block {
                    while self.try_parse(TokenizeCtx::parse_newline_or_eof).is_err() {
                        self.next();
                    }

                    return Err(e);
                }
            }

            while self.try_parse(TokenizeCtx::parse_empty_line).is_ok() {}

            if let Ok(cur_indent) = self.look_ahead(|x| x.parse_indentation()) {
                if cur_indent.value > indent_level.value {
                    // handle continuation
                    let new_block = self.try_parse(|x| {
                        x.parse_block(indent_level.value, NewBlockType::Continuation)
                    })?;

                    tokens.extend(new_block.value);

                    continue;
                } else if cur_indent.value < indent_level.value {
                    // end of the block - rewind
                    self.input.rewind(before_newline);
                    break;
                }

                self.parse_indentation()?;

                if block_type != NewBlockType::Continuation {
                    // don't push another eol if the last token is already eol (edge case fix)
                    if tokens.last().is_none_or(|t| t.value != Token::Eol) {
                        tokens.push(Token::Eol.spanned(eol_span));
                    }
                }
            } else {
                // couldn't parse indentation - end of file?
                break;
            }
        }

        // empty block is possible with some weird continuation layouts:
        /*
         * x = [
         *      1
         *      2
         *   ] then ...
         *
         *   ^ this is a continuation that immediately ends due to ]
         */
        let block_span = if let Some(last_block_token) = tokens.last() {
            Span::new(
                indent_level.span.context,
                indent_level.span.start..last_block_token.span.end,
            )
        } else {
            Span::new(
                indent_level.span.context,
                indent_level.span.start..indent_level.span.end,
            )
        };

        Ok(TokenList(tokens).spanned(block_span))
    }

    fn tokenize_input(&mut self) -> TResult<'src, TokenList<'src>> {
        while self.try_parse(TokenizeCtx::parse_empty_line).is_ok() {}

        if let None = self.peek() {
            return Ok(TokenList(vec![]));
        }

        let mut tokens = TokenList(vec![]);
        tokens
            .0
            .push(Token::Indent.spanned(self.span_since(&self.cursor())));

        tokens
            .0
            .extend(self.parse_block(0, NewBlockType::BeginInput)?.value);

        tokens
            .0
            .push(Token::Eol.spanned(self.span_since(&self.cursor())));

        tokens
            .0
            .push(Token::Dedent.spanned(self.span_since(&self.cursor())));

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
