#![allow(unused_variables, dead_code)]

const FMT_DELIMITER: &str = "%";

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(range: Range<usize>) -> Self {
        Span {
            start: range.start,
            end: range.end,
        }
    }
}

use std::{
    borrow::Cow,
    collections::HashSet,
    fmt::{self},
    ops::Range,
    str::CharIndices,
};

#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum Token<'src> {
    Ident(&'src str),
    None,
    Bool(bool),

    Str(&'src str, String),

    FstrBegin(&'src str),
    FstrEnd(&'src str),
    VerbatimFstrBegin(&'src str),
    VerbatimFstrEnd(&'src str),
    FstrInner(&'src str, String),

    Int(&'src str),
    IntBin(&'src str),
    IntOct(&'src str),
    IntHex(&'src str),

    Float(&'src str),

    Kw(&'src str),

    Symbol(&'src str),

    Indent,
    Dedent,
    Eol,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TrivialTokenType {
    Newline,
    Whitespace,
    LineComment,
    BlockComment,
}

#[derive(Debug, Clone)]
pub struct TrivialToken<'src> {
    pub span: Span,
    pub typ: TrivialTokenType,
    pub value: &'src str,
}

#[derive(Debug, Clone)]
pub struct SToken<'src> {
    pub span: Span,
    pub token: Token<'src>,
    pub leading_trivia: Vec<TrivialToken<'src>>,
    pub trailing_trivia: Vec<TrivialToken<'src>>,
}

impl<'src> SToken<'src> {
    pub fn new(token: Token<'src>, span: Span, leading_trivia: Vec<TrivialToken<'src>>) -> Self {
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
            Token::Int(n) => write!(f, "<literal {n}>"),
            Token::IntBin(n) => write!(f, "<literal 0b{n}>"),
            Token::IntHex(n) => write!(f, "<literal 0x{n}>"),
            Token::IntOct(n) => write!(f, "<literal 0o{n}>"),
            Token::Float(n) => write!(f, "<literal {n}>"),
            Token::Str(_, s) => write!(f, "<literal {s}>"),
            Token::Symbol(s) => write!(f, "{s}"),
            Token::Ident(s) => write!(f, "{s}"),
            Token::Kw(s) => write!(f, "<{s}>"),
            Token::FstrBegin(s) => write!(f, "{s}"),
            Token::FstrEnd(s) => write!(f, "{s}"),
            Token::VerbatimFstrBegin(s) => write!(f, "{s}"),
            Token::VerbatimFstrEnd(s) => write!(f, "{s}"),
            Token::FstrInner(s, _) => write!(f, "{s}"),
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
    FmtExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseBlockEndType {
    Delimiter,
    EolOrEof,
}

#[derive(Debug)]
pub struct LexError<'src> {
    pub span: Span,
    pub message: Cow<'src, str>,
}

impl<'src> LexError<'src> {
    pub fn custom(span: Span, message: impl Into<Cow<'src, str>>) -> Self {
        LexError {
            span,
            message: message.into(),
        }
    }
}

pub type TOutput<'src> = TokenList<'src>;

type TResult<'src, T> = Result<T, LexError<'src>>;

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
            '\'' => escaped_string.push_str("\\\'"),
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

struct TokenizeCtx<'src> {
    input: &'src str,
    iterator: CharIndices<'src>,
    peeked: Option<(usize, char)>,

    keywords: HashSet<String>,
    keep_trivia: bool,
    unassigned_trivia: Vec<TrivialToken<'src>>,
}

impl<'src> TokenizeCtx<'src> {
    fn new(input: &'src str) -> Self {
        static KEYWORDS: &[&str] = &[
            "if", "then", "else", "import", "export", "as", "class", "while", "for", "in", "break",
            "continue", "with", "yield", "global", "return", "raise", "try", "except", "finally",
            "and", "or", "not", "await", "let", "const", "with",
        ];

        let keywords = HashSet::<String>::from_iter(KEYWORDS.iter().map(|s| s.to_string()));
        let mut it = input.char_indices();
        let next = it.next();

        TokenizeCtx {
            input,
            iterator: it,
            peeked: next,

            keywords,
            keep_trivia: false,
            unassigned_trivia: Vec::new(),
        }
    }

    fn cursor(&self) -> usize {
        match self.peeked {
            Some((pos, _)) => pos,
            None => self.input.len(),
        }
    }

    fn save(&self) -> usize {
        self.cursor()
    }

    fn rewind(&mut self, x: usize) {
        self.iterator = self.input[x..].char_indices();
        self.peeked = self.iterator.next();
        // Adjust the position to be absolute, not relative to the slice
        if let Some((offset, ch)) = self.peeked {
            self.peeked = Some((x + offset, ch));
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.peeked.map(|(_, c)| c)
    }

    fn next(&mut self) -> Option<char> {
        let v = self.peek();
        let old_pos = self.cursor();
        self.peeked = self.iterator.next();

        // Adjust the position to be absolute, not relative to the slice
        if let Some((offset, ch)) = self.peeked {
            // Calculate the absolute position based on the previous position
            let new_pos = if let Some(consumed_char) = v {
                old_pos + consumed_char.len_utf8()
            } else {
                old_pos
            };
            // The offset from CharIndices should align with our calculated position
            self.peeked = Some((new_pos, ch));
        }

        v
    }

    fn span_since(&mut self, start: &usize) -> Span {
        Span {
            start: *start,
            end: self.cursor(),
        }
    }

    fn slice_since(&mut self, start: &usize) -> &'src str {
        &self.input[*start..self.cursor()]
    }

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

    fn look_ahead<TOut>(
        &mut self,
        parse_fn: impl FnOnce(&mut Self) -> TResult<'src, TOut>,
    ) -> TResult<'src, TOut> {
        let start = self.save();

        match parse_fn(self) {
            Ok(r) => {
                self.rewind(start);
                Ok(r)
            }
            Err(e) => {
                self.rewind(start);
                Err(e)
            }
        }
    }

    fn parse_indentation(&mut self) -> TResult<'src, (usize, Span, Vec<TrivialToken<'src>>)> {
        let start = self.cursor();
        let mut indent_level: usize = 0;

        if self.peek().is_none() {
            return Err(LexError::custom(
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
            return Err(LexError::custom(
                self.span_since(&start),
                "expected identifier",
            ));
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
            "//", "%%", "@@", "**", "??", ".=", "::", "||", "&&", "^^", ">>", "<<",
        ];
        const MONOGRAMS: &str = "[](){}<>.,;:!?@$%^&*+-=|\\/`~";

        let saved = self.save();
        let start = self.cursor();
        for _ in 0..3 {
            self.next();
        }

        let sl = self.slice_since(&start);
        for polygram in POLYGRAMS {
            if sl.starts_with(polygram) {
                self.rewind(saved);
                for _ in 0..polygram.len() {
                    self.next();
                }
                return Ok((Token::Symbol(polygram), self.span_since(&start)));
            }
        }
        for monogram in MONOGRAMS.chars() {
            if sl.starts_with(monogram) {
                self.rewind(saved);
                self.next();
                let symbol = self.slice_since(&start);
                return Ok((Token::Symbol(symbol), self.span_since(&start)));
            }
        }

        Err(LexError::custom(
            self.span_since(&start),
            "expected a symbol",
        ))
    }

    fn parse_number(&mut self) -> TResult<'src, (Token<'src>, Span, bool)> {
        let start = self.cursor();

        let c = self.peek();

        if c.is_none_or(|c| !(c.is_ascii_digit() || c == '.')) {
            return Err(LexError::custom(
                self.span_since(&start),
                "expected a number",
            ));
        }

        // Handle different number formats
        if self.peek() == Some('0') {
            self.next(); // consume '0'

            // Check for binary, octal, or hex prefix
            match self.peek() {
                Some('b') | Some('B') => {
                    self.next();
                    return self.parse_binary_number(start);
                }
                Some('o') | Some('O') => {
                    self.next();
                    return self.parse_octal_number(start);
                }
                Some('x') | Some('X') => {
                    self.next();
                    return self.parse_hex_number(start);
                }
                Some('.') => {
                    // Handle 0. or 0.123
                    if self.look_ahead(|x| x.parse_seq("..")).is_ok() {
                        // This is 0 followed by .., so just return integer 0
                        let span = self.span_since(&start);
                        let num_str = self.slice_since(&start);
                        return Ok((Token::Int(num_str), span, false));
                    } else {
                        // This is a float starting with 0.
                        return self.parse_decimal_float(start, true);
                    }
                }
                Some(c) if c.is_ascii_digit() || c == '_' => {
                    // Continue parsing as decimal integer or float
                    return self.parse_decimal_number(start, true);
                }
                Some('e') | Some('E') => {
                    // Handle 0e5 format
                    return self.parse_decimal_float(start, true);
                }
                _ => {
                    // Just "0" followed by something else, return integer 0
                    let span = self.span_since(&start);
                    let num_str = self.slice_since(&start);
                    return Ok((Token::Int(num_str), span, false));
                }
            }
        } else if self.peek() == Some('.') {
            // Handle .2 format
            return self.parse_decimal_float(start, false);
        } else {
            return self.parse_decimal_number(start, false);
        }
    }

    fn parse_binary_number(&mut self, start: usize) -> TResult<'src, (Token<'src>, Span, bool)> {
        let mut has_digits = false;

        while let Some(c) = self.peek() {
            if c == '0' || c == '1' {
                has_digits = true;
                self.next();
            } else if c == '_' {
                self.next();
            } else {
                break;
            }
        }

        if !has_digits {
            return Err(LexError::custom(
                self.span_since(&start),
                "expected at least one binary digit after 0b",
            ));
        }

        let span = self.span_since(&start);
        let num_str = self.slice_since(&start);
        Ok((Token::IntBin(num_str), span, false))
    }

    fn parse_octal_number(&mut self, start: usize) -> TResult<'src, (Token<'src>, Span, bool)> {
        let mut has_digits = false;

        while let Some(c) = self.peek() {
            if c.is_ascii_digit() && c <= '7' {
                has_digits = true;
                self.next();
            } else if c == '_' {
                self.next(); // Allow underscores for readability
            } else {
                break;
            }
        }

        if !has_digits {
            return Err(LexError::custom(
                self.span_since(&start),
                "expected at least one octal digit after 0o",
            ));
        }

        let span = self.span_since(&start);
        let num_str = self.slice_since(&start);
        Ok((Token::IntOct(num_str), span, false))
    }

    fn parse_hex_number(&mut self, start: usize) -> TResult<'src, (Token<'src>, Span, bool)> {
        let mut has_digits = false;

        while let Some(c) = self.peek() {
            if c.is_ascii_hexdigit() {
                has_digits = true;
                self.next();
            } else if c == '_' {
                self.next();
            } else {
                break;
            }
        }

        if !has_digits {
            return Err(LexError::custom(
                self.span_since(&start),
                "expected at least one hex digit after 0x",
            ));
        }

        let span = self.span_since(&start);
        let num_str = self.slice_since(&start);
        Ok((Token::IntHex(num_str), span, false))
    }

    fn parse_decimal_number(
        &mut self,
        start: usize,
        has_leading_zero: bool,
    ) -> TResult<'src, (Token<'src>, Span, bool)> {
        let mut has_digits = has_leading_zero;

        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                has_digits = true;
                self.next();
            } else if c == '_' {
                self.next();
            } else {
                break;
            }
        }

        if self.peek() == Some('.') && !self.look_ahead(|x| x.parse_seq("..")).is_ok() {
            return self.parse_decimal_float(start, has_digits);
        } else if matches!(self.peek(), Some('e') | Some('E')) {
            return self.parse_decimal_float(start, has_digits);
        }

        if !has_digits {
            return Err(LexError::custom(
                self.span_since(&start),
                "expected at least one digit",
            ));
        }

        let span = self.span_since(&start);
        let num_str = self.slice_since(&start);
        Ok((Token::Int(num_str), span, false))
    }

    fn parse_decimal_float(
        &mut self,
        start: usize,
        has_integer_part: bool,
    ) -> TResult<'src, (Token<'src>, Span, bool)> {
        let mut has_fractional_digits = false;

        // Parse decimal point and fractional part
        if self.peek() == Some('.') && !self.look_ahead(|x| x.parse_seq("..")).is_ok() {
            self.next();

            // Parse fractional digits
            while let Some(c) = self.peek() {
                if c.is_ascii_digit() {
                    has_fractional_digits = true;
                    self.next();
                } else if c == '_' {
                    self.next(); // Allow underscores for readability
                } else {
                    break;
                }
            }
        }

        // Must have digits either before or after decimal point
        if !has_integer_part && !has_fractional_digits {
            return Err(LexError::custom(
                self.span_since(&start),
                "expected digits in float literal",
            ));
        }

        if matches!(self.peek(), Some('e') | Some('E')) {
            self.next(); // consume 'e' or 'E'

            if matches!(self.peek(), Some('+') | Some('-')) {
                self.next();
            }

            let mut has_exp_digits = false;
            while let Some(c) = self.peek() {
                if c.is_ascii_digit() {
                    has_exp_digits = true;
                    self.next();
                } else if c == '_' {
                    self.next();
                } else {
                    break;
                }
            }

            if !has_exp_digits {
                return Err(LexError::custom(
                    self.span_since(&start),
                    "expected digits in exponent",
                ));
            }
        }

        let span = self.span_since(&start);
        let num_str = self.slice_since(&start);
        Ok((Token::Float(num_str), span, true))
    }

    fn parse_newline(&mut self) -> TResult<'src, TrivialToken<'src>> {
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
            return Err(LexError::custom(
                self.span_since(&start),
                "expected newline",
            ));
        } else {
            Ok(TrivialToken {
                span: self.span_since(&start),
                typ: TrivialTokenType::Newline,
                value: self.slice_since(&start),
            })
        }
    }

    fn parse_newline_or_eof(&mut self) -> TResult<'src, Vec<TrivialToken<'src>>> {
        if self.peek().is_none() {
            return Ok(vec![]);
        }

        if let Ok(tr) = self.try_parse(TokenizeCtx::parse_newline) {
            if self.keep_trivia {
                return Ok(vec![tr]);
            }
            return Ok(vec![]);
        }

        return Err(LexError::custom(
            self.span_since(&self.cursor()),
            "expected newline or end of file",
        ));
    }

    fn parse_whitespace(&mut self) -> TResult<'src, TrivialToken<'src>> {
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
            return Err(LexError::custom(
                self.span_since(&start),
                "expected whitespace",
            ));
        }

        Ok(TrivialToken {
            span: self.span_since(&start),
            typ: TrivialTokenType::Whitespace,
            value: self.slice_since(&start),
        })
    }

    fn parse_empty_line(&mut self) -> TResult<'src, Vec<TrivialToken<'src>>> {
        if self.peek().is_none() {
            return Err(LexError::custom(
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

    fn parse_line_comment(&mut self) -> TResult<'src, TrivialToken<'src>> {
        let start = self.cursor();

        if self.next() != Some('#') {
            return Err(LexError::custom(
                self.span_since(&start),
                "expected line comment",
            ));
        }

        while self.look_ahead(TokenizeCtx::parse_newline_or_eof).is_err() {
            self.next();
        }

        Ok(TrivialToken {
            span: self.span_since(&start),
            typ: TrivialTokenType::LineComment,
            value: self.slice_since(&start),
        })
    }

    fn parse_block_comment(&mut self) -> TResult<'src, TrivialToken<'src>> {
        let start = self.cursor();

        self.parse_seq("#-")?;

        while let Some(_) = self.peek() {
            if self.try_parse(|ctx| ctx.parse_seq("-#")).is_ok() {
                return Ok(TrivialToken {
                    span: self.span_since(&start),
                    typ: TrivialTokenType::BlockComment,
                    value: self.slice_since(&start),
                });
            }

            // nesting
            if self.try_parse(|ctx| ctx.parse_block_comment()).is_ok() {
                continue;
            }

            self.next();
        }

        return Err(LexError::custom(
            self.span_since(&start),
            "unterminated block comment",
        ));
    }

    fn parse_trivia(&mut self) -> TResult<'src, Vec<TrivialToken<'src>>> {
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
            return Err(LexError::custom(
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
                return Err(LexError::custom(
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

                Err(LexError::custom(
                    self.span_since(&start),
                    "unterminated escape",
                ))
            }
            Some(c) => Ok(c),
            None => Err(LexError::custom(
                self.span_since(&start),
                "unterminated string",
            )),
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
        let mut ending_marker = self.save();
        loop {
            if !verbatim && self.try_parse(|x| x.parse_newline()).is_ok() {
                return Err(LexError::custom(
                    self.span_since(&marker),
                    "unterminated fstring",
                ));
            }

            let Some(peeked) = self.peek() else {
                return Err(LexError::custom(
                    self.span_since(&marker),
                    "unterminated fstring",
                ));
            };

            let mut save_marker = false;

            if peeked == ending_char {
                seen_quotes += 1;

                if seen_quotes == ending_char_count {
                    let span = self.span_since(&marker);
                    tokens.push(SToken::new(
                        Token::FstrInner(self.slice_since(&marker), current_str),
                        span,
                        Vec::new(),
                    ));

                    self.rewind(ending_marker);

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
                return Err(LexError::custom(self.span_since(&marker), "unexpected '}'"));
            } else if self.try_parse(|x| x.parse_char('{')).is_ok() {
                let span = self.span_since(&marker);
                let trivia = self.parse_trivia()?;

                let mut new_token = SToken::new(
                    Token::FstrInner(self.slice_since(&marker), current_str),
                    span,
                    Vec::new(),
                );

                new_token.trailing_trivia = trivia;
                tokens.push(new_token);
                current_str = String::new();

                let (inner_tokens, _, inner_span) =
                    self.try_parse(|x| x.parse_block(0, ParseBlockMode::FmtExpr))?;

                marker = self.cursor();
                let mut format_tokens = vec![];

                if self.try_parse(|x| x.parse_seq(FMT_DELIMITER)).is_ok() {
                    format_tokens.push(SToken::new(
                        Token::Symbol(FMT_DELIMITER),
                        self.span_since(&marker),
                        Vec::new(),
                    ));
                    format_tokens.extend(self.parse_fstr_inner('}', 1, verbatim)?);
                }

                tokens.extend(inner_tokens.0);
                tokens.extend(format_tokens);

                marker = self.cursor();
                self.parse_seq("}")?;
            } else {
                let next_char = if verbatim {
                    self.next().ok_or_else(|| {
                        LexError::custom(self.span_since(&marker), "unterminated verbatim fstring")
                    })?
                } else {
                    self.parse_escaped_char()?
                };

                if save_marker {
                    current_str.push(next_char);
                }
            }

            if save_marker {
                ending_marker = self.save();
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
            return Ok((Token::Str(self.slice_since(&start), String::new()), span));
        }

        verbatim |= quote_count >= 3;

        let mut seen_quotes = 0;
        let mut s = String::new();
        loop {
            if !verbatim && self.try_parse(|x| x.parse_newline()).is_ok() {
                return Err(LexError::custom(
                    self.span_since(&start),
                    "unterminated string",
                ));
            }

            let Some(peeked) = self.peek() else {
                return Err(LexError::custom(
                    self.span_since(&start),
                    "unterminated string",
                ));
            };

            let mut push_next = false;
            if peeked == ch {
                seen_quotes += 1;

                if seen_quotes == quote_count {
                    self.next();
                    let span = self.span_since(&start);
                    return Ok((Token::Str(self.slice_since(&start), s), span));
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

        let after_rf = self.cursor();

        while self.try_parse(|x| x.parse_char(ch)).is_ok() {
            quote_count += 1;
        }

        if quote_count == 2 {
            let start_rng = start..after_rf + 1;
            let end_rng = after_rf + 1..after_rf + 2;

            return Ok(TokenList(vec![
                SToken::new(
                    if verbatim {
                        Token::VerbatimFstrBegin(&self.input[start_rng.clone()])
                    } else {
                        Token::FstrBegin(&self.input[start_rng.clone()])
                    },
                    Span::new(start_rng.clone()),
                    Vec::new(),
                ),
                SToken::new(
                    Token::FstrInner("", "".into()),
                    Span::new(start_rng.end..start_rng.end),
                    Vec::new(),
                ),
                SToken::new(
                    if verbatim {
                        Token::VerbatimFstrEnd(&self.input[end_rng.clone()])
                    } else {
                        Token::FstrEnd(&self.input[end_rng.clone()])
                    },
                    Span::new(end_rng),
                    Vec::new(),
                ),
            ]));
        }

        verbatim |= quote_count >= 3;

        let mut tokens = vec![];
        tokens.push(SToken::new(
            if verbatim {
                Token::VerbatimFstrBegin(self.slice_since(&start))
            } else {
                Token::FstrBegin(self.slice_since(&start))
            },
            self.span_since(&start),
            Vec::new(),
        ));
        tokens.extend(self.parse_fstr_inner(ch, quote_count, verbatim)?);

        let start = self.cursor();

        let mut close_quote_count = 0;
        while self.try_parse(|x| x.parse_char(ch)).is_ok() {
            close_quote_count += 1;
        }

        if quote_count != close_quote_count {
            return Err(LexError::custom(
                self.span_since(&start),
                format!(
                    "expected {} closing quotes, found {}",
                    quote_count, close_quote_count
                ),
            ));
        }

        tokens.push(SToken::new(
            if verbatim {
                Token::VerbatimFstrEnd(self.slice_since(&start))
            } else {
                Token::FstrEnd(self.slice_since(&start))
            },
            self.span_since(&start),
            Vec::new(),
        ));

        Ok(TokenList(tokens))
    }

    fn parse_str_start(&mut self) -> TResult<'src, ()> {
        let start = self.cursor();

        let _ = self.try_parse(|x| x.parse_ident_or_token());
        if self.try_parse(|ctx| ctx.parse_char('"')).is_ok() {
            return Ok(());
        }
        if self.try_parse(|ctx| ctx.parse_char('\'')).is_ok() {
            return Ok(());
        }

        Err(LexError::custom(
            self.span_since(&start),
            "expected string start",
        ))
    }

    fn parse_str(&mut self) -> TResult<'src, TokenList<'src>> {
        // ' variants

        if self.look_ahead(|x| x.parse_seq("rf\'")).is_ok() {
            let tokens = self.parse_fstr('\'')?;
            return Ok(tokens);
        }

        if self.look_ahead(|x| x.parse_seq("f\'")).is_ok() {
            let tokens = self.parse_fstr('\'')?;
            return Ok(tokens);
        }

        if self.look_ahead(|x| x.parse_seq("r\'")).is_ok() {
            let (token, span) = self.parse_regular_str('\'')?;
            return Ok(TokenList(vec![SToken::new(token, span, Vec::new())]));
        }

        if self.look_ahead(|x| x.parse_seq("\'")).is_ok() {
            let (token, span) = self.parse_regular_str('\'')?;
            return Ok(TokenList(vec![SToken::new(token, span, Vec::new())]));
        }

        // " variants

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

        Err(LexError::custom(
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
        let mut cur_block_unassigned_trivia = vec![];

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
                    Span {
                        start: new_block_span.start,
                        end: new_block_span.start,
                    },
                    Vec::new(),
                ));
                line_tokens.extend(new_block_tokens.0);
                line_tokens.push(SToken::new(
                    Token::Dedent,
                    Span {
                        start: new_block_span.end,
                        end: new_block_span.end,
                    },
                    Vec::new(),
                ));

                if new_block_break_type == ParseBlockEndType::EolOrEof {
                    continue 'lines;
                }
            } else {
                // prune empty lines
                cur_block_unassigned_trivia.extend(std::mem::take(&mut self.unassigned_trivia));

                while let Ok(empty_line_trivia) =
                    self.try_parse(|ctx| TokenizeCtx::parse_empty_line(ctx))
                {
                    cur_block_unassigned_trivia.extend(empty_line_trivia);
                }

                let before_indentation = self.save();

                let Ok((line_indent_level, line_indent_span, line_indent_trivia)) =
                    self.try_parse(|x| x.parse_indentation())
                else {
                    if block_indent.is_none() {
                        return Err(LexError::custom(
                            self.span_since(&self.cursor()),
                            "expected an indented block",
                        ));
                    }

                    break ParseBlockEndType::EolOrEof;
                };

                cur_block_unassigned_trivia.extend(line_indent_trivia);

                if let Some(block_indent) = block_indent {
                    if line_indent_level > block_indent {
                        self.rewind(before_indentation);

                        // handle continuation
                        let (mut continuation_tokens, continuation_break_type, _span) = self
                            .try_parse(|x| {
                                x.parse_block(block_indent, ParseBlockMode::Continuation)
                            })?;

                        if let Some(first_token) = continuation_tokens.0.first_mut() {
                            let mut t = std::mem::take(&mut cur_block_unassigned_trivia);
                            t.extend(first_token.leading_trivia.drain(..));
                            first_token.leading_trivia = t;

                            line_tokens.extend(continuation_tokens.0);
                        }

                        if continuation_break_type == ParseBlockEndType::EolOrEof {
                            continue 'lines;
                        }
                    } else if line_indent_level < block_indent {
                        self.rewind(before_indentation);

                        break ParseBlockEndType::EolOrEof;
                    } else if !line_tokens.is_empty() {
                        tokens.extend(line_tokens.drain(..));

                        if mode != ParseBlockMode::Continuation && mode != ParseBlockMode::FmtExpr {
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
                        ParseBlockMode::FmtExpr => {}
                        ParseBlockMode::NewBlock => {
                            if line_indent_level <= current_indent {
                                return Err(LexError::custom(
                                    line_indent_span,
                                    "expected an indented block",
                                ));
                            }
                        }
                        ParseBlockMode::Continuation => {
                            if line_indent_level <= current_indent {
                                return Err(LexError::custom(
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
                    let saved = self.save();

                    if let Ok((token, span, after_dot)) = self.try_parse(|ctx| ctx.parse_number()) {
                        if after_dot && self.peek() == Some('.') {
                            return Err(LexError::custom(
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

                    if let Token::Symbol(FMT_DELIMITER) = &tok.token {
                        // Format specifier delimiter; end block.
                        self.rewind(saved);
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
                                    return Err(LexError::custom(
                                        self.span_since(&start_curs),
                                        format!(
                                            "unmatched delimiter '{}', found '{}'",
                                            open_delim_char, s
                                        ),
                                    ));
                                }
                            } else {
                                self.rewind(saved);
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
                return Err(LexError::custom(
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

            if mode != ParseBlockMode::Continuation && mode != ParseBlockMode::FmtExpr {
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
            Span {
                start: start,
                end: last_block_token.span.end,
            }
        } else {
            Span {
                start: start,
                end: start,
            }
        };

        if delim_stack.len() > 0 {
            let (_unmatched_char, span) = delim_stack.pop().unwrap();
            return Err(LexError::custom(span, "unmatched delimiter"));
        }

        let len = tokens.len();
        for i in (0..len).rev() {
            // avoid putting trivia on an eol token
            if matches!(tokens[i].token, Token::Eol) {
                continue;
            }

            tokens[i]
                .trailing_trivia
                .extend(cur_block_unassigned_trivia.drain(..));

            break;
        }

        self.unassigned_trivia
            .extend(cur_block_unassigned_trivia.drain(..));

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

pub fn tokenize<'src>(
    s: &'src str,
    keep_trivia: bool,
) -> (Option<TokenList<'src>>, Vec<LexError<'src>>) {
    let mut ctx = TokenizeCtx::new(s);
    if keep_trivia {
        ctx.keep_trivia = true;
    }

    let tokens = ctx.tokenize_input();

    match tokens {
        Ok(tokens) => (Some(tokens), vec![]),
        Err(err) => (None, vec![err]),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn match_trivia(expected: &Vec<TrivialToken>, actual: &Vec<TrivialToken>) -> bool {
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
        leading_trivia: Vec<TrivialToken>,
        trailing_trivia: Vec<TrivialToken>,
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

    fn simple_trivium(typ: TrivialTokenType, value: &str) -> TrivialToken {
        TrivialToken {
            span: Span { start: 0, end: 0 },
            typ,
            value,
        }
    }

    fn newline_trivium() -> TrivialToken<'static> {
        simple_trivium(TrivialTokenType::Newline, "\n")
    }

    fn whitespace_trivium(value: &str) -> TrivialToken {
        simple_trivium(TrivialTokenType::Whitespace, value)
    }

    fn comment_trivium(value: &str) -> TrivialToken {
        simple_trivium(TrivialTokenType::LineComment, value)
    }

    fn block_comment_trivium(value: &str) -> TrivialToken {
        simple_trivium(TrivialTokenType::BlockComment, value)
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
            Token::Int("42"),
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
            Token::Int("42"),
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
            Token::Int("42"),
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
            Token::Int("42"),
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
            vec![simple_trivium(TrivialTokenType::Whitespace, " ")],
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
            vec![simple_trivium(TrivialTokenType::Whitespace, "  ")],
        );

        assert_has_token_with_trivia(
            &token_list,
            Token::Symbol("="),
            vec![],
            vec![simple_trivium(TrivialTokenType::Whitespace, "  ")],
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
            Token::Str("\"hello world\"", "hello world".into()),
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
            Token::FstrBegin("f\""),
            vec![
                newline_trivium(),
                block_comment_trivium("#- comment before string -#"),
                whitespace_trivium(" "),
            ],
            vec![],
        );

        assert_has_token_with_trivia(
            &token_list,
            Token::FstrEnd("\""),
            vec![],
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
            Token::Int("1"),
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

    #[test]
    fn test_number_literals() {
        // Test various number literal formats
        let test_cases = vec![
            ("42", Token::Int("42")),
            ("0", Token::Int("0")),
            ("0b1010", Token::IntBin("0b1010")),
            ("0B1010", Token::IntBin("0B1010")),
            ("0b01_010", Token::IntBin("0b01_010")),
            ("0o123", Token::IntOct("0o123")),
            ("0O123", Token::IntOct("0O123")),
            ("0x123f", Token::IntHex("0x123f")),
            ("0X123F", Token::IntHex("0X123F")),
            ("0xDEAD_BEEF", Token::IntHex("0xDEAD_BEEF")),
            ("1.34", Token::Float("1.34")),
            (".2", Token::Float(".2")),
            ("5.", Token::Float("5.")),
            ("5.e-5", Token::Float("5.e-5")),
            ("1.23e10", Token::Float("1.23e10")),
            ("1e5", Token::Float("1e5")),
            ("1E-5", Token::Float("1E-5")),
            ("2.5e+3", Token::Float("2.5e+3")),
            ("123_456", Token::Int("123_456")),
            ("12.34_56", Token::Float("12.34_56")),
        ];

        for (source, expected_token) in test_cases {
            let token_list = simple_tokenize(source);

            // Find the expected token in the list
            let found = token_list
                .0
                .iter()
                .any(|stoken| stoken.token == expected_token);
            assert!(
                found,
                "Expected token {:?} not found in source '{}'",
                expected_token, source
            );
        }
    }

    #[test]
    fn test_zero_followed_by_range() {
        // Test that "0.." is parsed as "0" followed by ".."
        let source = "0..5";
        let token_list = simple_tokenize(source);

        // Should have tokens: Int("0"), Symbol(".."), Int("5")
        assert!(token_list.0.len() >= 3);
        assert_eq!(token_list.0[0].token, Token::Int("0"));
        assert_eq!(token_list.0[1].token, Token::Symbol(".."));
        assert_eq!(token_list.0[2].token, Token::Int("5"));
    }
}
