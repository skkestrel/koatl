use std::hash::Hash;

use koatl_parser::lexer::Span;

pub struct LineColCache {
    line_starts: Vec<usize>,
}

impl LineColCache {
    pub fn new(source: &str) -> Self {
        let mut line_starts = vec![0];

        for (i, c) in source.char_indices() {
            if c == '\n' {
                line_starts.push(i + 1);
            }
        }

        line_starts.push(usize::MAX);

        Self { line_starts }
    }

    pub fn linecol(&self, cursor: usize) -> (usize, usize) {
        for (i, pos) in self.line_starts.iter().enumerate() {
            if *pos > cursor {
                return (i, cursor - self.line_starts[i - 1]);
            }
        }
        panic!();
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RefHash {
    id: usize,
}

impl<T> From<&T> for RefHash {
    fn from(value: &T) -> Self {
        RefHash {
            id: (value as *const T) as usize,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub enum TlErrKind {
    Tokenize,
    Parse,
    Transform,
    Emit,

    #[default]
    Unknown,
}

#[derive(Debug, Clone)]
pub struct TlErr {
    pub kind: TlErrKind,
    pub message: String,
    pub span: Option<Span>,
    pub contexts: Vec<(String, Span)>,
}

pub type TlResult<T> = Result<T, TlErrs>;

#[derive(Debug, Clone)]
pub struct TlErrs(pub Vec<TlErr>);

impl TlErrs {
    pub fn new() -> Self {
        TlErrs(vec![])
    }

    pub fn extend(&mut self, other: TlErrs) {
        self.0.extend(other.0);
    }
}

impl From<TlErr> for TlErrs {
    fn from(err: TlErr) -> Self {
        TlErrs(vec![err])
    }
}

impl From<Vec<TlErr>> for TlErrs {
    fn from(errs: Vec<TlErr>) -> Self {
        TlErrs(errs)
    }
}

pub struct TlErrBuilder {
    message: String,
    span: Option<Span>,
    contexts: Vec<(String, Span)>,
    kind: TlErrKind,
}

impl TlErrBuilder {
    pub fn new() -> Self {
        TlErrBuilder {
            message: String::new(),
            span: None,
            contexts: vec![],
            kind: TlErrKind::Unknown,
        }
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn message<S: Into<String>>(mut self, message: S) -> Self {
        self.message = message.into();
        self
    }

    pub fn build_one(self) -> TlErr {
        TlErr {
            kind: self.kind,
            message: self.message,
            span: self.span,
            contexts: self.contexts,
        }
    }

    pub fn context(mut self, message: impl Into<String>, span: Span) -> Self {
        self.contexts.push((message.into(), span));
        self
    }

    pub fn build(self) -> TlErrs {
        TlErrs(vec![self.build_one()])
    }
}
