use std::{borrow::Cow, fmt};

use chumsky::{DefaultExpected, error::Error, input::Input, label::LabelError, util::MaybeRef};

use crate::{Token, lexer::SToken, lexer::Span};

#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum TriviaRichPattern<'a, 'src> {
    /// A specific token.
    Token(MaybeRef<'a, Token<'src>>),
    /// A labelled pattern.
    Label(Cow<'a, str>),
    /// A specific keyword.
    Identifier(String),
    /// Anything other than the end of input.
    Any,
    /// Something other than the provided input.
    SomethingElse,
    /// The end of input.
    EndOfInput,
}

impl<'a, 'src> From<DefaultExpected<'a, SToken<'src>>> for TriviaRichPattern<'a, 'src> {
    fn from(expected: DefaultExpected<'a, SToken<'src>>) -> Self {
        match expected {
            DefaultExpected::Token(tok) => Self::Token(MaybeRef::Val(tok.token.clone())),
            DefaultExpected::Any => Self::Any,
            DefaultExpected::SomethingElse => Self::SomethingElse,
            DefaultExpected::EndOfInput => Self::EndOfInput,
            _ => Self::Any,
        }
    }
}

impl<'a, 'src> From<MaybeRef<'a, Token<'src>>> for TriviaRichPattern<'a, 'src> {
    fn from(tok: MaybeRef<'a, Token<'src>>) -> Self {
        Self::Token(tok)
    }
}

impl From<&'static str> for TriviaRichPattern<'_, '_> {
    fn from(label: &'static str) -> Self {
        Self::Label(Cow::Borrowed(label))
    }
}

impl From<String> for TriviaRichPattern<'_, '_> {
    fn from(label: String) -> Self {
        Self::Label(Cow::Owned(label))
    }
}

impl<'a, 'src> TriviaRichPattern<'a, 'src> {
    fn write(
        &self,
        f: &mut fmt::Formatter,
        mut fmt_token: impl FnMut(&Token<'a>, &mut fmt::Formatter<'_>) -> fmt::Result,
    ) -> fmt::Result {
        match self {
            Self::Token(tok) => {
                write!(f, "'")?;
                fmt_token(tok, f)?;
                write!(f, "'")
            }
            Self::Label(l) => write!(f, "{l}"),
            Self::Identifier(i) => write!(f, "'{i}'"),
            Self::Any => write!(f, "any"),
            Self::SomethingElse => write!(f, "something else"),
            Self::EndOfInput => write!(f, "end of input"),
        }
    }
}

impl fmt::Debug for TriviaRichPattern<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.write(f, |t, f| write!(f, "{t:?}"))
    }
}

impl fmt::Display for TriviaRichPattern<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.write(f, |t, f| write!(f, "'{t}'"))
    }
}

#[derive(Debug, Clone, Hash)]
pub enum TriviaRichReason<'a, 'src> {
    /// An unexpected input was found
    ExpectedFound {
        /// The tokens expected
        expected: Vec<TriviaRichPattern<'a, 'src>>,
        /// The tokens found
        found: Option<MaybeRef<'a, Token<'src>>>,
    },
    /// An error with a custom message
    Custom(String),
}

impl<'a, 'src> TriviaRichReason<'a, 'src> {
    /// Return the token that was found by this error reason. `None` implies that the end of input was expected.
    pub fn found(&self) -> Option<&Token<'a>> {
        match self {
            Self::ExpectedFound { found, .. } => found.as_deref(),
            Self::Custom(_) => None,
        }
    }

    fn take_found(&mut self) -> Option<MaybeRef<'a, Token<'src>>> {
        match self {
            TriviaRichReason::ExpectedFound { found, .. } => found.take(),
            TriviaRichReason::Custom(_) => None,
        }
    }

    fn inner_fmt<S>(
        &self,
        f: &mut fmt::Formatter<'_>,
        mut fmt_token: impl FnMut(&Token<'a>, &mut fmt::Formatter<'_>) -> fmt::Result,
        mut fmt_span: impl FnMut(&S, &mut fmt::Formatter<'_>) -> fmt::Result,
        span: Option<&S>,
        context: &[(TriviaRichPattern<'a, 'src>, S)],
    ) -> fmt::Result {
        match self {
            TriviaRichReason::ExpectedFound { expected, found } => {
                write!(f, "found ")?;
                write_token(f, &mut fmt_token, found.as_deref())?;
                if let Some(span) = span {
                    write!(f, " at ")?;
                    fmt_span(span, f)?;
                }
                write!(f, " expected ")?;
                match &expected[..] {
                    [] => write!(f, "something else")?,
                    [expected] => expected.write(f, &mut fmt_token)?,
                    _ => {
                        for expected in &expected[..expected.len() - 1] {
                            expected.write(f, &mut fmt_token)?;
                            write!(f, ", ")?;
                        }
                        write!(f, "or ")?;
                        expected.last().unwrap().write(f, &mut fmt_token)?;
                    }
                }
            }
            TriviaRichReason::Custom(msg) => {
                write!(f, "{msg}")?;
                if let Some(span) = span {
                    write!(f, " at ")?;
                    fmt_span(span, f)?;
                }
            }
        }
        for (l, s) in context {
            write!(f, " in ")?;
            l.write(f, &mut fmt_token)?;
            write!(f, " at ")?;
            fmt_span(s, f)?;
        }
        Ok(())
    }
}

impl TriviaRichReason<'_, '_> {
    #[inline]
    fn flat_merge(self, other: Self) -> Self {
        match (self, other) {
            // Prefer first error, if ambiguous
            (a @ TriviaRichReason::Custom(_), _) => a,
            (_, b @ TriviaRichReason::Custom(_)) => b,
            (
                TriviaRichReason::ExpectedFound {
                    expected: mut this_expected,
                    found,
                },
                TriviaRichReason::ExpectedFound {
                    expected: mut other_expected,
                    ..
                },
            ) => {
                // Try to avoid allocations if we possibly can by using the longer vector
                if other_expected.len() > this_expected.len() {
                    core::mem::swap(&mut this_expected, &mut other_expected);
                }
                for expected in other_expected {
                    if !this_expected[..].contains(&expected) {
                        this_expected.push(expected);
                    }
                }
                TriviaRichReason::ExpectedFound {
                    expected: this_expected,
                    found,
                }
            }
        }
    }
}

impl fmt::Display for TriviaRichReason<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner_fmt(f, Token::fmt, |_: &(), _| Ok(()), None, &[])
    }
}

#[derive(Clone, Hash)]
pub struct TriviaRich<'a, 'src, S = Span> {
    span: S,
    reason: Box<TriviaRichReason<'a, 'src>>,
    context: Vec<(TriviaRichPattern<'a, 'src>, S)>,
}

impl<'a, 'src, S> TriviaRich<'a, 'src, S> {
    fn inner_fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
        fmt_token: impl FnMut(&Token<'a>, &mut fmt::Formatter<'_>) -> fmt::Result,
        fmt_span: impl FnMut(&S, &mut fmt::Formatter<'_>) -> fmt::Result,
        with_spans: bool,
    ) -> fmt::Result {
        self.reason.inner_fmt(
            f,
            fmt_token,
            fmt_span,
            if with_spans { Some(&self.span) } else { None },
            &self.context,
        )
    }
}

impl<'a, 'src, S> TriviaRich<'a, 'src, S> {
    /// Create an error with a custom message and span
    #[inline]
    pub fn custom<M: ToString>(span: S, msg: M) -> Self {
        TriviaRich {
            span,
            reason: Box::new(TriviaRichReason::Custom(msg.to_string())),
            context: Vec::new(),
        }
    }

    /// Get the span associated with this error.
    ///
    /// If the span type is unspecified, it is [`SimpleSpan`].
    pub fn span(&self) -> &S {
        &self.span
    }

    /// Get the reason for this error.
    pub fn reason(&self) -> &TriviaRichReason<'a, 'src> {
        &self.reason
    }

    /// Take the reason from this error.
    pub fn into_reason(self) -> TriviaRichReason<'a, 'src> {
        *self.reason
    }

    /// Get the token found by this error when parsing. `None` implies that the error expected the end of input.
    pub fn found(&self) -> Option<&Token> {
        self.reason.found()
    }

    /// Return an iterator over the labelled contexts of this error, from least general to most.
    ///
    /// 'Context' here means parser patterns that the parser was in the process of parsing when the error occurred. To
    /// add labelled contexts, see [`Parser::labelled`].
    pub fn contexts(&self) -> impl Iterator<Item = (&TriviaRichPattern<'a, 'src>, &S)> {
        self.context.iter().map(|(l, s)| (l, s))
    }

    /// Get an iterator over the expected items associated with this error
    pub fn expected(&self) -> impl ExactSizeIterator<Item = &TriviaRichPattern<'a, 'src>> {
        match &*self.reason {
            TriviaRichReason::ExpectedFound { expected, .. } => expected.iter(),
            TriviaRichReason::Custom(_) => [].iter(),
        }
    }
}

impl<'a, 'src, I: Input<'a, Token = SToken<'src>>> Error<'a, I> for TriviaRich<'a, 'src, I::Span> {
    #[inline]
    fn merge(self, other: Self) -> Self {
        let new_reason = self.reason.flat_merge(*other.reason);
        Self {
            span: self.span,
            reason: Box::new(new_reason),
            context: self.context, // TOOD: Merge contexts
        }
    }
}

impl<'a, 'src, I: Input<'a, Token = SToken<'src>>, L> LabelError<'a, I, L>
    for TriviaRich<'a, 'src, I::Span>
where
    L: Into<TriviaRichPattern<'a, 'src>>,
{
    #[inline]
    fn expected_found<E: IntoIterator<Item = L>>(
        expected: E,
        found: Option<MaybeRef<'a, I::Token>>,
        span: I::Span,
    ) -> Self {
        Self {
            span,
            reason: Box::new(TriviaRichReason::ExpectedFound {
                expected: expected.into_iter().map(|tok| tok.into()).collect(),
                found: found.map(|x| MaybeRef::Val(x.token.clone())),
            }),
            context: Vec::new(),
        }
    }

    #[inline]
    fn merge_expected_found<E: IntoIterator<Item = L>>(
        mut self,
        new_expected: E,
        new_found: Option<MaybeRef<'a, I::Token>>,
        _span: I::Span,
    ) -> Self {
        match &mut *self.reason {
            TriviaRichReason::ExpectedFound { expected, found } => {
                for new_expected in new_expected {
                    let new_expected = new_expected.into();
                    if !expected[..].contains(&new_expected) {
                        expected.push(new_expected);
                    }
                }
                *found = found
                    .take()
                    .or(new_found.map(|x| MaybeRef::Val(x.token.clone()))); //land
            }
            TriviaRichReason::Custom(_) => {}
        }
        // TOOD: Merge contexts
        self
    }

    #[inline]
    fn replace_expected_found<E: IntoIterator<Item = L>>(
        mut self,
        new_expected: E,
        new_found: Option<MaybeRef<'a, I::Token>>,
        span: I::Span,
    ) -> Self {
        self.span = span;
        match &mut *self.reason {
            TriviaRichReason::ExpectedFound { expected, found } => {
                expected.clear();
                expected.extend(new_expected.into_iter().map(|tok| tok.into()));
                *found = new_found.map(|x| MaybeRef::Val(x.token.clone()));
            }
            _ => {
                self.reason = Box::new(TriviaRichReason::ExpectedFound {
                    expected: new_expected.into_iter().map(|tok| tok.into()).collect(),
                    found: new_found.map(|x| MaybeRef::Val(x.token.clone())),
                });
            }
        }
        self.context.clear();
        self
    }

    #[inline]
    fn label_with(&mut self, label: L) {
        // Opportunistically attempt to reuse allocations if we can
        match &mut *self.reason {
            TriviaRichReason::ExpectedFound { expected, found: _ } => {
                expected.clear();
                expected.push(label.into());
            }
            _ => {
                self.reason = Box::new(TriviaRichReason::ExpectedFound {
                    expected: vec![label.into()],
                    found: self.reason.take_found(),
                });
            }
        }
    }

    #[inline]
    fn in_context(&mut self, label: L, span: I::Span) {
        let label = label.into();
        if self.context.iter().all(|(l, _)| l != &label) {
            self.context.push((label, span));
        }
    }
}

impl<S> fmt::Debug for TriviaRich<'_, '_, S>
where
    S: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.inner_fmt(f, Token::fmt, S::fmt, true)
    }
}

impl<S> fmt::Display for TriviaRich<'_, '_, S>
where
    S: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner_fmt(f, Token::fmt, S::fmt, false)
    }
}

fn write_token<T>(
    f: &mut fmt::Formatter,
    mut fmt_token: impl FnMut(&T, &mut fmt::Formatter<'_>) -> fmt::Result,
    tok: Option<&T>,
) -> fmt::Result {
    match tok {
        Some(tok) => {
            write!(f, "'")?;
            fmt_token(tok, f)?;
            write!(f, "'")
        }
        None => write!(f, "end of input"),
    }
}
