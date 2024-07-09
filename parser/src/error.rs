#![allow(clippy::use_self)]
mod diagnostic;

use crate::{
    convenience_types::ParserInput,
    span::{SourceId, Span},
    Token,
};
use chumsky::{
    util::{Maybe, MaybeRef},
    ParseResult,
};
pub use diagnostic::{Diagnostic, Level, Pattern, Reason};

type Label = &'static str;

/// A rich default error type that tracks error spans, expected inputs, and the actual input found at an error site.
#[derive(Clone, Debug)]
pub struct ParseError {
    pub reason: Reason,
    span: Span,
    context: Vec<(Label, Span)>,
}

impl ParseError {
    /// Create an error with a custom message and span
    #[inline]
    pub fn custom<M: ToString>(span: Span, msg: &M) -> Self {
        ParseError {
            span,
            reason: Reason::Custom(Diagnostic::new(Level::Error, msg.to_string())),
            context: Vec::new(),
        }
    }
    #[inline]
    pub fn expected_found(span: Span, expected: Vec<&'static str>, found: Option<Token>) -> Self {
        ParseError {
            reason: Reason::expected_found(
                expected
                    .into_iter()
                    .map(crate::error::Pattern::Label)
                    .collect::<Vec<_>>(),
                found,
            ),
            span,
            context: Vec::new(),
        }
    }
    #[inline]
    #[must_use]
    pub fn expected_found_help(
        span: Span,
        expected: Vec<Pattern>,
        found: Option<String>,
        help: String,
    ) -> Self {
        ParseError {
            reason: Reason::ExpectedFoundHelp {
                expected,
                found,
                help,
            },
            span,
            context: Vec::new(),
        }
    }

    /// Get the span associated with this error.
    #[must_use]
    pub const fn span(&self) -> Span {
        self.span
    }

    /// Get the reason for this error.
    #[must_use]
    pub const fn reason(&self) -> &Reason {
        &self.reason
    }

    /// Get the token found by this error when parsing. `None` implies that the error expected the end of input.
    #[must_use]
    pub fn found(&self) -> Option<Token> {
        self.reason.found()
    }

    /// Return an iterator over the labelled contexts of this error, from least general to most.
    ///
    /// 'Context' here means parser patterns that the parser was in the process of parsing when the error occurred. To
    /// add labelled contexts, see [`Parser::labelled`].
    pub fn contexts(&self) -> impl Iterator<Item = (&Label, &Span)> {
        self.context.iter().map(|(l, s)| (l, s))
    }

    /// Get an iterator over the expected items associated with this error
    #[must_use]
    pub fn expected(&self) -> impl ExactSizeIterator<Item = &Pattern> {
        fn push_expected<'a>(reason: &'a Reason, v: &mut Vec<&'a Pattern>) {
            match reason {
                Reason::Custom(_) => {}
                Reason::ExpectedFoundHelp { expected, .. }
                | Reason::ExpectedFound { expected, .. } => v.extend(expected.iter()),
            }
        }
        let mut v = Vec::new();
        push_expected(&self.reason, &mut v);
        v.into_iter()
    }
}

impl<'a, 'src> chumsky::error::Error<'a, ParserInput<'a, 'src>> for ParseError {
    #[inline]
    fn expected_found<E: IntoIterator<Item = Option<MaybeRef<'a, Token>>>>(
        expected: E,
        found: Option<Maybe<Token, &'a Token>>,
        span: Span,
    ) -> Self {
        Self {
            span,
            reason: Reason::ExpectedFound {
                expected: expected
                    .into_iter()
                    .map(|tok| {
                        tok.map_or(Pattern::EndOfInput, |inner| {
                            Pattern::Token(inner.into_inner())
                        })
                    })
                    .collect(),
                found: found.map(chumsky::util::Maybe::into_inner),
            },
            context: Vec::new(),
        }
    }

    #[inline]
    fn merge(mut self, mut other: Self) -> Self {
        let new_reason = self.reason.flat_merge(other.reason);
        Self {
            span: self.span,
            reason: new_reason,
            // TODO Merging contexts correctly?
            context: {
                self.context.append(&mut other.context);
                self.context.dedup_by_key(|(label, _)| *label);
                self.context
            },
        }
    }

    #[inline]
    fn merge_expected_found<E: IntoIterator<Item = Option<MaybeRef<'a, Token>>>>(
        mut self,
        new_expected: E,
        _found: Option<Maybe<Token, &'a Token>>,
        _span: Span,
    ) -> Self {
        match &mut self.reason {
            Reason::ExpectedFound { expected, found: _ } => {
                for new_expected in new_expected {
                    let new_expected = new_expected.map_or(Pattern::EndOfInput, |inner| {
                        Pattern::Token(inner.into_inner())
                    });
                    if !expected[..].contains(&new_expected) {
                        expected.push(new_expected);
                    }
                }
            }
            Reason::Custom(_) => todo!(),
            Reason::ExpectedFoundHelp { .. } => todo!(),
        }
        // TOOD: Merge contexts
        self
    }

    #[inline]
    fn replace_expected_found<E: IntoIterator<Item = Option<MaybeRef<'a, Token>>>>(
        mut self,
        new_expected: E,
        new_found: Option<MaybeRef<'a, Token>>,
        span: Span,
    ) -> Self {
        self.span = span;
        match &mut self.reason {
            Reason::ExpectedFound { expected, found } => {
                expected.clear();
                expected.extend(new_expected.into_iter().map(|tok| {
                    tok.map_or(Pattern::EndOfInput, |inner| {
                        Pattern::Token(inner.into_inner())
                    })
                }));
                *found = new_found.map(chumsky::util::Maybe::into_inner);
            }
            _ => {
                self.reason = Reason::ExpectedFound {
                    expected: new_expected
                        .into_iter()
                        .map(|tok| {
                            tok.map_or(Pattern::EndOfInput, |inner| {
                                Pattern::Token(inner.into_inner())
                            })
                        })
                        .collect(),
                    found: new_found.map(chumsky::util::Maybe::into_inner),
                };
            }
        }
        self.context.clear();
        self
    }
}

impl<'a, 'src> chumsky::label::LabelError<'a, ParserInput<'a, 'src>, Label> for ParseError {
    #[inline]
    fn label_with(&mut self, label: Label) {
        // Opportunistically attempt to reuse allocations if we can
        match &mut self.reason {
            Reason::ExpectedFound { expected, found: _ } => {
                expected.clear();
                expected.push(Pattern::Label(label));
            }
            _ => {
                self.reason = Reason::ExpectedFound {
                    expected: vec![Pattern::Label(label)],
                    found: self.reason.take_found(),
                };
            }
        }
    }

    #[inline]
    fn in_context(&mut self, label: Label, span: Span) {
        if self.context.iter().all(|(l, _)| l != &label) {
            self.context.push((label, span));
        }
    }
}
pub fn errors_to_diagnostics<T: std::fmt::Debug>(
    parse_result: ParseResult<T, ParseError>,
    src_id: SourceId,
) -> (Option<T>, Vec<Diagnostic>) {
    let error_to_diagostic = |err: ParseError| -> Diagnostic {
        use crate::error::Reason;
        let mut span = err.span().with_id(src_id);
        if span.end < span.start {
            std::mem::swap(&mut span.start, &mut span.end);
        }

        match err.reason {
            Reason::ExpectedFound { expected, found } => match (expected.is_empty(), found.clone())
            {
                (true, _) => report_unexpected(span, found),
                (false, None) => report_expected(span, &expected),
                (false, Some(found)) => report_expected_found(span, &expected, &found),
            },
            Reason::Custom(diagnostic) => diagnostic,
            Reason::ExpectedFoundHelp {
                expected,
                found,
                help,
            } => match (expected.is_empty(), found.clone()) {
                (true, _) => Diagnostic::spanned(
                    span,
                    Level::Error,
                    format!(
                        "Unexpected {}",
                        found.unwrap_or_else(|| "Token found".to_owned())
                    ),
                )
                .with_help(help),
                (false, None) => report_expected(span, &expected).with_help(help),
                (false, Some(found)) => {
                    let patterns = patterns_to_string(&expected);
                    Diagnostic::new(Level::Error, format!("Expected {patterns}, found {found}"))
                        .with_child(span, Level::Error, format!("Expected {patterns}"))
                        .with_help(help)
                }
            },
        }
    };

    let (output, errors) = parse_result.into_output_errors();
    (output, errors.into_iter().map(error_to_diagostic).collect())
}

fn report_expected_found(span: Span, expected: &[Pattern], found: &Token) -> Diagnostic {
    let patterns = patterns_to_string(expected);
    Diagnostic::new(Level::Error, format!("Expected {patterns}, found {found}")).with_child(
        span,
        Level::Error,
        format!("Expected {patterns}"),
    )
}

fn report_expected(span: Span, expected: &[Pattern]) -> Diagnostic {
    let message = format!("Expected {}", patterns_to_string(expected));
    Diagnostic::spanned(span, Level::Error, message)
}

fn report_unexpected(span: Span, found: Option<Token>) -> Diagnostic {
    let message = format!(
        "Unexpected {}",
        found.map_or("end of input".to_owned(), |token| token.to_string())
    );
    Diagnostic::spanned(span, Level::Error, message)
}

fn patterns_to_string(patterns: &[Pattern]) -> String {
    use std::collections::HashSet;

    fn replace_subset(
        super_set: &mut HashSet<&Pattern>,
        search: &'static [Pattern],
        replacement: &'static Pattern,
    ) {
        let search_set: HashSet<&Pattern> = HashSet::from_iter(search);
        if search_set.is_subset(super_set) {
            for pattern in search_set {
                super_set.remove(pattern);
            }
            super_set.insert(replacement);
        }
    }

    fn replace_element(
        haystack: &mut HashSet<&Pattern>,
        needle: &'static Pattern,
        replacement: &'static Pattern,
    ) {
        if haystack.remove(needle) {
            haystack.insert(replacement);
        }
    }
    if patterns.is_empty() {
        return "nothing".into();
    };
    let mut patterns: HashSet<&Pattern> = patterns.iter().collect::<HashSet<_>>();
    replace_element(
        &mut patterns,
        &Pattern::Token(Token::Assign),
        &Pattern::Label("Assignment"),
    );
    replace_subset(
        &mut patterns,
        &[Pattern::Label("Assignment"), Pattern::Label("Function")],
        &Pattern::Label("Top level item"),
    );
    replace_subset(
        &mut patterns,
        &[
            Pattern::Token(Token::Eq),
            Pattern::Token(Token::Neq),
            Pattern::Token(Token::Lt),
            Pattern::Token(Token::Lte),
            Pattern::Token(Token::Gt),
            Pattern::Token(Token::Gte),
        ],
        &Pattern::Label("a comparison operator"),
    );

    replace_subset(
        &mut patterns,
        &[
            Pattern::Token(Token::Slash),
            Pattern::Token(Token::Mul),
            Pattern::Token(Token::Plus),
            Pattern::Token(Token::Minus),
        ],
        &Pattern::Label("an arithmetic operator"),
    );

    replace_subset(
        &mut patterns,
        &[
            Pattern::Label("a comparison operator"),
            Pattern::Label("an arithmetic operator"),
            Pattern::Label("an assignment operator"),
        ],
        &Pattern::Label("an operator"),
    );

    replace_subset(
        &mut patterns,
        &[
            Pattern::Label("literal"),
            Pattern::Token(Token::Lparen),
            Pattern::Token(Token::Lbracket),
        ],
        &Pattern::Label("expression"),
    );

    replace_subset(
        &mut patterns,
        &[
            Pattern::Label("expression"),
            Pattern::Token(Token::Bang),
            Pattern::Token(Token::Minus),
        ],
        &Pattern::Label("expression"),
    );
    let parenthesize = |x: String| {
        if !x.chars().any(char::is_alphanumeric) {
            "\"".to_owned() + &x + "\""
        } else {
            x
        }
    };

    let patterns = patterns.into_iter().collect::<Vec<_>>();
    let (last, start) = patterns.split_last().unwrap();
    format!(
        "{}{}{}",
        start
            .iter()
            .map(std::string::ToString::to_string)
            .map(parenthesize)
            .collect::<Vec<String>>()
            .join(", "),
        if start.is_empty() { "" } else { " or " },
        parenthesize(last.to_string())
    )
}
