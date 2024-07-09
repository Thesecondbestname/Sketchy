use ariadne::{Color, ReportKind, Source};

use crate::convenience_types::Span;
use std::{fmt::Display, io::Write};

/// An enum representing a diagnostic level.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[non_exhaustive]
pub enum Level {
    /// An error.
    Error,
    /// A warning.
    Warning,
    /// A note.
    Note,
    /// A help message.
    Help,
    /// Some debug information.
    Debug,
}

impl Display for Level {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Level::Error => write!(f, "Error"),
            Level::Warning => write!(f, "Warning"),
            Level::Note => write!(f, "Note"),
            Level::Help => write!(f, "Help"),
            Level::Debug => write!(f, "Debug"),
        }
    }
}
use core::fmt;

type Label = &'static str;

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Pattern {
    /// A specific token was expected.
    Token(crate::Token),
    /// A labelled pattern was expected.
    Label(Label),
    /// The end of input was expected.
    EndOfInput,
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Token(token) => write!(f, "{token}"),
            Self::Label(label) => f.write_str(label),
            Self::EndOfInput => f.write_str("EndOfInput"),
        }
    }
}

impl fmt::Debug for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Token(token) => write!(f, "{token:?}"),
            Self::Label(label) => f.write_str(label),
            Self::EndOfInput => f.write_str("EndOfInput"),
        }
    }
}

// TODO: Maybe should make ExpectedFound encapsulated a bit more
/// The reason for a [`Rich`] error.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Reason {
    /// An unexpected input was found
    ExpectedFound {
        /// The tokens expected
        expected: Vec<Pattern>,
        /// The tokens found
        found: Option<crate::Token>,
    },
    // Additional type for help
    ExpectedFoundHelp {
        /// The tokens expected
        expected: Vec<Pattern>,
        /// The tokens found
        found: Option<String>,
        /// The optional help Text
        help: String,
    },
    /// An error with a custom message
    Custom(Diagnostic),
}

impl Reason {
    /// Return the token that was found by this error reason. `None` implies that the end of input was expected.
    pub fn found(&self) -> Option<crate::Token> {
        match self {
            Self::ExpectedFound { found, .. } => found.clone(),
            Self::Custom(_) | Self::ExpectedFoundHelp { .. } => None,
        }
    }

    pub fn expected_found(expected: Vec<Pattern>, found: Option<crate::Token>) -> Self {
        Reason::ExpectedFound { expected, found }
    }
    pub fn take_found(&mut self) -> Option<crate::Token> {
        match self {
            Reason::ExpectedFound { found, .. } => found.take(),
            Reason::Custom(_) | Reason::ExpectedFoundHelp { .. } => None,
        }
    }

    #[inline]
    pub fn flat_merge(self, other: Self) -> Self {
        match (self, other) {
            (
                Reason::ExpectedFound {
                    expected: mut this_expected,
                    found,
                },
                Reason::ExpectedFound {
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
                Reason::ExpectedFound {
                    expected: this_expected,
                    found,
                }
            }
            (Reason::Custom(_this), Reason::Custom(_other)) => todo!(),
            (this @ Reason::Custom(_), _) => this,
            (_, other @ Reason::Custom(_)) => other,
            a => panic!("[INTERNAL ERROR] had to fuse errors {a:?}"),
        }
    }
}

/// A structure representing a diagnostic message and associated children messages.
#[must_use]
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Diagnostic {
    pub level: Level,
    pub message: String,
    pub help: Option<String>,
    pub span: Option<Span>,
    pub children: Vec<SubDiagnostic>,
}

impl Diagnostic {
    pub fn new(level: Level, message: impl Into<String>) -> Self {
        Self {
            level,
            message: message.into(),
            span: None,
            children: vec![],
            help: None,
        }
    }

    pub fn spanned(span: Span, level: Level, message: impl Into<String>) -> Self {
        Diagnostic {
            level,
            message: message.into(),
            span: Some(span),
            children: vec![],
            help: None,
        }
    }
    pub fn with_help(mut self, help: String) -> Self {
        self.help = Some(help);
        self
    }
    pub fn with_child(
        mut self,
        spans: impl MultiSpan,
        level: Level,
        message: impl Into<String>,
    ) -> Self {
        self.children.push(SubDiagnostic {
            level,
            message: message.into(),
            spans: spans.into_vec(),
        });
        self
    }
    pub fn emit<T: Write>(&self, mut out: T, name: &str, inp: &str) {
        type Report<'a> = ariadne::Report<'a, (&'a str, core::ops::Range<usize>)>;
        type Informations<'b, 'a> = (
            Vec<ariadne::Label<(&'b str, std::ops::Range<usize>)>>,
            Vec<&'a String>,
            Vec<&'a String>,
        );

        fn map_children<'a, 'b>(
            children: &'a [SubDiagnostic],
            source: &'b str,
        ) -> Informations<'b, 'a> {
            let mut labels = vec![];
            let mut notes = vec![];
            let mut helps = vec![];

            for child in children {
                if child.spans.is_empty() {
                    match child.level {
                        Level::Note | Level::Debug => notes.push(&child.message),
                        Level::Help => helps.push(&child.message),
                        Level::Error | Level::Warning => todo!(),
                    }
                    notes.push(&child.message);
                };
                labels.extend(child.spans.iter().map(|span| {
                    ariadne::Label::new((source, (*span).into()))
                        .with_message(&child.message)
                        .with_color(level_to_color(child.level))
                }));
            }

            (labels, notes, helps)
        }
        if self.span.is_none() && self.level == Level::Debug {
            Report::build(
                level_to_kind(self.level),
                name,
                self.span.unwrap_or_else(|| Span::new(0, 0)).start,
            )
            .with_message(&self.message)
            .finish()
            .eprint((name, Source::from(inp)))
            .expect("Falied to build report!");
            for child in &self.children {
                write!(out.by_ref(), "{}", child.message).unwrap();
            }
            writeln!(out.by_ref()).unwrap();
            return;
        };

        let builder = ariadne::Report::build(
            level_to_kind(self.level),
            name,
            self.span.unwrap_or_else(|| Span::new(0, 0)).start,
        )
        .with_message(&self.message);
        let mut builder = if let Some(help) = self.help.clone() {
            builder.with_help(help)
        } else {
            builder
        };

        if let Some(span) = self.span {
            if self.children.is_empty() {
                builder.add_label(
                    ariadne::Label::new((name, span.into()))
                        .with_color(level_to_color(self.level))
                        .with_message(&self.message),
                );
            }
        } else {
            let (labels, notes, helps) = map_children(&self.children, name);
            builder.add_labels(labels);

            // TODO Properly render multiple notes/helps
            if !notes.is_empty() {
                let (start, end) = notes
                    .split_first()
                    .expect("[INTERNALL ERROR] diagnostic has less than 1 note");
                builder.set_note(
                    end.iter()
                        .fold((*start).to_string(), |acc, b| acc + ", " + b),
                );
            }
            if !helps.is_empty() {
                let (start, end) = notes
                    .split_first()
                    .expect("[INTERNALL ERROR] diagnostic has less than 1 help");
                builder.set_help(
                    end.iter()
                        .fold((*start).to_string(), |acc, b| acc + ", " + b),
                );
            }
        }
        builder.finish().eprint((name, Source::from(inp))).unwrap();
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SubDiagnostic {
    pub level: Level,
    pub message: String,
    pub spans: Vec<Span>,
}

/// Trait implemented by types that can be converted into a set of `Span`s.
pub trait MultiSpan {
    /// Converts `self` into a `Vec<Span>`.
    fn into_vec(self) -> Vec<Span>;
}

impl MultiSpan for Span {
    fn into_vec(self) -> Vec<Span> {
        vec![self]
    }
}

impl MultiSpan for Vec<Span> {
    fn into_vec(self) -> Vec<Span> {
        self
    }
}

impl MultiSpan for &[Span] {
    fn into_vec(self) -> Vec<Span> {
        self.to_vec()
    }
}
const fn level_to_kind(level: Level) -> ReportKind<'static> {
    match level {
        Level::Error => ReportKind::Error,
        Level::Warning => ReportKind::Warning,
        Level::Note | Level::Help => ReportKind::Advice,
        Level::Debug => ReportKind::Custom("Debug", Color::Cyan),
    }
}

const fn level_to_color(level: Level) -> Color {
    match level {
        Level::Error => Color::Red,
        Level::Warning => Color::Yellow,
        Level::Note | Level::Debug | Level::Help => Color::White,
    }
}
