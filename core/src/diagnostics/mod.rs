//! One diagnostic type for every stage.
//!
//! The lexer, parser, compiler and VM each used to report problems in their own
//! shape — one of them by stuffing the message into a token's text field — so
//! nothing composed and no tool could ask for "everything wrong with this file".
//! They all build this type now, and rendering lives in exactly one place.
//!
//! The structure is what makes a good error possible rather than just a nicer
//! string:
//!
//! * a **primary** label says where the problem is;
//! * **secondary** labels say what it conflicts with ("declared immutable
//!   here") — the single largest readability win, and impossible while spans
//!   were points;
//! * **notes** explain *why* the rule exists;
//! * **helps** say *what to do*, and a help carrying [`Edit`]s is a concrete
//!   replacement an editor can apply as a quick fix.

pub mod registry;
pub mod render;

use crate::token::Span;

/// A stable identifier for a class of problem, e.g. `E0003`.
///
/// Codes — not message text — are the interface. Tests assert on them,
/// suppression keys on them, and `oxigen explain` looks them up, which leaves
/// the prose free to improve without breaking anything.
///
/// Allocated **sequentially as concepts are added**, not partitioned by stage:
/// the stage that reports a given problem is not stable (argument-count
/// checking wants to move from runtime to compile time), but a code is forever.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Code(pub &'static str);

impl std::fmt::Display for Code {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Note,
}

impl Severity {
    pub fn as_str(self) -> &'static str {
        match self {
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Note => "note",
        }
    }
}

impl std::fmt::Display for Severity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

/// A span with optional inline text, rendered under the source line.
#[derive(Debug, Clone, PartialEq)]
pub struct Label {
    pub span: Span,
    pub text: Option<String>,
}

/// How safe a suggestion is to apply without a human reading it.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Applicability {
    /// Correct as-is — an editor may offer it as a one-click fix.
    MachineApplicable,
    /// Plausible but not certainly what was meant; show, never auto-apply.
    MaybeIncorrect,
    /// Contains placeholders the user must fill in.
    HasPlaceholders,
}

impl Applicability {
    pub fn as_str(self) -> &'static str {
        match self {
            Applicability::MachineApplicable => "machine-applicable",
            Applicability::MaybeIncorrect => "maybe-incorrect",
            Applicability::HasPlaceholders => "has-placeholders",
        }
    }
}

/// A replacement of the source covered by `span`.
#[derive(Debug, Clone, PartialEq)]
pub struct Edit {
    pub span: Span,
    pub replacement: String,
}

/// Advice. Either prose, or a concrete edit that can become a quick fix.
#[derive(Debug, Clone, PartialEq)]
pub enum Help {
    Text(String),
    Suggestion {
        message: String,
        edits: Vec<Edit>,
        applicability: Applicability,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Diagnostic {
    pub code: Code,
    pub severity: Severity,
    /// One line, lowercase, no trailing period.
    pub message: String,
    pub primary: Label,
    pub secondary: Vec<Label>,
    pub notes: Vec<String>,
    pub helps: Vec<Help>,
}

impl Diagnostic {
    /// The primary span is required at construction.
    ///
    /// (The design sketch had `primary` as a builder step, which makes an
    /// un-located diagnostic representable. Requiring it here means a
    /// diagnostic without a location cannot be built at all.)
    pub fn new(severity: Severity, code: Code, span: Span, message: impl Into<String>) -> Self {
        Self {
            code,
            severity,
            message: message.into(),
            primary: Label { span, text: None },
            secondary: Vec::new(),
            notes: Vec::new(),
            helps: Vec::new(),
        }
    }

    pub fn error(code: Code, span: Span, message: impl Into<String>) -> Self {
        Self::new(Severity::Error, code, span, message)
    }

    pub fn warning(code: Code, span: Span, message: impl Into<String>) -> Self {
        Self::new(Severity::Warning, code, span, message)
    }

    /// Short text rendered inline under the primary span.
    pub fn label(mut self, text: impl Into<String>) -> Self {
        self.primary.text = Some(text.into());
        self
    }

    /// A second location that explains or conflicts with the primary one.
    pub fn secondary(mut self, span: Span, text: impl Into<String>) -> Self {
        self.secondary.push(Label {
            span,
            text: Some(text.into()),
        });
        self
    }

    /// Why the rule exists.
    pub fn note(mut self, text: impl Into<String>) -> Self {
        self.notes.push(text.into());
        self
    }

    /// What to do, as prose.
    pub fn help(mut self, text: impl Into<String>) -> Self {
        self.helps.push(Help::Text(text.into()));
        self
    }

    /// What to do, as a concrete edit.
    pub fn suggest(
        mut self,
        message: impl Into<String>,
        edits: Vec<Edit>,
        applicability: Applicability,
    ) -> Self {
        self.helps.push(Help::Suggestion {
            message: message.into(),
            edits,
            applicability,
        });
        self
    }

    pub fn is_error(&self) -> bool {
        self.severity == Severity::Error
    }

    pub fn span(&self) -> Span {
        self.primary.span
    }
}

/// The source a batch of diagnostics refers to.
///
/// `name` is what makes `--> app.oxi:12:5` a clickable link in an editor
/// terminal; without it the renderer can only say `--> line 12:5`.
#[derive(Debug, Clone)]
pub struct SourceFile {
    pub name: Option<String>,
    pub text: String,
}

impl SourceFile {
    pub fn new(text: impl Into<String>) -> Self {
        Self {
            name: None,
            text: text.into(),
        }
    }

    pub fn named(name: impl Into<String>, text: impl Into<String>) -> Self {
        Self {
            name: Some(name.into()),
            text: text.into(),
        }
    }

    /// 1-based line `n`, without its terminator.
    pub fn line(&self, n: usize) -> Option<&str> {
        self.text.lines().nth(n.saturating_sub(1))
    }
}

/// Where every stage writes. One per compilation.
#[derive(Debug, Clone, Default)]
pub struct DiagnosticSink {
    diagnostics: Vec<Diagnostic>,
}

impl DiagnosticSink {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, d: Diagnostic) {
        self.diagnostics.push(d);
    }

    pub fn extend(&mut self, ds: impl IntoIterator<Item = Diagnostic>) {
        self.diagnostics.extend(ds);
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics.iter().any(Diagnostic::is_error)
    }

    pub fn is_empty(&self) -> bool {
        self.diagnostics.is_empty()
    }

    pub fn len(&self) -> usize {
        self.diagnostics.len()
    }

    /// Drops diagnostics whose primary span starts inside an earlier
    /// diagnostic's primary span *on the same line*.
    ///
    /// This is a deliberately narrow rule. Suppressing anything merely
    /// *contained* in an earlier span would swallow independent errors inside a
    /// block, since a block's span contains all of them. Real cascade
    /// suppression wants causal tracking (poisoned nodes); until then this only
    /// removes the obvious same-line repeats.
    pub fn deduplicate(&mut self) {
        let mut kept: Vec<Diagnostic> = Vec::with_capacity(self.diagnostics.len());
        for d in std::mem::take(&mut self.diagnostics) {
            let shadowed = kept.iter().any(|k| {
                k.span().line() == d.span().line()
                    && d.span().start.offset >= k.span().start.offset
                    && d.span().start.offset < k.span().end.offset
            });
            if !shadowed {
                kept.push(d);
            }
        }
        self.diagnostics = kept;
    }
}
