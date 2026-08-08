//! Rendering. The only place that knows what a diagnostic looks like.
//!
//! Three formats share one traversal: human (terminal), JSON (the LSP and any
//! other tool), and short (`file:line:col: error[E0003]: msg`, for grep).

use super::{Diagnostic, Help, Label, SourceFile, registry};

/// Renders the caret/underline row for a label.
///
/// A span covering real text is underlined across its width; a point span —
/// synthetic locations, and anything the lexer could not give an extent —
/// still gets the single caret it always had.
fn underline(label: &Label, src: &SourceFile) -> Option<String> {
    let line_no = label.span.line();
    let col = label.span.column();
    if line_no == 0 || col == 0 {
        return None;
    }
    let line_text = src.line(line_no)?;

    // Width in characters, clamped so a multi-line span underlines only its first line.
    let start_byte = label.span.start.offset;
    let end_byte = label.span.end.offset;
    let width = if end_byte > start_byte {
        let covered = src
            .text
            .get(start_byte..end_byte)
            .unwrap_or_default()
            .lines()
            .next()
            .unwrap_or_default()
            .chars()
            .count();
        covered.max(1)
    } else {
        1
    };
    // Don't run past the end of the line.
    let remaining = line_text.chars().count().saturating_sub(col - 1).max(1);
    let width = width.min(remaining);

    let pad = " ".repeat(col.saturating_sub(1));
    Some(format!("{pad}{}", "^".repeat(width)))
}

fn secondary_underline(label: &Label, src: &SourceFile) -> Option<String> {
    underline(label, src).map(|u| u.replace('^', "-"))
}

/// Human-readable, rustc-style.
pub fn render_human(d: &Diagnostic, src: &SourceFile) -> String {
    let mut out = String::new();

    // Header: `error[E0003]: message`
    out.push_str(&format!("{}[{}]: {}\n", d.severity, d.code, d.message));

    // Location. Naming the file is what makes editor terminals linkify it.
    let line_no = d.primary.span.line();
    let col = d.primary.span.column();
    // Line 0 means no location was given; print what we know rather than an invented 0:0.
    match (&src.name, line_no) {
        (Some(name), 0) => out.push_str(&format!("  --> {name}\n")),
        (Some(name), _) => out.push_str(&format!("  --> {name}:{line_no}:{col}\n")),
        (None, 0) => {}
        (None, _) => out.push_str(&format!("  --> line {line_no}:{col}\n")),
    }

    let gutter = " ".repeat(format!("{line_no}").len());

    // Secondary labels first so the primary reads last, next to the notes explaining it.
    let mut secondaries: Vec<&Label> = d.secondary.iter().collect();
    secondaries.sort_by_key(|l| (l.span.line(), l.span.column()));
    for label in secondaries {
        if let Some(text) = src.line(label.span.line()) {
            let n = label.span.line();
            out.push_str(&format!("{gutter} |\n"));
            out.push_str(&format!("{n} | {text}\n"));
            if let Some(marks) = secondary_underline(label, src) {
                out.push_str(&format!("{gutter} | {marks}"));
                if let Some(t) = &label.text {
                    out.push_str(&format!(" {t}"));
                }
                out.push('\n');
            }
        }
    }

    if let Some(text) = src.line(line_no).filter(|_| line_no > 0) {
        out.push_str(&format!("{gutter} |\n"));
        out.push_str(&format!("{line_no} | {text}\n"));
        if let Some(marks) = underline(&d.primary, src) {
            out.push_str(&format!("{gutter} | {marks}"));
            if let Some(t) = &d.primary.text {
                out.push_str(&format!(" {t}"));
            }
            out.push('\n');
        }
    }

    for note in &d.notes {
        out.push_str(&format!("{gutter} = note: {note}\n"));
    }
    for help in &d.helps {
        match help {
            Help::Text(t) => out.push_str(&format!("{gutter} = help: {t}\n")),
            Help::Suggestion { message, edits, .. } => {
                out.push_str(&format!("{gutter} = help: {message}\n"));
                for edit in edits {
                    let n = edit.span.line();
                    if let Some(line_text) = src.line(n) {
                        let start = edit.span.start.offset;
                        let end = edit.span.end.offset;
                        // Splice the replacement in so the reader sees the fixed line, not a diff.
                        if let (Some(before), Some(after)) =
                            (src.text.get(..start), src.text.get(end..))
                        {
                            let line_start =
                                before.rfind('\n').map(|i| i + 1).unwrap_or(0);
                            let line_end = after.find('\n').unwrap_or(after.len());
                            let fixed = format!(
                                "{}{}{}",
                                &before[line_start..],
                                edit.replacement,
                                &after[..line_end]
                            );
                            out.push_str(&format!("{gutter} |\n"));
                            out.push_str(&format!("{n} | {fixed}\n"));
                        } else {
                            out.push_str(&format!("{n} | {line_text}\n"));
                        }
                    }
                }
            }
        }
    }

    if registry::lookup(d.code).is_some() {
        out.push_str(&format!(
            "{gutter} = note: run `oxigen explain {}` for more information\n",
            d.code
        ));
    }

    out
}

/// `file:line:col: error[E0003]: message`
pub fn render_short(d: &Diagnostic, src: &SourceFile) -> String {
    let name = src.name.as_deref().unwrap_or("<input>");
    format!(
        "{}:{}:{}: {}[{}]: {}",
        name,
        d.primary.span.line(),
        d.primary.span.column(),
        d.severity,
        d.code,
        d.message
    )
}

fn pos_json(p: crate::token::Pos) -> serde_json::Value {
    serde_json::json!({ "line": p.line, "column": p.column, "offset": p.offset })
}

fn label_json(l: &Label) -> serde_json::Value {
    serde_json::json!({
        "start": pos_json(l.span.start),
        "end": pos_json(l.span.end),
        "text": l.text,
    })
}

/// Machine-readable. A superset of the old `oxigen check` shape — `line`,
/// `column`, `message`, `severity` and `suggestion` are still present at the
/// top level so an existing consumer keeps working while it migrates to the
/// richer fields.
pub fn render_json(d: &Diagnostic, _src: &SourceFile) -> serde_json::Value {
    let legacy_suggestion = d.helps.first().map(|h| match h {
        Help::Text(t) => t.clone(),
        Help::Suggestion { message, .. } => message.clone(),
    });

    serde_json::json!({
        // legacy shape
        "line": d.primary.span.line(),
        "column": d.primary.span.column(),
        "message": d.message,
        "suggestion": legacy_suggestion,
        "severity": d.severity.as_str(),
        // new shape
        "code": d.code.0,
        "primary": label_json(&d.primary),
        "secondary": d.secondary.iter().map(label_json).collect::<Vec<_>>(),
        "notes": d.notes,
        "helps": d.helps.iter().map(|h| match h {
            Help::Text(t) => serde_json::json!({ "kind": "text", "message": t }),
            Help::Suggestion { message, edits, applicability } => serde_json::json!({
                "kind": "suggestion",
                "message": message,
                "applicability": applicability.as_str(),
                "edits": edits.iter().map(|e| serde_json::json!({
                    "start": pos_json(e.span.start),
                    "end": pos_json(e.span.end),
                    "replacement": e.replacement,
                })).collect::<Vec<_>>(),
            }),
        }).collect::<Vec<_>>(),
    })
}

/// Long-form explanation for `oxigen explain <CODE>`.
pub fn explain(code: &str) -> Option<String> {
    let entry = registry::all().find(|e| e.code.0.eq_ignore_ascii_case(code))?;
    Some(format!(
        "{}[{}]: {}\n\n{}",
        "error", entry.code, entry.title, entry.explanation
    ))
}
