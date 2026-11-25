//! Tests that the caret under a diagnostic aligns with the error column on lines containing tabs and wide (CJK) characters.

use oxigen_core::diagnostics::registry as codes;
use oxigen_core::diagnostics::{Diagnostic, SourceFile, render};
use oxigen_core::token::{Pos, Span};

fn caret_line(source: &str, line: usize, column: usize, offset: usize) -> String {
    let span = Span::range(
        Pos::new(line, column, offset),
        Pos::new(line, column + 1, offset + 1),
    );
    let d = Diagnostic::error(codes::UNEXPECTED_TOKEN, span, "unexpected token");
    let out = render::render_human(&d, &SourceFile::new(source));
    out.lines()
        .find(|l| l.contains('^'))
        .unwrap_or_else(|| panic!("no caret line in:\n{out}"))
        .to_string()
}

#[test]
fn caret_keeps_tabs_before_the_error() {
    let line = caret_line("\tx ]\n", 1, 4, 3);
    let marks = line.split(" | ").nth(1).unwrap();
    assert!(
        marks.starts_with('\t'),
        "tab replaced by a space, caret misaligned: {line:?}"
    );
}

#[test]
fn caret_accounts_for_wide_characters() {
    let line = caret_line("s := \"日本\" ]\n", 1, 11, 14);
    let marks = line.split(" | ").nth(1).unwrap();
    assert_eq!(
        marks.find('^'),
        Some(12),
        "caret misaligned under wide characters: {line:?}"
    );
}
