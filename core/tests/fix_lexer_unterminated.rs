// Regression tests for the lexer's unterminated-string-literal diagnostic.
//
// Before this fix, a missing closing quote made the lexer silently swallow
// characters until the next quote or EOF, producing a misleading cascade of
// parser errors far from the real mistake (e.g. `{"stream: False}` reported
// "expected Colon, got Ident(\"body\")" three lines later). The lexer now
// detects an unterminated string (for single-line strings: a raw newline or
// EOF before the closing delimiter; triple-quoted strings may span lines, so
// only EOF before the closing fence is unterminated) and reports a clear error
// anchored at the OPENING quote.

use oxigen_core::{lexer::Lexer, parser::Parser};

/// Parse `src` and return the collected parser diagnostics rendered as a single
/// lowercased string plus the (line, column) of the first error, if any.
fn parse_errors(src: &str) -> Vec<(usize, usize, String)> {
    let lexer = Lexer::new(src);
    let mut parser = Parser::new(lexer, src);
    let _program = parser.parse_program();
    parser
        .errors()
        .iter()
        .map(|d| (d.span.line, d.span.column, d.message.to_lowercase()))
        .collect()
}

#[test]
fn unterminated_string_reports_error_at_opening_quote() {
    // The opening quote sits on line 1, column 6 (1-based). The reported error
    // must mention the unterminated string and be anchored there — not on a
    // later line where the lexer happens to recover.
    let src = "x := \"hello\ny := 5\n";
    let errors = parse_errors(src);

    assert!(
        !errors.is_empty(),
        "an unterminated string must produce at least one diagnostic"
    );

    let mentions = errors
        .iter()
        .find(|(_, _, msg)| msg.contains("unterminated") || msg.contains("string"));
    let (line, col, msg) =
        mentions.expect("a diagnostic should mention the unterminated string literal");

    assert_eq!(
        *line, 1,
        "error should be anchored at the opening quote's line, got {:?}",
        errors
    );
    assert_eq!(
        *col, 6,
        "error should be anchored at the opening quote's column, got {:?}",
        errors
    );
    assert!(
        msg.contains("unterminated"),
        "diagnostic message should say the string is unterminated, got {:?}",
        msg
    );
}

#[test]
fn unterminated_string_at_eof_reports_error() {
    let src = "y := \"oops";
    let errors = parse_errors(src);
    assert!(
        errors
            .iter()
            .any(|(_, _, msg)| msg.contains("unterminated")),
        "an unterminated string at EOF must be reported, got {:?}",
        errors
    );
}

#[test]
fn missing_quote_in_map_does_not_cascade_far_away() {
    // Mirrors the real-world report: a missing closing quote on a map key.
    // The error must be anchored on the offending line (line 2), NOT three
    // lines later as a confusing "expected Colon" cascade.
    let src = "config := {\n    \"stream: False,\n    \"body\": data,\n}\n";
    let errors = parse_errors(src);

    assert!(
        !errors.is_empty(),
        "a missing closing quote must produce a diagnostic"
    );
    assert!(
        errors
            .iter()
            .any(|(line, _, msg)| *line == 2 && msg.contains("unterminated")),
        "the unterminated-string error should be reported on the offending \
         line (2), got {:?}",
        errors
    );
}

#[test]
fn well_formed_program_parses_cleanly() {
    // A normal program with ordinary strings, an escaped newline, an escaped
    // quote, and interpolation must parse with zero errors.
    let src = concat!(
        "name := \"world\"\n",
        "greeting := \"hello, {name}!\"\n",
        "quoted := \"she said \\\"hi\\\"\"\n",
        "lines := \"a\\nb\"\n",
    );
    let errors = parse_errors(src);
    assert!(
        errors.is_empty(),
        "a well-formed program must parse without errors, got {:?}",
        errors
    );
}
