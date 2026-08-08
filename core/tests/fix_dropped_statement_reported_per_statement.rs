//! A dropped statement is reported *per statement*, not per file.
//!
//! `assert_nothing_dropped_silently` used to bail out if `self.errors` was
//! non-empty at the end of the parse. That enforces "something, somewhere, was
//! reported" — so if statement A produced a diagnostic and statement B was then
//! dropped quietly, B vanished unmentioned and the check still passed. The
//! condition is now per statement: either the parser's error count rose while
//! parsing it, or some diagnostic lands inside its extent.
//!
//! The "parser gave up" note (E0030) is a safety net for a bug in Oxigen, not a
//! user-facing syntax error, so the tests below are mostly about it staying
//! quiet — in particular for statements the *lexer* reported. The lexer runs
//! ahead of the parser, so those diagnostics exist before the parser reaches
//! the statement and are only folded in at the end; matching them by span is
//! what keeps the stricter check from crying "gave up" over them.

use oxigen_core::diagnostics::Diagnostic;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;

fn parse_errors(src: &str) -> Vec<Diagnostic> {
    let lexer = Lexer::new(src);
    let mut parser = Parser::with_file(lexer, src, "test.oxi");
    parser.parse_program();
    parser.errors().to_vec()
}

fn gave_up(src: &str) -> bool {
    parse_errors(src).iter().any(|d| d.code.0 == "E0030")
}

fn lines_reported(src: &str) -> Vec<usize> {
    let mut lines: Vec<usize> = parse_errors(src).iter().map(|d| d.span().line()).collect();
    lines.sort_unstable();
    lines.dedup();
    lines
}

#[test]
fn a_second_bad_statement_is_reported_too() {
    // The masking case: line 1 fails, line 2 must not ride on line 1's
    // diagnostic. Both lines get their own.
    for src in ["fun\n= 5", "= 5\n= 6", "x := 1 +\n= 5", "each x in\n)"] {
        assert_eq!(lines_reported(src), vec![1, 2], "for:\n{src}");
        assert!(!gave_up(src), "ordinary syntax errors are not parser bugs");
    }
}

#[test]
fn a_lexically_reported_statement_does_not_get_a_gave_up_note() {
    // The lexer already said what is wrong here; the parser dropping the
    // statement afterwards is expected, not a bug in Oxigen.
    for src in [
        "x := \"unterminated\nz := 1",
        "x := 1\nb := \"unterminated\ny := 2",
        "x := $",
        "x := 1\n$\ny := 2",
        "x := 99999999999999999999999999\n",
        "/* unclosed\n",
        "x := 1.2.3\n",
        "x := 1;\n",
    ] {
        let errs = parse_errors(src);
        assert!(!errs.is_empty(), "should still be reported:\n{src}");
        assert!(!gave_up(src), "spurious gave-up note for:\n{src}");
    }
}

#[test]
fn an_earlier_parser_error_does_not_change_the_lexical_verdict() {
    // Same sources, now preceded by a statement the *parser* rejects. Before,
    // that first error suppressed the whole check; now each statement is judged
    // on its own, so this is where a too-strict span match would show up.
    for tail in [
        "x := \"unterminated\n",
        "/* unclosed\n",
        "x := 1.2.3\n",
        "x := 99999999999999999999999999\n",
    ] {
        let src = format!("= 5\n{tail}");
        assert!(!gave_up(&src), "spurious gave-up note for:\n{src}");
    }
}

#[test]
fn no_syntax_error_is_ever_dropped_unreported() {
    // Deterministic token soup: every one of these programs is garbage, so the
    // parser drops statements constantly. None of them may produce the gave-up
    // note (that would mean either a genuinely silent drop, or — the regression
    // this guards — a span check too strict to recognise the diagnostic that
    // *was* emitted). Multi-line so the masking case is exercised throughout.
    const TOKENS: &[&str] = &[
        "fun", "struct", "each", "in", "if", "else", "match", "return", "spawn", "introduce",
        "from", "repeat", "when", "unless", "then", "x", "1", "\"s\"", ":=", "=", "<", ">", "(",
        ")", "[", "]", "{", "}", ",", ".", ":", "->", "|", "+", "?", "@", "$", "..",
    ];
    let mut seed: u64 = 0x243F6A8885A308D3;
    let mut next = |m: usize| {
        seed = seed
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        (seed >> 33) as usize % m
    };
    for _ in 0..5000 {
        let mut src = String::new();
        for _ in 0..1 + next(4) {
            for _ in 0..2 + next(5) {
                src.push_str(TOKENS[next(TOKENS.len())]);
                src.push(' ');
            }
            src.push('\n');
        }
        assert!(!gave_up(&src), "gave up on:\n{src}");
    }
}
