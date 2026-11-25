//! A dropped statement is reported per statement, not per file.
//!
//! The condition is per statement: either the parser's error count rose while
//! parsing it, or some diagnostic lands inside its extent. The "parser gave
//! up" note (E0030) stays quiet for statements the lexer already reported,
//! which are matched by span.

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
    for src in ["fun\n= 5", "= 5\n= 6", "x := 1 +\n= 5", "each x in\n)"] {
        assert_eq!(lines_reported(src), vec![1, 2], "for:\n{src}");
        assert!(!gave_up(src), "ordinary syntax errors are not parser bugs");
    }
}

#[test]
fn a_lexically_reported_statement_does_not_get_a_gave_up_note() {
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
    const TOKENS: &[&str] = &[
        "fun",
        "struct",
        "each",
        "in",
        "if",
        "else",
        "match",
        "return",
        "spawn",
        "introduce",
        "from",
        "repeat",
        "when",
        "unless",
        "then",
        "x",
        "1",
        "\"s\"",
        ":=",
        "=",
        "<",
        ">",
        "(",
        ")",
        "[",
        "]",
        "{",
        "}",
        ",",
        ".",
        ":",
        "->",
        "|",
        "+",
        "?",
        "@",
        "$",
        "..",
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
