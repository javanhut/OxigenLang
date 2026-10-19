//! Lexer cases where the input was valid (or plainly invalid) and the lexer
//! quietly did something else.
//!
//! - A raw NUL byte was the end-of-input sentinel, so everything after one was
//!   ignored: not run, not checked, and deleted by `fmt`.
//! - In `#[indent]` files a comment line counted as indentation, a dedent to a
//!   level that was never opened was accepted (silently moving code out of a
//!   loop), a blank CRLF line closed the block, and indentation was tracked
//!   inside brackets, so a multi-line map or call could not be written.
//! - Each line start rescanned every following blank line: quadratic time.
//! - A string inside an interpolation queued its tokens ahead of its own start
//!   token; `1.5e3` lexed as `1.5` followed by a statement `e3`; a leading
//!   UTF-8 BOM was an illegal character.
//! - Comments and nested interpolations recursed per token, so a long enough
//!   run of them overflowed the stack.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;
use std::time::{Duration, Instant};

fn errors(source: &str) -> Vec<String> {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    parser.parse_program();
    parser.errors().iter().map(|d| d.message.clone()).collect()
}

fn run(source: &str) -> Result<String, String> {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    if !parser.errors().is_empty() {
        return Err(format!("parse errors: {}", parser.format_errors()));
    }
    let function = Compiler::new()
        .compile(&program)
        .map_err(|errs| format!("compile errors: {errs:?}"))?;
    VM::new_interpreter()
        .run(function)
        .map(|v| format!("{v}"))
        .map_err(|e| e.message)
}

fn on_big_stack<T: Send + 'static>(f: impl FnOnce() -> T + Send + 'static) -> T {
    std::thread::Builder::new()
        .stack_size(64 << 20)
        .spawn(f)
        .unwrap()
        .join()
        .unwrap()
}

// ── C3: NUL is not end of file ──

#[test]
fn embedded_nul_is_an_error_not_end_of_input() {
    let errs = errors("x := 1\n\0\ny := ( not valid ]]\n");
    assert!(!errs.is_empty(), "code after a NUL byte was silently ignored");
}

#[test]
fn code_after_nul_is_not_dropped() {
    // Even if the NUL is reported, the rest of the file must still be lexed.
    let errs = errors("x := 1\n\0\nthis is ( not valid ]]\n");
    assert!(
        errs.iter().any(|m| !m.contains("NUL") && !m.contains("\\0")),
        "only the NUL was reported; the broken line after it was never looked at: {errs:?}"
    );
}

// ── M1: comment lines don't take part in indentation ──

#[test]
fn indent_comment_lines_are_ignored() {
    assert_eq!(
        run("#[indent]\nout := []\neach i in [1]:\n    out.push(\"a\")\n// c\n    out.push(\"b\")\nout\n"),
        Ok("[a, b]".to_string())
    );
    assert_eq!(
        run("#[indent]\nout := []\neach i in [1]:\n    out.push(\"a\")\n            // deeper\n    out.push(\"b\")\nout\n"),
        Ok("[a, b]".to_string())
    );
}

// ── M2: dedent to an unknown level ──

#[test]
fn indent_dedent_to_unknown_level_is_an_error() {
    let errs = errors("#[indent]\neach i in [1, 2]:\n        println(\"a\")\n    println(\"b\")\nprintln(\"end\")\n");
    assert!(
        errs.iter().any(|m| m.contains("indent")),
        "inconsistent dedent was accepted silently: {errs:?}"
    );
}

// ── M3: CRLF ──

#[test]
fn indent_crlf_blank_line_does_not_close_the_block() {
    assert_eq!(
        run("#[indent]\r\nout := []\r\neach i in [1, 2]:\r\n    out.push(i)\r\n\r\n    out.push(i * 10)\r\nout\r\n"),
        Ok("[1, 10, 2, 20]".to_string())
    );
}

// ── H7: brackets suspend indentation ──

#[test]
fn indent_multiline_map_and_call() {
    assert_eq!(
        run("#[indent]\nm := {\n    \"a\": 1,\n    \"b\": 2\n}\nfun add(x, y):\n    x + y\nr := add(\n    1,\n    2\n)\n[m[\"b\"], r]\n"),
        Ok("[2, 3]".to_string())
    );
}

#[test]
fn indent_multiline_array() {
    assert_eq!(
        run("#[indent]\nxs := [\n    1,\n    2,\n]\nlen(xs)\n"),
        Ok("2".to_string())
    );
}

// ── L5: blank lines are linear ──

#[test]
fn indent_many_blank_lines_lex_in_linear_time() {
    let source = format!("#[indent]\nx := 1\n{}x\n", "\n".repeat(60_000));
    let start = Instant::now();
    assert_eq!(run(&source), Ok("1".to_string()));
    assert!(
        start.elapsed() < Duration::from_secs(5),
        "60k blank lines took {:?}",
        start.elapsed()
    );
}

// ── L1: nested interpolated strings ──

#[test]
fn string_inside_an_interpolation_can_interpolate() {
    assert_eq!(run("n := 5\n\"a{\"b{n}c\"}d\"\n"), Ok("ab5cd".to_string()));
}

// ── M10: float exponents ──

#[test]
fn float_literals_take_an_exponent() {
    assert_eq!(run("1.5e3\n"), Ok("1500".to_string()));
    assert_eq!(run("x := 2.5E-1\nx * 4.0\n"), Ok("1".to_string()));
    assert_eq!(run("type(1e3)\n"), Ok("FLOAT".to_string()));
}

// ── L8: byte-order mark ──

#[test]
fn leading_bom_is_ignored() {
    assert_eq!(run("\u{feff}x := 1\nx\n"), Ok("1".to_string()));
}

// ── recursion in the lexer ──

#[test]
fn long_runs_of_comments_do_not_overflow() {
    let source = format!("x := 1 {}\nx\n", "/**/ ".repeat(200_000));
    let got = std::thread::Builder::new()
        .stack_size(2 << 20)
        .spawn(move || run(&source))
        .unwrap()
        .join()
        .expect("lexer overflowed its stack on a run of comments");
    assert_eq!(got, Ok("1".to_string()));
}

#[test]
fn deeply_nested_interpolations_are_an_error_not_a_crash() {
    let depth = 50_000;
    let source = format!("{}1{}\n", "\"{".repeat(depth), "}\"".repeat(depth));
    let errs = on_big_stack(move || errors(&source));
    assert!(!errs.is_empty(), "absurdly nested interpolation was accepted");
}
