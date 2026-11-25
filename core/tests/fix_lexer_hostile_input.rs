//! Lexer behaviour on hostile or unusual input:
//!
//! - raw NUL bytes, and a leading UTF-8 BOM;
//! - `#[indent]` handling of comment lines, bad dedents, blank CRLF lines and brackets;
//! - linear-time handling of blank lines;
//! - strings inside interpolations, and numbers such as `1.5e3`;
//! - deeply nested comments and interpolations do not overflow the stack.

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

#[test]
fn embedded_nul_is_an_error_not_end_of_input() {
    let errs = errors("x := 1\n\0\ny := ( not valid ]]\n");
    assert!(
        !errs.is_empty(),
        "code after a NUL byte was silently ignored"
    );
}

#[test]
fn code_after_nul_is_not_dropped() {
    let errs = errors("x := 1\n\0\nthis is ( not valid ]]\n");
    assert!(
        errs.iter()
            .any(|m| !m.contains("NUL") && !m.contains("\\0")),
        "only the NUL was reported; the broken line after it was never looked at: {errs:?}"
    );
}

#[test]
fn indent_comment_lines_are_ignored() {
    assert_eq!(
        run(
            "#[indent]\nout := []\neach i in [1]:\n    out.push(\"a\")\n// c\n    out.push(\"b\")\nout\n"
        ),
        Ok("[a, b]".to_string())
    );
    assert_eq!(
        run(
            "#[indent]\nout := []\neach i in [1]:\n    out.push(\"a\")\n            // deeper\n    out.push(\"b\")\nout\n"
        ),
        Ok("[a, b]".to_string())
    );
}

#[test]
fn indent_dedent_to_unknown_level_is_an_error() {
    let errs = errors(
        "#[indent]\neach i in [1, 2]:\n        println(\"a\")\n    println(\"b\")\nprintln(\"end\")\n",
    );
    assert!(
        errs.iter().any(|m| m.contains("indent")),
        "inconsistent dedent was accepted silently: {errs:?}"
    );
}

#[test]
fn indent_crlf_blank_line_does_not_close_the_block() {
    assert_eq!(
        run(
            "#[indent]\r\nout := []\r\neach i in [1, 2]:\r\n    out.push(i)\r\n\r\n    out.push(i * 10)\r\nout\r\n"
        ),
        Ok("[1, 10, 2, 20]".to_string())
    );
}

#[test]
fn indent_multiline_map_and_call() {
    assert_eq!(
        run(
            "#[indent]\nm := {\n    \"a\": 1,\n    \"b\": 2\n}\nfun add(x, y):\n    x + y\nr := add(\n    1,\n    2\n)\n[m[\"b\"], r]\n"
        ),
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

#[test]
fn string_inside_an_interpolation_can_interpolate() {
    assert_eq!(run("n := 5\n\"a{\"b{n}c\"}d\"\n"), Ok("ab5cd".to_string()));
}

#[test]
fn float_literals_take_an_exponent() {
    assert_eq!(run("1.5e3\n"), Ok("1500".to_string()));
    assert_eq!(run("x := 2.5E-1\nx * 4.0\n"), Ok("1".to_string()));
    assert_eq!(run("type(1e3)\n"), Ok("FLOAT".to_string()));
}

#[test]
fn leading_bom_is_ignored() {
    assert_eq!(run("\u{feff}x := 1\nx\n"), Ok("1".to_string()));
}

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
    assert!(
        !errs.is_empty(),
        "absurdly nested interpolation was accepted"
    );
}
