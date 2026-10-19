//! Parser cases that either crashed or accepted code they should have rejected
//! (or rejected code they should have accepted).
//!
//! - Nesting had no limit, so a few thousand `(` overflowed the stack and
//!   aborted the process instead of producing a diagnostic.
//! - Anything at all was accepted after `.` as a field name (`m.(`, `m."a"`,
//!   and even the newline in `p.` at end of line), and any token as a loop,
//!   pattern or choose-arm name.
//! - `5 < log` committed to the `<log>` angle form on the name alone.
//! - `<Error<tag>>` written as a whole type annotation did not parse, because
//!   the `>>` that closes both brackets was treated as closing only one.
//! - Two statements could share a line with nothing between them, which is
//!   how `1.5e3` used to "work" as `1.5` and a separate statement `e3`.
//! - A float literal too large for f64 became `inf` silently.
//! - `repeat unless a < b` was rewritten to `repeat when a >= b`, which is not
//!   the same loop when either side is NaN.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn errors(source: &str) -> Vec<String> {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    parser.parse_program();
    parser.errors().iter().map(|d| d.message.clone()).collect()
}

fn assert_rejected(source: &str) {
    let errs = errors(source);
    assert!(!errs.is_empty(), "accepted malformed code:\n{source}");
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

/// The parser must bound its own recursion; the stack here is generous, but
/// far smaller than what 100k levels of unbounded recursion need.
fn errors_on_stack(source: String) -> Vec<String> {
    std::thread::Builder::new()
        .stack_size(16 << 20)
        .spawn(move || errors(&source))
        .unwrap()
        .join()
        .expect("parser overflowed its stack instead of reporting the nesting")
}

// ── M4: nesting depth ──

#[test]
fn deep_parentheses_are_a_diagnostic() {
    let n = 100_000;
    let errs = errors_on_stack(format!("x := {}1{}\n", "(".repeat(n), ")".repeat(n)));
    assert!(errs.iter().any(|m| m.contains("nest")), "{:?}", errs.first());
}

#[test]
fn deep_brackets_are_a_diagnostic() {
    let n = 100_000;
    let errs = errors_on_stack(format!("x := {}1{}\n", "[".repeat(n), "]".repeat(n)));
    assert!(errs.iter().any(|m| m.contains("nest")), "{:?}", errs.first());
}

#[test]
fn deep_prefix_chains_are_a_diagnostic() {
    let errs = errors_on_stack(format!("x := {}True\n", "not ".repeat(100_000)));
    assert!(errs.iter().any(|m| m.contains("nest")), "{:?}", errs.first());
}

#[test]
fn deep_blocks_are_a_diagnostic() {
    let n = 20_000;
    let errs = errors_on_stack(format!(
        "{}x := 1\n{}",
        "each i in [1] {\n".repeat(n),
        "}\n".repeat(n)
    ));
    assert!(errs.iter().any(|m| m.contains("nest")), "{:?}", errs.first());
}

#[test]
fn reasonable_nesting_still_parses() {
    let n = 100;
    assert_eq!(
        run(&format!("{}1{}\n", "(".repeat(n), ")".repeat(n))),
        Ok("1".to_string())
    );
    assert_eq!(
        run(&format!("{}1{}\n", "[".repeat(n), "]".repeat(n))),
        Ok(format!("{}1{}", "[".repeat(n), "]".repeat(n)))
    );
}

// ── M5: field names ──

#[test]
fn dot_requires_a_field_name() {
    for bad in ["y := m.(", "y := m.{", "y := m.+", "y := m.\"a\"", "x := p.\ny := 1"] {
        assert_rejected(&format!("m := {{\"a\": 1}}\np := m\n{bad}\n"));
    }
}

#[test]
fn dot_still_accepts_field_names() {
    assert_eq!(run("m := {\"a\": 7}\nm.a\n"), Ok("7".to_string()));
    assert_eq!(
        run("struct P {\n    type <str>\n}\np := P(\"x\")\np.type\n"),
        Ok("x".to_string())
    );
}

// ── M6: `<` before a name that is also an angle form ──

#[test]
fn less_than_an_angle_form_name() {
    assert_eq!(run("log := 10\n5 < log\n"), Ok("True".to_string()));
    assert_eq!(run("type := 10\n5 < type\n"), Ok("True".to_string()));
    assert_eq!(run("Value := 10\nx := 3\nx < Value\n"), Ok("True".to_string()));
}

// ── M7: `<Error<tag>>` as a whole annotation ──

#[test]
fn tagged_error_type_annotation() {
    assert_eq!(
        run("r <Error<net>> := <Error<net>>(\"x\")\ntype(r)\n"),
        Ok("ERROR<net>".to_string())
    );
    assert!(errors("x <Error<net>>\n").is_empty(), "{:?}", errors("x <Error<net>>\n"));
    assert!(
        errors("fun f(e <Error<net>>) { e }\n").is_empty(),
        "{:?}",
        errors("fun f(e <Error<net>>) { e }\n")
    );
}

// ── M10: statements need a separator ──

#[test]
fn two_statements_on_one_line_are_rejected() {
    assert_rejected("x := 1 println(\"same line\")\n");
    assert_rejected("fun f() {\n    a := 1 b := 2\n}\n");
}

#[test]
fn single_line_blocks_still_parse() {
    assert_eq!(run("fun f(x) { x + 1 }\nf(1)\n"), Ok("2".to_string()));
    assert_eq!(run("out := []\neach i in [1, 2] { out.push(i) }\nout\n"), Ok("[1, 2]".to_string()));
}

// ── L2: float overflow ──

#[test]
fn float_literal_out_of_range_is_an_error() {
    let big = format!("x := {}.0\n", "9".repeat(400));
    let errs = errors(&big);
    assert!(errs.iter().any(|m| m.contains("float")), "{errs:?}");
}

// ── L3: `repeat unless` with NaN ──

#[test]
fn repeat_unless_negates_rather_than_inverting_the_comparison() {
    assert_eq!(
        run("n := float(\"nan\")\ncount := 0\nrepeat unless n < 1.0 {\n    count = count + 1\n    stop\n}\ncount\n"),
        Ok("1".to_string())
    );
}

// ── L4: names must be names ──

#[test]
fn loop_and_pattern_names_must_be_identifiers() {
    assert_rejected("each 5 in [1, 2] {\n}\n");
    assert_rejected("each ( in [1] {\n}\n");
    assert_rejected("pattern 9(x) when x > 1\n");
    assert_rejected("choose 1 {\n    42 -> println(1)\n}\n");
    assert_rejected("t := diverge each 7 in [1] { 1 }\n");
}

#[test]
fn tagged_error_inside_a_type_conversion() {
    assert!(errors("r := <type<Value || Error<parse>>>(5)\n").is_empty());
    assert!(errors("r := <type<int>>(5)\n").is_empty());
    assert!(errors("r := <type<Error || Value>>(5)\n").is_empty());
}
