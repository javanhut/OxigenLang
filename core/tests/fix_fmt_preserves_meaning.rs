//! `oxigen fmt` rewrites files in place, so it must never change what a
//! program does, never write a file that no longer parses, and must reach a
//! fixed point after one pass.
//!
//! Every case here was a real rewrite that broke one of those three rules:
//!
//! - A single-statement `unless`/`when` block was folded into a postfix guard
//!   whatever the statement was. A `:=` (not guard-eligible) no longer parsed,
//!   and an `unless ... then` inside re-associated so the outer guard landed
//!   on the `then` branch.
//! - In `#[indent]` files a block expression in the middle of a line closed by
//!   dedent, so whatever followed it (`+ 5`, `when flag`) was glued onto the
//!   block's last line and ran inside it.
//! - Char literals came back as strings, an empty indent-style body as the
//!   undefined name `none`, tagged guards as `guard net:_err -> ...` (no such
//!   syntax), inline choose patterns lost their `pattern` keyword, block
//!   expressions inside interpolations broke the string across lines, `- -x`
//!   became the decrement token, and a NUL escape was written out raw.
//! - A comment inside a one-line function body was moved out of it.

use oxigen_core::compiler::Compiler;
use oxigen_core::formatter::Formatter;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

const INDENT: &str = "#[indent]\n";

fn parse_ok(source: &str, what: &str) -> oxigen_core::ast::Program {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "{what} does not parse:\n{}\n--- source ---\n{source}",
        parser.format_errors()
    );
    program
}

fn run(source: &str, what: &str) -> Result<String, String> {
    let program = parse_ok(source, what);
    let function = Compiler::new()
        .compile(&program)
        .map_err(|errs| format!("compile errors: {errs:?}"))?;
    VM::new_interpreter()
        .run(function)
        .map(|v| format!("{v}"))
        .map_err(|e| e.message)
}

fn fmt(source: &str) -> String {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "input does not parse:\n{}\n--- source ---\n{source}",
        parser.format_errors()
    );
    let indent = source.starts_with(INDENT);
    let body = Formatter::format_source(&program, parser.comments(), indent);
    if indent {
        format!("{INDENT}{body}")
    } else {
        body
    }
}

/// Formatting must keep the result, produce parseable output, and be stable.
fn assert_fmt_preserves(source: &str) -> String {
    let before = run(source, "original");
    let once = fmt(source);
    let after = run(&once, "formatted output");
    assert_eq!(
        before, after,
        "fmt changed the program's result\n--- before ---\n{source}\n--- after ---\n{once}"
    );
    let twice = fmt(&once);
    assert_eq!(
        once, twice,
        "fmt is not idempotent\n--- once ---\n{once}\n--- twice ---\n{twice}"
    );
    once
}

// ── C1 / H1: folding a one-statement block into a postfix guard ──

#[test]
fn unless_block_around_unless_then_keeps_its_meaning() {
    assert_fmt_preserves(
        "out := []\nskip_it := True\ncond := False\nunless skip_it {\n    out.push(\"a\") unless cond then out.push(\"b\")\n}\nout\n",
    );
}

#[test]
fn unless_block_around_assigned_unless_then_keeps_its_meaning() {
    assert_fmt_preserves(
        "x := 0\nskip_it := True\ncond := False\nunless skip_it {\n    x = 1 unless cond then 2\n}\nx\n",
    );
}

#[test]
fn unless_block_with_a_declaration_still_parses() {
    assert_fmt_preserves("total := 5\ndone := False\nunless done {\n    total := 0\n}\ntotal\n");
}

#[test]
fn unless_block_with_a_loop_still_parses() {
    assert_fmt_preserves(
        "out := []\ndone := False\nunless done {\n    each i in [1, 2] {\n        out.push(i)\n    }\n}\nout\n",
    );
    assert_fmt_preserves(
        "n := 0\ndone := False\nunless done {\n    repeat when n < 3 {\n        n = n + 1\n    }\n}\nn\n",
    );
}

#[test]
fn unless_block_with_a_choose_still_parses() {
    assert_fmt_preserves(
        "out := []\ndone := False\nv := 1\nunless done {\n    choose v {\n        else -> out.push(v)\n    }\n}\nout\n",
    );
}

#[test]
fn unless_block_with_a_guarded_statement_still_parses() {
    assert_fmt_preserves(
        "out := []\ndone := False\nunless done {\n    out.push(3) when True\n}\nout\n",
    );
}

#[test]
fn unless_block_whose_condition_interpolates_an_unless_then() {
    assert_fmt_preserves(
        "a := 1\nb := 0\nunless \"v{a unless False then 2}w\" == \"x\" {\n    b = 5\n}\nb\n",
    );
}

#[test]
fn plain_postfix_guards_still_fold() {
    let out = assert_fmt_preserves(
        "out := []\nx := 1\nout.push(1) when x > 0\nout.push(2) unless x > 0\nout\n",
    );
    assert!(out.contains("out.push(1) when x > 0"), "{out}");
    assert!(out.contains("out.push(2) unless x > 0"), "{out}");
}

#[test]
fn unless_then_expression_in_an_option_arm_stays_an_expression() {
    // Inline, the arm is an `unless ... then` expression; moved into a block
    // body it would read as a guarded statement, which yields another value.
    assert_fmt_preserves("a := 1\nx := option { False -> 5, a unless True then -a }\nx\n");
    assert_fmt_preserves("a := 1\nx := option { a > 0 -> a unless False then 9, 0 }\nx\n");
}

#[test]
fn comparison_moved_to_the_start_of_a_line_is_not_a_declaration() {
    assert_fmt_preserves("a := 1\nb := 2\nc := option { False -> 1, b < a > 7 guard e -> 3 }\nc\n");
}

// ── C2 / H7: block expressions mid-line in #[indent] files ──

#[test]
fn indent_option_followed_by_an_operator() {
    assert_fmt_preserves("#[indent]\nc := True\nx := option { c -> 1, 2 } + 5\nx\n");
}

#[test]
fn indent_option_followed_by_a_guard() {
    assert_fmt_preserves(
        "#[indent]\nout := []\nx := True\nflag := False\noption { x -> out.push(\"a\"), out.push(\"b\") } when flag\nout\n",
    );
}

#[test]
fn indent_block_expressions_inside_calls_and_long_lists() {
    assert_fmt_preserves(
        "#[indent]\nfun apply(f, v):\n    f(v)\nxs := [1111111111, 2222222222, 3333333333, 4444444444, 5555555555, 6666666666, 7777777777]\nr := option { False -> \"a\", \"b\" }\n[apply(fun(x) { x * 2 }, 21), len(xs), r, option { False -> \"a\", \"b\" }]\n",
    );
}

#[test]
fn indent_whole_line_block_expressions_keep_colon_style() {
    let out = assert_fmt_preserves("#[indent]\nc := True\nx := option { c -> 1, 2 }\nx\n");
    assert!(out.contains("x := option:"), "{out}");
}

// ── H2 / H3 / H8 / H9: literals and operators ──

#[test]
fn char_literal_stays_a_char() {
    let out = assert_fmt_preserves("c := `a`\ntype(c)\n");
    assert!(out.contains("`a`"), "{out}");
}

#[test]
fn indent_empty_function_body_returns_none() {
    assert_fmt_preserves("#[indent]\nfun f() {}\nf()\n");
}

#[test]
fn double_negation_does_not_become_decrement() {
    assert_fmt_preserves("x := 3\ny := - -x\ny\n");
}

#[test]
fn control_characters_in_strings_are_escaped() {
    let out = assert_fmt_preserves("s := \"nul\\0end\"\nt := \"bell\\x07\"\n[len(s), len(t)]\n");
    assert!(
        !out.contains('\0'),
        "raw NUL written into the file:\n{out:?}"
    );
    assert!(
        !out.contains('\x07'),
        "raw control character written into the file:\n{out:?}"
    );
    let out = assert_fmt_preserves("s := \"\"\"a\\0b\"\"\"\nlen(s)\n");
    assert!(
        !out.contains('\0'),
        "raw NUL written into the file:\n{out:?}"
    );
}

// ── H4: tagged and synthetic guards ──

#[test]
fn angle_form_guards_and_fails_round_trip() {
    assert_fmt_preserves(
        "fun boom() { <fail>(<Error<net>>(\"lost\")) }\n\
         fun other() { <fail>(<Error<disk>>(\"full\")) }\n\
         a := boom() <guard<Error<net>>>(\"recovered\")\n\
         c := boom() <guard<Error>>(\"any\")\n\
         fun wrap() {\n    v := other() <guard<Error<net>>>(\"nope\")\n    give v\n}\n\
         fun f2() { boom() <fail<Error<net>>>(\"rewrapped\") }\n\
         b := boom() <guard> e -> \"g2\"\n\
         d := boom() <guard<Error<net>>> e -> \"named\"\n\
         [a, c, type(wrap()), type(f2()), b, d]\n",
    );
}

// ── H5: inline choose patterns ──

#[test]
fn inline_choose_pattern_keeps_its_keyword() {
    assert_fmt_preserves(
        "out := []\nv := 3\nchoose v { pattern small(n) when n < 5 -> out.push(\"s\"), else -> out.push(\"o\") }\nout\n",
    );
}

// ── H6: block expressions inside interpolations ──

#[test]
fn interpolations_stay_on_one_line() {
    assert_fmt_preserves(
        "a := 5\nc := 9\ns := \"v{option { a - 2 -> 7, c }}w\"\nt := \"n={len([1111111111, 2222222222, 3333333333, 4444444444, 5555555555, 6666666666, 7777777777])}\"\n[s, t]\n",
    );
}

// ── M8: comments inside function bodies ──

#[test]
fn trailing_comment_stays_inside_a_one_line_body() {
    let out = assert_fmt_preserves("fun g(x) {\n    x + 1 // add one\n}\ng(1)\n");
    let close = out.find('}').expect("body was inlined away");
    let comment = out.find("// add one").expect("comment dropped");
    assert!(comment < close, "comment moved out of the body:\n{out}");
}

#[test]
fn comments_in_an_empty_body_stay_put() {
    let out = assert_fmt_preserves("fun f() { // note\n    // body\n}\nf()\n");
    let close = out.find('}').expect("closing brace");
    assert!(
        out.find("// body").unwrap() < close,
        "comment moved out of the body:\n{out}"
    );
}
