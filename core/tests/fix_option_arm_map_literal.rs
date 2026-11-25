//! Tests that a `{` in `option`/`choose` arm position is a block unless it opens
//! a map literal (`{}` or `{ key :`). Map literals parse, and every block shape
//! still parses as a block.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

/// Runs a program, returning its final value rendered as a string.
fn eval(source: &str) -> String {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "parse errors for:\n{source}\n{}",
        parser.format_errors()
    );
    let function = Compiler::new()
        .compile(&program)
        .map_err(|e| format!("{e:?}"))
        .expect("compile should succeed");
    let mut vm = VM::new();
    vm.set_source(source);
    vm.set_script_args(&[]);
    match vm.run(function) {
        Ok(v) => format!("{v}"),
        Err(e) => panic!("runtime error for:\n{source}\n{}", e.message),
    }
}

#[test]
fn empty_braces_in_a_default_arm_are_an_empty_map_not_an_empty_block() {
    assert_eq!(eval("type(option { False -> \"no\", {} })"), "MAP");
    assert_eq!(eval("len(option { False -> \"no\", {} })"), "0");
}

#[test]
fn map_literal_parses_as_a_default_arm() {
    assert_eq!(eval("option { False -> \"no\", {\"k\": 1} }[\"k\"]"), "1");
}

#[test]
fn map_literal_parses_as_an_arm_body() {
    assert_eq!(eval("option { True -> {\"k\": 7}, \"no\" }[\"k\"]"), "7");
}

#[test]
fn empty_map_arm_body_is_a_map() {
    assert_eq!(eval("type(option { True -> {}, \"no\" })"), "MAP");
}

#[test]
fn multi_statement_block_body_still_works() {
    assert_eq!(eval("option { True -> { x := 1\n x + 1 }, \"no\" }"), "2");
}

#[test]
fn multi_statement_block_default_still_works() {
    assert_eq!(eval("option { False -> \"no\", { y := 5\n y * 2 } }"), "10");
}

#[test]
fn walrus_is_not_mistaken_for_a_map_colon() {
    assert_eq!(eval("option { False -> \"no\", { z := 9\n z } }"), "9");
}

#[test]
fn single_statement_block_still_works() {
    assert_eq!(eval("option { True -> { 42 }, \"no\" }"), "42");
}

#[test]
fn struct_literal_inside_a_block_arm_still_works() {
    let src = "struct P { name <str> }\n\
               option { True -> { p := P { name: \"x\" }\n p.name }, \"no\" }";
    assert_eq!(eval(src), "x");
}

#[test]
fn map_literal_nested_inside_a_block_arm_still_works() {
    let src = "option { True -> { m := {\"k\": 7}\n m[\"k\"] }, \"no\" }";
    assert_eq!(eval(src), "7");
}

#[test]
fn nested_option_inside_a_block_arm_still_works() {
    assert_eq!(
        eval("option { True -> { option { False -> 1, 2 } }, \"no\" }"),
        "2"
    );
}

#[test]
fn ternary_form_still_accepts_map_values() {
    assert_eq!(eval("option { False, {\"t\": 1}, {\"f\": 2} }[\"f\"]"), "2");
}

#[test]
fn no_default_arm_and_no_match_yields_none_rather_than_failing() {
    assert_eq!(eval("x := 3\noption { x == 10 -> { \"ten\" } }"), "None");
}

#[test]
fn no_default_arm_still_runs_the_matching_arm() {
    assert_eq!(eval("x := 10\noption { x == 10 -> { \"ten\" } }"), "ten");
}

#[test]
fn a_map_literal_body_and_an_empty_map_default_coexist() {
    let src = "d := {\"value\": False}\n\
               type(option { d[\"value\"] == True -> {\"status\": \"ok\"}, {} })";
    assert_eq!(eval(src), "MAP");
}

#[test]
fn set_in_creates_intermediate_objects_with_a_bare_empty_map_default() {
    let src = "introduce json\njson.stringify(json.set_in({}, \"a.b.c\", 1))";
    assert_eq!(eval(src), "{\"a\":{\"b\":{\"c\":1}}}");
}
