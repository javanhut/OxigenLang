//! B8 / decision D1 regression: cross-type numeric equality COERCES.
//!
//! `2 == 2.0` -> True, `2 == 3.0` -> False; the same for byte==int and
//! uint==int, mirroring the coercion the ordering operators already do.
//! Non-numeric cross-kind pairs stay unequal (`1 == "1"` -> False), enum
//! equality is unaffected, and map int-keys still resolve (Value::eq stays
//! strict so hashing is unchanged).

#![cfg(feature = "jit")]

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn run(source: &str, jit_threshold: Option<u32>) -> String {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "parser errors:\n{}",
        parser.format_errors()
    );
    let function = Compiler::new()
        .compile(&program)
        .expect("compile should succeed");
    let mut vm = VM::new();
    if let Some(t) = jit_threshold {
        vm.jit.set_threshold(t);
    }
    match vm.run(function) {
        Ok(v) => format!("{}", v),
        Err(e) => panic!("runtime error: {}", e.message),
    }
}

/// Assert the program evaluates to `expected` under both the interpreter
/// and the JIT path (parity).
fn both(source: &str, expected: &str) {
    assert_eq!(run(source, None), expected, "interpreter: {}", source);
    assert_eq!(run(source, Some(1)), expected, "jit: {}", source);
}

#[test]
fn int_float_equality_coerces() {
    both("2 == 2.0", "True");
    both("2.0 == 2", "True");
    both("2 == 3.0", "False");
    both("3.0 == 2", "False");
    both("2 != 2.0", "False");
    both("2 != 3.0", "True");
}

#[test]
fn byte_int_equality_coerces() {
    both("byte(2) == 2", "True");
    both("2 == byte(2)", "True");
    both("byte(2) == 3", "False");
    both("byte(2) == 2.0", "True");
    both("2.0 == byte(2)", "True");
}

#[test]
fn uint_int_equality_coerces() {
    both("uint(2) == 2", "True");
    both("2 == uint(2)", "True");
    both("uint(2) == 3", "False");
    both("uint(2) == 2.0", "True");
    both("uint(2) == byte(2)", "True");
}

#[test]
fn non_numeric_cross_kind_stays_unequal() {
    both("1 == \"1\"", "False");
    both("1 != \"1\"", "True");
    both("True == 1", "False");
    both("None == 0", "False");
}

#[test]
fn same_kind_equality_unchanged() {
    both("2 == 2", "True");
    both("2.0 == 2.0", "True");
    both("\"hi\" == \"hi\"", "True");
    both("True == True", "True");
}

#[test]
fn enum_equality_unaffected() {
    let src = r#"
enum Sig { Red, Green }
enum Dir { North, South }
Sig.Red == Sig.Red
"#;
    both(src, "True");

    let src2 = r#"
enum Sig { Red, Green }
enum Dir { North, South }
Sig.Red == Dir.North
"#;
    both(src2, "False");
}

#[test]
fn map_int_keys_still_resolve() {
    // Value::eq stays strict, so int keys hash/resolve unchanged.
    let src = r#"
m := {1: "a", 2: "b"}
m[2]
"#;
    both(src, "b");
}
