//! V4 regression: typed `<Error || Value>` declaration must NOT double-wrap
//! an already-wrapped value.
//!
//! `res <Error || Value> := <Value>("hi")` then `res.value` must yield `hi`
//! (matching the tree-walker). The declaration-position `TypeWrap` used to
//! re-wrap the already-`Wrapped` value, producing `Value(Value(hi))` so that
//! `res.value` returned `Value(hi)`. The fix makes the Error||Value
//! normalization idempotent on the success side.

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

fn both(source: &str, expected: &str) {
    assert_eq!(run(source, None), expected, "interpreter: {}", source);
    assert_eq!(run(source, Some(1)), expected, "jit: {}", source);
}

#[test]
fn errorvalue_decl_does_not_double_wrap() {
    let src = r#"
res <Error || Value> := <Value>("hi")
res.value
"#;
    both(src, "hi");
}

#[test]
fn errorvalue_decl_value_type_is_value() {
    let src = r#"
res <Error || Value> := <Value>("hi")
type(res)
"#;
    both(src, "VALUE");
}

#[test]
fn errorvalue_decl_error_side_still_normalizes() {
    // The error side of the union must still surface its message.
    let src = r#"
res <Error || Value> := <Error<oops>>("boom")
res.msg
"#;
    both(src, "boom");
}

#[test]
fn errorvalue_decl_plain_value_wraps_once() {
    // A bare (un-wrapped) value still gets wrapped exactly once.
    let src = r#"
res <Error || Value> := 42
res.value
"#;
    both(src, "42");
}
