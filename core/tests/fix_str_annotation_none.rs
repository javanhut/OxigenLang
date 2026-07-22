//! Regression: `<str>` must not stringify `None`.
//!
//! `t <str> := os.env_get("UNSET")` used to bind the 4-char string "None"
//! (`convert_to_type` STRING was a blanket `format!("{}", value)`). That
//! string is truthy and never `== None`, so every absence guard silently
//! fell through. Every other conversion target already rejects None; STRING
//! now does too. Scalar coercions into `<str>` are unaffected.

#![cfg(feature = "jit")]

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn run(source: &str, jit_threshold: Option<u32>) -> Result<String, String> {
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
        Ok(v) => Ok(format!("{}", v)),
        Err(e) => Err(e.message),
    }
}

fn both_ok(source: &str, expected: &str) {
    assert_eq!(run(source, None).as_deref(), Ok(expected), "interpreter");
    assert_eq!(run(source, Some(1)).as_deref(), Ok(expected), "jit");
}

fn both_err(source: &str) {
    for jit in [None, Some(1)] {
        let err = run(source, jit).expect_err("expected a runtime error");
        assert!(
            err.contains("cannot convert NONE to STRING"),
            "unexpected error: {}",
            err
        );
    }
}

#[test]
fn str_annotation_rejects_none() {
    both_err("t <str> := None");
}

#[test]
fn str_annotation_still_coerces_scalars() {
    both_ok("t <str> := 42\nt", "42");
    both_ok("t <str> := 3.5\nt", "3.5");
    both_ok("t <str> := True\nt", "True");
    both_ok("t <str> := \"hi\"\nt", "hi");
}

#[test]
fn none_guard_fires_on_untyped_binding() {
    // The idiom the bug broke: bind an absent lookup, compare against None.
    both_ok("t := None\nt == (None or \"\")", "True");
    both_ok("t := \"x\"\nt == (None or \"\")", "False");
    // ...and the value must still be None, not the 4-char string "None".
    both_ok("t := None\nt", "None");
    both_ok("t := None\nlen(str(t))", "4");
}
