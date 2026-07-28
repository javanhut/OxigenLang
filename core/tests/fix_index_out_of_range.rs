//! Out-of-range indexing is a runtime error.
//!
//! It used to be silent: a read past the end produced `None` (which then failed
//! somewhere unrelated, or read as a legitimately-absent map entry), and a write
//! past the end was discarded entirely — no error, no growth, and the next read
//! returned the old value.
//!
//! The bounds check lives in one place, `VM::sequence_index`, because three
//! call sites had their own copy of the arithmetic: the interpreter's read, the
//! interpreter's write, and the JIT's array fast path.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn compile(source: &str) -> oxigen_core::vm::value::Function {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "parser errors:\n{}",
        parser.format_errors()
    );
    Compiler::new()
        .compile(&program)
        .unwrap_or_else(|errs| panic!("compile errors: {errs:?}"))
}

fn run_on(mut vm: VM, source: &str) -> Result<String, String> {
    vm.run(compile(source))
        .map(|v| format!("{v}"))
        .map_err(|e| e.message)
}

/// Asserts every backend rejects `source`, with the same message.
fn assert_all_backends_reject(source: &str) -> String {
    let interp = run_on(VM::new_interpreter(), source).expect_err("interpreter should reject");
    let jit = run_on(VM::new_eager_jit(), source).expect_err("jit should reject");
    assert_eq!(interp, jit, "backends disagree for:\n{source}");
    assert!(
        interp.contains("out of range"),
        "expected an out-of-range error, got: {interp}"
    );
    interp
}

/// Asserts every backend accepts `source` and yields `expected`.
fn assert_all_backends_yield(source: &str, expected: &str) {
    for (name, vm) in [
        ("interpreter", VM::new_interpreter()),
        ("jit", VM::new_eager_jit()),
    ] {
        let got = run_on(vm, source).unwrap_or_else(|e| panic!("{name} failed: {e}"));
        assert_eq!(got, expected, "{name} mismatch for:\n{source}");
    }
}

#[test]
fn reading_past_the_end_is_an_error() {
    for source in [
        "a := [10, 20, 30]\na[3]",
        "a := [10, 20, 30]\na[99]",
        "\"abc\"[3]",
        "(1, 2)[2]",
    ] {
        assert_all_backends_reject(source);
    }
}

#[test]
fn reading_before_the_start_is_an_error() {
    // Negative indices count from the end, so -len is the floor.
    for source in ["a := [10, 20, 30]\na[-4]", "\"abc\"[-4]", "(1, 2)[-3]"] {
        assert_all_backends_reject(source);
    }
}

#[test]
fn indexing_an_empty_collection_is_an_error() {
    let err = assert_all_backends_reject("a := []\na[0]");
    assert!(err.contains("empty"), "empty case should say so: {err}");
}

#[test]
fn writing_past_the_end_is_an_error_not_a_silent_no_op() {
    // The dangerous one: this used to change nothing and report nothing.
    assert_all_backends_reject("a := [10, 20, 30]\na[3] = 1\na");
    assert_all_backends_reject("a := [10, 20, 30]\na[-4] = 1\na");
}

#[test]
fn in_range_indexing_is_unchanged() {
    assert_all_backends_yield("a := [10, 20, 30]\na[0]", "10");
    assert_all_backends_yield("a := [10, 20, 30]\na[2]", "30");
    assert_all_backends_yield("a := [10, 20, 30]\na[-1]", "30");
    assert_all_backends_yield("a := [10, 20, 30]\na[-3]", "10");
    assert_all_backends_yield("(1, 2, 3)[-2]", "2");
    assert_all_backends_yield("a := [10, 20, 30]\na[-1] = 77\na", "[10, 20, 77]");
}

#[test]
fn string_indices_count_characters_not_bytes() {
    // `héllo` is 6 bytes but 5 characters. The negative-index bound used the
    // byte length against a character lookup, so `[-1]` fell off the end and
    // returned None instead of the last character.
    assert_all_backends_yield("\"héllo\"[-1]", "o");
    assert_all_backends_yield("\"héllo\"[1]", "é");
    assert_all_backends_yield("\"héllo\"[4]", "o");
    assert_all_backends_reject("\"héllo\"[5]");
}

#[test]
fn a_missing_map_key_is_still_none() {
    // Maps are lookups, not sequences — absence is a legitimate answer.
    assert_all_backends_yield("m := {\"k\": 1}\nm[\"nope\"]", "None");
    assert_all_backends_yield("m := {\"k\": 1}\nm[99]", "None");
    // And assigning a new key still inserts rather than erroring.
    assert_all_backends_yield("m := {\"k\": 1}\nm[\"new\"] = 2\nm[\"new\"]", "2");
}

#[test]
fn slicing_still_clamps_instead_of_erroring() {
    // Slices deliberately keep their forgiving contract.
    assert_all_backends_yield("a := [1, 2, 3]\na[0:99]", "[1, 2, 3]");
    assert_all_backends_yield("a := [1, 2, 3]\na[5:9]", "[]");
}

#[test]
fn the_error_names_the_index_and_the_length() {
    let err = assert_all_backends_reject("a := [10, 20, 30]\na[7]");
    assert!(err.contains('7'), "should name the index: {err}");
    assert!(err.contains('3'), "should name the length: {err}");
}
