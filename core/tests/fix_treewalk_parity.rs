//! VM regression tests for lane-`eval` items (the VM is the reference impl):
//!
//!   TW1 — `=` reassignment of an UNTYPED binding (`s := 0; s = s + 1`) must
//!         succeed and mutate it (handle_set_global). Type-lock and
//!         immutability violations must STILL error.
//!   TW2 — negative index READ (`arr[-1]`, `"s"[-1]`) must index from the end
//!         (eval_index). Out-of-range still yields None.
//!   TW4 — String `<` lexicographic comparison must return a bool
//!         (compare_less). (The VM only implements `<` for strings.)

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

/// Run a program on the bytecode VM (interpreter path). Returns the formatted
/// final value, or the error message string on failure.
fn run_vm(source: &str) -> Result<String, String> {
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
        .map_err(|errs| format!("{:?}", errs))?;
    let mut vm = VM::new();
    vm.run(function)
        .map(|v| format!("{}", v))
        .map_err(|e| e.message)
}

/// Assert VM(interp) == `expected` for a program that succeeds.
fn assert_parity(source: &str, expected: &str) {
    let interp = run_vm(source).expect("VM interp should succeed");
    assert_eq!(interp, expected, "VM interp mismatch for:\n{}", source);
}

/// Assert the VM rejects `source`.
fn assert_both_error(source: &str) {
    let interp = run_vm(source);
    assert!(
        interp.is_err(),
        "VM should have errored but returned {:?} for:\n{}",
        interp,
        source
    );
}

// ── TW1: `=` on untyped bindings ────────────────────────────────────────────

#[test]
fn tw1_eq_on_untyped_accumulator() {
    // The canonical repro: bare `=` reassigns an untyped binding.
    let src = "s := 0\ns = s + 1\ns";
    assert_parity(src, "1");
}

#[test]
fn tw1_eq_on_untyped_loop_accumulator() {
    let src = "total := 0\neach n in [1, 2, 3, 4] {\n  total = total + n\n}\ntotal";
    assert_parity(src, "10");
}

#[test]
fn tw1_eq_on_untyped_string_rebind() {
    // Untyped binding has no type lock, so `=` may even change the value type.
    let src = "x := 1\nx = \"hi\"\nx";
    assert_parity(src, "hi");
}

#[test]
fn tw1_walras_retype_untyped() {
    // `:=` retype of an untyped var, then `=` on the new (still untyped) value.
    let src = "x := 1\nx := \"a\"\nx = \"b\"\nx";
    assert_parity(src, "b");
}

#[test]
fn tw1_typed_strict_eq_in_range() {
    // A mutable typed binding (`:=`) accepts `=` of a matching type.
    let src = "x <int> := 10\nx = 20\nx";
    assert_parity(src, "20");
}

#[test]
fn tw1_type_mismatch_assign_errors() {
    // Type-lock violation must STILL error in both engines.
    assert_both_error("x <int> := 10\nx = \"hi\"\nx");
}

#[test]
fn tw1_immutable_reassign_errors() {
    // `x <int> = 10` is an immutable typed declaration; `x = 20` must error.
    assert_both_error("x <int> = 10\nx = 20\nx");
}

// ── TW2: negative index reads ───────────────────────────────────────────────

#[test]
fn tw2_array_negative_last() {
    assert_parity("a := [10, 20, 30]\na[-1]", "30");
}

#[test]
fn tw2_array_negative_middle() {
    assert_parity("a := [10, 20, 30]\na[-2]", "20");
}

#[test]
fn tw2_string_negative_last() {
    assert_parity("s := \"hello\"\ns[-1]", "o");
}

#[test]
fn tw2_string_negative_middle() {
    assert_parity("s := \"hello\"\ns[-2]", "l");
}

#[test]
fn tw2_tuple_negative_last() {
    assert_parity("t := (1, 2, 3)\nt[-1]", "3");
}

#[test]
fn tw2_array_positive_still_works() {
    assert_parity("a := [10, 20, 30]\na[0]", "10");
}

#[test]
fn tw2_array_out_of_range_positive_none() {
    // Out-of-range positive index yields None in both engines.
    assert_parity("a := [10, 20, 30]\na[5]", "None");
}

// ── TW4: String `<` lexicographic ───────────────────────────────────────────

#[test]
fn tw4_string_less_true() {
    assert_parity("\"a\" < \"b\"", "True");
}

#[test]
fn tw4_string_less_false() {
    assert_parity("\"b\" < \"a\"", "False");
}

#[test]
fn tw4_string_less_equal_prefix() {
    // "ab" < "abc": shorter prefix is lexicographically smaller.
    assert_parity("\"ab\" < \"abc\"", "True");
}

#[test]
fn tw4_string_less_equal_strings() {
    assert_parity("\"a\" < \"a\"", "False");
}
