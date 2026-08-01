//! `int_min / -1` and `int_min % -1` are a runtime error, not a process abort.
//!
//! Rust traps signed division overflow unconditionally (independent of
//! `overflow-checks`), and the workspace builds with `panic = "abort"`, so the
//! unguarded `/` and `%` in the VM's integer paths killed the process with
//! SIGABRT and a raw Rust panic message pointing into the compiler's own
//! source — no line, no hint, nothing the user could act on.
//!
//! Both dispatch sites needed widening: the `int_fast_binop!` guard (which
//! only checked `r != 0`) and `VM::binary_div` / `binary_mod`. The JIT already
//! branched `i64::MIN / -1` to its slow block, which calls those same helpers,
//! so fixing them covers every backend — these tests assert the two engines
//! agree anyway, since that is exactly the kind of thing that drifts.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

/// `-9223372036854775808` is not a literal (the `-` is a prefix op), so build
/// `int_min` the way a user has to: `int_max`'s negation minus one.
const INT_MIN: &str = "m := -9223372036854775807 - 1\n";

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

fn assert_both_reject(source: &str, needle: &str) {
    let interp = run_on(VM::new_interpreter(), source).unwrap_or_else(|e| e);
    let jit = run_on(VM::new_eager_jit(), source).unwrap_or_else(|e| e);
    assert_eq!(interp, jit, "backends disagree for: {source}");
    assert!(
        interp.contains(needle),
        "expected {needle:?} for {source}, got: {interp}"
    );
}

fn assert_both_yield(source: &str, expected: &str) {
    for (name, vm) in [
        ("interpreter", VM::new_interpreter()),
        ("jit", VM::new_eager_jit()),
    ] {
        let got = run_on(vm, source).unwrap_or_else(|e| panic!("{name} failed: {e}"));
        assert_eq!(got, expected, "{name} mismatch for: {source}");
    }
}

#[test]
fn int_min_divided_by_negative_one_is_an_error() {
    // Direct, and through a variable divisor so the fast path's constant-free
    // shape is covered too.
    assert_both_reject(&format!("{INT_MIN}m / -1"), "integer overflow");
    assert_both_reject(&format!("{INT_MIN}d := -1\nm / d"), "integer overflow");
}

#[test]
fn int_min_modulo_negative_one_is_an_error() {
    assert_both_reject(&format!("{INT_MIN}m % -1"), "integer overflow");
    assert_both_reject(&format!("{INT_MIN}d := -1\nm % d"), "integer overflow");
}

#[test]
fn the_error_carries_the_runtime_diagnostic_code() {
    // Same code as `division by zero` (E0031), so `oxigen explain` covers it.
    assert_both_reject(&format!("{INT_MIN}m / -1"), "E0031");
}

#[test]
fn division_by_zero_still_reports_zero_not_overflow() {
    assert_both_reject(&format!("{INT_MIN}m / 0"), "division by zero");
    assert_both_reject(&format!("{INT_MIN}m % 0"), "modulo by zero");
}

#[test]
fn dividing_by_negative_one_still_works_for_every_other_int() {
    // The fast-path guard now sends *all* `/ -1` to the slow helper; make sure
    // the helper still computes the ordinary cases.
    assert_both_yield("7 / -1", "-7");
    assert_both_yield("-7 / -1", "7");
    assert_both_yield("7 % -1", "0");
    assert_both_yield("9223372036854775807 / -1", "-9223372036854775807");
    assert_both_yield("d := -1\nx := 42\nx / d", "-42");
}

#[test]
fn int_min_arithmetic_that_is_representable_is_unaffected() {
    assert_both_yield(&format!("{INT_MIN}m / 2"), "-4611686018427387904");
    assert_both_yield(&format!("{INT_MIN}m % 2"), "0");
    assert_both_yield(&format!("{INT_MIN}m / 1"), "-9223372036854775808");
}

#[test]
fn overflowing_division_inside_a_hot_loop_stays_a_clean_error() {
    // Warm `divide` up with harmless operands first, so the JIT has compiled
    // it by the time the overflowing call arrives and the guard on the
    // compiled path is what reports the error.
    let source = format!(
        "fun divide(a <int>, b <int>) {{ a / b }}\n{INT_MIN}acc := 0\ni := 1\nrepeat when i < 200 {{\n    acc = acc + divide(i, 3)\n    i = i + 1\n}}\ndivide(m, -1)"
    );
    assert_both_reject(&source, "integer overflow");
}
