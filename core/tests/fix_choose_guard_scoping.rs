//! Regression: `choose` subjects, inline patterns and `guard` bindings are
//! scoped to their construct instead of living in the VM's globals.
//!
//! - The `choose` subject was stored in the global `__choose_tmp__`, so a
//!   pattern condition that ran another `choose` overwrote the outer subject
//!   and the remaining arms tested the wrong value.
//! - An inline `pattern big(n) ...` arm was a `DefineGlobal`, so calling a
//!   function that used one redefined a top-level pattern of the same name.
//! - `guard err -> ...` inserted the error into the globals: it clobbered a
//!   user global named `err` (even an immutable typed one), and inside a
//!   function a local of the same name won, so the fallback saw the local.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn run_on(mut vm: VM, source: &str) -> Result<String, String> {
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
        .map_err(|errs| format!("{errs:?}"))?;
    vm.run(function)
        .map(|v| format!("{v}"))
        .map_err(|e| e.message)
}

fn assert_all_ok(source: &str, expected: &str) {
    for (name, make) in [
        ("interpreter", VM::new_interpreter as fn() -> VM),
        ("tiered", VM::new_tiered),
        ("eager_jit", VM::new_eager_jit),
    ] {
        let got = run_on(make(), source).unwrap_or_else(|e| panic!("{name} failed: {e}\n{source}"));
        assert_eq!(got, expected, "{name} mismatch for:\n{source}");
    }
}

#[test]
fn nested_choose_in_a_pattern_condition_keeps_the_outer_subject() {
    let src = "fun sign(n) {\n  choose n {\n    pattern neg(x) when x < 0 -> \"neg\",\n    else -> \"nonneg\"\n  }\n}\n\
               pattern pos(n) when n > 0\n\
               fun classify(v) {\n  choose v {\n    pattern h(n) when sign(-100) == \"zzz\" -> \"h\",\n    pos -> \"positive\",\n    else -> \"other\"\n  }\n}\n\
               classify(7)";
    assert_all_ok(src, "positive");
}

#[test]
fn inline_pattern_does_not_replace_a_top_level_pattern() {
    let src = "pattern big(n) when n > 100\n\
               fun f(v) {\n  choose v {\n    pattern big(n) when n > 1 -> \"f big\",\n    else -> \"f small\"\n  }\n}\n\
               fun g(v) {\n  choose v {\n    big -> \"top big\",\n    else -> \"top small\"\n  }\n}\n\
               a := f(5)\n\
               [a, g(3)]";
    assert_all_ok(src, "[f big, top small]");
}

#[test]
fn choose_subject_is_reentrant_under_recursion() {
    let src = "fun depth(n) {\n  choose n {\n    pattern zero(x) when x == 0 -> 0,\n    else -> 1 + depth(n - 1)\n  }\n}\ndepth(5)";
    assert_all_ok(src, "5");
}

#[test]
fn guard_binding_does_not_clobber_a_global() {
    assert_all_ok(
        "err <int> = 5\ny := fail \"x\" guard err -> 0\n[err, y]",
        "[5, 0]",
    );
}

#[test]
fn guard_binding_shadows_a_local_of_the_same_name() {
    let src =
        "fun t() {\n  e := \"local\"\n  y := fail \"bang\" guard e -> e.msg\n  [y, e]\n}\nt()";
    assert_all_ok(src, "[bang, local]");
}

#[test]
fn guard_binding_inside_call_arguments() {
    let src = "fun t() {\n  a := 1\n  [a, fail \"in arg\" guard e -> e.msg, a + 1]\n}\nt()";
    assert_all_ok(src, "[1, in arg, 2]");
}

#[test]
fn guard_without_error_keeps_the_value() {
    assert_all_ok(
        "fun t() {\n  e := 3\n  y := 10 guard e -> 0\n  [y, e]\n}\nt()",
        "[10, 3]",
    );
}
