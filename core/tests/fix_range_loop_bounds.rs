//! Regression: the lowered `each i in range(...)` loop behaves like the
//! `range` builtin at the edges, without leaving the JIT.
//!
//! The compiler turns `each i in range(a, b, step)` into a counter instead of
//! building an array. That counter was advanced with a wrapping add and tested
//! against the bound afterwards, so a step past i64::MAX wrapped to a huge
//! negative number that was still `< end`, and the loop ran on (practically
//! forever). The builtin stops instead. The lowered loop also never checked
//! its bounds' types: `range(0.5, 3)` iterated 0.5, 1.5, 2.5 and
//! `range("a", "c")` ran once, where the builtin reports
//! `range() requires integer arguments`.
//!
//! Both checks have to stay compilable: a check the JIT can't compile would
//! push every function with a range loop back to the interpreter.

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

fn backends() -> [(&'static str, fn() -> VM); 3] {
    [
        ("interpreter", VM::new_interpreter),
        ("tiered", VM::new_tiered),
        ("eager_jit", VM::new_eager_jit),
    ]
}

fn assert_all_ok(source: &str, expected: &str) {
    for (name, make) in backends() {
        let got = run_on(make(), source).unwrap_or_else(|e| panic!("{name} failed: {e}\n{source}"));
        assert_eq!(got, expected, "{name} mismatch for:\n{source}");
    }
}

fn assert_all_err(source: &str, needle: &str) {
    for (name, make) in backends() {
        match run_on(make(), source) {
            Ok(v) => panic!("{name}: expected an error containing {needle:?}, got Ok({v})\n{source}"),
            Err(e) => assert!(
                e.contains(needle),
                "{name}: error {e:?} does not mention {needle:?}\n{source}"
            ),
        }
    }
}

/// Collects the loop's values inside a function, so the JIT compiles it. The
/// cap turns a wrapped counter into a wrong answer instead of a hang.
fn collected(range_args: &str) -> String {
    format!(
        "fun f() {{\n  out := []\n  each i in range({range_args}) {{\n    out = push(out, i)\n    stop when len(out) == 20\n  }}\n  out\n}}\nf()"
    )
}

#[test]
fn counting_up_stops_at_i64_max() {
    assert_all_ok(
        &collected("9223372036854775800, 9223372036854775807, 5"),
        "[9223372036854775800, 9223372036854775805]",
    );
    assert_all_ok(
        &collected("9223372036854775805, 9223372036854775807"),
        "[9223372036854775805, 9223372036854775806]",
    );
}

#[test]
fn counting_down_stops_at_i64_min() {
    assert_all_ok(
        &collected("-9223372036854775800, -9223372036854775807 - 1, -5"),
        "[-9223372036854775800, -9223372036854775805]",
    );
}

#[test]
fn skip_and_stop_still_work_near_the_edge() {
    let src = "fun f() {\n  out := []\n  each i in range(9223372036854775797, 9223372036854775807, 3) {\n    skip when i == 9223372036854775800\n    out = push(out, i)\n    stop when len(out) == 20\n  }\n  out\n}\nf()";
    assert_all_ok(src, "[9223372036854775797, 9223372036854775803, 9223372036854775806]");
    let src = "fun f() {\n  n := 0\n  each i in range(0, 9223372036854775807, 4611686018427387904) {\n    n = n + 1\n    stop when n == 20\n  }\n  n\n}\nf()";
    assert_all_ok(src, "2");
}

#[test]
fn ordinary_ranges_are_unchanged() {
    assert_all_ok(&collected("5"), "[0, 1, 2, 3, 4]");
    assert_all_ok(&collected("2, 5"), "[2, 3, 4]");
    assert_all_ok(&collected("0, 10, 3"), "[0, 3, 6, 9]");
    assert_all_ok(&collected("10, 0, -3"), "[10, 7, 4, 1]");
    assert_all_ok(&collected("3, 3"), "[]");
    assert_all_ok(&collected("5, 0"), "[]");
    assert_all_ok(&collected("0, 5, -1"), "[]");
    assert_all_ok(&collected("-3"), "[]");
}

#[test]
fn every_bound_must_be_an_integer() {
    assert_all_err(&collected("3.5"), "range() requires integer arguments");
    assert_all_err(&collected("0, 2.5"), "range() requires integer arguments");
    assert_all_err(&collected("1.5, 4"), "range() requires integer arguments");
    assert_all_err(&collected("\"a\", \"c\""), "range() requires integer arguments");
    assert_all_err(&collected("None"), "range() requires integer arguments");
}

#[test]
fn a_bound_error_can_be_recovered_with_guard() {
    let src = "fun f(n) {\n  t := 0\n  each i in range(n) {\n    t = t + i\n  }\n  t\n}\nf(1.5) <guard>(-1)";
    assert_all_ok(src, "-1");
}

#[cfg(feature = "jit")]
#[test]
fn functions_with_range_loops_still_compile() {
    let src = "fun f(n) {\n  t := 0\n  each i in range(n) {\n    t = t + i\n  }\n  each j in range(n, 0, -2) {\n    t = t + j\n  }\n  t\n}\nf(100)";
    let mut vm = VM::new_eager_jit();
    let got = vm.run(compile(src)).map(|v| format!("{v}")).unwrap();
    assert_eq!(got, "7500");
    assert!(vm.jit.compiled_ok_count() >= 1, "f was not compiled");
    assert_eq!(vm.jit.compile_failed_count(), 0, "a range loop left the JIT");
}

#[test]
fn nested_range_loops_do_not_leak_stack_slots() {
    // More outer iterations than the operand stack has slots: an inner loop
    // that left one slot behind per entry overflowed the stack under the JIT.
    let src = "fun f(n) {\n  t := 0\n  each i in range(n) {\n    each j in range(4) {\n      t = t + j\n    }\n    each k in range(i, i - 2, -1) {\n      t = t + 1\n    }\n  }\n  t\n}\nf(300000)";
    assert_all_ok(src, "2400000");
}

#[test]
fn an_empty_inner_range_exits_cleanly_every_time() {
    let src = "fun f(n) {\n  t := 0\n  each i in range(n) {\n    each j in range(i, i) {\n      t = t + 100\n    }\n    t = t + 1\n  }\n  t\n}\nf(300000)";
    assert_all_ok(src, "300000");
}
