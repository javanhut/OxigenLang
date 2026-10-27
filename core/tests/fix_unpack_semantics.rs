//! Regression: multi-name assignment evaluates every right-hand value before
//! binding any name, and the counts must match.
//!
//! - `a, b = b, a` bound `a` before evaluating the second value, so it gave
//!   `2 2` instead of swapping.
//! - `a, b := 1, 2, 3` silently dropped the `3` (it was never evaluated) and
//!   `a, b, c := 1, 2` left `c` undefined.
//! - `x, y := [1, 2, 3]` truncated and `x, y, z := [1]` padded with `None`,
//!   where `docs/variables.md` documents "expected 2 values, got 3".

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
    let function = Compiler::new().compile(&program).map_err(|errs| {
        errs.iter()
            .map(|e| e.message.clone())
            .collect::<Vec<_>>()
            .join("; ")
    })?;
    vm.run(function)
        .map(|v| format!("{v}"))
        .map_err(|e| e.message)
}

/// A backend's name and constructor.
type Backend = (&'static str, fn() -> VM);

fn backends() -> [Backend; 3] {
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
            Ok(v) => {
                panic!("{name}: expected an error containing {needle:?}, got Ok({v})\n{source}")
            }
            Err(e) => assert!(
                e.contains(needle),
                "{name}: error {e:?} does not mention {needle:?}\n{source}"
            ),
        }
    }
}

#[test]
fn swap_at_top_level() {
    assert_all_ok("a := 1\nb := 2\na, b = b, a\n[a, b]", "[2, 1]");
}

#[test]
fn swap_inside_a_function() {
    assert_all_ok(
        "fun f() {\n  a := 1\n  b := 2\n  a, b = b, a\n  [a, b]\n}\nf()",
        "[2, 1]",
    );
}

#[test]
fn rotate_three_through_a_closure() {
    assert_all_ok(
        "fun f() {\n  a := 1\n  b := 2\n  c := 3\n  g := fun() { a, b, c = b, c, a }\n  g()\n  [a, b, c]\n}\nf()",
        "[2, 3, 1]",
    );
}

#[test]
fn multi_expression_define_binds_in_order() {
    assert_all_ok("a, b := 1, 2\n[a, b]", "[1, 2]");
    assert_all_ok("fun f() {\n  a, b := 1, 2\n  [a, b]\n}\nf()", "[1, 2]");
}

#[test]
fn multi_expression_count_mismatch_is_a_compile_error() {
    assert_all_err("a, b := 1, 2, 3\na", "expected 2 values, got 3");
    assert_all_err("a, b, c := 1, 2\na", "expected 3 values, got 2");
    assert_all_err(
        "a := 0\nb := 0\na, b = 1, 2, 3\na",
        "expected 2 values, got 3",
    );
}

#[test]
fn single_value_count_mismatch_is_a_runtime_error() {
    assert_all_err("x, y := [1, 2, 3]\nx", "expected 2 values, got 3");
    assert_all_err("x, y, z := [1]\nx", "expected 3 values, got 1");
    assert_all_err(
        "fun f() {\n  x, y := (1, 2, 3)\n  x\n}\nf()",
        "expected 2 values, got 3",
    );
}

#[test]
fn exact_unpacks_still_work() {
    assert_all_ok("x, y := [1, 2]\n[x, y]", "[1, 2]");
    assert_all_ok(
        "fun f() {\n  first, _, third := (1, 2, 3)\n  [first, third]\n}\nf()",
        "[1, 3]",
    );
}
