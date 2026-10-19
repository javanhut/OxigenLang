//! Regression: a type lock or immutability belongs to the BINDING, so no
//! write path may skip it.
//!
//! - Locks were stored per physical stack slot and dropped whenever two
//!   locals in disjoint scopes shared a slot. An `each` loop before
//!   `count <int> := 0` was enough for `count = "oops"` to succeed.
//! - Writes from a closure (`SetUpvalue`) checked neither the lock nor
//!   immutability: `x <int> = 1; g := fun() { x = 5 }` changed `x`.
//! - `x, y = ...` reassignment checked nothing, and `x, y := ...` inside a
//!   function silently created a same-scope shadow instead of updating `x`
//!   (at top level both are already rejected).

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

/// Compile errors and runtime errors both count as a rejection.
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
            Ok(v) => panic!("{name}: expected an error containing {needle:?}, got Ok({v})\n{source}"),
            Err(e) => assert!(
                e.contains(needle),
                "{name}: error {e:?} does not mention {needle:?}\n{source}"
            ),
        }
    }
}

#[test]
fn lock_survives_slot_reuse_after_a_loop() {
    assert_all_err(
        "fun f() {\n  each i in [1, 2] { }\n  count <int> := 0\n  count = \"oops\"\n  count\n}\nf()",
        "type mismatch",
    );
}

#[test]
fn lock_survives_slot_reuse_by_a_later_untyped_local() {
    // Typed first, untyped later in a sibling scope: the typed write must
    // still be checked even though the slot's metadata had to be cleared.
    assert_all_err(
        "fun f(flag) {\n  option {\n    flag -> {\n      n <int> := 1\n      n = \"bad\"\n      n\n    },\n    {\n      s := \"fine\"\n      s = 2\n      s\n    }\n  }\n}\nf(True)",
        "type mismatch",
    );
    assert_all_ok(
        "fun f(flag) {\n  option {\n    flag -> {\n      n <int> := 1\n      n = 2\n      n\n    },\n    {\n      s := \"fine\"\n      s = 2\n      s\n    }\n  }\n}\n[f(True), f(False)]",
        "[2, 2]",
    );
}

#[test]
fn lock_still_allows_same_type_writes_after_slot_reuse() {
    assert_all_ok(
        "fun f() {\n  each i in [1, 2] { }\n  count <int> := 0\n  each j in [1, 2, 3] { count = count + j }\n  count\n}\nf()",
        "6",
    );
}

#[test]
fn closure_cannot_reassign_an_immutable_outer_local() {
    assert_all_err(
        "fun f() {\n  x <int> = 1\n  g := fun() { x = 5 }\n  g()\n  x\n}\nf()",
        "immutable",
    );
}

#[test]
fn closure_write_respects_the_outer_type_lock() {
    assert_all_err(
        "fun f() {\n  x <int> := 1\n  g := fun() { x = \"s\" }\n  g()\n  x\n}\nf()",
        "type mismatch",
    );
    assert_all_ok(
        "fun f() {\n  x <int> := 1\n  g := fun() { x = 7 }\n  g()\n  x\n}\nf()",
        "7",
    );
}

#[test]
fn closure_cannot_reassign_a_loop_variable() {
    assert_all_err(
        "fun f() {\n  out := []\n  each i in [10, 20] {\n    g := fun() { i = 100 }\n    g()\n    out.push(i)\n  }\n  out\n}\nf()",
        "loop variable",
    );
}

#[test]
fn unpack_reassign_respects_immutability_and_locks() {
    assert_all_err(
        "fun f() {\n  x <int> = 10\n  y := 0\n  x, y = 99, 2\n  x\n}\nf()",
        "immutable",
    );
    assert_all_err(
        "fun f() {\n  x <int> := 10\n  y := 0\n  x, y = \"a\", 2\n  x\n}\nf()",
        "type mismatch",
    );
}

#[test]
fn unpack_define_updates_an_existing_local_instead_of_shadowing() {
    assert_all_err(
        "fun f() {\n  x <int> := 10\n  x, y := \"a\", 2\n  x\n}\nf()",
        "type mismatch",
    );
    assert_all_ok(
        "fun f() {\n  x := 10\n  x, y := 1, 2\n  [x, y]\n}\nf()",
        "[1, 2]",
    );
    assert_all_ok(
        "fun f() {\n  x := 10\n  x, y := [3, 4]\n  [x, y]\n}\nf()",
        "[3, 4]",
    );
}
