//! `lock name = value`: a variable that can never change, bound to a deep,
//! read-only copy of the value. The original stays changeable; writes through
//! any name for the locked copy (index, field, in-place builtins) are refused.

#![cfg(feature = "jit")]

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

/// `jit_threshold == Some(1)` forces the JIT path; `None` keeps the interpreter.
fn run_vm(source: &str, jit_threshold: Option<u32>) -> Result<String, String> {
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
    if let Some(t) = jit_threshold {
        vm.jit.set_threshold(t);
    }
    vm.run(function)
        .map(|v| format!("{}", v))
        .map_err(|e| e.message)
}

fn assert_parity(source: &str, expected: &str) {
    for jit in [None, Some(1)] {
        let got = run_vm(source, jit).unwrap_or_else(|e| panic!("jit={jit:?} failed: {e}"));
        assert_eq!(got, expected, "jit={jit:?} for: {source}");
    }
}

/// Fails on both backends with `expected` somewhere in the error.
fn assert_error(source: &str, expected: &str) {
    for jit in [None, Some(1)] {
        let err = run_vm(source, jit).expect_err("should fail");
        assert!(
            err.contains(expected),
            "jit={jit:?}: expected {expected:?} in error, got: {err}"
        );
    }
}

#[test]
fn original_stays_changeable() {
    assert_parity(
        r#"
fun f() {
    arr := [1, 2, 3]
    lock static_arr = arr
    push(arr, 4)
    (arr, static_arr)
}
f()
"#,
        "([1, 2, 3, 4], [1, 2, 3])",
    );
}

#[test]
fn push_on_locked_array_errors() {
    assert_error(
        "arr := [1, 2, 3]\nlock static_arr = arr\npush(static_arr, 4)\n",
        "push() cannot change a locked array",
    );
}

#[test]
fn every_in_place_builtin_is_refused() {
    for (setup, call, func) in [
        ("[1, 2]", "pop(xs)", "pop"),
        ("[1, 2]", "insert(xs, 0, 9)", "insert"),
        ("[1, 2]", "remove(xs, 0)", "remove"),
        ("[1, 2]", "extend(xs, [3])", "extend"),
        (r#"{"a": 1}"#, "clear(xs)", "clear"),
        ("set(1)", "push(xs, 2)", "push"),
    ] {
        assert_error(
            &format!("lock xs = {setup}\n{call}\n"),
            &format!("{func}() cannot change a locked"),
        );
    }
}

#[test]
fn index_and_field_writes_are_refused() {
    assert_error("lock a = [1]\na[0] = 2\n", "cannot change a locked array");
    assert_error(
        "lock m = {\"a\": 1}\nm[\"a\"] += 1\n",
        "cannot change a locked map",
    );
    assert_error(
        "lock m = {\"a\": 1}\nm.a = 2\n",
        "cannot change a locked map",
    );
    assert_error(
        "struct P {\n    v <int>\n}\nlock p = P { v: 1 }\np.v++\n",
        "cannot change a locked `P`",
    );
}

#[test]
fn lock_is_deep() {
    assert_error(
        "lock grid = [[1, 2], [3, 4]]\npush(grid[0], 5)\n",
        "push() cannot change a locked array",
    );
    assert_error(
        "lock grid = [[1, 2], [3, 4]]\ngrid[1][0] = 9\n",
        "cannot change a locked array",
    );
}

#[test]
fn locked_value_stays_locked_under_other_names() {
    // Passing it on or giving it another name does not unlock it.
    assert_error(
        "fun add_one(xs) {\n    push(xs, 1)\n}\nlock xs = [0]\nadd_one(xs)\n",
        "push() cannot change a locked array",
    );
    assert_error(
        "lock xs = [0]\nys := xs\nys[0] = 5\n",
        "cannot change a locked array",
    );
}

#[test]
fn copy_gives_a_changeable_copy() {
    assert_parity(
        r#"
fun f() {
    lock xs = [1, 2]
    ys := copy(xs)
    push(ys, 3)
    (xs, ys)
}
f()
"#,
        "([1, 2], [1, 2, 3])",
    );
}

#[test]
fn reads_work_normally() {
    assert_parity(
        r#"
lock cfg = {"w": 3, "dirs": [(0, 1), (1, 0)]}
total <int>
each d in cfg["dirs"] {
    total += d[0] + d[1]
}
total * 10 + cfg["w"]
"#,
        "23",
    );
}

#[test]
fn methods_on_locked_struct_cannot_write_fields() {
    // Warm the JIT's field caches on an unlocked instance of the same struct
    // first, so the locked one has to be caught per instance.
    assert_error(
        r#"
struct Counter {
    val <int>
}
Counter includes {
    fun inc() {
        self.val = self.val + 1
    }
    fun add(k) {
        self.val += k
    }
}
fun hammer(c, n) {
    i <int>
    repeat when i < n {
        c.add(2)
        c.inc()
        i++
    }
    c.val
}
hammer(Counter { val: 0 }, 500)
lock frozen = Counter { val: 7 }
hammer(frozen, 500)
"#,
        "cannot change a locked `Counter`",
    );
}

#[test]
fn self_referencing_value_locks() {
    assert_parity(
        r#"
fun f() {
    a := [1]
    push(a, a)
    lock b = a
    len(b) * 10 + len(b[1][1])
}
f()
"#,
        "22",
    );
}

#[test]
fn locked_variable_rejects_every_write() {
    for (decl_scope, write) in [
        ("top", "x = [2]"),
        ("top", "x := [2]"),
        ("top", "x += [2]"),
        ("fun", "x = [2]"),
        ("fun", "x := [2]"),
    ] {
        let source = if decl_scope == "top" {
            format!("lock x = [1]\n{write}\n")
        } else {
            format!("fun f() {{\n    lock x = [1]\n    {write}\n}}\nf()\n")
        };
        let err = run_vm(&source, None).expect_err("should not compile");
        assert!(err.contains("locked variable `x`"), "{write}: {err}");
    }
    let err = run_vm("lock n = 1\nn++\n", None).expect_err("should not compile");
    assert!(err.contains("cannot mutate locked variable `n`"), "{err}");
}

#[test]
fn closure_cannot_write_a_locked_variable() {
    let err = run_vm(
        "fun f() {\n    lock xs = [1]\n    g := fun() { xs = [3] }\n    g()\n}\nf()\n",
        None,
    )
    .expect_err("should not compile");
    assert!(err.contains("locked variable `xs`"), "{err}");
}

#[test]
fn write_compiled_before_the_lock_fails_at_run_time() {
    assert_error(
        "fun f() {\n    x = 2\n}\nlock x = 1\nf()\n",
        "cannot change locked variable `x`",
    );
}

#[test]
fn typed_lock_converts_like_typed_declarations() {
    assert_parity("lock limit <int> = 10\ntype(limit)\n", "INTEGER");
}

#[test]
fn lock_reports_as_immutable() {
    assert_parity("lock xs = [1]\nis_mut(xs)\n", "False");
}

#[test]
fn lock_is_still_an_ordinary_name() {
    assert_parity("lock := 5\nlock += 1\nlock\n", "6");
}
