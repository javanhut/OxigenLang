//! Tests `++`/`--` on an index or a field (`m[k]++`, `arr[i]--`, `obj.val++`).
//! The result is the old value, and the object and index are evaluated once.
//!
//! Compound assignment (`+=`, `-=`, `*=`, `/=`) shares the same place-update
//! path for index and field targets.

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
    let interp = run_vm(source, None).expect("VM interp should succeed");
    let jit = run_vm(source, Some(1)).expect("VM JIT should succeed");
    assert_eq!(interp, expected, "VM interp mismatch for: {source}");
    assert_eq!(jit, expected, "VM JIT mismatch for: {source}");
}

#[test]
fn map_value_increment_counts_letters() {
    assert_parity(
        r#"
fun counts(s) {
    m <map>
    each letter in s {
        option {
            has(m, letter) -> { m[letter]++ }
            { m[letter] = 1 }
        }
    }
    m
}
c := counts("anagram")
c["a"]
"#,
        "3",
    );
}

#[test]
fn index_postfix_yields_old_value() {
    assert_parity(
        r#"
fun f() {
    arr := [10, 20]
    old := arr[1]--
    old * 100 + arr[1]
}
f()
"#,
        "2019",
    );
}

#[test]
fn index_postfix_inside_larger_expression() {
    assert_parity(
        r#"
fun f() {
    arr := [5]
    total := 100 + arr[0]++
    total * 10 + arr[0]
}
f()
"#,
        "1056",
    );
}

#[test]
fn index_expression_is_evaluated_once() {
    assert_parity(
        r#"
calls <int>
fun key() {
    calls++
    "k"
}
fun f() {
    m <map>
    m["k"] = 0
    m[key()]++
    calls * 10 + m["k"]
}
f()
"#,
        "11",
    );
}

#[test]
fn global_map_increment() {
    assert_parity(
        r#"
g <map>
g["a"] = 5
g["a"]++
g["a"]
"#,
        "6",
    );
}

#[test]
fn field_increment() {
    assert_parity(
        r#"
struct Counter {
    val <int>
}
Counter includes {
    fun inc() { self.val++ }
}
fun f() {
    c := Counter { val: 3 }
    c.val++
    c.inc()
    c.val
}
f()
"#,
        "5",
    );
}

#[test]
fn implicit_self_field_postfix() {
    assert_parity(
        r#"
struct C {
    n <int>
}
C includes {
    fun inc() { n++ }
    fun dec_in_closure() {
        g := fun() { n-- }
        g()
    }
}
fun f() {
    c := C { n: 1 }
    c.inc()
    c.inc()
    c.dec_in_closure()
    c.n
}
f()
"#,
        "2",
    );
}

/// `m[k]++` on a missing key names the key in the error.
fn assert_missing_key_error(source: &str, expected: &str) {
    for jit in [None, Some(1)] {
        let err = run_vm(source, jit).expect_err("missing key should error");
        assert!(
            err.contains(expected),
            "jit={jit:?}: expected {expected:?} in error, got: {err}"
        );
    }
}

#[test]
fn missing_key_postfix_names_the_key() {
    assert_missing_key_error(
        r#"
fun f() {
    m <map>
    m["zz"]++
}
f()
"#,
        r#"key "zz" not found in map"#,
    );
    assert_missing_key_error(
        r#"
fun f() {
    m <map>
    m[(1, 2)]--
}
f()
"#,
        "key (1, 2) not found in map",
    );
}

#[test]
fn missing_key_plain_read_is_still_none() {
    assert_parity(
        r#"
fun f() {
    m <map>
    m["zz"]
}
f()
"#,
        "None",
    );
}

#[test]
fn hot_loop_map_increment() {
    assert_parity(
        r#"
fun f() {
    m <map>
    m["x"] = 0
    i <int>
    repeat when i < 1000 {
        m["x"]++
        i++
    }
    m["x"]
}
f()
"#,
        "1000",
    );
}

#[test]
fn compound_assign_variable() {
    assert_parity(
        r#"
fun f() {
    x := 5
    x += 2
    x *= 3
    x -= 1
    x /= 4
    x
}
f()
"#,
        "5",
    );
}

#[test]
fn compound_assign_string_and_global() {
    assert_parity(
        r#"
g := "ab"
g += "cd"
g
"#,
        "abcd",
    );
}

#[test]
fn compound_assign_index_and_field() {
    assert_parity(
        r#"
struct P {
    v <int>
}
fun f() {
    arr := [1, 2, 3]
    arr[2] *= 5
    m <map>
    m[(1, 2)] = 10
    m[(1, 2)] -= 4
    p := P { v: 7 }
    p.v += arr[2]
    p.v * 1000 + m[(1, 2)] * 10 + arr[0]
}
f()
"#,
        "22061",
    );
}

#[test]
fn compound_assign_evaluates_index_once() {
    assert_parity(
        r#"
calls <int>
fun key() {
    calls++
    "k"
}
fun f() {
    m <map>
    m["k"] = 1
    m[key()] += 10
    calls * 100 + m["k"]
}
f()
"#,
        "111",
    );
}

#[test]
fn compound_assign_implicit_self_and_upvalue() {
    assert_parity(
        r#"
struct C {
    n <int>
}
C includes {
    fun bump(k) {
        n += k
        self.n *= 2
    }
}
fun f() {
    c := C { n: 1 }
    c.bump(4)
    total := 0
    add := fun(v) { total += v }
    add(3)
    add(4)
    c.n * 100 + total
}
f()
"#,
        "1007",
    );
}

#[test]
fn compound_assign_block_value_on_right() {
    assert_parity(
        r#"
fun f() {
    m <map>
    m["k"] = 1
    m["k"] += option {
        True -> {
            y := 100
            y + 1
        }
    }
    m["k"]
}
f()
"#,
        "102",
    );
}

#[test]
fn compound_assign_missing_key_errors() {
    assert_missing_key_error(
        r#"
fun f() {
    m <map>
    m["q"] += 1
}
f()
"#,
        r#"key "q" not found in map"#,
    );
}

#[test]
fn compound_assign_respects_immutability() {
    let source = r#"
fun f() {
    x <int> = 1
    x += 1
}
f()
"#;
    let err = run_vm(source, None).expect_err("immutable += should not compile");
    assert!(err.contains("immutable"), "got: {err}");
}
