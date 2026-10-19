//! Regression: printing, comparing or dropping a cyclic or very deep value
//! must not overflow the native stack.
//!
//! `Display` and `PartialEq` recursed with no limit, so
//! `a := [1]; a.push(a); str(a)` (or `a == b`, or `println(m)` with
//! `m["self"] = m`) overflowed the stack and aborted the process. Dropping a
//! list nested millions deep still recurses (see the ignored test).

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

fn interp(source: &str) -> Result<String, String> {
    run_on(VM::new_interpreter(), source)
}

#[test]
fn printing_a_self_containing_array_terminates() {
    let out = interp("a := [1]\na.push(a)\nstr(a)").expect("str of a cyclic array");
    assert!(out.starts_with("[1, "), "{out}");
    assert!(out.contains("..."), "the cycle should be elided: {out}");
}

#[test]
fn printing_a_self_containing_map_terminates() {
    let out = interp("m := {\"k\": 1}\nm[\"self\"] = m\nstr(m)").expect("str of a cyclic map");
    assert!(out.contains("..."), "{out}");
}

#[test]
fn comparing_cyclic_arrays_terminates() {
    // Either answer is acceptable; aborting the process is not.
    let _ = interp("a := [1]\na.push(a)\nb := [1]\nb.push(b)\na == b");
    assert_eq!(interp("a := [1]\na.push(a)\na == a").as_deref(), Ok("True"));
}

#[test]
#[ignore = "needs an iterative Drop for Value; an `impl Drop for Value` forbids moving out \
            of Value in embedders' pattern matches (E0509), a public API break"]
fn deeply_nested_value_drops_without_overflow() {
    let out = interp("x := []\neach i in range(300000) { x = [x] }\nx = 0\n\"done\"");
    assert_eq!(out.as_deref(), Ok("done"));
}

#[test]
fn deeply_nested_value_prints_without_overflow() {
    let out = interp("x := []\neach i in range(5000) { x = [x] }\nlen(str(x)) > 0");
    assert_eq!(out.as_deref(), Ok("True"));
}
