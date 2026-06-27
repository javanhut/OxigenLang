//! Regression: a spawned closure that references a TOP-LEVEL binding must
//! resolve it on the worker. Top-level (`main`-body) bindings compile to
//! GLOBALS, not upvalues, so the upvalue-snapshot path never sees them; the
//! worker loads only declarations. `spawn` ships a snapshot of the main
//! thread's user globals so the closure resolves them. Before the fix the
//! worker reported `undefined variable: x` and the task resolved to an error.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn run(source: &str) -> String {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "parse errors:\n{}",
        parser.format_errors()
    );
    let function = Compiler::new()
        .compile(&program)
        .map_err(|e| format!("{:?}", e))
        .expect("compile should succeed");
    oxigen_core::concurrent::set_src(source.to_string());
    let mut vm = VM::new();
    vm.run(function)
        .map(|v| format!("{}", v))
        .unwrap_or_else(|e| format!("ERR: {}", e.message))
}

#[test]
fn spawn_resolves_captured_top_level_global() {
    // `x` is a top-level global; the spawned thunk reads it. The worker must
    // see x=5 (shipped from the main thread), not "undefined variable: x".
    let out = run("x <int> := 5\nr := diverge { x + 1 }\nconverge r within 1000\n");
    assert_eq!(out, "6");
}
