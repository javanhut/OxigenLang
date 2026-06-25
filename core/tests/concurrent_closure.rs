//! Closure transfer: `spawn`/`pmap` accept lambdas, including ones that capture
//! an enclosing variable. A capturing lambda's upvalue is snapshotted at spawn
//! (the VM closes it over the stack value) and the worker rebuilds the closure
//! by its compile-order function id. Separate test binary from
//! `concurrent_pmap` so the process-global worker pool / source aren't shared.

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
fn pmap_transfers_capturing_lambda() {
    // Lambda captures `k`; created in a function scope so the capture is a real
    // upvalue. pmap spawns it across the pool — the snapshot must carry k=10.
    let out = run(
        "introduce array\nfun work() { k := 10\narray.pmap([1, 2, 3, 4], fun(x) { x * k }) }\nwork()\n",
    );
    assert_eq!(out, "[10, 20, 30, 40]");
}
