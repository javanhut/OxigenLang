//! End-to-end test for `array.pmap` and the spawn/join worker pool it rides on.
//! `pmap` must run a named function across the pool and return results in input
//! order. Exercises the whole concurrency stack in one shot: spawn, the
//! `Sendable` transfer (detach/attach), the elastic worker pool, and join.

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
    // Workers rebuild a VM from this source to resolve spawned functions by name.
    oxigen_core::concurrent::set_src(source.to_string());
    let mut vm = VM::new();
    vm.run(function)
        .map(|v| format!("{}", v))
        .unwrap_or_else(|e| format!("ERR: {}", e.message))
}

#[test]
fn pmap_runs_named_fn_across_pool_in_order() {
    let out = run("introduce array\nfun dbl(x) { x * 2 }\narray.pmap([1, 2, 3, 4, 5], dbl)\n");
    assert_eq!(out, "[2, 4, 6, 8, 10]");
}
