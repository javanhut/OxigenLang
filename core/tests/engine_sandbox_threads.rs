//! A restricted VM that allows threads must not let a spawned task escape
//! the restriction. Tasks run on a process-wide worker pool whose VMs are
//! built from the program source, not from the VM that spawned them, so the
//! restriction has to travel with each task.
//!
//! A separate test binary because the worker pool and its source are
//! process-global.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::{Capability, VM};

fn run_restricted(source: &str, allowed: &[Capability]) -> Result<String, String> {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(parser.errors().is_empty(), "{}", parser.format_errors());
    let function = Compiler::new().compile(&program).expect("compiles");
    oxigen_core::concurrent::set_src(source.to_string());
    let mut vm = VM::new();
    vm.restrict(allowed);
    vm.run(function)
        .map(|v| format!("{v}"))
        .map_err(|e| e.message)
}

const PEEK: &str = "fun peek() { __env_get(\"HOME\") }\nt := __spawn(peek)\n__join_task(t)";

/// A task's failure comes back from `join` as an error value, not a thrown
/// error, so look at whichever arrives.
fn outcome(allowed: &[Capability]) -> String {
    match run_restricted(PEEK, allowed) {
        Ok(value) => value,
        Err(message) => message,
    }
}

#[test]
fn a_spawned_task_runs_under_the_spawners_restriction() {
    let got = outcome(&[Capability::Threads]);
    assert!(got.contains("needs the 'process' capability"), "got: {got}");
}

/// The control: with the capability, the same task does read the
/// environment, so the refusal above comes from the restriction.
#[test]
fn a_spawned_task_has_what_the_spawner_allows() {
    let home = std::env::var("HOME").expect("HOME is set");
    let got = outcome(&[Capability::Threads, Capability::Process]);
    assert!(got.contains(&home), "got: {got}");
}
