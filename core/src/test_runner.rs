//! VM-backed runner for `oxigen test`.
//!
//! Runs each `<test>("name") { ... }` block on the bytecode VM (not the
//! tree-walker), so test execution uses the SAME semantics as `oxigen file.oxi`
//! — in particular **in-place `push`/`insert`** mutation, cross-type numeric
//! coercion, etc. The interactive REPL stays on the tree-walker.
//!
//! Each test is compiled and run INDEPENDENTLY: the program for a single test is
//! `[auto-import expect] + [top-level setup] + [test body]`, where "setup" is the
//! file's top-level statements other than `<test>` blocks and a `main { }` block
//! (which is suppressed under `oxigen test`, matching the tree-walker). Running
//! each test on a fresh VM means a failure or mutation in one test cannot affect
//! another. Test setup is normally just `fun` definitions (idempotent, no
//! output), so re-running it per test is invisible.
//!
//! A test PASSES iff its body runs without raising an error. Assertions
//! (`expect(x).eq(y)`) fail by raising an error via `<fail>`, which short-
//! circuits the body — exactly the contract the tree-walker enforced.

use crate::ast::{Expression, Program, Statement};
use crate::compiler::Compiler;
use crate::evaluator::TestOutcome;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::vm::VM;
use std::path::PathBuf;

/// Parse a snippet into its statements, discarding it on any parse error
/// (the caller's program already parsed cleanly; this is only the fixed
/// `introduce {expect} from test` prelude).
fn parse_stmts(src: &str) -> Vec<Statement> {
    let lexer = Lexer::new(src);
    let mut parser = Parser::new(lexer, src);
    let program = parser.parse_program();
    if parser.errors().is_empty() {
        program.statements
    } else {
        Vec::new()
    }
}

/// Evaluate a `<test>(...)` name expression to a display string. Names are
/// almost always string literals, so this compiles+runs the bare expression on
/// its own; a name that can't be evaluated standalone falls back to `<test>`.
fn eval_test_name(name: &Expression, source: &str, file_path: &Option<PathBuf>) -> String {
    let prog = Program {
        statements: vec![Statement::Expr(name.clone())],
    };
    match Compiler::new().compile(&prog) {
        Ok(func) => {
            let mut vm = VM::new();
            vm.set_source(source);
            if let Some(p) = file_path {
                vm.set_file(p.clone());
            }
            match vm.run(func) {
                Ok(v) => format!("{}", v),
                Err(_) => "<test>".to_string(),
            }
        }
        Err(_) => "<test>".to_string(),
    }
}

/// Run every `<test>` block in `program` on the VM, returning one outcome per
/// test (in source order). Files with no `<test>` blocks yield an empty vec.
pub fn run_vm_tests(
    program: &Program,
    source: &str,
    file_path: Option<PathBuf>,
) -> Vec<TestOutcome> {
    let has_tests = program
        .statements
        .iter()
        .any(|s| matches!(s, Statement::Test { .. }));
    if !has_tests {
        return Vec::new();
    }

    // Top-level setup: everything except `<test>` blocks and a `main { }` block
    // (suppressed during `oxigen test`).
    let setup: Vec<Statement> = program
        .statements
        .iter()
        .filter(|s| !matches!(s, Statement::Test { .. } | Statement::Main { .. }))
        .cloned()
        .collect();

    // Auto-import `expect` so test files need no explicit `introduce`.
    let mut prelude = parse_stmts("introduce {expect} from test");
    prelude.extend(setup);

    let mut outcomes = Vec::new();
    for stmt in &program.statements {
        let Statement::Test { name, body, .. } = stmt else {
            continue;
        };
        let test_name = eval_test_name(name, source, &file_path);

        let mut statements = prelude.clone();
        statements.extend(body.iter().cloned());
        let sub = Program { statements };

        let outcome = match Compiler::new().compile(&sub) {
            Ok(func) => {
                let mut vm = VM::new();
                vm.set_source(source);
                if let Some(ref p) = file_path {
                    vm.set_file(p.clone());
                }
                match vm.run(func) {
                    Ok(_) => TestOutcome {
                        name: test_name,
                        passed: true,
                        message: None,
                    },
                    Err(e) => TestOutcome {
                        name: test_name,
                        passed: false,
                        message: Some(e.message),
                    },
                }
            }
            Err(errs) => TestOutcome {
                name: test_name,
                passed: false,
                message: Some(
                    errs.first()
                        .map(|e| e.message.clone())
                        .unwrap_or_else(|| "compile error".to_string()),
                ),
            },
        };
        outcomes.push(outcome);
    }

    outcomes
}
