//! VM-backed runner for `oxigen test`.
//!
//! Runs each `<test>("name") { ... }` block on the bytecode VM, so test
//! execution uses the SAME semantics as `oxigen file.oxi` — in particular
//! **in-place `push`/`insert`** mutation, cross-type numeric coercion, etc.
//!
//! Each test is compiled and run INDEPENDENTLY: the program for a single test is
//! `[auto-import expect] + [top-level setup] + [test body]`, where "setup" is the
//! file's top-level statements other than `<test>` blocks and a `main { }` block
//! (which is suppressed under `oxigen test`). Running each test on a fresh VM
//! means a failure or mutation in one test cannot affect another. Test setup is
//! normally just `fun` definitions (idempotent, no output), so re-running it per
//! test is invisible.
//!
//! A test PASSES iff its body runs without raising an error. Assertions
//! (`expect(x).eq(y)`) fail by raising an error via `<fail>`, which short-
//! circuits the body.

use crate::ast::{Expression, Program, Statement};
use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::vm::VM;
use std::path::PathBuf;

/// Result of executing a single `<test>("name") { ... }` block.
#[derive(Debug, Clone)]
pub struct TestOutcome {
    /// The test's name (the string passed to `<test>(...)`).
    pub name: String,
    /// Whether the test passed (its body produced no error).
    pub passed: bool,
    /// On failure, the assertion/error message that caused it.
    pub message: Option<String>,
}

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
        hidden: Vec::new(),
    };
    match Compiler::new().compile(&prog) {
        Ok(func) => {
            let mut vm = VM::new();
            vm.set_source(source);
            // A test run has no argv, but os.args() reads __args, which must exist or it raises.
            vm.set_script_args(&[]);
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

/// `<test>` blocks that are not at the top level, where the runner never
/// sees them: inside `main { }` or a function body (`where` names which).
fn nested_tests<'a>(stmts: &'a [Statement], where_: &str, out: &mut Vec<(&'a Expression, String)>) {
    for stmt in stmts {
        match stmt {
            Statement::Test { name, body, .. } => {
                out.push((name, where_.to_string()));
                nested_tests(body, where_, out);
            }
            Statement::Main { body, .. } => nested_tests(body, "`main`", out),
            Statement::Let {
                name,
                value: Expression::FunctionLiteral { body, .. },
            }
            | Statement::TypedLet {
                name,
                value: Expression::FunctionLiteral { body, .. },
                ..
            } => nested_tests(body, &format!("function `{}`", name.value), out),
            _ => {}
        }
    }
}

/// Run `program` (a test, or a test file's top level) on a fresh VM.
fn run_test_program(
    program: &Program,
    source: &str,
    file_path: &Option<PathBuf>,
) -> Result<(), String> {
    let func = Compiler::new().compile(program).map_err(|errs| {
        errs.first()
            .map(|e| e.message.clone())
            .unwrap_or_else(|| "compile error".to_string())
    })?;
    // Tasks a test spawns run against this test's program.
    crate::concurrent::register_program(&func, program, source, file_path.clone(), false);
    let mut vm = VM::new();
    vm.set_source(source);
    // See above: `os.args()` needs `__args` to exist even with no argv.
    vm.set_script_args(&[]);
    if let Some(p) = file_path {
        vm.set_file(p.clone());
    }
    // Exiting would end the whole run, reporting success for whatever
    // failed before; inside a test it is a failure instead.
    vm.register_native("__exit", |_| {
        Err("os.exit() called while testing — a test cannot exit the test runner".to_string())
    });
    // A panic is a bug in Oxigen, but it fails this test, not the run.
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run(func))) {
        Ok(Ok(_)) => Ok(()),
        Ok(Err(e)) => Err(e.message),
        Err(panic) => Err(format!(
            "internal error (Oxigen panicked): {}",
            crate::concurrent::panic_message(&panic)
        )),
    }
}

/// Run every `<test>` block in `program` on the VM, returning one outcome per
/// test (in source order). A file with no top-level `<test>` blocks yields an
/// empty vec, unless its top level fails, or it has `<test>` blocks where
/// they can't run.
pub fn run_vm_tests(
    program: &Program,
    source: &str,
    file_path: Option<PathBuf>,
) -> Vec<TestOutcome> {
    let has_tests = program
        .statements
        .iter()
        .any(|s| matches!(s, Statement::Test { .. }));

    // Everything except <test> blocks and main, which is suppressed during `oxigen test`.
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
        let sub = Program {
            statements,
            hidden: Vec::new(),
        };

        let result = run_test_program(&sub, source, &file_path);
        outcomes.push(TestOutcome {
            name: test_name,
            passed: result.is_ok(),
            message: result.err(),
        });
    }

    // A `<test>` the runner can't reach would otherwise vanish from the
    // count: "ok. 0 passed" for a file whose tests never ran.
    let mut nested = Vec::new();
    for stmt in &program.statements {
        if !matches!(stmt, Statement::Test { .. }) {
            nested_tests(std::slice::from_ref(stmt), "", &mut nested);
        }
    }
    for (name, where_) in nested {
        outcomes.push(TestOutcome {
            name: eval_test_name(name, source, &file_path),
            passed: false,
            message: Some(format!(
                "this `<test>` is inside {where_}, so it never runs — move it to the file's top level"
            )),
        });
    }

    // With no tests to run it, a failing top level would go unnoticed.
    if !has_tests && outcomes.is_empty() {
        let top = Program {
            statements: prelude,
            hidden: program.hidden.clone(),
        };
        if let Err(message) = run_test_program(&top, source, &file_path) {
            outcomes.push(TestOutcome {
                name: "<top level>".to_string(),
                passed: false,
                message: Some(message),
            });
        }
    }

    outcomes
}
