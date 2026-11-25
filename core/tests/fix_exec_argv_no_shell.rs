//! Tests `__exec_argv(program, [args])` (behind `os.exec_argv`), which runs a
//! subprocess without a shell and returns the same `{stdout, stderr, code}` map
//! as `__exec`. These tests run real subprocesses, so they are POSIX-only.
#![cfg(unix)]

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

const MARKER: &str = "INJECTED-COMMAND-RAN";
const HOSTILE_ARG: &str = "nope.txt; echo INJECTED-COMMAND-RAN";

fn compile(source: &str) -> oxigen_core::vm::value::Function {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "parser errors:\n{}",
        parser.format_errors()
    );
    Compiler::new()
        .compile(&program)
        .unwrap_or_else(|errs| panic!("compile errors: {errs:?}"))
}

fn run_on(mut vm: VM, source: &str) -> Result<String, String> {
    vm.run(compile(source))
        .map(|v| format!("{v}"))
        .map_err(|e| e.message)
}

/// Asserts every backend accepts `source`, and returns the agreed value.
fn eval(source: &str) -> String {
    let interp =
        run_on(VM::new_interpreter(), source).unwrap_or_else(|e| panic!("interpreter: {e}"));
    let jit = run_on(VM::new_eager_jit(), source).unwrap_or_else(|e| panic!("jit: {e}"));
    assert_eq!(interp, jit, "backends disagree for:\n{source}");
    interp
}

/// Asserts every backend rejects `source`, and returns the agreed message.
fn eval_err(source: &str) -> String {
    let interp = run_on(VM::new_interpreter(), source).expect_err("interpreter should reject");
    let jit = run_on(VM::new_eager_jit(), source).expect_err("jit should reject");
    assert_eq!(interp, jit, "backends disagree for:\n{source}");
    interp
}

#[test]
fn argv_arguments_are_literal_not_shell_source() {
    let out = eval(&format!(
        "r := __exec_argv(\"cat\", ['{HOSTILE_ARG}'])\nr[\"stdout\"]"
    ));
    assert!(!out.contains(MARKER), "the injected command ran: {out:?}");
    assert_eq!(out, "", "cat of a bogus path writes nothing to stdout");

    let err = eval(&format!(
        "r := __exec_argv(\"cat\", ['{HOSTILE_ARG}'])\nr[\"stderr\"]"
    ));
    assert!(
        err.contains(HOSTILE_ARG),
        "argument should have arrived intact, stderr was: {err:?}"
    );
    assert_ne!(
        eval(&format!(
            "r := __exec_argv(\"cat\", ['{HOSTILE_ARG}'])\nr[\"code\"]"
        )),
        "0"
    );
}

#[test]
fn the_shell_form_really_does_execute_it() {
    let out = eval(&format!("r := __exec('cat {HOSTILE_ARG}')\nr[\"stdout\"]"));
    assert!(
        out.contains(MARKER),
        "__exec is a shell and is expected to stay one: {out:?}"
    );
}

#[test]
fn arguments_keep_their_spaces_and_quotes() {
    let out = eval(
        "r := __exec_argv(\"printf\", [\"[%s]\", \"a b c\", '\"q\"', \"it's\"])\nr[\"stdout\"]",
    );
    assert_eq!(out, "[a b c][\"q\"][it's]");
}

#[test]
fn an_empty_argv_runs_the_program_bare() {
    assert_eq!(eval("r := __exec_argv(\"echo\", [])\nr[\"stdout\"]"), "\n");
    assert_eq!(eval("r := __exec_argv(\"echo\", [])\nr[\"code\"]"), "0");
}

#[test]
fn a_missing_program_is_a_clean_error() {
    let err = eval_err("__exec_argv(\"oxigen-no-such-program-xyz\", [])");
    assert!(
        err.contains("oxigen-no-such-program-xyz"),
        "error should name the program: {err}"
    );
}

#[test]
fn a_nonzero_exit_code_is_reported() {
    assert_eq!(eval("r := __exec_argv(\"false\", [])\nr[\"code\"]"), "1");
    assert_eq!(
        eval("r := __exec_argv(\"sh\", [\"-c\", \"exit 7\"])\nr[\"code\"]"),
        "7"
    );
}

#[test]
fn argv_must_be_an_array() {
    let err = eval_err("__exec_argv(\"echo\", \"a b\")");
    assert!(err.contains("array"), "{err}");
}

#[test]
fn exec_no_longer_takes_a_variadic_tail() {
    let err = eval_err("__exec(\"echo\", \"a\")");
    assert!(
        err.contains("__exec_argv"),
        "should point at the fix: {err}"
    );
}
