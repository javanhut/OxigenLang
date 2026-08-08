//! There is a way to run a subprocess without a shell.
//!
//! `__exec()` (behind `os.exec`) hands its command to `sh -c`, so anything
//! interpolated into it is executed, not quoted: `__exec("cat", "f; echo PWNED")`
//! ran `echo PWNED`. A shell-exec builtin is a legitimate feature, but it was
//! the *only* way to start a process, so a program that had to run a command
//! over untrusted data had no correct option at all.
//!
//! `__exec_argv(program, [args])` (behind `os.exec_argv`) is the missing
//! primitive: `Command::new(program).args(argv)`, no shell anywhere, same
//! `{stdout, stderr, code}` map so the two are drop-in comparable.
//!
//! These tests shell out for real, so they are POSIX-only.
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
    let interp = run_on(VM::new_interpreter(), source).unwrap_or_else(|e| panic!("interpreter: {e}"));
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
    // `cat` is used because it does not echo its arguments back: if the `;` were
    // ever honoured, the marker would appear on stdout. It must not.
    let out = eval(&format!(
        "r := __exec_argv(\"cat\", ['{HOSTILE_ARG}'])\nr[\"stdout\"]"
    ));
    assert!(!out.contains(MARKER), "the injected command ran: {out:?}");
    assert_eq!(out, "", "cat of a bogus path writes nothing to stdout");

    // And the whole string arrived as one filename — cat names it back at us.
    let err = eval(&format!(
        "r := __exec_argv(\"cat\", ['{HOSTILE_ARG}'])\nr[\"stderr\"]"
    ));
    assert!(
        err.contains(HOSTILE_ARG),
        "argument should have arrived intact, stderr was: {err:?}"
    );
    // Failure is reported as a nonzero code, not swallowed.
    assert_ne!(
        eval(&format!(
            "r := __exec_argv(\"cat\", ['{HOSTILE_ARG}'])\nr[\"code\"]"
        )),
        "0"
    );
}

#[test]
fn the_shell_form_really_does_execute_it() {
    // The contrast that makes the test above meaningful — and the documented
    // behaviour of os.exec, which stays as it is.
    let out = eval(&format!("r := __exec('cat {HOSTILE_ARG}')\nr[\"stdout\"]"));
    assert!(
        out.contains(MARKER),
        "__exec is a shell and is expected to stay one: {out:?}"
    );
}

#[test]
fn arguments_keep_their_spaces_and_quotes() {
    // printf brackets each argument, so argument boundaries are visible: a
    // space-splitting implementation would produce more brackets than inputs.
    let out = eval("r := __exec_argv(\"printf\", [\"[%s]\", \"a b c\", '\"q\"', \"it's\"])\nr[\"stdout\"]");
    assert_eq!(out, "[a b c][\"q\"][it's]");
}

#[test]
fn an_empty_argv_runs_the_program_bare() {
    assert_eq!(eval("r := __exec_argv(\"echo\", [])\nr[\"stdout\"]"), "\n");
    assert_eq!(eval("r := __exec_argv(\"echo\", [])\nr[\"code\"]"), "0");
}

#[test]
fn a_missing_program_is_a_clean_error() {
    // Not a panic, and not a silent success with code 0.
    let err = eval_err("__exec_argv(\"oxigen-no-such-program-xyz\", [])");
    assert!(
        err.contains("oxigen-no-such-program-xyz"),
        "error should name the program: {err}"
    );
}

#[test]
fn a_nonzero_exit_code_is_reported() {
    assert_eq!(eval("r := __exec_argv(\"false\", [])\nr[\"code\"]"), "1");
    // sh is being run deliberately here, as the program, not as a hidden layer.
    assert_eq!(
        eval("r := __exec_argv(\"sh\", [\"-c\", \"exit 7\"])\nr[\"code\"]"),
        "7"
    );
}

#[test]
fn argv_must_be_an_array() {
    // The whole point is that the argument vector is a vector; accepting a
    // string here would re-open the space-splitting question.
    let err = eval_err("__exec_argv(\"echo\", \"a b\")");
    assert!(err.contains("array"), "{err}");
}

#[test]
fn exec_no_longer_takes_a_variadic_tail() {
    // It looked like argv (`__exec("cat", f)`) but space-joined into the shell
    // string, which is precisely the trap. Rejecting is loud; a program relying
    // on it gets an error naming the replacement rather than a quiet injection.
    let err = eval_err("__exec(\"echo\", \"a\")");
    assert!(err.contains("__exec_argv"), "should point at the fix: {err}");
}
