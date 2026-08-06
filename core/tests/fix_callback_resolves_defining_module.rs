//! Regression: a function resolves its free names in the module it was
//! DEFINED in, never the module that happens to be calling it.
//!
//! Frames used to inherit the caller's module globals whenever the callee had
//! none of its own, and `GetGlobal` consults module globals before the script's.
//! So a handler passed into `api.serve` ran with `stdlib/api.oxi`'s globals, and
//! the script's own `json.parse(...)` resolved `json` to api's `fun json`
//! response helper — "cannot call method 'parse' on FUNCTION". The leak was
//! transitive: everything the callback called inherited it too.
//!
//! The module's own scope must still win for module code, or file-local helpers
//! break — both directions are asserted here, in the interpreter and under the
//! JIT (the emitted call sequence copied the same caller-frame pointer).

use std::path::PathBuf;

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

/// A module that defines `json` — the collision — plus a file-local helper it
/// must still be able to reach, and two functions that call back into script code.
const HELPERS: &str = "\
fun json(x) { \"module json helper\" }\n\
fun local_only() { \"module local\" }\n\
fun apply(cb) { cb() }\n\
fun call_local() { local_only() }\n";

fn run_with_helpers(main_src: &str, tag: &str) -> String {
    let dir = std::env::temp_dir().join(format!("oxi_mod_scope_{}_{}", std::process::id(), tag));
    std::fs::create_dir_all(&dir).expect("mk tempdir");
    std::fs::write(dir.join("helpers.oxi"), HELPERS).expect("write module");

    let source = main_src.to_string();
    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer, &source);
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

    let mut vm = VM::new();
    vm.set_source(&source);
    vm.set_file(dir.join("main.oxi"));
    let out = vm
        .run(function)
        .map(|v| format!("{}", v))
        .unwrap_or_else(|e| format!("ERR: {}", e.message));
    let _ = std::fs::remove_dir_all(&dir);
    out
}

#[test]
fn callback_resolves_names_in_its_own_file() {
    // `json` is the stdlib module here, even though the frame underneath belongs
    // to a module whose globals bind `json` to a function.
    let out = run_with_helpers(
        "introduce json\n\
         introduce .helpers\n\
         fun parse_it() { json.parse(\"[1, 2]\")[1] }\n\
         helpers.apply(parse_it)\n",
        "callback",
    );
    assert_eq!(
        out, "2",
        "a callback must resolve `json` in its own file, not the caller's module"
    );
}

#[test]
fn leak_is_not_transitive_through_nested_calls() {
    // The original failure surfaced two frames below the module boundary.
    let out = run_with_helpers(
        "introduce json\n\
         introduce .helpers\n\
         fun parse_it() { json.parse(\"[1, 2]\")[1] }\n\
         fun handler() { parse_it() }\n\
         helpers.apply(handler)\n",
        "nested",
    );
    assert_eq!(out, "2", "the caller's module must not leak down the chain");
}

#[test]
fn module_function_still_sees_its_own_helpers() {
    // The other direction: module code keeps module scope even when the script
    // defines the same name. Without this, imported functions lose their
    // file-local helpers.
    let out = run_with_helpers(
        "introduce .helpers\n\
         fun local_only() { \"script local\" }\n\
         helpers.call_local()\n",
        "modulescope",
    );
    assert_eq!(
        out, "module local",
        "a module function must reach its own file-local helper, not the script's"
    );
}

#[test]
fn hot_callback_keeps_its_own_module_under_jit() {
    // Enough iterations to tier up: the JIT emits its own frame push, which
    // copied the caller frame's module pointer instead of the callee's.
    let out = run_with_helpers(
        "introduce json\n\
         introduce .helpers\n\
         fun parse_it() { json.parse(\"[1, 2]\")[1] }\n\
         total := 0\n\
         each i in range(0, 300) { total := total + helpers.apply(parse_it) }\n\
         total\n",
        "jit",
    );
    assert_eq!(out, "600", "compiled calls must carry the callee's module too");
}
