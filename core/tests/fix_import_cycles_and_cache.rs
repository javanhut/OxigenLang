//! Regression: module imports share one cycle stack and one module cache
//! across the whole import graph.
//!
//! Each imported module ran in a child VM with an EMPTY import stack and
//! module cache, and the main file was never on the stack. So:
//! - `a` importing `b` importing `a` (or a file importing itself) recursed
//!   until the native stack overflowed, instead of raising the
//!   `circular import` error `docs/imports.md` promises;
//! - a module imported from two places ran twice, with two copies of its
//!   state (and a diamond-shaped graph re-ran modules exponentially).
//!
//! Also: `restrict()` must not leave a module cached from before the
//! restriction usable with its old, unrestricted builtins.

use std::path::{Path, PathBuf};

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;
use oxigen_core::vm::capabilities::Capability;

fn temp_dir(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("oxi_import_graph_{}_{}", std::process::id(), tag));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("mk tempdir");
    dir
}

fn write(dir: &Path, name: &str, src: &str) {
    std::fs::write(dir.join(name), src).expect("write module");
}

fn compile(source: &str) -> oxigen_core::vm::value::Function {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "parse errors:\n{}",
        parser.format_errors()
    );
    Compiler::new()
        .compile(&program)
        .unwrap_or_else(|e| panic!("compile errors: {e:?}"))
}

fn run_main(dir: &Path, main_src: &str, make: fn() -> VM) -> Result<String, String> {
    write(dir, "main.oxi", main_src);
    let mut vm = make();
    vm.set_source(main_src);
    vm.set_file(dir.join("main.oxi"));
    vm.run(compile(main_src))
        .map(|v| format!("{v}"))
        .map_err(|e| e.message)
}

/// A backend's name and constructor.
type Backend = (&'static str, fn() -> VM);

const BACKENDS: [Backend; 2] = [
    ("interpreter", VM::new_interpreter),
    ("eager_jit", VM::new_eager_jit),
];

#[test]
fn two_module_cycle_is_reported() {
    for (name, make) in BACKENDS {
        let dir = temp_dir(&format!("cycle2_{name}"));
        write(&dir, "a.oxi", "introduce .b\nfun fa() { 1 }\n");
        write(&dir, "b.oxi", "introduce .a\nfun fb() { 2 }\n");
        let err = run_main(&dir, "introduce .a\n1", make).expect_err(name);
        assert!(err.contains("circular import"), "{name}: {err}");
        let _ = std::fs::remove_dir_all(&dir);
    }
}

#[test]
fn self_import_is_reported() {
    for (name, make) in BACKENDS {
        let dir = temp_dir(&format!("selfimp_{name}"));
        write(&dir, "selfimp.oxi", "introduce .selfimp\nfun f() { 1 }\n");
        let err = run_main(&dir, "introduce .selfimp\n1", make).expect_err(name);
        assert!(err.contains("circular import"), "{name}: {err}");
        let _ = std::fs::remove_dir_all(&dir);
    }
}

#[test]
fn main_file_importing_itself_is_reported() {
    for (name, make) in BACKENDS {
        let dir = temp_dir(&format!("mainself_{name}"));
        let err = run_main(&dir, "introduce .main\n1", make).expect_err(name);
        assert!(err.contains("circular import"), "{name}: {err}");
        let _ = std::fs::remove_dir_all(&dir);
    }
}

#[test]
fn shared_module_is_evaluated_once_and_its_state_is_shared() {
    for (name, make) in BACKENDS {
        let dir = temp_dir(&format!("shared_{name}"));
        write(
            &dir,
            "shared.oxi",
            "runs := [0]\nruns[0] = runs[0] + 1\nhits := [0]\nfun bump() {\n  hits[0] = hits[0] + 1\n  hits[0]\n}\nfun run_count() { runs[0] }\n",
        );
        write(
            &dir,
            "user1.oxi",
            "introduce .shared\nfun go() { shared.bump() }\n",
        );
        let out = run_main(
            &dir,
            "introduce .shared\nintroduce .user1\na := shared.bump()\nb := user1.go()\nc := shared.bump()\n[a, b, c, shared.run_count()]",
            make,
        )
        .unwrap_or_else(|e| panic!("{name}: {e}"));
        assert_eq!(out, "[1, 2, 3, 1]", "{name}");
        let _ = std::fs::remove_dir_all(&dir);
    }
}

#[test]
fn diamond_import_graph_runs_each_module_once() {
    let dir = temp_dir("diamond");
    // d is reachable through b and c; it records each evaluation in a file.
    let log = dir.join("evals.log");
    write(
        &dir,
        "d.oxi",
        &format!(
            "introduce io\nio.append_file(\"{}\", \"d\\n\")\nfun dv() {{ 4 }}\n",
            log.display()
        ),
    );
    write(&dir, "b.oxi", "introduce .d\nfun bv() { d.dv() }\n");
    write(&dir, "c.oxi", "introduce .d\nfun cv() { d.dv() }\n");
    let out = run_main(
        &dir,
        "introduce .b\nintroduce .c\nintroduce .d\nb.bv() + c.cv() + d.dv()",
        VM::new_interpreter,
    )
    .unwrap_or_else(|e| panic!("diamond import failed: {e}"));
    assert_eq!(out, "12");
    let evals = std::fs::read_to_string(&log).unwrap_or_default();
    assert_eq!(
        evals.lines().count(),
        1,
        "d evaluated {} times",
        evals.lines().count()
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn restrict_drops_modules_cached_before_the_restriction() {
    let dir = temp_dir("restrict_cache");
    write(&dir, "f.txt", "secret");
    let read = format!(
        "introduce io\nio.read_file(\"{}\")",
        dir.join("f.txt").display()
    );

    let mut vm = VM::new_interpreter();
    vm.set_file(dir.join("main.oxi"));
    let first = vm
        .run(compile(&read))
        .map(|v| format!("{v}"))
        .map_err(|e| e.message);
    assert_eq!(
        first.as_deref(),
        Ok("secret"),
        "unrestricted read should work"
    );

    vm.restrict(&[Capability::Network]);
    let second = vm.run(compile(&read)).map(|v| format!("{v}"));
    match second {
        Ok(v) => assert!(
            v.contains("FileSystem") || v.to_lowercase().contains("capab"),
            "restricted VM still read the file through the cached module: {v}"
        ),
        Err(e) => assert!(
            e.message.contains("FileSystem") || e.message.to_lowercase().contains("capab"),
            "unexpected error: {}",
            e.message
        ),
    }
    let _ = std::fs::remove_dir_all(&dir);
}
