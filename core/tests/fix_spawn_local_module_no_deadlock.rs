//! Regression: spawning when the program imports a LOCAL module must not
//! deadlock. Workers rebuild the program's declarations — including
//! `introduce .mod` — but never set their file path, so relative module
//! resolution failed at worker init. Every worker died, the queued task was
//! never served, and `converge`/join blocked forever. Fixes: workers set the
//! main file path (so local imports resolve), and a worker that still fails to
//! build replies to tasks with the error instead of dying and orphaning them.
//!
//! A watchdog thread turns a regression (deadlock) into a fast test failure
//! rather than a hung CI job.

use std::path::PathBuf;
use std::sync::mpsc;
use std::time::Duration;

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn run_in_dir(source: String, file: PathBuf) -> String {
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
    oxigen_core::concurrent::set_src(source.clone());
    oxigen_core::concurrent::set_main_file(file.clone());
    let mut vm = VM::new();
    vm.set_source(&source);
    vm.set_file(file);
    vm.run(function)
        .map(|v| format!("{}", v))
        .unwrap_or_else(|e| format!("ERR: {}", e.message))
}

#[test]
fn spawn_with_local_module_import_does_not_deadlock() {
    let dir = std::env::temp_dir().join(format!("oxi_spawn_mod_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("mk tempdir");
    // A local module with TOP-LEVEL executable code — the case that used to
    // sink worker init. It must NOT define `n`, so the spawned thunk's `n`
    // can only come from the main thread's shipped globals.
    std::fs::write(
        dir.join("mcp.oxi"),
        "introduce json\ng <str> := json.stringify({\"ok\": 1})\nfun mcp_id(x) { x }\n",
    )
    .expect("write module");
    let main_path = dir.join("main.oxi");
    let source =
        "introduce json\nintroduce .mcp\nn <int> := 41\nr := diverge { n + 1 }\nconverge r within 2000\n"
            .to_string();

    let (tx, rx) = mpsc::channel();
    std::thread::spawn(move || {
        let _ = tx.send(run_in_dir(source, main_path));
    });
    let out = match rx.recv_timeout(Duration::from_secs(10)) {
        Ok(s) => s,
        Err(_) => panic!("DEADLOCK: spawn with a local module import did not complete"),
    };
    let _ = std::fs::remove_dir_all(&dir);
    assert_eq!(out, "42");
}
