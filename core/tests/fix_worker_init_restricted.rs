//! A worker builds its VM by running the program's declarations, imports
//! included, and only then applied the spawner's restriction. A module's top
//! level therefore ran with every capability on each worker: a restricted
//! script could import a local module whose top level writes a file (the
//! write fails quietly on the restricted main VM) and have it succeed the
//! moment anything was spawned. The restriction now applies before the
//! worker runs any of the program.
//!
//! A separate test binary because the worker pool and its source are
//! process-global.

use std::path::PathBuf;

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::{Capability, VM};

#[test]
fn a_module_top_level_runs_restricted_on_workers_too() {
    let dir = std::env::temp_dir().join(format!("oxi_worker_restrict_{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let escaped: PathBuf = dir.join("escaped.txt");
    let _ = std::fs::remove_file(&escaped);
    std::fs::write(
        dir.join("evil.oxi"),
        format!(
            "done := __write_file(\"{}\", \"pwned\") <guard>(0)\nfun id(x) {{ x }}\n",
            escaped.display()
        ),
    )
    .unwrap();
    let main = dir.join("main.oxi");
    let source = "introduce .evil\nr := converge diverge { 1 + 1 }\nr\n".to_string();
    std::fs::write(&main, &source).unwrap();

    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer, &source);
    let program = parser.parse_program();
    assert!(parser.errors().is_empty(), "{}", parser.format_errors());
    let function = Compiler::new().compile(&program).expect("compiles");
    oxigen_core::concurrent::set_src(source.clone());
    oxigen_core::concurrent::set_main_file(main.clone());

    let mut vm = VM::new();
    vm.set_source(&source);
    vm.set_file(main);
    vm.restrict(&[Capability::Threads]);
    let result = vm.run(function).map(|v| format!("{v}")).map_err(|e| e.message);

    let wrote = escaped.exists();
    let _ = std::fs::remove_dir_all(&dir);
    assert_eq!(result, Ok("2".to_string()));
    assert!(!wrote, "a worker ran the module's top level without the restriction");
}
