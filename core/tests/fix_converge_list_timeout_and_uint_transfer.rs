//! Tests for `core/src/concurrent.rs`:
//!
//! 1. `converge [tasks] within <ms>` applies one total deadline to the batch, and
//!    tasks past it yield a catchable `Error { tag: timeout }`, so the array keeps
//!    its length.
//! 2. `uint` values above `i64::MAX` keep their value and type across the spawn
//!    boundary: `uint("18446744073709551615")` round-trips.
//!
//! All checks run in one program in one `run()`.

use std::time::Instant;

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
    oxigen_core::concurrent::set_src(source.to_string());
    let mut vm = VM::new();
    vm.run(function)
        .map(|v| format!("{}", v))
        .unwrap_or_else(|e| format!("ERR: {}", e.message))
}

#[test]
fn converge_list_honours_its_timeout_and_uints_survive_the_round_trip() {
    let started = Instant::now();
    let out = run("fun idn(x) { x }\n\
         fun slow(x) {\n\
           s := 0\n\
           each i in range(400000000) { s = s + i }\n\
           x + s - s\n\
         }\n\
         u := uint(\"18446744073709551615\")\n\
         fast := diverge { idn(u) }\n\
         s1 := diverge { slow(1) }\n\
         s2 := diverge { slow(2) }\n\
         s3 := diverge { slow(3) }\n\
         s4 := diverge { slow(4) }\n\
         r := converge [fast, 7, s1, s2, s3, s4] within 200\n\
         [r[0] == u, r[1], is_error(r[2]), is_error(r[5]), len(r), converge [fast]]\n");
    let elapsed = started.elapsed();

    assert_eq!(out, "[True, 7, True, True, 6, [18446744073709551615]]");

    assert!(
        elapsed.as_millis() < 700,
        "`within 200` on a list must be one total deadline, not per task; took {elapsed:?}"
    );
}
