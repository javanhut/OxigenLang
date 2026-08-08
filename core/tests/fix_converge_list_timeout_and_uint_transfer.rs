//! Three concurrency regressions, all in `core/src/concurrent.rs`.
//!
//! 1. `converge [tasks] within <ms>` accepted the timeout and ignored it: only
//!    the single-task arm of `join_with_vm` ever read `args[1]`, so the list arm
//!    blocked for as long as the slowest task wanted. `within` on a list is now
//!    one TOTAL deadline for the batch (not `ms` per task — k tasks would then
//!    legitimately block for k*ms, the very wait the timeout exists to bound),
//!    and tasks past it yield the same catchable `Error { tag: timeout }` the
//!    single-task form produces, so the array keeps its length.
//!
//! 2. `Sendable` had no `Uint` variant, so uints crossed the spawn boundary as
//!    `u as i64` and every value above `i64::MAX` came back wrapped (and typed
//!    `int` besides): `uint("18446744073709551615")` returned `-1`.
//!
//! 3. A failed `thread::Builder::spawn` was swallowed while the task was queued
//!    and `OUTSTANDING` incremented anyway — an unjoinable task and a `drain()`
//!    that spins forever at exit. That one is not reachable from a test (it
//!    needs thread creation to actually fail); the guard lives in `spawn_inner`.
//!
//! ONE program in ONE `run()`: the worker pool and the program source are
//! process-global, so a second program in this binary would resolve `diverge`
//! thunks against the first program's function table (see concurrent_autojoin).

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
    // Workers rebuild a VM from this source to resolve spawned functions.
    oxigen_core::concurrent::set_src(source.to_string());
    let mut vm = VM::new();
    vm.run(function)
        .map(|v| format!("{}", v))
        .unwrap_or_else(|e| format!("ERR: {}", e.message))
}

#[test]
fn converge_list_honours_its_timeout_and_uints_survive_the_round_trip() {
    // `slow` is a callless counting loop, so cancellation can't cut it short —
    // it is guaranteed to still be running when the deadline expires. The trip
    // count is sized to outlast 200ms by a wide margin even in a release build,
    // but to still terminate (rather than hang the suite) if the fix regresses
    // and these joins go back to blocking.
    let started = Instant::now();
    let out = run(
        "fun idn(x) { x }\n\
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
         [r[0] == u, r[1], is_error(r[2]), is_error(r[5]), len(r), converge [fast]]\n",
    );
    let elapsed = started.elapsed();

    // r[0]: a uint above i64::MAX comes back intact and still equal to the
    //       original (it used to arrive as Integer(-1)).
    // r[1]: non-task elements pass through untouched.
    // r[2], r[5]: unfinished tasks become catchable timeout errors...
    // len:  ...instead of shrinking or dropping the array.
    // last: a later, untimed `converge` of an already-resolved task still works.
    assert_eq!(out, "[True, 7, True, True, 6, [18446744073709551615]]");

    // The deadline is for the batch: four unfinished tasks must cost ~200ms in
    // total, not 200ms each. Bounded well below 4 * 200ms so the assertion is
    // about the semantics, not about machine speed.
    assert!(
        elapsed.as_millis() < 700,
        "`within 200` on a list must be one total deadline, not per task; took {elapsed:?}"
    );
}
