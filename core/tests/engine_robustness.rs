//! A host embedding the VM (a game engine running scripts every frame) must
//! survive any script: a bug in the script is a script error, never a host
//! crash or hang.
//!
//! - `+`, `-` and `*` wrap as `docs/operators.md` defines. The interpreter
//!   used plain `l + r`, which panics in any build with overflow checks on
//!   (every debug build, and so every host's debug build).
//! - Exhausting the operand stack is a `stack overflow` error. It used to
//!   `panic!`, which aborts the host when it builds with `panic = "abort"`.
//! - A host can bound a script's loop iterations (`set_loop_budget`) and stop
//!   it from another thread (`interrupt_handle`). Both apply to JIT-compiled
//!   loops too, and neither can be caught by the script itself, or a
//!   `<guard>` would let a runaway script carry on.

use std::path::PathBuf;
use std::sync::mpsc::{self, RecvTimeoutError};
use std::thread;
use std::time::{Duration, Instant};

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

const INT_MIN: &str = "m := 9223372036854775807 + 1\n";

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

fn run_on(vm: &mut VM, source: &str) -> Result<String, String> {
    vm.run(compile(source))
        .map(|v| format!("{v}"))
        .map_err(|e| e.message)
}

fn backends() -> [(&'static str, VM); 2] {
    [
        ("interpreter", VM::new_interpreter()),
        ("jit", VM::new_eager_jit()),
    ]
}

fn assert_both_yield(source: &str, expected: &str) {
    for (name, mut vm) in backends() {
        let got = run_on(&mut vm, source).unwrap_or_else(|e| panic!("{name} failed: {e}"));
        assert_eq!(got, expected, "{name} mismatch for: {source}");
    }
}

fn assert_both_reject(source: &str, needle: &str) {
    for (name, mut vm) in backends() {
        let err =
            run_on(&mut vm, source).expect_err(&format!("{name} should have failed for: {source}"));
        assert!(
            err.contains(needle),
            "{name}: expected {needle:?} for {source}, got: {err}"
        );
    }
}

/// Runs `body` on its own thread and fails if it hasn't finished within 30
/// seconds, so a script that never stops fails the test instead of hanging
/// the run. The VM is not `Send`, so `body` builds its own.
fn within_time(body: impl FnOnce() + Send + 'static) {
    let (done, finished) = mpsc::channel();
    let worker = thread::spawn(move || {
        body();
        let _ = done.send(());
    });
    match finished.recv_timeout(Duration::from_secs(30)) {
        Ok(()) => worker.join().unwrap(),
        Err(RecvTimeoutError::Disconnected) => {
            if let Err(panic) = worker.join() {
                std::panic::resume_unwind(panic);
            }
        }
        Err(RecvTimeoutError::Timeout) => panic!("the script was never stopped"),
    }
}

// ── Integer overflow wraps, as documented ──────────────────────────────

#[test]
fn addition_wraps_to_the_minimum() {
    assert_both_yield("9223372036854775807 + 1", "-9223372036854775808");
    assert_both_yield(
        "x := 9223372036854775807\ny := 1\nx + y",
        "-9223372036854775808",
    );
}

#[test]
fn subtraction_wraps_to_the_maximum() {
    assert_both_yield(&format!("{INT_MIN}m - 1"), "9223372036854775807");
}

#[test]
fn multiplication_wraps() {
    assert_both_yield("9223372036854775807 * 2", "-2");
    assert_both_yield(&format!("{INT_MIN}m * -1"), "-9223372036854775808");
}

#[test]
fn negating_the_minimum_wraps_to_itself() {
    assert_both_yield(&format!("{INT_MIN}-m"), "-9223372036854775808");
}

/// Mixed-width arithmetic promotes `byte`/`uint` with `int` to `int`, so it
/// wraps the same way (`mixed_width_arithmetic.rs` covers the other
/// operators).
#[test]
fn mixed_width_arithmetic_wraps() {
    assert_both_yield("9223372036854775807 + byte(1)", "-9223372036854775808");
    assert_both_yield("byte(1) + 9223372036854775807", "-9223372036854775808");
    assert_both_yield("uint(1) + 9223372036854775807", "-9223372036854775808");
    assert_both_yield(&format!("{INT_MIN}m - byte(1)"), "9223372036854775807");
    assert_both_yield(&format!("{INT_MIN}m - uint(1)"), "9223372036854775807");
}

#[test]
fn wrapping_inside_a_loop_counter() {
    let src = "x := 9223372036854775806\ni := 0\nrepeat when i < 3 {\n    x = x + 1\n    i++\n}\nx";
    assert_both_yield(src, "-9223372036854775807");
}

// ── Stack exhaustion is an error, not a panic ──────────────────────────

/// Recursion is already bounded (`call_closure` refuses a call within a
/// margin of the stack's end), but one frame can still outgrow that margin
/// on its own. An array literal pushes every element before building the
/// array, and a literal nested in another keeps the outer elements pushed,
/// so five nested 65,534-element literals need about 327K slots of a 256K
/// stack. That used to reach the `panic!` in `push`.
#[test]
fn one_frame_outgrowing_the_operand_stack_is_an_error() {
    let level = vec!["True"; 65_534].join(", ");
    let mut literal = String::from("True");
    for _ in 0..5 {
        literal = format!("[{level}, {literal}]");
    }
    assert_both_reject(&format!("a := {literal}\nlen(a)"), "stack overflow");
}

// ── Loop budget ────────────────────────────────────────────────────────

const SPIN: &str = "i := 0\nrepeat when True {\n    i++\n}\ni";

#[test]
fn loop_budget_stops_an_infinite_loop() {
    within_time(|| {
        for (name, mut vm) in backends() {
            vm.set_loop_budget(Some(10_000));
            let err = run_on(&mut vm, SPIN).expect_err(&format!("{name} should stop"));
            assert!(err.contains("loop budget"), "{name}: got {err}");
        }
    });
}

#[test]
fn loop_budget_allows_loops_that_fit() {
    within_time(|| {
        for (name, mut vm) in backends() {
            vm.set_loop_budget(Some(1_000));
            let got = run_on(&mut vm, "i := 0\nrepeat when i < 500 {\n    i++\n}\ni")
                .unwrap_or_else(|e| panic!("{name} failed: {e}"));
            assert_eq!(got, "500", "{name}");
        }
    });
}

#[test]
fn loop_budget_is_spent_across_runs_until_reset() {
    within_time(|| {
        let count = "i := 0\nrepeat when i < 600 {\n    i++\n}\ni";
        for (name, mut vm) in backends() {
            vm.set_loop_budget(Some(1_000));
            run_on(&mut vm, count).unwrap_or_else(|e| panic!("{name} first run: {e}"));
            let err = run_on(&mut vm, count).expect_err(&format!("{name}: budget should run out"));
            assert!(err.contains("loop budget"), "{name}: got {err}");

            vm.set_loop_budget(Some(1_000));
            run_on(&mut vm, count).unwrap_or_else(|e| panic!("{name} after reset: {e}"));

            vm.set_loop_budget(None);
            run_on(&mut vm, count).unwrap_or_else(|e| panic!("{name} unlimited: {e}"));
            run_on(&mut vm, count).unwrap_or_else(|e| panic!("{name} unlimited: {e}"));
        }
    });
}

#[test]
fn loop_budget_cannot_be_caught_by_the_script() {
    within_time(|| {
        let src = "fun spin() {\n    i := 0\n    repeat when True {\n        i++\n    }\n    i\n}\nx := spin() <guard>(\"caught\")\nx";
        for (name, mut vm) in backends() {
            vm.set_loop_budget(Some(10_000));
            let err = run_on(&mut vm, src).expect_err(&format!("{name}: guard must not catch it"));
            assert!(err.contains("loop budget"), "{name}: got {err}");
        }
    });
}

// ── Interrupt ──────────────────────────────────────────────────────────

fn run_interrupted(vm: &mut VM, source: &str) -> (Result<String, String>, Duration) {
    let handle = vm.interrupt_handle();
    let trigger = thread::spawn(move || {
        thread::sleep(Duration::from_millis(100));
        handle.interrupt();
    });
    let start = Instant::now();
    let result = run_on(vm, source);
    let elapsed = start.elapsed();
    trigger.join().unwrap();
    (result, elapsed)
}

#[test]
fn interrupt_from_another_thread_stops_an_infinite_loop() {
    within_time(|| {
        for (name, mut vm) in backends() {
            let (result, elapsed) = run_interrupted(&mut vm, SPIN);
            let err = result.expect_err(&format!("{name} should be interrupted"));
            assert!(err.contains("interrupted"), "{name}: got {err}");
            assert!(elapsed < Duration::from_secs(5), "{name}: took {elapsed:?}");
        }
    });
}

#[test]
fn interrupt_cannot_be_caught_by_the_script() {
    within_time(|| {
        let src = "fun spin() {\n    i := 0\n    repeat when True {\n        i++\n    }\n    i\n}\nx := spin() <guard>(\"caught\")\nx";
        for (name, mut vm) in backends() {
            let (result, _) = run_interrupted(&mut vm, src);
            let err = result.expect_err(&format!("{name}: guard must not catch it"));
            assert!(err.contains("interrupted"), "{name}: got {err}");
        }
    });
}

#[test]
fn an_interrupt_is_spent_once_delivered() {
    within_time(|| {
        for (name, mut vm) in backends() {
            let (result, _) = run_interrupted(&mut vm, SPIN);
            assert!(result.is_err(), "{name}: should be interrupted");
            let got = run_on(&mut vm, "1 + 1").unwrap_or_else(|e| panic!("{name} next run: {e}"));
            assert_eq!(got, "2", "{name}");
        }
    });
}

/// An all-integer function with a loop is the shape the JIT's lean tier
/// compiles: raw machine integers, no VM pointer. Its loop must still stop.
const INT_ONLY_SPIN: &str = "fun spin(n) {\n    i := 0\n    repeat when i >= 0 {\n        i = i + n\n        i = i - n\n    }\n    i\n}\nspin(1)\nspin(1)\nspin(1)";

#[test]
fn an_integer_only_loop_obeys_the_loop_budget() {
    within_time(|| {
        for (name, mut vm) in backends() {
            vm.set_loop_budget(Some(100_000));
            let err = run_on(&mut vm, INT_ONLY_SPIN).expect_err(&format!("{name} should stop"));
            assert!(err.contains("loop budget"), "{name}: got {err}");
        }
    });
}

#[test]
fn an_integer_only_loop_can_be_interrupted() {
    within_time(|| {
        for (name, mut vm) in backends() {
            let (result, elapsed) = run_interrupted(&mut vm, INT_ONLY_SPIN);
            let err = result.expect_err(&format!("{name} should be interrupted"));
            assert!(err.contains("interrupted"), "{name}: got {err}");
            assert!(elapsed < Duration::from_secs(5), "{name}: took {elapsed:?}");
        }
    });
}

/// A fresh directory holding `spin_mod.oxi`, a module whose top level never
/// finishes. Imported modules run in a child VM, which must inherit the
/// importer's limits.
fn spinning_module(test: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("oxigen-{test}-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(dir.join("spin_mod.oxi"), SPIN).unwrap();
    dir
}

const IMPORT_SPIN: &str = "x := 1\nintroduce .spin_mod\nx";

#[test]
fn a_modules_top_level_loop_obeys_the_loop_budget() {
    within_time(|| {
        let dir = spinning_module("budget");
        for (name, mut vm) in backends() {
            vm.set_file(dir.join("main.oxi"));
            vm.set_loop_budget(Some(10_000));
            let err = run_on(&mut vm, IMPORT_SPIN).expect_err(&format!("{name} should stop"));
            assert!(err.contains("loop budget"), "{name}: got {err}");
        }
    });
}

#[test]
fn a_modules_top_level_loop_can_be_interrupted() {
    within_time(|| {
        let dir = spinning_module("interrupt");
        for (name, mut vm) in backends() {
            vm.set_file(dir.join("main.oxi"));
            let (result, _) = run_interrupted(&mut vm, IMPORT_SPIN);
            let err = result.expect_err(&format!("{name} should be interrupted"));
            assert!(err.contains("interrupted"), "{name}: got {err}");
        }
    });
}

#[test]
fn a_guarded_import_cannot_catch_the_loop_budget() {
    within_time(|| {
        let dir = spinning_module("guarded");
        let src =
            "fun load() {\n    introduce .spin_mod\n    1\n}\nx := load() <guard>(\"caught\")\nx";
        for (name, mut vm) in backends() {
            vm.set_file(dir.join("main.oxi"));
            vm.set_loop_budget(Some(10_000));
            let err = run_on(&mut vm, src).expect_err(&format!("{name}: guard must not catch it"));
            assert!(err.contains("loop budget"), "{name}: got {err}");
        }
    });
}

/// The JIT half of the tests above only means something if the loops really
/// were compiled, not left to the interpreter.
#[cfg(feature = "jit")]
#[test]
fn the_spinning_loops_are_jit_compiled() {
    within_time(|| {
        for src in [SPIN, INT_ONLY_SPIN] {
            let mut vm = VM::new_eager_jit();
            vm.set_loop_budget(Some(100_000));
            assert!(run_on(&mut vm, src).is_err());
            assert!(
                vm.jit.compiled_ok_count() > 0,
                "nothing was compiled for: {src}"
            );
        }
    });
}

#[test]
fn interrupt_handle_is_send_and_sync() {
    fn assert_send_sync<T: Send + Sync + Clone + 'static>(_: &T) {}
    let vm = VM::new_interpreter();
    assert_send_sync(&vm.interrupt_handle());
}
