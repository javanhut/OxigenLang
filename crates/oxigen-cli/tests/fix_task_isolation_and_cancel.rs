//! What a task may observe, and how it ends.
//!
//! - Each task runs on its own VM (docs/concurrency.md). Workers used to keep
//!   one VM for every task they ran, so a module's state and the globals a
//!   previous task received leaked into the next task, and results depended
//!   on which worker picked the task up.
//! - `cancel()` was only checked when the task made an interpreted call, so
//!   a JIT-compiled loop ran to completion, and a loop with no calls could
//!   never be stopped (the program then hung at exit, waiting for it).
//! - A task that `diverge`s again runs the inner task inline, on the
//!   worker's own stack, with no depth limit: deep nesting overflowed the
//!   stack and aborted the process.
//! - `converge t within ms` didn't remember a failed task's error, so a later
//!   `converge t` reported "worker dropped without replying".
//! - Every spawn copied every global of the main program, used or not.
//! - `diverge` in the REPL ran against an empty program.
//!
//! Every case runs the real CLI in its own process, because the worker pool
//! is process-global.

use std::fs;
use std::io::Write as _;
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf()
}

fn temp_dir(label: &str) -> PathBuf {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("oxigen-{label}-{unique}"));
    fs::create_dir_all(&dir).unwrap();
    dir
}

/// Write `files` into a fresh directory and run the first one, killing it
/// after `limit` so a hang fails the test instead of the CI job.
fn run_files(label: &str, files: &[(&str, &str)], limit: Duration) -> Output {
    let dir = temp_dir(label);
    for (name, source) in files {
        fs::write(dir.join(name), source).unwrap();
    }
    run_with_limit(&dir.join(files[0].0), limit)
}

fn run_with_limit(script: &Path, limit: Duration) -> Output {
    let mut child = Command::new(env!("CARGO_BIN_EXE_oxigen"))
        .current_dir(workspace_root())
        .arg(script)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    wait_with_limit(&mut child, limit, &script.display().to_string());
    child.wait_with_output().unwrap()
}

fn wait_with_limit(child: &mut std::process::Child, limit: Duration, what: &str) {
    let start = Instant::now();
    while child.try_wait().unwrap().is_none() {
        if start.elapsed() > limit {
            let _ = child.kill();
            panic!("{what} did not finish within {limit:?}");
        }
        std::thread::sleep(Duration::from_millis(10));
    }
}

fn stdout(out: &Output) -> String {
    String::from_utf8_lossy(&out.stdout).into_owned()
}

fn stderr(out: &Output) -> String {
    String::from_utf8_lossy(&out.stderr).into_owned()
}

fn assert_prints(out: &Output, expected: &str) {
    assert!(
        out.status.success(),
        "exit {:?}\nstdout:\n{}\nstderr:\n{}",
        out.status,
        stdout(out),
        stderr(out)
    );
    assert_eq!(stdout(out), expected, "stderr:\n{}", stderr(out));
}

const LIMIT: Duration = Duration::from_secs(30);

#[test]
fn a_task_does_not_see_module_state_left_by_an_earlier_task() {
    // Run it a few times: which worker gets which task varies between runs.
    for _ in 0..3 {
        let out = run_files(
            "task-module-state",
            &[
                (
                    "main.oxi",
                    "introduce .counter\nrs := converge diverge each i in range(0, 40) { counter.inc() }\nprintln(rs)\n",
                ),
                (
                    "counter.oxi",
                    "hits := []\nfun inc() {\n    push(hits, 1)\n    len(hits)\n}\n",
                ),
            ],
            LIMIT,
        );
        let ones = format!("[{}]\n", vec!["1"; 40].join(", "));
        assert_prints(&out, &ones);
    }
}

#[test]
fn a_task_does_not_see_globals_shipped_to_an_earlier_task() {
    // The second snapshot can't carry `table` (it holds a function), so the
    // second task must not find the first task's copy of it.
    for _ in 0..3 {
        let out = run_files(
            "task-stale-global",
            &[(
                "main.oxi",
                "fun h(x) { x * 2 }\ntable := {\"a\": 1}\nt1 := diverge each i in range(0, 32) { len(table) }\nprintln(converge t1)\ntable = {\"a\": h}\nt2 := diverge each i in range(0, 32) { len(table) }\nr := converge t2\nerrs := 0\neach x in r {\n    option { is_error(x) -> { errs = errs + 1 } }\n}\nprintln(errs)\n",
            )],
            LIMIT,
        );
        let ones = format!("[{}]\n32\n", vec!["1"; 32].join(", "));
        assert_prints(&out, &ones);
    }
}

#[test]
fn cancel_stops_a_jit_compiled_loop_that_makes_calls() {
    let start = Instant::now();
    let out = run_files(
        "cancel-jit-calls",
        &[(
            "main.oxi",
            "fun tick(i) { i + 1 }\nfun spin() {\n    i := 0\n    repeat when i < 3000000000 { i = tick(i) }\n    i\n}\nt := diverge { spin() }\nr := converge t within 100\ncancel(t)\nr2 := converge t\nprintln(is_error(r2))\n",
        )],
        LIMIT,
    );
    assert_prints(&out, "True\n");
    assert!(
        start.elapsed() < Duration::from_secs(20),
        "cancel took {:?}",
        start.elapsed()
    );
}

#[test]
fn cancel_stops_a_loop_with_no_calls() {
    // Without cancellation the program hangs at exit, waiting for the task.
    let out = run_files(
        "cancel-no-calls",
        &[(
            "main.oxi",
            "fun spin() {\n    i := 0\n    repeat when True { i = i + 1 }\n}\nt := diverge { spin() }\nr := converge t within 50\nprintln(is_error(r))\ncancel(t)\nprintln(is_error(converge t))\nprintln(\"done\")\n",
        )],
        LIMIT,
    );
    assert_prints(&out, "True\nTrue\ndone\n");
}

#[test]
fn deeply_nested_diverge_is_an_error_not_a_crash() {
    let out = run_files(
        "nested-diverge",
        &[(
            "main.oxi",
            "fun r(n) {\n    option {\n        n > 100000 -> n,\n        converge diverge { r(n + 1) }\n    }\n}\nv := r(0)\nprintln(is_error(v))\nprintln(\"still running\")\n",
        )],
        LIMIT,
    );
    assert!(
        !stderr(&out).contains("overflowed its stack"),
        "stderr:\n{}",
        stderr(&out)
    );
    assert_prints(&out, "True\nstill running\n");
}

#[test]
fn modestly_nested_diverge_still_works() {
    let out = run_files(
        "nested-diverge-ok",
        &[(
            "main.oxi",
            "fun r(n) {\n    option {\n        n >= 20 -> n,\n        converge diverge { r(n + 1) }\n    }\n}\nprintln(r(0))\n",
        )],
        LIMIT,
    );
    assert_prints(&out, "20\n");
}

#[test]
fn a_failed_tasks_error_is_remembered_after_a_timed_converge() {
    let out = run_files(
        "converge-memo-error",
        &[(
            "main.oxi",
            "fun bad() { 1 / 0 }\nt := diverge { bad() }\na := converge t within 5000\nb := converge t\nc := converge t within 10\nprintln(is_error(a))\nprintln(str(a) == str(b))\nprintln(str(a) == str(c))\n",
        )],
        LIMIT,
    );
    assert_prints(&out, "True\nTrue\nTrue\n");
}

#[test]
fn a_spawn_does_not_copy_globals_the_task_never_reads() {
    // A million-element global the tasks never touch used to be deep-copied
    // into every one of them: seconds of work and a gigabyte of memory.
    let start = Instant::now();
    let out = run_files(
        "spawn-unused-global",
        &[(
            "main.oxi",
            "big := range(0, 1000000)\nr := converge diverge each i in range(0, 200) { i }\nprintln(len(r))\n",
        )],
        LIMIT,
    );
    assert_prints(&out, "200\n");
    assert!(
        start.elapsed() < Duration::from_secs(10),
        "200 trivial tasks took {:?}",
        start.elapsed()
    );
}

#[test]
fn a_task_still_reads_the_globals_it_uses() {
    // Including ones reached only through a function the task calls.
    let out = run_files(
        "spawn-used-globals",
        &[(
            "main.oxi",
            "scale := 3\noffset := 100\nfun shift(x) { x + offset }\nr := converge diverge each i in [1, 2] { shift(i * scale) }\nprintln(r)\n",
        )],
        LIMIT,
    );
    assert_prints(&out, "[103, 106]\n");
}

#[test]
fn diverge_works_in_the_repl() {
    let mut child = Command::new(env!("CARGO_BIN_EXE_oxigen"))
        .current_dir(workspace_root())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    {
        let mut stdin = child.stdin.take().unwrap();
        stdin
            .write_all(
                b"fun sq(n) { n * n }\nfun twice(n) { sq(n) * 2 }\nprintln(converge diverge { 42 })\nprintln(converge diverge each n in [1, 2, 3] { twice(n) })\n",
            )
            .unwrap();
    }
    wait_with_limit(&mut child, LIMIT, "repl");
    let out = child.wait_with_output().unwrap();
    let text = stdout(&out);
    assert!(
        text.contains("42\n"),
        "stdout:\n{text}\nstderr:\n{}",
        stderr(&out)
    );
    assert!(
        text.contains("[2, 8, 18]"),
        "stdout:\n{text}\nstderr:\n{}",
        stderr(&out)
    );
}
