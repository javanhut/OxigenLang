//! A worker rebuilds a spawned function from source, so it has to find the
//! same code the spawner ran. It used to look code up by a compile-order id
//! that restarts at 0 for every compiled file, in the main program's table
//! only, and named functions by bare name in the main program's globals. A
//! `diverge` inside an imported module then ran whichever main-program
//! function had the same id or name: a different function, the whole main
//! script again (recursing until the stack overflowed), or nothing at all
//! ("function #4 not found").
//!
//! Structs had the same problem: a worker looked a struct up by bare name in
//! the main program, so a struct defined in a module could not cross threads
//! at all, and the failure was an uncatchable error that ended the program.
//!
//! Every case runs the real CLI in its own process, because the worker pool
//! is process-global.

use std::fs;
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
    let start = Instant::now();
    while child.try_wait().unwrap().is_none() {
        if start.elapsed() > limit {
            let _ = child.kill();
            let out = child.wait_with_output().unwrap();
            panic!(
                "{} did not finish within {limit:?}\nstdout:\n{}\nstderr:\n{}",
                script.display(),
                String::from_utf8_lossy(&out.stdout),
                String::from_utf8_lossy(&out.stderr)
            );
        }
        std::thread::sleep(Duration::from_millis(10));
    }
    child.wait_with_output().unwrap()
}

fn assert_prints(out: &Output, expected: &str) {
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(out.status.success(), "exit {:?}\nstdout:\n{stdout}\nstderr:\n{stderr}", out.status);
    assert_eq!(stdout, expected, "stderr:\n{stderr}");
}

const LIMIT: Duration = Duration::from_secs(30);

const MODPAR: &str = "fun tenfold(xs) {\n    diverge each x in xs { x * 10 }\n}\n";

#[test]
fn diverge_in_a_module_runs_the_modules_block_not_a_main_function() {
    let out = run_files(
        "spawn-mod-named",
        &[
            (
                "main.oxi",
                "introduce .modpar\nfun greet() { \"hello from main\" }\nprintln(modpar.tenfold([1, 2, 3]))\n",
            ),
            ("modpar.oxi", MODPAR),
        ],
        LIMIT,
    );
    assert_prints(&out, "[10, 20, 30]\n");
}

#[test]
fn diverge_in_a_module_does_not_rerun_the_main_script() {
    let out = run_files(
        "spawn-mod-script",
        &[
            ("main.oxi", "introduce .modpar\nprintln(modpar.tenfold([1, 2, 3]))\n"),
            ("modpar.oxi", MODPAR),
        ],
        LIMIT,
    );
    assert_prints(&out, "[10, 20, 30]\n");
}

#[test]
fn spawning_a_named_module_function_runs_the_modules_function() {
    let out = run_files(
        "spawn-mod-fn",
        &[
            (
                "main.oxi",
                "introduce .par\nfun work(x) { \"MAIN work {x}\" }\nprintln(par.par_named(4))\nprintln(par.spawn_named(5))\n",
            ),
            (
                "par.oxi",
                "fun work(x) { x * 10 }\nfun par_named(x) {\n    converge diverge { work(x) }\n}\nfun spawn_named(x) {\n    t := __spawn(work, x)\n    converge t\n}\n",
            ),
        ],
        LIMIT,
    );
    assert_prints(&out, "40\n50\n");
}

#[test]
fn a_capturing_block_in_a_module_runs_on_a_worker() {
    let out = run_files(
        "spawn-mod-capture",
        &[
            ("main.oxi", "introduce .scale\nprintln(scale.by([1, 2, 3], 7))\n"),
            (
                "scale.oxi",
                "fun by(xs, k) {\n    diverge each x in xs { x * k }\n}\n",
            ),
        ],
        LIMIT,
    );
    assert_prints(&out, "[7, 14, 21]\n");
}

#[test]
fn a_spawned_module_function_sees_its_own_modules_helpers() {
    // `helper` and `base` exist only in the module; main has a `helper` of
    // its own that must not be the one called.
    let out = run_files(
        "spawn-mod-helpers",
        &[
            (
                "main.oxi",
                "introduce .lib\nfun helper(x) { -1 }\nprintln(lib.run([1, 2]))\n",
            ),
            (
                "lib.oxi",
                "base := 100\nfun helper(x) { x + base }\nfun run(xs) {\n    diverge each x in xs { helper(x) }\n}\n",
            ),
        ],
        LIMIT,
    );
    assert_prints(&out, "[101, 102]\n");
}

#[test]
fn a_nested_named_function_is_not_resolved_by_name_in_main() {
    // `inner` is a named function inside another function, so it is not a
    // global anywhere. Spawning it by name used to find main's `inner`.
    let out = run_files(
        "spawn-nested-named",
        &[(
            "main.oxi",
            "fun inner(x) { \"wrong\" }\nfun outer() {\n    fun inner(x) { x * 3 }\n    converge __spawn(inner, 5)\n}\nprintln(outer())\n",
        )],
        LIMIT,
    );
    assert_prints(&out, "15\n");
}

const SHAPES: &str = "struct Point {\n    x <int>\n    y <int>\n}\nfun mk(a) { Point(x=a, y=a+1) }\n";

#[test]
fn a_struct_from_a_module_comes_back_from_a_task() {
    let out = run_files(
        "struct-mod-back",
        &[
            (
                "main.oxi",
                "introduce .shapes\np := converge diverge { shapes.mk(5) }\nprintln(p.x)\nprintln(p.y)\n",
            ),
            ("shapes.oxi", SHAPES),
        ],
        LIMIT,
    );
    assert_prints(&out, "5\n6\n");
}

#[test]
fn a_struct_from_a_module_goes_into_a_task() {
    let out = run_files(
        "struct-mod-in",
        &[
            (
                "main.oxi",
                "introduce .shapes\npt := shapes.mk(1)\nprintln(converge diverge { pt.x + 1 })\nprintln(converge diverge each v in [pt] { v.y })\n",
            ),
            ("shapes.oxi", SHAPES),
        ],
        LIMIT,
    );
    assert_prints(&out, "2\n[2]\n");
}

#[test]
fn a_struct_the_receiver_cannot_rebuild_is_a_catchable_error() {
    // Only the worker imports `shapes` (inside the task's call), so the main
    // thread has no definition of `Point` to rebuild the result with. That
    // used to raise an uncatchable error and end the program.
    let out = run_files(
        "struct-unknown",
        &[
            (
                "main.oxi",
                "fun make() {\n    introduce .shapes\n    shapes.mk(1)\n}\nr := converge diverge { make() }\nprintln(is_error(r))\nprintln(\"still running\")\n",
            ),
            ("shapes.oxi", SHAPES),
        ],
        LIMIT,
    );
    assert_prints(&out, "True\nstill running\n");
}
