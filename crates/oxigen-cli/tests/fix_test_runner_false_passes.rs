//! `oxigen test` must never report success for a suite that did not pass.
//!
//! - A test (or a test file's top level) calling `os.exit(0)` ended the whole
//!   run with exit 0 and no output, failures included.
//! - `<test>` blocks nested in `main { }` or a function were silently
//!   dropped: "ok. 0 passed".
//! - A test file with no tests whose top level fails reported ok.
//! - `diverge` in a test ran against an empty program: `None` for a plain
//!   block, "function #1 not found" for a named function.

use std::fs;
use std::path::PathBuf;
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

/// Write `files` into a fresh directory and run `oxigen test` on it.
fn run_tests(label: &str, files: &[(&str, &str)]) -> Output {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("oxigen-{label}-{unique}"));
    fs::create_dir_all(&dir).unwrap();
    for (name, source) in files {
        fs::write(dir.join(name), source).unwrap();
    }
    let mut child = Command::new(env!("CARGO_BIN_EXE_oxigen"))
        .current_dir(workspace_root())
        .arg("test")
        .arg(&dir)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    let start = Instant::now();
    while child.try_wait().unwrap().is_none() {
        if start.elapsed() > Duration::from_secs(30) {
            let _ = child.kill();
            panic!("oxigen test {} did not finish", dir.display());
        }
        std::thread::sleep(Duration::from_millis(10));
    }
    child.wait_with_output().unwrap()
}

fn stdout(out: &Output) -> String {
    String::from_utf8_lossy(&out.stdout).into_owned()
}

fn stderr(out: &Output) -> String {
    String::from_utf8_lossy(&out.stderr).into_owned()
}

fn assert_failed(out: &Output) {
    assert_eq!(
        out.status.code(),
        Some(1),
        "stdout:\n{}\nstderr:\n{}",
        stdout(out),
        stderr(out)
    );
}

#[test]
fn os_exit_in_a_test_fails_that_test_and_the_run_continues() {
    let out = run_tests(
        "test-exit-in-test",
        &[(
            "x_test.oxi",
            "introduce os\n<test>(\"fails\") { expect(1).eq(2) }\n<test>(\"exits\") { os.exit(0) }\n<test>(\"after\") { expect(1).eq(1) }\n",
        )],
    );
    assert_failed(&out);
    let text = stdout(&out);
    assert!(text.contains("[fail] fails"), "stdout:\n{text}");
    assert!(text.contains("[fail] exits"), "stdout:\n{text}");
    assert!(
        text.contains("[ok]") && text.contains("after"),
        "stdout:\n{text}"
    );
}

#[test]
fn os_exit_at_a_test_files_top_level_does_not_end_the_run() {
    let out = run_tests(
        "test-exit-top",
        &[
            ("a_test.oxi", "<test>(\"a fails\") { expect(1).eq(2) }\n"),
            (
                "b_test.oxi",
                "introduce os\nos.exit(0)\n<test>(\"b\") { expect(1).eq(1) }\n",
            ),
        ],
    );
    assert_failed(&out);
    assert!(
        stdout(&out).contains("[fail] a fails"),
        "stdout:\n{}",
        stdout(&out)
    );
}

#[test]
fn a_test_nested_in_main_or_a_function_is_reported() {
    let out = run_tests(
        "test-nested",
        &[(
            "n_test.oxi",
            "main {\n    <test>(\"in main\") { expect(1).eq(1) }\n}\nfun helper() {\n    <test>(\"in fun\") { expect(1).eq(2) }\n}\n",
        )],
    );
    assert_failed(&out);
    let text = stdout(&out);
    assert!(text.contains("top level"), "stdout:\n{text}");
}

#[test]
fn a_test_file_with_no_tests_and_a_failing_top_level_fails() {
    let out = run_tests("test-top-fails", &[("t_test.oxi", "x := 1 / 0\n")]);
    assert_failed(&out);
}

#[test]
fn a_test_file_with_no_tests_and_a_clean_top_level_still_passes() {
    let out = run_tests("test-top-clean", &[("t_test.oxi", "fun helper() { 1 }\n")]);
    assert!(
        out.status.success(),
        "stdout:\n{}\nstderr:\n{}",
        stdout(&out),
        stderr(&out)
    );
}

#[test]
fn diverge_works_inside_a_test() {
    let out = run_tests(
        "test-diverge",
        &[(
            "d_test.oxi",
            "fun sq(n) { n * n }\n<test>(\"plain block\") {\n    expect(converge diverge { 42 }).eq(42)\n}\n<test>(\"named function\") {\n    t := diverge { sq(4) }\n    expect(converge t).eq(16)\n}\n<test>(\"each\") {\n    expect(converge diverge each n in [1, 2, 3] { sq(n) }).eq([1, 4, 9])\n}\n",
        )],
    );
    assert!(
        out.status.success(),
        "stdout:\n{}\nstderr:\n{}",
        stdout(&out),
        stderr(&out)
    );
    assert!(
        stdout(&out).contains("3 passed"),
        "stdout:\n{}",
        stdout(&out)
    );
}
