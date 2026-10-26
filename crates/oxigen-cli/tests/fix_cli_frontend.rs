//! Command-line behaviour around `fmt`, `check` and `test` that the exit-code
//! contract (0 success, 1 the code failed, 2 the command line was wrong) and
//! the "fmt never damages a file" rule did not hold for.
//!
//! - A non-UTF-8 argument panicked (exit 134) inside `std::env::args`.
//! - File discovery followed directory symlinks, so a `sub/up -> ..` link ran
//!   the same tests dozens of times and let `fmt` rewrite files outside the
//!   tree it was pointed at.
//! - A missing file for `test`/`check`, or a directory named `x.oxi`, exited 1
//!   instead of 2; `test --help` was taken as a path; an unknown option was
//!   reported as an unknown subcommand.
//! - `fmt` disagreed with the lexer about the header: `#[location=...]` with
//!   trailing whitespace was dropped, and `#[indent]` after a blank line (or
//!   followed by spaces) was removed and the file converted to brace style.
//! - `fmt` stopped at the first file with a syntax error, half-way through a
//!   batch, and `--check` was only recognised straight after `fmt`.
//! - `check` ran on the main thread and aborted on deeply nested input.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::time::{SystemTime, UNIX_EPOCH};

fn oxigen() -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_oxigen"));
    cmd.env("NO_COLOR", "1");
    cmd
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

fn write(dir: &Path, name: &str, source: &str) -> PathBuf {
    let path = dir.join(name);
    fs::write(&path, source).unwrap();
    path
}

fn code(output: &Output) -> i32 {
    output.status.code().unwrap_or(-1)
}

fn stdout(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).into_owned()
}

fn stderr(output: &Output) -> String {
    String::from_utf8_lossy(&output.stderr).into_owned()
}

// ── #15: non-UTF-8 arguments ──

#[cfg(unix)]
#[test]
fn non_utf8_argument_is_a_usage_error() {
    use std::ffi::OsStr;
    use std::os::unix::ffi::OsStrExt;
    let dir = temp_dir("non-utf8");
    let script = write(&dir, "ok.oxi", "println(1)\n");
    let out = oxigen()
        .arg(&script)
        .arg(OsStr::from_bytes(b"\xff"))
        .output()
        .unwrap();
    assert_eq!(code(&out), 2, "stderr: {}", stderr(&out));
    assert!(!stderr(&out).contains("panicked"), "{}", stderr(&out));
}

// ── #18: symlinked directories ──

#[cfg(unix)]
#[test]
fn test_discovery_does_not_follow_directory_symlinks() {
    let dir = temp_dir("test-symlink");
    write(
        &dir,
        "x_test.oxi",
        "<test>(\"one\") {\n    expect(1).eq(1)\n}\n",
    );
    fs::create_dir_all(dir.join("sub")).unwrap();
    std::os::unix::fs::symlink("..", dir.join("sub").join("up")).unwrap();
    // The test runner finds the stdlib (`expect`) relative to the workspace.
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
    let out = oxigen()
        .current_dir(root)
        .arg("test")
        .arg(&dir)
        .output()
        .unwrap();
    assert!(
        stdout(&out).contains("1 passed"),
        "ran tests through a symlink loop:\n{}",
        stdout(&out)
    );
}

#[cfg(unix)]
#[test]
fn fmt_does_not_follow_directory_symlinks() {
    let dir = temp_dir("fmt-symlink");
    let outside = temp_dir("fmt-symlink-outside");
    let victim = write(&outside, "victim.oxi", "x:=1\n");
    write(&dir, "a.oxi", "y := 2\n");
    std::os::unix::fs::symlink(&outside, dir.join("link")).unwrap();
    let out = oxigen().arg("fmt").arg(&dir).output().unwrap();
    assert_eq!(code(&out), 0, "{}", stderr(&out));
    assert_eq!(
        fs::read_to_string(&victim).unwrap(),
        "x:=1\n",
        "fmt rewrote a file outside the tree through a symlink"
    );
}

// ── #19: exit codes ──

#[test]
fn missing_paths_are_usage_errors() {
    let out = oxigen()
        .args(["test", "definitely-missing.oxi"])
        .output()
        .unwrap();
    assert_eq!(code(&out), 2, "test: {}", stderr(&out));
    let out = oxigen()
        .args(["check", "definitely-missing.oxi"])
        .output()
        .unwrap();
    assert_eq!(code(&out), 2, "check: {}", stderr(&out));
}

#[test]
fn a_directory_named_like_a_script_is_a_usage_error() {
    let dir = temp_dir("dir-oxi");
    let fake = dir.join("dir.oxi");
    fs::create_dir_all(&fake).unwrap();
    let out = oxigen().arg(&fake).output().unwrap();
    assert_eq!(code(&out), 2, "{}", stderr(&out));
}

#[test]
fn test_help_prints_usage() {
    let out = oxigen().args(["test", "--help"]).output().unwrap();
    assert_eq!(code(&out), 0, "{}", stderr(&out));
    assert!(stdout(&out).contains("Usage"), "{}", stdout(&out));
}

#[test]
fn unknown_option_is_reported_as_an_option() {
    let out = oxigen().args(["--bogus", "x.oxi"]).output().unwrap();
    assert_eq!(code(&out), 2);
    assert!(stderr(&out).contains("unknown option"), "{}", stderr(&out));
}

// ── M9: fmt keeps the header the lexer reads ──

#[test]
fn fmt_keeps_a_location_directive_with_trailing_space() {
    let dir = temp_dir("fmt-location");
    let file = write(
        &dir,
        "a.oxi",
        "#[location=/usr/bin/env oxigen] \nprintln(1)\n",
    );
    let out = oxigen().arg("fmt").arg(&file).output().unwrap();
    assert_eq!(code(&out), 0, "{}", stderr(&out));
    assert!(
        fs::read_to_string(&file)
            .unwrap()
            .contains("#[location=/usr/bin/env oxigen]"),
        "location directive dropped:\n{}",
        fs::read_to_string(&file).unwrap()
    );
}

#[test]
fn fmt_keeps_indent_mode_after_a_blank_line_or_trailing_space() {
    for source in [
        "\n#[indent]\nfun f(a):\n    println(a)\nf(1)\n",
        "#[indent]   \nfun f(a):\n    println(a)\nf(1)\n",
    ] {
        let dir = temp_dir("fmt-indent-header");
        let file = write(&dir, "a.oxi", source);
        let out = oxigen().arg("fmt").arg(&file).output().unwrap();
        assert_eq!(code(&out), 0, "{}", stderr(&out));
        let formatted = fs::read_to_string(&file).unwrap();
        assert!(
            formatted.contains("#[indent]"),
            "directive dropped:\n{formatted}"
        );
        assert!(
            formatted.contains("fun f(a):"),
            "converted to brace style:\n{formatted}"
        );
        let run = oxigen().arg(&file).output().unwrap();
        assert_eq!(stdout(&run), "1\n", "{}", stderr(&run));
    }
}

// ── fmt batches ──

#[test]
fn fmt_reports_a_broken_file_and_still_formats_the_rest() {
    let dir = temp_dir("fmt-batch");
    let bad = write(&dir, "a_bad.oxi", "x := (\n");
    let good = write(&dir, "b_good.oxi", "y:=1\n");
    let out = oxigen().arg("fmt").arg(&dir).output().unwrap();
    assert_eq!(code(&out), 1, "{}", stderr(&out));
    assert_eq!(fs::read_to_string(&bad).unwrap(), "x := (\n");
    assert_eq!(fs::read_to_string(&good).unwrap(), "y := 1\n");
}

#[test]
fn fmt_check_flag_works_after_the_paths() {
    let dir = temp_dir("fmt-check-late");
    let file = write(&dir, "a.oxi", "y:=1\n");
    let out = oxigen()
        .arg("fmt")
        .arg(&file)
        .arg("--check")
        .output()
        .unwrap();
    assert_eq!(code(&out), 1, "{}", stderr(&out));
    assert_eq!(
        fs::read_to_string(&file).unwrap(),
        "y:=1\n",
        "--check wrote the file"
    );
}

#[test]
fn fmt_never_truncates_a_file_at_a_nul_byte() {
    let dir = temp_dir("fmt-nul");
    let source = "println(\"a\")\n\0\nprintln(\"b\")\n";
    let file = write(&dir, "a.oxi", source);
    oxigen().arg("fmt").arg(&file).output().unwrap();
    assert_eq!(
        fs::read_to_string(&file).unwrap(),
        source,
        "fmt deleted code after a NUL"
    );
}

// ── M4: check on deep input ──

#[test]
fn check_reports_deep_nesting_instead_of_aborting() {
    let dir = temp_dir("check-deep");
    let n = 20_000;
    let file = write(
        &dir,
        "deep.oxi",
        &format!("x := {}1{}\n", "(".repeat(n), ")".repeat(n)),
    );
    let out = oxigen().arg("check").arg(&file).output().unwrap();
    assert_eq!(code(&out), 1, "stderr: {}", stderr(&out));
    assert!(stdout(&out).contains("nest"), "{}", stdout(&out));
}
