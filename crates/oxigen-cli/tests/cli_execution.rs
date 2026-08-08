use std::fs;
#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::time::{SystemTime, UNIX_EPOCH};

fn oxigen_bin() -> &'static str {
    env!("CARGO_BIN_EXE_oxigen")
}

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

fn write_script(dir: &Path, name: &str, source: &str) -> PathBuf {
    let path = dir.join(name);
    fs::write(&path, source).unwrap();
    path
}

fn run_oxigen(script: &Path, script_args: &[&str]) -> Output {
    Command::new(oxigen_bin())
        .current_dir(workspace_root())
        .arg(script)
        .args(script_args)
        .output()
        .unwrap()
}

fn stdout(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).into_owned()
}

fn stderr(output: &Output) -> String {
    String::from_utf8_lossy(&output.stderr).into_owned()
}

#[test]
fn forwards_script_args_to_os_module() {
    let dir = temp_dir("script-args");
    let script = write_script(
        &dir,
        "args.oxi",
        r#"introduce os

main {
    println(len(os.args()))
    each arg in os.args() {
        println(arg)
    }
}
"#,
    );

    let output = run_oxigen(&script, &["Alice", "--flag=value"]);
    assert!(output.status.success(), "stderr:\n{}", stderr(&output));
    assert_eq!(stdout(&output), "2\nAlice\n--flag=value\n");
}

#[test]
fn script_receives_flag_like_arguments_without_triggering_cli_version() {
    let dir = temp_dir("script-flag-arg");
    let script = write_script(
        &dir,
        "flag_arg.oxi",
        r#"introduce os

main {
    println(len(os.args()))
    println(os.args()[0])
}
"#,
    );

    let output = run_oxigen(&script, &["--version"]);
    assert!(output.status.success(), "stderr:\n{}", stderr(&output));
    assert_eq!(stdout(&output), "1\n--version\n");
}

#[test]
fn empty_script_args_return_empty_array() {
    let dir = temp_dir("empty-args");
    let script = write_script(
        &dir,
        "empty.oxi",
        r#"introduce os

main {
    println(len(os.args()))
}
"#,
    );

    let output = run_oxigen(&script, &[]);
    assert!(output.status.success(), "stderr:\n{}", stderr(&output));
    assert_eq!(stdout(&output), "0\n");
}

#[test]
fn cli_version_flag_still_works_before_script_path() {
    let output = Command::new(oxigen_bin())
        .current_dir(workspace_root())
        .arg("--version")
        .output()
        .unwrap();
    assert!(output.status.success(), "stderr:\n{}", stderr(&output));
    assert!(stdout(&output).starts_with("Oxigen Version: "));
}

#[test]
fn fmt_preserves_shebang_line() {
    let dir = temp_dir("fmt-shebang");
    let script = write_script(&dir, "fmt.oxi", "#!/usr/bin/env oxigen\nmain{println(1)}\n");

    let output = Command::new(oxigen_bin())
        .current_dir(workspace_root())
        .arg("fmt")
        .arg(&script)
        .output()
        .unwrap();

    assert!(output.status.success(), "stderr:\n{}", stderr(&output));

    let formatted = fs::read_to_string(&script).unwrap();
    assert!(formatted.starts_with("#!/usr/bin/env oxigen\n"));
    assert!(formatted.contains("main {"));
}

#[test]
fn fmt_preserves_location_and_indent_directives() {
    let dir = temp_dir("fmt-location");
    let script = write_script(
        &dir,
        "fmt_location.oxi",
        "#!/usr/bin/env oxigen\n#[location=/usr/bin/env oxigen]\n#[indent]\nmain:\n  println(1)\n",
    );

    let output = Command::new(oxigen_bin())
        .current_dir(workspace_root())
        .arg("fmt")
        .arg(&script)
        .output()
        .unwrap();

    assert!(output.status.success(), "stderr:\n{}", stderr(&output));

    let formatted = fs::read_to_string(&script).unwrap();
    assert!(formatted.starts_with("#!/usr/bin/env oxigen\n"));
    assert!(formatted.contains("#[location=/usr/bin/env oxigen]\n"));
    assert!(formatted.contains("#[indent]\n"));
}

#[cfg(unix)]
#[test]
fn executable_script_runs_via_shebang() {
    let dir = temp_dir("direct-exec");
    let script = write_script(
        &dir,
        "direct.oxi",
        &format!(
            "#!{}\n#[indent]\nintroduce os\n\nmain:\n    println(os.args()[0])\n",
            oxigen_bin()
        ),
    );

    let mut perms = fs::metadata(&script).unwrap().permissions();
    perms.set_mode(0o755);
    fs::set_permissions(&script, perms).unwrap();

    // Sibling tests fork while we hold a writable fd, so exec can hit ETXTBSY; retry.
    let output = loop {
        match Command::new(&script)
            .current_dir(workspace_root())
            .arg("Alice")
            .output()
        {
            Err(e) if e.kind() == std::io::ErrorKind::ExecutableFileBusy => {
                std::thread::sleep(std::time::Duration::from_millis(50));
            }
            other => break other.unwrap(),
        }
    };
    assert!(output.status.success(), "stderr:\n{}", stderr(&output));
    assert_eq!(stdout(&output), "Alice\n");
}

fn run_oxigen_subcommand(args: &[&str]) -> Output {
    Command::new(oxigen_bin())
        .current_dir(workspace_root())
        .args(args)
        .output()
        .unwrap()
}

#[test]
fn test_subcommand_runs_test_blocks() {
    let dir = temp_dir("test-cmd");
    write_script(
        &dir,
        "math_test.oxi",
        r#"fun add(a <int>, b <int>) { a + b }

<test>("addition") {
    expect(add(2, 3)).eq(5)
}

<test>("contains") {
    expect([1, 2, 3]).contains(2)
}
"#,
    );

    let output = run_oxigen_subcommand(&["test", dir.to_str().unwrap()]);
    assert!(output.status.success(), "stderr:\n{}", stderr(&output));
    let out = stdout(&output);
    assert!(out.contains("[ok]"), "stdout:\n{out}");
    assert!(out.contains("addition"), "stdout:\n{out}");
    assert!(out.contains("contains"), "stdout:\n{out}");
    assert!(out.contains("test result: ok. 2 passed"), "stdout:\n{out}");
    // Output is captured (not a TTY), so no ANSI color codes should leak.
    assert!(!out.contains('\u{1b}'), "unexpected color codes:\n{out}");
}

#[test]
fn test_subcommand_reports_failures_and_exits_nonzero() {
    let dir = temp_dir("test-cmd-fail");
    write_script(
        &dir,
        "fail_test.oxi",
        r#"<test>("bad math") {
    expect(2 + 2).eq(5)
}
"#,
    );

    let output = run_oxigen_subcommand(&["test", dir.to_str().unwrap()]);
    assert!(!output.status.success());
    let out = stdout(&output);
    assert!(out.contains("[fail] bad math"), "stdout:\n{out}");
    assert!(out.contains("expected 5 but got 4"), "stdout:\n{out}");
    assert!(out.contains("1 failed"), "stdout:\n{out}");
}

#[test]
fn vm_enforces_parameter_types() {
    // Regression: the default (VM) backend must enforce parameter type
    // annotations, not just the tree-walking interpreter.
    let dir = temp_dir("vm-type-enforce");
    let script = write_script(
        &dir,
        "bad.oxi",
        "fun f(x <int>) { x }\nprintln(f(\"not an int\"))\n",
    );
    let output = run_oxigen(&script, &[]);
    assert!(!output.status.success(), "expected non-zero exit");
    assert!(
        stderr(&output).contains("type mismatch for parameter 'x'"),
        "stderr:\n{}",
        stderr(&output)
    );
}

#[test]
fn vm_accepts_correct_parameter_types() {
    let dir = temp_dir("vm-type-ok");
    let script = write_script(
        &dir,
        "ok.oxi",
        "fun f(x <int>) { x + 1 }\nprintln(f(41))\n",
    );
    let output = run_oxigen(&script, &[]);
    assert!(output.status.success(), "stderr:\n{}", stderr(&output));
    assert_eq!(stdout(&output), "42\n");
}

#[test]
fn vm_generic_enum_param_accepts_specific_enum() {
    // `<Enum>` must accept any specific enum value on the VM, matching the
    // evaluator.
    let dir = temp_dir("vm-enum-param");
    let script = write_script(
        &dir,
        "enum.oxi",
        "enum Color { Red: 1, Green: 2 }\nfun code(c <Enum>) { c.value }\nprintln(code(Color.Green))\n",
    );
    let output = run_oxigen(&script, &[]);
    assert!(output.status.success(), "stderr:\n{}", stderr(&output));
    assert_eq!(stdout(&output), "2\n");
}

// ── command-line handling ───────────────────────────────────────────────────
// Anything that wasn't a recognised subcommand or a `.oxi` path used to fall
// through to the REPL, so a typo'd command silently opened an interactive
// prompt instead of reporting the mistake. Exit code 2 now means "the command
// line was wrong", separate from 1 ("the code failed").

const EXIT_USAGE: i32 = 2;

/// Runs oxigen with raw arguments (no implicit script path).
fn run_args(args: &[&str]) -> Output {
    Command::new(oxigen_bin())
        .current_dir(workspace_root())
        .args(args)
        .output()
        .unwrap()
}

#[test]
fn unknown_subcommand_is_an_error_not_a_repl() {
    let output = run_args(&["run", "app.oxi"]);
    assert_eq!(output.status.code(), Some(EXIT_USAGE), "{}", stderr(&output));
    let err = stderr(&output);
    assert!(err.contains("unknown subcommand `run`"), "{err}");
    // `oxigen run` was in the docs once; point at the real spelling.
    assert!(err.contains("oxigen <file.oxi>"), "{err}");
}

#[test]
fn unknown_bare_word_is_an_error() {
    let output = run_args(&["nonsense"]);
    assert_eq!(output.status.code(), Some(EXIT_USAGE), "{}", stderr(&output));
    assert!(
        stderr(&output).contains("unknown subcommand `nonsense`"),
        "{}",
        stderr(&output)
    );
}

#[test]
fn non_oxi_file_is_an_error_not_a_repl() {
    let dir = temp_dir("cli-ext");
    let path = write_script(&dir, "app.txt", "hello\n");
    let output = run_args(&[path.to_str().unwrap()]);
    assert_eq!(output.status.code(), Some(EXIT_USAGE), "{}", stderr(&output));
    assert!(
        stderr(&output).contains("not an Oxigen source file"),
        "{}",
        stderr(&output)
    );
}

#[test]
fn missing_script_is_an_error() {
    let output = run_args(&["definitely-missing.oxi"]);
    assert_eq!(output.status.code(), Some(EXIT_USAGE), "{}", stderr(&output));
    assert!(stderr(&output).contains("no such file"), "{}", stderr(&output));
}

#[test]
fn subcommands_require_their_argument() {
    for sub in ["check", "fmt"] {
        let output = run_args(&[sub]);
        assert_eq!(
            output.status.code(),
            Some(EXIT_USAGE),
            "`{sub}` with no argument: {}",
            stderr(&output)
        );
    }
}

#[test]
fn bare_invocation_still_starts_the_repl() {
    // The REPL is reachable only with no arguments at all. Feed it EOF so it
    // exits immediately rather than blocking the test.
    use std::process::Stdio;
    let mut child = Command::new(oxigen_bin())
        .current_dir(workspace_root())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    drop(child.stdin.take());
    let output = child.wait_with_output().unwrap();
    assert!(stdout(&output).contains("Oxigen REPL"), "{}", stdout(&output));
}

#[test]
fn check_exits_nonzero_when_the_file_has_errors() {
    let dir = temp_dir("cli-check");

    let broken = write_script(&dir, "broken.oxi", "main {\n  x :=\n}\n");
    let output = run_args(&["check", broken.to_str().unwrap()]);
    assert_eq!(output.status.code(), Some(1), "{}", stderr(&output));
    // The JSON still goes to stdout — the LSP parses it regardless of status.
    assert!(stdout(&output).contains("\"severity\":\"error\""), "{}", stdout(&output));

    let good = write_script(&dir, "good.oxi", "main {\n  x := 1\n}\n");
    let output = run_args(&["check", good.to_str().unwrap()]);
    assert_eq!(output.status.code(), Some(0), "{}", stderr(&output));
    assert_eq!(stdout(&output).trim(), "[]");
}

#[test]
fn jit_flags_before_the_path_are_consumed_by_oxigen() {
    let dir = temp_dir("cli-flags-pre");
    let script = write_script(
        &dir,
        "args.oxi",
        "introduce os\n\nmain {\n    println(len(os.args()))\n}\n",
    );
    for flag in ["--jit", "--no-jit"] {
        let output = run_args(&[flag, script.to_str().unwrap()]);
        assert!(output.status.success(), "{}", stderr(&output));
        assert_eq!(stdout(&output).trim(), "0", "{flag} leaked into script args");
    }
}

#[test]
fn arguments_after_the_path_belong_to_the_script() {
    // Regression: `--no-jit` was stripped from anywhere in argv, so a script
    // could never receive it as its own argument.
    let dir = temp_dir("cli-flags-post");
    let script = write_script(
        &dir,
        "args.oxi",
        "introduce os\n\nmain {\n    each a in os.args() { println(a) }\n}\n",
    );
    let output = run_args(&[script.to_str().unwrap(), "--no-jit", "extra"]);
    assert!(output.status.success(), "{}", stderr(&output));
    assert_eq!(stdout(&output), "--no-jit\nextra\n");
}

// ── diagnostics reach the editor ────────────────────────────────────────────
// `check` used to parse and stop, and the LSP is `oxigen check` read back out,
// so a compile error had never once appeared in an editor.

#[test]
fn check_reports_compile_errors_not_just_parse_errors() {
    let dir = temp_dir("check-compile");
    // Parses fine; fails to compile — `skip` needs an enclosing loop.
    let script = write_script(&dir, "c.oxi", "main {\n    skip\n}\n");
    let output = run_args(&["check", script.to_str().unwrap()]);

    assert_eq!(output.status.code(), Some(1), "{}", stderr(&output));
    let out = stdout(&output);
    assert!(out.contains("\"code\""), "diagnostic has no code:\n{out}");
    assert!(out.contains("skip"), "{out}");
    // A real location, not the `0:0` a line-only error would give.
    assert!(!out.contains("\"line\":0"), "compile error has no location:\n{out}");
}

#[test]
fn check_json_keeps_the_fields_the_lsp_reads() {
    let dir = temp_dir("check-shape");
    let script = write_script(&dir, "s.oxi", "main {\n  y :=\n}\n");
    let output = run_args(&["check", script.to_str().unwrap()]);
    let out = stdout(&output);
    for field in ["\"line\"", "\"column\"", "\"message\"", "\"severity\"", "\"suggestion\""] {
        assert!(out.contains(field), "missing {field} in:\n{out}");
    }
}

#[test]
fn explain_prints_a_codes_long_form() {
    let output = run_args(&["explain", "E0003"]);
    assert!(output.status.success(), "{}", stderr(&output));
    assert!(stdout(&output).contains("E0003"), "{}", stdout(&output));

    let unknown = run_args(&["explain", "E9999"]);
    assert_eq!(unknown.status.code(), Some(EXIT_USAGE));
}

#[test]
fn compile_errors_render_like_parse_errors() {
    // They used to print as `[line N] Compile error: ...`, so the same file
    // produced two visually unrelated kinds of error.
    let dir = temp_dir("compile-render");
    let script = write_script(&dir, "c.oxi", "main {\n    skip\n}\n");
    let output = run_oxigen(&script, &[]);
    let err = stderr(&output);
    assert!(err.contains("error[E"), "no code:\n{err}");
    assert!(err.contains("-->"), "no location line:\n{err}");
    assert!(!err.contains("Compile error:"), "old format survived:\n{err}");
}
