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
    assert!(stdout(&output).starts_with("oxigen "));
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

    let output = Command::new(&script)
        .current_dir(workspace_root())
        .arg("Alice")
        .output()
        .unwrap();
    assert!(output.status.success(), "stderr:\n{}", stderr(&output));
    assert_eq!(stdout(&output), "Alice\n");
}
