//! Tests path containment: `__path_normalize` for lexical `.`/`..` resolution
//! and `__path_is_within` for deciding whether a resolved path is inside a
//! directory. `path.join` keeps `PathBuf::push` semantics.
//!
//! The containment comparison is component-wise, not by string prefix.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

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

fn run_on(mut vm: VM, source: &str) -> Result<String, String> {
    vm.run(compile(source))
        .map(|v| format!("{v}"))
        .map_err(|e| e.message)
}

/// Asserts every backend accepts `source` and yields `expected`.
fn assert_all_backends_yield(source: &str, expected: &str) {
    for (name, vm) in [
        ("interpreter", VM::new_interpreter()),
        ("jit", VM::new_eager_jit()),
    ] {
        let got = run_on(vm, source).unwrap_or_else(|e| panic!("{name} failed: {e}"));
        assert_eq!(got, expected, "{name} mismatch for:\n{source}");
    }
}

/// The stdlib wrappers are one-liners over these builtins, which the tests
/// exercise directly.
fn assert_normalizes(input: &str, expected: &str) {
    assert_all_backends_yield(&format!("__path_normalize(\"{input}\")"), expected);
}

fn assert_within(base: &str, candidate: &str, expected: bool) {
    assert_all_backends_yield(
        &format!("__path_is_within(\"{base}\", \"{candidate}\")"),
        if expected { "True" } else { "False" },
    );
}

#[test]
fn join_still_lets_an_absolute_component_replace_the_base() {
    assert_all_backends_yield(
        "__path_join([\"/srv/uploads\", \"/etc/passwd\"])",
        "/etc/passwd",
    );
    assert_all_backends_yield(
        "__path_join([\"/srv/uploads\", \"../../etc/passwd\"])",
        "/srv/uploads/../../etc/passwd",
    );
}

#[test]
fn the_repro_paths_are_now_detectable_as_escapes() {
    assert_within("/srv/uploads", "/etc/passwd", false);
    assert_within("/srv/uploads", "/srv/uploads/../../etc/passwd", false);
}

#[test]
fn normalize_resolves_dot_and_dotdot_without_the_filesystem() {
    assert_normalizes("/srv/uploads/../../etc/passwd", "/etc/passwd");
    assert_normalizes("/srv/./uploads/./a.txt", "/srv/uploads/a.txt");
    assert_normalizes("/srv/uploads/sub/../a.txt", "/srv/uploads/a.txt");
    assert_normalizes("a/b/../c", "a/c");
}

#[test]
fn normalize_clamps_dotdot_at_the_root_but_keeps_it_in_a_relative_path() {
    assert_normalizes("/../../etc", "/etc");
    assert_normalizes("/..", "/");
    assert_normalizes("../x", "../x");
    assert_normalizes("a/../../x", "../x");
}

#[test]
fn normalize_of_nothing_is_the_current_directory() {
    assert_normalizes(".", ".");
    assert_normalizes("a/..", ".");
    assert_normalizes("", ".");
}

#[test]
fn a_path_inside_the_base_is_within() {
    assert_within("/srv/uploads", "/srv/uploads/a.txt", true);
    assert_within("/srv/uploads", "/srv/uploads/deep/nested/a.txt", true);
    assert_within("/srv/uploads", "/srv/uploads/sub/../a.txt", true);
}

#[test]
fn the_base_itself_is_within_the_base() {
    assert_within("/srv/uploads", "/srv/uploads", true);
    assert_within("/srv/uploads", "/srv/uploads/.", true);
    assert_within("/srv/uploads", "/srv/uploads/sub/..", true);
}

#[test]
fn escaping_upward_is_outside() {
    assert_within("/srv/uploads", "/srv/uploads/../secret", false);
    assert_within("/srv/uploads", "/srv", false);
    assert_within("/srv/uploads", "/", false);
}

#[test]
fn a_sibling_sharing_a_string_prefix_is_outside() {
    assert_within("/srv/up", "/srv/uploaded/x", false);
    assert_within("/srv/uploads", "/srv/uploads-old/x", false);
    assert_within("/srv/uploads", "/srv/uploadsecret", false);
}

#[test]
fn relative_paths_resolve_against_the_working_directory() {
    assert_within("uploads", "uploads/a.txt", true);
    assert_within("uploads", "uploads/../secret", false);
    assert_within(".", "anything/inside", true);
    assert_within("uploads", "../elsewhere", false);
}

#[test]
#[cfg(unix)]
fn an_existing_symlink_out_of_the_base_is_detected() {
    let root = std::env::temp_dir().join(format!(
        "oxigen_is_within_{}_{}",
        std::process::id(),
        line!()
    ));
    let base = root.join("base");
    let outside = root.join("outside");
    std::fs::create_dir_all(&base).unwrap();
    std::fs::create_dir_all(&outside).unwrap();
    std::fs::write(outside.join("secret.txt"), b"s").unwrap();

    std::os::unix::fs::symlink(&outside, base.join("link")).unwrap();

    let base_s = base.display().to_string();
    assert_within(&base_s, &base.join("link").display().to_string(), false);
    assert_within(
        &base_s,
        &base.join("link/secret.txt").display().to_string(),
        false,
    );
    std::fs::write(base.join("ok.txt"), b"o").unwrap();
    assert_within(&base_s, &base.join("ok.txt").display().to_string(), true);
    assert_within(
        &base_s,
        &base.join("not/created/yet.txt").display().to_string(),
        true,
    );

    std::fs::remove_dir_all(&root).ok();
}
