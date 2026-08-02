//! The path module had no way to answer "is this resolved path inside that
//! directory".
//!
//! `path.join` uses `PathBuf::push` semantics — an absolute component discards
//! the base, and `..` is never resolved — so an upload handler building
//! `join([base, user_input])` could be pointed anywhere on disk, and there was
//! no normalize/resolve/is_within to check the result with. Join keeps its
//! semantics (every language's join works that way, and callers depend on it);
//! the fix is `__path_normalize` for lexical `.`/`..` resolution and
//! `__path_is_within` for the containment decision.
//!
//! The classic bug in a helper like this is comparing path strings by prefix,
//! which says "/srv/uploaded/x" is inside "/srv/up". The comparison is
//! component-wise instead — see `a_sibling_sharing_a_string_prefix_is_outside`.

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

/// The stdlib wrappers are one-liners over these; the tests exercise the
/// builtins directly so they do not depend on stdlib resolution.
fn assert_normalizes(input: &str, expected: &str) {
    assert_all_backends_yield(&format!("__path_normalize(\"{input}\")"), expected);
}

fn assert_within(base: &str, candidate: &str, expected: bool) {
    assert_all_backends_yield(
        &format!("__path_is_within(\"{base}\", \"{candidate}\")"),
        if expected { "True" } else { "False" },
    );
}

// ── the reported repro ──────────────────────────────────────────────────────

#[test]
fn join_still_lets_an_absolute_component_replace_the_base() {
    // Unchanged on purpose — silently ignoring an absolute component would be
    // its own surprise. This test pins the behaviour so the danger stays
    // visible, and is_within is what callers check the result with.
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

// ── lexical normalisation ───────────────────────────────────────────────────

#[test]
fn normalize_resolves_dot_and_dotdot_without_the_filesystem() {
    // None of these paths exist; canonicalize() would fail on every one.
    assert_normalizes("/srv/uploads/../../etc/passwd", "/etc/passwd");
    assert_normalizes("/srv/./uploads/./a.txt", "/srv/uploads/a.txt");
    assert_normalizes("/srv/uploads/sub/../a.txt", "/srv/uploads/a.txt");
    assert_normalizes("a/b/../c", "a/c");
}

#[test]
fn normalize_clamps_dotdot_at_the_root_but_keeps_it_in_a_relative_path() {
    // "/" has no parent, so climbing past it stays at the root.
    assert_normalizes("/../../etc", "/etc");
    assert_normalizes("/..", "/");
    // A relative "../x" genuinely names a sibling — dropping it would change
    // which file is meant.
    assert_normalizes("../x", "../x");
    assert_normalizes("a/../../x", "../x");
}

#[test]
fn normalize_of_nothing_is_the_current_directory() {
    assert_normalizes(".", ".");
    assert_normalizes("a/..", ".");
    assert_normalizes("", ".");
}

// ── containment ─────────────────────────────────────────────────────────────

#[test]
fn a_path_inside_the_base_is_within() {
    assert_within("/srv/uploads", "/srv/uploads/a.txt", true);
    assert_within("/srv/uploads", "/srv/uploads/deep/nested/a.txt", true);
    // Traversal that stays inside is fine — only the destination matters.
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
    // The classic bug: "/srv/uploaded/x".starts_with("/srv/up") is true as a
    // string and false as a sequence of path components.
    assert_within("/srv/up", "/srv/uploaded/x", false);
    assert_within("/srv/uploads", "/srv/uploads-old/x", false);
    assert_within("/srv/uploads", "/srv/uploadsecret", false);
}

#[test]
fn relative_paths_resolve_against_the_working_directory() {
    // Both sides become absolute before comparison, so a relative base and a
    // relative candidate still compare correctly, and mixing the two forms of
    // the same directory agrees.
    assert_within("uploads", "uploads/a.txt", true);
    assert_within("uploads", "uploads/../secret", false);
    assert_within(".", "anything/inside", true);
    assert_within("uploads", "../elsewhere", false);
}

// ── filesystem-aware resolution ─────────────────────────────────────────────

#[test]
#[cfg(unix)]
fn an_existing_symlink_out_of_the_base_is_detected() {
    // Lexical normalisation alone cannot see this: "base/link/x" has no ".." in
    // it at all. Only canonicalising the part that exists catches it.
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
    // The symlink target exists, so canonicalize() resolves it and the escape
    // is visible.
    assert_within(&base_s, &base.join("link").display().to_string(), false);
    assert_within(
        &base_s,
        &base.join("link/secret.txt").display().to_string(),
        false,
    );
    // A real file under the base still passes.
    std::fs::write(base.join("ok.txt"), b"o").unwrap();
    assert_within(&base_s, &base.join("ok.txt").display().to_string(), true);
    // And so does a file that does not exist yet under a base that does — the
    // longest-existing-ancestor fallback.
    assert_within(
        &base_s,
        &base.join("not/created/yet.txt").display().to_string(),
        true,
    );

    std::fs::remove_dir_all(&root).ok();
}
