//! Ratchet against the silent-failure pattern: error conditions converted into plausible-looking values.
//!
//! The test greps the source for shapes such as `unwrap_or_default()` on a `Result` and discarded `thread::Builder::spawn` results, and compares per-file counts against a frozen baseline.
//! A count that goes up or down fails the test; the baseline number is then updated to match.
//! It matches text rather than types, so it cannot tell a swallowed `Result` from a harmless `Option`.

use std::path::{Path, PathBuf};

/// Frozen per-file totals.
const BASELINE: &[(&str, usize)] = &[
    ("core/src/clock.rs", 3),
    ("core/src/compiler/slot_types.rs", 1),
    ("core/src/concurrent.rs", 7),
    ("core/src/diagnostics/render.rs", 2),
    ("core/src/jit/engine/lean.rs", 1),
    ("core/src/jit/engine/mod.rs", 9),
    ("core/src/jit/mod.rs", 5),
    ("core/src/keyinput.rs", 1),
    ("core/src/output.rs", 2),
    ("core/src/process.rs", 5),
    ("core/src/vm/builtins.rs", 3),
    ("core/src/vm/mod.rs", 5),
    ("core/src/vm/nanvalue.rs", 2),
    ("core/src/vm/value.rs", 2),
    ("crates/oxigen-cli/src/main.rs", 5),
    ("crates/oxigen-cli/src/repl/mod.rs", 1),
];

/// The shapes that turn an error into a value.
const SHAPES: &[&str] = &["unwrap_or_default()", ".ok();", "let _ = "];

/// Files where integers arrive from an Oxigen program rather than from internal bytecode, so a narrowing cast silently rewrites a user's value (such as `port as u16`, `c as u8`, `n as u32`, `n as i32`).
const BOUNDARY_FILES: &[(&str, usize)] = &[
    ("core/src/vm/builtins.rs", 7),
    ("core/src/netres/mod.rs", 1),
    ("core/src/concurrent.rs", 1),
];

/// Narrowing casts. `usize` is excluded because it is 64-bit and does not narrow from `i64`.
const NARROWING: &[&str] = &["as u8", "as u16", "as u32", "as i32"];

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("core/ always has a parent")
        .to_path_buf()
}

fn rs_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            rs_files(&path, out);
        } else if path.extension().is_some_and(|e| e == "rs")
            && path.file_name().is_some_and(|n| n != "tests.rs")
        {
            out.push(path);
        }
    }
}

/// Counts shape occurrences, ignoring `//` comments.
fn count_shapes(source: &str) -> usize {
    source
        .lines()
        .map(|line| line.split("//").next().unwrap_or(""))
        .map(|code| {
            SHAPES
                .iter()
                .map(|s| code.matches(s).count())
                .sum::<usize>()
        })
        .sum()
}

#[test]
fn no_new_silently_swallowed_errors() {
    let root = repo_root();
    let mut files = Vec::new();
    for src in ["core/src", "crates/oxigen-cli/src"] {
        rs_files(&root.join(src), &mut files);
    }
    assert!(
        !files.is_empty(),
        "found no sources under {} — this test would pass vacuously",
        root.display()
    );

    let mut added = Vec::new();
    let mut fixed = Vec::new();

    for path in &files {
        let rel = path
            .strip_prefix(&root)
            .unwrap_or(path)
            .to_string_lossy()
            .replace('\\', "/");
        let source = std::fs::read_to_string(path).expect("source is readable");
        let found = count_shapes(&source);
        let allowed = BASELINE
            .iter()
            .find(|(f, _)| *f == rel)
            .map_or(0, |(_, n)| *n);

        if found > allowed {
            added.push(format!("  {rel}: {allowed} allowed, {found} found"));
        } else if found < allowed {
            fixed.push(format!("  {rel}: baseline says {allowed}, now {found}"));
        }
    }

    assert!(
        added.is_empty(),
        "new silently-swallowed error(s):\n{}\n\n\
         An error was turned into a value, so nothing will report it at runtime.\n\
         Shapes watched: {SHAPES:?}\n\n\
         Propagate the error if it can matter. If it genuinely cannot, write a\n\
         comment at the site saying why, then raise that file's count in\n\
         BASELINE in {}.",
        added.join("\n"),
        file!(),
    );

    assert!(
        fixed.is_empty(),
        "silent failure(s) fixed — lower the baseline to lock it in:\n{}\n\n\
         This is a ratchet: leaving the number high lets the next one back in\n\
         unnoticed. Edit BASELINE in {}.",
        fixed.join("\n"),
        file!(),
    );
}

#[test]
fn no_new_narrowing_casts_at_the_language_boundary() {
    let root = repo_root();
    let mut drift = Vec::new();

    for (rel, allowed) in BOUNDARY_FILES {
        let source = std::fs::read_to_string(root.join(rel))
            .unwrap_or_else(|e| panic!("{rel} is readable: {e}"));
        let found: usize = source
            .lines()
            .map(|line| line.split("//").next().unwrap_or(""))
            .map(|code| {
                NARROWING
                    .iter()
                    .map(|c| code.matches(c).count())
                    .sum::<usize>()
            })
            .sum();
        if found != *allowed {
            let verb = if found > *allowed { "added" } else { "removed" };
            drift.push(format!(
                "  {rel}: baseline {allowed}, found {found} ({verb})"
            ));
        }
    }

    assert!(
        drift.is_empty(),
        "narrowing cast(s) changed at the language boundary:\n{}\n\n\
         A cast here silently rewrites a value that came from an Oxigen program:\n\
         `os.exit(256)` exited 0, ``byte(`€`)`` returned 172, `chr(4294967393)`\n\
         returned `a`, and `net.connect(host, 74626)` reached port 9090.\n\n\
         Range-check and return an error instead of casting. Note that checking\n\
         the value *after* the cast does not work — the cast is what destroys\n\
         the evidence, and a wrapped value can land back inside the valid range.\n\
         If a cast is genuinely safe, say why in a comment and update\n\
         BOUNDARY_FILES in {}.",
        drift.join("\n"),
        file!(),
    );
}
