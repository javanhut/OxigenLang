//! Ratchet against the silent-failure pattern.
//!
//! Two adversarial review passes over this repo turned up 14 bugs. The single
//! most common shape, by a wide margin, was **an error condition converted into
//! a plausible-looking value** — the program keeps running and nobody is told:
//!
//!   - `resp_body.read_to_string().unwrap_or_default()` — a failed HTTP body
//!     read became `{status: 200, body: ""}`, indistinguishable from a server
//!     that genuinely returned nothing.
//!   - a discarded `thread::Builder::spawn` result — the worker never started,
//!     the task was enqueued anyway, and `join()` blocked forever.
//!
//! Clippy cannot catch this. There is no lint for `unwrap_or_default()` on a
//! `Result` (verified: `manual_unwrap_or_default` and `let_underscore_must_use`
//! both report zero here), and the cast/unwrap lints that *do* exist fire 273
//! times across this crate — overwhelmingly on bytecode-internal conversions
//! that are perfectly correct. A check that noisy gets muted, so it protects
//! nothing.
//!
//! So this is a **ratchet, not a cleanup**. The 44 existing sites are frozen as
//! a baseline; most are legitimate (`Rc::into_raw` to leak a refcount on
//! purpose, `OnceLock::set` where losing the race is fine, `write!` into a
//! `String` which cannot fail). Auditing all 44 is not the point. The point is
//! that the 45th has to be a deliberate act.
//!
//! ## When this test fails
//!
//! **Count went up** — you added one. If the error genuinely cannot matter, say
//! why in a comment at the site and raise that file's number below. Raising it
//! silently is the exact habit this guards against, so the comment is the part
//! that counts.
//!
//! **Count went down** — you fixed one. Lower the number to lock the
//! improvement in. That is what makes it a ratchet rather than a high-water mark.
//!
//! Known limits: it greps text rather than types, so it cannot tell a swallowed
//! `Result` from a harmless `Option`, and per-file counts miss a same-file swap.
//! Both are accepted — this catches the careless addition, which is how all 14
//! arrived. Reach for `dylint` only if someone games the counts.

use std::path::{Path, PathBuf};

/// Frozen per-file totals. See the module docs before changing a number.
const BASELINE: &[(&str, usize)] = &[
    ("core/src/compiler/slot_types.rs", 1),
    ("core/src/concurrent.rs", 6),
    ("core/src/diagnostics/render.rs", 2),
    ("core/src/jit/engine/mod.rs", 9),
    ("core/src/jit/mod.rs", 5),
    ("core/src/keyinput.rs", 1),
    ("core/src/vm/builtins.rs", 5),
    ("core/src/vm/mod.rs", 5),
    ("core/src/vm/nanvalue.rs", 2),
    ("core/src/vm/value.rs", 3),
    ("crates/oxigen-cli/src/main.rs", 4),
    ("crates/oxigen-cli/src/repl/mod.rs", 1),
];

/// The shapes that turn an error into a value. Each one is how a real bug in
/// this repo actually looked.
const SHAPES: &[&str] = &[
    "unwrap_or_default()", // the HTTP-body bug: read failure -> ""
    ".ok();",              // Result discarded in statement position
    "let _ = ",            // Result bound to nothing
];

/// Files where integers arrive from an Oxigen *program* rather than from our
/// own bytecode. A narrowing cast here silently rewrites a user's value:
///
///   - `net.connect(host, 74626)` reached port 9090        (`port as u16`)
///   - ``byte(`€`)`` returned 172 instead of erroring       (`c as u8`)
///   - `chr(4294967393)` returned `a`                       (`n as u32` wrapped
///     back into the valid range, defeating the range check it fed)
///   - `os.exit(256)` exited **0** — a failure reported as success (`n as i32`)
///
/// Bytecode-internal casts (constant indices, slot numbers) are fine and are
/// why this cannot be a blanket clippy lint: `cast_possible_truncation` fires
/// 273 times across this crate. Scoping it to the boundary is what makes the
/// signal usable.
const BOUNDARY_FILES: &[(&str, usize)] = &[
    ("core/src/vm/builtins.rs", 7),
    ("core/src/netres/mod.rs", 1),
    ("core/src/concurrent.rs", 0),
];

/// Narrowing casts. Not `usize` — it is 64-bit here, so it does not narrow from
/// `i64`, and including it would bury the real hits under indexing arithmetic.
const NARROWING: &[&str] = &["as u8", "as u16", "as u32", "as i32"];

fn repo_root() -> PathBuf {
    // CARGO_MANIFEST_DIR is <repo>/core for this test's package.
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
            // Unit-test modules live beside the code they test and are allowed
            // to swallow freely — a discarded Result in a test is the test.
            && path.file_name().is_some_and(|n| n != "tests.rs")
        {
            out.push(path);
        }
    }
}

/// Counts shape occurrences, ignoring `//` comments so that *documenting* a
/// swallow (which this test actively wants people to do) never trips it.
fn count_shapes(source: &str) -> usize {
    source
        .lines()
        .map(|line| line.split("//").next().unwrap_or(""))
        .map(|code| SHAPES.iter().map(|s| code.matches(s).count()).sum::<usize>())
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
            .map(|code| NARROWING.iter().map(|c| code.matches(c).count()).sum::<usize>())
            .sum();
        if found != *allowed {
            let verb = if found > *allowed { "added" } else { "removed" };
            drift.push(format!("  {rel}: baseline {allowed}, found {found} ({verb})"));
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
