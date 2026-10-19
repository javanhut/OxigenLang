use std::env;
use std::fs;
use std::io::IsTerminal;
use std::path::PathBuf;

mod repl;

/// Single source of truth for the version banner — `--version`, `--help` and
/// the REPL's `version` command all route here so they can't drift apart.
pub fn print_version() {
    println!("Oxigen Version: {}", env!("CARGO_PKG_VERSION"));
}

use oxigen_core::compiler::Compiler;
use oxigen_core::diagnostics::{DiagnosticSink, SourceFile, render};
use oxigen_core::formatter::Formatter;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;


fn read_source(file_path: &str) -> String {
    match fs::read_to_string(file_path) {
        Ok(contents) => contents,
        Err(err) => {
            eprintln!("Error reading {}: {}", file_path, err);
            std::process::exit(1);
        }
    }
}

/// Puts the shebang and directives the lexer stripped back in front of the
/// formatted body. Asking the lexer which lines those are keeps `fmt` from
/// disagreeing with it: a `#[location=...]` with trailing space used to be
/// dropped, and an `#[indent]` after a blank line removed while the body was
/// reprinted in brace style.
fn restore_header(directives: &[String], body: String) -> String {
    if directives.is_empty() {
        body
    } else {
        format!("{}\n{body}", directives.join("\n"))
    }
}

/// Stack for the thread that parses, compiles or runs user code, so deep but
/// legal nesting is handled rather than overflowing the main thread's 8 MB.
const STACK_SIZE: usize = 256 << 20; // 256 MB

fn on_big_stack<T: Send + 'static>(f: impl FnOnce() -> T + Send + 'static) -> T {
    let handle = std::thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(f)
        .expect("failed to spawn execution thread");
    match handle.join() {
        Ok(value) => value,
        // The panic hook has already printed the message. Re-panicking here
        // exited 101, outside the documented 0/1/2 contract: an internal
        // error running the code is still "the code failed".
        Err(_) => std::process::exit(1),
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum JitMode {
    /// Default tiering: compile after 50 calls.
    Default,
    /// `--jit`: compile on first call (threshold = 1).
    Eager,
    /// `--no-jit`: never compile.
    Disabled,
}

fn run_file_vm(file_path: &str, script_args: &[String], jit_mode: JitMode) {
    let contents = read_source(file_path);
    let file_path_buf = PathBuf::from(file_path)
        .canonicalize()
        .expect("Could not resolve file path");

    // Large stack so all 16384 JIT frames fit natively and the V7 guard is the graceful limit.
    let run = {
        let script_args: Vec<String> = script_args.to_vec();
        move || {
            let lexer = Lexer::new(&contents);
            let mut parser = Parser::new(lexer, &contents);
            let program = parser.parse_program();

            oxigen_core::concurrent::set_src(contents.clone());
            oxigen_core::concurrent::set_main_file(file_path_buf.clone());

            let errors = parser.errors();
            if !errors.is_empty() {
                eprintln!("{}", parser.format_errors());
                std::process::exit(1);
            }

            let compiler = Compiler::new();
            let function = match compiler.compile(&program) {
                Ok(f) => f,
                Err(errors) => {
                    // Render through the shared renderer so a compile error looks like every other diagnostic.
                    let source = SourceFile::named(
                        file_path_buf.display().to_string(),
                        contents.clone(),
                    );
                    for err in errors {
                        eprint!("{}", render::render_human(&err.into_diagnostic(), &source));
                    }
                    std::process::exit(1);
                }
            };

            let mut vm = VM::new();
            match jit_mode {
                JitMode::Eager => {
                    vm.jit.set_threshold(1);
                    vm.jit.set_loop_threshold(1);
                }
                JitMode::Disabled => vm.jit.disable(),
                JitMode::Default => {}
            }
            vm.set_source(&contents);
            vm.set_file(file_path_buf);
            vm.set_script_args(&script_args);

            match vm.run(function) {
                Ok(result) => match &result {
                    oxigen_core::vm::value::Value::None => {}
                    oxigen_core::vm::value::Value::Error(msg) => {
                        eprintln!("Error: {}", msg);
                        std::process::exit(1);
                    }
                    _ => println!("{}", result),
                },
                Err(err) => {
                    eprintln!("{}", err);
                    std::process::exit(1);
                }
            }

            // Let any fire-and-forget spawned tasks finish before exit.
            oxigen_core::concurrent::drain();
        }
    };
    on_big_stack(run);
}

// ── Terminal colors for `oxigen test` output ──
const C_GREEN: &str = "\x1b[32m";
const C_RED: &str = "\x1b[31m";
const C_RESET: &str = "\x1b[0m";

/// Emit ANSI colors only when stdout is a terminal and the conventional
/// `NO_COLOR` variable is unset, so piped/redirected output stays clean.
fn color_enabled() -> bool {
    std::env::var_os("NO_COLOR").is_none() && std::io::stdout().is_terminal()
}

/// Wrap `text` in `code` when color is enabled, otherwise return it as-is.
fn paint(text: &str, code: &str, color: bool) -> String {
    if color {
        format!("{code}{text}{C_RESET}")
    } else {
        text.to_string()
    }
}

/// A `[ok]` (green) / `[fail]` (red) status marker, padded so test names line
/// up regardless of marker width. Color codes are zero-width, so padding is
/// applied to the plain text length.
fn status_marker(passed: bool, color: bool) -> String {
    let (text, code) = if passed {
        ("[ok]", C_GREEN)
    } else {
        ("[fail]", C_RED)
    };
    let pad = " ".repeat("[fail]".len() - text.len());
    format!("{}{}", paint(text, code, color), pad)
}

/// Recursively collect files ending in `suffix` under `dir`, skipping hidden
/// directories and common build/vendor folders. Results are sorted for
/// deterministic ordering. `test` passes `_test.oxi`; `fmt` passes `.oxi`.
fn discover_files(dir: &std::path::Path, suffix: &str, out: &mut Vec<PathBuf>) {
    let entries = match fs::read_dir(dir) {
        Ok(e) => e,
        Err(_) => return,
    };

    // Symlinks are skipped: following them ran a `sub/up -> ..` loop's tests
    // dozens of times, and let `fmt` rewrite files outside the tree it was given.
    let mut paths: Vec<PathBuf> = entries
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_ok_and(|t| !t.is_symlink()))
        .map(|e| e.path())
        .collect();
    paths.sort();

    for path in paths {
        if path.is_dir() {
            let name = path
                .file_name()
                .and_then(|n| n.to_str())
                .unwrap_or_default();
            if name.starts_with('.') || name == "target" || name == "node_modules" {
                continue;
            }
            discover_files(&path, suffix, out);
        } else if path
            .file_name()
            .and_then(|n| n.to_str())
            .map(|n| n.ends_with(suffix))
            .unwrap_or(false)
        {
            out.push(path);
        }
    }
}

/// Outcome of running a single test file.
struct FileResult {
    passed: usize,
    failed: usize,
    /// The stdout block for this file (empty for files with no `<test>` blocks).
    output: String,
}

/// Run every `<test>` block in a single file. The per-file output is returned
/// in `output` rather than printed, so the caller controls separators.
fn run_test_file(path: &std::path::Path, color: bool) -> FileResult {
    use std::fmt::Write as _;

    let display = path.display();
    let contents = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(e) => {
            return FileResult {
                passed: 0,
                failed: 1,
                output: format!("{}\n  error reading file: {}", display, e),
            };
        }
    };

    let lexer = Lexer::new(&contents);
    let mut parser = Parser::new(lexer, &contents);
    let program = parser.parse_program();

    if !parser.errors().is_empty() {
        return FileResult {
            passed: 0,
            failed: 1,
            output: format!("{}\n{}", display, parser.format_errors()),
        };
    }

    let file_path_buf = match path.canonicalize() {
        Ok(p) => p,
        Err(e) => {
            return FileResult {
                passed: 0,
                failed: 1,
                output: format!("{}\n  could not resolve path: {}", display, e),
            };
        }
    };

    // `oxigen test` runs on the bytecode VM so tests see the same semantics as running the file.
    let outcomes = oxigen_core::test_runner::run_vm_tests(&program, &contents, Some(file_path_buf));

    if outcomes.is_empty() {
        return FileResult {
            passed: 0,
            failed: 0,
            output: String::new(),
        };
    }

    let mut output = format!("{}", display);
    let mut passed = 0;
    let mut failed = 0;
    for outcome in &outcomes {
        if outcome.passed {
            passed += 1;
            let _ = write!(
                output,
                "\n  {} {}",
                status_marker(true, color),
                outcome.name
            );
        } else {
            failed += 1;
            let _ = write!(
                output,
                "\n  {} {}",
                status_marker(false, color),
                outcome.name
            );
            if let Some(msg) = &outcome.message {
                let _ = write!(output, "\n       {}", paint(msg, C_RED, color));
            }
        }
    }
    FileResult {
        passed,
        failed,
        output,
    }
}

/// `oxigen test [path ...]` — discover and run test files.
fn run_tests_command(paths: &[String]) {
    let mut files: Vec<PathBuf> = Vec::new();

    if paths.is_empty() {
        let cwd = env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
        discover_files(&cwd, "_test.oxi", &mut files);
    } else {
        for arg in paths {
            let p = PathBuf::from(arg);
            if p.is_dir() {
                discover_files(&p, "_test.oxi", &mut files);
            } else if p.is_file() {
                files.push(p);
            } else {
                usage_error(&format!("no such file or directory: {arg}"), None);
            }
        }
    }

    if files.is_empty() {
        println!("No test files found (looking for *_test.oxi).");
        return;
    }

    let color = color_enabled();
    let mut total_passed = 0;
    let mut total_failed = 0;
    let mut any_printed = false;
    for file in &files {
        let result = run_test_file(file, color);
        if !result.output.is_empty() {
            // Blank line between consecutive files that produced output.
            if any_printed {
                println!();
            }
            println!("{}", result.output);
            any_printed = true;
        }
        total_passed += result.passed;
        total_failed += result.failed;
    }

    if any_printed {
        println!();
    }
    let summary = if total_failed == 0 {
        format!(
            "test result: {}. {} passed",
            paint("ok", C_GREEN, color),
            total_passed
        )
    } else {
        format!(
            "test result: {}. {} passed; {} failed",
            paint("FAILED", C_RED, color),
            total_passed,
            total_failed
        )
    };
    println!("{}", summary);

    if total_failed > 0 {
        std::process::exit(1);
    }
}

fn check_file(file_path: &str) {
    if !PathBuf::from(file_path).is_file() {
        usage_error(&format!("no such file: `{file_path}`"), None);
    }
    let contents = read_source(file_path);

    let lexer = Lexer::new(&contents);
    let mut parser = Parser::with_file(lexer, &contents, file_path);
    let program = parser.parse_program();

    let source = parser.source_file();
    let mut sink = DiagnosticSink::new();
    sink.extend(parser.errors().iter().cloned());

    // check used to parse and stop, so a compile error never reached an editor.
    if !sink.has_errors()
        && let Err(errors) = Compiler::new().compile(&program)
    {
        sink.extend(errors.into_iter().map(|e| e.into_diagnostic()));
    }

    sink.deduplicate();
    let payload: Vec<serde_json::Value> = sink
        .diagnostics()
        .iter()
        .map(|d| render::render_json(d, &source))
        .collect();
    println!("{}", serde_json::to_string(&payload).unwrap());

    // Non-zero only on real failure, so check works as a CI gate; warnings still exit 0.
    if sink.has_errors() {
        std::process::exit(1);
    }
}

/// `check` reports what would change and exits 1 without writing, so CI can
/// gate on formatting. Without it a formatted tree silently drifts back out.
fn fmt_files(args: &[String], check: bool) {
    // A directory used to fall into the non-.oxi branch below, print "Skipping"
    // and exit 0 — so `oxigen fmt .` formatted nothing and reported success.
    let mut paths: Vec<PathBuf> = Vec::new();
    for arg in args {
        let p = PathBuf::from(arg);
        if p.is_dir() {
            discover_files(&p, ".oxi", &mut paths);
        } else if p.is_file() {
            if !arg.ends_with(".oxi") {
                usage_error(
                    &format!("not an Oxigen source file: {arg}"),
                    Some("fmt only formats .oxi files"),
                );
            }
            paths.push(p);
        } else {
            usage_error(
                &format!("no such file or directory: {arg}"),
                Some("fmt takes .oxi files or directories; `oxigen fmt .` walks the tree"),
            );
        }
    }

    if paths.is_empty() {
        println!("No .oxi files found.");
        return;
    }

    let mut changed = 0;
    let mut failed = 0;
    for path in &paths {
        let path = path.display().to_string();
        let contents = match fs::read_to_string(&path) {
            Ok(c) => c,
            Err(e) => {
                eprintln!("Error reading {}: {}", path, e);
                failed += 1;
                continue;
            }
        };

        // A file that does not parse is reported and left alone; the rest of
        // the batch is still formatted rather than stopping half-way.
        let formatted = {
            let contents = contents.clone();
            on_big_stack(move || {
                let lexer = Lexer::new(&contents);
                let mut parser = Parser::new(lexer, &contents);
                let program = parser.parse_program();
                if !parser.errors().is_empty() {
                    return Err(parser.format_errors());
                }
                let (directives, indent_style) = Lexer::header_directives(&contents);
                let body = Formatter::format_source(&program, parser.comments(), indent_style);
                Ok(restore_header(&directives, body))
            })
        };
        let formatted = match formatted {
            Ok(f) => f,
            Err(errors) => {
                eprintln!("Syntax errors in {}:", path);
                eprintln!("{errors}");
                failed += 1;
                continue;
            }
        };

        if formatted != contents {
            changed += 1;
            if check {
                println!("Would reformat {}", path);
                continue;
            }
            if let Err(e) = write_atomically(&path, &formatted) {
                eprintln!("Error writing {}: {}", path, e);
                failed += 1;
                continue;
            }
            println!("Formatted {}", path);
        }
    }

    if failed > 0 {
        eprintln!("\n{failed} of {} file(s) could not be formatted.", paths.len());
        std::process::exit(1);
    }

    // Say how many files were considered: silence after walking a tree is
    // ambiguous between "already formatted" and "found nothing".
    if changed == 0 {
        println!("{} file(s) already formatted.", paths.len());
        return;
    }

    if check {
        eprintln!(
            "\n{changed} of {} file(s) need formatting. Run `oxigen fmt` to fix.",
            paths.len()
        );
        std::process::exit(1);
    }
}

/// Replaces `path` with `contents` via a temporary file and a rename, so an
/// interrupted write cannot leave a half-written source file behind.
fn write_atomically(path: &str, contents: &str) -> std::io::Result<()> {
    let target = std::path::Path::new(path);
    let tmp = target.with_file_name(format!(
        ".{}.oxigen-fmt.tmp",
        target.file_name().and_then(|n| n.to_str()).unwrap_or("file")
    ));
    fs::write(&tmp, contents)?;
    let result = fs::metadata(target)
        .and_then(|meta| fs::set_permissions(&tmp, meta.permissions()))
        .and_then(|()| fs::rename(&tmp, target));
    if result.is_err() {
        // Best-effort cleanup: the write error above is what gets reported,
        // and a stray temp file is harmless next to it.
        let _ = fs::remove_file(&tmp);
    }
    result
}

/// Exit code for a malformed command line, kept distinct from `1` (which means
/// the tool ran and the *code* was bad) so scripts can tell the two apart.
const EXIT_USAGE: i32 = 2;

const SUBCOMMANDS: [&str; 4] = ["check", "fmt", "test", "explain"];

fn print_usage() {
    print_version();
    println!();
    println!("Usage:");
    println!("  oxigen [options] <file.oxi> [script args...]   Run a script");
    println!("  oxigen [options]                               Start the REPL");
    println!("  oxigen check <file.oxi>                        Report syntax errors as JSON");
    println!("  oxigen fmt [--check] <file.oxi|dir>...         Format files in place");
    println!("                                                 --check: report and exit 1 instead");
    println!("  oxigen test [file.oxi|dir]                     Run *_test.oxi suites");
    println!("  oxigen explain <CODE>                          Explain an error code");
    println!();
    println!("Options (must come BEFORE the file; anything after it goes to the script):");
    println!("  --jit          Compile eagerly instead of tiering up");
    println!("  --no-jit       Interpreter only");
    println!("  --version/-v   Print version");
    println!("  --help/-h      Print this help");
    println!();
    println!("Exit codes: 0 success, 1 the code failed, {EXIT_USAGE} the command line was wrong.");
}

/// Reports a bad command line and exits. `hint` is a suggested fix.
fn usage_error(message: &str, hint: Option<&str>) -> ! {
    eprintln!("error: {message}");
    if let Some(hint) = hint {
        eprintln!("  hint: {hint}");
    }
    eprintln!("  run `oxigen --help` for usage");
    std::process::exit(EXIT_USAGE);
}

/// Rejects a leading argument that is neither a subcommand nor a `.oxi` script,
/// instead of silently falling through to the REPL.
fn reject_leading_arg(arg: &str) -> ! {
    // A .oxi path is handled by the caller, so any other extension is the wrong kind of file.
    let looks_like_path = arg.contains('/') || arg.contains('.');

    if !looks_like_path {
        // There is no `run` subcommand; scripts are passed directly.
        let hint = if arg == "run" {
            "run a script with `oxigen <file.oxi>` — there is no `run` subcommand".to_string()
        } else {
            format!("expected a .oxi file or one of: {}", SUBCOMMANDS.join(", "))
        };
        usage_error(&format!("unknown subcommand `{arg}`"), Some(&hint));
    }

    if !PathBuf::from(arg).exists() {
        usage_error(&format!("no such file: `{arg}`"), None);
    }
    usage_error(
        &format!("not an Oxigen source file: `{arg}`"),
        Some("Oxigen scripts must end in `.oxi`"),
    )
}

fn main() {
    // `env::args` panics on an argument that is not UTF-8.
    let args: Vec<String> = env::args_os()
        .map(|arg| {
            arg.into_string().unwrap_or_else(|bad| {
                usage_error(
                    &format!("argument is not valid UTF-8: {}", bad.to_string_lossy()),
                    None,
                )
            })
        })
        .collect();

    // Recognised only before the subcommand or script path, or a script's own --no-jit is swallowed.
    let mut no_jit = false;
    let mut want_eager = false;
    let mut cursor = 1;
    while let Some(arg) = args.get(cursor) {
        match arg.as_str() {
            "--no-jit" => no_jit = true,
            "--jit" => want_eager = true,
            _ => break,
        }
        cursor += 1;
    }

    let eager_jit = !no_jit
        && (want_eager || env::var("OXIGEN_JIT").map(|v| v != "0").unwrap_or(false));
    let jit_mode = if no_jit {
        JitMode::Disabled
    } else if eager_jit {
        JitMode::Eager
    } else {
        JitMode::Default
    };

    let rest = &args[cursor..];
    let tail = rest.get(1..).unwrap_or(&[]);

    match rest.first().map(|s| s.as_str()) {
        None => repl::run_repl(),
        Some("--version" | "-v") => print_version(),
        Some("--help" | "-h") => print_usage(),
        Some("check") => match rest.get(1) {
            Some(path) => {
                let path = path.clone();
                on_big_stack(move || check_file(&path));
            }
            None => usage_error("`check` needs a file", Some("oxigen check <file.oxi>")),
        },
        Some("fmt") => {
            // Accepted anywhere after `fmt`, not only straight after it.
            let check = tail.iter().any(|a| a == "--check");
            let paths: Vec<String> = tail.iter().filter(|a| *a != "--check").cloned().collect();
            if paths.is_empty() {
                usage_error(
                    "`fmt` needs a file or directory",
                    Some("oxigen fmt <file.oxi|dir>... (add --check to report instead of write)"),
                );
            }
            fmt_files(&paths, check);
        }
        Some("test") if tail.iter().any(|a| a == "--help" || a == "-h") => print_usage(),
        Some("test") => {
            let paths = tail.to_vec();
            on_big_stack(move || run_tests_command(&paths));
        }
        Some("explain") => match rest.get(1) {
            Some(code) => match render::explain(code) {
                Some(text) => println!("{text}"),
                None => usage_error(
                    &format!("unknown error code `{code}`"),
                    Some("run `oxigen explain --list` to see every code"),
                ),
            },
            None => usage_error("`explain` needs an error code", Some("oxigen explain E0003")),
        },
        Some(path) if path.ends_with(".oxi") => {
            let p = PathBuf::from(path);
            if !p.exists() {
                usage_error(&format!("no such file: `{path}`"), None);
            }
            if p.is_dir() {
                usage_error(&format!("`{path}` is a directory, not a script"), None);
            }
            run_file_vm(path, tail, jit_mode);
        }
        Some(option) if option.starts_with('-') => usage_error(
            &format!("unknown option `{option}`"),
            Some("options are --jit, --no-jit, --version and --help, and come before the file"),
        ),
        Some(other) => reject_leading_arg(other),
    }
}
