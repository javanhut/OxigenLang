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
use oxigen_core::formatter::Formatter;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

use serde_json::json;

fn read_source(file_path: &str) -> String {
    match fs::read_to_string(file_path) {
        Ok(contents) => contents,
        Err(err) => {
            eprintln!("Error reading {}: {}", file_path, err);
            std::process::exit(1);
        }
    }
}

fn is_header_line(line: &str, allow_shebang: bool) -> bool {
    if allow_shebang && line.starts_with("#!") {
        return true;
    }

    line == "#[indent]" || (line.starts_with("#[location=") && line.ends_with(']'))
}

fn header_prefix(source: &str) -> String {
    let mut header_lines = Vec::new();

    for (idx, line) in source.lines().enumerate() {
        if is_header_line(line, idx == 0) {
            header_lines.push(line);
        } else {
            break;
        }
    }

    if header_lines.is_empty() {
        String::new()
    } else {
        format!("{}\n", header_lines.join("\n"))
    }
}

fn restore_header(source: &str, body: String) -> String {
    let header = header_prefix(source);

    if header.is_empty() {
        body
    } else if body.is_empty() {
        header
    } else {
        format!("{header}{body}")
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

    // Run parse/compile/VM execution on a large-stack thread so all FRAMES_MAX
    // (16384) native JIT frames fit on the native stack and the V7 recursion
    // guard is the graceful limit (instead of the native stack overflowing
    // first -> rc=134). Parse/compile run inside the thread too so no non-Send
    // (Rc-backed) value is captured across the thread boundary.
    const STACK_SIZE: usize = 256 << 20; // 256 MB
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
                    for err in &errors {
                        eprintln!("{}", err);
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
    std::thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(run)
        .expect("failed to spawn execution thread")
        .join()
        .expect("execution thread panicked");
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

/// Recursively collect `*_test.oxi` files under `dir`, skipping hidden
/// directories and common build/vendor folders. Results are sorted for
/// deterministic ordering.
fn discover_test_files(dir: &std::path::Path, out: &mut Vec<PathBuf>) {
    let entries = match fs::read_dir(dir) {
        Ok(e) => e,
        Err(_) => return,
    };

    let mut paths: Vec<PathBuf> = entries.filter_map(|e| e.ok().map(|e| e.path())).collect();
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
            discover_test_files(&path, out);
        } else if path
            .file_name()
            .and_then(|n| n.to_str())
            .map(|n| n.ends_with("_test.oxi"))
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

    // `oxigen test` runs each `<test>` block on the bytecode VM, so tests
    // observe the same semantics as `oxigen file.oxi` — notably in-place
    // `push`/`insert`.
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
        discover_test_files(&cwd, &mut files);
    } else {
        for arg in paths {
            let p = PathBuf::from(arg);
            if p.is_dir() {
                discover_test_files(&p, &mut files);
            } else if p.is_file() {
                files.push(p);
            } else {
                eprintln!("No such file or directory: {}", arg);
                std::process::exit(1);
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
    let contents = read_source(file_path);

    let lexer = Lexer::new(&contents);
    let mut parser = Parser::new(lexer, &contents);
    let _program = parser.parse_program();

    let diagnostics: Vec<serde_json::Value> = parser
        .errors()
        .iter()
        .map(|d| {
            let severity = match d.severity {
                oxigen_core::parser::Severity::Error => "error",
                oxigen_core::parser::Severity::Warning => "warning",
            };
            json!({
                "line": d.span.line,
                "column": d.span.column,
                "message": d.message,
                "suggestion": d.suggestion,
                "severity": severity
            })
        })
        .collect();

    println!("{}", serde_json::to_string(&diagnostics).unwrap());

    // Exit non-zero when the file actually failed to parse, so `oxigen check`
    // is usable as a CI gate. Warnings alone still exit 0. The JSON is written
    // either way — the LSP reads stdout regardless of status (see
    // `lsp-go/diagnostics.go`), so this stays compatible with it.
    let has_error = parser
        .errors()
        .iter()
        .any(|d| d.severity == oxigen_core::parser::Severity::Error);
    if has_error {
        std::process::exit(1);
    }
}

fn fmt_files(paths: &[String]) {
    for path in paths {
        if !path.ends_with(".oxi") {
            eprintln!("Skipping non-.oxi file: {}", path);
            continue;
        }

        let contents = match fs::read_to_string(path) {
            Ok(c) => c,
            Err(e) => {
                eprintln!("Error reading {}: {}", path, e);
                std::process::exit(1);
            }
        };

        let lexer = Lexer::new(&contents);
        let mut parser = Parser::new(lexer, &contents);
        let program = parser.parse_program();

        let errors = parser.errors();
        if !errors.is_empty() {
            eprintln!("Syntax errors in {}:", path);
            eprintln!("{}", parser.format_errors());
            std::process::exit(1);
        }

        // `restore_header` puts `#[indent]` back, so the body has to be emitted
        // in indent style or the reformatted file no longer parses.
        let indent_style = header_prefix(&contents)
            .lines()
            .any(|l| l.trim() == "#[indent]");
        let body = Formatter::format_source(&program, parser.comments(), indent_style);
        let formatted = restore_header(&contents, body);

        if formatted != contents {
            if let Err(e) = fs::write(path, &formatted) {
                eprintln!("Error writing {}: {}", path, e);
                std::process::exit(1);
            }
            println!("Formatted {}", path);
        }
    }
}

/// Exit code for a malformed command line, kept distinct from `1` (which means
/// the tool ran and the *code* was bad) so scripts can tell the two apart.
const EXIT_USAGE: i32 = 2;

const SUBCOMMANDS: [&str; 3] = ["check", "fmt", "test"];

fn print_usage() {
    print_version();
    println!();
    println!("Usage:");
    println!("  oxigen [options] <file.oxi> [script args...]   Run a script");
    println!("  oxigen [options]                               Start the REPL");
    println!("  oxigen check <file.oxi>                        Report syntax errors as JSON");
    println!("  oxigen fmt <file.oxi|dir>...                   Format files in place");
    println!("  oxigen test [file.oxi|dir]                     Run *_test.oxi suites");
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
    // A `.oxi` path is handled by the caller, so anything with an extension
    // here is the wrong kind of file.
    let looks_like_path = arg.contains('/') || arg.contains('.');

    if !looks_like_path {
        // `oxigen run app.oxi` — there is no `run` subcommand; scripts are
        // passed directly. Older docs suggested otherwise, so name the fix.
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
    let args: Vec<String> = env::args().collect();

    // The bytecode VM is the only backend. `--jit` compiles eagerly (threshold
    // 1), `--no-jit` disables compilation; default is lazy tiering.
    //
    // These are only recognised BEFORE the subcommand or script path. Scanning
    // all of argv would swallow a script's own `--no-jit` argument, so once a
    // non-option argument appears everything after it belongs to the script.
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
            Some(path) => check_file(path),
            None => usage_error("`check` needs a file", Some("oxigen check <file.oxi>")),
        },
        Some("fmt") => {
            if tail.is_empty() {
                usage_error("`fmt` needs a file or directory", Some("oxigen fmt <file.oxi>"));
            }
            fmt_files(tail);
        }
        Some("test") => run_tests_command(tail),
        Some(path) if path.ends_with(".oxi") => {
            if !PathBuf::from(path).exists() {
                usage_error(&format!("no such file: `{path}`"), None);
            }
            run_file_vm(path, tail, jit_mode);
        }
        Some(other) => reject_leading_arg(other),
    }
}
