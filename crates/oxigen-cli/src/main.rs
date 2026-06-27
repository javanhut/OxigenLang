use std::env;
use std::fs;
use std::io::IsTerminal;
use std::path::PathBuf;

mod repl;

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

        let formatted = restore_header(&contents, Formatter::format(&program));

        if formatted != contents {
            if let Err(e) = fs::write(path, &formatted) {
                eprintln!("Error writing {}: {}", path, e);
                std::process::exit(1);
            }
            println!("Formatted {}", path);
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    // The bytecode VM is the only backend. `--jit` compiles eagerly (threshold
    // 1), `--no-jit` disables compilation; default is lazy tiering.
    let no_jit = args.iter().any(|a| a == "--no-jit");
    let eager_jit = !no_jit
        && (args.iter().any(|a| a == "--jit")
            || env::var("OXIGEN_JIT").map(|v| v != "0").unwrap_or(false));
    let jit_mode = if no_jit {
        JitMode::Disabled
    } else if eager_jit {
        JitMode::Eager
    } else {
        JitMode::Default
    };
    let filtered_args: Vec<String> = args
        .iter()
        .filter(|a| {
            let s = a.as_str();
            s != "--jit" && s != "--no-jit"
        })
        .cloned()
        .collect();

    match filtered_args.get(1).map(|s| s.as_str()) {
        Some("--version") | Some("-v") => {
            println!("oxigen {}", env!("CARGO_PKG_VERSION"));
        }
        Some("check") => {
            if let Some(path) = filtered_args.get(2) {
                check_file(path);
            } else {
                eprintln!("Usage: oxigen check <file.oxi>");
                std::process::exit(1);
            }
        }
        Some("fmt") => fmt_files(&filtered_args[2..]),
        Some("test") => run_tests_command(&filtered_args[2..]),
        Some(path) if path.ends_with(".oxi") => {
            run_file_vm(path, &filtered_args[2..], jit_mode);
        }
        _ => repl::run_repl(),
    }
}
