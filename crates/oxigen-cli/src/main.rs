use std::cell::RefCell;
use std::env;
use std::fs;
use std::path::PathBuf;
use std::rc::Rc;

mod repl;

use oxigen_core::compiler::Compiler;
use oxigen_core::evaluator::Evaluator;
use oxigen_core::formatter::Formatter;
use oxigen_core::lexer::Lexer;
use oxigen_core::object;
use oxigen_core::object::environment::Environment;
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

    let lexer = Lexer::new(&contents);
    let mut parser = Parser::new(lexer, &contents);
    let program = parser.parse_program();

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

    let file_path_buf = PathBuf::from(file_path)
        .canonicalize()
        .expect("Could not resolve file path");
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
    vm.set_script_args(script_args);

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
}

fn run_file(file_path: &str, script_args: &[String]) {
    let contents = read_source(file_path);

    let lexer = Lexer::new(&contents);
    let mut parser = Parser::new(lexer, &contents);
    let program = parser.parse_program();

    let errors = parser.errors();
    if !errors.is_empty() {
        eprintln!("{}", parser.format_errors());
        std::process::exit(1);
    }

    let file_path_buf = PathBuf::from(file_path)
        .canonicalize()
        .expect("Could not resolve file path");
    let env = Rc::new(RefCell::new(Environment::new()));
    let mut evaluator = Evaluator::new_with_path(file_path_buf);
    evaluator.set_source(&contents);
    evaluator.set_script_args(script_args);
    let result = evaluator.eval_program(&program, env);

    match result.as_ref() {
        object::Object::None => {}
        object::Object::Error(msg) => {
            eprintln!("{}", msg);
            std::process::exit(1);
        }
        object::Object::ErrorValue { .. } => println!("{}", result),
        _ => println!("{}", result),
    }
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
fn run_test_file(path: &std::path::Path) -> FileResult {
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

    let env = Rc::new(RefCell::new(Environment::new()));
    let mut evaluator = Evaluator::new_with_path(file_path_buf);
    evaluator.set_source(&contents);
    let outcomes = evaluator.run_tests(&program, env);

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
            let _ = write!(output, "\n  ok   {}", outcome.name);
        } else {
            failed += 1;
            let _ = write!(output, "\n  FAIL {}", outcome.name);
            if let Some(msg) = &outcome.message {
                let _ = write!(output, "\n       {}", msg);
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

    let mut total_passed = 0;
    let mut total_failed = 0;
    let mut any_printed = false;
    for file in &files {
        let result = run_test_file(file);
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
        format!("test result: ok. {} passed", total_passed)
    } else {
        format!(
            "test result: FAILED. {} passed; {} failed",
            total_passed, total_failed
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
    if paths.is_empty() {
        eprintln!("Usage: oxigen fmt <file.oxi> [file2.oxi ...]");
        std::process::exit(1);
    }

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

    // VM is the default. --tree-walk falls back to the tree-walking interpreter.
    let use_tree_walk = args.iter().any(|a| a == "--tree-walk");
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
            s != "--tree-walk" && s != "--vm" && s != "--jit" && s != "--no-jit"
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
            if use_tree_walk {
                run_file(path, &filtered_args[2..]);
            } else {
                run_file_vm(path, &filtered_args[2..], jit_mode);
            }
        }
        _ => repl::run_repl(),
    }
}
