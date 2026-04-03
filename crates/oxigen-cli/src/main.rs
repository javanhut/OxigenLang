use std::cell::RefCell;
use std::env;
use std::fs;
use std::path::PathBuf;
use std::rc::Rc;

mod repl;

use oxigen_core::evaluator::Evaluator;
use oxigen_core::formatter::Formatter;
use oxigen_core::lexer::Lexer;
use oxigen_core::object;
use oxigen_core::object::environment::Environment;
use oxigen_core::parser::Parser;

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

    match args.get(1).map(|s| s.as_str()) {
        Some("--version") | Some("-v") => {
            println!("oxigen {}", env!("CARGO_PKG_VERSION"));
        }
        Some("check") => {
            if let Some(path) = args.get(2) {
                check_file(path);
            } else {
                eprintln!("Usage: oxigen check <file.oxi>");
                std::process::exit(1);
            }
        }
        Some("fmt") => fmt_files(&args[2..]),
        Some(path) if path.ends_with(".oxi") => run_file(path, &args[2..]),
        _ => repl::run_repl(),
    }
}
