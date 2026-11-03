//! OxigenLang compiled to WebAssembly, for the playground on oxigenlang.org.
//!
//! The browser build targets WASI (`wasm32-wasip1`, and `wasm32-wasip1-threads`
//! for the playground's runs), and the page supplies the system: a file
//! system in memory, standard input typed into the output pane, the clock,
//! sleeping, environment variables and arguments, and, in the threads build,
//! a Web Worker per thread. So a script runs the way `oxigen main.oxi` runs
//! it, through the same std calls, and `input`, `io`, `os`, `time.sleep`,
//! `diverge` and `channel` work. What a browser cannot do stays refused:
//! sockets and HTTP, running programs, and C libraries.
//!
//! The page talks to the module through linear memory. It calls `oxi_main`
//! with the WASI arguments set as for the command line (`oxigen main.oxi a b`,
//! `oxigen test main.oxi`) and gets the exit code back; output arrives on
//! file descriptors 1 and 2 as it is printed. For the editor it copies UTF-8
//! source into a buffer from `oxi_alloc`, calls `oxi_check`, `oxi_format`,
//! `oxi_highlight` or `oxi_tokens`, and reads the reply from `oxi_reply_ptr`
//! and `oxi_reply_len`.
//!
//! Natively the crate is a library: [`run`], [`check`] and [`format`] are what
//! the website generator checks the documentation's examples with, under the
//! sandbox the docs pages' Run buttons have (no capabilities at all).

use oxigen_core::compiler::Compiler;
use oxigen_core::diagnostics::{DiagnosticSink, render};
use oxigen_core::formatter::Formatter;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::value::Value;
use oxigen_core::vm::{Capability, VM};
use std::path::PathBuf;

/// Loop iterations a playground run may make. Generous for real programs,
/// and an endless loop still ends in seconds rather than hanging the tab.
pub const LOOP_BUDGET: u64 = 200_000_000;

/// The file name errors are reported against.
pub const FILE_NAME: &str = "main.oxi";

/// `stdlib/*.oxi`, by module name; written by build.rs.
pub const STDLIB: &[(&str, &str)] = include!(concat!(env!("OUT_DIR"), "/stdlib.rs"));

/// The embedded source of stdlib module `name`.
pub fn stdlib_module(name: &str) -> Option<&'static str> {
    STDLIB
        .iter()
        .find(|(n, _)| *n == name)
        .map(|(_, source)| *source)
}

/// How a run ended. Output is not here: it went to the output sink as the
/// script printed it.
#[derive(Debug, Clone, PartialEq)]
pub enum Outcome {
    Finished,
    /// A syntax, compile or runtime error, rendered as `oxigen` prints it.
    Failed(String),
}

/// Run `source` sandboxed, with at most `loop_budget` loop iterations.
pub fn run(source: &str, loop_budget: u64) -> Outcome {
    let lexer = Lexer::new(source);
    let mut parser = Parser::with_file(lexer, source, FILE_NAME);
    let program = parser.parse_program();
    if !parser.errors().is_empty() {
        return Outcome::Failed(parser.format_errors());
    }

    let function = match Compiler::new().compile(&program) {
        Ok(function) => function,
        Err(errors) => {
            let file = parser.source_file();
            let rendered = errors
                .into_iter()
                .map(|e| render::render_human(&e.into_diagnostic(), &file))
                .collect();
            return Outcome::Failed(rendered);
        }
    };

    let mut vm = VM::new();
    vm.restrict(&[]);
    vm.set_loop_budget(Some(loop_budget));
    vm.set_module_source(|path| stdlib_module(path).map(str::to_string));
    vm.set_source(source);
    vm.set_file(PathBuf::from(FILE_NAME));

    match vm.run(function) {
        // The same as the command line: a script's final value is printed.
        Ok(Value::None) => Outcome::Finished,
        Ok(Value::Error(message)) => Outcome::Failed(format!("Error: {message}")),
        Ok(result) => {
            oxigen_core::output::write(&format!("{result}\n"));
            Outcome::Finished
        }
        Err(err) => Outcome::Failed(err.to_string()),
    }
}

/// Diagnostics for `source` as `oxigen check` prints them: a JSON array.
pub fn check(source: &str) -> String {
    let lexer = Lexer::new(source);
    let mut parser = Parser::with_file(lexer, source, FILE_NAME);
    let program = parser.parse_program();
    let file = parser.source_file();
    let mut sink = DiagnosticSink::new();
    sink.extend(parser.errors().iter().cloned());
    if !sink.has_errors()
        && let Err(errors) = Compiler::new().compile(&program)
    {
        sink.extend(errors.into_iter().map(|e| e.into_diagnostic()));
    }
    sink.deduplicate();
    let payload: Vec<serde_json::Value> = sink
        .diagnostics()
        .iter()
        .map(|d| render::render_json(d, &file))
        .collect();
    serde_json::to_string(&payload).unwrap_or_else(|_| "[]".into())
}

/// `source` formatted as `oxigen fmt` would, or the syntax errors that stop it.
pub fn format(source: &str) -> Result<String, String> {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    if !parser.errors().is_empty() {
        return Err(parser.format_errors());
    }
    let (directives, indent_style) = Lexer::header_directives(source);
    let body = Formatter::format_source(&program, parser.comments(), indent_style);
    Ok(if directives.is_empty() {
        body
    } else {
        format!("{}\n{body}", directives.join("\n"))
    })
}

/// What a playground run may reach. Everything a browser tab can provide:
/// files (in memory), standard input, sleeping, and the process facts WASI
/// has (arguments, environment, working directory, exit). Threads only in
/// the threads build. Not the network, other programs or C libraries.
pub fn playground_capabilities() -> Vec<Capability> {
    let mut allowed = vec![
        Capability::FileSystem,
        Capability::Stdin,
        Capability::Sleep,
        Capability::Process,
    ];
    if cfg!(wasm_threads) {
        allowed.push(Capability::Threads);
    }
    allowed
}

/// The UTF-16 ranges of `source`'s highlighted tokens, as a flat JSON array
/// of `start, end, class` triples, for an editor that works in JavaScript
/// string offsets. Classes index [`TOKEN_CLASSES`].
pub fn tokens(source: &str) -> String {
    let spans = oxigen_core::highlight::highlight(source);
    let mut out = String::from("[");
    // Walk the source once, converting byte offsets to UTF-16 as they come.
    let mut unit = 0;
    let mut chars = source.char_indices().peekable();
    let mut to_unit = |target: usize| {
        while let Some(&(i, c)) = chars.peek() {
            if i >= target {
                break;
            }
            unit += c.len_utf16();
            chars.next();
        }
        unit
    };
    for (i, (range, class)) in spans.iter().enumerate() {
        let start = to_unit(range.start);
        let end = to_unit(range.end);
        let index = TOKEN_CLASSES
            .iter()
            .position(|c| *c == class.css())
            .unwrap_or(0);
        if i > 0 {
            out.push(',');
        }
        out.push_str(&format!("{start},{end},{index}"));
    }
    out.push(']');
    out
}

/// The CSS classes [`tokens`] refers to by index.
pub const TOKEN_CLASSES: [&str; 8] = [
    "tk-kw", "tk-const", "tk-str", "tk-num", "tk-com", "tk-fn", "tk-type", "tk-dir",
];

#[cfg(target_os = "wasi")]
mod exports {
    //! The C ABI the page calls. The reply buffer is a thread local that
    //! each call replaces; only the main thread calls in.

    use super::*;
    use std::cell::RefCell;

    thread_local! {
        static REPLY: RefCell<Vec<u8>> = const { RefCell::new(Vec::new()) };
    }

    fn reply(text: String) {
        REPLY.with(|r| *r.borrow_mut() = text.into_bytes());
    }

    /// Borrow the `len` bytes the page wrote at `ptr` as source text.
    ///
    /// # Safety
    /// `ptr` came from `oxi_alloc(len)` and the page filled it with UTF-8.
    unsafe fn source<'a>(ptr: *const u8, len: usize) -> &'a str {
        let bytes = unsafe { std::slice::from_raw_parts(ptr, len) };
        std::str::from_utf8(bytes).unwrap_or("")
    }

    #[unsafe(no_mangle)]
    pub extern "C" fn oxi_alloc(len: usize) -> *mut u8 {
        let mut buffer = Vec::<u8>::with_capacity(len.max(1));
        let ptr = buffer.as_mut_ptr();
        std::mem::forget(buffer);
        ptr
    }

    /// # Safety
    /// `ptr` and `len` are a pair `oxi_alloc` returned and not yet freed.
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn oxi_free(ptr: *mut u8, len: usize) {
        drop(unsafe { Vec::from_raw_parts(ptr, 0, len.max(1)) });
    }

    #[unsafe(no_mangle)]
    pub extern "C" fn oxi_reply_ptr() -> *const u8 {
        REPLY.with(|r| r.borrow().as_ptr())
    }

    #[unsafe(no_mangle)]
    pub extern "C" fn oxi_reply_len() -> usize {
        REPLY.with(|r| r.borrow().len())
    }

    /// `oxigen` on the WASI arguments: `oxigen FILE [ARGS...]` runs a script,
    /// `oxigen test [FILE...]` runs its `<test>` blocks. Returns the exit code.
    /// Output and errors go to standard output and standard error as the
    /// command line prints them.
    #[unsafe(no_mangle)]
    pub extern "C" fn oxi_main() -> i32 {
        // WASI starts in `/`; the page says where the files are, as a shell would.
        if let Ok(dir) = std::env::var("PWD") {
            let _ = std::env::set_current_dir(dir);
        }
        let args: Vec<String> = std::env::args().skip(1).collect();
        match args.first().map(String::as_str) {
            Some("test") => super::cli::test(&args[1..]),
            Some(file) => super::cli::run_file(file, &args[1..]),
            None => {
                eprintln!("usage: oxigen FILE [ARGS...] | oxigen test [FILE...]");
                2
            }
        }
    }

    /// # Safety
    /// See `source`.
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn oxi_check(ptr: *const u8, len: usize) {
        reply(check(unsafe { source(ptr, len) }));
    }

    /// The reply is `{"ok":true,"source":"..."}` or `{"ok":false,"error":"..."}`.
    ///
    /// # Safety
    /// See `source`.
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn oxi_format(ptr: *const u8, len: usize) {
        reply(
            match format(unsafe { source(ptr, len) }) {
                Ok(source) => serde_json::json!({ "ok": true, "source": source }),
                Err(error) => serde_json::json!({ "ok": false, "error": error }),
            }
            .to_string(),
        );
    }

    /// The reply is the source as highlighted HTML.
    ///
    /// # Safety
    /// See `source`.
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn oxi_highlight(ptr: *const u8, len: usize) {
        reply(oxigen_core::highlight::to_html(unsafe { source(ptr, len) }));
    }

    /// The reply is the embedded stdlib as JSON, `{"math": "...source..."}`,
    /// for the page to put in the file system threads and tests read it from.
    #[unsafe(no_mangle)]
    pub extern "C" fn oxi_stdlib() {
        let modules: serde_json::Map<String, serde_json::Value> = STDLIB
            .iter()
            .map(|(name, source)| (name.to_string(), (*source).into()))
            .collect();
        reply(serde_json::Value::Object(modules).to_string());
    }

    /// The reply is `tokens`' JSON.
    ///
    /// # Safety
    /// See `source`.
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn oxi_tokens(ptr: *const u8, len: usize) {
        reply(tokens(unsafe { source(ptr, len) }));
    }
}

/// The `oxigen` commands the playground runs, printing as the command line does.
#[cfg_attr(not(target_os = "wasi"), allow(dead_code))]
mod cli {
    use super::*;
    use oxigen_core::diagnostics::SourceFile;

    /// `oxigen FILE ARGS...`: the exit code the command line would give.
    pub fn run_file(file: &str, script_args: &[String]) -> i32 {
        let contents = match std::fs::read_to_string(file) {
            Ok(text) => text,
            Err(e) => {
                eprintln!("Error: could not read {file}: {e}");
                return 1;
            }
        };
        let path = absolute(file);
        let lexer = Lexer::new(&contents);
        let mut parser = Parser::with_file(lexer, &contents, file);
        let program = parser.parse_program();
        if !parser.errors().is_empty() {
            eprintln!("{}", parser.format_errors());
            return 1;
        }
        let function = match Compiler::new().compile(&program) {
            Ok(function) => function,
            Err(errors) => {
                let source = SourceFile::named(file.to_string(), contents.clone());
                for err in errors {
                    eprint!("{}", render::render_human(&err.into_diagnostic(), &source));
                }
                return 1;
            }
        };
        oxigen_core::concurrent::set_src(contents.clone());
        oxigen_core::concurrent::set_main_file(path.clone());

        let mut vm = VM::new();
        vm.restrict(&playground_capabilities());
        vm.set_loop_budget(Some(LOOP_BUDGET));
        vm.set_source(&contents);
        vm.set_file(path);
        vm.set_script_args(script_args);
        let code = match vm.run(function) {
            Ok(Value::None) => 0,
            Ok(Value::Error(message)) => {
                eprintln!("Error: {message}");
                1
            }
            Ok(result) => {
                println!("{result}");
                0
            }
            Err(err) => {
                eprintln!("{err}");
                1
            }
        };
        // Let fire-and-forget tasks finish, as the command line does.
        if cfg!(wasm_threads) {
            oxigen_core::concurrent::drain();
        }
        code
    }

    /// `oxigen test FILES...`, without colour: the results and a summary.
    pub fn test(files: &[String]) -> i32 {
        let (mut passed, mut failed) = (0, 0);
        let mut printed = false;
        for file in files {
            let contents = match std::fs::read_to_string(file) {
                Ok(text) => text,
                Err(e) => {
                    eprintln!("{file}\n  error reading file: {e}");
                    failed += 1;
                    continue;
                }
            };
            let lexer = Lexer::new(&contents);
            let mut parser = Parser::new(lexer, &contents);
            let program = parser.parse_program();
            if !parser.errors().is_empty() {
                eprintln!("{file}\n{}", parser.format_errors());
                failed += 1;
                continue;
            }
            let outcomes =
                oxigen_core::test_runner::run_vm_tests(&program, &contents, Some(absolute(file)));
            if outcomes.is_empty() {
                continue;
            }
            if printed {
                println!();
            }
            printed = true;
            println!("{file}");
            for outcome in outcomes {
                if outcome.passed {
                    passed += 1;
                    println!("  [ok]   {}", outcome.name);
                } else {
                    failed += 1;
                    println!("  [fail] {}", outcome.name);
                    if let Some(message) = outcome.message {
                        println!("       {message}");
                    }
                }
            }
        }
        if !printed && failed == 0 {
            println!("No <test> blocks found. Write one as <test>(\"name\") {{ ... }}.");
            return 0;
        }
        println!();
        if failed == 0 {
            println!("test result: ok. {passed} passed");
            0
        } else {
            println!("test result: FAILED. {passed} passed; {failed} failed");
            1
        }
    }

    /// `file` against the working directory, without `canonicalize`, which
    /// needs a `realpath` WASI does not have.
    fn absolute(file: &str) -> PathBuf {
        let path = PathBuf::from(file);
        if path.is_absolute() {
            return path;
        }
        std::env::current_dir()
            .map(|cwd| cwd.join(&path))
            .unwrap_or(path)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::RefCell;
    use std::rc::Rc;

    fn run_capturing(source: &str) -> (String, Outcome) {
        let output = Rc::new(RefCell::new(String::new()));
        let into = Rc::clone(&output);
        oxigen_core::output::set_sink(Some(Box::new(move |t| into.borrow_mut().push_str(t))));
        let outcome = run(source, 1_000_000);
        oxigen_core::output::set_sink(None);
        let text = output.borrow().clone();
        (text, outcome)
    }

    #[test]
    fn runs_with_the_embedded_stdlib() {
        let (out, outcome) = run_capturing(
            "introduce math\nintroduce strings\nmain {\n    println(math.sqrt(16), strings.upper(\"ok\"))\n}\n",
        );
        assert_eq!(outcome, Outcome::Finished);
        assert_eq!(out, "4 OK\n");
    }

    #[test]
    fn capabilities_are_refused() {
        let (_, outcome) =
            run_capturing("introduce io\nmain {\n    io.read_file(\"/etc/passwd\")\n}\n");
        let Outcome::Failed(error) = outcome else {
            panic!("read a file")
        };
        assert!(
            error.contains("FileSystem") || error.contains("file"),
            "{error}"
        );
    }

    #[test]
    fn an_endless_loop_runs_out_of_budget() {
        let (_, outcome) = run_capturing("main {\n    repeat when True {\n    }\n}\n");
        let Outcome::Failed(error) = outcome else {
            panic!("loop finished")
        };
        assert!(error.contains("loop budget"), "{error}");
    }

    #[test]
    fn errors_render_against_main_oxi() {
        let (out, outcome) =
            run_capturing("main {\n    println(\"hi\")\n    x := [1]\n    println(x[3])\n}\n");
        assert_eq!(out, "hi\n");
        let Outcome::Failed(error) = outcome else {
            panic!("no error")
        };
        assert!(error.contains("main.oxi:4"), "{error}");
    }

    #[test]
    fn tokens_are_utf16_offsets() {
        // "é" is 2 bytes and 1 UTF-16 unit; "😀" is 4 bytes and 2 units.
        let source = "// é\nx := \"😀\" + 1";
        let tokens = tokens(source);
        let classes = |name: &str| TOKEN_CLASSES.iter().position(|c| *c == name).unwrap();
        assert_eq!(
            tokens,
            format!(
                "[0,4,{},10,14,{},17,18,{}]",
                classes("tk-com"),
                classes("tk-str"),
                classes("tk-num")
            )
        );
    }

    #[test]
    fn formats_and_checks() {
        assert_eq!(
            format("main {\nprintln(1)\n}").unwrap(),
            "main {\n    println(1)\n}\n"
        );
        assert_eq!(check("main {\n    println(1)\n}\n"), "[]");
        assert!(check("main {\n    x :=\n}\n").contains("\"severity\":\"error\""));
    }
}
