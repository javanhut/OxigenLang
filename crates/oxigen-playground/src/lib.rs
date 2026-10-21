//! OxigenLang compiled to WebAssembly, for the playground on oxigenlang.com.
//!
//! A script runs the way `oxigen file.oxi` runs it, but sandboxed: the VM is
//! restricted to no capabilities (no files, network, processes, stdin,
//! threads or sleeping) and given a loop budget, and the stdlib is served from
//! the copy embedded at build time. The pure language and the stdlib modules
//! built on it all work.
//!
//! The page talks to the module through linear memory. It copies UTF-8 source
//! into a buffer from `oxi_alloc`, calls `oxi_run`, `oxi_check`, `oxi_format`
//! or `oxi_highlight`, and reads the reply from `oxi_reply_ptr` and
//! `oxi_reply_len`. While a script runs, its output arrives through the
//! `oxi_host_write` import as it is printed. The page supplies the clock
//! (`oxi_host_now`, milliseconds since the epoch) and hears about a panic
//! through `oxi_host_panic` before the module traps.

use oxigen_core::compiler::Compiler;
use oxigen_core::diagnostics::{DiagnosticSink, render};
use oxigen_core::formatter::Formatter;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;
use oxigen_core::vm::value::Value;
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

#[cfg(target_arch = "wasm32")]
mod exports {
    //! The C ABI the page calls. Single-threaded, so the reply buffer is a
    //! thread local that each call replaces.

    use super::*;
    use std::cell::RefCell;

    #[link(wasm_import_module = "oxigen")]
    unsafe extern "C" {
        fn oxi_host_write(ptr: *const u8, len: usize);
        fn oxi_host_now() -> f64;
        fn oxi_host_panic(ptr: *const u8, len: usize);
    }

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

    fn setup() {
        oxigen_core::clock::set_host_clock(|| unsafe { oxi_host_now() });
        std::panic::set_hook(Box::new(|info| {
            let message = info.to_string();
            unsafe { oxi_host_panic(message.as_ptr(), message.len()) };
        }));
        oxigen_core::output::set_sink(Some(Box::new(|text: &str| unsafe {
            oxi_host_write(text.as_ptr(), text.len())
        })));
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

    /// Runs the script; the reply is `{"ok":true}` or `{"ok":false,"error":"..."}`.
    ///
    /// # Safety
    /// See `source`.
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn oxi_run(ptr: *const u8, len: usize) {
        setup();
        let outcome = run(unsafe { source(ptr, len) }, LOOP_BUDGET);
        reply(
            match outcome {
                Outcome::Finished => serde_json::json!({ "ok": true }),
                Outcome::Failed(error) => serde_json::json!({ "ok": false, "error": error }),
            }
            .to_string(),
        );
    }

    /// # Safety
    /// See `source`.
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn oxi_check(ptr: *const u8, len: usize) {
        setup();
        reply(check(unsafe { source(ptr, len) }));
    }

    /// The reply is `{"ok":true,"source":"..."}` or `{"ok":false,"error":"..."}`.
    ///
    /// # Safety
    /// See `source`.
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn oxi_format(ptr: *const u8, len: usize) {
        setup();
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
        setup();
        reply(oxigen_core::highlight::to_html(unsafe { source(ptr, len) }));
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
    fn formats_and_checks() {
        assert_eq!(
            format("main {\nprintln(1)\n}").unwrap(),
            "main {\n    println(1)\n}\n"
        );
        assert_eq!(check("main {\n    println(1)\n}\n"), "[]");
        assert!(check("main {\n    x :=\n}\n").contains("\"severity\":\"error\""));
    }
}
