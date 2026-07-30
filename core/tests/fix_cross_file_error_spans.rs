//! Regression: a runtime error inside an imported function must render against
//! the file that function was compiled from.
//!
//! A frame's line number is meaningless without the source it indexes into, and
//! the VM held exactly one `source` — the entry script's. So an error raised on
//! line 8 of an imported module underlined line 8 of the *entry script*: real
//! code, correct-looking, and completely unrelated to the failure. When the
//! entry file was shorter than the reported line the snippet silently rendered
//! blank instead.
//!
//! Functions now carry a `ModuleOrigin` (file + source) stamped at import, and
//! the renderer resolves the innermost frame's own source.

use std::path::PathBuf;

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

/// Runs `source` as the entry script located at `file`, returning the error text.
fn run_expecting_error(source: &str, file: PathBuf) -> String {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "parse errors:\n{}",
        parser.format_errors()
    );
    let function = Compiler::new()
        .compile(&program)
        .map_err(|e| format!("{:?}", e))
        .expect("compile should succeed");
    let mut vm = VM::new();
    vm.set_source(source);
    vm.set_file(file);
    match vm.run(function) {
        Ok(v) => panic!("expected a runtime error, got {}", v),
        Err(e) => e.message,
    }
}

/// A module whose failing statement sits on a line that also exists — with
/// entirely different, innocuous content — in the entry script.
fn write_fixture(dir: &PathBuf) {
    std::fs::create_dir_all(dir).expect("mk tempdir");
    std::fs::write(
        dir.join("mylib.oxi"),
        // The undefined variable is on line 8.
        "// 1\n// 2\n// 3\n// 4\n// 5\n// 6\nfun boom() {\n    undefined_thing_in_mylib\n}\n",
    )
    .expect("write module");
}

#[test]
fn error_in_imported_function_points_at_the_module_not_the_entry_script() {
    let dir = std::env::temp_dir().join(format!("oxi_xfile_span_{}", std::process::id()));
    write_fixture(&dir);

    // Line 8 of the entry script is a decoy: it is real code, and it is what
    // the renderer used to underline.
    let source = "introduce .mylib\nx := 1\nx := 2\nx := 3\nx := 4\nx := 5\nx := 6\n\
                  decoy_line_eight := 99\nx := 8\nmylib.boom()\n";
    let err = run_expecting_error(source, dir.join("main.oxi"));

    assert!(
        err.contains("undefined_thing_in_mylib"),
        "expected the module's error, got:\n{err}"
    );
    assert!(
        err.contains("mylib.oxi:8"),
        "location must name the module and its line, got:\n{err}"
    );
    assert!(
        !err.contains("decoy_line_eight"),
        "rendered the entry script's line 8 instead of the module's:\n{err}"
    );

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn stack_trace_names_the_file_of_each_frame() {
    let dir = std::env::temp_dir().join(format!("oxi_xfile_trace_{}", std::process::id()));
    write_fixture(&dir);

    let source = "introduce .mylib\nmylib.boom()\n";
    let err = run_expecting_error(source, dir.join("main.oxi"));

    assert!(
        err.contains("stack trace"),
        "expected a stack trace, got:\n{err}"
    );
    // The callee frame belongs to the module, the caller to the entry script.
    assert!(
        err.contains("`boom` (") && err.contains("mylib.oxi:8"),
        "callee frame must name the module, got:\n{err}"
    );
    assert!(
        err.contains("main.oxi:2"),
        "caller frame must name the entry script, got:\n{err}"
    );

    let _ = std::fs::remove_dir_all(&dir);
}

/// The blank-snippet case: the module's failing line is past the end of the
/// entry script, so the old renderer found no line at all and printed nothing.
#[test]
fn module_line_beyond_the_entry_script_still_renders_source() {
    let dir = std::env::temp_dir().join(format!("oxi_xfile_short_{}", std::process::id()));
    write_fixture(&dir);

    // Two lines long; the failure is on line 8 of the module.
    let source = "introduce .mylib\nmylib.boom()\n";
    let err = run_expecting_error(source, dir.join("main.oxi"));

    assert!(
        err.contains("undefined_thing_in_mylib"),
        "the offending source line must still be rendered, got:\n{err}"
    );

    let _ = std::fs::remove_dir_all(&dir);
}
