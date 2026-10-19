//! Regression: `len` of a string counts characters, like indexing, slicing
//! and `each` do.
//!
//! `docs/builtins.md` says `len` returns "the number of characters in a
//! string", but the builtin returned `s.len()` — UTF-8 bytes. With slices now
//! counting characters, `s[len(s) - 1]` on non-ASCII text indexed past the
//! end, and `len("héllo")` disagreed with the five iterations of
//! `each c in "héllo"`.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn run_on(mut vm: VM, source: &str) -> Result<String, String> {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "parser errors:\n{}",
        parser.format_errors()
    );
    let function = Compiler::new()
        .compile(&program)
        .map_err(|errs| format!("{errs:?}"))?;
    vm.run(function)
        .map(|v| format!("{v}"))
        .map_err(|e| e.message)
}

fn assert_all_ok(source: &str, expected: &str) {
    for (name, make) in [
        ("interpreter", VM::new_interpreter as fn() -> VM),
        ("tiered", VM::new_tiered),
        ("eager_jit", VM::new_eager_jit),
    ] {
        let got = run_on(make(), source).unwrap_or_else(|e| panic!("{name} failed: {e}\n{source}"));
        assert_eq!(got, expected, "{name} mismatch for: {source}");
    }
}

#[test]
fn len_of_a_string_counts_characters() {
    assert_all_ok("len(\"héllo\")", "5");
    assert_all_ok("len(\"日本語\")", "3");
    assert_all_ok("len(\"\")", "0");
    assert_all_ok("len(\"ascii\")", "5");
}

#[test]
fn len_agrees_with_indexing_and_each() {
    assert_all_ok("s := \"naïve\"\ns[len(s) - 1]", "e");
    assert_all_ok(
        "n := 0\neach c in \"héllo\" {\n    n = n + 1\n}\nn == len(\"héllo\")",
        "True",
    );
}
