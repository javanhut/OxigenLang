//! Regression: slicing works in elements, and out-of-range bounds clamp.
//!
//! - String slices used BYTE offsets (`&s[start..end]`) while indexing
//!   counts characters. `"héllo"[0:2]` panicked ("not a char
//!   boundary") — an abort in release builds — and `"héllo"[1:3]` returned
//!   `é` instead of `él`.
//! - A negative bound below `-len` wrapped through `usize` and clamped to the
//!   wrong end: `[1,2,3,4,5][-100:2]` gave `[]`, `[1:-100]` gave `[2,3,4,5]`.
//! - Tuple slices ignored negative bounds: `(1,2,3)[-2:]` gave `()`.

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
fn string_slices_count_characters() {
    assert_all_ok("\"héllo\"[0:2]", "hé");
    assert_all_ok("\"héllo\"[1:3]", "él");
    assert_all_ok("\"héllo\"[1:2]", "é");
    assert_all_ok("\"日本語\"[1:2]", "本");
    assert_all_ok("\"héllo\"[3:]", "lo");
    assert_all_ok("\"héllo\"[-2:]", "lo");
    assert_all_ok("\"héllo\"[:-3]", "hé");
    assert_all_ok("\"héllo\"[1:]", "éllo");
}

#[test]
fn string_slices_inside_a_function() {
    assert_all_ok("fun f(s) { s[1:3] }\nf(\"naïve\")", "aï");
}

#[test]
fn out_of_range_bounds_clamp_to_the_near_end() {
    assert_all_ok("[1, 2, 3, 4, 5][-100:2]", "[1, 2]");
    assert_all_ok("[1, 2, 3, 4, 5][1:-100]", "[]");
    assert_all_ok("[1, 2, 3, 4, 5][-100:]", "[1, 2, 3, 4, 5]");
    assert_all_ok("\"hello\"[-100:2]", "he");
    assert_all_ok("\"hello\"[2:-100]", "");
}

#[test]
fn tuple_slices_honour_negative_bounds() {
    assert_all_ok("(1, 2, 3)[-2:]", "(2, 3)");
    assert_all_ok("(1, 2, 3)[:-1]", "(1, 2)");
    assert_all_ok("(1, 2, 3)[-100:1]", "(1,)");
}
