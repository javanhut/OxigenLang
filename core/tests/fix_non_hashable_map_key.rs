//! Map keys and set elements must be hashable.
//!
//! `Value::try_hash_key` returns `None` for arrays, maps, sets, struct instances
//! and closures, and every `OxMap`/`OxSet` operation quietly fell back to a
//! linear scan for those. So `{[1, 2]: "v"}` was accepted, and the map it built
//! had two defects the program was never told about: lookups against it were
//! O(n) instead of O(1), and the key was mutable, so it could be changed after
//! insertion and no longer match its own entry.
//!
//! The refusal goes at the six places a user-controlled key can enter — the
//! `BuildMap`/`BuildSet` opcodes, index assignment, and the `insert()`/`set()`
//! builtins. The other `from_pairs`/`from_iter_dedup` callers (JSON and TOML
//! parsing, env vars, process output, HTTP responses, cross-task transfer) build
//! their keys from strings by construction and cannot carry a bad one.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn run(source: &str) -> Result<String, String> {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "parser errors for:\n{source}\n{}",
        parser.format_errors()
    );
    let function = Compiler::new()
        .compile(&program)
        .unwrap_or_else(|errs| panic!("compile errors: {errs:?}"));
    let mut vm = VM::new();
    vm.set_source(source);
    vm.set_script_args(&[]);
    vm.run(function).map(|v| format!("{v}")).map_err(|e| e.message)
}

fn assert_rejected(source: &str, needle: &str) {
    match run(source) {
        Ok(v) => panic!("expected a rejection for:\n{source}\ngot: {v}"),
        Err(e) => assert!(
            e.contains(needle),
            "wrong message for:\n{source}\nwanted {needle:?}, got: {e}"
        ),
    }
}

// ── the six entry points a user-controlled key can arrive through ─────────

#[test]
fn a_map_literal_rejects_a_non_hashable_key() {
    assert_rejected("m := {[1, 2]: \"v\"}", "ARRAY is not hashable");
    assert_rejected("k := {\"a\": 1}\nm := {k: \"v\"}", "MAP is not hashable");
}

#[test]
fn index_assignment_rejects_a_non_hashable_key() {
    assert_rejected("m := {}\nm[[1]] = 1", "ARRAY is not hashable");
}

#[test]
fn the_insert_builtin_rejects_a_non_hashable_map_key() {
    assert_rejected("insert({}, [1], \"v\")", "ARRAY is not hashable");
}

#[test]
fn the_insert_builtin_rejects_a_non_hashable_set_element() {
    // `insert()` is arity-3 for sets too; the set branch ignores the value.
    assert_rejected("insert(set(1), [1], 0)", "ARRAY is not hashable");
}

#[test]
fn the_set_builtin_rejects_a_non_hashable_element() {
    assert_rejected("set(1, [2, 3])", "ARRAY is not hashable");
}

#[test]
fn the_error_names_the_kind_and_carries_its_code() {
    let e = run("m := {[1]: 1}").unwrap_err();
    assert!(e.contains("E0035"), "expected code E0035, got: {e}");
    assert!(e.contains("map key"), "expected the position named, got: {e}");
}

// ── every hashable kind still works ───────────────────────────────────────

#[test]
fn all_hashable_key_kinds_are_still_accepted() {
    let src = "m := {\"s\": 1, 42: 2, True: 3, 3.5: 4, None: 5, (1, 2): 6}\nlen(m)";
    assert_eq!(run(src).unwrap(), "6");
}

#[test]
fn a_tuple_key_round_trips() {
    assert_eq!(run("m := {(1, 2): \"v\"}\nm[(1, 2)]").unwrap(), "v");
}

#[test]
fn a_tuple_holding_a_non_hashable_element_is_itself_not_hashable() {
    // `try_hash_key` recurses into tuples, so this must be refused too.
    assert_rejected("m := {(1, [2]): \"v\"}", "TUPLE is not hashable");
}

#[test]
fn index_assignment_and_insert_still_work_with_hashable_keys() {
    assert_eq!(run("m := {}\nm[\"k\"] = \"v\"\nm[\"k\"]").unwrap(), "v");
    assert_eq!(run("len(insert({}, \"k\", \"v\"))").unwrap(), "1");
}

#[test]
fn a_set_of_hashable_elements_still_dedups() {
    assert_eq!(run("len(set(1, \"a\", (2, 3), 1))").unwrap(), "3");
}
