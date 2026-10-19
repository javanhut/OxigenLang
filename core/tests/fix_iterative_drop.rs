//! Regression: freeing a deeply nested value doesn't overflow the native stack.
//!
//! Dropping `[[[...]]]` ran the compiler-generated drop glue: the outer array
//! dropped its element, which dropped its element, one Rust frame per level.
//! A script that built a few hundred thousand levels (`x = [x]` in a loop,
//! a long linked list of struct nodes, an enum `Cons` chain) aborted the
//! whole process with a stack overflow the moment the value was released —
//! on reassignment, at scope exit, or when the VM itself was dropped. With
//! `panic = "abort"` or a host's small thread stack there is nothing to catch.
//!
//! Each case runs on a thread with a fixed 1 MiB stack, so a recursive drop
//! fails deterministically instead of depending on the default stack size.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

const DEPTH: usize = 200_000;

fn run_small_stack(source: String, make: fn() -> VM) -> Result<String, String> {
    std::thread::Builder::new()
        .stack_size(1 << 20)
        .spawn(move || {
            let lexer = Lexer::new(&source);
            let mut parser = Parser::new(lexer, &source);
            let program = parser.parse_program();
            assert!(parser.errors().is_empty(), "parser errors:\n{}", parser.format_errors());
            let function = Compiler::new()
                .compile(&program)
                .unwrap_or_else(|e| panic!("compile errors: {e:?}"));
            let mut vm = make();
            let out = vm.run(function).map(|v| format!("{v}")).map_err(|e| e.message);
            // Dropping the VM frees whatever the script still holds.
            drop(vm);
            out
        })
        .unwrap()
        .join()
        .expect("the VM thread panicked")
}

fn assert_frees(source: &str, expected: &str) {
    let source = source.replace("DEPTH", &DEPTH.to_string());
    for (name, make) in [
        ("interpreter", VM::new_interpreter as fn() -> VM),
        ("eager_jit", VM::new_eager_jit),
    ] {
        let got = run_small_stack(source.clone(), make);
        assert_eq!(got.as_deref(), Ok(expected), "{name}:\n{source}");
    }
}

#[test]
fn nested_arrays_free_on_reassignment() {
    assert_frees("x := []\neach i in range(DEPTH) { x = [x] }\nx = 0\n\"done\"", "done");
}

#[test]
fn nested_tuples_free_on_reassignment() {
    assert_frees("x := (0,)\neach i in range(DEPTH) { x = (x, i) }\nx = 0\n\"done\"", "done");
}

#[test]
fn nested_maps_free_on_reassignment() {
    assert_frees(
        "m := {\"end\": True}\neach i in range(DEPTH) { m = {\"next\": m} }\nm = 0\n\"done\"",
        "done",
    );
}

#[test]
fn a_long_struct_linked_list_frees() {
    assert_frees(
        "struct Node {\n    next <Node>\n}\nn := Node(None)\neach i in range(DEPTH) { n = Node(n) }\nn = 0\n\"done\"",
        "done",
    );
}

#[test]
fn an_enum_cons_chain_frees() {
    assert_frees(
        "enum L {\n    Cons(next <L>)\n    Nil\n}\nl := L.Nil\neach i in range(DEPTH) { l = L.Cons(l) }\nl = 0\n\"done\"",
        "done",
    );
}

#[test]
fn a_mixed_chain_frees() {
    assert_frees(
        "struct Box {\n    inner <generic>\n}\nx := []\neach i in range(DEPTH) {\n    x = option {\n        i % 4 == 0 -> [x]\n        i % 4 == 1 -> (x, 1)\n        i % 4 == 2 -> {\"k\": x}\n        Box(x)\n    }\n}\nx = 0\n\"done\"",
        "done",
    );
}

#[test]
fn a_deep_value_still_alive_when_the_vm_drops_is_freed() {
    assert_frees("x := []\neach i in range(DEPTH) { x = [x] }\n\"done\"", "done");
}

#[test]
fn a_deep_local_frees_when_its_function_returns() {
    assert_frees(
        "fun build(n) {\n    x := []\n    each i in range(n) { x = [x] }\n    len(x)\n}\nbuild(DEPTH)",
        "1",
    );
}

#[test]
fn shared_children_outlive_their_containers() {
    // Only containers whose last owner is the dropped value may be emptied.
    assert_frees(
        "a := [7]\nm := {\"k\": a}\nx := [a, (a, m), [[a]]]\nx = 0\nm = 0\na[0]",
        "7",
    );
    assert_frees(
        "struct Node {\n    next <Node>\n}\nshared := Node(None)\nl := [Node(shared), Node(Node(shared))]\nl = 0\nshared.next == None",
        "True",
    );
}
