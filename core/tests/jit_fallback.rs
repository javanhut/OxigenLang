//! Milestone 1 fallback-harness tests.
//!
//! These exercise the full JIT plumbing end-to-end: the Cranelift module
//! emits a thunk, the thunk calls `jit_run_via_interpreter`, and the helper
//! drives the interpreter for one activation. Program output must match
//! the interpreter-only baseline byte-for-byte.
//!
//! No actual bytecode translation happens yet (that's Step 3).

#![cfg(feature = "jit")]

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn run(source: &str, jit_threshold: Option<u32>) -> (String, usize, usize) {
    run_with_thresholds(source, jit_threshold, None)
}

fn run_result(source: &str, jit_threshold: Option<u32>) -> Result<String, String> {
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
        .expect("compile should succeed");

    let mut vm = VM::new();
    if let Some(t) = jit_threshold {
        vm.jit.set_threshold(t);
    }
    vm.run(function)
        .map(|v| format!("{}", v))
        .map_err(|e| e.message)
}

/// Like `run_result` but expects an Err and returns both the message
/// and the source line the VMError carries. Used by line-store elision
/// tests that need to verify the JIT writes `JitFrame.line` correctly
/// at every fault site.
fn run_expect_err(source: &str, jit_threshold: Option<u32>) -> (String, u32) {
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
        .expect("compile should succeed");
    let mut vm = VM::new();
    if let Some(t) = jit_threshold {
        vm.jit.set_threshold(t);
    }
    match vm.run(function) {
        Ok(v) => panic!("expected runtime error, got Ok({})", v),
        Err(e) => (e.message, e.line),
    }
}

fn run_with_thresholds(
    source: &str,
    jit_threshold: Option<u32>,
    loop_threshold: Option<u32>,
) -> (String, usize, usize) {
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
        .expect("compile should succeed");

    let mut vm = VM::new();
    if let Some(t) = jit_threshold {
        vm.jit.set_threshold(t);
    }
    if let Some(t) = loop_threshold {
        vm.jit.set_loop_threshold(t);
    }
    let result = vm.run(function).expect("run should succeed");
    (
        format!("{}", result),
        vm.jit.compiled_ok_count(),
        vm.jit.compile_failed_count(),
    )
}

fn run_disabled(source: &str) -> (String, usize, usize) {
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
        .expect("compile should succeed");

    let mut vm = VM::new();
    vm.jit.disable();
    let result = vm.run(function).expect("run should succeed");
    (
        format!("{}", result),
        vm.jit.compiled_ok_count(),
        vm.jit.compile_failed_count(),
    )
}

fn compile_function(source: &str) -> oxigen_core::vm::value::Function {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "parser errors:\n{}",
        parser.format_errors()
    );

    Compiler::new()
        .compile(&program)
        .expect("compile should succeed")
}

#[test]
fn fallback_matches_interpreter_simple_add() {
    // The nested fn `compute` has a body of just `Constant Constant Add
    // Return` — every opcode is in the M1 allow-list, so scan passes and
    // the fallback thunk gets installed.
    let source = r#"
fun compute() {
    1 + 2
}
compute()
"#;

    let (baseline, b_ok, b_failed) = run(source, None);
    let (jitted, j_ok, j_failed) = run(source, Some(1));

    assert_eq!(
        baseline, jitted,
        "program output must match across backends"
    );
    assert_eq!(
        b_ok, 0,
        "with JIT disabled (default threshold), nothing should compile"
    );
    assert_eq!(b_failed, 0);
    assert!(
        j_ok >= 1,
        "JIT should have compiled at least `compute` (got {} ok, {} failed)",
        j_ok,
        j_failed
    );
}

#[test]
fn fallback_matches_interpreter_arith_via_args() {
    // The function body is pure arithmetic on a parameter — exercises
    // GetLocal + Constant + Add + Multiply + Return, all in the M1
    // allow-list. Type annotations trigger `TypeWrap`, which is NOT in
    // the allow-list, so we deliberately leave the parameter untyped.
    let source = r#"
fun poly(x) {
    x * x + x + 1
}
poly(5)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, j_failed) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert!(
        j_ok >= 1,
        "expected a compile; got {} ok, {} failed",
        j_ok,
        j_failed
    );
}

#[test]
fn fallback_matches_interpreter_conditional_branch() {
    // Exercises JumpIfFalse + Jump + Pop: the `option` construct compiles
    // to a classic then/else branch.
    let source = r#"
fun max2(a, b) {
    option { a > b -> a, b }
}
max2(3, 7)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert!(j_ok >= 1, "max2 should compile");
}

#[test]
fn fallback_matches_interpreter_recursive_fib() {
    // True recursive fib — requires GetGlobal (fib references itself via
    // the enclosing scope). Exercises JIT-to-JIT recursion with globals.
    let source = r#"
fun fib(n) {
    option { n < 2 -> n, fib(n - 1) + fib(n - 2) }
}
fib(10)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert!(j_ok >= 1, "fib should compile; got {} ok", j_ok);
}

#[test]
fn fallback_matches_interpreter_recursive_globals_other_than_self() {
    let source = r#"
step := 3

fun sumdown(n) {
    option { n < 1 -> 0, step + sumdown(n - 1) }
}

sumdown(5)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert_eq!(jitted, "15");
    assert!(j_ok >= 1, "sumdown should compile; got {} ok", j_ok);
}

#[test]
fn fallback_matches_interpreter_compiled_recursion_with_interpreted_helper() {
    let source = r#"
fun helper(x) {
    (x,)
}

fun walk(n) {
    option { n < 1 -> 0, helper(walk(n - 1) + 1)[0] }
}

walk(6)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, j_failed) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert_eq!(jitted, "6");
    assert!(
        j_ok >= 1,
        "walk should compile even though helper stays interpreted; got {} ok, {} failed",
        j_ok,
        j_failed
    );
}

#[test]
fn fallback_matches_interpreter_closure_with_upvalue() {
    // Verifies Closure + GetUpvalue. `make_adder` returns a closure that
    // captures `n` as an upvalue; the inner closure reads it via
    // GetUpvalue and adds it to its argument.
    let source = r#"
fun make_adder(n) {
    fun(x) { x + n }
}
add5 := make_adder(5)
add5(10)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert!(
        j_ok >= 2,
        "make_adder and the inner closure should both compile; got {} ok",
        j_ok
    );
}

#[test]
fn fallback_matches_interpreter_div_mod_and_eq() {
    let source = r#"
fun collatzish(n) {
    option {
        n % 2 == 0 -> n / 2,
        n * 3 + 1
    }
}
collatzish(10) + collatzish(9)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert!(j_ok >= 1, "expected collatzish to compile");
}

#[test]
fn fallback_matches_interpreter_divide_by_zero_error() {
    let source = r#"
fun boom(n) {
    n / 0
}
boom(10)
"#;

    let baseline = run_result(source, None).expect_err("baseline should error");
    let jitted = run_result(source, Some(1)).expect_err("jitted run should error");

    assert_eq!(baseline, jitted);
}

#[test]
fn fallback_matches_interpreter_jit_to_jit_call() {
    // Exercises the Call opcode from inside JIT code. `apply` receives the
    // callee as an argument (a local) rather than a global, so its body
    // stays inside the M1 allow-list (GetLocal / Call / Return). When
    // `apply` invokes `double`, both call sides run JIT-compiled code.
    //
    // (A classic recursive fib can't be used here: fib references itself
    // via `GetGlobal`, which is intentionally outside M1.)
    let source = r#"
fun double(x) {
    x * 2
}

fun apply(f, x) {
    f(x)
}

apply(double, 5)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert!(
        j_ok >= 2,
        "both `double` and `apply` should compile; got {}",
        j_ok
    );
}

#[test]
fn fallback_matches_interpreter_struct_methods() {
    // Exercises StructLiteral / GetField / MethodCall all inside JIT
    // code.
    let source = r#"
struct Point {
    x <int>
    y <int>
}

Point contains {
    fun sum_xy() { self.x + self.y }
}

p <Point> := Point(3, 4)
p.sum_xy()
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert!(j_ok >= 1, "expected some compile; got {}", j_ok);
}

#[test]
fn fallback_matches_interpreter_struct_method_mutation_shapes() {
    let source = r#"
struct Counter {
    val <int>
}

Counter contains {
    fun inc() { self.val = self.val + 1 }
    fun add(amount) { self.val = self.val + amount }
}

fun run() {
    c <Counter> := Counter(0)
    c.inc()
    c.add(4)
    c.val
}

run()
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert_eq!(jitted, "5");
    assert!(j_ok >= 2, "expected method bodies to compile; got {}", j_ok);
}

#[test]
fn fallback_matches_interpreter_struct_fields_holding_struct_instances() {
    let source = r#"
struct Node {
    data <generic>
    next <Node> || <None>
}

struct LinkedList {
    head <Node> || <None>
}

LinkedList contains {
    fun add(data <generic>) {
        new_node <Node> := Node { data: data, next: None }
        option {
            self.head == None -> self.head = new_node,
            self.append_node(new_node)
        }
    }

    fun append_node(new_node <Node>) {
        current <Node> || <None> := self.head
        repeat unless current.next == None {
            current = current.next
        }
        current.next = new_node
    }

    fun values() {
        current <Node> || <None> := self.head
        acc := 0
        repeat unless current == None {
            acc = acc + current.data
            current = current.next
        }
        acc
    }
}

fun run() {
    ll <LinkedList> := LinkedList(None)
    ll.add(10)
    ll.add(20)
    ll.add(30)
    ll.values()
}

run()
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert_eq!(jitted, "60");
    assert!(
        j_ok >= 2,
        "expected linked-list methods to compile; got {}",
        j_ok
    );
}

#[test]
fn fallback_matches_interpreter_mixed_numeric_add() {
    // Integer+Float takes the slow path in the JIT (tag check fails on
    // the right operand). Make sure the slow-path fallback still
    // produces the correct promoted-float result.
    let source = r#"
fun addem(n) {
    n + 1.5
}
addem(2)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert!(j_ok >= 1);
}

#[test]
fn jit_local_const_update_peephole_matches_interpreter() {
    let source = r#"
fun update(n) {
    x := 1
    x := x + 4
    x := x * 3
    x := x - 2
    x := x - n
    x
}
update(2)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert_eq!(jitted, "11");
    assert!(j_ok >= 1);
}

#[test]
fn jit_local_const_update_peephole_slow_path_keeps_float_semantics() {
    let source = r#"
fun update() {
    x := 1.5
    x := x + 2
    x
}
update()
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert_eq!(jitted, "3.5");
    assert!(j_ok >= 1);
}

#[test]
fn jit_local_const_update_peephole_requires_same_destination_slot() {
    let source = r#"
fun update() {
    a := 1
    b := 10
    a := b + 1
    a * 100 + b
}
update()
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert_eq!(jitted, "1110");
    assert!(j_ok >= 1);
}

#[test]
fn jit_local_scaled_update_peephole_matches_loop_shape() {
    let source = r#"
fun update(n) {
    total := 0
    i := 1
    repeat when i <= n {
        total := total + i * 2
        i := i + 1
    }
    total
}
update(5)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert_eq!(jitted, "30");
    assert!(j_ok >= 1);
}

#[test]
fn jit_local_scaled_update_peephole_supports_subtract() {
    let source = r#"
fun update() {
    total := 100
    i := 3
    total := total - i * 4
    total
}
update()
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert_eq!(jitted, "88");
    assert!(j_ok >= 1);
}

#[test]
fn jit_local_scaled_update_peephole_slow_path_keeps_float_semantics() {
    let source = r#"
fun update() {
    total := 1.5
    i := 3
    total := total + i * 2
    total
}
update()
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert_eq!(jitted, "7.5");
    assert!(j_ok >= 1);
}

#[test]
fn default_jit_compiles_single_call_hot_loop_by_backedge_count() {
    let source = r#"
fun update(n) {
    total := 0
    i := 1
    repeat when i <= n {
        total := total + i * 2
        i := i + 1
    }
    total
}
update(20)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run_with_thresholds(source, None, Some(3));

    assert_eq!(baseline, jitted);
    assert_eq!(jitted, "420");
    assert!(
        j_ok >= 1,
        "loop-hot function should compile even though it is called once"
    );
}

#[test]
fn no_jit_disables_loop_hot_compilation() {
    let source = r#"
fun update(n) {
    total := 0
    i := 1
    repeat when i <= n {
        total := total + i * 2
        i := i + 1
    }
    total
}
update(20)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, j_failed) = run_disabled(source);

    assert_eq!(baseline, jitted);
    assert_eq!(j_ok, 0);
    assert_eq!(j_failed, 0);
}

#[test]
fn unsupported_opcodes_bail_out_cleanly() {
    // Map literals are still outside the JIT allow-list, so the top-level
    // scan must fail and the interpreter must take over without disruption.
    let source = r#"
m := {"x": 7}
m["x"]
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, _, j_failed) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert!(
        j_failed >= 1,
        "top-level with map literal should have been rejected by scan"
    );
}

#[test]
fn typed_local_metadata_preserves_type_lock() {
    let function = compile_function(
        r#"
fun typed(a <int>, b <array>) {
    c <float> := 1.5
    b[0] + a
}
typed(2, [40])
"#,
    );

    let closure = function
        .chunk
        .constants
        .iter()
        .find_map(|v| match v {
            oxigen_core::vm::value::Value::Closure(c) => Some(c),
            _ => None,
        })
        .expect("expected closure constant");
    let locals = &closure.function.locals;
    assert_eq!(locals[1].type_constraint.as_deref(), Some("INTEGER"));
    assert_eq!(locals[2].type_constraint.as_deref(), Some("ARRAY"));
    assert_eq!(locals[3].type_constraint.as_deref(), Some("FLOAT"));
    assert!(locals[1].mutable);
    assert!(locals[2].mutable);
}

#[test]
fn jit_compiles_array_indexing() {
    let source = r#"
fun pick() {
    arr <array> := [10, 20, 30]
    arr[1] + arr[-1]
}
pick()
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, "50");
    assert_eq!(baseline, jitted);
    assert!(j_ok >= 1, "array indexing function should compile");
}

#[test]
fn jit_array_index_out_of_range_matches_interpreter() {
    let source = r#"
fun pick() {
    arr <array> := [10, 20, 30]
    arr[99]
}
pick()
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, "None");
    assert_eq!(baseline, jitted);
    assert!(j_ok >= 1, "array indexing function should compile");
}

#[test]
fn jit_fuses_local_array_mod_index_sum_loop() {
    let source = r#"
fun array_index_sum(n <int>) {
    arr <array> := [1, 2, 3, 4]
    i <int> := 0
    total <int> := 0
    repeat unless i >= n {
        total = total + arr[i % 4]
        i = i + 1
    }
    total
}
array_index_sum(20)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run_with_thresholds(source, Some(1), Some(1));

    assert_eq!(baseline, "50");
    assert_eq!(baseline, jitted);
    assert!(j_ok >= 1, "array sum loop should compile");
}

#[test]
fn jit_fuses_local_to_local_sum_loop() {
    let source = r#"
fun loop_sum(n <int>) {
    i <int> := 0
    total <int> := 0
    repeat unless i >= n {
        total = total + i
        i = i + 1
    }
    total
}
loop_sum(20)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, j_failed) = run_with_thresholds(source, Some(1), Some(1));

    assert_eq!(baseline, "190");
    assert_eq!(baseline, jitted);
    assert!(
        j_ok >= 2,
        "top-level and local-to-local sum loop should compile (ok={j_ok}, failed={j_failed})"
    );
}

// ── MethodCall IC hit path refcount regression tests ─────────────────────
//
// These two tests pin down the inline MethodCall IR fast path's
// refcount + Vec::len semantics. The disabled-then-re-enabled code
// (see `core/src/jit/engine.rs` OpCode::MethodCall with arg_count <= 1)
// must (a) bump the method closure's Rc strong count before stamping a
// synthetic Value::Closure on the stack, and (b) sync Vec::len via
// `jit_stack_commit_len` so bounds-checked helpers see the new slots.
// Without either, a tight method-call loop crashes within a few
// iterations (Rc free while cache still holds the pointer; or OOB
// panic on `self.stack[idx]`). 5000 iterations is more than enough to
// surface either bug if reintroduced.

#[test]
fn method_call_ic_hit_path_getter_loop_is_refcount_safe() {
    // arg_count == 0 getter — exercises the `if arg_count == 0` branch:
    // one emit_copy_value (move receiver) + emit_write_closure_value.
    let source = r#"
struct C { val <int> }
C contains { fun get() { self.val } }
fun run(n <int>) {
    c <C> := C(42)
    total <int> := 0
    i <int> := 0
    repeat when i < n {
        total = total + c.get()
        i = i + 1
    }
    total
}
run(5000)
"#;
    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, j_failed) = run_with_thresholds(source, Some(1), Some(1));

    assert_eq!(baseline, jitted);
    assert!(
        j_ok >= 2,
        "run + get should both JIT (ok={j_ok}, failed={j_failed})"
    );
}

#[test]
fn method_call_ic_hit_path_setter_loop_is_refcount_safe() {
    // arg_count == 1 setter — exercises the `else` branch of the
    // inline IR: two emit_copy_value calls (move arg, then move
    // receiver over arg's old slot) + emit_write_closure_value.
    let source = r#"
struct C { val <int> }
C contains {
    fun add(v <int>) { self.val = self.val + v }
    fun get() { self.val }
}
fun run(n <int>) {
    c <C> := C(0)
    i <int> := 0
    repeat when i < n {
        c.add(i)
        i = i + 1
    }
    c.get()
}
run(5000)
"#;
    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, j_failed) = run_with_thresholds(source, Some(1), Some(1));

    assert_eq!(baseline, jitted);
    assert!(
        j_ok >= 2,
        "run + add + get should JIT (ok={j_ok}, failed={j_failed})"
    );
}

// ── Line-store elision regression tests ──────────────────────────────
//
// These cover the `opcode_always_needs_line` / `maybe_emit_current_line`
// path in `core/src/jit/engine/mod.rs`. The JIT now elides writes to
// `JitFrame.line` for opcodes whose emission cannot reach a runtime
// helper that calls `vm.jit.stash_error()` from this IP. The slow
// paths of arithmetic/comparison opcodes (and every always-fault op)
// still write the line. These tests confirm that runtime errors raised
// by either path report the correct source line.

#[test]
fn jit_line_store_correct_for_modulo_by_zero_in_hot_loop() {
    // `total % 0` triggers `binary_mod` → "modulo by zero" on the
    // first iteration. The Modulo opcode is on the source line written
    // below; the line-elision change must still stamp the line at the
    // virt-int divmod emit site.
    let source = "
fun bad(n <int>) {
    total <int> := 0
    i <int> := 0
    repeat when i < n {
        total = total + (i % 0)
        i = i + 1
    }
    total
}
bad(5)
";
    let (msg, line) = run_expect_err(source, Some(1));
    assert!(msg.contains("modulo by zero"), "msg = {msg:?}");
    // The `i % 0` expression spans line 6 in the source above (line 1
    // is empty, line 2 is `fun bad...`, etc.). 1-indexed.
    assert_eq!(line, 6, "expected line 6, got {} (msg: {msg})", line);
}

#[test]
fn jit_line_store_correct_for_type_mismatch_in_arith() {
    // Hot int loop suddenly hits a string addition — the slow-path
    // helper raises a type-mismatch error from inside the JIT-emitted
    // arith fast path's fallback. The line must point at the offending
    // `+`, not at any preceding GetLocal/Constant.
    let source = "
fun trigger() {
    a := 1
    b := \"hello\"
    a + b
}
trigger()
";
    let (msg, line) = run_expect_err(source, Some(1));
    assert!(
        msg.contains("type mismatch") || msg.contains("operands must"),
        "msg = {msg:?}"
    );
    // `a + b` is on source line 5.
    assert_eq!(line, 5, "expected line 5, got {} (msg: {msg})", line);
}

#[test]
fn jit_line_store_correct_for_undefined_global_call() {
    // A `Call` opcode is in the always-need-line set. When the callee
    // is undefined, `handle_get_global` raises with the current line.
    let source = "
fun outer() {
    1 + 2
    not_a_real_fn()
}
outer()
";
    let (msg, line) = run_expect_err(source, Some(1));
    assert!(
        msg.contains("undefined") || msg.contains("not_a_real_fn"),
        "msg = {msg:?}"
    );
    // The undefined call is on source line 4.
    assert_eq!(line, 4, "expected line 4, got {} (msg: {msg})", line);
}

#[test]
fn jit_line_store_correct_for_int_divide_by_zero() {
    // The proven-int Divide path emits its own zero / overflow guards
    // that exit through `exit_block` with a stashed error. Make sure
    // the line is written before that emission so the resulting
    // VMError points at the divide.
    let source = "
fun zero_div() {
    a <int> := 10
    b <int> := 0
    a / b
}
zero_div()
";
    let (msg, line) = run_expect_err(source, Some(1));
    assert!(
        msg.contains("zero") || msg.contains("division"),
        "msg = {msg:?}"
    );
    // `a / b` is on source line 5.
    assert_eq!(line, 5, "expected line 5, got {} (msg: {msg})", line);
}

#[test]
fn jit_line_store_correct_for_typed_negate_on_non_numeric() {
    // Negate is in the always-need-line set. Negating a string
    // surfaces a type error attributed to the unary minus.
    let source = "
fun neg_bad() {
    s := \"text\"
    -s
}
neg_bad()
";
    let (msg, line) = run_expect_err(source, Some(1));
    assert!(
        msg.contains("negation") || msg.contains("number"),
        "msg = {msg:?}"
    );
    assert_eq!(line, 4, "expected line 4, got {} (msg: {msg})", line);
}

#[test]
fn jit_line_store_correct_for_index_on_non_collection() {
    // `Index` is in the always-need-line set. Indexing a non-
    // collection value (here, an integer) surfaces a type error
    // from `eval_index`. (Out-of-range indices on real arrays return
    // `None` in Oxigen; only structurally invalid indexing errors,
    // so we use a non-collection target to trigger the helper-stash
    // path.)
    let source = "
fun bad_index() {
    a := 5
    a[0]
}
bad_index()
";
    let (msg, line) = run_expect_err(source, Some(1));
    assert!(
        msg.contains("Index")
            || msg.contains("index")
            || msg.contains("not indexable")
            || msg.contains("subscript"),
        "msg = {msg:?}"
    );
    // The indexing expression `a[0]` is on line 4.
    assert_eq!(line, 4, "expected line 4, got {} (msg: {msg})", line);
}

// ── Bitwise + Log JIT coverage (Tier 1) ───────────────────────────────
//
// Oxigen has no 0b/0x literal prefixes, no `while`, and no `return` —
// these tests use decimal integers, `repeat when`, and option-as-value.

#[test]
fn fallback_bitwise_band_matches_interpreter() {
    // 12 & 10 == 8
    let source = r#"
fun f() { 12 & 10 }
f()
"#;
    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));
    assert_eq!(baseline, jitted);
    assert!(j_ok >= 1, "JIT should compile f");
}

#[test]
fn fallback_bitwise_bor_matches_interpreter() {
    // 12 | 10 == 14
    let source = r#"
fun f() { 12 | 10 }
f()
"#;
    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));
    assert_eq!(baseline, jitted);
    assert!(j_ok >= 1);
}

#[test]
fn fallback_bitwise_bxor_matches_interpreter() {
    // 12 ^ 10 == 6
    let source = r#"
fun f() { 12 ^ 10 }
f()
"#;
    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));
    assert_eq!(baseline, jitted);
    assert!(j_ok >= 1);
}

#[test]
fn fallback_bitwise_bnot_matches_interpreter() {
    let source = r#"
fun f() { ~5 }
f()
"#;
    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));
    assert_eq!(baseline, jitted);
    assert!(j_ok >= 1);
}

#[test]
fn fallback_bitwise_shl_matches_interpreter() {
    // 1 << 4 == 16
    let source = r#"
fun f() { 1 << 4 }
f()
"#;
    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));
    assert_eq!(baseline, jitted);
    assert!(j_ok >= 1);
}

#[test]
fn fallback_bitwise_shr_matches_interpreter() {
    // 256 >> 3 == 32
    let source = r#"
fun f() { 256 >> 3 }
f()
"#;
    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));
    assert_eq!(baseline, jitted);
    assert!(j_ok >= 1);
}

#[test]
fn fallback_bitwise_loop_virt_int_fast_path() {
    // total = total ^ i across a tight loop — exercises the virt-int
    // SSA fast path. The single-Constant inits `total := 0` and
    // `i := 0` avoid an unrelated pre-existing JIT bug in multi-op
    // initializers (`c := 1 + 5` style); the BitXor in the loop body
    // exercises in-place update semantics, which is the shape we want
    // to cover here.
    let source = r#"
fun loop_xor(n) {
    total := 0
    i := 0
    repeat when i < n {
        total = total ^ i
        i = i + 1
    }
    total
}
loop_xor(64)
"#;
    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));
    assert_eq!(baseline, jitted);
    assert!(j_ok >= 1);
}

// Shift edges, split per-case so a failure tells us which edge diverged.
// Pinned semantics: shift count masked to low 6 bits (matches x86
// SHL/SAR + Cranelift ishl/sshr + Rust wrapping_shl/shr).

fn check_match(source: &str) {
    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));
    assert_eq!(baseline, jitted, "JIT and interpreter must agree on shift");
    assert!(j_ok >= 1);
}

#[test]
fn fallback_shift_edge_shl_zero() {
    check_match("fun f() { 1 << 0 }\nf()");
}

#[test]
fn fallback_shift_edge_shl_one() {
    check_match("fun f() { 1 << 1 }\nf()");
}

#[test]
fn fallback_shift_edge_shl_sixty_three() {
    check_match("fun f() { 1 << 63 }\nf()");
}

#[test]
fn fallback_shift_edge_shl_sixty_four_masks_to_zero() {
    // 1 << 64 should equal 1 << 0 = 1 under the pinned masking semantics.
    check_match("fun f() { 1 << 64 }\nf()");
}

#[test]
fn fallback_shift_edge_shr_basic() {
    check_match("fun f() { 256 >> 3 }\nf()");
}

#[test]
fn fallback_shift_edge_shr_negative_one_arithmetic() {
    // -1 >> 1 is the arithmetic shift right on a sign-extended value:
    // still -1.
    check_match("fun f() { -1 >> 1 }\nf()");
}

// NOTE on multi-op initializers (`c := 1 << 63`, `c := 1 + 5`, etc.):
//
// A pre-existing JIT bug causes any `c := <expr>` whose initializer is
// multi-op AND starts with a Constant to mis-store: the Constant handler's
// init_slot path does `def_var(c_var, <first_constant>)` eagerly, and the
// later SetLocal's def_var doesn't override it (suspected Cranelift
// SSA/CSE interaction). Result: `c` ends up holding the LHS constant
// rather than the expression's value. Reproduces with both `1 + 5` and
// `1 << 4` — not bitwise-specific. Out of scope for the Tier 1 PR;
// existing benchmarks don't hit it because they only use single-Constant
// inits like `n := 0`. The shifts-through-arg test below avoids the
// pattern by passing the shift result as a parameter.

#[test]
fn fallback_shift_int_min_through_param() {
    // INT_MIN = 1 << 63 produced at the call site (function value, not a
    // multi-op init), then summed inside the function. Exercises an i64
    // payload at the parameter slot and an Add on top.
    let source = r#"
fun f(c) { c + 1 }
f(1 << 63)
"#;
    check_match(source);
}

#[test]
fn fallback_shift_six_via_args_sum() {
    // Same idea as the original six-locals test but with the shifted
    // values passed as arguments — bypasses the multi-op-init bug.
    let source = r#"
fun sum6(a, b, c, d, e, f) { a + b + c + d + e + f }
sum6(1 << 0, 1 << 1, 1 << 63, 1 << 64, 256 >> 3, -1 >> 1)
"#;
    check_match(source);
}

#[test]
fn fallback_bitwise_type_error_matches_interpreter() {
    // f(1) tier-ups via the int branch; f(0) then exercises the
    // JIT-compiled type-check slow path of `1.5 & 2`.
    let source = r#"
fun f(b) {
    option {
        b == 1 -> 1 & 2,
        1.5 & 2
    }
}
f(1)
f(0)
"#;
    let baseline_err = run_result(source, None).expect_err("baseline should error");
    let jitted_err = run_result(source, Some(1)).expect_err("jitted should error");
    assert_eq!(baseline_err, jitted_err);
    assert!(
        jitted_err.contains("bitwise &"),
        "expected `bitwise &` in {jitted_err:?}"
    );
}

#[test]
fn fallback_log_no_msg_matches_interpreter() {
    let source = r#"
fun f() {
    <log<INFO>>
    42
}
f()
"#;
    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));
    assert_eq!(baseline, jitted, "post-Log return value must match");
    assert!(j_ok >= 1, "JIT should compile a Log-bearing function");
}

#[test]
fn fallback_log_msg_only_matches_interpreter() {
    let source = r#"
fun f() {
    <log>("hello")
    7
}
f()
"#;
    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));
    assert_eq!(baseline, jitted);
    assert!(j_ok >= 1);
}

#[test]
fn fallback_log_tag_msg_matches_interpreter() {
    let source = r#"
fun f() {
    <log<DEBUG>>("hi")
    11
}
f()
"#;
    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));
    assert_eq!(baseline, jitted);
    assert!(j_ok >= 1);
}

#[test]
fn fallback_log_tag_sub_msg_matches_interpreter() {
    let source = r#"
fun f() {
    <log<Error<network>>>("connection lost")
    99
}
f()
"#;
    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));
    assert_eq!(baseline, jitted);
    assert!(j_ok >= 1);
}

#[test]
fn fallback_post_log_stack_coherence() {
    // Pre-Log virt-int staging then Log flush then post-Log continuation.
    // Pass the staged value as an argument to avoid the multi-op-init
    // bug noted above; the function body is single-statement so Log's
    // flush + post-Log expression is the load-bearing test surface.
    let source = r#"
fun f(x) {
    <log>("x")
    (x & 3) + 40
}
f((1 + 2) ^ 7)
"#;
    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));
    assert_eq!(baseline, jitted);
    assert!(j_ok >= 1);
}

#[test]
fn fallback_bitwise_plus_log_compiles() {
    // 4660 (0x1234) & 255 == 52; 52 << 1 == 104. `masked := n & 255`
    // starts with GetLocal (not Constant), so the multi-op-init bug
    // doesn't trip — masked stays out of int_locals and goes through
    // memory, which is correct.
    let source = r#"
fun f(n) {
    masked := n & 255
    <log>("masked")
    masked << 1
}
f(4660)
"#;
    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));
    assert_eq!(baseline, jitted);
    assert!(j_ok >= 1, "function with both bitwise and log should JIT");
}
