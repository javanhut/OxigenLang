//! JIT fallback-harness tests.
//!
//! These exercise the JIT plumbing end-to-end: the Cranelift module emits a
//! thunk, the thunk calls `jit_run_via_interpreter`, and the helper drives the
//! interpreter for one activation. Program output must match the
//! interpreter-only baseline byte-for-byte.

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

    let mut vm = if jit_threshold.is_some() {
        VM::new_tiered()
    } else {
        VM::new_interpreter()
    };
    if let Some(t) = jit_threshold {
        vm.jit.set_threshold(t);
    }
    vm.run(function)
        .map(|v| format!("{}", v))
        .map_err(|e| e.message)
}

/// Like `run_result` but expects an Err and returns both the message and the
/// source line the VMError carries.
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

/// `arr[i] = v` in a hot loop is JIT-compiled via the shared
/// `VM::eval_index_assign` helper. Covers negative and out-of-bounds (no-op)
/// indices plus read-after-write.
#[test]
fn jit_index_assign_matches_and_compiles() {
    let src = "fun t(n <int>){\n\
        arr := [10, 20, 30, 40, 50]\n\
        i <int> := 0\n\
        acc <int> := 0\n\
        repeat when i < n {\n\
            arr[0] = 99\n\
            arr[-1] = 88\n\
            arr[2] = arr[0] + arr[4]\n\
            acc = acc + arr[0] * 100000 + arr[4] * 1000 + arr[2]\n\
            i = i + 1\n\
        }\n\
        acc }\n\
        t(500)";
    let expected = "4994093500";
    assert_eq!(run_disabled(src).0, expected, "interp baseline");
    let (out, ok, failed) = run(src, Some(1));
    assert_eq!(out, expected, "jit output must match interpreter");
    assert!(
        ok >= 1,
        "expected the array-mutating fn to JIT-compile (ok={ok})"
    );
    assert_eq!(failed, 0, "no compile failures expected (failed={failed})");
}

/// `map[k] = v` index-assign also routes through the helper. The map is passed
/// in as a param.
#[test]
fn jit_map_index_assign_matches_and_compiles() {
    let src = "fun mutate(m <generic>, n <int>){\n\
        i <int> := 0\n\
        repeat when i < n { m[\"k\"] = i\n i = i + 1 }\n\
        m[\"k\"] }\n\
        fun go(){ m := {\"k\": 0}\n mutate(m, 300) }\n\
        go()";
    let expected = "299";
    assert_eq!(run_disabled(src).0, expected, "interp baseline");
    let (out, ok, _failed) = run(src, Some(1));
    assert_eq!(out, expected, "jit output must match interpreter");
    assert!(
        ok >= 1,
        "expected the map-mutating fn to JIT-compile (ok={ok})"
    );
}

/// Assigning to an index of a non-indexable value errors identically under the
/// JIT and the interpreter.
#[test]
fn jit_index_assign_error_matches_interpreter() {
    let src = "fun t(){ x <int> := 5\n x[0] = 1\n x }\nt()";
    let interp = run_result(src, None);
    let jit = run_result(src, Some(1));
    assert!(interp.is_err(), "interp should error: {interp:?}");
    assert_eq!(
        interp, jit,
        "JIT and interpreter must report the same error"
    );
}

#[test]
fn fallback_matches_interpreter_simple_add() {
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
    let source = r#"
struct Point {
    x <int>
    y <int>
}

Point includes {
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

Counter includes {
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

LinkedList includes {
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
    for index in ["99", "-4"] {
        let source = format!(
            "\nfun pick() {{\n    arr <array> := [10, 20, 30]\n    arr[{index}]\n}}\npick()\n"
        );
        let baseline = run_result(&source, None).expect_err("interpreter should error");
        let jitted = run_result(&source, Some(1)).expect_err("jit should error");
        assert!(baseline.contains("out of range"), "{baseline}");
        assert_eq!(
            baseline, jitted,
            "jit and interpreter must agree for [{index}]"
        );
    }
}

#[test]
fn jit_index_assign_out_of_range_matches_interpreter() {
    let src = "fun t() {\n\
        arr := [10, 20, 30]\n\
        arr[100] = 7\n\
        arr }\n\
        t()";
    let baseline = run_result(src, None).expect_err("interpreter should error");
    let jitted = run_result(src, Some(1)).expect_err("jit should error");
    assert!(baseline.contains("out of range"), "{baseline}");
    assert_eq!(baseline, jitted);
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

#[test]
fn method_call_ic_hit_path_getter_loop_is_refcount_safe() {
    let source = r#"
struct C { val <int> }
C includes { fun get() { self.val } }
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
    let source = r#"
struct C { val <int> }
C includes {
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

#[test]
fn jit_line_store_correct_for_modulo_by_zero_in_hot_loop() {
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
    assert_eq!(line, 6, "expected line 6, got {} (msg: {msg})", line);
}

#[test]
fn jit_line_store_correct_for_type_mismatch_in_arith() {
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
    assert_eq!(line, 5, "expected line 5, got {} (msg: {msg})", line);
}

#[test]
fn jit_line_store_correct_for_undefined_global_call() {
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
    assert_eq!(line, 4, "expected line 4, got {} (msg: {msg})", line);
}

#[test]
fn jit_line_store_correct_for_int_divide_by_zero() {
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
    assert_eq!(line, 5, "expected line 5, got {} (msg: {msg})", line);
}

#[test]
fn jit_line_store_correct_for_typed_negate_on_non_numeric() {
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
    assert_eq!(line, 4, "expected line 4, got {} (msg: {msg})", line);
}

#[test]
fn fallback_bitwise_band_matches_interpreter() {
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
    check_match("fun f() { 1 << 64 }\nf()");
}

#[test]
fn fallback_shift_edge_shr_basic() {
    check_match("fun f() { 256 >> 3 }\nf()");
}

#[test]
fn fallback_shift_edge_shr_negative_one_arithmetic() {
    check_match("fun f() { -1 >> 1 }\nf()");
}

#[test]
fn type_contract_form3_walrus_multiop_init() {
    check_match("fun f() {\n    c <int> := 1 + 5\n    c + 1\n}\nf()");
}

#[test]
fn type_contract_form2_strict_multiop_init() {
    check_match("fun f() {\n    c <int> = 1 + 5\n    c + 1\n}\nf()");
}

#[test]
fn type_contract_form4_zero_init_then_assign() {
    check_match("fun f() {\n    c <int>\n    c = 6\n    c + 1\n}\nf()");
}

#[test]
fn type_contract_form3_walrus_int_min() {
    check_match("fun f() {\n    c <int> := 1 << 63\n    c + 1\n}\nf()");
}

#[test]
fn fold_form1_untyped_multiop_init() {
    check_match("fun f() {\n    c := 1 + 5\n    c + 1\n}\nf()");
}

#[test]
fn fold_form1_untyped_shift_int_min() {
    check_match("fun f() {\n    c := 1 << 63\n    c + 1\n}\nf()");
}

#[test]
fn fold_form1_two_locals_sum() {
    let source = r#"
fun f() {
    a := 1 << 4
    b := 1 << 5
    a + b
}
f()
"#;
    check_match(source);
}

#[test]
fn fold_form1_three_locals_sum() {
    let source = r#"
fun f() {
    a := 1 << 4
    b := 1 << 5
    c := 1 << 6
    a + b + c
}
f()
"#;
    check_match(source);
}

#[test]
fn fold_form1_six_locals_sum() {
    let source = r#"
fun shift_table() {
    a := 1 << 0
    b := 1 << 1
    c := 1 << 63
    d := 1 << 64
    e := 256 >> 3
    f := -1 >> 1
    a + b + c + d + e + f
}
shift_table()
"#;
    check_match(source);
}

#[test]
fn fold_form3_typed_three_locals_sum() {
    let source = r#"
fun f() {
    a <int> := 1 << 4
    b <int> := 1 << 5
    c <int> := 1 << 6
    a + b + c
}
f()
"#;
    check_match(source);
}

#[test]
fn fold_form2_immutable_init() {
    check_match("fun f() {\n    c <int> = 1 + 5\n    c + 1\n}\nf()");
}

#[test]
fn fold_arith_chain() {
    check_match("fun f() {\n    c := (1 + 2) * 3 - 4\n    c\n}\nf()");
}

#[test]
fn fold_bitwise_chain() {
    check_match("fun f() {\n    c := (12 & 10) | (4 ^ 1)\n    c\n}\nf()");
}

#[test]
fn fold_division_by_zero_falls_through() {
    let source = "fun f() {\n    c := 1 / 0\n    c\n}\nf()";
    let baseline_err = run_result(source, None).expect_err("interpreter must error");
    let jitted_err = run_result(source, Some(1)).expect_err("JIT must error");
    assert_eq!(baseline_err, jitted_err);
}

#[test]
fn fold_non_literal_rhs_uses_runtime() {
    let source = r#"
fun get_five() { 5 }
fun caller() {
    c := get_five() + 1
    c + 1
}
caller()
"#;
    check_match(source);
}

#[test]
fn fallback_shift_int_min_through_param() {
    let source = r#"
fun f(c) { c + 1 }
f(1 << 63)
"#;
    check_match(source);
}

#[test]
fn fallback_shift_six_via_args_sum() {
    let source = r#"
fun sum6(a, b, c, d, e, f) { a + b + c + d + e + f }
sum6(1 << 0, 1 << 1, 1 << 63, 1 << 64, 256 >> 3, -1 >> 1)
"#;
    check_match(source);
}

#[test]
fn fallback_bitwise_type_error_matches_interpreter() {
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

/// `option { cond -> X, Y }` whose arms are bare locals returns the selected
/// arm.
#[test]
fn regression_option_arms_select_correct_value() {
    let src_min = r#"
fun mn(a, b) { option { a <= b -> a, b } }
mn(3, 7)
"#;
    let (baseline, _, _) = run(src_min, None);
    let (jitted, j_ok, _) = run(src_min, Some(1));
    assert_eq!(baseline, "3", "interpreter min(3,7) must be 3");
    assert_eq!(jitted, baseline, "JIT min(3,7) must match interpreter");
    assert!(j_ok >= 1, "mn should JIT-compile");

    let src_max = r#"
fun mx(a, b) { option { a >= b -> a, b } }
mx(3, 7)
"#;
    let (b2, _, _) = run(src_max, None);
    let (j2, _, _) = run(src_max, Some(1));
    assert_eq!(b2, "7");
    assert_eq!(j2, b2, "JIT max(3,7) must match interpreter");
}

/// A hot `option`-returning function stays correct after it tiers up in default
/// mode.
#[test]
fn regression_option_correct_when_hot_default_mode() {
    let src = r#"
fun mn(a, b) { option { a <= b -> a, b } }
fun run() {
    bad := 0
    i := 0
    repeat unless i >= 200 {
        r := mn(3, 7)
        option { r != 3 -> bad = bad + 1, 0 }
        i = i + 1
    }
    bad
}
run()
"#;
    let (baseline, _, _) = run(src, None);
    assert_eq!(baseline, "0", "hot min(3,7) must return 3 every iteration");
}

/// Calling a builtin (`len`, `str`, `push`, ...) inside a JIT-compiled loop does
/// not crash.
#[test]
fn regression_builtin_call_in_loop_no_crash() {
    let src = r#"
fun go() {
    a <array>
    s <int>
    repeat unless s >= 5 {
        len(a)
        s = s + 1
    }
    s
}
go()
"#;
    let (baseline, _, _) = run(src, None);
    let (jitted, j_ok, _) = run(src, Some(1));
    assert_eq!(baseline, "5");
    assert_eq!(
        jitted, baseline,
        "builtin call in JIT loop must match interpreter"
    );
    assert!(j_ok >= 1, "go should JIT-compile");
}

/// A declared type lock on a local is enforced on reassignment in both the
/// interpreter and the JIT.
#[test]
fn jit_enforces_local_type_lock_on_reassignment() {
    for src in [
        "fun f(){ x <int> := 1\nx = \"hi\"\nx }\nf()",
        "fun f(){ x <int> := 1\nx = 2.5\nx }\nf()",
        "fun f(){ x <float> := 1.0\nx = 2\nx }\nf()",
    ] {
        assert!(
            run_result(src, None).is_err(),
            "interpreter must reject local type-lock violation: {src}"
        );
        assert!(
            run_result(src, Some(1)).is_err(),
            "eager JIT must reject local type-lock violation (no tiering bypass): {src}"
        );
    }

    let ok = "fun run(n <int>){ i <int> := 0\ntotal <int> := 0\nrepeat when i < n { total = total + i\ni = i + 1 }\ntotal }\nrun(1000)";
    assert_eq!(run_result(ok, None).unwrap(), "499500");
    assert_eq!(
        run_result(ok, Some(1)),
        run_result(ok, None),
        "valid typed hot loop under the JIT must match the interpreter"
    );
}

/// A bare identifier in a method body that names a struct field resolves to
/// `self.field` in the interpreter and the JIT alike.
#[test]
fn jit_resolves_implicit_self_field_read() {
    let read = "struct C { r <float> }\nC includes { fun area(){ 3.14 * r * r } }\nC(2.0).area()";
    assert_eq!(run_result(read, None).unwrap(), "12.56");
    assert_eq!(
        run_result(read, Some(1)),
        run_result(read, None),
        "implicit-self field read under the JIT must match the interpreter"
    );

    let shadow = "struct C { r <int> }\nC includes { fun set(r <int>){ r } }\nC(1).set(99)";
    assert_eq!(run_result(shadow, Some(1)).unwrap(), "99");

    let glob = "g := 42\nstruct C { r <int> }\nC includes { fun h(){ g } }\nC(1).h()";
    assert_eq!(run_result(glob, Some(1)).unwrap(), "42");

    let hot = "struct A { v <int> }\nA includes { fun g(){ v } }\nfun run(){ a := A(7)\ns := 0\neach i in range(1000){ s = s + a.g() }\ns }\nrun()";
    assert_eq!(run_result(hot, None).unwrap(), "7000");
    assert_eq!(run_result(hot, Some(1)).unwrap(), "7000");
}

/// `skip` (continue) cleans up the loop body's operand stack and continues
/// correctly.
#[test]
fn skip_in_loop_does_not_leak_stack() {
    let each = "fun f(){ s := 0\neach i in range(6){ x := i * 10\nskip when i % 2 == 0\ns = s + x }\ns }\nf()";
    assert_eq!(run_result(each, None).unwrap(), "90");
    assert_eq!(run_result(each, Some(1)).unwrap(), "90");

    let bare = "fun f(){ each i in range(4){ skip }\n42 }\nf()";
    assert_eq!(run_result(bare, None).unwrap(), "42");
    assert_eq!(run_result(bare, Some(1)).unwrap(), "42");

    let rep = "fun f(){ i := 0\ns := 0\nrepeat when i < 5 { i = i + 1\ny := i * 10\nskip when i == 3\ns = s + y }\ns }\nf()";
    assert_eq!(run_result(rep, None).unwrap(), "120");
    assert_eq!(run_result(rep, Some(1)).unwrap(), "120");
}

/// A closure capturing an `each` loop variable leaves no dangling open upvalue,
/// and each closure captures its own iteration's value.
#[test]
fn loop_var_capture_is_per_iteration() {
    let src = "fun f(){ fns := []\neach i in range(4){ fns := push(fns, fun(){ i * i }) }\ns := 0\neach g in fns { s = s + g() }\ns }\nf()";
    assert_eq!(run_result(src, None).unwrap(), "14");
    assert_eq!(run_result(src, Some(1)).unwrap(), "14");
}

/// `guard` (angle/keyword/tag-filtered), the `option` `<Error>` arm,
/// `<Value>.value`, and `<type<Error||Value>>` normalization all recover
/// runtime errors / `<fail>` in-band, identically under the interpreter and the
/// JIT-mode bailout path.
#[test]
fn error_model_recovery() {
    let cases: &[(&str, &str)] = &[
        (
            "fun rn(){ <fail>(\"no\") }\nrn() <guard>(\"Guest\")",
            "Guest",
        ),
        ("fun bad(){ 1/0 }\nbad() <guard>(\"safe\")", "safe"),
        ("fun ok(){ 42 }\nok() <guard>(\"x\")", "42"),
        ("fun n(){ None }\nn() <guard>(\"fb\")", "None"),
        ("fail \"boom\" guard err -> err.msg", "boom"),
        (
            "fun risky(){ <fail>(\"boom\") }\noption { risky() -> \"ok\", <Error> -> \"fb\" }",
            "fb",
        ),
        ("option { True -> \"ok\", <Error> -> \"fb\" }", "ok"),
        ("option { False -> \"x\" }", "None"),
        ("<Value>(\"ok\").value", "ok"),
        ("(<type<Error || Value>>(\"x\")).value", "x"),
        (
            "r := <type<Error || Value>>(<fail>(\"boom\"))\nr.msg",
            "boom",
        ),
        (
            "fun rn(){ <fail>(<Error<retry>>(\"lost\")) }\nrn() <guard<Error<retry>>>(\"Guest\")",
            "Guest",
        ),
        (
            "fun rn(){ <fail>(<Error<retry>>(\"lost\")) }\nrn() <guard<Error<x>>>(\"inner\") <guard>(\"outer\")",
            "outer",
        ),
        (
            "fun a(){ <fail>(\"e1\") }\nfun b(){ <fail>(\"e2\") }\na() <guard>(b() <guard>(\"both\"))",
            "both",
        ),
    ];
    for (src, expected) in cases {
        assert_eq!(run_result(src, None).unwrap(), *expected, "interp: {src}");
        assert_eq!(run_result(src, Some(1)).unwrap(), *expected, "jit: {src}");
    }
}

/// `stop` (break) cleans the loop variable and body locals off the operand
/// stack before exiting, including when `stop` runs from inside a recovered
/// `option` `<Error>` arm.
#[test]
fn stop_does_not_leak_loop_locals() {
    let nested = "fun f(){ s := 0\neach i in range(3){ each k in range(3){ stop when k == 1\ns = s + 1 } }\ns }\nf()";
    assert_eq!(run_result(nested, None).unwrap(), "3");
    assert_eq!(run_result(nested, Some(1)).unwrap(), "3");
    let in_arm = "fun f(){ each i in range(2){ each k in range(3){ option { k == 1 -> <fail>(\"x\"), <Error> -> { stop }, k } } }\n\"done\" }\nf()";
    assert_eq!(run_result(in_arm, None).unwrap(), "done");
    assert_eq!(run_result(in_arm, Some(1)).unwrap(), "done");
    let cap = "fun f(){ fns := []\neach i in range(5){ fns := push(fns, fun(){ i })\nstop when i == 2 }\ns := 0\neach g in fns { s = s + g() }\ns }\nf()";
    assert_eq!(run_result(cap, None).unwrap(), "3");
    assert_eq!(run_result(cap, Some(1)).unwrap(), "3");
}

/// A `:=` reassignment of a top-level variable from inside a nested block
/// updates the global. A new block-local `:=` stays local.
#[test]
fn global_walrus_reassign_in_nested_block() {
    let loop_src = "x := 0\nrepeat when x < 1 { x := 1 }\nx";
    assert_eq!(run_result(loop_src, None).unwrap(), "1");
    assert_eq!(run_result(loop_src, Some(1)).unwrap(), "1");
    let acc = "total := 0\neach i in range(4){ total := total + i }\ntotal";
    assert_eq!(run_result(acc, None).unwrap(), "6");
    assert_eq!(run_result(acc, Some(1)).unwrap(), "6");
    let local = "fun f(){ s := 0\neach i in range(3){ t := i + 1\ns = s + t }\ns }\nf()";
    assert_eq!(run_result(local, None).unwrap(), "6");
    assert_eq!(run_result(local, Some(1)).unwrap(), "6");
}

/// A JIT-specialized self-recursive function that can return a non-integer
/// (float/string/None) returns it correctly and matches the interpreter. Int
/// self-recursion stays correct.
#[test]
fn jit_specialized_recursion_non_int_return() {
    let cases: &[(&str, &str)] = &[
        (
            "fun f(n) { option { n <= 0 -> 3.5, f(n - 1) } }\nf(3)",
            "3.5",
        ),
        (
            "fun f(n) { option { n <= 0 -> \"done\", f(n - 1) } }\nf(3)",
            "done",
        ),
        (
            "fun f(n) { option { n <= 0 -> None, f(n - 1) } }\nf(3)",
            "None",
        ),
        (
            "fun f(n) { option { n <= 0 -> 2, n == 1 -> 3.5, f(n - 1) } }\nf(3)",
            "3.5",
        ),
        (
            "fun fib(n) { give n when n < 2\nfib(n-1) + fib(n-2) }\nfib(15)",
            "610",
        ),
        (
            "fun g(n <int>) { option { n <= 0 -> 3.5, n == 1 -> 7, g(n - 1) } }\n\
             fun caller(k <int>) { 1 + g(k) }\ncaller(1)",
            "8",
        ),
        (
            "fun g(n <int>) { option { n <= 0 -> 3.5, n == 1 -> 7, g(n - 1) } }\n\
             fun caller(k <int>) { 1 + g(k) }\ncaller(0)",
            "4.5",
        ),
        (
            "fun g(n <int>) { option { n <= 0 -> 3.5, n == 1 -> 7, g(n - 1) } }\n\
             fun caller(k <int>) { 10 - g(k) }\ncaller(1)",
            "3",
        ),
        (
            "fun g(n <int>) { option { n <= 0 -> 3.5, n == 1 -> 7, g(n - 1) } }\n\
             fun caller(k <int>) { 10 - g(k) }\ncaller(0)",
            "6.5",
        ),
        (
            "fun g(n <int>) { option { n <= 0 -> 3.5, n == 1 -> 7, g(n - 1) } }\n\
             fun caller(k <int>) { 3 * g(k) }\ncaller(1)",
            "21",
        ),
    ];
    for (src, expected) in cases {
        assert_eq!(run_result(src, None).unwrap(), *expected, "interp: {src}");
        assert_eq!(
            run_result(src, Some(1)).unwrap(),
            *expected,
            "eager-jit: {src}"
        );
    }
}

/// A `guard` inside a hot loop does not leak handlers or desync the stack
/// across iterations.
#[test]
fn guard_in_loop_no_leak() {
    let src = "fun boom(){ <fail>(\"e\") }\nfun f(){ s := 0\neach i in range(100){ x := boom() <guard>(i)\ns = s + x }\ns }\nf()";
    assert_eq!(run_result(src, None).unwrap(), "4950");
    assert_eq!(run_result(src, Some(1)).unwrap(), "4950");
}

/// `each x in <collection>` JIT-compiles and matches the interpreter across
/// every iterable kind, including `stop` over an array.
#[test]
fn jit_each_over_collections_matches_and_compiles() {
    let cases: &[(&str, &str)] = &[
        (
            "fun f(a){ t := 0\neach x in a { t = t + x }\nt }\nf([10,20,30])",
            "60",
        ),
        (
            "fun f(s){ n := 0\neach c in s { n = n + 1 }\nn }\nf(\"abcde\")",
            "5",
        ),
        (
            "fun f(p){ t := 0\neach x in p { t = t + x }\nt }\nf((1,2,3,4))",
            "10",
        ),
        (
            "fun f(a){ s := 0\neach x in a { stop when x == 3\ns = s + x }\ns }\nf([1,2,3,4])",
            "3",
        ),
        (
            "fun f(a){ s := 0\neach x in a { skip when x == 2\ns = s + x }\ns }\nf([1,2,3,4])",
            "8",
        ),
    ];
    for (src, expected) in cases {
        assert_eq!(run_disabled(src).0, *expected, "interp: {src}");
        let (out, ok, failed) = run_with_thresholds(src, None, Some(1));
        assert_eq!(out, *expected, "jit: {src}");
        assert!(
            ok >= 1,
            "expected each-loop to JIT-compile: {src} (ok={ok})"
        );
        assert_eq!(failed, 0, "compile failure for: {src} (failed={failed})");
    }
}

/// `each i in range(a, b)` with the builtin `range` lowers to a counting loop
/// that matches the interpreter and JIT-compiles. A user-shadowed `range` falls
/// back to ordinary iteration.
#[test]
fn jit_each_in_range_counting_loop_matches_and_compiles() {
    let cases: &[(&str, &str)] = &[
        (
            "fun f(n){ t := 0\neach i in range(n) { t = t + i }\nt }\nf(100)",
            "4950",
        ),
        (
            "fun f(){ t := 0\neach i in range(2, 7) { t = t + i }\nt }\nf()",
            "20",
        ),
    ];
    for (src, expected) in cases {
        assert_eq!(run_disabled(src).0, *expected, "interp: {src}");
        let (out, ok, failed) = run_with_thresholds(src, None, Some(1));
        assert_eq!(out, *expected, "jit: {src}");
        assert!(
            ok >= 1,
            "expected counting-loop to JIT-compile: {src} (ok={ok})"
        );
        assert_eq!(failed, 0, "compile failure for: {src} (failed={failed})");
    }

    let shadow =
        "fun range(n){ give [9, 9] }\nfun f(){ t := 0\neach x in range(3) { t = t + x }\nt }\nf()";
    assert_eq!(run_disabled(shadow).0, "18");
}

/// Compile-time constant folding in every expression position produces results
/// identical to runtime evaluation.
#[test]
fn constant_folding_all_positions() {
    let cases: &[(&str, &str)] = &[
        ("60 * 60 * 24", "86400"),
        ("(1 + 2) * (3 + 4)", "21"),
        ("2 < 3", "True"),
        ("-5 + 10", "5"),
        ("println(7 * 6)\n0", "0"),
    ];
    for (src, expected) in cases {
        assert_eq!(run_result(src, None).unwrap(), *expected, "interp: {src}");
        assert_eq!(run_result(src, Some(1)).unwrap(), *expected, "jit: {src}");
    }
}

/// The inlinable closure shape and every neighbouring shape that must not
/// inline, each asserted against the interpreter.
#[test]
fn closure_inline_shapes_match_interpreter() {
    let cases: &[(&str, &str)] = &[
        (
            "fun mk(x) { fun(y) { x + y } }\nadd5 := mk(5)\n\
             fun run(n <int>) { t <int> := 0\ni <int> := 0\n\
             repeat when i < n { t = t + add5(i)\ni = i + 1 }\nt }\nrun(200)",
            "20900",
        ),
        (
            "fun mk() { c := 0\nfun(y) { c = c + y\nc } }\nacc := mk()\n\
             fun run(n <int>) { t <int> := 0\ni <int> := 0\n\
             repeat when i < n { t = t + acc(i)\ni = i + 1 }\nt }\nrun(50)",
            "20825",
        ),
        (
            "fun mk(x) { fun(y) { x + y } }\nf := mk(1.5)\n\
             fun run(n <int>) { t := 0.0\ni <int> := 0\n\
             repeat when i < n { t = t + f(i)\ni = i + 1 }\nt }\nrun(100)",
            "5100",
        ),
        (
            "fun mk(x) { fun(y) { x + y } }\nf := mk(2)\n\
             fun run(n <int>) { t := 0.0\ni <int> := 0\n\
             repeat when i < n { t = t + f(i + 0.5)\ni = i + 1 }\nt }\nrun(100)",
            "5200",
        ),
        (
            "fun mk(x) { fun(y) { x + y } }\nf := mk(\"a\")\n\
             fun run(n <int>) { s := \"\"\ni <int> := 0\n\
             repeat when i < 3 { s = s + f(\"b\")\ni = i + 1 }\ns }\nrun(3)",
            "ababab",
        ),
        (
            "fun mk(x) { fun(a, b) { x + a + b } }\nf := mk(1)\n\
             fun run(n <int>) { t <int> := 0\ni <int> := 0\n\
             repeat when i < n { t = t + f(i, 2)\ni = i + 1 }\nt }\nrun(100)",
            "5250",
        ),
    ];
    for (src, expected) in cases {
        assert_eq!(run_result(src, None).unwrap(), *expected, "interp: {src}");
        assert_eq!(
            run_result(src, Some(1)).unwrap(),
            *expected,
            "eager-jit: {src}"
        );
    }
}

/// Two different closures reaching one call site do not reuse each other's
/// inline metadata.
#[test]
fn closure_inline_does_not_reuse_stale_metadata_across_callees() {
    let src = r#"
fun mk(x) { fun(y) { x + y } }
fun apply(f, k <int>) { f(k) }
fun run(n <int>) {
  a := mk(5)
  b := mk(100)
  t <int> := 0
  i <int> := 0
  repeat when i < n {
    t = t + apply(a, i) + apply(b, i)
    i = i + 1
  }
  t
}
run(200)
"#;
    assert_eq!(run_result(src, None).unwrap(), "60800");
    assert_eq!(run_result(src, Some(1)).unwrap(), "60800");
}

/// A closure whose call site later sees a non-inlinable callee keeps producing
/// correct results.
#[test]
fn closure_inline_site_survives_callee_swap() {
    let src = r#"
fun mk(x) { fun(y) { x + y } }
fun mul3(y <int>) { y * 3 }
fun apply(f, k <int>) { f(k) }
fun run(n <int>) {
  a := mk(5)
  t <int> := 0
  i <int> := 0
  repeat when i < n {
    t = t + apply(a, i)
    i = i + 1
  }
  i = 0
  repeat when i < n {
    t = t + apply(mul3, i)
    i = i + 1
  }
  t
}
run(100)
"#;
    assert_eq!(run_result(src, None).unwrap(), "20300");
    assert_eq!(run_result(src, Some(1)).unwrap(), "20300");
}
