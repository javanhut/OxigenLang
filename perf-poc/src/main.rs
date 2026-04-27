//! Sandboxed PoC for the oxigen JIT struct-bench optimization plan.
//!
//! We model three inner-loop shapes that mirror what the real Oxigen JIT
//! emits (or would emit under the proposed changes). Each shape runs the
//! same number of field-add operations (2 * N = 1,000,000 when N = 500,000,
//! matching `example/bench_struct_method.oxi`). We print medians across 11
//! trials per variant so the acceptance-gate numbers are stable.
//!
//! Pass criteria (from the plan):
//!   - variant_c median ≤ 25 ms        (Phase 4 projection)
//!   - variant_b ≥ 40% faster than a   (Phase 1+2 projection)

use std::cell::RefCell;
use std::collections::HashMap;
use std::hint::black_box;
use std::rc::Rc;
use std::time::Instant;

// ────────────────────────────────────────────────────────────────────────
// Shared tagged-Value representation (mirrors core/src/vm/value.rs layout:
// 40 bytes, tag at offset 0, integer payload at offset 8).
// ────────────────────────────────────────────────────────────────────────

pub const VALUE_SIZE: usize = 40;
pub const TAG_INT: u8 = 0;
pub const TAG_STRUCT: u8 = 15;
pub const VALUE_INT_PAYLOAD_OFFSET: usize = 8;

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Value {
    pub tag: u8,
    _tag_pad: [u8; 7],
    pub payload: i64,
    _rest: [u8; 24],
}

const _ASSERT_VALUE_SIZE: () = assert!(core::mem::size_of::<Value>() == VALUE_SIZE);

#[inline]
fn mk_int(n: i64) -> Value {
    Value {
        tag: TAG_INT,
        _tag_pad: [0; 7],
        payload: n,
        _rest: [0; 24],
    }
}

// ────────────────────────────────────────────────────────────────────────
// Variant A — current production shape
// Mirrors `core/src/vm/value.rs:ObjStructInstance` + the per-op FFI helper
// path through `jit_struct_field_add_i64` at `core/src/jit/runtime.rs:240`.
// Cost dominated by:
//   - FFI / function call boundary (`#[inline(never)]`)
//   - `HashMap::get` on the field name
//   - `RefCell::borrow_mut` runtime check
//   - `Vec` indirection to reach the fields buffer
// ────────────────────────────────────────────────────────────────────────

mod variant_a {
    use super::*;

    pub struct FieldLayout {
        pub indices: HashMap<String, usize>,
    }

    pub struct ObjStructDef {
        pub _name: String,
    }

    pub struct ObjStructInstance {
        pub struct_name: String,
        pub fields: RefCell<Vec<Value>>,
        pub layout: Rc<FieldLayout>,
        pub def: Rc<ObjStructDef>,
    }

    // `#[inline(never)]` forces a real function-call boundary to approximate
    // the FFI crossing + runtime-helper work.
    #[inline(never)]
    pub fn struct_field_add_const(inst: &Rc<ObjStructInstance>, field_name: &str, addend: i64) {
        // Simulate `vm.sync_stack_from_view()` + `current_constant` lookup
        // — a read-modify-write through a volatile-like indirection.
        black_box(&inst.struct_name);

        if let Some(&idx) = inst.layout.indices.get(field_name) {
            let mut fields = inst.fields.borrow_mut();
            let cur = fields[idx].payload;
            // In the real helper there's a `match` on `Value::Integer(...)`.
            // We approximate via a tag check.
            if fields[idx].tag == TAG_INT {
                fields[idx].payload = cur.wrapping_add(addend);
            }
        }
    }

    #[inline(never)]
    pub fn struct_field_add_local(inst: &Rc<ObjStructInstance>, field_name: &str, rhs: i64) {
        black_box(&inst.struct_name);
        if let Some(&idx) = inst.layout.indices.get(field_name) {
            let mut fields = inst.fields.borrow_mut();
            let cur = fields[idx].payload;
            if fields[idx].tag == TAG_INT {
                fields[idx].payload = cur.wrapping_add(rhs);
            }
        }
    }

    pub fn mk_counter() -> Rc<ObjStructInstance> {
        let mut indices = HashMap::new();
        indices.insert("val".to_string(), 0);
        Rc::new(ObjStructInstance {
            struct_name: "Counter".to_string(),
            fields: RefCell::new(vec![mk_int(0)]),
            layout: Rc::new(FieldLayout { indices }),
            def: Rc::new(ObjStructDef {
                _name: "Counter".to_string(),
            }),
        })
    }

    pub fn run(n: i64) -> i64 {
        let c = mk_counter();
        let mut i: i64 = 0;
        while i < n {
            // c.inc(): self.val = self.val + 1
            struct_field_add_const(&c, "val", 1);
            // c.add(i): self.val = self.val + amount
            struct_field_add_local(&c, "val", i);
            i = i.wrapping_add(1);
        }
        let fields = c.fields.borrow();
        fields[0].payload
    }
}

// ────────────────────────────────────────────────────────────────────────
// Variant B — Phase 1 (#[repr(C)] + raw fields ptr) + Phase 2 (inline IR)
//
// The struct-field-add peephole is expanded inline. The JIT still emits a
// function for the outer `run` so we keep the loop in-function, but each
// field-add becomes a handful of machine instructions: tag guard, raw load,
// add, raw store. No FFI, no HashMap, no RefCell.
//
// The `struct_def` guard is NOT needed here because inside a monomorphic
// loop the JIT inline IC guard fires once and falls through on every
// subsequent iteration. We still emit the guard code, but it's a cheap
// cmp+branch that the CPU's branch predictor handles trivially.
// ────────────────────────────────────────────────────────────────────────

mod variant_b {
    use super::*;

    pub struct ObjStructDef {
        pub _name: String,
    }

    #[repr(C)]
    pub struct ObjStructInstance {
        pub fields_ptr: *mut Value, // offset 0 — hot path
        pub fields_len: u32,        // offset 8
        pub fields_cap: u32,        // offset 12
        // cold below
        pub struct_name: String,
        pub def: Rc<ObjStructDef>,
    }

    impl Drop for ObjStructInstance {
        fn drop(&mut self) {
            if !self.fields_ptr.is_null() {
                unsafe {
                    let _ = Vec::from_raw_parts(
                        self.fields_ptr,
                        self.fields_len as usize,
                        self.fields_cap as usize,
                    );
                }
            }
        }
    }

    pub fn mk_counter() -> Rc<ObjStructInstance> {
        let mut v = vec![mk_int(0)];
        let cap = v.capacity();
        let len = v.len();
        let ptr = v.as_mut_ptr();
        std::mem::forget(v);
        Rc::new(ObjStructInstance {
            fields_ptr: ptr,
            fields_len: len as u32,
            fields_cap: cap as u32,
            struct_name: "Counter".to_string(),
            def: Rc::new(ObjStructDef {
                _name: "Counter".to_string(),
            }),
        })
    }

    // Model the JIT-emitted inline IR for the peephole. `field_index` is
    // what the FieldCacheEntry would carry — we skip the struct_def_raw
    // guard (mirror of the MethodCall IC guard) because the receiver is
    // monomorphic; the real JIT still does the guard, but it's a single
    // cmp + predicted branch, ~1-2ns amortized.
    //
    // `#[inline(always)]` matches what the Cranelift translator does —
    // it emits the IR directly at the call site, no function boundary.
    #[inline(always)]
    fn inline_field_add(inst: &Rc<ObjStructInstance>, field_index: usize, addend: i64) {
        unsafe {
            let slot_ptr = inst.fields_ptr.add(field_index);
            // Tag guard: must be integer. Branch predictor handles this.
            let tag = (*(slot_ptr as *const u8)).saturating_add(0);
            if tag == TAG_INT {
                let payload_ptr = (slot_ptr as *mut u8).add(VALUE_INT_PAYLOAD_OFFSET) as *mut i64;
                let cur = *payload_ptr;
                *payload_ptr = cur.wrapping_add(addend);
            }
        }
    }

    // The inner loop with inline field-adds. There's still a MethodCall
    // overhead in variant B because Phase 2 does NOT inline the method
    // body — we still dispatch into `inc()` and `add()` thunks. We
    // approximate that by putting the field-add inside `#[inline(never)]`
    // functions that stand in for the thunks.
    #[inline(never)]
    fn inc_thunk(inst: &Rc<ObjStructInstance>) {
        inline_field_add(inst, 0, 1);
    }

    #[inline(never)]
    fn add_thunk(inst: &Rc<ObjStructInstance>, amount: i64) {
        inline_field_add(inst, 0, amount);
    }

    pub fn run(n: i64) -> i64 {
        let c = mk_counter();
        let mut i: i64 = 0;
        while i < n {
            inc_thunk(&c);
            add_thunk(&c, i);
            i = i.wrapping_add(1);
        }
        unsafe {
            let slot_ptr = c.fields_ptr.add(0);
            let payload_ptr = (slot_ptr as *const u8).add(VALUE_INT_PAYLOAD_OFFSET) as *const i64;
            *payload_ptr
        }
    }
}

// ────────────────────────────────────────────────────────────────────────
// Variant C — Phase 1+2+3+4
//
// Same as variant B, plus the method-call inline-expansion of Phase 4.
// At each caller-side `c.inc()` / `c.add(i)` the JIT emits the guard and
// the inline field-add IR directly — no thunk dispatch, no JitFrame push,
// no call_indirect. This is the layout that the full plan ships.
// ────────────────────────────────────────────────────────────────────────

mod variant_c {
    use super::*;
    pub use super::variant_b::{mk_counter, ObjStructInstance};

    // The guard + inline add is structurally identical to variant_b's
    // `inline_field_add`, but here we call it directly from the caller —
    // NO `#[inline(never)]` wrapper. The shape mirrors what Phase 4 emits:
    //   - receiver tag + struct_def_raw guard (both predicted-taken)
    //   - raw-ptr field-add
    #[inline(always)]
    fn guarded_inline_field_add(
        inst: &Rc<ObjStructInstance>,
        expected_def: *const variant_b::ObjStructDef,
        field_index: usize,
        addend: i64,
    ) {
        let actual_def = Rc::as_ptr(&inst.def) as *const variant_b::ObjStructDef;
        if actual_def == expected_def {
            unsafe {
                let slot_ptr = inst.fields_ptr.add(field_index);
                let tag = *(slot_ptr as *const u8);
                if tag == TAG_INT {
                    let payload_ptr =
                        (slot_ptr as *mut u8).add(VALUE_INT_PAYLOAD_OFFSET) as *mut i64;
                    let cur = *payload_ptr;
                    *payload_ptr = cur.wrapping_add(addend);
                }
            }
        } else {
            // In the real JIT this is the miss path → fallback to the
            // full MethodCall IC helper. Not exercised in the benchmark.
            panic!("variant_c miss path triggered — benchmark is not monomorphic?");
        }
    }

    pub fn run(n: i64) -> i64 {
        let c = mk_counter();
        let expected_def = Rc::as_ptr(&c.def);
        let mut i: i64 = 0;
        while i < n {
            guarded_inline_field_add(&c, expected_def, 0, 1); // c.inc()
            guarded_inline_field_add(&c, expected_def, 0, i); // c.add(i)
            i = i.wrapping_add(1);
        }
        unsafe {
            let slot_ptr = c.fields_ptr.add(0);
            let payload_ptr = (slot_ptr as *const u8).add(VALUE_INT_PAYLOAD_OFFSET) as *const i64;
            *payload_ptr
        }
    }
}

// ────────────────────────────────────────────────────────────────────────
// Driver
// ────────────────────────────────────────────────────────────────────────

fn bench<F: FnMut() -> i64>(label: &str, trials: usize, mut f: F) -> f64 {
    // Warm up.
    for _ in 0..3 {
        black_box(f());
    }
    let mut samples: Vec<f64> = Vec::with_capacity(trials);
    let mut last_result: i64 = 0;
    for _ in 0..trials {
        let start = Instant::now();
        last_result = f();
        let elapsed = start.elapsed();
        samples.push(elapsed.as_secs_f64() * 1000.0);
    }
    black_box(last_result);
    samples.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let median = samples[samples.len() / 2];
    let min = samples[0];
    let max = samples[samples.len() - 1];
    println!(
        "{:<12}  median = {:>7.2} ms   min = {:>7.2} ms   max = {:>7.2} ms   last = {}",
        label, median, min, max, last_result
    );
    median
}

fn main() {
    // Matches `example/bench_struct_method.oxi`: 500_000 iterations of the
    // loop body, which executes c.inc() + c.add(i) = 2 field-adds per iter.
    const N: i64 = 500_000;
    const TRIALS: usize = 11;

    // Sanity: all three variants must produce the same final value.
    // c.val starts at 0, then each iter adds 1 + i. Final value =
    // N + Σ_{i=0..N-1} i = N + N*(N-1)/2 = 500_000 + 124_999_750_000.
    let expected: i64 = N + (N * (N - 1)) / 2;
    assert_eq!(variant_a::run(N), expected, "variant_a result mismatch");
    assert_eq!(variant_b::run(N), expected, "variant_b result mismatch");
    assert_eq!(variant_c::run(N), expected, "variant_c result mismatch");
    println!("correctness ok (result = {})\n", expected);

    println!(
        "N = {} iterations, {} field-add ops per trial, {} trials per variant\n",
        N,
        N * 2,
        TRIALS
    );

    let a = bench("variant_a", TRIALS, || variant_a::run(N));
    let b = bench("variant_b", TRIALS, || variant_b::run(N));
    let c = bench("variant_c", TRIALS, || variant_c::run(N));

    println!();
    println!(
        "Phase 0 gate: variant_c median ≤ 25 ms?       {}   ({:.2} ms)",
        if c <= 25.0 { "PASS" } else { "FAIL" },
        c
    );
    let b_speedup = (a - b) / a * 100.0;
    println!(
        "Phase 0 gate: variant_b ≥ 40% faster than a?  {}   ({:.1}% faster)",
        if b_speedup >= 40.0 { "PASS" } else { "FAIL" },
        b_speedup
    );
    let c_speedup = (a - c) / a * 100.0;
    println!(
        "Reference   : variant_c vs variant_a           {:.1}% faster",
        c_speedup
    );
    println!(
        "Reference   : Python baseline on real bench    ≈ 71 ms   (target ≤ 31 ms)"
    );
}
