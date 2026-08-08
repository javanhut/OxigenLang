//! Phase 2.3: can a non-local exit cross real Cranelift-generated frames?
//!
//! Design 2's lean ABI (`fn(args.., budget) -> i64`) has no status channel, so
//! recursion exhaustion can only be reported by skipping the intervening frames
//! outright. The C A/B showed that costs ~1.8%, but it longjmp'd out of *C*
//! frames. Cranelift frames differ in ways that could matter: on AArch64 they
//! sign the return address (`Aarch64SetPointerAuth`) and save callee-saved
//! registers with their own unwind directives.
//!
//! This builds a genuinely recursive Cranelift function with the same ISA
//! settings the engine uses (`opt_level=speed`, host ISA via cranelift-native),
//! recurses to a real depth, and longjmps out of the middle of it.
//!
//! `longjmp` needs no unwind tables — it restores sp/pc/callee-saved registers
//! from the `jmp_buf` captured at `setjmp`, so the skipped frames are simply
//! never returned through. What this test proves is that nothing in Cranelift's
//! prologue (pointer authentication in particular) makes that restore invalid.

#![cfg(feature = "jit")]

use std::cell::{Cell, UnsafeCell};

use cranelift_codegen::Context;
use cranelift_codegen::ir::{AbiParam, InstBuilder, types};
use cranelift_codegen::settings;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module, default_libcall_names};

// The libc crate does not export setjmp/longjmp on this target, so declare
// them directly. The buffer is deliberately over-sized and 16-byte aligned:
// Darwin/arm64 `jmp_buf` is 48 ints (192 bytes), and over-allocating costs
// nothing while removing any doubt about layout.
#[repr(C, align(16))]
struct JmpBuf([u64; 64]);

unsafe extern "C" {
    fn setjmp(env: *mut JmpBuf) -> i32;
    fn longjmp(env: *mut JmpBuf, val: i32) -> !;
}

// The unwind target MUST be per-thread. A global one raced across cargo's
// parallel test threads and SIGTRAP'd: one thread's setjmp overwrote another's
// buffer, so the longjmp restored a stack pointer belonging to a different
// stack. Oxigen has spawn/pmap/a threaded server, so a global here would
// reproduce that in production under concurrency, not just in tests.
thread_local! {
    static JUMP_TARGET: UnsafeCell<JmpBuf> = const { UnsafeCell::new(JmpBuf([0; 64])) };
    static DEPTH_AT_EXIT: Cell<i64> = const { Cell::new(-1) };
}

/// Raw pointer to this thread's buffer.
///
/// Fetched separately from the `setjmp` call on purpose: `setjmp` must run in
/// the frame control returns to, so calling it inside a `with()` closure would
/// capture the closure's frame and land in one that no longer exists.
fn jump_target() -> *mut JmpBuf {
    JUMP_TARGET.with(|c| c.get())
}

/// Called from JIT'd code once the budget runs out. Never returns.
extern "C" fn cold_exit(depth: i64) -> i64 {
    DEPTH_AT_EXIT.with(|d| d.set(depth));
    unsafe { longjmp(jump_target(), 1) }
}

/// Builds `fn recurse(n: i64) -> i64`:
///
/// ```text
/// if n <= 0 { return cold_exit(n) }   // never returns
/// return recurse(n - 1) + 1
/// ```
///
/// Genuinely recursive, so the longjmp has many live Cranelift frames to cross,
/// and each carries a real prologue/epilogue rather than being a leaf.
fn build_recursive_jit() -> (JITModule, *const u8) {
    use cranelift_codegen::settings::Configurable;
    let mut flag_builder = settings::builder();
    flag_builder
        .set("opt_level", "speed")
        .expect("Cranelift accepts opt_level=speed");
    let flags = settings::Flags::new(flag_builder);
    let isa = cranelift_native::builder()
        .expect("host ISA")
        .finish(flags)
        .expect("finalize ISA");

    let mut jit_builder = JITBuilder::with_isa(isa, default_libcall_names());
    jit_builder.symbol("cold_exit", cold_exit as *const u8);
    let mut module = JITModule::new(jit_builder);

    // extern: fn(i64) -> i64
    let mut cold_sig = module.make_signature();
    cold_sig.params.push(AbiParam::new(types::I64));
    cold_sig.returns.push(AbiParam::new(types::I64));
    let cold_id = module
        .declare_function("cold_exit", Linkage::Import, &cold_sig)
        .expect("declare cold_exit");

    let mut sig = module.make_signature();
    sig.params.push(AbiParam::new(types::I64));
    sig.returns.push(AbiParam::new(types::I64));
    let func_id = module
        .declare_function("recurse", Linkage::Export, &sig)
        .expect("declare recurse");

    let mut ctx = Context::new();
    ctx.func.signature = sig;
    let mut fbc = FunctionBuilderContext::new();
    {
        let mut b = FunctionBuilder::new(&mut ctx.func, &mut fbc);
        let entry = b.create_block();
        b.append_block_params_for_function_params(entry);
        b.switch_to_block(entry);
        let n = b.block_params(entry)[0];

        let cold_block = b.create_block();
        let recurse_block = b.create_block();
        let zero = b.ins().iconst(types::I64, 0);
        let done = b
            .ins()
            .icmp(cranelift_codegen::ir::condcodes::IntCC::SignedLessThanOrEqual, n, zero);
        b.ins().brif(done, cold_block, &[], recurse_block, &[]);

        // Budget exhausted: hand off to the cold adapter, which does not return.
        b.switch_to_block(cold_block);
        let cold_ref = module.declare_func_in_func(cold_id, b.func);
        let call = b.ins().call(cold_ref, &[n]);
        let r = b.inst_results(call)[0];
        b.ins().return_(&[r]);

        // Recurse: keeps a live frame with a real prologue at every level.
        b.switch_to_block(recurse_block);
        let self_ref = module.declare_func_in_func(func_id, b.func);
        let n1 = b.ins().iadd_imm_s(n, -1);
        let rcall = b.ins().call(self_ref, &[n1]);
        let rv = b.inst_results(rcall)[0];
        let out = b.ins().iadd_imm_s(rv, 1);
        b.ins().return_(&[out]);

        b.seal_all_blocks();
        b.finalize(module.target_config());
    }
    module
        .define_function(func_id, &mut ctx)
        .expect("define recurse");
    module.clear_context(&mut ctx);
    module.finalize_definitions().expect("finalize");
    let code = module.get_finalized_function(func_id);
    (module, code)
}

#[test]
fn longjmp_crosses_cranelift_frames() {
    let (_module, code) = build_recursive_jit();
    let recurse: extern "C" fn(i64) -> i64 = unsafe { std::mem::transmute(code) };

    // Deep enough that the longjmp must cross many live JIT frames, not one.
    const DEPTH: i64 = 512;

    let landed: i32 = unsafe {
        let rc = setjmp(jump_target());
        if rc == 0 {
            // Should never return — cold_exit longjmps out of the recursion.
            let v = recurse(DEPTH);
            panic!("recursion returned {v} instead of taking the non-local exit");
        }
        rc
    };

    assert_eq!(landed, 1, "must land via longjmp, not a normal return");
    assert_eq!(
        DEPTH_AT_EXIT.with(|d| d.get()),
        0,
        "cold adapter should have been reached at the bottom of the recursion"
    );
}

#[test]
fn jit_still_usable_after_a_non_local_exit() {
    // The mechanism is only useful if the JIT is reusable afterwards: a lean
    // entry that overflowed once must not poison later calls.
    let (_module, code) = build_recursive_jit();
    let recurse: extern "C" fn(i64) -> i64 = unsafe { std::mem::transmute(code) };

    for round in 0..4 {
        let landed: i32 = unsafe {
            let rc = setjmp(jump_target());
            if rc == 0 {
                recurse(64);
                panic!("round {round}: expected a non-local exit");
            }
            rc
        };
        assert_eq!(landed, 1, "round {round} must land via longjmp");
        assert_eq!(DEPTH_AT_EXIT.with(|d| d.get()), 0, "round {round} depth");
    }
}


/// Control: the same setjmp/longjmp round trip with NO JIT code involved, so a
/// failure here indicts the Rust-side FFI rather than Cranelift's frames.
///
/// `setjmp` returns twice, which Rust's FFI declaration cannot express — the
/// optimizer is free to keep values in callee-saved registers across the call
/// that `longjmp` then restores from under it. That hazard is independent of
/// what the intervening frames were compiled by.
#[test]
fn control_longjmp_across_rust_frames_only() {
    #[inline(never)]
    extern "C" fn rust_recurse(n: i64) -> i64 {
        if n <= 0 {
            DEPTH_AT_EXIT.with(|d| d.set(n));
            unsafe { longjmp(jump_target(), 1) }
        }
        rust_recurse(n - 1) + 1
    }

    let landed: i32 = unsafe {
        let rc = setjmp(jump_target());
        if rc == 0 {
            rust_recurse(512);
            panic!("expected a non-local exit");
        }
        rc
    };
    assert_eq!(landed, 1);
}


/// The constraint the global buffer violated: several threads unwinding through
/// their own JIT frames at once must not interfere. This is the shape Oxigen's
/// `spawn`/`pmap` would produce, and it is what a lean entry has to survive.
#[test]
fn concurrent_non_local_exits_are_independent() {
    let handles: Vec<_> = (0..4)
        .map(|t| {
            std::thread::spawn(move || {
                // Each thread builds and drives its own JIT module.
                let (_module, code) = build_recursive_jit();
                let recurse: extern "C" fn(i64) -> i64 = unsafe { std::mem::transmute(code) };
                for _ in 0..8 {
                    let landed: i32 = unsafe {
                        let rc = setjmp(jump_target());
                        if rc == 0 {
                            recurse(128 + t);
                            panic!("thread {t}: expected a non-local exit");
                        }
                        rc
                    };
                    assert_eq!(landed, 1, "thread {t} must land via longjmp");
                    assert_eq!(DEPTH_AT_EXIT.with(|d| d.get()), 0, "thread {t} depth");
                }
            })
        })
        .collect();
    for h in handles {
        h.join().expect("no thread may trap");
    }
}
