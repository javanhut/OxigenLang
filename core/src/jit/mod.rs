//! Baseline JIT compiler for the Oxigen VM.
//!
//! The public facade ([`JitEngine`]) is always available so the VM can hold
//! a `JitEngine` field unconditionally. When the `jit` Cargo feature is off,
//! the facade is a zero-cost stub: `bump_and_maybe_compile` always returns
//! `false` and `try_invoke` always bails out. When the feature is on, the
//! stub defers to the Cranelift-backed [`engine::JitInner`].
//!
//! Milestone 1: fallback harness only. Compilation succeeds for functions
//! whose opcodes are all in the allow-list (see [`scan::scan`]), but the
//! emitted code just delegates back to the interpreter via
//! [`runtime::jit_run_via_interpreter`].

use std::rc::Rc;

use crate::vm::VM;
use crate::vm::VMError;
use crate::vm::value::{Function, ObjClosure};

pub mod scan;

#[cfg(feature = "jit")]
mod engine;
#[cfg(feature = "jit")]
pub(crate) mod runtime;
#[cfg(feature = "jit")]
mod virt_stack;

/// Default call-count threshold at which a function is considered hot
/// enough to compile.
pub const JIT_HOT_THRESHOLD: u32 = 50;
/// Default loop-backedge threshold at which a long-running function is
/// considered hot enough to compile even if it is only called once.
pub const JIT_LOOP_HOT_THRESHOLD: u32 = 10_000;

/// Native entry point emitted by the JIT for one Oxigen function.
pub(crate) type CompiledThunk = unsafe extern "C" fn(*mut VM) -> u32;

/// Opaque raw pointer to the specialized `fn(*mut VM, i64, ..., i64)
/// -> (i64, u32)` entry point. Its actual signature depends on the
/// arity, so we never rebuild it into a typed Rust function — it is
/// only invoked through Cranelift `call_indirect` with the right
/// signature built at call-site emission time.
pub(crate) type SpecializedThunkRaw = *const ();

/// Pair of entries produced by a single `compile_function` call.
/// `generic` is the always-present `fn(*mut VM) -> u32` thunk.
/// `specialized` is Some only when the function qualifies per
/// `slot_types.specialized_entry_eligible` (A1 analysis).
/// `specialized_kind` discriminates WHICH specialized body the pointer
/// points at (forward trampoline vs. native int body) so callers can
/// gate direct-call optimizations on a real body being present.
#[cfg(feature = "jit")]
pub(crate) struct CompiledEntries {
    pub generic: CompiledThunk,
    pub specialized: Option<SpecializedThunkRaw>,
    pub specialized_arity: u8,
    pub specialized_kind: Option<engine::SpecializedEntryKind>,
}

/// Outcome of invoking a JIT-compiled function.
pub enum JitExit {
    /// The compiled function completed normally; its return value is on the
    /// VM stack where the caller expects it.
    Returned,
    /// The JIT refused — either no compiled entry exists for this function
    /// or a deopt fired mid-execution. The caller must fall back to the
    /// interpreter.
    Bailout,
    /// The compiled function surfaced a runtime error; retrieve it via the
    /// `err` field.
    RuntimeError(VMError),
}

/// The JIT engine.
pub struct JitEngine {
    threshold: u32,
    loop_threshold: u32,
    array_index_fast_hits: usize,
    array_index_fast_misses: usize,
    #[cfg(feature = "jit")]
    inner: Option<engine::JitInner>,
}

impl Default for JitEngine {
    fn default() -> Self {
        Self::new()
    }
}

impl JitEngine {
    pub fn new() -> Self {
        Self {
            threshold: JIT_HOT_THRESHOLD,
            loop_threshold: JIT_LOOP_HOT_THRESHOLD,
            array_index_fast_hits: 0,
            array_index_fast_misses: 0,
            #[cfg(feature = "jit")]
            inner: None,
        }
    }

    /// Raw pointer to the JIT counters block, if the inner engine has
    /// been instantiated. Callers use this from runtime helpers that
    /// only have `&mut self` access to JitEngine via the VM and need
    /// to bump diagnostic counters without going through borrow-check
    /// acrobatics. Returns None before first JIT compilation.
    #[cfg(feature = "jit")]
    pub(crate) fn counters_ptr_opt(&self) -> Option<*const engine::JitCounters> {
        self.inner.as_ref().map(|i| i.counters_ptr())
    }

    /// Step 0 attribution: bump a per-helper FFI call counter. Called
    /// from the entry of every `extern "C"` runtime helper. Cheap when
    /// counters are present (load + cmp + load + inc + store), no-op
    /// otherwise. The Option-on-self pattern keeps non-JIT runs paying
    /// only a nullable check.
    #[cfg(feature = "jit")]
    #[inline]
    pub(crate) fn bump_helper(&self, h: engine::HelperCounter) {
        if let Some(inner) = self.inner.as_ref() {
            inner.counters.bump_helper(h);
        }
    }

    /// Lower the hot-threshold so `--jit` runs and tests don't need a long
    /// warmup. Clamped to at least 1.
    pub fn set_threshold(&mut self, t: u32) {
        self.threshold = t.max(1);
    }

    /// Lower the loop hot-threshold. Clamped to at least 1.
    pub fn set_loop_threshold(&mut self, t: u32) {
        self.loop_threshold = t.max(1);
    }

    /// Completely disable JIT compilation — no function will ever cross
    /// the threshold. Used by `--no-jit` benchmarking to pin the baseline
    /// to pure interpreter execution even on a JIT-enabled build.
    pub fn disable(&mut self) {
        self.threshold = u32::MAX;
        self.loop_threshold = u32::MAX;
    }

    pub fn threshold(&self) -> u32 {
        self.threshold
    }

    pub fn loop_threshold(&self) -> u32 {
        self.loop_threshold
    }

    /// Called from `call_closure` after the closure's per-call counter has
    /// been bumped. Compiles the function if it has reached the threshold
    /// and hasn't been compiled (or marked failed) yet. Returns `true`
    /// when a compiled entry is now available.
    pub fn maybe_compile(&mut self, func: &Rc<Function>, call_count: u32) -> bool {
        if call_count < self.threshold {
            return false;
        }

        #[cfg(not(feature = "jit"))]
        {
            let _ = func;
            return false;
        }

        #[cfg(feature = "jit")]
        {
            // Lazy-init the Cranelift module on first hot function.
            let inner = self.inner.get_or_insert_with(engine::JitInner::new);
            inner.maybe_compile(func)
        }
    }

    /// Compile if hot and return the compiled thunk, if one is available.
    /// Used by VM call sites that want to cache the thunk on the closure
    /// and avoid the compiled-entry lookup on subsequent calls.
    pub(crate) fn maybe_compile_thunk(
        &mut self,
        func: &Rc<Function>,
        call_count: u32,
    ) -> Option<CompiledThunk> {
        if call_count < self.threshold {
            return None;
        }

        #[cfg(not(feature = "jit"))]
        {
            let _ = func;
            None
        }

        #[cfg(feature = "jit")]
        {
            let inner = self.inner.get_or_insert_with(engine::JitInner::new);
            inner.maybe_compile_thunk(func)
        }
    }

    /// Compile if hot and return the full CompiledEntries fields,
    /// exposing generic + specialized + kind discriminator. VM call
    /// sites use this to install all three on the closure.
    ///
    /// The fourth tuple element is the u8 encoding of
    /// `SpecializedEntryKind` (see `vm::value::SPECIALIZED_KIND_*`).
    /// Returns 0 (`SPECIALIZED_KIND_NONE`) when there's no specialized
    /// entry.
    #[cfg(feature = "jit")]
    pub(crate) fn maybe_compile_entries_for(
        &mut self,
        func: &Rc<Function>,
        call_count: u32,
    ) -> Option<(CompiledThunk, Option<SpecializedThunkRaw>, u8, u8)> {
        if call_count < self.threshold {
            return None;
        }
        let inner = self.inner.get_or_insert_with(engine::JitInner::new);
        let e = inner.maybe_compile_entries(func)?;
        let kind_u8 = match e.specialized_kind {
            None => crate::vm::value::SPECIALIZED_KIND_NONE,
            Some(engine::SpecializedEntryKind::ForwardTrampoline) => {
                crate::vm::value::SPECIALIZED_KIND_FORWARD_TRAMPOLINE
            }
            Some(engine::SpecializedEntryKind::NativeIntBody) => {
                crate::vm::value::SPECIALIZED_KIND_NATIVE_INT_BODY
            }
        };
        Some((e.generic, e.specialized, e.specialized_arity, kind_u8))
    }

    #[cfg(not(feature = "jit"))]
    pub(crate) fn maybe_compile_entries_for(
        &mut self,
        func: &Rc<Function>,
        call_count: u32,
    ) -> Option<(CompiledThunk, Option<SpecializedThunkRaw>, u8, u8)> {
        let _ = (func, call_count);
        None
    }

    /// Compile if loop-hot and return the compiled thunk, if one is
    /// available. This uses the loop threshold rather than the call
    /// threshold so single-call long-running loops can tier up.
    pub(crate) fn maybe_compile_loop_thunk(
        &mut self,
        func: &Rc<Function>,
        loop_count: u32,
    ) -> Option<CompiledThunk> {
        if loop_count < self.loop_threshold {
            return None;
        }

        #[cfg(not(feature = "jit"))]
        {
            let _ = func;
            None
        }

        #[cfg(feature = "jit")]
        {
            let inner = self.inner.get_or_insert_with(engine::JitInner::new);
            inner.maybe_compile_thunk(func)
        }
    }

    /// Compile a function that statically contains a loop. This is used at
    /// function entry for default-mode smart tiering so single-call hot
    /// loops can execute natively without waiting for a second call.
    pub(crate) fn maybe_compile_loop_entry_thunk(
        &mut self,
        func: &Rc<Function>,
    ) -> Option<CompiledThunk> {
        if self.loop_threshold == u32::MAX || !func.has_loop {
            return None;
        }

        #[cfg(not(feature = "jit"))]
        {
            None
        }

        #[cfg(feature = "jit")]
        {
            let inner = self.inner.get_or_insert_with(engine::JitInner::new);
            inner.maybe_compile_thunk(func)
        }
    }

    /// Invoke the compiled entry for this closure's function, if any.
    ///
    /// # Preconditions
    /// - The caller has already pushed a `CallFrame` for the callee.
    /// - `stop_depth` is the frame count *before* that push, so the helper
    ///   knows when to exit the interpreter loop it's driving.
    ///
    /// # Safety
    /// `vm` must be the unique `&mut VM` borrow that invoked us; no other
    /// code may alias it while this call runs.
    pub unsafe fn try_invoke(
        &mut self,
        vm: *mut VM,
        closure: &Rc<ObjClosure>,
        stop_depth: usize,
    ) -> JitExit {
        #[cfg(not(feature = "jit"))]
        {
            let _ = (vm, closure, stop_depth);
            JitExit::Bailout
        }

        #[cfg(feature = "jit")]
        {
            let Some(inner) = self.inner.as_mut() else {
                return JitExit::Bailout;
            };
            match unsafe { inner.invoke(vm, &closure.function, stop_depth) } {
                engine::InvokeOutcome::Returned => JitExit::Returned,
                engine::InvokeOutcome::Bailout => JitExit::Bailout,
                engine::InvokeOutcome::RuntimeError(err) => JitExit::RuntimeError(err),
            }
        }
    }

    /// Invoke an already-known compiled thunk. The caller has already
    /// performed any compiled-entry lookup and pushed the callee frame.
    pub(crate) unsafe fn invoke_thunk(
        &mut self,
        vm: *mut VM,
        thunk: CompiledThunk,
        stop_depth: usize,
    ) -> JitExit {
        #[cfg(not(feature = "jit"))]
        {
            let _ = (vm, thunk, stop_depth);
            JitExit::Bailout
        }

        #[cfg(feature = "jit")]
        {
            let Some(inner) = self.inner.as_mut() else {
                return JitExit::Bailout;
            };
            match unsafe { inner.invoke_thunk(vm, thunk, stop_depth) } {
                engine::InvokeOutcome::Returned => JitExit::Returned,
                engine::InvokeOutcome::Bailout => JitExit::Bailout,
                engine::InvokeOutcome::RuntimeError(err) => JitExit::RuntimeError(err),
            }
        }
    }

    // ── Observability (primarily for tests) ───────────────────────────

    /// Number of functions for which the JIT successfully emitted native
    /// code. Always `0` when the `jit` feature is disabled.
    pub fn compiled_ok_count(&self) -> usize {
        #[cfg(not(feature = "jit"))]
        {
            0
        }
        #[cfg(feature = "jit")]
        {
            self.inner
                .as_ref()
                .map(|i| i.compiled_ok_count())
                .unwrap_or(0)
        }
    }

    /// Number of functions the JIT refused to compile (scan rejection,
    /// Cranelift error). Always `0` when the `jit` feature is disabled.
    pub fn compile_failed_count(&self) -> usize {
        #[cfg(not(feature = "jit"))]
        {
            0
        }
        #[cfg(feature = "jit")]
        {
            self.inner
                .as_ref()
                .map(|i| i.compile_failed_count())
                .unwrap_or(0)
        }
    }

    pub fn array_index_fast_hits(&self) -> usize {
        self.array_index_fast_hits
    }

    pub fn array_index_fast_misses(&self) -> usize {
        self.array_index_fast_misses
    }

    // ── Helpers used by runtime.rs (feature-gated) ────────────────────

    #[cfg(feature = "jit")]
    pub(crate) fn record_array_index_fast_hit(&mut self) {
        self.array_index_fast_hits = self.array_index_fast_hits.saturating_add(1);
    }

    #[cfg(feature = "jit")]
    pub(crate) fn record_array_index_fast_miss(&mut self) {
        self.array_index_fast_misses = self.array_index_fast_misses.saturating_add(1);
    }

    #[cfg(feature = "jit")]
    pub(crate) fn current_stop_depth(&self) -> usize {
        self.inner
            .as_ref()
            .map(|i| i.current_stop_depth())
            .unwrap_or(0)
    }

    #[cfg(feature = "jit")]
    #[allow(dead_code)]
    pub(crate) fn has_pending_error(&self) -> bool {
        self.inner
            .as_ref()
            .map(|i| i.has_pending_error())
            .unwrap_or(false)
    }

    #[cfg(feature = "jit")]
    pub(crate) fn stash_error(&mut self, err: VMError) {
        if let Some(inner) = self.inner.as_mut() {
            inner.stash_error(err);
        }
    }
}
