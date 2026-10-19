//! Limits a host puts on a running script: a loop budget and an interrupt.
//!
//! Both are checked on loop back-edges, the only place a script can run
//! forever (unbounded recursion already ends in a `stack overflow` error).
//!
//! The check has to be nearly free when neither limit is in use, because
//! every loop iteration pays it. So the common path reads one byte, the
//! armed byte, which is non-zero only while an interrupt is pending or a
//! budget is set. Only then does a back-edge do more: it stops on an
//! interrupt, and otherwise counts the iteration against
//! `VM::loop_countdown`, the remaining budget, stopping when it is spent.
//! The byte lives in an `Arc` shared with every [`InterruptHandle`], so
//! another thread can set it.
//!
//! JIT-compiled back-edges keep the counting inline and call out only on
//! the way to stopping. A call that could return into the loop would make
//! the register allocator keep the loop's values safe across it, spilling
//! them, which doubled the cost of a tight loop.

use std::sync::Arc;
use std::sync::atomic::{AtomicU8, Ordering};

/// Armed-byte bits. JIT-compiled back-edges test `INTERRUPT` directly.
pub(crate) const INTERRUPT: u8 = 1;
const BUDGET: u8 = 2;

/// The state a VM shares with its interrupt handles (and its child VMs).
#[derive(Debug, Default)]
pub(crate) struct Limits {
    armed: AtomicU8,
}

impl Limits {
    /// The byte JIT-compiled back-edges read: non-zero means take the slow path.
    pub(crate) fn armed_ptr(&self) -> *const AtomicU8 {
        &self.armed
    }

    pub(crate) fn is_armed(&self) -> bool {
        self.armed.load(Ordering::Relaxed) != 0
    }

    pub(crate) fn budget_on(&self) -> bool {
        self.armed.load(Ordering::Relaxed) & BUDGET != 0
    }

    pub(crate) fn set_budget(&self, on: bool) {
        if on {
            self.armed.fetch_or(BUDGET, Ordering::Relaxed);
        } else {
            self.armed.fetch_and(!BUDGET, Ordering::Relaxed);
        }
    }

    /// Clears a pending interrupt and reports whether there was one.
    pub(crate) fn take_interrupt(&self) -> bool {
        self.armed.fetch_and(!INTERRUPT, Ordering::Relaxed) & INTERRUPT != 0
    }
}

/// Stops a running script from any thread. Cloned from
/// [`VM::interrupt_handle`](crate::vm::VM::interrupt_handle); every clone
/// controls the same VM.
#[derive(Clone, Debug, Default)]
pub struct InterruptHandle(pub(crate) Arc<Limits>);

impl InterruptHandle {
    /// Ask the VM to stop. The running script fails with an `interrupted`
    /// error at its next loop iteration; the request is used up once
    /// delivered. If no script is running, the next one to loop is stopped.
    pub fn interrupt(&self) {
        self.0.armed.fetch_or(INTERRUPT, Ordering::Relaxed);
    }
}
