//! Bounded string interner for symbol-like strings.
//!
//! Used ONLY for compile-time constant-pool strings (literals,
//! identifiers, field/method/global names, map-key literals). Those are
//! bounded by the program text, so the table cannot grow without bound.
//!
//! It is deliberately **not** used for arbitrary runtime strings
//! (concatenation results, user input, formatted output): there is no GC
//! to evict them, so interning the unbounded set would leak. Runtime
//! strings continue to use `value::rc_str`.
//!
//! Payoff: every interned literal shares one canonical `Rc<String>`, so
//! comparing two of them is a single pointer compare via the `Rc::ptr_eq`
//! fast path in `Value`'s `PartialEq` (and they dedup in memory).
//!
//! Single-threaded: compilation runs on one thread, so a `thread_local`
//! `RefCell` needs no locking, and the `!Send` `Rc` values are confined
//! to that thread.

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

thread_local! {
    static INTERN: RefCell<HashMap<Box<str>, Rc<String>>> = RefCell::new(HashMap::new());
}

/// Return the canonical `Rc<String>` for `s`, interning it if new.
///
/// Use ONLY for symbol-like / constant-pool strings (see module docs).
pub fn intern(s: &str) -> Rc<String> {
    INTERN.with(|t| {
        let mut t = t.borrow_mut();
        if let Some(rc) = t.get(s) {
            return rc.clone();
        }
        let rc = Rc::new(s.to_owned());
        t.insert(s.into(), rc.clone());
        rc
    })
}

/// Number of distinct interned strings. Test/diagnostic only.
#[cfg(test)]
pub fn interned_count() -> usize {
    INTERN.with(|t| t.borrow().len())
}
