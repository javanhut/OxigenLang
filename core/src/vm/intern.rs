//! Bounded string interner for symbol-like strings.
//!
//! Used for compile-time constant-pool strings (literals, identifiers, field/method/global names, map-key literals).
//! Every interned literal shares one canonical `Rc<String>`, so comparing two of them is a pointer compare.
//!
//! The table is thread-local and single-threaded.

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

thread_local! {
    static INTERN: RefCell<HashMap<Box<str>, Rc<String>>> = RefCell::new(HashMap::new());
}

/// Return the canonical `Rc<String>` for `s`, interning it if new.
///
/// Use only for symbol-like / constant-pool strings.
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
