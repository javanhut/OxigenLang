//! Phase 1 regression tests for the hash-backed Map/Set backing stores
//! (`core/src/vm/collections.rs`). See docs/PERF_PLAN_LUAJIT.md.

use oxigen_core::vm::collections::{OxMap, OxSet};
use oxigen_core::vm::value::Value;
use std::rc::Rc;

fn s(x: &str) -> Value {
    Value::String(Rc::new(x.to_string()))
}

#[test]
fn map_insert_get_overwrite_preserves_order() {
    let mut m = OxMap::new();
    m.insert(s("a"), Value::Integer(1));
    m.insert(s("b"), Value::Integer(2));
    m.insert(s("a"), Value::Integer(3)); // overwrite, must keep position 0

    assert_eq!(m.get(&s("a")), Some(&Value::Integer(3)));
    assert_eq!(m.get(&s("b")), Some(&Value::Integer(2)));
    assert_eq!(m.len(), 2);

    let order: Vec<&Value> = m.keys().collect();
    assert_eq!(order, vec![&s("a"), &s("b")]); // insertion order preserved
}

#[test]
fn map_int_vs_float_keys_distinct() {
    let mut m = OxMap::new();
    m.insert(Value::Integer(1), s("int"));
    m.insert(Value::Float(1.0), s("float"));
    assert_eq!(m.len(), 2, "Integer(1) and Float(1.0) are distinct keys");
    assert_eq!(m.get(&Value::Integer(1)), Some(&s("int")));
    assert_eq!(m.get(&Value::Float(1.0)), Some(&s("float")));
}

#[test]
fn map_nan_key_is_stable() {
    // Old linear-scan behavior: NaN != NaN, so a NaN key was unreadable.
    // New behavior: NaN canonicalizes to a single stable key.
    let mut m = OxMap::new();
    m.insert(Value::Float(f64::NAN), Value::Integer(42));
    assert_eq!(m.get(&Value::Float(f64::NAN)), Some(&Value::Integer(42)));
    // Re-inserting the same NaN key overwrites rather than duplicating.
    m.insert(Value::Float(f64::NAN), Value::Integer(43));
    assert_eq!(m.len(), 1);
    assert_eq!(m.get(&Value::Float(f64::NAN)), Some(&Value::Integer(43)));
}

#[test]
fn map_neg_zero_and_zero_unify() {
    let mut m = OxMap::new();
    m.insert(Value::Float(0.0), Value::Integer(1));
    m.insert(Value::Float(-0.0), Value::Integer(2)); // same key (0.0 == -0.0)
    assert_eq!(m.len(), 1);
    assert_eq!(m.get(&Value::Float(0.0)), Some(&Value::Integer(2)));
}

#[test]
fn map_non_hashable_key_falls_back_to_linear() {
    // An Array used as a key has no hashable projection; the linear
    // fallback must still find it.
    let mut m = OxMap::new();
    let arr = Value::Array(Rc::new(std::cell::RefCell::new(vec![
        Value::Integer(1),
        Value::Integer(2),
    ])));
    m.insert(arr.clone(), s("found"));
    m.insert(s("plain"), Value::Integer(9));
    assert_eq!(m.get(&arr), Some(&s("found")));
    assert_eq!(m.get(&s("plain")), Some(&Value::Integer(9)));
}

#[test]
fn map_remove_preserves_order_and_reindexes() {
    let mut m = OxMap::new();
    for (k, v) in [("a", 1), ("b", 2), ("c", 3)] {
        m.insert(s(k), Value::Integer(v));
    }
    assert_eq!(m.remove(&s("a")), Some(Value::Integer(1)));
    assert_eq!(m.len(), 2);
    // Surviving keys still resolve via the rebuilt index.
    assert_eq!(m.get(&s("b")), Some(&Value::Integer(2)));
    assert_eq!(m.get(&s("c")), Some(&Value::Integer(3)));
    let order: Vec<&Value> = m.keys().collect();
    assert_eq!(order, vec![&s("b"), &s("c")]);
}

#[test]
fn map_eq_is_order_insensitive() {
    let mut a = OxMap::new();
    a.insert(s("x"), Value::Integer(1));
    a.insert(s("y"), Value::Integer(2));
    let mut b = OxMap::new();
    b.insert(s("y"), Value::Integer(2));
    b.insert(s("x"), Value::Integer(1));
    assert_eq!(a, b, "maps with same entries in different order are equal");
}

#[test]
fn set_dedup_and_membership() {
    let s_ = OxSet::from_iter_dedup(vec![
        Value::Integer(1),
        Value::Integer(2),
        Value::Integer(1), // dup
        Value::Integer(3),
    ]);
    assert_eq!(s_.len(), 3);
    assert!(s_.contains(&Value::Integer(2)));
    assert!(!s_.contains(&Value::Integer(9)));
    // Insertion order preserved, dup dropped.
    let order: Vec<&Value> = s_.iter().collect();
    assert_eq!(
        order,
        vec![&Value::Integer(1), &Value::Integer(2), &Value::Integer(3)]
    );
}

#[test]
fn set_eq_is_order_insensitive() {
    let a = OxSet::from_iter_dedup(vec![Value::Integer(1), Value::Integer(2)]);
    let b = OxSet::from_iter_dedup(vec![Value::Integer(2), Value::Integer(1)]);
    assert_eq!(a, b);
}

#[test]
fn map_large_is_not_linear() {
    // Smoke test: 100k inserts + 100k lookups must complete quickly. A
    // regression to O(n) would make this take seconds; O(1) is instant.
    let mut m = OxMap::new();
    for i in 0..100_000 {
        m.insert(Value::Integer(i), Value::Integer(i * 2));
    }
    let mut acc = 0i64;
    for i in 0..100_000 {
        if let Some(Value::Integer(v)) = m.get(&Value::Integer(i)) {
            acc += v;
        }
    }
    assert_eq!(acc, (0..100_000i64).map(|i| i * 2).sum());
}
