//! Ordered, hash-indexed Map/Set backing stores.
//!
//! Replaces the previous `Vec<(Value, Value)>` / `Vec<Value>` linear-scan
//! representations. Insertion order is preserved (iteration and Display
//! depend on it); lookups are O(1) average via `index`, with a linear
//! fallback for keys that have no hashable projection (`try_hash_key`
//! returned `None` — e.g. an `Array`/`Map`/`StructInstance` used as a key).

use std::collections::HashMap;
use std::rc::Rc;

use super::value::Value;

/// Hashable projection of a `Value` usable as a map/set key.
///
/// Mirrors `Value`'s `PartialEq` for the hashable kinds. `Integer` and
/// `Float` are deliberately distinct (matching `Value::eq`, where
/// `Integer(1) != Float(1.0)`).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum HashKey {
    Int(i64),
    /// Canonicalized f64 bits: `-0.0` folds to `+0.0`, all NaNs fold to a
    /// single quiet-NaN pattern. See `f64_key_bits` and the module note.
    Float(u64),
    Bool(bool),
    Char(char),
    Byte(u8),
    Uint(u64),
    /// Content-hashed/compared via `Rc<String>`'s `Hash`/`Eq` (delegates
    /// to `String`), so distinct `Rc`s with equal contents are one key.
    Str(Rc<String>),
    None,
    /// Tuple of hashable elements; `None` if any element is non-hashable.
    Tuple(Vec<HashKey>),
}

#[inline]
fn f64_key_bits(f: f64) -> u64 {
    if f == 0.0 {
        0 // unify +0.0 and -0.0 (they are `==`, so must be one key)
    } else if f.is_nan() {
        0x7ff8_0000_0000_0000 // canonical qNaN
    } else {
        f.to_bits()
    }
}

impl Value {
    /// Hashable projection for use as a Map/Set key. `None` means the
    /// value is not hashable and callers must use a linear scan.
    pub fn try_hash_key(&self) -> Option<HashKey> {
        Some(match self {
            Value::Integer(i) => HashKey::Int(*i),
            Value::Float(f) => HashKey::Float(f64_key_bits(*f)),
            Value::Boolean(b) => HashKey::Bool(*b),
            Value::Char(c) => HashKey::Char(*c),
            Value::Byte(b) => HashKey::Byte(*b),
            Value::Uint(u) => HashKey::Uint(*u),
            Value::String(s) => HashKey::Str(s.clone()),
            Value::None => HashKey::None,
            Value::Tuple(t) => {
                let mut ks = Vec::with_capacity(t.len());
                for e in t.iter() {
                    ks.push(e.try_hash_key()?);
                }
                HashKey::Tuple(ks)
            }
            _ => return None, // Array, Map, Set, StructInstance, closures…
        })
    }
}

/// Insertion-ordered map with O(1) average lookup for hashable keys.
#[derive(Debug, Clone, Default)]
pub struct OxMap {
    entries: Vec<(Value, Value)>,
    index: HashMap<HashKey, usize>,
}

impl OxMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Borrow the ordered entries (for iteration / Display / builtins).
    pub fn entries(&self) -> &[(Value, Value)] {
        &self.entries
    }
    pub fn iter(&self) -> std::slice::Iter<'_, (Value, Value)> {
        self.entries.iter()
    }

    pub fn get(&self, key: &Value) -> Option<&Value> {
        if let Some(hk) = key.try_hash_key() {
            self.index.get(&hk).map(|&i| &self.entries[i].1)
        } else {
            self.entries.iter().find(|(k, _)| k == key).map(|(_, v)| v)
        }
    }

    pub fn contains_key(&self, key: &Value) -> bool {
        if let Some(hk) = key.try_hash_key() {
            self.index.contains_key(&hk)
        } else {
            self.entries.iter().any(|(k, _)| k == key)
        }
    }

    /// Insert or overwrite, preserving insertion order on overwrite.
    pub fn insert(&mut self, key: Value, val: Value) {
        if let Some(hk) = key.try_hash_key() {
            if let Some(&i) = self.index.get(&hk) {
                self.entries[i].1 = val;
            } else {
                let i = self.entries.len();
                self.entries.push((key, val));
                self.index.insert(hk, i);
            }
        } else if let Some(e) = self.entries.iter_mut().find(|(k, _)| *k == key) {
            e.1 = val;
        } else {
            self.entries.push((key, val));
        }
    }

    /// Remove by key, preserving order. Returns the removed value.
    /// O(n) (entry shift + index rebuild); deletes are rare.
    pub fn remove(&mut self, key: &Value) -> Option<Value> {
        let pos = if let Some(hk) = key.try_hash_key() {
            self.index.get(&hk).copied()
        } else {
            self.entries.iter().position(|(k, _)| k == key)
        }?;
        let (_, v) = self.entries.remove(pos);
        self.reindex();
        Some(v)
    }

    fn reindex(&mut self) {
        self.index.clear();
        for (i, (k, _)) in self.entries.iter().enumerate() {
            if let Some(hk) = k.try_hash_key() {
                self.index.insert(hk, i);
            }
        }
    }

    pub fn keys(&self) -> impl Iterator<Item = &Value> {
        self.entries.iter().map(|(k, _)| k)
    }
    pub fn values(&self) -> impl Iterator<Item = &Value> {
        self.entries.iter().map(|(_, v)| v)
    }

    pub fn from_pairs(pairs: Vec<(Value, Value)>) -> Self {
        let mut m = Self::new();
        for (k, v) in pairs {
            m.insert(k, v);
        }
        m
    }
}

impl PartialEq for OxMap {
    fn eq(&self, other: &Self) -> bool {
        // Order-insensitive, key-based — the correct map semantics. The old
        // `Vec` equality was order-sensitive; see the migration note in
        // docs/PERF_PLAN_LUAJIT.md for the (intentional) behavior delta.
        if self.entries.len() != other.entries.len() {
            return false;
        }
        self.entries.iter().all(|(k, v)| other.get(k) == Some(v))
    }
}

/// Insertion-ordered set with O(1) average membership for hashable elems.
#[derive(Debug, Clone, Default)]
pub struct OxSet {
    items: Vec<Value>,
    index: HashMap<HashKey, usize>,
}

impl OxSet {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn len(&self) -> usize {
        self.items.len()
    }
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
    pub fn items(&self) -> &[Value] {
        &self.items
    }
    pub fn iter(&self) -> std::slice::Iter<'_, Value> {
        self.items.iter()
    }

    pub fn contains(&self, v: &Value) -> bool {
        if let Some(hk) = v.try_hash_key() {
            self.index.contains_key(&hk)
        } else {
            self.items.iter().any(|i| i == v)
        }
    }

    /// Insert if absent. Returns true if newly inserted.
    pub fn insert(&mut self, v: Value) -> bool {
        if self.contains(&v) {
            return false;
        }
        if let Some(hk) = v.try_hash_key() {
            self.index.insert(hk, self.items.len());
        }
        self.items.push(v);
        true
    }

    /// Remove a value, preserving order. Returns true if it was present.
    pub fn remove(&mut self, v: &Value) -> bool {
        let pos = if let Some(hk) = v.try_hash_key() {
            self.index.get(&hk).copied()
        } else {
            self.items.iter().position(|i| i == v)
        };
        match pos {
            Some(p) => {
                self.items.remove(p);
                self.reindex();
                true
            }
            None => false,
        }
    }

    fn reindex(&mut self) {
        self.index.clear();
        for (i, v) in self.items.iter().enumerate() {
            if let Some(hk) = v.try_hash_key() {
                self.index.insert(hk, i);
            }
        }
    }

    pub fn from_iter_dedup(vals: impl IntoIterator<Item = Value>) -> Self {
        let mut s = Self::new();
        for v in vals {
            s.insert(v);
        }
        s
    }
}

impl PartialEq for OxSet {
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len() && self.items.iter().all(|i| other.contains(i))
    }
}
