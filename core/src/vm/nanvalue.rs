//! NaN-boxed `Value` representation — A1 of the optimization roadmap.
//!
//! This module defines `NanValue`, a Value representation that fits in a
//! single `u64` (8 bytes) instead of the current 40-byte tagged enum. It
//! is **not yet wired into the rest of the VM** — that migration is
//! A1.1–A1.5 in `docs/optimization-roadmap.md`. This file is the
//! foundation: the encoding, the Clone/Drop semantics, and the
//! round-trip tests that establish correctness before the wholesale
//! migration begins.
//!
//! # Why NaN-boxing
//!
//! An IEEE-754 `f64` has 2^52 NaN bit patterns but only one "real" NaN
//! semantically. We can store any value that fits in 48 bits inside the
//! unused-NaN bit space: integers up to ±2^47, booleans, pointers to
//! heap objects, function pointers, etc. A 64-bit word is either a
//! genuine finite `f64` **or** a tagged payload — never both.
//!
//! Net effect: Value shrinks from 40 B to 8 B (5× cache reduction),
//! stack stride from `imul slot, 40` to `shl slot, 3`, and the JIT can
//! keep values live in single registers across opcodes.
//!
//! # Encoding scheme
//!
//! The top 16 bits of the `u64` carry the tag. Tag values are chosen so
//! that no non-NaN `f64` ever uses them, and the one canonical NaN we
//! allow from `f64` storage (`0x7FF8_0000_0000_0000`) sits safely below
//! the tagged-value range.
//!
//! ```text
//! Bit pattern                         Kind
//! ──────────────────────────────────  ──────────────────────────────────
//! any pattern < 0xFFF8_…              f64 (including canonical NaN at
//!                                     0x7FF8_0000_0000_0000)
//! 0xFFF8_<48-bit i48>                 SMI: signed integer in [-2^47, 2^47)
//! 0xFFF9_<48-bit u48>                 SMI-Uint: unsigned integer < 2^48
//! 0xFFFA_<subkind byte><40-bit pay>   Primitive: Bool / None / Char / Byte
//! 0xFFFB_<48-bit fn ptr>              Builtin function pointer
//! 0xFFFC_<subkind:3 bits>_<44-bit ptr> Pointer group A (8 kinds)
//! 0xFFFD_<subkind:3 bits>_<44-bit ptr> Pointer group B (8 kinds)
//! 0xFFFE, 0xFFFF                       reserved for future use
//! ```
//!
//! ## Why two pointer groups
//!
//! We have 16 heap-object variants in the existing `Value` enum
//! (`String`, `Array`, `Tuple`, `Map`, `Set`, `Closure`, `StructDef`,
//! `StructInstance`, `EnumDef`, `EnumInstance`, `Module`, `ErrorValue`,
//! `Wrapped`, `Error`, and space for boxed ints/uints). Encoding all 16
//! kinds in a single tag would require 4 subkind bits — more than the
//! 3 bits we can safely carry in the low bits of an 8-aligned pointer.
//! Two tag slots × 8 subkinds = 16 kinds, with the pointer occupying
//! the full 44–47 available bits.
//!
//! Alignment invariant: every heap object stored through this Value is
//! reached via `Rc<T>`, which allocates with alignment `max(8,
//! align_of::<T>())`. The low 3 bits of every Rc pointer are therefore
//! zero and we can repurpose them for the subkind tag.
//!
//! ## Why not 4 bits of subkind in the top nibble of payload
//!
//! That would leave only 44 bits of pointer, and x86_64 Linux user-space
//! can reach 47-bit addresses (128 TiB virtual). We'd eventually trip on
//! ASLR producing an allocation outside the representable range.
//! Packing the subkind into the 8-byte-alignment slack of the pointer
//! avoids this: the pointer keeps its full 48-bit range, and subkind
//! lives in bits 0–2 (always 0 for aligned allocations anyway).
//!
//! # Drop / Clone semantics
//!
//! Pointer-bearing tags (`TAG_POINTER_A`, `TAG_POINTER_B`) hold the raw
//! bit pattern of an `Rc<T>`, meaning the `NanValue` **owns a logical
//! strong-count reference**. Clone and Drop MUST maintain the refcount:
//! - `Clone`: decode the Rc pointer + subkind, call `Rc::clone` (which
//!   bumps `strong`), re-encode.
//! - `Drop`: decode, reconstruct the `Rc<T>` via `Rc::from_raw`, and let
//!   it drop naturally (which decrements `strong` and, on 0, frees).
//!
//! The JIT's inline method-call IC (currently in `jit/engine.rs`) does
//! the strong-count bump via an inline i64 load+add+store at RcBox
//! offset 0. That invariant still holds under NaN-box: the pointer we
//! store is the `NonNull<RcBox<T>>`, same bit pattern as the current
//! `Value::StructInstance(Rc<_>)` payload, just moved from byte offset 8
//! to the low 48 bits of a 64-bit word (and subkind-masked to 0 for
//! comparisons).
//!
//! # Non-goals for A1.0 (this file)
//!
//! - Wiring `NanValue` into the VM (A1.1).
//! - Wiring into the JIT (A1.3, A1.4).
//! - SMI arithmetic fast paths (A2).
//!
//! Those come later. This file exists so the encoding and its tests can
//! be reviewed and committed in isolation.

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::value::{
    BuiltinFn, ErrorValueData, ObjClosure, ObjEnumDef, ObjEnumInstance, ObjModule, ObjStructDef,
    ObjStructInstance, Value as OldValue,
};

// ── Bit-level constants ────────────────────────────────────────────────

/// Mask covering the 16 tag bits at the top of the word.
pub const TAG_MASK: u64 = 0xFFFF_0000_0000_0000;

/// Mask covering the low 48 bits (payload).
pub const PAYLOAD_MASK: u64 = 0x0000_FFFF_FFFF_FFFF;

/// The one canonical `f64` NaN bit pattern we allow. All other incoming
/// NaN bit patterns (e.g., `-NaN = 0xFFF8_…`) are normalized to this on
/// encode so they don't collide with the tagged-value space.
pub const CANONICAL_F64_NAN_BITS: u64 = 0x7FF8_0000_0000_0000;

/// Tag values (top 16 bits) — only these eight patterns are tagged
/// values. Any bit pattern whose top 16 bits are strictly less than
/// `0xFFF8_…` is a genuine `f64`.
pub const TAG_SMI: u64 = 0xFFF8_0000_0000_0000;
pub const TAG_SMI_UINT: u64 = 0xFFF9_0000_0000_0000;
pub const TAG_PRIMITIVE: u64 = 0xFFFA_0000_0000_0000;
pub const TAG_BUILTIN: u64 = 0xFFFB_0000_0000_0000;
pub const TAG_POINTER_A: u64 = 0xFFFC_0000_0000_0000;
pub const TAG_POINTER_B: u64 = 0xFFFD_0000_0000_0000;
// 0xFFFE, 0xFFFF reserved for future use.

/// SMI range: [SMI_MIN, SMI_MAX]. Integers outside this range box to the
/// heap as a pointer-tagged `Rc<i64>` (`HeapKind::BoxedInt`).
pub const SMI_MIN: i64 = -(1 << 47);
pub const SMI_MAX: i64 = (1 << 47) - 1;

/// Unsigned SMI range: u64 values below `SMI_UINT_MAX` encode inline.
/// Larger values box to a heap `Rc<u64>` (`HeapKind::BoxedUint`).
pub const SMI_UINT_MAX: u64 = (1 << 48) - 1;

/// Subkinds stored in byte 5 (bits 40–47) of a `TAG_PRIMITIVE` value.
/// Payload (low 40 bits) carries the primitive's data.
#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum PrimitiveKind {
    Bool = 0,
    None = 1,
    Char = 2,
    Byte = 3,
}

/// Subkinds for `TAG_POINTER_A`. Low 3 bits of the 48-bit payload.
#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum PointerKindA {
    String = 0,
    Array = 1,
    Tuple = 2,
    Map = 3,
    Set = 4,
    Closure = 5,
    StructDef = 6,
    StructInstance = 7,
}

/// Subkinds for `TAG_POINTER_B`. Low 3 bits of the 48-bit payload.
#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum PointerKindB {
    EnumDef = 0,
    EnumInstance = 1,
    Module = 2,
    ErrorValue = 3,
    Wrapped = 4,
    Error = 5,
    BoxedInt = 6,
    BoxedUint = 7,
}

// ── Pre-computed complete bit patterns for None and Bool singletons ────

const NANVALUE_NONE_BITS: u64 = TAG_PRIMITIVE | ((PrimitiveKind::None as u64) << 40);
const NANVALUE_BOOL_FALSE_BITS: u64 = TAG_PRIMITIVE | ((PrimitiveKind::Bool as u64) << 40);
const NANVALUE_BOOL_TRUE_BITS: u64 = NANVALUE_BOOL_FALSE_BITS | 1;

// ── Boxed storage types for non-primitive values we can't inline ───────

/// Heap box for `ErrorValue { msg, tag }`. Two `Rc<str>` don't fit in a
/// 48-bit payload, so we store them via this single-pointer indirection.
#[derive(Debug)]
pub struct ErrorValueStorage {
    pub msg: Rc<str>,
    pub tag: Option<Rc<str>>,
}

/// Heap box for `Rc<Value>` (`Wrapped`). Keeps the same ownership shape
/// the current enum has: a single-pointer indirection to a Value.
#[derive(Debug)]
pub struct WrappedStorage {
    pub inner: NanValue,
}

// ── The NaN-boxed Value itself ─────────────────────────────────────────

/// A NaN-boxed runtime value. 8 bytes on all 64-bit targets. `Drop` runs
/// the Rc decrement on pointer-bearing kinds; `Clone` runs the bump.
#[repr(transparent)]
pub struct NanValue {
    raw: u64,
}

impl std::fmt::Debug for NanValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Hex dump of the raw bits plus a decoded kind tag. Detailed
        // per-variant pretty-printing will be added once the type is
        // actually wired into the VM.
        let tag = self.raw & TAG_MASK;
        let kind = match tag {
            _ if self.is_f64() => "f64",
            TAG_SMI => "smi",
            TAG_SMI_UINT => "smi_uint",
            TAG_PRIMITIVE => match (self.raw >> 40) & 0xFF {
                x if x == PrimitiveKind::Bool as u64 => "bool",
                x if x == PrimitiveKind::None as u64 => "none",
                x if x == PrimitiveKind::Char as u64 => "char",
                x if x == PrimitiveKind::Byte as u64 => "byte",
                _ => "primitive?",
            },
            TAG_BUILTIN => "builtin",
            TAG_POINTER_A => "ptr_a",
            TAG_POINTER_B => "ptr_b",
            _ => "unknown",
        };
        write!(f, "NanValue(0x{:016x} {})", self.raw, kind)
    }
}

impl NanValue {
    // ── Primitive / SMI constructors ──────────────────────────────────

    #[inline]
    pub fn none() -> Self {
        NanValue {
            raw: NANVALUE_NONE_BITS,
        }
    }

    #[inline]
    pub fn from_bool(b: bool) -> Self {
        NanValue {
            raw: if b {
                NANVALUE_BOOL_TRUE_BITS
            } else {
                NANVALUE_BOOL_FALSE_BITS
            },
        }
    }

    #[inline]
    pub fn from_char(c: char) -> Self {
        NanValue {
            raw: TAG_PRIMITIVE | ((PrimitiveKind::Char as u64) << 40) | (c as u32 as u64),
        }
    }

    #[inline]
    pub fn from_byte(b: u8) -> Self {
        NanValue {
            raw: TAG_PRIMITIVE | ((PrimitiveKind::Byte as u64) << 40) | (b as u64),
        }
    }

    #[inline]
    pub fn from_f64(x: f64) -> Self {
        let bits = if x.is_nan() {
            CANONICAL_F64_NAN_BITS
        } else {
            x.to_bits()
        };
        NanValue { raw: bits }
    }

    /// Integer constructor: inlines to an SMI when in range, otherwise
    /// boxes on the heap. Use this for `Value::Integer(i64)` parity.
    pub fn from_i64(n: i64) -> Self {
        if (SMI_MIN..=SMI_MAX).contains(&n) {
            Self::smi_from_i64_unchecked(n)
        } else {
            // Box the outlier into a heap i64.
            let rc: Rc<i64> = Rc::new(n);
            Self::from_rc_pointer_b(Rc::into_raw(rc) as *const (), PointerKindB::BoxedInt)
        }
    }

    /// Unsigned integer constructor: inlines when < 2^48, else boxes.
    pub fn from_u64(n: u64) -> Self {
        if n <= SMI_UINT_MAX {
            NanValue {
                raw: TAG_SMI_UINT | n,
            }
        } else {
            let rc: Rc<u64> = Rc::new(n);
            Self::from_rc_pointer_b(Rc::into_raw(rc) as *const (), PointerKindB::BoxedUint)
        }
    }

    /// Inline SMI without range check — caller must have checked.
    #[inline]
    fn smi_from_i64_unchecked(n: i64) -> Self {
        NanValue {
            raw: TAG_SMI | ((n as u64) & PAYLOAD_MASK),
        }
    }

    // ── Pointer-kind constructors ─────────────────────────────────────

    pub fn from_string(s: Rc<str>) -> Self {
        // Rc<str> is a fat pointer — Rc::into_raw returns a *const str.
        // The raw pointer's data part is what we want to NaN-box; the
        // length lives in the RcBox header (Rc<str> stores the slice
        // metadata there). We use Rc::into_raw to get the thin
        // *const u8 to the start of the T area inside RcBox.
        //
        // For NaN-boxing we need the NonNull<RcBox<str>> bit pattern to
        // round-trip Rc::from_raw correctly. We obtain it via
        // Rc::into_raw, which returns a *const T pointing at T inside
        // the box.
        let raw = Rc::into_raw(s);
        // `Rc<str>` has a data-and-length raw pointer (fat). We can
        // round-trip it through Rc::from_raw because Rc preserves the
        // fat-pointer shape. But NaN-box only has 48 bits of payload —
        // we can't fit a fat pointer in 48 bits.
        //
        // Resolution: store as Rc<String> instead of Rc<str>. The
        // migration will need to change all String construction sites.
        // For A1.0 we store Rc<str> by wrapping it in a thin pointer
        // via Box::into_raw + Box::leak.
        //
        // See `from_rc_str_via_box` below for the full dance. We
        // chose to use Rc<String> as the canonical heap string from
        // here on; `Rc<str>` consumers convert at the boundary.
        unsafe {
            // Safety: we just got `raw` from `Rc::into_raw`. To go to a
            // thin-pointer form, reconstruct and rebox as Rc<String>.
            let s_back: Rc<str> = Rc::from_raw(raw);
            let s_owned: String = (*s_back).to_owned();
            let rc_string: Rc<String> = Rc::new(s_owned);
            Self::from_rc_pointer_a(Rc::into_raw(rc_string) as *const (), PointerKindA::String)
        }
    }

    pub fn from_rc_string(s: Rc<String>) -> Self {
        Self::from_rc_pointer_a(Rc::into_raw(s) as *const (), PointerKindA::String)
    }

    pub fn from_array(a: Rc<RefCell<Vec<NanValue>>>) -> Self {
        Self::from_rc_pointer_a(Rc::into_raw(a) as *const (), PointerKindA::Array)
    }

    pub fn from_tuple(t: Rc<Vec<NanValue>>) -> Self {
        Self::from_rc_pointer_a(Rc::into_raw(t) as *const (), PointerKindA::Tuple)
    }

    pub fn from_map(m: Rc<RefCell<Vec<(NanValue, NanValue)>>>) -> Self {
        Self::from_rc_pointer_a(Rc::into_raw(m) as *const (), PointerKindA::Map)
    }

    pub fn from_set(s: Rc<RefCell<Vec<NanValue>>>) -> Self {
        Self::from_rc_pointer_a(Rc::into_raw(s) as *const (), PointerKindA::Set)
    }

    pub fn from_closure(c: Rc<ObjClosure>) -> Self {
        Self::from_rc_pointer_a(Rc::into_raw(c) as *const (), PointerKindA::Closure)
    }

    pub fn from_struct_def(d: Rc<ObjStructDef>) -> Self {
        Self::from_rc_pointer_a(Rc::into_raw(d) as *const (), PointerKindA::StructDef)
    }

    pub fn from_struct_instance(i: Rc<ObjStructInstance>) -> Self {
        Self::from_rc_pointer_a(Rc::into_raw(i) as *const (), PointerKindA::StructInstance)
    }

    pub fn from_enum_def(d: Rc<ObjEnumDef>) -> Self {
        Self::from_rc_pointer_b(Rc::into_raw(d) as *const (), PointerKindB::EnumDef)
    }

    pub fn from_enum_instance(i: Rc<ObjEnumInstance>) -> Self {
        Self::from_rc_pointer_b(Rc::into_raw(i) as *const (), PointerKindB::EnumInstance)
    }

    pub fn from_module(m: Rc<ObjModule>) -> Self {
        Self::from_rc_pointer_b(Rc::into_raw(m) as *const (), PointerKindB::Module)
    }

    pub fn from_error_value(storage: Rc<ErrorValueStorage>) -> Self {
        Self::from_rc_pointer_b(Rc::into_raw(storage) as *const (), PointerKindB::ErrorValue)
    }

    pub fn from_wrapped(inner: NanValue) -> Self {
        let rc = Rc::new(WrappedStorage { inner });
        Self::from_rc_pointer_b(Rc::into_raw(rc) as *const (), PointerKindB::Wrapped)
    }

    pub fn from_error(msg: Rc<str>) -> Self {
        // Same Rc<str> → Rc<String> conversion as `from_string`.
        let s_owned: String = (*msg).to_owned();
        Self::from_rc_pointer_b(
            Rc::into_raw(Rc::new(s_owned)) as *const (),
            PointerKindB::Error,
        )
    }

    /// Zero-copy variant of [`from_error`] when the caller already owns
    /// an `Rc<String>` (e.g. the bridge layer from `Value::Error`).
    pub fn from_error_rc_string(msg: Rc<String>) -> Self {
        Self::from_rc_pointer_b(Rc::into_raw(msg) as *const (), PointerKindB::Error)
    }

    pub fn from_builtin(f: BuiltinFn) -> Self {
        // `fn(Vec<Value>) -> Value` — a function pointer. Cast to usize.
        let addr = f as usize as u64;
        debug_assert!(
            addr & TAG_MASK == 0,
            "Builtin fn pointer 0x{:x} exceeds 48-bit payload space",
            addr
        );
        NanValue {
            raw: TAG_BUILTIN | (addr & PAYLOAD_MASK),
        }
    }

    // ── Internal pointer assembly ─────────────────────────────────────

    #[inline]
    fn from_rc_pointer_a(ptr: *const (), kind: PointerKindA) -> Self {
        debug_assert!(!ptr.is_null(), "null pointer into NanValue");
        let addr = ptr as u64;
        debug_assert!(
            addr & 0x7 == 0,
            "Rc<_> payload pointer 0x{:x} is not 8-byte aligned",
            addr
        );
        debug_assert!(
            addr & TAG_MASK == 0,
            "pointer 0x{:x} has high bits that would collide with tag",
            addr
        );
        NanValue {
            raw: TAG_POINTER_A | (addr & PAYLOAD_MASK) | (kind as u64),
        }
    }

    #[inline]
    fn from_rc_pointer_b(ptr: *const (), kind: PointerKindB) -> Self {
        debug_assert!(!ptr.is_null());
        let addr = ptr as u64;
        debug_assert!(addr & 0x7 == 0);
        debug_assert!(addr & TAG_MASK == 0);
        NanValue {
            raw: TAG_POINTER_B | (addr & PAYLOAD_MASK) | (kind as u64),
        }
    }

    // ── Accessors / introspection ─────────────────────────────────────

    #[inline]
    pub fn raw_bits(&self) -> u64 {
        self.raw
    }

    /// True if the word represents a finite or infinite `f64` (any value
    /// whose bit pattern falls below the tagged-value range).
    #[inline]
    pub fn is_f64(&self) -> bool {
        (self.raw & TAG_MASK) < TAG_SMI
    }

    #[inline]
    pub fn as_f64(&self) -> Option<f64> {
        if self.is_f64() {
            Some(f64::from_bits(self.raw))
        } else {
            None
        }
    }

    /// True if the word is either an SMI (inline) or a boxed 64-bit int.
    pub fn is_integer(&self) -> bool {
        if (self.raw & TAG_MASK) == TAG_SMI {
            return true;
        }
        if (self.raw & TAG_MASK) == TAG_POINTER_B {
            let kind = (self.raw & 0x7) as u8;
            return kind == PointerKindB::BoxedInt as u8;
        }
        false
    }

    pub fn as_i64(&self) -> Option<i64> {
        let masked = self.raw & TAG_MASK;
        if masked == TAG_SMI {
            // Sign-extend the 48-bit payload back to i64.
            let payload = self.raw & PAYLOAD_MASK;
            // Shift left 16 to put the sign bit of i48 into the i64 sign
            // position, then arithmetic-shift right 16 to sign-extend.
            let v = ((payload as i64) << 16) >> 16;
            Some(v)
        } else if masked == TAG_POINTER_B {
            let kind = (self.raw & 0x7) as u8;
            if kind == PointerKindB::BoxedInt as u8 {
                let ptr = ((self.raw & PAYLOAD_MASK) & !0x7) as *const i64;
                // SAFETY: we created this via Rc<i64>::into_raw; the
                // pointer is live as long as the NanValue is live.
                Some(unsafe { *ptr })
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn as_u64(&self) -> Option<u64> {
        let masked = self.raw & TAG_MASK;
        if masked == TAG_SMI_UINT {
            Some(self.raw & PAYLOAD_MASK)
        } else if masked == TAG_POINTER_B {
            let kind = (self.raw & 0x7) as u8;
            if kind == PointerKindB::BoxedUint as u8 {
                let ptr = ((self.raw & PAYLOAD_MASK) & !0x7) as *const u64;
                Some(unsafe { *ptr })
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        if (self.raw & TAG_MASK) == TAG_PRIMITIVE {
            let kind = (self.raw >> 40) & 0xFF;
            if kind == PrimitiveKind::Bool as u64 {
                return Some((self.raw & 1) != 0);
            }
        }
        None
    }

    pub fn as_char(&self) -> Option<char> {
        if (self.raw & TAG_MASK) == TAG_PRIMITIVE {
            let kind = (self.raw >> 40) & 0xFF;
            if kind == PrimitiveKind::Char as u64 {
                let cp = (self.raw & 0xFFFF_FFFF) as u32;
                return char::from_u32(cp);
            }
        }
        None
    }

    pub fn as_byte(&self) -> Option<u8> {
        if (self.raw & TAG_MASK) == TAG_PRIMITIVE {
            let kind = (self.raw >> 40) & 0xFF;
            if kind == PrimitiveKind::Byte as u64 {
                return Some((self.raw & 0xFF) as u8);
            }
        }
        None
    }

    pub fn is_none(&self) -> bool {
        self.raw == NANVALUE_NONE_BITS
    }

    pub fn is_pointer(&self) -> bool {
        let masked = self.raw & TAG_MASK;
        masked == TAG_POINTER_A || masked == TAG_POINTER_B
    }

    /// Return the pointer-group kind (A or B) and subkind index 0..=7 if
    /// this value is a pointer; otherwise `None`.
    pub fn pointer_kind(&self) -> Option<(PointerGroup, u8)> {
        let masked = self.raw & TAG_MASK;
        let sub = (self.raw & 0x7) as u8;
        if masked == TAG_POINTER_A {
            Some((PointerGroup::A, sub))
        } else if masked == TAG_POINTER_B {
            Some((PointerGroup::B, sub))
        } else {
            None
        }
    }

    #[inline]
    fn decode_pointer_addr(&self) -> *const () {
        // Low 48 bits with the subkind (low 3 bits) masked off.
        ((self.raw & PAYLOAD_MASK) & !0x7) as *const ()
    }
}

/// Which of the two pointer-tag groups a value lives in.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum PointerGroup {
    A,
    B,
}

// ── Clone / Drop ownership plumbing ────────────────────────────────────

impl Clone for NanValue {
    fn clone(&self) -> Self {
        // Primitives / SMIs / f64 / Builtin are bitwise copies — no Rc
        // to bump.
        let masked = self.raw & TAG_MASK;
        if masked != TAG_POINTER_A && masked != TAG_POINTER_B {
            return NanValue { raw: self.raw };
        }

        // Pointer: reconstruct the typed Rc, bump via Rc::clone, drop
        // the temporary so the original Rc we're cloning isn't touched.
        let addr = self.decode_pointer_addr();
        let sub = (self.raw & 0x7) as u8;
        unsafe {
            match masked {
                m if m == TAG_POINTER_A => match sub {
                    s if s == PointerKindA::String as u8 => {
                        let rc = Rc::<String>::from_raw(addr as *const String);
                        let bumped = Rc::clone(&rc);
                        std::mem::forget(rc);
                        let _ = Rc::into_raw(bumped);
                    }
                    s if s == PointerKindA::Array as u8 => {
                        clone_and_forget::<RefCell<Vec<NanValue>>>(addr)
                    }
                    s if s == PointerKindA::Tuple as u8 => clone_and_forget::<Vec<NanValue>>(addr),
                    s if s == PointerKindA::Map as u8 => {
                        clone_and_forget::<RefCell<Vec<(NanValue, NanValue)>>>(addr)
                    }
                    s if s == PointerKindA::Set as u8 => {
                        clone_and_forget::<RefCell<Vec<NanValue>>>(addr)
                    }
                    s if s == PointerKindA::Closure as u8 => clone_and_forget::<ObjClosure>(addr),
                    s if s == PointerKindA::StructDef as u8 => {
                        clone_and_forget::<ObjStructDef>(addr)
                    }
                    s if s == PointerKindA::StructInstance as u8 => {
                        clone_and_forget::<ObjStructInstance>(addr)
                    }
                    _ => unreachable_subkind(sub),
                },
                m if m == TAG_POINTER_B => match sub {
                    s if s == PointerKindB::EnumDef as u8 => clone_and_forget::<ObjEnumDef>(addr),
                    s if s == PointerKindB::EnumInstance as u8 => {
                        clone_and_forget::<ObjEnumInstance>(addr)
                    }
                    s if s == PointerKindB::Module as u8 => clone_and_forget::<ObjModule>(addr),
                    s if s == PointerKindB::ErrorValue as u8 => {
                        clone_and_forget::<ErrorValueStorage>(addr)
                    }
                    s if s == PointerKindB::Wrapped as u8 => {
                        clone_and_forget::<WrappedStorage>(addr)
                    }
                    s if s == PointerKindB::Error as u8 => clone_and_forget::<String>(addr),
                    s if s == PointerKindB::BoxedInt as u8 => clone_and_forget::<i64>(addr),
                    s if s == PointerKindB::BoxedUint as u8 => clone_and_forget::<u64>(addr),
                    _ => unreachable_subkind(sub),
                },
                _ => (),
            }
        }
        NanValue { raw: self.raw }
    }
}

impl Drop for NanValue {
    fn drop(&mut self) {
        let masked = self.raw & TAG_MASK;
        if masked != TAG_POINTER_A && masked != TAG_POINTER_B {
            return;
        }
        let addr = self.decode_pointer_addr();
        let sub = (self.raw & 0x7) as u8;
        unsafe {
            match masked {
                m if m == TAG_POINTER_A => match sub {
                    s if s == PointerKindA::String as u8 => drop_rc::<String>(addr),
                    s if s == PointerKindA::Array as u8 => drop_rc::<RefCell<Vec<NanValue>>>(addr),
                    s if s == PointerKindA::Tuple as u8 => drop_rc::<Vec<NanValue>>(addr),
                    s if s == PointerKindA::Map as u8 => {
                        drop_rc::<RefCell<Vec<(NanValue, NanValue)>>>(addr)
                    }
                    s if s == PointerKindA::Set as u8 => drop_rc::<RefCell<Vec<NanValue>>>(addr),
                    s if s == PointerKindA::Closure as u8 => drop_rc::<ObjClosure>(addr),
                    s if s == PointerKindA::StructDef as u8 => drop_rc::<ObjStructDef>(addr),
                    s if s == PointerKindA::StructInstance as u8 => {
                        drop_rc::<ObjStructInstance>(addr)
                    }
                    _ => unreachable_subkind(sub),
                },
                m if m == TAG_POINTER_B => match sub {
                    s if s == PointerKindB::EnumDef as u8 => drop_rc::<ObjEnumDef>(addr),
                    s if s == PointerKindB::EnumInstance as u8 => drop_rc::<ObjEnumInstance>(addr),
                    s if s == PointerKindB::Module as u8 => drop_rc::<ObjModule>(addr),
                    s if s == PointerKindB::ErrorValue as u8 => drop_rc::<ErrorValueStorage>(addr),
                    s if s == PointerKindB::Wrapped as u8 => drop_rc::<WrappedStorage>(addr),
                    s if s == PointerKindB::Error as u8 => drop_rc::<String>(addr),
                    s if s == PointerKindB::BoxedInt as u8 => drop_rc::<i64>(addr),
                    s if s == PointerKindB::BoxedUint as u8 => drop_rc::<u64>(addr),
                    _ => unreachable_subkind(sub),
                },
                _ => (),
            }
        }
    }
}

#[inline]
unsafe fn clone_and_forget<T>(addr: *const ()) {
    let typed = addr as *const T;
    // Reconstitute the Rc, clone it (bumping strong), forget the
    // original, and convert the clone back to a raw pointer which we
    // discard — the strong count has been bumped and the original Rc we
    // were cloning is still held by the NanValue caller.
    unsafe {
        let rc = Rc::<T>::from_raw(typed);
        let bumped = Rc::clone(&rc);
        std::mem::forget(rc);
        let _ = Rc::into_raw(bumped);
    }
}

#[inline]
unsafe fn drop_rc<T>(addr: *const ()) {
    let typed = addr as *const T;
    unsafe {
        drop(Rc::<T>::from_raw(typed));
    }
}

#[cold]
#[inline(never)]
fn unreachable_subkind(sub: u8) -> ! {
    panic!("NanValue: unexpected pointer subkind {}", sub)
}

// ── Suppress unused-import warnings for types that the rest of the ────
// ── VM will consume once A1.1 migrates callers. These are part of the
// ── public contract of this module and will be used then.
const _: Option<&HashMap<String, OldValue>> = None;

// ──────────────────────────────────────────────────────────────────────
// Bridge layer: `Value` ↔ `NanValue` (A1.2.1)
//
// Until the flag-day migration replaces `Value` with `NanValue`
// throughout the VM, the two representations coexist. This bridge lets
// us:
//   1. Verify the encoding round-trips faithfully (conformance test).
//   2. Migrate consumers incrementally — a subsystem can switch its
//      internal representation while still exchanging `Value`s at its
//      boundary.
//
// Limitations: container variants (`Array`, `Tuple`, `Map`, `Set`)
// store `NanValue` interiors in NaN-box land but `Value` interiors in
// the legacy enum. Bridging therefore allocates a fresh container with
// converted elements — aliasing is **not** preserved across the bridge
// for containers. This is acceptable for testing the encoding; the
// real migration changes the inner storage uniformly so the bridge
// goes away.
// ──────────────────────────────────────────────────────────────────────

impl From<&OldValue> for NanValue {
    fn from(v: &OldValue) -> Self {
        match v {
            OldValue::Integer(n) => NanValue::from_i64(*n),
            OldValue::Float(x) => NanValue::from_f64(*x),
            OldValue::Char(c) => NanValue::from_char(*c),
            OldValue::Boolean(b) => NanValue::from_bool(*b),
            OldValue::Byte(b) => NanValue::from_byte(*b),
            OldValue::Uint(n) => NanValue::from_u64(*n),
            OldValue::None => NanValue::none(),
            OldValue::String(s) => NanValue::from_rc_string(Rc::clone(s)),
            OldValue::Error(s) => NanValue::from_error_rc_string(Rc::clone(s)),
            OldValue::Builtin(f) => NanValue::from_builtin(*f),
            OldValue::Closure(c) => NanValue::from_closure(Rc::clone(c)),
            OldValue::StructDef(d) => NanValue::from_struct_def(Rc::clone(d)),
            OldValue::StructInstance(i) => NanValue::from_struct_instance(Rc::clone(i)),
            OldValue::EnumDef(d) => NanValue::from_enum_def(Rc::clone(d)),
            OldValue::EnumInstance(i) => NanValue::from_enum_instance(Rc::clone(i)),
            OldValue::Module(m) => NanValue::from_module(Rc::clone(m)),
            OldValue::Array(arr) => {
                let nans: Vec<NanValue> = arr.borrow().iter().map(NanValue::from).collect();
                NanValue::from_array(Rc::new(RefCell::new(nans)))
            }
            OldValue::Tuple(t) => {
                let nans: Vec<NanValue> = t.iter().map(NanValue::from).collect();
                NanValue::from_tuple(Rc::new(nans))
            }
            OldValue::Map(m) => {
                let nans: Vec<(NanValue, NanValue)> = m
                    .borrow()
                    .iter()
                    .map(|(k, v)| (NanValue::from(k), NanValue::from(v)))
                    .collect();
                NanValue::from_map(Rc::new(RefCell::new(nans)))
            }
            OldValue::Set(s) => {
                let nans: Vec<NanValue> = s.borrow().iter().map(NanValue::from).collect();
                NanValue::from_set(Rc::new(RefCell::new(nans)))
            }
            OldValue::ErrorValue(data) => {
                // ErrorValueStorage uses Rc<str>; ErrorValueData uses
                // Rc<String> (post-A1.1b). Convert each component.
                let storage = ErrorValueStorage {
                    msg: Rc::from(data.msg.as_str()),
                    tag: data.tag.as_ref().map(|t| Rc::from(t.as_str())),
                };
                NanValue::from_error_value(Rc::new(storage))
            }
            OldValue::Wrapped(inner) => NanValue::from_wrapped(NanValue::from(&**inner)),
        }
    }
}

impl NanValue {
    /// Bridge back into the legacy `Value` enum, consuming `self`.
    /// Pointer-bearing kinds transfer their strong-count to the resulting
    /// `Value` (or, for container kinds, decrement and reallocate). The
    /// caller is responsible for ensuring `self` is in a well-formed
    /// state — every constructor in this module produces well-formed
    /// values, so this is just `from_*` round-trip plus `From<&Value>`.
    pub fn into_value(self) -> OldValue {
        let masked = self.raw & TAG_MASK;
        if masked < TAG_SMI {
            // f64 — no Drop needed (no Rc payload).
            let v = OldValue::Float(f64::from_bits(self.raw));
            std::mem::forget(self);
            return v;
        }
        match masked {
            TAG_SMI => {
                let v = OldValue::Integer(self.as_i64().unwrap());
                std::mem::forget(self);
                v
            }
            TAG_SMI_UINT => {
                let v = OldValue::Uint(self.as_u64().unwrap());
                std::mem::forget(self);
                v
            }
            TAG_PRIMITIVE => {
                let kind_byte = (self.raw >> 40) & 0xFF;
                let v = if kind_byte == PrimitiveKind::Bool as u64 {
                    OldValue::Boolean((self.raw & 1) != 0)
                } else if kind_byte == PrimitiveKind::None as u64 {
                    OldValue::None
                } else if kind_byte == PrimitiveKind::Char as u64 {
                    let cp = (self.raw & 0xFFFF_FFFF) as u32;
                    OldValue::Char(char::from_u32(cp).expect("valid char codepoint"))
                } else if kind_byte == PrimitiveKind::Byte as u64 {
                    OldValue::Byte((self.raw & 0xFF) as u8)
                } else {
                    panic!("NanValue: unknown primitive subkind {}", kind_byte)
                };
                std::mem::forget(self);
                v
            }
            TAG_BUILTIN => {
                let addr = (self.raw & PAYLOAD_MASK) as usize;
                // SAFETY: addr was produced by `from_builtin` from a live
                // `BuiltinFn`; the function pointer is reproducible from
                // the same address bits.
                let f: BuiltinFn = unsafe { std::mem::transmute(addr) };
                std::mem::forget(self);
                OldValue::Builtin(f)
            }
            TAG_POINTER_A | TAG_POINTER_B => self.into_value_pointer(),
            other => panic!("NanValue: unknown tag 0x{:x}", other >> 48),
        }
    }

    fn into_value_pointer(self) -> OldValue {
        let masked = self.raw & TAG_MASK;
        let sub = (self.raw & 0x7) as u8;
        let addr = self.decode_pointer_addr();
        // SAFETY: addr/subkind pairs are produced exclusively by the
        // matching `from_*` constructors in this module, so each pointer
        // type is recovered through `Rc::from_raw` of the same `T`.
        let v = unsafe {
            if masked == TAG_POINTER_A {
                if sub == PointerKindA::String as u8 {
                    let rc = Rc::<String>::from_raw(addr as *const String);
                    OldValue::String(rc)
                } else if sub == PointerKindA::Array as u8 {
                    let rc = Rc::<RefCell<Vec<NanValue>>>::from_raw(
                        addr as *const RefCell<Vec<NanValue>>,
                    );
                    let owned: Vec<OldValue> = rc
                        .borrow()
                        .iter()
                        .map(|n| n.clone().into_value())
                        .collect();
                    drop(rc);
                    OldValue::Array(Rc::new(RefCell::new(owned)))
                } else if sub == PointerKindA::Tuple as u8 {
                    let rc =
                        Rc::<Vec<NanValue>>::from_raw(addr as *const Vec<NanValue>);
                    let owned: Vec<OldValue> =
                        rc.iter().map(|n| n.clone().into_value()).collect();
                    drop(rc);
                    OldValue::Tuple(Rc::new(owned))
                } else if sub == PointerKindA::Map as u8 {
                    let rc = Rc::<RefCell<Vec<(NanValue, NanValue)>>>::from_raw(
                        addr as *const RefCell<Vec<(NanValue, NanValue)>>,
                    );
                    let owned: Vec<(OldValue, OldValue)> = rc
                        .borrow()
                        .iter()
                        .map(|(k, v)| (k.clone().into_value(), v.clone().into_value()))
                        .collect();
                    drop(rc);
                    OldValue::Map(Rc::new(RefCell::new(owned)))
                } else if sub == PointerKindA::Set as u8 {
                    let rc = Rc::<RefCell<Vec<NanValue>>>::from_raw(
                        addr as *const RefCell<Vec<NanValue>>,
                    );
                    let owned: Vec<OldValue> = rc
                        .borrow()
                        .iter()
                        .map(|n| n.clone().into_value())
                        .collect();
                    drop(rc);
                    OldValue::Set(Rc::new(RefCell::new(owned)))
                } else if sub == PointerKindA::Closure as u8 {
                    let rc = Rc::<ObjClosure>::from_raw(addr as *const ObjClosure);
                    OldValue::Closure(rc)
                } else if sub == PointerKindA::StructDef as u8 {
                    let rc = Rc::<ObjStructDef>::from_raw(addr as *const ObjStructDef);
                    OldValue::StructDef(rc)
                } else if sub == PointerKindA::StructInstance as u8 {
                    let rc = Rc::<ObjStructInstance>::from_raw(
                        addr as *const ObjStructInstance,
                    );
                    OldValue::StructInstance(rc)
                } else {
                    unreachable_subkind(sub)
                }
            } else {
                debug_assert_eq!(masked, TAG_POINTER_B);
                if sub == PointerKindB::EnumDef as u8 {
                    let rc = Rc::<ObjEnumDef>::from_raw(addr as *const ObjEnumDef);
                    OldValue::EnumDef(rc)
                } else if sub == PointerKindB::EnumInstance as u8 {
                    let rc =
                        Rc::<ObjEnumInstance>::from_raw(addr as *const ObjEnumInstance);
                    OldValue::EnumInstance(rc)
                } else if sub == PointerKindB::Module as u8 {
                    let rc = Rc::<ObjModule>::from_raw(addr as *const ObjModule);
                    OldValue::Module(rc)
                } else if sub == PointerKindB::ErrorValue as u8 {
                    let rc = Rc::<ErrorValueStorage>::from_raw(
                        addr as *const ErrorValueStorage,
                    );
                    let data = ErrorValueData {
                        msg: Rc::new(rc.msg.to_string()),
                        tag: rc.tag.as_ref().map(|t| Rc::new(t.to_string())),
                    };
                    drop(rc);
                    OldValue::ErrorValue(Rc::new(data))
                } else if sub == PointerKindB::Wrapped as u8 {
                    let rc =
                        Rc::<WrappedStorage>::from_raw(addr as *const WrappedStorage);
                    let inner_value = rc.inner.clone().into_value();
                    drop(rc);
                    OldValue::Wrapped(Rc::new(inner_value))
                } else if sub == PointerKindB::Error as u8 {
                    // `from_error*` always boxes a `Rc<String>`.
                    let rc = Rc::<String>::from_raw(addr as *const String);
                    OldValue::Error(rc)
                } else if sub == PointerKindB::BoxedInt as u8 {
                    let rc = Rc::<i64>::from_raw(addr as *const i64);
                    let n = *rc;
                    drop(rc);
                    OldValue::Integer(n)
                } else if sub == PointerKindB::BoxedUint as u8 {
                    let rc = Rc::<u64>::from_raw(addr as *const u64);
                    let n = *rc;
                    drop(rc);
                    OldValue::Uint(n)
                } else {
                    unreachable_subkind(sub)
                }
            }
        };
        std::mem::forget(self);
        v
    }
}

// ──────────────────────────────────────────────────────────────────────
// Tests
// ──────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ── Size / layout pins ────────────────────────────────────────────

    #[test]
    fn nanvalue_is_exactly_eight_bytes() {
        assert_eq!(std::mem::size_of::<NanValue>(), 8);
        assert_eq!(std::mem::align_of::<NanValue>(), 8);
    }

    /// The JIT will eventually emit `iconst(TAG_MASK)` + `band` to test
    /// the top 16 bits, and `iconst(PAYLOAD_MASK)` + `band` to extract
    /// the 48-bit payload. Pin those constants so a future encoding tweak
    /// fails fast at `cargo test`.
    #[test]
    fn tag_and_payload_masks_are_pinned() {
        assert_eq!(TAG_MASK, 0xFFFF_0000_0000_0000);
        assert_eq!(PAYLOAD_MASK, 0x0000_FFFF_FFFF_FFFF);
        // The two masks must partition u64 exactly.
        assert_eq!(TAG_MASK | PAYLOAD_MASK, u64::MAX);
        assert_eq!(TAG_MASK & PAYLOAD_MASK, 0);
    }

    /// JIT IR will branch on `(raw & TAG_MASK) == TAG_X` for each kind.
    /// Pin tag values so the JIT-emitted `icmp.eq` constants stay
    /// in sync with the Rust-side encoding.
    #[test]
    fn tag_constants_are_pinned() {
        assert_eq!(TAG_SMI, 0xFFF8_0000_0000_0000);
        assert_eq!(TAG_SMI_UINT, 0xFFF9_0000_0000_0000);
        assert_eq!(TAG_PRIMITIVE, 0xFFFA_0000_0000_0000);
        assert_eq!(TAG_BUILTIN, 0xFFFB_0000_0000_0000);
        assert_eq!(TAG_POINTER_A, 0xFFFC_0000_0000_0000);
        assert_eq!(TAG_POINTER_B, 0xFFFD_0000_0000_0000);
        // f64 sentinel must sit strictly below the tagged-value range.
        assert!(CANONICAL_F64_NAN_BITS < TAG_SMI);
    }

    /// Pointer-group subkind tags ride in the low 3 bits of the 48-bit
    /// payload. JIT field-access ICs will eventually emit
    /// `iconst(0x7); band(raw, mask)` to extract subkind, then dispatch.
    /// Pin the values here so a refactor of `PointerKindA/B` doesn't
    /// silently shift dispatch.
    #[test]
    fn pointer_subkind_layout_is_pinned() {
        assert_eq!(PointerKindA::String as u8, 0);
        assert_eq!(PointerKindA::Array as u8, 1);
        assert_eq!(PointerKindA::Tuple as u8, 2);
        assert_eq!(PointerKindA::Map as u8, 3);
        assert_eq!(PointerKindA::Set as u8, 4);
        assert_eq!(PointerKindA::Closure as u8, 5);
        assert_eq!(PointerKindA::StructDef as u8, 6);
        assert_eq!(PointerKindA::StructInstance as u8, 7);

        assert_eq!(PointerKindB::EnumDef as u8, 0);
        assert_eq!(PointerKindB::EnumInstance as u8, 1);
        assert_eq!(PointerKindB::Module as u8, 2);
        assert_eq!(PointerKindB::ErrorValue as u8, 3);
        assert_eq!(PointerKindB::Wrapped as u8, 4);
        assert_eq!(PointerKindB::Error as u8, 5);
        assert_eq!(PointerKindB::BoxedInt as u8, 6);
        assert_eq!(PointerKindB::BoxedUint as u8, 7);

        // Subkind index must fit in 3 bits.
        assert!(PointerKindA::StructInstance as u8 <= 0x7);
        assert!(PointerKindB::BoxedUint as u8 <= 0x7);
    }

    /// Pin the byte position of the primitive subkind inside the
    /// payload: byte 5 (= bit 40). The JIT's primitive dispatch will
    /// shift right 40 and band 0xFF to recover the subkind without going
    /// through Rust enum pattern matching.
    #[test]
    fn primitive_subkind_lives_at_bit_40() {
        let v = NanValue::from_char('a');
        let extracted_subkind = ((v.raw_bits() >> 40) & 0xFF) as u8;
        assert_eq!(extracted_subkind, PrimitiveKind::Char as u8);
        let v_byte = NanValue::from_byte(0x42);
        let sk = ((v_byte.raw_bits() >> 40) & 0xFF) as u8;
        assert_eq!(sk, PrimitiveKind::Byte as u8);
    }

    /// SMI sign-extension boundary: -1 must encode with all 48 payload
    /// bits set, since the JIT's eventual SMI inline-decode will do
    /// `(payload << 16) >> 16` (arithmetic shift) to sign-extend.
    #[test]
    fn smi_sign_extension_at_boundary() {
        let neg = NanValue::from_i64(-1);
        // Payload is 48 ones — entire low 48 bits set.
        assert_eq!(neg.raw_bits() & PAYLOAD_MASK, PAYLOAD_MASK);
        assert_eq!(neg.as_i64(), Some(-1));

        let smi_min = NanValue::from_i64(SMI_MIN);
        // SMI_MIN payload sets only bit 47 (the sign bit position
        // within the 48-bit payload). `(payload << 16) >> 16` recovers
        // the original i64.
        let recovered = ((smi_min.raw_bits() & PAYLOAD_MASK) as i64) << 16 >> 16;
        assert_eq!(recovered, SMI_MIN);
    }

    /// Pointer-bearing values store the raw `Rc::into_raw` pointer in the
    /// low 48 bits, with the subkind in bits 0..3. The JIT will load the
    /// pointer via `iconst(!0x7); band(raw, mask)` to reach the typed Rc
    /// payload. Pin that the round-trip is bit-exact for an aligned Rc.
    #[test]
    fn pointer_bits_round_trip_through_payload_mask() {
        use crate::vm::value::{FieldLayout, ObjStructDef, ObjStructInstance};
        let layout = Rc::new(FieldLayout {
            slots: Vec::new(),
            indices: HashMap::new(),
        });
        let def = Rc::new(ObjStructDef {
            name: "Empty".to_string(),
            fields: Vec::new(),
            methods: RefCell::new(HashMap::new()),
            parent: None,
            layout: std::cell::OnceCell::new(),
            module_globals: RefCell::new(None),
        });
        let inst = Rc::new(ObjStructInstance::new(
            "Empty".to_string(),
            Vec::new(),
            layout,
            def,
        ));
        // The NaN-box stores `Rc::into_raw(rc)` — a pointer to T inside
        // the RcBox, not the RcBox header. The eventual JIT loads the
        // payload bits and casts directly to `*const T`, so the pin
        // checks that exact value (via the non-consuming `Rc::as_ptr`).
        let expected_raw = Rc::as_ptr(&inst) as usize;
        // Sanity: alignment lets us repurpose the low 3 bits.
        assert_eq!(expected_raw & 0x7, 0, "Rc::into_raw target must be 8-aligned");

        let nan = NanValue::from_struct_instance(Rc::clone(&inst));
        let extracted = ((nan.raw_bits() & PAYLOAD_MASK) & !0x7) as usize;
        assert_eq!(
            extracted, expected_raw,
            "pointer bits must survive payload-mask + subkind-mask"
        );
        // Tag dispatch picks Pointer A.
        assert_eq!(nan.raw_bits() & TAG_MASK, TAG_POINTER_A);
        let sub = (nan.raw_bits() & 0x7) as u8;
        assert_eq!(sub, PointerKindA::StructInstance as u8);
        drop(nan);
    }

    #[test]
    fn tag_constants_do_not_collide_with_finite_f64() {
        // Every finite f64 has bits < TAG_SMI.
        let samples = [
            0.0_f64,
            -0.0,
            1.0,
            -1.0,
            f64::MIN_POSITIVE,
            f64::MAX,
            f64::MIN,
            f64::INFINITY,
            f64::NEG_INFINITY,
        ];
        for x in samples {
            assert!(
                x.to_bits() < TAG_SMI,
                "finite f64 {} has bits 0x{:x}, would collide with tag range",
                x,
                x.to_bits()
            );
        }
    }

    // ── SMI round-trips ───────────────────────────────────────────────

    #[test]
    fn smi_roundtrips_common_values() {
        for n in [
            0i64,
            1,
            -1,
            42,
            -42,
            i32::MAX as i64,
            i32::MIN as i64,
            SMI_MAX,
            SMI_MIN,
        ] {
            let v = NanValue::from_i64(n);
            assert_eq!(v.as_i64(), Some(n), "round-trip failed for {}", n);
        }
    }

    #[test]
    fn integer_outside_smi_boxes_correctly() {
        for n in [SMI_MAX + 1, SMI_MIN - 1, i64::MAX, i64::MIN] {
            let v = NanValue::from_i64(n);
            assert_eq!(v.as_i64(), Some(n), "boxed i64 round-trip failed for {}", n);
            assert!(v.is_integer());
        }
    }

    #[test]
    fn uint_smi_roundtrips() {
        for n in [0u64, 1, 0xFFFF, 0xFFFF_FFFF, SMI_UINT_MAX] {
            let v = NanValue::from_u64(n);
            assert_eq!(v.as_u64(), Some(n));
        }
    }

    #[test]
    fn uint_outside_smi_boxes() {
        for n in [SMI_UINT_MAX + 1, u64::MAX] {
            let v = NanValue::from_u64(n);
            assert_eq!(v.as_u64(), Some(n));
        }
    }

    // ── Primitives ────────────────────────────────────────────────────

    #[test]
    fn bool_roundtrips() {
        assert_eq!(NanValue::from_bool(true).as_bool(), Some(true));
        assert_eq!(NanValue::from_bool(false).as_bool(), Some(false));
    }

    #[test]
    fn none_is_distinct_from_bool_false() {
        let none = NanValue::none();
        let bool_false = NanValue::from_bool(false);
        assert!(none.is_none());
        assert!(!bool_false.is_none());
        assert_ne!(none.raw_bits(), bool_false.raw_bits());
    }

    #[test]
    fn char_roundtrips_boundary_codepoints() {
        for c in ['\0', 'a', '☃', '\u{10FFFF}'] {
            let v = NanValue::from_char(c);
            assert_eq!(v.as_char(), Some(c));
        }
    }

    #[test]
    fn byte_roundtrips() {
        for b in [0u8, 1, 127, 255] {
            let v = NanValue::from_byte(b);
            assert_eq!(v.as_byte(), Some(b));
        }
    }

    // ── f64 handling ──────────────────────────────────────────────────

    #[test]
    fn f64_roundtrips_normal_values() {
        for x in [0.0, -0.0, 1.0, -1.0, 3.14, f64::INFINITY, f64::NEG_INFINITY] {
            let v = NanValue::from_f64(x);
            let back = v.as_f64().expect("should be f64");
            if x == 0.0 {
                // 0.0 and -0.0 both equal 0.0 under ==; compare bits.
                assert_eq!(back.to_bits(), x.to_bits());
            } else {
                assert_eq!(back, x);
            }
        }
    }

    #[test]
    fn f64_nan_is_canonicalized() {
        // Any NaN bit pattern going in comes out as the canonical NaN.
        let bad_nan = f64::from_bits(0xFFF8_0000_1234_5678);
        assert!(bad_nan.is_nan());
        let v = NanValue::from_f64(bad_nan);
        assert_eq!(v.raw_bits(), CANONICAL_F64_NAN_BITS);
        let back = v.as_f64().unwrap();
        assert!(back.is_nan());
    }

    // ── Pointer Rc semantics ──────────────────────────────────────────

    #[test]
    fn struct_instance_clone_drop_preserves_refcount() {
        use crate::vm::value::{FieldLayout, ObjStructDef, ObjStructInstance};
        let mut indices = HashMap::new();
        indices.insert("x".to_string(), 0);
        let layout = Rc::new(FieldLayout {
            slots: vec![("x".to_string(), "int".to_string(), false)],
            indices,
        });
        let def = Rc::new(ObjStructDef {
            name: "Box".to_string(),
            fields: vec![("x".to_string(), "int".to_string(), false)],
            methods: RefCell::new(HashMap::new()),
            parent: None,
            layout: std::cell::OnceCell::new(),
            module_globals: RefCell::new(None),
        });
        let inst = Rc::new(ObjStructInstance::new(
            "Box".to_string(),
            vec![OldValue::Integer(42)],
            layout,
            def,
        ));
        let before = Rc::strong_count(&inst);

        // Store in a NanValue (bumps to before+1 because from_struct_instance
        // consumes one Rc via into_raw but we still hold the original).
        let v = NanValue::from_struct_instance(Rc::clone(&inst));
        assert_eq!(
            Rc::strong_count(&inst),
            before + 1,
            "NanValue::from_struct_instance should own one strong reference"
        );

        // Clone the NanValue: another strong reference.
        let v2 = v.clone();
        assert_eq!(Rc::strong_count(&inst), before + 2);

        // Drop both NanValues.
        drop(v);
        assert_eq!(Rc::strong_count(&inst), before + 1);
        drop(v2);
        assert_eq!(Rc::strong_count(&inst), before);
    }

    #[test]
    fn string_clone_drop_roundtrip() {
        let v = NanValue::from_rc_string(Rc::new("hello".to_string()));
        let v2 = v.clone();
        // Both values should resolve to pointers of the same heap kind.
        let (group, sub) = v.pointer_kind().unwrap();
        assert_eq!(group, PointerGroup::A);
        assert_eq!(sub, PointerKindA::String as u8);
        let (group2, sub2) = v2.pointer_kind().unwrap();
        assert_eq!(group2, group);
        assert_eq!(sub2, sub);
        // Drop both — must not double-free.
        drop(v);
        drop(v2);
    }

    #[test]
    fn boxed_i64_clone_drop_roundtrip() {
        // Outside SMI range — forces heap boxing.
        let big = SMI_MAX + 1;
        let v = NanValue::from_i64(big);
        let v2 = v.clone();
        assert_eq!(v.as_i64(), Some(big));
        assert_eq!(v2.as_i64(), Some(big));
        drop(v);
        drop(v2); // must not double-free the underlying Rc<i64>
    }

    // ── Tag exclusivity ───────────────────────────────────────────────

    #[test]
    fn tag_dispatch_is_exclusive() {
        // Every constructor should land in exactly one accessor.
        let values: Vec<(NanValue, &str)> = vec![
            (NanValue::from_i64(7), "smi"),
            (NanValue::from_u64(7), "uint"),
            (NanValue::from_bool(true), "bool"),
            (NanValue::none(), "none"),
            (NanValue::from_char('z'), "char"),
            (NanValue::from_byte(0xFE), "byte"),
            (NanValue::from_f64(1.5), "f64"),
        ];
        for (v, name) in &values {
            let accessors = [
                v.as_i64().is_some(),
                v.as_u64().is_some(),
                v.as_bool().is_some(),
                v.is_none(),
                v.as_char().is_some(),
                v.as_byte().is_some(),
                v.as_f64().is_some(),
            ];
            let hits = accessors.iter().filter(|b| **b).count();
            assert_eq!(
                hits, 1,
                "value `{}` matched {} accessors — tag encoding not exclusive",
                name, hits
            );
        }
    }

    // ── Bridge: Value ↔ NanValue (A1.2.1) ─────────────────────────────

    /// Construct a representative corpus covering every `Value` variant.
    /// Used to exercise the bridge round-trip.
    fn build_value_corpus() -> Vec<OldValue> {
        use crate::vm::value::{
            FieldLayout, Function, ObjEnumDef, ObjEnumInstance, ObjModule,
            ObjStructDef, ObjStructInstance, VmEnumPayload, VmEnumVariantDef,
            VmEnumVariantKind, make_upvalue_int_caches,
        };
        use std::cell::Cell;

        // Struct def + instance for the corpus.
        let mut indices = HashMap::new();
        indices.insert("a".to_string(), 0);
        indices.insert("b".to_string(), 1);
        let layout = Rc::new(FieldLayout {
            slots: vec![
                ("a".to_string(), "int".to_string(), false),
                ("b".to_string(), "int".to_string(), false),
            ],
            indices,
        });
        let struct_def = Rc::new(ObjStructDef {
            name: "Pair".to_string(),
            fields: vec![
                ("a".to_string(), "int".to_string(), false),
                ("b".to_string(), "int".to_string(), false),
            ],
            methods: RefCell::new(HashMap::new()),
            parent: None,
            layout: std::cell::OnceCell::new(),
            module_globals: RefCell::new(None),
        });
        let struct_inst = Rc::new(ObjStructInstance::new(
            "Pair".to_string(),
            vec![OldValue::Integer(1), OldValue::Integer(2)],
            Rc::clone(&layout),
            Rc::clone(&struct_def),
        ));

        // Closure (no upvalues).
        let func = Rc::new(Function::new(Some("noop".to_string()), 0));
        let (kinds, values) = make_upvalue_int_caches(0);
        let closure = Rc::new(ObjClosure {
            function: func,
            upvalues: Vec::new(),
            module_globals: RefCell::new(None),
            call_count: Cell::new(0),
            loop_count: Cell::new(0),
            jit_state: Cell::new(0),
            jit_thunk: Cell::new(None),
            specialized_thunk: Cell::new(None),
            specialized_arity: Cell::new(0),
            specialized_kind: Cell::new(0),
            upvalue_int_kinds: kinds,
            upvalue_int_values: values,
        });

        // Enum def + instance.
        let enum_def = Rc::new(ObjEnumDef {
            name: "Color".to_string(),
            variants: vec![
                VmEnumVariantDef {
                    name: "Red".to_string(),
                    kind: VmEnumVariantKind::Unit(None),
                },
                VmEnumVariantDef {
                    name: "Pair".to_string(),
                    kind: VmEnumVariantKind::Tuple(vec!["int".into(), "int".into()]),
                },
            ],
        });
        let enum_inst = Rc::new(ObjEnumInstance {
            enum_name: "Color".to_string(),
            variant_name: "Pair".to_string(),
            payload: VmEnumPayload::Tuple(vec![OldValue::Integer(7), OldValue::Integer(8)]),
        });

        // Module.
        let mut globals = HashMap::new();
        globals.insert("PI".to_string(), OldValue::Float(3.14));
        let module = Rc::new(ObjModule {
            name: "math".to_string(),
            globals: Rc::new(globals),
        });

        vec![
            OldValue::Integer(0),
            OldValue::Integer(42),
            OldValue::Integer(-1),
            OldValue::Integer(i64::MAX),  // forces boxed-int path
            OldValue::Integer(i64::MIN),  // forces boxed-int path
            OldValue::Float(0.0),
            OldValue::Float(-3.14),
            OldValue::Float(f64::INFINITY),
            OldValue::Char('a'),
            OldValue::Char('☃'),
            OldValue::Boolean(true),
            OldValue::Boolean(false),
            OldValue::Byte(0xAB),
            OldValue::Uint(0),
            OldValue::Uint(SMI_UINT_MAX),
            OldValue::Uint(SMI_UINT_MAX + 1), // forces boxed-uint
            OldValue::None,
            OldValue::String(Rc::new("hello".into())),
            OldValue::Error(Rc::new("oops".into())),
            OldValue::Array(Rc::new(RefCell::new(vec![
                OldValue::Integer(1),
                OldValue::Integer(2),
            ]))),
            OldValue::Tuple(Rc::new(vec![
                OldValue::Boolean(true),
                OldValue::Integer(7),
            ])),
            OldValue::Map(Rc::new(RefCell::new(vec![(
                OldValue::String(Rc::new("k".into())),
                OldValue::Integer(1),
            )]))),
            OldValue::Set(Rc::new(RefCell::new(vec![OldValue::Integer(9)]))),
            OldValue::ErrorValue(Rc::new(ErrorValueData {
                msg: Rc::new("bad".into()),
                tag: Some(Rc::new("ERR".into())),
            })),
            OldValue::Wrapped(Rc::new(OldValue::Integer(5))),
            OldValue::StructInstance(struct_inst),
            OldValue::StructDef(struct_def),
            OldValue::EnumDef(enum_def),
            OldValue::EnumInstance(enum_inst),
            OldValue::Module(module),
            OldValue::Closure(closure),
        ]
    }

    /// Round-trip every variant through the bridge: the resulting
    /// `OldValue` must compare equal to the original (where `PartialEq`
    /// is defined). For `Closure` / `StructDef` / `Builtin`, fall back
    /// to identity-pointer compare via `Rc::as_ptr`.
    #[test]
    fn bridge_round_trips_all_variants() {
        for (idx, original) in build_value_corpus().into_iter().enumerate() {
            let label = variant_label(&original);
            let nan = NanValue::from(&original);
            let back = nan.into_value();
            assert!(
                values_equivalent(&original, &back),
                "round-trip mismatch at corpus[{}] ({}): nan_bits=...; \
                 see values_equivalent for which equality the variant uses",
                idx, label,
            );
        }
    }

    fn variant_label(v: &OldValue) -> &'static str {
        match v {
            OldValue::Integer(_) => "Integer",
            OldValue::Float(_) => "Float",
            OldValue::Char(_) => "Char",
            OldValue::Boolean(_) => "Boolean",
            OldValue::Byte(_) => "Byte",
            OldValue::Uint(_) => "Uint",
            OldValue::None => "None",
            OldValue::String(_) => "String",
            OldValue::Array(_) => "Array",
            OldValue::Tuple(_) => "Tuple",
            OldValue::Map(_) => "Map",
            OldValue::Set(_) => "Set",
            OldValue::Closure(_) => "Closure",
            OldValue::Builtin(_) => "Builtin",
            OldValue::StructDef(_) => "StructDef",
            OldValue::StructInstance(_) => "StructInstance",
            OldValue::EnumDef(_) => "EnumDef",
            OldValue::EnumInstance(_) => "EnumInstance",
            OldValue::Module(_) => "Module",
            OldValue::ErrorValue(_) => "ErrorValue",
            OldValue::Wrapped(_) => "Wrapped",
            OldValue::Error(_) => "Error",
        }
    }

    fn values_equivalent(a: &OldValue, b: &OldValue) -> bool {
        match (a, b) {
            // PartialEq covers most variants. Variants below either lack a
            // PartialEq case on the legacy enum or carry function pointers
            // we want to compare by identity.
            (OldValue::Closure(x), OldValue::Closure(y)) => Rc::ptr_eq(x, y),
            (OldValue::StructDef(x), OldValue::StructDef(y)) => Rc::ptr_eq(x, y),
            (OldValue::EnumDef(x), OldValue::EnumDef(y)) => Rc::ptr_eq(x, y),
            (OldValue::Module(x), OldValue::Module(y)) => Rc::ptr_eq(x, y),
            (OldValue::Error(x), OldValue::Error(y)) => x == y,
            (OldValue::Builtin(x), OldValue::Builtin(y)) => {
                (*x as usize) == (*y as usize)
            }
            _ => a == b,
        }
    }

    /// Bridging a pointer-bearing value through the bridge must preserve
    /// the underlying `Rc` strong count (no leak, no double-free).
    #[test]
    fn bridge_preserves_struct_instance_refcount() {
        use crate::vm::value::{FieldLayout, ObjStructDef, ObjStructInstance};
        let mut indices = HashMap::new();
        indices.insert("x".to_string(), 0);
        let layout = Rc::new(FieldLayout {
            slots: vec![("x".to_string(), "int".to_string(), false)],
            indices,
        });
        let def = Rc::new(ObjStructDef {
            name: "Box".to_string(),
            fields: vec![("x".to_string(), "int".to_string(), false)],
            methods: RefCell::new(HashMap::new()),
            parent: None,
            layout: std::cell::OnceCell::new(),
            module_globals: RefCell::new(None),
        });
        let inst = Rc::new(ObjStructInstance::new(
            "Box".to_string(),
            vec![OldValue::Integer(1)],
            layout,
            def,
        ));
        let baseline = Rc::strong_count(&inst);

        let v = OldValue::StructInstance(Rc::clone(&inst));
        assert_eq!(Rc::strong_count(&inst), baseline + 1);

        let nan = NanValue::from(&v);
        // From<&OldValue> clones the Rc — both holders alive.
        assert_eq!(Rc::strong_count(&inst), baseline + 2);

        let v2 = nan.into_value();
        // into_value transfers the Rc from NanValue to the new OldValue.
        // Total live Rcs unchanged from the NanValue state.
        assert_eq!(Rc::strong_count(&inst), baseline + 2);

        drop(v);
        drop(v2);
        assert_eq!(Rc::strong_count(&inst), baseline);
    }

    // ── Refcount conformance for every pointer kind (A1.2.4) ──────────
    //
    // The existing struct_instance_clone_drop_preserves_refcount test
    // pinned StructInstance specifically. These tests extend coverage
    // to every PointerKindA / PointerKindB variant so a future tweak to
    // the dispatch tables in `Clone for NanValue` / `Drop for NanValue`
    // (e.g. adding a kind, reordering subkinds) is caught immediately.

    /// Generic helper: build a `Rc<T>` once, wrap it via `wrap`, then
    /// clone the NanValue and drop both — refcount must round-trip
    /// exactly. Works for every pointer kind whose constructor takes a
    /// single Rc.
    fn check_rc_refcount_round_trip<T>(rc: Rc<T>, wrap: impl Fn(Rc<T>) -> NanValue) {
        let baseline = Rc::strong_count(&rc);
        let v = wrap(Rc::clone(&rc));
        assert_eq!(
            Rc::strong_count(&rc),
            baseline + 1,
            "constructor must bump strong"
        );
        let v2 = v.clone();
        assert_eq!(
            Rc::strong_count(&rc),
            baseline + 2,
            "clone must bump strong"
        );
        drop(v);
        assert_eq!(
            Rc::strong_count(&rc),
            baseline + 1,
            "drop must decrement strong by one"
        );
        drop(v2);
        assert_eq!(
            Rc::strong_count(&rc),
            baseline,
            "final drop must restore strong"
        );
    }

    // PointerKindA — already-covered: StructInstance and String. Add
    // the rest: Array, Tuple, Map, Set, Closure, StructDef.

    #[test]
    fn refcount_array_round_trip() {
        let rc = Rc::new(RefCell::new(vec![NanValue::from_i64(1)]));
        check_rc_refcount_round_trip(rc, NanValue::from_array);
    }

    #[test]
    fn refcount_tuple_round_trip() {
        let rc = Rc::new(vec![NanValue::from_bool(true), NanValue::from_byte(7)]);
        check_rc_refcount_round_trip(rc, NanValue::from_tuple);
    }

    #[test]
    fn refcount_map_round_trip() {
        let rc = Rc::new(RefCell::new(vec![(
            NanValue::from_i64(1),
            NanValue::from_i64(2),
        )]));
        check_rc_refcount_round_trip(rc, NanValue::from_map);
    }

    #[test]
    fn refcount_set_round_trip() {
        let rc = Rc::new(RefCell::new(vec![NanValue::from_i64(42)]));
        check_rc_refcount_round_trip(rc, NanValue::from_set);
    }

    #[test]
    fn refcount_closure_round_trip() {
        use crate::vm::value::{Function, make_upvalue_int_caches};
        use std::cell::Cell;
        let func = Rc::new(Function::new(None, 0));
        let (kinds, values) = make_upvalue_int_caches(0);
        let rc = Rc::new(ObjClosure {
            function: func,
            upvalues: Vec::new(),
            module_globals: RefCell::new(None),
            call_count: Cell::new(0),
            loop_count: Cell::new(0),
            jit_state: Cell::new(0),
            jit_thunk: Cell::new(None),
            specialized_thunk: Cell::new(None),
            specialized_arity: Cell::new(0),
            specialized_kind: Cell::new(0),
            upvalue_int_kinds: kinds,
            upvalue_int_values: values,
        });
        check_rc_refcount_round_trip(rc, NanValue::from_closure);
    }

    #[test]
    fn refcount_struct_def_round_trip() {
        use crate::vm::value::ObjStructDef;
        let rc = Rc::new(ObjStructDef {
            name: "Empty".to_string(),
            fields: Vec::new(),
            methods: RefCell::new(HashMap::new()),
            parent: None,
            layout: std::cell::OnceCell::new(),
            module_globals: RefCell::new(None),
        });
        check_rc_refcount_round_trip(rc, NanValue::from_struct_def);
    }

    // PointerKindB — EnumDef, EnumInstance, Module, ErrorValue, Wrapped
    // (special), Error, BoxedInt, BoxedUint.

    #[test]
    fn refcount_enum_def_round_trip() {
        use crate::vm::value::ObjEnumDef;
        let rc = Rc::new(ObjEnumDef {
            name: "T".to_string(),
            variants: Vec::new(),
        });
        check_rc_refcount_round_trip(rc, NanValue::from_enum_def);
    }

    #[test]
    fn refcount_enum_instance_round_trip() {
        use crate::vm::value::{ObjEnumInstance, VmEnumPayload};
        let rc = Rc::new(ObjEnumInstance {
            enum_name: "E".to_string(),
            variant_name: "V".to_string(),
            payload: VmEnumPayload::Unit(None),
        });
        check_rc_refcount_round_trip(rc, NanValue::from_enum_instance);
    }

    #[test]
    fn refcount_module_round_trip() {
        use crate::vm::value::ObjModule;
        let rc = Rc::new(ObjModule {
            name: "m".to_string(),
            globals: Rc::new(HashMap::new()),
        });
        check_rc_refcount_round_trip(rc, NanValue::from_module);
    }

    #[test]
    fn refcount_error_value_round_trip() {
        let rc = Rc::new(ErrorValueStorage {
            msg: Rc::from("oops"),
            tag: None,
        });
        check_rc_refcount_round_trip(rc, NanValue::from_error_value);
    }

    #[test]
    fn refcount_error_round_trip() {
        let rc: Rc<String> = Rc::new("err".to_string());
        check_rc_refcount_round_trip(rc, NanValue::from_error_rc_string);
    }

    #[test]
    fn refcount_boxed_i64_round_trip() {
        // The boxed-int path is reached implicitly via from_i64 when the
        // value is outside SMI range — there's no `Rc<i64>` constructor.
        // We can still verify clone/drop preserve count by going through
        // the public API.
        let big = SMI_MAX + 1;
        let v = NanValue::from_i64(big);
        // The boxed Rc<i64> is hidden inside; we can't observe its count
        // directly, but clone+drop must not panic and the value must
        // round-trip.
        let v2 = v.clone();
        assert_eq!(v.as_i64(), Some(big));
        assert_eq!(v2.as_i64(), Some(big));
        drop(v);
        drop(v2); // must not double-free the underlying Rc<i64>
    }

    #[test]
    fn refcount_boxed_u64_round_trip() {
        let big = SMI_UINT_MAX + 1;
        let v = NanValue::from_u64(big);
        let v2 = v.clone();
        assert_eq!(v.as_u64(), Some(big));
        assert_eq!(v2.as_u64(), Some(big));
        drop(v);
        drop(v2);
    }

    /// Wrapped is special because its constructor takes a `NanValue`,
    /// not an `Rc<_>`. Verify the inner NanValue's refcount is preserved
    /// through a Wrapped clone/drop cycle.
    #[test]
    fn refcount_wrapped_preserves_inner_refcount() {
        use crate::vm::value::ObjModule;
        let inner_rc = Rc::new(ObjModule {
            name: "inner".to_string(),
            globals: Rc::new(HashMap::new()),
        });
        let baseline = Rc::strong_count(&inner_rc);

        let inner_nan = NanValue::from_module(Rc::clone(&inner_rc));
        assert_eq!(Rc::strong_count(&inner_rc), baseline + 1);

        let wrapped = NanValue::from_wrapped(inner_nan);
        // wrapped owns inner_nan via Rc<WrappedStorage>; inner Module
        // refcount stays at +1 (the from_module strong didn't move).
        assert_eq!(Rc::strong_count(&inner_rc), baseline + 1);

        let wrapped2 = wrapped.clone();
        // Cloning Wrapped bumps Rc<WrappedStorage> only — the inner
        // Module's strong is unchanged because the WrappedStorage holds
        // it once.
        assert_eq!(Rc::strong_count(&inner_rc), baseline + 1);

        drop(wrapped);
        assert_eq!(Rc::strong_count(&inner_rc), baseline + 1);

        drop(wrapped2);
        // Last Wrapped drop frees the WrappedStorage, which drops its
        // inner NanValue, which decrements the Module strong.
        assert_eq!(Rc::strong_count(&inner_rc), baseline);
    }

    #[test]
    fn bridge_preserves_array_contents_with_cycle_free_recursion() {
        let original = OldValue::Array(Rc::new(RefCell::new(vec![
            OldValue::Tuple(Rc::new(vec![
                OldValue::Integer(1),
                OldValue::Integer(2),
            ])),
            OldValue::Boolean(true),
            OldValue::String(Rc::new("nested".into())),
        ])));
        let nan = NanValue::from(&original);
        let back = nan.into_value();
        assert_eq!(original, back);
    }
}
