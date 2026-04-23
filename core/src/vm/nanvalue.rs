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
    BuiltinFn, ObjClosure, ObjEnumDef, ObjEnumInstance, ObjModule, ObjStructDef,
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
                    s if s == PointerKindA::Array as u8 => clone_and_forget::<RefCell<Vec<NanValue>>>(addr),
                    s if s == PointerKindA::Tuple as u8 => clone_and_forget::<Vec<NanValue>>(addr),
                    s if s == PointerKindA::Map as u8 => {
                        clone_and_forget::<RefCell<Vec<(NanValue, NanValue)>>>(addr)
                    }
                    s if s == PointerKindA::Set as u8 => clone_and_forget::<RefCell<Vec<NanValue>>>(addr),
                    s if s == PointerKindA::Closure as u8 => clone_and_forget::<ObjClosure>(addr),
                    s if s == PointerKindA::StructDef as u8 => clone_and_forget::<ObjStructDef>(addr),
                    s if s == PointerKindA::StructInstance as u8 => {
                        clone_and_forget::<ObjStructInstance>(addr)
                    }
                    _ => unreachable_subkind(sub),
                },
                m if m == TAG_POINTER_B => match sub {
                    s if s == PointerKindB::EnumDef as u8 => clone_and_forget::<ObjEnumDef>(addr),
                    s if s == PointerKindB::EnumInstance as u8 => clone_and_forget::<ObjEnumInstance>(addr),
                    s if s == PointerKindB::Module as u8 => clone_and_forget::<ObjModule>(addr),
                    s if s == PointerKindB::ErrorValue as u8 => clone_and_forget::<ErrorValueStorage>(addr),
                    s if s == PointerKindB::Wrapped as u8 => clone_and_forget::<WrappedStorage>(addr),
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
                    s if s == PointerKindA::Map as u8 => drop_rc::<RefCell<Vec<(NanValue, NanValue)>>>(addr),
                    s if s == PointerKindA::Set as u8 => drop_rc::<RefCell<Vec<NanValue>>>(addr),
                    s if s == PointerKindA::Closure as u8 => drop_rc::<ObjClosure>(addr),
                    s if s == PointerKindA::StructDef as u8 => drop_rc::<ObjStructDef>(addr),
                    s if s == PointerKindA::StructInstance as u8 => drop_rc::<ObjStructInstance>(addr),
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
        for n in [0i64, 1, -1, 42, -42, i32::MAX as i64, i32::MIN as i64, SMI_MAX, SMI_MIN] {
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
}
