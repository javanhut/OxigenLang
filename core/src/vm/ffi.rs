//! Calling C from scripts (`introduce ffi`), through libffi.
//!
//! Functions are declared with C signatures (`"double pow(double, double)"`)
//! and come back as natives the script calls like any function. Values cross
//! by C type: numbers, `char*` as strings, structs as maps, and `T*` as an
//! address or, for an array or map argument, a temporary buffer copied back
//! after the call (out parameters). See `docs/stdlib/ffi.md`.

use crate::vm::value::{NativeFunction, Value, rc_str};
use std::cell::Cell;
use std::collections::HashMap;

thread_local! {
    /// The VM running the current native call, for callbacks to re-enter.
    /// Set by `VM::call_value` around every native.
    pub(crate) static CURRENT_VM: Cell<*mut crate::vm::VM> = const { Cell::new(std::ptr::null_mut()) };
}

pub(crate) fn register(globals: &mut HashMap<String, Value>) {
    for (name, f) in imp::BUILTINS {
        globals.insert(name.to_string(), Value::Builtin(*f));
    }
}

#[cfg(all(not(target_arch = "wasm32"), any(unix, windows)))]
mod imp {
    use super::*;
    use crate::vm::collections::OxMap;
    use crate::vm::value::{ObjModule, ValueRepr};
    use libffi::middle::{Arg, Cif, Closure, CodePtr, Ret, Type, ffi_abi_FFI_DEFAULT_ABI};
    use std::cell::RefCell;
    use std::ffi::{CStr, CString, c_char, c_void};
    use std::rc::Rc;

    pub(super) const BUILTINS: &[(&str, crate::vm::value::BuiltinFn)] = &[
        ("__ffi_open", builtin_open),
        ("__ffi_fn", builtin_fn),
        ("__ffi_struct", builtin_struct),
        ("__ffi_callback", builtin_callback),
        ("__ffi_sym", builtin_sym),
        ("__ffi_alloc", builtin_alloc),
        ("__ffi_free", builtin_free),
        ("__ffi_sizeof", builtin_sizeof),
        ("__ffi_read", builtin_read),
        ("__ffi_write", builtin_write),
        ("__ffi_string", builtin_string),
        ("__ffi_cstring", builtin_cstring),
    ];

    // ── C types ─────────────────────────────────────────────────────────

    #[derive(Clone)]
    enum CType {
        Void,
        Bool,
        I8,
        U8,
        I16,
        U16,
        I32,
        U32,
        I64,
        U64,
        F32,
        F64,
        /// `char*`: a string in, a string out.
        Str,
        /// `T*`; `None` for `void*` and function pointers.
        Ptr(Option<Rc<CType>>),
        Struct(Rc<StructDef>),
    }

    struct StructDef {
        name: String,
        fields: Vec<(String, CType, usize)>, // name, type, offset
        size: usize,
        align: usize,
        ffi: Type,
    }

    impl CType {
        fn ffi(&self) -> Type {
            match self {
                CType::Void => Type::void(),
                CType::Bool | CType::U8 => Type::u8(),
                CType::I8 => Type::i8(),
                CType::I16 => Type::i16(),
                CType::U16 => Type::u16(),
                CType::I32 => Type::i32(),
                CType::U32 => Type::u32(),
                CType::I64 => Type::i64(),
                CType::U64 => Type::u64(),
                CType::F32 => Type::f32(),
                CType::F64 => Type::f64(),
                CType::Str | CType::Ptr(_) => Type::pointer(),
                CType::Struct(s) => s.ffi.clone(),
            }
        }

        fn size(&self) -> usize {
            match self {
                CType::Void => 0,
                CType::Bool | CType::I8 | CType::U8 => 1,
                CType::I16 | CType::U16 => 2,
                CType::I32 | CType::U32 | CType::F32 => 4,
                CType::I64 | CType::U64 | CType::F64 => 8,
                CType::Str | CType::Ptr(_) => size_of::<usize>(),
                CType::Struct(s) => s.size,
            }
        }

        fn align(&self) -> usize {
            match self {
                CType::Struct(s) => s.align,
                t => t.size().max(1),
            }
        }

        fn is_int(&self) -> bool {
            !matches!(self, CType::Void | CType::F32 | CType::F64 | CType::Struct(_))
        }
    }

    thread_local! {
        static STRUCTS: RefCell<HashMap<String, Rc<StructDef>>> = RefCell::new(HashMap::new());
        static LIBS: RefCell<HashMap<String, usize>> = RefCell::new(HashMap::new());
        // ponytail: callbacks live until the thread exits; add ffi.free_callback if a
        // script makes them in a loop.
        static CALLBACKS: RefCell<Vec<Closure<'static>>> = const { RefCell::new(Vec::new()) };
        /// An error a callback hit, reported when the C call it ran under returns.
        static PENDING: RefCell<Option<String>> = const { RefCell::new(None) };
    }

    /// `long` is 4 bytes on Windows, 8 on 64-bit Unix.
    fn c_long(unsigned: bool) -> CType {
        match (size_of::<std::ffi::c_long>(), unsigned) {
            (8, false) => CType::I64,
            (8, true) => CType::U64,
            (_, false) => CType::I32,
            (_, true) => CType::U32,
        }
    }

    /// One C type: `int`, `unsigned long`, `const char *`, `Vec2*`, `u8`, ...
    fn parse_type(src: &str) -> Result<CType, String> {
        let t = src.trim();
        let stars = t.matches('*').count();
        let words: Vec<&str> = t
            .trim_end_matches(|c: char| c == '*' || c.is_whitespace())
            .split_whitespace()
            .filter(|w| !matches!(*w, "const" | "volatile" | "struct" | "union"))
            .collect();
        if stars > 0 && words.join(" ") == "char" {
            let mut ty = CType::Str;
            for _ in 1..stars {
                ty = CType::Ptr(Some(Rc::new(ty)));
            }
            return Ok(ty);
        }
        let base = match words.join(" ").as_str() {
            "void" => CType::Void,
            "bool" | "_Bool" => CType::Bool,
            "char" | "signed char" | "i8" | "int8_t" => CType::I8,
            "unsigned char" | "u8" | "uint8_t" | "byte" => CType::U8,
            "short" | "short int" | "signed short" | "i16" | "int16_t" => CType::I16,
            "unsigned short" | "unsigned short int" | "u16" | "uint16_t" => CType::U16,
            "int" | "signed" | "signed int" | "i32" | "int32_t" => CType::I32,
            "unsigned" | "unsigned int" | "u32" | "uint32_t" => CType::U32,
            "long" | "long int" | "signed long" => c_long(false),
            "unsigned long" | "unsigned long int" => c_long(true),
            "long long" | "long long int" | "signed long long" | "i64" | "int64_t"
            | "ssize_t" | "intptr_t" | "ptrdiff_t" | "off_t" => CType::I64,
            "unsigned long long" | "unsigned long long int" | "u64" | "uint64_t" | "size_t"
            | "uintptr_t" => CType::U64,
            "float" | "f32" => CType::F32,
            "double" | "f64" => CType::F64,
            "str" | "string" => CType::Str,
            "ptr" | "pointer" => CType::Ptr(None),
            w if w.starts_with("enum ") || w == "enum" => CType::I32,
            name => match STRUCTS.with(|s| s.borrow().get(name).cloned()) {
                Some(def) => CType::Struct(def),
                None => {
                    return Err(format!(
                        "unknown C type '{}'{}",
                        src.trim(),
                        if stars > 0 { "" } else { " (declare structs with ffi.cstruct first)" }
                    ));
                }
            },
        };
        let base = if t.starts_with("enum ") { CType::I32 } else { base };
        Ok(match stars {
            0 => base,
            _ => {
                let mut ty = match base {
                    CType::Void => CType::Ptr(None),
                    b => CType::Ptr(Some(Rc::new(b))),
                };
                for _ in 1..stars {
                    ty = CType::Ptr(Some(Rc::new(ty)));
                }
                ty
            }
        })
    }

    /// A type, optionally followed by a name: `const char *path`, `int`.
    fn parse_decl(src: &str) -> Result<(CType, Option<String>), String> {
        if let Ok(t) = parse_type(src) {
            return Ok((t, None));
        }
        let src = src.trim();
        let cut = src
            .rfind(|c: char| !(c.is_alphanumeric() || c == '_'))
            .map_or(0, |i| i + 1);
        let (ty, name) = src.split_at(cut);
        if name.is_empty() || ty.trim().is_empty() {
            return parse_type(src).map(|t| (t, None));
        }
        Ok((parse_type(ty)?, Some(name.to_string())))
    }

    struct Sig {
        name: Option<String>,
        ret: CType,
        args: Vec<CType>,
        variadic: bool,
    }

    /// `double pow(double, double)`, `int(const char*, ...)`.
    fn parse_sig(src: &str) -> Result<Sig, String> {
        let bad = || format!("bad C signature '{src}': expected like \"int abs(int)\"");
        let open = src.find('(').ok_or_else(bad)?;
        let close = src.rfind(')').filter(|&c| c > open).ok_or_else(bad)?;
        let (ret, name) = parse_decl(&src[..open])?;
        let params = src[open + 1..close].trim();
        let mut args = Vec::new();
        let mut variadic = false;
        if !params.is_empty() && params != "void" {
            for p in params.split(',') {
                if variadic {
                    return Err(format!("bad C signature '{src}': '...' must come last"));
                }
                match p.trim() {
                    "..." => variadic = true,
                    p => args.push(parse_decl(p)?.0),
                }
            }
        }
        if args.iter().any(|a| matches!(a, CType::Void)) {
            return Err(format!("bad C signature '{src}': a parameter cannot be void"));
        }
        Ok(Sig { name, ret, args, variadic })
    }

    fn sig_label(sig: &Sig) -> &str {
        sig.name.as_deref().unwrap_or("C function")
    }

    // ── Values <-> C memory ─────────────────────────────────────────────

    fn int_of(v: &Value) -> Option<i64> {
        match v.repr() {
            ValueRepr::Integer(n) => Some(n),
            ValueRepr::Uint(n) => Some(n as i64),
            ValueRepr::Byte(n) => Some(n as i64),
            ValueRepr::Boolean(b) => Some(b as i64),
            ValueRepr::Char(c) => Some(c as i64),
            ValueRepr::None => Some(0),
            _ => None,
        }
    }

    fn str_of(v: &Value) -> Option<String> {
        match v.repr() {
            ValueRepr::String(s) => Some(s.to_string()),
            _ => None,
        }
    }

    /// What to copy back into a script array or map once the call returns.
    struct WriteBack {
        target: Value,
        elem: CType,
        buf: Vec<u64>,
    }

    /// Memory that lives for one call: string copies and out-parameter buffers.
    #[derive(Default)]
    struct Temps {
        strings: Vec<CString>,
        back: Vec<WriteBack>,
    }

    fn zeroed(size: usize) -> Vec<u64> {
        vec![0u64; size.div_ceil(8).max(1)]
    }

    fn cstring(s: String, temps: &mut Option<&mut Temps>) -> Result<i64, String> {
        let Some(temps) = temps else {
            return Err("a string here needs memory that outlives the call; use ffi.cstring".into());
        };
        let c = CString::new(s).map_err(|_| "string contains a NUL byte")?;
        let p = c.as_ptr() as i64;
        temps.strings.push(c);
        Ok(p)
    }

    /// Write `v` as a `ty` at `dst`, which is zeroed and `ty.size()` long.
    unsafe fn write_value(ty: &CType, v: &Value, dst: *mut u8, temps: &mut Option<&mut Temps>) -> Result<(), String> {
        let expected = || format!("expected a value for {}", type_name(ty));
        unsafe {
            match ty {
                CType::Void => {}
                CType::F32 | CType::F64 => {
                    let x = match v.repr() {
                        ValueRepr::Float(x) => x,
                        ValueRepr::Integer(n) => n as f64,
                        _ => return Err(format!("expected a float, got {}", v.type_name())),
                    };
                    if matches!(ty, CType::F32) {
                        (dst as *mut f32).write_unaligned(x as f32)
                    } else {
                        (dst as *mut f64).write_unaligned(x)
                    }
                }
                CType::Struct(def) => match v.repr() {
                    ValueRepr::Map(m) => {
                        let m = m.borrow();
                        for (name, fty, off) in &def.fields {
                            if let Some(fv) = m.get(&Value::String(rc_str(name.as_str()))) {
                                write_value(fty, fv, dst.add(*off), temps)
                                    .map_err(|e| format!("{}.{name}: {e}", def.name))?;
                            }
                        }
                    }
                    ValueRepr::Array(a) => write_fields(def, &a.borrow(), dst, temps)?,
                    ValueRepr::Tuple(t) => write_fields(def, t, dst, temps)?,
                    _ => return Err(format!("expected a map for struct {}, got {}", def.name, v.type_name())),
                },
                CType::Str | CType::Ptr(_) => {
                    let p = match (ty, v.repr()) {
                        (_, ValueRepr::String(s)) => cstring(s.to_string(), temps)?,
                        // An array or map for `T*`: point at a copy, copied back after the call.
                        (CType::Ptr(Some(elem)), ValueRepr::Array(_) | ValueRepr::Map(_)) => {
                            let Some(temps) = temps.as_deref_mut() else {
                                return Err("an array or map here needs a pointer from ffi.alloc".into());
                            };
                            let items: Vec<Value> = match v.repr() {
                                ValueRepr::Array(a) => a.borrow().iter().cloned().collect(),
                                _ => vec![v.clone()],
                            };
                            let mut buf = zeroed(elem.size() * items.len().max(1));
                            let base = buf.as_mut_ptr() as *mut u8;
                            let mut inner = Some(&mut *temps);
                            for (i, item) in items.iter().enumerate() {
                                write_value(elem, item, base.add(i * elem.size()), &mut inner)
                                    .map_err(|e| format!("element {i}: {e}"))?;
                            }
                            let p = base as i64;
                            temps.back.push(WriteBack { target: v.clone(), elem: (**elem).clone(), buf });
                            p
                        }
                        (CType::Ptr(None), ValueRepr::Array(_)) => {
                            return Err("to pass an array, declare the element type (int *, not void *)".into());
                        }
                        _ => int_of(v).ok_or_else(expected)?,
                    };
                    (dst as *mut usize).write_unaligned(p as usize)
                }
                _ => {
                    let n = int_of(v).ok_or_else(|| format!("expected an int, got {}", v.type_name()))?;
                    match ty.size() {
                        1 => dst.write(n as u8),
                        2 => (dst as *mut u16).write_unaligned(n as u16),
                        4 => (dst as *mut u32).write_unaligned(n as u32),
                        _ => (dst as *mut i64).write_unaligned(n),
                    }
                }
            }
        }
        Ok(())
    }

    unsafe fn write_fields(def: &StructDef, vals: &[Value], dst: *mut u8, temps: &mut Option<&mut Temps>) -> Result<(), String> {
        if vals.len() > def.fields.len() {
            return Err(format!("struct {} has {} fields, got {}", def.name, def.fields.len(), vals.len()));
        }
        for ((name, fty, off), v) in def.fields.iter().zip(vals) {
            unsafe { write_value(fty, v, dst.add(*off), temps) }.map_err(|e| format!("{}.{name}: {e}", def.name))?;
        }
        Ok(())
    }

    unsafe fn read_value(ty: &CType, src: *const u8) -> Value {
        unsafe {
            match ty {
                CType::Void => Value::None,
                CType::Bool => Value::Boolean(src.read() != 0),
                CType::I8 => Value::Integer(src.read() as i8 as i64),
                CType::U8 => Value::Integer(src.read() as i64),
                CType::I16 => Value::Integer((src as *const i16).read_unaligned() as i64),
                CType::U16 => Value::Integer((src as *const u16).read_unaligned() as i64),
                CType::I32 => Value::Integer((src as *const i32).read_unaligned() as i64),
                CType::U32 => Value::Integer((src as *const u32).read_unaligned() as i64),
                CType::I64 => Value::Integer((src as *const i64).read_unaligned()),
                CType::U64 => Value::Uint((src as *const u64).read_unaligned()),
                CType::F32 => Value::Float((src as *const f32).read_unaligned() as f64),
                CType::F64 => Value::Float((src as *const f64).read_unaligned()),
                CType::Str => c_str_value((src as *const usize).read_unaligned()),
                CType::Ptr(_) => match (src as *const usize).read_unaligned() {
                    0 => Value::None,
                    p => Value::Integer(p as i64),
                },
                CType::Struct(def) => {
                    let pairs = def
                        .fields
                        .iter()
                        .map(|(name, fty, off)| (Value::String(rc_str(name.as_str())), read_value(fty, src.add(*off))))
                        .collect();
                    Value::Map(Rc::new(RefCell::new(OxMap::from_pairs(pairs))))
                }
            }
        }
    }

    fn c_str_value(p: usize) -> Value {
        if p == 0 {
            return Value::None;
        }
        let s = unsafe { CStr::from_ptr(p as *const c_char) };
        Value::String(rc_str(s.to_string_lossy()))
    }

    fn type_name(ty: &CType) -> String {
        match ty {
            CType::Struct(d) => d.name.clone(),
            CType::Str => "char*".into(),
            CType::Ptr(_) => "a pointer".into(),
            _ => "this C type".into(),
        }
    }

    /// The C type a variadic argument goes as, after C's default promotions.
    fn vararg_type(v: &Value) -> Result<CType, String> {
        Ok(match v.repr() {
            ValueRepr::Float(_) => CType::F64,
            ValueRepr::String(_) => CType::Str,
            ValueRepr::Uint(_) => CType::U64,
            _ if int_of(v).is_some() => CType::I64,
            _ => return Err(format!("cannot pass {} as a variadic argument", v.type_name())),
        })
    }

    // ── Calls ───────────────────────────────────────────────────────────

    fn cif(args: &[CType], fixed: usize, variadic: bool, ret: &CType) -> Cif {
        let types = args.iter().map(CType::ffi).collect::<Vec<_>>();
        if variadic {
            Cif::new_variadic(types, fixed, ret.ffi())
        } else {
            Cif::new(types, ret.ffi())
        }
    }

    fn call(addr: usize, sig: &Sig, cached: Option<&Cif>, args: &[Value]) -> Result<Value, String> {
        let fixed = sig.args.len();
        if args.len() < fixed || (!sig.variadic && args.len() > fixed) {
            return Err(format!(
                "{} takes {}{} argument{}, got {}",
                sig_label(sig),
                if sig.variadic { "at least " } else { "" },
                fixed,
                if fixed == 1 { "" } else { "s" },
                args.len()
            ));
        }
        let mut types = sig.args.clone();
        for a in &args[fixed..] {
            types.push(vararg_type(a)?);
        }
        let built;
        let cif = match cached {
            Some(c) => c,
            None => {
                built = cif(&types, fixed, sig.variadic, &sig.ret);
                &built
            }
        };

        let mut temps = Temps::default();
        let mut bufs = Vec::with_capacity(args.len());
        for (i, (ty, v)) in types.iter().zip(args).enumerate() {
            let mut buf = zeroed(ty.size());
            unsafe { write_value(ty, v, buf.as_mut_ptr() as *mut u8, &mut Some(&mut temps)) }
                .map_err(|e| format!("{} argument {}: {e}", sig_label(sig), i + 1))?;
            bufs.push(buf);
        }
        // From here on `args` is not touched: a callback may grow the VM
        // stack it points into.
        let argv: Vec<Arg> = bufs.iter().map(|b| Arg::new(&b[0])).collect();
        // libffi widens small integer returns to a full register.
        let mut ret = zeroed(sig.ret.size().max(16));
        PENDING.with(|p| p.borrow_mut().take());
        unsafe { cif.call_return_into(CodePtr(addr as *mut c_void), &argv, Ret::new(&mut ret[0])) };
        if let Some(e) = PENDING.with(|p| p.borrow_mut().take()) {
            return Err(e);
        }

        for wb in temps.back {
            let base = wb.buf.as_ptr() as *const u8;
            match wb.target.repr() {
                ValueRepr::Array(a) => {
                    for (i, slot) in a.borrow_mut().iter_mut().enumerate() {
                        *slot = unsafe { read_value(&wb.elem, base.add(i * wb.elem.size())) };
                    }
                }
                ValueRepr::Map(m) => {
                    if let ValueRepr::Map(fresh) = unsafe { read_value(&wb.elem, base) }.repr() {
                        let mut m = m.borrow_mut();
                        for (k, v) in fresh.borrow().iter() {
                            m.insert(k.clone(), v.clone());
                        }
                    }
                }
                _ => {}
            }
        }
        Ok(unsafe { read_value(&sig.ret, ret.as_ptr() as *const u8) })
    }

    fn native(addr: usize, sig: Sig) -> Value {
        let cached = (!sig.variadic).then(|| cif(&sig.args, sig.args.len(), false, &sig.ret));
        let name = sig.name.clone().unwrap_or_else(|| "ffi function".into());
        Value::Native(Rc::new(NativeFunction {
            name,
            func: Box::new(move |args| call(addr, &sig, cached.as_ref(), args)),
        }))
    }

    // ── Callbacks ───────────────────────────────────────────────────────

    struct CallbackData {
        sig: Sig,
        func: Value,
    }

    unsafe extern "C" fn trampoline(
        _cif: &libffi::low::ffi_cif,
        result: &mut u64,
        args: *const *const c_void,
        data: &CallbackData,
    ) {
        let out = result as *mut u64 as *mut u8;
        let sig = &data.sig;
        let ran = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| -> Result<(), String> {
            let vals = (0..sig.args.len())
                .map(|i| unsafe { read_value(&sig.args[i], *args.add(i) as *const u8) })
                .collect();
            let vm = CURRENT_VM.get();
            if vm.is_null() {
                return Err("ffi callback called outside a call from Oxigen (another thread?)".into());
            }
            let v = unsafe { (*vm).call_reentrant(data.func.clone(), vals) }.map_err(|e| e.message)?;
            write_return(&sig.ret, &v, out).map_err(|e| format!("ffi callback returned a bad value: {e}"))
        }));
        let failed = match ran {
            Ok(Ok(())) => return,
            Ok(Err(e)) => e,
            Err(_) => "ffi callback panicked".to_string(),
        };
        unsafe { std::ptr::write_bytes(out, 0, if sig.ret.is_int() { 8 } else { sig.ret.size() }) };
        // The first failure is the one worth reporting.
        PENDING.with(|p| {
            p.borrow_mut().get_or_insert(failed);
        });
    }

    /// Integers narrower than a register go back widened, as libffi requires.
    fn write_return(ty: &CType, v: &Value, out: *mut u8) -> Result<(), String> {
        match ty {
            CType::Void => Ok(()),
            CType::Str => Err("callbacks cannot return char*; return a pointer from ffi.cstring as void*".into()),
            t if t.is_int() => {
                let mut tmp = [0u64; 1];
                unsafe { write_value(t, v, tmp.as_mut_ptr() as *mut u8, &mut None)? };
                let n = match unsafe { read_value(t, tmp.as_ptr() as *const u8) }.repr() {
                    ValueRepr::Integer(n) => n,
                    ValueRepr::Uint(n) => n as i64,
                    ValueRepr::Boolean(b) => b as i64,
                    _ => 0,
                };
                unsafe { (out as *mut i64).write_unaligned(n) };
                Ok(())
            }
            t => unsafe { write_value(t, v, out, &mut None) },
        }
    }

    // ── Libraries ───────────────────────────────────────────────────────

    #[cfg(unix)]
    mod os {
        use std::ffi::{CStr, c_void};
        pub const PROCESS: &str = "";

        fn last_error() -> String {
            let e = unsafe { libc::dlerror() };
            if e.is_null() { "unknown error".into() } else { unsafe { CStr::from_ptr(e) }.to_string_lossy().into_owned() }
        }

        pub fn open(path: Option<&CStr>) -> Result<*mut c_void, String> {
            let h = unsafe { libc::dlopen(path.map_or(std::ptr::null(), |p| p.as_ptr()), libc::RTLD_NOW | libc::RTLD_LOCAL) };
            if h.is_null() { Err(last_error()) } else { Ok(h) }
        }

        pub fn sym(lib: *mut c_void, name: &CStr) -> Result<*mut c_void, String> {
            unsafe { libc::dlerror() };
            let p = unsafe { libc::dlsym(lib, name.as_ptr()) };
            if p.is_null() { Err(last_error()) } else { Ok(p) }
        }

        pub fn candidates(name: &str) -> Vec<String> {
            if cfg!(target_os = "macos") {
                vec![
                    format!("lib{name}.dylib"),
                    format!("/opt/homebrew/lib/lib{name}.dylib"),
                    format!("/usr/local/lib/lib{name}.dylib"),
                    format!("{name}.framework/{name}"),
                ]
            } else {
                // `libm.so` is often a linker script dlopen cannot load; the
                // versioned names are the real files.
                ["", ".6", ".1", ".0", ".2", ".3", ".4", ".5"].iter().map(|v| format!("lib{name}.so{v}")).collect()
            }
        }
    }

    // ponytail: untested on Windows (no Windows machine in CI here).
    #[cfg(windows)]
    mod os {
        use std::ffi::{CStr, c_char, c_void};
        /// No single module exports the C runtime the way the main program
        /// does on Unix; `None` means the universal CRT.
        pub const PROCESS: &str = "ucrtbase.dll";

        unsafe extern "system" {
            fn LoadLibraryA(name: *const c_char) -> *mut c_void;
            fn GetProcAddress(lib: *mut c_void, name: *const c_char) -> *mut c_void;
            fn GetLastError() -> u32;
        }

        pub fn open(path: Option<&CStr>) -> Result<*mut c_void, String> {
            let path = path.unwrap_or(c"ucrtbase.dll");
            let h = unsafe { LoadLibraryA(path.as_ptr()) };
            if h.is_null() { Err(format!("LoadLibrary failed (error {})", unsafe { GetLastError() })) } else { Ok(h) }
        }

        pub fn sym(lib: *mut c_void, name: &CStr) -> Result<*mut c_void, String> {
            let p = unsafe { GetProcAddress(lib, name.as_ptr()) };
            if p.is_null() { Err(format!("symbol {} not found", name.to_string_lossy())) } else { Ok(p) }
        }

        pub fn candidates(name: &str) -> Vec<String> {
            vec![format!("{name}.dll"), format!("lib{name}.dll")]
        }
    }

    /// `None`, a bare name (`"m"`, `"sqlite3"`), a file name or a path.
    fn library(v: Option<&Value>) -> Result<usize, String> {
        let name = match v.map(|v| v.repr()) {
            None | Some(ValueRepr::None) => os::PROCESS.to_string(),
            Some(ValueRepr::String(s)) => s.to_string(),
            Some(ValueRepr::Integer(h)) => return Ok(h as usize),
            Some(_) => return Err("a library is a name, a path, or None for the C runtime".into()),
        };
        if let Some(h) = LIBS.with(|l| l.borrow().get(&name).copied()) {
            return Ok(h);
        }
        let plain = !name.is_empty()
            && !name.contains(['/', '\\'])
            && !name.contains(".so")
            && !name.ends_with(".dylib")
            && !name.ends_with(".dll");
        let tries = if plain { os::candidates(&name) } else { vec![name.clone()] };
        let mut first_err = None;
        for t in &tries {
            let c = CString::new(t.as_str()).map_err(|_| "library name contains a NUL byte")?;
            let path = (!t.is_empty()).then_some(c.as_c_str());
            match os::open(path) {
                Ok(h) => {
                    LIBS.with(|l| l.borrow_mut().insert(name, h as usize));
                    return Ok(h as usize);
                }
                Err(e) => {
                    first_err.get_or_insert(e);
                }
            }
        }
        Err(match (plain, first_err) {
            (true, _) => format!("cannot find library '{name}' (tried {})", tries.join(", ")),
            (false, Some(e)) => e,
            (false, None) => format!("cannot open '{name}'"),
        })
    }

    fn symbol(lib: usize, name: &str) -> Result<usize, String> {
        let c = CString::new(name).map_err(|_| "symbol name contains a NUL byte")?;
        os::sym(lib as *mut c_void, &c).map(|p| p as usize)
    }

    // ── Builtins ────────────────────────────────────────────────────────

    fn wrap(r: Result<Value, String>, what: &str) -> Value {
        r.unwrap_or_else(|e| Value::Error(rc_str(format!("ffi.{what}: {e}"))))
    }

    fn sig_arg(v: Option<&Value>) -> Result<Sig, String> {
        parse_sig(&v.and_then(str_of).ok_or("expected a C signature string")?)
    }

    /// `__ffi_open(lib, {name: "signature", ...})` -> a module of functions.
    fn builtin_open(args: &[Value]) -> Value {
        wrap(
            (|| {
                let lib = library(args.first())?;
                let label = args.first().and_then(str_of).unwrap_or_else(|| "c".into());
                let mut globals = HashMap::new();
                match args.get(1).map(|v| v.repr()) {
                    None | Some(ValueRepr::None) => {}
                    Some(ValueRepr::Map(m)) => {
                        for (k, v) in m.borrow().iter() {
                            let key = str_of(k).ok_or("function names must be strings")?;
                            let sig = str_of(v).ok_or_else(|| format!("{key}: expected a C signature string"))?;
                            let mut sig = parse_sig(&sig).map_err(|e| format!("{key}: {e}"))?;
                            let sym = sig.name.clone().unwrap_or_else(|| key.clone());
                            let addr = symbol(lib, &sym).map_err(|e| format!("{key}: {e}"))?;
                            sig.name.get_or_insert(key.clone());
                            globals.insert(key, native(addr, sig));
                        }
                    }
                    Some(_) => return Err("declarations must be a map of name to C signature".into()),
                }
                Ok(Value::Module(Rc::new(ObjModule {
                    name: label,
                    globals: Rc::new(globals),
                    hidden: Default::default(),
                })))
            })(),
            "open",
        )
    }

    /// `__ffi_fn(lib_or_address, "double cos(double)")`
    fn builtin_fn(args: &[Value]) -> Value {
        wrap(
            (|| {
                let sig = sig_arg(args.get(1))?;
                let addr = match args.first().map(|v| v.repr()) {
                    Some(ValueRepr::Integer(p)) if p != 0 => p as usize,
                    _ => {
                        let name = sig.name.clone().ok_or("the signature needs a function name, like \"int abs(int)\"")?;
                        symbol(library(args.first())?, &name)?
                    }
                };
                Ok(native(addr, sig))
            })(),
            "fn",
        )
    }

    /// `__ffi_struct("Vec2", "double x; double y")` -> "Vec2". The fields may
    /// also be an array of declarations, or the whole declaration one string
    /// pasted from a header (with its braces escaped, `\{`).
    fn builtin_struct(args: &[Value]) -> Value {
        wrap(
            (|| {
                let first = args.first().and_then(str_of).ok_or("expected (name, fields)")?;
                let (name, body) = match args.get(1).map(|v| v.repr()) {
                    Some(ValueRepr::String(f)) => (first.trim().to_string(), f.to_string()),
                    Some(ValueRepr::Array(a)) => {
                        let fields: Option<Vec<String>> = a.borrow().iter().map(str_of).collect();
                        (first.trim().to_string(), fields.ok_or("fields must be strings")?.join(";"))
                    }
                    Some(ValueRepr::None) | None => {
                        let bad = || format!("expected (name, \"double x; double y\"), got '{first}'");
                        let (open, close) = (first.find('{').ok_or_else(bad)?, first.rfind('}').ok_or_else(bad)?);
                        let ident = |s: &str| {
                            s.split_whitespace()
                                .filter(|w| !matches!(*w, "typedef" | "struct"))
                                .last()
                                .map(|w| w.trim_end_matches(';').to_string())
                                .filter(|w| !w.is_empty())
                        };
                        // `struct Name { ... }` or `typedef struct { ... } Name;`
                        let name = ident(&first[..open]).or_else(|| ident(&first[close + 1..])).ok_or_else(bad)?;
                        (name, first[open + 1..close].to_string())
                    }
                    Some(_) => return Err("fields must be a string or an array of strings".into()),
                };
                let src = &body;
                let mut fields = Vec::new();
                for f in src.split([';', ',']).map(str::trim).filter(|f| !f.is_empty()) {
                    let (ty, fname) = parse_decl(f)?;
                    let fname = fname.ok_or_else(|| format!("field '{f}' needs a name"))?;
                    if matches!(ty, CType::Void) {
                        return Err(format!("field '{fname}' cannot be void"));
                    }
                    // ponytail: no inline arrays (`char name[16]`); add an Array CType when a real header needs one.
                    fields.push((fname, ty));
                }
                if fields.is_empty() {
                    return Err(format!("struct {name} has no fields"));
                }
                let mut ffi = Type::structure(fields.iter().map(|(_, t)| t.ffi()).collect::<Vec<_>>());
                let offsets = ffi.struct_offsets(ffi_abi_FFI_DEFAULT_ABI).map_err(|e| format!("{e:?}"))?;
                let raw = unsafe { &*ffi.as_raw_ptr() };
                let def = StructDef {
                    name: name.clone(),
                    fields: fields.into_iter().zip(offsets).map(|((n, t), o)| (n, t, o)).collect(),
                    size: raw.size,
                    align: raw.alignment as usize,
                    ffi,
                };
                STRUCTS.with(|s| s.borrow_mut().insert(name.clone(), Rc::new(def)));
                Ok(Value::String(rc_str(name)))
            })(),
            "cstruct",
        )
    }

    /// `__ffi_callback("int(const void*, const void*)", fun)` -> function pointer
    fn builtin_callback(args: &[Value]) -> Value {
        wrap(
            (|| {
                let sig = sig_arg(args.first())?;
                if sig.variadic {
                    return Err("a callback cannot be variadic".into());
                }
                let func = match args.get(1).map(|v| v.repr()) {
                    Some(ValueRepr::Closure(_) | ValueRepr::Native(_) | ValueRepr::Builtin(_)) => args[1].clone(),
                    _ => return Err("expected a function to call".into()),
                };
                let cif = cif(&sig.args, sig.args.len(), false, &sig.ret);
                let data: &'static CallbackData = Box::leak(Box::new(CallbackData { sig, func }));
                let closure = Closure::new(cif, trampoline, data);
                let p = *closure.code_ptr() as usize;
                CALLBACKS.with(|c| c.borrow_mut().push(closure));
                Ok(Value::Integer(p as i64))
            })(),
            "callback",
        )
    }

    fn builtin_sym(args: &[Value]) -> Value {
        wrap(
            (|| {
                let name = args.get(1).and_then(str_of).ok_or("expected (lib, name)")?;
                Ok(Value::Integer(symbol(library(args.first())?, &name)? as i64))
            })(),
            "sym",
        )
    }

    unsafe extern "C" {
        fn calloc(n: usize, size: usize) -> *mut c_void;
        fn free(p: *mut c_void);
    }

    fn builtin_alloc(args: &[Value]) -> Value {
        match args.first().and_then(int_of) {
            Some(n) if n > 0 => match unsafe { calloc(1, n as usize) } {
                p if p.is_null() => Value::Error(rc_str("ffi.alloc: out of memory")),
                p => Value::Integer(p as i64),
            },
            _ => Value::Error(rc_str("ffi.alloc takes a positive size in bytes")),
        }
    }

    fn builtin_free(args: &[Value]) -> Value {
        match args.first().and_then(int_of) {
            Some(p) => {
                unsafe { free(p as *mut c_void) };
                Value::None
            }
            None => Value::Error(rc_str("ffi.free takes a pointer")),
        }
    }

    fn type_arg(v: Option<&Value>) -> Result<CType, String> {
        parse_type(&v.and_then(str_of).ok_or("expected a C type name")?)
    }

    fn builtin_sizeof(args: &[Value]) -> Value {
        wrap(type_arg(args.first()).map(|t| Value::Integer(t.size() as i64)), "sizeof")
    }

    /// The address of element `index` of a `ty` array at `ptr`.
    fn element(args: &[Value], index_at: usize) -> Result<(*mut u8, CType), String> {
        let p = args.first().and_then(int_of).filter(|&p| p != 0).ok_or("expected a non-null pointer")?;
        let ty = type_arg(args.get(1))?;
        let i = match args.get(index_at) {
            None => 0,
            Some(v) => int_of(v).ok_or("index must be an int")?,
        };
        let stride = ty.size().next_multiple_of(ty.align()) as i64;
        Ok(((p + i * stride) as *mut u8, ty))
    }

    /// `__ffi_read(ptr, type, index?)`
    fn builtin_read(args: &[Value]) -> Value {
        wrap(element(args, 2).map(|(p, ty)| unsafe { read_value(&ty, p) }), "read")
    }

    /// `__ffi_write(ptr, type, value, index?)`
    fn builtin_write(args: &[Value]) -> Value {
        wrap(
            (|| {
                let (p, ty) = element(args, 3)?;
                let v = args.get(2).ok_or("expected a value to write")?;
                // Write into a zeroed copy first, so a bad field leaves memory untouched.
                let mut buf = zeroed(ty.size());
                unsafe {
                    write_value(&ty, v, buf.as_mut_ptr() as *mut u8, &mut None)?;
                    std::ptr::copy_nonoverlapping(buf.as_ptr() as *const u8, p, ty.size());
                }
                Ok(Value::None)
            })(),
            "write",
        )
    }

    /// `__ffi_string(ptr, len?)`: a NUL-terminated string, or `len` bytes.
    fn builtin_string(args: &[Value]) -> Value {
        let len = args.get(1).filter(|v| !matches!(v.repr(), ValueRepr::None));
        match (args.first().and_then(int_of), len.map(int_of)) {
            (Some(0), _) => Value::None,
            (Some(p), Some(Some(n))) if n >= 0 => {
                let bytes = unsafe { std::slice::from_raw_parts(p as *const u8, n as usize) };
                Value::String(rc_str(String::from_utf8_lossy(bytes)))
            }
            (Some(p), None) => c_str_value(p as usize),
            _ => Value::Error(rc_str("ffi.string takes (ptr, len?)")),
        }
    }

    /// `__ffi_cstring(s)`: a C copy of `s` that lives until `ffi.free`.
    fn builtin_cstring(args: &[Value]) -> Value {
        let Some(s) = args.first().and_then(str_of) else {
            return Value::Error(rc_str("ffi.cstring takes a string"));
        };
        let Ok(c) = CString::new(s) else {
            return Value::Error(rc_str("ffi.cstring: string contains a NUL byte"));
        };
        let bytes = c.as_bytes_with_nul();
        let p = unsafe { calloc(1, bytes.len()) } as *mut u8;
        if p.is_null() {
            return Value::Error(rc_str("ffi.cstring: out of memory"));
        }
        unsafe { std::ptr::copy_nonoverlapping(bytes.as_ptr(), p, bytes.len()) };
        Value::Integer(p as i64)
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn parses_c_signatures() {
            let s = parse_sig("double pow(double x, double y)").unwrap();
            assert_eq!(s.name.as_deref(), Some("pow"));
            assert!(matches!(s.ret, CType::F64) && s.args.len() == 2 && !s.variadic);

            let s = parse_sig("int(const char *fmt, ...)").unwrap();
            assert!(s.name.is_none() && matches!(s.args[..], [CType::Str]) && s.variadic);

            let s = parse_sig("unsigned long long f(void)").unwrap();
            assert!(matches!(s.ret, CType::U64) && s.args.is_empty());

            let s = parse_sig("void qsort(void*, size_t, size_t, void*)").unwrap();
            assert!(matches!(s.args[..], [CType::Ptr(None), CType::U64, CType::U64, CType::Ptr(None)]));

            assert!(matches!(parse_type("int *").unwrap(), CType::Ptr(Some(t)) if matches!(*t, CType::I32)));
            assert!(matches!(parse_type("char **").unwrap(), CType::Ptr(Some(t)) if matches!(*t, CType::Str)));
            assert!(parse_sig("int f(int, ..., int)").is_err());
            assert!(parse_type("Nope").is_err());
        }
    }
}

#[cfg(not(all(not(target_arch = "wasm32"), any(unix, windows))))]
mod imp {
    use super::*;
    fn unsupported(_: &[Value]) -> Value {
        Value::Error(rc_str("ffi is not supported on this platform"))
    }
    pub(super) const BUILTINS: &[(&str, crate::vm::value::BuiltinFn)] = &[
        ("__ffi_open", unsupported),
        ("__ffi_fn", unsupported),
        ("__ffi_struct", unsupported),
        ("__ffi_callback", unsupported),
        ("__ffi_sym", unsupported),
        ("__ffi_alloc", unsupported),
        ("__ffi_free", unsupported),
        ("__ffi_sizeof", unsupported),
        ("__ffi_read", unsupported),
        ("__ffi_write", unsupported),
        ("__ffi_string", unsupported),
        ("__ffi_cstring", unsupported),
    ];
}
