pub mod builtins;
pub mod capabilities;
pub mod collections;
pub mod ffi;
pub mod intern;
pub mod limits;
pub mod nanvalue;
pub mod value;
pub mod vector;

use crate::compiler::opcode::OpCode;
use crate::jit::JitEngine;
use crate::vm::value::*;
pub use capabilities::Capability;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::rc::Rc;

pub(crate) const STACK_MAX: usize = 262144;
pub(crate) const FRAMES_MAX: usize = 16384;
/// Stack slots every call keeps free. A function whose own
/// [`Chunk::stack_bound`](crate::compiler::opcode::Chunk::stack_bound) is
/// larger reserves that much instead (see [`Function::stack_reserve`]).
pub(crate) const STACK_GUARD_MARGIN: usize = 8192;

/// Type names the runtime itself uses. A user struct or enum may not take one.
pub(crate) const BUILTIN_TYPE_NAMES: &[&str] = &[
    "INTEGER",
    "FLOAT",
    "CHAR",
    "STRING",
    "BOOLEAN",
    "ARRAY",
    "FUNCTION",
    "STRUCT_DEF",
    "STRUCT",
    "BYTE",
    "UINT",
    "TUPLE",
    "MAP",
    "SET",
    "MODULE",
    "BUILTIN",
    "NONE",
    "ERROR",
    "VALUE",
    "ENUM_DEF",
    "ENUM",
    "TASK",
    "HOST",
    "GENERIC",
];

pub(crate) fn is_builtin_type_name(name: &str) -> bool {
    BUILTIN_TYPE_NAMES.contains(&name)
}

/// Checks if `actual` satisfies the type constraint `expected`.
/// Handles GENERIC (accepts anything) and union types like "INTEGER || STRING".
pub(crate) fn type_matches(expected: &str, actual: &str) -> bool {
    if expected == "GENERIC" {
        return true;
    }
    if expected == "ERROR" && actual.starts_with("ERROR") {
        return true;
    }
    if expected.contains(" || ") {
        expected
            .split(" || ")
            .any(|t| t == "GENERIC" || t == actual || (t == "ERROR" && actual.starts_with("ERROR")))
    } else {
        expected == actual
    }
}

/// The common type for arithmetic on two different kinds of number, by the
/// same rule `==` uses: `byte` or `uint` with `int` is `int`, `byte` with
/// `uint` is `uint`, anything with `float` is `float`, and `byte` with `byte`
/// is `int`. `None` for any other pair, including two numbers that already
/// share a type.
fn promote_numeric(a: &Value, b: &Value) -> Option<(Value, Value)> {
    use Value::{Byte, Float, Integer, Uint};
    fn to_float(v: &Value) -> Option<f64> {
        match v {
            Integer(n) => Some(*n as f64),
            Uint(n) => Some(*n as f64),
            Byte(n) => Some(f64::from(*n)),
            Float(x) => Some(*x),
            _ => None,
        }
    }
    fn to_int(v: &Value) -> Option<i64> {
        match v {
            Integer(n) => Some(*n),
            Uint(n) => Some(*n as i64),
            Byte(n) => Some(i64::from(*n)),
            _ => None,
        }
    }
    match (a, b) {
        (Integer(_), Integer(_)) | (Uint(_), Uint(_)) | (Float(_), Float(_)) => None,
        (Float(_), _) | (_, Float(_)) => Some((Float(to_float(a)?), Float(to_float(b)?))),
        (Byte(x), Uint(y)) => Some((Uint(u64::from(*x)), Uint(*y))),
        (Uint(x), Byte(y)) => Some((Uint(*x), Uint(u64::from(*y)))),
        _ => Some((Integer(to_int(a)?), Integer(to_int(b)?))),
    }
}

/// How `a` and `b` order, for `<`, `<=`, `>` and `>=`. `None` when they
/// cannot be compared; `Some(None)` when they can but have no order (a NaN).
///
/// Integers of any width (`int`, `uint`, `byte`) compare by exact value.
/// Anything with a float compares as floats. Strings compare with strings and
/// chars with chars.
fn order(a: &Value, b: &Value) -> Option<Option<Ordering>> {
    fn integer(v: &Value) -> Option<i128> {
        match v {
            Value::Integer(n) => Some(i128::from(*n)),
            Value::Uint(n) => Some(i128::from(*n)),
            Value::Byte(n) => Some(i128::from(*n)),
            _ => None,
        }
    }
    fn number(v: &Value) -> Option<f64> {
        match v {
            Value::Float(x) => Some(*x),
            other => integer(other).map(|n| n as f64),
        }
    }
    match (a, b) {
        (Value::String(l), Value::String(r)) => Some(Some(l.cmp(r))),
        (Value::Char(l), Value::Char(r)) => Some(Some(l.cmp(r))),
        _ => match (integer(a), integer(b)) {
            (Some(l), Some(r)) => Some(Some(l.cmp(&r))),
            _ => Some(number(a)?.partial_cmp(&number(b)?)),
        },
    }
}

/// How a file is named in a diagnostic.
///
/// Paths under the working directory `cwd` are shown relative to it. Anything
/// outside stays absolute.
fn display_path(path: &Path, cwd: Option<&Path>) -> String {
    cwd.and_then(|c| path.strip_prefix(c).ok())
        .unwrap_or(path)
        .display()
        .to_string()
}

/// `display_path` against the real working directory.
type ModuleSource = Rc<dyn Fn(&str) -> Option<String>>;

fn display_path_cwd(path: &Path) -> String {
    display_path(path, std::env::current_dir().ok().as_deref())
}

/// Locate the stdlib directory: next to the executable (dev or system install),
/// then the cwd, then a user install under `~/.oxigen`.
pub(crate) fn find_stdlib_path() -> PathBuf {
    if let Ok(exe) = std::env::current_exe()
        && let Some(exe_dir) = exe.parent()
    {
        let candidate = exe_dir.join("stdlib");
        if candidate.is_dir() {
            return candidate;
        }
        if let Some(prefix) = exe_dir.parent() {
            let candidate = prefix.join("lib").join("oxigen").join("stdlib");
            if candidate.is_dir() {
                return candidate;
            }
        }
    }
    let candidate = PathBuf::from("stdlib");
    if candidate.is_dir() {
        return candidate;
    }
    let candidate = PathBuf::from("..").join("stdlib");
    if candidate.is_dir() {
        return candidate;
    }
    if let Ok(home) = std::env::var("HOME") {
        let candidate = PathBuf::from(home)
            .join(".oxigen")
            .join("lib")
            .join("stdlib");
        if candidate.is_dir() {
            return candidate;
        }
    }
    PathBuf::from("stdlib")
}

/// A call frame: one per active function invocation.
#[derive(Debug)]
struct CallFrame {
    closure: Rc<ObjClosure>,
    ip: usize,
    slot_offset: usize,
    /// When executing a module function, holds the module's globals
    /// so `GetGlobal` can resolve module-scoped variables (e.g. `io` in `toml`).
    module_globals: Option<Rc<HashMap<String, Value>>>,
}

impl CallFrame {
    fn read_byte(&mut self) -> u8 {
        let byte = self.closure.function.chunk.code[self.ip];
        self.ip += 1;
        byte
    }

    fn read_u16(&mut self) -> u16 {
        let hi = self.closure.function.chunk.code[self.ip] as u16;
        let lo = self.closure.function.chunk.code[self.ip + 1] as u16;
        self.ip += 2;
        (hi << 8) | lo
    }

    fn read_constant(&self, idx: u16) -> &Value {
        &self.closure.function.chunk.constants[idx as usize]
    }
}

/// Recover an owned `Rc` from a `JitFrame`'s module-globals raw pointer.
/// Sound only for pointers produced by `Rc::as_ptr` on an `Rc` that is still
/// alive at the call site.
unsafe fn mg_rc(ptr: *const HashMap<String, Value>) -> Option<Rc<HashMap<String, Value>>> {
    if ptr.is_null() {
        return None;
    }
    unsafe {
        Rc::increment_strong_count(ptr);
        Some(Rc::from_raw(ptr))
    }
}

/// Opaque handle returned by `VM::frames_pop` to the JIT runtime, used to
/// complete a Return.
#[allow(dead_code)]
pub(crate) struct CallFrameHandle {
    pub(crate) slot_offset: usize,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub(crate) struct JitFrame {
    pub(crate) closure_raw: *const ObjClosure,
    pub(crate) slot_offset: usize,
    pub(crate) module_globals: *const HashMap<String, Value>,
    pub(crate) line: u32,
    pub(crate) column: u32,
}

#[allow(dead_code)]
impl JitFrame {
    pub(crate) const OFFSET_CLOSURE_RAW: i32 = 0;
    pub(crate) const OFFSET_SLOT_OFFSET: i32 = 8;
    pub(crate) const OFFSET_MODULE_GLOBALS: i32 = 16;
    pub(crate) const OFFSET_LINE: i32 = 24;
    pub(crate) const OFFSET_COLUMN: i32 = 28;
}

#[repr(C)]
pub(crate) struct JitFrameView {
    pub ptr: *mut JitFrame,
    pub len: usize,
}

#[cfg_attr(not(feature = "jit"), allow(dead_code))]
impl JitFrameView {
    pub const OFFSET_PTR: i32 = 0;
    pub const OFFSET_LEN: i32 = 8;
}

/// Runtime error with source information.
#[derive(Debug)]
pub struct VMError {
    pub message: String,
    pub line: u32,
    /// Stable identity for this failure.
    pub code: crate::diagnostics::Code,
    /// Error category tag, if any (e.g. from `<fail>(<Error<network>>(...))`).
    /// Preserved across a `guard`/`option <Error>` recovery.
    pub tag: Option<String>,
}

impl VMError {
    /// A loop budget or interrupt error: the host stopping the script,
    /// which no handler in the script may catch.
    pub fn is_host_limit(&self) -> bool {
        use crate::diagnostics::registry::{INTERRUPTED, LOOP_BUDGET_EXHAUSTED};
        self.code == INTERRUPTED || self.code == LOOP_BUDGET_EXHAUSTED
    }
}

impl std::fmt::Display for VMError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

/// Cached view of the VM stack's `(ptr, len)`, readable directly from
/// JIT-compiled code. `len` is synced by every stack mutator.
///
/// The layout is `#[repr(C)]`; field order matches the `OFFSET_*` constants.
#[repr(C)]
pub struct StackView {
    pub ptr: *mut Value,
    pub len: usize,
}

impl StackView {
    pub const OFFSET_PTR: i32 = 0;
    pub const OFFSET_LEN: i32 = 8;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExecutionMode {
    Interpreter,
    TieredJit,
    EagerJit,
}

pub struct VM {
    /// Cached view of `stack`'s raw parts for the JIT.
    pub(crate) stack_view: StackView,
    pub(crate) jit_frame_view: JitFrameView,

    /// True when a JIT thunk is the innermost executing frame, as opposed to a
    /// nested `execute_until`. Decides whether the active frame lives in
    /// `jit_frame_view` (JIT running) or `frames` (interpreter running).
    pub(crate) jit_executing: std::cell::Cell<bool>,

    stack: Vec<Value>,
    #[allow(dead_code)]
    jit_frames: Vec<JitFrame>,
    frames: Vec<CallFrame>,
    globals: HashMap<String, Value>,
    open_upvalues: Vec<Rc<RefCell<Upvalue>>>,

    pub current_file: Option<PathBuf>,
    pub stdlib_path: PathBuf,
    /// Supplies module sources ahead of the filesystem; see `set_module_source`.
    module_source: Option<ModuleSource>,
    module_cache: HashMap<PathBuf, Rc<ObjModule>>,
    import_stack: Vec<PathBuf>,
    pub is_main_context: bool,

    source: String,

    script_args: Vec<String>,

    global_mutability: HashMap<String, bool>,
    global_type_constraints: HashMap<String, Option<String>>,
    /// Globals declared with `lock`: never reassigned or re-declared.
    locked_globals: std::collections::HashSet<String>,

    /// Active error handlers (innermost last). `PushHandler`/`PopHandler` manage
    /// this; on a runtime error the interpreter loop unwinds to the top handler
    /// whose frame is still within the current `execute_until` scope.
    error_handlers: Vec<ErrorHandler>,

    /// Baseline JIT engine. Stub/no-op when the `jit` feature is off.
    pub jit: JitEngine,
    execution_mode: ExecutionMode,

    /// `interrupt`'s armed byte (`limits::Limits::armed_ptr`), cached where
    /// JIT-compiled back-edges can load it at `VM::limits_armed_offset`.
    pub(crate) limits_armed: *const std::sync::atomic::AtomicU8,
    /// Loop iterations the budget still allows; meaningful only while a
    /// budget is set. JIT-compiled loops read and decrement it in place, at
    /// `VM::loop_countdown_offset`.
    pub(crate) loop_countdown: i64,
    interrupt: limits::InterruptHandle,

    /// Host-registered natives, registered again in each child VM so
    /// imported modules can call them.
    natives: Vec<(String, Value)>,
    /// What scripts may reach outside the VM; `None` is everything.
    capabilities: Option<Vec<Capability>>,
    /// For each cached module, every file it imported, directly or through
    /// other modules, so invalidating a file also drops what was built on it.
    module_deps: HashMap<PathBuf, std::collections::HashSet<PathBuf>>,
    /// Every module this VM's own code imported (cache hits included); a
    /// module's `module_deps` entry is built from its sub-VM's set.
    imported_here: std::collections::HashSet<PathBuf>,
}

/// A recoverable-error landing pad installed by `PushHandler`. Records the
/// machine state to restore so an error raised in the protected region becomes
/// an in-band error value at the catch target.
#[derive(Clone, Copy)]
struct ErrorHandler {
    /// Absolute ip in the catching frame to resume at.
    catch_ip: usize,
    /// `self.frames.len()` at install time — frames to unwind to.
    frame_len: usize,
    /// Operand-stack height at install time — truncate to this, then push error.
    stack_len: usize,
    /// `jit_frame_view.len` at install time — reset to clean leaked JIT frames.
    jit_frame_len: usize,
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}

impl VM {
    pub fn new() -> Self {
        Self::new_with_mode(ExecutionMode::TieredJit)
    }

    pub fn new_interpreter() -> Self {
        Self::new_with_mode(ExecutionMode::Interpreter)
    }

    pub fn new_tiered() -> Self {
        Self::new_with_mode(ExecutionMode::TieredJit)
    }

    pub fn new_eager_jit() -> Self {
        Self::new_with_mode(ExecutionMode::EagerJit)
    }

    pub fn new_with_mode(mode: ExecutionMode) -> Self {
        let mut globals = HashMap::new();
        builtins::register_builtins(&mut globals);

        let mut stack: Vec<Value> = Vec::with_capacity(STACK_MAX);
        let mut jit_frames: Vec<JitFrame> = Vec::with_capacity(FRAMES_MAX);
        let stack_view = StackView {
            ptr: stack.as_mut_ptr(),
            len: 0,
        };
        let jit_frame_view = JitFrameView {
            ptr: jit_frames.as_mut_ptr(),
            len: 0,
        };

        let mut vm = VM {
            stack_view,
            jit_frame_view,
            jit_executing: std::cell::Cell::new(false),
            stack,
            jit_frames,
            frames: Vec::with_capacity(64),
            globals,
            open_upvalues: Vec::new(),
            current_file: None,
            stdlib_path: find_stdlib_path(),
            module_source: None,
            module_cache: HashMap::new(),
            import_stack: Vec::new(),
            is_main_context: true,
            source: String::new(),
            script_args: Vec::new(),
            global_mutability: HashMap::new(),
            global_type_constraints: HashMap::new(),
            locked_globals: std::collections::HashSet::new(),
            error_handlers: Vec::new(),
            jit: JitEngine::new(),
            execution_mode: mode,
            limits_armed: std::ptr::null(),
            loop_countdown: 0,
            interrupt: limits::InterruptHandle::default(),
            natives: Vec::new(),
            capabilities: None,
            module_deps: HashMap::new(),
            imported_here: std::collections::HashSet::new(),
        };
        vm.limits_armed = vm.interrupt.0.armed_ptr();
        vm.jit.set_limits_armed(vm.limits_armed as usize);
        match mode {
            ExecutionMode::Interpreter => vm.jit.disable(),
            ExecutionMode::TieredJit => {}
            ExecutionMode::EagerJit => {
                vm.jit.set_threshold(1);
                vm.jit.set_loop_threshold(1);
            }
        }
        vm
    }

    pub fn execution_mode(&self) -> ExecutionMode {
        self.execution_mode
    }

    /// Runtime-computed offset of `stack_view` within `VM`.
    #[cfg_attr(not(feature = "jit"), allow(dead_code))]
    pub const fn stack_view_offset() -> usize {
        std::mem::offset_of!(VM, stack_view)
    }

    /// Byte offset of `limits_armed`, the pointer JIT-compiled loops check.
    #[cfg_attr(not(feature = "jit"), allow(dead_code))]
    pub const fn limits_armed_offset() -> usize {
        std::mem::offset_of!(VM, limits_armed)
    }

    /// Byte offset of `loop_countdown`, which JIT-compiled loops decrement in place.
    #[cfg_attr(not(feature = "jit"), allow(dead_code))]
    pub const fn loop_countdown_offset() -> usize {
        std::mem::offset_of!(VM, loop_countdown)
    }

    #[cfg_attr(not(feature = "jit"), allow(dead_code))]
    pub const fn jit_frame_view_offset() -> usize {
        std::mem::offset_of!(VM, jit_frame_view)
    }

    /// Re-sync `self.stack`'s `Vec::len` from the JIT-visible `stack_view.len`.
    /// Rust code that reads `self.stack` via Vec's API must call this first.
    ///
    /// # Safety invariant
    ///
    /// JIT inline paths may only decrement `stack_view.len`, and only for stack
    /// slots that hold primitive-tag values (tags 0..=6: Integer, Float, Char,
    /// Boolean, Byte, Uint, None) with no `Drop` side-effect.
    #[cfg_attr(not(feature = "jit"), allow(dead_code))]
    #[inline(always)]
    pub(crate) fn sync_stack_from_view(&mut self) {
        unsafe {
            self.stack.set_len(self.stack_view.len);
        }
    }

    pub fn set_source(&mut self, source: &str) {
        self.source = source.to_string();
    }

    pub fn set_file(&mut self, path: PathBuf) {
        self.current_file = Some(path);
    }

    pub fn set_script_args(&mut self, args: &[String]) {
        self.script_args = args.to_vec();
        let args_val: Vec<Value> = args
            .iter()
            .map(|a| Value::String(rc_str(a.as_str())))
            .collect();
        self.globals.insert(
            "__args".to_string(),
            Value::Array(Rc::new(RefCell::new(args_val.into()))),
        );
    }

    fn maybe_compile_current_loop_hot_function(&mut self, frame_idx: usize) {
        let closure = Rc::clone(&self.frames[frame_idx].closure);
        let loop_count = closure.loop_count.get().saturating_add(1);
        closure.loop_count.set(loop_count);

        if closure.jit_state.get() != 0 || loop_count < self.jit.loop_threshold() {
            return;
        }

        if let Some(thunk) = self
            .jit
            .maybe_compile_loop_thunk(&closure.function, loop_count)
        {
            closure.jit_thunk.set(Some(thunk));
            closure.jit_state.set(1);
        } else {
            closure.jit_state.set(2);
        }
    }

    /// Run a compiled function.
    pub fn run(&mut self, function: Function) -> Result<Value, VMError> {
        let (uv_kinds, uv_values) = crate::vm::value::make_upvalue_int_caches(0);
        let closure = Rc::new(ObjClosure {
            function: Rc::new(function),
            upvalues: Vec::new(),
            module_globals: crate::vm::value::ModuleGlobals::none(),
            call_count: std::cell::Cell::new(0),
            loop_count: std::cell::Cell::new(0),
            jit_state: std::cell::Cell::new(0),
            jit_bailouts: std::cell::Cell::new(0),
            jit_thunk: std::cell::Cell::new(None),
            specialized_thunk: std::cell::Cell::new(None),
            specialized_arity: std::cell::Cell::new(0),
            specialized_kind: std::cell::Cell::new(0),
            lean_thunk: std::cell::Cell::new(None),
            lean_arity: std::cell::Cell::new(0),
            upvalue_int_kinds: uv_kinds,
            upvalue_int_values: uv_values,
        });

        let from_host = self.is_idle();
        let result = self.run_closure(closure);
        if result.is_err() && from_host {
            self.reset_exec_state();
        }
        result
    }

    fn run_closure(&mut self, closure: Rc<ObjClosure>) -> Result<Value, VMError> {
        self.push(Value::Closure(Rc::clone(&closure)));

        self.call_closure(closure, 0, &[], None)?;

        self.execute_until(0)?;
        Ok(self.pop())
    }

    /// Clear transient execution state (stack, call frames, open upvalues,
    /// error handlers) while preserving globals and the module cache.
    pub fn reset_exec_state(&mut self) {
        self.stack.clear();
        self.stack_view.len = 0;
        self.jit_frames.clear();
        self.jit_frame_view.len = 0;
        self.frames.clear();
        self.open_upvalues.clear();
        self.error_handlers.clear();
        self.import_stack.clear();
        self.jit_executing.set(false);
    }

    /// Limit how many loop iterations scripts may run from now on, across
    /// runs and calls, until the next call. `None` removes the limit.
    ///
    /// Running out is a `loop budget exhausted` error that the script cannot
    /// catch. A host that calls a script every frame sets the budget before
    /// each call.
    pub fn set_loop_budget(&mut self, budget: Option<u64>) {
        self.interrupt.0.set_budget(budget.is_some());
        self.loop_countdown = budget.map_or(0, |n| i64::try_from(n).unwrap_or(i64::MAX));
    }

    /// Make `f` callable from scripts as `name`. Unlike a builtin it is a
    /// closure, so it can hold the host's state. An `Err` is a runtime error
    /// in the script, which the script can catch.
    pub fn register_native(
        &mut self,
        name: &str,
        f: impl Fn(&[Value]) -> Result<Value, String> + 'static,
    ) {
        let native = Value::Native(Rc::new(NativeFunction {
            name: name.to_string(),
            func: Box::new(f),
        }));
        self.natives.retain(|(existing, _)| existing != name);
        self.natives.push((name.to_string(), native.clone()));
        self.set_host_global(name, native);
    }

    /// Keep only the `allowed` capabilities. Builtins behind the others fail
    /// with an error naming the capability, here and in imported modules.
    pub fn restrict(&mut self, allowed: &[Capability]) {
        self.capabilities = Some(allowed.to_vec());
        self.module_cache.clear();
        self.module_deps.clear();
        for &(name, capability) in capabilities::GATED_BUILTINS {
            if !allowed.contains(&capability) {
                self.set_host_global(name, capabilities::refused(name, capability));
            }
        }
    }

    /// Ask `source` for a module's text before looking on disk. It gets the
    /// path as the script wrote it (`math`, `./util`) and returns the source,
    /// or `None` to fall back to the filesystem. Imported modules ask it too.
    pub fn set_module_source(&mut self, source: impl Fn(&str) -> Option<String> + 'static) {
        self.module_source = Some(Rc::new(source));
        self.module_cache.clear();
        self.module_deps.clear();
    }

    /// The capabilities scripts in this VM may use; `None` is all of them.
    pub fn capabilities(&self) -> Option<&[Capability]> {
        self.capabilities.as_deref()
    }

    /// A global the host sets from outside any script.
    fn set_host_global(&mut self, name: &str, value: Value) {
        self.jit.bump_global_version(name);
        self.globals.insert(name.to_string(), value);
    }

    /// Drop the module at `path`, and every cached module that imported it,
    /// from the cache, so the next import reads it again. Returns how many
    /// cache entries this VM dropped.
    pub fn invalidate_module(&mut self, path: &Path) -> usize {
        let target = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
        let doomed: Vec<PathBuf> = self
            .module_cache
            .keys()
            .filter(|cached| {
                **cached == target
                    || self
                        .module_deps
                        .get(*cached)
                        .is_some_and(|deps| deps.contains(&target))
            })
            .cloned()
            .collect();
        for cached in &doomed {
            self.module_cache.remove(cached);
            self.module_deps.remove(cached);
        }
        doomed.len()
    }

    /// A handle that stops this VM's running script from another thread.
    pub fn interrupt_handle(&self) -> limits::InterruptHandle {
        self.interrupt.clone()
    }

    /// One loop iteration's share of the host's limits, taken when the armed
    /// byte is set: delivers a pending interrupt, or counts the iteration
    /// against the budget, failing once it is spent.
    pub(crate) fn loop_limit_step(&mut self) -> Result<(), VMError> {
        if self.interrupt.0.take_interrupt() {
            return Err(self.runtime_error_coded(
                crate::diagnostics::registry::INTERRUPTED,
                "interrupted",
                Some("the program running this script stopped it"),
            ));
        }
        if self.interrupt.0.budget_on() {
            if self.loop_countdown == 0 {
                return Err(self.runtime_error_coded(
                    crate::diagnostics::registry::LOOP_BUDGET_EXHAUSTED,
                    "loop budget exhausted",
                    Some("the program running this script limits how many loop iterations it may run"),
                ));
            }
            self.loop_countdown -= 1;
        }
        Ok(())
    }

    /// A VM for an imported module's top level, run on the importer's
    /// behalf: same execution mode, and the same interrupt and budget
    /// switch. What is left of the budget is lent with [`VM::lend_limits`]
    /// around the run.
    fn child_vm(&self) -> VM {
        let mut child = VM::new_with_mode(self.execution_mode);
        child.interrupt = self.interrupt.clone();
        child.limits_armed = child.interrupt.0.armed_ptr();
        child.jit.set_limits_armed(child.limits_armed as usize);
        if let Some(allowed) = &self.capabilities {
            child.restrict(allowed);
        }
        for (name, native) in &self.natives {
            child.natives.push((name.clone(), native.clone()));
            child.set_host_global(name, native.clone());
        }
        child.module_source = self.module_source.clone();
        child
    }

    /// Moves the remaining loop budget from `self` to `to`.
    fn lend_limits(&mut self, to: &mut VM) {
        to.loop_countdown = std::mem::take(&mut self.loop_countdown);
    }

    /// Look up a global value by name. Returns a clone if present.
    pub fn get_global(&self, name: &str) -> Option<Value> {
        self.globals.get(name).cloned()
    }

    /// Iterate all globals.
    pub fn globals_iter(&self) -> impl Iterator<Item = (&String, &Value)> {
        self.globals.iter()
    }

    /// Install a global.
    pub fn define_global(&mut self, name: String, val: Value) {
        self.globals.insert(name, val);
    }

    /// Where the module whose globals are `globals` was loaded from, if this
    /// VM loaded it.
    pub fn module_path_of(&self, globals: &Rc<HashMap<String, Value>>) -> Option<PathBuf> {
        self.module_cache
            .iter()
            .find(|(_, module)| Rc::ptr_eq(&module.globals, globals))
            .map(|(path, _)| path.clone())
    }

    /// The globals of the module this VM loaded from `path`, if it has.
    pub fn loaded_module_globals(&self, path: &Path) -> Option<Rc<HashMap<String, Value>>> {
        let key = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
        self.module_cache
            .get(&key)
            .map(|module| Rc::clone(&module.globals))
    }

    /// The globals of the module at `path`, importing it into this VM first
    /// if it hasn't been.
    pub fn module_globals_at(
        &mut self,
        path: &Path,
    ) -> Result<Rc<HashMap<String, Value>>, VMError> {
        if let Some(globals) = self.loaded_module_globals(path) {
            return Ok(globals);
        }
        let path_str = path.to_string_lossy().into_owned();
        let bound = path_str.rsplit('/').next().unwrap_or(&path_str).to_string();
        let saved = self.globals.get(&bound).cloned();
        self.import_module(&path_str, &[])?;
        match saved {
            Some(v) => {
                self.globals.insert(bound, v);
            }
            None => {
                self.globals.remove(&bound);
            }
        }
        self.loaded_module_globals(path)
            .ok_or_else(|| self.runtime_error(&format!("module not loaded: {}", path.display())))
    }

    /// Call a callee `Value` (closure or builtin) with positional `args` from
    /// external Rust code, driving the VM to completion and returning the
    /// result.
    pub fn call_with_args(&mut self, callee: Value, args: Vec<Value>) -> Result<Value, VMError> {
        let from_host = self.is_idle();
        let result = self.call_with_args_inner(callee, args);
        if result.is_err() && from_host {
            self.reset_exec_state();
        }
        result
    }

    fn call_with_args_inner(&mut self, callee: Value, args: Vec<Value>) -> Result<Value, VMError> {
        let depth = self.frames.len();
        let arg_count = args.len();
        self.push(callee);
        for a in args {
            self.push(a);
        }
        self.call_value(arg_count, &[])?;
        if self.frames.len() > depth {
            self.execute_until(depth)?;
        }
        Ok(self.pop())
    }

    /// Call a script function from native code that a script called (an ffi
    /// callback). A failed call is unwound here, so the script that made the
    /// native call carries on from a consistent VM.
    pub(crate) fn call_reentrant(
        &mut self,
        callee: Value,
        args: Vec<Value>,
    ) -> Result<Value, VMError> {
        let (frames, jit_frames) = (self.frames.len(), self.jit_frame_view.len);
        let (stack, handlers) = (self.stack.len(), self.error_handlers.len());
        let result = self.call_with_args_inner(callee, args);
        if result.is_err() {
            self.frames.truncate(frames);
            self.jit_frame_view.len = jit_frames;
            self.error_handlers.truncate(handlers);
            self.close_upvalues(stack);
            self.stack_truncate(stack);
        }
        result
    }

    /// No script is running: the host, not a script, is calling in.
    fn is_idle(&self) -> bool {
        self.frames.is_empty() && self.jit_frame_view.len == 0
    }

    /// Read the name string at constant index `name_idx`. Returns an error if the
    /// constant is not a string.
    pub(crate) fn name_constant(&self, name_idx: u16) -> Result<Rc<String>, VMError> {
        match self.current_constant(name_idx).repr() {
            ValueRepr::String(s) => Ok(Rc::clone(s)),
            _ => Err(self.runtime_error("invalid global name")),
        }
    }

    pub(crate) fn handle_get_global(&mut self, name_idx: u16) -> Result<(), VMError> {
        let name = self.name_constant(name_idx)?;
        let found = self
            .active_module_globals()
            .and_then(|mg| mg.get(name.as_ref()))
            .cloned()
            .or_else(|| self.globals.get(name.as_ref()).cloned());
        match found {
            Some(val) => {
                self.push(val);
                Ok(())
            }
            None => Err(self.runtime_error_hint(
                &format!("undefined variable: {}", name),
                "use `:=` to declare new variables",
            )),
        }
    }

    pub(crate) fn handle_set_global(&mut self, name_idx: u16) -> Result<(), VMError> {
        let name = self.name_constant(name_idx)?;
        if !self.globals.contains_key(name.as_ref()) {
            return Err(self.runtime_error_hint(
                &format!("undefined variable: {}", name),
                "use `:=` to declare new variables, or `=` to reassign an existing typed variable",
            ));
        }
        self.reject_locked_global(&name, "change")?;
        if let Some(false) = self.global_mutability.get(name.as_ref()) {
            return Err(self.runtime_error_hint(
                &format!("cannot assign to immutable variable `{}`", name),
                "`=` binds immutably; declare it with `:=` to allow reassignment",
            ));
        }
        let constraint = self
            .global_type_constraints
            .get(name.as_ref())
            .and_then(|c| c.clone());
        if let Some(expected) = constraint {
            let actual = self.peek(0).effective_type_name();
            if !type_matches(&expected, &actual) {
                return Err(self.runtime_error_hint(
                    &format!("type mismatch: expected {}, got {}", expected, actual),
                    &format!("'{}' is locked to type {}", name, expected),
                ));
            }
        }
        let val = self.peek(0).clone();
        self.globals.insert(name.to_string(), val);
        self.jit.bump_global_version(name.as_str());
        Ok(())
    }

    /// A locked global can never be reassigned or declared again.
    fn reject_locked_global(&self, name: &str, verb: &str) -> Result<(), VMError> {
        if self.locked_globals.contains(name) {
            return Err(self.runtime_error_hint(
                &format!("cannot {verb} locked variable `{name}`"),
                "`lock` makes a variable permanently read-only: it can't be reassigned or \
                 overridden with `:=`; use a new name",
            ));
        }
        Ok(())
    }

    pub(crate) fn handle_define_global(&mut self, name_idx: u16) -> Result<(), VMError> {
        let name = self.name_constant(name_idx)?;
        self.reject_locked_global(&name, "declare")?;
        let constraint = self
            .global_type_constraints
            .get(name.as_ref())
            .and_then(|c| c.clone());
        if let Some(expected) = constraint {
            let actual = self.peek(0).effective_type_name();
            if !type_matches(&expected, &actual) {
                return Err(self.runtime_error_hint(
                    &format!(
                        "type mismatch: '{}' is locked to {}, got {}",
                        name, expected, actual
                    ),
                    "use an explicit type annotation to change the locked type",
                ));
            }
        }
        let val = self.pop();
        self.globals.insert(name.to_string(), val);
        self.jit.bump_global_version(name.as_str());
        Ok(())
    }

    /// `mode` is the `DefineGlobalTyped` operand: 0 for `x <T> = v`
    /// (immutable), 1 for `x <T> := v` / `x <T>` (mutable), 2 for `lock`.
    pub(crate) fn handle_define_global_typed(
        &mut self,
        name_idx: u16,
        mode: u8,
        type_idx: u16,
    ) -> Result<(), VMError> {
        let name = self.name_constant(name_idx)?;
        self.reject_locked_global(&name, "declare")?;
        let type_name = self.current_constant(type_idx);
        let val = self.pop();
        self.globals.insert(name.to_string(), val);
        self.global_mutability.insert(name.to_string(), mode == 1);
        if mode == 2 {
            self.locked_globals.insert(name.to_string());
        }
        if let Some(tn) = type_name.as_string() {
            self.global_type_constraints
                .insert(name.to_string(), Some(tn.to_string()));
        }
        self.jit.bump_global_version(name.as_str());
        Ok(())
    }

    pub(crate) fn handle_get_upvalue(&mut self, idx: u16) -> Result<(), VMError> {
        let upvalue = Rc::clone(&self.active_closure().upvalues[idx as usize]);
        let val = match &*upvalue.borrow() {
            Upvalue::Open(slot) => self.stack[*slot].clone(),
            Upvalue::Closed(val) => val.clone(),
        };
        self.push(val);
        Ok(())
    }

    pub(crate) fn handle_set_upvalue(&mut self, idx: u16) -> Result<(), VMError> {
        let val = self.peek(0).clone();
        let upvalue = Rc::clone(&self.active_closure().upvalues[idx as usize]);
        match &mut *upvalue.borrow_mut() {
            Upvalue::Open(slot) => {
                self.stack[*slot] = val;
            }
            Upvalue::Closed(v) => {
                *v = val;
                crate::jit::note_upvalue_write();
            }
        }
        Ok(())
    }

    /// `RangeLimit`: [start, end] -> [end - step, saturated]. Both bounds
    /// must be Integers, as the `range` builtin requires.
    pub(crate) fn handle_range_limit(&mut self, step: i64) -> Result<(), VMError> {
        let end = self.pop();
        let start = self.pop();
        match (&start, &end) {
            (Value::Integer(_), Value::Integer(end)) => {
                self.push(Value::Integer(end.saturating_sub(step)));
                Ok(())
            }
            _ => Err(self.runtime_error("range() requires integer arguments")),
        }
    }

    pub(crate) fn handle_close_upvalue(&mut self) {
        let stack_top = self.stack.len() - 1;
        self.close_upvalues(stack_top);
        self.pop();
    }

    pub(crate) fn handle_log(&mut self, flags: u8) {
        let has_msg = flags & 0x04 != 0;
        let has_sub = flags & 0x02 != 0;
        let has_tag = flags & 0x01 != 0;

        let msg = if has_msg { Some(self.pop()) } else { None };
        let sub = if has_sub { Some(self.pop()) } else { None };
        let tag = if has_tag { Some(self.pop()) } else { None };

        let tag_str = match (&tag, &sub) {
            (Some(t), Some(s)) => format!(
                "[{}:{}]",
                format!("{}", t).to_uppercase(),
                format!("{}", s).to_uppercase()
            ),
            (Some(t), None) => format!("[{}]", format!("{}", t).to_uppercase()),
            _ => String::new(),
        };
        let msg_str = msg.map(|m| format!("{}", m));

        let now = crate::clock::since_epoch().unwrap_or_default();
        let secs = now.as_secs();
        let (hours, mins, seconds) = ((secs / 3600) % 24, (secs / 60) % 60, secs % 60);
        let days = secs / 86400;
        let z = days + 719468;
        let era = z / 146097;
        let doe = z - era * 146097;
        let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
        let y = yoe + era * 400;
        let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
        let mp = (5 * doy + 2) / 153;
        let d = doy - (153 * mp + 2) / 5 + 1;
        let m = if mp < 10 { mp + 3 } else { mp - 9 };
        let year = if m <= 2 { y + 1 } else { y };
        let timestamp = format!(
            "{:04}-{:02}-{:02} {:02}:{:02}:{:02}",
            year, m, d, hours, mins, seconds
        );

        let output = match (tag_str.is_empty(), &msg_str) {
            (true, Some(msg)) => format!("{}: {}", timestamp, msg),
            (false, Some(msg)) => format!("{}: {} {}", timestamp, tag_str, msg),
            (false, None) => format!("{}: {}", timestamp, tag_str),
            (true, None) => format!("{}: <empty log>", timestamp),
        };

        crate::output::write(&format!("{output}\n"));
        self.push(Value::None);
    }

    pub(crate) fn handle_struct_literal(
        &mut self,
        name_idx: u16,
        field_count: u8,
    ) -> Result<(), VMError> {
        let name = self.current_constant(name_idx);
        let Value::String(struct_name) = name else {
            return Err(self.runtime_error("expected struct name"));
        };
        let field_count = field_count as usize;
        let start = self.stack.len() - field_count * 2;
        let flat: Vec<Value> = self.stack_drain_from(start);

        let def = self.struct_def_for_literal(struct_name.as_str())?;
        self.build_struct_literal(def, &flat)
    }

    /// Builds `def` from a literal's `[name, value, ...]` pairs and pushes it.
    /// No pairs gives the zero value.
    fn build_struct_literal(
        &mut self,
        def: Rc<crate::vm::value::ObjStructDef>,
        flat: &[Value],
    ) -> Result<(), VMError> {
        let struct_name_str = def.name.clone();
        let layout = self.field_layout_of(&def)?;
        let mut field_vec: Vec<Value> = layout.slots.iter().map(|_| Value::None).collect();

        let mut assigned = vec![false; layout.slots.len()];
        for pair in flat.chunks(2) {
            if let Some(fname) = pair[0].as_string() {
                if let Some(&idx) = layout.indices.get(fname.as_ref()) {
                    if layout.slots[idx].2
                        && self.active_closure_method_of().is_none_or(|owner| {
                            !self.struct_chain_contains(&struct_name_str, owner)
                        })
                    {
                        return Err(self.runtime_error_hint(
                            &format!(
                                "field '{}' is hidden on {} and cannot be set in a literal",
                                fname, struct_name_str
                            ),
                            "declare the value (`x <Struct>`) and set it through a method of the struct",
                        ));
                    }
                    field_vec[idx] = self.check_field_value(
                        &struct_name_str,
                        fname,
                        &layout.slots[idx].1,
                        pair[1].clone(),
                    )?;
                    assigned[idx] = true;
                } else {
                    return Err(self.runtime_error_hint(
                        &format!("field '{}' not declared on {}", fname, struct_name_str),
                        "check the struct definition for available fields",
                    ));
                }
            }
        }

        let supplied = flat.len() / 2;
        let mut missing: Vec<String> = Vec::new();
        for (i, slot) in layout.slots.iter().enumerate() {
            if !assigned[i] {
                if supplied > 0 && !Self::field_is_optional(&slot.1) {
                    missing.push(format!("`{}`", slot.0));
                }
                field_vec[i] = Self::zero_value_for_type(&slot.1);
            }
        }
        if !missing.is_empty() {
            let plural = if missing.len() == 1 {
                "field"
            } else {
                "fields"
            };
            return Err(self.runtime_error_coded(
                crate::diagnostics::registry::STRUCT_MISSING_FIELDS,
                &format!(
                    "{} is missing {} {}",
                    struct_name_str,
                    plural,
                    missing.join(", ")
                ),
                Some("give every field a value, or none at all for zero-value initialization"),
            ));
        }

        self.push(Value::StructInstance(Rc::new(ObjStructInstance::new(
            struct_name_str,
            field_vec,
            layout,
            def,
        ))));
        Ok(())
    }

    /// The struct a literal names: `Pt` in scope, or `shapes.Pt` through the
    /// module bound to `shapes`.
    fn struct_def_for_literal(
        &self,
        name: &str,
    ) -> Result<Rc<crate::vm::value::ObjStructDef>, VMError> {
        let Some((module, member)) = name.split_once('.') else {
            if let Some(def) = self.get_struct_def(name) {
                return Ok(def);
            }
            let bound = self
                .globals
                .get(name)
                .or_else(|| self.active_module_globals().and_then(|mg| mg.get(name)));
            return Err(match bound {
                Some(other) => self.not_a_struct(name, other),
                None => {
                    self.runtime_error(&format!("struct '{}' is not defined in this scope", name))
                }
            });
        };
        let bound = self
            .globals
            .get(module)
            .or_else(|| self.active_module_globals().and_then(|mg| mg.get(module)));
        let Some(m) = bound.and_then(|v| v.as_module()) else {
            return Err(self.runtime_error_hint(
                &format!("'{module}' in <{name}> is not an imported module"),
                &format!("import it first: `introduce {module}`"),
            ));
        };
        self.struct_def_in_module(m, member, name)
    }

    /// The struct `member` of module `m`; `name` is how the source wrote it.
    fn struct_def_in_module(
        &self,
        m: &crate::vm::value::ObjModule,
        member: &str,
        name: &str,
    ) -> Result<Rc<crate::vm::value::ObjStructDef>, VMError> {
        if m.hidden.contains(member) {
            return Err(self.runtime_error_hint(
                &format!("'{}' is hidden inside module '{}'", member, m.name),
                "it is declared `hide` — use a type the module exposes",
            ));
        }
        match m.globals.get(member) {
            Some(Value::StructDef(def)) => Ok(Rc::clone(def)),
            Some(other) => Err(self.not_a_struct(name, other)),
            None => Err(self.runtime_error_hint(
                &format!("module '{}' has no member '{}'", m.name, member),
                "check the module's public API for available members",
            )),
        }
    }

    /// `name` is bound, but not to a struct, so there's nothing to build.
    fn not_a_struct(&self, name: &str, value: &Value) -> VMError {
        match value {
            Value::EnumDef(_) => self.runtime_error_hint(
                &format!("{name} is an enum, not a struct"),
                &format!("an enum value is one of its variants: `{name}.<Variant>`"),
            ),
            other => {
                let kind = other.type_name().to_lowercase();
                let article = if kind.starts_with(['a', 'e', 'i', 'o', 'u']) {
                    "an"
                } else {
                    "a"
                };
                self.runtime_error_hint(
                    &format!("{name} is {article} {kind}, not a struct"),
                    "a type annotation names a struct or an enum",
                )
            }
        }
    }

    /// A struct def's flattened field layout (parent fields first, then own).
    /// Computed lazily on first call and cached in the def's `OnceCell`.
    pub(crate) fn field_layout_of(
        &self,
        def: &Rc<crate::vm::value::ObjStructDef>,
    ) -> Result<Rc<crate::vm::value::FieldLayout>, VMError> {
        if let Some(layout) = def.layout.get() {
            return Ok(Rc::clone(layout));
        }

        let mut chain: Vec<Rc<crate::vm::value::ObjStructDef>> = Vec::new();
        let mut cur = Some(Rc::clone(def));
        while let Some(d) = cur {
            cur = self.parent_struct_def(&d);
            if cur.is_none()
                && let Some(p) = &d.parent
            {
                return Err(self.runtime_error_hint(
                    &format!(
                        "parent struct '{p}' of '{}' is not defined in this scope",
                        d.name
                    ),
                    "import the parent struct, or check it is defined before the child",
                ));
            }
            chain.push(d);
        }
        chain.reverse();

        let mut slots: Vec<(String, String, bool)> = Vec::new();
        let mut indices: HashMap<String, usize> = HashMap::new();
        for d in &chain {
            for f in &d.fields {
                if !indices.contains_key(&f.0) {
                    indices.insert(f.0.clone(), slots.len());
                    slots.push(f.clone());
                }
            }
        }

        let layout = Rc::new(crate::vm::value::FieldLayout { slots, indices });
        let _ = def.layout.set(Rc::clone(&layout));
        Ok(def.layout.get().cloned().unwrap_or(layout))
    }

    pub(crate) fn handle_get_field(&mut self, field_idx: u16) -> Result<(), VMError> {
        let object = self.pop();
        let value = self.handle_get_field_from_value(object, field_idx)?;
        self.push(value);
        Ok(())
    }

    pub(crate) fn handle_get_field_from_value(
        &mut self,
        object: Value,
        field_idx: u16,
    ) -> Result<Value, VMError> {
        let field_name = self.current_constant(field_idx);
        let Value::String(fname) = &field_name else {
            return Err(self.runtime_error("invalid field name"));
        };

        match object.repr() {
            ValueRepr::Vector(vector) => match vector.component(fname) {
                Some(c) => Ok(Value::Float(f64::from(c))),
                None => Err(self.runtime_error(&format!(
                    "{} has no component {}",
                    vector.constructor(),
                    fname
                ))),
            },
            ValueRepr::StructInstance(inst) => {
                if let Some(&idx) = inst.layout.indices.get(fname.as_ref()) {
                    if inst.layout.slots[idx].2 && !self.hidden_field_access_allowed(inst) {
                        return Err(self.hidden_field_error(fname, &inst.struct_name));
                    }
                    return Ok(inst.get_field(idx));
                }
                let method_val = if let Some(def) = self
                    .globals
                    .get(&inst.struct_name)
                    .and_then(|v| v.as_struct_def())
                {
                    let methods = def.methods.borrow();
                    methods.get(fname.as_ref()).cloned()
                } else {
                    None
                };
                if let Some(method) = method_val {
                    Ok(method)
                } else {
                    Err(self.runtime_error_hint(
                        &format!("field '{}' not found on {}", fname, inst.struct_name),
                        "check the struct definition for available fields and methods",
                    ))
                }
            }
            ValueRepr::Module(m) => {
                if m.hidden.contains(fname.as_ref()) {
                    return Err(self.runtime_error_hint(
                        &format!("'{}' is hidden inside module '{}'", fname, m.name),
                        "it is declared `hide fun` — call it through a function the module exposes",
                    ));
                }
                match m.globals.get(fname.as_ref()) {
                    Some(val) => Ok(val.clone()),
                    None => Err(self.runtime_error_hint(
                        &format!("module '{}' has no member '{}'", m.name, fname),
                        "check the module's public API for available members",
                    )),
                }
            }
            ValueRepr::ErrorValue(data) => match fname.as_str() {
                "msg" => Ok(Value::String(Rc::clone(&data.msg))),
                "tag" => Ok(match &data.tag {
                    Some(t) => Value::String(Rc::clone(t)),
                    None => Value::None,
                }),
                _ => Err(self.runtime_error(&format!("ErrorValue has no field '{}'", fname))),
            },
            ValueRepr::Map(entries) => {
                let entries = entries.borrow();
                let key = Value::String(Rc::clone(fname));
                match entries.get(&key) {
                    Some(v) => Ok(v.clone()),
                    None => Err(self.runtime_error_hint(
                        &format!("key '{fname}' not found on map"),
                        &format!(
                            "use m[\"{fname}\"] or has(m, \"{fname}\") if the key may be absent"
                        ),
                    )),
                }
            }
            ValueRepr::EnumDef(def) => {
                let variant = def
                    .variants
                    .iter()
                    .find(|v| v.name.as_str() == fname.as_ref());
                match variant {
                    Some(v) => match &v.kind {
                        crate::vm::value::VmEnumVariantKind::Unit(disc) => Ok(Value::EnumInstance(
                            Rc::new(crate::vm::value::ObjEnumInstance {
                                enum_name: def.name.clone(),
                                variant_name: v.name.clone(),
                                payload: crate::vm::value::VmEnumPayload::Unit(disc.clone()),
                            }),
                        )),
                        crate::vm::value::VmEnumVariantKind::Tuple(_) => Err(self
                            .runtime_error_hint(
                                &format!(
                                    "variant '{}' of enum {} requires arguments",
                                    v.name, def.name
                                ),
                                "use EnumName.Variant(args) to construct tuple variants",
                            )),
                        crate::vm::value::VmEnumVariantKind::Struct(_) => Err(self
                            .runtime_error_hint(
                                &format!(
                                    "variant '{}' of enum {} requires struct fields",
                                    v.name, def.name
                                ),
                                "use EnumName.Variant { field: value } for struct variants",
                            )),
                    },
                    None => Err(self
                        .runtime_error(&format!("enum {} has no variant '{}'", def.name, fname))),
                }
            }
            ValueRepr::EnumInstance(inst) => {
                let name = fname.as_str();
                match name {
                    "name" => Ok(Value::String(rc_str(inst.variant_name.as_str()))),
                    "value" => match &inst.payload {
                        crate::vm::value::VmEnumPayload::Unit(Some(v)) => Ok(v.clone()),
                        crate::vm::value::VmEnumPayload::Unit(None) => Ok(Value::None),
                        _ => Err(self.runtime_error(".value is only defined on unit variants")),
                    },
                    other => match &inst.payload {
                        crate::vm::value::VmEnumPayload::Tuple(items) => {
                            let idx_opt = other.parse::<usize>().ok().or_else(|| {
                                self.globals.get(&inst.enum_name).and_then(|gv| {
                                    if let Some(def) = gv.as_enum_def() {
                                        def.variants
                                            .iter()
                                            .find(|v| v.name == inst.variant_name)
                                            .and_then(|v| match &v.kind {
                                                crate::vm::value::VmEnumVariantKind::Tuple(
                                                    names,
                                                ) => names.iter().position(|n| n == other),
                                                _ => None,
                                            })
                                    } else {
                                        None
                                    }
                                })
                            });
                            match idx_opt.and_then(|i| items.get(i).cloned()) {
                                Some(v) => Ok(v),
                                None => Err(self.runtime_error(&format!(
                                    "tuple variant {}.{} has no field '{}'",
                                    inst.enum_name, inst.variant_name, other
                                ))),
                            }
                        }
                        crate::vm::value::VmEnumPayload::Struct(fields) => {
                            match fields.iter().find(|(k, _)| k == other) {
                                Some((_, v)) => Ok(v.clone()),
                                None => Err(self.runtime_error(&format!(
                                    "struct variant {}.{} has no field '{}'",
                                    inst.enum_name, inst.variant_name, other
                                ))),
                            }
                        }
                        crate::vm::value::VmEnumPayload::Unit(_) => {
                            Err(self.runtime_error(&format!(
                                "unit variant {}.{} has no field '{}'",
                                inst.enum_name, inst.variant_name, other
                            )))
                        }
                    },
                }
            }
            ValueRepr::Wrapped(inner) => match fname.as_str() {
                "value" => Ok((**inner).clone()),
                _ => Err(self.runtime_error(&format!("Value wrapper has no field '{}'", fname))),
            },
            _ => Err(self.runtime_error_hint(
                &format!("cannot access field '{}' on {}", fname, object.type_name()),
                "field access is supported on structs, enums, modules, maps, and error values",
            )),
        }
    }

    /// Is the code in the current frame allowed to touch a hidden field of
    /// `inst`? Yes when that code was compiled as part of a struct in the
    /// instance's own inheritance chain: a method of the struct, a method it
    /// inherited, or a closure nested inside one of those.
    ///
    /// A `Person` method may read a hidden field of an `American`, but a method
    /// of an unrelated struct may not, and neither may top-level code.
    pub(crate) fn hidden_field_access_allowed(
        &self,
        inst: &crate::vm::value::ObjStructInstance,
    ) -> bool {
        if self.frames.is_empty() && !self.jit_executing.get() {
            return false;
        }
        let Some(owner) = self.active_closure().function.method_of.as_deref() else {
            return false;
        };
        self.struct_chain_contains(&inst.struct_name, owner)
    }

    /// `method_of` of the innermost activation, interpreted or JIT'd.
    fn active_closure_method_of(&self) -> Option<&str> {
        if self.frames.is_empty() && !self.jit_executing.get() {
            return None;
        }
        self.active_closure().function.method_of.as_deref()
    }

    /// Does `struct_name`'s inheritance chain include `owner`?
    fn struct_chain_contains(&self, struct_name: &str, owner: &str) -> bool {
        if struct_name == owner {
            return true;
        }
        let mut current = self
            .globals
            .get(struct_name)
            .and_then(|v| v.as_struct_def())
            .and_then(|d| d.parent.clone());
        while let Some(name) = current {
            if name == owner {
                return true;
            }
            current = self
                .globals
                .get(&name)
                .and_then(|v| v.as_struct_def())
                .and_then(|d| d.parent.clone());
        }
        false
    }

    /// Error for a refused hidden-field access.
    fn hidden_field_error(&self, field: &str, struct_name: &str) -> VMError {
        self.runtime_error_hint(
            &format!("field '{}' is hidden on {}", field, struct_name),
            "it is declared `hide` — reach it through a method of the struct",
        )
    }

    pub(crate) fn handle_set_field(&mut self, field_idx: u16) -> Result<(), VMError> {
        let value = self.pop();
        let object = self.pop();
        self.handle_set_field_on_value(object, field_idx, value)
    }

    pub(crate) fn handle_set_field_on_value(
        &mut self,
        object: Value,
        field_idx: u16,
        value: Value,
    ) -> Result<(), VMError> {
        let field_name = self.current_constant(field_idx);
        let Value::String(fname) = &field_name else {
            return Err(self.runtime_error("invalid field name"));
        };
        match object.repr() {
            ValueRepr::Vector(vector) => Err(self.runtime_error_hint(
                &format!(
                    "cannot assign to {}.{}: vectors are values",
                    vector.constructor(),
                    fname
                ),
                "build a new vector instead, e.g. v = vec2(5, v.y)",
            )),
            ValueRepr::StructInstance(inst) => {
                if let Some(&idx) = inst.layout.indices.get(fname.as_ref()) {
                    if inst.locked.get() {
                        return Err(self.locked_value_error(&object));
                    }
                    if inst.layout.slots[idx].2 && !self.hidden_field_access_allowed(inst) {
                        return Err(self.hidden_field_error(fname, &inst.struct_name));
                    }
                    let value = self.check_field_value(
                        &inst.struct_name,
                        fname,
                        &inst.layout.slots[idx].1,
                        value,
                    )?;
                    inst.set_field(idx, value);
                    Ok(())
                } else {
                    Err(self.runtime_error_hint(
                        &format!("field '{}' not declared on {}", fname, inst.struct_name),
                        "check the struct definition for available fields",
                    ))
                }
            }
            ValueRepr::Map(entries) => {
                if entries.borrow().is_locked() {
                    return Err(self.locked_value_error(&object));
                }
                let key = Value::String(Rc::clone(fname));
                entries.borrow_mut().insert(key, value);
                Ok(())
            }
            _ => Err(self.runtime_error_hint(
                &format!("cannot set field on {}", object.type_name()),
                "field assignment is supported on struct instances and maps",
            )),
        }
    }

    pub(crate) fn handle_define_method(
        &mut self,
        struct_name_idx: u16,
        method_count: u8,
    ) -> Result<(), VMError> {
        let struct_name = self.current_constant(struct_name_idx);
        let Value::String(name_str) = struct_name else {
            return Err(self.runtime_error("invalid struct name"));
        };
        let method_count = method_count as usize;
        let start = self.stack.len() - method_count * 2;
        let flat: Vec<Value> = self.stack_drain_from(start);
        if let Some(def) = self
            .globals
            .get(name_str.as_ref())
            .and_then(|v| v.as_struct_def())
        {
            let mut methods = def.methods.borrow_mut();
            for pair in flat.chunks(2) {
                if let Some(mname) = pair[1].as_string() {
                    methods.insert(mname.to_string(), pair[0].clone());
                }
            }
            drop(methods);
            self.jit.invalidate_method_caches();
        }
        Ok(())
    }

    pub(crate) fn handle_method_call(
        &mut self,
        method_idx: u16,
        arg_count: u8,
    ) -> Result<(), VMError> {
        let method_name = self.current_constant(method_idx);
        let Value::String(mname) = method_name else {
            return Err(self.runtime_error("invalid method name"));
        };
        self.call_method(&mname, arg_count as usize, &[])
    }

    /// Handle the `Closure` opcode. Reads the upvalue descriptors starting
    /// at `descriptors_offset` in the current frame's chunk. On return the
    /// new closure is on top of the stack.
    pub(crate) fn handle_closure(
        &mut self,
        fn_const_idx: u16,
        descriptors_offset: usize,
    ) -> Result<(), VMError> {
        let constant = self.current_constant(fn_const_idx);
        let Value::Closure(template) = constant else {
            return Err(self.runtime_error("expected closure constant"));
        };

        let upvalue_count = template.function.upvalue_count as usize;

        let (chunk_rc, parent_upvalues, slot_offset) = {
            let closure = self.active_closure();
            (
                Rc::clone(&closure.function),
                closure.upvalues.clone(),
                self.current_slot_offset(),
            )
        };
        let code = &chunk_rc.chunk.code;

        let mut upvalues = Vec::with_capacity(upvalue_count);
        let mut off = descriptors_offset;
        for _ in 0..upvalue_count {
            let is_local = code[off] == 1;
            off += 1;
            let index = ((code[off] as u16) << 8) | (code[off + 1] as u16);
            off += 2;

            if is_local {
                let abs_slot = slot_offset + index as usize;
                let upvalue = self.capture_upvalue(abs_slot);
                upvalues.push(upvalue);
            } else {
                upvalues.push(Rc::clone(&parent_upvalues[index as usize]));
            }
        }

        let (uv_kinds, uv_values) = crate::vm::value::make_upvalue_int_caches(upvalues.len());
        let closure = Rc::new(ObjClosure {
            function: Rc::clone(&template.function),
            upvalues,
            module_globals: crate::vm::value::ModuleGlobals::from(
                template
                    .module_globals
                    .get()
                    .or_else(|| self.active_module_globals_rc()),
            ),
            call_count: std::cell::Cell::new(0),
            loop_count: std::cell::Cell::new(0),
            jit_state: std::cell::Cell::new(0),
            jit_bailouts: std::cell::Cell::new(0),
            jit_thunk: std::cell::Cell::new(None),
            specialized_thunk: std::cell::Cell::new(None),
            specialized_arity: std::cell::Cell::new(0),
            specialized_kind: std::cell::Cell::new(0),
            lean_thunk: std::cell::Cell::new(None),
            lean_arity: std::cell::Cell::new(0),
            upvalue_int_kinds: uv_kinds,
            upvalue_int_values: uv_values,
        });
        self.push(Value::Closure(closure));
        Ok(())
    }

    /// Closure transfer: if `self.stack[start]` is a closure with live (open)
    /// captures, replace it with a snapshot whose upvalues are `Closed` over the
    /// current stack values, so a no-stack builtin (`spawn`) can read them.
    #[cold]
    #[inline(never)]
    fn snapshot_spawn_closure(&mut self, start: usize) {
        let c = match &self.stack[start] {
            Value::Closure(c) => Rc::clone(c),
            _ => return,
        };
        if !c
            .upvalues
            .iter()
            .any(|uv| matches!(&*uv.borrow(), Upvalue::Open(_)))
        {
            return;
        }
        let vals: Vec<Value> = c
            .upvalues
            .iter()
            .map(|uv| match &*uv.borrow() {
                Upvalue::Open(slot) => self.stack[*slot].clone(),
                Upvalue::Closed(v) => v.clone(),
            })
            .collect();
        let closed = crate::vm::value::ObjClosure::closed(Rc::clone(&c.function), vals);
        closed.module_globals.set(c.module_globals.get());
        self.stack[start] = Value::Closure(Rc::new(closed));
    }

    #[inline(always)]
    pub(crate) fn push(&mut self, value: Value) {
        unsafe {
            self.stack.set_len(self.stack_view.len);
        }
        if self.stack.len() >= STACK_MAX {
            panic!("stack overflow: exceeded {} slots", STACK_MAX);
        }
        self.stack.push(value);
        self.stack_view.len = self.stack.len();
    }

    #[inline(always)]
    pub(crate) fn pop(&mut self) -> Value {
        unsafe {
            self.stack.set_len(self.stack_view.len);
        }
        let v = self.stack.pop().expect("stack underflow");
        self.stack_view.len = self.stack.len();
        v
    }

    /// `push`/`pop` without the `set_len` resync. Only callable from the
    /// dispatch loop, where `stack.len() == stack_view.len` always holds.
    #[inline(always)]
    fn push_fast(&mut self, value: Value) {
        debug_assert_eq!(self.stack.len(), self.stack_view.len);
        if self.stack.len() >= STACK_MAX {
            panic!("stack overflow: exceeded {} slots", STACK_MAX);
        }
        self.stack.push(value);
        self.stack_view.len = self.stack.len();
    }

    #[inline(always)]
    fn pop_fast(&mut self) -> Value {
        debug_assert_eq!(self.stack.len(), self.stack_view.len);
        let v = self.stack.pop().expect("stack underflow");
        self.stack_view.len = self.stack.len();
        v
    }

    pub(crate) fn peek(&self, distance: usize) -> &Value {
        &self.stack[self.stack.len() - 1 - distance]
    }

    /// Stack slot `n` counted from 0 at the bottom.
    #[allow(dead_code)]
    pub(crate) fn stack_slot(&self, n: usize) -> &Value {
        &self.stack[n]
    }

    #[allow(dead_code)]
    pub(crate) fn set_stack_slot(&mut self, n: usize, v: Value) {
        self.stack[n] = v;
    }

    #[allow(dead_code)]
    pub(crate) fn current_slot_offset(&self) -> usize {
        if self.jit_executing.get()
            && let Some(f) = self.jit_frame_top()
        {
            return f.slot_offset;
        }
        self.frames.last().map(|f| f.slot_offset).unwrap_or(0)
    }

    /// Clone the constant at `idx` from the current frame's chunk.
    pub(crate) fn current_constant(&self, idx: u16) -> Value {
        if self.jit_executing.get()
            && let Some(frame) = self.jit_frame_top()
        {
            let closure = unsafe { &*frame.closure_raw };
            return closure.function.chunk.constants[idx as usize].fresh_clone();
        }
        let frame = self.frames.last().unwrap();
        frame.closure.function.chunk.constants[idx as usize].fresh_clone()
    }

    #[allow(dead_code)]
    #[inline(always)]
    pub(crate) fn jit_frame_push_raw(
        &mut self,
        closure_raw: *const ObjClosure,
        slot_offset: usize,
        module_globals: *const HashMap<String, Value>,
        line: u32,
    ) {
        let len = self.jit_frame_view.len;
        debug_assert!(len < FRAMES_MAX);
        unsafe {
            self.jit_frame_view.ptr.add(len).write(JitFrame {
                closure_raw,
                slot_offset,
                module_globals,
                line,
                column: 0,
            });
        }
        self.jit_frame_view.len = len + 1;
    }

    #[allow(dead_code)]
    #[inline(always)]
    pub(crate) fn jit_frame_pop_raw(&mut self) -> Option<JitFrame> {
        let len = self.jit_frame_view.len;
        if len == 0 {
            return None;
        }
        let new_len = len - 1;
        self.jit_frame_view.len = new_len;
        Some(unsafe { self.jit_frame_view.ptr.add(new_len).read() })
    }

    #[allow(dead_code)]
    #[inline(always)]
    pub(crate) fn jit_frame_top(&self) -> Option<&JitFrame> {
        let len = self.jit_frame_view.len;
        if len == 0 {
            None
        } else {
            Some(unsafe { &*self.jit_frame_view.ptr.add(len - 1) })
        }
    }

    #[allow(dead_code)]
    #[inline(always)]
    pub(crate) fn jit_frame_len(&self) -> usize {
        self.jit_frame_view.len
    }

    #[allow(dead_code)]
    #[inline(always)]
    pub(crate) fn set_jit_frame_line(&mut self, line: u32) {
        let len = self.jit_frame_view.len;
        if len != 0 {
            unsafe {
                (*self.jit_frame_view.ptr.add(len - 1)).line = line;
            }
        }
    }

    /// Pop the topmost call frame. Returns `None` if there is none.
    #[allow(dead_code)]
    pub(crate) fn frames_pop(&mut self) -> Option<CallFrameHandle> {
        self.frames.pop().map(|f| CallFrameHandle {
            slot_offset: f.slot_offset,
        })
    }

    #[allow(dead_code)]
    #[inline(always)]
    pub(crate) fn stack_truncate(&mut self, len: usize) {
        self.stack.truncate(len);
        self.stack_view.len = self.stack.len();
    }

    /// Drain `start..` from the stack, syncing `stack_view.len`.
    #[inline(always)]
    pub(crate) fn stack_drain_from(&mut self, start: usize) -> Vec<Value> {
        let out: Vec<Value> = self.stack.drain(start..).collect();
        self.stack_view.len = self.stack.len();
        out
    }

    /// Insert `value` at `idx`, syncing `stack_view.len`. O(stack_len - idx).
    ///
    /// Refuses to grow the stack past `STACK_MAX`.
    #[inline(always)]
    pub(crate) fn stack_insert(&mut self, idx: usize, value: Value) -> Result<(), VMError> {
        if self.stack.len() >= STACK_MAX {
            return Err(self.runtime_error_hint(
                "stack overflow",
                "check for infinite recursion or deeply nested calls",
            ));
        }
        self.stack.insert(idx, value);
        self.stack_view.len = self.stack.len();
        Ok(())
    }

    /// Overwrite an existing slot in-place (no length change, no sync).
    #[inline(always)]
    #[allow(dead_code)]
    pub(crate) fn stack_overwrite(&mut self, idx: usize, value: Value) {
        self.stack[idx] = value;
    }

    #[allow(dead_code)]
    pub(crate) fn frames_len(&self) -> usize {
        self.frames.len()
    }

    #[allow(dead_code)]
    pub(crate) fn stack_as_mut_ptr(&mut self) -> *mut Value {
        self.stack.as_mut_ptr()
    }

    #[allow(dead_code)]
    pub(crate) fn stack_len(&self) -> usize {
        self.stack.len()
    }

    #[allow(dead_code)]
    #[inline(always)]
    pub(crate) fn stack_at(&self, idx: usize) -> &Value {
        &self.stack[idx]
    }

    /// Shrink the stack by `n` elements, dropping the values.
    #[allow(dead_code)]
    #[inline(always)]
    pub(crate) fn stack_shrink(&mut self, n: usize) {
        let new_len = self.stack.len().saturating_sub(n);
        self.stack.truncate(new_len);
        self.stack_view.len = self.stack.len();
    }

    /// Commit a raw-IR stack rearrangement: tell the backing `Vec<Value>`
    /// that its logical length is now `new_len`, matching the writes the
    /// JIT emitted through `stack_view.ptr`.
    /// # Safety
    /// - `new_len <= STACK_MAX` (the capacity `VM::new` pre-allocates).
    /// - Every slot in `[old_len, new_len)` must hold initialized,
    ///   bit-valid `Value` data, including correctly-adjusted `Rc` strong
    ///   counts for heap-allocated variants.
    #[allow(dead_code)]
    #[inline(always)]
    pub(crate) unsafe fn stack_set_len_raw(&mut self, new_len: usize) {
        debug_assert!(new_len <= self.stack.capacity());
        unsafe {
            self.stack.set_len(new_len);
        }
        self.stack_view.len = new_len;
    }

    fn current_line(&self) -> u32 {
        if self.jit_executing.get()
            && let Some(frame) = self.jit_frame_top()
        {
            return frame.line;
        }
        let Some(frame) = self.frames.last() else {
            return 0;
        };
        let ip = if frame.ip > 0 { frame.ip - 1 } else { 0 };
        if ip < frame.closure.function.chunk.lines.len() {
            frame.closure.function.chunk.lines[ip]
        } else {
            0
        }
    }

    /// Source column (1-based) of the opcode that just executed, paired
    /// with `current_line()`. Returns 0 when the chunk has no column
    /// metadata for this IP, and on the JIT path.
    fn current_column(&self) -> u32 {
        if self.jit_executing.get()
            && let Some(frame) = self.jit_frame_top()
        {
            return frame.column;
        }
        let Some(frame) = self.frames.last() else {
            return 0;
        };
        let ip = if frame.ip > 0 { frame.ip - 1 } else { 0 };
        frame
            .closure
            .function
            .chunk
            .columns
            .get(ip)
            .copied()
            .unwrap_or(0)
    }

    #[inline(always)]
    pub(crate) fn active_closure(&self) -> &ObjClosure {
        if self.jit_executing.get()
            && let Some(frame) = self.jit_frame_top()
        {
            return unsafe { &*frame.closure_raw };
        }
        &self.frames.last().unwrap().closure
    }

    #[inline(always)]
    #[allow(dead_code)]
    pub(crate) fn active_compiled_closure(&self) -> Option<&ObjClosure> {
        self.jit_frame_top()
            .map(|frame| unsafe { &*frame.closure_raw })
    }

    /// The module scope of the innermost activation. A JIT frame's answer is
    /// authoritative, including "none".
    #[inline(always)]
    fn active_module_globals(&self) -> Option<&HashMap<String, Value>> {
        if self.jit_executing.get()
            && let Some(frame) = self.jit_frame_top()
        {
            return (!frame.module_globals.is_null()).then(|| unsafe { &*frame.module_globals });
        }
        self.frames.last().and_then(|f| f.module_globals.as_deref())
    }

    /// Owned-`Rc` variant of `active_module_globals` for call paths that
    /// store the dict in a new `CallFrame`/`ObjClosure`.
    fn active_module_globals_rc(&self) -> Option<Rc<HashMap<String, Value>>> {
        if self.jit_executing.get()
            && let Some(frame) = self.jit_frame_top()
        {
            return unsafe { mg_rc(frame.module_globals) };
        }
        self.frames.last().and_then(|f| f.module_globals.clone())
    }

    /// Presence-only test for `active_module_globals`. Only called from JIT code.
    #[cfg_attr(not(feature = "jit"), allow(dead_code))]
    #[inline(always)]
    pub(crate) fn has_active_module_globals(&self) -> bool {
        if self.jit_executing.get()
            && let Some(frame) = self.jit_frame_top()
        {
            return !frame.module_globals.is_null();
        }
        self.frames
            .last()
            .map(|f| f.module_globals.is_some())
            .unwrap_or(false)
    }

    fn format_error_coded(
        &self,
        code: crate::diagnostics::Code,
        message: &str,
        hint: Option<&str>,
    ) -> String {
        let line_num = self.current_line() as usize;
        let col = self.current_column() as usize;
        let mut out = String::new();

        out.push_str(&format!("error[{}]: {}\n", code, message));
        let frame_name = self.innermost_frame_name();
        let origin = self.innermost_frame_origin();
        let file = origin
            .as_ref()
            .map(|o| o.file.clone())
            .or_else(|| self.current_file.as_deref().map(display_path_cwd));
        let source = origin
            .as_ref()
            .map(|o| o.source.as_str())
            .unwrap_or(&self.source);

        let location = match (&file, col > 0) {
            (Some(f), true) => format!("{}:{}:{}", f, line_num, col),
            (Some(f), false) => format!("{}:{}", f, line_num),
            (None, true) => format!("line {}:{}", line_num, col),
            (None, false) => format!("line {}", line_num),
        };
        match &frame_name {
            Some(name) => out.push_str(&format!("  --> {} (in `{}`)\n", location, name)),
            None => out.push_str(&format!("  --> {}\n", location)),
        }

        if !source.is_empty()
            && let Some(source_line) = source.lines().nth(line_num.saturating_sub(1))
        {
            let line_str = format!("{}", line_num);
            let padding = " ".repeat(line_str.len());
            out.push_str(&format!("{} |\n", padding));
            out.push_str(&format!("{} | {}", line_str, source_line));
            if col > 0 {
                let caret_padding = " ".repeat(col.saturating_sub(1));
                out.push_str(&format!("\n{} | {}^", padding, caret_padding));
            }
        }

        if let Some(hint) = hint {
            out.push_str(&format!("\n  = hint: {}", hint));
        }
        if crate::diagnostics::registry::lookup(code).is_some() {
            out.push_str(&format!(
                "\n  = note: run `oxigen explain {code}` for more information"
            ));
        }

        let trace = self.stack_trace();
        if trace.len() >= 2 {
            out.push_str("\nstack trace (innermost first):");
            for (name, line, frame_file) in &trace {
                let label = name.as_deref().unwrap_or("<top-level>");
                match frame_file {
                    Some(f) => out.push_str(&format!("\n   at `{}` ({}:{})", label, f, line)),
                    None => out.push_str(&format!("\n   at `{}` (line {})", label, line)),
                }
            }
        }

        out
    }

    /// Source and file of the function active at the innermost frame.
    /// `None` means the entry script, whose source the VM holds directly.
    fn innermost_frame_origin(&self) -> Option<Rc<crate::vm::value::ModuleOrigin>> {
        if self.jit_executing.get()
            && let Some(frame) = self.jit_frame_top()
        {
            let closure = unsafe { &*frame.closure_raw };
            return closure.function.origin.borrow().clone();
        }
        self.frames
            .last()
            .and_then(|f| f.closure.function.origin.borrow().clone())
    }

    /// Name of the function active at the innermost frame. `None` means
    /// the error fired at the top-level script body.
    fn innermost_frame_name(&self) -> Option<String> {
        if self.jit_executing.get()
            && let Some(frame) = self.jit_frame_top()
        {
            let closure = unsafe { &*frame.closure_raw };
            return closure.function.name.clone();
        }
        self.frames
            .last()
            .and_then(|f| f.closure.function.name.clone())
    }

    /// Build an error as the caller of the innermost JIT frame would see it.
    /// The callee's frame is hidden while the error is rendered, so the error
    /// names the call site.
    #[cfg(feature = "jit")]
    pub(crate) fn error_in_jit_caller(&mut self, message: &str, hint: &str) -> VMError {
        let Some(callee) = self.jit_frame_pop_raw() else {
            return self.runtime_error_hint(message, hint);
        };
        let jit_caller = self.jit_frame_top().map(|f| f.slot_offset);
        let interp_caller = self.frames.last().map(|f| f.slot_offset);
        let prev = self
            .jit_executing
            .replace(match (jit_caller, interp_caller) {
                (Some(j), Some(i)) => j > i,
                (Some(_), None) => true,
                _ => false,
            });
        let err = self.runtime_error_hint(message, hint);
        self.jit_executing.set(prev);
        let len = self.jit_frame_view.len;
        unsafe { self.jit_frame_view.ptr.add(len).write(callee) };
        self.jit_frame_view.len = len + 1;
        err
    }

    /// Walk the active call frames from innermost to outermost, returning
    /// `(function_name, line)` per frame. The top-level script body has
    /// `name == None`. JIT and interpreter frames are merged by stack base.
    fn stack_trace(&self) -> Vec<(Option<String>, u32, Option<String>)> {
        let entry_file = self.current_file.as_deref().map(display_path_cwd);
        let frame_file = |closure: &ObjClosure| -> Option<String> {
            closure
                .function
                .origin
                .borrow()
                .as_ref()
                .map(|o| o.file.clone())
                .or_else(|| entry_file.clone())
        };

        type TraceEntry = (Option<String>, u32, Option<String>);
        let jit_on_top = self.jit_executing.get();
        let mut entries: Vec<(usize, bool, TraceEntry)> = Vec::new();
        for i in 0..self.jit_frame_view.len {
            let frame = unsafe { &*self.jit_frame_view.ptr.add(i) };
            let closure = unsafe { &*frame.closure_raw };
            entries.push((
                frame.slot_offset,
                jit_on_top,
                (
                    closure.function.name.clone(),
                    frame.line,
                    frame_file(closure),
                ),
            ));
        }
        for frame in &self.frames {
            let ip = if frame.ip > 0 { frame.ip - 1 } else { 0 };
            let line = frame
                .closure
                .function
                .chunk
                .lines
                .get(ip)
                .copied()
                .unwrap_or(0);
            entries.push((
                frame.slot_offset,
                !jit_on_top,
                (
                    frame.closure.function.name.clone(),
                    line,
                    frame_file(&frame.closure),
                ),
            ));
        }
        entries.sort_by(|a, b| b.0.cmp(&a.0).then(b.1.cmp(&a.1)));
        entries.dedup_by(|later, kept| later.0 == kept.0);
        entries.into_iter().map(|(_, _, e)| e).collect()
    }

    /// Whether a field's declared type admits `None`, such as a linked-list
    /// `next <Node> || <None>`.
    fn field_is_optional(type_name: &str) -> bool {
        type_name
            .split("||")
            .any(|t| t.trim().eq_ignore_ascii_case("none"))
    }

    /// Check a value about to be stored in field `field` of `struct_name`,
    /// declared `type_name`. Returns the value to store: an `<int>` widens
    /// into a `<float>` field, everything else must already match. `None` is
    /// the zero value of a struct- or enum-typed field, so it stays valid
    /// there; a primitive field only takes it when declared `|| None`.
    pub(crate) fn check_field_value(
        &self,
        struct_name: &str,
        field: &str,
        type_name: &str,
        value: Value,
    ) -> Result<Value, VMError> {
        if type_name.is_empty() || type_name == "GENERIC" {
            return Ok(value);
        }
        if matches!(value, Value::None)
            && (Self::field_is_optional(type_name)
                || type_name
                    .split(" || ")
                    .any(|t| !is_builtin_type_name(t) && !t.starts_with("ERROR")))
        {
            return Ok(value);
        }
        if value.effective_type_is(type_name) || type_matches(type_name, value.type_name()) {
            return Ok(value);
        }
        if let Value::Integer(n) = value
            && type_matches(type_name, "FLOAT")
        {
            return Ok(Value::Float(n as f64));
        }
        Err(self.runtime_error_hint(
            &format!(
                "type mismatch: field '{}' of {} is {}, got {}",
                field,
                struct_name,
                type_name,
                value.effective_type_name()
            ),
            &format!("field '{}' is declared <{}>", field, type_name),
        ))
    }

    /// Check a tuple variant's payload against its declared field types.
    fn check_variant_args(
        &self,
        enum_name: &str,
        variant: &crate::vm::value::VmEnumVariantDef,
        names: &[String],
        args: Vec<Value>,
    ) -> Result<Vec<Value>, VMError> {
        let owner = format!("{}.{}", enum_name, variant.name);
        args.into_iter()
            .enumerate()
            .map(
                |(i, arg)| match (names.get(i), variant.field_types.get(i)) {
                    (Some(name), Some(ty)) => self.check_field_value(&owner, name, ty, arg),
                    _ => Ok(arg),
                },
            )
            .collect()
    }

    /// Reports a call whose argument count the signature cannot accept.
    fn arity_error(
        &self,
        function: &crate::vm::value::Function,
        got: usize,
        required: usize,
        max: usize,
    ) -> VMError {
        let name = function
            .name
            .as_deref()
            .map(|n| format!("`{n}`"))
            .unwrap_or_else(|| "this function".to_string());
        let plural = |n: usize| if n == 1 { "argument" } else { "arguments" };
        let implicit_self = usize::from(
            function.method_of.is_some()
                && function.params.first().is_some_and(|p| p.name == "self"),
        );
        let (got, required, max) = (
            got.saturating_sub(implicit_self),
            required.saturating_sub(implicit_self),
            max.saturating_sub(implicit_self),
        );

        let expectation = if required == max {
            format!("{required} {}", plural(required))
        } else {
            format!("{required} to {max} arguments")
        };
        let was = if got == 1 { "was" } else { "were" };
        let message = format!("{name} takes {expectation} but {got} {was} supplied");

        let hint = if got < required {
            let missing: Vec<String> = function
                .params
                .iter()
                .skip(got + implicit_self)
                .filter(|p| !p.has_default && !p.optional)
                .map(|p| format!("`{}`", p.name))
                .collect();
            if missing.is_empty() {
                "supply the missing arguments".to_string()
            } else {
                format!("missing {}", missing.join(", "))
            }
        } else {
            format!("remove the extra {}", plural(got - max))
        };

        self.runtime_error_coded(
            crate::diagnostics::registry::WRONG_ARGUMENT_COUNT,
            &message,
            Some(&hint),
        )
    }

    pub(crate) fn runtime_error(&self, message: &str) -> VMError {
        self.runtime_error_coded(crate::diagnostics::registry::RUNTIME_ERROR, message, None)
    }

    /// A runtime error with a specific code.
    pub(crate) fn runtime_error_coded(
        &self,
        code: crate::diagnostics::Code,
        message: &str,
        hint: Option<&str>,
    ) -> VMError {
        let line = self.current_line();
        VMError {
            message: self.format_error_coded(code, message, hint),
            line,
            code,
            tag: None,
        }
    }

    pub(crate) fn runtime_error_hint(&self, message: &str, hint: &str) -> VMError {
        self.runtime_error_coded(
            crate::diagnostics::registry::RUNTIME_ERROR,
            message,
            Some(hint),
        )
    }

    /// Map keys and set elements must have a hashable projection; non-hashable
    /// keys are refused.
    pub(crate) fn require_hashable_key(&self, key: &Value, what: &str) -> Result<(), VMError> {
        if key.try_hash_key().is_some() {
            return Ok(());
        }
        Err(self.runtime_error_coded(
            crate::diagnostics::registry::NON_HASHABLE_KEY,
            &format!("{} is not hashable and cannot be used as a {what}", key.type_name()),
            Some("hashable kinds are int, uint, float, bool, char, byte, str, None, and tuples of those"),
        ))
    }

    /// Drive the interpreter until the current frame's depth returns to
    /// `stop_depth`. The top-level entry point uses `stop_depth = 0`.
    ///
    /// On normal exit the last-popped frame's return value sits on top of
    /// the stack.
    pub(crate) fn execute_until(&mut self, stop_depth: usize) -> Result<(), VMError> {
        if self.frames.len() <= stop_depth {
            return Ok(());
        }
        let prev_jit_exec = self.jit_executing.replace(false);
        let result = loop {
            match self.execute_until_inner(stop_depth) {
                Ok(()) => break Ok(()),
                Err(e) => match self.recover_to_handler(stop_depth, e) {
                    Ok(()) => continue,
                    Err(e) => break Err(e),
                },
            }
        };
        self.jit_executing.set(prev_jit_exec);
        result
    }

    /// On a raised error, look for the innermost installed error handler that
    /// lives within the current `execute_until` scope (`frame_len > stop_depth`).
    /// If found, unwind frames + operand stack + JIT frames to its saved state,
    /// push the error as an in-band `ErrorValue`, set `ip` to the catch target,
    /// and return `Ok(())`. Otherwise return the error for propagation.
    fn recover_to_handler(&mut self, stop_depth: usize, err: VMError) -> Result<(), VMError> {
        if err.is_host_limit() {
            return Err(err);
        }
        match self.error_handlers.last().copied() {
            Some(h) if h.frame_len > stop_depth => {
                self.error_handlers.pop();
                self.frames.truncate(h.frame_len);
                self.jit_frame_view.len = h.jit_frame_len;
                self.close_upvalues(h.stack_len);
                self.stack_truncate(h.stack_len);
                self.push(Value::ErrorValue(Rc::new(ErrorValueData {
                    msg: rc_str(err.message),
                    tag: err.tag.map(rc_str),
                })));
                let fi = self.frames.len() - 1;
                self.frames[fi].ip = h.catch_ip;
                Ok(())
            }
            _ => Err(err),
        }
    }

    #[inline(always)]
    fn execute_until_inner(&mut self, stop_depth: usize) -> Result<(), VMError> {
        macro_rules! int_fast_binop {
            ($l:ident, $r:ident, $guard:expr, $fast:expr, $slow:ident) => {{
                let n = self.stack.len();
                if n >= 2
                    && let (&Value::Integer($l), &Value::Integer($r)) =
                        (&self.stack[n - 2], &self.stack[n - 1])
                    && $guard
                {
                    self.stack.pop();
                    self.stack[n - 2] = $fast;
                    self.stack_view.len = n - 1;
                } else {
                    let b = self.pop_fast();
                    let a = self.pop_fast();
                    let result = self.$slow(a, b)?;
                    self.push_fast(result);
                }
            }};
        }
        loop {
            let frame_idx = self.frames.len() - 1;

            let instruction = self.frames[frame_idx].read_byte();
            let op = match OpCode::from_byte(instruction) {
                Some(op) => op,
                None => {
                    return Err(self.runtime_error(&format!("unknown opcode: {}", instruction)));
                }
            };

            match op {
                OpCode::Constant => {
                    let idx = self.frames[frame_idx].read_u16();
                    let value = self.frames[frame_idx].read_constant(idx).fresh_clone();
                    self.push_fast(value);
                }
                OpCode::None => self.push_fast(Value::None),
                OpCode::True => self.push_fast(Value::Boolean(true)),
                OpCode::False => self.push_fast(Value::Boolean(false)),

                OpCode::Pop => {
                    self.pop_fast();
                }
                OpCode::Dup => {
                    let val = self.peek(0).clone();
                    self.push_fast(val);
                }

                OpCode::Add => {
                    int_fast_binop!(l, r, true, Value::Integer(l.wrapping_add(r)), binary_add)
                }
                OpCode::Subtract => {
                    int_fast_binop!(l, r, true, Value::Integer(l.wrapping_sub(r)), binary_sub)
                }
                OpCode::Multiply => {
                    int_fast_binop!(l, r, true, Value::Integer(l.wrapping_mul(r)), binary_mul)
                }
                OpCode::Divide => {
                    int_fast_binop!(l, r, r != 0 && r != -1, Value::Integer(l / r), binary_div)
                }
                OpCode::Modulo => {
                    int_fast_binop!(l, r, r != 0 && r != -1, Value::Integer(l % r), binary_mod)
                }

                OpCode::Equal | OpCode::NotEqual => {
                    let want = op == OpCode::Equal;
                    let n = self.stack.len();
                    if n >= 2
                        && let (&Value::Integer(l), &Value::Integer(r)) =
                            (&self.stack[n - 2], &self.stack[n - 1])
                    {
                        self.stack.pop();
                        self.stack[n - 2] = Value::Boolean((l == r) == want);
                        self.stack_view.len = n - 1;
                    } else {
                        let b = self.pop_fast();
                        let a = self.pop_fast();
                        let eq = self.values_equal_forced(a, b);
                        self.push_fast(Value::Boolean(eq == want));
                    }
                }
                OpCode::Less => int_fast_binop!(l, r, true, Value::Boolean(l < r), compare_less),
                OpCode::LessEqual => {
                    int_fast_binop!(l, r, true, Value::Boolean(l <= r), compare_less_equal)
                }
                OpCode::Greater => {
                    int_fast_binop!(l, r, true, Value::Boolean(l > r), compare_greater)
                }
                OpCode::GreaterEqual => {
                    int_fast_binop!(l, r, true, Value::Boolean(l >= r), compare_greater_equal)
                }

                OpCode::Not => {
                    let val = self.pop_fast();
                    let val = self.force(val);
                    self.push_fast(Value::Boolean(!val.is_truthy()));
                }

                OpCode::BitAnd => {
                    let b = self.pop();
                    let a = self.pop();
                    let v = self.binary_band(a, b)?;
                    self.push(v);
                }
                OpCode::BitOr => {
                    let b = self.pop();
                    let a = self.pop();
                    let v = self.binary_bor(a, b)?;
                    self.push(v);
                }
                OpCode::BitXor => {
                    let b = self.pop();
                    let a = self.pop();
                    let v = self.binary_bxor(a, b)?;
                    self.push(v);
                }
                OpCode::BitNot => {
                    let val = self.pop();
                    let v = self.unary_bnot(val)?;
                    self.push(v);
                }
                OpCode::ShiftLeft => {
                    let b = self.pop();
                    let a = self.pop();
                    let v = self.binary_shl(a, b)?;
                    self.push(v);
                }
                OpCode::ShiftRight => {
                    let b = self.pop();
                    let a = self.pop();
                    let v = self.binary_shr(a, b)?;
                    self.push(v);
                }

                OpCode::Negate => {
                    let val = self.pop();
                    let negated = self.unary_negate(val)?;
                    self.push(negated);
                }

                OpCode::Increment | OpCode::Decrement => {
                    return Err(self.runtime_error("unexpected increment/decrement opcode"));
                }

                OpCode::GetLocal => {
                    let slot = self.frames[frame_idx].read_u16() as usize;
                    let offset = self.frames[frame_idx].slot_offset;
                    let value = self.stack[offset + slot].clone();
                    self.push_fast(value);
                }
                OpCode::SetLocal => {
                    let slot = self.frames[frame_idx].read_u16() as usize;
                    self.check_local_lock(&self.frames[frame_idx].closure.function, slot)?;
                    let offset = self.frames[frame_idx].slot_offset;
                    let value = self.peek(0).clone();
                    self.stack[offset + slot] = value;
                }
                OpCode::SetLocalLocked => {
                    let op_offset = self.frames[frame_idx].ip - 1;
                    let slot = self.frames[frame_idx].read_u16() as usize;
                    let lock = self.frames[frame_idx]
                        .closure
                        .function
                        .chunk
                        .local_lock_at(op_offset)
                        .cloned();
                    if let Some(expected) = lock.as_ref().and_then(|v| v.as_string()) {
                        self.check_type_lock(expected)?;
                    }
                    let offset = self.frames[frame_idx].slot_offset;
                    let value = self.peek(0).clone();
                    self.stack[offset + slot] = value;
                }
                OpCode::RangeLimit => {
                    let idx = self.frames[frame_idx].read_u16();
                    let step = match self.frames[frame_idx].read_constant(idx) {
                        Value::Integer(step) => *step,
                        _ => return Err(self.runtime_error("invalid step constant in RangeLimit")),
                    };
                    self.handle_range_limit(step)?;
                }
                OpCode::CheckType => {
                    let idx = self.frames[frame_idx].read_u16();
                    let expected = self.frames[frame_idx].read_constant(idx).clone();
                    if let Some(expected) = expected.as_string() {
                        self.check_type_lock(expected)?;
                    }
                }
                OpCode::CheckReturn => {
                    let idx = self.frames[frame_idx].read_u16();
                    let expected = self.frames[frame_idx].read_constant(idx).clone();
                    if let Some(expected) = expected.as_string() {
                        self.check_return_type(expected)?;
                    }
                }
                OpCode::GetGlobal => {
                    let idx = self.frames[frame_idx].read_u16();
                    self.handle_get_global(idx)?;
                }
                OpCode::SetGlobal => {
                    let idx = self.frames[frame_idx].read_u16();
                    self.handle_set_global(idx)?;
                }
                OpCode::DefineGlobal => {
                    let idx = self.frames[frame_idx].read_u16();
                    self.handle_define_global(idx)?;
                }
                OpCode::DefineGlobalTyped => {
                    let name_idx = self.frames[frame_idx].read_u16();
                    let mode = self.frames[frame_idx].read_byte();
                    let type_idx = self.frames[frame_idx].read_u16();
                    self.handle_define_global_typed(name_idx, mode, type_idx)?;
                }

                OpCode::GetUpvalue => {
                    let idx = self.frames[frame_idx].read_u16();
                    self.handle_get_upvalue(idx)?;
                }
                OpCode::SetUpvalue => {
                    let idx = self.frames[frame_idx].read_u16();
                    self.handle_set_upvalue(idx)?;
                }
                OpCode::CloseUpvalue => {
                    self.handle_close_upvalue();
                }
                OpCode::CloseUpvalueAt => {
                    let slot = self.frames[frame_idx].read_u16() as usize;
                    let offset = self.frames[frame_idx].slot_offset;
                    self.close_upvalue_at_slot(offset + slot);
                }

                OpCode::Jump => {
                    let offset = self.frames[frame_idx].read_u16() as usize;
                    self.frames[frame_idx].ip += offset;
                }
                OpCode::JumpIfFalse => {
                    let offset = self.frames[frame_idx].read_u16() as usize;
                    if !self.peek(0).is_truthy() {
                        self.frames[frame_idx].ip += offset;
                    }
                }
                OpCode::JumpIfTrue => {
                    let offset = self.frames[frame_idx].read_u16() as usize;
                    if self.peek(0).is_truthy() {
                        self.frames[frame_idx].ip += offset;
                    }
                }
                OpCode::Loop => {
                    let offset = self.frames[frame_idx].read_u16() as usize;
                    if self.interrupt.0.is_armed() {
                        self.loop_limit_step()?;
                    }
                    self.maybe_compile_current_loop_hot_function(frame_idx);
                    self.frames[frame_idx].ip -= offset;
                }
                OpCode::PopJumpIfFalse => {
                    let offset = self.frames[frame_idx].read_u16() as usize;
                    let val = self.pop_fast();
                    if !val.is_truthy() {
                        self.frames[frame_idx].ip += offset;
                    }
                }

                OpCode::Closure => {
                    let fn_idx = self.frames[frame_idx].read_u16();
                    let upvalue_count = match self.frames[frame_idx].read_constant(fn_idx).repr() {
                        ValueRepr::Closure(t) => t.function.upvalue_count as usize,
                        _ => {
                            return Err(self.runtime_error("expected closure constant"));
                        }
                    };
                    let descriptors_offset = self.frames[frame_idx].ip;
                    self.frames[frame_idx].ip += 3 * upvalue_count;
                    self.handle_closure(fn_idx, descriptors_offset)?;
                }

                OpCode::Call => {
                    let arg_count = self.frames[frame_idx].read_byte() as usize;
                    self.call_value(arg_count, &[])?;
                }

                OpCode::CallNamed => {
                    let pos_count = self.frames[frame_idx].read_byte() as usize;
                    let named_count = self.frames[frame_idx].read_byte() as usize;

                    let mut named_args = Vec::with_capacity(named_count);
                    for _ in 0..named_count {
                        let val = self.pop();
                        let name = self.pop();
                        if let Some(name_str) = name.as_string() {
                            named_args.push((name_str.to_string(), val));
                        }
                    }
                    named_args.reverse();

                    self.call_value(pos_count, &named_args)?;
                }

                OpCode::Return => {
                    let result = self.pop();
                    let frame = self.frames.pop().unwrap();

                    while self
                        .error_handlers
                        .last()
                        .is_some_and(|h| h.frame_len > self.frames.len())
                    {
                        self.error_handlers.pop();
                    }

                    self.close_upvalues(frame.slot_offset);

                    self.stack_truncate(frame.slot_offset);

                    self.push(result);

                    if self.frames.len() <= stop_depth {
                        return Ok(());
                    }
                }

                OpCode::BuildArray => {
                    let count = self.frames[frame_idx].read_u16() as usize;
                    let start = self.stack.len() - count;
                    let elements: Vec<Value> = self.stack_drain_from(start);
                    self.push(Value::Array(Rc::new(RefCell::new(elements.into()))));
                }
                OpCode::BuildTuple => {
                    let count = self.frames[frame_idx].read_u16() as usize;
                    let start = self.stack.len() - count;
                    let elements: Vec<Value> = self.stack_drain_from(start);
                    self.push(Value::Tuple(Rc::new(elements.into())));
                }
                OpCode::BuildMap => {
                    let count = self.frames[frame_idx].read_u16() as usize;
                    let start = self.stack.len() - count * 2;
                    let flat: Vec<Value> = self.stack_drain_from(start);
                    let mut map = crate::vm::collections::OxMap::new();
                    for pair in flat.chunks(2) {
                        self.require_hashable_key(&pair[0], "map key")?;
                        map.insert(pair[0].clone(), pair[1].clone());
                    }
                    self.push(Value::Map(Rc::new(RefCell::new(map))));
                }
                OpCode::BuildSet => {
                    let count = self.frames[frame_idx].read_u16() as usize;
                    let start = self.stack.len() - count;
                    let elements: Vec<Value> = self.stack_drain_from(start);
                    for e in &elements {
                        self.require_hashable_key(e, "set element")?;
                    }
                    let set = crate::vm::collections::OxSet::from_iter_dedup(elements);
                    self.push(Value::Set(Rc::new(RefCell::new(set))));
                }

                OpCode::Index => {
                    let index = self.pop();
                    let collection = self.pop();
                    let result = self.eval_index(collection, index)?;
                    self.push(result);
                }

                OpCode::Lock => {
                    let value = self.pop();
                    let locked = self.lock_value(&value);
                    self.push(locked);
                }

                OpCode::IndexExisting => {
                    let index = self.pop();
                    let collection = self.pop();
                    let result = self.eval_index_existing(collection, index)?;
                    self.push(result);
                }

                OpCode::IndexAssign => {
                    let value = self.pop();
                    let index = self.pop();
                    let collection = self.pop();
                    self.eval_index_assign(collection, index, value)?;
                }

                OpCode::Slice => {
                    let flags = self.frames[frame_idx].read_byte();
                    let has_end = flags & 0x02 != 0;
                    let has_start = flags & 0x01 != 0;

                    let end = if has_end { Some(self.pop()) } else { None };
                    let start = if has_start { Some(self.pop()) } else { None };
                    let collection = self.pop();

                    let result = self.eval_slice(collection, start, end)?;
                    self.push(result);
                }

                OpCode::StructDef => {
                    let _idx = self.frames[frame_idx].read_u16();
                }

                OpCode::StructLiteral => {
                    let name_idx = self.frames[frame_idx].read_u16();
                    let field_count = self.frames[frame_idx].read_byte();
                    self.handle_struct_literal(name_idx, field_count)?;
                }

                OpCode::GetField => {
                    let field_idx = self.frames[frame_idx].read_u16();
                    self.handle_get_field(field_idx)?;
                }

                OpCode::SetField => {
                    let field_idx = self.frames[frame_idx].read_u16();
                    self.handle_set_field(field_idx)?;
                }

                OpCode::DefineMethod => {
                    let struct_name_idx = self.frames[frame_idx].read_u16();
                    let method_count = self.frames[frame_idx].read_byte();
                    self.handle_define_method(struct_name_idx, method_count)?;
                }

                OpCode::DefinePattern => {
                    let _name_idx = self.frames[frame_idx].read_u16();
                    let _param_count = self.frames[frame_idx].read_byte();
                }

                OpCode::TestPattern => {
                    let _offset = self.frames[frame_idx].read_u16();
                    let _hi = self.frames[frame_idx].read_byte();
                    let _lo = self.frames[frame_idx].read_byte();
                }

                OpCode::ErrorConstruct => {
                    let has_tag = self.frames[frame_idx].read_byte();
                    if has_tag == 1 {
                        let tag = self.pop();
                        let value = self.pop();
                        let tag_str: Rc<String> = if let Some(s) = tag.as_string() {
                            Rc::clone(s)
                        } else {
                            Rc::new(format!("{}", tag))
                        };
                        self.push(Value::ErrorValue(Rc::new(ErrorValueData {
                            msg: rc_str(format!("{}", value)),
                            tag: Some(tag_str),
                        })));
                    } else {
                        let value = self.pop();
                        self.push(Value::ErrorValue(Rc::new(ErrorValueData {
                            msg: rc_str(format!("{}", value)),
                            tag: None,
                        })));
                    }
                }

                OpCode::ValueConstruct => {
                    let value = self.pop();
                    self.push(Value::Wrapped(Rc::new(value)));
                }

                OpCode::Guard => {
                    let jump_offset = self.frames[frame_idx].read_u16() as usize;
                    self.frames[frame_idx].read_u16();
                    let tag_hi = self.frames[frame_idx].read_byte();
                    let tag_lo = self.frames[frame_idx].read_byte();
                    let tag_idx = ((tag_hi as u16) << 8) | (tag_lo as u16);

                    let value = self.peek(0).clone();
                    let err: Option<(bool, String, Option<String>)> =
                        if let Some(data) = value.as_error_value() {
                            let matches = if tag_idx == 0xFFFF {
                                true
                            } else {
                                let filter = self.frames[frame_idx].read_constant(tag_idx).clone();
                                match (filter.as_string(), data.tag.as_ref()) {
                                    (Some(f), Some(t)) => f.as_str() == t.as_str(),
                                    _ => false,
                                }
                            };
                            Some((
                                matches,
                                data.msg.to_string(),
                                data.tag.as_ref().map(|t| t.to_string()),
                            ))
                        } else {
                            None
                        };

                    match err {
                        Some((true, _, _)) => {
                            self.frames[frame_idx].ip += jump_offset;
                        }
                        Some((false, msg, tag)) => {
                            self.pop();
                            return Err(VMError {
                                message: msg,
                                line: self.current_line(),
                                code: crate::diagnostics::registry::RUNTIME_ERROR,
                                tag,
                            });
                        }
                        None => {}
                    }
                }

                OpCode::Fail => {
                    let value = self.pop();
                    match value.repr() {
                        ValueRepr::ErrorValue(data) => {
                            return Err(VMError {
                                message: data.msg.to_string(),
                                line: self.current_line(),
                                code: crate::diagnostics::registry::RUNTIME_ERROR,
                                tag: data.tag.as_ref().map(|t| t.to_string()),
                            });
                        }
                        _ => {
                            return Err(VMError {
                                message: format!("{}", value),
                                line: self.current_line(),
                                code: crate::diagnostics::registry::RUNTIME_ERROR,
                                tag: None,
                            });
                        }
                    }
                }

                OpCode::PushHandler => {
                    let offset = self.frames[frame_idx].read_u16() as usize;
                    let catch_ip = self.frames[frame_idx].ip + offset;
                    self.error_handlers.push(ErrorHandler {
                        catch_ip,
                        frame_len: self.frames.len(),
                        stack_len: self.stack.len(),
                        jit_frame_len: self.jit_frame_view.len,
                    });
                }
                OpCode::PopHandler => {
                    self.error_handlers.pop();
                }

                OpCode::Import => {
                    let path_idx = self.frames[frame_idx].read_u16();
                    let selective_count = self.frames[frame_idx].read_byte() as usize;
                    let path = self.frames[frame_idx].read_constant(path_idx).clone();

                    let mut selective_names = Vec::new();
                    for _ in 0..selective_count {
                        let name = self.pop();
                        if let Some(s) = name.as_string() {
                            selective_names.push(s.to_string());
                        }
                    }
                    selective_names.reverse();

                    if let Some(path_str) = path.as_string() {
                        self.import_module(path_str, &selective_names)?;
                    }
                }

                OpCode::GetModuleField => {
                    let field_idx = self.frames[frame_idx].read_u16();
                    let _field_name = self.frames[frame_idx].read_constant(field_idx).clone();
                }

                OpCode::StringInterp => {
                    use std::fmt::Write as _;
                    let count = self.frames[frame_idx].read_u16() as usize;
                    let start = self.stack.len() - count;
                    let parts: Vec<Value> = self.stack_drain_from(start);
                    let mut result = String::with_capacity(count * 8);
                    for part in &parts {
                        let _ = write!(result, "{}", part);
                    }
                    self.push(Value::String(rc_str(result)));
                }

                OpCode::Log => {
                    let flags = self.frames[frame_idx].read_byte();
                    self.handle_log(flags);
                }

                OpCode::Unpack => {
                    let count = self.frames[frame_idx].read_byte() as usize;
                    let value = self.pop();
                    let mismatch = |got: usize| {
                        self.runtime_error_hint(
                            &format!("expected {} values, got {}", count, got),
                            "the number of names on the left must match the number of elements",
                        )
                    };
                    match value.repr() {
                        ValueRepr::Array(arr) => {
                            let items: Vec<Value> = arr.borrow().to_vec();
                            if items.len() != count {
                                return Err(mismatch(items.len()));
                            }
                            for val in items {
                                self.push(val);
                            }
                        }
                        ValueRepr::Tuple(t) => {
                            if t.len() != count {
                                return Err(mismatch(t.len()));
                            }
                            for val in t.iter() {
                                self.push(val.clone());
                            }
                        }
                        _ => {
                            return Err(self.runtime_error_hint(
                                &format!(
                                    "can only unpack arrays and tuples, got {}",
                                    value.type_name()
                                ),
                                "unpack syntax: `a, b := [1, 2]` or `a, b := (1, 2)`",
                            ));
                        }
                    }
                }

                OpCode::Main => {
                    let offset = self.frames[frame_idx].read_u16() as usize;
                    if !self.is_main_context {
                        self.frames[frame_idx].ip += offset;
                    }
                }

                OpCode::Unless => {
                    let offset = self.frames[frame_idx].read_u16() as usize;
                    if !self.peek(0).is_truthy() {
                        self.frames[frame_idx].ip += offset;
                    }
                }

                OpCode::IterLen => {
                    let iterable = self.pop();
                    let len = self.iter_len(&iterable)?;
                    self.push(Value::Integer(len));
                }

                OpCode::TypeWrap => {
                    let type_idx = self.frames[frame_idx].read_u16();
                    let type_name = self.frames[frame_idx].read_constant(type_idx).clone();
                    let value = self.pop();

                    if let Some(target) = type_name.as_string() {
                        let result = self.type_wrap(target, value)?;
                        self.push(result);
                    } else {
                        return Err(self.runtime_error("invalid type name in TypeWrap"));
                    }
                }

                OpCode::IterGet => {
                    let index = self.pop();
                    let iterable = self.pop();
                    let val = self.iter_get(&iterable, &index)?;
                    self.push(val);
                }

                OpCode::IterEntry => {
                    let index = self.pop();
                    let iterable = self.pop();
                    let (key, val) = self.iter_entry(&iterable, &index)?;
                    self.push(key);
                    self.push(val);
                }

                OpCode::MethodCall => {
                    let method_idx = self.frames[frame_idx].read_u16();
                    let arg_count = self.frames[frame_idx].read_byte();
                    self.handle_method_call(method_idx, arg_count)?;
                }

                OpCode::MethodCallNamed => {
                    let method_idx = self.frames[frame_idx].read_u16();
                    let arg_count = self.frames[frame_idx].read_byte() as usize;
                    let named_count = self.frames[frame_idx].read_byte() as usize;
                    let method_name = self.frames[frame_idx].read_constant(method_idx).clone();

                    let mut named_args = Vec::with_capacity(named_count);
                    for _ in 0..named_count {
                        let val = self.pop();
                        let name = self.pop();
                        if let Some(name_str) = name.as_string() {
                            named_args.push((name_str.to_string(), val));
                        }
                    }
                    named_args.reverse();

                    if let Some(mname) = method_name.as_string() {
                        self.call_method(mname, arg_count, &named_args)?;
                    } else {
                        return Err(self.runtime_error("invalid method name"));
                    }
                }

                OpCode::IsMut => {
                    let name_idx = self.frames[frame_idx].read_u16();
                    let name = self.frames[frame_idx].read_constant(name_idx).clone();
                    if let Some(var_name) = name.as_string() {
                        let is_mut = self
                            .global_mutability
                            .get(var_name.as_ref())
                            .copied()
                            .unwrap_or(true);
                        self.push(Value::Boolean(is_mut));
                    } else {
                        self.push(Value::Boolean(true));
                    }
                }

                OpCode::IsType => {
                    let type_idx = self.frames[frame_idx].read_u16();
                    let type_name = self.frames[frame_idx].read_constant(type_idx).clone();
                    let value = self.pop();

                    if let Some(tname) = type_name.as_string() {
                        let matches = match (tname.as_str(), &value) {
                            ("int", Value::Integer(_)) => true,
                            ("float", Value::Float(_)) => true,
                            ("str", Value::String(_)) => true,
                            ("char", Value::Char(_)) => true,
                            ("bool", Value::Boolean(_)) => true,
                            ("array", Value::Array(_)) => true,
                            ("byte", Value::Byte(_)) => true,
                            ("uint", Value::Uint(_)) => true,
                            ("tuple", Value::Tuple(_)) => true,
                            ("map", Value::Map(_)) => true,
                            ("set", Value::Set(_)) => true,
                            ("None", Value::None) => true,
                            ("fun", Value::Closure(_)) => true,
                            ("fun", Value::Builtin(_)) => true,
                            (name, Value::StructInstance(inst)) => inst.struct_name == name,
                            _ => false,
                        };
                        self.push(Value::Boolean(matches));
                    } else {
                        self.push(Value::Boolean(false));
                    }
                }

                OpCode::IsTypeMut => {
                    let name_idx = self.frames[frame_idx].read_u16();
                    let name = self.frames[frame_idx].read_constant(name_idx).clone();
                    if let Some(var_name) = name.as_string() {
                        let has_constraint =
                            self.global_type_constraints.contains_key(var_name.as_ref());
                        self.push(Value::Boolean(!has_constraint));
                    } else {
                        self.push(Value::Boolean(true));
                    }
                }

                OpCode::EnumDef => {
                    let _idx = self.frames[frame_idx].read_u16();
                }

                OpCode::MakeEnumVariantUnit => {
                    let variant_idx = self.frames[frame_idx].read_u16();
                    let variant_name = self.frames[frame_idx].read_constant(variant_idx).clone();
                    let enum_val = self.pop();
                    if let (Value::EnumDef(def), Value::String(vname)) = (&enum_val, &variant_name)
                    {
                        let variant = def
                            .variants
                            .iter()
                            .find(|v| v.name.as_str() == vname.as_ref());
                        match variant {
                            Some(v) => {
                                if let crate::vm::value::VmEnumVariantKind::Unit(disc) = &v.kind {
                                    self.push(Value::EnumInstance(Rc::new(
                                        crate::vm::value::ObjEnumInstance {
                                            enum_name: def.name.clone(),
                                            variant_name: v.name.clone(),
                                            payload: crate::vm::value::VmEnumPayload::Unit(
                                                disc.clone(),
                                            ),
                                        },
                                    )));
                                } else {
                                    return Err(self.runtime_error(&format!(
                                        "variant {}.{} is not a unit variant",
                                        def.name, v.name
                                    )));
                                }
                            }
                            None => {
                                return Err(self.runtime_error(&format!(
                                    "enum {} has no variant '{}'",
                                    def.name, vname
                                )));
                            }
                        }
                    } else {
                        return Err(self.runtime_error(
                            "MakeEnumVariantUnit requires an EnumDef on the stack",
                        ));
                    }
                }

                OpCode::MakeEnumVariantTuple => {
                    let variant_idx = self.frames[frame_idx].read_u16();
                    let arg_count = self.frames[frame_idx].read_byte() as usize;
                    let variant_name = self.frames[frame_idx].read_constant(variant_idx).clone();
                    let start = self.stack.len() - arg_count;
                    let args: Vec<Value> = self.stack_drain_from(start);
                    let enum_val = self.pop();
                    let vname = if let Some(s) = variant_name.as_string() {
                        Rc::clone(s)
                    } else {
                        return Err(self.runtime_error("expected variant name string"));
                    };
                    if let Some(def) = enum_val.as_enum_def() {
                        let variant = def
                            .variants
                            .iter()
                            .find(|v| v.name.as_str() == vname.as_ref());
                        match variant {
                            Some(v) => {
                                if let crate::vm::value::VmEnumVariantKind::Tuple(expected) =
                                    &v.kind
                                {
                                    if expected.len() != args.len() {
                                        return Err(self.runtime_error(&format!(
                                            "tuple variant {}.{} expects {} arguments, got {}",
                                            def.name,
                                            v.name,
                                            expected.len(),
                                            args.len()
                                        )));
                                    }
                                    let args =
                                        self.check_variant_args(&def.name, v, expected, args)?;
                                    self.push(Value::EnumInstance(Rc::new(
                                        crate::vm::value::ObjEnumInstance {
                                            enum_name: def.name.clone(),
                                            variant_name: v.name.clone(),
                                            payload: crate::vm::value::VmEnumPayload::Tuple(args),
                                        },
                                    )));
                                } else {
                                    return Err(self.runtime_error(&format!(
                                        "variant {}.{} is not a tuple variant",
                                        def.name, v.name
                                    )));
                                }
                            }
                            None => {
                                return Err(self.runtime_error(&format!(
                                    "enum {} has no variant '{}'",
                                    def.name, vname
                                )));
                            }
                        }
                    } else {
                        return Err(self.runtime_error(
                            "MakeEnumVariantTuple requires an EnumDef on the stack",
                        ));
                    }
                }

                OpCode::MakeEnumVariantStruct => {
                    let variant_idx = self.frames[frame_idx].read_u16();
                    let field_count = self.frames[frame_idx].read_byte() as usize;
                    let variant_name = self.frames[frame_idx].read_constant(variant_idx).clone();
                    let start = self.stack.len() - field_count * 2;
                    let flat: Vec<Value> = self.stack_drain_from(start);
                    let mut fields: Vec<(String, Value)> = Vec::new();
                    for pair in flat.chunks(2) {
                        if let Some(fname) = pair[0].as_string() {
                            fields.push((fname.to_string(), pair[1].clone()));
                        }
                    }
                    let enum_val = self.pop();
                    let vname = if let Some(s) = variant_name.as_string() {
                        Rc::clone(s)
                    } else {
                        return Err(self.runtime_error("expected variant name string"));
                    };
                    if let Some(def) = enum_val.as_enum_def() {
                        let variant = def
                            .variants
                            .iter()
                            .find(|v| v.name.as_str() == vname.as_ref());
                        match variant {
                            Some(v) => {
                                if let crate::vm::value::VmEnumVariantKind::Struct(expected) =
                                    &v.kind
                                {
                                    if expected.len() != fields.len() {
                                        return Err(self.runtime_error(&format!(
                                            "struct variant {}.{} expects {} fields, got {}",
                                            def.name,
                                            v.name,
                                            expected.len(),
                                            fields.len()
                                        )));
                                    }
                                    for (fname, value) in fields.iter_mut() {
                                        let Some(i) = expected.iter().position(|n| n == fname)
                                        else {
                                            return Err(self.runtime_error(&format!(
                                                "struct variant {}.{} has no field '{}'",
                                                def.name, v.name, fname
                                            )));
                                        };
                                        if let Some(ty) = v.field_types.get(i) {
                                            let owner = format!("{}.{}", def.name, v.name);
                                            let checked = std::mem::replace(value, Value::None);
                                            *value =
                                                self.check_field_value(&owner, fname, ty, checked)?;
                                        }
                                    }
                                    self.push(Value::EnumInstance(Rc::new(
                                        crate::vm::value::ObjEnumInstance {
                                            enum_name: def.name.clone(),
                                            variant_name: v.name.clone(),
                                            payload: crate::vm::value::VmEnumPayload::Struct(
                                                fields,
                                            ),
                                        },
                                    )));
                                } else {
                                    return Err(self.runtime_error(&format!(
                                        "variant {}.{} is not a struct variant",
                                        def.name, v.name
                                    )));
                                }
                            }
                            None => {
                                return Err(self.runtime_error(&format!(
                                    "enum {} has no variant '{}'",
                                    def.name, vname
                                )));
                            }
                        }
                    } else if let Some(m) = enum_val.as_module() {
                        let binding = m.name.rsplit('/').next().unwrap_or(&m.name);
                        let binding = binding.strip_suffix(".oxi").unwrap_or(binding);
                        let name = format!("{binding}.{vname}");
                        let def = self.struct_def_in_module(m, &vname, &name)?;
                        self.build_struct_literal(def, &flat)?;
                    } else {
                        return Err(self.runtime_error(
                            "MakeEnumVariantStruct requires an EnumDef on the stack",
                        ));
                    }
                }
            }
        }
    }

    /// Auto-join: a spawned task handle resolves to its result when consumed.
    /// Called at every value-consuming site (arithmetic, bitwise, shift,
    /// comparison, equality, unary, and indexing). Builtins (`converge`,
    /// `cancel`) do not force; they receive the raw handle.
    pub(crate) fn force(&self, v: Value) -> Value {
        match v {
            Value::Task(h) => h.join(self),
            v => v,
        }
    }

    /// A set type stays locked; only `<generic>` or untyped carries no
    /// constraint. Checks the value about to be stored in `slot` (the top of
    /// the stack) against `function`'s lock for it.
    pub(crate) fn check_local_lock(&self, function: &Function, slot: usize) -> Result<(), VMError> {
        let Some(expected) = function
            .locals
            .get(slot)
            .and_then(|li| li.type_constraint.as_deref())
        else {
            return Ok(());
        };
        self.check_type_lock(expected)
    }

    /// The lock check itself: the value on top of the stack must have type
    /// `expected`.
    pub(crate) fn check_type_lock(&self, expected: &str) -> Result<(), VMError> {
        let value = self.peek(0);
        if value.effective_type_is(expected) {
            return Ok(());
        }
        let actual = value.effective_type_name();
        Err(self.runtime_error_hint(
            &format!("type mismatch: expected {}, got {}", expected, actual),
            &format!("variable is locked to type {}", expected),
        ))
    }

    /// `CheckReturn`: the value on top of the stack (left in place) against
    /// the running function's declared return type. `<Enum>` accepts any enum
    /// value.
    pub(crate) fn check_return_type(&self, expected: &str) -> Result<(), VMError> {
        let value = self.peek(0);
        if value.effective_type_is(expected) || type_matches(expected, value.type_name()) {
            return Ok(());
        }
        let actual = value.effective_type_name();
        let name = self.current_function_name();
        let subject = match name.as_deref() {
            Some(name) => format!("'{}'", name),
            None => "anonymous function".to_string(),
        };
        Err(self.runtime_error_hint(
            &format!(
                "return type mismatch in {}: expected {}, got {}",
                subject, expected, actual
            ),
            &format!("{} declares return type {}", subject, expected),
        ))
    }

    /// Name of the function running right now, under the interpreter or the
    /// JIT. `None` for an anonymous function or the top-level script.
    fn current_function_name(&self) -> Option<String> {
        if self.jit_executing.get()
            && let Some(frame) = self.jit_frame_top()
        {
            let closure = unsafe { &*frame.closure_raw };
            return closure.function.name.clone();
        }
        self.frames.last()?.closure.function.name.clone()
    }

    /// Unary `-`, shared by the interpreter and the JIT.
    pub(crate) fn unary_negate(&self, v: Value) -> Result<Value, VMError> {
        let v = self.force(v);
        match v.repr() {
            ValueRepr::Integer(n) => Ok(Value::Integer(n.wrapping_neg())),
            ValueRepr::Float(f) => Ok(Value::Float(-f)),
            ValueRepr::Vector(vector) => Ok(vector.map(|c| -c).into_value()),
            _ => Err(self.runtime_error_hint(
                &format!(
                    "negation is only supported for numbers, got {}",
                    v.type_name()
                ),
                "only <int>, <float> and vector values can be negated",
            )),
        }
    }

    pub(crate) fn binary_add(&self, a: Value, b: Value) -> Result<Value, VMError> {
        let (a, b) = (self.force(a), self.force(b));
        match (&a, &b) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l.wrapping_add(*r))),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l + r)),
            (Value::Integer(l), Value::Float(r)) => Ok(Value::Float(*l as f64 + r)),
            (Value::Float(l), Value::Integer(r)) => Ok(Value::Float(l + *r as f64)),
            (Value::String(l), Value::String(r)) => {
                let mut s = String::with_capacity(l.len() + r.len());
                s.push_str(l);
                s.push_str(r);
                Ok(Value::String(rc_str(s)))
            }
            (Value::Byte(l), Value::Byte(r)) => {
                Ok(Value::Integer((*l as i64).wrapping_add(*r as i64)))
            }
            (Value::Byte(l), Value::Integer(r)) => Ok(Value::Integer((*l as i64).wrapping_add(*r))),
            (Value::Integer(l), Value::Byte(r)) => Ok(Value::Integer((*l).wrapping_add(*r as i64))),
            (Value::Uint(l), Value::Uint(r)) => Ok(Value::Uint(l.wrapping_add(*r))),
            (Value::Uint(l), Value::Integer(r)) => Ok(Value::Integer((*l as i64).wrapping_add(*r))),
            (Value::Integer(l), Value::Uint(r)) => Ok(Value::Integer((*l).wrapping_add(*r as i64))),
            (Value::Byte(l), Value::Float(r)) => Ok(Value::Float(*l as f64 + r)),
            (Value::Float(l), Value::Byte(r)) => Ok(Value::Float(l + *r as f64)),
            (Value::Uint(l), Value::Float(r)) => Ok(Value::Float(*l as f64 + r)),
            (Value::Float(l), Value::Uint(r)) => Ok(Value::Float(l + *r as f64)),
            (Value::String(l), Value::Char(r)) => {
                let mut s = String::with_capacity(l.len() + r.len_utf8());
                s.push_str(l);
                s.push(*r);
                Ok(Value::String(rc_str(s)))
            }
            (Value::Char(l), Value::String(r)) => {
                let mut s = String::with_capacity(l.len_utf8() + r.len());
                s.push(*l);
                s.push_str(r);
                Ok(Value::String(rc_str(s)))
            }
            (Value::Char(l), Value::Char(r)) => {
                let mut s = String::with_capacity(l.len_utf8() + r.len_utf8());
                s.push(*l);
                s.push(*r);
                Ok(Value::String(rc_str(s)))
            }
            (Value::Tuple(l), Value::Tuple(r)) => {
                let mut new = (**l).clone();
                new.extend((**r).clone());
                Ok(Value::Tuple(Rc::new(new)))
            }
            _ if let Some((a, b)) = promote_numeric(&a, &b) => self.binary_add(a, b),
            _ => match vector::arithmetic(&a, &b, '+') {
                Some(result) => result.map_err(|message| self.runtime_error(&message)),
                None => Err(self.runtime_error_hint(
                    &format!("type mismatch: {} + {}", a.type_name(), b.type_name()),
                    "operands must be the same type for this operator",
                )),
            },
        }
    }

    pub(crate) fn binary_sub(&self, a: Value, b: Value) -> Result<Value, VMError> {
        let (a, b) = (self.force(a), self.force(b));
        match (&a, &b) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l.wrapping_sub(*r))),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l - r)),
            (Value::Integer(l), Value::Float(r)) => Ok(Value::Float(*l as f64 - r)),
            (Value::Float(l), Value::Integer(r)) => Ok(Value::Float(l - *r as f64)),
            (Value::Byte(l), Value::Byte(r)) => {
                Ok(Value::Integer((*l as i64).wrapping_sub(*r as i64)))
            }
            (Value::Byte(l), Value::Integer(r)) => Ok(Value::Integer((*l as i64).wrapping_sub(*r))),
            (Value::Integer(l), Value::Byte(r)) => Ok(Value::Integer((*l).wrapping_sub(*r as i64))),
            (Value::Uint(l), Value::Uint(r)) => {
                if r > l {
                    Err(self.runtime_error("unsigned integer underflow"))
                } else {
                    Ok(Value::Uint(l - r))
                }
            }
            (Value::Uint(l), Value::Integer(r)) => Ok(Value::Integer((*l as i64).wrapping_sub(*r))),
            (Value::Integer(l), Value::Uint(r)) => Ok(Value::Integer((*l).wrapping_sub(*r as i64))),
            _ if let Some((a, b)) = promote_numeric(&a, &b) => self.binary_sub(a, b),
            _ => match vector::arithmetic(&a, &b, '-') {
                Some(result) => result.map_err(|message| self.runtime_error(&message)),
                None => Err(self.runtime_error_hint(
                    &format!("type mismatch: {} - {}", a.type_name(), b.type_name()),
                    "operands must be the same type for this operator",
                )),
            },
        }
    }

    pub(crate) fn binary_mul(&self, a: Value, b: Value) -> Result<Value, VMError> {
        let (a, b) = (self.force(a), self.force(b));
        match (&a, &b) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l.wrapping_mul(*r))),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l * r)),
            (Value::Integer(l), Value::Float(r)) => Ok(Value::Float(*l as f64 * r)),
            (Value::Float(l), Value::Integer(r)) => Ok(Value::Float(l * *r as f64)),
            (Value::Byte(l), Value::Byte(r)) => {
                Ok(Value::Integer((*l as i64).wrapping_mul(*r as i64)))
            }
            (Value::Uint(l), Value::Uint(r)) => Ok(Value::Uint(l.wrapping_mul(*r))),
            _ if let Some((a, b)) = promote_numeric(&a, &b) => self.binary_mul(a, b),
            _ => match vector::arithmetic(&a, &b, '*') {
                Some(result) => result.map_err(|message| self.runtime_error(&message)),
                None => Err(self.runtime_error_hint(
                    &format!("type mismatch: {} * {}", a.type_name(), b.type_name()),
                    "operands must be the same type for this operator",
                )),
            },
        }
    }

    /// The one division-by-zero error, shared by every numeric type.
    fn division_by_zero(&self) -> VMError {
        self.runtime_error_hint(
            "division by zero",
            "ensure the divisor is not zero before dividing",
        )
    }

    fn modulo_by_zero(&self) -> VMError {
        self.runtime_error_hint(
            "modulo by zero",
            "ensure the divisor is not zero before using %",
        )
    }

    /// `i64::MIN / -1` (and `%`) has no representable result; this reports it
    /// as an error with the same shape (and code) as `division_by_zero`.
    fn division_overflow(&self, op: char) -> VMError {
        self.runtime_error_hint(
            &format!("integer overflow: int_min {op} -1 has no int result"),
            "the smallest int has no positive counterpart; special-case this divisor",
        )
    }

    pub(crate) fn binary_div(&self, a: Value, b: Value) -> Result<Value, VMError> {
        let (a, b) = (self.force(a), self.force(b));
        match (&a, &b) {
            (Value::Integer(l), Value::Integer(r)) => {
                if *r == 0 {
                    Err(self.division_by_zero())
                } else {
                    l.checked_div(*r)
                        .map(Value::Integer)
                        .ok_or_else(|| self.division_overflow('/'))
                }
            }
            (Value::Float(l), Value::Float(r)) => {
                if *r == 0.0 {
                    Err(self.division_by_zero())
                } else {
                    Ok(Value::Float(l / r))
                }
            }
            (Value::Integer(l), Value::Float(r)) => {
                if *r == 0.0 {
                    Err(self.division_by_zero())
                } else {
                    Ok(Value::Float(*l as f64 / r))
                }
            }
            (Value::Float(l), Value::Integer(r)) => {
                if *r == 0 {
                    Err(self.division_by_zero())
                } else {
                    Ok(Value::Float(l / *r as f64))
                }
            }
            (Value::Uint(l), Value::Uint(r)) => {
                if *r == 0 {
                    Err(self.division_by_zero())
                } else {
                    Ok(Value::Uint(l / r))
                }
            }
            _ if let Some((a, b)) = promote_numeric(&a, &b) => self.binary_div(a, b),
            _ => match vector::arithmetic(&a, &b, '/') {
                Some(result) => result.map_err(|message| self.runtime_error(&message)),
                None => Err(self.runtime_error_hint(
                    &format!("type mismatch: {} / {}", a.type_name(), b.type_name()),
                    "operands must be the same type for this operator",
                )),
            },
        }
    }

    pub(crate) fn binary_mod(&self, a: Value, b: Value) -> Result<Value, VMError> {
        let (a, b) = (self.force(a), self.force(b));
        match (&a, &b) {
            (Value::Integer(l), Value::Integer(r)) => {
                if *r == 0 {
                    Err(self.modulo_by_zero())
                } else {
                    l.checked_rem(*r)
                        .map(Value::Integer)
                        .ok_or_else(|| self.division_overflow('%'))
                }
            }
            (Value::Float(l), Value::Float(r)) => {
                if *r == 0.0 {
                    Err(self.modulo_by_zero())
                } else {
                    Ok(Value::Float(l % r))
                }
            }
            (Value::Integer(l), Value::Float(r)) => {
                if *r == 0.0 {
                    Err(self.modulo_by_zero())
                } else {
                    Ok(Value::Float(*l as f64 % r))
                }
            }
            (Value::Float(l), Value::Integer(r)) => {
                if *r == 0 {
                    Err(self.modulo_by_zero())
                } else {
                    Ok(Value::Float(l % *r as f64))
                }
            }
            (Value::Uint(l), Value::Uint(r)) => {
                if *r == 0 {
                    Err(self.modulo_by_zero())
                } else {
                    Ok(Value::Uint(l % r))
                }
            }
            _ if let Some((a, b)) = promote_numeric(&a, &b) => self.binary_mod(a, b),
            _ => Err(self.runtime_error_hint(
                &format!("type mismatch: {} % {}", a.type_name(), b.type_name()),
                "operands must be the same type for this operator",
            )),
        }
    }

    pub(crate) fn binary_band(&self, a: Value, b: Value) -> Result<Value, VMError> {
        let (a, b) = (self.force(a), self.force(b));
        match (&a, &b) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l & r)),
            _ => Err(self.runtime_error("bitwise & requires integers")),
        }
    }

    pub(crate) fn binary_bor(&self, a: Value, b: Value) -> Result<Value, VMError> {
        let (a, b) = (self.force(a), self.force(b));
        match (&a, &b) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l | r)),
            _ => Err(self.runtime_error("bitwise | requires integers")),
        }
    }

    pub(crate) fn binary_bxor(&self, a: Value, b: Value) -> Result<Value, VMError> {
        let (a, b) = (self.force(a), self.force(b));
        match (&a, &b) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l ^ r)),
            _ => Err(self.runtime_error("bitwise ^ requires integers")),
        }
    }

    pub(crate) fn unary_bnot(&self, a: Value) -> Result<Value, VMError> {
        let a = self.force(a);
        match a.repr() {
            ValueRepr::Integer(n) => Ok(Value::Integer(!n)),
            _ => Err(self.runtime_error("bitwise ~ requires an integer")),
        }
    }

    pub(crate) fn binary_shl(&self, a: Value, b: Value) -> Result<Value, VMError> {
        let (a, b) = (self.force(a), self.force(b));
        match (&a, &b) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l.wrapping_shl(*r as u32))),
            _ => Err(self.runtime_error("<< requires integers")),
        }
    }

    pub(crate) fn binary_shr(&self, a: Value, b: Value) -> Result<Value, VMError> {
        let (a, b) = (self.force(a), self.force(b));
        match (&a, &b) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l.wrapping_shr(*r as u32))),
            _ => Err(self.runtime_error(">> requires integers")),
        }
    }

    /// Equality for the `==` / `!=` opcodes.
    ///
    /// `Value::eq` is strict (it backs map-key hashing, so `2` and `2.0` are
    /// distinct keys). The `==` operator coerces across numeric kinds like the
    /// ordering operators do:
    /// `2 == 2.0` is True, `2 == 3.0` is False, `byte(1) == 1` is True,
    /// `uint(1) == 1` is True. Non-numeric cross-kind pairs (`1 == "1"`)
    /// are unequal, and same-kind pairs fall through to the strict `Value::eq`.
    pub(crate) fn values_equal(a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Integer(l), Value::Float(r)) => (*l as f64) == *r,
            (Value::Float(l), Value::Integer(r)) => *l == (*r as f64),
            (Value::Byte(l), Value::Integer(r)) => (*l as i64) == *r,
            (Value::Integer(l), Value::Byte(r)) => *l == (*r as i64),
            (Value::Byte(l), Value::Float(r)) => (*l as f64) == *r,
            (Value::Float(l), Value::Byte(r)) => *l == (*r as f64),
            (Value::Uint(l), Value::Integer(r)) => (*l as i64) == *r,
            (Value::Integer(l), Value::Uint(r)) => *l == (*r as i64),
            (Value::Uint(l), Value::Float(r)) => (*l as f64) == *r,
            (Value::Float(l), Value::Uint(r)) => *l == (*r as f64),
            (Value::Uint(l), Value::Byte(r)) => *l == (*r as u64),
            (Value::Byte(l), Value::Uint(r)) => (*l as u64) == *r,
            _ => a == b,
        }
    }

    /// `==`/`!=` with auto-join.
    pub(crate) fn values_equal_forced(&self, a: Value, b: Value) -> bool {
        let (a, b) = (self.force(a), self.force(b));
        Self::values_equal(&a, &b)
    }

    pub(crate) fn compare_less(&self, a: Value, b: Value) -> Result<Value, VMError> {
        self.compare(a, b, "<", Ordering::is_lt)
    }

    pub(crate) fn compare_less_equal(&self, a: Value, b: Value) -> Result<Value, VMError> {
        self.compare(a, b, "<=", Ordering::is_le)
    }

    pub(crate) fn compare_greater(&self, a: Value, b: Value) -> Result<Value, VMError> {
        self.compare(a, b, ">", Ordering::is_gt)
    }

    pub(crate) fn compare_greater_equal(&self, a: Value, b: Value) -> Result<Value, VMError> {
        self.compare(a, b, ">=", Ordering::is_ge)
    }

    /// `a symbol b` for the four ordering operators, which all accept the
    /// same operands (see [`order`]). A float NaN orders with nothing, so
    /// every comparison involving one is false.
    fn compare(
        &self,
        a: Value,
        b: Value,
        symbol: &str,
        holds: fn(Ordering) -> bool,
    ) -> Result<Value, VMError> {
        let (a, b) = (self.force(a), self.force(b));
        match order(&a, &b) {
            Some(ordering) => Ok(Value::Boolean(ordering.is_some_and(holds))),
            None => Err(self.runtime_error_hint(
                &format!(
                    "cannot compare {} {} {}",
                    a.type_name(),
                    symbol,
                    b.type_name()
                ),
                "comparison requires numbers, strings or chars on both sides",
            )),
        }
    }

    pub(crate) fn call_value(
        &mut self,
        arg_count: usize,
        named_args: &[(String, Value)],
    ) -> Result<(), VMError> {
        let callee_idx = self.stack.len() - 1 - arg_count;
        let callee = self.stack[callee_idx].clone();

        match callee.repr() {
            ValueRepr::Closure(closure) => {
                let module_globals = closure.module_globals.get();
                self.call_closure(Rc::clone(closure), arg_count, named_args, module_globals)
            }
            ValueRepr::Native(native) => {
                if !named_args.is_empty() {
                    return Err(self.runtime_error(
                        "named arguments are not supported for built-in functions",
                    ));
                }
                let start = self.stack.len() - arg_count;
                let outer = ffi::CURRENT_VM.replace(self as *mut VM);
                let result = (native.func)(&self.stack[start..]);
                ffi::CURRENT_VM.set(outer);
                self.stack_truncate(start - 1);
                match result {
                    Ok(value) => {
                        self.push(value);
                        Ok(())
                    }
                    Err(message) => Err(self.runtime_error(&message)),
                }
            }
            ValueRepr::Builtin(func) => {
                if !named_args.is_empty() {
                    return Err(self.runtime_error(
                        "named arguments are not supported for built-in functions",
                    ));
                }

                let start = self.stack.len() - arg_count;
                let is_spawn = std::ptr::fn_addr_eq(
                    func,
                    crate::concurrent::builtin_spawn as fn(&[Value]) -> Value,
                );
                if is_spawn {
                    self.snapshot_spawn_closure(start);
                }
                let result = if is_spawn {
                    crate::concurrent::spawn_with_vm(self, &self.stack[start..])
                } else if let Some(with_vm) = crate::concurrent::with_vm(func) {
                    with_vm(self, &self.stack[start..])
                } else {
                    func(&self.stack[start..])
                };
                self.stack_truncate(start - 1);
                if let Value::Error(ref msg) = result {
                    return Err(VMError {
                        message: msg.to_string(),
                        line: self.current_line(),
                        code: crate::diagnostics::registry::RUNTIME_ERROR,
                        tag: None,
                    });
                }
                self.push(result);
                Ok(())
            }
            ValueRepr::StructDef(def) => {
                let start = self.stack.len() - arg_count;
                let args: Vec<Value> = self.stack_drain_from(start);
                self.pop();

                let def_name = def.name.clone();
                let layout = self.field_layout_of(def)?;
                let mut field_vec: Vec<Value> = Vec::with_capacity(layout.slots.len());
                let mut missing: Vec<String> = Vec::new();
                let outside = self
                    .active_closure_method_of()
                    .is_none_or(|owner| !self.struct_chain_contains(&def_name, owner));
                for (i, slot) in layout.slots.iter().enumerate() {
                    let supplied_here =
                        i < args.len() || named_args.iter().any(|(n, _)| *n == slot.0);
                    if slot.2 && supplied_here && outside {
                        return Err(self.runtime_error_hint(
                            &format!(
                                "field '{}' is hidden on {} and cannot be set from outside",
                                slot.0, def_name
                            ),
                            "declare the value (`x <Struct>`) and set it through a method of the struct",
                        ));
                    }
                    let val = if i < args.len() {
                        self.check_field_value(&def_name, &slot.0, &slot.1, args[i].clone())?
                    } else if let Some((_, v)) = named_args.iter().find(|(n, _)| *n == slot.0) {
                        self.check_field_value(&def_name, &slot.0, &slot.1, v.clone())?
                    } else {
                        if !Self::field_is_optional(&slot.1) {
                            missing.push(format!("`{}`", slot.0));
                        }
                        Self::zero_value_for_type(&slot.1)
                    };
                    field_vec.push(val);
                }
                let supplied = args.len() + named_args.len();
                if supplied > 0 && !missing.is_empty() {
                    let plural = if missing.len() == 1 {
                        "field"
                    } else {
                        "fields"
                    };
                    return Err(self.runtime_error_coded(
                        crate::diagnostics::registry::STRUCT_MISSING_FIELDS,
                        &format!("{} is missing {} {}", def_name, plural, missing.join(", ")),
                        Some(
                            "give every field a value, or none at all for zero-value \
                             initialization",
                        ),
                    ));
                }
                if args.len() > layout.slots.len() {
                    return Err(self.runtime_error_coded(
                        crate::diagnostics::registry::STRUCT_MISSING_FIELDS,
                        &format!(
                            "{} takes {} fields but {} were supplied",
                            def_name,
                            layout.slots.len(),
                            args.len()
                        ),
                        Some("remove the extra arguments"),
                    ));
                }

                self.push(Value::StructInstance(Rc::new(ObjStructInstance::new(
                    def_name,
                    field_vec,
                    layout,
                    Rc::clone(def),
                ))));
                Ok(())
            }
            _ => Err(self.runtime_error_hint(
                &format!("cannot call {}", callee.type_name()),
                "only functions, closures, builtins, and struct constructors are callable",
            )),
        }
    }

    /// Fast positional-only closure call used by the JIT `Call` helper.
    /// Returns `Ok(false)` when the callee is not a closure.
    #[cfg_attr(not(feature = "jit"), allow(dead_code))]
    #[inline(always)]
    pub(crate) fn call_closure_from_stack(&mut self, arg_count: usize) -> Result<bool, VMError> {
        let callee_idx = self.stack.len() - 1 - arg_count;
        let Value::Closure(closure) = &self.stack[callee_idx] else {
            return Ok(false);
        };
        let closure = Rc::clone(closure);
        if closure.jit_state.get() == 1 && closure.function.arity as usize == arg_count {
            self.check_param_types(&closure.function)?;
            if let Some(result) = self.call_closure_fast_path(&closure) {
                return result.map(|_| true);
            }
        }
        self.call_closure(closure, arg_count, &[], None)?;
        Ok(true)
    }

    /// Dispatch a struct method with a pre-resolved closure. Rearranges
    /// the stack so `self` is the first arg, then calls the closure.
    /// Stack on entry: `[..., instance, arg1, ..., argN]`.
    /// Stack on exit (after the callee returns): `[..., result]`.
    #[cfg_attr(not(feature = "jit"), allow(dead_code))]
    #[inline(always)]
    pub(crate) fn call_struct_method_with_closure(
        &mut self,
        closure: Rc<ObjClosure>,
        arg_count: usize,
        module_globals: Option<Rc<HashMap<String, Value>>>,
    ) -> Result<(), VMError> {
        let instance_idx = self.stack.len() - 1 - arg_count;
        let instance = self.stack[instance_idx].clone();
        self.stack[instance_idx] = Value::Closure(Rc::clone(&closure));
        self.stack_insert(instance_idx + 1, instance)?;
        self.call_closure(closure, arg_count + 1, &[], module_globals)
    }

    /// Call a method on an object. Stack: [instance, arg1, ..., argN]
    pub(crate) fn call_method(
        &mut self,
        method_name: &str,
        arg_count: usize,
        named_args: &[(String, Value)],
    ) -> Result<(), VMError> {
        let instance_idx = self.stack.len() - 1 - arg_count;
        let instance = self.stack[instance_idx].clone();

        match instance.repr() {
            ValueRepr::StructInstance(inst) => {
                let field_val = if let Some(&idx) = inst.layout.indices.get(method_name) {
                    Some(inst.get_field(idx))
                } else {
                    None
                };
                if let Some(closure) = field_val.as_ref().and_then(|v| v.as_closure()).cloned() {
                    self.stack[instance_idx] = Value::Closure(Rc::clone(&closure));
                    return self.call_closure(closure, arg_count, named_args, None);
                }

                let method = self
                    .find_method_on_def(&inst.def, method_name)
                    .ok_or_else(|| {
                        self.runtime_error_hint(
                            &format!("method '{}' not found on {}", method_name, inst.struct_name),
                            "check the struct definition and its parent for available methods",
                        )
                    })?;

                let owning_module_globals = inst.def.module_globals.get();

                if let Some(closure) = method.as_closure().cloned() {
                    self.stack[instance_idx] = Value::Closure(Rc::clone(&closure));
                    self.stack_insert(instance_idx + 1, instance)?;
                    return self.call_closure(
                        closure,
                        arg_count + 1,
                        named_args,
                        owning_module_globals,
                    );
                }

                Err(self.runtime_error_hint(
                    &format!("method '{}' not found on {}", method_name, inst.struct_name),
                    "check the struct definition for available methods",
                ))
            }
            ValueRepr::Array(_)
            | ValueRepr::String(_)
            | ValueRepr::Map(_)
            | ValueRepr::Set(_)
            | ValueRepr::Tuple(_) => {
                let builtin_name = format!("__{}", method_name);
                if let Some(builtin) = self.globals.get(&builtin_name).cloned() {
                    self.stack[instance_idx] = builtin;
                    self.stack_insert(instance_idx + 1, instance)?;
                    return self.call_value(arg_count + 1, named_args);
                }
                if let Some(builtin) = self.globals.get(method_name).cloned() {
                    self.stack[instance_idx] = builtin;
                    self.stack_insert(instance_idx + 1, instance)?;
                    return self.call_value(arg_count + 1, named_args);
                }
                Err(self.runtime_error_hint(
                    &format!(
                        "method '{}' not found on {}",
                        method_name,
                        instance.type_name()
                    ),
                    "check available built-in methods for this type",
                ))
            }
            ValueRepr::Module(m) => {
                if m.hidden.contains(method_name) {
                    return Err(self.runtime_error_hint(
                        &format!("'{}' is hidden inside module '{}'", method_name, m.name),
                        "it is declared `hide fun` — call it through a function the module exposes",
                    ));
                }
                if let Some(func) = m.globals.get(method_name).cloned() {
                    self.stack[instance_idx] = func.clone();
                    if let Some(closure) = func.as_closure().cloned() {
                        return self.call_closure(
                            closure,
                            arg_count,
                            named_args,
                            Some(Rc::clone(&m.globals)),
                        );
                    }
                    return self.call_value(arg_count, named_args);
                }
                Err(self.runtime_error_hint(
                    &format!("module '{}' has no function '{}'", m.name, method_name),
                    "check the module's public API for available functions",
                ))
            }
            ValueRepr::EnumDef(def) => {
                let variant = def.variants.iter().find(|v| v.name == method_name).cloned();
                match variant {
                    Some(v) => match v.kind {
                        crate::vm::value::VmEnumVariantKind::Tuple(ref expected) => {
                            if expected.len() != arg_count {
                                return Err(self.runtime_error(&format!(
                                    "tuple variant {}.{} expects {} arguments, got {}",
                                    def.name,
                                    v.name,
                                    expected.len(),
                                    arg_count
                                )));
                            }
                            let start = self.stack.len() - arg_count;
                            let args: Vec<Value> = self.stack_drain_from(start);
                            self.pop();
                            let args = self.check_variant_args(&def.name, &v, expected, args)?;
                            self.push(Value::EnumInstance(Rc::new(
                                crate::vm::value::ObjEnumInstance {
                                    enum_name: def.name.clone(),
                                    variant_name: v.name.clone(),
                                    payload: crate::vm::value::VmEnumPayload::Tuple(args),
                                },
                            )));
                            Ok(())
                        }
                        crate::vm::value::VmEnumVariantKind::Unit(_) => Err(self
                            .runtime_error_hint(
                                &format!("unit variant {}.{} cannot be called", def.name, v.name),
                                "access unit variants without parentheses",
                            )),
                        crate::vm::value::VmEnumVariantKind::Struct(_) => Err(self
                            .runtime_error_hint(
                                &format!(
                                    "struct variant {}.{} must be constructed with braces",
                                    def.name, v.name
                                ),
                                "use EnumName.Variant { field: value }",
                            )),
                    },
                    None => Err(self.runtime_error(&format!(
                        "enum {} has no variant '{}'",
                        def.name, method_name
                    ))),
                }
            }
            _ => Err(self.runtime_error_hint(
                &format!(
                    "cannot call method '{}' on {}",
                    method_name,
                    instance.type_name()
                ),
                "methods can only be called on struct instances, collections, and modules",
            )),
        }
    }

    /// Resolve a struct def by name (from globals).
    #[cfg_attr(not(feature = "jit"), allow(dead_code))]
    #[inline(always)]
    pub(crate) fn get_struct_def(&self, name: &str) -> Option<Rc<crate::vm::value::ObjStructDef>> {
        if let Some(Value::StructDef(def)) = self.globals.get(name) {
            return Some(Rc::clone(def));
        }
        if let Some(mg) = self.active_module_globals()
            && let Some(Value::StructDef(def)) = mg.get(name)
        {
            return Some(Rc::clone(def));
        }
        None
    }

    /// Resolve a struct def's parent def. The parent is looked up in the
    /// defining module's globals first, then in the active scope.
    pub(crate) fn parent_struct_def(
        &self,
        def: &Rc<crate::vm::value::ObjStructDef>,
    ) -> Option<Rc<crate::vm::value::ObjStructDef>> {
        let parent = def.parent.as_ref()?;
        if let Some(mg) = def.module_globals.get()
            && let Some(Value::StructDef(pd)) = mg.get(parent)
        {
            return Some(Rc::clone(pd));
        }
        self.get_struct_def(parent)
    }

    /// Look up a method on a struct def, following the inheritance chain.
    #[cfg_attr(not(feature = "jit"), allow(dead_code))]
    /// Resolve a method starting from a struct instance's carried def, walking
    /// the inheritance chain. Parent defs are resolved via the def's own module
    /// globals first, then the current scope's globals. Works for instances used
    /// outside their defining module.
    fn find_method_on_def(
        &self,
        def: &Rc<crate::vm::value::ObjStructDef>,
        method_name: &str,
    ) -> Option<Value> {
        let mut cur = Some(Rc::clone(def));
        while let Some(d) = cur {
            if let Some(m) = d.methods.borrow().get(method_name).cloned() {
                return Some(m);
            }
            cur = self.parent_struct_def(&d);
        }
        None
    }

    #[cfg(feature = "jit")]
    pub(crate) fn find_struct_method(
        &self,
        struct_name: &str,
        method_name: &str,
    ) -> Result<Value, VMError> {
        let mut current = struct_name.to_string();
        loop {
            if let Some(def) = self.get_struct_def(&current) {
                let methods = def.methods.borrow();
                if let Some(method) = methods.get(method_name) {
                    return Ok(method.clone());
                }
                if let Some(parent) = &def.parent {
                    current = parent.clone();
                    continue;
                }
            }
            return Err(self.runtime_error_hint(
                &format!("method '{}' not found on {}", method_name, struct_name),
                "check the struct definition and its parent for available methods",
            ));
        }
    }

    /// Check the `arity` arguments on top of the stack against `function`'s
    /// parameter annotations. Every call path runs this before entering the
    /// callee. A None in a defaulted slot is skipped.
    #[inline(always)]
    pub(crate) fn check_param_types(&self, function: &Function) -> Result<(), VMError> {
        let expected = function.arity as usize;
        if expected == 0 || !function.has_typed_params() {
            return Ok(());
        }
        let slot_base = self.stack.len() - expected;
        for (i, param) in function.params.iter().enumerate() {
            let Some(expected_ty) = param.type_ann.as_deref() else {
                continue;
            };
            let val = &self.stack[slot_base + i];
            if (param.optional || param.has_default) && matches!(val, Value::None) {
                continue;
            }
            if !val.effective_type_is(expected_ty) && !type_matches(expected_ty, val.type_name()) {
                let actual = val.effective_type_name();
                return Err(self.runtime_error_hint(
                    &format!(
                        "type mismatch for parameter '{}': expected {}, got {}",
                        param.name, expected_ty, actual
                    ),
                    &format!("parameter '{}' requires type {}", param.name, expected_ty),
                ));
            }
        }
        Ok(())
    }

    /// Call path for JIT-compiled closures with exact arity, no named args,
    /// and no module globals.
    ///
    /// Returns `Ok(true)` on success, `Ok(false)` if the fast path can't
    /// handle this call.
    #[inline(always)]
    #[cfg_attr(not(feature = "jit"), allow(dead_code))]
    pub(crate) fn call_closure_fast_path(
        &mut self,
        closure: &Rc<ObjClosure>,
    ) -> Option<Result<(), VMError>> {
        let thunk = closure.jit_thunk.get()?;
        if self.frames.len().saturating_add(self.jit_frame_len()) >= FRAMES_MAX {
            return Some(Err(self.runtime_error_hint(
                "stack overflow",
                "check for infinite recursion or deeply nested calls",
            )));
        }
        let expected = closure.function.arity as usize;
        let slot_offset = self.stack.len() - expected - 1;
        let stop_depth = self.frames.len();
        let inherited_mg = closure.module_globals.as_ptr();
        let entry_line = closure.function.chunk.lines.first().copied().unwrap_or(0);
        self.jit_frame_push_raw(Rc::as_ptr(closure), slot_offset, inherited_mg, entry_line);
        let vm_ptr = self as *mut VM;
        let exit = unsafe { self.jit.invoke_thunk(vm_ptr, thunk, stop_depth) };
        Some(match exit {
            crate::jit::JitExit::Returned => {
                closure.jit_bailouts.set(0);
                Ok(())
            }
            crate::jit::JitExit::Bailout => {
                let jit_frame = self
                    .jit_frame_pop_raw()
                    .expect("compiled bailout without active jit frame");
                self.note_jit_bailout(closure);
                let inherited_mg = unsafe { mg_rc(jit_frame.module_globals) };
                self.frames.push(CallFrame {
                    closure: Rc::clone(closure),
                    ip: 0,
                    slot_offset: jit_frame.slot_offset,
                    module_globals: inherited_mg,
                });
                Ok(())
            }
            crate::jit::JitExit::RuntimeError(err) => Err(err),
        })
    }

    /// Record an entry-guard bailout. The frame is re-run by the
    /// interpreter and the thunk stays installed for the next call. Only
    /// `JIT_BAILOUT_LIMIT` consecutive bailouts (counter reset on every
    /// successful JIT return) deopt permanently.
    fn note_jit_bailout(&self, closure: &Rc<ObjClosure>) {
        const JIT_BAILOUT_LIMIT: u8 = 4;
        let n = closure.jit_bailouts.get().saturating_add(1);
        closure.jit_bailouts.set(n);
        if n >= JIT_BAILOUT_LIMIT {
            closure.jit_thunk.set(None);
            closure.jit_state.set(2);
        }
    }

    /// Whether `closure`'s own name still resolves to it, in the globals its
    /// body would read (`module_globals` first, as `GetGlobal` does).
    #[cfg(feature = "jit")]
    fn lean_self_name_current(
        &self,
        closure: &Rc<ObjClosure>,
        module_globals: Option<&HashMap<String, Value>>,
    ) -> bool {
        let Some(name) = closure.function.name.as_deref() else {
            return false;
        };
        let bound = module_globals
            .and_then(|mg| mg.get(name))
            .or_else(|| self.globals.get(name));
        matches!(bound, Some(Value::Closure(c)) if Rc::ptr_eq(c, closure))
    }

    fn call_closure(
        &mut self,
        closure: Rc<ObjClosure>,
        arg_count: usize,
        named_args: &[(String, Value)],
        module_globals: Option<Rc<HashMap<String, Value>>>,
    ) -> Result<(), VMError> {
        if crate::concurrent::cancelled() {
            return Err(self.runtime_error("task cancelled"));
        }
        let expected = closure.function.arity as usize;
        let state = closure.jit_state.get();

        if state == 1 && named_args.is_empty() && arg_count == expected && module_globals.is_none()
        {
            self.check_param_types(&closure.function)?;
            if let Some(result) = self.call_closure_fast_path(&closure) {
                return result;
            }
        }

        if state != 1 {
            closure
                .call_count
                .set(closure.call_count.get().saturating_add(1));
        }

        if !named_args.is_empty() {
            let params = &closure.function.params;
            let required = params
                .iter()
                .filter(|p| !p.has_default && !p.optional)
                .count();
            if arg_count > expected {
                return Err(self.arity_error(
                    &closure.function,
                    arg_count + named_args.len(),
                    required,
                    expected,
                ));
            }
            let mut provided = vec![false; expected];
            provided[..arg_count].iter_mut().for_each(|p| *p = true);
            for (name, _) in named_args {
                let Some(idx) = params.iter().position(|p| p.name == *name) else {
                    return Err(self.runtime_error_hint(
                        &format!("unknown named argument '{}'", name),
                        "check the function signature for its parameter names",
                    ));
                };
                if provided[idx] {
                    return Err(self.runtime_error_hint(
                        &format!("argument '{}' was given more than once", name),
                        "pass each parameter either positionally or by name, not both",
                    ));
                }
                provided[idx] = true;
            }
            if let Some(missing) = params
                .iter()
                .zip(&provided)
                .find(|(p, given)| !**given && !p.has_default && !p.optional)
                .map(|(p, _)| p.name.clone())
            {
                return Err(self.runtime_error_hint(
                    &format!("missing required argument '{}'", missing),
                    "pass it positionally or by name",
                ));
            }

            for _ in arg_count..expected {
                self.push(Value::None);
            }

            let slot_base = self.stack.len() - expected;
            for (name, val) in named_args {
                if let Some(idx) = closure.function.params.iter().position(|p| p.name == *name) {
                    self.stack[slot_base + idx] = val.clone();
                }
            }
        } else {
            let required = closure
                .function
                .params
                .iter()
                .filter(|p| !p.has_default && !p.optional)
                .count();
            if arg_count < required || arg_count > expected {
                return Err(self.arity_error(&closure.function, arg_count, required, expected));
            }
            for _ in arg_count..expected {
                self.push(Value::None);
            }
        }

        self.check_param_types(&closure.function)?;

        if self.frames.len().saturating_add(self.jit_frame_len()) >= FRAMES_MAX {
            return Err(self.runtime_error_hint(
                "stack overflow",
                "check for infinite recursion or deeply nested calls",
            ));
        }

        if self.stack.len() >= STACK_MAX.saturating_sub(closure.function.stack_reserve()) {
            return Err(self.runtime_error_hint(
                "stack overflow",
                "check for infinite recursion or deeply nested calls",
            ));
        }

        let slot_offset = self.stack.len() - expected - 1;

        let inherited_mg = module_globals.or_else(|| closure.module_globals.get());

        let stop_depth = self.frames.len();

        let count = closure.call_count.get();
        let thunk = match closure.jit_state.get() {
            1 => closure.jit_thunk.get(),
            2 => None,
            _ => {
                let loop_thunk = self.jit.maybe_compile_loop_entry_thunk(&closure.function);
                if let Some(thunk) = loop_thunk {
                    closure.jit_thunk.set(Some(thunk));
                    closure.jit_state.set(1);
                    Some(thunk)
                } else if let Some(e) = self.jit.maybe_compile_entries_for(&closure.function, count)
                {
                    closure.jit_thunk.set(Some(e.generic));
                    closure.specialized_thunk.set(e.specialized);
                    closure.specialized_arity.set(e.specialized_arity);
                    closure.specialized_kind.set(e.specialized_kind);
                    closure.lean_thunk.set(e.lean);
                    closure.lean_arity.set(e.lean_arity);
                    closure.jit_state.set(1);
                    Some(e.generic)
                } else {
                    if count >= self.jit.threshold() {
                        closure.jit_state.set(2);
                    }
                    None
                }
            }
        };

        #[cfg(feature = "jit")]
        if let Some(lean) = closure.lean_thunk.get()
            && self.lean_self_name_current(&closure, inherited_mg.as_deref())
        {
            let arity = closure.lean_arity.get() as usize;
            let mut args: Vec<i64> = Vec::with_capacity(arity);
            let base = slot_offset + 1;
            for i in 0..arity {
                match self.stack.get(base + i) {
                    Some(Value::Integer(n)) => args.push(*n),
                    _ => {
                        args.clear();
                        break;
                    }
                }
            }
            if args.len() == arity && arity > 0 {
                let budget = (FRAMES_MAX as i64 - self.frames.len() as i64).max(1);
                match unsafe { crate::jit::runtime::invoke_lean(lean, &args, budget) } {
                    Ok(v) => {
                        self.stack_truncate(slot_offset);
                        self.push(Value::Integer(v));
                        closure.jit_bailouts.set(0);
                        return Ok(());
                    }
                    Err(crate::jit::runtime::LeanExit::Overflow) => {
                        return Err(self.runtime_error_hint(
                            "stack overflow",
                            "check for infinite recursion or deeply nested calls",
                        ));
                    }
                    Err(crate::jit::runtime::LeanExit::Unsupported) => {}
                }
            }
        }

        if let Some(thunk) = thunk {
            let entry_line = closure.function.chunk.lines.first().copied().unwrap_or(0);
            let mg_ptr = inherited_mg
                .as_ref()
                .map(Rc::as_ptr)
                .unwrap_or(std::ptr::null());
            self.jit_frame_push_raw(Rc::as_ptr(&closure), slot_offset, mg_ptr, entry_line);
            let vm_ptr = self as *mut VM;
            let exit = unsafe { self.jit.invoke_thunk(vm_ptr, thunk, stop_depth) };
            match exit {
                crate::jit::JitExit::Returned => closure.jit_bailouts.set(0),
                crate::jit::JitExit::Bailout => {
                    let _ = self.jit_frame_pop_raw();
                    self.note_jit_bailout(&closure);
                    self.frames.push(CallFrame {
                        closure: Rc::clone(&closure),
                        ip: 0,
                        slot_offset,
                        module_globals: inherited_mg.clone(),
                    });
                }
                crate::jit::JitExit::RuntimeError(err) => return Err(err),
            }
        } else {
            self.frames.push(CallFrame {
                closure: Rc::clone(&closure),
                ip: 0,
                slot_offset,
                module_globals: inherited_mg.clone(),
            });
        }

        Ok(())
    }

    pub(crate) fn capture_upvalue(&mut self, stack_slot: usize) -> Rc<RefCell<Upvalue>> {
        for uv in &self.open_upvalues {
            if let Upvalue::Open(slot) = &*uv.borrow()
                && *slot == stack_slot
            {
                return Rc::clone(uv);
            }
        }

        let upvalue = Rc::new(RefCell::new(Upvalue::Open(stack_slot)));
        self.open_upvalues.push(Rc::clone(&upvalue));
        upvalue
    }

    pub(crate) fn close_upvalues(&mut self, last: usize) {
        let stack_len = self.stack.len();
        let mut i = 0;
        while i < self.open_upvalues.len() {
            let should_close = {
                let uv = self.open_upvalues[i].borrow();
                matches!(&*uv, Upvalue::Open(slot) if *slot >= last)
            };

            if should_close {
                let uv = self.open_upvalues.remove(i);
                let value = {
                    let borrow = uv.borrow();
                    if let Upvalue::Open(slot) = &*borrow {
                        if *slot < stack_len {
                            self.stack[*slot].clone()
                        } else {
                            Value::None
                        }
                    } else {
                        continue;
                    }
                };
                *uv.borrow_mut() = Upvalue::Closed(value);
            } else {
                i += 1;
            }
        }
    }

    /// Close any open upvalue captured at exactly `slot`, snapshotting its
    /// live value from `self.stack[slot]`. Does not touch upvalues at higher
    /// slots and never pops the stack. Used for `OpCode::CloseUpvalueAt`.
    pub(crate) fn close_upvalue_at_slot(&mut self, slot: usize) {
        let stack_len = self.stack.len();
        let mut i = 0;
        while i < self.open_upvalues.len() {
            let should_close = {
                let uv = self.open_upvalues[i].borrow();
                matches!(&*uv, Upvalue::Open(s) if *s == slot)
            };
            if should_close {
                let uv = self.open_upvalues.remove(i);
                let value = if slot < stack_len {
                    self.stack[slot].clone()
                } else {
                    Value::None
                };
                *uv.borrow_mut() = Upvalue::Closed(value);
            } else {
                i += 1;
            }
        }
    }

    /// Length of an iterable for an `each` loop. `[it] -> len`.
    pub(crate) fn iter_len(&self, iterable: &Value) -> Result<i64, VMError> {
        match iterable.repr() {
            ValueRepr::Array(a) => Ok(a.borrow().len() as i64),
            ValueRepr::String(s) => Ok(s.chars().count() as i64),
            ValueRepr::Tuple(t) => Ok(t.len() as i64),
            ValueRepr::Set(s) => Ok(s.borrow().len() as i64),
            ValueRepr::Map(m) => Ok(m.borrow().len() as i64),
            _ => Err(self.runtime_error_hint(
                &format!("cannot iterate over {}", iterable.type_name()),
                "each loops work with arrays, tuples, strings, sets, and maps",
            )),
        }
    }

    /// Both halves of an `each k, v in coll` step. `[it, idx] -> (key, value)`.
    ///
    /// A map yields its entry's own key; every other iterable yields the
    /// ordinal index. The value half matches what the one-name form would
    /// bind, except for maps, where the one-name form binds the whole `(k, v)`
    /// tuple and this splits it.
    pub(crate) fn iter_entry(
        &self,
        iterable: &Value,
        index: &Value,
    ) -> Result<(Value, Value), VMError> {
        if let ValueRepr::Map(m) = iterable.repr() {
            let borrowed = m.borrow();
            let idx = match index.repr() {
                ValueRepr::Integer(i) => i as usize,
                _ => return Err(self.runtime_error("index must be integer")),
            };
            return Ok(match borrowed.entries().get(idx) {
                Some((k, v)) => (k.clone(), v.clone()),
                None => (Value::None, Value::None),
            });
        }
        let value = self.iter_get(iterable, index)?;
        Ok((index.clone(), value))
    }

    /// Element `iterable[index]` for an `each` loop. `[it, idx] -> elem`.
    pub(crate) fn iter_get(&self, iterable: &Value, index: &Value) -> Result<Value, VMError> {
        let idx = match index.repr() {
            ValueRepr::Integer(i) => i as usize,
            _ => return Err(self.runtime_error("index must be integer")),
        };
        Ok(match iterable.repr() {
            ValueRepr::Array(a) => a.borrow().get(idx).cloned().unwrap_or(Value::None),
            ValueRepr::String(s) => s
                .chars()
                .nth(idx)
                .map(|c| Value::String(rc_str(c.to_string())))
                .unwrap_or(Value::None),
            ValueRepr::Tuple(t) => t.get(idx).cloned().unwrap_or(Value::None),
            ValueRepr::Set(s) => s.borrow().items().get(idx).cloned().unwrap_or(Value::None),
            ValueRepr::Map(m) => {
                let borrowed = m.borrow();
                if let Some((k, v)) = borrowed.entries().get(idx) {
                    Value::Tuple(Rc::new(vec![k.clone(), v.clone()].into()))
                } else {
                    Value::None
                }
            }
            _ => return Err(self.runtime_error("cannot iterate")),
        })
    }

    /// Resolves a sequence index against `len`, with negative indices counting
    /// back from the end, and reports an out-of-range access as an error.
    /// Maps yield `None` for a missing key.
    pub(crate) fn sequence_index(
        &self,
        index: i64,
        len: usize,
        type_name: &str,
    ) -> Result<usize, VMError> {
        let len_i64 = len as i64;
        let resolved = if index < 0 {
            len_i64.checked_add(index)
        } else {
            Some(index)
        };
        match resolved {
            Some(i) if i >= 0 && i < len_i64 => Ok(i as usize),
            _ if len == 0 => Err(self.runtime_error_hint(
                &format!("index {index} out of range: {type_name} is empty"),
                "check the length before indexing",
            )),
            _ => Err(self.runtime_error_hint(
                &format!("index {index} out of range for {type_name} of length {len}",),
                &format!(
                    "valid indices are 0 to {} (or -1 to -{len} counting from the end)",
                    len - 1
                ),
            )),
        }
    }

    pub(crate) fn eval_index(&self, collection: Value, index: Value) -> Result<Value, VMError> {
        let (collection, index) = (self.force(collection), self.force(index));
        match (&collection, &index) {
            (Value::Array(arr), Value::Integer(i)) => {
                let borrowed = arr.borrow();
                let idx = self.sequence_index(*i, borrowed.len(), "ARRAY")?;
                Ok(borrowed[idx].clone())
            }
            (Value::String(s), Value::Integer(i)) => {
                let chars: Vec<char> = s.chars().collect();
                let idx = self.sequence_index(*i, chars.len(), "STRING")?;
                Ok(Value::String(rc_str(chars[idx].to_string())))
            }
            (Value::Tuple(t), Value::Integer(i)) => {
                let idx = self.sequence_index(*i, t.len(), "TUPLE")?;
                Ok(t[idx].clone())
            }
            (Value::Map(m), _) => Ok(m.borrow().get(&index).cloned().unwrap_or(Value::None)),
            _ => Err(self.runtime_error_hint(
                &format!("cannot index {} with {}", collection.type_name(), index.type_name()),
                "arrays and tuples use <int> indices, maps use key values, strings use <int> indices",
            )),
        }
    }

    /// The error for writing into a value `lock` made read-only.
    pub(crate) fn locked_value_error(&self, value: &Value) -> VMError {
        let what = value
            .locked_description()
            .unwrap_or_else(|| "a locked value".to_string());
        let hint = if matches!(value, Value::StructInstance(_)) {
            "`lock` makes a value permanently read-only; build a new one instead"
        } else {
            "`lock` makes a value permanently read-only; use `copy(...)` for a changeable copy"
        };
        self.runtime_error_hint(&format!("cannot change {what}"), hint)
    }

    /// Deep, locked copy of `value` for `lock`: arrays, maps, sets and struct
    /// instances inside it are copied and marked read-only, so no other name
    /// for the original can change the locked one. Already-locked values are
    /// shared as they are. Shared and cyclic structure is preserved.
    pub(crate) fn lock_value(&self, value: &Value) -> Value {
        let mut seen: HashMap<usize, Value> = HashMap::new();
        lock_rec(value, &mut seen)
    }

    /// `eval_index` for a read that will be written back (`m[k]++`): a missing
    /// map key is an error, since there is no value to update.
    pub(crate) fn eval_index_existing(
        &self,
        collection: Value,
        index: Value,
    ) -> Result<Value, VMError> {
        let (collection, index) = (self.force(collection), self.force(index));
        if let Value::Map(m) = &collection {
            if let Some(value) = m.borrow().get(&index) {
                return Ok(value.clone());
            }
            let key = match &index {
                Value::String(s) => format!("\"{s}\""),
                other => other.to_string(),
            };
            return Err(self.runtime_error_hint(
                &format!("key {key} not found in map"),
                &format!("assign it first, e.g. `map[{key}] = 0`, or check with `has(map, {key})`"),
            ));
        }
        self.eval_index(collection, index)
    }

    pub(crate) fn eval_index_assign(
        &self,
        collection: Value,
        index: Value,
        value: Value,
    ) -> Result<(), VMError> {
        if collection.is_locked() {
            return Err(self.locked_value_error(&collection));
        }
        match (&collection, &index) {
            (Value::Array(arr), Value::Integer(i)) => {
                let mut borrowed = arr.borrow_mut();
                let idx = self.sequence_index(*i, borrowed.len(), "ARRAY")?;
                borrowed[idx] = value;
                Ok(())
            }
            (Value::Map(m), _) => {
                self.require_hashable_key(&index, "map key")?;
                m.borrow_mut().insert(index, value);
                Ok(())
            }
            _ => Err(self.runtime_error_hint(
                &format!("cannot assign to index on {}", collection.type_name()),
                "index assignment is supported on arrays and maps",
            )),
        }
    }

    /// Resolve one slice bound against `len`: negative counts from the end,
    /// and anything out of range clamps to the nearer end.
    fn slice_bound(
        &self,
        bound: Option<Value>,
        default: usize,
        len: usize,
    ) -> Result<usize, VMError> {
        match bound {
            Some(Value::Integer(i)) if i < 0 => Ok(len.saturating_sub(i.unsigned_abs() as usize)),
            Some(Value::Integer(i)) => Ok((i as u64).min(len as u64) as usize),
            None => Ok(default),
            _ => Err(self.runtime_error("slice index must be integer")),
        }
    }

    fn eval_slice(
        &self,
        collection: Value,
        start: Option<Value>,
        end: Option<Value>,
    ) -> Result<Value, VMError> {
        match collection.repr() {
            ValueRepr::Array(arr) => {
                let borrowed = arr.borrow();
                let len = borrowed.len();
                let s = self.slice_bound(start, 0, len)?;
                let e = self.slice_bound(end, len, len)?;
                let sliced: Vec<Value> = if s <= e {
                    borrowed[s..e].to_vec()
                } else {
                    Vec::new()
                };
                Ok(Value::Array(Rc::new(RefCell::new(sliced.into()))))
            }
            ValueRepr::String(s) => {
                let len = s.chars().count();
                let start_idx = self.slice_bound(start, 0, len)?;
                let end_idx = self.slice_bound(end, len, len)?;
                if start_idx < end_idx {
                    let sliced: String = s
                        .chars()
                        .skip(start_idx)
                        .take(end_idx - start_idx)
                        .collect();
                    Ok(Value::String(rc_str(sliced)))
                } else {
                    Ok(Value::String(rc_str("")))
                }
            }
            ValueRepr::Tuple(t) => {
                let len = t.len();
                let s = self.slice_bound(start, 0, len)?;
                let e = self.slice_bound(end, len, len)?;
                let sliced: Vec<Value> = if s <= e { t[s..e].to_vec() } else { Vec::new() };
                Ok(Value::Tuple(Rc::new(sliced.into())))
            }
            _ => Err(self.runtime_error_hint(
                &format!("cannot slice {}", collection.type_name()),
                "slicing is supported on arrays, strings, and tuples: collection[start:end]",
            )),
        }
    }

    fn import_module(&mut self, path_str: &str, selective_names: &[String]) -> Result<(), VMError> {
        let supplied = self
            .module_source
            .as_ref()
            .and_then(|source| source(path_str));
        let module_path = match supplied {
            Some(_) => PathBuf::from(format!("<{path_str}>")),
            None => self.resolve_module_path(path_str)?,
        };
        self.imported_here.insert(module_path.clone());

        if let Some(cached) = self.module_cache.get(&module_path).cloned() {
            return self.bind_module(&cached, path_str, selective_names);
        }

        let importing_self = self
            .current_file
            .as_ref()
            .and_then(|f| f.canonicalize().ok())
            .is_some_and(|f| f == module_path);
        if importing_self || self.import_stack.contains(&module_path) {
            return Err(self.runtime_error_hint(
                &format!("circular import: {}", module_path.display()),
                "two modules are importing each other — restructure to break the cycle",
            ));
        }

        let source = match supplied {
            Some(source) => source,
            None => std::fs::read_to_string(&module_path).map_err(|e| {
                self.runtime_error(&format!("cannot read module '{}': {}", path_str, e))
            })?,
        };

        let lexer = crate::lexer::Lexer::new(&source);
        let mut parser = crate::parser::Parser::new(lexer, &source);
        let program = parser.parse_program();

        if !parser.errors().is_empty() {
            return Err(self.runtime_error(&format!(
                "parse errors in module '{}': {}",
                path_str,
                parser.format_errors()
            )));
        }

        let compiler = crate::compiler::Compiler::new();
        let function = compiler.compile(&program).map_err(|errors| {
            let msgs: Vec<String> = errors.iter().map(|e| e.to_string()).collect();
            self.runtime_error(&format!(
                "compile errors in module '{}': {}",
                path_str,
                msgs.join("; ")
            ))
        })?;

        let origin = Rc::new(crate::vm::value::ModuleOrigin {
            file: display_path_cwd(&module_path),
            source: source.clone(),
        });
        crate::vm::value::set_module_origin(&function, &origin);

        let mut sub_vm = self.child_vm();
        sub_vm.set_source(&source);
        sub_vm.set_file(module_path.clone());
        sub_vm.is_main_context = false;
        self.import_stack.push(module_path.clone());
        sub_vm.import_stack = self.import_stack.clone();
        sub_vm.module_cache = std::mem::take(&mut self.module_cache);
        sub_vm.module_deps = std::mem::take(&mut self.module_deps);

        let module_hidden: std::collections::HashSet<String> =
            function.hidden_globals.iter().cloned().collect();

        self.lend_limits(&mut sub_vm);
        let result = sub_vm.run(function);
        sub_vm.lend_limits(self);
        self.module_cache = std::mem::take(&mut sub_vm.module_cache);
        self.module_deps = std::mem::take(&mut sub_vm.module_deps);
        self.import_stack.pop();
        let mut deps = std::mem::take(&mut sub_vm.imported_here);
        let nested: Vec<PathBuf> = deps
            .iter()
            .filter_map(|d| self.module_deps.get(d))
            .flatten()
            .cloned()
            .collect();
        deps.extend(nested);
        let _result = result.map_err(|e| {
            if e.is_host_limit() {
                return e;
            }
            self.runtime_error(&format!("error in module '{}': {}", path_str, e.message))
        })?;

        let globals_rc = Rc::new(sub_vm.globals);
        for value in globals_rc.values() {
            match value.repr() {
                ValueRepr::Closure(closure) => {
                    closure.module_globals.set(Some(Rc::clone(&globals_rc)));
                }
                ValueRepr::StructDef(def) => {
                    def.module_globals.set(Some(Rc::clone(&globals_rc)));
                }
                _ => {}
            }
        }
        let module = Rc::new(ObjModule {
            name: path_str.to_string(),
            globals: globals_rc,
            hidden: module_hidden,
        });

        self.module_deps.insert(module_path.clone(), deps);
        self.module_cache.insert(module_path, Rc::clone(&module));

        self.bind_module(&module, path_str, selective_names)
    }

    fn bind_module(
        &mut self,
        module: &Rc<ObjModule>,
        path_str: &str,
        selective_names: &[String],
    ) -> Result<(), VMError> {
        if selective_names.is_empty() {
            let name = path_str.rsplit('/').next().unwrap_or(path_str).to_string();
            self.jit.bump_global_version(&name);
            self.globals.insert(name, Value::Module(Rc::clone(module)));
        } else {
            for name in selective_names {
                if module.hidden.contains(name) {
                    return Err(self.runtime_error_hint(
                        &format!("'{}' is hidden inside module '{}'", name, path_str),
                        "it is declared `hide fun` and cannot be imported",
                    ));
                }
                if let Some(val) = module.globals.get(name) {
                    self.globals.insert(name.clone(), val.clone());
                    self.jit.bump_global_version(name);
                }
            }
        }
        Ok(())
    }

    fn resolve_module_path(&self, path_str: &str) -> Result<PathBuf, VMError> {
        if (path_str.starts_with("./") || path_str.starts_with("../"))
            && let Some(current) = &self.current_file
        {
            let base = current.parent().unwrap_or(current);
            let resolved = base.join(path_str).with_extension("oxi");
            if resolved.exists() {
                return resolved
                    .canonicalize()
                    .map_err(|e| self.runtime_error(&format!("cannot resolve path: {}", e)));
            }
        }

        let stdlib = self.stdlib_path.join(path_str).with_extension("oxi");
        if stdlib.exists() {
            return stdlib
                .canonicalize()
                .map_err(|e| self.runtime_error(&format!("cannot resolve stdlib path: {}", e)));
        }

        if let Some(current) = &self.current_file {
            let base = current.parent().unwrap_or(current);
            let resolved = base.join(path_str).with_extension("oxi");
            if resolved.exists() {
                return resolved
                    .canonicalize()
                    .map_err(|e| self.runtime_error(&format!("cannot resolve path: {}", e)));
            }
        }

        Err(self.runtime_error_hint(
            &format!("module not found: {}", path_str),
            "check that the module file exists and the path is correct",
        ))
    }

    /// Implements `<type<T>>(value)`, the angle-bracket type wrap form.
    /// Handles Error/Value union wrapping, type conversion, etc.
    /// Return the zero/default value for a type name.
    fn zero_value_for_type(type_name: &str) -> Value {
        match type_name {
            "INTEGER" => Value::Integer(0),
            "FLOAT" => Value::Float(0.0),
            "STRING" => Value::String(rc_str("")),
            "BOOLEAN" => Value::Boolean(false),
            "CHAR" => Value::Char('\0'),
            "BYTE" => Value::Byte(0),
            "UINT" => Value::Uint(0),
            "ARRAY" => Value::Array(Rc::new(RefCell::new(Vec::new().into()))),
            "TUPLE" => Value::Tuple(Rc::new(Vec::new().into())),
            "MAP" => Value::Map(Rc::new(RefCell::new(crate::vm::collections::OxMap::new()))),
            "SET" => Value::Set(Rc::new(RefCell::new(crate::vm::collections::OxSet::new()))),
            "NONE" => Value::None,
            "GENERIC" => Value::None,
            _ => Value::None,
        }
    }

    pub(crate) fn type_wrap(&self, target: &str, value: Value) -> Result<Value, VMError> {
        if target.contains(" || ") {
            let parts: Vec<&str> = target.split(" || ").collect();
            let has_value = parts.contains(&"VALUE");
            let has_error = parts.iter().any(|p| p.starts_with("ERROR"));
            if has_value && has_error {
                if let Value::Wrapped(_) = value {
                    return Ok(value);
                }
                if let Some((tag, msg)) = Self::error_info_from_value(&value) {
                    let default_tag = parts.iter().find_map(|p| {
                        if p.starts_with("ERROR<") && p.ends_with('>') {
                            Some(p[6..p.len() - 1].to_string())
                        } else {
                            None
                        }
                    });
                    let final_tag = tag.or(default_tag);
                    return Ok(Value::ErrorValue(Rc::new(ErrorValueData {
                        msg: rc_str(msg),
                        tag: final_tag.map(|t| t.into()),
                    })));
                } else {
                    return Ok(Value::Wrapped(Rc::new(value)));
                }
            }
            let actual = value.effective_type_name();
            for member in parts {
                if member == actual || member == "GENERIC" {
                    return Ok(value);
                }
                if let Ok(converted) = self.convert_to_type(&value, member) {
                    return Ok(converted);
                }
            }
            return Err(self.runtime_error(&format!("cannot convert {} to {}", actual, target)));
        }

        match target {
            "GENERIC" => Ok(value),
            "VALUE" => Ok(Value::Wrapped(Rc::new(value))),
            "ERROR" => {
                if let Some((tag, msg)) = Self::error_info_from_value(&value) {
                    Ok(Value::ErrorValue(Rc::new(ErrorValueData {
                        msg: rc_str(msg),
                        tag: tag.map(|t| t.into()),
                    })))
                } else {
                    Err(self
                        .runtime_error(&format!("cannot convert {} to ERROR", value.type_name())))
                }
            }
            t if t.starts_with("ERROR<") && t.ends_with('>') => {
                let wanted_tag = &t[6..t.len() - 1];
                if let Some((tag, msg)) = Self::error_info_from_value(&value) {
                    if tag.as_deref() == Some(wanted_tag) {
                        Ok(Value::ErrorValue(Rc::new(ErrorValueData {
                            msg: rc_str(msg),
                            tag: Some(rc_str(wanted_tag)),
                        })))
                    } else {
                        Err(self.runtime_error(&format!(
                            "cannot convert ERROR<{}> to ERROR<{}>",
                            tag.unwrap_or_default(),
                            wanted_tag
                        )))
                    }
                } else {
                    Err(self.runtime_error(&format!(
                        "cannot convert {} to {}",
                        value.type_name(),
                        t
                    )))
                }
            }
            _ => self.convert_to_type(&value, target),
        }
    }

    /// Extract error info from a value (tag, message).
    fn error_info_from_value(value: &Value) -> Option<(Option<String>, String)> {
        match value.repr() {
            ValueRepr::ErrorValue(data) => Some((
                data.tag.as_ref().map(|t| t.to_string()),
                data.msg.to_string(),
            )),
            ValueRepr::Error(msg) => Some((None, msg.to_string())),
            _ => None,
        }
    }

    /// Convert a value to a target type.
    fn convert_to_type(&self, value: &Value, target: &str) -> Result<Value, VMError> {
        match target {
            "GENERIC" => Ok(value.clone()),
            "NONE" => {
                match value.repr() {
                    ValueRepr::None => Ok(Value::None),
                    _ => Err(self
                        .runtime_error(&format!("cannot convert {} to NONE", value.type_name()))),
                }
            }
            "INTEGER" => match value.repr() {
                ValueRepr::Integer(_) => Ok(value.clone()),
                ValueRepr::Float(f) => {
                    let t = f.trunc();
                    if f.is_finite() && (-9223372036854775808.0..9223372036854775808.0).contains(&t)
                    {
                        Ok(Value::Integer(t as i64))
                    } else {
                        Err(self.runtime_error(&format!(
                            "cannot convert FLOAT {} to INTEGER: out of range",
                            f
                        )))
                    }
                }
                ValueRepr::String(s) => s.parse::<i64>().map(Value::Integer).map_err(|_| {
                    self.runtime_error(&format!("cannot convert STRING \"{}\" to INTEGER", s))
                }),
                ValueRepr::Boolean(b) => Ok(Value::Integer(if b { 1 } else { 0 })),
                ValueRepr::Byte(b) => Ok(Value::Integer(b as i64)),
                ValueRepr::Uint(u) => Ok(Value::Integer(u as i64)),
                ValueRepr::Char(c) => Ok(Value::Integer(c as i64)),
                _ => {
                    Err(self
                        .runtime_error(&format!("cannot convert {} to INTEGER", value.type_name())))
                }
            },
            "FLOAT" => {
                match value.repr() {
                    ValueRepr::Float(_) => Ok(value.clone()),
                    ValueRepr::Integer(n) => Ok(Value::Float(n as f64)),
                    ValueRepr::String(s) => s.parse::<f64>().map(Value::Float).map_err(|_| {
                        self.runtime_error(&format!("cannot convert STRING \"{}\" to FLOAT", s))
                    }),
                    ValueRepr::Boolean(b) => Ok(Value::Float(if b { 1.0 } else { 0.0 })),
                    ValueRepr::Byte(b) => Ok(Value::Float(b as f64)),
                    ValueRepr::Uint(u) => Ok(Value::Float(u as f64)),
                    _ => Err(self
                        .runtime_error(&format!("cannot convert {} to FLOAT", value.type_name()))),
                }
            }
            "STRING" => match value.repr() {
                ValueRepr::None => Err(self.runtime_error("cannot convert NONE to STRING")),
                _ => Ok(Value::String(rc_str(format!("{}", value)))),
            },
            "BOOLEAN" => match value.repr() {
                ValueRepr::Boolean(_) => Ok(value.clone()),
                _ => Ok(Value::Boolean(value.is_truthy())),
            },
            "CHAR" => {
                match value.repr() {
                    ValueRepr::Char(_) => Ok(value.clone()),
                    ValueRepr::Integer(n) => char::from_u32(n as u32)
                        .map(Value::Char)
                        .ok_or_else(|| self.runtime_error(&format!("invalid char code: {}", n))),
                    ValueRepr::String(s) => {
                        let mut chars = s.chars();
                        match (chars.next(), chars.next()) {
                            (Some(c), None) => Ok(Value::Char(c)),
                            _ => Err(self
                                .runtime_error(&format!("cannot convert string '{}' to CHAR", s))),
                        }
                    }
                    _ => Err(self
                        .runtime_error(&format!("cannot convert {} to CHAR", value.type_name()))),
                }
            }
            "BYTE" => match value.repr() {
                ValueRepr::Byte(_) => Ok(value.clone()),
                ValueRepr::Integer(n) => {
                    if !(0..=255).contains(&n) {
                        Err(self.runtime_error(&format!(
                            "cannot convert INTEGER {} to BYTE (0-255)",
                            n
                        )))
                    } else {
                        Ok(Value::Byte(n as u8))
                    }
                }
                ValueRepr::Uint(u) => {
                    if u > 255 {
                        Err(self
                            .runtime_error(&format!("cannot convert UINT {} to BYTE (0-255)", u)))
                    } else {
                        Ok(Value::Byte(u as u8))
                    }
                }
                ValueRepr::Char(c) => Ok(Value::Byte(c as u8)),
                ValueRepr::Boolean(b) => Ok(Value::Byte(if b { 1 } else { 0 })),
                _ => {
                    Err(self
                        .runtime_error(&format!("cannot convert {} to BYTE", value.type_name())))
                }
            },
            "UINT" => match value.repr() {
                ValueRepr::Uint(_) => Ok(value.clone()),
                ValueRepr::Integer(n) => u64::try_from(n).map(Value::Uint).map_err(|_| {
                    self.runtime_error(&format!("cannot convert INTEGER {} to UINT (>= 0)", n))
                }),
                ValueRepr::Float(f) => {
                    let t = f.trunc();
                    if f.is_finite() && (0.0..18446744073709551616.0).contains(&t) {
                        Ok(Value::Uint(t as u64))
                    } else {
                        Err(self
                            .runtime_error(&format!("cannot convert FLOAT {} to UINT (>= 0)", f)))
                    }
                }
                ValueRepr::Byte(b) => Ok(Value::Uint(b as u64)),
                ValueRepr::String(s) => s.trim().parse::<u64>().map(Value::Uint).map_err(|_| {
                    self.runtime_error(&format!("cannot convert STRING \"{}\" to UINT", s))
                }),
                ValueRepr::Boolean(b) => Ok(Value::Uint(u64::from(b))),
                _ => {
                    Err(self
                        .runtime_error(&format!("cannot convert {} to UINT", value.type_name())))
                }
            },
            "ARRAY" => {
                match value.repr() {
                    ValueRepr::Array(_) => Ok(value.clone()),
                    ValueRepr::String(s) => {
                        let elements: Vec<Value> = s
                            .chars()
                            .map(|c| Value::String(rc_str(c.to_string())))
                            .collect();
                        Ok(Value::Array(Rc::new(RefCell::new(elements.into()))))
                    }
                    _ => Err(self
                        .runtime_error(&format!("cannot convert {} to ARRAY", value.type_name()))),
                }
            }
            "TUPLE" => {
                match value.repr() {
                    ValueRepr::Tuple(_) => Ok(value.clone()),
                    _ => Err(self
                        .runtime_error(&format!("cannot convert {} to TUPLE", value.type_name()))),
                }
            }
            "MAP" => match value.repr() {
                ValueRepr::Map(_) => Ok(value.clone()),
                _ => {
                    Err(self.runtime_error(&format!("cannot convert {} to MAP", value.type_name())))
                }
            },
            "SET" => match value.repr() {
                ValueRepr::Set(_) => Ok(value.clone()),
                _ => {
                    Err(self.runtime_error(&format!("cannot convert {} to SET", value.type_name())))
                }
            },
            "VALUE" => Ok(Value::Wrapped(Rc::new(value.clone()))),
            "ENUM" => {
                match value.repr() {
                    ValueRepr::EnumInstance(_) | ValueRepr::EnumDef(_) => Ok(value.clone()),
                    _ => Err(self
                        .runtime_error(&format!("cannot convert {} to ENUM", value.type_name()))),
                }
            }
            _ => {
                if value.effective_type_name() == target {
                    Ok(value.clone())
                } else {
                    Err(self.runtime_error(&format!(
                        "cannot convert {} to {}",
                        value.effective_type_name(),
                        target
                    )))
                }
            }
        }
    }
}

/// Worker for [`VM::lock_value`]. `seen` maps an original container's address
/// to its locked copy, so a value reachable twice is copied once and a cycle
/// ends at the copy already being built.
fn lock_rec(value: &Value, seen: &mut HashMap<usize, Value>) -> Value {
    if value.is_locked() {
        return value.clone();
    }
    match value {
        Value::Array(arr) => {
            let key = Rc::as_ptr(arr) as *const () as usize;
            if let Some(done) = seen.get(&key) {
                return done.clone();
            }
            let copy = Rc::new(RefCell::new(ValueVec::default()));
            seen.insert(key, Value::Array(Rc::clone(&copy)));
            let items: Vec<Value> = arr.borrow().iter().map(|v| lock_rec(v, seen)).collect();
            let mut locked = copy.borrow_mut();
            locked.0 = items;
            locked.1.set();
            drop(locked);
            Value::Array(copy)
        }
        Value::Map(m) => {
            let key = Rc::as_ptr(m) as *const () as usize;
            if let Some(done) = seen.get(&key) {
                return done.clone();
            }
            let copy = Rc::new(RefCell::new(crate::vm::collections::OxMap::new()));
            seen.insert(key, Value::Map(Rc::clone(&copy)));
            let entries: Vec<(Value, Value)> = m
                .borrow()
                .iter()
                .map(|(k, v)| (k.clone(), lock_rec(v, seen)))
                .collect();
            let mut locked = copy.borrow_mut();
            for (k, v) in entries {
                locked.insert(k, v);
            }
            locked.lock();
            drop(locked);
            Value::Map(copy)
        }
        Value::Set(s) => {
            let mut copy = s.borrow().clone();
            copy.lock();
            Value::Set(Rc::new(RefCell::new(copy)))
        }
        Value::Tuple(t) => {
            let key = Rc::as_ptr(t) as *const () as usize;
            if let Some(done) = seen.get(&key) {
                return done.clone();
            }
            let items: ValueVec = t.iter().map(|v| lock_rec(v, seen)).collect();
            let locked = Value::Tuple(Rc::new(items));
            seen.insert(key, locked.clone());
            locked
        }
        Value::StructInstance(inst) => {
            let key = Rc::as_ptr(inst) as *const () as usize;
            if let Some(done) = seen.get(&key) {
                return done.clone();
            }
            let copy = Rc::new(ObjStructInstance::new(
                inst.struct_name.clone(),
                vec![Value::None; inst.field_slice().len()],
                Rc::clone(&inst.layout),
                Rc::clone(&inst.def),
            ));
            seen.insert(key, Value::StructInstance(Rc::clone(&copy)));
            let fields: Vec<Value> = inst.field_slice().to_vec();
            for (idx, field) in fields.iter().enumerate() {
                copy.set_field(idx, lock_rec(field, seen));
            }
            copy.locked.set(true);
            Value::StructInstance(copy)
        }
        Value::EnumInstance(e) => {
            let payload = match &e.payload {
                VmEnumPayload::Unit(v) => {
                    VmEnumPayload::Unit(v.as_ref().map(|v| lock_rec(v, seen)))
                }
                VmEnumPayload::Tuple(vs) => {
                    VmEnumPayload::Tuple(vs.iter().map(|v| lock_rec(v, seen)).collect())
                }
                VmEnumPayload::Struct(fs) => VmEnumPayload::Struct(
                    fs.iter()
                        .map(|(n, v)| (n.clone(), lock_rec(v, seen)))
                        .collect(),
                ),
            };
            Value::EnumInstance(Rc::new(ObjEnumInstance {
                enum_name: e.enum_name.clone(),
                variant_name: e.variant_name.clone(),
                payload,
            }))
        }
        Value::Wrapped(inner) => Value::Wrapped(Rc::new(lock_rec(inner, seen))),
        other => other.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::Compiler;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use std::path::PathBuf;

    #[test]
    fn diagnostic_paths_are_relative_only_inside_the_working_directory() {
        let cwd = PathBuf::from("/home/u/proj");

        assert_eq!(
            display_path(Path::new("/home/u/proj/src/app.oxi"), Some(&cwd)),
            "src/app.oxi"
        );
        assert_eq!(
            display_path(Path::new("/usr/local/lib/oxigen/stdlib/os.oxi"), Some(&cwd)),
            "/usr/local/lib/oxigen/stdlib/os.oxi"
        );
        assert_eq!(
            display_path(Path::new("/home/u/proj-other/a.oxi"), Some(&cwd)),
            "/home/u/proj-other/a.oxi"
        );
        assert_eq!(
            display_path(Path::new("/home/u/proj/src/app.oxi"), None),
            "/home/u/proj/src/app.oxi"
        );
    }

    #[test]
    fn execution_mode_constructors_are_explicit() {
        assert_eq!(
            VM::new_interpreter().execution_mode(),
            ExecutionMode::Interpreter
        );
        assert_eq!(VM::new_tiered().execution_mode(), ExecutionMode::TieredJit);
        assert_eq!(
            VM::new_eager_jit().execution_mode(),
            ExecutionMode::EagerJit
        );
        assert_eq!(VM::new().execution_mode(), ExecutionMode::TieredJit);
    }

    fn test_vm(input: &str) -> Result<Value, VMError> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer, input);
        let program = parser.parse_program();

        if !parser.errors().is_empty() {
            panic!("Parser errors:\n{}", parser.format_errors());
        }

        let compiler = Compiler::new();
        let function = compiler.compile(&program).unwrap_or_else(|errors| {
            let messages: Vec<String> = errors.iter().map(|err| err.to_string()).collect();
            panic!("Compile errors: {}", messages.join("; "));
        });

        let mut vm = VM::new();
        vm.set_source(input);
        vm.set_file(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/vm_inline.oxi"));
        vm.run(function)
    }

    #[test]
    fn test_vm_module_dot_call_preserves_named_args() {
        let stdlib_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("core crate should live under repo root")
            .join("stdlib");
        let input = format!(
            r#"
                introduce os
                os.is_dir(path="{}")
            "#,
            stdlib_dir.display()
        );

        match test_vm(&input) {
            Ok(Value::Boolean(result)) => assert!(result),
            Ok(other) => panic!("expected Ok(Boolean(true)), got {}", other.type_name()),
            Err(err) => panic!("expected Ok(Boolean(true)), got error: {}", err.message),
        }
    }

    #[test]
    fn test_vm_struct_method_accepts_named_args() {
        let input = r#"
            struct Counter {
                count <int>
            }
            Counter includes {
                fun add(delta) { self.count + delta }
            }
            c := Counter(1)
            c.add(delta=4)
        "#;

        match test_vm(input) {
            Ok(Value::Integer(result)) => assert_eq!(result, 5),
            Ok(other) => panic!("expected Ok(Integer(5)), got {}", other.type_name()),
            Err(err) => panic!("expected Ok(Integer(5)), got error: {}", err.message),
        }
    }

    #[test]
    fn test_vm_builtin_method_named_args_fail_clearly() {
        let input = r#"
            text := "hello"
            text.upper(value="ignored")
        "#;

        match test_vm(input) {
            Err(err) => assert!(
                err.message
                    .contains("named arguments are not supported for built-in functions"),
                "unexpected error: {}",
                err.message
            ),
            Ok(other) => panic!(
                "expected builtin named-arg error, got {}",
                other.type_name()
            ),
        }
    }
}
