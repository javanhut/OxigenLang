//! Minimal parallelism spike: `spawn`/`join` builtins backed by a worker pool.
//!
//! A `spawn(func, args...)` hands a task to a pool of worker threads. Each
//! task runs on a VM of its own, built from the spawning program's
//! declarations (top-level `fun` defs, types and imports registered, `main`
//! body skipped). The call result is sent back over an mpsc channel;
//! `join(handle)` blocks for it.
//!
//! Only a small set of "plain old data" values cross the thread boundary
//! (`Sendable`), since the VM's `Value` is `Rc`-backed and not `Send`.
//! Functions don't cross either: a worker finds a spawned function's code by
//! compiling the program it came from again (see `Unit`).

use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::mpsc;
use std::sync::{Arc, Mutex, OnceLock};

use crate::ast::{Expression, Program, Statement};
use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::vm::VM;
use crate::vm::collections::{OxMap, OxSet};
use crate::vm::limits::InterruptHandle;
use crate::vm::value::{
    ErrorValueData, FieldLayout, Function, ObjClosure, ObjEnumInstance, ObjStructDef,
    ObjStructInstance, Upvalue, Value, ValueRepr, VmEnumPayload,
};
use std::cell::RefCell as StdRefCell;
use std::rc::Rc;

/// A thread-safe ("plain") value that can cross the worker boundary.
pub enum Sendable {
    Unit,
    Int(i64),
    /// Uints get their own variant: routing them through `Int` wrapped every
    /// value above `i64::MAX` (18446744073709551615 came back as -1) and
    /// returned the wrong type besides.
    Uint(u64),
    Float(f64),
    Bool(bool),
    Str(String),
    Arr(Vec<Sendable>),
    Tuple(Vec<Sendable>),
    Map(Vec<(Sendable, Sendable)>),
    Set(Vec<Sendable>),
    /// `module` is the file of the module that defined the struct, `None`
    /// for the program itself: two files may each define a `Point`, and a
    /// module's struct isn't among the program's globals.
    Struct {
        name: String,
        module: Option<PathBuf>,
        fields: Vec<Sendable>,
    },
    Enum {
        ename: String,
        variant: String,
        payload: SendPayload,
    },
}

/// Transfer form of `VmEnumPayload`.
pub enum SendPayload {
    Unit(Option<Box<Sendable>>),
    Tuple(Vec<Sendable>),
    Fields(Vec<(String, Sendable)>),
}

/// Convert a VM `Value` into a `Sendable`. Errors on anything unsupported.
/// `vm` is the VM the value lives in, which knows where its modules came
/// from; without it a module's struct travels by name only.
fn detach(v: &Value, vm: Option<&VM>) -> Result<Sendable, String> {
    let each = |items: &mut dyn Iterator<Item = &Value>| -> Result<Vec<Sendable>, String> {
        items.map(|x| detach(x, vm)).collect()
    };
    match v.repr() {
        ValueRepr::None => Ok(Sendable::Unit),
        ValueRepr::Integer(n) => Ok(Sendable::Int(n)),
        ValueRepr::Float(f) => Ok(Sendable::Float(f)),
        ValueRepr::Boolean(b) => Ok(Sendable::Bool(b)),
        ValueRepr::Uint(u) => Ok(Sendable::Uint(u)),
        ValueRepr::String(s) => Ok(Sendable::Str(s.as_ref().clone())),
        ValueRepr::Array(a) => Ok(Sendable::Arr(each(&mut a.borrow().iter())?)),
        ValueRepr::Tuple(t) => Ok(Sendable::Tuple(each(&mut t.iter())?)),
        ValueRepr::Map(m) => Ok(Sendable::Map(
            m.borrow()
                .iter()
                .map(|(k, v)| Ok((detach(k, vm)?, detach(v, vm)?)))
                .collect::<Result<_, String>>()?,
        )),
        ValueRepr::Set(s) => Ok(Sendable::Set(each(&mut s.borrow().iter())?)),
        ValueRepr::StructInstance(s) => Ok(Sendable::Struct {
            name: s.struct_name.clone(),
            module: s
                .def
                .module_globals
                .get()
                .and_then(|globals| vm.and_then(|vm| vm.module_path_of(&globals))),
            fields: each(&mut s.field_slice().iter())?,
        }),
        ValueRepr::EnumInstance(e) => Ok(Sendable::Enum {
            ename: e.enum_name.clone(),
            variant: e.variant_name.clone(),
            payload: detach_payload(&e.payload, vm)?,
        }),
        _ => Err(format!(
            "cannot send value of type {} across threads",
            v.type_name()
        )),
    }
}

fn detach_payload(p: &VmEnumPayload, vm: Option<&VM>) -> Result<SendPayload, String> {
    Ok(match p {
        VmEnumPayload::Unit(v) => SendPayload::Unit(match v {
            Some(v) => Some(Box::new(detach(v, vm)?)),
            None => None,
        }),
        VmEnumPayload::Tuple(t) => {
            SendPayload::Tuple(t.iter().map(|x| detach(x, vm)).collect::<Result<_, _>>()?)
        }
        VmEnumPayload::Struct(f) => SendPayload::Fields(
            f.iter()
                .map(|(n, v)| Ok((n.clone(), detach(v, vm)?)))
                .collect::<Result<_, String>>()?,
        ),
    })
}

/// Rebuild a VM `Value` from a `Sendable`. Needs the VM only to look up a
/// struct's definition/layout when rebuilding a `StructInstance`; a struct
/// it has no definition for is an error.
fn attach(s: Sendable, vm: &VM) -> Result<Value, String> {
    let all = |items: Vec<Sendable>| -> Result<Vec<Value>, String> {
        items.into_iter().map(|x| attach(x, vm)).collect()
    };
    Ok(match s {
        Sendable::Unit => Value::None,
        Sendable::Int(n) => Value::Integer(n),
        Sendable::Uint(u) => Value::Uint(u),
        Sendable::Float(f) => Value::Float(f),
        Sendable::Bool(b) => Value::Boolean(b),
        Sendable::Str(s) => Value::String(Rc::new(s)),
        Sendable::Arr(items) => Value::Array(Rc::new(StdRefCell::new(all(items)?.into()))),
        Sendable::Tuple(items) => Value::Tuple(Rc::new(all(items)?.into())),
        Sendable::Map(pairs) => {
            let mut out = Vec::with_capacity(pairs.len());
            for (k, v) in pairs {
                out.push((attach(k, vm)?, attach(v, vm)?));
            }
            Value::Map(Rc::new(StdRefCell::new(OxMap::from_pairs(out))))
        }
        Sendable::Set(items) => Value::Set(Rc::new(StdRefCell::new(OxSet::from_iter_dedup(all(
            items,
        )?)))),
        Sendable::Struct {
            name,
            module,
            fields,
        } => {
            let vals = all(fields)?;
            let def = struct_def(vm, &name, module.as_deref()).ok_or_else(|| {
                format!(
                    "struct '{}' is not defined on the thread receiving it",
                    name
                )
            })?;
            let layout = field_layout(vm, &def)?;
            Value::StructInstance(Rc::new(ObjStructInstance::new(name, vals, layout, def)))
        }
        Sendable::Enum {
            ename,
            variant,
            payload,
        } => Value::EnumInstance(Rc::new(ObjEnumInstance {
            enum_name: ename,
            variant_name: variant,
            payload: attach_payload(payload, vm)?,
        })),
    })
}

fn attach_payload(p: SendPayload, vm: &VM) -> Result<VmEnumPayload, String> {
    Ok(match p {
        SendPayload::Unit(v) => VmEnumPayload::Unit(match v {
            Some(b) => Some(attach(*b, vm)?),
            None => None,
        }),
        SendPayload::Tuple(t) => VmEnumPayload::Tuple(
            t.into_iter()
                .map(|x| attach(x, vm))
                .collect::<Result<_, _>>()?,
        ),
        SendPayload::Fields(f) => VmEnumPayload::Struct(
            f.into_iter()
                .map(|(n, v)| Ok((n, attach(v, vm)?)))
                .collect::<Result<_, String>>()?,
        ),
    })
}

/// The definition of struct `name` from `module` (or the program itself)
/// as `vm` knows it.
fn struct_def(vm: &VM, name: &str, module: Option<&Path>) -> Option<Rc<ObjStructDef>> {
    match module {
        Some(path) => match vm.loaded_module_globals(path)?.get(name) {
            Some(Value::StructDef(def)) => Some(Rc::clone(def)),
            _ => None,
        },
        None => vm.get_struct_def(name),
    }
}

/// A struct's flattened field layout, parents' fields first — the layout
/// `VM::resolve_field_layout` builds, but for a definition found in a
/// module rather than by name in the current scope.
fn field_layout(vm: &VM, def: &Rc<ObjStructDef>) -> Result<Rc<FieldLayout>, String> {
    if let Some(layout) = def.layout.get() {
        return Ok(Rc::clone(layout));
    }
    let mut chain = Vec::new();
    let mut cur = Some(Rc::clone(def));
    while let Some(d) = cur {
        cur = vm.parent_struct_def(&d);
        if cur.is_none()
            && let Some(p) = &d.parent
        {
            return Err(format!(
                "parent struct '{p}' of '{}' is not defined on this thread",
                d.name
            ));
        }
        chain.push(d);
    }
    chain.reverse();
    let mut slots = Vec::new();
    let mut indices = HashMap::new();
    for d in &chain {
        for f in &d.fields {
            if !indices.contains_key(&f.0) {
                indices.insert(f.0.clone(), slots.len());
                slots.push(f.clone());
            }
        }
    }
    let layout = Rc::new(FieldLayout { slots, indices });
    // Already set means another path built the layout first; use that one.
    let _ = def.layout.set(Rc::clone(&layout));
    Ok(def.layout.get().cloned().unwrap_or(layout))
}

// ── Programs ─────────────────────────────────────────────────────────

/// A compiled program that spawned functions can come from, kept so a
/// worker can compile it again under the same unit and find the same code
/// by id (see `Function::id`).
struct Unit {
    /// The compilation unit its functions' ids carry.
    number: u32,
    program: Program,
    source: String,
    file: Option<PathBuf>,
    /// An imported module rather than a program the host ran: its functions
    /// run with that module's globals.
    is_module: bool,
    /// The program registered before this one in the same VM (the REPL's
    /// earlier lines), whose declarations a worker needs as well.
    previous: Option<Arc<Unit>>,
}

impl Unit {
    fn parse(
        number: u32,
        source: String,
        file: Option<PathBuf>,
        is_module: bool,
    ) -> Result<Unit, String> {
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer, &source);
        let program = parser.parse_program();
        if !parser.errors().is_empty() {
            return Err(format!("worker parse error: {}", parser.format_errors()));
        }
        Ok(Unit {
            number,
            program,
            source,
            file,
            is_module,
            previous: None,
        })
    }

    /// This program and the ones registered before it, oldest first.
    fn chain(self: &Arc<Unit>) -> Vec<Arc<Unit>> {
        let mut out = vec![Arc::clone(self)];
        while let Some(prev) = out.last().unwrap().previous.clone() {
            out.push(prev);
        }
        out.reverse();
        out
    }
}

/// Every unit a spawned function has come from, by unit number.
static UNITS: OnceLock<Mutex<HashMap<u32, Arc<Unit>>>> = OnceLock::new();
/// The program the host is running, whose declarations every worker VM
/// gets. Replaced per test by the test runner and per line by the REPL.
static CURRENT: Mutex<Option<Arc<Unit>>> = Mutex::new(None);
/// The program source (`set_src`) and file (`set_main_file`), for a host
/// that registers source text rather than a compiled program.
static SRC: OnceLock<String> = OnceLock::new();
/// The program's file path, so a worker VM can resolve LOCAL module imports
/// (`introduce .mod`) — relative resolution needs `current_file`. Without it,
/// worker init fails on the import and the worker dies, orphaning tasks.
static MAIN_FILE: OnceLock<PathBuf> = OnceLock::new();
/// `SRC` parsed, once a spawn needs it.
static SRC_PROGRAM: OnceLock<Option<Arc<Unit>>> = OnceLock::new();

fn units() -> std::sync::MutexGuard<'static, HashMap<u32, Arc<Unit>>> {
    UNITS
        .get_or_init(|| Mutex::new(HashMap::new()))
        .lock()
        .unwrap_or_else(|e| e.into_inner())
}

fn current_program() -> Option<Arc<Unit>> {
    if let Some(unit) = CURRENT.lock().unwrap_or_else(|e| e.into_inner()).clone() {
        return Some(unit);
    }
    // `set_src`: the source is the program; its unit number is unknown but
    // a worker only needs its declarations. Parsed once, not per spawn.
    let src = SRC.get()?;
    SRC_PROGRAM
        .get_or_init(|| {
            Unit::parse(u32::MAX, src.clone(), MAIN_FILE.get().cloned(), false)
                .ok()
                .map(Arc::new)
        })
        .clone()
}

/// The compilation unit a function was compiled in.
pub fn unit_of(function: &Function) -> u32 {
    // Lossless: the unit number is exactly the id's top 32 bits.
    (function.id >> 32) as u32
}

/// Make `program`, compiled into `function`, the program spawned tasks run
/// against: a worker builds its VM from the program's declarations (`file`
/// resolves its local imports) and finds a spawned function by compiling it
/// again. `continues` keeps the declarations of the program registered
/// before, for a REPL that compiles one line at a time. Call it before
/// running `function`.
pub fn register_program(
    function: &Function,
    program: &Program,
    source: &str,
    file: Option<PathBuf>,
    continues: bool,
) {
    let mut current = CURRENT.lock().unwrap_or_else(|e| e.into_inner());
    let unit = Arc::new(Unit {
        number: unit_of(function),
        program: program.clone(),
        source: source.to_string(),
        file,
        is_module: false,
        previous: if continues { current.clone() } else { None },
    });
    units().insert(unit.number, Arc::clone(&unit));
    *current = Some(unit);
}

/// Undo `register_program` for `function`'s program (a REPL line that
/// failed), going back to the program registered before it.
pub fn unregister_program(function_unit: u32) {
    let mut current = CURRENT.lock().unwrap_or_else(|e| e.into_inner());
    if let Some(unit) = current.clone()
        && unit.number == function_unit
    {
        *current = unit.previous.clone();
        units().remove(&function_unit);
    }
}

/// Register the program source so workers can rebuild a VM. Call once before
/// running the program.
pub fn set_src(s: String) {
    let _ = SRC.set(s);
}

/// Register the program's file path so workers can resolve local module imports.
pub fn set_main_file(path: PathBuf) {
    let _ = MAIN_FILE.set(path);
}

/// The unit `function` was compiled in, as a worker can rebuild it: a
/// registered program, the module it came from, or the `set_src` source.
fn unit_for(closure: &ObjClosure, vm: Option<&VM>) -> Result<Arc<Unit>, String> {
    let number = unit_of(&closure.function);
    if let Some(unit) = units().get(&number) {
        return Ok(Arc::clone(unit));
    }
    let unit = if let Some(origin) = closure.function.origin.borrow().clone() {
        let path = closure
            .module_globals
            .get()
            .and_then(|globals| vm.and_then(|vm| vm.module_path_of(&globals)))
            .unwrap_or_else(|| {
                let shown = PathBuf::from(&origin.file);
                let path = match std::env::current_dir() {
                    Ok(cwd) if shown.is_relative() => cwd.join(shown),
                    _ => shown,
                };
                path.canonicalize().unwrap_or(path)
            });
        Unit::parse(number, origin.source.clone(), Some(path), true)?
    } else if let Some(src) = SRC.get() {
        Unit::parse(number, src.clone(), MAIN_FILE.get().cloned(), false)?
    } else {
        return Err(
            "spawn(): the program this function comes from is not registered with the worker pool"
                .to_string(),
        );
    };
    let unit = Arc::new(unit);
    units().insert(number, Arc::clone(&unit));
    Ok(unit)
}

/// Enough of a function to tell that a worker found the same one: a
/// different program compiled under the same unit (a host that registered
/// the wrong source) would otherwise run whatever function has the id.
#[derive(PartialEq, Eq, Debug)]
struct Shape {
    name: Option<String>,
    arity: u8,
    code_len: usize,
    constants: usize,
}

impl Shape {
    fn of(f: &Function) -> Shape {
        Shape {
            name: f.name.clone(),
            arity: f.arity,
            code_len: f.chunk.code.len(),
            constants: f.chunk.constants.len(),
        }
    }
}

/// What a worker runs: the function with `id` in `unit`, with its captured
/// upvalues.
struct Callee {
    unit: Arc<Unit>,
    id: u64,
    shape: Shape,
    upvalues: Vec<Sendable>,
}

// ── Tasks and workers ───────────────────────────────────────────────────

/// A task's cancel request, and the VM running it once a worker has
/// started it, so a cancel can stop the task's loops, not just its calls.
#[derive(Default)]
struct Cancel {
    requested: AtomicBool,
    running: Mutex<Option<InterruptHandle>>,
}

impl Cancel {
    fn request(&self) {
        let running = self.running.lock().unwrap_or_else(|e| e.into_inner());
        self.requested.store(true, Ordering::Relaxed);
        if let Some(handle) = &*running {
            handle.interrupt();
        }
    }

    fn is_requested(&self) -> bool {
        self.requested.load(Ordering::Relaxed)
    }

    /// Point cancellation at `handle` while a task runs on it; returns what
    /// it pointed at before (an outer task, for a nested inline spawn).
    fn running_on(&self, handle: Option<InterruptHandle>) -> Option<InterruptHandle> {
        let mut running = self.running.lock().unwrap_or_else(|e| e.into_inner());
        if let Some(h) = &handle
            && self.is_requested()
        {
            h.interrupt();
        }
        std::mem::replace(&mut *running, handle)
    }
}

/// What a task runs, and what it needs to run it.
struct TaskBody {
    callee: Callee,
    args: Vec<Sendable>,
    /// The program the task runs in: its declarations are the worker VM's.
    program: Option<Arc<Unit>>,
    /// The main thread's globals the callee reads: a spawned closure reads
    /// top-level `main` bindings as GLOBALS, not upvalues.
    globals: Vec<(String, Sendable)>,
    /// Every global name the callee's code can reach (see `reachable`): the
    /// worker runs only the imports that bind one. `None` runs them all.
    names: Option<Arc<HashSet<String>>>,
}

/// A unit of work for a worker thread.
struct Task {
    body: TaskBody,
    reply: mpsc::Sender<Result<Sendable, String>>,
    cancel: Arc<Cancel>,
    /// The spawning VM's restriction (`VM::restrict`), if it had one.
    capabilities: Option<Vec<crate::vm::Capability>>,
}

/// The worker pool: the task sender plus what's needed to grow it on demand.
struct Pool {
    tx: mpsc::Sender<Task>,
    rx: Arc<Mutex<mpsc::Receiver<Task>>>,
}

static POOL: OnceLock<Pool> = OnceLock::new();

/// Count of tasks handed to the pool but not yet finished by a worker. Drained
/// at program exit so fire-and-forget spawns aren't killed when the process ends.
static OUTSTANDING: AtomicUsize = AtomicUsize::new(0);

/// Number of live worker threads. The pool grows toward `MAX_WORKERS` when every
/// worker is busy, so I/O fan-out isn't capped at core count (blocked threads
/// park for free); it never shrinks.
static WORKERS: AtomicUsize = AtomicUsize::new(0);
const MAX_WORKERS: usize = 256;

/// Worker stack size — mirror the CLI's large execution stack.
const STACK_SIZE: usize = 256 << 20; // 256 MB

/// How deeply tasks may nest on one worker. A task's own spawns run inline
/// on the worker's stack, so without a limit deep nesting overflows it and
/// aborts the process.
const MAX_INLINE_DEPTH: u32 = 64;

thread_local! {
    /// True when running inside a worker thread. Used to force `spawn` to run
    /// inline so workers never enqueue more tasks (prevents task explosion).
    static IS_WORKER: Cell<bool> = const { Cell::new(false) };

    /// Worker: the running task's cancel state (set by cancel() via the handle).
    static CUR_CANCEL: RefCell<Option<Arc<Cancel>>> = const { RefCell::new(None) };

    /// Worker: the running task's program, which an inline spawn inherits.
    static CUR_PROGRAM: RefCell<Option<Arc<Unit>>> = const { RefCell::new(None) };

    /// Worker: how many inline spawns are running on this thread's stack.
    static INLINE_DEPTH: Cell<u32> = const { Cell::new(0) };

    /// Worker: each unit's id → function table, compiled once per thread.
    /// The functions are only code; every task makes its own closures.
    static TABLES: RefCell<HashMap<u32, FunctionTable>> = RefCell::new(HashMap::new());

    /// Worker: a unit's declarations compiled, by which of its statements a
    /// task keeps. Cloned for each task's VM to run.
    static DECLARATIONS: RefCell<HashMap<(u32, Vec<bool>), Function>> =
        RefCell::new(HashMap::new());
}

/// A compiled unit's functions by id.
type FunctionTable = Rc<HashMap<u64, Rc<Function>>>;

/// Entries a worker's code caches hold before starting over, so a host that
/// registers a program per test or per REPL line doesn't grow them forever.
const CODE_CACHE_LIMIT: usize = 256;

/// A spawned task's handle: its result channel, a memoized result, and the
/// cancel state shared with the worker. Lives inside `Value::Task`.
pub struct TaskHandle {
    rx: mpsc::Receiver<Result<Sendable, String>>,
    memo: RefCell<Option<Value>>,
    cancel: Arc<Cancel>,
}

/// A task outcome that the program can recover from: produced as a *catchable*
/// `ErrorValue` (testable with `is_error`), not the raising `Value::Error` kind
/// that halts execution. Timeouts, cancellation, and failed tasks use this so
/// `join`/`converge` always return a value you can branch on.
fn task_error(msg: impl Into<String>, tag: &str) -> Value {
    Value::ErrorValue(Rc::new(ErrorValueData {
        msg: Rc::new(msg.into()),
        tag: Some(Rc::new(tag.to_string())),
    }))
}

/// A task's reply as a value on the joining thread.
fn settle(reply: Result<Sendable, String>, vm: &VM) -> Value {
    match reply {
        Ok(s) => attach(s, vm).unwrap_or_else(|e| task_error(e, "task")),
        Err(e) => task_error(e, "task"),
    }
}

impl TaskHandle {
    pub fn join(&self, vm: &VM) -> Value {
        if let Some(v) = &*self.memo.borrow() {
            return v.clone();
        }
        let v = match self.rx.recv() {
            Ok(reply) => settle(reply, vm),
            Err(_) => task_error("join(): worker dropped without replying", "task"),
        };
        *self.memo.borrow_mut() = Some(v.clone());
        v
    }

    /// Stops waiting, not the task: a timeout is not remembered, so a later
    /// join still collects. A result, failed or not, is.
    pub fn join_timeout(&self, vm: &VM, ms: u64) -> Value {
        if let Some(v) = &*self.memo.borrow() {
            return v.clone();
        }
        let v = match self.rx.recv_timeout(std::time::Duration::from_millis(ms)) {
            Ok(reply) => settle(reply, vm),
            Err(mpsc::RecvTimeoutError::Timeout) => {
                return task_error("join(): timed out", "timeout");
            }
            Err(mpsc::RecvTimeoutError::Disconnected) => {
                task_error("join(): worker dropped without replying", "task")
            }
        };
        *self.memo.borrow_mut() = Some(v.clone());
        v
    }

    pub fn cancel(&self) {
        self.cancel.request();
    }
}

/// Returns true if the current thread is a spawn worker.
pub fn is_worker() -> bool {
    IS_WORKER.with(|w| w.get())
}

/// Set once the first worker picks up a task. Lets `cancelled()` skip the
/// TLS read entirely in programs that never spawn — it's on every
/// `call_closure`, so a recursion-heavy no-spawn program pays millions of
/// TLS round-trips otherwise. Only worker threads can have a cancel flag,
/// and each sets this before running its task body.
static ANY_TASK_RUN: AtomicBool = AtomicBool::new(false);

/// True if the running task was cancelled. Checked at every `call_closure`;
/// loops see it through the VM's interrupt (see `Cancel`).
#[inline]
pub fn cancelled() -> bool {
    if !ANY_TASK_RUN.load(Ordering::Relaxed) {
        return false;
    }
    CUR_CANCEL.with(|c| c.borrow().as_ref().is_some_and(|f| f.is_requested()))
}

/// `cancel(handle)` — stop a spawned task at its next call or loop iteration.
pub fn builtin_cancel(args: &[Value]) -> Value {
    // A non-task used to be accepted and ignored, reporting success.
    match args.first() {
        Some(Value::Task(h)) => {
            h.cancel();
            Value::None
        }
        Some(other) => Value::Error(crate::vm::value::rc_str(format!(
            "cancel() requires a task, got {}",
            other.type_name()
        ))),
        None => Value::Error(crate::vm::value::rc_str(
            "cancel() takes exactly 1 argument".to_string(),
        )),
    }
}

/// Block until every task handed to the worker pool has finished. Call once at
/// program exit so never-joined ("fire-and-forget") spawns run to completion
/// instead of dying with the process. A task that never returns will block
/// exit unless it is cancelled.
pub fn drain() {
    while OUTSTANDING.load(Ordering::SeqCst) > 0 {
        std::thread::sleep(std::time::Duration::from_micros(200));
    }
}

/// A top-level statement a worker needs in order to resolve spawned functions:
/// type/function/pattern/import declarations only. Everything else (the `main`
/// block, bare expression statements, value bindings, control flow) is the
/// program's *work* and belongs to the main thread — running it during worker
/// init would re-execute the driver (e.g. a top-level `spawn`) on every worker
/// and recurse. Function *bodies* are kept; they only run when the task calls
/// them, not at init.
fn is_declaration(stmt: &Statement) -> bool {
    match stmt {
        Statement::StructDef { .. }
        | Statement::EnumDef { .. }
        | Statement::IncludesDef { .. }
        | Statement::Pattern { .. }
        | Statement::Introduce { .. } => true,
        Statement::Let { value, .. } | Statement::TypedLet { value, .. } => {
            matches!(value, Expression::FunctionLiteral { .. })
        }
        _ => false,
    }
}

/// Walk a compiled program's function tree, mapping each function's id to
/// its code, so a worker can find a spawned function.
fn collect_fns(f: &Rc<Function>, table: &mut HashMap<u64, Rc<Function>>) {
    table.insert(f.id, Rc::clone(f));
    for c in f.chunk.constants.iter() {
        if let Value::Closure(inner) = c {
            collect_fns(&inner.function, table);
        }
    }
}

/// The names an `introduce` binds: the module's name, or the names it picks.
fn introduced_names(stmt: &Statement) -> Vec<&str> {
    match stmt {
        Statement::Introduce {
            selective: Some(names),
            ..
        } => names.iter().map(|n| n.value.as_str()).collect(),
        Statement::Introduce { path, .. } => path
            .segments
            .last()
            .map(String::as_str)
            .into_iter()
            .collect(),
        _ => Vec::new(),
    }
}

/// A fresh VM for one task: restricted the way the spawning VM was before
/// anything runs on it (a module's top level included), then with the
/// declarations of `program` (and the programs registered before it)
/// registered but no executable top-level code run. An import runs only
/// when it binds one of `names`: each task imports for itself, so imports
/// the task can't reach would only cost time.
fn build_task_vm(
    program: Option<&Arc<Unit>>,
    capabilities: Option<&[crate::vm::Capability]>,
    names: Option<&HashSet<String>>,
) -> Result<VM, String> {
    let mut vm = VM::new();
    if let Some(allowed) = capabilities {
        vm.restrict(allowed);
    }
    vm.is_main_context = false; // register fun defs, skip main body
    // No registered program means no declarations to replay, not an error.
    for unit in program.map(Unit::chain).unwrap_or_default() {
        let keep: Vec<bool> =
            unit.program
                .statements
                .iter()
                .map(|s| match s {
                    Statement::Introduce { .. } => names
                        .is_none_or(|names| introduced_names(s).iter().any(|n| names.contains(*n))),
                    _ => is_declaration(s),
                })
                .collect();
        let key = (unit.number, keep);
        let cached = DECLARATIONS.with(|d| d.borrow().get(&key).cloned());
        let function = match cached {
            Some(f) => f,
            None => {
                let declarations = Program {
                    statements: unit
                        .program
                        .statements
                        .iter()
                        .zip(&key.1)
                        .filter(|(_, keep)| **keep)
                        .map(|(s, _)| s.clone())
                        .collect(),
                    hidden: unit.program.hidden.clone(),
                };
                let f = Compiler::new()
                    .compile(&declarations)
                    .map_err(|errs| format!("worker compile error: {:?}", errs))?;
                DECLARATIONS.with(|d| {
                    let mut d = d.borrow_mut();
                    if d.len() >= CODE_CACHE_LIMIT {
                        d.clear();
                    }
                    d.insert(key, f.clone());
                });
                f
            }
        };
        vm.set_source(&unit.source);
        if let Some(path) = &unit.file {
            vm.set_file(path.clone()); // so local `introduce .mod` imports resolve
        }
        vm.run(function)
            .map_err(|e| format!("worker init error: {}", e))?;
    }
    Ok(vm)
}

/// `unit`'s id → function table, compiling it on this thread's first use.
fn function_table(unit: &Unit) -> Result<FunctionTable, String> {
    if let Some(table) = TABLES.with(|t| t.borrow().get(&unit.number).cloned()) {
        return Ok(table);
    }
    let compiled = Compiler::for_unit(unit.number)
        .compile(&unit.program)
        .map_err(|errs| format!("worker compile error: {:?}", errs))?;
    let compiled = Rc::new(compiled);
    if unit.is_module {
        let origin = Rc::new(crate::vm::value::ModuleOrigin {
            file: unit
                .file
                .as_ref()
                .map(|p| p.display().to_string())
                .unwrap_or_default(),
            source: unit.source.clone(),
        });
        crate::vm::value::set_module_origin(&compiled, &origin);
    }
    let mut table = HashMap::new();
    collect_fns(&compiled, &mut table);
    let table = Rc::new(table);
    TABLES.with(|t| {
        let mut t = t.borrow_mut();
        if t.len() >= CODE_CACHE_LIMIT {
            t.clear();
        }
        t.insert(unit.number, Rc::clone(&table));
    });
    Ok(table)
}

/// The closure a task calls, rebuilt on the worker's VM.
fn rebuild_callee(vm: &mut VM, callee: Callee) -> Result<Value, String> {
    let unit = &callee.unit;
    let function = function_table(unit)?
        .get(&callee.id)
        .cloned()
        .ok_or_else(|| format!("spawn: function #{} not found", callee.id))?;
    if Shape::of(&function) != callee.shape {
        return Err(format!(
            "spawn: the worker's copy of the program has a different function #{} — \
             was the program registered with the wrong source?",
            callee.id
        ));
    }
    let mut ups = Vec::with_capacity(callee.upvalues.len());
    for u in callee.upvalues {
        ups.push(attach(u, vm)?);
    }
    let closure = ObjClosure::closed(function, ups);
    if unit.is_module
        && let Some(path) = &unit.file
    {
        let globals = vm
            .module_globals_at(path)
            .map_err(|e| format!("worker init error: {}", e))?;
        closure.module_globals.set(Some(globals));
    }
    Ok(Value::Closure(Rc::new(closure)))
}

/// The modules whose structs `s` holds, so the receiving VM can load them
/// before rebuilding the structs.
fn struct_modules(s: &Sendable, out: &mut Vec<PathBuf>) {
    match s {
        Sendable::Struct { module, fields, .. } => {
            if let Some(path) = module
                && !out.contains(path)
            {
                out.push(path.clone());
            }
            fields.iter().for_each(|f| struct_modules(f, out));
        }
        Sendable::Arr(items) | Sendable::Tuple(items) | Sendable::Set(items) => {
            items.iter().for_each(|x| struct_modules(x, out));
        }
        Sendable::Map(pairs) => pairs.iter().for_each(|(k, v)| {
            struct_modules(k, out);
            struct_modules(v, out);
        }),
        Sendable::Enum { payload, .. } => match payload {
            SendPayload::Unit(Some(v)) => struct_modules(v, out),
            SendPayload::Unit(None) => {}
            SendPayload::Tuple(items) => items.iter().for_each(|x| struct_modules(x, out)),
            SendPayload::Fields(fields) => fields.iter().for_each(|(_, v)| struct_modules(v, out)),
        },
        _ => {}
    }
}

/// Run a task on its own VM and produce a Sendable result.
fn run_task(
    task: TaskBody,
    capabilities: Option<&[crate::vm::Capability]>,
    cancel: &Arc<Cancel>,
) -> Result<Sendable, String> {
    let TaskBody {
        callee,
        args,
        program,
        globals,
        names,
    } = task;
    if cancel.is_requested() {
        return Err("task cancelled".to_string());
    }
    let mut vm = build_task_vm(program.as_ref(), capabilities, names.as_deref())?;
    // The callee's own module, and the modules of any struct the task was sent.
    let mut modules: Vec<PathBuf> = Vec::new();
    if callee.unit.is_module
        && let Some(path) = &callee.unit.file
    {
        modules.push(path.clone());
    }
    for s in args
        .iter()
        .chain(globals.iter().map(|(_, s)| s))
        .chain(callee.upvalues.iter())
    {
        struct_modules(s, &mut modules);
    }
    for path in &modules {
        vm.module_globals_at(path)
            .map_err(|e| format!("worker init error: {}", e))?;
    }
    // Seed main's globals: a spawned closure reads them as globals, not upvalues.
    for (name, s) in globals {
        let v = attach(s, &vm)?;
        vm.define_global(name, v);
    }
    let callee = rebuild_callee(&mut vm, callee)?;
    let mut val_args = Vec::with_capacity(args.len());
    for a in args {
        val_args.push(attach(a, &vm)?);
    }

    let outer_program = CUR_PROGRAM.with(|p| p.replace(program));
    let outer_cancel = CUR_CANCEL.with(|c| c.replace(Some(Arc::clone(cancel))));
    let outer_running = cancel.running_on(Some(vm.interrupt_handle()));
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        vm.call_with_args(callee, val_args)
    }));
    cancel.running_on(outer_running);
    CUR_CANCEL.with(|c| *c.borrow_mut() = outer_cancel);
    CUR_PROGRAM.with(|p| *p.borrow_mut() = outer_program);

    match result {
        Ok(Ok(value)) => detach(&value, Some(&vm)),
        Ok(Err(_)) if cancel.is_requested() => Err("task cancelled".to_string()),
        Ok(Err(e)) => Err(format!("{}", e)),
        Err(panic) => Err(format!("task panicked: {}", panic_message(&panic))),
    }
}

/// The message a panic carried, for reporting it as a task error.
pub(crate) fn panic_message(panic: &Box<dyn std::any::Any + Send>) -> String {
    if let Some(s) = panic.downcast_ref::<&str>() {
        (*s).to_string()
    } else if let Some(s) = panic.downcast_ref::<String>() {
        s.clone()
    } else {
        "unknown panic".to_string()
    }
}

/// Spawn one worker thread: pull and run tasks until the queue closes.
fn spawn_worker(rx: Arc<Mutex<mpsc::Receiver<Task>>>) {
    let id = WORKERS.fetch_add(1, Ordering::SeqCst);
    let res = std::thread::Builder::new()
        .name(format!("oxi-worker-{}", id))
        .stack_size(STACK_SIZE)
        .spawn(move || {
            IS_WORKER.with(|w| w.set(true));
            loop {
                let task = {
                    let guard = rx.lock().unwrap_or_else(|e| e.into_inner());
                    guard.recv()
                };
                let task = match task {
                    Ok(t) => t,
                    Err(_) => break, // channel closed
                };
                ANY_TASK_RUN.store(true, Ordering::Relaxed);
                // Each task gets a VM of its own, so nothing one task leaves
                // behind (module state, the globals it was sent) reaches the next.
                let result = run_task(task.body, task.capabilities.as_deref(), &task.cancel);
                let _ = task.reply.send(result);
                OUTSTANDING.fetch_sub(1, Ordering::SeqCst);
            }
            WORKERS.fetch_sub(1, Ordering::SeqCst);
        });
    if res.is_err() {
        WORKERS.fetch_sub(1, Ordering::SeqCst);
    }
}

/// Lazily start the worker pool on first spawn, seeded with one worker per core.
fn ensure_pool() -> &'static Pool {
    POOL.get_or_init(|| {
        let (tx, rx) = mpsc::channel::<Task>();
        let rx = Arc::new(Mutex::new(rx));
        let n = std::thread::available_parallelism()
            .map(|v| v.get())
            .unwrap_or(4);
        for _ in 0..n {
            spawn_worker(Arc::clone(&rx));
        }
        Pool { tx, rx }
    })
}

/// `spawn(func, args...)` -> task handle. Address sentinel for the VM's
/// dispatch (see `call_value`) and the rare direct-call path; the VM routes
/// real spawns through `spawn_with_vm` so they carry the main thread's globals.
pub fn builtin_spawn(args: &[Value]) -> Value {
    spawn_inner(args, None)
}

/// `spawn` with VM access: the spawning VM knows the globals a spawned
/// closure may read (top-level `main` bindings are GLOBALS, not upvalues),
/// its restriction, and where its modules came from.
pub fn spawn_with_vm(vm: &VM, args: &[Value]) -> Value {
    spawn_inner(args, Some(vm))
}

/// Every global name a task calling `root` can reach: the names its code
/// mentions — names are string constants — and, through the globals they
/// name, those of the functions it can call, plus the methods of every
/// struct (a method call names no struct). A superset, which is fine for
/// what it decides: which globals to copy into the task, and which imports
/// the task's VM runs.
fn reachable(vm: &VM, root: &Rc<Function>) -> HashSet<String> {
    let mut seen: HashSet<*const Function> = HashSet::new();
    let mut names: HashSet<String> = HashSet::new();
    let mut todo: Vec<Rc<Function>> = vec![Rc::clone(root)];
    let methods = |def: &ObjStructDef, todo: &mut Vec<Rc<Function>>| {
        for m in def.methods.borrow().values() {
            if let Value::Closure(m) = m {
                todo.push(Rc::clone(&m.function));
            }
        }
    };
    for (_, v) in vm.globals_iter() {
        if let Value::StructDef(def) = v {
            methods(def, &mut todo);
        }
    }
    while let Some(f) = todo.pop() {
        if !seen.insert(Rc::as_ptr(&f)) {
            continue;
        }
        for c in f.chunk.constants.iter() {
            match c {
                Value::Closure(inner) => todo.push(Rc::clone(&inner.function)),
                Value::String(s) if names.insert(s.as_ref().clone()) => {
                    if let Some(Value::Closure(g)) = vm.get_global(s) {
                        todo.push(Rc::clone(&g.function));
                    }
                }
                _ => {}
            }
        }
    }
    names
}

/// The main program's globals among `names` whose values can cross.
/// Shipping only these keeps an unrelated large global from being copied
/// into every task.
fn globals_named(vm: &VM, names: &HashSet<String>) -> Vec<(String, Sendable)> {
    names
        .iter()
        .filter_map(|name| {
            let v = vm.get_global(name)?;
            detach(&v, Some(vm)).ok().map(|s| (name.clone(), s))
        })
        .collect()
}

fn spawn_inner(args: &[Value], vm: Option<&VM>) -> Value {
    if args.is_empty() {
        return Value::Error(Rc::new("spawn() requires a function argument".to_string()));
    }
    let closure = match args[0].repr() {
        ValueRepr::Closure(c) => Rc::clone(c),
        _ => {
            return Value::Error(Rc::new(
                "spawn() first argument must be a function".to_string(),
            ));
        }
    };
    let unit = match unit_for(&closure, vm) {
        Ok(unit) => unit,
        Err(e) => return Value::Error(Rc::new(e)),
    };
    let mut ups = Vec::with_capacity(closure.upvalues.len());
    for uv in &closure.upvalues {
        match &*uv.borrow() {
            Upvalue::Closed(v) => match detach(v, vm) {
                Ok(s) => ups.push(s),
                Err(e) => return Value::Error(Rc::new(format!("spawn(): capture {}", e))),
            },
            Upvalue::Open(_) => {
                return Value::Error(Rc::new(
                    "spawn(): internal error — open upvalue".to_string(),
                ));
            }
        }
    }
    // A module's function reads its module's globals, never the program's,
    // and its module does its own imports.
    let (globals, names) = match vm {
        Some(_) if unit.is_module => (Vec::new(), Some(Arc::new(HashSet::new()))),
        Some(vm) => {
            let names = reachable(vm, &closure.function);
            (globals_named(vm, &names), Some(Arc::new(names)))
        }
        None => (Vec::new(), None),
    };
    let callee = Callee {
        id: closure.function.id,
        shape: Shape::of(&closure.function),
        unit,
        upvalues: ups,
    };

    let mut sendable_args = Vec::with_capacity(args.len() - 1);
    for a in &args[1..] {
        match detach(a, vm) {
            Ok(s) => sendable_args.push(s),
            Err(e) => return Value::Error(Rc::new(format!("spawn(): {}", e))),
        }
    }
    let capabilities = vm.and_then(|vm| vm.capabilities()).map(<[_]>::to_vec);

    let (reply_tx, reply_rx) = mpsc::channel::<Result<Sendable, String>>();
    let cancel = Arc::new(Cancel::default());

    // Inside a worker: run inline so workers can't enqueue more tasks.
    if is_worker() {
        let depth = INLINE_DEPTH.with(|d| d.get());
        let result = if depth >= MAX_INLINE_DEPTH {
            Err(format!(
                "spawn(): tasks nested more than {} deep — keep tasks as leaves",
                MAX_INLINE_DEPTH
            ))
        } else {
            INLINE_DEPTH.with(|d| d.set(depth + 1));
            let body = TaskBody {
                callee,
                args: sendable_args,
                program: CUR_PROGRAM.with(|p| p.borrow().clone()),
                globals,
                names,
            };
            let result = run_task(body, capabilities.as_deref(), &cancel);
            INLINE_DEPTH.with(|d| d.set(depth));
            result
        };
        let _ = reply_tx.send(result);
    } else {
        let pool = ensure_pool();
        let task = Task {
            body: TaskBody {
                callee,
                args: sendable_args,
                program: current_program(),
                globals,
                names,
            },
            reply: reply_tx,
            cancel: cancel.clone(),
            capabilities,
        };
        let pending = OUTSTANDING.fetch_add(1, Ordering::SeqCst) + 1;
        // Grow when all workers are busy; blocked threads park for free.
        let workers = WORKERS.load(Ordering::SeqCst);
        if pending > workers && workers < MAX_WORKERS {
            spawn_worker(Arc::clone(&pool.rx));
        }
        // With no workers the task would never drain: join() and drain() hang forever.
        if WORKERS.load(Ordering::SeqCst) == 0 {
            OUTSTANDING.fetch_sub(1, Ordering::SeqCst);
            return Value::Error(Rc::new(
                "spawn(): could not create worker threads (out of memory or thread limit reached)"
                    .to_string(),
            ));
        }
        if pool.tx.send(task).is_err() {
            OUTSTANDING.fetch_sub(1, Ordering::SeqCst);
            return Value::Error(Rc::new("spawn(): worker pool unavailable".to_string()));
        }
    }

    Value::Task(Rc::new(TaskHandle {
        rx: reply_rx,
        memo: RefCell::new(None),
        cancel,
    }))
}

/// `join(handle)` -> the spawned call's result (blocks).
/// `join(handle, ms)` -> result, or an error if it doesn't finish within `ms`.
/// Stub: `join` is routed through `join_with_vm` at the builtin dispatch site
/// (it needs VM access to rebuild struct/enum results). Never reached normally.
pub fn builtin_join(_args: &[Value]) -> Value {
    Value::Error(Rc::new("join(): internal dispatch error".to_string()))
}

/// `join(handle[, ms])` — blocks for a spawned call's result, with VM access so
/// struct/enum results can be rebuilt on this thread.
pub fn join_with_vm(vm: &VM, args: &[Value]) -> Value {
    match args.first() {
        Some(Value::Task(h)) => match args.get(1).and_then(|v| v.as_integer()) {
            Some(ms) => h.join_timeout(vm, ms.max(0) as u64),
            None => h.join(vm),
        },
        // Join in order so `converge [t1, t2]` works; non-tasks pass through.
        Some(Value::Array(arr)) => {
            // One deadline for the whole batch; per-task would allow k*N ms.
            let deadline = args.get(1).and_then(|v| v.as_integer()).map(|ms| {
                std::time::Instant::now() + std::time::Duration::from_millis(ms.max(0) as u64)
            });
            let joined: Vec<Value> = arr
                .borrow()
                .iter()
                .map(|v| match (v, deadline) {
                    (Value::Task(h), Some(end)) => {
                        let left = end.saturating_duration_since(std::time::Instant::now());
                        h.join_timeout(vm, left.as_millis() as u64)
                    }
                    (Value::Task(h), None) => h.join(vm),
                    (other, _) => other.clone(),
                })
                .collect();
            Value::Array(Rc::new(RefCell::new(joined.into())))
        }
        _ => Value::Error(Rc::new("join() requires a task handle".to_string())),
    }
}

// ── Channels ────────────────────────────────────────────────────────────
//
// A channel is a queue of `Sendable` messages in a process-wide registry, and
// a script holds its integer id, so it crosses into tasks like a socket does.
// Values are copied in on `send` and rebuilt on `recv`, exactly as task
// arguments and results are.

struct Chan {
    state: Mutex<ChanState>,
    ready: std::sync::Condvar,
}

#[derive(Default)]
struct ChanState {
    items: std::collections::VecDeque<Sendable>,
    closed: bool,
}

// ponytail: channels are never freed; drop closed, drained ones from the
// registry if a script makes channels by the thousand.
static CHANNELS: OnceLock<Mutex<HashMap<u64, Arc<Chan>>>> = OnceLock::new();
static NEXT_CHANNEL: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(1);

fn channels() -> std::sync::MutexGuard<'static, HashMap<u64, Arc<Chan>>> {
    CHANNELS
        .get_or_init(Default::default)
        .lock()
        .unwrap_or_else(|e| e.into_inner())
}

fn channel_of(v: Option<&Value>, what: &str) -> Result<Arc<Chan>, Value> {
    let id = v.and_then(|v| v.as_integer());
    id.and_then(|id| channels().get(&(id as u64)).cloned())
        .ok_or_else(|| Value::Error(Rc::new(format!("channel.{what}: not a channel"))))
}

pub(crate) fn new_channel() -> u64 {
    let id = NEXT_CHANNEL.fetch_add(1, Ordering::Relaxed);
    channels().insert(
        id,
        Arc::new(Chan {
            state: Mutex::default(),
            ready: Default::default(),
        }),
    );
    id
}

/// Queue a message without a VM, for messages Rust produces (signals).
pub(crate) fn push_str(id: u64, msg: &str) {
    if let Some(ch) = channels().get(&id).cloned() {
        let mut st = ch.state.lock().unwrap_or_else(|e| e.into_inner());
        if !st.closed {
            st.items.push_back(Sendable::Str(msg.to_string()));
            ch.ready.notify_one();
        }
    }
}

fn builtin_channel_new(_args: &[Value]) -> Value {
    Value::Integer(new_channel() as i64)
}

pub fn builtin_channel_send(_args: &[Value]) -> Value {
    Value::Error(Rc::new("channel.send: internal dispatch error".to_string()))
}

pub fn builtin_channel_recv(_args: &[Value]) -> Value {
    Value::Error(Rc::new("channel.recv: internal dispatch error".to_string()))
}

/// `__channel_send(ch, value)`
fn channel_send(vm: &VM, args: &[Value]) -> Value {
    let ch = match channel_of(args.first(), "send") {
        Ok(ch) => ch,
        Err(e) => return e,
    };
    let msg = match args.get(1).map(|v| detach(v, Some(vm))) {
        Some(Ok(m)) => m,
        Some(Err(e)) => return Value::Error(Rc::new(format!("channel.send: {e}"))),
        None => return Value::Error(Rc::new("channel.send takes (channel, value)".to_string())),
    };
    let mut st = ch.state.lock().unwrap_or_else(|e| e.into_inner());
    if st.closed {
        return Value::Error(Rc::new("channel.send: the channel is closed".to_string()));
    }
    st.items.push_back(msg);
    ch.ready.notify_one();
    Value::None
}

/// `__channel_recv(ch, timeout_ms)`: the next message, waiting for one.
/// `None` once the channel is closed and empty, when `timeout_ms` passes,
/// or at once for a timeout of 0.
fn channel_recv(vm: &VM, args: &[Value]) -> Value {
    let ch = match channel_of(args.first(), "recv") {
        Ok(ch) => ch,
        Err(e) => return e,
    };
    let deadline = args
        .get(1)
        .and_then(|v| v.as_integer())
        .map(|ms| std::time::Instant::now() + std::time::Duration::from_millis(ms.max(0) as u64));
    let mut st = ch.state.lock().unwrap_or_else(|e| e.into_inner());
    loop {
        if let Some(msg) = st.items.pop_front() {
            drop(st);
            return attach(msg, vm)
                .unwrap_or_else(|e| Value::Error(Rc::new(format!("channel.recv: {e}"))));
        }
        if st.closed {
            return Value::None;
        }
        st = match deadline {
            None => ch.ready.wait(st).unwrap_or_else(|e| e.into_inner()),
            Some(end) => {
                let left = end.saturating_duration_since(std::time::Instant::now());
                if left.is_zero() {
                    return Value::None;
                }
                ch.ready
                    .wait_timeout(st, left)
                    .unwrap_or_else(|e| e.into_inner())
                    .0
            }
        };
    }
}

fn builtin_channel_close(args: &[Value]) -> Value {
    match channel_of(args.first(), "close") {
        Ok(ch) => {
            ch.state.lock().unwrap_or_else(|e| e.into_inner()).closed = true;
            ch.ready.notify_all();
            Value::None
        }
        Err(e) => e,
    }
}

fn builtin_channel_len(args: &[Value]) -> Value {
    match channel_of(args.first(), "len") {
        Ok(ch) => Value::Integer(
            ch.state
                .lock()
                .unwrap_or_else(|e| e.into_inner())
                .items
                .len() as i64,
        ),
        Err(e) => e,
    }
}

fn builtin_channel_is_closed(args: &[Value]) -> Value {
    match channel_of(args.first(), "is_closed") {
        Ok(ch) => Value::Boolean(ch.state.lock().unwrap_or_else(|e| e.into_inner()).closed),
        Err(e) => e,
    }
}

pub(crate) fn register_channel_builtins(globals: &mut HashMap<String, Value>) {
    globals.insert(
        "__channel_new".to_string(),
        Value::Builtin(builtin_channel_new),
    );
    globals.insert(
        "__channel_send".to_string(),
        Value::Builtin(builtin_channel_send),
    );
    globals.insert(
        "__channel_recv".to_string(),
        Value::Builtin(builtin_channel_recv),
    );
    globals.insert(
        "__channel_close".to_string(),
        Value::Builtin(builtin_channel_close),
    );
    globals.insert(
        "__channel_len".to_string(),
        Value::Builtin(builtin_channel_len),
    );
    globals.insert(
        "__channel_is_closed".to_string(),
        Value::Builtin(builtin_channel_is_closed),
    );
}

/// The builtins that need the calling VM, and the function that gets it.
/// `VM::call_value` routes these here; every other builtin is a plain `fn`.
pub(crate) fn with_vm(func: crate::vm::value::BuiltinFn) -> Option<fn(&VM, &[Value]) -> Value> {
    let is = |f: crate::vm::value::BuiltinFn| std::ptr::fn_addr_eq(func, f);
    if is(builtin_join) {
        Some(join_with_vm)
    } else if is(builtin_channel_send) {
        Some(channel_send)
    } else if is(builtin_channel_recv) {
        Some(channel_recv)
    } else {
        None
    }
}
