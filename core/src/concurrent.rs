//! Minimal parallelism spike: `spawn`/`join` builtins backed by a worker pool.
//!
//! A `spawn(func, args...)` hands a task to a pool of worker threads, each
//! running its own VM built from the program source (top-level `fun` defs
//! registered, `main` body skipped). The call result is sent back over an
//! mpsc channel; `join(handle)` blocks for it.
//!
//! Only a small set of "plain old data" values cross the thread boundary
//! (`Sendable`), since the VM's `Value` is `Rc`-backed and not `Send`.

use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::mpsc;
use std::sync::{Arc, Mutex, OnceLock};

use crate::ast::{Expression, Statement};
use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::vm::collections::{OxMap, OxSet};
use crate::vm::value::{
    Function, ObjClosure, ObjEnumInstance, ObjStructInstance, Upvalue, Value, ValueRepr,
    VmEnumPayload,
};
use crate::vm::VM;
use std::cell::RefCell as StdRefCell;
use std::rc::Rc;

/// A thread-safe ("plain") value that can cross the worker boundary.
pub enum Sendable {
    Unit,
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Arr(Vec<Sendable>),
    Tuple(Vec<Sendable>),
    Map(Vec<(Sendable, Sendable)>),
    Set(Vec<Sendable>),
    Struct { name: String, fields: Vec<Sendable> },
    Enum { ename: String, variant: String, payload: SendPayload },
}

/// Transfer form of `VmEnumPayload`.
pub enum SendPayload {
    Unit(Option<Box<Sendable>>),
    Tuple(Vec<Sendable>),
    Fields(Vec<(String, Sendable)>),
}

/// Convert a VM `Value` into a `Sendable`. Errors on anything unsupported.
fn detach(v: &Value) -> Result<Sendable, String> {
    match v.repr() {
        ValueRepr::None => Ok(Sendable::Unit),
        ValueRepr::Integer(n) => Ok(Sendable::Int(n)),
        ValueRepr::Float(f) => Ok(Sendable::Float(f)),
        ValueRepr::Boolean(b) => Ok(Sendable::Bool(b)),
        ValueRepr::Uint(u) => Ok(Sendable::Int(u as i64)),
        ValueRepr::String(s) => Ok(Sendable::Str(s.as_ref().clone())),
        ValueRepr::Array(a) => {
            let mut out = Vec::new();
            for el in a.borrow().iter() {
                out.push(detach(el)?);
            }
            Ok(Sendable::Arr(out))
        }
        ValueRepr::Tuple(t) => Ok(Sendable::Tuple(
            t.iter().map(detach).collect::<Result<_, _>>()?,
        )),
        ValueRepr::Map(m) => Ok(Sendable::Map(
            m.borrow()
                .iter()
                .map(|(k, v)| Ok((detach(k)?, detach(v)?)))
                .collect::<Result<_, String>>()?,
        )),
        ValueRepr::Set(s) => Ok(Sendable::Set(
            s.borrow().iter().map(detach).collect::<Result<_, _>>()?,
        )),
        ValueRepr::StructInstance(s) => Ok(Sendable::Struct {
            name: s.struct_name.clone(),
            fields: s.field_slice().iter().map(detach).collect::<Result<_, _>>()?,
        }),
        ValueRepr::EnumInstance(e) => Ok(Sendable::Enum {
            ename: e.enum_name.clone(),
            variant: e.variant_name.clone(),
            payload: detach_payload(&e.payload)?,
        }),
        _ => Err(format!("cannot send value of type {} across threads", v.type_name())),
    }
}

fn detach_payload(p: &VmEnumPayload) -> Result<SendPayload, String> {
    Ok(match p {
        VmEnumPayload::Unit(v) => SendPayload::Unit(match v {
            Some(v) => Some(Box::new(detach(v)?)),
            None => None,
        }),
        VmEnumPayload::Tuple(t) => SendPayload::Tuple(t.iter().map(detach).collect::<Result<_, _>>()?),
        VmEnumPayload::Struct(f) => SendPayload::Fields(
            f.iter().map(|(n, v)| Ok((n.clone(), detach(v)?))).collect::<Result<_, String>>()?,
        ),
    })
}

/// Rebuild a VM `Value` from a `Sendable`. Needs the VM only to look up a
/// struct's definition/layout when rebuilding a `StructInstance`.
fn attach(s: Sendable, vm: &VM) -> Value {
    match s {
        Sendable::Unit => Value::None,
        Sendable::Int(n) => Value::Integer(n),
        Sendable::Float(f) => Value::Float(f),
        Sendable::Bool(b) => Value::Boolean(b),
        Sendable::Str(s) => Value::String(Rc::new(s)),
        Sendable::Arr(items) => Value::Array(Rc::new(StdRefCell::new(
            items.into_iter().map(|x| attach(x, vm)).collect(),
        ))),
        Sendable::Tuple(items) => {
            Value::Tuple(Rc::new(items.into_iter().map(|x| attach(x, vm)).collect()))
        }
        Sendable::Map(pairs) => Value::Map(Rc::new(StdRefCell::new(OxMap::from_pairs(
            pairs.into_iter().map(|(k, v)| (attach(k, vm), attach(v, vm))).collect(),
        )))),
        Sendable::Set(items) => Value::Set(Rc::new(StdRefCell::new(OxSet::from_iter_dedup(
            items.into_iter().map(|x| attach(x, vm)),
        )))),
        Sendable::Struct { name, fields } => {
            let vals: Vec<Value> = fields.into_iter().map(|f| attach(f, vm)).collect();
            match (vm.get_struct_def(&name), vm.resolve_field_layout(&name)) {
                (Some(def), Ok(layout)) => {
                    Value::StructInstance(Rc::new(ObjStructInstance::new(name, vals, layout, def)))
                }
                _ => Value::Error(Rc::new(format!("struct '{}' not defined on this thread", name))),
            }
        }
        Sendable::Enum { ename, variant, payload } => Value::EnumInstance(Rc::new(ObjEnumInstance {
            enum_name: ename,
            variant_name: variant,
            payload: attach_payload(payload, vm),
        })),
    }
}

fn attach_payload(p: SendPayload, vm: &VM) -> VmEnumPayload {
    match p {
        SendPayload::Unit(v) => VmEnumPayload::Unit(v.map(|b| attach(*b, vm))),
        SendPayload::Tuple(t) => VmEnumPayload::Tuple(t.into_iter().map(|x| attach(x, vm)).collect()),
        SendPayload::Fields(f) => {
            VmEnumPayload::Struct(f.into_iter().map(|(n, v)| (n, attach(v, vm))).collect())
        }
    }
}

/// How a worker resolves the function to run: a named top-level function
/// (resolved in the worker's globals, preserving module context) or — for
/// lambdas and capturing closures — a compile-order function id plus a snapshot
/// of its captured upvalues.
enum Callee {
    Named(String),
    Fn { id: u32, upvalues: Vec<Sendable> },
}

/// A unit of work for a worker thread.
struct Task {
    callee: Callee,
    args: Vec<Sendable>,
    reply: mpsc::Sender<Result<Sendable, String>>,
    cancel: Arc<AtomicBool>,
}

/// The worker pool: the task sender plus what's needed to grow it on demand.
struct Pool {
    tx: mpsc::Sender<Task>,
    rx: Arc<Mutex<mpsc::Receiver<Task>>>,
    src: String,
}

static POOL: OnceLock<Pool> = OnceLock::new();
static SRC: OnceLock<String> = OnceLock::new();

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

thread_local! {
    /// True when running inside a worker thread. Used to force `spawn` to run
    /// inline so workers never enqueue more tasks (prevents task explosion),
    /// and to skip the `main` body when building the worker VM.
    static IS_WORKER: Cell<bool> = const { Cell::new(false) };

    /// Worker-only: compile-order id -> function code, for rebuilding spawned
    /// closures (lambdas/captures) that the name path can't resolve.
    static WORKER_FNS: RefCell<HashMap<u32, Rc<Function>>> = RefCell::new(HashMap::new());

    /// Worker: the running task's cancel flag (set by cancel() via the handle).
    static CUR_CANCEL: RefCell<Option<Arc<AtomicBool>>> = RefCell::new(None);
}

/// A spawned task's handle: its result channel, a memoized result, and the
/// cancel flag shared with the worker. Lives inside `Value::Task`.
pub struct TaskHandle {
    rx: mpsc::Receiver<Result<Sendable, String>>,
    memo: RefCell<Option<Value>>,
    cancel: Arc<AtomicBool>,
}

impl TaskHandle {
    pub fn join(&self, vm: &VM) -> Value {
        if let Some(v) = &*self.memo.borrow() {
            return v.clone();
        }
        let v = match self.rx.recv() {
            Ok(Ok(s)) => attach(s, vm),
            Ok(Err(e)) => Value::Error(Rc::new(e)),
            Err(_) => Value::Error(Rc::new("join(): worker dropped without replying".to_string())),
        };
        *self.memo.borrow_mut() = Some(v.clone());
        v
    }

    /// ponytail: timeout stops *waiting*, not the task; don't memoize, so a
    /// later join can still collect.
    pub fn join_timeout(&self, vm: &VM, ms: u64) -> Value {
        if let Some(v) = &*self.memo.borrow() {
            return v.clone();
        }
        match self.rx.recv_timeout(std::time::Duration::from_millis(ms)) {
            Ok(Ok(s)) => {
                let v = attach(s, vm);
                *self.memo.borrow_mut() = Some(v.clone());
                v
            }
            Ok(Err(e)) => Value::Error(Rc::new(e)),
            Err(mpsc::RecvTimeoutError::Timeout) => Value::Error(Rc::new("join(): timed out".to_string())),
            Err(_) => Value::Error(Rc::new("join(): worker dropped without replying".to_string())),
        }
    }

    pub fn cancel(&self) {
        self.cancel.store(true, Ordering::Relaxed);
    }
}

/// Register the program source so workers can rebuild a VM. Call once before
/// running the program.
pub fn set_src(s: String) {
    let _ = SRC.set(s);
}

/// Returns true if the current thread is a spawn worker.
pub fn is_worker() -> bool {
    IS_WORKER.with(|w| w.get())
}

/// True if the running task was cancelled. Checked at every `call_closure`, so a
/// tight callless loop or a parked recv won't see it until the next call.
pub fn cancelled() -> bool {
    CUR_CANCEL.with(|c| c.borrow().as_ref().is_some_and(|f| f.load(Ordering::Relaxed)))
}

/// `cancel(handle)` — cooperatively stop a spawned task at its next call.
pub fn builtin_cancel(args: &[Value]) -> Value {
    if let Some(Value::Task(h)) = args.first() {
        h.cancel();
    }
    Value::None
}

/// Block until every task handed to the worker pool has finished. Call once at
/// program exit so never-joined ("fire-and-forget") spawns run to completion
/// instead of dying with the process. A task that never returns will block
/// exit — cancellation/timeouts are out of scope (P3).
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

/// Walk a compiled program's function tree, mapping each function's stable id
/// to its code, so a worker can rebuild a spawned closure (lambda/captures)
/// that lives in `main` and so isn't among the declarations-only globals.
fn collect_fns(f: &Rc<Function>, table: &mut HashMap<u32, Rc<Function>>) {
    table.insert(f.id, Rc::clone(f));
    for c in f.chunk.constants.iter() {
        if let Value::Closure(inner) = c {
            collect_fns(&inner.function, table);
        }
    }
}

/// Build a VM with the program's top-level declarations (functions, types,
/// patterns, imports) registered but no executable top-level code run.
fn build_worker_vm(src: &str) -> Result<VM, String> {
    let lexer = Lexer::new(src);
    let mut parser = Parser::new(lexer, src);
    let mut program = parser.parse_program();
    if !parser.errors().is_empty() {
        return Err(format!("worker parse error: {}", parser.format_errors()));
    }
    // Compile the FULL program (not run) to harvest the id->function table —
    // lambdas live in `main`, which the declarations filter below drops. Same
    // source + compiler as the main thread, so the ids match.
    let full = Rc::new(
        Compiler::new()
            .compile(&program)
            .map_err(|errs| format!("worker compile error: {:?}", errs))?,
    );
    let mut table = HashMap::new();
    collect_fns(&full, &mut table);
    WORKER_FNS.with(|t| *t.borrow_mut() = table);

    // Workers load declarations only — drop all executable top-level statements
    // so no top-level `spawn`/`join`/side effect ever runs at worker init.
    program.statements.retain(is_declaration);
    let function = Compiler::new()
        .compile(&program)
        .map_err(|errs| format!("worker compile error: {:?}", errs))?;

    let mut vm = VM::new();
    vm.set_source(src);
    vm.is_main_context = false; // register fun defs, skip main body
    vm.run(function).map_err(|e| format!("worker init error: {}", e))?;
    Ok(vm)
}

/// Run a task synchronously on the given VM and produce a Sendable result.
fn run_task(vm: &mut VM, callee: Callee, args: Vec<Sendable>) -> Result<Sendable, String> {
    let callee = match callee {
        Callee::Named(name) => vm
            .get_global(&name)
            .ok_or_else(|| format!("spawn: function '{}' not found", name))?,
        Callee::Fn { id, upvalues } => {
            let f = WORKER_FNS
                .with(|t| t.borrow().get(&id).cloned())
                .ok_or_else(|| format!("spawn: function #{} not found", id))?;
            let ups: Vec<Value> = upvalues.into_iter().map(|u| attach(u, vm)).collect();
            Value::Closure(Rc::new(ObjClosure::closed(f, ups)))
        }
    };
    let val_args: Vec<Value> = args.into_iter().map(|a| attach(a, vm)).collect();
    let result = vm
        .call_with_args(callee, val_args)
        .map_err(|e| format!("{}", e))?;
    detach(&result)
}

/// Spawn one worker thread: build its VM from the program's declarations, then
/// pull and run tasks until the queue closes.
fn spawn_worker(rx: Arc<Mutex<mpsc::Receiver<Task>>>, src: String) {
    let id = WORKERS.fetch_add(1, Ordering::SeqCst);
    let res = std::thread::Builder::new()
        .name(format!("oxi-worker-{}", id))
        .stack_size(STACK_SIZE)
        .spawn(move || {
            IS_WORKER.with(|w| w.set(true));
            let mut vm = match build_worker_vm(&src) {
                Ok(vm) => vm,
                Err(_) => {
                    WORKERS.fetch_sub(1, Ordering::SeqCst);
                    return;
                }
            };
            loop {
                let task = {
                    let guard = rx.lock().unwrap();
                    guard.recv()
                };
                let task = match task {
                    Ok(t) => t,
                    Err(_) => break, // channel closed
                };
                CUR_CANCEL.with(|c| *c.borrow_mut() = Some(task.cancel.clone()));
                let result = run_task(&mut vm, task.callee, task.args);
                let _ = task.reply.send(result);
                OUTSTANDING.fetch_sub(1, Ordering::SeqCst);
            }
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
        let src = SRC.get().cloned().unwrap_or_default();

        let n = std::thread::available_parallelism()
            .map(|v| v.get())
            .unwrap_or(4);
        for _ in 0..n {
            spawn_worker(Arc::clone(&rx), src.clone());
        }
        Pool { tx, rx, src }
    })
}

/// `spawn(func, args...)` -> task handle (`Value::Uint`).
pub fn builtin_spawn(args: &[Value]) -> Value {
    if args.is_empty() {
        return Value::Error(Rc::new("spawn() requires a function argument".to_string()));
    }
    let callee = match args[0].repr() {
        ValueRepr::Closure(c) => {
            // Named top-level function with no captures: resolve by name on the
            // worker (preserves module context). Otherwise ship the function id
            // plus a snapshot of its (already-closed) upvalues.
            if c.upvalues.is_empty() && c.function.name.is_some() {
                Callee::Named(c.function.name.clone().unwrap())
            } else {
                let mut ups = Vec::with_capacity(c.upvalues.len());
                for uv in &c.upvalues {
                    match &*uv.borrow() {
                        Upvalue::Closed(v) => match detach(v) {
                            Ok(s) => ups.push(s),
                            Err(e) => {
                                return Value::Error(Rc::new(format!("spawn(): capture {}", e)))
                            }
                        },
                        Upvalue::Open(_) => {
                            return Value::Error(Rc::new(
                                "spawn(): internal error — open upvalue".to_string(),
                            ))
                        }
                    }
                }
                Callee::Fn {
                    id: c.function.id,
                    upvalues: ups,
                }
            }
        }
        _ => return Value::Error(Rc::new("spawn() first argument must be a function".to_string())),
    };

    let mut sendable_args = Vec::with_capacity(args.len() - 1);
    for a in &args[1..] {
        match detach(a) {
            Ok(s) => sendable_args.push(s),
            Err(e) => return Value::Error(Rc::new(format!("spawn(): {}", e))),
        }
    }

    let (reply_tx, reply_rx) = mpsc::channel::<Result<Sendable, String>>();
    let cancel = Arc::new(AtomicBool::new(false));

    // Inside a worker: run inline so workers can't enqueue more tasks.
    if is_worker() {
        // Workers don't have access to their own VM here, so we build a
        // throwaway one. This path is rare (nested spawn) and intentionally
        // simple — correctness over speed.
        let src = SRC.get().cloned().unwrap_or_default();
        let result = match build_worker_vm(&src) {
            Ok(mut vm) => run_task(&mut vm, callee, sendable_args),
            Err(e) => Err(e),
        };
        let _ = reply_tx.send(result);
    } else {
        let pool = ensure_pool();
        let task = Task {
            callee,
            args: sendable_args,
            reply: reply_tx,
            cancel: cancel.clone(),
        };
        let pending = OUTSTANDING.fetch_add(1, Ordering::SeqCst) + 1;
        // Grow the pool when every worker is busy so I/O-bound fan-out isn't
        // capped at core count. Blocked threads park for free.
        let workers = WORKERS.load(Ordering::SeqCst);
        if pending > workers && workers < MAX_WORKERS {
            spawn_worker(Arc::clone(&pool.rx), pool.src.clone());
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
        _ => Value::Error(Rc::new("join() requires a task handle".to_string())),
    }
}
