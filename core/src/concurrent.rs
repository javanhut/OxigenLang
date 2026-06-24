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
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::mpsc;
use std::sync::{Arc, Mutex, OnceLock};

use crate::ast::{Expression, Statement};
use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::vm::collections::{OxMap, OxSet};
use crate::vm::value::{Value, ValueRepr};
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
        _ => Err(format!("cannot send value of type {} across threads", v.type_name())),
    }
}

/// Rebuild a VM `Value` from a `Sendable`. No VM required.
fn attach(s: Sendable) -> Value {
    match s {
        Sendable::Unit => Value::None,
        Sendable::Int(n) => Value::Integer(n),
        Sendable::Float(f) => Value::Float(f),
        Sendable::Bool(b) => Value::Boolean(b),
        Sendable::Str(s) => Value::String(Rc::new(s)),
        Sendable::Arr(items) => {
            let vals: Vec<Value> = items.into_iter().map(attach).collect();
            Value::Array(Rc::new(StdRefCell::new(vals)))
        }
        Sendable::Tuple(items) => {
            let vals: Vec<Value> = items.into_iter().map(attach).collect();
            Value::Tuple(Rc::new(vals))
        }
        Sendable::Map(pairs) => {
            let ps: Vec<(Value, Value)> =
                pairs.into_iter().map(|(k, v)| (attach(k), attach(v))).collect();
            Value::Map(Rc::new(StdRefCell::new(OxMap::from_pairs(ps))))
        }
        Sendable::Set(items) => {
            let vs = items.into_iter().map(attach);
            Value::Set(Rc::new(StdRefCell::new(OxSet::from_iter_dedup(vs))))
        }
    }
}

/// A unit of work for a worker thread.
struct Task {
    func: String,
    args: Vec<Sendable>,
    reply: mpsc::Sender<Result<Sendable, String>>,
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

    /// Per-thread handle table mapping a ticket id to the reply receiver.
    static HANDLES: RefCell<HashMap<u64, mpsc::Receiver<Result<Sendable, String>>>> =
        RefCell::new(HashMap::new());
    static NEXT: Cell<u64> = const { Cell::new(1) };
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

/// Build a VM with the program's top-level declarations (functions, types,
/// patterns, imports) registered but no executable top-level code run.
fn build_worker_vm(src: &str) -> Result<VM, String> {
    let lexer = Lexer::new(src);
    let mut parser = Parser::new(lexer, src);
    let mut program = parser.parse_program();
    if !parser.errors().is_empty() {
        return Err(format!("worker parse error: {}", parser.format_errors()));
    }
    // Workers load declarations only — drop all executable top-level statements
    // so no top-level `spawn`/`join`/side effect ever runs at worker init.
    program.statements.retain(is_declaration);
    let compiler = Compiler::new();
    let function = compiler
        .compile(&program)
        .map_err(|errs| format!("worker compile error: {:?}", errs))?;

    let mut vm = VM::new();
    vm.set_source(src);
    vm.is_main_context = false; // register fun defs, skip main body
    vm.run(function).map_err(|e| format!("worker init error: {}", e))?;
    Ok(vm)
}

/// Run a task synchronously on the given VM and produce a Sendable result.
fn run_task(vm: &mut VM, func: &str, args: Vec<Sendable>) -> Result<Sendable, String> {
    let callee = vm
        .get_global(func)
        .ok_or_else(|| format!("spawn: function '{}' not found", func))?;
    let val_args: Vec<Value> = args.into_iter().map(attach).collect();
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
                let result = run_task(&mut vm, &task.func, task.args);
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

fn next_id() -> u64 {
    NEXT.with(|n| {
        let id = n.get();
        n.set(id + 1);
        id
    })
}

/// `spawn(func, args...)` -> task handle (`Value::Uint`).
pub fn builtin_spawn(args: &[Value]) -> Value {
    if args.is_empty() {
        return Value::Error(Rc::new("spawn() requires a function argument".to_string()));
    }
    let func_name = match args[0].repr() {
        ValueRepr::Closure(c) => match c.function.name.as_deref() {
            Some(name) => name.to_string(),
            None => {
                return Value::Error(Rc::new(
                    "spawn() cannot spawn an anonymous function".to_string(),
                ))
            }
        },
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

    // Inside a worker: run inline so workers can't enqueue more tasks.
    if is_worker() {
        // Workers don't have access to their own VM here, so we build a
        // throwaway one. This path is rare (nested spawn) and intentionally
        // simple — correctness over speed.
        let src = SRC.get().cloned().unwrap_or_default();
        let result = match build_worker_vm(&src) {
            Ok(mut vm) => run_task(&mut vm, &func_name, sendable_args),
            Err(e) => Err(e),
        };
        let _ = reply_tx.send(result);
    } else {
        let pool = ensure_pool();
        let task = Task {
            func: func_name,
            args: sendable_args,
            reply: reply_tx,
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

    let id = next_id();
    HANDLES.with(|h| h.borrow_mut().insert(id, reply_rx));
    Value::Uint(id)
}

/// `join(handle)` -> the spawned call's result (blocks).
pub fn builtin_join(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(Rc::new("join() takes exactly one argument".to_string()));
    }
    let id = match args[0].repr() {
        ValueRepr::Uint(u) => u,
        ValueRepr::Integer(n) if n >= 0 => n as u64,
        _ => return Value::Error(Rc::new("join() requires a task handle".to_string())),
    };

    let rx = HANDLES.with(|h| h.borrow_mut().remove(&id));
    let rx = match rx {
        Some(rx) => rx,
        None => return Value::Error(Rc::new(format!("join(): invalid or used handle {}", id))),
    };

    match rx.recv() {
        Ok(Ok(s)) => attach(s),
        Ok(Err(e)) => Value::Error(Rc::new(e)),
        Err(_) => Value::Error(Rc::new("join(): worker dropped without replying".to_string())),
    }
}
