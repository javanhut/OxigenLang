//! Background processes (`os.spawn`) and signals (`os.signals`).
//!
//! A spawned process lives in a process-wide registry and a script holds its
//! integer id, so a handle crosses into `diverge` tasks like a socket does.
//! Each part of a child (its stdin, its stdout, the `Child` itself) has its own
//! lock, so a task blocked reading the output does not stop another from
//! writing to it or killing it.
//!
//! Children still running when the script ends are stopped: at a normal exit
//! (an `atexit` hook), and on SIGINT, SIGTERM or SIGHUP the script is not
//! handling itself. Signals reach the script as messages on a channel, which
//! a self-pipe and a watcher thread deliver, since a signal handler can do
//! almost nothing safely.

use crate::vm::value::{Value, ValueRepr, rc_str};
use std::collections::HashMap;
use std::io::{BufRead, BufReader, Read, Write};
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex, MutexGuard, OnceLock};
use std::time::{Duration, Instant};

struct Proc {
    pid: u32,
    child: Mutex<Child>,
    stdin: Mutex<Option<ChildStdin>>,
    stdout: Mutex<Option<BufReader<ChildStdout>>>,
}

static PROCS: OnceLock<Mutex<HashMap<u64, Arc<Proc>>>> = OnceLock::new();
static NEXT_PROC: AtomicU64 = AtomicU64::new(1);

fn lock<T>(m: &Mutex<T>) -> MutexGuard<'_, T> {
    m.lock().unwrap_or_else(|e| e.into_inner())
}

fn procs() -> MutexGuard<'static, HashMap<u64, Arc<Proc>>> {
    lock(PROCS.get_or_init(Default::default))
}

fn err(msg: impl Into<String>) -> Value {
    Value::Error(rc_str(msg))
}

fn proc_of(v: Option<&Value>, what: &str) -> Result<Arc<Proc>, Value> {
    v.and_then(|v| v.as_integer())
        .and_then(|id| procs().get(&(id as u64)).cloned())
        .ok_or_else(|| err(format!("os.{what}: not a process from os.spawn")))
}

fn str_of(v: Option<&Value>) -> Option<String> {
    match v.map(|v| v.repr()) {
        Some(ValueRepr::String(s)) => Some(s.to_string()),
        _ => None,
    }
}

/// `__spawn_process(prog, argv, capture)`: start `prog` with `argv`, its stdin
/// a pipe the script writes to, and its stdout a pipe when `capture` is true
/// (inherited otherwise, so a child that prints cannot fill a pipe nobody
/// reads). stderr is always inherited.
fn builtin_spawn_process(args: &[Value]) -> Value {
    let Some(prog) = str_of(args.first()) else {
        return err("os.spawn takes (program, argv)");
    };
    let argv: Vec<String> = match args.get(1).map(|v| v.repr()) {
        Some(ValueRepr::Array(a)) => a.borrow().iter().map(|a| format!("{a}")).collect(),
        None | Some(ValueRepr::None) => Vec::new(),
        _ => return err("os.spawn: argv must be an array"),
    };
    let capture = matches!(args.get(2).map(|v| v.repr()), Some(ValueRepr::Boolean(true)));
    // Before the child starts: a child that could not be stopped at exit
    // would outlive the script, which is what spawn promises it won't.
    if let Err(e) = signals::watch_for_exit() {
        return err(format!("os.spawn: cannot arrange to stop children at exit: {e}"));
    }
    let mut cmd = Command::new(&prog);
    cmd.args(&argv)
        .stdin(Stdio::piped())
        .stdout(if capture { Stdio::piped() } else { Stdio::inherit() });
    let mut child = match cmd.spawn() {
        Ok(c) => c,
        Err(e) => return err(format!("os.spawn({prog}): {e}")),
    };
    let proc = Proc {
        pid: child.id(),
        stdin: Mutex::new(child.stdin.take()),
        stdout: Mutex::new(child.stdout.take().map(BufReader::new)),
        child: Mutex::new(child),
    };
    let id = NEXT_PROC.fetch_add(1, Ordering::Relaxed);
    procs().insert(id, Arc::new(proc));
    Value::Integer(id as i64)
}

/// `__process_write(p, text)`: write to the child's stdin and flush.
fn builtin_process_write(args: &[Value]) -> Value {
    let p = match proc_of(args.first(), "write_stdin") {
        Ok(p) => p,
        Err(e) => return e,
    };
    let Some(text) = str_of(args.get(1)) else {
        return err("os.write_stdin takes (process, text)");
    };
    let mut stdin = lock(&p.stdin);
    let Some(pipe) = stdin.as_mut() else {
        return err("os.write_stdin: stdin is closed");
    };
    match pipe.write_all(text.as_bytes()).and_then(|()| pipe.flush()) {
        Ok(()) => Value::None,
        Err(e) => err(format!("os.write_stdin: {e}")),
    }
}

/// `__process_close_stdin(p)`: end of input, for children that read to EOF.
fn builtin_process_close_stdin(args: &[Value]) -> Value {
    match proc_of(args.first(), "close_stdin") {
        Ok(p) => {
            lock(&p.stdin).take();
            Value::None
        }
        Err(e) => e,
    }
}

fn with_stdout(args: &[Value], what: &str, f: impl FnOnce(&mut BufReader<ChildStdout>) -> Value) -> Value {
    let p = match proc_of(args.first(), what) {
        Ok(p) => p,
        Err(e) => return e,
    };
    let mut stdout = lock(&p.stdout);
    match stdout.as_mut() {
        Some(out) => f(out),
        None => err(format!("os.{what}: output is not captured; spawn with capture=True")),
    }
}

/// `__process_read_line(p)`: the next line of output without its newline, or
/// `None` at the end.
fn builtin_process_read_line(args: &[Value]) -> Value {
    with_stdout(args, "read_output_line", |out| {
        let mut line = Vec::new();
        match out.read_until(b'\n', &mut line) {
            Ok(0) => Value::None,
            Ok(_) => {
                if line.ends_with(b"\n") {
                    line.pop();
                    if line.ends_with(b"\r") {
                        line.pop();
                    }
                }
                Value::String(rc_str(String::from_utf8_lossy(&line)))
            }
            Err(e) => err(format!("os.read_output_line: {e}")),
        }
    })
}

/// `__process_read_all(p)`: the rest of the output, up to the end.
fn builtin_process_read_all(args: &[Value]) -> Value {
    with_stdout(args, "read_output", |out| {
        let mut all = Vec::new();
        match out.read_to_end(&mut all) {
            Ok(_) => Value::String(rc_str(String::from_utf8_lossy(&all))),
            Err(e) => err(format!("os.read_output: {e}")),
        }
    })
}

/// The exit code once the child has ended: `-1` if a signal killed it.
fn try_exit_code(p: &Proc) -> Result<Option<i64>, String> {
    match lock(&p.child).try_wait() {
        Ok(Some(status)) => Ok(Some(status.code().map_or(-1, i64::from))),
        Ok(None) => Ok(None),
        Err(e) => Err(e.to_string()),
    }
}

/// `__process_wait(p, timeout_ms)`: the exit code, or `None` on timeout.
/// Polls instead of blocking in `wait`, so the child's lock is free for
/// `kill` from another task in the meantime.
fn builtin_process_wait(args: &[Value]) -> Value {
    let p = match proc_of(args.first(), "wait") {
        Ok(p) => p,
        Err(e) => return e,
    };
    let deadline = args
        .get(1)
        .and_then(|v| v.as_integer())
        .map(|ms| Instant::now() + Duration::from_millis(ms.max(0) as u64));
    // Waiting on input it is still being sent would never end.
    lock(&p.stdin).take();
    let mut pause = Duration::from_millis(1);
    loop {
        match try_exit_code(&p) {
            Ok(Some(code)) => return Value::Integer(code),
            Ok(None) => {}
            Err(e) => return err(format!("os.wait: {e}")),
        }
        if deadline.is_some_and(|d| Instant::now() >= d) {
            return Value::None;
        }
        std::thread::sleep(pause);
        pause = (pause * 2).min(Duration::from_millis(20));
    }
}

fn builtin_process_is_running(args: &[Value]) -> Value {
    match proc_of(args.first(), "is_running") {
        Ok(p) => match try_exit_code(&p) {
            Ok(code) => Value::Boolean(code.is_none()),
            Err(e) => err(format!("os.is_running: {e}")),
        },
        Err(e) => e,
    }
}

fn builtin_process_pid(args: &[Value]) -> Value {
    match proc_of(args.first(), "process_pid") {
        Ok(p) => Value::Integer(p.pid as i64),
        Err(e) => e,
    }
}

/// `__process_kill(p, signal)`: send a signal (default `"TERM"`). Killing a
/// child that has already ended does nothing.
fn builtin_process_kill(args: &[Value]) -> Value {
    let p = match proc_of(args.first(), "kill") {
        Ok(p) => p,
        Err(e) => return e,
    };
    if matches!(try_exit_code(&p), Ok(Some(_))) {
        return Value::None;
    }
    let name = str_of(args.get(1)).unwrap_or_else(|| "TERM".to_string());
    match signals::send(&p, &name) {
        Ok(()) => Value::None,
        Err(e) => err(format!("os.kill: {e}")),
    }
}

/// `__signals(names)`: a channel that receives each of these signals by name
/// (`"INT"`, `"TERM"`, ...). Once subscribed, the signal no longer ends the
/// program; the script decides what to do.
fn builtin_signals(args: &[Value]) -> Value {
    let names: Vec<String> = match args.first().map(|v| v.repr()) {
        Some(ValueRepr::Array(a)) => {
            let names: Option<Vec<String>> = a.borrow().iter().map(|v| str_of(Some(v))).collect();
            match names {
                Some(n) if !n.is_empty() => n,
                _ => return err("os.signals takes an array of signal names, like [\"INT\", \"TERM\"]"),
            }
        }
        Some(ValueRepr::String(s)) => vec![s.to_string()],
        _ => return err("os.signals takes an array of signal names, like [\"INT\", \"TERM\"]"),
    };
    match signals::subscribe(&names) {
        Ok(ch) => Value::Integer(ch as i64),
        Err(e) => err(format!("os.signals: {e}")),
    }
}

/// Stop every child still running: SIGTERM, a moment to exit, then SIGKILL.
fn stop_children() {
    // Never block an exiting process on a registry another thread holds.
    let Some(all) = PROCS.get().and_then(|m| m.try_lock().ok()).map(|m| m.values().cloned().collect::<Vec<_>>())
    else {
        return;
    };
    let running: Vec<_> = all.into_iter().filter(|p| matches!(try_exit_code(p), Ok(None))).collect();
    for p in &running {
        // A child that has just exited refuses the signal; the SIGKILL pass
        // below covers any other failure, and the process is ending anyway.
        let _ = signals::send(p, "TERM");
    }
    let deadline = Instant::now() + Duration::from_millis(200);
    for p in &running {
        while matches!(try_exit_code(p), Ok(None)) && Instant::now() < deadline {
            std::thread::sleep(Duration::from_millis(5));
        }
        if matches!(try_exit_code(p), Ok(None)) {
            // The last resort while the process exits: there is nobody left to
            // tell if SIGKILL or reaping fails.
            let _ = lock(&p.child).kill();
            let _ = lock(&p.child).wait();
        }
    }
}

#[cfg(unix)]
mod signals {
    use super::*;
    use std::sync::atomic::AtomicI32;

    const NAMES: &[(&str, libc::c_int)] = &[
        ("INT", libc::SIGINT),
        ("TERM", libc::SIGTERM),
        ("HUP", libc::SIGHUP),
        ("QUIT", libc::SIGQUIT),
        ("USR1", libc::SIGUSR1),
        ("USR2", libc::SIGUSR2),
        ("WINCH", libc::SIGWINCH),
        ("KILL", libc::SIGKILL),
        ("STOP", libc::SIGSTOP),
        ("CONT", libc::SIGCONT),
    ];

    /// The signals that end a program by default, and so end its children.
    const ENDING: &[libc::c_int] = &[libc::SIGINT, libc::SIGTERM, libc::SIGHUP];

    fn number(name: &str) -> Result<libc::c_int, String> {
        let bare = name.trim().trim_start_matches("SIG").to_ascii_uppercase();
        NAMES
            .iter()
            .find(|(n, _)| *n == bare)
            .map(|(_, s)| *s)
            .ok_or_else(|| format!("unknown signal '{name}' (known: {})", NAMES.iter().map(|(n, _)| *n).collect::<Vec<_>>().join(", ")))
    }

    fn name(sig: libc::c_int) -> &'static str {
        NAMES.iter().find(|(_, s)| *s == sig).map_or("?", |(n, _)| n)
    }

    pub(super) fn send(p: &Proc, name: &str) -> Result<(), String> {
        let sig = number(name)?;
        if unsafe { libc::kill(p.pid as libc::pid_t, sig) } == 0 {
            Ok(())
        } else {
            Err(std::io::Error::last_os_error().to_string())
        }
    }

    /// Write end of the self-pipe; the handler's only job is to write to it.
    static PIPE_W: AtomicI32 = AtomicI32::new(-1);
    /// Channels subscribed to each signal number.
    static SUBSCRIBERS: Mutex<Vec<(libc::c_int, u64)>> = Mutex::new(Vec::new());
    static INSTALLED: Mutex<Vec<libc::c_int>> = Mutex::new(Vec::new());

    extern "C" fn on_signal(sig: libc::c_int) {
        let byte = sig as u8;
        let fd = PIPE_W.load(Ordering::Relaxed);
        // write(2) is async-signal-safe; nothing else here may allocate or lock.
        unsafe { libc::write(fd, &byte as *const u8 as *const libc::c_void, 1) };
    }

    /// Start the watcher thread and the self-pipe, once per process. A
    /// failure is remembered, so every later caller reports it too.
    fn start_watcher() -> Result<(), String> {
        static START: OnceLock<Result<(), String>> = OnceLock::new();
        START
            .get_or_init(|| {
                let mut fds = [0 as libc::c_int; 2];
                if unsafe { libc::pipe(fds.as_mut_ptr()) } != 0 {
                    return Err(format!("signal pipe: {}", std::io::Error::last_os_error()));
                }
                let read_fd = fds[0];
                let watcher = std::thread::Builder::new().name("oxigen-signals".into()).spawn(move || {
                    let mut byte = 0u8;
                    while unsafe { libc::read(read_fd, &mut byte as *mut u8 as *mut libc::c_void, 1) } == 1 {
                        deliver(libc::c_int::from(byte));
                    }
                });
                if let Err(e) = watcher {
                    unsafe {
                        libc::close(fds[0]);
                        libc::close(fds[1]);
                    }
                    return Err(format!("signal watcher thread: {e}"));
                }
                PIPE_W.store(fds[1], Ordering::Relaxed);
                Ok(())
            })
            .clone()
    }

    fn deliver(sig: libc::c_int) {
        let subs: Vec<u64> = lock(&SUBSCRIBERS).iter().filter(|(s, _)| *s == sig).map(|(_, ch)| *ch).collect();
        if !subs.is_empty() {
            for ch in subs {
                crate::concurrent::push_str(ch, name(sig));
            }
            return;
        }
        // Nobody asked for it: end the program as the signal would have, and
        // its children with it.
        stop_children();
        unsafe {
            libc::signal(sig, libc::SIG_DFL);
            libc::raise(sig);
        }
    }

    fn install(sig: libc::c_int) -> Result<(), String> {
        let mut installed = lock(&INSTALLED);
        if installed.contains(&sig) {
            return Ok(());
        }
        start_watcher()?;
        unsafe {
            let mut action: libc::sigaction = std::mem::zeroed();
            action.sa_sigaction = on_signal as extern "C" fn(libc::c_int) as usize;
            action.sa_flags = libc::SA_RESTART;
            libc::sigemptyset(&mut action.sa_mask);
            if libc::sigaction(sig, &action, std::ptr::null_mut()) != 0 {
                return Err(std::io::Error::last_os_error().to_string());
            }
        }
        installed.push(sig);
        Ok(())
    }

    pub(super) fn subscribe(names: &[String]) -> Result<u64, String> {
        let sigs = names.iter().map(|n| number(n)).collect::<Result<Vec<_>, _>>()?;
        if let Some(&bad) = sigs.iter().find(|&&s| s == libc::SIGKILL || s == libc::SIGSTOP) {
            return Err(format!("{} cannot be caught", name(bad)));
        }
        let ch = crate::concurrent::new_channel();
        for &sig in &sigs {
            install(sig)?;
            lock(&SUBSCRIBERS).push((sig, ch));
        }
        Ok(ch)
    }

    /// Before the first child: stop children at exit, and on the signals
    /// that would otherwise end the program without running exit hooks.
    pub(super) fn watch_for_exit() -> Result<(), String> {
        static HOOK: OnceLock<Result<(), String>> = OnceLock::new();
        HOOK.get_or_init(|| {
            extern "C" fn at_exit() {
                stop_children();
            }
            if unsafe { libc::atexit(at_exit) } != 0 {
                return Err("atexit failed".into());
            }
            for &sig in ENDING {
                // A signal the host or shell ignores (nohup) stays ignored.
                let mut old: libc::sigaction = unsafe { std::mem::zeroed() };
                unsafe { libc::sigaction(sig, std::ptr::null(), &mut old) };
                if old.sa_sigaction != libc::SIG_IGN {
                    install(sig)?;
                }
            }
            Ok(())
        })
        .clone()
    }
}

// ponytail: no signals on Windows, and children are not stopped at exit there;
// add SetConsoleCtrlHandler and a job object if Windows users need them.
#[cfg(not(unix))]
mod signals {
    use super::*;

    pub(super) fn send(p: &Proc, name: &str) -> Result<(), String> {
        match name.trim_start_matches("SIG") {
            "TERM" | "KILL" | "INT" => lock(&p.child).kill().map_err(|e| e.to_string()),
            _ => Err(format!("signal '{name}' is not supported on this platform")),
        }
    }

    pub(super) fn subscribe(_names: &[String]) -> Result<u64, String> {
        Err("signals are not supported on this platform".into())
    }

    pub(super) fn watch_for_exit() -> Result<(), String> {
        Ok(())
    }
}

pub(crate) fn register(globals: &mut HashMap<String, Value>) {
    let builtins: &[(&str, crate::vm::value::BuiltinFn)] = &[
        ("__spawn_process", builtin_spawn_process),
        ("__process_write", builtin_process_write),
        ("__process_close_stdin", builtin_process_close_stdin),
        ("__process_read_line", builtin_process_read_line),
        ("__process_read_all", builtin_process_read_all),
        ("__process_wait", builtin_process_wait),
        ("__process_is_running", builtin_process_is_running),
        ("__process_pid", builtin_process_pid),
        ("__process_kill", builtin_process_kill),
        ("__signals", builtin_signals),
    ];
    for (name, f) in builtins {
        globals.insert(name.to_string(), Value::Builtin(*f));
    }
}

/// Run `cmd` to completion, feeding it `stdin` when given. The input is
/// written from another thread, so a child that fills its output pipe before
/// reading all its input cannot deadlock against us.
pub(crate) fn output_with_stdin(mut cmd: Command, stdin: Option<String>) -> std::io::Result<std::process::Output> {
    let Some(input) = stdin else {
        return cmd.output();
    };
    let mut child = cmd.stdin(Stdio::piped()).stdout(Stdio::piped()).stderr(Stdio::piped()).spawn()?;
    let mut pipe = child.stdin.take().expect("stdin is piped");
    let writer = std::thread::spawn(move || {
        // A child may exit without reading all its input (`head`), which
        // shows here as a broken pipe; its exit code says how it went.
        let _ = pipe.write_all(input.as_bytes());
    });
    let out = child.wait_with_output();
    // The writer only returns or hits the broken pipe above; it cannot fail
    // in a way the output does not already report.
    let _ = writer.join();
    out
}
