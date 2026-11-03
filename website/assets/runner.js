// Runs OxigenLang in the browser. The interpreter is the real one, compiled
// to WebAssembly for WASI (crates/oxigen-playground), with the system it
// expects supplied by wasi.js. Each run gets fresh workers and a fresh file
// system, so a run cannot see the last one's state, and an endless loop or a
// long recursion can be stopped by ending the workers.
//
// On a cross-origin isolated page (the playground) runs use the threads build
// and the page answers every system call, so standard input is typed while
// the program waits and `diverge` runs tasks on real threads. Elsewhere (the
// docs pages' Run buttons) a run is single-threaded and its input is given up
// front.

import { Kernel, TypedInput, importedMemory, answer, createSlot, decode, wasiImports } from "./wasi.js";

const meta = (name) => document.querySelector(`meta[name="${name}"]`)?.content;
const WASM_URL = meta("oxigen-wasm");
const THREADS_URL = meta("oxigen-wasm-threads");
const WORKER_URL = meta("oxigen-worker");

/** Seconds a run may compute before it is stopped. Time waiting for input does not count. */
export const TIME_LIMIT = 20;

/** Where things are in a run's file system. */
export const WORK = "/work";
export const HOME = "/home/oxigen";
const STDLIB_DIR = `${HOME}/.oxigen/lib/stdlib`;

/** True when runs can block on input and use threads. */
export const interactive = () => typeof SharedArrayBuffer === "function" && globalThis.crossOriginIsolated === true && !!THREADS_URL;

const modules = new Map();
/**
 * A compiled module, fetched once and shared by every run and tool, with the
 * memory it imports, if it imports one: `{ module, memory }`.
 */
function load(url) {
  if (!modules.has(url)) {
    const compiled = (async () => {
      const response = await fetch(url);
      if (!response.ok) throw new Error(`${url}: ${response.status}`);
      const bytes = await response.arrayBuffer();
      return { module: await WebAssembly.compile(bytes), memory: importedMemory(bytes) };
    })();
    compiled.catch(() => modules.delete(url));
    modules.set(url, compiled);
  }
  return modules.get(url);
}
/** The single-threaded module, for tools that want to instantiate it themselves. */
export const wasmModule = async (url = WASM_URL) => (await load(url)).module;

let stdlibFiles;
async function stdlib() {
  stdlibFiles ??= tools().then((t) => {
    const files = {};
    for (const [name, source] of Object.entries(t.stdlib())) files[`${STDLIB_DIR}/${name}.oxi`] = source;
    return files;
  });
  return stdlibFiles;
}

/**
 * Run a program. `program` is source text, or `{ files: { "main.oxi": text,
 * ... }, main: "main.oxi" }`. Options:
 * - `onOutput(text, fd)`: output as it is printed; fd 1 is standard output.
 *   Standard error comes here too with `stderr: "stream"`; otherwise it is
 *   collected into the result's `error`.
 * - `onWaiting(waiting)`: the program is (or is no longer) waiting for input.
 * - `stdin`: text to read first. `args`: the script's arguments.
 * - `test`: run the `<test>` blocks, as `oxigen test`, instead: those in
 *   `testFiles`, or in the main file.
 * Returns `{ done, stop, input(text), endInput() }`. `done` resolves to
 * `{ ok, code, error?, ms, stopped?, files }`, `files` being the working
 * directory afterwards as { name: bytes }.
 */
export function run(program, opts = {}) {
  const { onOutput = () => {}, onWaiting = () => {}, stdin = "", args = [], test = false, timeLimit = TIME_LIMIT } = opts;
  const files = typeof program === "string" ? { "main.oxi": program } : program.files;
  const main = typeof program === "string" ? "main.oxi" : program.main || "main.oxi";
  const argv = test ? ["oxigen", "test", ...(opts.testFiles ?? [main])] : ["oxigen", main, ...args];
  const env = { HOME, PWD: WORK, USER: "oxigen", LANG: "C.UTF-8", TERM: "dumb" };

  const workers = new Set();
  let stderr = "";
  let finish;
  let ended = false;
  let kernel = null;
  let typed = null;
  let waiting = [];
  let busyMs = 0;
  let tick;
  const done = new Promise((resolve) => (finish = resolve));
  const started = performance.now();

  const output = (fd, text) => {
    if (fd === 2 && opts.stderr !== "stream") stderr += text;
    else onOutput(text, fd);
  };
  const end = (result) => {
    if (ended) return;
    ended = true;
    clearInterval(tick);
    for (const w of workers) w.terminate();
    workers.clear();
    if (waiting.length) onWaiting(false);
    const out = result.files ?? (kernel ? kernel.files(WORK) : {});
    const ok = result.code === 0 && !result.stopped;
    let error = result.error;
    if (!error && !ok) error = stderr.trimEnd() || (result.trap ? `Internal interpreter error: ${result.trap}\nPlease report it at https://github.com/javanhut/OxigenLang/issues` : `Exited with code ${result.code}`);
    finish({ ok, code: result.code, error, ms: result.ms ?? performance.now() - started, stopped: result.stopped, files: out });
  };

  // Only computing counts towards the limit, not waiting for the user.
  tick = setInterval(() => {
    if (!waiting.length) busyMs += 250;
    if (busyMs > timeLimit * 1000) end({ code: 124, stopped: true, error: `Stopped after ${timeLimit} seconds. The playground limits how long a program may run.` });
  }, 250);

  const retry = () => {
    const still = [];
    for (const call of waiting) {
      const r = kernel.call("read", call.args);
      if (r.wait) still.push(call);
      else answer(call.slot, r);
    }
    const was = waiting.length;
    waiting = still;
    if (!!was !== !!waiting.length) onWaiting(!!waiting.length);
  };

  const fail = (message) => end({ code: 1, error: message });
  const listen = (worker, slot) => {
    worker.onmessage = ({ data }) => {
      if (ended) return;
      if (data.type === "out") output(data.fd, data.text);
      else if (data.type === "sys") {
        if (data.op === "spawn") return answer(slot, { e: 0, tid: spawn(data.args.arg) });
        const r = kernel.call(data.op, data.args);
        if (r.wait) {
          waiting.push({ slot, args: data.args });
          if (waiting.length === 1) onWaiting(true);
        } else answer(slot, r);
      } else if (data.type === "thread-exit") {
        worker.terminate();
        workers.delete(worker);
      } else if (data.type === "done") end(data);
    };
    worker.onerror = (e) => fail(`The playground worker failed: ${e.message || "unknown error"}`);
  };

  let threads = null;
  let nextTid = 1;
  const spawn = (arg) => {
    const tid = nextTid++;
    const slot = createSlot();
    const worker = new Worker(WORKER_URL, { type: "module" });
    workers.add(worker);
    listen(worker, slot);
    worker.postMessage({ module: threads.module, memory: threads.memory, slot, args: argv, env, thread: { tid, arg } });
    return tid;
  };

  (async () => {
    const shared = interactive();
    const [{ module, memory: limits }, lib] = await Promise.all([load(shared ? THREADS_URL : WASM_URL), stdlib()]);
    if (ended) return;
    const all = { ...lib };
    for (const [name, text] of Object.entries(files)) all[`${WORK}/${name}`] = text;
    const worker = new Worker(WORKER_URL, { type: "module" });
    workers.add(worker);
    if (shared) {
      typed = new TypedInput(stdin);
      kernel = new Kernel({ stdout: (fd, bytes) => output(fd, decode(bytes)), stdin: typed });
      for (const [path, text] of Object.entries(all)) kernel.writeFile(path, text);
      const memory = new WebAssembly.Memory(limits);
      threads = { module, memory };
      const slot = createSlot();
      listen(worker, slot);
      worker.postMessage({ module, memory, slot, args: argv, env, cwd: WORK });
    } else {
      listen(worker, null);
      worker.postMessage({ module, files: all, stdin, args: argv, env, cwd: WORK });
    }
  })().catch(() => fail("Could not load the OxigenLang WebAssembly module. Check your connection and try again."));

  return {
    done,
    stop: () => end({ code: 130, stopped: true, error: "Stopped." }),
    input(text) {
      if (!typed) return false;
      typed.push(text);
      retry();
      return true;
    },
    endInput() {
      if (!typed) return;
      typed.end();
      retry();
    },
  };
}

// Loops, calls, methods, closures, maps and the stdlib: enough for the
// browser to optimise the interpreter's hot paths, which it only does for
// code it has seen run. Without it the first run is about half as fast.
const WARM_UP = `introduce array
struct Point { x <int>  y <int> }
Point includes {
    fun sum() { x + y }
}
fun fib(n <int>) {
    give n when n < 2
    fib(n - 1) + fib(n - 2)
}
main {
    total := 0
    each i in range(0, 20000) {
        p := Point(i, 2)
        total = total + p.sum()
    }
    words := {}
    each i in range(0, 3000) {
        words["k{i % 50}"] = i
    }
    squares := array.map(range(0, 3000), fun(n) { n * n })
    println(total, len(words), len(squares), fib(18))
}
`;
let warmed;
/** Run a short program, unseen, so the first real run is at full speed. */
export function warmUp() {
  warmed ??= run(WARM_UP, { timeLimit: 10 }).done;
  return warmed;
}

let toolsPromise;
/**
 * Synchronous helpers on the main thread, for the editor: highlight, tokens,
 * format and check. They never run user code, so they cannot hang the page.
 */
export function tools() {
  toolsPromise ??= wasmModule().then(async (module) => {
    let memory;
    const kernel = new Kernel();
    const { exports } = await WebAssembly.instantiate(module, {
      wasi_snapshot_preview1: wasiImports({ sys: (op, a) => kernel.call(op, a), memory: () => memory }),
    });
    memory = exports.memory;
    const encoder = new TextEncoder();
    const reply = () => decode(new Uint8Array(memory.buffer, exports.oxi_reply_ptr(), exports.oxi_reply_len()));
    const call = (name, text) => {
      const bytes = encoder.encode(text);
      const ptr = exports.oxi_alloc(bytes.length);
      new Uint8Array(memory.buffer, ptr, bytes.length).set(bytes);
      exports[name](ptr, bytes.length);
      exports.oxi_free(ptr, bytes.length);
      return reply();
    };
    return {
      highlight: (text) => call("oxi_highlight", text),
      tokens: (text) => JSON.parse(call("oxi_tokens", text)),
      format: (text) => JSON.parse(call("oxi_format", text)),
      check: (text) => JSON.parse(call("oxi_check", text)),
      stdlib: () => (exports.oxi_stdlib(), JSON.parse(reply())),
    };
  });
  toolsPromise.catch(() => (toolsPromise = undefined));
  return toolsPromise;
}

/** The line a rendered error points at (`--> main.oxi:4:14`), if any. */
export function errorLine(error, file = "main.oxi") {
  const escaped = file.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const match = new RegExp(`-->\\s*(?:${escaped}|line)\\s*:?\\s*(\\d+)`).exec(error || "");
  return match ? Number(match[1]) : null;
}

// ── Share links: #code=<z|b><base64url> (z: deflate-raw, b: plain UTF-8) ──
// Several files share as #files=<z|b><base64url of JSON { name: text }>.

const toBase64Url = (bytes) => {
  let binary = "";
  for (let i = 0; i < bytes.length; i += 0x8000) binary += String.fromCharCode(...bytes.subarray(i, i + 0x8000));
  return btoa(binary).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
};
const fromBase64Url = (text) => {
  const binary = atob(text.replace(/-/g, "+").replace(/_/g, "/"));
  return Uint8Array.from(binary, (c) => c.charCodeAt(0));
};
const pipe = async (bytes, stream) => new Uint8Array(await new Response(new Blob([bytes]).stream().pipeThrough(stream)).arrayBuffer());

export async function encodeSource(source) {
  const bytes = new TextEncoder().encode(source);
  if (typeof CompressionStream === "function") {
    try {
      return "z" + toBase64Url(await pipe(bytes, new CompressionStream("deflate-raw")));
    } catch {}
  }
  return "b" + toBase64Url(bytes);
}

export async function decodeSource(encoded) {
  const bytes = fromBase64Url(encoded.slice(1));
  if (encoded[0] === "z") return new TextDecoder().decode(await pipe(bytes, new DecompressionStream("deflate-raw")));
  return new TextDecoder().decode(bytes);
}

/** A playground link to `source`, or to several files `{ name: text }` (main.oxi first). */
export async function playgroundUrl(source) {
  const base = document.documentElement.dataset.base || "";
  if (typeof source === "string") return `${base}/playground/#code=${await encodeSource(source)}`;
  return `${base}/playground/#files=${await encodeSource(JSON.stringify(source))}`;
}
