// One OxigenLang run, or one of its threads, off the main thread. runner.js
// starts a worker per run and ends it when the run finishes, overruns or is
// stopped.
//
// Two ways to run, by what the page can give:
// - Shared memory (the page is cross-origin isolated): the threads build.
//   The page holds the file system and standard input, and every call to
//   them blocks here until the page answers, so `input()` waits for the
//   user and `diverge` gets a worker per thread.
// - Otherwise: the single-threaded build with its own file system in this
//   worker, standard input given up front, and the files sent back at the end.

import { Exit, Kernel, PresetInput, decode, remote, wasiImports } from "./wasi.js";

const OUTPUT_LIMIT = 1 << 20; // characters forwarded before output is cut off

let sent = 0;
let pending = [];
let pendingSize = 0;
let flushed = performance.now();
const flush = () => {
  for (const [fd, text] of pending) postMessage({ type: "out", fd, text });
  pending = [];
  pendingSize = 0;
  flushed = performance.now();
};
const out = (fd, bytes) => {
  if (sent > OUTPUT_LIMIT) return;
  let text = decode(bytes);
  sent += text.length;
  if (sent > OUTPUT_LIMIT) text += "\n… output cut off after 1 MB\n";
  const last = pending.at(-1);
  if (last && last[0] === fd) last[1] += text;
  else pending.push([fd, text]);
  pendingSize += text.length;
  // Batch writes, so a print in a tight loop doesn't flood the page.
  if (pendingSize > 16384 || performance.now() - flushed > 40) flush();
};

globalThis.onmessage = async ({ data }) => {
  const started = performance.now();
  let memory;
  let exports;
  const shared = !!data.slot;
  let kernel = null;
  let sys;
  if (shared) {
    sys = remote(data.slot, (message) => postMessage(message));
  } else {
    kernel = new Kernel({ stdout: out, stdin: new PresetInput(data.stdin ?? "") });
    for (const [path, contents] of Object.entries(data.files)) kernel.writeFile(path, contents);
    sys = (op, args) => kernel.call(op, args);
  }

  const imports = {
    wasi_snapshot_preview1: wasiImports({
      sys,
      memory: () => memory,
      args: data.args,
      env: data.env,
      out,
      beforeBlock: flush,
    }),
  };
  if (shared) {
    memory = data.memory;
    imports.env = { memory };
    // A new thread: the page starts its worker, as only the page can reach every thread.
    imports.wasi = { "thread-spawn": (arg) => sys("spawn", { arg }).tid };
  }

  const finish = (message) => {
    flush();
    postMessage({ ...message, ms: performance.now() - started, files: kernel?.files(data.cwd) });
  };
  try {
    ({ exports } = await WebAssembly.instantiate(data.module, imports));
    memory ??= exports.memory;
    if (data.thread) {
      exports.wasi_thread_start(data.thread.tid, data.thread.arg);
      flush();
      postMessage({ type: "thread-exit" });
      return;
    }
    const code = exports.oxi_main();
    finish({ type: "done", code });
  } catch (err) {
    if (err instanceof Exit) return finish({ type: "done", code: err.code });
    // A panic printed its message to standard error before trapping.
    finish({ type: "done", code: 101, trap: String(err) });
  }
};
