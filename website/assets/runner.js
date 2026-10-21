// Runs OxigenLang in the browser. The interpreter is the real one, compiled
// to WebAssembly (crates/oxigen-playground). Each run gets a fresh worker and
// a fresh instance, so a run cannot see the last one's state, and an endless
// loop or a long recursion can be stopped by ending the worker.

const meta = (name) => document.querySelector(`meta[name="${name}"]`)?.content;
const WASM_URL = meta("oxigen-wasm");
const WORKER_URL = meta("oxigen-worker");

/** Seconds a run may take before it is stopped. */
export const TIME_LIMIT = 20;

let compiled;
/** The compiled module, fetched once and shared by every run and tool. */
export function wasmModule() {
  compiled ??= (async () => {
    const response = fetch(WASM_URL);
    try {
      return await WebAssembly.compileStreaming(response);
    } catch {
      // A server without the application/wasm type defeats streaming compile.
      return WebAssembly.compile(await (await fetch(WASM_URL)).arrayBuffer());
    }
  })();
  compiled.catch(() => (compiled = undefined));
  return compiled;
}

/**
 * Run `source`. `onOutput(text)` receives output as it is printed. Returns
 * `{ done, stop }`: `done` resolves to `{ ok, error?, ms, stopped? }`.
 */
export function run(source, { onOutput = () => {}, timeLimit = TIME_LIMIT } = {}) {
  let worker;
  let finish;
  let timer;
  let ended = false;
  const done = new Promise((resolve) => (finish = resolve));
  const end = (result) => {
    if (ended) return;
    ended = true;
    clearTimeout(timer);
    worker?.terminate();
    worker = null;
    finish(result);
  };
  const started = performance.now();
  wasmModule().then(
    (module) => {
      // Stopped while the module was still downloading.
      if (ended) return;
      worker = new Worker(WORKER_URL);
      worker.onmessage = ({ data }) => {
        if (data.type === "out") onOutput(data.text);
        else if (data.type === "done") end(data);
      };
      worker.onerror = (e) => end({ ok: false, error: `The playground worker failed: ${e.message || "unknown error"}` });
      timer = setTimeout(
        () => end({ ok: false, stopped: true, error: `Stopped after ${timeLimit} seconds. The playground limits how long a program may run.`, ms: performance.now() - started }),
        timeLimit * 1000,
      );
      worker.postMessage({ module, source });
    },
    () => end({ ok: false, error: "Could not load the OxigenLang WebAssembly module. Check your connection and try again." }),
  );
  return {
    done,
    stop: () => end({ ok: false, stopped: true, error: "Stopped.", ms: performance.now() - started }),
  };
}

let toolsPromise;
/**
 * Synchronous helpers on the main thread, for the editor: highlight, format
 * and check. They never run user code, so they cannot hang the page.
 */
export function tools() {
  toolsPromise ??= wasmModule().then(async (module) => {
    let memory;
    const { exports } = await WebAssembly.instantiate(module, {
      oxigen: { oxi_host_write() {}, oxi_host_now: () => Date.now(), oxi_host_panic() {} },
    });
    memory = exports.memory;
    const encoder = new TextEncoder();
    const decoder = new TextDecoder();
    const call = (name, text) => {
      const bytes = encoder.encode(text);
      const ptr = exports.oxi_alloc(bytes.length);
      new Uint8Array(memory.buffer, ptr, bytes.length).set(bytes);
      exports[name](ptr, bytes.length);
      exports.oxi_free(ptr, bytes.length);
      return decoder.decode(new Uint8Array(memory.buffer, exports.oxi_reply_ptr(), exports.oxi_reply_len()));
    };
    return {
      highlight: (text) => call("oxi_highlight", text),
      format: (text) => JSON.parse(call("oxi_format", text)),
      check: (text) => JSON.parse(call("oxi_check", text)),
    };
  });
  toolsPromise.catch(() => (toolsPromise = undefined));
  return toolsPromise;
}

/** The line a rendered error points at (`--> main.oxi:4:14`), if any. */
export function errorLine(error) {
  const match = /-->\s*(?:main\.oxi|line)\s*:?\s*(\d+)/.exec(error || "");
  return match ? Number(match[1]) : null;
}

// ── Share links: #code=<z|b><base64url> (z: deflate-raw, b: plain UTF-8) ──

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

export async function playgroundUrl(source) {
  const base = document.documentElement.dataset.base || "";
  return `${base}/playground/#code=${await encodeSource(source)}`;
}
