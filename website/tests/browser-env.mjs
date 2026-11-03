// A browser-shaped environment in Node for running the playground's runner:
// `document` with the module addresses, a module `Worker` on worker_threads,
// and cross-origin isolation (Node has SharedArrayBuffer and Atomics.wait).
import { Worker as NodeWorker } from "node:worker_threads";
import { readFile } from "node:fs/promises";
import { fileURLToPath, pathToFileURL } from "node:url";

const assets = new URL("../assets/", import.meta.url);

/** Install the globals. `wasm` and `threads` are paths to the two builds. */
export function install({ wasm, threads, isolated = true }) {
  const metas = {
    "oxigen-wasm": pathToFileURL(wasm).href,
    "oxigen-wasm-threads": threads ? pathToFileURL(threads).href : undefined,
    "oxigen-worker": new URL("worker.js", assets).href,
  };
  globalThis.document = {
    querySelector: (sel) => {
      const name = /name="([^"]+)"/.exec(sel)?.[1];
      return metas[name] ? { content: metas[name] } : null;
    },
    documentElement: { dataset: {} },
  };
  globalThis.crossOriginIsolated = isolated;
  // fetch() of file: URLs, for compileStreaming's fallback.
  globalThis.fetch = async (url) => new Response(await readFile(fileURLToPath(url)), { headers: { "content-type": "application/wasm" } });
  globalThis.Worker = class {
    constructor(url) {
      this.worker = new NodeWorker(new URL("./worker-shim.mjs", import.meta.url), { workerData: { url: String(url) } });
      this.worker.on("message", (data) => this.onmessage?.({ data }));
      this.worker.on("error", (e) => this.onerror?.(e));
    }
    postMessage(message) {
      this.worker.postMessage(message);
    }
    terminate() {
      this.worker.terminate();
    }
  };
}
