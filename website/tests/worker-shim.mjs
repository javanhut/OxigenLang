// Inside a worker_threads worker: the dedicated-worker globals worker.js uses.
import { parentPort, workerData } from "node:worker_threads";
import { install } from "./browser-env.mjs";

globalThis.postMessage = (message) => parentPort.postMessage(message);
globalThis.crossOriginIsolated = true;
// Nested workers are only made by the page here, but keep `Worker` defined.
install({ wasm: "", isolated: true });
const queued = [];
parentPort.on("message", (data) => (globalThis.onmessage ? globalThis.onmessage({ data }) : queued.push(data)));
await import(workerData.url);
for (const data of queued) globalThis.onmessage({ data });
