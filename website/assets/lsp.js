// A client for the Oxigen language server running in lsp-worker.js: JSON-RPC
// over the server's standard input and output, framed the way LSP frames it
// (a Content-Length header, a blank line, the body).

import { SabPipe } from "./wasi.js";

const encoder = new TextEncoder();
const decoder = new TextDecoder();

/**
 * Start the server. `module` is its compiled WebAssembly, `files` the file
 * system it starts with ({ absolute path: text }). Resolves once it has
 * answered `initialize`.
 */
export async function start({ module, workerUrl, files, env, rootUri = "file:///work" }) {
  const sab = SabPipe.create();
  const pipe = new SabPipe(sab);
  const worker = new Worker(workerUrl, { type: "module" });
  const pending = new Map();
  const handlers = new Map();
  let nextId = 1;
  let buffer = new Uint8Array(0);
  let exited = null;

  const send = (message) => {
    const body = encoder.encode(JSON.stringify({ jsonrpc: "2.0", ...message }));
    const head = encoder.encode(`Content-Length: ${body.length}\r\n\r\n`);
    const framed = new Uint8Array(head.length + body.length);
    framed.set(head);
    framed.set(body, head.length);
    pipe.write(framed);
  };

  const receive = (message) => {
    if (message.id != null && pending.has(message.id) && !message.method) {
      const { resolve, reject } = pending.get(message.id);
      pending.delete(message.id);
      if (message.error) reject(Object.assign(new Error(message.error.message), message.error));
      else resolve(message.result);
    } else if (message.method) {
      handlers.get(message.method)?.(message.params);
      // A request from the server that needs an answer, such as workspace/configuration.
      if (message.id != null) send({ id: message.id, result: null });
    }
  };

  // Split the byte stream into messages.
  const parse = () => {
    for (;;) {
      let headerEnd = -1;
      for (let i = 0; i + 3 < buffer.length; i++) {
        if (buffer[i] === 13 && buffer[i + 1] === 10 && buffer[i + 2] === 13 && buffer[i + 3] === 10) {
          headerEnd = i;
          break;
        }
      }
      if (headerEnd < 0) return;
      const header = decoder.decode(buffer.subarray(0, headerEnd));
      const length = Number(/Content-Length:\s*(\d+)/i.exec(header)?.[1]);
      const start = headerEnd + 4;
      if (!(length >= 0) || buffer.length < start + length) return;
      const body = decoder.decode(buffer.subarray(start, start + length));
      buffer = buffer.slice(start + length);
      try {
        receive(JSON.parse(body));
      } catch {}
    }
  };

  worker.onmessage = ({ data }) => {
    if (data.type === "out") {
      const joined = new Uint8Array(buffer.length + data.bytes.length);
      joined.set(buffer);
      joined.set(data.bytes, buffer.length);
      buffer = joined;
      parse();
    } else if (data.type === "exit") {
      exited = data;
      for (const { reject } of pending.values()) reject(new Error(data.error || "the language server exited"));
      pending.clear();
      handlers.get("$/exit")?.(data);
    }
  };
  worker.postMessage({ module, pipe: sab, files, env });

  const client = {
    request(method, params) {
      if (exited) return Promise.reject(new Error("the language server exited"));
      const id = nextId++;
      send({ id, method, params });
      return new Promise((resolve, reject) => pending.set(id, { resolve, reject }));
    },
    notify(method, params) {
      if (!exited) send({ method, params });
    },
    on(method, handler) {
      handlers.set(method, handler);
    },
    stop() {
      worker.terminate();
    },
  };

  client.capabilities = (
    await client.request("initialize", {
      processId: null,
      rootUri,
      capabilities: {
        textDocument: {
          completion: { completionItem: { snippetSupport: false, documentationFormat: ["markdown", "plaintext"] } },
          hover: { contentFormat: ["markdown", "plaintext"] },
          synchronization: { didSave: false },
        },
      },
    })
  ).capabilities;
  client.notify("initialized", {});
  return client;
}
