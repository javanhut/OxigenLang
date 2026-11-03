// The Oxigen language server (lsp-go, compiled for WASI) in a worker. The
// page writes LSP messages into a shared pipe that is the server's standard
// input, and gets its standard output back as messages. The server reads
// the stdlib from its usual place under $HOME, for module completions and
// hovers; the diagnostics and formatting it would get by running `oxigen`
// come from the interpreter's own WebAssembly on the page instead.

import { Exit, Kernel, SabPipe, decode, wasiImports } from "./wasi.js";

globalThis.onmessage = async ({ data }) => {
  const pipe = new SabPipe(data.pipe);
  const kernel = new Kernel({
    stdout: (fd, bytes) => (fd === 1 ? postMessage({ type: "out", bytes }, [bytes.buffer]) : postMessage({ type: "log", text: decode(bytes) })),
    stdin: pipe,
  });
  for (const [path, text] of Object.entries(data.files)) kernel.writeFile(path, text);
  let memory;
  const imports = {
    wasi_snapshot_preview1: wasiImports({
      sys: (op, args) => kernel.call(op, args),
      memory: () => memory,
      args: ["oxigen-lsp"],
      env: data.env,
      input: { ready: () => pipe.available() > 0, wait: (ms) => pipe.wait(ms) },
    }),
  };
  try {
    const { exports } = await WebAssembly.instantiate(data.module, imports);
    memory = exports.memory;
    exports._start();
    postMessage({ type: "exit", code: 0 });
  } catch (err) {
    postMessage({ type: "exit", code: err instanceof Exit ? err.code : 1, error: err instanceof Exit ? undefined : String(err) });
  }
};
