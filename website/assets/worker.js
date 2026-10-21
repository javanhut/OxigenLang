// One OxigenLang run, off the main thread. runner.js starts a worker per run
// and ends it when the run finishes, overruns or is stopped.

const OUTPUT_LIMIT = 1 << 20; // characters forwarded before output is cut off

onmessage = async ({ data: { module, source } }) => {
  const decoder = new TextDecoder();
  let memory;
  let pending = "";
  let sent = 0;
  let flushed = performance.now();
  let panic = "";

  const flush = () => {
    if (pending) postMessage({ type: "out", text: pending });
    pending = "";
    flushed = performance.now();
  };

  const { exports } = await WebAssembly.instantiate(module, {
    oxigen: {
      oxi_host_write(ptr, len) {
        if (sent > OUTPUT_LIMIT) return;
        const text = decoder.decode(new Uint8Array(memory.buffer, ptr, len));
        sent += text.length;
        pending += text;
        if (sent > OUTPUT_LIMIT) pending += "\n… output cut off after 1 MB\n";
        // Batch writes, so a print in a tight loop doesn't flood the page.
        if (pending.length > 16384 || performance.now() - flushed > 40) flush();
      },
      oxi_host_now: () => Date.now(),
      oxi_host_panic(ptr, len) {
        panic = decoder.decode(new Uint8Array(memory.buffer, ptr, len));
      },
    },
  });
  memory = exports.memory;

  const bytes = new TextEncoder().encode(source);
  const ptr = exports.oxi_alloc(bytes.length);
  new Uint8Array(memory.buffer, ptr, bytes.length).set(bytes);
  const started = performance.now();
  try {
    exports.oxi_run(ptr, bytes.length);
  } catch (trap) {
    flush();
    const why = panic || String(trap);
    postMessage({ type: "done", ok: false, error: `Internal interpreter error: ${why}\nPlease report it at https://github.com/javanhut/OxigenLang/issues`, ms: performance.now() - started });
    return;
  }
  const ms = performance.now() - started;
  flush();
  const reply = JSON.parse(decoder.decode(new Uint8Array(memory.buffer, exports.oxi_reply_ptr(), exports.oxi_reply_len())));
  postMessage({ type: "done", ...reply, ms });
};
