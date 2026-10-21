// The playground editor: a textarea over a highlighted copy of its text.
// Highlighting, formatting and error checking come from the real lexer,
// parser and formatter in the WebAssembly module, so what the editor shows
// is what the language accepts.

const runnerUrl = document.querySelector('meta[name="oxigen-runner"]').content;
const DRAFT = "oxigen-playground-draft";

const store = {
  get: (k) => { try { return localStorage.getItem(k); } catch { return null; } },
  set: (k, v) => { try { localStorage.setItem(k, v); } catch {} },
};
const escapeHtml = (s) => s.replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" })[c]);

// Loaded as a module, which runs after the document is parsed.
(async () => {
  const runner = await import(runnerUrl);
  const examples = JSON.parse(document.getElementById("pg-examples").textContent);
  const el = (id) => document.getElementById(id);
  const area = el("pg-src");
  const code = el("pg-hl");
  const scroller = el("pg-code");
  const gutter = el("pg-gutter");
  const output = el("pg-out");
  const status = el("pg-status");
  const runBtn = el("pg-run");
  const picker = el("pg-example");

  let tools = null;
  let job = null;
  let errorLines = new Map();

  const setStatus = (text, state = "ready") => {
    status.textContent = text;
    status.dataset.state = state;
  };

  // ── Rendering the editor ──
  const paint = () => {
    const text = area.value;
    // A trailing newline needs a character after it to take up a line.
    code.innerHTML = (tools ? tools.highlight(text) : escapeHtml(text)) + "\n ";
    const lines = text.split("\n").length;
    let numbers = "";
    for (let i = 1; i <= lines; i++) numbers += errorLines.has(i) ? `<span class="err-line" title="${escapeHtml(errorLines.get(i))}">${i}</span>\n` : `${i}\n`;
    gutter.innerHTML = numbers;
    const pre = code.parentElement;
    area.style.height = "0px";
    area.style.width = "0px";
    area.style.height = `${Math.max(pre.scrollHeight, scroller.clientHeight)}px`;
    area.style.width = `${Math.max(pre.scrollWidth, scroller.clientWidth)}px`;
  };
  let frame = 0;
  const schedule = () => {
    cancelAnimationFrame(frame);
    frame = requestAnimationFrame(paint);
  };
  scroller.addEventListener("scroll", () => (gutter.scrollTop = scroller.scrollTop));
  addEventListener("resize", schedule);

  // ── Live diagnostics ──
  let checkTimer = 0;
  const check = () => {
    clearTimeout(checkTimer);
    checkTimer = setTimeout(() => {
      if (!tools) return;
      errorLines = new Map();
      for (const d of tools.check(area.value)) if (d.severity === "error") errorLines.set(d.line, d.message);
      paint();
    }, 350);
  };

  const setSource = (text) => {
    area.value = text;
    errorLines = new Map();
    paint();
    check();
  };

  // ── Editing keys ──
  const insert = (text) => {
    // execCommand keeps the browser's undo history; setRangeText is the fallback.
    if (!document.execCommand("insertText", false, text)) area.setRangeText(text, area.selectionStart, area.selectionEnd, "end");
  };
  let escaped = false;
  area.addEventListener("keydown", (e) => {
    if ((e.ctrlKey || e.metaKey) && e.key === "Enter") return e.preventDefault(), start();
    if ((e.ctrlKey || e.metaKey) && e.key.toLowerCase() === "s") return e.preventDefault(), format();
    if (e.key === "Escape") return (escaped = true);
    if (e.key === "Tab" && !escaped) {
      e.preventDefault();
      const { value, selectionStart: s } = area;
      const lineStart = value.lastIndexOf("\n", s - 1) + 1;
      if (e.shiftKey) {
        const spaces = /^ {1,4}/.exec(value.slice(lineStart))?.[0].length ?? 0;
        if (spaces) {
          area.setSelectionRange(lineStart, lineStart + spaces);
          insert("");
          area.setSelectionRange(s - spaces, s - spaces);
        }
      } else insert("    ");
      return;
    }
    escaped = false;
    if (e.key === "Enter" && !e.shiftKey && !e.altKey) {
      e.preventDefault();
      const { value, selectionStart: s } = area;
      const line = value.slice(value.lastIndexOf("\n", s - 1) + 1, s);
      let indent = /^\s*/.exec(line)[0];
      if (/[{(\[:]\s*$/.test(line)) indent += "    ";
      insert("\n" + indent);
    }
  });
  area.addEventListener("input", () => {
    paint();
    check();
    store.set(DRAFT, area.value);
  });

  // ── Running ──
  const write = (html) => output.insertAdjacentHTML("beforeend", html);
  async function start() {
    if (job) return job.stop();
    output.textContent = "";
    errorLines = new Map();
    paint();
    setStatus("Running…", "running");
    runBtn.classList.add("stop");
    runBtn.lastChild.textContent = " Stop";
    const source = area.value;
    job = runner.run(source, { onOutput: (text) => output.append(text) });
    const result = await job.done;
    job = null;
    runBtn.classList.remove("stop");
    runBtn.lastChild.textContent = " Run";
    const ms = result.ms != null ? (result.ms < 10 ? result.ms.toFixed(1) : Math.round(result.ms)) + " ms" : "";
    if (result.ok) {
      if (!output.textContent) write('<span class="note">The program finished without printing anything.</span>');
      setStatus(`Finished in ${ms}`);
    } else {
      write(`${output.textContent && !output.textContent.endsWith("\n") ? "\n" : ""}<span class="err">${escapeHtml(result.error || "Error")}</span>`);
      const line = runner.errorLine(result.error);
      if (line) errorLines.set(line, result.error.split("\n")[0]), paint();
      setStatus(result.stopped ? "Stopped" : `Error${ms ? ` after ${ms}` : ""}`, "error");
    }
  }
  runBtn.addEventListener("click", start);

  async function format() {
    if (!tools) return;
    const result = tools.format(area.value);
    if (!result.ok) {
      setStatus("Fix the syntax errors before formatting", "error");
      return check();
    }
    area.focus();
    area.select();
    insert(result.source);
    setStatus("Formatted");
  }
  el("pg-format").addEventListener("click", format);

  el("pg-share").addEventListener("click", async () => {
    const url = new URL(await runner.playgroundUrl(area.value), location.href).href;
    history.replaceState(null, "", url);
    try {
      await navigator.clipboard.writeText(url);
      setStatus("Link copied to the clipboard");
    } catch {
      setStatus("Link is in the address bar");
    }
  });

  el("pg-clear").addEventListener("click", () => (output.innerHTML = ""));

  picker.addEventListener("change", () => {
    const example = examples.find((x) => x.slug === picker.value);
    if (!example) return;
    setSource(example.source);
    history.replaceState(null, "", location.pathname);
    store.set(DRAFT, example.source);
    output.innerHTML = `<span class="placeholder">${escapeHtml(example.summary)}\nPress Run or Ctrl+Enter.</span>`;
  });
  el("pg-reset").addEventListener("click", () => picker.dispatchEvent(new Event("change")));

  // ── Starting source: a share link, else the last draft, else the first example ──
  const hash = new URLSearchParams(location.hash.slice(1)).get("code");
  let initial = null;
  if (hash) {
    try {
      initial = await runner.decodeSource(hash);
    } catch {
      setStatus("That share link could not be read", "error");
    }
  }
  initial ??= store.get(DRAFT);
  if (initial != null && initial !== area.value) setSource(initial);
  else paint();

  setStatus("Loading interpreter…", "running");
  try {
    tools = await runner.tools();
    paint();
    check();
    setStatus("Ready · press Ctrl+Enter to run");
  } catch {
    setStatus("Could not load the interpreter", "error");
  }
  area.focus();
})();
