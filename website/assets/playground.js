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
  const highlight = code.parentElement.parentElement;
  const position = el("pg-position");
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
  const active = document.createElement("div");
  active.className = "pg-active";
  active.setAttribute("aria-hidden", "true");
  scroller.prepend(active);
  const lineHeight = () => parseFloat(getComputedStyle(area).lineHeight);
  const padTop = () => parseFloat(getComputedStyle(area).paddingTop);
  const caretPosition = () => area.selectionDirection === "backward" ? area.selectionStart : area.selectionEnd;
  const syncScroll = () => {
    code.parentElement.style.transform = `translate(${-area.scrollLeft}px, ${-area.scrollTop}px)`;
    // Clip the copy before the native scrollbars, including on platforms
    // whose scrollbars take up layout space.
    highlight.style.width = `${area.clientWidth}px`;
    highlight.style.height = `${area.clientHeight}px`;
    gutter.scrollTop = area.scrollTop;
    markLine();
  };

  // The caret's line (1-based) and visual column, tabs counted as 4.
  const caretAt = (pos) => {
    const before = area.value.slice(0, pos);
    const lineStart = before.lastIndexOf("\n") + 1;
    let col = 0;
    for (const c of before.slice(lineStart)) col = c === "\t" ? col + 4 - (col % 4) : col + 1;
    return { line: before.split("\n").length, col };
  };
  let currentLine = 0;
  const markLine = () => {
    const { line, col } = caretAt(caretPosition());
    position.textContent = `Ln ${line}, Col ${col + 1}`;
    active.style.top = `${padTop() + (line - 1) * lineHeight() - area.scrollTop}px`;
    active.style.height = `${lineHeight()}px`;
    active.hidden = document.activeElement !== area || area.selectionStart !== area.selectionEnd;
    if (line !== currentLine) {
      gutter.querySelector(".cur")?.classList.remove("cur");
      gutter.children[line - 1]?.classList.add("cur");
      currentLine = line;
    }
  };
  // Programmatic edits need to reveal the final selection. Measure the
  // actual rendered caret, so tabs, Unicode and font changes stay accurate.
  // Ordinary typing, navigation and selection use native textarea scrolling.
  const reveal = () => {
    let remaining = caretPosition();
    const walker = document.createTreeWalker(code, NodeFilter.SHOW_TEXT);
    let node;
    while ((node = walker.nextNode())) {
      if (remaining <= node.length) break;
      remaining -= node.length;
    }
    if (!node) return;
    const range = document.createRange();
    range.setStart(node, remaining);
    range.collapse(true);
    const rect = range.getBoundingClientRect();
    const bounds = area.getBoundingClientRect();
    const top = padTop() + (caretAt(caretPosition()).line - 1) * lineHeight();
    if (top < area.scrollTop) area.scrollTop = top;
    else if (top + lineHeight() > area.scrollTop + area.clientHeight) area.scrollTop = top + lineHeight() - area.clientHeight;
    if (rect.height) {
      if (rect.left < bounds.left + 8) area.scrollLeft -= bounds.left + 8 - rect.left;
      else if (rect.right > bounds.left + area.clientWidth - 20) area.scrollLeft += rect.right - (bounds.left + area.clientWidth - 20);
    }
    syncScroll();
  };

  const paint = () => {
    const text = area.value;
    // A sentinel gives empty text and a trailing newline their final line.
    code.innerHTML = (tools ? tools.highlight(text) : escapeHtml(text)) + " ";
    const lines = text.split("\n").length;
    let numbers = "";
    for (let i = 1; i <= lines; i++) numbers += errorLines.has(i) ? `<span class="err-line" title="${escapeHtml(errorLines.get(i))}">${i}</span>` : `<span>${i}</span>`;
    gutter.innerHTML = numbers;
    currentLine = 0;
    // Match the gutter's scroll range even when a horizontal scrollbar
    // reduces the textarea's visible height.
    gutter.style.paddingBottom = `${20 + area.offsetHeight - area.clientHeight}px`;
    syncScroll();
  };
  let frame = 0;
  const schedule = () => {
    cancelAnimationFrame(frame);
    frame = requestAnimationFrame(paint);
  };
  area.addEventListener("scroll", syncScroll);
  new ResizeObserver(schedule).observe(scroller);
  addEventListener("resize", schedule);
  document.addEventListener("selectionchange", () => document.activeElement === area && markLine());
  area.addEventListener("focus", markLine);
  area.addEventListener("blur", markLine);
  document.fonts?.ready.then(paint);

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
    area.setSelectionRange(0, 0);
    area.scrollTop = area.scrollLeft = 0;
    errorLines = new Map();
    paint();
    check();
  };

  // ── Editing keys ──
  // Use the browser's undo history for custom edits as well as native typing.
  const INDENT = "    ";
  const PAIRS = { "{": "}", "(": ")", "[": "]", '"': '"' };
  const CLOSERS = new Set(["}", ")", "]"]);
  // Pairs closed as they are typed. Not braces: a block's closing brace is
  // typed on its own line, where it would otherwise end up twice.
  const AUTO = new Set(["(", "[", '"']);
  const insert = (text) => {
    // execCommand keeps the browser's undo history; setRangeText is the fallback.
    if (!document.execCommand("insertText", false, text)) {
      area.setRangeText(text, area.selectionStart, area.selectionEnd, "end");
      area.dispatchEvent(new Event("input"));
    }
  };
  // Replace [from, to) with `text`, then select [selFrom, selTo).
  const replace = (from, to, text, selFrom, selTo = selFrom, direction = "none") => {
    area.setSelectionRange(from, to);
    insert(text);
    area.setSelectionRange(selFrom, selTo, direction);
    markLine();
    reveal();
  };
  const lineStartOf = (pos) => pos === 0 ? 0 : area.value.lastIndexOf("\n", pos - 1) + 1;
  const lineEndOf = (pos) => {
    const end = area.value.indexOf("\n", pos);
    return end === -1 ? area.value.length : end;
  };
  // The whole lines a selection touches; a selection ending at the start of
  // a line doesn't take that line.
  const selectedLines = () => {
    const { selectionStart: s, selectionEnd: e } = area;
    const from = lineStartOf(s);
    const to = lineEndOf(e > s && area.value[e - 1] === "\n" ? e - 1 : e);
    return { from, to, lines: area.value.slice(from, to).split("\n") };
  };
  const editLines = (fn) => {
    const { selectionStart: s, selectionEnd: e, selectionDirection: direction } = area;
    const { from, to, lines } = selectedLines();
    const edited = lines.map(fn);
    const text = edited.join("\n");
    if (text === area.value.slice(from, to)) return;
    // Keep both selection endpoints on their original text, including a
    // backward selection and one ending at the start of the following line.
    const mapPosition = (pos) => {
      let oldStart = from, newStart = from;
      for (let i = 0; i < lines.length; i++) {
        if (pos <= oldStart + lines[i].length) {
          return newStart + Math.max(0, pos - oldStart + edited[i].length - lines[i].length);
        }
        oldStart += lines[i].length + 1;
        newStart += edited[i].length + 1;
      }
      return pos + text.length - (to - from);
    };
    replace(from, to, text, mapPosition(s), mapPosition(e), direction);
  };
  const outdent = (line) => line.replace(/^(\t| {1,4})/, "");

  const indentKey = (shift) => {
    const { selectionStart: s, selectionEnd: e, value } = area;
    if (shift) {
      // Shift+Tab with a bare caret outdents its line and keeps the caret on the same text.
      if (s === e) {
        const from = lineStartOf(s);
        const line = value.slice(from, lineEndOf(s));
        const removed = line.length - outdent(line).length;
        if (removed) replace(from, from + removed, "", Math.max(from, s - removed));
        return;
      }
      return editLines(outdent);
    }
    if (s !== e) return editLines((line) => (line.trim() ? INDENT + line : line));
    const { col } = caretAt(s);
    insert(" ".repeat(4 - (col % 4)));
  };

  const enterKey = () => {
    const { selectionStart: s, selectionEnd: e, value } = area;
    const line = value.slice(lineStartOf(s), s);
    let indent = /^[ \t]*/.exec(line)[0];
    const opener = /[{(\[]\s*$/.exec(line);
    if (opener) indent += INDENT;
    // Between a pair, as in `{|}`: open the block, closer on its own line.
    if (opener && value[e] === PAIRS[opener[0].trim()]) {
      const outer = indent.slice(0, -INDENT.length);
      replace(s, e, `\n${indent}\n${outer}`, s + 1 + indent.length);
      return;
    }
    insert("\n" + indent);
  };

  const backspaceKey = () => {
    const { selectionStart: s, selectionEnd: e, value } = area;
    if (s !== e || s === 0) return false;
    // An empty pair: delete both halves.
    if (PAIRS[value[s - 1]] && PAIRS[value[s - 1]] === value[s]) return replace(s - 1, s + 1, "", s - 1), true;
    // In the indentation: back to the previous tab stop.
    const from = lineStartOf(s);
    const before = value.slice(from, s);
    if (before.length && /^ +$/.test(before)) {
      const n = before.length % 4 || 4;
      return replace(s - n, s, "", s - n), true;
    }
    return false;
  };

  const typed = (ch) => {
    const { selectionStart: s, selectionEnd: e, value } = area;
    const next = value[e] ?? "";
    const prev = value[s - 1] ?? "";
    // An odd number of quotes before the caret on this line means it is inside a string.
    const inString = (value.slice(lineStartOf(s), s).match(/(?<!\\)"/g)?.length ?? 0) % 2 === 1;
    // Typing a closer that is already there steps over it; a quote only when it ends the string.
    if (s === e && next === ch && (CLOSERS.has(ch) || (ch === '"' && inString))) {
      area.setSelectionRange(s + 1, s + 1);
      markLine();
      reveal();
      return true;
    }
    if (CLOSERS.has(ch)) {
      // A closer typed as the first thing on a line moves out one level.
      const from = lineStartOf(s);
      const before = value.slice(from, s);
      if (s === e && before.length >= 4 && /^\s+$/.test(before)) {
        const kept = before.slice(0, -(before.length % 4 || 4));
        return replace(from, s, kept + ch, from + kept.length + 1), true;
      }
      return false;
    }
    if (!PAIRS[ch]) return false;
    // A selection gets wrapped: (selection).
    if (s !== e) return replace(s, e, ch + value.slice(s, e) + PAIRS[ch], s + 1, e + 1), true;
    if (!AUTO.has(ch)) return false;
    if (ch === '"' && (inString || /[\w"\\]/.test(prev) || /\w/.test(next))) return false;
    if (next && !/[\s)\]},;]/.test(next)) return false;
    return replace(s, e, ch + PAIRS[ch], s + 1), true;
  };

  // Ctrl+/: comment the selected lines out, or back in.
  const toggleComment = () => {
    const { lines } = selectedLines();
    const filled = lines.filter((l) => l.trim());
    if (!filled.length) return;
    const commented = filled.every((l) => /^\s*\/\//.test(l));
    if (commented) return editLines((l) => l.replace(/^(\s*)\/\/ ?/, "$1"));
    const depth = Math.min(...filled.map((l) => /^\s*/.exec(l)[0].length));
    editLines((l) => (l.trim() ? l.slice(0, depth) + "// " + l.slice(depth) : l));
  };

  let escaped = false;
  area.addEventListener("keydown", (e) => {
    if (e.isComposing) return;
    const mod = e.ctrlKey || e.metaKey;
    if (mod && e.key === "Enter") return e.preventDefault(), start();
    if (mod && e.key.toLowerCase() === "s") return e.preventDefault(), format();
    if (mod && e.key === "/") return e.preventDefault(), toggleComment();
    // Escape, then Tab, leaves the editor, so keyboard users aren't trapped in it.
    if (e.key === "Escape") return (escaped = true);
    if (e.key === "Tab" && !escaped && !mod && !e.altKey) return e.preventDefault(), indentKey(e.shiftKey);
    escaped = false;
    if (mod || e.altKey) return;
    if (e.key === "Enter" && !e.shiftKey) return e.preventDefault(), enterKey();
    if (e.key === "Backspace" && backspaceKey()) return e.preventDefault();
    if (e.key.length === 1 && typed(e.key)) return e.preventDefault();
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
    if (result.source === area.value) return setStatus("Already formatted");
    // Keep the caret on the same line, as near the same place as the new text allows.
    const { line } = caretAt(area.selectionStart);
    const column = area.selectionStart - lineStartOf(area.selectionStart);
    area.focus();
    area.select();
    insert(result.source);
    const lines = result.source.split("\n");
    const index = Math.min(line, lines.length) - 1;
    const at = lines.slice(0, index).reduce((n, l) => n + l.length + 1, 0) + Math.min(column, lines[index].length);
    area.setSelectionRange(at, at);
    markLine();
    reveal();
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
