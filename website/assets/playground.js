// The playground: a CodeMirror editor over a workspace of files, the real
// interpreter (WebAssembly, see runner.js) to run them, and the Oxigen
// language server (lsp-go, also WebAssembly) for completion and hovers.
//
// Highlighting, error checking and formatting come from the interpreter's
// own lexer, parser and formatter, so what the editor shows is what the
// language accepts. Keys are standard or vim (`:w` formats, `:run`, `:test`).

import { Compartment, EditorState, Prec, StateEffect } from "@codemirror/state";
import {
  Decoration, EditorView, ViewPlugin, crosshairCursor, drawSelection, dropCursor, highlightActiveLine,
  highlightActiveLineGutter, highlightSpecialChars, hoverTooltip, keymap, lineNumbers, rectangularSelection,
} from "@codemirror/view";
import { defaultKeymap, history as undoHistory, historyKeymap, indentWithTab } from "@codemirror/commands";
import { bracketMatching, indentOnInput, indentService, indentUnit } from "@codemirror/language";
import { autocompletion, closeBrackets, closeBracketsKeymap, completeAnyWord, completionKeymap } from "@codemirror/autocomplete";
import { forceLinting, lintGutter, lintKeymap, linter } from "@codemirror/lint";
import { highlightSelectionMatches, search, searchKeymap } from "@codemirror/search";
import { Vim, getCM, vim } from "@replit/codemirror-vim";

const meta = (name) => document.querySelector(`meta[name="${name}"]`)?.content;
const STORE = "oxigen-playground-workspace";
const OLD_DRAFT = "oxigen-playground-draft";
const BLANK = "main {\n    \n}\n";
const TOKEN_CLASSES = ["tk-kw", "tk-const", "tk-str", "tk-num", "tk-com", "tk-fn", "tk-type", "tk-dir"];

const store = {
  get: (k) => { try { return localStorage.getItem(k); } catch { return null; } },
  set: (k, v) => { try { localStorage.setItem(k, v); } catch {} },
};
const escapeHtml = (s) => s.replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" })[c]);
const el = (id) => document.getElementById(id);

const runner = await import(meta("oxigen-runner"));
const examples = JSON.parse(el("pg-examples").textContent);
const host = el("pg-cm");
const output = el("pg-out");
const status = el("pg-status");
const runBtn = el("pg-run");
const testBtn = el("pg-test");
const picker = el("pg-example");
const tabs = el("pg-tabs");
const position = el("pg-position");
const modeLabel = el("pg-mode");
const lspLabel = el("pg-lsp");
const stdinForm = el("pg-stdin");
const stdinLine = el("pg-stdin-line");
const presetInput = el("pg-input");
const argsInput = el("pg-args");
const keymapPicker = el("pg-keymap");

let tools = null;
let lsp = null;
let job = null;

const setStatus = (text, state = "ready") => {
  status.textContent = text;
  status.dataset.state = state;
};

// ── The workspace: files, which one is open, input, arguments, keys ──

let ws = null;
ws = load();
function load() {
  try {
    const saved = JSON.parse(store.get(STORE));
    if (saved?.files?.length) return saved;
  } catch {}
  // The editor before this one kept one file.
  const draft = store.get(OLD_DRAFT);
  return fresh(draft ?? BLANK);
}
function fresh(source, extra = {}) {
  return { files: [{ name: "main.oxi", text: source }], active: "main.oxi", stdin: ws?.stdin ?? "", args: ws?.args ?? "", keymap: ws?.keymap ?? "standard", pristine: null, ...extra };
}
let saveTimer = 0;
const save = () => {
  clearTimeout(saveTimer);
  saveTimer = setTimeout(() => store.set(STORE, JSON.stringify(ws)), 250);
};
const file = (name) => ws.files.find((f) => f.name === name);
const uri = (name) => `file:///work/${encodeURIComponent(name)}`;
// Unsaved changes worth asking about before replacing everything.
const dirty = () => {
  if (ws.pristine == null) return ws.files.some((f) => f.text.trim() && f.text !== BLANK);
  return ws.files.length !== 1 || ws.files[0].text !== ws.pristine;
};

// ── Editor extensions ──

// Highlighting from the interpreter's lexer, recomputed on each change.
const refresh = StateEffect.define();
const marks = Object.fromEntries(TOKEN_CLASSES.map((c) => [c, Decoration.mark({ class: c })]));
const highlighter = ViewPlugin.fromClass(
  class {
    constructor(view) {
      this.decorations = this.build(view);
    }
    update(update) {
      if (update.docChanged || update.transactions.some((t) => t.effects.some((e) => e.is(refresh)))) this.decorations = this.build(update.view);
    }
    build(view) {
      if (!tools) return Decoration.none;
      const flat = tools.tokens(view.state.doc.toString());
      const ranges = [];
      for (let i = 0; i < flat.length; i += 3) if (flat[i + 1] > flat[i]) ranges.push(marks[TOKEN_CLASSES[flat[i + 2]]].range(flat[i], flat[i + 1]));
      return Decoration.set(ranges, true);
    }
  },
  { decorations: (v) => v.decorations },
);

// Indentation: one level in after an opening bracket, one out for a closer.
const oxigenIndent = indentService.of((cx, pos) => {
  // The nearest line with text before the one that starts at `pos`. While
  // Enter is pressed, `lineAt` sees the line as already broken at `pos`.
  let prev = cx.lineAt(pos, -1);
  if (prev.from === pos) {
    if (pos === 0) return 0;
    prev = cx.lineAt(pos - 1, -1);
  }
  while (!prev.text.trim() && prev.from > 0) prev = cx.lineAt(prev.from - 1, -1);
  let indent = /^\s*/.exec(prev.text)[0].replace(/\t/g, "    ").length;
  if (/[{(\[]\s*(\/\/.*)?$/.test(prev.text)) indent += 4;
  const rest = cx.textAfterPos(pos);
  if (/^\s*[}\])]/.test(rest)) indent -= 4;
  return Math.max(0, indent);
});
const language = EditorState.languageData.of(() => [{
  commentTokens: { line: "//" },
  closeBrackets: { brackets: ["(", "[", "{", '"'] },
  indentOnInput: /^\s*[}\])]$/,
}]);

// Errors as you type, from the parser and compiler; a run's runtime error until the next edit.
let runtimeError = null; // { file, doc, line, message }
const diagnostics = linter((view) => {
  if (!tools) return [];
  const doc = view.state.doc;
  const pos = (line, column) => {
    const l = doc.line(Math.min(Math.max(1, line), doc.lines));
    return Math.min(l.to, l.from + Math.max(0, column - 1));
  };
  const found = tools.check(doc.toString()).map((d) => {
    const start = d.primary?.start ?? { line: d.line, column: d.column };
    const end = d.primary?.end ?? start;
    const from = pos(start.line, start.column);
    let to = pos(end.line, end.column);
    if (to <= from) to = Math.min(doc.line(Math.min(start.line, doc.lines)).to, from + 1);
    const extra = [...(d.helps ?? []), ...(d.notes ?? [])].map((h) => `\n${h}`).join("");
    return { from, to: Math.max(from, to), severity: d.severity === "error" ? "error" : d.severity === "warning" ? "warning" : "info", message: d.message + extra, source: d.code };
  });
  if (runtimeError && runtimeError.file === ws.active && runtimeError.doc === doc) {
    const l = doc.line(Math.min(runtimeError.line, doc.lines));
    found.push({ from: l.from + (/^\s*/.exec(l.text)[0].length), to: l.to, severity: "error", message: runtimeError.message, source: "run" });
  }
  return found;
}, { delay: 300 });

// Completion from the language server, else from the words in the file.
const KINDS = { 2: "method", 3: "function", 4: "function", 5: "property", 6: "variable", 7: "class", 9: "namespace", 10: "property", 13: "enum", 14: "keyword", 20: "enum", 21: "constant", 22: "class", 25: "type" };
async function complete(cx) {
  const word = cx.matchBefore(/\w*/);
  if (!lsp) return completeAnyWord(cx);
  if (!cx.explicit && word.from === word.to && cx.state.sliceDoc(word.from - 1, word.from) !== ".") return null;
  syncNow();
  const line = cx.state.doc.lineAt(cx.pos);
  let result;
  try {
    result = await lsp.request("textDocument/completion", { textDocument: { uri: uri(ws.active) }, position: { line: line.number - 1, character: cx.pos - line.from } });
  } catch {
    return completeAnyWord(cx);
  }
  const items = Array.isArray(result) ? result : result?.items ?? [];
  if (!items.length) return null;
  return {
    from: word.from,
    options: items.map((item) => ({
      label: item.label,
      detail: item.detail,
      info: item.documentation ? () => markdown(typeof item.documentation === "string" ? item.documentation : item.documentation.value) : undefined,
      type: KINDS[item.kind],
      apply: item.insertText || item.textEdit?.newText || item.label,
    })),
    validFor: /^\w*$/,
  };
}

// Hovers from the language server.
const hovers = hoverTooltip(async (view, pos) => {
  if (!lsp) return null;
  syncNow();
  const line = view.state.doc.lineAt(pos);
  let result;
  try {
    result = await lsp.request("textDocument/hover", { textDocument: { uri: uri(ws.active) }, position: { line: line.number - 1, character: pos - line.from } });
  } catch {
    return null;
  }
  const contents = result?.contents;
  const text = typeof contents === "string" ? contents : Array.isArray(contents) ? contents.map((c) => c.value ?? c).join("\n\n") : contents?.value;
  if (!text) return null;
  return { pos, above: true, create: () => ({ dom: markdown(text, "pg-hover") }) };
}, { hoverTime: 350 });

/** The little Markdown the language server writes: fences, `code`, **bold**, lists. */
function markdown(text, className = "pg-doc") {
  const dom = document.createElement("div");
  dom.className = className;
  const inline = (s) => escapeHtml(s).replace(/`([^`]+)`/g, "<code>$1</code>").replace(/\*\*([^*]+)\*\*/g, "<strong>$1</strong>");
  let html = "";
  const parts = text.split(/```\w*\n?/);
  parts.forEach((part, i) => {
    if (i % 2) html += `<pre>${tools ? tools.highlight(part.replace(/\n$/, "")) : escapeHtml(part)}</pre>`;
    else for (const para of part.split(/\n{2,}/)) if (para.trim()) html += `<p>${para.trim().split("\n").map(inline).join("<br>")}</p>`;
  });
  dom.innerHTML = html;
  return dom;
}

const theme = EditorView.theme({
  "&": { height: "100%", backgroundColor: "var(--code-bg)", color: "var(--code-ink)", fontSize: "var(--editor-size)" },
  ".cm-scroller": { fontFamily: "var(--mono)", lineHeight: "var(--editor-line)" },
  ".cm-content": { padding: "16px 0", caretColor: "var(--code-ink)" },
  ".cm-gutters": { backgroundColor: "var(--code-bg)", color: "var(--code-muted)", border: "0", paddingLeft: "8px" },
  ".cm-activeLineGutter": { backgroundColor: "transparent", color: "var(--code-ink)" },
  ".cm-activeLine": { backgroundColor: "rgba(255, 255, 255, 0.045)" },
  ".cm-cursor, .cm-dropCursor": { borderLeftColor: "var(--code-ink)" },
  "&.cm-focused .cm-selectionBackground, .cm-selectionBackground, ::selection": { backgroundColor: "rgba(79, 163, 184, 0.32) !important" },
  ".cm-selectionMatch": { backgroundColor: "rgba(79, 163, 184, 0.16)" },
  "&.cm-focused .cm-matchingBracket": { backgroundColor: "rgba(79, 163, 184, 0.28)", outline: "0" },
  ".cm-tooltip": { backgroundColor: "var(--code-bar)", color: "var(--code-ink)", border: "1px solid var(--code-rule)", borderRadius: "var(--radius)" },
  ".cm-tooltip-autocomplete > ul > li[aria-selected]": { backgroundColor: "var(--signal)", color: "var(--on-signal)" },
  ".cm-panels": { backgroundColor: "var(--code-bar)", color: "var(--code-ink)", borderTop: "1px solid var(--code-rule)" },
  ".cm-panels input, .cm-panels button": { color: "var(--code-ink)" },
  ".cm-fat-cursor": { background: "var(--accent) !important", color: "var(--code-bg) !important" },
  "&:not(.cm-focused) .cm-fat-cursor": { background: "none !important", outline: "1px solid var(--accent)" },
  ".cm-vim-panel input": { color: "var(--code-ink)", fontFamily: "var(--mono)" },
}, { dark: true });

const keys = new Compartment();
const keysFor = (name) => (name === "vim" ? vim({ status: true }) : []);

const extensions = () => [
  keys.of(keysFor(ws.keymap)),
  lineNumbers(),
  highlightActiveLineGutter(),
  highlightSpecialChars(),
  undoHistory(),
  drawSelection(),
  dropCursor(),
  EditorState.allowMultipleSelections.of(true),
  EditorState.tabSize.of(4),
  indentUnit.of("    "),
  indentOnInput(),
  bracketMatching(),
  closeBrackets(),
  autocompletion({ override: [complete], activateOnTyping: true }),
  rectangularSelection(),
  crosshairCursor(),
  highlightActiveLine(),
  highlightSelectionMatches(),
  search({ top: true }),
  language,
  oxigenIndent,
  highlighter,
  diagnostics,
  lintGutter(),
  hovers,
  theme,
  Prec.high(keymap.of([
    { key: "Mod-Enter", run: () => (start(), true) },
    { key: "Mod-s", run: () => (format(), true) },
    { key: "Mod-Shift-Enter", run: () => (start({ test: true }), true) },
  ])),
  keymap.of([...closeBracketsKeymap, ...defaultKeymap, ...searchKeymap, ...historyKeymap, ...completionKeymap, ...lintKeymap, indentWithTab]),
  EditorView.updateListener.of((u) => {
    if (u.docChanged) {
      file(ws.active).text = u.state.doc.toString();
      save();
      changed(ws.active);
    }
    if (u.selectionSet || u.docChanged) {
      const head = u.state.selection.main.head;
      const line = u.state.doc.lineAt(head);
      position.textContent = `Ln ${line.number}, Col ${head - line.from + 1}`;
    }
  }),
  EditorView.contentAttributes.of({ "aria-label": "OxigenLang source code", "aria-describedby": "pg-shortcuts" }),
];

const states = new Map();
const stateFor = (name) => {
  if (!states.has(name)) states.set(name, EditorState.create({ doc: file(name).text, extensions: extensions() }));
  return states.get(name);
};

host.textContent = "";
const view = new EditorView({ state: stateFor(ws.active), parent: host });

// Vim's modes, shown in the status bar; its ex commands for this page.
const watchVim = () => {
  modeLabel.textContent = "";
  if (ws.keymap !== "vim") return;
  modeLabel.textContent = "NORMAL";
  getCM(view)?.on("vim-mode-change", (e) => (modeLabel.textContent = (e.subMode ? `${e.mode} ${e.subMode}` : e.mode).toUpperCase()));
};
Vim.defineEx("write", "w", () => format());
Vim.defineEx("run", "run", () => start());
Vim.defineEx("test", "test", () => start({ test: true }));
Vim.defineEx("new", "new", () => addFile());

const setKeymap = (name) => {
  ws.keymap = name;
  keymapPicker.value = name;
  save();
  view.dispatch({ effects: keys.reconfigure(keysFor(name)) });
  watchVim();
  view.focus();
};
keymapPicker.value = ws.keymap;
keymapPicker.addEventListener("change", () => setKeymap(keymapPicker.value));
watchVim();

// ── Files and tabs ──

function renderTabs() {
  tabs.textContent = "";
  for (const f of ws.files) {
    const tab = document.createElement("button");
    tab.type = "button";
    tab.className = "pg-tab";
    tab.setAttribute("role", "tab");
    tab.setAttribute("aria-selected", f.name === ws.active);
    tab.dataset.name = f.name;
    tab.title = f.name === "main.oxi" ? "main.oxi runs when you press Run" : "Double-click to rename";
    tab.append(f.name);
    if (f.name !== "main.oxi") {
      const close = document.createElement("span");
      close.className = "pg-tab-close";
      close.setAttribute("aria-label", `Delete ${f.name}`);
      close.textContent = "×";
      tab.append(close);
    }
    tabs.append(tab);
  }
  const add = document.createElement("button");
  add.type = "button";
  add.className = "pg-tab-add";
  add.title = "New file";
  add.setAttribute("aria-label", "New file");
  add.textContent = "+";
  tabs.append(add);
}

function openFile(name) {
  if (!file(name)) return;
  if (name === ws.active) return view.focus();
  // Keep the open file's undo history and selection, unless it was just deleted.
  if (file(ws.active)) states.set(ws.active, view.state);
  ws.active = name;
  view.setState(stateFor(name));
  view.dispatch({ effects: [keys.reconfigure(keysFor(ws.keymap)), refresh.of(null)] });
  watchVim();
  renderTabs();
  save();
  view.focus();
}

// A name typed into the tab bar, for a new file or a rename.
function askName(initial, onDone) {
  const input = document.createElement("input");
  input.className = "pg-tab-name";
  input.value = initial;
  input.setAttribute("aria-label", "File name");
  input.spellcheck = false;
  tabs.insertBefore(input, tabs.querySelector(".pg-tab-add"));
  input.focus();
  input.setSelectionRange(0, initial.lastIndexOf(".") > 0 ? initial.lastIndexOf(".") : initial.length);
  let finished = false;
  const finish = (ok) => {
    if (finished) return;
    finished = true;
    const name = input.value.trim();
    input.remove();
    if (!ok || !name || name === initial) return onDone(null);
    if (!/^[\w.-]+(\/[\w.-]+)*$/.test(name) || name.split("/").some((p) => p === "." || p === "..")) {
      setStatus("File names are letters, digits, . _ - and / for folders", "error");
      return onDone(null);
    }
    if (file(name)) {
      setStatus(`There is already a file called ${name}`, "error");
      return onDone(null);
    }
    onDone(name);
  };
  input.addEventListener("keydown", (e) => {
    if (e.key === "Enter") e.preventDefault(), finish(true);
    if (e.key === "Escape") e.preventDefault(), finish(false), view.focus();
  });
  input.addEventListener("blur", () => finish(true));
}

function addFile() {
  let n = 1;
  while (file(n === 1 ? "module.oxi" : `module${n}.oxi`)) n++;
  askName(n === 1 ? "module.oxi" : `module${n}.oxi`, (name) => {
    if (!name) return;
    ws.files.push({ name, text: "" });
    lspOpen(name);
    openFile(name);
  });
}

function renameFile(old) {
  askName(old, (name) => {
    if (!name) return;
    const f = file(old);
    lspClose(old);
    f.name = name;
    if (states.has(old)) states.set(name, states.get(old)), states.delete(old);
    if (ws.active === old) ws.active = name;
    lspOpen(name);
    renderTabs();
    save();
  });
}

function deleteFile(name) {
  const f = file(name);
  if (!f || name === "main.oxi") return;
  if (f.text.trim() && !confirm(`Delete ${name}?`)) return;
  ws.files = ws.files.filter((x) => x !== f);
  states.delete(name);
  lspClose(name);
  if (ws.active === name) openFile("main.oxi");
  else renderTabs(), save();
}

tabs.addEventListener("click", (e) => {
  if (e.target.closest(".pg-tab-add")) return addFile();
  const tab = e.target.closest(".pg-tab");
  if (!tab) return;
  if (e.target.closest(".pg-tab-close")) return deleteFile(tab.dataset.name);
  openFile(tab.dataset.name);
});
tabs.addEventListener("dblclick", (e) => {
  const tab = e.target.closest(".pg-tab");
  if (tab && tab.dataset.name !== "main.oxi") renameFile(tab.dataset.name);
});

/** Replace the whole workspace, for New, an example or a share link. */
function replaceWorkspace(next) {
  for (const f of ws.files) lspClose(f.name);
  ws = next;
  states.clear();
  view.setState(stateFor(ws.active));
  view.dispatch({ effects: [keys.reconfigure(keysFor(ws.keymap)), refresh.of(null)] });
  watchVim();
  for (const f of ws.files) lspOpen(f.name);
  presetInput.value = ws.stdin;
  argsInput.value = ws.args;
  renderTabs();
  save();
}

/** Bring files a run created or changed into the workspace. */
function takeFiles(files) {
  const decoder = new TextDecoder("utf-8", { fatal: true });
  const notes = [];
  for (const [name, bytes] of Object.entries(files)) {
    let text;
    try {
      text = decoder.decode(bytes);
    } catch {
      notes.push(`${name} (${bytes.length} bytes, not text)`);
      continue;
    }
    if (bytes.length > 512 * 1024) {
      notes.push(`${name} (${Math.round(bytes.length / 1024)} KB, too large to open)`);
      continue;
    }
    const f = file(name);
    if (!f) {
      ws.files.push({ name, text });
      lspOpen(name);
      notes.push(`${name} (new)`);
    } else if (f.text !== text) {
      f.text = text;
      if (name === ws.active) view.dispatch({ changes: { from: 0, to: view.state.doc.length, insert: text } });
      else states.delete(name);
      changed(name);
      notes.push(`${name} (changed)`);
    }
  }
  if (notes.length) {
    renderTabs();
    save();
  }
  return notes;
}

// ── Language server ──

const synced = new Map(); // name -> version sent
let syncTimer = 0;
const pendingSync = new Set();
function changed(name) {
  if (!lsp) return;
  pendingSync.add(name);
  clearTimeout(syncTimer);
  syncTimer = setTimeout(syncNow, 200);
}
function syncNow() {
  if (!lsp) return;
  clearTimeout(syncTimer);
  for (const name of pendingSync) {
    const f = file(name);
    if (!f || !synced.has(name)) continue;
    const version = synced.get(name) + 1;
    synced.set(name, version);
    lsp.notify("textDocument/didChange", { textDocument: { uri: uri(name), version }, contentChanges: [{ text: f.text }] });
  }
  pendingSync.clear();
}
function lspOpen(name) {
  if (!lsp || !name.endsWith(".oxi") || synced.has(name)) return;
  synced.set(name, 1);
  lsp.notify("textDocument/didOpen", { textDocument: { uri: uri(name), languageId: "oxigen", version: 1, text: file(name).text } });
}
function lspClose(name) {
  if (!lsp || !synced.has(name)) return;
  synced.delete(name);
  lsp.notify("textDocument/didClose", { textDocument: { uri: uri(name) } });
}

async function startLsp() {
  if (!runner.interactive() || !meta("oxigen-lsp")) {
    lspLabel.textContent = "LSP off";
    lspLabel.title = "The language server needs a cross-origin isolated page (SharedArrayBuffer). Completion uses the words in the file.";
    return;
  }
  lspLabel.textContent = "LSP starting";
  try {
    const { start } = await import(meta("oxigen-lsp-client"));
    const response = await fetch(meta("oxigen-lsp"));
    const module = await WebAssembly.compile(await response.arrayBuffer());
    const files = {};
    for (const [name, source] of Object.entries(tools.stdlib())) files[`${runner.HOME}/.oxigen/lib/stdlib/${name}.oxi`] = source;
    lsp = await start({ module, workerUrl: meta("oxigen-lsp-worker"), files, env: { HOME: runner.HOME, PWD: runner.WORK } });
    lsp.on("$/exit", () => {
      lsp = null;
      synced.clear();
      lspLabel.textContent = "LSP stopped";
    });
    for (const f of ws.files) lspOpen(f.name);
    lspLabel.textContent = "LSP ready";
    lspLabel.title = "oxigen-lsp, running in your browser: completion (Ctrl+Space) and hover";
  } catch (err) {
    lsp = null;
    lspLabel.textContent = "LSP unavailable";
    lspLabel.title = String(err);
  }
}

// ── Running ──

const write = (text, className) => {
  if (!className) return output.append(text);
  const span = document.createElement("span");
  span.className = className;
  span.textContent = text;
  output.append(span);
};
const scrollOutput = () => (output.scrollTop = output.scrollHeight);

async function start({ test = false } = {}) {
  if (job) return job.stop();
  syncNow();
  output.textContent = "";
  runtimeError = null;
  const main = file("main.oxi") ? "main.oxi" : ws.active;
  const files = Object.fromEntries(ws.files.map((f) => [f.name, f.text]));
  setStatus(test ? "Testing…" : "Running…", "running");
  runBtn.classList.add("stop");
  runBtn.lastChild.textContent = " Stop";
  testBtn.disabled = true;
  const args = argsInput.value.trim() ? argsInput.value.trim().split(/\s+/) : [];
  const testFiles = ws.files.filter((f) => f.name.endsWith(".oxi")).map((f) => f.name);
  let printed = false;
  job = runner.run(
    { files, main },
    {
      stderr: "stream",
      stdin: presetInput.value ? presetInput.value.replace(/\n?$/, "\n") : "",
      args,
      test,
      testFiles,
      onOutput(text, fd) {
        printed = true;
        write(text, fd === 2 ? "err" : null);
        scrollOutput();
      },
      onWaiting(waiting) {
        stdinForm.hidden = !waiting;
        if (waiting) {
          setStatus("Waiting for input", "running");
          stdinLine.focus();
        } else {
          setStatus(test ? "Testing…" : "Running…", "running");
        }
      },
    },
  );
  const result = await job.done;
  job = null;
  stdinForm.hidden = true;
  runBtn.classList.remove("stop");
  runBtn.lastChild.textContent = " Run";
  testBtn.disabled = false;
  const ms = result.ms != null ? (result.ms < 10 ? result.ms.toFixed(1) : Math.round(result.ms)) + " ms" : "";
  const notes = takeFiles(result.files);
  if (notes.length) write(`${output.textContent && !output.textContent.endsWith("\n") ? "\n" : ""}Files written: ${notes.join(", ")}\n`, "note");
  if (result.ok) {
    if (!printed) write("The program finished without printing anything.", "note");
    setStatus(`Finished in ${ms}`);
  } else {
    if (result.stopped || result.code === 124 || !printed || result.error?.startsWith("Internal")) {
      write(`${output.textContent && !output.textContent.endsWith("\n") ? "\n" : ""}${result.error || "Error"}`, result.stopped ? "note" : "err");
    }
    const text = output.textContent;
    for (const f of ws.files) {
      const line = runner.errorLine(text, f.name);
      if (line) {
        runtimeError = { file: f.name, doc: f.name === ws.active ? view.state.doc : null, line, message: (/error\[\w+\]: (.*)/.exec(text)?.[1] ?? "error").trim() };
        if (f.name === ws.active) forceLinting(view);
        break;
      }
    }
    setStatus(result.stopped ? "Stopped" : `Exited with code ${result.code}${ms ? ` after ${ms}` : ""}`, "error");
  }
  scrollOutput();
}
runBtn.addEventListener("click", () => start());
testBtn.addEventListener("click", () => start({ test: true }));

// Standard input typed while the program waits. Ctrl+D ends it, as in a terminal.
stdinForm.addEventListener("submit", (e) => {
  e.preventDefault();
  if (!job) return;
  const line = stdinLine.value;
  stdinLine.value = "";
  write(line + "\n", "echo");
  scrollOutput();
  job.input(line + "\n");
});
const endInput = () => {
  if (!job) return;
  write("^D\n", "note");
  job.endInput();
};
stdinLine.addEventListener("keydown", (e) => {
  if (e.key === "d" && e.ctrlKey) e.preventDefault(), endInput();
  if (e.key === "c" && e.ctrlKey && !stdinLine.selectionEnd) e.preventDefault(), job?.stop();
});
el("pg-eof").addEventListener("click", endInput);

presetInput.value = ws.stdin;
argsInput.value = ws.args;
presetInput.addEventListener("input", () => ((ws.stdin = presetInput.value), save()));
argsInput.addEventListener("input", () => ((ws.args = argsInput.value), save()));

// ── Format, share, new, examples ──

function format() {
  if (!tools) return;
  const source = view.state.doc.toString();
  const result = tools.format(source);
  if (!result.ok) return setStatus("Fix the syntax errors before formatting", "error");
  if (result.source === source) return setStatus("Already formatted");
  // Keep the caret on the same line, as near the same place as the new text allows.
  const head = view.state.selection.main.head;
  const line = view.state.doc.lineAt(head);
  const lines = result.source.split("\n");
  const index = Math.min(line.number, lines.length) - 1;
  const at = lines.slice(0, index).reduce((n, l) => n + l.length + 1, 0) + Math.min(head - line.from, lines[index].length);
  view.dispatch({ changes: { from: 0, to: source.length, insert: result.source }, selection: { anchor: at } });
  setStatus("Formatted");
}
el("pg-format").addEventListener("click", () => (format(), view.focus()));

el("pg-share").addEventListener("click", async () => {
  syncNow();
  const only = ws.files.length === 1 && ws.files[0].name === "main.oxi";
  const url = new URL(await runner.playgroundUrl(only ? ws.files[0].text : Object.fromEntries(ws.files.map((f) => [f.name, f.text]))), location.href).href;
  history.replaceState(null, "", url);
  try {
    await navigator.clipboard.writeText(url);
    setStatus("Link copied to the clipboard");
  } catch {
    setStatus("Link is in the address bar");
  }
});

el("pg-clear").addEventListener("click", () => (output.textContent = ""));

el("pg-new").addEventListener("click", () => {
  if (dirty() && !confirm("Start a new, empty program? This replaces the files you have open.")) return;
  replaceWorkspace(fresh(BLANK, { pristine: BLANK }));
  history.replaceState(null, "", location.pathname);
  picker.value = "";
  output.innerHTML = '<span class="placeholder">A new program. Write some code and press Run or Ctrl+Enter.</span>';
  view.dispatch({ selection: { anchor: BLANK.indexOf("\n    ") + 5 } });
  view.focus();
});

picker.addEventListener("change", () => {
  const example = examples.find((x) => x.slug === picker.value);
  if (!example) return;
  if (dirty() && !confirm(`Open the "${example.title}" example? This replaces the files you have open.`)) {
    picker.value = "";
    return;
  }
  replaceWorkspace(fresh(example.source, { pristine: example.source, stdin: example.stdin ?? "", args: example.args ?? "" }));
  history.replaceState(null, "", location.pathname);
  output.innerHTML = `<span class="placeholder">${escapeHtml(example.summary)}\nPress Run or Ctrl+Enter.</span>`;
  picker.value = "";
  view.focus();
});

// ── Starting up: a share link, else the saved workspace ──

const params = new URLSearchParams(location.hash.slice(1));
try {
  if (params.get("code")) replaceWorkspace(fresh(await runner.decodeSource(params.get("code"))));
  else if (params.get("files")) {
    const shared = JSON.parse(await runner.decodeSource(params.get("files")));
    const names = Object.keys(shared).sort((a, b) => (a === "main.oxi" ? -1 : b === "main.oxi" ? 1 : 0));
    if (names.length) replaceWorkspace({ ...fresh(""), files: names.map((name) => ({ name, text: String(shared[name]) })), active: names[0] });
  }
} catch {
  setStatus("That share link could not be read", "error");
}
renderTabs();

setStatus("Loading interpreter…", "running");
try {
  tools = await runner.tools();
  view.dispatch({ effects: refresh.of(null) });
  forceLinting(view);
  setStatus(runner.interactive() ? "Ready · Ctrl+Enter runs" : "Ready · Ctrl+Enter runs (input given up front)");
  el("pg-mode-note").textContent = runner.interactive()
    ? "Type into the output pane when the program asks for input; diverge runs tasks on threads."
    : "This browser did not isolate the page, so input is read from the box below and diverge is unavailable.";
} catch {
  setStatus("Could not load the interpreter", "error");
}
view.focus();
startLsp();
// Let the browser optimise the interpreter before the first real run.
(globalThis.requestIdleCallback ?? setTimeout)(() => runner.warmUp?.(), { timeout: 2000 });
