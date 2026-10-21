// Every page: theme, menus, search, tabs, the docs' table of contents, copy
// and Run buttons on code, and the benchmark chart. Everything here is an
// enhancement: each page reads fine without it.

const $ = (sel, root = document) => root.querySelector(sel);
const $$ = (sel, root = document) => [...root.querySelectorAll(sel)];
const store = {
  get: (k) => { try { return localStorage.getItem(k); } catch { return null; } },
  set: (k, v) => { try { localStorage.setItem(k, v); } catch {} },
};
const runnerUrl = document.querySelector('meta[name="oxigen-runner"]')?.content;
const runner = () => import(runnerUrl);

// ── Theme ──
const isDark = () => {
  const set = document.documentElement.dataset.theme;
  return set ? set === "dark" : matchMedia("(prefers-color-scheme: dark)").matches;
};
$$("[data-theme-toggle]").forEach((btn) =>
  btn.addEventListener("click", () => {
    const next = isDark() ? "light" : "dark";
    document.documentElement.dataset.theme = next;
    store.set("theme", next);
  }),
);

// ── Menus ──
const menuBtn = $("[data-menu-toggle]");
menuBtn?.addEventListener("click", () => {
  const open = document.body.classList.toggle("menu-open");
  menuBtn.setAttribute("aria-expanded", open);
});
const docsBtn = $("[data-docs-nav-toggle]");
docsBtn?.addEventListener("click", () => {
  const open = document.body.classList.toggle("docs-nav-open");
  docsBtn.setAttribute("aria-expanded", open);
});
$(".docs-sidebar [aria-current]")?.scrollIntoView({ block: "center" });

// ── Copy ──
async function copy(text, button) {
  try {
    await navigator.clipboard.writeText(text);
    const label = button.textContent;
    button.textContent = "Copied";
    setTimeout(() => (button.textContent = label), 1400);
  } catch {}
}
document.addEventListener("click", (e) => {
  const btn = e.target.closest(".btn-copy, [data-copy]");
  if (!btn) return;
  const text = btn.dataset.copy ?? btn.closest(".code")?.querySelector("pre code")?.textContent ?? "";
  copy(text, btn);
});

// ── Run buttons on code cards ──
const escapeHtml = (s) => s.replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" })[c]);
const running = new WeakMap();

document.addEventListener("click", async (e) => {
  const runBtn = e.target.closest(".btn-run");
  const openBtn = e.target.closest(".btn-open");
  if (!runBtn && !openBtn) return;
  const card = (runBtn || openBtn).closest(".code");
  const source = card.querySelector("pre code").textContent;
  if (openBtn) {
    const { playgroundUrl } = await runner();
    location.href = await playgroundUrl(source);
    return;
  }
  const active = running.get(card);
  if (active) return active.stop();

  let panel = card.querySelector(".code-output");
  if (!panel) {
    panel = document.createElement("div");
    panel.className = "code-output";
    panel.innerHTML = '<div class="code-output-bar"><span>Output</span><span class="time"></span></div><pre aria-live="polite"></pre>';
    card.append(panel);
  }
  const out = panel.querySelector("pre");
  const time = panel.querySelector(".time");
  out.innerHTML = '<span class="note">Loading the OxigenLang interpreter…</span>';
  time.textContent = "";
  runBtn.setAttribute("aria-busy", "true");
  const label = runBtn.lastChild;
  label.textContent = "Stop";

  const { run } = await runner();
  let first = true;
  const job = run(source, {
    onOutput(text) {
      if (first) (out.textContent = ""), (first = false);
      out.append(text);
    },
  });
  running.set(card, job);
  const result = await job.done;
  running.delete(card);
  if (first) out.textContent = "";
  if (!result.ok) out.insertAdjacentHTML("beforeend", `<span class="err">${escapeHtml(result.error || "Error")}</span>`);
  else if (!out.textContent) out.innerHTML = '<span class="note">(no output)</span>';
  if (result.ms != null) time.textContent = `${result.ms < 10 ? result.ms.toFixed(1) : Math.round(result.ms)} ms · WebAssembly`;
  runBtn.removeAttribute("aria-busy");
  label.textContent = "Run";
});

// ── Tabs ──
$$("[data-tabs]").forEach((root) => {
  const tabs = $$('[role="tab"]', root);
  const select = (tab) => {
    tabs.forEach((t) => {
      const on = t === tab;
      t.setAttribute("aria-selected", on);
      t.tabIndex = on ? 0 : -1;
      document.getElementById(t.getAttribute("aria-controls")).hidden = !on;
    });
  };
  tabs.forEach((tab, i) => {
    tab.addEventListener("click", () => select(tab));
    tab.addEventListener("keydown", (e) => {
      const step = { ArrowRight: 1, ArrowDown: 1, ArrowLeft: -1, ArrowUp: -1 }[e.key];
      if (!step) return;
      e.preventDefault();
      const next = tabs[(i + step + tabs.length) % tabs.length];
      select(next);
      next.focus();
    });
  });
});

// ── Table of contents: mark the section being read ──
const tocLinks = $$(".docs-toc a");
if (tocLinks.length && "IntersectionObserver" in window) {
  const byId = new Map(tocLinks.map((a) => [decodeURIComponent(a.hash.slice(1)), a]));
  const headings = [...byId.keys()].map((id) => document.getElementById(id)).filter(Boolean);
  const visible = new Set();
  const observer = new IntersectionObserver(
    (entries) => {
      entries.forEach((e) => (e.isIntersecting ? visible.add(e.target) : visible.delete(e.target)));
      const current = headings.find((h) => visible.has(h)) ?? headings.filter((h) => h.getBoundingClientRect().top < 120).pop();
      tocLinks.forEach((a) => a.classList.toggle("active", byId.get(current?.id) === a));
    },
    { rootMargin: "-64px 0px -70% 0px" },
  );
  headings.forEach((h) => observer.observe(h));
}

// ── Benchmark chart ──
$$(".bench-chart").forEach((chart) => {
  $$(".segmented button", chart).forEach((btn) =>
    btn.addEventListener("click", () => {
      chart.dataset.mode = btn.dataset.mode;
      $$(".segmented button", chart).forEach((b) => b.setAttribute("aria-pressed", b === btn));
      $$(".bar-value", chart).forEach((v) => (v.textContent = v.dataset[btn.dataset.mode]));
    }),
  );
  const tip = $(".chart-tip", chart);
  const show = (row) => {
    const [title, ...lines] = row.dataset.tip.split("|");
    tip.innerHTML = `<strong>${escapeHtml(title)}</strong>` + lines.map((line) => {
      const [label, value] = line.split("=");
      return `<div><span>${escapeHtml(label)}</span><b>${escapeHtml(value)}</b></div>`;
    }).join("");
    tip.hidden = false;
    const box = chart.getBoundingClientRect();
    const r = row.querySelector(".bar").getBoundingClientRect();
    const left = Math.min(r.right - box.left + 12, box.width - tip.offsetWidth - 12);
    tip.style.left = `${Math.max(12, left)}px`;
    tip.style.top = `${r.bottom - box.top + 8}px`;
  };
  $$(".bar-row", chart).forEach((row) => {
    row.addEventListener("mouseenter", () => show(row));
    row.addEventListener("focus", () => show(row));
    row.addEventListener("mouseleave", () => (tip.hidden = true));
    row.addEventListener("blur", () => (tip.hidden = true));
  });
});

// ── Search ──
const dialog = $("[data-search]");
const input = $("[data-search-input]");
const results = $("[data-search-results]");
let index;
let selected = 0;

async function loadIndex() {
  index ??= fetch("/search.json").then((r) => r.json()).then((entries) =>
    entries.map((e) => ({ ...e, lt: e.t.toLowerCase(), lp: e.p.toLowerCase(), lx: e.x.toLowerCase() })),
  );
  return index;
}

function openSearch() {
  dialog.hidden = false;
  document.body.style.overflow = "hidden";
  input.value = "";
  results.innerHTML = "";
  input.focus();
  loadIndex();
}
function closeSearch() {
  dialog.hidden = true;
  document.body.style.overflow = "";
}

function mark(text, terms) {
  let html = escapeHtml(text);
  for (const t of terms) html = html.replace(new RegExp(`(${t.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")})`, "ig"), "<mark>$1</mark>");
  return html;
}

async function search() {
  const terms = input.value.toLowerCase().split(/\s+/).filter(Boolean);
  if (!terms.length) return (results.innerHTML = "");
  const entries = await loadIndex();
  const scored = [];
  for (const e of entries) {
    let score = 0;
    for (const t of terms) {
      let s = 0;
      if (e.lt === t) s = 30;
      else if (e.lt.startsWith(t)) s = 18;
      else if (new RegExp(`\\b${t.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}`).test(e.lt)) s = 12;
      else if (e.lt.includes(t)) s = 8;
      else if (e.lp.includes(t)) s = 4;
      else if (e.lx.includes(t)) s = 1;
      if (!s) { score = 0; break; }
      score += s;
    }
    if (score) scored.push([score, e]);
  }
  scored.sort((a, b) => b[0] - a[0]);
  selected = 0;
  results.innerHTML = scored.length
    ? scored.slice(0, 12).map(([, e], i) =>
        `<li aria-selected="${i === 0}"><a href="${e.u}"><small>${escapeHtml(e.p)}</small>${mark(e.t, terms)}<span>${escapeHtml(e.x)}</span></a></li>`).join("")
    : `<li class="search-empty">No results for “${escapeHtml(input.value)}”.</li>`;
}

if (dialog) {
  $$("[data-search-open]").forEach((b) => b.addEventListener("click", openSearch));
  dialog.addEventListener("click", (e) => e.target === dialog && closeSearch());
  input.addEventListener("input", search);
  input.addEventListener("keydown", (e) => {
    const items = $$("li[aria-selected]", results);
    if (e.key === "ArrowDown" || e.key === "ArrowUp") {
      e.preventDefault();
      if (!items.length) return;
      selected = (selected + (e.key === "ArrowDown" ? 1 : -1) + items.length) % items.length;
      items.forEach((li, i) => li.setAttribute("aria-selected", i === selected));
      items[selected].scrollIntoView({ block: "nearest" });
    } else if (e.key === "Enter") {
      const link = items[selected]?.querySelector("a");
      if (link) location.href = link.href;
    }
  });
  document.addEventListener("keydown", (e) => {
    const typing = /^(INPUT|TEXTAREA|SELECT)$/.test(document.activeElement?.tagName) || document.activeElement?.isContentEditable;
    if ((e.key === "k" && (e.metaKey || e.ctrlKey)) || (e.key === "/" && !typing)) {
      e.preventDefault();
      dialog.hidden ? openSearch() : closeSearch();
    } else if (e.key === "Escape" && !dialog.hidden) closeSearch();
  });
  results.addEventListener("click", (e) => e.target.closest("a") && closeSearch());
}

// ── Download page: suggest the build for this machine ──
// The installer works the platform out exactly; this is the browser's best
// guess, for people who would rather download the archive themselves.
async function detectPlatform() {
  const ua = navigator.userAgent;
  let os = null;
  if (/Windows/.test(ua)) os = "windows";
  else if (/iPhone|iPad|iPod/.test(ua) || (/Macintosh/.test(ua) && navigator.maxTouchPoints > 1)) os = "ios";
  else if (/Android/.test(ua)) os = "android";
  else if (/Macintosh|Mac OS X/.test(ua)) os = "macos";
  else if (/Linux|X11|CrOS/.test(ua)) os = "linux";

  let arch = null;
  try {
    // Chromium browsers say outright, when asked.
    const hints = await navigator.userAgentData?.getHighEntropyValues(["architecture", "bitness"]);
    if (hints?.architecture === "arm") arch = "arm64";
    else if (hints?.architecture === "x86" && hints.bitness !== "32") arch = "x86";
  } catch {}
  if (!arch && os === "linux") {
    if (/aarch64|arm64/i.test(ua)) arch = "arm64";
    else if (/x86_64|amd64/i.test(ua)) arch = "x86";
  }
  if (!arch && os === "macos") {
    // Safari and Firefox call every Mac "Intel"; the GPU's name tells.
    try {
      const gl = document.createElement("canvas").getContext("webgl");
      const info = gl?.getExtension("WEBGL_debug_renderer_info");
      const gpu = info ? gl.getParameter(info.UNMASKED_RENDERER_WEBGL) : "";
      if (/Apple (M\d|GPU)/i.test(gpu)) arch = "arm64";
      else if (/Intel|AMD|Radeon|NVIDIA/i.test(gpu)) arch = "x86";
    } catch {}
  }
  return { os, arch };
}

const detectBox = $("[data-detect]");
if (detectBox) {
  const files = JSON.parse($("#downloads")?.textContent || "[]");
  const name = $("[data-detect-name]", detectBox);
  const link = $("[data-detect-link]", detectBox);
  const label = $("[data-detect-label]", detectBox);
  const note = $("[data-detect-note]", detectBox);
  detectPlatform().then(({ os, arch }) => {
    if (os === "windows") {
      name.textContent = "Windows";
      label.textContent = "Install WSL";
      link.href = "https://learn.microsoft.com/windows/wsl/install";
      note.textContent = "No native Windows build yet. Run the one-line installer inside WSL.";
      return;
    }
    if (os === "ios" || os === "android" || !os) {
      name.textContent = os === "ios" ? "iPhone or iPad" : os === "android" ? "Android" : "Unknown";
      label.textContent = "Open the playground";
      link.href = "/playground/";
      note.textContent = "Oxigen installs on Linux and macOS. On this device, the playground runs it in the browser.";
      return;
    }
    // A Mac we couldn't read is most likely Apple silicon: every Mac since 2020.
    const guess = arch ?? (os === "macos" ? "arm64" : "x86");
    const file = files.find((f) => f.platform === `${os}_${guess}`);
    if (!file) return;
    name.textContent = `${file.os} on ${file.cpu}`;
    label.textContent = `Download for ${file.os} (${file.cpu}) · ${file.size}`;
    link.href = file.url;
    link.setAttribute("download", "");
    const other = files.find((f) => f.platform.startsWith(os) && f !== file);
    note.textContent = arch
      ? `Or use the one-line installer, which does this for you.`
      : `Your browser didn't say which CPU this is. If that's wrong, get the ${other ? other.cpu : "other"} build below, or use the one-line installer, which checks.`;
    $$(`tr[data-platform="${file.platform}"]`).forEach((row) => row.classList.add("is-yours"));
  });
}
