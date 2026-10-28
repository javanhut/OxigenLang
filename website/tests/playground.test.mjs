// Editing and scroll synchronization regressions, without a DOM dependency.
// Run: node --test website/tests/playground.test.mjs
// Layout, native caret placement and browser undo still need a browser check.
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { test } from "node:test";
import vm from "node:vm";

const source = await readFile(new URL("../assets/playground.js", import.meta.url), "utf8");

async function editor(toolOverrides = {}) {
  class Element extends EventTarget {
    style = {};
    dataset = {};
    children = [];
    classList = { add() {}, remove() {} };
    value = "";
    textContent = "";
    selectionStart = 0;
    selectionEnd = 0;
    selectionDirection = "none";
    scrollTop = 0;
    scrollLeft = 0;
    clientWidth = 600;
    clientHeight = 400;
    offsetHeight = 415;
    setAttribute() {}
    prepend(child) { this.children.unshift(child); }
    querySelector() { return null; }
    setSelectionRange(start, end, direction = "none") {
      this.selectionStart = start;
      this.selectionEnd = end;
      this.selectionDirection = direction;
    }
    setRangeText(text, start, end) {
      this.value = this.value.slice(0, start) + text + this.value.slice(end);
      this.setSelectionRange(start + text.length, start + text.length);
    }
    select() { this.setSelectionRange(0, this.value.length); }
    focus() {
      document.activeElement = this;
      this.dispatchEvent(new Event("focus"));
    }
    getBoundingClientRect() { return { left: 0, top: 0 }; }
  }
  const elements = new Map();
  const el = (id) => {
    if (!elements.has(id)) elements.set(id, new Element());
    return elements.get(id);
  };
  const document = new EventTarget();
  Object.assign(document, {
    querySelector: () => ({ content: "runner.js" }),
    getElementById: el,
    createElement: () => new Element(),
    // Exercise the setRangeText fallback; real execCommand/undo is browser-owned.
    execCommand: () => false,
    createTreeWalker: () => ({ nextNode: () => null }),
  });
  el("pg-examples").textContent = JSON.stringify([{ slug: "hello", source: "hello\n", summary: "Hello" }]);
  el("pg-hl").parentElement = new Element();
  el("pg-hl").parentElement.parentElement = new Element();
  const drafts = new Map();
  const context = vm.createContext({
    document, Event, URLSearchParams,
    runner: { tools: async () => ({ highlight: (text) => text, check: () => [], ...toolOverrides }) },
    localStorage: { getItem: (k) => drafts.get(k), setItem: (k, v) => drafts.set(k, v) },
    getComputedStyle: () => ({ lineHeight: "24px", paddingTop: "20px" }),
    NodeFilter: { SHOW_TEXT: 4 },
    ResizeObserver: class { observe() {} },
    addEventListener() {},
    setTimeout: () => 0, clearTimeout() {},
    requestAnimationFrame: () => 0, cancelAnimationFrame() {},
    location: { hash: "", pathname: "/playground/" },
    history: { replaceState() {} },
  });
  await vm.runInContext(source.replace("await import(runnerUrl)", "globalThis.runner"), context);
  const area = el("pg-src");
  return {
    area, el, drafts,
    load(text, start = 0, end = start, direction = "none") {
      area.value = text;
      area.setSelectionRange(start, end, direction);
      area.dispatchEvent(new Event("input"));
    },
    key(key, options = {}) {
      const event = new Event("keydown", { cancelable: true });
      Object.assign(event, { key, ...options });
      area.dispatchEvent(event);
      document.dispatchEvent(new Event("selectionchange"));
      return event.defaultPrevented;
    },
  };
}

test("scrolling follows the textarea on both axes without resizing it", async () => {
  const e = await editor();
  e.load("first\n\tthird", 12);
  e.area.scrollTop = 48;
  e.area.scrollLeft = 120;
  e.area.dispatchEvent(new Event("scroll"));
  const pre = e.el("pg-hl").parentElement;
  assert.equal(pre.style.transform, "translate(-120px, -48px)");
  assert.equal(pre.parentElement.style.width, "600px");
  assert.equal(pre.parentElement.style.height, "400px");
  assert.equal(e.el("pg-gutter").scrollTop, 48);
  assert.equal(e.el("pg-gutter").style.paddingBottom, "35px");
  assert.equal(e.area.style.width, undefined);
  assert.equal(e.area.style.height, undefined);
  assert.equal(e.el("pg-position").textContent, "Ln 2, Col 10");
  assert.equal(e.el("pg-code").children[0].style.top, "-4px");
});

test("a trailing newline uses one sentinel without adding an extra line", async () => {
  const e = await editor();
  e.load("hello\n", 6);
  assert.equal(e.el("pg-hl").innerHTML, "hello\n ");
  assert.equal(e.el("pg-position").textContent, "Ln 2, Col 1");
});

test("indent and outdent preserve a backward selection and exclude its end line", async () => {
  const e = await editor();
  e.load("one\ntwo\nthree", 1, 8, "backward");
  assert.ok(e.key("Tab"));
  assert.equal(e.area.value, "    one\n    two\nthree");
  assert.equal(e.area.selectionStart, 5);
  assert.equal(e.area.selectionEnd, 16);
  assert.equal(e.area.selectionDirection, "backward");
  assert.equal(e.el("pg-position").textContent, "Ln 1, Col 6");
  e.key("Tab", { shiftKey: true });
  assert.equal(e.area.value, "one\ntwo\nthree");
  assert.equal(e.area.selectionStart, 1);
  assert.equal(e.area.selectionEnd, 8);
  assert.equal(e.area.selectionDirection, "backward");
});

test("comment toggles keep a collapsed caret on the same text", async () => {
  const e = await editor();
  e.load("    print(1)", 7);
  e.key("/", { ctrlKey: true });
  assert.equal(e.area.value, "    // print(1)");
  assert.equal(e.area.selectionStart, 10);
  assert.equal(e.area.selectionEnd, 10);
  e.key("/", { metaKey: true });
  assert.equal(e.area.value, "    print(1)");
  assert.equal(e.area.selectionStart, 7);
  assert.equal(e.area.selectionEnd, 7);
});

test("editing at position zero works when the source starts with a newline", async () => {
  const e = await editor();
  e.load("\ncode");
  e.key("Tab");
  assert.equal(e.area.value, "    \ncode");
  assert.equal(e.area.selectionStart, 4);
});

test("Tab uses visual tab stops after existing tabs", async () => {
  const e = await editor();
  e.load("\tx", 2);
  e.key("Tab");
  assert.equal(e.area.value, "\tx   ");
  assert.equal(e.area.selectionStart, 5);
});

test("Enter between braces preserves mixed tab and space indentation", async () => {
  const e = await editor();
  e.load("\t  {}", 4);
  e.key("Enter");
  assert.equal(e.area.value, "\t  {\n\t      \n\t  }");
  assert.equal(e.area.selectionStart, 12);
  assert.equal(e.area.selectionEnd, 12);
});

test("pair insertion, closer skip and paired deletion keep the caret inside the edit", async () => {
  const e = await editor();
  e.load("");
  e.key("(");
  assert.equal(e.area.value, "()");
  assert.equal(e.area.selectionStart, 1);
  e.key(")");
  assert.equal(e.area.value, "()");
  assert.equal(e.area.selectionStart, 2);
  e.area.setSelectionRange(1, 1);
  e.key("Backspace");
  assert.equal(e.area.value, "");
  assert.equal(e.area.selectionStart, 0);
  assert.equal(e.drafts.get("oxigen-playground-draft"), "");
});

test("Escape then Tab lets keyboard focus leave the editor", async () => {
  const e = await editor();
  e.load("hello");
  assert.equal(e.key("Escape"), false);
  assert.equal(e.key("Tab"), false);
  assert.equal(e.area.value, "hello");
});

test("formatting preserves the caret column, clamped to the new line length", async () => {
  const e = await editor({ format: () => ({ ok: true, source: "first\nshort" }) });
  e.load("first\na much longer line", 18);
  e.key("s", { ctrlKey: true });
  assert.equal(e.area.value, "first\nshort");
  assert.equal(e.area.selectionStart, 11);
  assert.equal(e.area.selectionEnd, 11);
  assert.equal(e.el("pg-position").textContent, "Ln 2, Col 6");
});

test("loading an example resets the caret and both scroll positions", async () => {
  const e = await editor();
  e.load("old source", 8);
  e.area.scrollTop = 300;
  e.area.scrollLeft = 200;
  e.el("pg-example").value = "hello";
  e.el("pg-example").dispatchEvent(new Event("change"));
  assert.equal(e.area.value, "hello\n");
  assert.equal(e.area.selectionStart, 0);
  assert.equal(e.area.scrollTop, 0);
  assert.equal(e.area.scrollLeft, 0);
});
