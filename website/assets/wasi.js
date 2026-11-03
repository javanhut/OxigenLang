// A small WASI (preview 1) system for running OxigenLang, and the Go language
// server, in the browser.
//
// The Kernel owns a file system in memory, the file descriptor table and
// standard input. It is plain synchronous JavaScript, so it can sit in the
// same worker as the program (`local`), or on the page, serving the program's
// worker and every thread it spawns through `remote`: a request goes out by
// postMessage and the worker sleeps on a SharedArrayBuffer until the page
// writes the answer. That is how a program blocks on `input()` until the user
// types, and how threads share one file system, as threads of a process do.
//
// `wasiImports` turns those kernel calls into the `wasi_snapshot_preview1`
// functions a module imports.

export const E = {
  SUCCESS: 0, ACCES: 2, AGAIN: 6, BADF: 8, EXIST: 20, FBIG: 22, INVAL: 28, IO: 29, ISDIR: 31,
  NAMETOOLONG: 37, NOENT: 44, NOSYS: 52, NOTDIR: 54, NOTEMPTY: 55, NOTSUP: 58, PERM: 63, SPIPE: 70, XDEV: 75,
};
const FT = { CHAR: 2, DIR: 3, REG: 4 };
const O = { CREAT: 1, DIRECTORY: 2, EXCL: 4, TRUNC: 8 };
const FD = { APPEND: 1, NONBLOCK: 4 };
const RIGHT_READ = 1n << 1n;
const RIGHT_WRITE = 1n << 6n;
const ALL_RIGHTS = 0x1fffffffn;

/**
 * The limits of the memory a module imports, read from its import section,
 * as `WebAssembly.Memory` takes them: the threads build imports a shared one.
 */
export function importedMemory(bytes) {
  const b = new Uint8Array(bytes);
  let at = 8;
  const leb = () => {
    let result = 0;
    let shift = 0;
    for (;;) {
      const byte = b[at++];
      result += (byte & 0x7f) * 2 ** shift;
      shift += 7;
      if (byte < 0x80) return result;
    }
  };
  const name = () => {
    const len = leb();
    at += len;
  };
  while (at < b.length) {
    const id = b[at++];
    const size = leb();
    const end = at + size;
    if (id === 2) {
      for (let count = leb(); count > 0; count--) {
        name();
        name();
        const kind = b[at++];
        if (kind === 0) leb();
        else if (kind === 1) (at++, b[at++] & 1 ? (leb(), leb()) : leb());
        else if (kind === 3) at += 2;
        else if (kind === 2) {
          const flags = b[at++];
          const initial = leb();
          const maximum = flags & 1 ? leb() : undefined;
          return { initial, maximum, shared: !!(flags & 2) };
        }
      }
    }
    at = end;
  }
  return null;
}

/** Thrown by `proc_exit`, so the caller sees the exit code instead of a trap. */
export class Exit extends Error {
  constructor(code) {
    super(`exit ${code}`);
    this.code = code;
  }
}

const encoder = new TextEncoder();
const decoder = new TextDecoder();
// TextDecoder and crypto refuse views of a SharedArrayBuffer; copy those first.
const plain = (view) => (view.buffer instanceof ArrayBuffer ? view : view.slice());
export const decode = (view) => decoder.decode(plain(view));

const now = () => performance.timeOrigin + performance.now();

// ── The file system ──

let inodes = 1;
const dir = () => ({ type: FT.DIR, ino: inodes++, entries: new Map(), mtime: now() });
const file = (data = new Uint8Array(0)) => ({ type: FT.REG, ino: inodes++, data, size: data.length, mtime: now() });

/** `path` without `.` and `..` parts, as an array of names. */
function split(path) {
  const parts = [];
  for (const part of path.split("/")) {
    if (!part || part === ".") continue;
    if (part === "..") parts.pop();
    else parts.push(part);
  }
  return parts;
}

export class Kernel {
  /**
   * `stdout(fd, bytes)` receives output on 1 and 2. `stdin` is the source of
   * standard input: `read(max)` returns bytes, an empty array at the end, or
   * null while nothing has been typed yet (the caller then waits).
   */
  constructor({ stdout = () => {}, stdin = null } = {}) {
    this.root = dir();
    this.stdout = stdout;
    this.stdin = stdin;
    this.fds = new Map([
      [0, { kind: "stdin" }],
      [1, { kind: "out" }],
      [2, { kind: "out" }],
      [3, { kind: "node", node: this.root, path: "/", pos: 0, flags: 0, read: true, write: true, preopen: "/" }],
    ]);
    this.nextFd = 4;
  }

  // ── Setting up and reading back, for the page ──

  /** Write `data` (a string or bytes) at absolute `path`, making directories. */
  writeFile(path, data) {
    const parts = split(path);
    const name = parts.pop();
    let node = this.root;
    for (const part of parts) {
      let next = node.entries.get(part);
      if (!next) node.entries.set(part, (next = dir()));
      node = next;
    }
    const bytes = typeof data === "string" ? encoder.encode(data) : data;
    node.entries.set(name, file(bytes));
  }

  /** Every file under `path`, as { relative path: bytes }. */
  files(path) {
    const out = {};
    const walk = (node, prefix) => {
      for (const [name, child] of node.entries) {
        if (child.type === FT.DIR) walk(child, `${prefix}${name}/`);
        else out[prefix + name] = child.data.subarray(0, child.size);
      }
    };
    const start = this.lookup(split(path));
    if (start?.type === FT.DIR) walk(start, "");
    return out;
  }

  lookup(parts) {
    let node = this.root;
    for (const part of parts) {
      if (node?.type !== FT.DIR) return null;
      node = node.entries.get(part);
    }
    return node ?? null;
  }

  // Resolve `path` against directory fd `dirfd`: [parent node, name, parts].
  resolve(dirfd, path) {
    const base = this.fds.get(dirfd);
    if (!base || base.kind !== "node") return { e: E.BADF };
    if (base.node.type !== FT.DIR) return { e: E.NOTDIR };
    const parts = path.startsWith("/") ? split(path) : [...split(base.path), ...split(path)];
    return { parts };
  }

  stat(node) {
    const size = node.type === FT.REG ? node.size : node.type === FT.DIR ? node.entries.size : 0;
    return { e: 0, filetype: node.type, ino: node.ino, size, mtime: node.mtime };
  }

  // ── WASI calls. Each takes plain arguments and returns { e, ... }. ──

  open({ dirfd, path, oflags, fdflags, rights }) {
    const r = this.resolve(dirfd, path);
    if (r.e) return r;
    const { parts } = r;
    let node = this.lookup(parts);
    const wantsWrite = (BigInt(rights) & RIGHT_WRITE) !== 0n || oflags & O.TRUNC;
    if (node) {
      if (oflags & O.CREAT && oflags & O.EXCL) return { e: E.EXIST };
      if (oflags & O.DIRECTORY && node.type !== FT.DIR) return { e: E.NOTDIR };
      if (node.type === FT.DIR && wantsWrite && !(oflags & O.DIRECTORY)) return { e: E.ISDIR };
      if (oflags & O.TRUNC && node.type === FT.REG) {
        node.data = new Uint8Array(0);
        node.size = 0;
        node.mtime = now();
      }
    } else {
      if (!(oflags & O.CREAT)) return { e: E.NOENT };
      const parent = this.lookup(parts.slice(0, -1));
      if (!parent) return { e: E.NOENT };
      if (parent.type !== FT.DIR) return { e: E.NOTDIR };
      if (!parts.length) return { e: E.ISDIR };
      node = file();
      parent.entries.set(parts.at(-1), node);
      parent.mtime = now();
    }
    const fd = this.nextFd++;
    this.fds.set(fd, {
      kind: "node",
      node,
      path: "/" + parts.join("/"),
      pos: 0,
      flags: fdflags,
      read: (BigInt(rights) & RIGHT_READ) !== 0n || node.type === FT.DIR,
      write: !!wantsWrite,
    });
    return { e: 0, fd };
  }

  close({ fd }) {
    if (!this.fds.has(fd)) return { e: E.BADF };
    if (fd > 3) this.fds.delete(fd);
    return { e: 0 };
  }

  read({ fd, len, offset }) {
    const f = this.fds.get(fd);
    if (!f) return { e: E.BADF };
    if (f.kind === "stdin") {
      if (!this.stdin) return { e: 0, data: new Uint8Array(0) };
      const data = this.stdin.read(len);
      if (data === null) return { e: f.flags & FD.NONBLOCK ? E.AGAIN : 0, wait: true };
      return { e: 0, data };
    }
    if (f.kind !== "node") return { e: E.BADF };
    if (f.node.type === FT.DIR) return { e: E.ISDIR };
    const at = offset ?? f.pos;
    const end = Math.min(f.node.size, at + len);
    const data = at < end ? f.node.data.slice(at, end) : new Uint8Array(0);
    if (offset == null) f.pos = Math.max(at, end);
    return { e: 0, data };
  }

  write({ fd, data, offset }) {
    const f = this.fds.get(fd);
    if (!f) return { e: E.BADF };
    if (f.kind === "out") {
      this.stdout(fd, data);
      return { e: 0, n: data.length };
    }
    if (f.kind !== "node" || f.node.type !== FT.REG) return { e: E.BADF };
    const node = f.node;
    const at = offset ?? (f.flags & FD.APPEND ? node.size : f.pos);
    const end = at + data.length;
    if (end > node.data.length) {
      const grown = new Uint8Array(Math.max(end, node.data.length * 2, 64));
      grown.set(node.data.subarray(0, node.size));
      node.data = grown;
    }
    node.data.set(data, at);
    node.size = Math.max(node.size, end);
    node.mtime = now();
    if (offset == null) f.pos = end;
    return { e: 0, n: data.length };
  }

  seek({ fd, offset, whence }) {
    const f = this.fds.get(fd);
    if (!f) return { e: E.BADF };
    if (f.kind !== "node") return { e: E.SPIPE };
    const base = whence === 0 ? 0 : whence === 1 ? f.pos : f.node.size ?? 0;
    const pos = base + offset;
    if (pos < 0) return { e: E.INVAL };
    f.pos = pos;
    return { e: 0, pos };
  }

  fdstat({ fd }) {
    const f = this.fds.get(fd);
    if (!f) return { e: E.BADF };
    const filetype = f.kind === "node" ? f.node.type : FT.CHAR;
    return { e: 0, filetype, flags: f.flags ?? 0 };
  }

  setFlags({ fd, flags }) {
    const f = this.fds.get(fd);
    if (!f) return { e: E.BADF };
    f.flags = flags;
    return { e: 0 };
  }

  filestat({ fd }) {
    const f = this.fds.get(fd);
    if (!f) return { e: E.BADF };
    if (f.kind !== "node") return { e: 0, filetype: FT.CHAR, ino: 0, size: 0, mtime: 0 };
    return this.stat(f.node);
  }

  truncate({ fd, size }) {
    const f = this.fds.get(fd);
    if (!f || f.kind !== "node") return { e: E.BADF };
    if (f.node.type !== FT.REG) return { e: E.ISDIR };
    const grown = new Uint8Array(size);
    grown.set(f.node.data.subarray(0, Math.min(size, f.node.size)));
    f.node.data = grown;
    f.node.size = size;
    f.node.mtime = now();
    return { e: 0 };
  }

  pathStat({ dirfd, path }) {
    const r = this.resolve(dirfd, path);
    if (r.e) return r;
    const node = this.lookup(r.parts);
    return node ? this.stat(node) : { e: E.NOENT };
  }

  readdir({ fd, cookie }) {
    const f = this.fds.get(fd);
    if (!f || f.kind !== "node") return { e: E.BADF };
    if (f.node.type !== FT.DIR) return { e: E.NOTDIR };
    const all = [
      [".", f.node],
      ["..", f.node],
      ...f.node.entries,
    ];
    const entries = all.slice(Number(cookie)).map(([name, node], i) => ({
      name,
      type: node.type,
      ino: node.ino,
      next: Number(cookie) + i + 1,
    }));
    return { e: 0, entries };
  }

  mkdir({ dirfd, path }) {
    const r = this.resolve(dirfd, path);
    if (r.e) return r;
    if (!r.parts.length || this.lookup(r.parts)) return { e: E.EXIST };
    const parent = this.lookup(r.parts.slice(0, -1));
    if (!parent) return { e: E.NOENT };
    if (parent.type !== FT.DIR) return { e: E.NOTDIR };
    parent.entries.set(r.parts.at(-1), dir());
    return { e: 0 };
  }

  remove({ dirfd, path, directory }) {
    const r = this.resolve(dirfd, path);
    if (r.e) return r;
    const node = this.lookup(r.parts);
    if (!node || !r.parts.length) return { e: r.parts.length ? E.NOENT : E.PERM };
    if (directory && node.type !== FT.DIR) return { e: E.NOTDIR };
    if (!directory && node.type === FT.DIR) return { e: E.ISDIR };
    if (directory && node.entries.size) return { e: E.NOTEMPTY };
    this.lookup(r.parts.slice(0, -1)).entries.delete(r.parts.at(-1));
    return { e: 0 };
  }

  rename({ dirfd, path, newDirfd, newPath }) {
    const from = this.resolve(dirfd, path);
    const to = this.resolve(newDirfd, newPath);
    if (from.e || to.e) return from.e ? from : to;
    const node = this.lookup(from.parts);
    if (!node || !from.parts.length) return { e: E.NOENT };
    const parent = this.lookup(to.parts.slice(0, -1));
    if (!parent) return { e: E.NOENT };
    if (parent.type !== FT.DIR) return { e: E.NOTDIR };
    const existing = parent.entries.get(to.parts.at(-1));
    if (existing?.type === FT.DIR && existing.entries.size) return { e: E.NOTEMPTY };
    this.lookup(from.parts.slice(0, -1)).entries.delete(from.parts.at(-1));
    parent.entries.set(to.parts.at(-1), node);
    return { e: 0 };
  }

  prestat({ fd }) {
    const f = this.fds.get(fd);
    if (!f || !f.preopen) return { e: E.BADF };
    return { e: 0, name: f.preopen };
  }

  /** Dispatch a call by name, for the page answering a worker. */
  call(op, args) {
    const fn = this[op];
    if (typeof fn !== "function" || !CALLS.has(op)) return { e: E.NOSYS };
    try {
      return fn.call(this, args);
    } catch (err) {
      return { e: E.IO, message: String(err) };
    }
  }
}

const CALLS = new Set([
  "open", "close", "read", "write", "seek", "fdstat", "setFlags", "filestat", "truncate", "pathStat",
  "readdir", "mkdir", "remove", "rename", "prestat",
]);

// ── Standard input fed from the page ──

/** Standard input that is text queued up front, then nothing (the end). */
export class PresetInput {
  constructor(text = "") {
    this.bytes = encoder.encode(text);
    this.at = 0;
  }
  read(max) {
    const data = this.bytes.slice(this.at, this.at + max);
    this.at += data.length;
    return data;
  }
}

/** Standard input that the user types: lines queue up, null means "waiting". */
export class TypedInput {
  constructor(text = "") {
    this.queue = text ? [encoder.encode(text)] : [];
    this.ended = false;
  }
  push(text) {
    this.queue.push(encoder.encode(text));
  }
  end() {
    this.ended = true;
  }
  read(max) {
    if (!this.queue.length) return this.ended ? new Uint8Array(0) : null;
    const head = this.queue[0];
    const data = head.slice(0, max);
    if (data.length === head.length) this.queue.shift();
    else this.queue[0] = head.slice(max);
    return data;
  }
}

/**
 * A one-way byte pipe through a SharedArrayBuffer, page to worker: the page
 * writes without blocking, the worker reads and sleeps while it is empty.
 * Used for the language server's standard input.
 */
export class SabPipe {
  static create(size = 1 << 22) {
    return new SharedArrayBuffer(8 + size);
  }
  constructor(sab) {
    this.head = new Int32Array(sab, 0, 2); // [0]: bytes written, [1]: bytes read (both total, mod 2^31)
    this.data = new Uint8Array(sab, 8);
    this.pending = [];
  }
  // Page side.
  write(bytes) {
    this.pending.push(bytes);
    this.flush();
  }
  flush() {
    while (this.pending.length) {
      const bytes = this.pending[0];
      const w = Atomics.load(this.head, 0);
      const r = Atomics.load(this.head, 1);
      const free = this.data.length - (w - r);
      if (free < bytes.length) {
        setTimeout(() => this.flush(), 10);
        return;
      }
      for (let i = 0; i < bytes.length; i++) this.data[(w + i) % this.data.length] = bytes[i];
      this.pending.shift();
      Atomics.store(this.head, 0, w + bytes.length);
      Atomics.notify(this.head, 0);
    }
  }
  // Worker side: the Kernel's stdin. Null while empty; `wait` blocks for more.
  read(max) {
    const r = Atomics.load(this.head, 1);
    const w = Atomics.load(this.head, 0);
    if (w === r) return null;
    const n = Math.min(max, w - r);
    const out = new Uint8Array(n);
    for (let i = 0; i < n; i++) out[i] = this.data[(r + i) % this.data.length];
    Atomics.store(this.head, 1, r + n);
    return out;
  }
  available() {
    return Atomics.load(this.head, 0) - Atomics.load(this.head, 1);
  }
  /** Sleep until there is something to read or `ms` pass; true if there is. */
  wait(ms = Infinity) {
    const w = Atomics.load(this.head, 0);
    if (w === Atomics.load(this.head, 1)) Atomics.wait(this.head, 0, w, ms);
    return this.available() > 0;
  }
}

// ── Kernel calls from a worker, answered by the page ──

const SLOT_DATA = 1 << 20; // the largest answer one call returns

/** Allocate the shared answer slot a worker waits on. */
export const createSlot = () => new SharedArrayBuffer(12 + SLOT_DATA);

/**
 * The worker's side: send `op` to the page and sleep until it answers.
 * `post(message)` delivers to the page.
 */
export function remote(slotBuffer, post) {
  const flag = new Int32Array(slotBuffer, 0, 3);
  const bytes = new Uint8Array(slotBuffer, 12);
  return (op, args) => {
    Atomics.store(flag, 0, 0);
    post({ type: "sys", op, args });
    while (Atomics.load(flag, 0) === 0) Atomics.wait(flag, 0, 0);
    const jsonLen = flag[1];
    const binLen = flag[2];
    const result = JSON.parse(decoder.decode(bytes.slice(0, jsonLen)));
    if (binLen >= 0) result.data = bytes.slice(jsonLen, jsonLen + binLen);
    return result;
  };
}

/** The page's side: write `result` into the worker's slot and wake it. */
export function answer(slotBuffer, result) {
  const flag = new Int32Array(slotBuffer, 0, 3);
  const bytes = new Uint8Array(slotBuffer, 12);
  const { data, ...rest } = result;
  const json = encoder.encode(JSON.stringify(rest));
  const bin = data ? data.subarray(0, SLOT_DATA - json.length) : null;
  bytes.set(json, 0);
  if (bin) bytes.set(bin, json.length);
  flag[1] = json.length;
  flag[2] = bin ? bin.length : -1;
  Atomics.store(flag, 0, 1);
  Atomics.notify(flag, 0);
}

// ── wasi_snapshot_preview1 ──

/**
 * The imports for a module. `sys(op, args)` makes a kernel call (the Kernel's
 * own `call`, or `remote`). `memory()` returns the module's memory once it
 * exists. `out(fd, bytes)` takes writes to 1 and 2 without a kernel call.
 * `input`, when standard input is read here rather than on the page, says
 * whether some is `ready()` and can `wait(ms)` for it (true once there is).
 * `sleep(ms)` blocks the thread; `beforeBlock()` runs before anything blocks.
 */
export function wasiImports({ sys, memory, args = [], env = {}, out, input = null, sleep = sleepFor, beforeBlock = () => {} }) {
  const view = () => new DataView(memory().buffer);
  const u8 = () => new Uint8Array(memory().buffer);
  const str = (ptr, len) => decode(u8().subarray(ptr, ptr + len));
  const argBytes = args.map((a) => encoder.encode(a + "\0"));
  const envBytes = Object.entries(env).map(([k, v]) => encoder.encode(`${k}=${v}\0`));

  const strings = (list) => ({
    sizes(countPtr, sizePtr) {
      const v = view();
      v.setUint32(countPtr, list.length, true);
      v.setUint32(sizePtr, list.reduce((n, b) => n + b.length, 0), true);
      return 0;
    },
    get(ptrsPtr, bufPtr) {
      const v = view();
      const m = u8();
      let at = bufPtr;
      list.forEach((b, i) => {
        v.setUint32(ptrsPtr + i * 4, at, true);
        m.set(b, at);
        at += b.length;
      });
      return 0;
    },
  });
  const argv = strings(argBytes);
  const environ = strings(envBytes);

  const iovecs = (ptr, len) => {
    const v = view();
    const list = [];
    for (let i = 0; i < len; i++) list.push([v.getUint32(ptr + i * 8, true), v.getUint32(ptr + i * 8 + 4, true)]);
    return list;
  };

  const writeStat = (ptr, s) => {
    const v = view();
    for (let i = 0; i < 64; i += 8) v.setBigUint64(ptr + i, 0n, true);
    v.setBigUint64(ptr + 8, BigInt(s.ino || 0), true);
    v.setUint8(ptr + 16, s.filetype);
    v.setBigUint64(ptr + 24, 1n, true);
    v.setBigUint64(ptr + 32, BigInt(s.size || 0), true);
    const t = BigInt(Math.round((s.mtime || 0) * 1e6));
    v.setBigUint64(ptr + 40, t, true);
    v.setBigUint64(ptr + 48, t, true);
    v.setBigUint64(ptr + 56, t, true);
  };

  // Reading standard input blocks; everything else answers at once.
  const readFd = (fd, len, offset) => {
    for (;;) {
      const r = sys("read", { fd, len, offset });
      if (!r.wait || r.e) return r;
      beforeBlock();
      if (!input) return { e: 0, data: new Uint8Array(0) };
      input.wait();
    }
  };

  const wasi = {
    args_sizes_get: argv.sizes,
    args_get: argv.get,
    environ_sizes_get: environ.sizes,
    environ_get: environ.get,

    clock_res_get(id, ptr) {
      view().setBigUint64(ptr, 1000n, true);
      return 0;
    },
    clock_time_get(id, precision, ptr) {
      const ns = id === 0 ? BigInt(Date.now()) * 1000000n : BigInt(Math.round(now() * 1e6));
      view().setBigUint64(ptr, ns, true);
      return 0;
    },
    random_get(ptr, len) {
      const chunk = new Uint8Array(Math.min(len, 65536));
      for (let at = 0; at < len; at += chunk.length) {
        const part = chunk.subarray(0, Math.min(chunk.length, len - at));
        crypto.getRandomValues(part);
        u8().set(part, ptr + at);
      }
      return 0;
    },
    sched_yield: () => 0,
    proc_exit(code) {
      beforeBlock();
      throw new Exit(code);
    },
    proc_raise: () => E.NOSYS,

    fd_write(fd, iovs, iovsLen, nwrittenPtr) {
      const m = u8();
      let total = 0;
      const parts = iovecs(iovs, iovsLen).map(([p, l]) => m.slice(p, p + l));
      const data = parts.length === 1 ? parts[0] : concat(parts);
      if ((fd === 1 || fd === 2) && out) {
        out(fd, data);
        total = data.length;
      } else if (data.length) {
        const r = sys("write", { fd, data });
        if (r.e) return r.e;
        total = r.n;
      }
      view().setUint32(nwrittenPtr, total, true);
      return 0;
    },
    fd_pwrite(fd, iovs, iovsLen, offset, nwrittenPtr) {
      const m = u8();
      const data = concat(iovecs(iovs, iovsLen).map(([p, l]) => m.slice(p, p + l)));
      const r = sys("write", { fd, data, offset: Number(offset) });
      if (r.e) return r.e;
      view().setUint32(nwrittenPtr, r.n, true);
      return 0;
    },
    fd_read(fd, iovs, iovsLen, nreadPtr) {
      const list = iovecs(iovs, iovsLen);
      const want = list.reduce((n, [, l]) => n + l, 0);
      const r = readFd(fd, want);
      if (r.e) return r.e;
      scatter(u8(), list, r.data);
      view().setUint32(nreadPtr, r.data.length, true);
      return 0;
    },
    fd_pread(fd, iovs, iovsLen, offset, nreadPtr) {
      const list = iovecs(iovs, iovsLen);
      const want = list.reduce((n, [, l]) => n + l, 0);
      const r = readFd(fd, want, Number(offset));
      if (r.e) return r.e;
      scatter(u8(), list, r.data);
      view().setUint32(nreadPtr, r.data.length, true);
      return 0;
    },
    fd_seek(fd, offset, whence, newPtr) {
      const r = sys("seek", { fd, offset: Number(offset), whence });
      if (r.e) return r.e;
      view().setBigUint64(newPtr, BigInt(r.pos), true);
      return 0;
    },
    fd_tell(fd, ptr) {
      const r = sys("seek", { fd, offset: 0, whence: 1 });
      if (r.e) return r.e;
      view().setBigUint64(ptr, BigInt(r.pos), true);
      return 0;
    },
    fd_close: (fd) => sys("close", { fd }).e,
    fd_sync: () => 0,
    fd_datasync: () => 0,
    fd_advise: () => 0,
    fd_allocate: () => 0,
    fd_fdstat_get(fd, ptr) {
      const r = sys("fdstat", { fd });
      if (r.e) return r.e;
      const v = view();
      v.setUint8(ptr, r.filetype);
      v.setUint16(ptr + 2, r.flags, true);
      v.setBigUint64(ptr + 8, ALL_RIGHTS, true);
      v.setBigUint64(ptr + 16, ALL_RIGHTS, true);
      return 0;
    },
    fd_fdstat_set_flags: (fd, flags) => sys("setFlags", { fd, flags }).e,
    fd_fdstat_set_rights: () => 0,
    fd_filestat_get(fd, ptr) {
      const r = sys("filestat", { fd });
      if (r.e) return r.e;
      writeStat(ptr, r);
      return 0;
    },
    fd_filestat_set_size: (fd, size) => sys("truncate", { fd, size: Number(size) }).e,
    fd_filestat_set_times: () => 0,
    fd_prestat_get(fd, ptr) {
      const r = sys("prestat", { fd });
      if (r.e) return r.e;
      const v = view();
      v.setUint8(ptr, 0);
      v.setUint32(ptr + 4, encoder.encode(r.name).length, true);
      return 0;
    },
    fd_prestat_dir_name(fd, ptr, len) {
      const r = sys("prestat", { fd });
      if (r.e) return r.e;
      u8().set(encoder.encode(r.name).subarray(0, len), ptr);
      return 0;
    },
    fd_readdir(fd, buf, len, cookie, usedPtr) {
      const r = sys("readdir", { fd, cookie: Number(cookie) });
      if (r.e) return r.e;
      const m = u8();
      const v = view();
      let used = 0;
      for (const entry of r.entries) {
        const name = encoder.encode(entry.name);
        const record = new Uint8Array(24 + name.length);
        const rv = new DataView(record.buffer);
        rv.setBigUint64(0, BigInt(entry.next), true);
        rv.setBigUint64(8, BigInt(entry.ino), true);
        rv.setUint32(16, name.length, true);
        rv.setUint8(20, entry.type);
        record.set(name, 24);
        const take = Math.min(record.length, len - used);
        m.set(record.subarray(0, take), buf + used);
        used += take;
        if (used >= len) break;
      }
      v.setUint32(usedPtr, used, true);
      return 0;
    },
    fd_renumber: () => E.NOTSUP,

    path_open(dirfd, lookupFlags, pathPtr, pathLen, oflags, rightsBase, rightsInheriting, fdflags, fdPtr) {
      const r = sys("open", { dirfd, path: str(pathPtr, pathLen), oflags, fdflags, rights: String(rightsBase) });
      if (r.e) return r.e;
      view().setUint32(fdPtr, r.fd, true);
      return 0;
    },
    path_filestat_get(dirfd, flags, pathPtr, pathLen, ptr) {
      const r = sys("pathStat", { dirfd, path: str(pathPtr, pathLen) });
      if (r.e) return r.e;
      writeStat(ptr, r);
      return 0;
    },
    path_filestat_set_times: () => 0,
    path_create_directory: (dirfd, p, l) => sys("mkdir", { dirfd, path: str(p, l) }).e,
    path_remove_directory: (dirfd, p, l) => sys("remove", { dirfd, path: str(p, l), directory: true }).e,
    path_unlink_file: (dirfd, p, l) => sys("remove", { dirfd, path: str(p, l), directory: false }).e,
    path_rename: (fd, p, l, newFd, np, nl) => sys("rename", { dirfd: fd, path: str(p, l), newDirfd: newFd, newPath: str(np, nl) }).e,
    path_readlink: () => E.INVAL,
    path_symlink: () => E.NOTSUP,
    path_link: () => E.NOTSUP,

    poll_oneoff(inPtr, outPtr, count, neventsPtr) {
      // Sleeping, and (for the language server) waiting for standard input.
      const v = view();
      let timeout = Infinity;
      const readers = [];
      const clocks = [];
      for (let i = 0; i < count; i++) {
        const at = inPtr + i * 48;
        const userdata = v.getBigUint64(at, true);
        const tag = v.getUint8(at + 8);
        if (tag === 0) {
          const ns = v.getBigUint64(at + 24, true);
          const absolute = v.getUint16(at + 40, true) & 1;
          let ms = Number(ns) / 1e6;
          if (absolute) ms -= v.getUint32(at + 16, true) === 0 ? Date.now() : now();
          timeout = Math.min(timeout, Math.max(0, ms));
          clocks.push(userdata);
        } else {
          readers.push({ userdata, tag, fd: v.getUint32(at + 16, true) });
        }
      }
      const events = [];
      const ready = () => {
        for (const r of readers) {
          if (r.tag === 2 || r.fd !== 0 || !input || input.ready()) events.push({ userdata: r.userdata, type: r.tag });
        }
      };
      ready();
      if (!events.length) {
        beforeBlock();
        const waited = readers.length ? input.wait(timeout) : (sleep(timeout), false);
        if (waited) ready();
        if (!events.length) for (const userdata of clocks) events.push({ userdata, type: 0 });
      }
      events.forEach((e, i) => {
        const at = outPtr + i * 32;
        v.setBigUint64(at, e.userdata, true);
        v.setUint16(at + 8, 0, true);
        v.setUint8(at + 10, e.type);
        v.setBigUint64(at + 16, 1n, true);
        v.setUint16(at + 24, 0, true);
      });
      v.setUint32(neventsPtr, events.length, true);
      return 0;
    },

    sock_accept: () => E.NOTSUP,
    sock_recv: () => E.NOTSUP,
    sock_send: () => E.NOTSUP,
    sock_shutdown: () => E.NOTSUP,
  };
  return wasi;
}

function concat(parts) {
  const total = parts.reduce((n, p) => n + p.length, 0);
  const out = new Uint8Array(total);
  let at = 0;
  for (const p of parts) {
    out.set(p, at);
    at += p.length;
  }
  return out;
}

function scatter(m, list, data) {
  let at = 0;
  for (const [p, l] of list) {
    if (at >= data.length) break;
    const n = Math.min(l, data.length - at);
    m.set(data.subarray(at, at + n), p);
    at += n;
  }
}

/** Block this worker for `ms`: Atomics.wait where it exists, else spin. */
export function sleepFor(ms) {
  if (!(ms > 0)) return;
  if (typeof SharedArrayBuffer === "function") {
    Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, ms);
    return;
  }
  const until = performance.now() + ms;
  while (performance.now() < until);
}
