// The playground's runtime, end to end in Node: the WASI layer, runs of the
// real interpreter in both builds (single-threaded with input given up front,
// and threads with the page answering system calls), typed input, files,
// threads, tests, the editor's tools and the language server.
//
// Run: imlazy site-test, or node --test website/tests/playground.test.mjs
// The editor itself (CodeMirror, vim keys, tabs) still needs a browser check.
import assert from "node:assert/strict";
import { existsSync, readFileSync, readdirSync } from "node:fs";
import { after, before, describe, test } from "node:test";
import { fileURLToPath } from "node:url";
import { install } from "./browser-env.mjs";

const root = fileURLToPath(new URL("../../", import.meta.url));
const built = (path) => {
  const opt = path.replace(/\.wasm$/, ".opt.wasm");
  return existsSync(root + opt) ? root + opt : root + path;
};
const SINGLE = built("target/wasm32-wasip1/wasm/oxigen_playground.wasm");
const THREADS = built("target/wasm32-wasip1-threads/wasm/oxigen_playground.wasm");
const LSP = root + "target/wasm-lsp/oxigen-lsp.wasm";
const assets = new URL("../assets/", import.meta.url);

const wasi = await import(new URL("wasi.js", assets));

describe("the in-memory kernel", () => {
  const k = new wasi.Kernel();
  const open = (path, oflags = 0, rights = 1n << 1n | 1n << 6n) => k.call("open", { dirfd: 3, path, oflags, fdflags: 0, rights: String(rights) });
  const bytes = (s) => new TextEncoder().encode(s);

  test("creates, writes, seeks and reads a file", () => {
    k.call("mkdir", { dirfd: 3, path: "/work" });
    const { e, fd } = open("/work/a.txt", 1 | 8);
    assert.equal(e, 0);
    assert.equal(k.call("write", { fd, data: bytes("hello world") }).n, 11);
    assert.equal(k.call("seek", { fd, offset: 6, whence: 0 }).pos, 6);
    assert.equal(new TextDecoder().decode(k.call("read", { fd, len: 100 }).data), "world");
    assert.equal(k.call("pathStat", { dirfd: 3, path: "work/a.txt" }).size, 11);
    k.call("close", { fd });
  });

  test("reports the errors a program expects", () => {
    assert.equal(open("/work/missing.txt").e, wasi.E.NOENT);
    assert.equal(open("/work/a.txt", 1 | 4).e, wasi.E.EXIST);
    assert.equal(k.call("remove", { dirfd: 3, path: "/work", directory: true }).e, wasi.E.NOTEMPTY);
    assert.equal(k.call("mkdir", { dirfd: 3, path: "/nope/deeper" }).e, wasi.E.NOENT);
    assert.equal(k.call("read", { fd: 99, len: 1 }).e, wasi.E.BADF);
  });

  test("lists, renames and removes", () => {
    const dir = open("/work", 2);
    const names = k.call("readdir", { fd: dir.fd, cookie: 0 }).entries.map((x) => x.name);
    assert.deepEqual(names, [".", "..", "a.txt"]);
    assert.equal(k.call("rename", { dirfd: 3, path: "/work/a.txt", newDirfd: 3, newPath: "/work/b.txt" }).e, 0);
    assert.deepEqual(Object.keys(k.files("/work")), ["b.txt"]);
    assert.equal(k.call("remove", { dirfd: 3, path: "/work/b.txt", directory: false }).e, 0);
    assert.deepEqual(k.files("/work"), {});
  });

  test("appends with O_APPEND", () => {
    k.writeFile("/work/log.txt", "a");
    const { fd } = k.call("open", { dirfd: 3, path: "/work/log.txt", oflags: 0, fdflags: 1, rights: String(1n << 6n) });
    k.call("write", { fd, data: bytes("b") });
    assert.equal(new TextDecoder().decode(k.files("/work")["log.txt"]), "ab");
  });

  test("waits for typed input, then ends it", () => {
    const typed = new wasi.TypedInput();
    const kk = new wasi.Kernel({ stdin: typed });
    assert.equal(kk.call("read", { fd: 0, len: 10 }).wait, true);
    typed.push("hi\n");
    assert.equal(new TextDecoder().decode(kk.call("read", { fd: 0, len: 10 }).data), "hi\n");
    typed.end();
    assert.equal(kk.call("read", { fd: 0, len: 10 }).data.length, 0);
  });

  test("reads a module's imported memory limits", () => {
    const limits = wasi.importedMemory(readFileSync(THREADS));
    assert.equal(limits.shared, true);
    assert.ok(limits.maximum >= limits.initial);
    assert.equal(wasi.importedMemory(readFileSync(SINGLE)), null);
  });
});

/** Run `program` and collect what it printed. `feed(job)` answers input requests. */
async function run(runner, program, opts = {}, feed) {
  let out = "";
  let err = "";
  const job = runner.run(program, {
    stderr: "stream",
    onOutput: (t, fd) => (fd === 2 ? (err += t) : (out += t)),
    onWaiting: (waiting) => waiting && feed && setTimeout(() => feed(job), 10),
    ...opts,
  });
  const result = await job.done;
  return { ...result, out, err };
}

for (const isolated of [false, true]) {
  describe(isolated ? "runs on threads, the page answering system calls" : "single-threaded runs, input given up front", () => {
    let runner;
    before(async () => {
      install({ wasm: SINGLE, threads: THREADS, isolated });
      runner = await import(new URL(`runner.js?${isolated}`, assets));
    });

    test("prints, and returns the exit code", async () => {
      const r = await run(runner, 'main {\n    println("hello", 1 + 2)\n}\n');
      assert.equal(r.out, "hello 3\n");
      assert.equal(r.code, 0);
      assert.ok(r.ok);
    });

    test("reads standard input given up front", async () => {
      const r = await run(runner, 'introduce io\nmain {\n    n := io.input("name? ")\n    println("hi {n}")\n    println(io.read_line())\n}\n', { stdin: "Ada\nmore\n" });
      assert.equal(r.out, "name? hi Ada\nmore\n");
    });

    test("files: the program's own, written ones, and modules in other files", async () => {
      const r = await run(runner, {
        files: {
          "main.oxi": 'introduce io\nintroduce os\nintroduce helper\nmain {\n    println(helper.twice(21))\n    println(io.read_file("data.txt"))\n    io.write_file("out.txt", "written")\n    println(os.cwd(), os.args())\n}\n',
          "helper.oxi": "fun twice(x) { x * 2 }\n",
          "data.txt": "some data",
        },
      }, { args: ["a", "b"] });
      assert.equal(r.err, "");
      assert.equal(r.out, "42\nsome data\n/work [a, b]\n");
      assert.equal(new TextDecoder().decode(r.files["out.txt"]), "written");
      assert.deepEqual(Object.keys(r.files).sort(), ["data.txt", "helper.oxi", "main.oxi", "out.txt"]);
    });

    test("sleeps", async () => {
      const r = await run(runner, 'introduce time\nmain {\n    t := time.now_ms()\n    time.sleep(120)\n    println(time.now_ms() - t >= 110)\n}\n');
      assert.equal(r.out, "True\n");
    });

    test("a runtime error goes to standard error, pointing at the line", async () => {
      const r = await run(runner, "main {\n    x := [1]\n    println(x[3])\n}\n");
      assert.equal(r.code, 1);
      assert.match(r.err, /index 3 out of range/);
      assert.equal(runner.errorLine(r.err), 3);
    });

    test("os.exit ends the run with its code", async () => {
      const r = await run(runner, 'introduce os\nmain {\n    println("before")\n    os.exit(3)\n    println("after")\n}\n');
      assert.equal(r.out, "before\n");
      assert.equal(r.code, 3);
    });

    test("runs <test> blocks as oxigen test does", async () => {
      const r = await run(runner, '<test>("adds") {\n    expect(1 + 1).eq(2)\n}\n<test>("fails") {\n    expect(1).eq(2)\n}\n', { test: true });
      assert.match(r.out, /\[ok\] {3}adds/);
      assert.match(r.out, /\[fail\] fails/);
      assert.match(r.out, /test result: FAILED\. 1 passed; 1 failed/);
      assert.equal(r.code, 1);
    });

    test("an endless loop is stopped", async () => {
      const r = await run(runner, "main {\n    repeat when True {\n    }\n}\n", { timeLimit: 2 });
      assert.equal(r.ok, false);
      assert.ok(r.stopped || /loop budget/.test(r.err), r.err);
    });

    test("the browser's limits are reported, not hidden", async () => {
      const r = await run(runner, 'introduce net\nmain {\n    println(net.get("https://example.com"))\n}\n');
      assert.equal(r.ok, false);
      assert.match(r.err, /network/);
    });

    if (isolated) {
      test("input() waits for the user to type", async () => {
        const answers = ["x\n", "y\n"];
        const r = await run(runner, 'introduce io\nmain {\n    a := io.input("a? ")\n    b := io.input("b? ")\n    println("got {a} {b}")\n}\n', {}, (job) => job.input(answers.shift()));
        assert.equal(r.out, "a? b? got x y\n");
      });

      test("ending input reads as the end of the file", async () => {
        const r = await run(runner, 'introduce io\nmain {\n    println("[" + io.read_line() + "]")\n}\n', {}, (job) => job.endInput());
        assert.equal(r.out, "[]\n");
      });

      test("waiting for input does not count towards the time limit", async () => {
        const r = await run(runner, 'introduce io\nmain {\n    println(io.read_line())\n}\n', { timeLimit: 1 }, (job) => setTimeout(() => job.input("late\n"), 1500));
        assert.equal(r.out, "late\n");
      });

      test("diverge runs tasks on threads", async () => {
        const r = await run(runner, "main {\n    r := diverge each n in [1, 2, 3, 4] {\n        n * n\n    }\n    println(r)\n}\n");
        assert.equal(r.err, "");
        assert.equal(r.out, "[1, 4, 9, 16]\n");
      });

      test("channels pass messages between threads, and tasks share the files", async () => {
        const r = await run(runner, 'introduce channel\nintroduce io\nmain {\n    jobs := channel.new()\n    w := diverge {\n        n := 0\n        repeat when True {\n            j := channel.recv(jobs)\n            stop when j == None\n            io.append_file("log.txt", j)\n            n++\n        }\n        n\n    }\n    each s in ["a", "b", "c"] {\n        channel.send(jobs, s)\n    }\n    channel.close(jobs)\n    println(converge w, io.read_file("log.txt"))\n}\n');
        assert.equal(r.err, "");
        assert.equal(r.out, "3 abc\n");
      });
    } else {
      test("without isolation, threads are refused with a reason", async () => {
        const r = await run(runner, "main {\n    r := diverge each n in [1, 2] {\n        n\n    }\n}\n");
        assert.match(r.err, /threads/);
      });
    }

    test("every playground example compiles and those that need nothing run", async () => {
      const dir = new URL("../examples/playground/", import.meta.url);
      for (const name of readdirSync(dir).filter((f) => f.endsWith(".oxi"))) {
        const source = readFileSync(new URL(name, dir), "utf8");
        const r = await run(runner, source, { stdin: "Ada\n2\n3\n\n", test: name.includes("tests") });
        if (!isolated && /diverge/.test(source)) continue;
        assert.equal(r.err, "", `${name}: ${r.err}`);
        assert.equal(r.code, 0, name);
      }
    });
  });
}

describe("the editor's tools", () => {
  let tools;
  before(async () => {
    install({ wasm: SINGLE, threads: THREADS, isolated: false });
    tools = await (await import(new URL("runner.js?tools", assets))).tools();
  });

  test("tokens are UTF-16 ranges of the real lexer's classes", () => {
    const source = '// é\nmain { println("ü", 1) }';
    const flat = tools.tokens(source);
    const spans = [];
    for (let i = 0; i < flat.length; i += 3) spans.push([source.slice(flat[i], flat[i + 1]), flat[i + 2]]);
    assert.deepEqual(spans, [["// é", 4], ["println", 5], ['"ü"', 2], ["1", 3]]);
  });

  test("check, format and the stdlib", () => {
    assert.deepEqual(tools.check("main {\n    println(1)\n}\n"), []);
    assert.equal(tools.check("main {\n    x :=\n}\n")[0].severity, "error");
    assert.equal(tools.format("main {\nprintln(1)\n}").source, "main {\n    println(1)\n}\n");
    assert.ok(tools.stdlib().math.includes("fun "));
  });

  test("share links round-trip one file or several", async () => {
    const runner = await import(new URL("runner.js?tools", assets));
    const encoded = await runner.encodeSource("main {\n}\n");
    assert.equal(await runner.decodeSource(encoded), "main {\n}\n");
    const many = { "main.oxi": "a", "b.oxi": "b" };
    const url = await runner.playgroundUrl(many);
    assert.deepEqual(JSON.parse(await runner.decodeSource(new URLSearchParams(url.split("#")[1]).get("files"))), many);
  });
});

describe("the language server", { skip: !existsSync(LSP) && "build it with `imlazy wasm-lsp`" }, () => {
  let client;
  const uri = "file:///work/main.oxi";
  before(async () => {
    install({ wasm: SINGLE, threads: THREADS, isolated: true });
    const { start } = await import(new URL("lsp.js", assets));
    const files = {};
    for (const f of readdirSync(root + "stdlib")) if (f.endsWith(".oxi")) files[`/home/oxigen/.oxigen/lib/stdlib/${f}`] = readFileSync(root + "stdlib/" + f, "utf8");
    client = await start({ module: await WebAssembly.compile(readFileSync(LSP)), workerUrl: new URL("lsp-worker.js", assets).href, files, env: { HOME: "/home/oxigen", PWD: "/work" } });
    client.notify("textDocument/didOpen", { textDocument: { uri, languageId: "oxigen", version: 1, text: "introduce math\nfun double(x) { x * 2 }\nmain {\n    math.\n    dou\n}\n" } });
  });
  after(() => client?.stop());

  test("completes stdlib functions after a module name", async () => {
    const result = await client.request("textDocument/completion", { textDocument: { uri }, position: { line: 3, character: 9 } });
    const labels = (Array.isArray(result) ? result : result.items).map((i) => i.label);
    assert.ok(labels.includes("sqrt"), labels.join(", "));
  });

  test("completes the file's own functions", async () => {
    const result = await client.request("textDocument/completion", { textDocument: { uri }, position: { line: 4, character: 7 } });
    assert.ok((Array.isArray(result) ? result : result.items).some((i) => i.label === "double"));
  });

  test("hovers over a module", async () => {
    const hover = await client.request("textDocument/hover", { textDocument: { uri }, position: { line: 0, character: 12 } });
    assert.match(hover.contents.value, /math/);
  });
});

describe("the vendored editor", () => {
  // Every bare import playground.js makes must resolve, through the import
  // map the site writes, to a vendored file that exports the names it uses.
  test("playground.js's imports resolve to vendored exports", async () => {
    const source = readFileSync(new URL("playground.js", assets), "utf8");
    const imports = [...source.matchAll(/import\s*\{([^}]*)\}\s*from\s*"(@[^"]+)"/g)];
    assert.ok(imports.length >= 8);
    for (const [, names, specifier] of imports) {
      const file = specifier.slice(1).replace("/", "-") + ".js";
      const vendored = readFileSync(new URL(`vendor/${file}`, assets), "utf8");
      const exported = new Set([...vendored.matchAll(/export\s*\{([^}]*)\}/g)].flatMap((m) => m[1].split(",").map((n) => n.trim().split(/\s+as\s+/).pop())));
      for (const name of names.split(",").map((n) => n.trim()).filter(Boolean)) assert.ok(exported.has(name), `${specifier} does not export ${name}`);
      // And whatever that file imports is vendored too.
      for (const [, dep] of vendored.matchAll(/from\s*['"]([^'"]+)['"]/g)) {
        const depFile = (dep.startsWith("@") ? dep.slice(1).replace("/", "-") : dep) + ".js";
        assert.ok(existsSync(new URL(`vendor/${depFile}`, assets)), `${file} imports ${dep}, which is not vendored`);
      }
    }
  });
});
