//! What a hostile client could do to a server built on the `api` module, and
//! what the `strings`, `env` and `parse_args` modules got wrong.
//!
//! Server side, each probe is one connection to a real server process:
//!
//! - A few million `[` in a JSON body overflowed a worker's stack and killed
//!   the process.
//! - `redirect`/`respond` copied `\r\n` from a query parameter into a header,
//!   so a client could inject headers and a body (response splitting).
//! - The idle timeout was per read, so a client sending one header byte at a
//!   time held its worker forever; a few such clients took the server down.
//! - `api.file` read files as text: a binary file, or a directory, got no
//!   response at all.
//! - The `app.get("/*", ...)` example in the module's own docs never matched.
//! - `Content-Length: 1e3` was read as 1000, a body shorter than its
//!   Content-Length reached the handler truncated, and `Transfer-Encoding:
//!   chunked` was ignored rather than refused.
//! - `serve` listened on every interface by default.

use std::fs;
use std::io::{BufRead, BufReader, Read, Write};
use std::net::{Shutdown, TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

fn oxigen_bin() -> &'static str {
    env!("CARGO_BIN_EXE_oxigen")
}

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf()
}

fn temp_dir(label: &str) -> PathBuf {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("oxigen-{label}-{unique}"));
    fs::create_dir_all(&dir).unwrap();
    dir
}

/// Kills the server when the test ends, pass or fail.
struct Server(Child);

impl Drop for Server {
    fn drop(&mut self) {
        let _ = self.0.kill();
        let _ = self.0.wait();
    }
}

fn free_port() -> u16 {
    TcpListener::bind("127.0.0.1:0")
        .unwrap()
        .local_addr()
        .unwrap()
        .port()
}

const PNG: &[u8] = &[0x89, b'P', b'N', b'G', 0x0d, 0x0a, 0x1a, 0x0a, 0x00, 0xff, 0xfe, 0x80, b'x'];

/// Starts the test server over `root` and returns it with the first line it
/// printed. The server leaves `host` at its default.
fn start(root: &Path, port: u16) -> (Server, String) {
    fs::write(root.join("img.png"), PNG).unwrap();
    fs::write(root.join("index.html"), "<h1>home</h1>").unwrap();
    fs::create_dir_all(root.join("sub")).unwrap();
    let root_s = root.display();
    let script = root.join("server.oxi");
    fs::write(
        &script,
        format!(
            r#"introduce api

fun bounce(req) {{ api.redirect(req.query["next"]) }}
fun named(req) {{ api.respond(200, "ok", {{"X-Name": req.query["q"]}}) }}
fun echo(req) {{ api.json(api.body_json(req)) }}
fun body_len(req) {{ api.text("{{len(req.body)}}") }}
fun png(req) {{ api.file("{root_s}/img.png") }}
fun site(req) {{ api.static_dir(req, "{root_s}") }}

fun routes(cfg <map>) {{
    app := api.app(cfg)
    app.get("/go", bounce)
    app.get("/name", named)
    app.post("/echo", echo)
    app.post("/len", body_len)
    app.get("/img", png)
    app.get("/*", site)
    app.listen()
}}

main {{ api.serve(routes, port={port}, workers=1, header_timeout=1000) }}
"#
        ),
    )
    .unwrap();
    let mut child = Command::new(oxigen_bin())
        .current_dir(workspace_root())
        .arg(&script)
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .unwrap();
    let mut first = String::new();
    let mut stdout = BufReader::new(child.stdout.take().unwrap());
    stdout.read_line(&mut first).unwrap();
    // Keep draining: the server logs failed requests to stdout, and a closed
    // pipe would kill it on the first log line.
    std::thread::spawn(move || std::io::copy(&mut stdout, &mut std::io::sink()));
    let server = Server(child);
    let deadline = Instant::now() + Duration::from_secs(10);
    while TcpStream::connect(("127.0.0.1", port)).is_err() {
        assert!(Instant::now() < deadline, "server never listened: {first}");
        std::thread::sleep(Duration::from_millis(50));
    }
    (server, first)
}

fn connect(port: u16) -> TcpStream {
    let s = TcpStream::connect(("127.0.0.1", port)).unwrap();
    s.set_read_timeout(Some(Duration::from_secs(15))).unwrap();
    s
}

/// Sends `request` raw and returns the whole response (the server closes).
fn exchange(port: u16, request: &[u8]) -> Vec<u8> {
    let mut s = connect(port);
    s.write_all(request).unwrap();
    let mut out = Vec::new();
    let _ = s.read_to_end(&mut out);
    out
}

fn get(port: u16, target: &str) -> Vec<u8> {
    exchange(
        port,
        format!("GET {target} HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n").as_bytes(),
    )
}

fn head_of(resp: &[u8]) -> String {
    let end = resp
        .windows(4)
        .position(|w| w == b"\r\n\r\n")
        .unwrap_or(resp.len());
    String::from_utf8_lossy(&resp[..end]).into_owned()
}

fn body_of(resp: &[u8]) -> &[u8] {
    match resp.windows(4).position(|w| w == b"\r\n\r\n") {
        Some(end) => &resp[end + 4..],
        None => &[],
    }
}

fn status_of(resp: &[u8]) -> String {
    head_of(resp)
        .split(' ')
        .nth(1)
        .unwrap_or("<none>")
        .to_string()
}

#[test]
fn the_api_server_survives_hostile_clients() {
    let root = temp_dir("api-hardening");
    let port = free_port();
    let (_server, banner) = start(&root, port);

    // serve defaults to loopback only.
    assert!(banner.contains("127.0.0.1"), "banner: {banner}");

    // Header injection: CR/LF from a query parameter must not end the header.
    let resp = get(port, "/go?next=/home%0d%0aSet-Cookie:%20pwned=1%0d%0a%0d%0a<script>");
    let head = head_of(&resp);
    assert_eq!(status_of(&resp), "302", "head: {head}");
    assert!(!head.contains("\r\nSet-Cookie"), "injected header in: {head:?}");
    assert!(body_of(&resp).is_empty(), "injected body: {:?}", String::from_utf8_lossy(&resp));
    let resp = get(port, "/name?q=a%0d%0aX-Evil:%201");
    assert!(!head_of(&resp).contains("\r\nX-Evil"), "injected header in: {:?}", head_of(&resp));

    // A JSON nesting bomb is refused, and the worker lives on.
    let bomb = "[".repeat(4_000_000);
    let resp = exchange(
        port,
        format!(
            "POST /echo HTTP/1.1\r\nHost: x\r\nConnection: close\r\nContent-Length: {}\r\n\r\n{bomb}",
            bomb.len()
        )
        .as_bytes(),
    );
    let status = status_of(&resp);
    assert!(status == "400" || status == "500", "bomb answered {status}");
    let resp = exchange(
        port,
        b"POST /echo HTTP/1.1\r\nHost: x\r\nConnection: close\r\nContent-Length: 7\r\n\r\n{\"a\":1}",
    );
    assert_eq!(body_of(&resp), b"{\"a\":1}", "server did not survive the bomb");

    // Binary files are served byte for byte.
    let resp = get(port, "/img");
    assert_eq!(status_of(&resp), "200", "head: {}", head_of(&resp));
    assert!(head_of(&resp).contains(&format!("Content-Length: {}", PNG.len())), "head: {}", head_of(&resp));
    assert!(head_of(&resp).contains("image/png"), "head: {}", head_of(&resp));
    assert_eq!(body_of(&resp), PNG);

    // The wildcard route from the module's docs, and a directory under it.
    let resp = get(port, "/index.html");
    assert_eq!(status_of(&resp), "200", "head: {}", head_of(&resp));
    assert_eq!(body_of(&resp), b"<h1>home</h1>");
    assert_eq!(status_of(&get(port, "/sub")), "404");
    assert_eq!(status_of(&get(port, "/")), "200");

    // Content-Length must be plain digits.
    let resp = exchange(
        port,
        b"POST /len HTTP/1.1\r\nHost: x\r\nConnection: close\r\nContent-Length: 1e3\r\n\r\nabc",
    );
    assert_eq!(status_of(&resp), "400");

    // A body that ends before its Content-Length is refused, not truncated.
    let mut s = connect(port);
    s.write_all(b"POST /len HTTP/1.1\r\nHost: x\r\nConnection: close\r\nContent-Length: 10\r\n\r\nabc")
        .unwrap();
    s.shutdown(Shutdown::Write).unwrap();
    let mut resp = Vec::new();
    let _ = s.read_to_end(&mut resp);
    assert_eq!(status_of(&resp), "400", "short body answered: {:?}", String::from_utf8_lossy(&resp));

    // Chunked bodies are not understood, so they are refused.
    let resp = exchange(
        port,
        b"POST /len HTTP/1.1\r\nHost: x\r\nConnection: close\r\nTransfer-Encoding: chunked\r\n\r\n3\r\nabc\r\n0\r\n\r\n",
    );
    assert_eq!(status_of(&resp), "501", "chunked answered: {:?}", head_of(&resp));

    // Slowloris: one header byte every 200ms keeps every read alive, but the
    // request as a whole must not outlive header_timeout (1s here).
    let mut slow = connect(port);
    let started = Instant::now();
    for b in b"GET /index.html HTTP/1.1\r\nHost: x\r\n".iter().cycle() {
        if slow.write_all(&[*b]).is_err() || started.elapsed() > Duration::from_secs(4) {
            break;
        }
        std::thread::sleep(Duration::from_millis(200));
    }
    let _ = slow.write_all(b"\r\n\r\n");
    let mut resp = Vec::new();
    let _ = slow.read_to_end(&mut resp);
    assert_ne!(status_of(&resp), "200", "a 4s trickle was served");
    // ...and the single worker is free again.
    assert_eq!(status_of(&get(port, "/index.html")), "200");

    let _ = fs::remove_dir_all(&root);
}

// ── stdlib modules driven as scripts ────────────────────────────────────────

fn run_script(dir: &Path, source: &str, args: &[&str]) -> (String, String, bool) {
    let script = dir.join("script.oxi");
    fs::write(&script, source).unwrap();
    let out = Command::new(oxigen_bin())
        .current_dir(workspace_root())
        .arg(&script)
        .args(args)
        .output()
        .unwrap();
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.success(),
    )
}

#[test]
fn env_loads_a_named_file_without_printing_it() {
    let dir = temp_dir("env-file");
    let file = dir.join("secrets.env");
    fs::write(
        &file,
        "# comment\r\nAPI_KEY=sk-secret=abc==\r\nDB=postgres://h/db?sslmode=require\r\n\r\n",
    )
    .unwrap();
    let (out, err, ok) = run_script(
        &dir,
        &format!(
            "introduce env\nenv.discover_dot_file(\"{}\")\nprintln(env.get_env(\"API_KEY\"))\nprintln(env.get_env(\"DB\"))\n",
            file.display()
        ),
        &[],
    );
    assert!(ok, "failed: {err}");
    assert_eq!(out, "sk-secret=abc==\npostgres://h/db?sslmode=require\n");
    let _ = fs::remove_dir_all(&dir);
}

const FLAGS: &str = "introduce {Parser} from parse_args\n\
main {\n\
    p <Parser>\n\
    p.define_flag(verbose_name=\"url\", alias=\"u\")\n\
    p.define_flag(verbose_name=\"offset\")\n\
    p.handle_args()\n\
    println(p.args_map.url)\n\
    println(p.args_map.offset)\n\
}\n";

#[test]
fn parse_args_keeps_everything_after_the_first_equals_sign() {
    let dir = temp_dir("parse-args-eq");
    let (out, err, ok) = run_script(&dir, FLAGS, &["--url=http://x/?a=b&c=d", "--offset", "-5"]);
    assert!(ok, "failed: {err}");
    assert_eq!(out, "http://x/?a=b&c=d\n-5\n");
    let _ = fs::remove_dir_all(&dir);
}

#[test]
fn parse_args_fills_the_long_name_from_its_alias() {
    let dir = temp_dir("parse-args-alias");
    let (out, err, ok) = run_script(&dir, FLAGS, &["-u", "http://h"]);
    assert!(ok, "failed: {err}");
    assert_eq!(out, "http://h\n\n");
    let _ = fs::remove_dir_all(&dir);
}
