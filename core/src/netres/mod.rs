//! Native network resources for the `net` stdlib.
//!
//! Backs the TCP/UDP socket builtins with a thread-local handle registry, and
//! provides the streaming HTTP helpers (`download`/`upload`) that avoid
//! buffering whole bodies in memory.
//!
//! A *handle* is an opaque `u64` "ticket number" handed to Oxigen code; the
//! real OS resource lives here. `close` returns the ticket and is idempotent.
//! The registry is a global lock-protected table, so a handle works from any
//! worker thread (lets an accepted socket be handed to a `spawn`ed task).
//!
//! Socket reads/writes operate on UTF-8 text (lossy on read). This covers the
//! common case (HTTP, line protocols, JSON over TCP); binary-safe byte I/O is
//! intentionally deferred until a concrete need exists.

use std::collections::HashMap;
use std::fs::File;
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream, UdpSocket};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::mpsc::{Receiver, RecvTimeoutError};
use std::sync::{Arc, LazyLock, Mutex};
use std::time::Duration;

enum Resource {
    Tcp(TcpStream),
    Listener(TcpListener),
    Udp(UdpSocket),
    // A streaming HTTP response body, drained on a background thread into a
    // channel so reads can be polled with a timeout (a live spinner) instead of
    // blocking the interpreter. Behind its own Arc<Mutex<…>> so the registry
    // lock is released before the (possibly slow) channel wait.
    HttpBody(Arc<Mutex<HttpStream>>),
}

/// The receiving end of a streaming HTTP body. A background thread owns the
/// reader and pushes raw byte chunks into `rx`; `carry` holds bytes not yet
/// returned, including the trailing bytes of a multi-byte UTF-8 char that
/// landed on a chunk boundary (so `http_read` only ever returns whole
/// codepoints — critical for token streams with emoji/accents/CJK).
struct HttpStream {
    rx: Receiver<Vec<u8>>,
    carry: Vec<u8>,
    done: bool,
}

// ponytail: one global lock on the socket table so ids cross threads (enables
// spawn handle(conn)). I/O runs on a cloned fd *outside* the lock, so a blocking
// recv never stalls other sockets. Shard the map only if the lock ever contends.
static REGISTRY: LazyLock<Mutex<HashMap<u64, Resource>>> = LazyLock::new(|| Mutex::new(HashMap::new()));
static NEXT_ID: AtomicU64 = AtomicU64::new(1);

fn insert(res: Resource) -> u64 {
    let id = NEXT_ID.fetch_add(1, Ordering::Relaxed);
    REGISTRY.lock().unwrap().insert(id, res);
    id
}

fn with_tcp<T>(id: u64, f: impl FnOnce(&mut TcpStream) -> Result<T, String>) -> Result<T, String> {
    let mut s = match REGISTRY.lock().unwrap().get(&id) {
        Some(Resource::Tcp(s)) => s.try_clone().map_err(|e| e.to_string())?,
        Some(_) => return Err(format!("handle {} is not a TCP connection", id)),
        None => return Err(format!("invalid or closed handle {}", id)),
    };
    f(&mut s)
}

fn with_udp<T>(id: u64, f: impl FnOnce(&UdpSocket) -> Result<T, String>) -> Result<T, String> {
    let s = match REGISTRY.lock().unwrap().get(&id) {
        Some(Resource::Udp(s)) => s.try_clone().map_err(|e| e.to_string())?,
        Some(_) => return Err(format!("handle {} is not a UDP socket", id)),
        None => return Err(format!("invalid or closed handle {}", id)),
    };
    f(&s)
}

// ── TCP ─────────────────────────────────────────────────────────────────

pub fn tcp_connect(host: &str, port: i64) -> Result<u64, String> {
    let stream = TcpStream::connect((host, port as u16)).map_err(|e| e.to_string())?;
    Ok(insert(Resource::Tcp(stream)))
}

pub fn tcp_listen(host: &str, port: i64) -> Result<u64, String> {
    let listener = TcpListener::bind((host, port as u16)).map_err(|e| e.to_string())?;
    Ok(insert(Resource::Listener(listener)))
}

pub fn tcp_accept(id: u64) -> Result<u64, String> {
    // Clone the listener so the registry borrow is released before the
    // (potentially long) blocking accept.
    let listener = match REGISTRY.lock().unwrap().get(&id) {
        Some(Resource::Listener(l)) => l.try_clone().map_err(|e| e.to_string())?,
        Some(_) => return Err(format!("handle {} is not a TCP server", id)),
        None => return Err(format!("invalid or closed handle {}", id)),
    };
    let (stream, _) = listener.accept().map_err(|e| e.to_string())?;
    Ok(insert(Resource::Tcp(stream)))
}

pub fn tcp_send(id: u64, data: &str) -> Result<i64, String> {
    with_tcp(id, |s| {
        s.write_all(data.as_bytes()).map_err(|e| e.to_string())?;
        s.flush().map_err(|e| e.to_string())?;
        Ok(data.len() as i64)
    })
}

/// Reads up to `max` bytes. Returns `""` on a clean EOF (peer closed).
pub fn tcp_receive(id: u64, max: i64) -> Result<String, String> {
    if max <= 0 {
        return Err("receive: max bytes must be positive".to_string());
    }
    with_tcp(id, |s| {
        let mut buf = vec![0u8; max as usize];
        let n = s.read(&mut buf).map_err(|e| e.to_string())?;
        buf.truncate(n);
        Ok(String::from_utf8_lossy(&buf).into_owned())
    })
}

// ── UDP ─────────────────────────────────────────────────────────────────

pub fn udp_bind(host: &str, port: i64) -> Result<u64, String> {
    let sock = UdpSocket::bind((host, port as u16)).map_err(|e| e.to_string())?;
    Ok(insert(Resource::Udp(sock)))
}

pub fn udp_send(id: u64, data: &str, host: &str, port: i64) -> Result<i64, String> {
    with_udp(id, |s| {
        s.send_to(data.as_bytes(), (host, port as u16))
            .map(|n| n as i64)
            .map_err(|e| e.to_string())
    })
}

/// Reads up to `max` bytes. Returns `(data, sender_address)`.
pub fn udp_receive(id: u64, max: i64) -> Result<(String, String), String> {
    if max <= 0 {
        return Err("udp_receive: max bytes must be positive".to_string());
    }
    with_udp(id, |s| {
        let mut buf = vec![0u8; max as usize];
        let (n, addr) = s.recv_from(&mut buf).map_err(|e| e.to_string())?;
        buf.truncate(n);
        Ok((String::from_utf8_lossy(&buf).into_owned(), addr.to_string()))
    })
}

// ── Lifecycle ─────────────────────────────────────────────────────────────

/// Closes a handle and frees its slot. Idempotent — closing an unknown or
/// already-closed handle is a no-op.
pub fn close(id: u64) {
    REGISTRY.lock().unwrap().remove(&id);
}

// ── Streaming HTTP ────────────────────────────────────────────────────────

/// Streams a GET response body to `path` without buffering it in memory.
/// Returns the HTTP status code.
pub fn http_download(url: &str, path: &str) -> Result<i64, String> {
    let resp = ureq::get(url)
        .call()
        .map_err(|e| format!("http error: {}", e))?;
    let status = resp.status().as_u16();
    let (_, body) = resp.into_parts();
    let mut reader = body.into_reader();
    let mut file = File::create(path).map_err(|e| e.to_string())?;
    std::io::copy(&mut reader, &mut file).map_err(|e| e.to_string())?;
    Ok(status as i64)
}

/// Opens a streaming HTTP request and stashes the response body behind a handle.
/// Read it line by line with `http_read_line` (the easy path) or byte-wise with
/// `http_read`; `close` frees it. The body is never buffered whole in memory.
///
/// `method` is GET/POST/PUT/PATCH; `body` is sent for the body methods and
/// ignored for GET. This is what lets you stream a real LLM API — POST the
/// model+prompt and read the tokens back.
pub fn http_open(
    method: &str,
    url: &str,
    headers: &[(String, String)],
    body: Option<&str>,
) -> Result<u64, String> {
    let resp = match method.to_uppercase().as_str() {
        "GET" => {
            let mut req = ureq::get(url);
            for (k, v) in headers {
                req = req.header(k.as_str(), v.as_str());
            }
            req.call()
        }
        m @ ("POST" | "PUT" | "PATCH") => {
            let mut req = match m {
                "POST" => ureq::post(url),
                "PUT" => ureq::put(url),
                _ => ureq::patch(url),
            };
            for (k, v) in headers {
                req = req.header(k.as_str(), v.as_str());
            }
            match body {
                Some(b) => req.send(b.as_bytes()),
                None => req.send_empty(),
            }
        }
        m => return Err(format!("open_stream: unsupported method '{}'", m)),
    }
    .map_err(|e| format!("http error: {}", e))?;

    let (_, resp_body) = resp.into_parts();
    Ok(spawn_body_reader(Box::new(resp_body.into_reader())))
}

/// Drains a response body on a background thread into a channel so reads can be
/// polled with a timeout instead of blocking the interpreter. The thread ends at
/// EOF, on error, or when the consumer drops the stream (`close`). ponytail: a
/// thread blocked in `read` at close lingers until the next byte/EOF — fine for
/// bodies that terminate; revisit if long idle streams pile up.
fn spawn_body_reader(mut reader: Box<dyn Read + Send>) -> u64 {
    let (tx, rx) = std::sync::mpsc::channel::<Vec<u8>>();
    std::thread::spawn(move || {
        let mut buf = [0u8; 8192];
        loop {
            match reader.read(&mut buf) {
                Ok(0) => break,
                Ok(n) => {
                    if tx.send(buf[..n].to_vec()).is_err() {
                        break;
                    }
                }
                Err(_) => break,
            }
        }
    });
    insert(Resource::HttpBody(Arc::new(Mutex::new(HttpStream {
        rx,
        carry: Vec::new(),
        done: false,
    }))))
}

/// Reads the next newline-delimited line from a stream, without the trailing
/// newline — the easy way to consume NDJSON/SSE token streams (each call returns
/// one complete line, so the caller just parses it).
///
/// `timeout_ms == 0` blocks until a line or EOF; `> 0` returns `Ok(None)` if no
/// full line arrived in time. Returns `Ok(Some(""))` at end of stream (NDJSON
/// has no blank lines, so "" reliably means EOF there). A line never splits a
/// codepoint — `\n` is one byte — so it is always returned as valid UTF-8.
pub fn http_read_line(id: u64, timeout_ms: i64) -> Result<Option<String>, String> {
    let stream = match REGISTRY.lock().unwrap().get(&id) {
        Some(Resource::HttpBody(s)) => Arc::clone(s),
        Some(_) => return Err(format!("handle {} is not an HTTP stream", id)),
        None => return Err(format!("invalid or closed handle {}", id)),
    };
    let mut st = stream.lock().unwrap();

    loop {
        if let Some(pos) = st.carry.iter().position(|&b| b == b'\n') {
            let line: Vec<u8> = st.carry.drain(..=pos).collect();
            let text = String::from_utf8_lossy(&line[..line.len() - 1]);
            return Ok(Some(text.trim_end_matches('\r').to_string())); // tolerate CRLF
        }
        if st.done {
            // No newline left: flush any remaining bytes as the last line, "" = EOF.
            let out = String::from_utf8_lossy(&st.carry).into_owned();
            st.carry.clear();
            return Ok(Some(out));
        }
        let next = if timeout_ms > 0 {
            match st.rx.recv_timeout(Duration::from_millis(timeout_ms as u64)) {
                Ok(c) => Some(c),
                Err(RecvTimeoutError::Timeout) => return Ok(None),
                Err(RecvTimeoutError::Disconnected) => None,
            }
        } else {
            st.rx.recv().ok()
        };
        match next {
            Some(chunk) => st.carry.extend_from_slice(&chunk),
            None => st.done = true,
        }
    }
}

/// Reads up to `max` bytes of valid UTF-8 from an HTTP stream handle.
///
/// `timeout_ms == 0` blocks until data or EOF. `timeout_ms > 0` waits at most
/// that long and returns `Ok(None)` if nothing arrived yet (so a caller can
/// animate a spinner and retry). Returns:
///   - `Ok(Some(text))` — `text` is "" only at end of stream
///   - `Ok(None)`       — timed out with no data yet
///
/// A multi-byte codepoint can straddle a chunk boundary; we hold the incomplete
/// trailing bytes in `carry` and only ever return whole characters.
pub fn http_read(id: u64, max: i64, timeout_ms: i64) -> Result<Option<String>, String> {
    if max <= 0 {
        return Err("read_chunk: max bytes must be positive".to_string());
    }
    let stream = match REGISTRY.lock().unwrap().get(&id) {
        Some(Resource::HttpBody(s)) => Arc::clone(s),
        Some(_) => return Err(format!("handle {} is not an HTTP stream", id)),
        None => return Err(format!("invalid or closed handle {}", id)),
    };
    let mut st = stream.lock().unwrap();

    // Pull from the channel until we hold at least one whole char, or hit EOF.
    while !has_whole_char(&st.carry) && !st.done {
        let next = if timeout_ms > 0 {
            match st.rx.recv_timeout(Duration::from_millis(timeout_ms as u64)) {
                Ok(c) => Some(c),
                Err(RecvTimeoutError::Timeout) => return Ok(None),
                Err(RecvTimeoutError::Disconnected) => None,
            }
        } else {
            st.rx.recv().ok()
        };
        match next {
            Some(chunk) => st.carry.extend_from_slice(&chunk),
            None => st.done = true,
        }
    }

    if st.carry.is_empty() {
        return Ok(Some(String::new())); // EOF, nothing buffered
    }

    // Return up to `max` bytes without splitting a multi-byte char.
    let cap = (max as usize).min(st.carry.len());
    let end = match std::str::from_utf8(&st.carry[..cap]) {
        Ok(_) => cap,
        Err(e) => e.valid_up_to(),
    };
    if end == 0 {
        // The next char is wider than `max`, or only a truncated tail remains at
        // EOF. Emit it (lossily for the truncated case) so we always progress.
        let out = String::from_utf8_lossy(&st.carry).into_owned();
        st.carry.clear();
        return Ok(Some(out));
    }
    let out = String::from_utf8_lossy(&st.carry[..end]).into_owned();
    st.carry.drain(..end);
    Ok(Some(out))
}

/// Whether `buf` holds at least one complete UTF-8 char ready to return.
fn has_whole_char(buf: &[u8]) -> bool {
    match std::str::from_utf8(buf) {
        Ok(s) => !s.is_empty(),
        Err(e) => e.valid_up_to() > 0,
    }
}

/// Streams the file at `file_path` as the request body without buffering it.
/// Returns `(status, response_body)`.
pub fn http_upload(
    method: &str,
    url: &str,
    file_path: &str,
    headers: Vec<(String, String)>,
) -> Result<(i64, String), String> {
    let file = File::open(file_path).map_err(|e| e.to_string())?;
    let mut req = match method.to_uppercase().as_str() {
        "POST" => ureq::post(url),
        "PUT" => ureq::put(url),
        "PATCH" => ureq::patch(url),
        m => return Err(format!("upload: unsupported method '{}'", m)),
    };
    for (k, v) in &headers {
        req = req.header(k.as_str(), v.as_str());
    }
    let resp = req.send(file).map_err(|e| format!("http error: {}", e))?;
    let status = resp.status().as_u16();
    let (_, mut body) = resp.into_parts();
    let body_str = body.read_to_string().unwrap_or_default();
    Ok((status as i64, body_str))
}
