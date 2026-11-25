//! Native network resources for the `net` stdlib.
//!
//! Backs the TCP/UDP socket builtins with a process-wide handle registry, and provides the streaming HTTP helpers (`download`/`upload`) that do not buffer whole bodies in memory.
//!
//! A handle is an opaque `u64` handed to Oxigen code; the real OS resource lives here. `close` releases it and is idempotent.
//! The registry is a global lock-protected table, so a handle works from any worker thread. Handle numbers are drawn from a keyed hash.
//!
//! Socket reads and writes operate on UTF-8 text (lossy on read).

use std::collections::HashMap;
use std::collections::hash_map::RandomState;
use std::fs::File;
use std::hash::{BuildHasher, Hasher};
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream, ToSocketAddrs, UdpSocket};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::mpsc::{Receiver, RecvTimeoutError};
use std::sync::{Arc, LazyLock, Mutex};
use std::time::Duration;

enum Resource {
    Tcp(TcpStream),
    Listener(TcpListener),
    Udp(UdpSocket),
    #[cfg_attr(target_arch = "wasm32", allow(dead_code))]
    HttpBody(Arc<Mutex<HttpStream>>),
}

/// The receiving end of a streaming HTTP body. A background thread owns the reader and pushes raw byte chunks into `rx`; `carry` holds bytes not yet returned, including the trailing bytes of a multi-byte UTF-8 char split across chunks, so `http_read` only returns whole codepoints.
struct HttpStream {
    rx: Receiver<Vec<u8>>,
    carry: Vec<u8>,
    done: bool,
}

/// How many 8 KiB chunks may sit unread before the reader thread blocks; see `spawn_body_reader`.
#[cfg(not(target_arch = "wasm32"))]
const BODY_QUEUE_CHUNKS: usize = 8;

/// The longest line `http_read_line` will collect.
const HTTP_MAX_LINE: usize = 8 * 1024 * 1024;

static REGISTRY: LazyLock<Mutex<HashMap<u64, Resource>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));
static NEXT_ID: AtomicU64 = AtomicU64::new(1);
static ID_KEY: LazyLock<RandomState> = LazyLock::new(RandomState::new);

/// Handles stay below 2^53 so they survive a round trip through a float.
const HANDLE_MASK: u64 = (1 << 53) - 1;

fn insert(res: Resource) -> u64 {
    let mut registry = REGISTRY.lock().unwrap();
    loop {
        let mut hasher = ID_KEY.build_hasher();
        hasher.write_u64(NEXT_ID.fetch_add(1, Ordering::Relaxed));
        let id = hasher.finish() & HANDLE_MASK;
        if id != 0 && !registry.contains_key(&id) {
            registry.insert(id, res);
            return id;
        }
    }
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

/// Validates a caller-supplied port before the `u16` cast, rejecting values outside the valid range.
///
/// Port 0 is accepted only when `allow_ephemeral` (the bind paths, `tcp_listen`/`udp_bind`), where it asks the OS to pick a free port; connecting or sending to port 0 is rejected.
fn port_u16(port: i64, allow_ephemeral: bool) -> Result<u16, String> {
    let low = if allow_ephemeral { 0 } else { 1 };
    if port < low || port > 65535 {
        return Err(format!("port {} out of range: must be {}-65535", port, low));
    }
    Ok(port as u16)
}

/// The timeout bounding a TCP connect handshake. `accept` and `receive` are not bounded by it, and name resolution is bounded only by the OS resolver.
const CONNECT_TIMEOUT: Duration = Duration::from_secs(30);

pub fn tcp_connect(host: &str, port: i64) -> Result<u64, String> {
    let port = port_u16(port, false)?;
    let addrs: Vec<_> = (host, port)
        .to_socket_addrs()
        .map_err(|e| e.to_string())?
        .collect();
    let mut last_err = None;
    for addr in &addrs {
        match TcpStream::connect_timeout(addr, CONNECT_TIMEOUT) {
            Ok(stream) => return Ok(insert(Resource::Tcp(stream))),
            Err(e) => last_err = Some(e),
        }
    }
    Err(match last_err {
        Some(e) => e.to_string(),
        None => format!("could not resolve {}:{}", host, port),
    })
}

pub fn tcp_listen(host: &str, port: i64) -> Result<u64, String> {
    let port = port_u16(port, true)?;
    let listener = TcpListener::bind((host, port)).map_err(|e| e.to_string())?;
    Ok(insert(Resource::Listener(listener)))
}

pub fn tcp_accept(id: u64) -> Result<u64, String> {
    let listener = match REGISTRY.lock().unwrap().get(&id) {
        Some(Resource::Listener(l)) => l.try_clone().map_err(|e| e.to_string())?,
        Some(_) => return Err(format!("handle {} is not a TCP server", id)),
        None => return Err(format!("invalid or closed handle {}", id)),
    };
    let (stream, _) = listener.accept().map_err(|e| e.to_string())?;
    Ok(insert(Resource::Tcp(stream)))
}

/// Bound how long a `tcp_receive` on this connection waits for data. `ms == 0` restores the default of waiting forever.
/// A timed-out read reports EOF (see `tcp_receive`).
pub fn tcp_set_read_timeout(id: u64, ms: i64) -> Result<(), String> {
    if ms < 0 {
        return Err("set_read_timeout: milliseconds must not be negative".to_string());
    }
    let timeout = (ms > 0).then(|| Duration::from_millis(ms as u64));
    with_tcp(id, |s| {
        s.set_read_timeout(timeout).map_err(|e| e.to_string())
    })
}

pub fn tcp_send(id: u64, data: &str) -> Result<i64, String> {
    with_tcp(id, |s| {
        s.write_all(data.as_bytes()).map_err(|e| e.to_string())?;
        s.flush().map_err(|e| e.to_string())?;
        Ok(data.len() as i64)
    })
}

/// The largest buffer one socket read uses; see `tcp_receive`.
const TCP_READ_CHUNK: usize = 64 * 1024;

/// Larger than any deliverable datagram; see `udp_receive`.
const UDP_MAX_DATAGRAM: usize = 64 * 1024;

/// Reads up to `max` bytes. Returns `""` on a clean EOF (peer closed), and also when a `tcp_set_read_timeout` deadline passes with nothing to read.
pub fn tcp_receive(id: u64, max: i64) -> Result<String, String> {
    if max <= 0 {
        return Err("receive: max bytes must be positive".to_string());
    }
    with_tcp(id, |s| {
        let mut buf = vec![0u8; (max as usize).min(TCP_READ_CHUNK)];
        let n = match s.read(&mut buf) {
            Ok(n) => n,
            Err(e)
                if matches!(
                    e.kind(),
                    std::io::ErrorKind::WouldBlock | std::io::ErrorKind::TimedOut
                ) =>
            {
                0
            }
            Err(e) => return Err(e.to_string()),
        };
        buf.truncate(n);
        Ok(String::from_utf8_lossy(&buf).into_owned())
    })
}

/// Writes the file at `path` to a TCP connection unchanged and returns how many bytes went out. Used to send binary files.
pub fn tcp_send_file(id: u64, path: &str) -> Result<i64, String> {
    let mut file = File::open(path).map_err(|e| format!("send_file: {}: {}", path, e))?;
    with_tcp(id, |s| {
        let n = std::io::copy(&mut file, s).map_err(|e| e.to_string())?;
        s.flush().map_err(|e| e.to_string())?;
        Ok(n as i64)
    })
}

pub fn udp_bind(host: &str, port: i64) -> Result<u64, String> {
    let port = port_u16(port, true)?;
    let sock = UdpSocket::bind((host, port)).map_err(|e| e.to_string())?;
    Ok(insert(Resource::Udp(sock)))
}

pub fn udp_send(id: u64, data: &str, host: &str, port: i64) -> Result<i64, String> {
    let port = port_u16(port, false)?;
    with_udp(id, |s| {
        s.send_to(data.as_bytes(), (host, port))
            .map(|n| n as i64)
            .map_err(|e| e.to_string())
    })
}

/// Reads one datagram of up to `max` bytes. Returns `(data, sender_address)`.
///
/// A datagram larger than `max` is an error, not a short read.
pub fn udp_receive(id: u64, max: i64) -> Result<(String, String), String> {
    if max <= 0 {
        return Err("udp_receive: max bytes must be positive".to_string());
    }
    with_udp(id, |s| {
        let want = max as usize;
        let mut buf = vec![0u8; want.saturating_add(1).min(UDP_MAX_DATAGRAM)];
        let (n, addr) = s.recv_from(&mut buf).map_err(|e| e.to_string())?;
        if n > want {
            return Err(format!(
                "udp_receive: datagram is larger than max ({} bytes); \
                 the rest was discarded by the OS and cannot be recovered — \
                 retry with a larger max (up to {})",
                want, UDP_MAX_DATAGRAM
            ));
        }
        buf.truncate(n);
        Ok((String::from_utf8_lossy(&buf).into_owned(), addr.to_string()))
    })
}

/// Closes a handle and frees its slot. Closing an unknown or already-closed handle is a no-op.
pub fn close(id: u64) {
    REGISTRY.lock().unwrap().remove(&id);
}

/// Why HTTP fails in a browser build: that build has no HTTP client.
pub const NO_HTTP: &str = "http is not available in this build of Oxigen (WebAssembly)";

#[cfg(target_arch = "wasm32")]
pub fn http_download(
    _: &str,
    _: &str,
    _: &str,
    _: &[(String, String)],
    _: Option<&str>,
) -> Result<i64, String> {
    Err(NO_HTTP.into())
}

#[cfg(target_arch = "wasm32")]
pub fn http_open(_: &str, _: &str, _: &[(String, String)], _: Option<&str>) -> Result<u64, String> {
    Err(NO_HTTP.into())
}

#[cfg(target_arch = "wasm32")]
pub fn http_upload(
    _: &str,
    _: &str,
    _: &str,
    _: Vec<(String, String)>,
) -> Result<(i64, String), String> {
    Err(NO_HTTP.into())
}

/// Streams a response body to `path` without buffering it in memory. `method` and `body` work as in `http_open`. Returns the HTTP status code.
#[cfg(not(target_arch = "wasm32"))]
pub fn http_download(
    url: &str,
    path: &str,
    method: &str,
    headers: &[(String, String)],
    body: Option<&str>,
) -> Result<i64, String> {
    let resp = send_request(method, url, headers, body, "download")?;
    let status = resp.status().as_u16();
    let (_, body) = resp.into_parts();
    let mut reader = body.into_reader();
    let mut file = File::create(path).map_err(|e| e.to_string())?;
    std::io::copy(&mut reader, &mut file).map_err(|e| e.to_string())?;
    Ok(status as i64)
}

/// Opens a streaming HTTP request and stashes the response body behind a handle.
/// Read it line by line with `http_read_line` or byte-wise with `http_read`; `close` frees it. The body is never buffered whole in memory.
///
/// `method` is GET/POST/PUT/PATCH; `body` is sent for the body methods and ignored for GET.
#[cfg(not(target_arch = "wasm32"))]
pub fn http_open(
    method: &str,
    url: &str,
    headers: &[(String, String)],
    body: Option<&str>,
) -> Result<u64, String> {
    let resp = send_request(method, url, headers, body, "open_stream")?;
    let (_, resp_body) = resp.into_parts();
    Ok(spawn_body_reader(Box::new(resp_body.into_reader())))
}

/// Sends one request: GET, or POST/PUT/PATCH with `body`. `what` names the
/// caller in the unsupported-method error.
#[cfg(not(target_arch = "wasm32"))]
fn send_request(
    method: &str,
    url: &str,
    headers: &[(String, String)],
    body: Option<&str>,
    what: &str,
) -> Result<ureq::http::Response<ureq::Body>, String> {
    match method.to_uppercase().as_str() {
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
        m => return Err(format!("{what}: unsupported method '{m}'")),
    }
    .map_err(|e| format!("http error: {}", e))
}

/// Drains a response body on a background thread into a bounded channel so reads can be polled with a timeout. The thread ends at EOF, on error, or when the consumer drops the stream (`close`).
///
/// The channel holds at most `(BODY_QUEUE_CHUNKS + 1) * 8 KiB` per open stream; a full channel blocks the reader.
#[cfg(not(target_arch = "wasm32"))]
fn spawn_body_reader(mut reader: Box<dyn Read + Send>) -> u64 {
    let (tx, rx) = std::sync::mpsc::sync_channel::<Vec<u8>>(BODY_QUEUE_CHUNKS);
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

/// Reads the next newline-delimited line from a stream, without the trailing newline.
///
/// `timeout_ms == 0` blocks until a line or EOF; `> 0` returns `Ok(None)` if no full line arrived in time. Returns `Ok(Some(""))` at end of stream. The line is always valid UTF-8.
pub fn http_read_line(id: u64, timeout_ms: i64) -> Result<Option<String>, String> {
    let stream = match REGISTRY.lock().unwrap().get(&id) {
        Some(Resource::HttpBody(s)) => Arc::clone(s),
        Some(_) => return Err(format!("handle {} is not an HTTP stream", id)),
        None => return Err(format!("invalid or closed handle {}", id)),
    };
    let mut st = stream.lock().unwrap();

    let mut scanned = 0;
    loop {
        if let Some(off) = st.carry[scanned..].iter().position(|&b| b == b'\n') {
            let pos = scanned + off;
            let line: Vec<u8> = st.carry.drain(..=pos).collect();
            let text = String::from_utf8_lossy(&line[..line.len() - 1]);
            return Ok(Some(text.trim_end_matches('\r').to_string()));
        }
        scanned = st.carry.len();
        if st.carry.len() > HTTP_MAX_LINE {
            st.carry.clear();
            return Err(format!(
                "read_line: line longer than {} bytes; read it with read_chunk instead",
                HTTP_MAX_LINE
            ));
        }
        if st.done {
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
/// `timeout_ms == 0` blocks until data or EOF. `timeout_ms > 0` waits at most that long. Returns:
///   - `Ok(Some(text))` — `text` is "" only at end of stream
///   - `Ok(None)`       — timed out with no data yet
///
/// Only whole characters are returned.
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
        return Ok(Some(String::new()));
    }

    let cap = (max as usize).min(st.carry.len());
    let end = match std::str::from_utf8(&st.carry[..cap]) {
        Ok(_) => cap,
        Err(e) => e.valid_up_to(),
    };
    if end == 0 {
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
#[cfg(not(target_arch = "wasm32"))]
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
    let body_str = body
        .read_to_string()
        .map_err(|e| format!("http error: reading response body: {}", e))?;
    Ok((status as i64, body_str))
}
