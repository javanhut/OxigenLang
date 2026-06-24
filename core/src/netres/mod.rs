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
use std::sync::{LazyLock, Mutex};

enum Resource {
    Tcp(TcpStream),
    Listener(TcpListener),
    Udp(UdpSocket),
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
