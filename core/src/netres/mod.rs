//! Native network resources for the `net` stdlib.
//!
//! Backs the TCP/UDP socket builtins with a thread-local handle registry, and
//! provides the streaming HTTP helpers (`download`/`upload`) that avoid
//! buffering whole bodies in memory.
//!
//! A *handle* is an opaque `u64` "ticket number" handed to Oxigen code; the
//! real OS resource lives here. `close` returns the ticket and is idempotent.
//! The registry is thread-local — Oxigen runs a program on a single thread, so
//! a handle is only ever used from the thread that created it.
//!
//! Socket reads/writes operate on UTF-8 text (lossy on read). This covers the
//! common case (HTTP, line protocols, JSON over TCP); binary-safe byte I/O is
//! intentionally deferred until a concrete need exists.

use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::fs::File;
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream, UdpSocket};

enum Resource {
    Tcp(TcpStream),
    Listener(TcpListener),
    Udp(UdpSocket),
}

thread_local! {
    static REGISTRY: RefCell<HashMap<u64, Resource>> = RefCell::new(HashMap::new());
    static NEXT_ID: Cell<u64> = const { Cell::new(1) };
}

fn insert(res: Resource) -> u64 {
    let id = NEXT_ID.with(|n| {
        let id = n.get();
        n.set(id + 1);
        id
    });
    REGISTRY.with(|r| r.borrow_mut().insert(id, res));
    id
}

fn with_tcp<T>(id: u64, f: impl FnOnce(&mut TcpStream) -> Result<T, String>) -> Result<T, String> {
    REGISTRY.with(|r| match r.borrow_mut().get_mut(&id) {
        Some(Resource::Tcp(s)) => f(s),
        Some(_) => Err(format!("handle {} is not a TCP connection", id)),
        None => Err(format!("invalid or closed handle {}", id)),
    })
}

fn with_udp<T>(id: u64, f: impl FnOnce(&UdpSocket) -> Result<T, String>) -> Result<T, String> {
    REGISTRY.with(|r| match r.borrow().get(&id) {
        Some(Resource::Udp(s)) => f(s),
        Some(_) => Err(format!("handle {} is not a UDP socket", id)),
        None => Err(format!("invalid or closed handle {}", id)),
    })
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
    let listener = REGISTRY.with(|r| match r.borrow().get(&id) {
        Some(Resource::Listener(l)) => l.try_clone().map_err(|e| e.to_string()),
        Some(_) => Err(format!("handle {} is not a TCP server", id)),
        None => Err(format!("invalid or closed handle {}", id)),
    })?;
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
    REGISTRY.with(|r| r.borrow_mut().remove(&id));
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
