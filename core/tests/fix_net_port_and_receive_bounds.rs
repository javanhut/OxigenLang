//! Ports are validated and receive buffers are bounded.
//!
//! Two ways the `net` builtins used to misbehave without saying anything:
//!
//! 1. `port as u16` kept the low 16 bits, so `connect(host, 74626)` reached the
//!    listener on 9090, `connect(host, -1)` reached 65535, and
//!    `listen(host, 65536)` masked to 0 and bound a *random* ephemeral port
//!    while reporting success.
//! 2. `vec![0u8; max as usize]` trusted the caller's number, so
//!    `receive(conn, 9223372036854775807)` aborted the whole process with
//!    "memory allocation of … bytes failed" — an abort, not a catchable error.
//!
//! These call the native layer directly: the failures are in `netres`, and a
//! test that reaches the same functions without a live stdlib install stays
//! runnable under plain `cargo test`.

use oxigen_core::netres;
use std::io::Write;
use std::net::{TcpListener, UdpSocket};

/// Binds a throwaway loopback listener and returns it with its real port.
fn loopback_listener() -> (TcpListener, i64) {
    let l = TcpListener::bind("127.0.0.1:0").expect("bind loopback");
    let port = l.local_addr().unwrap().port() as i64;
    (l, port)
}

// ── port validation ─────────────────────────────────────────────────────────

#[test]
fn a_port_above_65535_is_rejected_not_masked() {
    // 74626 & 0xFFFF == 9090: this used to connect to whatever ran on 9090.
    for port in [65536_i64, 65616, 74626, 9223372036854775807] {
        let err = netres::tcp_connect("127.0.0.1", port).expect_err("connect should reject");
        assert!(
            err.contains(&port.to_string()),
            "should name the port: {err}"
        );
        assert!(err.contains("65535"), "should name the range: {err}");
        netres::tcp_listen("127.0.0.1", port).expect_err("listen should reject");
        netres::udp_bind("127.0.0.1", port).expect_err("bind should reject");
    }
}

#[test]
fn a_negative_port_is_rejected_not_wrapped() {
    // -1 as u16 is 65535.
    for port in [-1_i64, -9090, i64::MIN] {
        netres::tcp_connect("127.0.0.1", port).expect_err("connect should reject");
        netres::tcp_listen("127.0.0.1", port).expect_err("listen should reject");
        netres::udp_bind("127.0.0.1", port).expect_err("bind should reject");
    }
}

#[test]
fn port_zero_binds_an_ephemeral_port_but_cannot_be_dialled() {
    // 0 means "OS picks a free port" on the bind paths — a real use, kept.
    let server = netres::tcp_listen("127.0.0.1", 0).expect("listen on 0 is legitimate");
    netres::close(server);
    let sock = netres::udp_bind("127.0.0.1", 0).expect("bind on 0 is legitimate");

    // ...but nothing can ever listen on port 0, so dialling it is a mistake.
    netres::tcp_connect("127.0.0.1", 0).expect_err("connect to 0 should reject");
    let err = netres::udp_send(sock, "hi", "127.0.0.1", 0).expect_err("send to 0 should reject");
    assert!(err.contains("1-65535"), "connect path excludes 0: {err}");
    netres::close(sock);
}

#[test]
fn a_valid_port_still_connects() {
    let (listener, port) = loopback_listener();
    let conn = netres::tcp_connect("127.0.0.1", port).expect("in-range connect still works");
    let (mut peer, _) = listener.accept().expect("accept");
    peer.write_all(b"hello").unwrap();
    assert_eq!(netres::tcp_receive(conn, 4096).unwrap(), "hello");
    netres::close(conn);
}

// ── receive buffer bounds ───────────────────────────────────────────────────

#[test]
fn a_huge_max_does_not_abort_the_process() {
    // If the buffer were still `vec![0u8; max]`, this test binary would abort
    // outright rather than fail — the allocator does not unwind.
    let (listener, port) = loopback_listener();
    let conn = netres::tcp_connect("127.0.0.1", port).unwrap();
    let (mut peer, _) = listener.accept().unwrap();
    peer.write_all(b"still here").unwrap();
    assert_eq!(netres::tcp_receive(conn, i64::MAX).unwrap(), "still here");
    netres::close(conn);
}

#[test]
fn a_large_max_still_reads_everything_available() {
    // The cap must not cost data: one read returns up to `max`, and a payload
    // well under the 64 KiB chunk must come back whole.
    let (listener, port) = loopback_listener();
    let conn = netres::tcp_connect("127.0.0.1", port).unwrap();
    let (mut peer, _) = listener.accept().unwrap();
    let payload = "x".repeat(20_000);
    let sent = payload.clone();
    std::thread::spawn(move || {
        peer.write_all(sent.as_bytes()).unwrap();
    });

    // TCP may split it across reads; loop until we have it all.
    let mut got = String::new();
    while got.len() < payload.len() {
        let chunk = netres::tcp_receive(conn, 1_000_000_000).unwrap();
        assert!(
            !chunk.is_empty(),
            "peer closed early with {} bytes",
            got.len()
        );
        got.push_str(&chunk);
    }
    assert_eq!(got, payload);
    netres::close(conn);
}

#[test]
fn a_udp_datagram_is_never_truncated_by_the_buffer_cap() {
    // UDP is the subtle one: the OS *discards* the tail of a datagram that does
    // not fit, so a buffer smaller than the datagram loses data silently. The
    // cap is larger than any possible datagram, so this must round-trip whole
    // even though `max` is absurd.
    // netres exposes no local_addr, so borrow a free port from a std socket and
    // hand it straight to udp_bind.
    let probe = UdpSocket::bind("127.0.0.1:0").expect("probe for a free port");
    let port = probe.local_addr().unwrap().port() as i64;
    drop(probe);

    let receiver = netres::udp_bind("127.0.0.1", port).expect("bind receiver");
    let sender = netres::udp_bind("127.0.0.1", 0).expect("bind sender");
    // 9000 bytes: over any 8 KiB chunk cap, under the platform datagram limit.
    let payload = "u".repeat(9_000);
    netres::udp_send(sender, &payload, "127.0.0.1", port).expect("send datagram");

    let (data, _from) = netres::udp_receive(receiver, i64::MAX).expect("no abort, no truncation");
    assert_eq!(data.len(), payload.len(), "datagram was truncated");
    netres::close(sender);
    netres::close(receiver);
}

/// Borrows a free UDP port from a std socket (netres exposes no `local_addr`)
/// and returns a bound receiver/sender pair on it.
fn udp_pair() -> (u64, u64, i64) {
    let probe = UdpSocket::bind("127.0.0.1:0").expect("probe for a free port");
    let port = probe.local_addr().unwrap().port() as i64;
    drop(probe);
    let receiver = netres::udp_bind("127.0.0.1", port).expect("bind receiver");
    let sender = netres::udp_bind("127.0.0.1", 0).expect("bind sender");
    (receiver, sender, port)
}

#[test]
fn a_datagram_larger_than_max_errors_instead_of_silently_truncating() {
    // The whole point: UDP has no stream to resume from, so the bytes that do
    // not fit are discarded by the OS and unrecoverable. Returning a short read
    // hands back a partial message indistinguishable from a complete one — this
    // used to return "0123" for a 16-byte datagram and drop 12 bytes in silence.
    let (receiver, sender, port) = udp_pair();
    netres::udp_send(sender, "0123456789ABCDEF", "127.0.0.1", port).expect("send 16 bytes");

    let err = netres::udp_receive(receiver, 4).expect_err("must not silently truncate");
    assert!(
        err.contains("larger than max"),
        "error should name the cause, got: {err}"
    );
    netres::close(sender);
    netres::close(receiver);
}

#[test]
fn a_datagram_that_exactly_fills_max_is_not_an_error() {
    // The off-by-one that makes the check exact: reading into `max + 1` bytes is
    // the only portable way to tell "filled the request" from "was cut short",
    // since both otherwise surface as `n == max`. An exact fit must still pass.
    let (receiver, sender, port) = udp_pair();
    netres::udp_send(sender, "0123", "127.0.0.1", port).expect("send exactly 4 bytes");

    let (data, _from) = netres::udp_receive(receiver, 4).expect("an exact fit is not truncation");
    assert_eq!(data, "0123");
    netres::close(sender);
    netres::close(receiver);
}

#[test]
fn a_large_max_never_reports_spurious_truncation() {
    // Above the internal cap the buffer already exceeds any deliverable
    // datagram, so `n > max` cannot fire — a caller asking for "whatever
    // arrives" must never see the new error.
    let (receiver, sender, port) = udp_pair();
    let payload = "u".repeat(9_000);
    netres::udp_send(sender, &payload, "127.0.0.1", port).expect("send datagram");

    let (data, _from) = netres::udp_receive(receiver, i64::MAX).expect("no spurious truncation");
    assert_eq!(data.len(), payload.len());
    netres::close(sender);
    netres::close(receiver);
}

#[test]
fn a_nonpositive_max_is_still_rejected() {
    let (listener, port) = loopback_listener();
    let conn = netres::tcp_connect("127.0.0.1", port).unwrap();
    let (peer, _) = listener.accept().unwrap();
    netres::tcp_receive(conn, 0).expect_err("0 is not a read");
    netres::tcp_receive(conn, -1).expect_err("negative is not a read");
    drop(peer);
    netres::close(conn);
}

// ── connect timeout ─────────────────────────────────────────────────────────

#[test]
fn a_refused_connect_still_fails_immediately() {
    // The connect timeout must not turn a fast, definite failure into a 30s
    // stall: a closed port is an RST, not a black hole.
    let (listener, port) = loopback_listener();
    drop(listener);
    let start = std::time::Instant::now();
    netres::tcp_connect("127.0.0.1", port).expect_err("nothing is listening");
    assert!(start.elapsed().as_secs() < 5, "refusal should be prompt");
}

#[test]
fn an_unresolvable_host_reports_an_error() {
    netres::tcp_connect("no-such-host.invalid", 80).expect_err("should not resolve");
}
