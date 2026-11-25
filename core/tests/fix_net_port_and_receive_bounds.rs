//! Tests that `net` builtins reject out-of-range ports and bound receive
//! buffers.
//!
//! Covers port validation for `connect`/`listen` and the maximum size accepted
//! by `receive`. These call the native layer (`netres`) directly.

use oxigen_core::netres;
use std::io::Write;
use std::net::{TcpListener, UdpSocket};

/// Binds a throwaway loopback listener and returns it with its real port.
fn loopback_listener() -> (TcpListener, i64) {
    let l = TcpListener::bind("127.0.0.1:0").expect("bind loopback");
    let port = l.local_addr().unwrap().port() as i64;
    (l, port)
}

#[test]
fn a_port_above_65535_is_rejected_not_masked() {
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
    for port in [-1_i64, -9090, i64::MIN] {
        netres::tcp_connect("127.0.0.1", port).expect_err("connect should reject");
        netres::tcp_listen("127.0.0.1", port).expect_err("listen should reject");
        netres::udp_bind("127.0.0.1", port).expect_err("bind should reject");
    }
}

#[test]
fn port_zero_binds_an_ephemeral_port_but_cannot_be_dialled() {
    let server = netres::tcp_listen("127.0.0.1", 0).expect("listen on 0 is legitimate");
    netres::close(server);
    let sock = netres::udp_bind("127.0.0.1", 0).expect("bind on 0 is legitimate");

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

#[test]
fn a_huge_max_does_not_abort_the_process() {
    let (listener, port) = loopback_listener();
    let conn = netres::tcp_connect("127.0.0.1", port).unwrap();
    let (mut peer, _) = listener.accept().unwrap();
    peer.write_all(b"still here").unwrap();
    assert_eq!(netres::tcp_receive(conn, i64::MAX).unwrap(), "still here");
    netres::close(conn);
}

#[test]
fn a_large_max_still_reads_everything_available() {
    let (listener, port) = loopback_listener();
    let conn = netres::tcp_connect("127.0.0.1", port).unwrap();
    let (mut peer, _) = listener.accept().unwrap();
    let payload = "x".repeat(20_000);
    let sent = payload.clone();
    std::thread::spawn(move || {
        peer.write_all(sent.as_bytes()).unwrap();
    });

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
    let (receiver, sender, port) = udp_pair();
    let payload = "u".repeat(9_000);
    netres::udp_send(sender, &payload, "127.0.0.1", port).expect("send datagram");

    let (data, _from) = netres::udp_receive(receiver, i64::MAX).expect("no abort, no truncation");
    assert_eq!(data.len(), payload.len(), "datagram was truncated");
    netres::close(sender);
    netres::close(receiver);
}

/// Returns a bound receiver/sender pair and the receiver's real port.
///
/// The receiver binds on port 0 and learns its own port by announcing itself to
/// a throwaway socket.
fn udp_pair() -> (u64, u64, i64) {
    let receiver = netres::udp_bind("127.0.0.1", 0).expect("bind receiver");
    let sender = netres::udp_bind("127.0.0.1", 0).expect("bind sender");

    let probe = UdpSocket::bind("127.0.0.1:0").expect("probe socket");
    let probe_port = probe.local_addr().unwrap().port() as i64;
    netres::udp_send(receiver, "?", "127.0.0.1", probe_port).expect("announce receiver port");
    let (_, from) = probe
        .recv_from(&mut [0u8; 1])
        .expect("hear the receiver announce itself");

    (receiver, sender, from.port() as i64)
}

#[test]
fn a_datagram_larger_than_max_errors_instead_of_silently_truncating() {
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
    let (receiver, sender, port) = udp_pair();
    netres::udp_send(sender, "0123", "127.0.0.1", port).expect("send exactly 4 bytes");

    let (data, _from) = netres::udp_receive(receiver, 4).expect("an exact fit is not truncation");
    assert_eq!(data, "0123");
    netres::close(sender);
    netres::close(receiver);
}

#[test]
fn a_large_max_never_reports_spurious_truncation() {
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

#[test]
fn a_refused_connect_still_fails_immediately() {
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
