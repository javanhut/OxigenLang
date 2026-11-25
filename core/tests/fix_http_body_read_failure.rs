//! Tests that a failed HTTP response-body read is an error, not an empty body.
//! The server is a one-shot loopback socket that announces `Content-Length: 64`
//! and hangs up after 5 bytes.

use std::io::{Read, Write};
use std::net::TcpListener;
use std::thread;

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn run(source: &str) -> Result<String, String> {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "parser errors:\n{}",
        parser.format_errors()
    );
    let func = Compiler::new()
        .compile(&program)
        .unwrap_or_else(|errs| panic!("compile errors: {errs:?}"));
    VM::new_interpreter()
        .run(func)
        .map(|v| format!("{v}"))
        .map_err(|e| e.message)
}

/// Serves one request, then truncates the body it promised.
fn truncating_server() -> u16 {
    let listener = TcpListener::bind(("127.0.0.1", 0)).expect("bind loopback");
    let port = listener.local_addr().expect("local_addr").port();
    thread::spawn(move || {
        if let Ok(mut sock) = listener.accept().map(|(s, _)| s) {
            let mut buf = [0u8; 1024];
            let _ = sock.read(&mut buf);
            let _ = sock.write_all(b"HTTP/1.1 200 OK\r\nContent-Length: 64\r\n\r\nshort");
            let _ = sock.flush();
        }
    });
    port
}

#[test]
fn truncated_response_body_is_an_error_not_an_empty_string() {
    let port = truncating_server();
    let source = format!(r#"__http_request("GET", "http://127.0.0.1:{port}/")"#);
    let err = run(&source).expect_err("a truncated body must not read as success");
    assert!(
        err.contains("http error"),
        "expected an http error, got: {err}"
    );
}
