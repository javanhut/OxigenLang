//! Builtins that a script — or the client of a script's server — could
//! crash, hang, or turn against the host.
//!
//! - `__json_parse` recursed once per nesting level with no limit, so a
//!   request body of a few million `[` overflowed the stack of an `api`
//!   worker and killed the whole server. `__json_stringify` recursed the same
//!   way and never terminated on a cyclic array.
//! - The JSON parser also dropped surrogate pairs (`"😀"` parsed to
//!   `""`), accepted `"\u+041"`, `01`, `1.` and unknown escapes, failed on
//!   integers past i64 instead of reading them as floats, and `stringify`
//!   wrote `{1:2}`, which is not JSON.
//! - `__sort` compared values of different types as equal. That is not a
//!   total order, Rust's sort panics when it notices, and ints mixed with
//!   floats came back unsorted.
//! - `__dt_format` with a bad format and `__env_set` with a key containing
//!   `=` panicked inside std.
//! - `range(i64::MAX)` tried to allocate the whole range, `__time_sleep(-1)`
//!   slept forever, and `__walk_dir` followed symlinked directories, so two
//!   `-> .` links made it explode exponentially.
//! - `__rand_int(min, max)` is documented as inclusive but never returned
//!   `max` (so `random.rand_bool()` was always False), and overflowed on a
//!   wide range.
//! - A VM restricted to `Network` could still read and write any file through
//!   `__net_download`/`__net_upload`, which take a local path.
//! - Network handles were small sequential integers in one process-wide
//!   table, so any VM in the process could guess another's sockets.
//! - `__net_http_read_line` buffered without bound while a server sent a
//!   line with no newline.

use std::io::{Read, Write};
use std::net::TcpListener;
use std::sync::mpsc;
use std::time::Duration;

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::{Capability, VM};

fn compile(source: &str) -> oxigen_core::vm::value::Function {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "parser errors:\n{}",
        parser.format_errors()
    );
    Compiler::new()
        .compile(&program)
        .unwrap_or_else(|errs| panic!("compile errors: {errs:?}"))
}

fn run_on(mut vm: VM, source: &str) -> Result<String, String> {
    vm.run(compile(source))
        .map(|v| format!("{v}"))
        .map_err(|e| e.message)
}

/// Runs `source` on every backend and returns the shared outcome.
fn run_all(source: &str) -> Result<String, String> {
    let interp = run_on(VM::new_interpreter(), source);
    let jit = run_on(VM::new_eager_jit(), source);
    assert_eq!(interp, jit, "backends disagree on:\n{source}");
    interp
}

fn assert_yields(source: &str, expected: &str) {
    match run_all(source) {
        Ok(got) => assert_eq!(got, expected, "for:\n{source}"),
        Err(e) => panic!("failed: {e}\nfor:\n{source}"),
    }
}

fn assert_fails_with(source: &str, needle: &str) {
    match run_all(source) {
        Ok(got) => panic!("expected an error containing {needle:?}, got {got}\nfor:\n{source}"),
        Err(e) => assert!(e.contains(needle), "error {e:?} lacks {needle:?}\nfor:\n{source}"),
    }
}

/// Runs `f` on a thread and fails the test if it has not finished in time,
/// so a hang reports instead of wedging the suite.
fn within<T: Send + 'static>(secs: u64, f: impl FnOnce() -> T + Send + 'static) -> T {
    let (tx, rx) = mpsc::channel();
    std::thread::spawn(move || {
        let _ = tx.send(f());
    });
    rx.recv_timeout(Duration::from_secs(secs))
        .unwrap_or_else(|_| panic!("did not finish within {secs}s"))
}

fn json_literal(s: &str) -> String {
    s.replace('\\', "\\\\").replace('"', "\\\"").replace('{', "\\{")
}

// ── JSON ────────────────────────────────────────────────────────────────────

#[test]
fn json_parse_refuses_deep_nesting_instead_of_overflowing_the_stack() {
    let bomb = "[".repeat(200_000);
    assert_fails_with(&format!("__json_parse(\"{bomb}\")"), "nest");
    let objects = json_literal(&"{\"a\":".repeat(200_000));
    assert_fails_with(&format!("__json_parse(\"{objects}\")"), "nest");
}

#[test]
fn json_parse_still_reads_reasonable_nesting() {
    let doc = format!("{}1{}", "[".repeat(100), "]".repeat(100));
    let expected = format!("{}1{}", "[".repeat(100), "]".repeat(100));
    assert_yields(&format!("__json_parse(\"{doc}\")"), &expected);
}

#[test]
fn json_stringify_reports_a_cycle_instead_of_recursing_forever() {
    assert_fails_with("a := [1, 2]\na[1] = a\n__json_stringify(a)", "cycl");
    assert_fails_with("m := {\"k\": 1}\nm[\"self\"] = m\n__json_stringify(m)", "cycl");
}

#[test]
fn json_stringify_refuses_nesting_it_could_not_parse_back() {
    // Built in a loop, not a literal: deep but acyclic.
    assert_fails_with(
        "x := [1]\neach i in range(1000) { x = [x] }\n__json_stringify(x)",
        "nest",
    );
    // A shared, acyclic sub-array is not a cycle.
    assert_yields("s := [1]\n__json_stringify([s, s])", "[[1],[1]]");
}

#[test]
fn json_parse_decodes_surrogate_pairs() {
    let src = format!("__json_parse(\"{}\")", json_literal(r#""😀""#));
    assert_yields(&src, "\u{1F600}");
    let lone = format!("__json_parse(\"{}\")", json_literal(r#""\ud83d""#));
    assert_fails_with(&lone, "surrogate");
}

#[test]
fn json_parse_rejects_malformed_escapes_and_numbers() {
    for bad in [
        r#""\u+041""#,
        r#""\u00g1""#,
        r#""\q""#,
        "[01]",
        "[1.]",
        "[-]",
        "[1e]",
        "[.5]",
    ] {
        let src = format!("__json_parse(\"{}\")", json_literal(bad));
        assert_fails_with(&src, "json parse error");
    }
}

#[test]
fn json_parse_reads_integers_past_i64_as_floats() {
    assert_yields("type(__json_parse(\"12345678901234567890\"))", "FLOAT");
    assert_yields("__json_parse(\"-0\")", "0");
    assert_yields("__json_parse(\"[1e2, 0.5, -3]\")", "[100, 0.5, -3]");
}

#[test]
fn json_stringify_writes_non_string_keys_as_strings() {
    assert_yields("__json_stringify({1: 2})", "{\"1\":2}");
    assert_yields("__json_stringify({True: 2})", "{\"true\":2}");
    assert_fails_with("__json_stringify({(1, 2): 3})", "key");
}

#[test]
fn json_stringify_escapes_a_quote_char() {
    // A Char holding `"` used to be written bare, breaking the document.
    assert_yields("__json_stringify([chr(34)])", "[\"\\\"\"]");
    assert_yields("__json_parse(__json_stringify([chr(34)]))[0]", "\"");
}

// ── sort ────────────────────────────────────────────────────────────────────

#[test]
fn sort_orders_ints_and_floats_numerically() {
    assert_yields("__sort([3, 2.5, 1, 0.5])", "[0.5, 1, 2.5, 3]");
    assert_yields("__sort([2, 1.5, -1, uint(7), byte(3)])", "[-1, 1.5, 2, 3, 7]");
}

#[test]
fn sort_of_mixed_types_does_not_panic_and_groups_by_type() {
    assert_yields(
        "__sort([\"b\", 3, None, \"a\", 1.5, True, False])",
        "[None, False, True, 1.5, 3, a, b]",
    );
    // Large enough that Rust's sort checks its comparator for a total order,
    // which panicked on the old one.
    let mut items = Vec::new();
    let mut seed: u64 = 3;
    for _ in 0..400 {
        seed ^= seed << 13;
        seed ^= seed >> 7;
        seed ^= seed << 17;
        items.push(match seed % 5 {
            0 => format!("{}", seed % 100),
            1 => format!("{}.5", seed % 100),
            2 => format!("\"s{}\"", seed % 100),
            3 => "float(\"nan\")".to_string(),
            _ => "None".to_string(),
        });
    }
    let src = format!(
        "s := __sort([{}])\nok := True\neach i in range(1, len(s)) {{ ok = ok and not (type(s[i - 1]) == \"STRING\" and type(s[i]) != \"STRING\") }}\nok",
        items.join(", ")
    );
    assert_yields(&src, "True");
}

// ── datetime / env / time / range ───────────────────────────────────────────

#[test]
fn dt_format_with_a_bad_format_is_an_error_not_a_panic() {
    assert_fails_with("__dt_format(0, \"%Q\")", "format");
    assert_yields("__dt_format(0, \"%Y-%m-%d\")", "1970-01-01");
}

#[test]
fn env_set_with_an_invalid_key_is_an_error_not_a_panic() {
    assert_fails_with("__env_set(\"A=B\", \"x\")", "__env_set");
    assert_fails_with("__env_set(\"\", \"x\")", "__env_set");
    assert_fails_with("__env_set(\"OXI_NUL\", \"a\" + str(chr(0)))", "__env_set");
}

#[test]
fn range_refuses_a_length_it_cannot_allocate() {
    assert_fails_with("range(9223372036854775807)", "range");
    assert_fails_with("range(-9223372036854775807, 9223372036854775807)", "range");
    assert_yields("range(2, 5)", "[2, 3, 4]");
    assert_yields("range(5, 2, -1)", "[5, 4, 3]");
}

#[test]
fn sleep_refuses_a_negative_duration() {
    within(10, || assert_fails_with("__time_sleep(-1)", "__time_sleep"));
}

#[cfg(unix)]
#[test]
fn walk_dir_does_not_descend_into_symlinked_directories() {
    let dir = std::env::temp_dir().join(format!("oxigen-walk-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(dir.join("sub")).unwrap();
    std::fs::write(dir.join("sub").join("f.txt"), "x").unwrap();
    std::os::unix::fs::symlink(".", dir.join("a")).unwrap();
    std::os::unix::fs::symlink(".", dir.join("b")).unwrap();
    let src = format!("len(__walk_dir(\"{}\"))", dir.display());
    // sub, sub/f.txt, a, b — the links are listed but not followed.
    let got = within(20, move || run_all(&src));
    let _ = std::fs::remove_dir_all(&dir);
    assert_eq!(got, Ok("4".to_string()));
}

// ── random ──────────────────────────────────────────────────────────────────

#[test]
fn rand_int_includes_its_upper_bound() {
    assert_yields(
        "__rand_seed(42)\nseen := {}\neach i in range(200) { seen[__rand_int(0, 1)] = True }\nlen(seen)",
        "2",
    );
    assert_yields(
        "__rand_seed(7)\nok := True\neach i in range(500) { r := __rand_int(1, 6)\n ok = ok and r >= 1 and r <= 6 }\nok",
        "True",
    );
}

#[test]
fn rand_int_over_the_full_i64_range_does_not_overflow() {
    assert_yields(
        "__rand_seed(9)\nseen := {}\neach i in range(20) { seen[__rand_int(-9223372036854775807, 9223372036854775807)] = True }\nlen(seen) > 1",
        "True",
    );
}

// ── capabilities ────────────────────────────────────────────────────────────

fn run_restricted(source: &str, allowed: &[Capability]) -> Result<String, String> {
    let mut vm = VM::new();
    vm.restrict(allowed);
    run_on(vm, source)
}

#[test]
fn net_download_and_upload_need_the_filesystem_capability_too() {
    let target = std::env::temp_dir().join("oxigen-cap-download.txt");
    let download = format!(
        "__net_download(\"http://127.0.0.1:9/x\", \"{}\")",
        target.display()
    );
    let upload = "__net_upload(\"POST\", \"http://127.0.0.1:9/x\", \"/etc/hostname\", {})";
    for src in [download.as_str(), upload] {
        let got = run_restricted(src, &[Capability::Network]).unwrap_err();
        assert!(got.contains("needs the 'filesystem' capability"), "got: {got}");
        let got = run_restricted(src, &[Capability::FileSystem]).unwrap_err();
        assert!(got.contains("needs the 'network' capability"), "got: {got}");
        // With both, the call is attempted (and fails to connect).
        let got = run_restricted(src, &[Capability::Network, Capability::FileSystem]);
        assert!(
            !format!("{got:?}").contains("capability"),
            "both capabilities given, still refused: {got:?}"
        );
    }
}

#[test]
fn network_handles_are_not_sequential() {
    let got = run_all(
        "h := []\neach i in range(6) { h = push(h, __net_udp_bind(\"127.0.0.1\", 0)) }\n\
         each x in h { __net_close(x) }\n\
         adjacent := 0\n\
         each i in range(1, len(h)) {\n d := h[i] - h[i - 1]\n option { d == 1 or d == -1 -> { adjacent = adjacent + 1 } }\n }\n\
         adjacent",
    );
    assert_eq!(got, Ok("0".to_string()));
}

// ── streaming HTTP ──────────────────────────────────────────────────────────

/// Serves one HTTP response whose body is `body`, then closes.
fn one_shot_server(body: Vec<u8>) -> u16 {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let port = listener.local_addr().unwrap().port();
    std::thread::spawn(move || {
        if let Ok((mut s, _)) = listener.accept() {
            let mut buf = [0u8; 4096];
            let _ = s.read(&mut buf);
            let head = format!(
                "HTTP/1.1 200 OK\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
                body.len()
            );
            let _ = s.write_all(head.as_bytes());
            let _ = s.write_all(&body);
        }
    });
    port
}

fn read_line_from(port: u16) -> Result<String, String> {
    let src = format!(
        "s := __net_http_open(\"GET\", \"http://127.0.0.1:{port}/\", {{}}, \"\")\n\
         line := __net_http_read_line(s, 0)\n__net_close(s)\nlen(line)"
    );
    run_on(VM::new_interpreter(), &src)
}

#[test]
fn http_read_line_caps_a_line_that_never_ends() {
    let port = one_shot_server(vec![b'a'; 9 * 1024 * 1024]);
    let got = within(30, move || read_line_from(port));
    let err = got.expect_err("an endless line must be refused, not buffered");
    assert!(err.contains("line"), "got: {err}");
}

#[test]
fn http_read_line_still_reads_ordinary_lines() {
    let port = one_shot_server(b"hello\nworld\n".to_vec());
    assert_eq!(within(30, move || read_line_from(port)), Ok("5".to_string()));
}
