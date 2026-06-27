//! Single-key terminal input for interactive TUIs.
//!
//! `read_key()` puts the terminal into raw mode, reads ONE key event (including
//! arrow keys and other escape sequences), restores the terminal, and returns a
//! normalized token. Exposed to Oxigen as the `__read_key` builtin.
//!
//! Tokens: "up" "down" "left" "right" "enter" "space" "tab" "backspace" "esc"
//! "home" "end" "delete" "pageup" "pagedown" "ctrl-c" "ctrl-d" "ctrl-<letter>"
//! "eof", or the literal character for a printable key ("a", "1", "?", "é", ...).
//!
//! Raw mode is toggled per call so the terminal is always restored even if the
//! caller crashes between keys.
//! ponytail: per-call raw toggle, not a held session — fine at human typing
//! speed; add raw_enable/raw_disable builtins if a redraw-heavy TUI needs it.

#[cfg(unix)]
pub fn read_key() -> std::io::Result<String> {
    use std::os::unix::io::AsRawFd;

    let fd = std::io::stdin().as_raw_fd();

    // Not a TTY (piped/redirected/tests) → degrade to a line read instead of
    // erroring, so scripts stay runnable in non-interactive contexts.
    if unsafe { libc::isatty(fd) } == 0 {
        let mut line = String::new();
        std::io::stdin().read_line(&mut line)?;
        if line.is_empty() {
            return Ok("eof".to_string());
        }
        return Ok(decode_line(&line));
    }

    let mut orig: libc::termios = unsafe { std::mem::zeroed() };
    if unsafe { libc::tcgetattr(fd, &mut orig) } != 0 {
        return Err(std::io::Error::last_os_error());
    }
    let mut raw = orig;
    unsafe { libc::cfmakeraw(&mut raw) };
    raw.c_cc[libc::VMIN] = 1; // block for at least one byte
    raw.c_cc[libc::VTIME] = 0; // no inter-byte timer for the first byte
    if unsafe { libc::tcsetattr(fd, libc::TCSANOW, &raw) } != 0 {
        return Err(std::io::Error::last_os_error());
    }

    let result = read_token(fd);

    // Always restore the original terminal settings.
    unsafe { libc::tcsetattr(fd, libc::TCSANOW, &orig) };
    result
}

#[cfg(unix)]
fn read_one(fd: i32) -> Option<u8> {
    let mut b = [0u8; 1];
    let n = unsafe { libc::read(fd, b.as_mut_ptr() as *mut libc::c_void, 1) };
    if n == 1 { Some(b[0]) } else { None }
}

// After ESC, stop blocking forever: read remaining sequence bytes with a short
// timer so a lone ESC press returns promptly.
#[cfg(unix)]
fn arm_escape_timeout(fd: i32) {
    let mut t: libc::termios = unsafe { std::mem::zeroed() };
    if unsafe { libc::tcgetattr(fd, &mut t) } == 0 {
        t.c_cc[libc::VMIN] = 0;
        t.c_cc[libc::VTIME] = 1; // 0.1s
        unsafe { libc::tcsetattr(fd, libc::TCSANOW, &t) };
    }
}

#[cfg(unix)]
fn read_token(fd: i32) -> std::io::Result<String> {
    let first = match read_one(fd) {
        Some(b) => b,
        None => return Ok("eof".to_string()),
    };
    let tok = match first {
        0x1b => {
            arm_escape_timeout(fd);
            match read_one(fd) {
                // CSI / SS3 introducer — the start of an arrow/nav sequence.
                Some(b'[') | Some(b'O') => decode_csi(fd),
                _ => "esc".to_string(), // lone ESC (or unrecognized)
            }
        }
        b'\r' | b'\n' => "enter".to_string(),
        b' ' => "space".to_string(),
        b'\t' => "tab".to_string(),
        0x7f | 0x08 => "backspace".to_string(),
        0x03 => "ctrl-c".to_string(),
        0x04 => "ctrl-d".to_string(),
        b if b < 0x20 => format!("ctrl-{}", (b + 96) as char),
        b if b < 0x80 => (b as char).to_string(),
        // UTF-8 lead byte → pull its continuation bytes and decode the char.
        b => {
            let extra = if b >= 0xf0 { 3 } else if b >= 0xe0 { 2 } else { 1 };
            let mut bytes = vec![b];
            for _ in 0..extra {
                if let Some(c) = read_one(fd) {
                    bytes.push(c);
                }
            }
            String::from_utf8_lossy(&bytes).to_string()
        }
    };
    Ok(tok)
}

// Decode the byte(s) after a `\x1b[` / `\x1bO` introducer.
#[cfg(unix)]
fn decode_csi(fd: i32) -> String {
    let b = match read_one(fd) {
        Some(b) => b,
        None => return "esc".to_string(),
    };
    match b {
        b'A' => "up".to_string(),
        b'B' => "down".to_string(),
        b'C' => "right".to_string(),
        b'D' => "left".to_string(),
        b'H' => "home".to_string(),
        b'F' => "end".to_string(),
        // Numeric form like \x1b[3~ : collect digits, expect a trailing '~'.
        b'0'..=b'9' => {
            let mut num = String::new();
            num.push(b as char);
            while let Some(n) = read_one(fd) {
                if n.is_ascii_digit() {
                    num.push(n as char);
                } else {
                    break; // '~' or anything else terminates
                }
            }
            match num.as_str() {
                "1" | "7" => "home",
                "4" | "8" => "end",
                "3" => "delete",
                "5" => "pageup",
                "6" => "pagedown",
                _ => "unknown",
            }
            .to_string()
        }
        _ => "esc".to_string(),
    }
}

// Map a whole typed line to a token (non-TTY fallback only).
fn decode_line(line: &str) -> String {
    let t = line.trim();
    match t.to_lowercase().as_str() {
        "" => "enter".to_string(),
        "up" => "up".to_string(),
        "down" => "down".to_string(),
        "left" => "left".to_string(),
        "right" => "right".to_string(),
        "enter" | "return" => "enter".to_string(),
        "space" => "space".to_string(),
        "esc" | "escape" => "esc".to_string(),
        _ => t.chars().next().map(|c| c.to_string()).unwrap_or_default(),
    }
}

// ponytail: Windows reads a line instead of a raw key — real raw mode there
// needs the console API. macOS/Linux (the target here) get the full version above.
#[cfg(not(unix))]
pub fn read_key() -> std::io::Result<String> {
    let mut line = String::new();
    let n = std::io::stdin().read_line(&mut line)?;
    if n == 0 {
        return Ok("eof".to_string());
    }
    Ok(decode_line(&line))
}
