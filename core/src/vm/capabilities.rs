//! What a script may reach outside the VM.
//!
//! A VM starts with every capability. A host that runs untrusted scripts
//! (a game running mods, say) calls [`VM::restrict`](crate::vm::VM::restrict)
//! with the ones it allows; the builtins behind the rest are replaced with
//! functions that fail, naming the missing capability. Everything else (the
//! language, pure builtins, and the stdlib modules built on them) keeps
//! working, and modules the script imports run under the same restriction.

/// Something outside the VM that a script can reach through builtins.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Capability {
    /// Read and write files and directories.
    FileSystem,
    /// Open sockets and make HTTP requests.
    Network,
    /// Run programs, read and set environment variables, change the working
    /// directory, and exit the process.
    Process,
    /// Read from standard input, which blocks until the user types.
    Stdin,
    /// Run code on other threads (`diverge`/`converge`).
    Threads,
    /// Sleep, which blocks the thread running the script.
    Sleep,
    /// Load shared libraries and call C functions (`ffi`). C code can do
    /// anything the process can, so this implies every other capability.
    Ffi,
}

impl Capability {
    /// Every capability, which is what a new VM has.
    pub const ALL: [Capability; 7] = [
        Capability::FileSystem,
        Capability::Network,
        Capability::Process,
        Capability::Stdin,
        Capability::Threads,
        Capability::Sleep,
        Capability::Ffi,
    ];

    /// The name used in error messages.
    pub fn name(self) -> &'static str {
        match self {
            Capability::FileSystem => "filesystem",
            Capability::Network => "network",
            Capability::Process => "process",
            Capability::Stdin => "stdin",
            Capability::Threads => "threads",
            Capability::Sleep => "sleep",
            Capability::Ffi => "ffi",
        }
    }
}

/// The builtins that reach outside the VM, and the capability each needs.
/// Every other builtin is pure; the test below makes sure a new builtin is
/// put in one list or the other.
pub(crate) const GATED_BUILTINS: &[(&str, Capability)] = &[
    ("__read_file", Capability::FileSystem),
    ("__write_file", Capability::FileSystem),
    ("__append_file", Capability::FileSystem),
    ("__file_exists", Capability::FileSystem),
    ("__list_dir", Capability::FileSystem),
    ("__walk_dir", Capability::FileSystem),
    ("__mkdir", Capability::FileSystem),
    ("__rmdir", Capability::FileSystem),
    ("__remove", Capability::FileSystem),
    ("__is_dir", Capability::FileSystem),
    ("__is_file", Capability::FileSystem),
    ("__file_size", Capability::FileSystem),
    ("__net_download", Capability::Network),
    ("__net_upload", Capability::Network),
    ("__net_http_open", Capability::Network),
    ("__net_http_read", Capability::Network),
    ("__net_http_read_line", Capability::Network),
    ("__net_set_timeout", Capability::Network),
    ("__net_tcp_connect", Capability::Network),
    ("__net_tcp_listen", Capability::Network),
    ("__net_tcp_accept", Capability::Network),
    ("__net_send", Capability::Network),
    ("__net_send_file", Capability::Network),
    ("__net_receive", Capability::Network),
    ("__net_close", Capability::Network),
    ("__net_udp_bind", Capability::Network),
    ("__net_udp_send", Capability::Network),
    ("__net_udp_receive", Capability::Network),
    ("__http_request", Capability::Network),
    ("__exec", Capability::Process),
    ("__exec_argv", Capability::Process),
    ("__env_get", Capability::Process),
    ("__env_set", Capability::Process),
    ("__env_vars", Capability::Process),
    ("__cwd", Capability::Process),
    ("__chdir", Capability::Process),
    ("__exit", Capability::Process),
    ("__pid", Capability::Process),
    ("__input", Capability::Stdin),
    ("__read_line", Capability::Stdin),
    ("__read_key", Capability::Stdin),
    ("__spawn", Capability::Threads),
    ("__join_task", Capability::Threads),
    ("cancel", Capability::Threads),
    ("__time_sleep", Capability::Sleep),
    ("__ffi_open", Capability::Ffi),
    ("__ffi_fn", Capability::Ffi),
    ("__ffi_struct", Capability::Ffi),
    ("__ffi_callback", Capability::Ffi),
    ("__ffi_sym", Capability::Ffi),
    ("__ffi_alloc", Capability::Ffi),
    ("__ffi_free", Capability::Ffi),
    ("__ffi_sizeof", Capability::Ffi),
    ("__ffi_read", Capability::Ffi),
    ("__ffi_write", Capability::Ffi),
    ("__ffi_string", Capability::Ffi),
    ("__ffi_cstring", Capability::Ffi),
    // These take a local path as well as a URL or socket: a script allowed
    // only the network could otherwise read and write any file through them.
    // A builtin listed twice needs both capabilities.
    ("__net_download", Capability::FileSystem),
    ("__net_upload", Capability::FileSystem),
    ("__net_send_file", Capability::FileSystem),
];

/// The stand-in for a refused builtin: callable like the original, but
/// every call fails, naming what the script would need.
pub(crate) fn refused(builtin: &str, capability: Capability) -> crate::vm::value::Value {
    use crate::vm::value::{NativeFunction, Value};
    let message = format!(
        "{builtin} needs the '{}' capability, which the program running this script has not enabled",
        capability.name()
    );
    Value::Native(std::rc::Rc::new(NativeFunction {
        name: builtin.to_string(),
        func: Box::new(move |_| Err(message.clone())),
    }))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Builtins that touch nothing outside the VM (`print` writes to the
    /// host's stdout, which no host has asked to gate).
    const PURE_BUILTINS: &[&str] = &[
        "print",
        "println",
        "len",
        "push",
        "first",
        "last",
        "rest",
        "type",
        "str",
        "int",
        "float",
        "range",
        "chars",
        "error",
        "is_value",
        "is_error",
        "keys",
        "values",
        "insert",
        "remove",
        "has",
        "tuple",
        "set",
        "byte",
        "uint",
        "chr",
        "ord",
        "__split",
        "__join",
        "__trim",
        "__upper",
        "__lower",
        "__replace",
        "__starts_with",
        "__ends_with",
        "__contains_str",
        "__str_index_of",
        "__str_count",
        "__byte_len",
        "__strip",
        "__strip_left",
        "__strip_right",
        "__sort",
        "__sqrt",
        "__floor",
        "__ceil",
        "__round",
        "__sin",
        "__cos",
        "__tan",
        "__asin",
        "__acos",
        "__atan",
        "__atan2",
        "__ln",
        "__log2",
        "__log10",
        "__exp",
        "__powf",
        "__regex_match",
        "__regex_find",
        "__regex_find_all",
        "__regex_replace",
        "__regex_split",
        "__regex_captures",
        "__dt_now",
        "__dt_format",
        "__dt_parse",
        "__base64_encode",
        "__base64_decode",
        "__hex_encode",
        "__hex_decode",
        "__url_encode",
        "__url_decode",
        "__sha256",
        "__sha1",
        "__md5",
        "__os_name",
        "__os_arch",
        "__time_now",
        "__time_now_ms",
        "__time_monotonic",
        "__rand_seed",
        "__rand_int",
        "__rand_float",
        "__path_join",
        "__path_ext",
        "__path_stem",
        "__path_filename",
        "__path_parent",
        "__path_is_absolute",
        "__path_normalize",
        "__path_is_within",
        "__json_parse",
        "__json_stringify",
        "__toml_parse",
        "__toml_stringify",
        "vec2",
        "vec3",
        "vec4",
        "__vec_dot",
        "__vec_cross",
        "__vec_length",
        "__vec_distance",
        "__vec_normalize",
        "__vec_lerp",
    ];

    #[test]
    fn every_builtin_is_either_gated_or_known_to_be_pure() {
        let mut globals = std::collections::HashMap::new();
        crate::vm::builtins::register_builtins(&mut globals);
        for name in globals.keys() {
            let gated = GATED_BUILTINS.iter().any(|(gated, _)| gated == name);
            let pure = PURE_BUILTINS.contains(&name.as_str());
            assert!(
                gated != pure,
                "builtin {name} must be in exactly one of GATED_BUILTINS and PURE_BUILTINS"
            );
        }
        for (name, _) in GATED_BUILTINS {
            assert!(
                globals.contains_key(*name),
                "{name} is gated but not a builtin"
            );
        }
    }
}
