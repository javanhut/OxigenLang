use crate::vm::value::{ErrorValueData, Value, ValueRepr, rc_str};
use base64::Engine as _;
use sha2::Digest as _;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Register all built-in functions into a globals map.
pub fn register_builtins(globals: &mut HashMap<String, Value>) {
    globals.insert("print".to_string(), Value::Builtin(builtin_print));
    globals.insert("println".to_string(), Value::Builtin(builtin_println));
    // Concurrency primitives are internal: the public surface is the
    // `diverge` / `converge` keywords, which desugar to these. `cancel` stays
    // public — it has no keyword form.
    globals.insert("__spawn".to_string(), Value::Builtin(crate::concurrent::builtin_spawn));
    globals.insert("__join_task".to_string(), Value::Builtin(crate::concurrent::builtin_join));
    globals.insert("cancel".to_string(), Value::Builtin(crate::concurrent::builtin_cancel));
    globals.insert("len".to_string(), Value::Builtin(builtin_len));
    globals.insert("push".to_string(), Value::Builtin(builtin_push));
    globals.insert("first".to_string(), Value::Builtin(builtin_first));
    globals.insert("last".to_string(), Value::Builtin(builtin_last));
    globals.insert("rest".to_string(), Value::Builtin(builtin_rest));
    globals.insert("type".to_string(), Value::Builtin(builtin_type));
    globals.insert("str".to_string(), Value::Builtin(builtin_str));
    globals.insert("int".to_string(), Value::Builtin(builtin_int));
    globals.insert("float".to_string(), Value::Builtin(builtin_float));
    globals.insert("range".to_string(), Value::Builtin(builtin_range));
    globals.insert("chars".to_string(), Value::Builtin(builtin_chars));
    globals.insert("error".to_string(), Value::Builtin(builtin_error));
    globals.insert("is_value".to_string(), Value::Builtin(builtin_is_value));
    globals.insert("is_error".to_string(), Value::Builtin(builtin_is_error));
    globals.insert("keys".to_string(), Value::Builtin(builtin_keys));
    globals.insert("values".to_string(), Value::Builtin(builtin_values));
    globals.insert("insert".to_string(), Value::Builtin(builtin_insert));
    globals.insert("remove".to_string(), Value::Builtin(builtin_remove));
    globals.insert("has".to_string(), Value::Builtin(builtin_has));
    globals.insert("tuple".to_string(), Value::Builtin(builtin_tuple));
    globals.insert("set".to_string(), Value::Builtin(builtin_set));
    globals.insert("byte".to_string(), Value::Builtin(builtin_byte));
    globals.insert("uint".to_string(), Value::Builtin(builtin_uint));
    globals.insert("chr".to_string(), Value::Builtin(builtin_chr));
    globals.insert("ord".to_string(), Value::Builtin(builtin_ord));

    // Internal builtins (used by stdlib wrappers)
    globals.insert("__split".to_string(), Value::Builtin(builtin_split));
    globals.insert("__join".to_string(), Value::Builtin(builtin_join));
    globals.insert("__trim".to_string(), Value::Builtin(builtin_trim));
    globals.insert("__upper".to_string(), Value::Builtin(builtin_upper));
    globals.insert("__lower".to_string(), Value::Builtin(builtin_lower));
    globals.insert("__replace".to_string(), Value::Builtin(builtin_replace));
    globals.insert(
        "__starts_with".to_string(),
        Value::Builtin(builtin_starts_with),
    );
    globals.insert("__ends_with".to_string(), Value::Builtin(builtin_ends_with));
    globals.insert(
        "__contains_str".to_string(),
        Value::Builtin(builtin_contains_str),
    );
    globals.insert("__strip".to_string(), Value::Builtin(builtin_strip));
    globals.insert(
        "__strip_left".to_string(),
        Value::Builtin(builtin_strip_left),
    );
    globals.insert(
        "__strip_right".to_string(),
        Value::Builtin(builtin_strip_right),
    );
    globals.insert("__sort".to_string(), Value::Builtin(builtin_sort));

    // Math builtins
    globals.insert("__sqrt".to_string(), Value::Builtin(builtin_sqrt));
    globals.insert("__floor".to_string(), Value::Builtin(builtin_floor));
    globals.insert("__ceil".to_string(), Value::Builtin(builtin_ceil));
    globals.insert("__round".to_string(), Value::Builtin(builtin_round));
    globals.insert("__sin".to_string(), Value::Builtin(builtin_sin));
    globals.insert("__cos".to_string(), Value::Builtin(builtin_cos));
    globals.insert("__tan".to_string(), Value::Builtin(builtin_tan));
    globals.insert("__asin".to_string(), Value::Builtin(builtin_asin));
    globals.insert("__acos".to_string(), Value::Builtin(builtin_acos));
    globals.insert("__atan".to_string(), Value::Builtin(builtin_atan));
    globals.insert("__atan2".to_string(), Value::Builtin(builtin_atan2));
    globals.insert("__ln".to_string(), Value::Builtin(builtin_ln));
    globals.insert("__log2".to_string(), Value::Builtin(builtin_log2));
    globals.insert("__log10".to_string(), Value::Builtin(builtin_log10));
    globals.insert("__exp".to_string(), Value::Builtin(builtin_exp));
    globals.insert("__powf".to_string(), Value::Builtin(builtin_powf));

    // regex
    globals.insert("__regex_match".to_string(), Value::Builtin(builtin_regex_match));
    globals.insert("__regex_find".to_string(), Value::Builtin(builtin_regex_find));
    globals.insert("__regex_find_all".to_string(), Value::Builtin(builtin_regex_find_all));
    globals.insert("__regex_replace".to_string(), Value::Builtin(builtin_regex_replace));
    globals.insert("__regex_split".to_string(), Value::Builtin(builtin_regex_split));
    globals.insert("__regex_captures".to_string(), Value::Builtin(builtin_regex_captures));
    // datetime
    globals.insert("__dt_now".to_string(), Value::Builtin(builtin_dt_now));
    globals.insert("__dt_format".to_string(), Value::Builtin(builtin_dt_format));
    globals.insert("__dt_parse".to_string(), Value::Builtin(builtin_dt_parse));
    // encoding
    globals.insert("__base64_encode".to_string(), Value::Builtin(builtin_base64_encode));
    globals.insert("__base64_decode".to_string(), Value::Builtin(builtin_base64_decode));
    globals.insert("__hex_encode".to_string(), Value::Builtin(builtin_hex_encode));
    globals.insert("__hex_decode".to_string(), Value::Builtin(builtin_hex_decode));
    globals.insert("__url_encode".to_string(), Value::Builtin(builtin_url_encode));
    globals.insert("__url_decode".to_string(), Value::Builtin(builtin_url_decode));
    // hash
    globals.insert("__sha256".to_string(), Value::Builtin(builtin_sha256));
    globals.insert("__sha1".to_string(), Value::Builtin(builtin_sha1));
    globals.insert("__md5".to_string(), Value::Builtin(builtin_md5));

    // I/O builtins
    globals.insert("__input".to_string(), Value::Builtin(builtin_input));
    globals.insert("__read_line".to_string(), Value::Builtin(builtin_read_line));
    globals.insert("__read_key".to_string(), Value::Builtin(builtin_read_key));

    // File I/O
    globals.insert("__read_file".to_string(), Value::Builtin(builtin_read_file));
    globals.insert(
        "__write_file".to_string(),
        Value::Builtin(builtin_write_file),
    );
    globals.insert(
        "__append_file".to_string(),
        Value::Builtin(builtin_append_file),
    );
    globals.insert(
        "__file_exists".to_string(),
        Value::Builtin(builtin_file_exists),
    );

    // OS builtins
    globals.insert("__exec".to_string(), Value::Builtin(builtin_exec));
    globals.insert("__os_name".to_string(), Value::Builtin(builtin_os_name));
    globals.insert("__os_arch".to_string(), Value::Builtin(builtin_os_arch));
    globals.insert("__env_get".to_string(), Value::Builtin(builtin_env_get));
    globals.insert("__env_set".to_string(), Value::Builtin(builtin_env_set));
    globals.insert("__env_vars".to_string(), Value::Builtin(builtin_env_vars));
    globals.insert("__cwd".to_string(), Value::Builtin(builtin_cwd));
    globals.insert("__chdir".to_string(), Value::Builtin(builtin_chdir));
    globals.insert("__exit".to_string(), Value::Builtin(builtin_exit));
    globals.insert("__pid".to_string(), Value::Builtin(builtin_pid));
    globals.insert("__list_dir".to_string(), Value::Builtin(builtin_list_dir));
    globals.insert("__walk_dir".to_string(), Value::Builtin(builtin_walk_dir));
    globals.insert("__mkdir".to_string(), Value::Builtin(builtin_mkdir));
    globals.insert("__rmdir".to_string(), Value::Builtin(builtin_rmdir));
    globals.insert("__remove".to_string(), Value::Builtin(builtin_remove_file));
    globals.insert("__is_dir".to_string(), Value::Builtin(builtin_is_dir));
    globals.insert("__is_file".to_string(), Value::Builtin(builtin_is_file));

    // Time builtins
    globals.insert("__time_now".to_string(), Value::Builtin(builtin_time_now));
    globals.insert(
        "__time_now_ms".to_string(),
        Value::Builtin(builtin_time_now_ms),
    );
    globals.insert(
        "__time_sleep".to_string(),
        Value::Builtin(builtin_time_sleep),
    );
    globals.insert(
        "__time_monotonic".to_string(),
        Value::Builtin(builtin_time_monotonic),
    );

    // Random builtins
    globals.insert("__rand_seed".to_string(), Value::Builtin(builtin_rand_seed));
    globals.insert("__rand_int".to_string(), Value::Builtin(builtin_rand_int));
    globals.insert(
        "__rand_float".to_string(),
        Value::Builtin(builtin_rand_float),
    );

    // Path builtins
    globals.insert("__path_join".to_string(), Value::Builtin(builtin_path_join));
    globals.insert("__path_ext".to_string(), Value::Builtin(builtin_path_ext));
    globals.insert(
        "__path_filename".to_string(),
        Value::Builtin(builtin_path_filename),
    );
    globals.insert(
        "__path_parent".to_string(),
        Value::Builtin(builtin_path_parent),
    );
    globals.insert("__path_stem".to_string(), Value::Builtin(builtin_path_stem));
    globals.insert(
        "__path_is_absolute".to_string(),
        Value::Builtin(builtin_path_is_absolute),
    );

    // JSON / TOML
    globals.insert(
        "__json_parse".to_string(),
        Value::Builtin(builtin_json_parse),
    );
    globals.insert(
        "__json_stringify".to_string(),
        Value::Builtin(builtin_json_stringify),
    );
    globals.insert(
        "__toml_parse".to_string(),
        Value::Builtin(builtin_toml_parse),
    );
    globals.insert(
        "__toml_stringify".to_string(),
        Value::Builtin(builtin_toml_stringify),
    );

    // Network
    globals.insert(
        "__http_request".to_string(),
        Value::Builtin(builtin_http_request),
    );
    globals.insert("__net_download".to_string(), Value::Builtin(builtin_net_download));
    globals.insert("__net_upload".to_string(), Value::Builtin(builtin_net_upload));
    globals.insert("__net_http_open".to_string(), Value::Builtin(builtin_net_http_open));
    globals.insert("__net_http_read".to_string(), Value::Builtin(builtin_net_http_read));
    globals.insert(
        "__net_http_read_line".to_string(),
        Value::Builtin(builtin_net_http_read_line),
    );
    globals.insert("__net_tcp_connect".to_string(), Value::Builtin(builtin_net_tcp_connect));
    globals.insert("__net_tcp_listen".to_string(), Value::Builtin(builtin_net_tcp_listen));
    globals.insert("__net_tcp_accept".to_string(), Value::Builtin(builtin_net_tcp_accept));
    globals.insert("__net_send".to_string(), Value::Builtin(builtin_net_send));
    globals.insert("__net_receive".to_string(), Value::Builtin(builtin_net_receive));
    globals.insert("__net_close".to_string(), Value::Builtin(builtin_net_close));
    globals.insert("__net_udp_bind".to_string(), Value::Builtin(builtin_net_udp_bind));
    globals.insert("__net_udp_send".to_string(), Value::Builtin(builtin_net_udp_send));
    globals.insert("__net_udp_receive".to_string(), Value::Builtin(builtin_net_udp_receive));
}

// ── Core builtins ──────────────────────────────────────────────────────

fn builtin_print(args: &[Value]) -> Value {
    use std::io::Write;
    let parts: Vec<String> = args.iter().map(|a| format!("{}", a)).collect();
    // Flush so `print` (no trailing newline) shows immediately — needed for
    // token-by-token streaming output, where nothing else triggers a flush.
    let mut out = std::io::stdout().lock();
    let _ = write!(out, "{}", parts.join(" "));
    let _ = out.flush();
    Value::None
}

fn builtin_println(args: &[Value]) -> Value {
    let parts: Vec<String> = args.iter().map(|a| format!("{}", a)).collect();
    println!("{}", parts.join(" "));
    Value::None
}

fn builtin_len(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("len() takes exactly 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::String(s) => Value::Integer(s.len() as i64),
        ValueRepr::Array(a) => Value::Integer(a.borrow().len() as i64),
        ValueRepr::Tuple(t) => Value::Integer(t.len() as i64),
        ValueRepr::Map(m) => Value::Integer(m.borrow().len() as i64),
        ValueRepr::Set(s) => Value::Integer(s.borrow().len() as i64),
        _ => Value::Error(rc_str(format!(
            "len() not supported for {}",
            args[0].type_name()
        ))),
    }
}

fn builtin_push(args: &[Value]) -> Value {
    if args.len() != 2 {
        return Value::Error(rc_str("push() takes exactly 2 arguments"));
    }
    match args[0].repr() {
        ValueRepr::Array(arr) => {
            arr.borrow_mut().push(args[1].clone());
            args[0].clone() // return the array (same behavior as tree-walker)
        }
        _ => Value::Error(rc_str("push() requires an array as first argument")),
    }
}

fn builtin_first(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("first() takes exactly 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::Array(arr) => arr.borrow().first().cloned().unwrap_or(Value::None),
        _ => Value::Error(rc_str("first() requires an array")),
    }
}

fn builtin_last(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("last() takes exactly 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::Array(arr) => arr.borrow().last().cloned().unwrap_or(Value::None),
        _ => Value::Error(rc_str("last() requires an array")),
    }
}

fn builtin_rest(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("rest() takes exactly 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::Array(arr) => {
            let borrowed = arr.borrow();
            if borrowed.is_empty() {
                Value::Array(Rc::new(RefCell::new(Vec::new())))
            } else {
                Value::Array(Rc::new(RefCell::new(borrowed[1..].to_vec())))
            }
        }
        _ => Value::Error(rc_str("rest() requires an array")),
    }
}

fn builtin_type(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("type() takes exactly 1 argument"));
    }
    Value::String(rc_str(args[0].effective_type_name()))
}

fn builtin_str(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("str() takes exactly 1 argument"));
    }
    match args[0].repr() {
        // A char stringifies to its bare character, not the `x`-quoted Display form.
        ValueRepr::Char(c) => Value::String(rc_str(c.to_string())),
        _ => Value::String(rc_str(format!("{}", args[0]))),
    }
}

fn builtin_int(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("int() takes exactly 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::Integer(n) => Value::Integer(n),
        ValueRepr::Float(f) => Value::Integer(f as i64),
        // Trim surrounding whitespace before parsing, matching the
        // tree-walker (int(" 5 ") == 5).
        ValueRepr::String(s) => match s.trim().parse::<i64>() {
            Ok(n) => Value::Integer(n),
            Err(_) => match s.trim().parse::<f64>() {
                Ok(f) => Value::Integer(f as i64),
                Err(_) => Value::Error(rc_str(format!("cannot convert '{}' to int", s))),
            },
        },
        ValueRepr::Boolean(b) => Value::Integer(if b { 1 } else { 0 }),
        ValueRepr::Byte(b) => Value::Integer(b as i64),
        ValueRepr::Uint(u) => Value::Integer(u as i64),
        ValueRepr::Char(c) => Value::Integer(c as i64),
        _ => Value::Error(rc_str(format!(
            "cannot convert {} to int",
            args[0].type_name()
        ))),
    }
}

fn builtin_float(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("float() takes exactly 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::Float(f) => Value::Float(f),
        ValueRepr::Integer(n) => Value::Float(n as f64),
        // Trim surrounding whitespace before parsing, matching the
        // tree-walker (float(" 1.5 ") == 1.5).
        ValueRepr::String(s) => match s.trim().parse::<f64>() {
            Ok(f) => Value::Float(f),
            Err(_) => Value::Error(rc_str(format!("cannot convert '{}' to float", s))),
        },
        // float(True) == 1.0, float(False) == 0.0, matching the tree-walker.
        ValueRepr::Boolean(b) => Value::Float(if b { 1.0 } else { 0.0 }),
        ValueRepr::Byte(b) => Value::Float(b as f64),
        ValueRepr::Uint(u) => Value::Float(u as f64),
        _ => Value::Error(rc_str(format!(
            "cannot convert {} to float",
            args[0].type_name()
        ))),
    }
}

fn builtin_range(args: &[Value]) -> Value {
    let (start, end) = match args.len() {
        1 => match args[0].repr() {
            ValueRepr::Integer(n) => (0i64, n),
            _ => return Value::Error(rc_str("range() requires integer arguments")),
        },
        2 => match (&args[0], &args[1]) {
            (Value::Integer(s), Value::Integer(e)) => (*s, *e),
            _ => return Value::Error(rc_str("range() requires integer arguments")),
        },
        _ => return Value::Error(rc_str("range() takes 1 or 2 arguments")),
    };
    let arr: Vec<Value> = (start..end).map(Value::Integer).collect();
    Value::Array(Rc::new(RefCell::new(arr)))
}

fn builtin_chars(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("chars() takes exactly 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::String(s) => {
            let arr: Vec<Value> = s.chars().map(Value::Char).collect();
            Value::Array(Rc::new(RefCell::new(arr)))
        }
        _ => Value::Error(rc_str("chars() requires a string")),
    }
}

fn builtin_error(args: &[Value]) -> Value {
    match args.len() {
        1 => {
            let msg = format!("{}", args[0]);
            Value::ErrorValue(Rc::new(ErrorValueData {
                msg: rc_str(msg),
                tag: None,
            }))
        }
        2 => {
            let msg = format!("{}", args[0]);
            let tag = format!("{}", args[1]);
            Value::ErrorValue(Rc::new(ErrorValueData {
                msg: rc_str(msg),
                tag: Some(rc_str(tag)),
            }))
        }
        _ => Value::Error(rc_str("error() takes 1 or 2 arguments")),
    }
}

fn builtin_is_value(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("is_value() takes exactly 1 argument"));
    }
    Value::Boolean(matches!(args[0], Value::Wrapped(_)))
}

fn builtin_is_error(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("is_error() takes exactly 1 argument"));
    }
    Value::Boolean(matches!(args[0], Value::ErrorValue(_)))
}

fn builtin_keys(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("keys() takes exactly 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::Map(m) => {
            let keys: Vec<Value> = m.borrow().iter().map(|(k, _)| k.clone()).collect();
            Value::Array(Rc::new(RefCell::new(keys)))
        }
        _ => Value::Error(rc_str("keys() requires a map")),
    }
}

fn builtin_values(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("values() takes exactly 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::Map(m) => {
            let vals: Vec<Value> = m.borrow().iter().map(|(_, v)| v.clone()).collect();
            Value::Array(Rc::new(RefCell::new(vals)))
        }
        _ => Value::Error(rc_str("values() requires a map")),
    }
}

fn builtin_insert(args: &[Value]) -> Value {
    if args.len() != 3 {
        return Value::Error(rc_str("insert() takes exactly 3 arguments"));
    }
    match args[0].repr() {
        ValueRepr::Map(m) => {
            m.borrow_mut().insert(args[1].clone(), args[2].clone());
            args[0].clone()
        }
        ValueRepr::Set(s) => {
            s.borrow_mut().insert(args[1].clone());
            args[0].clone()
        }
        _ => Value::Error(rc_str("insert() requires a map or set")),
    }
}

fn builtin_remove(args: &[Value]) -> Value {
    if args.len() != 2 {
        return Value::Error(rc_str("remove() takes exactly 2 arguments"));
    }
    match args[0].repr() {
        ValueRepr::Map(m) => {
            m.borrow_mut().remove(&args[1]);
            args[0].clone()
        }
        ValueRepr::Set(s) => {
            s.borrow_mut().remove(&args[1]);
            args[0].clone()
        }
        _ => Value::Error(rc_str("remove() requires a map or set")),
    }
}

fn builtin_has(args: &[Value]) -> Value {
    if args.len() != 2 {
        return Value::Error(rc_str("has() takes exactly 2 arguments"));
    }
    match args[0].repr() {
        ValueRepr::Map(m) => Value::Boolean(m.borrow().contains_key(&args[1])),
        ValueRepr::Set(s) => Value::Boolean(s.borrow().contains(&args[1])),
        ValueRepr::Array(a) => Value::Boolean(a.borrow().iter().any(|e| e == &args[1])),
        ValueRepr::Tuple(t) => Value::Boolean(t.iter().any(|e| e == &args[1])),
        _ => Value::Error(rc_str("has() requires a collection")),
    }
}

fn builtin_tuple(args: &[Value]) -> Value {
    Value::Tuple(Rc::new(args.to_vec()))
}

fn builtin_set(args: &[Value]) -> Value {
    Value::Set(Rc::new(RefCell::new(
        crate::vm::collections::OxSet::from_iter_dedup(args.iter().cloned()),
    )))
}

fn builtin_byte(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("byte() takes exactly 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::Byte(b) => Value::Byte(b),
        ValueRepr::Integer(n) => {
            if !(0..=255).contains(&n) {
                Value::Error(rc_str(format!(
                    "byte() argument out of range (0-255): {}",
                    n
                )))
            } else {
                Value::Byte(n as u8)
            }
        }
        ValueRepr::Uint(n) => {
            if n > 255 {
                Value::Error(rc_str(format!(
                    "byte() argument out of range (0-255): {}",
                    n
                )))
            } else {
                Value::Byte(n as u8)
            }
        }
        ValueRepr::Char(c) => Value::Byte(c as u8),
        _ => Value::Error(rc_str(format!(
            "cannot convert {} to byte",
            args[0].type_name()
        ))),
    }
}

fn builtin_uint(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("uint() takes exactly 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::Uint(u) => Value::Uint(u),
        ValueRepr::Integer(n) => {
            if n < 0 {
                Value::Error(rc_str(format!("uint() cannot convert negative: {}", n)))
            } else {
                Value::Uint(n as u64)
            }
        }
        ValueRepr::Float(f) => {
            if f < 0.0 {
                Value::Error(rc_str(format!("uint() cannot convert negative: {}", f)))
            } else {
                Value::Uint(f as u64)
            }
        }
        ValueRepr::Byte(b) => Value::Uint(b as u64),
        ValueRepr::String(s) => match s.parse::<u64>() {
            Ok(n) => Value::Uint(n),
            Err(_) => Value::Error(rc_str(format!("cannot convert '{}' to uint", s))),
        },
        _ => Value::Error(rc_str(format!(
            "cannot convert {} to uint",
            args[0].type_name()
        ))),
    }
}

fn builtin_chr(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("chr() takes exactly 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::Integer(n) => match char::from_u32(n as u32) {
            Some(c) => Value::Char(c),
            None => Value::Error(rc_str(format!("invalid char code: {}", n))),
        },
        _ => Value::Error(rc_str("chr() requires an integer")),
    }
}

fn builtin_ord(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("ord() takes exactly 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::Char(c) => Value::Integer(c as i64),
        _ => Value::Error(rc_str("ord() requires a char")),
    }
}

// ── String builtins ────────────────────────────────────────────────────

fn builtin_split(args: &[Value]) -> Value {
    if args.len() != 2 {
        return Value::Error(rc_str("__split() takes 2 arguments"));
    }
    match (&args[0], &args[1]) {
        (Value::String(s), Value::String(sep)) => {
            let parts: Vec<Value> = s
                .split(sep.as_ref())
                .map(|p| Value::String(rc_str(p)))
                .collect();
            Value::Array(Rc::new(RefCell::new(parts)))
        }
        _ => Value::Error(rc_str("__split() requires string arguments")),
    }
}

fn builtin_join(args: &[Value]) -> Value {
    if args.len() != 2 {
        return Value::Error(rc_str("__join() takes 2 arguments"));
    }
    match (&args[0], &args[1]) {
        (Value::Array(arr), Value::String(sep)) => {
            use std::fmt::Write as _;
            // Accumulate into one buffer instead of building a Vec<String>
            // of N throwaway `format!` allocations and then joining.
            let arr = arr.borrow();
            let mut result = String::new();
            for (i, v) in arr.iter().enumerate() {
                if i > 0 {
                    result.push_str(sep.as_ref());
                }
                let _ = write!(result, "{}", v);
            }
            Value::String(rc_str(result))
        }
        _ => Value::Error(rc_str("__join() requires (array, string)")),
    }
}

fn builtin_trim(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__trim() takes 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::String(s) => Value::String(rc_str(s.trim())),
        _ => Value::Error(rc_str("__trim() requires a string")),
    }
}

fn builtin_upper(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__upper() takes 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::String(s) => Value::String(rc_str(s.to_uppercase())),
        _ => Value::Error(rc_str("__upper() requires a string")),
    }
}

fn builtin_lower(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__lower() takes 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::String(s) => Value::String(rc_str(s.to_lowercase())),
        _ => Value::Error(rc_str("__lower() requires a string")),
    }
}

fn builtin_replace(args: &[Value]) -> Value {
    if args.len() != 3 {
        return Value::Error(rc_str("__replace() takes 3 arguments"));
    }
    match (&args[0], &args[1], &args[2]) {
        (Value::String(s), Value::String(from), Value::String(to)) => {
            Value::String(rc_str(s.replace(from.as_ref(), to.as_ref())))
        }
        _ => Value::Error(rc_str("__replace() requires string arguments")),
    }
}

fn builtin_starts_with(args: &[Value]) -> Value {
    if args.len() != 2 {
        return Value::Error(rc_str("__starts_with() takes 2 arguments"));
    }
    match (&args[0], &args[1]) {
        (Value::String(s), Value::String(prefix)) => Value::Boolean(s.starts_with(prefix.as_ref())),
        _ => Value::Error(rc_str("__starts_with() requires string arguments")),
    }
}

fn builtin_ends_with(args: &[Value]) -> Value {
    if args.len() != 2 {
        return Value::Error(rc_str("__ends_with() takes 2 arguments"));
    }
    match (&args[0], &args[1]) {
        (Value::String(s), Value::String(suffix)) => Value::Boolean(s.ends_with(suffix.as_ref())),
        _ => Value::Error(rc_str("__ends_with() requires string arguments")),
    }
}

fn builtin_contains_str(args: &[Value]) -> Value {
    if args.len() != 2 {
        return Value::Error(rc_str("__contains_str() takes 2 arguments"));
    }
    match (&args[0], &args[1]) {
        (Value::String(s), Value::String(sub)) => Value::Boolean(s.contains(sub.as_ref())),
        _ => Value::Error(rc_str("__contains_str() requires string arguments")),
    }
}

fn builtin_strip(args: &[Value]) -> Value {
    if args.len() != 2 {
        return Value::Error(rc_str("__strip() takes 2 arguments"));
    }
    match (&args[0], &args[1]) {
        (Value::String(s), Value::String(chars)) => {
            let chars_vec: Vec<char> = chars.chars().collect();
            let result: String = s.trim_matches(chars_vec.as_slice()).to_string();
            Value::String(rc_str(result))
        }
        _ => Value::Error(rc_str("__strip() requires string arguments")),
    }
}

fn builtin_strip_left(args: &[Value]) -> Value {
    if args.len() != 2 {
        return Value::Error(rc_str("__strip_left() takes 2 arguments"));
    }
    match (&args[0], &args[1]) {
        (Value::String(s), Value::String(chars)) => {
            let chars_vec: Vec<char> = chars.chars().collect();
            let result: String = s.trim_start_matches(chars_vec.as_slice()).to_string();
            Value::String(rc_str(result))
        }
        _ => Value::Error(rc_str("__strip_left() requires string arguments")),
    }
}

fn builtin_strip_right(args: &[Value]) -> Value {
    if args.len() != 2 {
        return Value::Error(rc_str("__strip_right() takes 2 arguments"));
    }
    match (&args[0], &args[1]) {
        (Value::String(s), Value::String(chars)) => {
            let chars_vec: Vec<char> = chars.chars().collect();
            let result: String = s.trim_end_matches(chars_vec.as_slice()).to_string();
            Value::String(rc_str(result))
        }
        _ => Value::Error(rc_str("__strip_right() requires string arguments")),
    }
}

fn builtin_sort(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__sort() takes 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::Array(arr) => {
            let mut sorted = arr.borrow().clone();
            sorted.sort_by(|a, b| match (a, b) {
                (Value::Integer(x), Value::Integer(y)) => x.cmp(y),
                (Value::Float(x), Value::Float(y)) => {
                    x.partial_cmp(y).unwrap_or(std::cmp::Ordering::Equal)
                }
                (Value::String(x), Value::String(y)) => x.cmp(y),
                _ => std::cmp::Ordering::Equal,
            });
            Value::Array(Rc::new(RefCell::new(sorted)))
        }
        _ => Value::Error(rc_str("__sort() requires an array")),
    }
}

// ── Math builtins ──────────────────────────────────────────────────────

fn builtin_sqrt(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__sqrt() takes 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::Float(f) => Value::Float(f.sqrt()),
        ValueRepr::Integer(n) => Value::Float((n as f64).sqrt()),
        _ => Value::Error(rc_str("__sqrt() requires a number")),
    }
}

fn builtin_floor(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__floor() takes 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::Float(f) => Value::Integer(f.floor() as i64),
        ValueRepr::Integer(n) => Value::Integer(n),
        _ => Value::Error(rc_str("__floor() requires a number")),
    }
}

fn builtin_ceil(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__ceil() takes 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::Float(f) => Value::Integer(f.ceil() as i64),
        ValueRepr::Integer(n) => Value::Integer(n),
        _ => Value::Error(rc_str("__ceil() requires a number")),
    }
}

fn builtin_round(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__round() takes 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::Float(f) => Value::Integer(f.round() as i64),
        ValueRepr::Integer(n) => Value::Integer(n),
        _ => Value::Error(rc_str("__round() requires a number")),
    }
}

// Shared helpers for floating-point math builtins.
fn unary_f64(args: &[Value], name: &str, f: fn(f64) -> f64) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str(format!("{}() takes 1 argument", name)));
    }
    match args[0].repr() {
        ValueRepr::Float(x) => Value::Float(f(x)),
        ValueRepr::Integer(n) => Value::Float(f(n as f64)),
        _ => Value::Error(rc_str(format!("{}() requires a number", name))),
    }
}

fn binary_f64(args: &[Value], name: &str, f: fn(f64, f64) -> f64) -> Value {
    if args.len() != 2 {
        return Value::Error(rc_str(format!("{}() takes 2 arguments", name)));
    }
    let a = match args[0].repr() {
        ValueRepr::Float(x) => x,
        ValueRepr::Integer(n) => n as f64,
        _ => return Value::Error(rc_str(format!("{}() requires numbers", name))),
    };
    let b = match args[1].repr() {
        ValueRepr::Float(x) => x,
        ValueRepr::Integer(n) => n as f64,
        _ => return Value::Error(rc_str(format!("{}() requires numbers", name))),
    };
    Value::Float(f(a, b))
}

fn builtin_sin(args: &[Value]) -> Value {
    unary_f64(args, "__sin", f64::sin)
}
fn builtin_cos(args: &[Value]) -> Value {
    unary_f64(args, "__cos", f64::cos)
}
fn builtin_tan(args: &[Value]) -> Value {
    unary_f64(args, "__tan", f64::tan)
}
fn builtin_asin(args: &[Value]) -> Value {
    unary_f64(args, "__asin", f64::asin)
}
fn builtin_acos(args: &[Value]) -> Value {
    unary_f64(args, "__acos", f64::acos)
}
fn builtin_atan(args: &[Value]) -> Value {
    unary_f64(args, "__atan", f64::atan)
}
fn builtin_atan2(args: &[Value]) -> Value {
    binary_f64(args, "__atan2", f64::atan2)
}
fn builtin_ln(args: &[Value]) -> Value {
    unary_f64(args, "__ln", f64::ln)
}
fn builtin_log2(args: &[Value]) -> Value {
    unary_f64(args, "__log2", f64::log2)
}
fn builtin_log10(args: &[Value]) -> Value {
    unary_f64(args, "__log10", f64::log10)
}
fn builtin_exp(args: &[Value]) -> Value {
    unary_f64(args, "__exp", f64::exp)
}
fn builtin_powf(args: &[Value]) -> Value {
    binary_f64(args, "__powf", f64::powf)
}

// ── Shared helpers for native extension builtins ──

fn vm_hard_err(msg: impl Into<String>) -> Value {
    Value::Error(rc_str(msg))
}

/// A catchable (Error || Value) error for recoverable failures.
fn vm_soft_err(msg: impl Into<String>, tag: &str) -> Value {
    Value::ErrorValue(Rc::new(ErrorValueData {
        msg: rc_str(msg),
        tag: Some(rc_str(tag)),
    }))
}

fn vm_str_arg(v: &Value) -> Option<&str> {
    match v {
        Value::String(s) => Some(s.as_str()),
        _ => None,
    }
}

fn vm_str_array(items: impl IntoIterator<Item = String>) -> Value {
    Value::Array(Rc::new(RefCell::new(
        items.into_iter().map(|s| Value::String(rc_str(s))).collect(),
    )))
}

// ── regex ──

fn vm_compile_re(pattern: &str, name: &str) -> Result<regex::Regex, Value> {
    regex::Regex::new(pattern).map_err(|e| vm_hard_err(format!("{}: invalid pattern: {}", name, e)))
}

fn builtin_regex_match(args: &[Value]) -> Value {
    match (args.first().and_then(vm_str_arg), args.get(1).and_then(vm_str_arg)) {
        (Some(pat), Some(text)) if args.len() == 2 => match vm_compile_re(pat, "__regex_match") {
            Ok(re) => Value::Boolean(re.is_match(text)),
            Err(e) => e,
        },
        _ => vm_hard_err("__regex_match: expected (pattern, text) strings"),
    }
}

fn builtin_regex_find(args: &[Value]) -> Value {
    match (args.first().and_then(vm_str_arg), args.get(1).and_then(vm_str_arg)) {
        (Some(pat), Some(text)) if args.len() == 2 => match vm_compile_re(pat, "__regex_find") {
            Ok(re) => match re.find(text) {
                Some(m) => Value::String(rc_str(m.as_str())),
                None => Value::None,
            },
            Err(e) => e,
        },
        _ => vm_hard_err("__regex_find: expected (pattern, text) strings"),
    }
}

fn builtin_regex_find_all(args: &[Value]) -> Value {
    match (args.first().and_then(vm_str_arg), args.get(1).and_then(vm_str_arg)) {
        (Some(pat), Some(text)) if args.len() == 2 => match vm_compile_re(pat, "__regex_find_all") {
            Ok(re) => vm_str_array(re.find_iter(text).map(|m| m.as_str().to_string())),
            Err(e) => e,
        },
        _ => vm_hard_err("__regex_find_all: expected (pattern, text) strings"),
    }
}

fn builtin_regex_replace(args: &[Value]) -> Value {
    if args.len() != 3 {
        return vm_hard_err("__regex_replace: expected 3 arguments");
    }
    match (vm_str_arg(&args[0]), vm_str_arg(&args[1]), vm_str_arg(&args[2])) {
        (Some(pat), Some(text), Some(rep)) => match vm_compile_re(pat, "__regex_replace") {
            Ok(re) => Value::String(rc_str(re.replace_all(text, rep).into_owned())),
            Err(e) => e,
        },
        _ => vm_hard_err("__regex_replace: expected (pattern, text, replacement) strings"),
    }
}

fn builtin_regex_split(args: &[Value]) -> Value {
    match (args.first().and_then(vm_str_arg), args.get(1).and_then(vm_str_arg)) {
        (Some(pat), Some(text)) if args.len() == 2 => match vm_compile_re(pat, "__regex_split") {
            Ok(re) => vm_str_array(re.split(text).map(|s| s.to_string())),
            Err(e) => e,
        },
        _ => vm_hard_err("__regex_split: expected (pattern, text) strings"),
    }
}

fn builtin_regex_captures(args: &[Value]) -> Value {
    match (args.first().and_then(vm_str_arg), args.get(1).and_then(vm_str_arg)) {
        (Some(pat), Some(text)) if args.len() == 2 => match vm_compile_re(pat, "__regex_captures") {
            Ok(re) => match re.captures(text) {
                Some(caps) => vm_str_array(
                    caps.iter()
                        .map(|m| m.map(|x| x.as_str().to_string()).unwrap_or_default()),
                ),
                None => Value::None,
            },
            Err(e) => e,
        },
        _ => vm_hard_err("__regex_captures: expected (pattern, text) strings"),
    }
}

// ── datetime (UTC) ──

fn builtin_dt_now(args: &[Value]) -> Value {
    if !args.is_empty() {
        return vm_hard_err("__dt_now: expected 0 arguments");
    }
    Value::Integer(chrono::Utc::now().timestamp())
}

fn builtin_dt_format(args: &[Value]) -> Value {
    if args.len() != 2 {
        return vm_hard_err("__dt_format: expected (timestamp, format) arguments");
    }
    let secs = match args[0].repr() {
        ValueRepr::Integer(n) => n,
        _ => return vm_hard_err("__dt_format: timestamp must be an integer (unix seconds)"),
    };
    let fmt = match vm_str_arg(&args[1]) {
        Some(f) => f,
        None => return vm_hard_err("__dt_format: format must be a string"),
    };
    match chrono::DateTime::<chrono::Utc>::from_timestamp(secs, 0) {
        Some(dt) => Value::String(rc_str(dt.format(fmt).to_string())),
        None => vm_soft_err("timestamp out of range", "datetime"),
    }
}

fn builtin_dt_parse(args: &[Value]) -> Value {
    if args.len() != 2 {
        return vm_hard_err("__dt_parse: expected (text, format) arguments");
    }
    match (vm_str_arg(&args[0]), vm_str_arg(&args[1])) {
        (Some(text), Some(fmt)) => match chrono::NaiveDateTime::parse_from_str(text, fmt) {
            Ok(ndt) => Value::Integer(ndt.and_utc().timestamp()),
            Err(e) => vm_soft_err(format!("could not parse '{}': {}", text, e), "datetime"),
        },
        _ => vm_hard_err("__dt_parse: expected (text, format) strings"),
    }
}

// ── encoding ──

fn builtin_base64_encode(args: &[Value]) -> Value {
    match args.first().and_then(vm_str_arg) {
        Some(s) if args.len() == 1 => {
            Value::String(rc_str(base64::engine::general_purpose::STANDARD.encode(s.as_bytes())))
        }
        _ => vm_hard_err("__base64_encode: expected 1 string argument"),
    }
}

fn builtin_base64_decode(args: &[Value]) -> Value {
    match args.first().and_then(vm_str_arg) {
        Some(s) if args.len() == 1 => {
            match base64::engine::general_purpose::STANDARD.decode(s.as_bytes()) {
                Ok(bytes) => match String::from_utf8(bytes) {
                    Ok(text) => Value::String(rc_str(text)),
                    Err(_) => vm_soft_err("decoded bytes are not valid UTF-8", "encoding"),
                },
                Err(e) => vm_soft_err(format!("invalid base64: {}", e), "encoding"),
            }
        }
        _ => vm_hard_err("__base64_decode: expected 1 string argument"),
    }
}

fn builtin_hex_encode(args: &[Value]) -> Value {
    match args.first().and_then(vm_str_arg) {
        Some(s) if args.len() == 1 => Value::String(rc_str(hex::encode(s.as_bytes()))),
        _ => vm_hard_err("__hex_encode: expected 1 string argument"),
    }
}

fn builtin_hex_decode(args: &[Value]) -> Value {
    match args.first().and_then(vm_str_arg) {
        Some(s) if args.len() == 1 => match hex::decode(s) {
            Ok(bytes) => match String::from_utf8(bytes) {
                Ok(text) => Value::String(rc_str(text)),
                Err(_) => vm_soft_err("decoded bytes are not valid UTF-8", "encoding"),
            },
            Err(e) => vm_soft_err(format!("invalid hex: {}", e), "encoding"),
        },
        _ => vm_hard_err("__hex_decode: expected 1 string argument"),
    }
}

fn builtin_url_encode(args: &[Value]) -> Value {
    match args.first().and_then(vm_str_arg) {
        Some(s) if args.len() == 1 => Value::String(rc_str(urlencoding::encode(s).into_owned())),
        _ => vm_hard_err("__url_encode: expected 1 string argument"),
    }
}

fn builtin_url_decode(args: &[Value]) -> Value {
    match args.first().and_then(vm_str_arg) {
        Some(s) if args.len() == 1 => match urlencoding::decode(s) {
            Ok(text) => Value::String(rc_str(text.into_owned())),
            Err(e) => vm_soft_err(format!("invalid url encoding: {}", e), "encoding"),
        },
        _ => vm_hard_err("__url_decode: expected 1 string argument"),
    }
}

// ── hash (hex digests) ──

fn builtin_sha256(args: &[Value]) -> Value {
    match args.first().and_then(vm_str_arg) {
        Some(s) if args.len() == 1 => {
            let mut h = sha2::Sha256::new();
            h.update(s.as_bytes());
            Value::String(rc_str(hex::encode(h.finalize())))
        }
        _ => vm_hard_err("__sha256: expected 1 string argument"),
    }
}

fn builtin_sha1(args: &[Value]) -> Value {
    match args.first().and_then(vm_str_arg) {
        Some(s) if args.len() == 1 => {
            let mut h = sha1::Sha1::new();
            h.update(s.as_bytes());
            Value::String(rc_str(hex::encode(h.finalize())))
        }
        _ => vm_hard_err("__sha1: expected 1 string argument"),
    }
}

fn builtin_md5(args: &[Value]) -> Value {
    match args.first().and_then(vm_str_arg) {
        Some(s) if args.len() == 1 => {
            let mut h = md5::Md5::new();
            h.update(s.as_bytes());
            Value::String(rc_str(hex::encode(h.finalize())))
        }
        _ => vm_hard_err("__md5: expected 1 string argument"),
    }
}

// ── I/O builtins ───────────────────────────────────────────────────────

fn builtin_input(args: &[Value]) -> Value {
    if !args.is_empty() {
        print!("{}", args[0]);
        use std::io::Write;
        std::io::stdout().flush().ok();
    }
    let mut line = String::new();
    match std::io::stdin().read_line(&mut line) {
        Ok(_) => Value::String(rc_str(line.trim_end_matches('\n').trim_end_matches('\r'))),
        Err(e) => Value::Error(rc_str(format!("input error: {}", e))),
    }
}

fn builtin_read_line(_args: &[Value]) -> Value {
    let mut line = String::new();
    match std::io::stdin().read_line(&mut line) {
        Ok(_) => Value::String(rc_str(line.trim_end_matches('\n').trim_end_matches('\r'))),
        Err(e) => Value::Error(rc_str(format!("read_line error: {}", e))),
    }
}

// Read a single key (arrow keys included) in raw mode; see crate::keyinput.
fn builtin_read_key(_args: &[Value]) -> Value {
    match crate::keyinput::read_key() {
        Ok(k) => Value::String(rc_str(k)),
        Err(e) => Value::Error(rc_str(format!("read_key error: {}", e))),
    }
}

// ── File I/O ───────────────────────────────────────────────────────────

fn builtin_read_file(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__read_file() takes 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::String(path) => match std::fs::read_to_string(path.as_ref()) {
            Ok(contents) => Value::String(rc_str(contents)),
            Err(e) => Value::ErrorValue(Rc::new(ErrorValueData {
                msg: rc_str(format!("{}", e)),
                tag: Some(rc_str("IO")),
            })),
        },
        _ => Value::Error(rc_str("__read_file() requires a string path")),
    }
}

fn builtin_write_file(args: &[Value]) -> Value {
    if args.len() != 2 {
        return Value::Error(rc_str("__write_file() takes 2 arguments"));
    }
    match (&args[0], &args[1]) {
        (Value::String(path), Value::String(content)) => {
            match std::fs::write(path.as_ref(), content.as_ref()) {
                Ok(_) => Value::None,
                Err(e) => Value::ErrorValue(Rc::new(ErrorValueData {
                    msg: rc_str(format!("{}", e)),
                    tag: Some(rc_str("IO")),
                })),
            }
        }
        _ => Value::Error(rc_str("__write_file() requires (string, string)")),
    }
}

fn builtin_append_file(args: &[Value]) -> Value {
    if args.len() != 2 {
        return Value::Error(rc_str("__append_file() takes 2 arguments"));
    }
    match (&args[0], &args[1]) {
        (Value::String(path), Value::String(content)) => {
            use std::io::Write;
            match std::fs::OpenOptions::new()
                .append(true)
                .create(true)
                .open(path.as_ref())
            {
                Ok(mut f) => match f.write_all(content.as_bytes()) {
                    Ok(_) => Value::None,
                    Err(e) => Value::ErrorValue(Rc::new(ErrorValueData {
                        msg: rc_str(format!("{}", e)),
                        tag: Some(rc_str("IO")),
                    })),
                },
                Err(e) => Value::ErrorValue(Rc::new(ErrorValueData {
                    msg: rc_str(format!("{}", e)),
                    tag: Some(rc_str("IO")),
                })),
            }
        }
        _ => Value::Error(rc_str("__append_file() requires (string, string)")),
    }
}

fn builtin_file_exists(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__file_exists() takes 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::String(path) => Value::Boolean(std::path::Path::new(path.as_ref()).exists()),
        _ => Value::Error(rc_str("__file_exists() requires a string")),
    }
}

// ── OS builtins ────────────────────────────────────────────────────────

fn builtin_exec(args: &[Value]) -> Value {
    if args.is_empty() {
        return Value::Error(rc_str("__exec() takes at least 1 argument"));
    }
    let cmd = match args[0].repr() {
        ValueRepr::String(s) => s.to_string(),
        _ => return Value::Error(rc_str("__exec() requires a string command")),
    };
    let extra_args: Vec<String> = args[1..].iter().map(|a| format!("{}", a)).collect();
    let full_cmd = if extra_args.is_empty() {
        cmd
    } else {
        format!("{} {}", cmd, extra_args.join(" "))
    };

    let shell = if cfg!(target_os = "windows") {
        "cmd"
    } else {
        "sh"
    };
    let flag = if cfg!(target_os = "windows") {
        "/C"
    } else {
        "-c"
    };

    match std::process::Command::new(shell)
        .arg(flag)
        .arg(&full_cmd)
        .output()
    {
        Ok(output) => {
            let stdout = String::from_utf8_lossy(&output.stdout).to_string();
            let stderr = String::from_utf8_lossy(&output.stderr).to_string();
            let code = output.status.code().unwrap_or(-1);
            let entries: Vec<(Value, Value)> = vec![
                (
                    Value::String(rc_str("stdout")),
                    Value::String(rc_str(stdout)),
                ),
                (
                    Value::String(rc_str("stderr")),
                    Value::String(rc_str(stderr)),
                ),
                (Value::String(rc_str("code")), Value::Integer(code as i64)),
            ];
            Value::Map(Rc::new(RefCell::new(
                crate::vm::collections::OxMap::from_pairs(entries),
            )))
        }
        Err(e) => Value::Error(rc_str(format!("__exec: {}", e))),
    }
}

fn builtin_os_name(_args: &[Value]) -> Value {
    Value::String(rc_str(std::env::consts::OS))
}

fn builtin_os_arch(_args: &[Value]) -> Value {
    Value::String(rc_str(std::env::consts::ARCH))
}

fn builtin_env_get(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__env_get() takes 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::String(name) => match std::env::var(name.as_ref()) {
            Ok(val) => Value::String(rc_str(val)),
            Err(_) => Value::None,
        },
        _ => Value::Error(rc_str("__env_get() requires a string")),
    }
}

fn builtin_env_set(args: &[Value]) -> Value {
    if args.len() != 2 {
        return Value::Error(rc_str("__env_set() takes 2 arguments"));
    }
    match (&args[0], &args[1]) {
        (Value::String(name), Value::String(val)) => {
            // SAFETY: We don't access env vars concurrently from other threads.
            unsafe {
                std::env::set_var(name.as_ref(), val.as_ref());
            }
            Value::None
        }
        _ => Value::Error(rc_str("__env_set() requires (string, string)")),
    }
}

fn builtin_env_vars(_args: &[Value]) -> Value {
    let entries: Vec<(Value, Value)> = std::env::vars()
        .map(|(k, v)| (Value::String(rc_str(k)), Value::String(rc_str(v))))
        .collect();
    Value::Map(Rc::new(RefCell::new(
                crate::vm::collections::OxMap::from_pairs(entries),
            )))
}

fn builtin_cwd(_args: &[Value]) -> Value {
    match std::env::current_dir() {
        Ok(path) => Value::String(rc_str(path.display().to_string())),
        Err(e) => Value::Error(rc_str(format!("cwd error: {}", e))),
    }
}

fn builtin_chdir(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__chdir() takes 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::String(path) => match std::env::set_current_dir(path.as_ref()) {
            Ok(_) => Value::None,
            Err(e) => Value::Error(rc_str(format!("chdir error: {}", e))),
        },
        _ => Value::Error(rc_str("__chdir() requires a string")),
    }
}

fn builtin_exit(args: &[Value]) -> Value {
    let code = if args.is_empty() {
        0
    } else {
        match args[0].repr() {
            ValueRepr::Integer(n) => n as i32,
            _ => 1,
        }
    };
    std::process::exit(code);
}

fn builtin_pid(_args: &[Value]) -> Value {
    Value::Integer(std::process::id() as i64)
}

fn builtin_list_dir(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__list_dir() takes 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::String(path) => match std::fs::read_dir(path.as_ref()) {
            Ok(entries) => {
                let items: Vec<Value> = entries
                    .filter_map(|e| e.ok())
                    .map(|e| Value::String(rc_str(e.path().to_string_lossy())))
                    .collect();
                Value::Array(Rc::new(RefCell::new(items)))
            }
            Err(e) => Value::ErrorValue(Rc::new(ErrorValueData {
                msg: rc_str(format!("{}", e)),
                tag: Some(rc_str("IO")),
            })),
        },
        _ => Value::Error(rc_str("__list_dir() requires a string")),
    }
}

fn builtin_walk_dir(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__walk_dir() takes 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::String(path) => {
            let mut items = Vec::new();
            fn walk(dir: &std::path::Path, items: &mut Vec<Value>) {
                if let Ok(entries) = std::fs::read_dir(dir) {
                    for entry in entries.filter_map(|e| e.ok()) {
                        let path = entry.path();
                        items.push(Value::String(rc_str(path.display().to_string())));
                        if path.is_dir() {
                            walk(&path, items);
                        }
                    }
                }
            }
            walk(std::path::Path::new(path.as_ref()), &mut items);
            Value::Array(Rc::new(RefCell::new(items)))
        }
        _ => Value::Error(rc_str("__walk_dir() requires a string")),
    }
}

fn builtin_mkdir(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__mkdir() takes 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::String(path) => match std::fs::create_dir_all(path.as_ref()) {
            Ok(_) => Value::None,
            Err(e) => Value::ErrorValue(Rc::new(ErrorValueData {
                msg: rc_str(format!("{}", e)),
                tag: Some(rc_str("IO")),
            })),
        },
        _ => Value::Error(rc_str("__mkdir() requires a string")),
    }
}

fn builtin_rmdir(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__rmdir() takes 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::String(path) => match std::fs::remove_dir_all(path.as_ref()) {
            Ok(_) => Value::None,
            Err(e) => Value::ErrorValue(Rc::new(ErrorValueData {
                msg: rc_str(format!("{}", e)),
                tag: Some(rc_str("IO")),
            })),
        },
        _ => Value::Error(rc_str("__rmdir() requires a string")),
    }
}

fn builtin_remove_file(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__remove() takes 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::String(path) => match std::fs::remove_file(path.as_ref()) {
            Ok(_) => Value::None,
            Err(e) => Value::ErrorValue(Rc::new(ErrorValueData {
                msg: rc_str(format!("{}", e)),
                tag: Some(rc_str("IO")),
            })),
        },
        _ => Value::Error(rc_str("__remove() requires a string")),
    }
}

fn builtin_is_dir(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__is_dir() takes 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::String(path) => Value::Boolean(std::path::Path::new(path.as_ref()).is_dir()),
        _ => Value::Error(rc_str("__is_dir() requires a string")),
    }
}

fn builtin_is_file(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__is_file() takes 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::String(path) => Value::Boolean(std::path::Path::new(path.as_ref()).is_file()),
        _ => Value::Error(rc_str("__is_file() requires a string")),
    }
}

// ── Time builtins ──────────────────────────────────────────────────────

fn builtin_time_now(_args: &[Value]) -> Value {
    use std::time::{SystemTime, UNIX_EPOCH};
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(d) => Value::Float(d.as_secs_f64()),
        Err(_) => Value::Error(rc_str("time error")),
    }
}

fn builtin_time_now_ms(_args: &[Value]) -> Value {
    use std::time::{SystemTime, UNIX_EPOCH};
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(d) => Value::Integer(d.as_millis() as i64),
        Err(_) => Value::Error(rc_str("time error")),
    }
}

fn builtin_time_sleep(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__time_sleep() takes 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::Integer(ms) => {
            std::thread::sleep(std::time::Duration::from_millis(ms as u64));
            Value::None
        }
        _ => Value::Error(rc_str("__time_sleep() requires an integer (ms)")),
    }
}

fn builtin_time_monotonic(_args: &[Value]) -> Value {
    use std::time::Instant;
    // Return monotonic time as nanoseconds integer (matches tree-walker)
    thread_local! {
        static EPOCH: Instant = Instant::now();
    }
    EPOCH.with(|epoch| Value::Integer(epoch.elapsed().as_nanos() as i64))
}

// ── Random builtins ────────────────────────────────────────────────────

thread_local! {
    static RNG_STATE: std::cell::Cell<u64> = const { std::cell::Cell::new(0) };
}

fn xorshift64(state: u64) -> u64 {
    let mut s = if state == 0 {
        // Auto-seed from time
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos() as u64
            | 1
    } else {
        state
    };
    s ^= s << 13;
    s ^= s >> 7;
    s ^= s << 17;
    s
}

fn next_rand() -> u64 {
    RNG_STATE.with(|state| {
        let s = xorshift64(state.get());
        state.set(s);
        s
    })
}

fn builtin_rand_seed(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__rand_seed() takes 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::Integer(n) => {
            let seed = if n == 0 { 1u64 } else { n as u64 };
            RNG_STATE.with(|state| state.set(seed));
            Value::None
        }
        _ => Value::Error(rc_str("__rand_seed() requires an integer")),
    }
}

fn builtin_rand_int(args: &[Value]) -> Value {
    if args.len() != 2 {
        return Value::Error(rc_str("__rand_int() takes 2 arguments"));
    }
    match (&args[0], &args[1]) {
        (Value::Integer(min), Value::Integer(max)) => {
            let range = max - min;
            if range <= 0 {
                return Value::Integer(*min);
            }
            let r = next_rand();
            Value::Integer(min + (r as i64).unsigned_abs() as i64 % range)
        }
        _ => Value::Error(rc_str("__rand_int() requires integer arguments")),
    }
}

fn builtin_rand_float(_args: &[Value]) -> Value {
    let r = next_rand();
    Value::Float((r as f64) / (u64::MAX as f64))
}

// ── Path builtins ──────────────────────────────────────────────────────

fn builtin_path_join(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__path_join() takes 1 argument (array)"));
    }
    match args[0].repr() {
        ValueRepr::Array(parts) => {
            let borrowed = parts.borrow();
            let mut path = std::path::PathBuf::new();
            for part in borrowed.iter() {
                path.push(format!("{}", part));
            }
            Value::String(rc_str(path.display().to_string()))
        }
        _ => Value::Error(rc_str("__path_join() requires an array")),
    }
}

fn builtin_path_ext(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__path_ext() takes 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::String(path) => {
            let p = std::path::Path::new(path.as_ref());
            match p.extension() {
                Some(ext) => Value::String(rc_str(ext.to_string_lossy())),
                None => Value::String(rc_str("")),
            }
        }
        _ => Value::Error(rc_str("__path_ext() requires a string")),
    }
}

fn builtin_path_filename(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__path_filename() takes 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::String(path) => {
            let p = std::path::Path::new(path.as_ref());
            match p.file_name() {
                Some(name) => Value::String(rc_str(name.to_string_lossy())),
                None => Value::String(rc_str("")),
            }
        }
        _ => Value::Error(rc_str("__path_filename() requires a string")),
    }
}

fn builtin_path_parent(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__path_parent() takes 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::String(path) => {
            let p = std::path::Path::new(path.as_ref());
            match p.parent() {
                Some(parent) => Value::String(rc_str(parent.display().to_string())),
                None => Value::String(rc_str("")),
            }
        }
        _ => Value::Error(rc_str("__path_parent() requires a string")),
    }
}

fn builtin_path_stem(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__path_stem() takes 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::String(path) => {
            let p = std::path::Path::new(path.as_ref());
            match p.file_stem() {
                Some(stem) => Value::String(rc_str(stem.to_string_lossy())),
                None => Value::String(rc_str("")),
            }
        }
        _ => Value::Error(rc_str("__path_stem() requires a string")),
    }
}

fn builtin_path_is_absolute(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__path_is_absolute() takes 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::String(path) => Value::Boolean(std::path::Path::new(path.as_ref()).is_absolute()),
        _ => Value::Error(rc_str("__path_is_absolute() requires a string")),
    }
}

// ── JSON builtins ──────────────────────────────────────────────────────

fn builtin_json_parse(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__json_parse() takes 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::String(s) => {
            let chars: Vec<char> = s.chars().collect();
            let mut pos = 0;
            match json_parse_value(&chars, &mut pos) {
                Ok(val) => val,
                Err(e) => Value::Error(rc_str(format!("json parse error: {}", e))),
            }
        }
        _ => Value::Error(rc_str("__json_parse() requires a string")),
    }
}

fn json_skip_ws(chars: &[char], pos: &mut usize) {
    while *pos < chars.len() && chars[*pos].is_ascii_whitespace() {
        *pos += 1;
    }
}

fn json_parse_value(chars: &[char], pos: &mut usize) -> Result<Value, String> {
    json_skip_ws(chars, pos);
    if *pos >= chars.len() {
        return Err("unexpected end of JSON".to_string());
    }
    match chars[*pos] {
        '"' => json_parse_string(chars, pos),
        't' | 'f' => json_parse_bool(chars, pos),
        'n' => json_parse_null(chars, pos),
        '[' => json_parse_array(chars, pos),
        '{' => json_parse_object(chars, pos),
        c if c == '-' || c.is_ascii_digit() => json_parse_number(chars, pos),
        c => Err(format!("unexpected character '{}' at position {}", c, pos)),
    }
}

fn json_parse_string(chars: &[char], pos: &mut usize) -> Result<Value, String> {
    *pos += 1; // skip opening "
    let mut s = String::new();
    while *pos < chars.len() && chars[*pos] != '"' {
        if chars[*pos] == '\\' {
            *pos += 1;
            if *pos >= chars.len() {
                return Err("unterminated string escape".to_string());
            }
            match chars[*pos] {
                '"' => s.push('"'),
                '\\' => s.push('\\'),
                '/' => s.push('/'),
                'n' => s.push('\n'),
                'r' => s.push('\r'),
                't' => s.push('\t'),
                'b' => s.push('\u{0008}'),
                'f' => s.push('\u{000C}'),
                'u' => {
                    *pos += 1;
                    if *pos + 4 > chars.len() {
                        return Err("incomplete unicode escape".to_string());
                    }
                    let hex: String = chars[*pos..*pos + 4].iter().collect();
                    *pos += 3; // will be incremented once more below
                    let cp = u32::from_str_radix(&hex, 16)
                        .map_err(|_| format!("invalid unicode escape: {}", hex))?;
                    if let Some(c) = char::from_u32(cp) {
                        s.push(c);
                    }
                }
                c => s.push(c),
            }
        } else {
            s.push(chars[*pos]);
        }
        *pos += 1;
    }
    if *pos >= chars.len() {
        return Err("unterminated string".to_string());
    }
    *pos += 1; // skip closing "
    Ok(Value::String(rc_str(s)))
}

fn json_parse_number(chars: &[char], pos: &mut usize) -> Result<Value, String> {
    let start = *pos;
    let mut is_float = false;
    if chars[*pos] == '-' {
        *pos += 1;
    }
    while *pos < chars.len() && chars[*pos].is_ascii_digit() {
        *pos += 1;
    }
    if *pos < chars.len() && chars[*pos] == '.' {
        is_float = true;
        *pos += 1;
        while *pos < chars.len() && chars[*pos].is_ascii_digit() {
            *pos += 1;
        }
    }
    if *pos < chars.len() && (chars[*pos] == 'e' || chars[*pos] == 'E') {
        is_float = true;
        *pos += 1;
        if *pos < chars.len() && (chars[*pos] == '+' || chars[*pos] == '-') {
            *pos += 1;
        }
        while *pos < chars.len() && chars[*pos].is_ascii_digit() {
            *pos += 1;
        }
    }
    let num_str: String = chars[start..*pos].iter().collect();
    if is_float {
        num_str
            .parse::<f64>()
            .map(Value::Float)
            .map_err(|e| format!("invalid number: {}", e))
    } else {
        num_str
            .parse::<i64>()
            .map(Value::Integer)
            .map_err(|e| format!("invalid number: {}", e))
    }
}

fn json_parse_bool(chars: &[char], pos: &mut usize) -> Result<Value, String> {
    if chars[*pos..].starts_with(&['t', 'r', 'u', 'e']) {
        *pos += 4;
        Ok(Value::Boolean(true))
    } else if chars[*pos..].starts_with(&['f', 'a', 'l', 's', 'e']) {
        *pos += 5;
        Ok(Value::Boolean(false))
    } else {
        Err(format!("unexpected token at position {}", pos))
    }
}

fn json_parse_null(chars: &[char], pos: &mut usize) -> Result<Value, String> {
    if chars[*pos..].starts_with(&['n', 'u', 'l', 'l']) {
        *pos += 4;
        Ok(Value::None)
    } else {
        Err(format!("unexpected token at position {}", pos))
    }
}

fn json_parse_array(chars: &[char], pos: &mut usize) -> Result<Value, String> {
    *pos += 1; // skip [
    let mut elements = Vec::new();
    json_skip_ws(chars, pos);
    if *pos < chars.len() && chars[*pos] == ']' {
        *pos += 1;
        return Ok(Value::Array(Rc::new(RefCell::new(elements))));
    }
    loop {
        elements.push(json_parse_value(chars, pos)?);
        json_skip_ws(chars, pos);
        if *pos >= chars.len() {
            return Err("unterminated array".to_string());
        }
        if chars[*pos] == ']' {
            *pos += 1;
            break;
        }
        if chars[*pos] != ',' {
            return Err(format!("expected ',' or ']' at {}", pos));
        }
        *pos += 1;
    }
    Ok(Value::Array(Rc::new(RefCell::new(elements))))
}

fn json_parse_object(chars: &[char], pos: &mut usize) -> Result<Value, String> {
    *pos += 1; // skip {
    let mut entries = Vec::new();
    json_skip_ws(chars, pos);
    if *pos < chars.len() && chars[*pos] == '}' {
        *pos += 1;
        return Ok(Value::Map(Rc::new(RefCell::new(
                crate::vm::collections::OxMap::from_pairs(entries),
            ))));
    }
    loop {
        json_skip_ws(chars, pos);
        let key = json_parse_string(chars, pos)?;
        json_skip_ws(chars, pos);
        if *pos >= chars.len() || chars[*pos] != ':' {
            return Err(format!("expected ':' at {}", pos));
        }
        *pos += 1;
        let value = json_parse_value(chars, pos)?;
        entries.push((key, value));
        json_skip_ws(chars, pos);
        if *pos >= chars.len() {
            return Err("unterminated object".to_string());
        }
        if chars[*pos] == '}' {
            *pos += 1;
            break;
        }
        if chars[*pos] != ',' {
            return Err(format!("expected ',' or '}}' at {}", pos));
        }
        *pos += 1;
    }
    Ok(Value::Map(Rc::new(RefCell::new(
                crate::vm::collections::OxMap::from_pairs(entries),
            ))))
}

fn builtin_json_stringify(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__json_stringify() takes 1 argument"));
    }
    match value_to_json(&args[0]) {
        Ok(s) => Value::String(rc_str(s)),
        Err(e) => Value::Error(rc_str(format!("json stringify error: {}", e))),
    }
}

fn value_to_json(val: &Value) -> Result<String, String> {
    match val.repr() {
        ValueRepr::None => Ok("null".to_string()),
        ValueRepr::Boolean(b) => Ok(if b { "true" } else { "false" }.to_string()),
        ValueRepr::Integer(n) => Ok(n.to_string()),
        ValueRepr::Float(f) => {
            if f.is_nan() || f.is_infinite() {
                Ok("null".to_string())
            } else {
                Ok(f.to_string())
            }
        }
        ValueRepr::String(s) => {
            let mut out = String::from('"');
            for c in s.chars() {
                match c {
                    '"' => out.push_str("\\\""),
                    '\\' => out.push_str("\\\\"),
                    '\n' => out.push_str("\\n"),
                    '\r' => out.push_str("\\r"),
                    '\t' => out.push_str("\\t"),
                    c if c < '\x20' => out.push_str(&format!("\\u{:04x}", c as u32)),
                    c => out.push(c),
                }
            }
            out.push('"');
            Ok(out)
        }
        ValueRepr::Array(arr) => {
            let items: Result<Vec<String>, String> =
                arr.borrow().iter().map(|e| value_to_json(e)).collect();
            Ok(format!("[{}]", items?.join(",")))
        }
        ValueRepr::Map(entries) => {
            let items: Result<Vec<String>, String> = entries
                .borrow()
                .iter()
                .map(|(k, v)| {
                    let kj = value_to_json(k)?;
                    let vj = value_to_json(v)?;
                    Ok(format!("{}:{}", kj, vj))
                })
                .collect();
            Ok(format!("{{{}}}", items?.join(",")))
        }
        ValueRepr::Tuple(t) => {
            let items: Result<Vec<String>, String> = t.iter().map(|e| value_to_json(e)).collect();
            Ok(format!("[{}]", items?.join(",")))
        }
        ValueRepr::Byte(n) => Ok(n.to_string()),
        ValueRepr::Uint(n) => Ok(n.to_string()),
        ValueRepr::Char(c) => Ok(format!("\"{}\"", c)),
        _ => Err(format!("cannot serialize {} to JSON", val.type_name())),
    }
}

// ── TOML builtins ──────────────────────────────────────────────────────

fn builtin_toml_parse(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__toml_parse() takes 1 argument"));
    }
    match args[0].repr() {
        ValueRepr::String(s) => match toml::from_str::<toml::Value>(s.as_ref()) {
            Ok(value) => toml_value_to_value(value),
            Err(e) => Value::Error(rc_str(format!("toml parse error: {}", e))),
        },
        _ => Value::Error(rc_str("__toml_parse() requires a string")),
    }
}

fn toml_value_to_value(tv: toml::Value) -> Value {
    match tv {
        toml::Value::String(s) => Value::String(rc_str(s)),
        toml::Value::Integer(i) => Value::Integer(i),
        toml::Value::Float(f) => Value::Float(f),
        toml::Value::Boolean(b) => Value::Boolean(b),
        toml::Value::Datetime(dt) => Value::String(rc_str(dt.to_string())),
        toml::Value::Array(values) => {
            let items: Vec<Value> = values.into_iter().map(toml_value_to_value).collect();
            Value::Array(Rc::new(RefCell::new(items)))
        }
        toml::Value::Table(entries) => {
            let items: Vec<(Value, Value)> = entries
                .into_iter()
                .map(|(k, v)| (Value::String(rc_str(k)), toml_value_to_value(v)))
                .collect();
            Value::Map(Rc::new(RefCell::new(
                crate::vm::collections::OxMap::from_pairs(items),
            )))
        }
    }
}

fn builtin_toml_stringify(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("__toml_stringify() takes 1 argument"));
    }
    match value_to_toml(&args[0]) {
        Ok(tv) => match toml::to_string_pretty(&tv) {
            Ok(s) => Value::String(rc_str(s)),
            Err(e) => Value::Error(rc_str(format!("toml stringify error: {}", e))),
        },
        Err(e) => Value::Error(rc_str(format!("toml stringify error: {}", e))),
    }
}

fn value_to_toml(val: &Value) -> Result<toml::Value, String> {
    match val.repr() {
        ValueRepr::String(s) => Ok(toml::Value::String(s.to_string())),
        ValueRepr::Integer(n) => Ok(toml::Value::Integer(n)),
        ValueRepr::Float(f) => {
            if f.is_nan() || f.is_infinite() {
                Err("TOML does not support NaN or Infinity".to_string())
            } else {
                Ok(toml::Value::Float(f))
            }
        }
        ValueRepr::Boolean(b) => Ok(toml::Value::Boolean(b)),
        ValueRepr::Uint(n) => Ok(toml::Value::Integer(n as i64)),
        ValueRepr::Byte(n) => Ok(toml::Value::Integer(n as i64)),
        ValueRepr::Char(c) => Ok(toml::Value::String(c.to_string())),
        ValueRepr::Array(arr) => {
            let items: Result<Vec<toml::Value>, String> =
                arr.borrow().iter().map(|e| value_to_toml(e)).collect();
            Ok(toml::Value::Array(items?))
        }
        ValueRepr::Tuple(t) => {
            let items: Result<Vec<toml::Value>, String> =
                t.iter().map(|e| value_to_toml(e)).collect();
            Ok(toml::Value::Array(items?))
        }
        ValueRepr::Map(entries) => {
            let mut table = toml::map::Map::new();
            for (k, v) in entries.borrow().iter() {
                let key = match k.repr() {
                    ValueRepr::String(s) => s.to_string(),
                    _ => return Err(format!("TOML keys must be strings, got {}", k.type_name())),
                };
                table.insert(key, value_to_toml(v)?);
            }
            Ok(toml::Value::Table(table))
        }
        ValueRepr::None => Err("TOML does not support null/None".to_string()),
        _ => Err(format!("cannot serialize {} to TOML", val.type_name())),
    }
}

// ── HTTP builtins ──────────────────────────────────────────────────────

fn builtin_http_request(args: &[Value]) -> Value {
    if args.len() < 2 || args.len() > 4 {
        return Value::Error(rc_str(
            "__http_request: expected 2-4 arguments (method, url, headers?, body?)",
        ));
    }
    let method = match args[0].repr() {
        ValueRepr::String(s) => s.to_uppercase(),
        _ => {
            return Value::Error(
                format!(
                    "__http_request: method must be STRING, got {}",
                    args[0].type_name()
                )
                .into(),
            );
        }
    };
    let url = match args[1].repr() {
        ValueRepr::String(s) => s.to_string(),
        _ => {
            return Value::Error(
                format!(
                    "__http_request: url must be STRING, got {}",
                    args[1].type_name()
                )
                .into(),
            );
        }
    };

    let headers: Vec<(String, String)> = if args.len() >= 3 {
        if let Value::Map(m) = &args[2] {
            m.borrow()
                .iter()
                .filter_map(|(k, v)| {
                    if let (Value::String(key), Value::String(val)) = (k, v) {
                        Some((key.to_string(), val.to_string()))
                    } else {
                        None
                    }
                })
                .collect()
        } else {
            Vec::new()
        }
    } else {
        Vec::new()
    };

    let body: Option<String> = if args.len() >= 4 {
        match args[3].repr() {
            ValueRepr::String(s) => Some(s.to_string()),
            ValueRepr::None => None,
            _ => {
                return Value::Error(
                    format!(
                        "__http_request: body must be STRING, got {}",
                        args[3].type_name()
                    )
                    .into(),
                );
            }
        }
    } else {
        None
    };

    macro_rules! apply_headers {
        ($req:expr) => {{
            let mut r = $req;
            for (k, v) in &headers {
                r = r.header(k.as_str(), v.as_str());
            }
            r
        }};
    }

    macro_rules! handle_response {
        ($result:expr) => {
            match $result {
                Ok(resp) => {
                    let status = resp.status().as_u16();
                    let (_, mut resp_body) = resp.into_parts();
                    let body_str = resp_body.read_to_string().unwrap_or_default();
                    let entries: Vec<(Value, Value)> = vec![
                        (
                            Value::String(rc_str("status")),
                            Value::Integer(status as i64),
                        ),
                        (
                            Value::String(rc_str("body")),
                            Value::String(rc_str(body_str)),
                        ),
                    ];
                    Value::Map(Rc::new(RefCell::new(
                crate::vm::collections::OxMap::from_pairs(entries),
            )))
                }
                Err(e) => Value::Error(rc_str(format!("http error: {}", e))),
            }
        };
    }

    match method.as_str() {
        "GET" => handle_response!(apply_headers!(ureq::get(&url)).call()),
        "HEAD" => handle_response!(apply_headers!(ureq::head(&url)).call()),
        "DELETE" => handle_response!(apply_headers!(ureq::delete(&url)).call()),
        "POST" => {
            let req = apply_headers!(ureq::post(&url));
            handle_response!(match &body {
                Some(b) => req.send(b.as_bytes()),
                None => req.send_empty(),
            })
        }
        "PUT" => {
            let req = apply_headers!(ureq::put(&url));
            handle_response!(match &body {
                Some(b) => req.send(b.as_bytes()),
                None => req.send_empty(),
            })
        }
        "PATCH" => {
            let req = apply_headers!(ureq::patch(&url));
            handle_response!(match &body {
                Some(b) => req.send(b.as_bytes()),
                None => req.send_empty(),
            })
        }
        _ => Value::Error(rc_str(format!(
            "__http_request: unsupported method '{}'",
            method
        ))),
    }
}

// ── Streaming / socket networking ───────────────────────────────────────

fn net_str(arg: &Value, name: &str) -> Result<String, Value> {
    match arg.repr() {
        ValueRepr::String(s) => Ok(s.to_string()),
        _ => Err(Value::Error(rc_str(format!(
            "{}: expected STRING, got {}",
            name,
            arg.type_name()
        )))),
    }
}

fn net_int(arg: &Value, name: &str) -> Result<i64, Value> {
    match arg.repr() {
        ValueRepr::Integer(n) => Ok(n),
        _ => Err(Value::Error(rc_str(format!(
            "{}: expected INTEGER, got {}",
            name,
            arg.type_name()
        )))),
    }
}

fn net_headers(arg: Option<&Value>) -> Vec<(String, String)> {
    match arg {
        Some(Value::Map(m)) => m
            .borrow()
            .iter()
            .filter_map(|(k, v)| match (k, v) {
                (Value::String(key), Value::String(val)) => {
                    Some((key.to_string(), val.to_string()))
                }
                _ => None,
            })
            .collect(),
        _ => Vec::new(),
    }
}

macro_rules! net_try {
    ($e:expr) => {
        match $e {
            Ok(v) => v,
            Err(err) => return err,
        }
    };
}

fn builtin_net_download(args: &[Value]) -> Value {
    if args.len() != 2 {
        return Value::Error(rc_str("download: expected (url, path)"));
    }
    let url = net_try!(net_str(&args[0], "download url"));
    let path = net_try!(net_str(&args[1], "download path"));
    match crate::netres::http_download(&url, &path) {
        Ok(status) => Value::Integer(status),
        Err(e) => Value::Error(rc_str(e)),
    }
}

fn builtin_net_upload(args: &[Value]) -> Value {
    if args.len() < 3 || args.len() > 4 {
        return Value::Error(rc_str("upload: expected (method, url, file_path, headers?)"));
    }
    let method = net_try!(net_str(&args[0], "upload method"));
    let url = net_try!(net_str(&args[1], "upload url"));
    let file_path = net_try!(net_str(&args[2], "upload file_path"));
    let headers = net_headers(args.get(3));
    match crate::netres::http_upload(&method, &url, &file_path, headers) {
        Ok((status, body)) => {
            let entries = vec![
                (Value::String(rc_str("status")), Value::Integer(status)),
                (Value::String(rc_str("body")), Value::String(rc_str(body))),
            ];
            Value::Map(Rc::new(RefCell::new(
                crate::vm::collections::OxMap::from_pairs(entries),
            )))
        }
        Err(e) => Value::Error(rc_str(e)),
    }
}

fn builtin_net_http_open(args: &[Value]) -> Value {
    if args.len() != 4 {
        return Value::Error(rc_str("open_stream: expected (method, url, headers, body)"));
    }
    let method = net_try!(net_str(&args[0], "open_stream method"));
    let url = net_try!(net_str(&args[1], "open_stream url"));
    let headers = net_headers(Some(&args[2]));
    let body = net_try!(net_str(&args[3], "open_stream body"));
    let body = if body.is_empty() { None } else { Some(body.as_str()) };
    match crate::netres::http_open(&method, &url, &headers, body) {
        Ok(id) => Value::Integer(id as i64),
        Err(e) => Value::Error(rc_str(e)),
    }
}

fn builtin_net_http_read_line(args: &[Value]) -> Value {
    if args.len() != 2 {
        return Value::Error(rc_str("read_line: expected (stream, timeout_ms)"));
    }
    let id = net_try!(net_int(&args[0], "read_line stream"));
    let timeout = net_try!(net_int(&args[1], "read_line timeout_ms"));
    match crate::netres::http_read_line(id as u64, timeout) {
        Ok(Some(s)) => Value::String(rc_str(s)),
        Ok(None) => Value::None,
        Err(e) => Value::Error(rc_str(e)),
    }
}

fn builtin_net_http_read(args: &[Value]) -> Value {
    if args.len() != 3 {
        return Value::Error(rc_str("read_chunk: expected (stream, max, timeout_ms)"));
    }
    let id = net_try!(net_int(&args[0], "read_chunk stream"));
    let max = net_try!(net_int(&args[1], "read_chunk max"));
    let timeout = net_try!(net_int(&args[2], "read_chunk timeout_ms"));
    match crate::netres::http_read(id as u64, max, timeout) {
        // None = timed out with no data yet (only when timeout_ms > 0); the
        // caller distinguishes this from "" (EOF) and an error value.
        Ok(Some(s)) => Value::String(rc_str(s)),
        Ok(None) => Value::None,
        Err(e) => Value::Error(rc_str(e)),
    }
}

fn builtin_net_tcp_connect(args: &[Value]) -> Value {
    if args.len() != 2 {
        return Value::Error(rc_str("connect: expected (host, port)"));
    }
    let host = net_try!(net_str(&args[0], "connect host"));
    let port = net_try!(net_int(&args[1], "connect port"));
    match crate::netres::tcp_connect(&host, port) {
        Ok(id) => Value::Integer(id as i64),
        Err(e) => Value::Error(rc_str(e)),
    }
}

fn builtin_net_tcp_listen(args: &[Value]) -> Value {
    if args.len() != 2 {
        return Value::Error(rc_str("listen: expected (host, port)"));
    }
    let host = net_try!(net_str(&args[0], "listen host"));
    let port = net_try!(net_int(&args[1], "listen port"));
    match crate::netres::tcp_listen(&host, port) {
        Ok(id) => Value::Integer(id as i64),
        Err(e) => Value::Error(rc_str(e)),
    }
}

fn builtin_net_tcp_accept(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("accept: expected (server)"));
    }
    let id = net_try!(net_int(&args[0], "accept server"));
    match crate::netres::tcp_accept(id as u64) {
        Ok(conn) => Value::Integer(conn as i64),
        Err(e) => Value::Error(rc_str(e)),
    }
}

fn builtin_net_send(args: &[Value]) -> Value {
    if args.len() != 2 {
        return Value::Error(rc_str("send: expected (conn, data)"));
    }
    let id = net_try!(net_int(&args[0], "send conn"));
    let data = net_try!(net_str(&args[1], "send data"));
    match crate::netres::tcp_send(id as u64, &data) {
        Ok(n) => Value::Integer(n),
        Err(e) => Value::Error(rc_str(e)),
    }
}

fn builtin_net_receive(args: &[Value]) -> Value {
    if args.len() != 2 {
        return Value::Error(rc_str("receive: expected (conn, max)"));
    }
    let id = net_try!(net_int(&args[0], "receive conn"));
    let max = net_try!(net_int(&args[1], "receive max"));
    match crate::netres::tcp_receive(id as u64, max) {
        Ok(s) => Value::String(rc_str(s)),
        Err(e) => Value::Error(rc_str(e)),
    }
}

fn builtin_net_close(args: &[Value]) -> Value {
    if args.len() != 1 {
        return Value::Error(rc_str("close: expected (handle)"));
    }
    let id = net_try!(net_int(&args[0], "close handle"));
    crate::netres::close(id as u64);
    Value::None
}

fn builtin_net_udp_bind(args: &[Value]) -> Value {
    if args.len() != 2 {
        return Value::Error(rc_str("udp_bind: expected (host, port)"));
    }
    let host = net_try!(net_str(&args[0], "udp_bind host"));
    let port = net_try!(net_int(&args[1], "udp_bind port"));
    match crate::netres::udp_bind(&host, port) {
        Ok(id) => Value::Integer(id as i64),
        Err(e) => Value::Error(rc_str(e)),
    }
}

fn builtin_net_udp_send(args: &[Value]) -> Value {
    if args.len() != 4 {
        return Value::Error(rc_str("udp_send: expected (sock, data, host, port)"));
    }
    let id = net_try!(net_int(&args[0], "udp_send sock"));
    let data = net_try!(net_str(&args[1], "udp_send data"));
    let host = net_try!(net_str(&args[2], "udp_send host"));
    let port = net_try!(net_int(&args[3], "udp_send port"));
    match crate::netres::udp_send(id as u64, &data, &host, port) {
        Ok(n) => Value::Integer(n),
        Err(e) => Value::Error(rc_str(e)),
    }
}

fn builtin_net_udp_receive(args: &[Value]) -> Value {
    if args.len() != 2 {
        return Value::Error(rc_str("udp_receive: expected (sock, max)"));
    }
    let id = net_try!(net_int(&args[0], "udp_receive sock"));
    let max = net_try!(net_int(&args[1], "udp_receive max"));
    match crate::netres::udp_receive(id as u64, max) {
        Ok((data, addr)) => Value::Tuple(Rc::new(vec![
            Value::String(rc_str(data)),
            Value::String(rc_str(addr)),
        ])),
        Err(e) => Value::Error(rc_str(e)),
    }
}
