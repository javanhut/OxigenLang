use crate::vm::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Register all built-in functions into a globals map.
pub fn register_builtins(globals: &mut HashMap<String, Value>) {
    globals.insert("print".to_string(), Value::Builtin(builtin_print));
    globals.insert("println".to_string(), Value::Builtin(builtin_println));
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
    globals.insert("__starts_with".to_string(), Value::Builtin(builtin_starts_with));
    globals.insert("__ends_with".to_string(), Value::Builtin(builtin_ends_with));
    globals.insert("__contains_str".to_string(), Value::Builtin(builtin_contains_str));
    globals.insert("__strip".to_string(), Value::Builtin(builtin_strip));
    globals.insert("__strip_left".to_string(), Value::Builtin(builtin_strip_left));
    globals.insert("__strip_right".to_string(), Value::Builtin(builtin_strip_right));
    globals.insert("__sort".to_string(), Value::Builtin(builtin_sort));

    // Math builtins
    globals.insert("__sqrt".to_string(), Value::Builtin(builtin_sqrt));
    globals.insert("__floor".to_string(), Value::Builtin(builtin_floor));
    globals.insert("__ceil".to_string(), Value::Builtin(builtin_ceil));
    globals.insert("__round".to_string(), Value::Builtin(builtin_round));

    // I/O builtins
    globals.insert("__input".to_string(), Value::Builtin(builtin_input));
    globals.insert("__read_line".to_string(), Value::Builtin(builtin_read_line));

    // File I/O
    globals.insert("__read_file".to_string(), Value::Builtin(builtin_read_file));
    globals.insert("__write_file".to_string(), Value::Builtin(builtin_write_file));
    globals.insert("__append_file".to_string(), Value::Builtin(builtin_append_file));
    globals.insert("__file_exists".to_string(), Value::Builtin(builtin_file_exists));

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
    globals.insert("__time_now_ms".to_string(), Value::Builtin(builtin_time_now_ms));
    globals.insert("__time_sleep".to_string(), Value::Builtin(builtin_time_sleep));
    globals.insert("__time_monotonic".to_string(), Value::Builtin(builtin_time_monotonic));

    // Random builtins
    globals.insert("__rand_seed".to_string(), Value::Builtin(builtin_rand_seed));
    globals.insert("__rand_int".to_string(), Value::Builtin(builtin_rand_int));
    globals.insert("__rand_float".to_string(), Value::Builtin(builtin_rand_float));

    // Path builtins
    globals.insert("__path_join".to_string(), Value::Builtin(builtin_path_join));
    globals.insert("__path_ext".to_string(), Value::Builtin(builtin_path_ext));
    globals.insert("__path_filename".to_string(), Value::Builtin(builtin_path_filename));
    globals.insert("__path_parent".to_string(), Value::Builtin(builtin_path_parent));
    globals.insert("__path_stem".to_string(), Value::Builtin(builtin_path_stem));
    globals.insert("__path_is_absolute".to_string(), Value::Builtin(builtin_path_is_absolute));

    // JSON / TOML
    globals.insert("__json_parse".to_string(), Value::Builtin(builtin_json_parse));
    globals.insert("__json_stringify".to_string(), Value::Builtin(builtin_json_stringify));
    globals.insert("__toml_parse".to_string(), Value::Builtin(builtin_toml_parse));
    globals.insert("__toml_stringify".to_string(), Value::Builtin(builtin_toml_stringify));

    // Network
    globals.insert("__http_request".to_string(), Value::Builtin(builtin_http_request));
}

// ── Core builtins ──────────────────────────────────────────────────────

fn builtin_print(args: Vec<Value>) -> Value {
    let parts: Vec<String> = args.iter().map(|a| format!("{}", a)).collect();
    print!("{}", parts.join(" "));
    Value::None
}

fn builtin_println(args: Vec<Value>) -> Value {
    let parts: Vec<String> = args.iter().map(|a| format!("{}", a)).collect();
    println!("{}", parts.join(" "));
    Value::None
}

fn builtin_len(args: Vec<Value>) -> Value {
    if args.len() != 1 {
        return Value::Error("len() takes exactly 1 argument".into());
    }
    match &args[0] {
        Value::String(s) => Value::Integer(s.len() as i64),
        Value::Array(a) => Value::Integer(a.borrow().len() as i64),
        Value::Tuple(t) => Value::Integer(t.len() as i64),
        Value::Map(m) => Value::Integer(m.borrow().len() as i64),
        Value::Set(s) => Value::Integer(s.borrow().len() as i64),
        _ => Value::Error(format!("len() not supported for {}", args[0].type_name()).into()),
    }
}

fn builtin_push(args: Vec<Value>) -> Value {
    if args.len() != 2 {
        return Value::Error("push() takes exactly 2 arguments".into());
    }
    match &args[0] {
        Value::Array(arr) => {
            arr.borrow_mut().push(args[1].clone());
            args[0].clone() // return the array (same behavior as tree-walker)
        }
        _ => Value::Error("push() requires an array as first argument".into()),
    }
}

fn builtin_first(args: Vec<Value>) -> Value {
    if args.len() != 1 {
        return Value::Error("first() takes exactly 1 argument".into());
    }
    match &args[0] {
        Value::Array(arr) => arr.borrow().first().cloned().unwrap_or(Value::None),
        _ => Value::Error("first() requires an array".into()),
    }
}

fn builtin_last(args: Vec<Value>) -> Value {
    if args.len() != 1 {
        return Value::Error("last() takes exactly 1 argument".into());
    }
    match &args[0] {
        Value::Array(arr) => arr.borrow().last().cloned().unwrap_or(Value::None),
        _ => Value::Error("last() requires an array".into()),
    }
}

fn builtin_rest(args: Vec<Value>) -> Value {
    if args.len() != 1 {
        return Value::Error("rest() takes exactly 1 argument".into());
    }
    match &args[0] {
        Value::Array(arr) => {
            let borrowed = arr.borrow();
            if borrowed.is_empty() {
                Value::Array(Rc::new(RefCell::new(Vec::new())))
            } else {
                Value::Array(Rc::new(RefCell::new(borrowed[1..].to_vec())))
            }
        }
        _ => Value::Error("rest() requires an array".into()),
    }
}

fn builtin_type(args: Vec<Value>) -> Value {
    if args.len() != 1 {
        return Value::Error("type() takes exactly 1 argument".into());
    }
    Value::String(args[0].effective_type_name().into())
}

fn builtin_str(args: Vec<Value>) -> Value {
    if args.len() != 1 {
        return Value::Error("str() takes exactly 1 argument".into());
    }
    Value::String(format!("{}", args[0]).into())
}

fn builtin_int(args: Vec<Value>) -> Value {
    if args.len() != 1 {
        return Value::Error("int() takes exactly 1 argument".into());
    }
    match &args[0] {
        Value::Integer(n) => Value::Integer(*n),
        Value::Float(f) => Value::Integer(*f as i64),
        Value::String(s) => match s.parse::<i64>() {
            Ok(n) => Value::Integer(n),
            Err(_) => match s.parse::<f64>() {
                Ok(f) => Value::Integer(f as i64),
                Err(_) => Value::Error(format!("cannot convert '{}' to int", s).into()),
            },
        },
        Value::Boolean(b) => Value::Integer(if *b { 1 } else { 0 }),
        Value::Byte(b) => Value::Integer(*b as i64),
        Value::Uint(u) => Value::Integer(*u as i64),
        Value::Char(c) => Value::Integer(*c as i64),
        _ => Value::Error(format!("cannot convert {} to int", args[0].type_name()).into()),
    }
}

fn builtin_float(args: Vec<Value>) -> Value {
    if args.len() != 1 {
        return Value::Error("float() takes exactly 1 argument".into());
    }
    match &args[0] {
        Value::Float(f) => Value::Float(*f),
        Value::Integer(n) => Value::Float(*n as f64),
        Value::String(s) => match s.parse::<f64>() {
            Ok(f) => Value::Float(f),
            Err(_) => Value::Error(format!("cannot convert '{}' to float", s).into()),
        },
        Value::Byte(b) => Value::Float(*b as f64),
        Value::Uint(u) => Value::Float(*u as f64),
        _ => Value::Error(format!("cannot convert {} to float", args[0].type_name()).into()),
    }
}

fn builtin_range(args: Vec<Value>) -> Value {
    let (start, end) = match args.len() {
        1 => match &args[0] {
            Value::Integer(n) => (0i64, *n),
            _ => return Value::Error("range() requires integer arguments".into()),
        },
        2 => match (&args[0], &args[1]) {
            (Value::Integer(s), Value::Integer(e)) => (*s, *e),
            _ => return Value::Error("range() requires integer arguments".into()),
        },
        _ => return Value::Error("range() takes 1 or 2 arguments".into()),
    };
    let arr: Vec<Value> = (start..end).map(Value::Integer).collect();
    Value::Array(Rc::new(RefCell::new(arr)))
}

fn builtin_chars(args: Vec<Value>) -> Value {
    if args.len() != 1 {
        return Value::Error("chars() takes exactly 1 argument".into());
    }
    match &args[0] {
        Value::String(s) => {
            let arr: Vec<Value> = s.chars().map(Value::Char).collect();
            Value::Array(Rc::new(RefCell::new(arr)))
        }
        _ => Value::Error("chars() requires a string".into()),
    }
}

fn builtin_error(args: Vec<Value>) -> Value {
    match args.len() {
        1 => {
            let msg = format!("{}", args[0]);
            Value::ErrorValue {
                msg: msg.into(),
                tag: None,
            }
        }
        2 => {
            let msg = format!("{}", args[0]);
            let tag = format!("{}", args[1]);
            Value::ErrorValue {
                msg: msg.into(),
                tag: Some(tag.into()),
            }
        }
        _ => Value::Error("error() takes 1 or 2 arguments".into()),
    }
}

fn builtin_is_value(args: Vec<Value>) -> Value {
    if args.len() != 1 {
        return Value::Error("is_value() takes exactly 1 argument".into());
    }
    Value::Boolean(matches!(args[0], Value::Wrapped(_)))
}

fn builtin_is_error(args: Vec<Value>) -> Value {
    if args.len() != 1 {
        return Value::Error("is_error() takes exactly 1 argument".into());
    }
    Value::Boolean(matches!(args[0], Value::ErrorValue { .. }))
}

fn builtin_keys(args: Vec<Value>) -> Value {
    if args.len() != 1 {
        return Value::Error("keys() takes exactly 1 argument".into());
    }
    match &args[0] {
        Value::Map(m) => {
            let keys: Vec<Value> = m.borrow().iter().map(|(k, _)| k.clone()).collect();
            Value::Array(Rc::new(RefCell::new(keys)))
        }
        _ => Value::Error("keys() requires a map".into()),
    }
}

fn builtin_values(args: Vec<Value>) -> Value {
    if args.len() != 1 {
        return Value::Error("values() takes exactly 1 argument".into());
    }
    match &args[0] {
        Value::Map(m) => {
            let vals: Vec<Value> = m.borrow().iter().map(|(_, v)| v.clone()).collect();
            Value::Array(Rc::new(RefCell::new(vals)))
        }
        _ => Value::Error("values() requires a map".into()),
    }
}

fn builtin_insert(args: Vec<Value>) -> Value {
    if args.len() != 3 {
        return Value::Error("insert() takes exactly 3 arguments".into());
    }
    match &args[0] {
        Value::Map(m) => {
            let mut map = m.borrow_mut();
            // Check if key exists, update if so
            for entry in map.iter_mut() {
                if entry.0 == args[1] {
                    entry.1 = args[2].clone();
                    return Value::None;
                }
            }
            map.push((args[1].clone(), args[2].clone()));
            Value::None
        }
        Value::Set(s) => {
            let mut set = s.borrow_mut();
            if !set.iter().any(|item| item == &args[1]) {
                set.push(args[1].clone());
            }
            Value::None
        }
        _ => Value::Error("insert() requires a map or set".into()),
    }
}

fn builtin_remove(args: Vec<Value>) -> Value {
    if args.len() != 2 {
        return Value::Error("remove() takes exactly 2 arguments".into());
    }
    match &args[0] {
        Value::Map(m) => {
            let mut map = m.borrow_mut();
            map.retain(|(k, _)| k != &args[1]);
            Value::None
        }
        Value::Set(s) => {
            let mut set = s.borrow_mut();
            set.retain(|item| item != &args[1]);
            Value::None
        }
        _ => Value::Error("remove() requires a map or set".into()),
    }
}

fn builtin_has(args: Vec<Value>) -> Value {
    if args.len() != 2 {
        return Value::Error("has() takes exactly 2 arguments".into());
    }
    match &args[0] {
        Value::Map(m) => {
            let map = m.borrow();
            Value::Boolean(map.iter().any(|(k, _)| k == &args[1]))
        }
        Value::Set(s) => {
            let set = s.borrow();
            Value::Boolean(set.iter().any(|item| item == &args[1]))
        }
        _ => Value::Error("has() requires a map or set".into()),
    }
}

fn builtin_tuple(args: Vec<Value>) -> Value {
    Value::Tuple(Rc::new(args))
}

fn builtin_set(args: Vec<Value>) -> Value {
    // Deduplicate
    let mut items = Vec::new();
    for arg in args {
        if !items.iter().any(|i: &Value| i == &arg) {
            items.push(arg);
        }
    }
    Value::Set(Rc::new(RefCell::new(items)))
}

fn builtin_byte(args: Vec<Value>) -> Value {
    if args.len() != 1 {
        return Value::Error("byte() takes exactly 1 argument".into());
    }
    match &args[0] {
        Value::Integer(n) => Value::Byte(*n as u8),
        Value::Uint(n) => Value::Byte(*n as u8),
        _ => Value::Error(format!("cannot convert {} to byte", args[0].type_name()).into()),
    }
}

fn builtin_uint(args: Vec<Value>) -> Value {
    if args.len() != 1 {
        return Value::Error("uint() takes exactly 1 argument".into());
    }
    match &args[0] {
        Value::Integer(n) => Value::Uint(*n as u64),
        Value::Float(f) => Value::Uint(*f as u64),
        Value::Byte(b) => Value::Uint(*b as u64),
        Value::String(s) => match s.parse::<u64>() {
            Ok(n) => Value::Uint(n),
            Err(_) => Value::Error(format!("cannot convert '{}' to uint", s).into()),
        },
        _ => Value::Error(format!("cannot convert {} to uint", args[0].type_name()).into()),
    }
}

fn builtin_chr(args: Vec<Value>) -> Value {
    if args.len() != 1 {
        return Value::Error("chr() takes exactly 1 argument".into());
    }
    match &args[0] {
        Value::Integer(n) => match char::from_u32(*n as u32) {
            Some(c) => Value::Char(c),
            None => Value::Error(format!("invalid char code: {}", n).into()),
        },
        _ => Value::Error("chr() requires an integer".into()),
    }
}

fn builtin_ord(args: Vec<Value>) -> Value {
    if args.len() != 1 {
        return Value::Error("ord() takes exactly 1 argument".into());
    }
    match &args[0] {
        Value::Char(c) => Value::Integer(*c as i64),
        _ => Value::Error("ord() requires a char".into()),
    }
}

// ── String builtins ────────────────────────────────────────────────────

fn builtin_split(args: Vec<Value>) -> Value {
    if args.len() != 2 {
        return Value::Error("__split() takes 2 arguments".into());
    }
    match (&args[0], &args[1]) {
        (Value::String(s), Value::String(sep)) => {
            let parts: Vec<Value> = s
                .split(sep.as_ref())
                .map(|p| Value::String(p.into()))
                .collect();
            Value::Array(Rc::new(RefCell::new(parts)))
        }
        _ => Value::Error("__split() requires string arguments".into()),
    }
}

fn builtin_join(args: Vec<Value>) -> Value {
    if args.len() != 2 {
        return Value::Error("__join() takes 2 arguments".into());
    }
    match (&args[0], &args[1]) {
        (Value::Array(arr), Value::String(sep)) => {
            let parts: Vec<String> = arr.borrow().iter().map(|v| format!("{}", v)).collect();
            Value::String(parts.join(sep.as_ref()).into())
        }
        _ => Value::Error("__join() requires (array, string)".into()),
    }
}

fn builtin_trim(args: Vec<Value>) -> Value {
    if args.len() != 1 {
        return Value::Error("__trim() takes 1 argument".into());
    }
    match &args[0] {
        Value::String(s) => Value::String(s.trim().into()),
        _ => Value::Error("__trim() requires a string".into()),
    }
}

fn builtin_upper(args: Vec<Value>) -> Value {
    if args.len() != 1 {
        return Value::Error("__upper() takes 1 argument".into());
    }
    match &args[0] {
        Value::String(s) => Value::String(s.to_uppercase().into()),
        _ => Value::Error("__upper() requires a string".into()),
    }
}

fn builtin_lower(args: Vec<Value>) -> Value {
    if args.len() != 1 {
        return Value::Error("__lower() takes 1 argument".into());
    }
    match &args[0] {
        Value::String(s) => Value::String(s.to_lowercase().into()),
        _ => Value::Error("__lower() requires a string".into()),
    }
}

fn builtin_replace(args: Vec<Value>) -> Value {
    if args.len() != 3 {
        return Value::Error("__replace() takes 3 arguments".into());
    }
    match (&args[0], &args[1], &args[2]) {
        (Value::String(s), Value::String(from), Value::String(to)) => {
            Value::String(s.replace(from.as_ref(), to.as_ref()).into())
        }
        _ => Value::Error("__replace() requires string arguments".into()),
    }
}

fn builtin_starts_with(args: Vec<Value>) -> Value {
    if args.len() != 2 {
        return Value::Error("__starts_with() takes 2 arguments".into());
    }
    match (&args[0], &args[1]) {
        (Value::String(s), Value::String(prefix)) => {
            Value::Boolean(s.starts_with(prefix.as_ref()))
        }
        _ => Value::Error("__starts_with() requires string arguments".into()),
    }
}

fn builtin_ends_with(args: Vec<Value>) -> Value {
    if args.len() != 2 {
        return Value::Error("__ends_with() takes 2 arguments".into());
    }
    match (&args[0], &args[1]) {
        (Value::String(s), Value::String(suffix)) => {
            Value::Boolean(s.ends_with(suffix.as_ref()))
        }
        _ => Value::Error("__ends_with() requires string arguments".into()),
    }
}

fn builtin_contains_str(args: Vec<Value>) -> Value {
    if args.len() != 2 {
        return Value::Error("__contains_str() takes 2 arguments".into());
    }
    match (&args[0], &args[1]) {
        (Value::String(s), Value::String(sub)) => Value::Boolean(s.contains(sub.as_ref())),
        _ => Value::Error("__contains_str() requires string arguments".into()),
    }
}

fn builtin_strip(args: Vec<Value>) -> Value {
    if args.len() != 2 {
        return Value::Error("__strip() takes 2 arguments".into());
    }
    match (&args[0], &args[1]) {
        (Value::String(s), Value::String(chars)) => {
            let chars_vec: Vec<char> = chars.chars().collect();
            let result: String = s.trim_matches(chars_vec.as_slice()).to_string();
            Value::String(result.into())
        }
        _ => Value::Error("__strip() requires string arguments".into()),
    }
}

fn builtin_strip_left(args: Vec<Value>) -> Value {
    if args.len() != 2 {
        return Value::Error("__strip_left() takes 2 arguments".into());
    }
    match (&args[0], &args[1]) {
        (Value::String(s), Value::String(chars)) => {
            let chars_vec: Vec<char> = chars.chars().collect();
            let result: String = s.trim_start_matches(chars_vec.as_slice()).to_string();
            Value::String(result.into())
        }
        _ => Value::Error("__strip_left() requires string arguments".into()),
    }
}

fn builtin_strip_right(args: Vec<Value>) -> Value {
    if args.len() != 2 {
        return Value::Error("__strip_right() takes 2 arguments".into());
    }
    match (&args[0], &args[1]) {
        (Value::String(s), Value::String(chars)) => {
            let chars_vec: Vec<char> = chars.chars().collect();
            let result: String = s.trim_end_matches(chars_vec.as_slice()).to_string();
            Value::String(result.into())
        }
        _ => Value::Error("__strip_right() requires string arguments".into()),
    }
}

fn builtin_sort(args: Vec<Value>) -> Value {
    if args.len() != 1 {
        return Value::Error("__sort() takes 1 argument".into());
    }
    match &args[0] {
        Value::Array(arr) => {
            let mut sorted = arr.borrow().clone();
            sorted.sort_by(|a, b| {
                match (a, b) {
                    (Value::Integer(x), Value::Integer(y)) => x.cmp(y),
                    (Value::Float(x), Value::Float(y)) => x.partial_cmp(y).unwrap_or(std::cmp::Ordering::Equal),
                    (Value::String(x), Value::String(y)) => x.cmp(y),
                    _ => std::cmp::Ordering::Equal,
                }
            });
            Value::Array(Rc::new(RefCell::new(sorted)))
        }
        _ => Value::Error("__sort() requires an array".into()),
    }
}

// ── Math builtins ──────────────────────────────────────────────────────

fn builtin_sqrt(args: Vec<Value>) -> Value {
    if args.len() != 1 { return Value::Error("__sqrt() takes 1 argument".into()); }
    match &args[0] {
        Value::Float(f) => Value::Float(f.sqrt()),
        Value::Integer(n) => Value::Float((*n as f64).sqrt()),
        _ => Value::Error("__sqrt() requires a number".into()),
    }
}

fn builtin_floor(args: Vec<Value>) -> Value {
    if args.len() != 1 { return Value::Error("__floor() takes 1 argument".into()); }
    match &args[0] {
        Value::Float(f) => Value::Integer(f.floor() as i64),
        Value::Integer(n) => Value::Integer(*n),
        _ => Value::Error("__floor() requires a number".into()),
    }
}

fn builtin_ceil(args: Vec<Value>) -> Value {
    if args.len() != 1 { return Value::Error("__ceil() takes 1 argument".into()); }
    match &args[0] {
        Value::Float(f) => Value::Integer(f.ceil() as i64),
        Value::Integer(n) => Value::Integer(*n),
        _ => Value::Error("__ceil() requires a number".into()),
    }
}

fn builtin_round(args: Vec<Value>) -> Value {
    if args.len() != 1 { return Value::Error("__round() takes 1 argument".into()); }
    match &args[0] {
        Value::Float(f) => Value::Integer(f.round() as i64),
        Value::Integer(n) => Value::Integer(*n),
        _ => Value::Error("__round() requires a number".into()),
    }
}

// ── I/O builtins ───────────────────────────────────────────────────────

fn builtin_input(args: Vec<Value>) -> Value {
    if !args.is_empty() {
        print!("{}", args[0]);
        use std::io::Write;
        std::io::stdout().flush().ok();
    }
    let mut line = String::new();
    match std::io::stdin().read_line(&mut line) {
        Ok(_) => Value::String(line.trim_end_matches('\n').trim_end_matches('\r').into()),
        Err(e) => Value::Error(format!("input error: {}", e).into()),
    }
}

fn builtin_read_line(_args: Vec<Value>) -> Value {
    let mut line = String::new();
    match std::io::stdin().read_line(&mut line) {
        Ok(_) => Value::String(line.trim_end_matches('\n').trim_end_matches('\r').into()),
        Err(e) => Value::Error(format!("read_line error: {}", e).into()),
    }
}

// ── File I/O ───────────────────────────────────────────────────────────

fn builtin_read_file(args: Vec<Value>) -> Value {
    if args.len() != 1 { return Value::Error("__read_file() takes 1 argument".into()); }
    match &args[0] {
        Value::String(path) => match std::fs::read_to_string(path.as_ref()) {
            Ok(contents) => Value::String(contents.into()),
            Err(e) => Value::ErrorValue { msg: format!("{}", e).into(), tag: Some("IO".into()) },
        },
        _ => Value::Error("__read_file() requires a string path".into()),
    }
}

fn builtin_write_file(args: Vec<Value>) -> Value {
    if args.len() != 2 { return Value::Error("__write_file() takes 2 arguments".into()); }
    match (&args[0], &args[1]) {
        (Value::String(path), Value::String(content)) => {
            match std::fs::write(path.as_ref(), content.as_ref()) {
                Ok(_) => Value::None,
                Err(e) => Value::ErrorValue { msg: format!("{}", e).into(), tag: Some("IO".into()) },
            }
        }
        _ => Value::Error("__write_file() requires (string, string)".into()),
    }
}

fn builtin_append_file(args: Vec<Value>) -> Value {
    if args.len() != 2 { return Value::Error("__append_file() takes 2 arguments".into()); }
    match (&args[0], &args[1]) {
        (Value::String(path), Value::String(content)) => {
            use std::io::Write;
            match std::fs::OpenOptions::new().append(true).create(true).open(path.as_ref()) {
                Ok(mut f) => match f.write_all(content.as_bytes()) {
                    Ok(_) => Value::None,
                    Err(e) => Value::ErrorValue { msg: format!("{}", e).into(), tag: Some("IO".into()) },
                },
                Err(e) => Value::ErrorValue { msg: format!("{}", e).into(), tag: Some("IO".into()) },
            }
        }
        _ => Value::Error("__append_file() requires (string, string)".into()),
    }
}

fn builtin_file_exists(args: Vec<Value>) -> Value {
    if args.len() != 1 { return Value::Error("__file_exists() takes 1 argument".into()); }
    match &args[0] {
        Value::String(path) => Value::Boolean(std::path::Path::new(path.as_ref()).exists()),
        _ => Value::Error("__file_exists() requires a string".into()),
    }
}

// ── OS builtins ────────────────────────────────────────────────────────

fn builtin_exec(args: Vec<Value>) -> Value {
    if args.is_empty() { return Value::Error("__exec() takes at least 1 argument".into()); }
    let cmd = match &args[0] {
        Value::String(s) => s.to_string(),
        _ => return Value::Error("__exec() requires a string command".into()),
    };
    let extra_args: Vec<String> = args[1..].iter().map(|a| format!("{}", a)).collect();
    let full_cmd = if extra_args.is_empty() {
        cmd
    } else {
        format!("{} {}", cmd, extra_args.join(" "))
    };

    let shell = if cfg!(target_os = "windows") { "cmd" } else { "sh" };
    let flag = if cfg!(target_os = "windows") { "/C" } else { "-c" };

    match std::process::Command::new(shell).arg(flag).arg(&full_cmd).output() {
        Ok(output) => {
            let stdout = String::from_utf8_lossy(&output.stdout).to_string();
            let stderr = String::from_utf8_lossy(&output.stderr).to_string();
            let code = output.status.code().unwrap_or(-1);
            let entries: Vec<(Value, Value)> = vec![
                (Value::String("stdout".into()), Value::String(stdout.into())),
                (Value::String("stderr".into()), Value::String(stderr.into())),
                (Value::String("code".into()), Value::Integer(code as i64)),
            ];
            Value::Map(Rc::new(RefCell::new(entries)))
        }
        Err(e) => Value::Error(format!("__exec: {}", e).into()),
    }
}

fn builtin_os_name(_args: Vec<Value>) -> Value {
    Value::String(std::env::consts::OS.into())
}

fn builtin_os_arch(_args: Vec<Value>) -> Value {
    Value::String(std::env::consts::ARCH.into())
}

fn builtin_env_get(args: Vec<Value>) -> Value {
    if args.len() != 1 { return Value::Error("__env_get() takes 1 argument".into()); }
    match &args[0] {
        Value::String(name) => match std::env::var(name.as_ref()) {
            Ok(val) => Value::String(val.into()),
            Err(_) => Value::None,
        },
        _ => Value::Error("__env_get() requires a string".into()),
    }
}

fn builtin_env_set(args: Vec<Value>) -> Value {
    if args.len() != 2 { return Value::Error("__env_set() takes 2 arguments".into()); }
    match (&args[0], &args[1]) {
        (Value::String(name), Value::String(val)) => {
            // SAFETY: We don't access env vars concurrently from other threads.
            unsafe { std::env::set_var(name.as_ref(), val.as_ref()); }
            Value::None
        }
        _ => Value::Error("__env_set() requires (string, string)".into()),
    }
}

fn builtin_env_vars(_args: Vec<Value>) -> Value {
    let entries: Vec<(Value, Value)> = std::env::vars()
        .map(|(k, v)| (Value::String(k.into()), Value::String(v.into())))
        .collect();
    Value::Map(Rc::new(RefCell::new(entries)))
}

fn builtin_cwd(_args: Vec<Value>) -> Value {
    match std::env::current_dir() {
        Ok(path) => Value::String(path.display().to_string().into()),
        Err(e) => Value::Error(format!("cwd error: {}", e).into()),
    }
}

fn builtin_chdir(args: Vec<Value>) -> Value {
    if args.len() != 1 { return Value::Error("__chdir() takes 1 argument".into()); }
    match &args[0] {
        Value::String(path) => {
            match std::env::set_current_dir(path.as_ref()) {
                Ok(_) => Value::None,
                Err(e) => Value::Error(format!("chdir error: {}", e).into()),
            }
        }
        _ => Value::Error("__chdir() requires a string".into()),
    }
}

fn builtin_exit(args: Vec<Value>) -> Value {
    let code = if args.is_empty() {
        0
    } else {
        match &args[0] {
            Value::Integer(n) => *n as i32,
            _ => 1,
        }
    };
    std::process::exit(code);
}

fn builtin_pid(_args: Vec<Value>) -> Value {
    Value::Integer(std::process::id() as i64)
}

fn builtin_list_dir(args: Vec<Value>) -> Value {
    if args.len() != 1 { return Value::Error("__list_dir() takes 1 argument".into()); }
    match &args[0] {
        Value::String(path) => match std::fs::read_dir(path.as_ref()) {
            Ok(entries) => {
                let items: Vec<Value> = entries
                    .filter_map(|e| e.ok())
                    .map(|e| Value::String(e.path().to_string_lossy().into()))
                    .collect();
                Value::Array(Rc::new(RefCell::new(items)))
            }
            Err(e) => Value::ErrorValue { msg: format!("{}", e).into(), tag: Some("IO".into()) },
        },
        _ => Value::Error("__list_dir() requires a string".into()),
    }
}

fn builtin_walk_dir(args: Vec<Value>) -> Value {
    if args.len() != 1 { return Value::Error("__walk_dir() takes 1 argument".into()); }
    match &args[0] {
        Value::String(path) => {
            let mut items = Vec::new();
            fn walk(dir: &std::path::Path, items: &mut Vec<Value>) {
                if let Ok(entries) = std::fs::read_dir(dir) {
                    for entry in entries.filter_map(|e| e.ok()) {
                        let path = entry.path();
                        items.push(Value::String(path.display().to_string().into()));
                        if path.is_dir() {
                            walk(&path, items);
                        }
                    }
                }
            }
            walk(std::path::Path::new(path.as_ref()), &mut items);
            Value::Array(Rc::new(RefCell::new(items)))
        }
        _ => Value::Error("__walk_dir() requires a string".into()),
    }
}

fn builtin_mkdir(args: Vec<Value>) -> Value {
    if args.len() != 1 { return Value::Error("__mkdir() takes 1 argument".into()); }
    match &args[0] {
        Value::String(path) => match std::fs::create_dir_all(path.as_ref()) {
            Ok(_) => Value::None,
            Err(e) => Value::ErrorValue { msg: format!("{}", e).into(), tag: Some("IO".into()) },
        },
        _ => Value::Error("__mkdir() requires a string".into()),
    }
}

fn builtin_rmdir(args: Vec<Value>) -> Value {
    if args.len() != 1 { return Value::Error("__rmdir() takes 1 argument".into()); }
    match &args[0] {
        Value::String(path) => match std::fs::remove_dir_all(path.as_ref()) {
            Ok(_) => Value::None,
            Err(e) => Value::ErrorValue { msg: format!("{}", e).into(), tag: Some("IO".into()) },
        },
        _ => Value::Error("__rmdir() requires a string".into()),
    }
}

fn builtin_remove_file(args: Vec<Value>) -> Value {
    if args.len() != 1 { return Value::Error("__remove() takes 1 argument".into()); }
    match &args[0] {
        Value::String(path) => match std::fs::remove_file(path.as_ref()) {
            Ok(_) => Value::None,
            Err(e) => Value::ErrorValue { msg: format!("{}", e).into(), tag: Some("IO".into()) },
        },
        _ => Value::Error("__remove() requires a string".into()),
    }
}

fn builtin_is_dir(args: Vec<Value>) -> Value {
    if args.len() != 1 { return Value::Error("__is_dir() takes 1 argument".into()); }
    match &args[0] {
        Value::String(path) => Value::Boolean(std::path::Path::new(path.as_ref()).is_dir()),
        _ => Value::Error("__is_dir() requires a string".into()),
    }
}

fn builtin_is_file(args: Vec<Value>) -> Value {
    if args.len() != 1 { return Value::Error("__is_file() takes 1 argument".into()); }
    match &args[0] {
        Value::String(path) => Value::Boolean(std::path::Path::new(path.as_ref()).is_file()),
        _ => Value::Error("__is_file() requires a string".into()),
    }
}

// ── Time builtins ──────────────────────────────────────────────────────

fn builtin_time_now(_args: Vec<Value>) -> Value {
    use std::time::{SystemTime, UNIX_EPOCH};
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(d) => Value::Float(d.as_secs_f64()),
        Err(_) => Value::Error("time error".into()),
    }
}

fn builtin_time_now_ms(_args: Vec<Value>) -> Value {
    use std::time::{SystemTime, UNIX_EPOCH};
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(d) => Value::Integer(d.as_millis() as i64),
        Err(_) => Value::Error("time error".into()),
    }
}

fn builtin_time_sleep(args: Vec<Value>) -> Value {
    if args.len() != 1 { return Value::Error("__time_sleep() takes 1 argument".into()); }
    match &args[0] {
        Value::Integer(ms) => {
            std::thread::sleep(std::time::Duration::from_millis(*ms as u64));
            Value::None
        }
        _ => Value::Error("__time_sleep() requires an integer (ms)".into()),
    }
}

fn builtin_time_monotonic(_args: Vec<Value>) -> Value {
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

fn builtin_rand_seed(args: Vec<Value>) -> Value {
    if args.len() != 1 { return Value::Error("__rand_seed() takes 1 argument".into()); }
    match &args[0] {
        Value::Integer(n) => {
            let seed = if *n == 0 { 1u64 } else { *n as u64 };
            RNG_STATE.with(|state| state.set(seed));
            Value::None
        }
        _ => Value::Error("__rand_seed() requires an integer".into()),
    }
}

fn builtin_rand_int(args: Vec<Value>) -> Value {
    if args.len() != 2 { return Value::Error("__rand_int() takes 2 arguments".into()); }
    match (&args[0], &args[1]) {
        (Value::Integer(min), Value::Integer(max)) => {
            let range = max - min;
            if range <= 0 { return Value::Integer(*min); }
            let r = next_rand();
            Value::Integer(min + (r as i64).unsigned_abs() as i64 % range)
        }
        _ => Value::Error("__rand_int() requires integer arguments".into()),
    }
}

fn builtin_rand_float(_args: Vec<Value>) -> Value {
    let r = next_rand();
    Value::Float((r as f64) / (u64::MAX as f64))
}

// ── Path builtins ──────────────────────────────────────────────────────

fn builtin_path_join(args: Vec<Value>) -> Value {
    if args.len() != 1 { return Value::Error("__path_join() takes 1 argument (array)".into()); }
    match &args[0] {
        Value::Array(parts) => {
            let borrowed = parts.borrow();
            let mut path = std::path::PathBuf::new();
            for part in borrowed.iter() {
                path.push(format!("{}", part));
            }
            Value::String(path.display().to_string().into())
        }
        _ => Value::Error("__path_join() requires an array".into()),
    }
}

fn builtin_path_ext(args: Vec<Value>) -> Value {
    if args.len() != 1 { return Value::Error("__path_ext() takes 1 argument".into()); }
    match &args[0] {
        Value::String(path) => {
            let p = std::path::Path::new(path.as_ref());
            match p.extension() {
                Some(ext) => Value::String(ext.to_string_lossy().into()),
                None => Value::String("".into()),
            }
        }
        _ => Value::Error("__path_ext() requires a string".into()),
    }
}

fn builtin_path_filename(args: Vec<Value>) -> Value {
    if args.len() != 1 { return Value::Error("__path_filename() takes 1 argument".into()); }
    match &args[0] {
        Value::String(path) => {
            let p = std::path::Path::new(path.as_ref());
            match p.file_name() {
                Some(name) => Value::String(name.to_string_lossy().into()),
                None => Value::String("".into()),
            }
        }
        _ => Value::Error("__path_filename() requires a string".into()),
    }
}

fn builtin_path_parent(args: Vec<Value>) -> Value {
    if args.len() != 1 { return Value::Error("__path_parent() takes 1 argument".into()); }
    match &args[0] {
        Value::String(path) => {
            let p = std::path::Path::new(path.as_ref());
            match p.parent() {
                Some(parent) => Value::String(parent.display().to_string().into()),
                None => Value::String("".into()),
            }
        }
        _ => Value::Error("__path_parent() requires a string".into()),
    }
}

fn builtin_path_stem(args: Vec<Value>) -> Value {
    if args.len() != 1 { return Value::Error("__path_stem() takes 1 argument".into()); }
    match &args[0] {
        Value::String(path) => {
            let p = std::path::Path::new(path.as_ref());
            match p.file_stem() {
                Some(stem) => Value::String(stem.to_string_lossy().into()),
                None => Value::String("".into()),
            }
        }
        _ => Value::Error("__path_stem() requires a string".into()),
    }
}

fn builtin_path_is_absolute(args: Vec<Value>) -> Value {
    if args.len() != 1 { return Value::Error("__path_is_absolute() takes 1 argument".into()); }
    match &args[0] {
        Value::String(path) => Value::Boolean(std::path::Path::new(path.as_ref()).is_absolute()),
        _ => Value::Error("__path_is_absolute() requires a string".into()),
    }
}

// ── JSON builtins ──────────────────────────────────────────────────────

fn builtin_json_parse(args: Vec<Value>) -> Value {
    if args.len() != 1 { return Value::Error("__json_parse() takes 1 argument".into()); }
    match &args[0] {
        Value::String(s) => {
            let chars: Vec<char> = s.chars().collect();
            let mut pos = 0;
            match json_parse_value(&chars, &mut pos) {
                Ok(val) => val,
                Err(e) => Value::Error(format!("json parse error: {}", e).into()),
            }
        }
        _ => Value::Error("__json_parse() requires a string".into()),
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
    Ok(Value::String(s.into()))
}

fn json_parse_number(chars: &[char], pos: &mut usize) -> Result<Value, String> {
    let start = *pos;
    let mut is_float = false;
    if chars[*pos] == '-' { *pos += 1; }
    while *pos < chars.len() && chars[*pos].is_ascii_digit() { *pos += 1; }
    if *pos < chars.len() && chars[*pos] == '.' {
        is_float = true;
        *pos += 1;
        while *pos < chars.len() && chars[*pos].is_ascii_digit() { *pos += 1; }
    }
    if *pos < chars.len() && (chars[*pos] == 'e' || chars[*pos] == 'E') {
        is_float = true;
        *pos += 1;
        if *pos < chars.len() && (chars[*pos] == '+' || chars[*pos] == '-') { *pos += 1; }
        while *pos < chars.len() && chars[*pos].is_ascii_digit() { *pos += 1; }
    }
    let num_str: String = chars[start..*pos].iter().collect();
    if is_float {
        num_str.parse::<f64>().map(Value::Float).map_err(|e| format!("invalid number: {}", e))
    } else {
        num_str.parse::<i64>().map(Value::Integer).map_err(|e| format!("invalid number: {}", e))
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
        if *pos >= chars.len() { return Err("unterminated array".to_string()); }
        if chars[*pos] == ']' { *pos += 1; break; }
        if chars[*pos] != ',' { return Err(format!("expected ',' or ']' at {}", pos)); }
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
        return Ok(Value::Map(Rc::new(RefCell::new(entries))));
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
        if *pos >= chars.len() { return Err("unterminated object".to_string()); }
        if chars[*pos] == '}' { *pos += 1; break; }
        if chars[*pos] != ',' { return Err(format!("expected ',' or '}}' at {}", pos)); }
        *pos += 1;
    }
    Ok(Value::Map(Rc::new(RefCell::new(entries))))
}

fn builtin_json_stringify(args: Vec<Value>) -> Value {
    if args.len() != 1 { return Value::Error("__json_stringify() takes 1 argument".into()); }
    match value_to_json(&args[0]) {
        Ok(s) => Value::String(s.into()),
        Err(e) => Value::Error(format!("json stringify error: {}", e).into()),
    }
}

fn value_to_json(val: &Value) -> Result<String, String> {
    match val {
        Value::None => Ok("null".to_string()),
        Value::Boolean(b) => Ok(if *b { "true" } else { "false" }.to_string()),
        Value::Integer(n) => Ok(n.to_string()),
        Value::Float(f) => {
            if f.is_nan() || f.is_infinite() { Ok("null".to_string()) } else { Ok(f.to_string()) }
        }
        Value::String(s) => {
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
        Value::Array(arr) => {
            let items: Result<Vec<String>, String> =
                arr.borrow().iter().map(|e| value_to_json(e)).collect();
            Ok(format!("[{}]", items?.join(",")))
        }
        Value::Map(entries) => {
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
        Value::Tuple(t) => {
            let items: Result<Vec<String>, String> = t.iter().map(|e| value_to_json(e)).collect();
            Ok(format!("[{}]", items?.join(",")))
        }
        Value::Byte(n) => Ok(n.to_string()),
        Value::Uint(n) => Ok(n.to_string()),
        Value::Char(c) => Ok(format!("\"{}\"", c)),
        _ => Err(format!("cannot serialize {} to JSON", val.type_name())),
    }
}

// ── TOML builtins ──────────────────────────────────────────────────────

fn builtin_toml_parse(args: Vec<Value>) -> Value {
    if args.len() != 1 { return Value::Error("__toml_parse() takes 1 argument".into()); }
    match &args[0] {
        Value::String(s) => {
            match toml::from_str::<toml::Value>(s.as_ref()) {
                Ok(value) => toml_value_to_value(value),
                Err(e) => Value::Error(format!("toml parse error: {}", e).into()),
            }
        }
        _ => Value::Error("__toml_parse() requires a string".into()),
    }
}

fn toml_value_to_value(tv: toml::Value) -> Value {
    match tv {
        toml::Value::String(s) => Value::String(s.into()),
        toml::Value::Integer(i) => Value::Integer(i),
        toml::Value::Float(f) => Value::Float(f),
        toml::Value::Boolean(b) => Value::Boolean(b),
        toml::Value::Datetime(dt) => Value::String(dt.to_string().into()),
        toml::Value::Array(values) => {
            let items: Vec<Value> = values.into_iter().map(toml_value_to_value).collect();
            Value::Array(Rc::new(RefCell::new(items)))
        }
        toml::Value::Table(entries) => {
            let items: Vec<(Value, Value)> = entries
                .into_iter()
                .map(|(k, v)| (Value::String(k.into()), toml_value_to_value(v)))
                .collect();
            Value::Map(Rc::new(RefCell::new(items)))
        }
    }
}

fn builtin_toml_stringify(args: Vec<Value>) -> Value {
    if args.len() != 1 { return Value::Error("__toml_stringify() takes 1 argument".into()); }
    match value_to_toml(&args[0]) {
        Ok(tv) => match toml::to_string_pretty(&tv) {
            Ok(s) => Value::String(s.into()),
            Err(e) => Value::Error(format!("toml stringify error: {}", e).into()),
        },
        Err(e) => Value::Error(format!("toml stringify error: {}", e).into()),
    }
}

fn value_to_toml(val: &Value) -> Result<toml::Value, String> {
    match val {
        Value::String(s) => Ok(toml::Value::String(s.to_string())),
        Value::Integer(n) => Ok(toml::Value::Integer(*n)),
        Value::Float(f) => {
            if f.is_nan() || f.is_infinite() {
                Err("TOML does not support NaN or Infinity".to_string())
            } else {
                Ok(toml::Value::Float(*f))
            }
        }
        Value::Boolean(b) => Ok(toml::Value::Boolean(*b)),
        Value::Uint(n) => Ok(toml::Value::Integer(*n as i64)),
        Value::Byte(n) => Ok(toml::Value::Integer(*n as i64)),
        Value::Char(c) => Ok(toml::Value::String(c.to_string())),
        Value::Array(arr) => {
            let items: Result<Vec<toml::Value>, String> =
                arr.borrow().iter().map(|e| value_to_toml(e)).collect();
            Ok(toml::Value::Array(items?))
        }
        Value::Tuple(t) => {
            let items: Result<Vec<toml::Value>, String> =
                t.iter().map(|e| value_to_toml(e)).collect();
            Ok(toml::Value::Array(items?))
        }
        Value::Map(entries) => {
            let mut table = toml::map::Map::new();
            for (k, v) in entries.borrow().iter() {
                let key = match k {
                    Value::String(s) => s.to_string(),
                    _ => return Err(format!("TOML keys must be strings, got {}", k.type_name())),
                };
                table.insert(key, value_to_toml(v)?);
            }
            Ok(toml::Value::Table(table))
        }
        Value::None => Err("TOML does not support null/None".to_string()),
        _ => Err(format!("cannot serialize {} to TOML", val.type_name())),
    }
}

// ── HTTP builtins ──────────────────────────────────────────────────────

fn builtin_http_request(args: Vec<Value>) -> Value {
    if args.len() < 2 || args.len() > 4 {
        return Value::Error("__http_request: expected 2-4 arguments (method, url, headers?, body?)".into());
    }
    let method = match &args[0] {
        Value::String(s) => s.to_uppercase(),
        _ => return Value::Error(format!("__http_request: method must be STRING, got {}", args[0].type_name()).into()),
    };
    let url = match &args[1] {
        Value::String(s) => s.to_string(),
        _ => return Value::Error(format!("__http_request: url must be STRING, got {}", args[1].type_name()).into()),
    };

    let headers: Vec<(String, String)> = if args.len() >= 3 {
        if let Value::Map(m) = &args[2] {
            m.borrow().iter().filter_map(|(k, v)| {
                if let (Value::String(key), Value::String(val)) = (k, v) {
                    Some((key.to_string(), val.to_string()))
                } else { None }
            }).collect()
        } else { Vec::new() }
    } else { Vec::new() };

    let body: Option<String> = if args.len() >= 4 {
        match &args[3] {
            Value::String(s) => Some(s.to_string()),
            Value::None => None,
            _ => return Value::Error(format!("__http_request: body must be STRING, got {}", args[3].type_name()).into()),
        }
    } else { None };

    macro_rules! apply_headers {
        ($req:expr) => {{
            let mut r = $req;
            for (k, v) in &headers { r = r.header(k.as_str(), v.as_str()); }
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
                        (Value::String("status".into()), Value::Integer(status as i64)),
                        (Value::String("body".into()), Value::String(body_str.into())),
                    ];
                    Value::Map(Rc::new(RefCell::new(entries)))
                }
                Err(e) => Value::Error(format!("http error: {}", e).into()),
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
        _ => Value::Error(format!("__http_request: unsupported method '{}'", method).into()),
    }
}
