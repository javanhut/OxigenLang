#![allow(non_snake_case)]

use crate::object::Object;
use base64::Engine as _;
use sha2::Digest as _;
use std::collections::HashMap;
use std::fs;
use std::rc::Rc;
use toml::Value as TomlValue;

pub fn get_builtins() -> HashMap<String, Rc<Object>> {
    let mut builtins: HashMap<String, Rc<Object>> = HashMap::new();

    builtins.insert("print".to_string(), Rc::new(Object::Builtin(builtin_print)));
    builtins.insert(
        "println".to_string(),
        Rc::new(Object::Builtin(builtin_println)),
    );
    builtins.insert("len".to_string(), Rc::new(Object::Builtin(builtin_len)));
    builtins.insert("push".to_string(), Rc::new(Object::Builtin(builtin_push)));
    builtins.insert("first".to_string(), Rc::new(Object::Builtin(builtin_first)));
    builtins.insert("last".to_string(), Rc::new(Object::Builtin(builtin_last)));
    builtins.insert("rest".to_string(), Rc::new(Object::Builtin(builtin_rest)));
    builtins.insert("type".to_string(), Rc::new(Object::Builtin(builtin_type)));
    builtins.insert("ord".to_string(), Rc::new(Object::Builtin(builtin_ord)));
    builtins.insert("chr".to_string(), Rc::new(Object::Builtin(builtin_chr)));
    builtins.insert("str".to_string(), Rc::new(Object::Builtin(builtin_str)));
    builtins.insert("range".to_string(), Rc::new(Object::Builtin(builtin_range)));
    builtins.insert("chars".to_string(), Rc::new(Object::Builtin(builtin_chars)));
    builtins.insert("byte".to_string(), Rc::new(Object::Builtin(builtin_byte)));
    builtins.insert("uint".to_string(), Rc::new(Object::Builtin(builtin_uint)));
    builtins.insert("set".to_string(), Rc::new(Object::Builtin(builtin_set)));
    builtins.insert("keys".to_string(), Rc::new(Object::Builtin(builtin_keys)));
    builtins.insert(
        "values".to_string(),
        Rc::new(Object::Builtin(builtin_values)),
    );
    builtins.insert(
        "insert".to_string(),
        Rc::new(Object::Builtin(builtin_insert)),
    );
    builtins.insert(
        "remove".to_string(),
        Rc::new(Object::Builtin(builtin_remove)),
    );
    builtins.insert("has".to_string(), Rc::new(Object::Builtin(builtin_has)));
    builtins.insert("tuple".to_string(), Rc::new(Object::Builtin(builtin_tuple)));
    builtins.insert("error".to_string(), Rc::new(Object::Builtin(builtin_error)));
    builtins.insert(
        "is_value".to_string(),
        Rc::new(Object::Builtin(builtin_is_value)),
    );
    builtins.insert(
        "is_error".to_string(),
        Rc::new(Object::Builtin(builtin_is_error)),
    );

    // Internal builtins for stdlib
    builtins.insert(
        "__sqrt".to_string(),
        Rc::new(Object::Builtin(builtin__sqrt)),
    );
    builtins.insert(
        "__floor".to_string(),
        Rc::new(Object::Builtin(builtin__floor)),
    );
    builtins.insert(
        "__ceil".to_string(),
        Rc::new(Object::Builtin(builtin__ceil)),
    );
    builtins.insert(
        "__round".to_string(),
        Rc::new(Object::Builtin(builtin__round)),
    );
    builtins.insert("__sin".to_string(), Rc::new(Object::Builtin(builtin__sin)));
    builtins.insert("__cos".to_string(), Rc::new(Object::Builtin(builtin__cos)));
    builtins.insert("__tan".to_string(), Rc::new(Object::Builtin(builtin__tan)));
    builtins.insert("__asin".to_string(), Rc::new(Object::Builtin(builtin__asin)));
    builtins.insert("__acos".to_string(), Rc::new(Object::Builtin(builtin__acos)));
    builtins.insert("__atan".to_string(), Rc::new(Object::Builtin(builtin__atan)));
    builtins.insert("__atan2".to_string(), Rc::new(Object::Builtin(builtin__atan2)));
    builtins.insert("__ln".to_string(), Rc::new(Object::Builtin(builtin__ln)));
    builtins.insert("__log2".to_string(), Rc::new(Object::Builtin(builtin__log2)));
    builtins.insert("__log10".to_string(), Rc::new(Object::Builtin(builtin__log10)));
    builtins.insert("__exp".to_string(), Rc::new(Object::Builtin(builtin__exp)));
    builtins.insert("__powf".to_string(), Rc::new(Object::Builtin(builtin__powf)));
    // regex
    builtins.insert("__regex_match".to_string(), Rc::new(Object::Builtin(builtin__regex_match)));
    builtins.insert("__regex_find".to_string(), Rc::new(Object::Builtin(builtin__regex_find)));
    builtins.insert("__regex_find_all".to_string(), Rc::new(Object::Builtin(builtin__regex_find_all)));
    builtins.insert("__regex_replace".to_string(), Rc::new(Object::Builtin(builtin__regex_replace)));
    builtins.insert("__regex_split".to_string(), Rc::new(Object::Builtin(builtin__regex_split)));
    builtins.insert("__regex_captures".to_string(), Rc::new(Object::Builtin(builtin__regex_captures)));
    // datetime
    builtins.insert("__dt_now".to_string(), Rc::new(Object::Builtin(builtin__dt_now)));
    builtins.insert("__dt_format".to_string(), Rc::new(Object::Builtin(builtin__dt_format)));
    builtins.insert("__dt_parse".to_string(), Rc::new(Object::Builtin(builtin__dt_parse)));
    // encoding
    builtins.insert("__base64_encode".to_string(), Rc::new(Object::Builtin(builtin__base64_encode)));
    builtins.insert("__base64_decode".to_string(), Rc::new(Object::Builtin(builtin__base64_decode)));
    builtins.insert("__hex_encode".to_string(), Rc::new(Object::Builtin(builtin__hex_encode)));
    builtins.insert("__hex_decode".to_string(), Rc::new(Object::Builtin(builtin__hex_decode)));
    builtins.insert("__url_encode".to_string(), Rc::new(Object::Builtin(builtin__url_encode)));
    builtins.insert("__url_decode".to_string(), Rc::new(Object::Builtin(builtin__url_decode)));
    // hash
    builtins.insert("__sha256".to_string(), Rc::new(Object::Builtin(builtin__sha256)));
    builtins.insert("__sha1".to_string(), Rc::new(Object::Builtin(builtin__sha1)));
    builtins.insert("__md5".to_string(), Rc::new(Object::Builtin(builtin__md5)));
    builtins.insert(
        "__split".to_string(),
        Rc::new(Object::Builtin(builtin__split)),
    );
    builtins.insert(
        "__join".to_string(),
        Rc::new(Object::Builtin(builtin__join)),
    );
    builtins.insert(
        "__trim".to_string(),
        Rc::new(Object::Builtin(builtin__trim)),
    );
    builtins.insert(
        "__strip".to_string(),
        Rc::new(Object::Builtin(builtin__strip)),
    );
    builtins.insert(
        "__strip_left".to_string(),
        Rc::new(Object::Builtin(builtin__strip_left)),
    );
    builtins.insert(
        "__strip_right".to_string(),
        Rc::new(Object::Builtin(builtin__strip_right)),
    );
    builtins.insert(
        "__upper".to_string(),
        Rc::new(Object::Builtin(builtin__upper)),
    );
    builtins.insert(
        "__lower".to_string(),
        Rc::new(Object::Builtin(builtin__lower)),
    );
    builtins.insert(
        "__replace".to_string(),
        Rc::new(Object::Builtin(builtin__replace)),
    );
    builtins.insert(
        "__starts_with".to_string(),
        Rc::new(Object::Builtin(builtin__starts_with)),
    );
    builtins.insert(
        "__ends_with".to_string(),
        Rc::new(Object::Builtin(builtin__ends_with)),
    );
    builtins.insert(
        "__contains_str".to_string(),
        Rc::new(Object::Builtin(builtin__contains_str)),
    );
    builtins.insert(
        "__sort".to_string(),
        Rc::new(Object::Builtin(builtin__sort)),
    );
    builtins.insert(
        "__read_file".to_string(),
        Rc::new(Object::Builtin(builtin__read_file)),
    );
    builtins.insert(
        "__write_file".to_string(),
        Rc::new(Object::Builtin(builtin__write_file)),
    );
    builtins.insert(
        "__append_file".to_string(),
        Rc::new(Object::Builtin(builtin__append_file)),
    );
    builtins.insert(
        "__file_exists".to_string(),
        Rc::new(Object::Builtin(builtin__file_exists)),
    );
    builtins.insert(
        "__input".to_string(),
        Rc::new(Object::Builtin(builtin__input)),
    );
    builtins.insert(
        "__read_line".to_string(),
        Rc::new(Object::Builtin(builtin__read_line)),
    );
    builtins.insert(
        "__read_key".to_string(),
        Rc::new(Object::Builtin(builtin__read_key)),
    );

    // OS builtins
    builtins.insert(
        "__exec".to_string(),
        Rc::new(Object::Builtin(builtin__exec)),
    );
    builtins.insert(
        "__os_name".to_string(),
        Rc::new(Object::Builtin(builtin__os_name)),
    );
    builtins.insert(
        "__os_arch".to_string(),
        Rc::new(Object::Builtin(builtin__os_arch)),
    );
    builtins.insert(
        "__env_get".to_string(),
        Rc::new(Object::Builtin(builtin__env_get)),
    );
    builtins.insert(
        "__env_set".to_string(),
        Rc::new(Object::Builtin(builtin__env_set)),
    );
    builtins.insert(
        "__env_vars".to_string(),
        Rc::new(Object::Builtin(builtin__env_vars)),
    );
    builtins.insert("__cwd".to_string(), Rc::new(Object::Builtin(builtin__cwd)));
    builtins.insert(
        "__chdir".to_string(),
        Rc::new(Object::Builtin(builtin__chdir)),
    );
    builtins.insert(
        "__list_dir".to_string(),
        Rc::new(Object::Builtin(builtin__list_dir)),
    );
    builtins.insert(
        "__walk_dir".to_string(),
        Rc::new(Object::Builtin(builtin__walk_dir)),
    );
    builtins.insert(
        "__mkdir".to_string(),
        Rc::new(Object::Builtin(builtin__mkdir)),
    );
    builtins.insert(
        "__rmdir".to_string(),
        Rc::new(Object::Builtin(builtin__rmdir)),
    );
    builtins.insert(
        "__remove".to_string(),
        Rc::new(Object::Builtin(builtin__remove_file)),
    );
    builtins.insert(
        "__is_dir".to_string(),
        Rc::new(Object::Builtin(builtin__is_dir)),
    );
    builtins.insert(
        "__is_file".to_string(),
        Rc::new(Object::Builtin(builtin__is_file)),
    );
    builtins.insert(
        "__exit".to_string(),
        Rc::new(Object::Builtin(builtin__exit)),
    );
    builtins.insert("__pid".to_string(), Rc::new(Object::Builtin(builtin__pid)));

    // Type conversion builtins
    builtins.insert("int".to_string(), Rc::new(Object::Builtin(builtin_int)));
    builtins.insert("float".to_string(), Rc::new(Object::Builtin(builtin_float)));

    // Time builtins
    builtins.insert(
        "__time_now".to_string(),
        Rc::new(Object::Builtin(builtin__time_now)),
    );
    builtins.insert(
        "__time_now_ms".to_string(),
        Rc::new(Object::Builtin(builtin__time_now_ms)),
    );
    builtins.insert(
        "__time_sleep".to_string(),
        Rc::new(Object::Builtin(builtin__time_sleep)),
    );
    builtins.insert(
        "__time_monotonic".to_string(),
        Rc::new(Object::Builtin(builtin__time_monotonic)),
    );

    // Random builtins
    builtins.insert(
        "__rand_int".to_string(),
        Rc::new(Object::Builtin(builtin__rand_int)),
    );
    builtins.insert(
        "__rand_float".to_string(),
        Rc::new(Object::Builtin(builtin__rand_float)),
    );
    builtins.insert(
        "__rand_seed".to_string(),
        Rc::new(Object::Builtin(builtin__rand_seed)),
    );

    // Path builtins
    builtins.insert(
        "__path_join".to_string(),
        Rc::new(Object::Builtin(builtin__path_join)),
    );
    builtins.insert(
        "__path_ext".to_string(),
        Rc::new(Object::Builtin(builtin__path_ext)),
    );
    builtins.insert(
        "__path_filename".to_string(),
        Rc::new(Object::Builtin(builtin__path_filename)),
    );
    builtins.insert(
        "__path_parent".to_string(),
        Rc::new(Object::Builtin(builtin__path_parent)),
    );
    builtins.insert(
        "__path_stem".to_string(),
        Rc::new(Object::Builtin(builtin__path_stem)),
    );
    builtins.insert(
        "__path_is_absolute".to_string(),
        Rc::new(Object::Builtin(builtin__path_is_absolute)),
    );

    // JSON builtins
    builtins.insert(
        "__json_parse".to_string(),
        Rc::new(Object::Builtin(builtin__json_parse)),
    );
    builtins.insert(
        "__json_stringify".to_string(),
        Rc::new(Object::Builtin(builtin__json_stringify)),
    );
    builtins.insert(
        "__toml_parse".to_string(),
        Rc::new(Object::Builtin(builtin__toml_parse)),
    );
    builtins.insert(
        "__toml_stringify".to_string(),
        Rc::new(Object::Builtin(builtin__toml_stringify)),
    );

    // HTTP builtins
    builtins.insert(
        "__http_request".to_string(),
        Rc::new(Object::Builtin(builtin__http_request)),
    );
    builtins.insert("__net_download".to_string(), Rc::new(Object::Builtin(builtin_net_download)));
    builtins.insert("__net_upload".to_string(), Rc::new(Object::Builtin(builtin_net_upload)));
    builtins.insert("__net_http_open".to_string(), Rc::new(Object::Builtin(builtin_net_http_open)));
    builtins.insert("__net_http_read".to_string(), Rc::new(Object::Builtin(builtin_net_http_read)));
    builtins.insert("__net_http_read_line".to_string(), Rc::new(Object::Builtin(builtin_net_http_read_line)));
    builtins.insert("__net_tcp_connect".to_string(), Rc::new(Object::Builtin(builtin_net_tcp_connect)));
    builtins.insert("__net_tcp_listen".to_string(), Rc::new(Object::Builtin(builtin_net_tcp_listen)));
    builtins.insert("__net_tcp_accept".to_string(), Rc::new(Object::Builtin(builtin_net_tcp_accept)));
    builtins.insert("__net_send".to_string(), Rc::new(Object::Builtin(builtin_net_send)));
    builtins.insert("__net_receive".to_string(), Rc::new(Object::Builtin(builtin_net_receive)));
    builtins.insert("__net_close".to_string(), Rc::new(Object::Builtin(builtin_net_close)));
    builtins.insert("__net_udp_bind".to_string(), Rc::new(Object::Builtin(builtin_net_udp_bind)));
    builtins.insert("__net_udp_send".to_string(), Rc::new(Object::Builtin(builtin_net_udp_send)));
    builtins.insert("__net_udp_receive".to_string(), Rc::new(Object::Builtin(builtin_net_udp_receive)));

    builtins
}

fn builtin_print(args: Vec<Rc<Object>>) -> Rc<Object> {
    use std::io::Write;
    let output: Vec<String> = args.iter().map(|a| a.to_string()).collect();
    // `print` does NOT append a newline (that's `println`); flush so it shows
    // immediately, matching the VM and supporting token-by-token streaming.
    let mut out = std::io::stdout().lock();
    let _ = write!(out, "{}", output.join(" "));
    let _ = out.flush();
    Rc::new(Object::None)
}

fn builtin_println(args: Vec<Rc<Object>>) -> Rc<Object> {
    let output: Vec<String> = args.iter().map(|a| a.to_string()).collect();
    println!("{}", output.join(" "));
    Rc::new(Object::None)
}

fn builtin_error(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "error: expected 1 argument, got {}",
            args.len()
        )));
    }
    Rc::new(Object::Error(args[0].to_string()))
}

fn builtin_len(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }

    match args[0].as_ref() {
        Object::String(s) => Rc::new(Object::Integer(s.len() as i64)),
        Object::Array(arr) => Rc::new(Object::Integer(arr.len() as i64)),
        Object::Tuple(t) => Rc::new(Object::Integer(t.len() as i64)),
        Object::Map(m) => Rc::new(Object::Integer(m.len() as i64)),
        Object::Set(s) => Rc::new(Object::Integer(s.len() as i64)),
        obj => Rc::new(Object::Error(format!(
            "argument to `len` not supported, got {}",
            obj.type_name()
        ))),
    }
}

fn builtin_push(mut args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments. got={}, want=2",
            args.len()
        )));
    }

    let new_elem = Rc::clone(&args[1]);

    // Try to reuse the array in-place if we hold the only reference
    match Rc::make_mut(&mut args[0]) {
        Object::Array(arr) => {
            arr.push(new_elem);
            Rc::clone(&args[0])
        }
        _ => Rc::new(Object::Error(
            "argument to `push` must be ARRAY".to_string(),
        )),
    }
}

fn builtin_first(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }

    match args[0].as_ref() {
        Object::Array(arr) => {
            if arr.is_empty() {
                Rc::new(Object::None)
            } else {
                Rc::clone(&arr[0])
            }
        }
        obj => Rc::new(Object::Error(format!(
            "argument to `first` must be ARRAY, got {}",
            obj.type_name()
        ))),
    }
}

fn builtin_last(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }

    match args[0].as_ref() {
        Object::Array(arr) => {
            if arr.is_empty() {
                Rc::new(Object::None)
            } else {
                Rc::clone(&arr[arr.len() - 1])
            }
        }
        obj => Rc::new(Object::Error(format!(
            "argument to `last` must be ARRAY, got {}",
            obj.type_name()
        ))),
    }
}

fn builtin_rest(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }

    match args[0].as_ref() {
        Object::Array(arr) => {
            // Match the VM: rest([]) -> [] (empty array), not None.
            if arr.is_empty() {
                Rc::new(Object::Array(Vec::new()))
            } else {
                Rc::new(Object::Array(arr[1..].to_vec()))
            }
        }
        obj => Rc::new(Object::Error(format!(
            "argument to `rest` must be ARRAY, got {}",
            obj.type_name()
        ))),
    }
}

fn builtin_type(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }

    if let Some(name) = args[0].struct_type_name() {
        return Rc::new(Object::String(name.to_string()));
    }
    Rc::new(Object::String(args[0].type_name().to_string()))
}

fn builtin_is_value(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }
    Rc::new(Object::Boolean(matches!(
        args[0].as_ref(),
        Object::Value(_)
    )))
}

fn builtin_is_error(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }
    Rc::new(Object::Boolean(matches!(
        args[0].as_ref(),
        Object::ErrorValue { .. }
    )))
}

// ord(`a`) -> 97 (char to integer)
fn builtin_ord(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }

    match args[0].as_ref() {
        Object::Char(c) => Rc::new(Object::Integer(*c as i64)),
        obj => Rc::new(Object::Error(format!(
            "argument to `ord` must be CHAR, got {}",
            obj.type_name()
        ))),
    }
}

// chr(97) -> `a` (integer to char)
fn builtin_chr(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }

    match args[0].as_ref() {
        Object::Integer(n) => {
            if *n < 0 || *n > 0x10FFFF {
                return Rc::new(Object::Error(format!("chr() argument out of range: {}", n)));
            }
            match char::from_u32(*n as u32) {
                Some(c) => Rc::new(Object::Char(c)),
                None => Rc::new(Object::Error(format!("invalid unicode codepoint: {}", n))),
            }
        }
        obj => Rc::new(Object::Error(format!(
            "argument to `chr` must be INTEGER, got {}",
            obj.type_name()
        ))),
    }
}

// str(`a`) -> "a" (char to string)
fn builtin_str(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }

    match args[0].as_ref() {
        Object::Char(c) => Rc::new(Object::String(c.to_string())),
        Object::Integer(n) => Rc::new(Object::String(n.to_string())),
        Object::Float(n) => Rc::new(Object::String(n.to_string())),
        Object::Boolean(b) => Rc::new(Object::String(
            if *b { "True" } else { "False" }.to_string(),
        )),
        Object::String(s) => Rc::new(Object::String(s.clone())),
        Object::Byte(n) => Rc::new(Object::String(n.to_string())),
        Object::Uint(n) => Rc::new(Object::String(n.to_string())),
        // Match the VM: fall back to the Display form for everything else
        // (arrays, maps, None, etc.) instead of erroring.
        obj => Rc::new(Object::String(obj.to_string())),
    }
}

// chars("hello") -> [`h`, `e`, `l`, `l`, `o`] (string to char array)
fn builtin_chars(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }

    match args[0].as_ref() {
        Object::String(s) => {
            let chars: Vec<Rc<Object>> = s.chars().map(|c| Rc::new(Object::Char(c))).collect();
            Rc::new(Object::Array(chars))
        }
        obj => Rc::new(Object::Error(format!(
            "argument to `chars` must be STRING, got {}",
            obj.type_name()
        ))),
    }
}

fn builtin_range(args: Vec<Rc<Object>>) -> Rc<Object> {
    // Match the VM: range(n) -> 0..n, range(start, end) -> start..end.
    let (start, end) = match args.len() {
        1 => match args[0].as_ref() {
            Object::Integer(n) => (0i64, *n),
            _ => {
                return Rc::new(Object::Error(
                    "range() requires integer arguments".to_string(),
                ));
            }
        },
        2 => match (args[0].as_ref(), args[1].as_ref()) {
            (Object::Integer(s), Object::Integer(e)) => (*s, *e),
            _ => {
                return Rc::new(Object::Error(
                    "range() requires integer arguments".to_string(),
                ));
            }
        },
        _ => {
            return Rc::new(Object::Error(format!(
                "range() takes 1 or 2 arguments, got {}",
                args.len()
            )));
        }
    };
    let mut num_range: Vec<Rc<Object>> = Vec::new();
    for i in start..end {
        num_range.push(Rc::new(Object::Integer(i)));
    }
    Rc::new(Object::Array(num_range))
}

fn builtin_byte(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::Byte(_) => Rc::clone(&args[0]),
        Object::Integer(n) => {
            if *n < 0 || *n > 255 {
                Rc::new(Object::Error(format!(
                    "byte() argument out of range (0-255): {}",
                    n
                )))
            } else {
                Rc::new(Object::Byte(*n as u8))
            }
        }
        Object::Uint(n) => {
            if *n > 255 {
                Rc::new(Object::Error(format!(
                    "byte() argument out of range (0-255): {}",
                    n
                )))
            } else {
                Rc::new(Object::Byte(*n as u8))
            }
        }
        Object::Char(c) => Rc::new(Object::Byte(*c as u8)),
        obj => Rc::new(Object::Error(format!(
            "cannot convert {} to BYTE",
            obj.type_name()
        ))),
    }
}

fn builtin_uint(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::Uint(_) => Rc::clone(&args[0]),
        Object::Integer(n) => {
            if *n < 0 {
                Rc::new(Object::Error(format!(
                    "uint() cannot convert negative: {}",
                    n
                )))
            } else {
                Rc::new(Object::Uint(*n as u64))
            }
        }
        Object::Byte(n) => Rc::new(Object::Uint(*n as u64)),
        Object::Float(f) => {
            if *f < 0.0 {
                Rc::new(Object::Error(format!(
                    "uint() cannot convert negative: {}",
                    f
                )))
            } else {
                Rc::new(Object::Uint(*f as u64))
            }
        }
        Object::String(s) => match s.parse::<u64>() {
            Ok(n) => Rc::new(Object::Uint(n)),
            Err(_) => Rc::new(Object::Error(format!("cannot convert \"{}\" to UINT", s))),
        },
        obj => Rc::new(Object::Error(format!(
            "cannot convert {} to UINT",
            obj.type_name()
        ))),
    }
}

fn builtin_set(args: Vec<Rc<Object>>) -> Rc<Object> {
    let mut elements = Vec::new();
    for arg in &args {
        if !elements.iter().any(|e: &Rc<Object>| e == arg) {
            elements.push(Rc::clone(arg));
        }
    }
    Rc::new(Object::Set(elements))
}

fn builtin_tuple(args: Vec<Rc<Object>>) -> Rc<Object> {
    Rc::new(Object::Tuple(args))
}

fn builtin_keys(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::Map(entries) => {
            let keys: Vec<Rc<Object>> = entries.iter().map(|(k, _)| Rc::clone(k)).collect();
            Rc::new(Object::Array(keys))
        }
        obj => Rc::new(Object::Error(format!(
            "argument to `keys` must be MAP, got {}",
            obj.type_name()
        ))),
    }
}

fn builtin_values(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::Map(entries) => {
            let vals: Vec<Rc<Object>> = entries.iter().map(|(_, v)| Rc::clone(v)).collect();
            Rc::new(Object::Array(vals))
        }
        obj => Rc::new(Object::Error(format!(
            "argument to `values` must be MAP, got {}",
            obj.type_name()
        ))),
    }
}

fn builtin_insert(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 3 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments. got={}, want=3",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::Map(entries) => {
            let mut new_entries = entries.clone();
            let key = Rc::clone(&args[1]);
            let val = Rc::clone(&args[2]);
            // Update existing or append
            if let Some(entry) = new_entries.iter_mut().find(|(k, _)| *k == key) {
                entry.1 = val;
            } else {
                new_entries.push((key, val));
            }
            Rc::new(Object::Map(new_entries))
        }
        obj => Rc::new(Object::Error(format!(
            "argument to `insert` must be MAP, got {}",
            obj.type_name()
        ))),
    }
}

fn builtin_remove(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments. got={}, want=2",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::Map(entries) => {
            let key = &args[1];
            let new_entries: Vec<_> = entries.iter().filter(|(k, _)| k != key).cloned().collect();
            Rc::new(Object::Map(new_entries))
        }
        Object::Set(elements) => {
            let val = &args[1];
            let new_elements: Vec<_> = elements.iter().filter(|e| e != &val).cloned().collect();
            Rc::new(Object::Set(new_elements))
        }
        obj => Rc::new(Object::Error(format!(
            "argument to `remove` must be MAP or SET, got {}",
            obj.type_name()
        ))),
    }
}

fn builtin_has(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments. got={}, want=2",
            args.len()
        )));
    }
    let val = &args[1];
    match args[0].as_ref() {
        Object::Array(arr) => Rc::new(Object::Boolean(arr.iter().any(|e| e == val))),
        Object::Set(elements) => Rc::new(Object::Boolean(elements.iter().any(|e| e == val))),
        Object::Map(entries) => Rc::new(Object::Boolean(entries.iter().any(|(k, _)| k == val))),
        Object::Tuple(elements) => Rc::new(Object::Boolean(elements.iter().any(|e| e == val))),
        obj => Rc::new(Object::Error(format!(
            "argument to `has` must be a collection, got {}",
            obj.type_name()
        ))),
    }
}

// ── Internal builtins for stdlib ──

fn to_f64(obj: &Object) -> Option<f64> {
    match obj {
        Object::Float(f) => Some(*f),
        Object::Integer(n) => Some(*n as f64),
        _ => None,
    }
}

fn builtin__sqrt(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__sqrt: expected 1 argument, got {}",
            args.len()
        )));
    }
    match to_f64(&args[0]) {
        Some(n) => Rc::new(Object::Float(n.sqrt())),
        None => Rc::new(Object::Error(format!(
            "__sqrt: expected number, got {}",
            args[0].type_name()
        ))),
    }
}

fn builtin__floor(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__floor: expected 1 argument, got {}",
            args.len()
        )));
    }
    match to_f64(&args[0]) {
        Some(n) => Rc::new(Object::Integer(n.floor() as i64)),
        None => Rc::new(Object::Error(format!(
            "__floor: expected number, got {}",
            args[0].type_name()
        ))),
    }
}

fn builtin__ceil(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__ceil: expected 1 argument, got {}",
            args.len()
        )));
    }
    match to_f64(&args[0]) {
        Some(n) => Rc::new(Object::Integer(n.ceil() as i64)),
        None => Rc::new(Object::Error(format!(
            "__ceil: expected number, got {}",
            args[0].type_name()
        ))),
    }
}

fn builtin__round(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__round: expected 1 argument, got {}",
            args.len()
        )));
    }
    match to_f64(&args[0]) {
        Some(n) => Rc::new(Object::Integer(n.round() as i64)),
        None => Rc::new(Object::Error(format!(
            "__round: expected number, got {}",
            args[0].type_name()
        ))),
    }
}

// Shared helpers for floating-point math builtins.
fn unary_f64(args: &[Rc<Object>], name: &str, f: fn(f64) -> f64) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "{}: expected 1 argument, got {}",
            name,
            args.len()
        )));
    }
    match to_f64(&args[0]) {
        Some(n) => Rc::new(Object::Float(f(n))),
        None => Rc::new(Object::Error(format!(
            "{}: expected number, got {}",
            name,
            args[0].type_name()
        ))),
    }
}

fn binary_f64(args: &[Rc<Object>], name: &str, f: fn(f64, f64) -> f64) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error(format!(
            "{}: expected 2 arguments, got {}",
            name,
            args.len()
        )));
    }
    match (to_f64(&args[0]), to_f64(&args[1])) {
        (Some(a), Some(b)) => Rc::new(Object::Float(f(a, b))),
        _ => Rc::new(Object::Error(format!("{}: expected numbers", name))),
    }
}

fn builtin__sin(args: Vec<Rc<Object>>) -> Rc<Object> {
    unary_f64(&args, "__sin", f64::sin)
}
fn builtin__cos(args: Vec<Rc<Object>>) -> Rc<Object> {
    unary_f64(&args, "__cos", f64::cos)
}
fn builtin__tan(args: Vec<Rc<Object>>) -> Rc<Object> {
    unary_f64(&args, "__tan", f64::tan)
}
fn builtin__asin(args: Vec<Rc<Object>>) -> Rc<Object> {
    unary_f64(&args, "__asin", f64::asin)
}
fn builtin__acos(args: Vec<Rc<Object>>) -> Rc<Object> {
    unary_f64(&args, "__acos", f64::acos)
}
fn builtin__atan(args: Vec<Rc<Object>>) -> Rc<Object> {
    unary_f64(&args, "__atan", f64::atan)
}
fn builtin__atan2(args: Vec<Rc<Object>>) -> Rc<Object> {
    binary_f64(&args, "__atan2", f64::atan2)
}
fn builtin__ln(args: Vec<Rc<Object>>) -> Rc<Object> {
    unary_f64(&args, "__ln", f64::ln)
}
fn builtin__log2(args: Vec<Rc<Object>>) -> Rc<Object> {
    unary_f64(&args, "__log2", f64::log2)
}
fn builtin__log10(args: Vec<Rc<Object>>) -> Rc<Object> {
    unary_f64(&args, "__log10", f64::log10)
}
fn builtin__exp(args: Vec<Rc<Object>>) -> Rc<Object> {
    unary_f64(&args, "__exp", f64::exp)
}
fn builtin__powf(args: Vec<Rc<Object>>) -> Rc<Object> {
    binary_f64(&args, "__powf", f64::powf)
}

// ── Shared helpers for native extension builtins ──

fn hard_err(msg: impl Into<String>) -> Rc<Object> {
    Rc::new(Object::Error(msg.into()))
}

/// A catchable (Error || Value) error, for recoverable failures like a bad
/// decode or parse. Callers can handle it with `is_error` / the `result` module.
fn soft_err(msg: impl Into<String>, tag: &str) -> Rc<Object> {
    Rc::new(Object::ErrorValue {
        msg: msg.into(),
        tag: Some(tag.to_string()),
    })
}

fn str_arg<'a>(obj: &'a Object) -> Option<&'a str> {
    match obj {
        Object::String(s) => Some(s.as_str()),
        _ => None,
    }
}

fn str_array(items: impl IntoIterator<Item = String>) -> Rc<Object> {
    Rc::new(Object::Array(
        items.into_iter().map(|s| Rc::new(Object::String(s))).collect(),
    ))
}

// ── regex ──

fn compile_re(pattern: &str, name: &str) -> Result<regex::Regex, Rc<Object>> {
    regex::Regex::new(pattern).map_err(|e| hard_err(format!("{}: invalid pattern: {}", name, e)))
}

fn builtin__regex_match(args: Vec<Rc<Object>>) -> Rc<Object> {
    match (args.first().and_then(|a| str_arg(a)), args.get(1).and_then(|a| str_arg(a))) {
        (Some(pat), Some(text)) if args.len() == 2 => match compile_re(pat, "__regex_match") {
            Ok(re) => Rc::new(Object::Boolean(re.is_match(text))),
            Err(e) => e,
        },
        _ => hard_err("__regex_match: expected (pattern, text) strings"),
    }
}

fn builtin__regex_find(args: Vec<Rc<Object>>) -> Rc<Object> {
    match (args.first().and_then(|a| str_arg(a)), args.get(1).and_then(|a| str_arg(a))) {
        (Some(pat), Some(text)) if args.len() == 2 => match compile_re(pat, "__regex_find") {
            Ok(re) => match re.find(text) {
                Some(m) => Rc::new(Object::String(m.as_str().to_string())),
                None => Rc::new(Object::None),
            },
            Err(e) => e,
        },
        _ => hard_err("__regex_find: expected (pattern, text) strings"),
    }
}

fn builtin__regex_find_all(args: Vec<Rc<Object>>) -> Rc<Object> {
    match (args.first().and_then(|a| str_arg(a)), args.get(1).and_then(|a| str_arg(a))) {
        (Some(pat), Some(text)) if args.len() == 2 => match compile_re(pat, "__regex_find_all") {
            Ok(re) => str_array(re.find_iter(text).map(|m| m.as_str().to_string())),
            Err(e) => e,
        },
        _ => hard_err("__regex_find_all: expected (pattern, text) strings"),
    }
}

fn builtin__regex_replace(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 3 {
        return hard_err("__regex_replace: expected 3 arguments");
    }
    match (str_arg(&args[0]), str_arg(&args[1]), str_arg(&args[2])) {
        (Some(pat), Some(text), Some(rep)) => match compile_re(pat, "__regex_replace") {
            Ok(re) => Rc::new(Object::String(re.replace_all(text, rep).into_owned())),
            Err(e) => e,
        },
        _ => hard_err("__regex_replace: expected (pattern, text, replacement) strings"),
    }
}

fn builtin__regex_split(args: Vec<Rc<Object>>) -> Rc<Object> {
    match (args.first().and_then(|a| str_arg(a)), args.get(1).and_then(|a| str_arg(a))) {
        (Some(pat), Some(text)) if args.len() == 2 => match compile_re(pat, "__regex_split") {
            Ok(re) => str_array(re.split(text).map(|s| s.to_string())),
            Err(e) => e,
        },
        _ => hard_err("__regex_split: expected (pattern, text) strings"),
    }
}

fn builtin__regex_captures(args: Vec<Rc<Object>>) -> Rc<Object> {
    match (args.first().and_then(|a| str_arg(a)), args.get(1).and_then(|a| str_arg(a))) {
        (Some(pat), Some(text)) if args.len() == 2 => match compile_re(pat, "__regex_captures") {
            Ok(re) => match re.captures(text) {
                Some(caps) => str_array(
                    caps.iter()
                        .map(|m| m.map(|x| x.as_str().to_string()).unwrap_or_default()),
                ),
                None => Rc::new(Object::None),
            },
            Err(e) => e,
        },
        _ => hard_err("__regex_captures: expected (pattern, text) strings"),
    }
}

// ── datetime (UTC) ──

fn builtin__dt_now(args: Vec<Rc<Object>>) -> Rc<Object> {
    if !args.is_empty() {
        return hard_err("__dt_now: expected 0 arguments");
    }
    Rc::new(Object::Integer(chrono::Utc::now().timestamp()))
}

fn builtin__dt_format(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 2 {
        return hard_err("__dt_format: expected (timestamp, format) arguments");
    }
    let secs = match args[0].as_ref() {
        Object::Integer(n) => *n,
        _ => return hard_err("__dt_format: timestamp must be an integer (unix seconds)"),
    };
    let fmt = match str_arg(&args[1]) {
        Some(f) => f,
        None => return hard_err("__dt_format: format must be a string"),
    };
    match chrono::DateTime::<chrono::Utc>::from_timestamp(secs, 0) {
        Some(dt) => Rc::new(Object::String(dt.format(fmt).to_string())),
        None => soft_err("timestamp out of range", "datetime"),
    }
}

fn builtin__dt_parse(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 2 {
        return hard_err("__dt_parse: expected (text, format) arguments");
    }
    match (str_arg(&args[0]), str_arg(&args[1])) {
        (Some(text), Some(fmt)) => {
            match chrono::NaiveDateTime::parse_from_str(text, fmt) {
                Ok(ndt) => Rc::new(Object::Integer(ndt.and_utc().timestamp())),
                Err(e) => soft_err(format!("could not parse '{}': {}", text, e), "datetime"),
            }
        }
        _ => hard_err("__dt_parse: expected (text, format) strings"),
    }
}

// ── encoding ──

fn builtin__base64_encode(args: Vec<Rc<Object>>) -> Rc<Object> {
    match args.first().and_then(|a| str_arg(a)) {
        Some(s) if args.len() == 1 => Rc::new(Object::String(
            base64::engine::general_purpose::STANDARD.encode(s.as_bytes()),
        )),
        _ => hard_err("__base64_encode: expected 1 string argument"),
    }
}

fn builtin__base64_decode(args: Vec<Rc<Object>>) -> Rc<Object> {
    match args.first().and_then(|a| str_arg(a)) {
        Some(s) if args.len() == 1 => {
            match base64::engine::general_purpose::STANDARD.decode(s.as_bytes()) {
                Ok(bytes) => match String::from_utf8(bytes) {
                    Ok(text) => Rc::new(Object::String(text)),
                    Err(_) => soft_err("decoded bytes are not valid UTF-8", "encoding"),
                },
                Err(e) => soft_err(format!("invalid base64: {}", e), "encoding"),
            }
        }
        _ => hard_err("__base64_decode: expected 1 string argument"),
    }
}

fn builtin__hex_encode(args: Vec<Rc<Object>>) -> Rc<Object> {
    match args.first().and_then(|a| str_arg(a)) {
        Some(s) if args.len() == 1 => Rc::new(Object::String(hex::encode(s.as_bytes()))),
        _ => hard_err("__hex_encode: expected 1 string argument"),
    }
}

fn builtin__hex_decode(args: Vec<Rc<Object>>) -> Rc<Object> {
    match args.first().and_then(|a| str_arg(a)) {
        Some(s) if args.len() == 1 => match hex::decode(s) {
            Ok(bytes) => match String::from_utf8(bytes) {
                Ok(text) => Rc::new(Object::String(text)),
                Err(_) => soft_err("decoded bytes are not valid UTF-8", "encoding"),
            },
            Err(e) => soft_err(format!("invalid hex: {}", e), "encoding"),
        },
        _ => hard_err("__hex_decode: expected 1 string argument"),
    }
}

fn builtin__url_encode(args: Vec<Rc<Object>>) -> Rc<Object> {
    match args.first().and_then(|a| str_arg(a)) {
        Some(s) if args.len() == 1 => {
            Rc::new(Object::String(urlencoding::encode(s).into_owned()))
        }
        _ => hard_err("__url_encode: expected 1 string argument"),
    }
}

fn builtin__url_decode(args: Vec<Rc<Object>>) -> Rc<Object> {
    match args.first().and_then(|a| str_arg(a)) {
        Some(s) if args.len() == 1 => match urlencoding::decode(s) {
            Ok(text) => Rc::new(Object::String(text.into_owned())),
            Err(e) => soft_err(format!("invalid url encoding: {}", e), "encoding"),
        },
        _ => hard_err("__url_decode: expected 1 string argument"),
    }
}

// ── hash (hex digests) ──

fn builtin__sha256(args: Vec<Rc<Object>>) -> Rc<Object> {
    match args.first().and_then(|a| str_arg(a)) {
        Some(s) if args.len() == 1 => {
            let mut h = sha2::Sha256::new();
            h.update(s.as_bytes());
            Rc::new(Object::String(hex::encode(h.finalize())))
        }
        _ => hard_err("__sha256: expected 1 string argument"),
    }
}

fn builtin__sha1(args: Vec<Rc<Object>>) -> Rc<Object> {
    match args.first().and_then(|a| str_arg(a)) {
        Some(s) if args.len() == 1 => {
            let mut h = sha1::Sha1::new();
            h.update(s.as_bytes());
            Rc::new(Object::String(hex::encode(h.finalize())))
        }
        _ => hard_err("__sha1: expected 1 string argument"),
    }
}

fn builtin__md5(args: Vec<Rc<Object>>) -> Rc<Object> {
    match args.first().and_then(|a| str_arg(a)) {
        Some(s) if args.len() == 1 => {
            let mut h = md5::Md5::new();
            h.update(s.as_bytes());
            Rc::new(Object::String(hex::encode(h.finalize())))
        }
        _ => hard_err("__md5: expected 1 string argument"),
    }
}

fn builtin__split(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error(format!(
            "__split: expected 2 arguments, got {}",
            args.len()
        )));
    }
    match (args[0].as_ref(), args[1].as_ref()) {
        (Object::String(s), Object::String(delim)) => {
            let parts: Vec<Rc<Object>> = s
                .split(delim.as_str())
                .map(|p| Rc::new(Object::String(p.to_string())) as Rc<Object>)
                .collect();
            Rc::new(Object::Array(parts))
        }
        _ => Rc::new(Object::Error(
            "__split: expected (STRING, STRING)".to_string(),
        )),
    }
}

fn builtin__join(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error(format!(
            "__join: expected 2 arguments, got {}",
            args.len()
        )));
    }
    match (args[0].as_ref(), args[1].as_ref()) {
        (Object::Array(arr), Object::String(delim)) => {
            let parts: Vec<String> = arr.iter().map(|e| e.to_string()).collect();
            Rc::new(Object::String(parts.join(delim)))
        }
        _ => Rc::new(Object::Error(
            "__join: expected (ARRAY, STRING)".to_string(),
        )),
    }
}

fn builtin__trim(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__trim: expected 1 argument, got {}",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::String(s) => Rc::new(Object::String(s.trim().to_string())),
        _ => Rc::new(Object::Error(format!(
            "__trim: expected STRING, got {}",
            args[0].type_name()
        ))),
    }
}

fn builtin__strip(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error(format!(
            "__strip: expected 2 arguments, got {}",
            args.len()
        )));
    }
    match (args[0].as_ref(), args[1].as_ref()) {
        (Object::String(s), Object::String(chars)) => {
            let char_set: Vec<char> = chars.chars().collect();
            let result = s.trim_matches(|c| char_set.contains(&c)).to_string();
            Rc::new(Object::String(result))
        }
        _ => Rc::new(Object::Error(format!(
            "__strip: expected (STRING, STRING), got ({}, {})",
            args[0].type_name(),
            args[1].type_name()
        ))),
    }
}

fn builtin__strip_left(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error(format!(
            "__strip_left: expected 2 arguments, got {}",
            args.len()
        )));
    }
    match (args[0].as_ref(), args[1].as_ref()) {
        (Object::String(s), Object::String(chars)) => {
            let char_set: Vec<char> = chars.chars().collect();
            let result = s.trim_start_matches(|c| char_set.contains(&c)).to_string();
            Rc::new(Object::String(result))
        }
        _ => Rc::new(Object::Error(format!(
            "__strip_left: expected (STRING, STRING), got ({}, {})",
            args[0].type_name(),
            args[1].type_name()
        ))),
    }
}

fn builtin__strip_right(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error(format!(
            "__strip_right: expected 2 arguments, got {}",
            args.len()
        )));
    }
    match (args[0].as_ref(), args[1].as_ref()) {
        (Object::String(s), Object::String(chars)) => {
            let char_set: Vec<char> = chars.chars().collect();
            let result = s.trim_end_matches(|c| char_set.contains(&c)).to_string();
            Rc::new(Object::String(result))
        }
        _ => Rc::new(Object::Error(format!(
            "__strip_right: expected (STRING, STRING), got ({}, {})",
            args[0].type_name(),
            args[1].type_name()
        ))),
    }
}

fn builtin__upper(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__upper: expected 1 argument, got {}",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::String(s) => Rc::new(Object::String(s.to_uppercase())),
        _ => Rc::new(Object::Error(format!(
            "__upper: expected STRING, got {}",
            args[0].type_name()
        ))),
    }
}

fn builtin__lower(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__lower: expected 1 argument, got {}",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::String(s) => Rc::new(Object::String(s.to_lowercase())),
        _ => Rc::new(Object::Error(format!(
            "__lower: expected STRING, got {}",
            args[0].type_name()
        ))),
    }
}

fn builtin__replace(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 3 {
        return Rc::new(Object::Error(format!(
            "__replace: expected 3 arguments, got {}",
            args.len()
        )));
    }
    match (args[0].as_ref(), args[1].as_ref(), args[2].as_ref()) {
        (Object::String(s), Object::String(old), Object::String(new_s)) => {
            Rc::new(Object::String(s.replace(old.as_str(), new_s.as_str())))
        }
        _ => Rc::new(Object::Error(
            "__replace: expected (STRING, STRING, STRING)".to_string(),
        )),
    }
}

fn builtin__starts_with(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error(format!(
            "__starts_with: expected 2 arguments, got {}",
            args.len()
        )));
    }
    match (args[0].as_ref(), args[1].as_ref()) {
        (Object::String(s), Object::String(prefix)) => {
            Rc::new(Object::Boolean(s.starts_with(prefix.as_str())))
        }
        _ => Rc::new(Object::Error(
            "__starts_with: expected (STRING, STRING)".to_string(),
        )),
    }
}

fn builtin__ends_with(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error(format!(
            "__ends_with: expected 2 arguments, got {}",
            args.len()
        )));
    }
    match (args[0].as_ref(), args[1].as_ref()) {
        (Object::String(s), Object::String(suffix)) => {
            Rc::new(Object::Boolean(s.ends_with(suffix.as_str())))
        }
        _ => Rc::new(Object::Error(
            "__ends_with: expected (STRING, STRING)".to_string(),
        )),
    }
}

fn builtin__contains_str(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error(format!(
            "__contains_str: expected 2 arguments, got {}",
            args.len()
        )));
    }
    match (args[0].as_ref(), args[1].as_ref()) {
        (Object::String(s), Object::String(sub)) => {
            Rc::new(Object::Boolean(s.contains(sub.as_str())))
        }
        _ => Rc::new(Object::Error(
            "__contains_str: expected (STRING, STRING)".to_string(),
        )),
    }
}

fn builtin__sort(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__sort: expected 1 argument, got {}",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::Array(arr) => {
            let mut sorted = arr.clone();
            sorted.sort_by(|a, b| match (a.as_ref(), b.as_ref()) {
                (Object::Integer(x), Object::Integer(y)) => x.cmp(y),
                (Object::Float(x), Object::Float(y)) => {
                    x.partial_cmp(y).unwrap_or(std::cmp::Ordering::Equal)
                }
                (Object::String(x), Object::String(y)) => x.cmp(y),
                _ => std::cmp::Ordering::Equal,
            });
            Rc::new(Object::Array(sorted))
        }
        _ => Rc::new(Object::Error(format!(
            "__sort: expected ARRAY, got {}",
            args[0].type_name()
        ))),
    }
}

fn builtin__read_file(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__read_file: expected 1 argument, got {}",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::String(path) => match fs::read_to_string(path) {
            Ok(contents) => Rc::new(Object::String(contents)),
            Err(e) => Rc::new(Object::Error(format!("__read_file: {}", e))),
        },
        _ => Rc::new(Object::Error(format!(
            "__read_file: expected STRING, got {}",
            args[0].type_name()
        ))),
    }
}

fn builtin__write_file(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error(format!(
            "__write_file: expected 2 arguments, got {}",
            args.len()
        )));
    }
    match (args[0].as_ref(), args[1].as_ref()) {
        (Object::String(path), Object::String(content)) => match fs::write(path, content) {
            Ok(_) => Rc::new(Object::None),
            Err(e) => Rc::new(Object::Error(format!("__write_file: {}", e))),
        },
        _ => Rc::new(Object::Error(
            "__write_file: expected (STRING, STRING)".to_string(),
        )),
    }
}

fn builtin__append_file(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error(format!(
            "__append_file: expected 2 arguments, got {}",
            args.len()
        )));
    }
    match (args[0].as_ref(), args[1].as_ref()) {
        (Object::String(path), Object::String(content)) => {
            use std::io::Write;
            match fs::OpenOptions::new().append(true).create(true).open(path) {
                Ok(mut file) => match file.write_all(content.as_bytes()) {
                    Ok(_) => Rc::new(Object::None),
                    Err(e) => Rc::new(Object::Error(format!("__append_file: {}", e))),
                },
                Err(e) => Rc::new(Object::Error(format!("__append_file: {}", e))),
            }
        }
        _ => Rc::new(Object::Error(
            "__append_file: expected (STRING, STRING)".to_string(),
        )),
    }
}

fn builtin__file_exists(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__file_exists: expected 1 argument, got {}",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::String(path) => Rc::new(Object::Boolean(std::path::Path::new(path).exists())),
        _ => Rc::new(Object::Error(format!(
            "__file_exists: expected STRING, got {}",
            args[0].type_name()
        ))),
    }
}

fn builtin__input(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() > 1 {
        return Rc::new(Object::Error(format!(
            "__input: expected 0 or 1 arguments, got {}",
            args.len()
        )));
    }
    // Print prompt if provided (without newline)
    if args.len() == 1 {
        match args[0].as_ref() {
            Object::String(prompt) => {
                use std::io::Write;
                print!("{}", prompt);
                if std::io::stdout().flush().is_err() {
                    return Rc::new(Object::Error("__input: failed to flush stdout".to_string()));
                }
            }
            _ => {
                return Rc::new(Object::Error(format!(
                    "__input: prompt must be STRING, got {}",
                    args[0].type_name()
                )));
            }
        }
    }
    let mut line = String::new();
    match std::io::stdin().read_line(&mut line) {
        Ok(0) => Rc::new(Object::None), // EOF
        Ok(_) => {
            // Strip trailing newline
            if line.ends_with('\n') {
                line.pop();
                if line.ends_with('\r') {
                    line.pop();
                }
            }
            Rc::new(Object::String(line))
        }
        Err(e) => Rc::new(Object::Error(format!("__input: {}", e))),
    }
}

fn builtin__read_line(args: Vec<Rc<Object>>) -> Rc<Object> {
    if !args.is_empty() {
        return Rc::new(Object::Error(format!(
            "__read_line: expected 0 arguments, got {}",
            args.len()
        )));
    }
    let mut line = String::new();
    match std::io::stdin().read_line(&mut line) {
        Ok(0) => Rc::new(Object::None), // EOF
        Ok(_) => {
            if line.ends_with('\n') {
                line.pop();
                if line.ends_with('\r') {
                    line.pop();
                }
            }
            Rc::new(Object::String(line))
        }
        Err(e) => Rc::new(Object::Error(format!("__read_line: {}", e))),
    }
}

// Read a single key (arrow keys included) in raw mode; see crate::keyinput.
fn builtin__read_key(args: Vec<Rc<Object>>) -> Rc<Object> {
    if !args.is_empty() {
        return Rc::new(Object::Error(format!(
            "__read_key: expected 0 arguments, got {}",
            args.len()
        )));
    }
    match crate::keyinput::read_key() {
        Ok(k) => Rc::new(Object::String(k)),
        Err(e) => Rc::new(Object::Error(format!("__read_key: {}", e))),
    }
}

// ── OS builtins ──

fn builtin__exec(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.is_empty() {
        return Rc::new(Object::Error(
            "__exec: expected at least 1 argument".to_string(),
        ));
    }
    let cmd = match args[0].as_ref() {
        Object::String(s) => s.clone(),
        _ => {
            return Rc::new(Object::Error(format!(
                "__exec: expected STRING command, got {}",
                args[0].type_name()
            )));
        }
    };
    let extra_args: Vec<String> = args[1..].iter().map(|a| a.to_string()).collect();

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

    let full_cmd = if extra_args.is_empty() {
        cmd
    } else {
        format!("{} {}", cmd, extra_args.join(" "))
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
            // Return a map with stdout, stderr, and exit code
            let entries: Vec<(Rc<Object>, Rc<Object>)> = vec![
                (
                    Rc::new(Object::String("stdout".to_string())),
                    Rc::new(Object::String(stdout)),
                ),
                (
                    Rc::new(Object::String("stderr".to_string())),
                    Rc::new(Object::String(stderr)),
                ),
                (
                    Rc::new(Object::String("code".to_string())),
                    Rc::new(Object::Integer(code as i64)),
                ),
            ];
            Rc::new(Object::Map(entries))
        }
        Err(e) => Rc::new(Object::Error(format!("__exec: {}", e))),
    }
}

fn builtin__os_name(_args: Vec<Rc<Object>>) -> Rc<Object> {
    Rc::new(Object::String(std::env::consts::OS.to_string()))
}

fn builtin__os_arch(_args: Vec<Rc<Object>>) -> Rc<Object> {
    Rc::new(Object::String(std::env::consts::ARCH.to_string()))
}

fn builtin__env_get(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__env_get: expected 1 argument, got {}",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::String(key) => match std::env::var(key) {
            Ok(val) => Rc::new(Object::String(val)),
            Err(_) => Rc::new(Object::None),
        },
        _ => Rc::new(Object::Error(format!(
            "__env_get: expected STRING, got {}",
            args[0].type_name()
        ))),
    }
}

fn builtin__env_set(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error(format!(
            "__env_set: expected 2 arguments, got {}",
            args.len()
        )));
    }
    match (args[0].as_ref(), args[1].as_ref()) {
        (Object::String(key), Object::String(val)) => {
            // SAFETY: This is called from single-threaded Oxigen interpreter
            unsafe {
                std::env::set_var(key, val);
            }
            Rc::new(Object::None)
        }
        _ => Rc::new(Object::Error(
            "__env_set: expected (STRING, STRING)".to_string(),
        )),
    }
}

fn builtin__env_vars(_args: Vec<Rc<Object>>) -> Rc<Object> {
    let entries: Vec<(Rc<Object>, Rc<Object>)> = std::env::vars()
        .map(|(k, v)| {
            (
                Rc::new(Object::String(k)) as Rc<Object>,
                Rc::new(Object::String(v)) as Rc<Object>,
            )
        })
        .collect();
    Rc::new(Object::Map(entries))
}

fn builtin__cwd(_args: Vec<Rc<Object>>) -> Rc<Object> {
    match std::env::current_dir() {
        Ok(p) => Rc::new(Object::String(p.to_string_lossy().to_string())),
        Err(e) => Rc::new(Object::Error(format!("__cwd: {}", e))),
    }
}

fn builtin__chdir(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__chdir: expected 1 argument, got {}",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::String(path) => match std::env::set_current_dir(path) {
            Ok(_) => Rc::new(Object::None),
            Err(e) => Rc::new(Object::Error(format!("__chdir: {}", e))),
        },
        _ => Rc::new(Object::Error(format!(
            "__chdir: expected STRING, got {}",
            args[0].type_name()
        ))),
    }
}

fn builtin__list_dir(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__list_dir: expected 1 argument, got {}",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::String(path) => match fs::read_dir(path) {
            Ok(entries) => {
                let items: Vec<Rc<Object>> = entries
                    .filter_map(|e| e.ok())
                    .map(|e| {
                        Rc::new(Object::String(e.path().to_string_lossy().to_string()))
                            as Rc<Object>
                    })
                    .collect();
                Rc::new(Object::Array(items))
            }
            Err(e) => Rc::new(Object::Error(format!("__list_dir: {}", e))),
        },
        _ => Rc::new(Object::Error(format!(
            "__list_dir: expected STRING, got {}",
            args[0].type_name()
        ))),
    }
}

fn walk_dir_recursive(dir: &std::path::Path, results: &mut Vec<Rc<Object>>) -> Result<(), String> {
    let entries = fs::read_dir(dir).map_err(|e| format!("__walk_dir: {}", e))?;
    for entry in entries {
        let entry = entry.map_err(|e| format!("__walk_dir: {}", e))?;
        let path = entry.path();
        results.push(Rc::new(Object::String(path.to_string_lossy().to_string())));
        if path.is_dir() {
            walk_dir_recursive(&path, results)?;
        }
    }
    Ok(())
}

fn builtin__walk_dir(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__walk_dir: expected 1 argument, got {}",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::String(path) => {
            let mut results = Vec::new();
            match walk_dir_recursive(std::path::Path::new(path), &mut results) {
                Ok(_) => Rc::new(Object::Array(results)),
                Err(e) => Rc::new(Object::Error(e)),
            }
        }
        _ => Rc::new(Object::Error(format!(
            "__walk_dir: expected STRING, got {}",
            args[0].type_name()
        ))),
    }
}

fn builtin__mkdir(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__mkdir: expected 1 argument, got {}",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::String(path) => match fs::create_dir_all(path) {
            Ok(_) => Rc::new(Object::None),
            Err(e) => Rc::new(Object::Error(format!("__mkdir: {}", e))),
        },
        _ => Rc::new(Object::Error(format!(
            "__mkdir: expected STRING, got {}",
            args[0].type_name()
        ))),
    }
}

fn builtin__rmdir(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__rmdir: expected 1 argument, got {}",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::String(path) => match fs::remove_dir_all(path) {
            Ok(_) => Rc::new(Object::None),
            Err(e) => Rc::new(Object::Error(format!("__rmdir: {}", e))),
        },
        _ => Rc::new(Object::Error(format!(
            "__rmdir: expected STRING, got {}",
            args[0].type_name()
        ))),
    }
}

fn builtin__remove_file(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__remove: expected 1 argument, got {}",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::String(path) => match fs::remove_file(path) {
            Ok(_) => Rc::new(Object::None),
            Err(e) => Rc::new(Object::Error(format!("__remove: {}", e))),
        },
        _ => Rc::new(Object::Error(format!(
            "__remove: expected STRING, got {}",
            args[0].type_name()
        ))),
    }
}

fn builtin__is_dir(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__is_dir: expected 1 argument, got {}",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::String(path) => Rc::new(Object::Boolean(
            std::path::Path::new(path.as_str()).is_dir(),
        )),
        _ => Rc::new(Object::Error(format!(
            "__is_dir: expected STRING, got {}",
            args[0].type_name()
        ))),
    }
}

fn builtin__is_file(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__is_file: expected 1 argument, got {}",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::String(path) => Rc::new(Object::Boolean(
            std::path::Path::new(path.as_str()).is_file(),
        )),
        _ => Rc::new(Object::Error(format!(
            "__is_file: expected STRING, got {}",
            args[0].type_name()
        ))),
    }
}

fn builtin__exit(args: Vec<Rc<Object>>) -> Rc<Object> {
    let code = if args.is_empty() {
        0
    } else {
        match args[0].as_ref() {
            Object::Integer(n) => *n as i32,
            _ => 1,
        }
    };
    std::process::exit(code);
}

fn builtin__pid(_args: Vec<Rc<Object>>) -> Rc<Object> {
    Rc::new(Object::Integer(std::process::id() as i64))
}

// ── Type conversion builtins ──

fn builtin_int(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "int: expected 1 argument, got {}",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::Integer(_) => Rc::clone(&args[0]),
        Object::Float(f) => Rc::new(Object::Integer(*f as i64)),
        Object::String(s) => match s.trim().parse::<i64>() {
            Ok(n) => Rc::new(Object::Integer(n)),
            Err(_) => match s.trim().parse::<f64>() {
                Ok(f) => Rc::new(Object::Integer(f as i64)),
                Err(_) => Rc::new(Object::Error(format!("int: cannot parse '{}'", s))),
            },
        },
        Object::Boolean(b) => Rc::new(Object::Integer(if *b { 1 } else { 0 })),
        Object::Char(c) => Rc::new(Object::Integer(*c as i64)),
        Object::Byte(b) => Rc::new(Object::Integer(*b as i64)),
        Object::Uint(n) => Rc::new(Object::Integer(*n as i64)),
        _ => Rc::new(Object::Error(format!(
            "int: cannot convert {} to INTEGER",
            args[0].type_name()
        ))),
    }
}

fn builtin_float(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "float: expected 1 argument, got {}",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::Float(_) => Rc::clone(&args[0]),
        Object::Integer(n) => Rc::new(Object::Float(*n as f64)),
        Object::String(s) => match s.trim().parse::<f64>() {
            Ok(f) => Rc::new(Object::Float(f)),
            Err(_) => Rc::new(Object::Error(format!("float: cannot parse '{}'", s))),
        },
        Object::Boolean(b) => Rc::new(Object::Float(if *b { 1.0 } else { 0.0 })),
        Object::Byte(b) => Rc::new(Object::Float(*b as f64)),
        Object::Uint(n) => Rc::new(Object::Float(*n as f64)),
        _ => Rc::new(Object::Error(format!(
            "float: cannot convert {} to FLOAT",
            args[0].type_name()
        ))),
    }
}

// ── Time builtins ──

fn builtin__time_now(_args: Vec<Rc<Object>>) -> Rc<Object> {
    match std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH) {
        Ok(d) => Rc::new(Object::Float(d.as_secs_f64())),
        Err(e) => Rc::new(Object::Error(format!("__time_now: {}", e))),
    }
}

fn builtin__time_now_ms(_args: Vec<Rc<Object>>) -> Rc<Object> {
    match std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH) {
        Ok(d) => Rc::new(Object::Integer(d.as_millis() as i64)),
        Err(e) => Rc::new(Object::Error(format!("__time_now_ms: {}", e))),
    }
}

fn builtin__time_sleep(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__time_sleep: expected 1 argument, got {}",
            args.len()
        )));
    }
    let ms = match args[0].as_ref() {
        Object::Integer(n) => *n as u64,
        Object::Float(f) => *f as u64,
        _ => {
            return Rc::new(Object::Error(format!(
                "__time_sleep: expected number, got {}",
                args[0].type_name()
            )));
        }
    };
    std::thread::sleep(std::time::Duration::from_millis(ms));
    Rc::new(Object::None)
}

fn builtin__time_monotonic(_args: Vec<Rc<Object>>) -> Rc<Object> {
    use std::time::Instant;
    thread_local! {
        static EPOCH: Instant = Instant::now();
    }
    EPOCH.with(|epoch| {
        let elapsed = epoch.elapsed();
        Rc::new(Object::Integer(elapsed.as_nanos() as i64))
    })
}

// ── Random builtins (xorshift64*) ──

use std::cell::Cell;

thread_local! {
    static RNG_STATE: Cell<u64> = {
        let seed = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos() as u64;
        Cell::new(if seed == 0 { 1 } else { seed })
    };
}

fn xorshift64star() -> u64 {
    RNG_STATE.with(|state| {
        let mut x = state.get();
        x ^= x >> 12;
        x ^= x << 25;
        x ^= x >> 27;
        state.set(x);
        x.wrapping_mul(0x2545F4914F6CDD1D)
    })
}

fn builtin__rand_int(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error(format!(
            "__rand_int: expected 2 arguments (min, max), got {}",
            args.len()
        )));
    }
    let min = match args[0].as_ref() {
        Object::Integer(n) => *n,
        _ => {
            return Rc::new(Object::Error(format!(
                "__rand_int: expected INTEGER, got {}",
                args[0].type_name()
            )));
        }
    };
    let max = match args[1].as_ref() {
        Object::Integer(n) => *n,
        _ => {
            return Rc::new(Object::Error(format!(
                "__rand_int: expected INTEGER, got {}",
                args[1].type_name()
            )));
        }
    };
    if min > max {
        return Rc::new(Object::Error(format!(
            "__rand_int: min ({}) > max ({})",
            min, max
        )));
    }
    let range = (max - min + 1) as u64;
    let val = min + (xorshift64star() % range) as i64;
    Rc::new(Object::Integer(val))
}

fn builtin__rand_float(_args: Vec<Rc<Object>>) -> Rc<Object> {
    let val = (xorshift64star() as f64) / (u64::MAX as f64);
    Rc::new(Object::Float(val))
}

fn builtin__rand_seed(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__rand_seed: expected 1 argument, got {}",
            args.len()
        )));
    }
    let seed = match args[0].as_ref() {
        Object::Integer(n) => *n as u64,
        _ => {
            return Rc::new(Object::Error(format!(
                "__rand_seed: expected INTEGER, got {}",
                args[0].type_name()
            )));
        }
    };
    RNG_STATE.with(|state| state.set(if seed == 0 { 1 } else { seed }));
    Rc::new(Object::None)
}

// ── Path builtins ──

fn builtin__path_join(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__path_join: expected 1 argument (array), got {}",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::Array(parts) => {
            let mut path = std::path::PathBuf::new();
            for part in parts {
                match part.as_ref() {
                    Object::String(s) => path.push(s),
                    _ => {
                        return Rc::new(Object::Error(
                            "__path_join: array elements must be strings".to_string(),
                        ));
                    }
                }
            }
            Rc::new(Object::String(path.to_string_lossy().to_string()))
        }
        _ => Rc::new(Object::Error(format!(
            "__path_join: expected ARRAY, got {}",
            args[0].type_name()
        ))),
    }
}

fn builtin__path_ext(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__path_ext: expected 1 argument, got {}",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::String(s) => {
            let p = std::path::Path::new(s);
            match p.extension() {
                Some(ext) => Rc::new(Object::String(ext.to_string_lossy().to_string())),
                None => Rc::new(Object::None),
            }
        }
        _ => Rc::new(Object::Error(format!(
            "__path_ext: expected STRING, got {}",
            args[0].type_name()
        ))),
    }
}

fn builtin__path_filename(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__path_filename: expected 1 argument, got {}",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::String(s) => {
            let p = std::path::Path::new(s);
            match p.file_name() {
                Some(name) => Rc::new(Object::String(name.to_string_lossy().to_string())),
                None => Rc::new(Object::None),
            }
        }
        _ => Rc::new(Object::Error(format!(
            "__path_filename: expected STRING, got {}",
            args[0].type_name()
        ))),
    }
}

fn builtin__path_parent(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__path_parent: expected 1 argument, got {}",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::String(s) => {
            let p = std::path::Path::new(s);
            match p.parent() {
                Some(parent) => Rc::new(Object::String(parent.to_string_lossy().to_string())),
                None => Rc::new(Object::None),
            }
        }
        _ => Rc::new(Object::Error(format!(
            "__path_parent: expected STRING, got {}",
            args[0].type_name()
        ))),
    }
}

fn builtin__path_stem(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__path_stem: expected 1 argument, got {}",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::String(s) => {
            let p = std::path::Path::new(s);
            match p.file_stem() {
                Some(stem) => Rc::new(Object::String(stem.to_string_lossy().to_string())),
                None => Rc::new(Object::None),
            }
        }
        _ => Rc::new(Object::Error(format!(
            "__path_stem: expected STRING, got {}",
            args[0].type_name()
        ))),
    }
}

fn builtin__path_is_absolute(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__path_is_absolute: expected 1 argument, got {}",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::String(s) => Rc::new(Object::Boolean(
            std::path::Path::new(s.as_str()).is_absolute(),
        )),
        _ => Rc::new(Object::Error(format!(
            "__path_is_absolute: expected STRING, got {}",
            args[0].type_name()
        ))),
    }
}

// ── JSON builtins ──

fn json_parse_value(chars: &[char], pos: &mut usize) -> Result<Rc<Object>, String> {
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

fn json_skip_ws(chars: &[char], pos: &mut usize) {
    while *pos < chars.len() && chars[*pos].is_ascii_whitespace() {
        *pos += 1;
    }
}

fn json_parse_string(chars: &[char], pos: &mut usize) -> Result<Rc<Object>, String> {
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
                'b' => s.push('\u{0008}'),
                'f' => s.push('\u{000C}'),
                'n' => s.push('\n'),
                'r' => s.push('\r'),
                't' => s.push('\t'),
                'u' => {
                    *pos += 1;
                    if *pos + 4 > chars.len() {
                        return Err("incomplete unicode escape".to_string());
                    }
                    let hex: String = chars[*pos..*pos + 4].iter().collect();
                    *pos += 3; // will be incremented below
                    let cp = u32::from_str_radix(&hex, 16)
                        .map_err(|_| format!("invalid unicode escape: \\u{}", hex))?;
                    s.push(char::from_u32(cp).unwrap_or('\u{FFFD}'));
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
    Ok(Rc::new(Object::String(s)))
}

fn json_parse_number(chars: &[char], pos: &mut usize) -> Result<Rc<Object>, String> {
    let start = *pos;
    if chars[*pos] == '-' {
        *pos += 1;
    }
    while *pos < chars.len() && chars[*pos].is_ascii_digit() {
        *pos += 1;
    }
    let mut is_float = false;
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
            .map(|f| Rc::new(Object::Float(f)))
            .map_err(|_| format!("invalid number: {}", num_str))
    } else {
        num_str
            .parse::<i64>()
            .map(|n| Rc::new(Object::Integer(n)))
            .map_err(|_| format!("invalid number: {}", num_str))
    }
}

fn json_parse_bool(chars: &[char], pos: &mut usize) -> Result<Rc<Object>, String> {
    if chars[*pos..].starts_with(&['t', 'r', 'u', 'e']) {
        *pos += 4;
        Ok(Rc::new(Object::Boolean(true)))
    } else if chars[*pos..].starts_with(&['f', 'a', 'l', 's', 'e']) {
        *pos += 5;
        Ok(Rc::new(Object::Boolean(false)))
    } else {
        Err(format!("unexpected token at position {}", pos))
    }
}

fn json_parse_null(chars: &[char], pos: &mut usize) -> Result<Rc<Object>, String> {
    if chars[*pos..].starts_with(&['n', 'u', 'l', 'l']) {
        *pos += 4;
        Ok(Rc::new(Object::None))
    } else {
        Err(format!("unexpected token at position {}", pos))
    }
}

fn json_parse_array(chars: &[char], pos: &mut usize) -> Result<Rc<Object>, String> {
    *pos += 1; // skip [
    json_skip_ws(chars, pos);
    let mut elements = Vec::new();
    if *pos < chars.len() && chars[*pos] == ']' {
        *pos += 1;
        return Ok(Rc::new(Object::Array(elements)));
    }
    loop {
        let val = json_parse_value(chars, pos)?;
        elements.push(val);
        json_skip_ws(chars, pos);
        if *pos >= chars.len() {
            return Err("unterminated array".to_string());
        }
        if chars[*pos] == ']' {
            *pos += 1;
            break;
        }
        if chars[*pos] != ',' {
            return Err(format!(
                "expected ',' or ']' in array, got '{}'",
                chars[*pos]
            ));
        }
        *pos += 1;
    }
    Ok(Rc::new(Object::Array(elements)))
}

fn json_parse_object(chars: &[char], pos: &mut usize) -> Result<Rc<Object>, String> {
    *pos += 1; // skip {
    json_skip_ws(chars, pos);
    let mut entries: Vec<(Rc<Object>, Rc<Object>)> = Vec::new();
    if *pos < chars.len() && chars[*pos] == '}' {
        *pos += 1;
        return Ok(Rc::new(Object::Map(entries)));
    }
    loop {
        json_skip_ws(chars, pos);
        if *pos >= chars.len() || chars[*pos] != '"' {
            return Err("expected string key in object".to_string());
        }
        let key = json_parse_string(chars, pos)?;
        json_skip_ws(chars, pos);
        if *pos >= chars.len() || chars[*pos] != ':' {
            return Err("expected ':' in object".to_string());
        }
        *pos += 1;
        let val = json_parse_value(chars, pos)?;
        entries.push((key, val));
        json_skip_ws(chars, pos);
        if *pos >= chars.len() {
            return Err("unterminated object".to_string());
        }
        if chars[*pos] == '}' {
            *pos += 1;
            break;
        }
        if chars[*pos] != ',' {
            return Err(format!(
                "expected ',' or '}}' in object, got '{}'",
                chars[*pos]
            ));
        }
        *pos += 1;
    }
    Ok(Rc::new(Object::Map(entries)))
}

fn object_to_json(obj: &Object) -> Result<String, String> {
    match obj {
        Object::None => Ok("null".to_string()),
        Object::Boolean(b) => Ok(if *b { "true" } else { "false" }.to_string()),
        Object::Integer(n) => Ok(n.to_string()),
        Object::Float(f) => {
            if f.is_nan() || f.is_infinite() {
                Ok("null".to_string())
            } else {
                Ok(f.to_string())
            }
        }
        Object::String(s) => {
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
        Object::Array(arr) => {
            let items: Result<Vec<String>, String> =
                arr.iter().map(|e| object_to_json(e)).collect();
            Ok(format!("[{}]", items?.join(",")))
        }
        Object::Map(entries) => {
            let items: Result<Vec<String>, String> = entries
                .iter()
                .map(|(k, v)| {
                    let key_json = object_to_json(k)?;
                    let val_json = object_to_json(v)?;
                    Ok(format!("{}:{}", key_json, val_json))
                })
                .collect();
            Ok(format!("{{{}}}", items?.join(",")))
        }
        Object::Tuple(elements) => {
            let items: Result<Vec<String>, String> =
                elements.iter().map(|e| object_to_json(e)).collect();
            Ok(format!("[{}]", items?.join(",")))
        }
        Object::Byte(n) => Ok(n.to_string()),
        Object::Uint(n) => Ok(n.to_string()),
        Object::Char(c) => Ok(format!("\"{}\"", c)),
        _ => Err(format!("cannot serialize {} to JSON", obj.type_name())),
    }
}

fn builtin__json_parse(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__json_parse: expected 1 argument, got {}",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::String(s) => {
            let chars: Vec<char> = s.chars().collect();
            let mut pos = 0;
            match json_parse_value(&chars, &mut pos) {
                Ok(val) => val,
                Err(e) => Rc::new(Object::Error(format!("json parse error: {}", e))),
            }
        }
        _ => Rc::new(Object::Error(format!(
            "__json_parse: expected STRING, got {}",
            args[0].type_name()
        ))),
    }
}

fn builtin__json_stringify(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__json_stringify: expected 1 argument, got {}",
            args.len()
        )));
    }
    match object_to_json(&args[0]) {
        Ok(s) => Rc::new(Object::String(s)),
        Err(e) => Rc::new(Object::Error(format!("json stringify error: {}", e))),
    }
}

fn toml_value_to_object(value: TomlValue) -> Rc<Object> {
    match value {
        TomlValue::String(s) => Rc::new(Object::String(s)),
        TomlValue::Integer(i) => Rc::new(Object::Integer(i)),
        TomlValue::Float(f) => Rc::new(Object::Float(f)),
        TomlValue::Boolean(b) => Rc::new(Object::Boolean(b)),
        TomlValue::Datetime(dt) => Rc::new(Object::String(dt.to_string())),
        TomlValue::Array(values) => Rc::new(Object::Array(
            values.into_iter().map(toml_value_to_object).collect(),
        )),
        TomlValue::Table(entries) => Rc::new(Object::Map(
            entries
                .into_iter()
                .map(|(key, value)| (Rc::new(Object::String(key)), toml_value_to_object(value)))
                .collect(),
        )),
    }
}

fn builtin__toml_parse(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__toml_parse: expected 1 argument, got {}",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::String(s) => match toml::from_str::<TomlValue>(s) {
            Ok(value) => toml_value_to_object(value),
            Err(e) => Rc::new(Object::Error(format!("toml parse error: {}", e))),
        },
        _ => Rc::new(Object::Error(format!(
            "__toml_parse: expected STRING, got {}",
            args[0].type_name()
        ))),
    }
}

fn object_to_toml_value(obj: &Object) -> Result<TomlValue, String> {
    match obj {
        Object::String(s) => Ok(TomlValue::String(s.clone())),
        Object::Integer(n) => Ok(TomlValue::Integer(*n)),
        Object::Float(f) => {
            if f.is_nan() || f.is_infinite() {
                Err("TOML does not support NaN or Infinity".to_string())
            } else {
                Ok(TomlValue::Float(*f))
            }
        }
        Object::Boolean(b) => Ok(TomlValue::Boolean(*b)),
        Object::Uint(n) => Ok(TomlValue::Integer(*n as i64)),
        Object::Byte(n) => Ok(TomlValue::Integer(*n as i64)),
        Object::Char(c) => Ok(TomlValue::String(c.to_string())),
        Object::Array(arr) => {
            let items: Result<Vec<TomlValue>, String> =
                arr.iter().map(|e| object_to_toml_value(e)).collect();
            Ok(TomlValue::Array(items?))
        }
        Object::Tuple(elements) => {
            let items: Result<Vec<TomlValue>, String> =
                elements.iter().map(|e| object_to_toml_value(e)).collect();
            Ok(TomlValue::Array(items?))
        }
        Object::Map(entries) => {
            let mut table = toml::map::Map::new();
            for (k, v) in entries {
                let key = match k.as_ref() {
                    Object::String(s) => s.clone(),
                    _ => {
                        return Err(format!(
                            "TOML table keys must be strings, got {}",
                            k.type_name()
                        ));
                    }
                };
                table.insert(key, object_to_toml_value(v)?);
            }
            Ok(TomlValue::Table(table))
        }
        Object::None => Err("TOML does not support null/None values".to_string()),
        _ => Err(format!("cannot serialize {} to TOML", obj.type_name())),
    }
}

fn builtin__toml_stringify(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "__toml_stringify: expected 1 argument, got {}",
            args.len()
        )));
    }
    match object_to_toml_value(&args[0]) {
        Ok(value) => match toml::to_string_pretty(&value) {
            Ok(s) => Rc::new(Object::String(s)),
            Err(e) => Rc::new(Object::Error(format!("toml stringify error: {}", e))),
        },
        Err(e) => Rc::new(Object::Error(format!("toml stringify error: {}", e))),
    }
}

// ── HTTP builtins ──

fn http_headers_from_args(args: &[Rc<Object>]) -> Vec<(String, String)> {
    let mut headers = Vec::new();
    if args.len() >= 3 {
        if let Object::Map(entries) = args[2].as_ref() {
            for (k, v) in entries {
                if let (Object::String(key), Object::String(val)) = (k.as_ref(), v.as_ref()) {
                    headers.push((key.clone(), val.clone()));
                }
            }
        }
    }
    headers
}

fn http_body_from_args(args: &[Rc<Object>]) -> Result<Option<String>, String> {
    if args.len() >= 4 {
        match args[3].as_ref() {
            Object::String(s) => Ok(Some(s.clone())),
            Object::None => Ok(None),
            _ => Err(format!(
                "__http_request: body must be STRING, got {}",
                args[3].type_name()
            )),
        }
    } else {
        Ok(None)
    }
}

fn http_response_to_object(body: &mut ureq::Body) -> Rc<Object> {
    match body.read_to_string() {
        Ok(s) => Rc::new(Object::String(s)),
        Err(_) => Rc::new(Object::String(String::new())),
    }
}

fn builtin__http_request(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() < 2 || args.len() > 4 {
        return Rc::new(Object::Error(
            "__http_request: expected 2-4 arguments (method, url, headers?, body?)".to_string(),
        ));
    }
    let method = match args[0].as_ref() {
        Object::String(s) => s.to_uppercase(),
        _ => {
            return Rc::new(Object::Error(format!(
                "__http_request: method must be STRING, got {}",
                args[0].type_name()
            )));
        }
    };
    let url = match args[1].as_ref() {
        Object::String(s) => s.clone(),
        _ => {
            return Rc::new(Object::Error(format!(
                "__http_request: url must be STRING, got {}",
                args[1].type_name()
            )));
        }
    };

    let headers = http_headers_from_args(&args);
    let body = match http_body_from_args(&args) {
        Ok(b) => b,
        Err(e) => return Rc::new(Object::Error(e)),
    };

    macro_rules! apply_headers {
        ($req:expr, $headers:expr) => {{
            let mut r = $req;
            for (k, v) in $headers {
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
                    let (_, mut body) = resp.into_parts();
                    let body_obj = http_response_to_object(&mut body);
                    let entries: Vec<(Rc<Object>, Rc<Object>)> = vec![
                        (
                            Rc::new(Object::String("status".to_string())),
                            Rc::new(Object::Integer(status as i64)),
                        ),
                        (Rc::new(Object::String("body".to_string())), body_obj),
                    ];
                    Rc::new(Object::Map(entries))
                }
                Err(e) => Rc::new(Object::Error(format!("http error: {}", e))),
            }
        };
    }

    match method.as_str() {
        "GET" => handle_response!(apply_headers!(ureq::get(&url), &headers).call()),
        "HEAD" => handle_response!(apply_headers!(ureq::head(&url), &headers).call()),
        "DELETE" => handle_response!(apply_headers!(ureq::delete(&url), &headers).call()),
        "POST" => {
            let req = apply_headers!(ureq::post(&url), &headers);
            handle_response!(match &body {
                Some(b) => req.send(b.as_bytes()),
                None => req.send_empty(),
            })
        }
        "PUT" => {
            let req = apply_headers!(ureq::put(&url), &headers);
            handle_response!(match &body {
                Some(b) => req.send(b.as_bytes()),
                None => req.send_empty(),
            })
        }
        "PATCH" => {
            let req = apply_headers!(ureq::patch(&url), &headers);
            handle_response!(match &body {
                Some(b) => req.send(b.as_bytes()),
                None => req.send_empty(),
            })
        }
        _ => Rc::new(Object::Error(format!(
            "__http_request: unsupported method '{}'",
            method
        ))),
    }
}

// ── Streaming / socket networking ───────────────────────────────────────

fn net_str(arg: &Rc<Object>, name: &str) -> Result<String, Rc<Object>> {
    match arg.as_ref() {
        Object::String(s) => Ok(s.clone()),
        _ => Err(Rc::new(Object::Error(format!(
            "{}: expected STRING, got {}",
            name,
            arg.type_name()
        )))),
    }
}

fn net_int(arg: &Rc<Object>, name: &str) -> Result<i64, Rc<Object>> {
    match arg.as_ref() {
        Object::Integer(n) => Ok(*n),
        _ => Err(Rc::new(Object::Error(format!(
            "{}: expected INTEGER, got {}",
            name,
            arg.type_name()
        )))),
    }
}

fn net_headers(arg: Option<&Rc<Object>>) -> Vec<(String, String)> {
    match arg.map(|a| a.as_ref()) {
        Some(Object::Map(pairs)) => pairs
            .iter()
            .filter_map(|(k, v)| match (k.as_ref(), v.as_ref()) {
                (Object::String(key), Object::String(val)) => Some((key.clone(), val.clone())),
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

fn builtin_net_download(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error("download: expected (url, path)".to_string()));
    }
    let url = net_try!(net_str(&args[0], "download url"));
    let path = net_try!(net_str(&args[1], "download path"));
    match crate::netres::http_download(&url, &path) {
        Ok(status) => Rc::new(Object::Integer(status)),
        Err(e) => Rc::new(Object::Error(e)),
    }
}

fn builtin_net_upload(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() < 3 || args.len() > 4 {
        return Rc::new(Object::Error(
            "upload: expected (method, url, file_path, headers?)".to_string(),
        ));
    }
    let method = net_try!(net_str(&args[0], "upload method"));
    let url = net_try!(net_str(&args[1], "upload url"));
    let file_path = net_try!(net_str(&args[2], "upload file_path"));
    let headers = net_headers(args.get(3));
    match crate::netres::http_upload(&method, &url, &file_path, headers) {
        Ok((status, body)) => Rc::new(Object::Map(vec![
            (
                Rc::new(Object::String("status".to_string())),
                Rc::new(Object::Integer(status)),
            ),
            (
                Rc::new(Object::String("body".to_string())),
                Rc::new(Object::String(body)),
            ),
        ])),
        Err(e) => Rc::new(Object::Error(e)),
    }
}

fn builtin_net_http_open(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 4 {
        return Rc::new(Object::Error(
            "open_stream: expected (method, url, headers, body)".to_string(),
        ));
    }
    let method = net_try!(net_str(&args[0], "open_stream method"));
    let url = net_try!(net_str(&args[1], "open_stream url"));
    let headers = net_headers(Some(&args[2]));
    let body = net_try!(net_str(&args[3], "open_stream body"));
    let body = if body.is_empty() { None } else { Some(body.as_str()) };
    match crate::netres::http_open(&method, &url, &headers, body) {
        Ok(id) => Rc::new(Object::Integer(id as i64)),
        Err(e) => Rc::new(Object::Error(e)),
    }
}

fn builtin_net_http_read_line(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error(
            "read_line: expected (stream, timeout_ms)".to_string(),
        ));
    }
    let id = net_try!(net_int(&args[0], "read_line stream"));
    let timeout = net_try!(net_int(&args[1], "read_line timeout_ms"));
    match crate::netres::http_read_line(id as u64, timeout) {
        Ok(Some(s)) => Rc::new(Object::String(s)),
        Ok(None) => Rc::new(Object::None),
        Err(e) => Rc::new(Object::Error(e)),
    }
}

fn builtin_net_http_read(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 3 {
        return Rc::new(Object::Error(
            "read_chunk: expected (stream, max, timeout_ms)".to_string(),
        ));
    }
    let id = net_try!(net_int(&args[0], "read_chunk stream"));
    let max = net_try!(net_int(&args[1], "read_chunk max"));
    let timeout = net_try!(net_int(&args[2], "read_chunk timeout_ms"));
    match crate::netres::http_read(id as u64, max, timeout) {
        // None = timed out with no data yet; distinct from "" (EOF) and error.
        Ok(Some(s)) => Rc::new(Object::String(s)),
        Ok(None) => Rc::new(Object::None),
        Err(e) => Rc::new(Object::Error(e)),
    }
}

fn builtin_net_tcp_connect(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error("connect: expected (host, port)".to_string()));
    }
    let host = net_try!(net_str(&args[0], "connect host"));
    let port = net_try!(net_int(&args[1], "connect port"));
    match crate::netres::tcp_connect(&host, port) {
        Ok(id) => Rc::new(Object::Integer(id as i64)),
        Err(e) => Rc::new(Object::Error(e)),
    }
}

fn builtin_net_tcp_listen(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error("listen: expected (host, port)".to_string()));
    }
    let host = net_try!(net_str(&args[0], "listen host"));
    let port = net_try!(net_int(&args[1], "listen port"));
    match crate::netres::tcp_listen(&host, port) {
        Ok(id) => Rc::new(Object::Integer(id as i64)),
        Err(e) => Rc::new(Object::Error(e)),
    }
}

fn builtin_net_tcp_accept(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error("accept: expected (server)".to_string()));
    }
    let id = net_try!(net_int(&args[0], "accept server"));
    match crate::netres::tcp_accept(id as u64) {
        Ok(conn) => Rc::new(Object::Integer(conn as i64)),
        Err(e) => Rc::new(Object::Error(e)),
    }
}

fn builtin_net_send(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error("send: expected (conn, data)".to_string()));
    }
    let id = net_try!(net_int(&args[0], "send conn"));
    let data = net_try!(net_str(&args[1], "send data"));
    match crate::netres::tcp_send(id as u64, &data) {
        Ok(n) => Rc::new(Object::Integer(n)),
        Err(e) => Rc::new(Object::Error(e)),
    }
}

fn builtin_net_receive(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error("receive: expected (conn, max)".to_string()));
    }
    let id = net_try!(net_int(&args[0], "receive conn"));
    let max = net_try!(net_int(&args[1], "receive max"));
    match crate::netres::tcp_receive(id as u64, max) {
        Ok(s) => Rc::new(Object::String(s)),
        Err(e) => Rc::new(Object::Error(e)),
    }
}

fn builtin_net_close(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error("close: expected (handle)".to_string()));
    }
    let id = net_try!(net_int(&args[0], "close handle"));
    crate::netres::close(id as u64);
    Rc::new(Object::None)
}

fn builtin_net_udp_bind(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error("udp_bind: expected (host, port)".to_string()));
    }
    let host = net_try!(net_str(&args[0], "udp_bind host"));
    let port = net_try!(net_int(&args[1], "udp_bind port"));
    match crate::netres::udp_bind(&host, port) {
        Ok(id) => Rc::new(Object::Integer(id as i64)),
        Err(e) => Rc::new(Object::Error(e)),
    }
}

fn builtin_net_udp_send(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 4 {
        return Rc::new(Object::Error(
            "udp_send: expected (sock, data, host, port)".to_string(),
        ));
    }
    let id = net_try!(net_int(&args[0], "udp_send sock"));
    let data = net_try!(net_str(&args[1], "udp_send data"));
    let host = net_try!(net_str(&args[2], "udp_send host"));
    let port = net_try!(net_int(&args[3], "udp_send port"));
    match crate::netres::udp_send(id as u64, &data, &host, port) {
        Ok(n) => Rc::new(Object::Integer(n)),
        Err(e) => Rc::new(Object::Error(e)),
    }
}

fn builtin_net_udp_receive(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error("udp_receive: expected (sock, max)".to_string()));
    }
    let id = net_try!(net_int(&args[0], "udp_receive sock"));
    let max = net_try!(net_int(&args[1], "udp_receive max"));
    match crate::netres::udp_receive(id as u64, max) {
        Ok((data, addr)) => Rc::new(Object::Tuple(vec![
            Rc::new(Object::String(data)),
            Rc::new(Object::String(addr)),
        ])),
        Err(e) => Rc::new(Object::Error(e)),
    }
}
