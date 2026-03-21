use crate::object::Object;
use std::collections::HashMap;
use std::rc::Rc;

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
    builtins.insert("values".to_string(), Rc::new(Object::Builtin(builtin_values)));
    builtins.insert("insert".to_string(), Rc::new(Object::Builtin(builtin_insert)));
    builtins.insert("remove".to_string(), Rc::new(Object::Builtin(builtin_remove)));
    builtins.insert("has".to_string(), Rc::new(Object::Builtin(builtin_has)));
    builtins.insert("tuple".to_string(), Rc::new(Object::Builtin(builtin_tuple)));

    builtins
}

fn builtin_print(args: Vec<Rc<Object>>) -> Rc<Object> {
    let output: Vec<String> = args.iter().map(|a| a.to_string()).collect();
    println!("{}", output.join(" "));
    Rc::new(Object::None)
}

fn builtin_println(args: Vec<Rc<Object>>) -> Rc<Object> {
    let output: Vec<String> = args.iter().map(|a| a.to_string()).collect();
    println!("{}", output.join(" "));
    Rc::new(Object::None)
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

fn builtin_push(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments. got={}, want=2",
            args.len()
        )));
    }

    match args[0].as_ref() {
        Object::Array(arr) => {
            let mut new_arr = arr.clone();
            new_arr.push(Rc::clone(&args[1]));
            Rc::new(Object::Array(new_arr))
        }
        obj => Rc::new(Object::Error(format!(
            "argument to `push` must be ARRAY, got {}",
            obj.type_name()
        ))),
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
            if arr.is_empty() {
                Rc::new(Object::None)
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
        obj => Rc::new(Object::Error(format!(
            "cannot convert {} to STRING",
            obj.type_name()
        ))),
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
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "range function requires at 1 argument. got={}, want=1",
            args.len()
        )));
    }
    match args[0].as_ref() {
        Object::Integer(n) => {
            let n = *n;
            let mut num_range: Vec<Rc<Object>> = Vec::new();
            for i in 0..n {
                num_range.push(Rc::new(Object::Integer(i)));
            }
            Rc::new(Object::Array(num_range))
        }
        obj => Rc::new(Object::Error(format!(
            "argument require a integer, got {}",
            obj.type_name()
        ))),
    }
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
                Rc::new(Object::Error(format!("byte() argument out of range (0-255): {}", n)))
            } else {
                Rc::new(Object::Byte(*n as u8))
            }
        }
        Object::Uint(n) => {
            if *n > 255 {
                Rc::new(Object::Error(format!("byte() argument out of range (0-255): {}", n)))
            } else {
                Rc::new(Object::Byte(*n as u8))
            }
        }
        Object::Char(c) => Rc::new(Object::Byte(*c as u8)),
        obj => Rc::new(Object::Error(format!(
            "cannot convert {} to BYTE", obj.type_name()
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
                Rc::new(Object::Error(format!("uint() cannot convert negative: {}", n)))
            } else {
                Rc::new(Object::Uint(*n as u64))
            }
        }
        Object::Byte(n) => Rc::new(Object::Uint(*n as u64)),
        Object::Float(f) => {
            if *f < 0.0 {
                Rc::new(Object::Error(format!("uint() cannot convert negative: {}", f)))
            } else {
                Rc::new(Object::Uint(*f as u64))
            }
        }
        Object::String(s) => match s.parse::<u64>() {
            Ok(n) => Rc::new(Object::Uint(n)),
            Err(_) => Rc::new(Object::Error(format!("cannot convert \"{}\" to UINT", s))),
        },
        obj => Rc::new(Object::Error(format!(
            "cannot convert {} to UINT", obj.type_name()
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
            "argument to `keys` must be MAP, got {}", obj.type_name()
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
            "argument to `values` must be MAP, got {}", obj.type_name()
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
            "argument to `insert` must be MAP, got {}", obj.type_name()
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
            let new_entries: Vec<_> = entries.iter()
                .filter(|(k, _)| k != key)
                .cloned()
                .collect();
            Rc::new(Object::Map(new_entries))
        }
        Object::Set(elements) => {
            let val = &args[1];
            let new_elements: Vec<_> = elements.iter()
                .filter(|e| e != &val)
                .cloned()
                .collect();
            Rc::new(Object::Set(new_elements))
        }
        obj => Rc::new(Object::Error(format!(
            "argument to `remove` must be MAP or SET, got {}", obj.type_name()
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
            "argument to `has` must be a collection, got {}", obj.type_name()
        ))),
    }
}
