use crate::object::Object;
use std::collections::HashMap;
use std::rc::Rc;

pub fn get_builtins() -> HashMap<String, Rc<Object>> {
    let mut builtins: HashMap<String, Rc<Object>> = HashMap::new();

    builtins.insert("print".to_string(), Rc::new(Object::Builtin(builtin_print)));
    builtins.insert("println".to_string(), Rc::new(Object::Builtin(builtin_println)));
    builtins.insert("len".to_string(), Rc::new(Object::Builtin(builtin_len)));
    builtins.insert("push".to_string(), Rc::new(Object::Builtin(builtin_push)));
    builtins.insert("first".to_string(), Rc::new(Object::Builtin(builtin_first)));
    builtins.insert("last".to_string(), Rc::new(Object::Builtin(builtin_last)));
    builtins.insert("rest".to_string(), Rc::new(Object::Builtin(builtin_rest)));
    builtins.insert("type".to_string(), Rc::new(Object::Builtin(builtin_type)));
    builtins.insert("ord".to_string(), Rc::new(Object::Builtin(builtin_ord)));
    builtins.insert("chr".to_string(), Rc::new(Object::Builtin(builtin_chr)));
    builtins.insert("str".to_string(), Rc::new(Object::Builtin(builtin_str)));
    builtins.insert("chars".to_string(), Rc::new(Object::Builtin(builtin_chars)));

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
                return Rc::new(Object::Error(format!(
                    "chr() argument out of range: {}",
                    n
                )));
            }
            match char::from_u32(*n as u32) {
                Some(c) => Rc::new(Object::Char(c)),
                None => Rc::new(Object::Error(format!(
                    "invalid unicode codepoint: {}",
                    n
                ))),
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
        Object::Boolean(b) => Rc::new(Object::String(if *b { "True" } else { "False" }.to_string())),
        Object::String(s) => Rc::new(Object::String(s.clone())),
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
