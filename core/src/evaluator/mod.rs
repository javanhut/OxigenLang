pub mod builtins;

use crate::ast::{
    Expression, Identifier, ModulePath, Program, Statement, StringInterpPart, TypeAnnotation,
    TypedParam,
};
use crate::lexer::Lexer;
use crate::object::Object;
use crate::object::environment::{Environment, PatternRegistry};
use crate::parser::Parser;
use crate::token::Span;
use builtins::get_builtins;
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;

pub struct Evaluator {
    builtins: HashMap<String, Rc<Object>>,
    patterns: PatternRegistry,
    struct_defs: HashMap<String, Rc<Object>>,
    current_file: Option<PathBuf>,
    stdlib_path: PathBuf,
    module_cache: HashMap<PathBuf, Rc<Object>>,
    import_stack: Vec<PathBuf>,
    source: String,
    is_main_context: bool,
}

/// Unwraps nested `Grouped(...)` wrappers to get the inner expression.
fn unwrap_grouped(expr: &Expression) -> &Expression {
    match expr {
        Expression::Grouped(inner) => unwrap_grouped(inner),
        other => other,
    }
}

/// Checks if `actual_type` satisfies the type constraint `expected`.
/// Handles GENERIC (accepts anything) and union types like "INTEGER || STRING".
fn type_matches(expected: &str, actual: &str) -> bool {
    if expected == "GENERIC" {
        return true;
    }
    if expected == "ERROR" && actual.starts_with("ERROR") {
        return true;
    }
    if expected.contains(" || ") {
        expected
            .split(" || ")
            .any(|t| t == "GENERIC" || t == actual)
    } else {
        expected == actual
    }
}

/// Compute the Levenshtein edit distance between two strings.
fn levenshtein(a: &str, b: &str) -> usize {
    let a: Vec<char> = a.chars().collect();
    let b: Vec<char> = b.chars().collect();
    let (m, n) = (a.len(), b.len());
    let mut prev = (0..=n).collect::<Vec<_>>();
    let mut curr = vec![0; n + 1];
    for i in 1..=m {
        curr[0] = i;
        for j in 1..=n {
            let cost = if a[i - 1] == b[j - 1] { 0 } else { 1 };
            curr[j] = (prev[j] + 1).min(curr[j - 1] + 1).min(prev[j - 1] + cost);
        }
        std::mem::swap(&mut prev, &mut curr);
    }
    prev[n]
}

/// Find the closest match to `name` from `candidates` using edit distance.
/// Returns None if no candidate is close enough (threshold: max 2 edits, and less than half the name length).
fn suggest_similar<'a>(
    name: &str,
    candidates: impl IntoIterator<Item = &'a str>,
) -> Option<String> {
    let name_lower = name.to_lowercase();
    let max_dist = 2.max(name.len() / 3);
    let mut best: Option<(usize, String)> = None;
    for candidate in candidates {
        if candidate == name {
            continue;
        }
        let dist = levenshtein(&name_lower, &candidate.to_lowercase());
        if dist <= max_dist {
            if best.is_none() || dist < best.as_ref().unwrap().0 {
                best = Some((dist, candidate.to_string()));
            }
        }
    }
    best.map(|(_, s)| s)
}

/// Locate the stdlib directory using the same search strategy as the evaluator.
pub fn find_stdlib_path() -> PathBuf {
    // 1. Relative to executable (dev: target/debug/oxigen + stdlib/)
    if let Ok(exe) = std::env::current_exe() {
        if let Some(exe_dir) = exe.parent() {
            let candidate = exe_dir.join("stdlib");
            if candidate.is_dir() {
                return candidate;
            }
            // 2. System install: <prefix>/bin/../lib/oxigen/stdlib
            if let Some(prefix) = exe_dir.parent() {
                let candidate = prefix.join("lib").join("oxigen").join("stdlib");
                if candidate.is_dir() {
                    return candidate;
                }
            }
        }
    }
    // 3. Current working directory (preferred for local development)
    let candidate = PathBuf::from("stdlib");
    if candidate.is_dir() {
        return candidate;
    }
    // 4. Parent of current working directory (useful when tests run from a crate dir)
    let candidate = PathBuf::from("..").join("stdlib");
    if candidate.is_dir() {
        return candidate;
    }
    // 5. User install: ~/.oxigen/lib/stdlib
    if let Ok(home) = std::env::var("HOME") {
        let candidate = PathBuf::from(home)
            .join(".oxigen")
            .join("lib")
            .join("stdlib");
        if candidate.is_dir() {
            return candidate;
        }
    }
    // Fallback
    PathBuf::from("stdlib")
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}

impl Evaluator {
    pub fn new() -> Self {
        let mut builtins = get_builtins();
        builtins.insert("__args".to_string(), Rc::new(Object::Array(Vec::new())));

        Evaluator {
            builtins,
            patterns: PatternRegistry::new(),
            struct_defs: HashMap::new(),
            current_file: None,
            stdlib_path: Self::find_stdlib_path(),
            module_cache: HashMap::new(),
            import_stack: Vec::new(),
            source: String::new(),
            is_main_context: true,
        }
    }

    pub fn new_with_path(file_path: PathBuf) -> Self {
        let mut builtins = get_builtins();
        builtins.insert("__args".to_string(), Rc::new(Object::Array(Vec::new())));

        Evaluator {
            builtins,
            patterns: PatternRegistry::new(),
            struct_defs: HashMap::new(),
            current_file: Some(file_path),
            stdlib_path: Self::find_stdlib_path(),
            module_cache: HashMap::new(),
            import_stack: Vec::new(),
            source: String::new(),
            is_main_context: true,
        }
    }

    pub fn set_source(&mut self, source: &str) {
        self.source = source.to_string();
    }

    pub fn set_script_args(&mut self, script_args: &[String]) {
        let args = script_args
            .iter()
            .map(|arg| Rc::new(Object::String(arg.clone())))
            .collect();
        self.builtins
            .insert("__args".to_string(), Rc::new(Object::Array(args)));
    }

    /// Format a runtime error with source context, matching the parser error style.
    fn format_error(&self, span: Span, message: &str, hint: Option<&str>) -> String {
        let mut out = String::new();
        let line_num = span.line;
        let col = span.column;

        out.push_str(&format!("error: {}\n", message));
        out.push_str(&format!("  --> line {}:{}\n", line_num, col));

        if let Some(source_line) = self.source.lines().nth(line_num.saturating_sub(1)) {
            let line_str = format!("{}", line_num);
            let padding = " ".repeat(line_str.len());
            out.push_str(&format!("{} |\n", padding));
            out.push_str(&format!("{} | {}\n", line_str, source_line));
            if col > 0 {
                let caret_padding = " ".repeat(col.saturating_sub(1));
                out.push_str(&format!("{} | {}^", padding, caret_padding));
            }
        }

        if let Some(hint) = hint {
            out.push_str(&format!("\n  = hint: {}", hint));
        }

        out
    }

    /// Create an error object with source context embedded in the message.
    fn runtime_error(&self, span: Span, message: &str, hint: Option<&str>) -> Rc<Object> {
        if self.source.is_empty() || span.line == 0 {
            // No source available — fall back to plain error
            let msg = match hint {
                Some(h) => format!("{}\n  = hint: {}", message, h),
                None => message.to_string(),
            };
            return Rc::new(Object::Error(msg));
        }
        Rc::new(Object::Error(self.format_error(span, message, hint)))
    }

    fn find_stdlib_path() -> PathBuf {
        find_stdlib_path()
    }

    fn encode_error_message(msg: &str, tag: Option<&str>) -> String {
        match tag {
            Some(tag) => format!("[{}] {}", tag, msg),
            None => msg.to_string(),
        }
    }

    fn decode_error_message(msg: &str) -> (Option<String>, String) {
        if let Some(rest) = msg.strip_prefix('[') {
            if let Some((tag, tail)) = rest.split_once("] ") {
                return (Some(tag.to_string()), tail.to_string());
            }
        }
        (None, msg.to_string())
    }

    fn error_object(msg: impl Into<String>) -> Rc<Object> {
        let msg = msg.into();
        Rc::new(Object::Error(msg))
    }

    fn error_object_with_tag(msg: impl Into<String>, tag: Option<String>) -> Rc<Object> {
        let msg = msg.into();
        Rc::new(Object::Error(Self::encode_error_message(
            &msg,
            tag.as_deref(),
        )))
    }

    fn tagged_error_value(msg: impl Into<String>, tag: Option<String>) -> Rc<Object> {
        Rc::new(Object::ErrorValue {
            msg: msg.into(),
            tag,
        })
    }

    fn epoch_days_to_date(days: u64) -> (u64, u64, u64) {
        // Civil calendar from days since Unix epoch
        let z = days + 719468;
        let era = z / 146097;
        let doe = z - era * 146097;
        let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
        let y = yoe + era * 400;
        let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
        let mp = (5 * doy + 2) / 153;
        let d = doy - (153 * mp + 2) / 5 + 1;
        let m = if mp < 10 { mp + 3 } else { mp - 9 };
        let year = if m <= 2 { y + 1 } else { y };
        (year, m, d)
    }

    fn error_info_from(obj: &Rc<Object>) -> Option<(Option<String>, String)> {
        match obj.as_ref() {
            Object::Error(msg) => Some(Self::decode_error_message(msg)),
            Object::ErrorValue { msg, tag } => Some((tag.clone(), msg.clone())),
            _ => None,
        }
    }

    fn format_log_tag(tag: Option<&str>, sub_tag: Option<&str>) -> String {
        match (tag, sub_tag) {
            (Some(t), Some(s)) => format!("[{}:{}]", t.to_uppercase(), s.to_uppercase()),
            (Some(t), None) => format!("[{}]", t.to_uppercase()),
            _ => String::new(),
        }
    }

    fn resolve_implicit_log_error(
        tag: Option<&str>,
        sub_tag: Option<&str>,
        env: &Rc<RefCell<Environment>>,
    ) -> Option<(String, String)> {
        if tag == Some("Error") && sub_tag.is_none() {
            return Some(("ERROR".to_string(), "generic".to_string()));
        }

        let binding_name = match (tag, sub_tag) {
            (Some("Error"), Some(sub)) => Some(sub),
            (Some(name), None) => Some(name),
            _ => None,
        }?;

        let bound = env.borrow().get(binding_name)?;
        let (bound_tag, msg) = Self::error_info_from(&bound)?;
        let display_tag = bound_tag.unwrap_or_else(|| binding_name.to_string());
        Some((display_tag.to_uppercase(), msg))
    }

    fn resolve_log_parts(
        tag: Option<&str>,
        sub_tag: Option<&str>,
        explicit_msg: Option<String>,
        env: &Rc<RefCell<Environment>>,
    ) -> (String, Option<String>) {
        if let Some(msg) = explicit_msg {
            return (Self::format_log_tag(tag, sub_tag), Some(msg));
        }

        if let Some((resolved_tag, msg)) = Self::resolve_implicit_log_error(tag, sub_tag, env) {
            return (format!("[{}]", resolved_tag), Some(msg));
        }

        (Self::format_log_tag(tag, sub_tag), None)
    }

    fn error_or_value_union(target: &TypeAnnotation) -> Option<Option<String>> {
        if let TypeAnnotation::Union(types) = target {
            let mut has_value = false;
            let mut error_tag: Option<Option<String>> = None;
            for t in types {
                match t {
                    TypeAnnotation::ValueType => has_value = true,
                    TypeAnnotation::ErrorType(tag) => error_tag = Some(tag.clone()),
                    _ => return None,
                }
            }
            if has_value && error_tag.is_some() {
                return error_tag;
            }
        }
        None
    }

    fn convert_to_type(obj: &Rc<Object>, target: &str) -> Result<Rc<Object>, String> {
        match target {
            "GENERIC" => Ok(Rc::clone(obj)),
            "NONE" => match obj.as_ref() {
                Object::None => Ok(Rc::clone(obj)),
                _ => Err(format!("cannot convert {} to NONE", obj.type_name())),
            },
            "VALUE" => Ok(Rc::new(Object::Value(Rc::clone(obj)))),
            "ERROR" => match Self::error_info_from(obj) {
                Some((tag, msg)) => Ok(Self::tagged_error_value(msg, tag)),
                None => Err(format!("cannot convert {} to ERROR", obj.type_name())),
            },
            t if t.starts_with("ERROR<") && t.ends_with('>') => {
                let wanted_tag = &t[6..t.len() - 1];
                match Self::error_info_from(obj) {
                    Some((Some(tag), msg)) if tag == wanted_tag => {
                        Ok(Self::tagged_error_value(msg, Some(tag)))
                    }
                    Some((None, _)) => Err(format!("cannot convert untagged ERROR to {}", t)),
                    Some((Some(actual), _)) => {
                        Err(format!("cannot convert ERROR<{}> to {}", actual, t))
                    }
                    None => Err(format!("cannot convert {} to {}", obj.type_name(), t)),
                }
            }
            t if t.contains(" || ") => {
                // Union type: accept value if it already matches any member type
                let actual = obj.effective_type_name();
                if t.split(" || ").any(|member| member == actual) {
                    return Ok(Rc::clone(obj));
                }
                // Try converting to each member type in order
                for member in t.split(" || ") {
                    if let Ok(converted) = Self::convert_to_type(obj, member) {
                        return Ok(converted);
                    }
                }
                Err(format!("cannot convert {} to {}", actual, t))
            }
            "INTEGER" => match obj.as_ref() {
                Object::Integer(_) => Ok(Rc::clone(obj)),
                Object::Float(f) => Ok(Rc::new(Object::Integer(*f as i64))),
                Object::String(s) => s
                    .parse::<i64>()
                    .map(|n| Rc::new(Object::Integer(n)))
                    .map_err(|_| format!("cannot convert STRING \"{}\" to INTEGER", s)),
                Object::Char(c) => Ok(Rc::new(Object::Integer(*c as i64))),
                Object::Boolean(b) => Ok(Rc::new(Object::Integer(if *b { 1 } else { 0 }))),
                Object::Byte(n) => Ok(Rc::new(Object::Integer(*n as i64))),
                Object::Uint(n) => Ok(Rc::new(Object::Integer(*n as i64))),
                _ => Err(format!("cannot convert {} to INTEGER", obj.type_name())),
            },
            "FLOAT" => match obj.as_ref() {
                Object::Float(_) => Ok(Rc::clone(obj)),
                Object::Integer(n) => Ok(Rc::new(Object::Float(*n as f64))),
                Object::String(s) => s
                    .parse::<f64>()
                    .map(|f| Rc::new(Object::Float(f)))
                    .map_err(|_| format!("cannot convert STRING \"{}\" to FLOAT", s)),
                Object::Boolean(b) => Ok(Rc::new(Object::Float(if *b { 1.0 } else { 0.0 }))),
                Object::Byte(n) => Ok(Rc::new(Object::Float(*n as f64))),
                Object::Uint(n) => Ok(Rc::new(Object::Float(*n as f64))),
                _ => Err(format!("cannot convert {} to FLOAT", obj.type_name())),
            },
            "STRING" => match obj.as_ref() {
                Object::String(_) => Ok(Rc::clone(obj)),
                Object::Integer(n) => Ok(Rc::new(Object::String(n.to_string()))),
                Object::Float(f) => Ok(Rc::new(Object::String(f.to_string()))),
                Object::Char(c) => Ok(Rc::new(Object::String(c.to_string()))),
                Object::Boolean(b) => Ok(Rc::new(Object::String(if *b {
                    "True".to_string()
                } else {
                    "False".to_string()
                }))),
                _ => Err(format!("cannot convert {} to STRING", obj.type_name())),
            },
            "CHAR" => match obj.as_ref() {
                Object::Char(_) => Ok(Rc::clone(obj)),
                Object::Integer(n) => {
                    if let Some(c) = char::from_u32(*n as u32) {
                        Ok(Rc::new(Object::Char(c)))
                    } else {
                        Err(format!("cannot convert INTEGER {} to CHAR", n))
                    }
                }
                Object::String(s) => {
                    let mut chars = s.chars();
                    if let Some(c) = chars.next() {
                        if chars.next().is_none() {
                            Ok(Rc::new(Object::Char(c)))
                        } else {
                            Err(format!("cannot convert multi-char STRING to CHAR"))
                        }
                    } else {
                        Err("cannot convert empty STRING to CHAR".to_string())
                    }
                }
                _ => Err(format!("cannot convert {} to CHAR", obj.type_name())),
            },
            "BOOLEAN" => match obj.as_ref() {
                Object::Boolean(_) => Ok(Rc::clone(obj)),
                _ => Ok(Rc::new(Object::Boolean(obj.is_truthy()))),
            },
            "ARRAY" => match obj.as_ref() {
                Object::Array(_) => Ok(Rc::clone(obj)),
                Object::String(s) => {
                    let elements: Vec<Rc<Object>> = s
                        .chars()
                        .map(|c| Rc::new(Object::String(c.to_string())))
                        .collect();
                    Ok(Rc::new(Object::Array(elements)))
                }
                _ => Err(format!("cannot convert {} to ARRAY", obj.type_name())),
            },
            "BYTE" => match obj.as_ref() {
                Object::Byte(_) => Ok(Rc::clone(obj)),
                Object::Integer(n) => {
                    if *n < 0 || *n > 255 {
                        Err(format!("cannot convert INTEGER {} to BYTE (0-255)", n))
                    } else {
                        Ok(Rc::new(Object::Byte(*n as u8)))
                    }
                }
                Object::Uint(n) => {
                    if *n > 255 {
                        Err(format!("cannot convert UINT {} to BYTE (0-255)", n))
                    } else {
                        Ok(Rc::new(Object::Byte(*n as u8)))
                    }
                }
                Object::Char(c) => Ok(Rc::new(Object::Byte(*c as u8))),
                Object::Boolean(b) => Ok(Rc::new(Object::Byte(if *b { 1 } else { 0 }))),
                _ => Err(format!("cannot convert {} to BYTE", obj.type_name())),
            },
            "UINT" => match obj.as_ref() {
                Object::Uint(_) => Ok(Rc::clone(obj)),
                Object::Integer(n) => {
                    if *n < 0 {
                        Err(format!("cannot convert negative INTEGER {} to UINT", n))
                    } else {
                        Ok(Rc::new(Object::Uint(*n as u64)))
                    }
                }
                Object::Float(f) => {
                    if *f < 0.0 {
                        Err(format!("cannot convert negative FLOAT {} to UINT", f))
                    } else {
                        Ok(Rc::new(Object::Uint(*f as u64)))
                    }
                }
                Object::Byte(n) => Ok(Rc::new(Object::Uint(*n as u64))),
                Object::Boolean(b) => Ok(Rc::new(Object::Uint(if *b { 1 } else { 0 }))),
                Object::String(s) => s
                    .parse::<u64>()
                    .map(|n| Rc::new(Object::Uint(n)))
                    .map_err(|_| format!("cannot convert STRING \"{}\" to UINT", s)),
                _ => Err(format!("cannot convert {} to UINT", obj.type_name())),
            },
            "TUPLE" => match obj.as_ref() {
                Object::Tuple(_) => Ok(Rc::clone(obj)),
                _ => Err(format!("cannot convert {} to TUPLE", obj.type_name())),
            },
            "MAP" => match obj.as_ref() {
                Object::Map(_) => Ok(Rc::clone(obj)),
                _ => Err(format!("cannot convert {} to MAP", obj.type_name())),
            },
            "SET" => match obj.as_ref() {
                Object::Set(_) => Ok(Rc::clone(obj)),
                _ => Err(format!("cannot convert {} to SET", obj.type_name())),
            },
            _ => {
                // Struct type: only identity conversion (value must already be that struct)
                if let Some(stn) = obj.struct_type_name() {
                    if stn == target {
                        return Ok(Rc::clone(obj));
                    }
                }
                Err(format!(
                    "cannot convert {} to {}",
                    obj.effective_type_name(),
                    target
                ))
            }
        }
    }

    fn zero_value_for_type(type_name: &str) -> Rc<Object> {
        match type_name {
            "INTEGER" => Rc::new(Object::Integer(0)),
            "FLOAT" => Rc::new(Object::Float(0.0)),
            "STRING" => Rc::new(Object::String(String::new())),
            "CHAR" => Rc::new(Object::Char('\0')),
            "BOOLEAN" => Rc::new(Object::Boolean(false)),
            "ARRAY" => Rc::new(Object::Array(Vec::new())),
            "BYTE" => Rc::new(Object::Byte(0)),
            "UINT" => Rc::new(Object::Uint(0)),
            "TUPLE" => Rc::new(Object::Tuple(Vec::new())),
            "MAP" => Rc::new(Object::Map(Vec::new())),
            "SET" => Rc::new(Object::Set(Vec::new())),
            "GENERIC" => Rc::new(Object::None),
            "NONE" => Rc::new(Object::None),
            t if t.contains(" || ") => {
                // Union type: use the first member's zero value
                let first = t.split(" || ").next().unwrap();
                Self::zero_value_for_type(first)
            }
            _ => Rc::new(Object::None),
        }
    }

    fn zero_value_for_struct(&self, struct_name: &str) -> Result<Rc<Object>, String> {
        let def = self
            .struct_defs
            .get(struct_name)
            .ok_or_else(|| format!("struct not found: {}", struct_name))?;
        if let Object::StructDef { name, fields, .. } = def.as_ref() {
            let mut instance_fields = HashMap::new();
            for (field_name, field_type, _) in fields {
                instance_fields.insert(field_name.clone(), Self::zero_value_for_type(field_type));
            }
            Ok(Rc::new(Object::StructInstance {
                struct_name: name.clone(),
                fields: Rc::new(RefCell::new(instance_fields)),
            }))
        } else {
            Err(format!("{} is not a struct", struct_name))
        }
    }

    pub fn eval_program(&mut self, program: &Program, env: Rc<RefCell<Environment>>) -> Rc<Object> {
        let mut result = Rc::new(Object::None);

        for stmt in &program.statements {
            result = self.eval_statement(stmt, Rc::clone(&env));

            // Handle return values and errors
            match result.as_ref() {
                Object::Return(val) => return Rc::clone(val),
                Object::Error(_) => return result,
                _ => {}
            }
        }

        result
    }

    pub fn eval_statement(
        &mut self,
        stmt: &Statement,
        env: Rc<RefCell<Environment>>,
    ) -> Rc<Object> {
        match stmt {
            Statement::Let { name, value } => {
                let val = self.eval_expression(value, Rc::clone(&env));
                if val.is_error() {
                    return val;
                }
                // If variable exists in any scope, update it; otherwise create new
                let existing = env.borrow().get(&name.value);
                if existing.is_some() {
                    // Check if the variable has a type constraint
                    let tc = env.borrow().get_type_constraint(&name.value);
                    if let Some(target_type) = tc {
                        // Walrus reassignment on typed variable: strict type check
                        // Type is immutable — no implicit conversion. Use explicit
                        // typed re-declaration (x <type> := val) to change the type.
                        if !type_matches(&target_type, &val.effective_type_name()) {
                            return self.runtime_error(
                                name.token.span,
                                &format!(
                                    "type mismatch: '{}' is locked to {}, got {}",
                                    name.value,
                                    target_type,
                                    val.effective_type_name()
                                ),
                                Some("use explicit type declaration to change type"),
                            );
                        }
                        env.borrow_mut().update(&name.value, val.clone());
                        return val;
                    }
                    env.borrow_mut().update(&name.value, val.clone());
                } else {
                    env.borrow_mut().set(name.value.clone(), val.clone());
                }
                val
            }
            Statement::Expr(expr) => self.eval_expression(expr, env),
            Statement::Each {
                token: each_token,
                variable,
                iterable,
                body,
                ..
            } => self.eval_each(variable, iterable, body, env, each_token.span),
            Statement::Repeat {
                condition, body, ..
            } => self.eval_repeat(condition, body, env),
            Statement::Pattern {
                name,
                params,
                condition,
                ..
            } => {
                let param_names: Vec<String> = params.iter().map(|p| p.value.clone()).collect();
                self.patterns
                    .register(name.value.clone(), param_names, condition.clone());
                Rc::new(Object::None)
            }
            Statement::Choose {
                token: choose_token,
                subject,
                arms,
                ..
            } => self.eval_choose(subject, arms, env, choose_token.span),
            Statement::If {
                condition,
                consequence,
                alternative,
                ..
            } => self.eval_if(condition, consequence, alternative, env),
            Statement::TypedLet {
                name,
                type_ann,
                value,
                walrus,
            } => {
                let val = self.eval_expression(value, Rc::clone(&env));
                if val.is_error() {
                    return val;
                }
                let target = type_ann.type_name();
                let final_val = if *walrus {
                    match Self::convert_to_type(&val, &target) {
                        Ok(converted) => converted,
                        Err(msg) => return Rc::new(Object::Error(msg)),
                    }
                } else {
                    if !type_matches(&target, &val.effective_type_name()) {
                        return self.runtime_error(
                            name.token.span,
                            &format!(
                                "type mismatch: expected {}, got {}",
                                target,
                                val.effective_type_name()
                            ),
                            Some(&format!(
                                "'{}' is declared as <{}>",
                                name.value,
                                target.to_lowercase()
                            )),
                        );
                    }
                    val
                };
                let immutable = !*walrus;
                if *type_ann == TypeAnnotation::NoneType {
                    // <None> is type-mutable: no type constraint set
                    env.borrow_mut().set(name.value.clone(), final_val.clone());
                } else {
                    env.borrow_mut().set_typed(
                        name.value.clone(),
                        final_val.clone(),
                        target,
                        immutable,
                    );
                }
                final_val
            }
            Statement::TypedDeclare { name, type_ann } => {
                let target = type_ann.type_name();
                let val = match type_ann {
                    TypeAnnotation::Struct(sname) => match self.zero_value_for_struct(sname) {
                        Ok(v) => v,
                        Err(msg) => return Rc::new(Object::Error(msg)),
                    },
                    _ => Self::zero_value_for_type(&target),
                };
                if *type_ann == TypeAnnotation::NoneType {
                    env.borrow_mut().set(name.value.clone(), val.clone());
                } else {
                    env.borrow_mut().set_typed(
                        name.value.clone(),
                        val.clone(),
                        target,
                        false, // as-declarations are mutable
                    );
                }
                val
            }
            Statement::Assign { name, value } => {
                let existing = env.borrow().get(&name.value);
                if existing.is_none() {
                    let all_names = env.borrow().all_keys();
                    let mut hint_parts = Vec::new();
                    if let Some(suggestion) =
                        suggest_similar(&name.value, all_names.iter().map(|s| s.as_str()))
                    {
                        hint_parts.push(format!("did you mean `{}`?", suggestion));
                    }
                    hint_parts.push("use `:=` to declare new variables".to_string());
                    return self.runtime_error(
                        name.token.span,
                        &format!("identifier not found: {}", name.value),
                        Some(&hint_parts.join(". ")),
                    );
                }
                let tc = env.borrow().get_type_constraint(&name.value);
                if tc.is_none() {
                    return self.runtime_error(
                        name.token.span,
                        &format!("`=` requires typed variable, use `:=` for '{}'", name.value),
                        Some("declare with a type first: x <type> := value"),
                    );
                }
                // Immutable check: = cannot reassign an immutable binding
                if env.borrow().is_immutable(&name.value) {
                    return self.runtime_error(
                        name.token.span,
                        &format!("cannot reassign immutable variable '{}'", name.value),
                        Some("use `:=` to override an immutable binding"),
                    );
                }
                let target_type = tc.unwrap();
                let val = self.eval_expression(value, Rc::clone(&env));
                if val.is_error() {
                    return val;
                }
                if !type_matches(&target_type, &val.effective_type_name()) {
                    return self.runtime_error(
                        name.token.span,
                        &format!(
                            "type mismatch: expected {}, got {}",
                            target_type,
                            val.effective_type_name()
                        ),
                        Some(&format!(
                            "'{}' is locked to type {}",
                            name.value, target_type
                        )),
                    );
                }
                env.borrow_mut().update(&name.value, val.clone());
                val
            }
            Statement::Skip => Rc::new(Object::Skip),
            Statement::Stop => Rc::new(Object::Stop),
            Statement::Give { value, .. } => {
                let val = self.eval_expression(value, Rc::clone(&env));
                if val.is_error() {
                    return val;
                }
                Rc::new(Object::Return(val))
            }
            Statement::StructDef {
                token,
                name,
                parent,
                fields,
            } => {
                // If parent specified, get parent's fields
                let mut all_fields: Vec<(String, String, bool)> = Vec::new();
                let parent_name = if let Some(parent_ident) = parent {
                    let parent_def = self.struct_defs.get(&parent_ident.value);
                    match parent_def {
                        Some(def) => {
                            if let Object::StructDef {
                                fields: parent_fields,
                                ..
                            } = def.as_ref()
                            {
                                all_fields.extend(parent_fields.clone());
                            }
                        }
                        None => {
                            return self.runtime_error(
                                token.span,
                                &format!("parent struct not found: {}", parent_ident.value),
                                Some("make sure the parent struct is defined before this struct"),
                            );
                        }
                    }
                    Some(parent_ident.value.clone())
                } else {
                    None
                };

                // Add own fields
                for field in fields {
                    all_fields.push((
                        field.name.value.clone(),
                        field.type_ann.type_name(),
                        field.hidden,
                    ));
                }

                let struct_obj = Rc::new(Object::StructDef {
                    name: name.value.clone(),
                    fields: all_fields,
                    methods: HashMap::new(),
                    parent: parent_name,
                });

                self.struct_defs
                    .insert(name.value.clone(), Rc::clone(&struct_obj));
                env.borrow_mut()
                    .set(name.value.clone(), Rc::clone(&struct_obj));
                struct_obj
            }
            Statement::ContainsDef {
                token,
                struct_name,
                methods,
            } => {
                let existing = self.struct_defs.get(&struct_name.value).cloned();
                match existing {
                    Some(def) => {
                        if let Object::StructDef {
                            name,
                            fields,
                            methods: existing_methods,
                            parent,
                        } = def.as_ref()
                        {
                            let mut new_methods = existing_methods.clone();
                            let sname = name.clone();
                            let sfields = fields.clone();
                            let sparent = parent.clone();
                            for (method_name, func_expr) in methods {
                                let func_obj = self.eval_expression(func_expr, Rc::clone(&env));
                                if func_obj.is_error() {
                                    return func_obj;
                                }
                                new_methods.insert(method_name.value.clone(), func_obj);
                            }
                            let updated = Rc::new(Object::StructDef {
                                name: sname,
                                fields: sfields,
                                methods: new_methods,
                                parent: sparent,
                            });
                            self.struct_defs
                                .insert(struct_name.value.clone(), Rc::clone(&updated));
                            env.borrow_mut()
                                .set(struct_name.value.clone(), Rc::clone(&updated));
                            updated
                        } else {
                            self.runtime_error(
                                token.span,
                                &format!("{} is not a struct", struct_name.value),
                                Some("'contains' can only add methods to a struct definition"),
                            )
                        }
                    }
                    None => self.runtime_error(
                        token.span,
                        &format!("struct not found: {}", struct_name.value),
                        Some("make sure the struct is defined before using 'contains'"),
                    ),
                }
            }
            Statement::DotAssign {
                token,
                object,
                field,
                value,
            } => {
                let is_self_access = matches!(object, Expression::Ident(id) if id.value == "self");
                let obj = self.eval_expression(object, Rc::clone(&env));
                if obj.is_error() {
                    return obj;
                }
                match obj.as_ref() {
                    Object::StructInstance {
                        struct_name,
                        fields,
                    } => {
                        // Look up struct def and find the expected type for this field
                        let (expected_type, hidden) = {
                            let def = self.struct_defs.get(struct_name);
                            if let Some(def) = def {
                                if let Object::StructDef {
                                    fields: def_fields, ..
                                } = def.as_ref()
                                {
                                    let field_def =
                                        def_fields.iter().find(|(n, _, _)| n == &field.value);
                                    match field_def {
                                        Some((_, et, h)) => (et.clone(), *h),
                                        None => {
                                            let field_names: Vec<&str> = def_fields
                                                .iter()
                                                .map(|(n, _, _)| n.as_str())
                                                .collect();
                                            let hint = suggest_similar(
                                                &field.value,
                                                field_names.into_iter(),
                                            )
                                            .map(|s| format!("did you mean `{}`?", s));
                                            return self.runtime_error(
                                                field.token.span,
                                                &format!(
                                                    "struct {} has no field '{}'",
                                                    struct_name, field.value
                                                ),
                                                hint.as_deref(),
                                            );
                                        }
                                    }
                                } else {
                                    return self.runtime_error(
                                        token.span,
                                        &format!("{} is not a struct", struct_name),
                                        Some("dot assignment is only valid on struct instances"),
                                    );
                                }
                            } else {
                                return self.runtime_error(
                                    token.span,
                                    &format!("struct definition not found: {}", struct_name),
                                    Some("the struct type may not have been defined in this scope"),
                                );
                            }
                        };
                        if hidden && !is_self_access {
                            return self.runtime_error(
                                field.token.span,
                                &format!(
                                    "field '{}' is hidden on struct {}",
                                    field.value, struct_name
                                ),
                                Some(
                                    "hidden fields can only be accessed via `self` inside a method",
                                ),
                            );
                        }
                        let val = self.eval_expression(value, Rc::clone(&env));
                        if val.is_error() {
                            return val;
                        }
                        let actual_type = val.effective_type_name();
                        if !type_matches(&expected_type, &actual_type) {
                            return self.runtime_error(
                                token.span,
                                &format!(
                                    "type mismatch: field '{}' expects {}, got {}",
                                    field.value, expected_type, actual_type
                                ),
                                Some("ensure the assigned value matches the field's declared type"),
                            );
                        }
                        fields.borrow_mut().insert(field.value.clone(), val.clone());
                        if is_self_access {
                            env.borrow_mut().set(field.value.clone(), val.clone());
                        }
                        val
                    }
                    _ => self.runtime_error(
                        token.span,
                        &format!("cannot assign field on non-struct: {}", obj.type_name()),
                        Some("dot assignment is only supported on struct instances"),
                    ),
                }
            }

            Statement::IndexAssign {
                token,
                object,
                index,
                value,
            } => {
                let idx = self.eval_expression(index, Rc::clone(&env));
                if idx.is_error() {
                    return idx;
                }
                let val = self.eval_expression(value, Rc::clone(&env));
                if val.is_error() {
                    return val;
                }
                let obj = self.eval_expression(object, Rc::clone(&env));
                if obj.is_error() {
                    return obj;
                }

                // Determine the variable name so we can update it in the env
                let var_name = match object {
                    Expression::Ident(ident) => ident.value.clone(),
                    _ => {
                        return self.runtime_error(
                            token.span,
                            "index assignment requires a variable on the left side",
                            Some("use like: m[key] = value"),
                        );
                    }
                };

                let new_obj = match obj.as_ref() {
                    Object::Map(entries) => {
                        let mut new_entries = entries.clone();
                        if let Some(entry) = new_entries.iter_mut().find(|(k, _)| *k == idx) {
                            entry.1 = val.clone();
                        } else {
                            new_entries.push((idx, val.clone()));
                        }
                        Rc::new(Object::Map(new_entries))
                    }
                    Object::Array(elements) => match idx.as_ref() {
                        Object::Integer(i) => {
                            let index = if *i < 0 {
                                (elements.len() as i64 + *i) as usize
                            } else {
                                *i as usize
                            };
                            if index >= elements.len() {
                                return self.runtime_error(
                                    token.span,
                                    &format!(
                                        "index out of bounds: {} (length {})",
                                        i,
                                        elements.len()
                                    ),
                                    None,
                                );
                            }
                            let mut new_elements = elements.clone();
                            new_elements[index] = val.clone();
                            Rc::new(Object::Array(new_elements))
                        }
                        _ => {
                            return self.runtime_error(
                                token.span,
                                &format!("array index must be INTEGER, got {}", idx.type_name()),
                                None,
                            );
                        }
                    },
                    _ => {
                        return self.runtime_error(
                            token.span,
                            &format!("cannot index-assign on {}", obj.type_name()),
                            Some("index assignment works on maps and arrays"),
                        );
                    }
                };

                // Update the variable in the environment
                if env.borrow_mut().update(&var_name, new_obj).is_none() {
                    env.borrow_mut().set(var_name, Rc::new(Object::None));
                }
                val
            }

            Statement::Main { body, .. } => {
                if self.is_main_context {
                    self.eval_block(body, env)
                } else {
                    Rc::new(Object::None)
                }
            }
            Statement::Introduce {
                token,
                path,
                selective,
            } => self.eval_introduce(token.span, path, selective, Rc::clone(&env)),
            Statement::Unpack { names, value } => {
                let val = self.eval_expression(value, Rc::clone(&env));
                if val.is_error() {
                    return val;
                }
                let elements = match val.as_ref() {
                    Object::Array(arr) => arr.clone(),
                    Object::Tuple(t) => t.clone(),
                    _ => {
                        let span = names.first().map(|n| n.token.span).unwrap_or_default();
                        return self.runtime_error(
                            span,
                            &format!("cannot unpack {}, expected array or tuple", val.type_name()),
                            None,
                        );
                    }
                };
                if elements.len() != names.len() {
                    let span = names.first().map(|n| n.token.span).unwrap_or_default();
                    return self.runtime_error(
                        span,
                        &format!(
                            "unpack length mismatch: expected {} values, got {}",
                            names.len(),
                            elements.len()
                        ),
                        Some(&format!(
                            "the right side has {} elements but the left side has {} names",
                            elements.len(),
                            names.len()
                        )),
                    );
                }
                for (name, element) in names.iter().zip(elements.iter()) {
                    env.borrow_mut().set(name.value.clone(), Rc::clone(element));
                }
                Rc::new(Object::None)
            }
        }
    }

    // ── Module / Import system ──

    fn eval_introduce(
        &mut self,
        span: Span,
        path: &ModulePath,
        selective: &Option<Vec<Identifier>>,
        env: Rc<RefCell<Environment>>,
    ) -> Rc<Object> {
        // 1. Resolve module path
        let resolved = match self.resolve_module_path(path) {
            Ok(p) => p,
            Err(msg) => {
                return self.runtime_error(
                    span,
                    &msg,
                    Some("check that the module path is correct"),
                );
            }
        };

        // 2. Check cache
        if let Some(module) = self.module_cache.get(&resolved) {
            let module = Rc::clone(module);
            return self.bind_module(span, &module, path, selective, env);
        }

        // 3. Circular import detection
        if self.import_stack.contains(&resolved) {
            return self.runtime_error(
                span,
                &format!("circular import detected: {}", resolved.display()),
                Some("two modules are importing each other; consider restructuring to break the cycle"),
            );
        }

        // 4. Read the module file
        let source = match std::fs::read_to_string(&resolved) {
            Ok(s) => s,
            Err(e) => {
                return self.runtime_error(
                    span,
                    &format!("could not read module '{}': {}", resolved.display(), e),
                    Some("verify the file exists and has the correct permissions"),
                );
            }
        };

        // 5. Lex and parse
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer, &source);
        let program = parser.parse_program();

        let errors = parser.errors();
        if !errors.is_empty() {
            return self.runtime_error(
                span,
                &format!(
                    "parse errors in module '{}':\n{}",
                    resolved.display(),
                    parser.format_errors()
                ),
                Some("fix the syntax errors in the imported module before using it"),
            );
        }

        // 6. Evaluate in a fresh environment
        let module_env = Rc::new(RefCell::new(Environment::new()));

        let saved_file = self.current_file.take();
        let saved_main = self.is_main_context;
        self.current_file = Some(resolved.clone());
        self.is_main_context = false;
        self.import_stack.push(resolved.clone());

        let result = self.eval_program(&program, Rc::clone(&module_env));

        self.import_stack.pop();
        self.current_file = saved_file;
        self.is_main_context = saved_main;

        if result.is_error() {
            return result;
        }

        // 7. Create Module object and cache
        let module_name = path
            .segments
            .last()
            .cloned()
            .unwrap_or_else(|| "unknown".to_string());

        let module = Rc::new(Object::Module {
            name: module_name,
            env: module_env,
        });

        self.module_cache.insert(resolved, Rc::clone(&module));

        // 8. Bind into current scope
        self.bind_module(span, &module, path, selective, env)
    }

    fn resolve_module_path(&self, path: &ModulePath) -> Result<PathBuf, String> {
        if path.is_relative {
            let current = self
                .current_file
                .as_ref()
                .ok_or_else(|| "cannot use relative import without a file context".to_string())?;
            let mut base = current
                .parent()
                .ok_or_else(|| "cannot determine parent directory".to_string())?
                .to_path_buf();

            for _ in 0..path.parent_levels {
                base = base
                    .parent()
                    .ok_or_else(|| "relative path goes above root".to_string())?
                    .to_path_buf();
            }

            for segment in &path.segments {
                base = base.join(segment);
            }
            base.set_extension("oxi");

            base.canonicalize()
                .map_err(|e| format!("module not found: {} ({})", base.display(), e))
        } else {
            let mut lib_path = self.stdlib_path.clone();
            for segment in &path.segments {
                lib_path = lib_path.join(segment);
            }
            lib_path.set_extension("oxi");

            lib_path
                .canonicalize()
                .map_err(|e| format!("stdlib module not found: {} ({})", lib_path.display(), e))
        }
    }

    fn bind_module(
        &self,
        span: Span,
        module: &Rc<Object>,
        path: &ModulePath,
        selective: &Option<Vec<Identifier>>,
        env: Rc<RefCell<Environment>>,
    ) -> Rc<Object> {
        match selective {
            None => {
                let name = path
                    .segments
                    .last()
                    .cloned()
                    .unwrap_or_else(|| "unknown".to_string());
                env.borrow_mut().set(name, Rc::clone(module));
                Rc::new(Object::None)
            }
            Some(names) => {
                if let Object::Module {
                    env: module_env, ..
                } = module.as_ref()
                {
                    for ident in names {
                        let val = module_env.borrow().get(&ident.value);
                        match val {
                            Some(v) => {
                                env.borrow_mut().set(ident.value.clone(), v);
                            }
                            None => {
                                return self.runtime_error(
                                    span,
                                    &format!("name '{}' not found in module", ident.value),
                                    Some("check the spelling or verify the name is exported by the module"),
                                );
                            }
                        }
                    }
                    Rc::new(Object::None)
                } else {
                    Rc::new(Object::Error(
                        "internal error: expected Module object".to_string(),
                    ))
                }
            }
        }
    }

    pub fn eval_expression(
        &mut self,
        expr: &Expression,
        env: Rc<RefCell<Environment>>,
    ) -> Rc<Object> {
        match expr {
            Expression::Int { value, .. } => Rc::new(Object::Integer(*value)),
            Expression::Float { value, .. } => Rc::new(Object::Float(*value)),
            Expression::Char { value, .. } => Rc::new(Object::Char(*value)),
            Expression::Str { value, .. } => Rc::new(Object::String(value.clone())),
            Expression::StringInterp { parts, .. } => {
                let mut result = std::string::String::new();
                for part in parts {
                    match part {
                        StringInterpPart::Literal(s) => result.push_str(s),
                        StringInterpPart::Expr(expr) => {
                            let val = self.eval_expression(expr, Rc::clone(&env));
                            if val.is_error() {
                                return val;
                            }
                            result.push_str(&val.to_string());
                        }
                    }
                }
                Rc::new(Object::String(result))
            }
            Expression::Boolean { value, .. } => Rc::new(Object::Boolean(*value)),
            Expression::NoneExpr { .. } => Rc::new(Object::None),

            Expression::Array { elements, .. } => {
                let elems = self.eval_expressions(elements, Rc::clone(&env));
                if elems.len() == 1 && elems[0].is_error() {
                    return Rc::clone(&elems[0]);
                }
                Rc::new(Object::Array(elems))
            }

            Expression::Ident(ident) => self.eval_identifier(ident, env),

            Expression::Prefix {
                token: prefix_token,
                operator,
                right,
                ..
            } => {
                let right_val = self.eval_expression(right, env);
                if right_val.is_error() {
                    return right_val;
                }
                self.eval_prefix_expression(operator, right_val, prefix_token.span)
            }

            Expression::Infix {
                token: infix_token,
                operator,
                left,
                right,
                ..
            } => {
                // Distributed comparison: (a or b) == c  →  a == c or b == c
                if matches!(operator.as_str(), "==" | "!=" | "<" | ">" | "<=" | ">=") {
                    let left_inner = unwrap_grouped(left);
                    let right_inner = unwrap_grouped(right);
                    let left_is_logical = matches!(left_inner,
                        Expression::Infix { operator: op, .. } if op == "or" || op == "and"
                    );
                    let right_is_logical = matches!(right_inner,
                        Expression::Infix { operator: op, .. } if op == "or" || op == "and"
                    );
                    if left_is_logical && !right_is_logical {
                        let cmp_val = self.eval_expression(right, Rc::clone(&env));
                        if cmp_val.is_error() {
                            return cmp_val;
                        }
                        return self
                            .distribute_comparison(operator, left_inner, &cmp_val, true, &env);
                    }
                    if right_is_logical && !left_is_logical {
                        let cmp_val = self.eval_expression(left, Rc::clone(&env));
                        if cmp_val.is_error() {
                            return cmp_val;
                        }
                        return self.distribute_comparison(
                            operator,
                            right_inner,
                            &cmp_val,
                            false,
                            &env,
                        );
                    }
                }

                // Short-circuit for logical operators
                if operator == "and" {
                    let left_val = self.eval_expression(left, Rc::clone(&env));
                    if left_val.is_error() {
                        return left_val;
                    }
                    if !left_val.is_truthy() {
                        return Rc::new(Object::Boolean(false));
                    }
                    let right_val = self.eval_expression(right, env);
                    if right_val.is_error() {
                        return right_val;
                    }
                    return Rc::new(Object::Boolean(right_val.is_truthy()));
                }
                if operator == "or" {
                    let left_val = self.eval_expression(left, Rc::clone(&env));
                    if left_val.is_error() {
                        return left_val;
                    }
                    if left_val.is_truthy() {
                        return Rc::new(Object::Boolean(true));
                    }
                    let right_val = self.eval_expression(right, env);
                    if right_val.is_error() {
                        return right_val;
                    }
                    return Rc::new(Object::Boolean(right_val.is_truthy()));
                }

                let left_val = self.eval_expression(left, Rc::clone(&env));
                if left_val.is_error() {
                    return left_val;
                }
                let right_val = self.eval_expression(right, env);
                if right_val.is_error() {
                    return right_val;
                }
                self.eval_infix_expression(operator, left_val, right_val, infix_token.span)
            }

            Expression::Postfix {
                token: postfix_token,
                operator,
                left,
                ..
            } => self.eval_postfix_expression(operator, left, env, postfix_token.span),

            Expression::Call {
                token: call_token,
                function,
                args,
                named_args,
                ..
            } => {
                // is_mut() needs the variable name, not its value
                if let Expression::Ident(ident) = function.as_ref() {
                    if ident.value == "is_mut" {
                        return self.eval_is_mut(args, env);
                    }
                    if ident.value == "is_type" {
                        return self.eval_is_type(args, env);
                    }
                    if ident.value == "is_type_mut" {
                        return self.eval_is_type_mut(args, env);
                    }
                }

                let func = self.eval_expression(function, Rc::clone(&env));
                if func.is_error() {
                    return func;
                }
                let arguments = self.eval_expressions(args, Rc::clone(&env));
                if arguments.len() == 1 && arguments[0].is_error() {
                    return Rc::clone(&arguments[0]);
                }
                // Evaluate named arguments
                let mut eval_named: Vec<(String, Rc<Object>)> = Vec::new();
                for (name, expr) in named_args {
                    let val = self.eval_expression(expr, Rc::clone(&env));
                    if val.is_error() {
                        return val;
                    }
                    eval_named.push((name.clone(), val));
                }
                self.apply_function(func, arguments, eval_named, env, call_token.span)
            }

            Expression::Index {
                token: index_token,
                left,
                index,
                ..
            } => {
                let left_val = self.eval_expression(left, Rc::clone(&env));
                if left_val.is_error() {
                    return left_val;
                }
                let index_val = self.eval_expression(index, env);
                if index_val.is_error() {
                    return index_val;
                }
                self.eval_index_expression(left_val, index_val, index_token.span)
            }

            Expression::Slice {
                token: slice_token,
                left,
                start,
                end,
                ..
            } => {
                let left_val = self.eval_expression(left, Rc::clone(&env));
                if left_val.is_error() {
                    return left_val;
                }
                let start_val = match start {
                    Some(s) => {
                        let v = self.eval_expression(s, Rc::clone(&env));
                        if v.is_error() {
                            return v;
                        }
                        match v.as_ref() {
                            Object::Integer(n) => Some(*n as usize),
                            _ => {
                                return self.runtime_error(
                                    slice_token.span,
                                    "slice start must be an integer",
                                    Some("slice indices must be integer values"),
                                );
                            }
                        }
                    }
                    None => None,
                };
                let end_val = match end {
                    Some(e) => {
                        let v = self.eval_expression(e, Rc::clone(&env));
                        if v.is_error() {
                            return v;
                        }
                        match v.as_ref() {
                            Object::Integer(n) => Some(*n as usize),
                            _ => {
                                return self.runtime_error(
                                    slice_token.span,
                                    "slice end must be an integer",
                                    Some("slice indices must be integer values"),
                                );
                            }
                        }
                    }
                    None => None,
                };
                self.eval_slice(left_val, start_val, end_val, slice_token.span)
            }

            Expression::Grouped(inner) => self.eval_expression(inner, env),

            Expression::FunctionLiteral {
                parameters, body, ..
            } => Rc::new(Object::Function {
                parameters: parameters.clone(),
                body: body.clone(),
                env: Rc::clone(&env),
            }),

            Expression::ErrorConstruct { tag, value, .. } => {
                let payload = self.eval_expression(value, env);
                if payload.is_error() {
                    return payload;
                }
                Self::tagged_error_value(payload.to_string(), tag.clone())
            }

            Expression::ValueConstruct { value, .. } => {
                let payload = self.eval_expression(value, env);
                if payload.is_error() {
                    return payload;
                }
                Rc::new(Object::Value(payload))
            }

            Expression::TypeWrap { target, value, .. } => {
                let evaluated = self.eval_expression(value, Rc::clone(&env));

                if let Some(default_tag) = Self::error_or_value_union(target) {
                    return match Self::error_info_from(&evaluated) {
                        Some((existing_tag, msg)) => {
                            let tag = existing_tag.or(default_tag);
                            Self::tagged_error_value(msg, tag)
                        }
                        None => Rc::new(Object::Value(evaluated)),
                    };
                }

                let target_name = target.type_name();
                if evaluated.is_error() {
                    return evaluated;
                }
                match Self::convert_to_type(&evaluated, &target_name) {
                    Ok(converted) => converted,
                    Err(msg) => Self::error_object(msg),
                }
            }

            Expression::Fail { value, .. } => {
                let payload = self.eval_expression(value, env);
                if payload.is_error() {
                    return payload;
                }
                match payload.as_ref() {
                    Object::ErrorValue { msg, tag } => {
                        Self::error_object_with_tag(msg.clone(), tag.clone())
                    }
                    _ => Self::error_object(payload.to_string()),
                }
            }

            Expression::DotAccess {
                token: dot_token,
                left,
                field,
                ..
            } => {
                let is_self_access =
                    matches!(left.as_ref(), Expression::Ident(id) if id.value == "self");
                let obj = self.eval_expression(left, Rc::clone(&env));
                if obj.is_error() {
                    return obj;
                }
                match obj.as_ref() {
                    Object::StructInstance {
                        struct_name,
                        fields,
                    } => {
                        // Check fields first
                        if let Some(val) = fields.borrow().get(&field.value) {
                            // Enforce hide: block external access to hidden fields
                            if !is_self_access {
                                if let Some(def) = self.struct_defs.get(struct_name) {
                                    if let Object::StructDef {
                                        fields: def_fields, ..
                                    } = def.as_ref()
                                    {
                                        if let Some((_, _, true)) =
                                            def_fields.iter().find(|(n, _, _)| n == &field.value)
                                        {
                                            return self.runtime_error(
                                                field.token.span,
                                                &format!("field '{}' is hidden on struct {}", field.value, struct_name),
                                                Some("hidden fields can only be accessed via `self` inside a method"),
                                            );
                                        }
                                    }
                                }
                            }
                            return Rc::clone(val);
                        }
                        // Check methods
                        self.find_method(struct_name, &field.value, &fields, field.token.span)
                    }
                    Object::Module {
                        env: module_env,
                        name: mod_name,
                    } => match module_env.borrow().get(&field.value) {
                        Some(val) => val,
                        None => {
                            let available = module_env.borrow().all_keys();
                            let hint =
                                suggest_similar(&field.value, available.iter().map(|s| s.as_str()))
                                    .map(|s| format!("did you mean `{}`?", s));
                            self.runtime_error(
                                field.token.span,
                                &format!("name '{}' not found in module {}", field.value, mod_name),
                                hint.as_deref(),
                            )
                        }
                    },
                    Object::ErrorValue { msg, tag } => match field.value.as_str() {
                        "msg" => Rc::new(Object::String(msg.clone())),
                        "tag" => match tag {
                            Some(tag) => Rc::new(Object::String(tag.clone())),
                            None => Rc::new(Object::None),
                        },
                        _ => Self::error_object(format!("error has no field '{}'", field.value)),
                    },
                    Object::Value(val) => match field.value.as_str() {
                        "value" => Rc::clone(val),
                        _ => Self::error_object(format!("value has no field '{}'", field.value)),
                    },
                    _ => self.runtime_error(
                        dot_token.span,
                        &format!(
                            "cannot access field '{}' on {}",
                            field.value,
                            obj.type_name()
                        ),
                        Some(
                            "dot access is only supported on structs, modules, errors, and values",
                        ),
                    ),
                }
            }

            Expression::StructLiteral {
                token: struct_token,
                struct_name,
                field_values,
                ..
            } => {
                let def = self.struct_defs.get(struct_name);
                match def {
                    Some(def) => {
                        let def = Rc::clone(def);
                        if let Object::StructDef {
                            fields: def_fields,
                            name,
                            ..
                        } = def.as_ref()
                        {
                            // Verify all fields provided
                            if field_values.len() != def_fields.len() {
                                return self.runtime_error(
                                    struct_token.span,
                                    &format!(
                                        "struct {} has {} fields, got {}",
                                        name,
                                        def_fields.len(),
                                        field_values.len()
                                    ),
                                    Some("ensure all struct fields are provided in the literal"),
                                );
                            }
                            let mut instance_fields = HashMap::new();
                            for (field_name, expr) in field_values {
                                let val = self.eval_expression(expr, Rc::clone(&env));
                                if val.is_error() {
                                    return val;
                                }
                                // Find field in definition
                                let field_def = def_fields.iter().find(|(n, _, _)| n == field_name);
                                match field_def {
                                    Some((_, expected_type, _)) => {
                                        let actual_type = val.effective_type_name();
                                        if !type_matches(expected_type, &actual_type) {
                                            return self.runtime_error(
                                                struct_token.span,
                                                &format!("type mismatch for field '{}': expected {}, got {}", field_name, expected_type, actual_type),
                                                Some("check that the value matches the declared field type"),
                                            );
                                        }
                                        instance_fields.insert(field_name.clone(), val);
                                    }
                                    None => {
                                        let field_names: Vec<&str> =
                                            def_fields.iter().map(|(n, _, _)| n.as_str()).collect();
                                        let hint =
                                            suggest_similar(field_name, field_names.into_iter())
                                                .map(|s| format!("did you mean `{}`?", s));
                                        return self.runtime_error(
                                            struct_token.span,
                                            &format!(
                                                "unknown field '{}' for struct {}",
                                                field_name, name
                                            ),
                                            hint.as_deref(),
                                        );
                                    }
                                }
                            }
                            Rc::new(Object::StructInstance {
                                struct_name: name.clone(),
                                fields: Rc::new(RefCell::new(instance_fields)),
                            })
                        } else {
                            self.runtime_error(
                                struct_token.span,
                                &format!("{} is not a struct", struct_name),
                                Some("only struct definitions can be instantiated with literal syntax"),
                            )
                        }
                    }
                    None => self.runtime_error(
                        struct_token.span,
                        &format!("struct not found: {}", struct_name),
                        Some("make sure the struct is defined before use"),
                    ),
                }
            }

            Expression::TupleLiteral { elements, .. } => {
                let elems = self.eval_expressions(elements, Rc::clone(&env));
                if elems.len() == 1 && elems[0].is_error() {
                    return Rc::clone(&elems[0]);
                }
                Rc::new(Object::Tuple(elems))
            }

            Expression::MapLiteral { entries, .. } => {
                let mut map_entries = Vec::new();
                for (key_expr, val_expr) in entries {
                    let key = self.eval_expression(key_expr, Rc::clone(&env));
                    if key.is_error() {
                        return key;
                    }
                    let val = self.eval_expression(val_expr, Rc::clone(&env));
                    if val.is_error() {
                        return val;
                    }
                    map_entries.push((key, val));
                }
                Rc::new(Object::Map(map_entries))
            }

            Expression::Option {
                arms,
                default,
                error_default,
                ..
            } => {
                for arm in arms {
                    let cond = self.eval_expression(&arm.condition, Rc::clone(&env));
                    if cond.is_error() {
                        return match error_default {
                            Some(stmts) => self.eval_block(stmts, Rc::clone(&env)),
                            None => cond,
                        };
                    }
                    if cond.is_truthy() {
                        let body_result = self.eval_block(&arm.body, Rc::clone(&env));
                        if body_result.is_error() {
                            return match error_default {
                                Some(stmts) => self.eval_block(stmts, Rc::clone(&env)),
                                None => body_result,
                            };
                        }
                        return body_result;
                    }
                }
                match default {
                    Some(stmts) => {
                        let result = self.eval_block(stmts, Rc::clone(&env));
                        if result.is_error() {
                            match error_default {
                                Some(error_stmts) => self.eval_block(error_stmts, Rc::clone(&env)),
                                None => result,
                            }
                        } else {
                            result
                        }
                    }
                    None => Rc::new(Object::None),
                }
            }
            Expression::Guard {
                value,
                binding,
                error_tag,
                fallback,
                ..
            } => {
                let evaluated = self.eval_expression(value, Rc::clone(&env));
                let Some((tag, msg)) = Self::error_info_from(&evaluated) else {
                    return evaluated;
                };
                if error_tag
                    .as_ref()
                    .is_some_and(|wanted| tag.as_ref() != Some(wanted))
                {
                    return evaluated;
                }

                let guard_env = Rc::new(RefCell::new(Environment::new_enclosed(Rc::clone(&env))));
                guard_env
                    .borrow_mut()
                    .set(binding.value.clone(), Self::tagged_error_value(msg, tag));
                self.eval_expression(fallback, guard_env)
            }
            Expression::Log {
                tag,
                sub_tag,
                message,
                ..
            } => {
                let explicit_msg = if let Some(msg_expr) = message {
                    let evaluated = self.eval_expression(msg_expr, Rc::clone(&env));
                    if evaluated.is_error() {
                        return evaluated;
                    }
                    Some(format!("{}", evaluated))
                } else {
                    None
                };

                let (tag_str, msg_str) =
                    Self::resolve_log_parts(tag.as_deref(), sub_tag.as_deref(), explicit_msg, &env);

                let now = std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap_or_default();
                let secs = now.as_secs();
                let (hours, mins, seconds) = ((secs / 3600) % 24, (secs / 60) % 60, secs % 60);
                // Convert to local-ish date/time from epoch
                let days = secs / 86400;
                let (year, month, day) = Self::epoch_days_to_date(days);
                let timestamp = format!(
                    "{:04}-{:02}-{:02} {:02}:{:02}:{:02}",
                    year, month, day, hours, mins, seconds
                );

                let output = match (tag_str.is_empty(), &msg_str) {
                    (true, Some(m)) => format!("{}: {}", timestamp, m),
                    (false, Some(m)) => format!("{}: {} {}", timestamp, tag_str, m),
                    (false, None) => format!("{}: {}", timestamp, tag_str),
                    (true, None) => format!("{}: <empty log>", timestamp),
                };

                eprintln!("{}", output);
                Rc::new(Object::None)
            }
            Expression::Unless {
                consequence,
                condition,
                alternative,
                ..
            } => {
                let cond = self.eval_expression(condition, Rc::clone(&env));
                if cond.is_error() {
                    return cond;
                }

                if !cond.is_truthy() {
                    self.eval_expression(consequence, env)
                } else {
                    self.eval_expression(alternative, env)
                }
            }
        }
    }

    fn find_method(
        &self,
        struct_name: &str,
        method_name: &str,
        instance_fields: &Rc<RefCell<HashMap<String, Rc<Object>>>>,
        span: Span,
    ) -> Rc<Object> {
        let mut current_name = struct_name.to_string();
        loop {
            let def = self.struct_defs.get(&current_name);
            match def {
                Some(def) => {
                    if let Object::StructDef {
                        methods, parent, ..
                    } = def.as_ref()
                    {
                        if let Some(method) = methods.get(method_name) {
                            if let Object::Function {
                                parameters,
                                body,
                                env,
                            } = method.as_ref()
                            {
                                let bound_env = Rc::new(RefCell::new(Environment::new_enclosed(
                                    Rc::clone(env),
                                )));
                                let field_names: Vec<String> =
                                    instance_fields.borrow().keys().cloned().collect();
                                for (k, v) in instance_fields.borrow().iter() {
                                    bound_env.borrow_mut().set(k.clone(), Rc::clone(v));
                                }
                                // Inject `self` so methods can use self.field
                                let self_obj = Rc::new(Object::StructInstance {
                                    struct_name: struct_name.to_string(),
                                    fields: Rc::clone(instance_fields),
                                });
                                bound_env.borrow_mut().set("self".to_string(), self_obj);
                                return Rc::new(Object::BoundMethod {
                                    parameters: parameters.clone(),
                                    body: body.clone(),
                                    env: bound_env,
                                    instance_fields: Rc::clone(instance_fields),
                                    field_names,
                                });
                            }
                            return Rc::clone(method);
                        }
                        if let Some(p) = parent {
                            current_name = p.clone();
                            continue;
                        }
                        // Method not found — collect all available names for suggestion
                        let mut all_names: Vec<String> = methods.keys().cloned().collect();
                        // Also add field names (user may confuse field/method)
                        all_names.extend(instance_fields.borrow().keys().cloned());
                        let hint = suggest_similar(
                            method_name,
                            all_names.iter().map(|s: &String| s.as_str()),
                        )
                        .map(|s| format!("did you mean `{}`?", s));
                        return self.runtime_error(
                            span,
                            &format!(
                                "method '{}' not found on struct {}",
                                method_name, struct_name
                            ),
                            hint.as_deref(),
                        );
                    }
                }
                None => {
                    return self.runtime_error(
                        span,
                        &format!("struct definition not found: {}", current_name),
                        Some(
                            "ensure the struct is defined or imported before calling methods on it",
                        ),
                    );
                }
            }
        }
    }

    fn eval_identifier(&self, ident: &Identifier, env: Rc<RefCell<Environment>>) -> Rc<Object> {
        // First check environment
        if let Some(val) = env.borrow().get(&ident.value) {
            return val;
        }

        // Then check builtins
        if let Some(builtin) = self.builtins.get(&ident.value) {
            return Rc::clone(builtin);
        }

        // Collect all known names for "did you mean?" suggestions
        let mut all_names = env.borrow().all_keys();
        all_names.extend(self.builtins.keys().cloned());
        all_names.extend(self.struct_defs.keys().cloned());

        let hint = suggest_similar(&ident.value, all_names.iter().map(|s| s.as_str()))
            .map(|s| format!("did you mean `{}`?", s));
        self.runtime_error(
            ident.token.span,
            &format!("identifier not found: {}", ident.value),
            hint.as_deref(),
        )
    }

    fn eval_prefix_expression(&self, operator: &str, right: Rc<Object>, span: Span) -> Rc<Object> {
        match operator {
            "!" | "not" => self.eval_bang_operator(right),
            "-" => self.eval_minus_prefix(right, span),
            _ => self.runtime_error(
                span,
                &format!("unknown operator: {}{}", operator, right.type_name()),
                Some("this prefix operator is not supported for this type"),
            ),
        }
    }

    fn eval_bang_operator(&self, right: Rc<Object>) -> Rc<Object> {
        Rc::new(Object::Boolean(!right.is_truthy()))
    }

    fn eval_minus_prefix(&self, right: Rc<Object>, span: Span) -> Rc<Object> {
        match right.as_ref() {
            Object::Integer(n) => Rc::new(Object::Integer(-n)),
            Object::Float(n) => Rc::new(Object::Float(-n)),
            _ => self.runtime_error(
                span,
                &format!("unknown operator: -{}", right.type_name()),
                Some("negation is only supported for integers and floats"),
            ),
        }
    }

    /// Distributes a comparison across an `or`/`and` group.
    /// `(a or b) == val`  →  `(a == val) or (b == val)`
    /// `(a and b) > val`  →  `(a > val) and (b > val)`
    fn distribute_comparison(
        &mut self,
        cmp_op: &str,
        expr: &Expression,
        cmp_value: &Rc<Object>,
        value_on_right: bool,
        env: &Rc<RefCell<Environment>>,
    ) -> Rc<Object> {
        if let Expression::Infix {
            operator,
            left,
            right,
            ..
        } = expr
        {
            if operator == "or" {
                let left_result =
                    self.distribute_comparison(cmp_op, left, cmp_value, value_on_right, env);
                if left_result.is_error() {
                    return left_result;
                }
                if left_result.is_truthy() {
                    return Rc::new(Object::Boolean(true));
                }
                let right_result =
                    self.distribute_comparison(cmp_op, right, cmp_value, value_on_right, env);
                if right_result.is_error() {
                    return right_result;
                }
                return Rc::new(Object::Boolean(right_result.is_truthy()));
            }
            if operator == "and" {
                let left_result =
                    self.distribute_comparison(cmp_op, left, cmp_value, value_on_right, env);
                if left_result.is_error() {
                    return left_result;
                }
                if !left_result.is_truthy() {
                    return Rc::new(Object::Boolean(false));
                }
                let right_result =
                    self.distribute_comparison(cmp_op, right, cmp_value, value_on_right, env);
                if right_result.is_error() {
                    return right_result;
                }
                return Rc::new(Object::Boolean(right_result.is_truthy()));
            }
        }
        // Leaf: evaluate the expression and apply the comparison
        let val = self.eval_expression(expr, Rc::clone(env));
        if val.is_error() {
            return val;
        }
        if value_on_right {
            self.eval_infix_expression(cmp_op, val, Rc::clone(cmp_value), Span::default())
        } else {
            self.eval_infix_expression(cmp_op, Rc::clone(cmp_value), val, Span::default())
        }
    }

    fn eval_infix_expression(
        &self,
        operator: &str,
        left: Rc<Object>,
        right: Rc<Object>,
        span: Span,
    ) -> Rc<Object> {
        match (left.as_ref(), right.as_ref()) {
            (Object::Integer(l), Object::Integer(r)) => {
                self.eval_integer_infix(operator, *l, *r, span)
            }
            (Object::Float(l), Object::Float(r)) => self.eval_float_infix(operator, *l, *r, span),
            (Object::Integer(l), Object::Float(r)) => {
                self.eval_float_infix(operator, *l as f64, *r, span)
            }
            (Object::Float(l), Object::Integer(r)) => {
                self.eval_float_infix(operator, *l, *r as f64, span)
            }
            (Object::String(l), Object::String(r)) => self.eval_string_infix(operator, l, r, span),
            (Object::Char(l), Object::Char(r)) => self.eval_char_infix(operator, *l, *r, span),
            (Object::Boolean(l), Object::Boolean(r)) => match operator {
                "==" => Rc::new(Object::Boolean(l == r)),
                "!=" => Rc::new(Object::Boolean(l != r)),
                _ => self.runtime_error(
                    span,
                    &format!("unknown operator: BOOLEAN {} BOOLEAN", operator),
                    Some("booleans only support == and != comparisons"),
                ),
            },
            // Byte arithmetic — promote to int
            (Object::Byte(l), Object::Byte(r)) => {
                self.eval_integer_infix(operator, *l as i64, *r as i64, span)
            }
            (Object::Byte(l), Object::Integer(r)) => {
                self.eval_integer_infix(operator, *l as i64, *r, span)
            }
            (Object::Integer(l), Object::Byte(r)) => {
                self.eval_integer_infix(operator, *l, *r as i64, span)
            }
            (Object::Byte(l), Object::Float(r)) => {
                self.eval_float_infix(operator, *l as f64, *r, span)
            }
            (Object::Float(l), Object::Byte(r)) => {
                self.eval_float_infix(operator, *l, *r as f64, span)
            }
            // Uint arithmetic
            (Object::Uint(l), Object::Uint(r)) => self.eval_uint_infix(operator, *l, *r, span),
            (Object::Uint(l), Object::Integer(r)) => {
                self.eval_integer_infix(operator, *l as i64, *r, span)
            }
            (Object::Integer(l), Object::Uint(r)) => {
                self.eval_integer_infix(operator, *l, *r as i64, span)
            }
            (Object::Uint(l), Object::Float(r)) => {
                self.eval_float_infix(operator, *l as f64, *r, span)
            }
            (Object::Float(l), Object::Uint(r)) => {
                self.eval_float_infix(operator, *l, *r as f64, span)
            }
            (Object::Uint(l), Object::Byte(r)) => {
                self.eval_uint_infix(operator, *l, *r as u64, span)
            }
            (Object::Byte(l), Object::Uint(r)) => {
                self.eval_uint_infix(operator, *l as u64, *r, span)
            }
            // Tuple concatenation
            (Object::Tuple(l), Object::Tuple(r)) => match operator {
                "+" => {
                    let mut new = l.clone();
                    new.extend(r.clone());
                    Rc::new(Object::Tuple(new))
                }
                "==" => Rc::new(Object::Boolean(l == r)),
                "!=" => Rc::new(Object::Boolean(l != r)),
                _ => self.runtime_error(
                    span,
                    &format!("unknown operator: TUPLE {} TUPLE", operator),
                    Some("tuples only support +, ==, and != operators"),
                ),
            },
            _ => {
                if operator == "==" {
                    Rc::new(Object::Boolean(left == right))
                } else if operator == "!=" {
                    Rc::new(Object::Boolean(left != right))
                } else {
                    self.runtime_error(
                        span,
                        &format!(
                            "type mismatch: {} {} {}",
                            left.type_name(),
                            operator,
                            right.type_name()
                        ),
                        Some("operands must be the same type for this operator"),
                    )
                }
            }
        }
    }

    fn eval_integer_infix(&self, operator: &str, left: i64, right: i64, span: Span) -> Rc<Object> {
        match operator {
            "+" => Rc::new(Object::Integer(left + right)),
            "-" => Rc::new(Object::Integer(left - right)),
            "*" => Rc::new(Object::Integer(left * right)),
            "/" => {
                if right == 0 {
                    self.runtime_error(
                        span,
                        "division by zero",
                        Some("ensure the divisor is not zero before dividing"),
                    )
                } else {
                    Rc::new(Object::Integer(left / right))
                }
            }
            "%" => {
                if right == 0 {
                    self.runtime_error(
                        span,
                        "modulo by zero",
                        Some("ensure the divisor is not zero before using %"),
                    )
                } else {
                    Rc::new(Object::Integer(left % right))
                }
            }
            "<" => Rc::new(Object::Boolean(left < right)),
            ">" => Rc::new(Object::Boolean(left > right)),
            "<=" | ">=" if operator == "<=" => Rc::new(Object::Boolean(left <= right)),
            ">=" => Rc::new(Object::Boolean(left >= right)),
            "==" => Rc::new(Object::Boolean(left == right)),
            "!=" => Rc::new(Object::Boolean(left != right)),
            _ => self.runtime_error(
                span,
                &format!("unknown operator: INTEGER {} INTEGER", operator),
                Some("this operator is not supported between integers"),
            ),
        }
    }

    fn eval_uint_infix(&self, operator: &str, left: u64, right: u64, span: Span) -> Rc<Object> {
        match operator {
            "+" => Rc::new(Object::Uint(left.wrapping_add(right))),
            "-" => {
                if right > left {
                    self.runtime_error(
                        span,
                        "unsigned integer underflow",
                        Some("subtraction would produce a negative value, which uint cannot hold"),
                    )
                } else {
                    Rc::new(Object::Uint(left - right))
                }
            }
            "*" => Rc::new(Object::Uint(left.wrapping_mul(right))),
            "/" => {
                if right == 0 {
                    self.runtime_error(
                        span,
                        "division by zero",
                        Some("ensure the divisor is not zero before dividing"),
                    )
                } else {
                    Rc::new(Object::Uint(left / right))
                }
            }
            "%" => {
                if right == 0 {
                    self.runtime_error(
                        span,
                        "modulo by zero",
                        Some("ensure the divisor is not zero before using %"),
                    )
                } else {
                    Rc::new(Object::Uint(left % right))
                }
            }
            "<" => Rc::new(Object::Boolean(left < right)),
            ">" => Rc::new(Object::Boolean(left > right)),
            "<=" => Rc::new(Object::Boolean(left <= right)),
            ">=" => Rc::new(Object::Boolean(left >= right)),
            "==" => Rc::new(Object::Boolean(left == right)),
            "!=" => Rc::new(Object::Boolean(left != right)),
            _ => self.runtime_error(
                span,
                &format!("unknown operator: UINT {} UINT", operator),
                Some("this operator is not supported between unsigned integers"),
            ),
        }
    }

    fn eval_float_infix(&self, operator: &str, left: f64, right: f64, span: Span) -> Rc<Object> {
        match operator {
            "+" => Rc::new(Object::Float(left + right)),
            "-" => Rc::new(Object::Float(left - right)),
            "*" => Rc::new(Object::Float(left * right)),
            "/" => Rc::new(Object::Float(left / right)),
            "%" => Rc::new(Object::Float(left % right)),
            "<" => Rc::new(Object::Boolean(left < right)),
            ">" => Rc::new(Object::Boolean(left > right)),
            "<=" => Rc::new(Object::Boolean(left <= right)),
            ">=" => Rc::new(Object::Boolean(left >= right)),
            "==" => Rc::new(Object::Boolean(left == right)),
            "!=" => Rc::new(Object::Boolean(left != right)),
            _ => self.runtime_error(
                span,
                &format!("unknown operator: FLOAT {} FLOAT", operator),
                Some("this operator is not supported between floats"),
            ),
        }
    }

    fn eval_string_infix(&self, operator: &str, left: &str, right: &str, span: Span) -> Rc<Object> {
        match operator {
            "+" => Rc::new(Object::String(format!("{}{}", left, right))),
            "==" => Rc::new(Object::Boolean(left == right)),
            "!=" => Rc::new(Object::Boolean(left != right)),
            _ => self.runtime_error(
                span,
                &format!("unknown operator: STRING {} STRING", operator),
                Some("strings only support +, ==, and != operators"),
            ),
        }
    }

    fn eval_char_infix(&self, operator: &str, left: char, right: char, span: Span) -> Rc<Object> {
        match operator {
            "==" => Rc::new(Object::Boolean(left == right)),
            "!=" => Rc::new(Object::Boolean(left != right)),
            "<" => Rc::new(Object::Boolean(left < right)),
            ">" => Rc::new(Object::Boolean(left > right)),
            "<=" => Rc::new(Object::Boolean(left <= right)),
            ">=" => Rc::new(Object::Boolean(left >= right)),
            _ => self.runtime_error(
                span,
                &format!("unknown operator: CHAR {} CHAR", operator),
                Some("chars only support comparison operators (==, !=, <, >, <=, >=)"),
            ),
        }
    }

    fn eval_postfix_expression(
        &mut self,
        operator: &str,
        left: &Expression,
        env: Rc<RefCell<Environment>>,
        span: Span,
    ) -> Rc<Object> {
        // Get the identifier name for postfix operations
        let ident_name = match left {
            Expression::Ident(ident) => ident.value.clone(),
            _ => {
                return self.runtime_error(
                    span,
                    "postfix operator requires identifier",
                    Some("use ++ or -- only on variable names, e.g. `count++`"),
                );
            }
        };

        // Get current value
        let current = env.borrow().get(&ident_name);
        let current = match current {
            Some(val) => val,
            None => {
                return self.runtime_error(
                    span,
                    &format!("identifier not found: {}", ident_name),
                    Some("make sure the variable is declared before using postfix operators"),
                );
            }
        };

        // Immutable check: ++/-- cannot mutate immutable variables
        if env.borrow().is_immutable(&ident_name) {
            return self.runtime_error(
                span,
                &format!(
                    "cannot mutate immutable variable '{}'. use := to override",
                    ident_name
                ),
                Some("immutable variables cannot be changed with ++ or --; use := to reassign"),
            );
        }

        // Calculate new value and return original
        let (new_val, return_val) = match (operator, current.as_ref()) {
            ("++", Object::Integer(n)) => (
                Rc::new(Object::Integer(n + 1)),
                Rc::new(Object::Integer(*n)),
            ),
            ("--", Object::Integer(n)) => (
                Rc::new(Object::Integer(n - 1)),
                Rc::new(Object::Integer(*n)),
            ),
            _ => {
                return self.runtime_error(
                    span,
                    &format!(
                        "unknown postfix operator: {}{}",
                        current.type_name(),
                        operator
                    ),
                    Some("postfix ++ and -- are only supported on integer values"),
                );
            }
        };

        // Update the variable in environment
        env.borrow_mut().update(&ident_name, new_val);

        return_val
    }

    fn eval_index_expression(&self, left: Rc<Object>, index: Rc<Object>, span: Span) -> Rc<Object> {
        match (left.as_ref(), index.as_ref()) {
            (Object::Array(arr), Object::Integer(idx)) => {
                let idx = *idx as usize;
                if idx >= arr.len() {
                    Rc::new(Object::None)
                } else {
                    Rc::clone(&arr[idx])
                }
            }
            (Object::String(s), Object::Integer(idx)) => {
                let idx = *idx as usize;
                if idx >= s.len() {
                    Rc::new(Object::None)
                } else {
                    Rc::new(Object::String(s.chars().nth(idx).unwrap().to_string()))
                }
            }
            (Object::Tuple(elements), Object::Integer(idx)) => {
                let idx = *idx as usize;
                if idx >= elements.len() {
                    Rc::new(Object::None)
                } else {
                    Rc::clone(&elements[idx])
                }
            }
            (Object::Map(entries), _) => {
                for (k, v) in entries {
                    if *k == index {
                        return Rc::clone(v);
                    }
                }
                Rc::new(Object::None)
            }
            _ => self.runtime_error(
                span,
                &format!(
                    "index operator not supported: {}[{}]",
                    left.type_name(),
                    index.type_name()
                ),
                Some("indexing is supported on arrays, strings, tuples, and maps"),
            ),
        }
    }

    fn eval_slice(
        &self,
        left: Rc<Object>,
        start: Option<usize>,
        end: Option<usize>,
        span: Span,
    ) -> Rc<Object> {
        match left.as_ref() {
            Object::Array(arr) => {
                let s = start.unwrap_or(0);
                let e = end.unwrap_or(arr.len()).min(arr.len());
                if s > e || s > arr.len() {
                    return Rc::new(Object::Array(Vec::new()));
                }
                Rc::new(Object::Array(arr[s..e].to_vec()))
            }
            Object::String(string) => {
                let chars: Vec<char> = string.chars().collect();
                let s = start.unwrap_or(0);
                let e = end.unwrap_or(chars.len()).min(chars.len());
                if s > e || s > chars.len() {
                    return Rc::new(Object::String(String::new()));
                }
                Rc::new(Object::String(chars[s..e].iter().collect()))
            }
            Object::Tuple(elements) => {
                let s = start.unwrap_or(0);
                let e = end.unwrap_or(elements.len()).min(elements.len());
                if s > e || s > elements.len() {
                    return Rc::new(Object::Tuple(Vec::new()));
                }
                Rc::new(Object::Tuple(elements[s..e].to_vec()))
            }
            _ => self.runtime_error(
                span,
                &format!("slice not supported on {}", left.type_name()),
                Some("slicing is supported on arrays, strings, and tuples"),
            ),
        }
    }

    fn eval_expressions(
        &mut self,
        exprs: &[Expression],
        env: Rc<RefCell<Environment>>,
    ) -> Vec<Rc<Object>> {
        let mut results = Vec::new();

        for expr in exprs {
            let evaluated = self.eval_expression(expr, Rc::clone(&env));
            if evaluated.is_error() {
                return vec![evaluated];
            }
            results.push(evaluated);
        }

        results
    }

    /// Bind function arguments to parameters, handling positional args, named args,
    /// defaults, and optional params. Returns Some(error) on failure, None on success.
    fn bind_params(
        &mut self,
        parameters: &[TypedParam],
        args: &[Rc<Object>],
        named_args: &[(String, Rc<Object>)],
        env: &Rc<RefCell<Environment>>,
        call_span: Span,
    ) -> Option<Rc<Object>> {
        // Count required params (no default, not optional)
        let required = parameters
            .iter()
            .filter(|p| p.default.is_none() && !p.optional)
            .count();
        let total_provided = args.len() + named_args.len();

        if total_provided < required {
            return Some(self.runtime_error(
                call_span,
                &format!(
                    "wrong number of arguments: expected at least {}, got {}",
                    required, total_provided
                ),
                None,
            ));
        }
        if total_provided > parameters.len() {
            return Some(self.runtime_error(
                call_span,
                &format!(
                    "wrong number of arguments: expected at most {}, got {}",
                    parameters.len(),
                    total_provided
                ),
                None,
            ));
        }

        // Check for duplicate named args or named args that conflict with positional
        for (name, _) in named_args {
            // Check if this name matches a param that was already filled positionally
            if let Some(pos) = parameters.iter().position(|p| p.ident.value == *name) {
                if pos < args.len() {
                    return Some(self.runtime_error(
                        call_span,
                        &format!(
                            "parameter '{}' was given both positionally and by name",
                            name
                        ),
                        None,
                    ));
                }
            } else {
                return Some(self.runtime_error(
                    call_span,
                    &format!("unknown parameter name: '{}'", name),
                    None,
                ));
            }
        }

        for (i, param) in parameters.iter().enumerate() {
            let val = if i < args.len() {
                // Filled by positional arg
                Rc::clone(&args[i])
            } else if let Some((_, v)) = named_args.iter().find(|(n, _)| *n == param.ident.value) {
                // Filled by named arg
                Rc::clone(v)
            } else if let Some(ref default_expr) = param.default {
                // Use default value
                let v = self.eval_expression(default_expr, Rc::clone(env));
                if v.is_error() {
                    return Some(v);
                }
                v
            } else if param.optional {
                // Optional param with no default — use None
                Rc::new(Object::None)
            } else {
                return Some(self.runtime_error(
                    call_span,
                    &format!("missing required argument: '{}'", param.ident.value),
                    None,
                ));
            };

            // Type check
            if let Some(ref expected_ta) = param.type_ann {
                // Skip type check for None on optional params
                if !(param.optional && matches!(val.as_ref(), Object::None)) {
                    let expected = expected_ta.type_name();
                    let actual = val.effective_type_name();
                    if !type_matches(&expected, &actual) {
                        return Some(self.runtime_error(
                            call_span,
                            &format!(
                                "type mismatch for parameter '{}': expected {}, got {}",
                                param.ident.value, expected, actual
                            ),
                            Some(&format!(
                                "parameter '{}' requires type {}",
                                param.ident.value, expected
                            )),
                        ));
                    }
                }
            }

            env.borrow_mut().set(param.ident.value.clone(), val);
        }

        None
    }

    fn apply_function(
        &mut self,
        func: Rc<Object>,
        args: Vec<Rc<Object>>,
        named_args: Vec<(String, Rc<Object>)>,
        _env: Rc<RefCell<Environment>>,
        call_span: Span,
    ) -> Rc<Object> {
        match func.as_ref() {
            Object::Builtin(f) => {
                if !named_args.is_empty() {
                    return self.runtime_error(
                        call_span,
                        "named arguments are not supported for built-in functions",
                        None,
                    );
                }
                f(args)
            }
            Object::Function {
                parameters,
                body,
                env,
            } => {
                let extended_env = Rc::new(RefCell::new(Environment::new_enclosed(Rc::clone(env))));
                if let Some(err) =
                    self.bind_params(parameters, &args, &named_args, &extended_env, call_span)
                {
                    return err;
                }

                let result = self.eval_block(body, extended_env);
                match result.as_ref() {
                    Object::Return(val) => Rc::clone(val),
                    _ => result,
                }
            }
            Object::BoundMethod {
                parameters,
                body,
                env,
                instance_fields,
                field_names,
            } => {
                let extended_env = Rc::new(RefCell::new(Environment::new_enclosed(Rc::clone(env))));
                if let Some(err) =
                    self.bind_params(parameters, &args, &named_args, &extended_env, call_span)
                {
                    return err;
                }

                let result = self.eval_block(body, Rc::clone(&extended_env));

                // Write back any changed field values to the instance
                for field_name in field_names {
                    if let Some(val) = extended_env.borrow().get(field_name) {
                        instance_fields.borrow_mut().insert(field_name.clone(), val);
                    }
                }

                match result.as_ref() {
                    Object::Return(val) => Rc::clone(val),
                    _ => result,
                }
            }
            Object::StructDef { name, fields, .. } => {
                // Positional instantiation: Person("Alice", 30)
                if args.len() != fields.len() {
                    return self.runtime_error(
                        call_span,
                        &format!(
                            "struct {} has {} fields, got {} arguments",
                            name,
                            fields.len(),
                            args.len()
                        ),
                        None,
                    );
                }

                let mut instance_fields = HashMap::new();
                for ((field_name, expected_type, _), arg) in fields.iter().zip(args.iter()) {
                    let actual_type = arg.effective_type_name();
                    if !type_matches(expected_type, &actual_type) {
                        return self.runtime_error(
                            call_span,
                            &format!(
                                "type mismatch for field '{}': expected {}, got {}",
                                field_name, expected_type, actual_type
                            ),
                            Some(&format!(
                                "field '{}' requires type {}",
                                field_name, expected_type
                            )),
                        );
                    }
                    instance_fields.insert(field_name.clone(), Rc::clone(arg));
                }

                Rc::new(Object::StructInstance {
                    struct_name: name.clone(),
                    fields: Rc::new(RefCell::new(instance_fields)),
                })
            }
            _ => self.runtime_error(
                call_span,
                &format!("not a function: {}", func.type_name()),
                None,
            ),
        }
    }

    fn eval_block(
        &mut self,
        statements: &[Statement],
        env: Rc<RefCell<Environment>>,
    ) -> Rc<Object> {
        let mut result = Rc::new(Object::None);

        for stmt in statements {
            result = self.eval_statement(stmt, Rc::clone(&env));

            match result.as_ref() {
                Object::Return(_) | Object::Error(_) | Object::Skip | Object::Stop => {
                    return result;
                }
                _ => {}
            }
        }

        result
    }

    // --- Control flow ---

    fn eval_each(
        &mut self,
        variable: &Identifier,
        iterable: &Expression,
        body: &[Statement],
        env: Rc<RefCell<Environment>>,
        span: Span,
    ) -> Rc<Object> {
        let iterable_val = self.eval_expression(iterable, Rc::clone(&env));
        if iterable_val.is_error() {
            return iterable_val;
        }

        let elements = match iterable_val.as_ref() {
            Object::Array(arr) => arr.clone(),
            Object::String(s) => s
                .chars()
                .map(|c| Rc::new(Object::String(c.to_string())))
                .collect(),
            Object::Tuple(t) => t.clone(),
            Object::Set(s) => s.clone(),
            Object::Map(entries) => entries
                .iter()
                .map(|(k, v)| Rc::new(Object::Tuple(vec![Rc::clone(k), Rc::clone(v)])))
                .collect(),
            _ => {
                return self.runtime_error(
                    span,
                    &format!("cannot iterate over {}", iterable_val.type_name()),
                    Some("each loops support arrays, strings, tuples, sets, and maps"),
                );
            }
        };

        for element in elements {
            let loop_env = Rc::new(RefCell::new(Environment::new_enclosed(Rc::clone(&env))));
            loop_env
                .borrow_mut()
                .set(variable.value.clone(), Rc::clone(&element));

            let result = self.eval_block(body, loop_env);

            match result.as_ref() {
                Object::Skip => continue,
                Object::Stop => break,
                Object::Error(_) | Object::Return(_) => return result,
                _ => {}
            }
        }

        Rc::new(Object::None)
    }

    fn eval_repeat(
        &mut self,
        condition: &Expression,
        body: &[Statement],
        env: Rc<RefCell<Environment>>,
    ) -> Rc<Object> {
        loop {
            let cond_val = self.eval_expression(condition, Rc::clone(&env));
            if cond_val.is_error() {
                return cond_val;
            }

            if !cond_val.is_truthy() {
                break;
            }

            let result = self.eval_block(body, Rc::clone(&env));

            match result.as_ref() {
                Object::Skip => continue,
                Object::Stop => break,
                Object::Error(_) | Object::Return(_) => return result,
                _ => {}
            }
        }

        Rc::new(Object::None)
    }

    fn eval_choose(
        &mut self,
        subject: &Expression,
        arms: &[crate::ast::ChooseArm],
        env: Rc<RefCell<Environment>>,
        span: Span,
    ) -> Rc<Object> {
        let subject_val = self.eval_expression(subject, Rc::clone(&env));
        if subject_val.is_error() {
            return subject_val;
        }

        for arm in arms {
            // Handle 'else' arm
            if arm.pattern_name == "else" {
                return self.eval_expression(&arm.body, Rc::clone(&env));
            }

            // If inline pattern, register it on the fly
            if let (Some(params), Some(condition)) = (&arm.inline_params, &arm.inline_condition) {
                let param_names: Vec<String> = params.iter().map(|p| p.value.clone()).collect();
                self.patterns
                    .register(arm.pattern_name.clone(), param_names, condition.clone());
            }

            // Look up pattern
            let pattern = match self.patterns.get(&arm.pattern_name) {
                Some(p) => p.clone(),
                None => {
                    return self.runtime_error(
                        span,
                        &format!("unknown pattern: {}", arm.pattern_name),
                        Some("define the pattern before using it in a choose expression"),
                    );
                }
            };

            // Create environment with pattern parameter bound to subject
            let pattern_env = Rc::new(RefCell::new(Environment::new_enclosed(Rc::clone(&env))));
            if !pattern.parameters.is_empty() {
                pattern_env
                    .borrow_mut()
                    .set(pattern.parameters[0].clone(), Rc::clone(&subject_val));
            }

            // Evaluate pattern condition
            let matches = self.eval_expression(&pattern.condition, Rc::clone(&pattern_env));
            if matches.is_error() {
                return matches;
            }

            if matches.is_truthy() {
                return self.eval_expression(&arm.body, Rc::clone(&env));
            }
        }

        Rc::new(Object::None)
    }

    fn eval_is_type(&mut self, args: &[Expression], env: Rc<RefCell<Environment>>) -> Rc<Object> {
        if args.len() != 2 {
            return Rc::new(Object::Error(format!(
                "wrong number of arguments. got={}, want=2",
                args.len()
            )));
        }

        let val = self.eval_expression(&args[0], Rc::clone(&env));
        if val.is_error() {
            return val;
        }

        // Second argument should be a type name identifier (e.g., int, str, float)
        let type_name = match &args[1] {
            Expression::Ident(ident) => ident.value.as_str(),
            _ => {
                return Rc::new(Object::Error(
                    "second argument to `is_type` must be a type name".to_string(),
                ));
            }
        };

        let matches = match (val.as_ref(), type_name) {
            (Object::Integer(_), "int") => true,
            (Object::Float(_), "float") => true,
            (Object::String(_), "str") => true,
            (Object::Char(_), "char") => true,
            (Object::Boolean(_), "bool") => true,
            (Object::Array(_), "array") => true,
            (Object::Byte(_), "byte") => true,
            (Object::Uint(_), "uint") => true,
            (Object::Tuple(_), "tuple") => true,
            (Object::Map(_), "map") => true,
            (Object::Set(_), "set") => true,
            (Object::None, "None") => true,
            (Object::Function { .. }, "fun") => true,
            (
                Object::StructInstance {
                    struct_name: stn, ..
                },
                name,
            ) => stn == name,
            _ => false,
        };

        Rc::new(Object::Boolean(matches))
    }

    fn eval_is_type_mut(&self, args: &[Expression], env: Rc<RefCell<Environment>>) -> Rc<Object> {
        if args.len() != 1 {
            return Rc::new(Object::Error(format!(
                "wrong number of arguments. got={}, want=1",
                args.len()
            )));
        }
        match &args[0] {
            Expression::Ident(ident) => {
                if env.borrow().get(&ident.value).is_none() {
                    return Rc::new(Object::Error(format!(
                        "identifier not found: {}",
                        ident.value
                    )));
                }
                // A variable's type is mutable only if it has no type constraint
                Rc::new(Object::Boolean(
                    env.borrow().get_type_constraint(&ident.value).is_none(),
                ))
            }
            Expression::DotAccess { .. } => {
                // Struct fields have locked types
                Rc::new(Object::Boolean(false))
            }
            _ => Rc::new(Object::Error(
                "argument to `is_type_mut` must be a variable name".to_string(),
            )),
        }
    }

    fn eval_is_mut(&self, args: &[Expression], env: Rc<RefCell<Environment>>) -> Rc<Object> {
        if args.len() != 1 {
            return Rc::new(Object::Error(format!(
                "wrong number of arguments. got={}, want=1",
                args.len()
            )));
        }
        match &args[0] {
            Expression::Ident(ident) => {
                if env.borrow().get(&ident.value).is_none() {
                    return Rc::new(Object::Error(format!(
                        "identifier not found: {}",
                        ident.value
                    )));
                }
                Rc::new(Object::Boolean(!env.borrow().is_immutable(&ident.value)))
            }
            Expression::DotAccess { .. } => {
                // Struct fields are always mutable
                Rc::new(Object::Boolean(true))
            }
            _ => Rc::new(Object::Error(
                "argument to `is_mut` must be a variable name".to_string(),
            )),
        }
    }

    fn eval_if(
        &mut self,
        condition: &Expression,
        consequence: &[Statement],
        alternative: &Option<Vec<Statement>>,
        env: Rc<RefCell<Environment>>,
    ) -> Rc<Object> {
        let cond_val = self.eval_expression(condition, Rc::clone(&env));
        if cond_val.is_error() {
            return cond_val;
        }

        if cond_val.is_truthy() {
            self.eval_block(consequence, env)
        } else if let Some(alt) = alternative {
            self.eval_block(alt, env)
        } else {
            Rc::new(Object::None)
        }
    }
}

#[cfg(test)]
mod tests;
