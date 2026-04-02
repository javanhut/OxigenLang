use std::collections::HashMap;
use std::fs;
use std::sync::OnceLock;

use tower_lsp::lsp_types::*;

use oxigen_core::ast::{Expression, Statement, TypedParam};
use oxigen_core::evaluator::find_stdlib_path;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::token::TokenType;

const KEYWORDS: &[(&str, &str)] = &[
    ("fun", "Define a function"),
    ("struct", "Define a struct"),
    ("contains", "Define methods for a struct"),
    ("introduce", "Import a module"),
    ("intro", "Import a module (shorthand)"),
    ("from", "Selective import from a module"),
    ("each", "Iterate over a collection"),
    ("repeat", "Loop while condition is true"),
    ("option", "Multi-arm conditional expression"),
    ("unless", "Inverse conditional expression"),
    ("choose", "Pattern matching expression"),
    ("pattern", "Define a named pattern"),
    ("when", "Postfix conditional guard"),
    ("guard", "Error recovery expression"),
    ("fail", "Propagate an error"),
    ("give", "Return a value from a function"),
    ("skip", "Skip to next loop iteration"),
    ("stop", "Break out of a loop"),
    ("in", "Membership / iteration keyword"),
    ("not", "Logical negation"),
    ("and", "Logical AND"),
    ("or", "Logical OR"),
    ("then", "Consequence in postfix conditional"),
    ("as", "Type conversion"),
    ("hide", "Mark struct field as private"),
    ("self", "Reference to current struct instance"),
    ("True", "Boolean true literal"),
    ("False", "Boolean false literal"),
    ("None", "Absence of a value"),
];

const BUILTINS: &[(&str, &str)] = &[
    ("print", "print(...args) — Print values to stdout"),
    ("println", "println(...args) — Print values with newline"),
    ("len", "len(collection) — Get length of a collection"),
    ("push", "push(array, value) — Append to an array"),
    ("first", "first(array) — Get first element"),
    ("last", "last(array) — Get last element"),
    ("rest", "rest(array) — Get all but first element"),
    ("type", "type(value) — Get the type name of a value"),
    ("ord", "ord(char) — Get Unicode codepoint of a character"),
    ("chr", "chr(int) — Get character from Unicode codepoint"),
    ("str", "str(value) — Convert to string"),
    ("int", "int(value) — Convert to integer"),
    ("float", "float(value) — Convert to float"),
    ("range", "range(start, end) — Generate integer range"),
    ("chars", "chars(string) — Split string into char array"),
    ("byte", "byte(value) — Convert to byte"),
    ("uint", "uint(value) — Convert to unsigned integer"),
    ("set", "set(array) — Create a set from an array"),
    ("keys", "keys(map) — Get keys of a map"),
    ("values", "values(map) — Get values of a map"),
    ("insert", "insert(map, key, value) — Insert into a map"),
    ("remove", "remove(collection, item) — Remove from collection"),
    ("has", "has(collection, item) — Check membership"),
    ("tuple", "tuple(...values) — Create a tuple"),
    ("error", "error(message) — Create an error value"),
    ("is_value", "is_value(obj) — Check if obj is a Value wrapper"),
    ("is_error", "is_error(obj) — Check if obj is an error"),
];

const TYPE_NAMES: &[(&str, &str)] = &[
    ("int", "64-bit signed integer"),
    ("str", "Unicode string"),
    ("float", "64-bit floating point"),
    ("char", "Single Unicode character"),
    ("bool", "Boolean (True/False)"),
    ("array", "Dynamic ordered collection"),
    ("byte", "Unsigned 8-bit integer"),
    ("uint", "Unsigned 64-bit integer"),
    ("tuple", "Fixed-size immutable collection"),
    ("map", "Key-value pairs"),
    ("set", "Unique unordered collection"),
    ("generic", "Dynamic type (any)"),
];

// ── Stdlib discovery ──

struct FunctionInfo {
    name: String,
    signature: String,
}

struct ModuleInfo {
    name: String,
    functions: Vec<FunctionInfo>,
}

fn format_signature(name: &str, params: &[TypedParam]) -> String {
    let params_str: Vec<String> = params
        .iter()
        .map(|p| match &p.type_ann {
            Some(ta) => format!("{} <{}>", p.ident.value, ta.type_name().to_lowercase()),
            None => p.ident.value.clone(),
        })
        .collect();
    format!("{}({})", name, params_str.join(", "))
}

fn discover_stdlib() -> Vec<ModuleInfo> {
    let stdlib_path = find_stdlib_path();
    let mut modules = Vec::new();

    let entries = match fs::read_dir(&stdlib_path) {
        Ok(entries) => entries,
        Err(_) => return modules,
    };

    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("oxi") {
            continue;
        }
        let module_name = match path.file_stem().and_then(|s| s.to_str()) {
            Some(name) => name.to_string(),
            None => continue,
        };

        let source = match fs::read_to_string(&path) {
            Ok(s) => s,
            Err(_) => continue,
        };

        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer, &source);
        let program = parser.parse_program();

        let mut functions = Vec::new();
        for stmt in &program.statements {
            if let Statement::Let {
                name,
                value: Expression::FunctionLiteral { parameters, .. },
            } = stmt
            {
                let sig = format_signature(&name.value, parameters);
                functions.push(FunctionInfo {
                    name: name.value.clone(),
                    signature: sig,
                });
            }
        }

        modules.push(ModuleInfo {
            name: module_name,
            functions,
        });
    }

    modules.sort_by(|a, b| a.name.cmp(&b.name));
    modules
}

static STDLIB_DB: OnceLock<Vec<ModuleInfo>> = OnceLock::new();

fn get_stdlib() -> &'static Vec<ModuleInfo> {
    STDLIB_DB.get_or_init(discover_stdlib)
}

fn stdlib_function_map() -> &'static HashMap<String, Vec<(String, String)>> {
    static MAP: OnceLock<HashMap<String, Vec<(String, String)>>> = OnceLock::new();
    MAP.get_or_init(|| {
        let mut map = HashMap::new();
        for module in get_stdlib() {
            let funcs: Vec<(String, String)> = module
                .functions
                .iter()
                .map(|f| (f.name.clone(), f.signature.clone()))
                .collect();
            map.insert(module.name.clone(), funcs);
        }
        map
    })
}

// ── Completion context ──

enum CompletionContext {
    AfterIntroduce,
    AfterModuleDot(String),
    TypeAnnotation,
    General,
}

fn get_completion_context(source: &str, position: Position) -> CompletionContext {
    let lines: Vec<&str> = source.lines().collect();
    let line_idx = position.line as usize;
    if line_idx >= lines.len() {
        return CompletionContext::General;
    }

    let col = position.character as usize;
    let line_text = lines[line_idx];
    let text_before_cursor = if col <= line_text.len() {
        &line_text[..col]
    } else {
        line_text
    };

    // Check if we're after `introduce` or `intro`
    let trimmed = text_before_cursor.trim();
    if trimmed == "introduce"
        || trimmed == "intro"
        || trimmed.ends_with("introduce ")
        || trimmed.ends_with("intro ")
    {
        return CompletionContext::AfterIntroduce;
    }

    // Check for module.member access — extract the word before the last `.`
    if let Some(dot_pos) = trimmed.rfind('.') {
        let before_dot = &trimmed[..dot_pos];
        let module_name: String = before_dot
            .chars()
            .rev()
            .take_while(|c| c.is_alphanumeric() || *c == '_')
            .collect::<String>()
            .chars()
            .rev()
            .collect();
        if !module_name.is_empty() && stdlib_function_map().contains_key(&module_name) {
            return CompletionContext::AfterModuleDot(module_name);
        }
    }

    // Check for type annotation (after `<`)
    let lexer = Lexer::new(text_before_cursor);
    let mut last_was_lt = false;
    let mut lex = lexer;
    loop {
        let tok = lex.next_token();
        if tok.token_type == TokenType::Eof {
            break;
        }
        last_was_lt = tok.token_type == TokenType::Lt;
    }

    if last_was_lt {
        return CompletionContext::TypeAnnotation;
    }

    CompletionContext::General
}

// ── Completion generators ──

pub fn get_completions(source: &str, position: Position) -> Vec<CompletionItem> {
    let context = get_completion_context(source, position);

    match context {
        CompletionContext::AfterIntroduce => module_completions(),
        CompletionContext::AfterModuleDot(module) => module_function_completions(&module),
        CompletionContext::TypeAnnotation => type_completions(),
        CompletionContext::General => general_completions(),
    }
}

fn general_completions() -> Vec<CompletionItem> {
    let mut items = Vec::new();

    for (name, detail) in KEYWORDS {
        items.push(CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some(detail.to_string()),
            ..Default::default()
        });
    }

    for (name, detail) in BUILTINS {
        items.push(CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some(detail.to_string()),
            ..Default::default()
        });
    }

    items
}

fn type_completions() -> Vec<CompletionItem> {
    TYPE_NAMES
        .iter()
        .map(|(name, detail)| CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::TYPE_PARAMETER),
            detail: Some(detail.to_string()),
            ..Default::default()
        })
        .collect()
}

fn module_completions() -> Vec<CompletionItem> {
    get_stdlib()
        .iter()
        .map(|module| {
            let func_names: Vec<&str> =
                module.functions.iter().map(|f| f.name.as_str()).collect();
            let detail = if func_names.is_empty() {
                module.name.clone()
            } else {
                func_names.join(", ")
            };
            CompletionItem {
                label: module.name.clone(),
                kind: Some(CompletionItemKind::MODULE),
                detail: Some(detail),
                ..Default::default()
            }
        })
        .collect()
}

fn module_function_completions(module_name: &str) -> Vec<CompletionItem> {
    let map = stdlib_function_map();
    let Some(funcs) = map.get(module_name) else {
        return Vec::new();
    };
    funcs
        .iter()
        .map(|(name, signature)| CompletionItem {
            label: name.clone(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some(signature.clone()),
            ..Default::default()
        })
        .collect()
}
