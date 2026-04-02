use tower_lsp::lsp_types::*;

use oxigen_core::lexer::Lexer;
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
    ("if", "Conditional branch"),
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

const STDLIB_MODULES: &[(&str, &str)] = &[
    ("math", "Mathematical functions (abs, min, max, pow, sqrt, etc.)"),
    ("strings", "String manipulation (split, join, trim, upper, lower, etc.)"),
    ("array", "Array functions (map, filter, reduce, reverse, zip, etc.)"),
    ("io", "File I/O operations (read_file, write_file, input, etc.)"),
    ("os", "OS operations (exec, env, cwd, mkdir, etc.)"),
    ("time", "Time functions (now, sleep, monotonic)"),
    ("random", "Random number generation (rand_int, rand_float)"),
    ("path", "Path manipulation (join, ext, filename, parent, etc.)"),
    ("json", "JSON parsing and stringification"),
    ("net", "HTTP client with HTTPS support"),
];

pub fn get_completions(source: &str, position: Position) -> Vec<CompletionItem> {
    let context = get_completion_context(source, position);

    match context {
        CompletionContext::AfterIntroduce => module_completions(),
        CompletionContext::TypeAnnotation => type_completions(),
        CompletionContext::General => general_completions(),
    }
}

enum CompletionContext {
    AfterIntroduce,
    TypeAnnotation,
    General,
}

fn get_completion_context(source: &str, position: Position) -> CompletionContext {
    // Get text up to cursor position
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

    // Check if we're after `introduce` or `intro` — lex to find preceding tokens
    let trimmed = text_before_cursor.trim();
    if trimmed == "introduce" || trimmed == "intro"
        || trimmed.ends_with("introduce ") || trimmed.ends_with("intro ")
    {
        return CompletionContext::AfterIntroduce;
    }

    // Check if we might be in a type annotation (after `<`)
    // Look for an unclosed `<` that suggests type context
    let lexer = Lexer::new(text_before_cursor);
    let mut last_was_lt = false;
    let mut tokens = Vec::new();
    let mut lex = lexer;
    loop {
        let tok = lex.next_token();
        if tok.token_type == TokenType::Eof {
            break;
        }
        last_was_lt = tok.token_type == TokenType::Lt;
        tokens.push(tok);
    }

    if last_was_lt {
        return CompletionContext::TypeAnnotation;
    }

    CompletionContext::General
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
    STDLIB_MODULES
        .iter()
        .map(|(name, detail)| CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::MODULE),
            detail: Some(detail.to_string()),
            ..Default::default()
        })
        .collect()
}
