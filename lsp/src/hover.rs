use tower_lsp::lsp_types::*;

pub fn get_hover(source: &str, position: Position) -> Option<Hover> {
    let word = get_word_at_position(source, position)?;
    let info = get_hover_info(&word)?;

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: info,
        }),
        range: None,
    })
}

fn get_word_at_position(source: &str, position: Position) -> Option<String> {
    let lines: Vec<&str> = source.lines().collect();
    let line_idx = position.line as usize;
    if line_idx >= lines.len() {
        return None;
    }

    let line = lines[line_idx];
    let col = position.character as usize;
    if col > line.len() {
        return None;
    }

    let bytes = line.as_bytes();

    // Find word boundaries
    let mut start = col;
    while start > 0 && is_ident_char(bytes[start - 1]) {
        start -= 1;
    }

    let mut end = col;
    while end < bytes.len() && is_ident_char(bytes[end]) {
        end += 1;
    }

    if start == end {
        return None;
    }

    Some(line[start..end].to_string())
}

fn is_ident_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

fn get_hover_info(word: &str) -> Option<String> {
    // Check keywords
    if let Some(info) = keyword_hover(word) {
        return Some(info);
    }

    // Check builtins
    if let Some(info) = builtin_hover(word) {
        return Some(info);
    }

    // Check type names
    if let Some(info) = type_hover(word) {
        return Some(info);
    }

    None
}

fn keyword_hover(word: &str) -> Option<String> {
    let info = match word {
        "fun" => "**fun** — Define a function\n\n```oxigen\nfun name(param <type>) {\n    body\n}\n```",
        "struct" => "**struct** — Define a composite data type\n\n```oxigen\nstruct Name {\n    field <type>\n}\n```",
        "contains" => "**contains** — Define methods for a struct\n\n```oxigen\ncontains StructName {\n    fun method(self) { body }\n}\n```",
        "introduce" | "intro" => "**introduce** — Import a module\n\n```oxigen\nintroduce math\nintroduce {split, join} from strings\n```",
        "each" => "**each** — Iterate over a collection\n\n```oxigen\neach item in collection {\n    body\n}\n```",
        "repeat" => "**repeat** — Loop while condition is true\n\n```oxigen\nrepeat condition {\n    body\n}\n```",
        "option" => "**option** — Multi-arm conditional expression\n\n```oxigen\noption {\n    condition -> result\n    else -> default\n}\n```",
        "unless" => "**unless** — Inverse conditional\n\n```oxigen\nvalue unless condition then alternative\n```",
        "choose" => "**choose** — Pattern matching\n\n```oxigen\nchoose value {\n    pattern_name -> result\n    else -> default\n}\n```",
        "pattern" => "**pattern** — Define a named pattern for use with choose\n\n```oxigen\npattern name(x) = condition\n```",
        "guard" => "**guard** — Error recovery expression\n\n```oxigen\nresult guard err -> fallback\n```",
        "fail" => "**fail** — Propagate an error value up the call stack\n\n```oxigen\nfail expression\n```",
        "give" => "**give** — Return a value from a function\n\n```oxigen\ngive value\n```",
        "skip" => "**skip** — Skip to next loop iteration (continue)",
        "stop" => "**stop** — Break out of a loop",
        "when" => "**when** — Postfix conditional guard\n\n```oxigen\nexpression when condition\n```",
        "and" => "**and** — Logical AND operator",
        "or" => "**or** — Logical OR operator",
        "not" => "**not** — Logical negation operator",
        "in" => "**in** — Membership test or iteration keyword",
        "as" => "**as** — Type conversion\n\n```oxigen\nvalue as <type>\n```",
        "hide" => "**hide** — Mark a struct field as private",
        "self" => "**self** — Reference to the current struct instance in methods",
        "True" => "**True** — Boolean true literal",
        "False" => "**False** — Boolean false literal",
        "None" => "**None** — Represents the absence of a value",
        "from" => "**from** — Selective import from a module\n\n```oxigen\nintroduce {name} from module\n```",
        "then" => "**then** — Consequence branch in postfix conditionals",
        _ => return None,
    };
    Some(info.to_string())
}

fn builtin_hover(word: &str) -> Option<String> {
    let info = match word {
        "print" => "**print**(...args)\n\nPrint values to stdout separated by spaces.",
        "println" => "**println**(...args)\n\nPrint values to stdout with a trailing newline.",
        "len" => "**len**(collection) -> int\n\nReturn the length of a string, array, map, tuple, or set.",
        "push" => "**push**(array, value) -> array\n\nAppend a value to the end of an array.",
        "first" => "**first**(array) -> value\n\nReturn the first element of an array.",
        "last" => "**last**(array) -> value\n\nReturn the last element of an array.",
        "rest" => "**rest**(array) -> array\n\nReturn all elements except the first.",
        "type" => "**type**(value) -> str\n\nReturn the type name of a value as a string.",
        "ord" => "**ord**(char) -> int\n\nReturn the Unicode codepoint of a character.",
        "chr" => "**chr**(int) -> char\n\nReturn the character for a Unicode codepoint.",
        "str" => "**str**(value) -> str\n\nConvert any value to its string representation.",
        "int" => "**int**(value) -> int\n\nConvert a value to an integer.",
        "float" => "**float**(value) -> float\n\nConvert a value to a float.",
        "range" => "**range**(start, end) -> array\n\nGenerate an array of integers from start to end (exclusive).",
        "chars" => "**chars**(string) -> array\n\nSplit a string into an array of individual characters.",
        "byte" => "**byte**(value) -> byte\n\nConvert a value to a byte (u8).",
        "uint" => "**uint**(value) -> uint\n\nConvert a value to an unsigned integer.",
        "set" => "**set**(array) -> set\n\nCreate a set from an array, removing duplicates.",
        "keys" => "**keys**(map) -> array\n\nReturn the keys of a map as an array.",
        "values" => "**values**(map) -> array\n\nReturn the values of a map as an array.",
        "insert" => "**insert**(map, key, value) -> map\n\nInsert a key-value pair into a map.",
        "remove" => "**remove**(collection, item) -> collection\n\nRemove an item from a collection.",
        "has" => "**has**(collection, item) -> bool\n\nCheck if a collection contains an item.",
        "tuple" => "**tuple**(...values) -> tuple\n\nCreate a tuple from the given values.",
        "error" => "**error**(message) -> error\n\nCreate an error value with the given message.",
        "is_value" => "**is_value**(obj) -> bool\n\nCheck if an object is a Value wrapper.",
        "is_error" => "**is_error**(obj) -> bool\n\nCheck if an object is an error value.",
        _ => return None,
    };
    Some(info.to_string())
}

fn type_hover(word: &str) -> Option<String> {
    let info = match word {
        // Only trigger for type names when they appear as type annotations
        // These overlap with builtins (int, float, str, etc.) but provide type context
        "array" => "**array** — Dynamic ordered collection type\n\nLiteral: `[1, 2, 3]`",
        "tuple" => "**tuple** — Fixed-size immutable collection type\n\nLiteral: `(1, 2, 3)`",
        "map" => "**map** — Key-value pair collection type\n\nLiteral: `{\"key\": value}`",
        "bool" => "**bool** — Boolean type (`True` or `False`)",
        "generic" => "**generic** — Dynamic type that accepts any value",
        _ => return None,
    };
    Some(info.to_string())
}
