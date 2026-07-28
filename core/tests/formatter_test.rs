use oxigen_core::formatter::Formatter;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;

fn format_source(source: &str) -> String {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "Parse errors: {:?}",
        parser.errors()
    );
    Formatter::format(&program)
}

#[test]
fn test_format_variable_declarations() {
    let input = "x:=10\ny  <int>  :=  20\nz <str>";
    let expected = "x := 10\ny <int> := 20\nz <str>\n";
    assert_eq!(format_source(input), expected);
}

#[test]
fn test_format_function() {
    let input = "fun add(a <int>,b <int>) {\na+b\n}";
    let expected = "fun add(a <int>, b <int>) { a + b }\n";
    assert_eq!(format_source(input), expected);
}

#[test]
fn test_format_struct() {
    let input = "struct Point {\nx <int>\ny <int>\n}";
    let expected = "struct Point {\n    x <int>\n    y <int>\n}\n";
    assert_eq!(format_source(input), expected);
}

#[test]
fn test_format_each_loop() {
    let input = "each item in items {\nprintln(item)\n}";
    let expected = "each item in items {\n    println(item)\n}\n";
    assert_eq!(format_source(input), expected);
}

#[test]
fn test_format_introduce() {
    let input = "introduce math";
    let expected = "introduce math\n";
    assert_eq!(format_source(input), expected);
}

#[test]
fn test_format_selective_import() {
    let input = "introduce {upper,lower} from strings";
    let expected = "introduce {upper, lower} from strings\n";
    assert_eq!(format_source(input), expected);
}

#[test]
fn test_format_pattern() {
    let input = "pattern is_even(x) when x%2==0";
    let expected = "pattern is_even(x) when x % 2 == 0\n";
    assert_eq!(format_source(input), expected);
}

#[test]
fn test_format_string_interpolation() {
    let input = "println(\"Hello, {name}!\")";
    let expected = "println(\"Hello, {name}!\")\n";
    assert_eq!(format_source(input), expected);
}

#[test]
fn test_format_struct_inheritance() {
    let input = "struct Dog(Animal) {\nbreed <str>\n}";
    let expected = "struct Dog(Animal) {\n    breed <str>\n}\n";
    assert_eq!(format_source(input), expected);
}

#[test]
fn test_format_array_literal() {
    let input = "[1,2,3,4]";
    let expected = "[1, 2, 3, 4]\n";
    assert_eq!(format_source(input), expected);
}

#[test]
fn test_format_long_array_multiline() {
    let input = "[100000, 200000, 300000, 400000, 500000, 600000, 700000, 800000, 900000, 1000000]";
    let expected = "[\n    100000,\n    200000,\n    300000,\n    400000,\n    500000,\n    600000,\n    700000,\n    800000,\n    900000,\n    1000000,\n]\n";
    assert_eq!(format_source(input), expected);
}

#[test]
fn test_format_empty_array() {
    let input = "[]";
    let expected = "[]\n";
    assert_eq!(format_source(input), expected);
}

#[test]
fn test_format_long_array_idempotent() {
    let input = "[100000, 200000, 300000, 400000, 500000, 600000, 700000, 800000, 900000, 1000000]";
    let first = format_source(input);
    let second = format_source(&first);
    assert_eq!(first, second, "Long array formatting should be idempotent");
}

#[test]
fn test_format_array_with_prefix_column() {
    let input = "long_variable_name_here := [100000, 200000, 300000, 400000, 500000, 600000]";
    let result = format_source(input);
    // The array should fold because prefix + inline width > 80
    assert!(
        result.contains('\n'),
        "Array after long prefix should fold to multi-line"
    );
}

#[test]
fn test_format_idempotent() {
    let input = "x <int> := 10\nname <str> := \"hello\"\n";
    let first = format_source(input);
    let second = format_source(&first);
    assert_eq!(first, second, "Formatter should be idempotent");
}

#[test]
fn test_format_unless_statement() {
    let input = "unless valid { 42 }";
    let expected = "42 unless valid\n";
    assert_eq!(format_source(input), expected);
}

#[test]
fn test_format_postfix_unless_guard() {
    let input = "println(\"ok\") unless debug == True";
    let expected = "println(\"ok\") unless debug == True\n";
    assert_eq!(format_source(input), expected);
}

#[test]
fn test_format_unless_block_multi_statement() {
    let input = "unless valid { x = 1\ny = 2 }";
    let expected = "unless valid {\n    x = 1\n    y = 2\n}\n";
    assert_eq!(format_source(input), expected);
}

#[test]
fn test_format_postfix_when_guard() {
    let input = "println(\"ok\") when ready";
    let expected = "println(\"ok\") when ready\n";
    assert_eq!(format_source(input), expected);
}

#[test]
fn test_format_postfix_unless_then_guard() {
    let input = "println(\"ok\") unless x == False then println(\"fallback\")";
    let expected = "println(\"ok\") unless x == False then println(\"fallback\")\n";
    assert_eq!(format_source(input), expected);
}

#[test]
fn test_format_log_with_nested_tags() {
    let input = "<log<Error<network>>>(\"connection lost\")";
    let expected = "<log<Error<network>>>(\"connection lost\")\n";
    assert_eq!(format_source(input), expected);
}

#[test]
fn test_format_error_constructor() {
    let input = "err <Error> = <Error<retry_error>>(\"bad input\")";
    let expected = "err <Error> = <Error<retry_error>>(\"bad input\")\n";
    assert_eq!(format_source(input), expected);
}

#[test]
fn test_format_value_constructor() {
    let input = "result := <Value>(\"ok\")";
    let expected = "result := <Value>(\"ok\")\n";
    assert_eq!(format_source(input), expected);
}

#[test]
fn test_format_top_level_sections() {
    let input = "introduce os\nintroduce strings\nx:=1\ny:=2\nfun first(){x}\nfun second(){y}";
    let expected = "introduce os\nintroduce strings\n\nx := 1\ny := 2\n\nfun first() { x }\n\nfun second() { y }\n";
    assert_eq!(format_source(input), expected);
}

#[test]
fn test_format_enum_unit_variants() {
    let input = "enum Stoplight {\nRed: 0\nYellow: 1\nGreen: 2\n}";
    let expected = "enum Stoplight {\n    Red: 0\n    Yellow: 1\n    Green: 2\n}\n";
    assert_eq!(format_source(input), expected);
}

#[test]
fn test_format_enum_auto_variants() {
    let input = "enum Direction { North, East, South, West }";
    let expected = "enum Direction {\n    North\n    East\n    South\n    West\n}\n";
    assert_eq!(format_source(input), expected);
}

#[test]
fn test_format_enum_payload_variants() {
    let input = "enum Shape {\nCircle(radius <float>)\nRectangle { w <float>, h <float> }\nUnit\n}";
    let expected = "enum Shape {\n    Circle(radius <float>)\n    Rectangle { w <float>, h <float> }\n    Unit\n}\n";
    assert_eq!(format_source(input), expected);
}

#[test]
fn test_format_enum_variant_construct_struct() {
    let input = "s := Shape.Rectangle{w:3.0,h:4.0}";
    let expected = "s := Shape.Rectangle { w: 3.0, h: 4.0 }\n";
    assert_eq!(format_source(input), expected);
}

// ── fmt must not corrupt the file it formats ────────────────────────────────
// Three ways it used to: dropping every comment, emitting brace blocks into an
// `#[indent]` file, and writing `{:}` for an empty map (which the parser
// rejects). Each left behind a file that no longer parsed.

/// Formats like `oxigen fmt` does: comments restored, block style preserved.
fn format_faithful(source: &str) -> String {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "Parse errors: {:?}",
        parser.errors()
    );
    let indent_style = source.lines().any(|l| l.trim() == "#[indent]");
    Formatter::format_source(&program, parser.comments(), indent_style)
}

/// Asserts the formatted output still parses, and that formatting is a fixed
/// point (running fmt twice changes nothing).
fn assert_reparses_and_stable(source: &str, formatted: &str) {
    let header: String = source
        .lines()
        .take_while(|l| l.trim() == "#[indent]")
        .map(|l| format!("{l}\n"))
        .collect();
    let round_tripped = format!("{header}{formatted}");

    let lexer = Lexer::new(&round_tripped);
    let mut parser = Parser::new(lexer, &round_tripped);
    parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "formatted output does not parse: {:?}\n--- output ---\n{round_tripped}",
        parser.errors()
    );
    assert_eq!(
        format_faithful(&round_tripped),
        formatted,
        "fmt is not idempotent"
    );
}

#[test]
fn fmt_preserves_comments() {
    let input = "// leading comment\nfun add(a, b) {\n// inner comment\na + b // trailing comment\n}\n";
    let out = format_faithful(input);
    assert!(out.contains("// leading comment"), "{out}");
    assert!(out.contains("// inner comment"), "{out}");
    assert!(out.contains("// trailing comment"), "{out}");
    assert_reparses_and_stable(input, &out);
}

#[test]
fn fmt_keeps_trailing_comment_on_its_statement() {
    let out = format_faithful("main {\nx := 1 // why one\ny := 2\n}\n");
    assert!(out.contains("x := 1  // why one"), "{out}");
}

#[test]
fn fmt_preserves_block_comments_and_trailing_file_comments() {
    let input = "/* block header */\nx := 1\n// last word\n";
    let out = format_faithful(input);
    assert!(out.contains("/* block header */"), "{out}");
    assert!(out.contains("// last word"), "{out}");
}

#[test]
fn fmt_emits_indent_style_for_indent_files() {
    let input = "#[indent]\nfun add(a, b):\n  // inner\n  a + b\n\nmain:\n  println(add(1, 2))\n";
    let out = format_faithful(input);
    assert!(!out.contains('{'), "indent file must not gain braces:\n{out}");
    assert!(!out.contains('}'), "indent file must not gain braces:\n{out}");
    assert!(out.contains("fun add(a, b):"), "{out}");
    assert!(out.contains("// inner"), "{out}");
    assert_reparses_and_stable(input, &out);
}

#[test]
fn fmt_emits_indent_style_for_nested_blocks() {
    let input = "#[indent]\nmain:\n  each i in [1, 2]:\n    repeat when i > 9:\n      stop\n    println(i)\n";
    let out = format_faithful(input);
    assert!(!out.contains('{') && !out.contains('}'), "{out}");
    assert_reparses_and_stable(input, &out);
}

#[test]
fn fmt_writes_reparsable_empty_map() {
    // `{:}` is rejected by the parser in every position, so fmt must not write it.
    let input = "fun f(h <map> = {}) { len(h) }\n";
    let out = format_faithful(input);
    assert!(!out.contains("{:}"), "{out}");
    assert_reparses_and_stable(input, &out);
}

#[test]
fn fmt_leaves_brace_style_files_alone() {
    let input = "main {\n    x := 1\n}\n";
    let out = format_faithful(input);
    assert!(out.contains('{') && out.contains('}'), "{out}");
    assert_reparses_and_stable(input, &out);
}

// A comment after a block's last statement belongs inside that block. There is
// no span for a `}` in the AST, so placement is decided by the next sibling
// statement's line plus the comment's own column.

/// Index of the line containing `needle`, for asserting relative order.
fn line_of(haystack: &str, needle: &str) -> usize {
    haystack
        .lines()
        .position(|l| l.contains(needle))
        .unwrap_or_else(|| panic!("{needle:?} missing from:\n{haystack}"))
}

#[test]
fn fmt_keeps_end_of_block_comment_inside_the_block() {
    let out = format_faithful("main {\nx := 1\n  // end of block\n}\n\ny := 2\n");
    assert!(line_of(&out, "// end of block") < line_of(&out, "}"), "{out}");
}

#[test]
fn fmt_treats_a_margin_comment_at_a_block_boundary_as_outside() {
    // These two sources differ only in which side of `}` the comment sits on,
    // and the AST records no position for `}` — so they necessarily format
    // alike. Attaching to the outer level matches the comment's own column.
    let inside = format_faithful("main {\nx := 1\n// boundary\n}\n\ny := 2\n");
    let outside = format_faithful("main {\nx := 1\n}\n// boundary\n\ny := 2\n");
    assert_eq!(inside, outside);
    assert!(line_of(&inside, "}") < line_of(&inside, "// boundary"), "{inside}");
}

#[test]
fn fmt_keeps_end_of_block_comment_inside_the_files_last_block() {
    // Nothing follows the block, so the line bound alone cannot place this —
    // the comment's indentation is what marks it as inside.
    let out = format_faithful("y := 2\n\nmain {\nx := 1\n  // end of block\n}\n");
    assert!(line_of(&out, "// end of block") < line_of(&out, "}"), "{out}");
}

#[test]
fn fmt_keeps_a_margin_comment_after_the_final_block_outside_it() {
    let out = format_faithful("main {\nx := 1\n}\n// file trailer\n");
    assert!(line_of(&out, "}") < line_of(&out, "// file trailer"), "{out}");
}

#[test]
fn fmt_places_nested_end_of_block_comments_in_the_innermost_block() {
    let src = "main {\n    each i in [1, 2] {\n        println(i)\n        // inner end\n    }\n    // outer end\n}\n\n// file trailer\n";
    let out = format_faithful(src);
    let (inner, outer) = (line_of(&out, "// inner end"), line_of(&out, "// outer end"));
    let close_each = line_of(&out, "    }");
    let close_main = out
        .lines()
        .position(|l| l == "}")
        .unwrap_or_else(|| panic!("no top-level close in:\n{out}"));
    assert!(inner < close_each, "inner comment escaped its block:\n{out}");
    assert!(close_each < outer && outer < close_main, "{out}");
    assert!(close_main < line_of(&out, "// file trailer"), "{out}");
    assert_reparses_and_stable(src, &out);
}

#[test]
fn fmt_keeps_end_of_block_comment_inside_indent_style_blocks() {
    let src = "#[indent]\nmain:\n  x := 1\n  // end of block\n";
    let out = format_faithful(src);
    assert!(out.contains("// end of block"), "{out}");
    assert_reparses_and_stable(src, &out);
}

// Methods, struct fields, enum variants and match arms are not statements, so
// they need their own comment flush — without it a comment written above one of
// them was swallowed into the previous item's body, and a second fmt pass then
// moved it again (formatting was not idempotent).

#[test]
fn fmt_keeps_comments_above_includes_methods() {
    let src = "struct S {\n    n <int>\n}\n\nS includes {\n    // doubles n\n    fun double() { self.n * 2 }\n\n    // triples n\n    fun triple() { self.n * 3 }\n}\n";
    let out = format_faithful(src);
    assert!(
        line_of(&out, "// doubles n") < line_of(&out, "fun double"),
        "{out}"
    );
    assert!(
        line_of(&out, "// triples n") < line_of(&out, "fun triple"),
        "{out}"
    );
    assert_reparses_and_stable(src, &out);
}

#[test]
fn fmt_keeps_comments_above_struct_fields_and_enum_variants() {
    let src = "struct Point {\n    // horizontal\n    x <int>\n    // vertical\n    y <int>\n}\n\nenum Color {\n    // warm\n    Red\n    // cool\n    Blue\n}\n";
    let out = format_faithful(src);
    for (comment, item) in [
        ("// horizontal", "x <int>"),
        ("// vertical", "y <int>"),
        ("// warm", "Red"),
        ("// cool", "Blue"),
    ] {
        assert!(line_of(&out, comment) < line_of(&out, item), "{out}");
    }
    assert_reparses_and_stable(src, &out);
}

#[test]
fn fmt_keeps_comments_above_option_arms() {
    let src = "fun pick(n) {\n    option {\n        // small\n        n < 10 -> \"small\"\n        // big\n        \"big\"\n    }\n}\n";
    let out = format_faithful(src);
    assert!(line_of(&out, "// small") < line_of(&out, "n < 10"), "{out}");
    assert!(out.contains("// big"), "{out}");
    assert_reparses_and_stable(src, &out);
}

// ── string interpolation ────────────────────────────────────────────────────
// The expression inside `{ ... }` used to be scanned by a hand-written lexer
// that accepted only identifiers, numbers, strings and `( ) , + - * / . [ ]`,
// so every comparison, `%`, and logical keyword was a syntax error even though
// the parser behind it handled them. It now runs the real lexer.

/// Parses a source string, asserting it has no parse errors.
fn parse_ok_source(source: &str) {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "unexpected parse errors for {source:?}:\n{}",
        parser.format_errors()
    );
}

#[test]
fn interpolation_accepts_operators_the_parser_supports() {
    for expr in [
        "a == b", "a != b", "a < b", "a <= b", "a > b", "a >= b", "x % 2",
        "a and b", "a or b", "not a", "-a", "a + b * 2", "(a + b) * 2",
        "f(a, b)", "items[0]", "p.n", "p.m()", "a == b or x % 2 == 0",
    ] {
        parse_ok_source(&format!("println(\"{{{expr}}}\")"));
    }
}

#[test]
fn interpolation_handles_nested_braces_and_strings() {
    // A map literal inside the interpolation must not be mistaken for the
    // closing brace, and a nested string literal must lex as one.
    parse_ok_source("println(\"{ {\"k\": 7}[\"k\"] }\")");
    parse_ok_source("println(\"{ upper(\"hi\") }\")");
}

#[test]
fn interpolated_expressions_survive_formatting() {
    // The formatter reprints the expression; it must still parse afterwards.
    let src = "println(\"{a == b} {x % 2} {a < b and b > 0} {items[0]} {(a + b) * 2}\")\n";
    let formatted = format_source(src);
    parse_ok_source(&formatted);
    assert_eq!(format_source(&formatted), formatted, "fmt is not idempotent");
    assert!(formatted.contains("{a == b}"), "{formatted}");
    assert!(formatted.contains("{x % 2}"), "{formatted}");
}

#[test]
fn escaped_braces_are_literal_and_round_trip() {
    // `\{` is the only way to write a brace that does not open an
    // interpolation. It must survive fmt: emitting a bare `{` back out would
    // turn the rest of the string into a live interpolation.
    let src = "println(\"literal \\{x\\} here\")\n";
    let formatted = format_source(src);
    parse_ok_source(&formatted);
    assert!(!formatted.contains("\\\\{"), "double-escaped: {formatted}");
    assert!(formatted.contains("\\{"), "brace not escaped: {formatted}");
    assert_eq!(format_source(&formatted), formatted, "fmt is not idempotent");
}
