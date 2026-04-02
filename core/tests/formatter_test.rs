use oxigen_core::formatter::Formatter;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;

fn format_source(source: &str) -> String {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(parser.errors().is_empty(), "Parse errors: {:?}", parser.errors());
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
fn test_format_idempotent() {
    let input = "x <int> := 10\nname <str> := \"hello\"\n";
    let first = format_source(input);
    let second = format_source(&first);
    assert_eq!(first, second, "Formatter should be idempotent");
}

#[test]
fn test_format_unless_statement() {
    let input = "unless valid { 42 }";
    let expected = "unless valid {\n    42\n}\n";
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
