use super::*;
use crate::lexer::Lexer;
use crate::parser::Parser;

fn test_eval(input: &str) -> Rc<Object> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer, input);
    let program = parser.parse_program();

    if !parser.errors().is_empty() {
        panic!("Parser errors:\n{}", parser.format_errors());
    }

    let env = Rc::new(RefCell::new(Environment::new()));
    let mut evaluator = Evaluator::new();
    evaluator.eval_program(&program, env)
}

fn test_integer(obj: &Object, expected: i64) {
    match obj {
        Object::Integer(val) => assert_eq!(*val, expected),
        _ => panic!("Expected Integer({}), got {:?}", expected, obj),
    }
}

fn test_float(obj: &Object, expected: f64) {
    match obj {
        Object::Float(val) => assert!((val - expected).abs() < f64::EPSILON),
        _ => panic!("Expected Float({}), got {:?}", expected, obj),
    }
}

fn test_boolean(obj: &Object, expected: bool) {
    match obj {
        Object::Boolean(val) => assert_eq!(*val, expected),
        _ => panic!("Expected Boolean({}), got {:?}", expected, obj),
    }
}

fn test_string(obj: &Object, expected: &str) {
    match obj {
        Object::String(val) => assert_eq!(val, expected),
        _ => panic!("Expected String({}), got {:?}", expected, obj),
    }
}

fn test_char(obj: &Object, expected: char) {
    match obj {
        Object::Char(val) => assert_eq!(*val, expected),
        _ => panic!("Expected Char({}), got {:?}", expected, obj),
    }
}

fn test_none(obj: &Object) {
    match obj {
        Object::None => {}
        _ => panic!("Expected None, got {:?}", obj),
    }
}

// ==================== INTEGER TESTS ====================

#[test]
fn test_integer_expressions() {
    let tests = vec![
        ("5", 5),
        ("10", 10),
        ("-5", -5),
        ("-10", -10),
        ("5 + 5", 10),
        ("5 - 5", 0),
        ("5 * 5", 25),
        ("10 / 2", 5),
        ("10 % 3", 1),
        ("2 + 3 * 4", 14),
        ("(2 + 3) * 4", 20),
        ("-50 + 100 + -50", 0),
        ("5 * 2 + 10", 20),
        ("5 + 2 * 10", 25),
        ("50 / 2 * 2 + 10", 60),
    ];

    for (input, expected) in tests {
        let result = test_eval(input);
        test_integer(&result, expected);
    }
}

// ==================== FLOAT TESTS ====================

#[test]
fn test_float_expressions() {
    let tests = vec![
        ("3.14", 3.14),
        ("2.5 + 1.5", 4.0),
        ("10.0 / 4.0", 2.5),
        ("3.14 * 2", 6.28),
        ("2 * 3.14", 6.28),
        ("-3.14", -3.14),
    ];

    for (input, expected) in tests {
        let result = test_eval(input);
        test_float(&result, expected);
    }
}

// ==================== CHAR TESTS ====================

#[test]
fn test_char_expressions() {
    let tests = vec![("`a`", 'a'), ("`Z`", 'Z'), ("`0`", '0'), ("`!`", '!')];

    for (input, expected) in tests {
        let result = test_eval(input);
        test_char(&result, expected);
    }
}

#[test]
fn test_char_comparisons() {
    let tests = vec![
        ("`a` == `a`", true),
        ("`a` == `b`", false),
        ("`a` != `b`", true),
        ("`a` < `b`", true),
        ("`b` > `a`", true),
        ("`a` <= `a`", true),
        ("`a` >= `a`", true),
    ];

    for (input, expected) in tests {
        let result = test_eval(input);
        test_boolean(&result, expected);
    }
}

// ==================== STRING TESTS ====================

#[test]
fn test_string_expressions() {
    let tests = vec![
        ("\"hello\"", "hello"),
        ("'world'", "world"),
        ("\"hello\" + \" \" + \"world\"", "hello world"),
        ("'single' + \" double\"", "single double"),
    ];

    for (input, expected) in tests {
        let result = test_eval(input);
        test_string(&result, expected);
    }
}

#[test]
fn test_string_comparisons() {
    let tests = vec![
        ("\"hello\" == \"hello\"", true),
        ("\"hello\" == \"world\"", false),
        ("\"hello\" != \"world\"", true),
    ];

    for (input, expected) in tests {
        let result = test_eval(input);
        test_boolean(&result, expected);
    }
}

// ==================== BOOLEAN TESTS ====================

#[test]
fn test_boolean_expressions() {
    let tests = vec![
        ("True", true),
        ("False", false),
        ("!True", false),
        ("!False", true),
        ("!!True", true),
        ("True == True", true),
        ("True == False", false),
        ("True != False", true),
    ];

    for (input, expected) in tests {
        let result = test_eval(input);
        test_boolean(&result, expected);
    }
}

// ==================== COMPARISON TESTS ====================

#[test]
fn test_integer_comparisons() {
    let tests = vec![
        ("1 < 2", true),
        ("1 > 2", false),
        ("1 < 1", false),
        ("1 > 1", false),
        ("1 == 1", true),
        ("1 != 1", false),
        ("1 == 2", false),
        ("1 != 2", true),
        ("1 <= 2", true),
        ("2 >= 1", true),
        ("1 <= 1", true),
        ("1 >= 1", true),
    ];

    for (input, expected) in tests {
        let result = test_eval(input);
        test_boolean(&result, expected);
    }
}

// ==================== ARRAY TESTS ====================

#[test]
fn test_array_literals() {
    let result = test_eval("[1, 2, 3]");
    match result.as_ref() {
        Object::Array(elements) => {
            assert_eq!(elements.len(), 3);
            test_integer(&elements[0], 1);
            test_integer(&elements[1], 2);
            test_integer(&elements[2], 3);
        }
        _ => panic!("Expected Array, got {:?}", result),
    }
}

#[test]
fn test_array_index() {
    let tests = vec![
        ("[1, 2, 3][0]", 1),
        ("[1, 2, 3][1]", 2),
        ("[1, 2, 3][2]", 3),
        ("x := [5, 10, 15]\nx[1]", 10),
    ];

    for (input, expected) in tests {
        let result = test_eval(input);
        test_integer(&result, expected);
    }
}

#[test]
fn test_array_out_of_bounds() {
    let result = test_eval("[1, 2, 3][10]");
    test_none(&result);
}

// ==================== LET STATEMENT TESTS ====================

#[test]
fn test_let_statements() {
    let tests = vec![
        ("x := 5\nx", 5),
        ("x := 5 * 5\nx", 25),
        ("x := 5\ny := x\ny", 5),
        ("x := 5\ny := x\nz := x + y\nz", 10),
    ];

    for (input, expected) in tests {
        let result = test_eval(input);
        test_integer(&result, expected);
    }
}

// ==================== POSTFIX TESTS ====================

#[test]
fn test_postfix_increment() {
    let result = test_eval("x := 5\nx++\nx");
    test_integer(&result, 6);
}

#[test]
fn test_postfix_decrement() {
    let result = test_eval("x := 5\nx--\nx");
    test_integer(&result, 4);
}

#[test]
fn test_postfix_returns_original() {
    // x++ should return original value
    let result = test_eval("x := 5\nx++");
    test_integer(&result, 5);
}

// ==================== OPTION EXPRESSION TESTS ====================

#[test]
fn test_option_expressions() {
    let tests = vec![
        ("option { True -> 10 }", Some(10)),
        ("option { False -> 10 }", None),
        ("option { 1 < 2 -> 10 }", Some(10)),
        ("option { 1 > 2 -> 10 }", None),
        ("option { 1 > 2 -> 10, 20 }", Some(20)),
        ("option { 1 < 2 -> 10, 20 }", Some(10)),
    ];

    for (input, expected) in tests {
        let result = test_eval(input);
        match expected {
            Some(val) => test_integer(&result, val),
            None => test_none(&result),
        }
    }
}

#[test]
fn test_option_simple_two_arm() {
    let result = test_eval("x := option { True -> 10, 20 }");
    test_integer(&result, 10);
    let result = test_eval("x := option { False -> 10, 20 }");
    test_integer(&result, 20);
}

#[test]
fn test_option_multi_arm() {
    let input = r#"
            x := 2
            option { x == 1 -> "one", x == 2 -> "two", "other" }
        "#;
    let result = test_eval(input);
    test_string(&result, "two");
}

#[test]
fn test_option_no_match_no_default() {
    let result = test_eval("option { False -> 1 }");
    test_none(&result);
}

#[test]
fn test_option_as_value() {
    let input = r#"
            y := 42
            result := option { y > 100 -> "big", y > 10 -> "medium", "small" }
            result
        "#;
    let result = test_eval(input);
    test_string(&result, "medium");
}

#[test]
fn test_option_block_bodies() {
    let input = "option { True -> { x := 5\n x * 2 }, 0 }";
    let result = test_eval(input);
    test_integer(&result, 10);
}

#[test]
fn test_option_ternary() {
    let input = r#"
            num := 10
            print_str := option { num > 5, "greater than 5", "less than 5" }
            print_str
        "#;
    let result = test_eval(input);
    test_string(&result, "greater than 5");

    let input = r#"
            num := 3
            print_str := option { num > 5, "greater than 5", "less than 5" }
            print_str
        "#;
    let result = test_eval(input);
    test_string(&result, "less than 5");
}

// ==================== UNLESS TESTS ====================

#[test]
fn test_unless_statement() {
    let result = test_eval("unless False { 42 }");
    test_integer(&result, 42);
    let result = test_eval("unless True { 42 }");
    test_none(&result);
}

#[test]
fn test_unless_expression_then() {
    let result = test_eval(r#""admin" unless False then "guest""#);
    test_string(&result, "admin");

    let result = test_eval(r#""admin" unless True then "guest""#);
    test_string(&result, "guest");
}

#[test]
fn test_unless_expression_then_with_assignment() {
    let input = r#"
            name := None
            result := "Guest" unless name != None then "Member"
            result
        "#;
    let result = test_eval(input);
    test_string(&result, "Guest");
}

#[test]
fn test_fail_expression() {
    let result = test_eval(r#"fail "bad input""#);
    match result.as_ref() {
        Object::Error(msg) => assert_eq!(msg, "bad input"),
        other => panic!("Expected error, got {:?}", other),
    }
}

#[test]
fn test_angle_fail_expression() {
    let result = test_eval(r#"<fail>("bad input")"#);
    match result.as_ref() {
        Object::Error(msg) => assert_eq!(msg, "bad input"),
        other => panic!("Expected error, got {:?}", other),
    }
}

#[test]
fn test_error_builtin() {
    let result = test_eval(r#"error("bad input")"#);
    match result.as_ref() {
        Object::Error(msg) => assert_eq!(msg, "bad input"),
        other => panic!("Expected error, got {:?}", other),
    }
}

#[test]
fn test_guard_expression_recovers_with_bound_error() {
    let input = r#"
            result := fail "boom" guard err -> err.msg
            result
        "#;
    let result = test_eval(input);
    test_string(&result, "boom");
}

#[test]
fn test_angle_guard_expression() {
    let input = r#"
            result := <fail>("boom") <guard>("fallback")
            result
        "#;
    let result = test_eval(input);
    test_string(&result, "fallback");
}

#[test]
fn test_guard_expression_keeps_success_value() {
    let input = r#"
            result := "ok" guard err -> err.msg
            result
        "#;
    let result = test_eval(input);
    test_string(&result, "ok");
}

#[test]
fn test_log_returns_none() {
    let result = test_eval(r#"<log>("hello")"#);
    test_none(&result);
}

#[test]
fn test_log_with_tag_returns_none() {
    let result = test_eval(r#"<log<debug>>("info message")"#);
    test_none(&result);
}

#[test]
fn test_log_no_message_returns_none() {
    let result = test_eval(r#"<log<Error>>"#);
    test_none(&result);
}

#[test]
fn test_log_error_without_message_uses_generic_message() {
    let env = Rc::new(RefCell::new(Environment::new()));
    let (tag, msg) = Evaluator::resolve_log_parts(Some("Error"), None, None, &env);
    assert_eq!(tag, "[ERROR]");
    assert_eq!(msg.as_deref(), Some("generic"));
}

#[test]
fn test_log_named_error_without_message_uses_bound_error_message() {
    let env = Rc::new(RefCell::new(Environment::new()));
    env.borrow_mut().set(
        "same_version_error".to_string(),
        Evaluator::tagged_error_value(
            "Oxigen version is same as installed version.".to_string(),
            Some("same_version_error".to_string()),
        ),
    );

    let (tag, msg) = Evaluator::resolve_log_parts(Some("same_version_error"), None, None, &env);
    assert_eq!(tag, "[SAME_VERSION_ERROR]");
    assert_eq!(
        msg.as_deref(),
        Some("Oxigen version is same as installed version.")
    );
}

#[test]
fn test_log_nested_error_without_message_uses_bound_error_message() {
    let env = Rc::new(RefCell::new(Environment::new()));
    env.borrow_mut().set(
        "same_version_error".to_string(),
        Evaluator::tagged_error_value(
            "Oxigen version is same as installed version.".to_string(),
            Some("same_version_error".to_string()),
        ),
    );

    let (tag, msg) =
        Evaluator::resolve_log_parts(Some("Error"), Some("same_version_error"), None, &env);
    assert_eq!(tag, "[SAME_VERSION_ERROR]");
    assert_eq!(
        msg.as_deref(),
        Some("Oxigen version is same as installed version.")
    );
}

#[test]
fn test_guard_expression_does_not_catch_none() {
    let input = r#"
            result := None guard err -> "fallback"
            result
        "#;
    let result = test_eval(input);
    test_none(&result);
}

#[test]
fn test_guard_expression_statement() {
    let input = r#"
            println(fail "boom") guard err -> err.msg
        "#;
    let result = test_eval(input);
    test_string(&result, "boom");
}

#[test]
fn test_option_error_arm_handles_condition_error() {
    let input = r#"
            option { missing == 1 -> 10, <Error> -> 0 }
        "#;
    let result = test_eval(input);
    test_integer(&result, 0);
}

#[test]
fn test_option_error_arm_handles_body_error() {
    let input = r#"
            option { True -> missing, <Error> -> 0 }
        "#;
    let result = test_eval(input);
    test_integer(&result, 0);
}

#[test]
fn test_error_constructor_expression_runtime() {
    let result = test_eval(r#"<Error>("bad input")"#);
    match result.as_ref() {
        Object::ErrorValue { msg, tag } => {
            assert_eq!(msg, "bad input");
            assert_eq!(tag, &None);
        }
        other => panic!("Expected ErrorValue, got {:?}", other),
    }
}

#[test]
fn test_tagged_error_constructor_expression_runtime() {
    let result = test_eval(r#"<Error<retry_error>>("bad input")"#);
    match result.as_ref() {
        Object::ErrorValue { msg, tag } => {
            assert_eq!(msg, "bad input");
            assert_eq!(tag.as_deref(), Some("retry_error"));
        }
        other => panic!("Expected tagged ErrorValue, got {:?}", other),
    }
}

#[test]
fn test_value_constructor_expression_runtime() {
    let result = test_eval(r#"<Value>("ok").value"#);
    test_string(&result, "ok");
}

#[test]
fn test_error_constructor_field_access() {
    let result = test_eval(r#"<Error<retry_error>>("bad input").tag"#);
    test_string(&result, "retry_error");

    let result = test_eval(r#"<Error>("bad input").msg"#);
    test_string(&result, "bad input");
}

#[test]
fn test_type_wrap_error_or_value_success() {
    let result = test_eval(r#"<type<Error || Value>>("ok").value"#);
    test_string(&result, "ok");
}

#[test]
fn test_type_wrap_error_or_value_error() {
    let result = test_eval(r#"<type<Error || Value>>(<fail>("boom")).msg"#);
    test_string(&result, "boom");
}

#[test]
fn test_typed_declaration_with_error_or_value() {
    let input = r#"
            result <Error || Value> := <type<Error || Value>>(<fail>("boom"))
            result.msg
        "#;
    let result = test_eval(input);
    test_string(&result, "boom");
}

#[test]
fn test_is_value_builtin() {
    test_boolean(&test_eval(r#"is_value(<Value>("ok"))"#), true);
    test_boolean(&test_eval(r#"is_value(<Error>("bad"))"#), false);
    test_boolean(&test_eval(r#"is_value("hello")"#), false);
    test_boolean(
        &test_eval(r#"is_value(<type<Error || Value>>("ok"))"#),
        true,
    );
}

#[test]
fn test_is_error_builtin() {
    test_boolean(&test_eval(r#"is_error(<Error>("bad"))"#), true);
    test_boolean(&test_eval(r#"is_error(<Error<net>>("fail"))"#), true);
    test_boolean(&test_eval(r#"is_error(<Value>("ok"))"#), false);
    test_boolean(&test_eval(r#"is_error("hello")"#), false);
    test_boolean(
        &test_eval(r#"is_error(<type<Error || Value>>(<fail>("boom")))"#),
        true,
    );
}

#[test]
fn test_type_wrap_tagged_error_union_applies_tag() {
    let result = test_eval(r#"<type<Error<custom> || Value>>(<fail>("boom")).tag"#);
    test_string(&result, "custom");
}

#[test]
fn test_type_wrap_tagged_error_union_success_path() {
    let result = test_eval(r#"<type<Value || Error<custom>>>("ok").value"#);
    test_string(&result, "ok");
}

#[test]
fn test_type_wrap_tagged_error_preserves_existing_tag() {
    let result =
        test_eval(r#"<type<Error<override> || Value>>(<fail>(<Error<original>>("boom"))).tag"#);
    test_string(&result, "original");
}

#[test]
fn test_log_with_nested_tags_returns_none() {
    let result = test_eval(r#"<log<Error<network>>>("connection lost")"#);
    test_none(&result);
}

#[test]
fn test_log_empty_parens_returns_none() {
    let result = test_eval(r#"<log<info>>()"#);
    test_none(&result);
}

// ==================== MAIN BLOCK TESTS ====================

#[test]
fn test_main_block_executes_in_main_context() {
    let result = test_eval(
        r#"
            main {
                42
            }
        "#,
    );
    test_integer(&result, 42);
}

#[test]
fn test_main_block_returns_last_value() {
    let result = test_eval(r#"main { 99 }"#);
    test_integer(&result, 99);
}

// ==================== POSTFIX GUARD TESTS ====================

#[test]
fn test_postfix_when_guard() {
    let input = r#"
            x <int> := 0
            x = 5 when True
            x
        "#;
    let result = test_eval(input);
    test_integer(&result, 5);
}

#[test]
fn test_postfix_when_guard_false() {
    let input = r#"
            x <int> := 0
            x = 99 when False
            x
        "#;
    let result = test_eval(input);
    test_integer(&result, 0);
}

#[test]
fn test_postfix_unless_guard() {
    let input = r#"
            x <int> := 0
            x = 5 unless False
            x
        "#;
    let result = test_eval(input);
    test_integer(&result, 5);
}

#[test]
fn test_postfix_unless_guard_true() {
    let input = r#"
            x <int> := 0
            x = 5 unless True
            x
        "#;
    let result = test_eval(input);
    test_integer(&result, 0);
}

// ==================== EACH LOOP TESTS ====================

#[test]
fn test_each_loop_array() {
    // Sum elements using each loop
    let input = r#"
            sum := 0
            each x in [1, 2, 3, 4, 5] {
                sum := sum + x
            }
            sum
        "#;
    let result = test_eval(input);
    test_integer(&result, 15);
}

#[test]
fn test_each_loop_string() {
    // Count characters
    let input = r#"
            count := 0
            each c in chars("hello") {
                count := count + 1
            }
            count
        "#;
    let result = test_eval(input);
    test_integer(&result, 5);
}

#[test]
fn test_each_with_stop() {
    let input = r#"
            sum := 0
            each x in [1, 2, 3, 4, 5] {
                stop when x == 3
                sum := sum + x
            }
            sum
        "#;
    let result = test_eval(input);
    test_integer(&result, 3); // 1 + 2
}

#[test]
fn test_each_with_skip() {
    let input = r#"
            sum := 0
            each x in [1, 2, 3, 4, 5] {
                skip when x == 3
                sum := sum + x
            }
            sum
        "#;
    let result = test_eval(input);
    test_integer(&result, 12); // 1 + 2 + 4 + 5
}

// ==================== REPEAT LOOP TESTS ====================

#[test]
fn test_repeat_loop() {
    let input = r#"
            x := 0
            repeat when x < 5 {
                x := x + 1
            }
            x
        "#;
    let result = test_eval(input);
    test_integer(&result, 5);
}

#[test]
fn test_repeat_with_decrement() {
    let input = r#"
            x := 10
            count := 0
            repeat when x > 0 {
                x--
                count := count + 1
            }
            count
        "#;
    let result = test_eval(input);
    test_integer(&result, 10);
}

#[test]
fn test_repeat_with_stop() {
    let input = r#"
            x := 0
            repeat when True {
                x := x + 1
                stop when x == 5
            }
            x
        "#;
    let result = test_eval(input);
    test_integer(&result, 5);
}

// ==================== PATTERN/CHOOSE TESTS ====================

#[test]
fn test_pattern_and_choose() {
    let input = r#"
            pattern is_ten(x) when x == 10
            pattern is_twenty(x) when x == 20

            y := 10
            choose y {
                is_ten -> 1,
                is_twenty -> 2,
                else -> 3
            }
        "#;
    let result = test_eval(input);
    test_integer(&result, 1);
}

#[test]
fn test_choose_else() {
    let input = r#"
            pattern is_ten(x) when x == 10

            y := 5
            choose y {
                is_ten -> 1,
                else -> 99
            }
        "#;
    let result = test_eval(input);
    test_integer(&result, 99);
}

#[test]
fn test_choose_inline_pattern() {
    let input = r#"
            y := 10
            choose y {
                pattern is_ten(x) when x == 10 -> 1,
                pattern is_twenty(x) when x == 20 -> 2,
                else -> 3
            }
        "#;
    let result = test_eval(input);
    test_integer(&result, 1);
}

#[test]
fn test_choose_mixed_inline_and_predefined() {
    let input = r#"
            pattern is_ten(x) when x == 10
            y := 20
            choose y {
                is_ten -> 1,
                pattern is_twenty(x) when x == 20 -> 2,
                else -> 3
            }
        "#;
    let result = test_eval(input);
    test_integer(&result, 2);
}

// ==================== BUILTIN FUNCTION TESTS ====================

#[test]
fn test_len_array() {
    let tests = vec![("len([])", 0), ("len([1])", 1), ("len([1, 2, 3])", 3)];

    for (input, expected) in tests {
        let result = test_eval(input);
        test_integer(&result, expected);
    }
}

#[test]
fn test_len_string() {
    let tests = vec![("len(\"\")", 0), ("len(\"hello\")", 5), ("len('world')", 5)];

    for (input, expected) in tests {
        let result = test_eval(input);
        test_integer(&result, expected);
    }
}

#[test]
fn test_first_last_rest() {
    test_integer(&test_eval("first([1, 2, 3])"), 1);
    test_integer(&test_eval("last([1, 2, 3])"), 3);

    let rest_result = test_eval("rest([1, 2, 3])");
    match rest_result.as_ref() {
        Object::Array(elements) => {
            assert_eq!(elements.len(), 2);
            test_integer(&elements[0], 2);
            test_integer(&elements[1], 3);
        }
        _ => panic!("Expected Array, got {:?}", rest_result),
    }
}

#[test]
fn test_push() {
    let result = test_eval("push([1, 2], 3)");
    match result.as_ref() {
        Object::Array(elements) => {
            assert_eq!(elements.len(), 3);
            test_integer(&elements[0], 1);
            test_integer(&elements[1], 2);
            test_integer(&elements[2], 3);
        }
        _ => panic!("Expected Array, got {:?}", result),
    }
}

#[test]
fn test_ord_chr() {
    test_integer(&test_eval("ord(`a`)"), 97);
    test_integer(&test_eval("ord(`A`)"), 65);
    test_integer(&test_eval("ord(`0`)"), 48);

    test_char(&test_eval("chr(97)"), 'a');
    test_char(&test_eval("chr(65)"), 'A');
    test_char(&test_eval("chr(48)"), '0');
}

#[test]
fn test_str_builtin() {
    test_string(&test_eval("str(`a`)"), "a");
    test_string(&test_eval("str(123)"), "123");
    test_string(&test_eval("str(3.14)"), "3.14");
    test_string(&test_eval("str(True)"), "True");
}

#[test]
fn test_chars_builtin() {
    let result = test_eval("chars(\"abc\")");
    match result.as_ref() {
        Object::Array(elements) => {
            assert_eq!(elements.len(), 3);
            test_char(&elements[0], 'a');
            test_char(&elements[1], 'b');
            test_char(&elements[2], 'c');
        }
        _ => panic!("Expected Array, got {:?}", result),
    }
}

#[test]
fn test_type_builtin() {
    test_string(&test_eval("type(5)"), "INTEGER");
    test_string(&test_eval("type(3.14)"), "FLOAT");
    test_string(&test_eval("type(`a`)"), "CHAR");
    test_string(&test_eval("type(\"hello\")"), "STRING");
    test_string(&test_eval("type(True)"), "BOOLEAN");
    test_string(&test_eval("type([1, 2])"), "ARRAY");
    test_string(&test_eval("type(None)"), "NONE");
}

// ==================== ERROR HANDLING TESTS ====================

#[test]
fn test_error_handling() {
    let tests = vec![
        ("5 + True", "type mismatch"),
        ("\"hello\" - \"world\"", "unknown operator"),
        ("foobar", "identifier not found"),
        ("-True", "unknown operator"),
        ("len(1)", "argument to `len` not supported"),
        ("len(\"one\", \"two\")", "wrong number of arguments"),
        ("first(1)", "argument to `first` must be ARRAY"),
    ];

    for (input, expected_msg) in tests {
        let result = test_eval(input);
        match result.as_ref() {
            Object::Error(msg) => {
                assert!(
                    msg.contains(expected_msg),
                    "Expected error containing '{}', got '{}'",
                    expected_msg,
                    msg
                );
            }
            _ => panic!("Expected error for '{}', got {:?}", input, result),
        }
    }
}

// ==================== NONE TESTS ====================

#[test]
fn test_none_literal() {
    test_none(&test_eval("None"));
}

#[test]
fn test_none_is_falsy() {
    let result = test_eval("option { None -> 1, 2 }");
    test_integer(&result, 2);
}

// ==================== TRUTHINESS TESTS ====================

#[test]
fn test_truthiness() {
    // Truthy values
    let truthy = vec!["1", "-1", "3.14", "`a`", "\"hello\"", "[1]", "True"];
    for input in truthy {
        let test_input = format!("option {{ {} -> 1, 0 }}", input);
        let result = test_eval(&test_input);
        test_integer(&result, 1);
    }

    // Falsy values
    let falsy = vec!["0", "0.0", "\"\"", "[]", "False", "None"];
    for input in falsy {
        let test_input = format!("option {{ {} -> 1, 0 }}", input);
        let result = test_eval(&test_input);
        test_integer(&result, 0);
    }
}

// ==================== COMPLEX INTEGRATION TESTS ====================

#[test]
fn test_fibonacci() {
    let input = r#"
            a := 0
            b := 1
            count := 0
            repeat when count < 10 {
                temp := a
                a := b
                b := temp + b
                count := count + 1
            }
            a
        "#;
    let result = test_eval(input);
    test_integer(&result, 55); // 10th Fibonacci number
}

#[test]
fn test_factorial_iterative() {
    let input = r#"
            n := 5
            result := 1
            repeat when n > 0 {
                result := result * n
                n--
            }
            result
        "#;
    let result = test_eval(input);
    test_integer(&result, 120); // 5!
}

#[test]
fn test_nested_loops() {
    let input = r#"
            sum := 0
            each i in [1, 2, 3] {
                each j in [10, 20] {
                    sum := sum + i * j
                }
            }
            sum
        "#;
    // 1*10 + 1*20 + 2*10 + 2*20 + 3*10 + 3*20 = 10+20+20+40+30+60 = 180
    let result = test_eval(input);
    test_integer(&result, 180);
}

// ==================== TYPED VARIABLE TESTS ====================

// --- Typed declaration with = (immutable value + immutable type) ---

#[test]
fn test_typed_strict_declaration() {
    let result = test_eval("x <int> = 10\nx");
    test_integer(&result, 10);
}

#[test]
fn test_typed_strict_rejects_wrong_type() {
    let result = test_eval("x <int> = \"hello\"");
    match result.as_ref() {
        Object::Error(msg) => assert!(msg.contains("type mismatch"), "got: {}", msg),
        _ => panic!("Expected error, got {:?}", result),
    }
}

// --- Typed declaration with := (mutable value + immutable type, conversion on init) ---

#[test]
fn test_typed_walrus_declaration_converts_float_to_int() {
    let result = test_eval("x <int> := 3.9\nx");
    test_integer(&result, 3); // conversion at declaration
}

#[test]
fn test_typed_walrus_declaration_converts_int_to_str() {
    let result = test_eval("x <str> := 42\nx");
    test_string(&result, "42");
}

#[test]
fn test_typed_walrus_declaration_converts_str_to_int() {
    let result = test_eval("x <int> := \"123\"\nx");
    test_integer(&result, 123);
}

#[test]
fn test_typed_walrus_declaration_converts_bool_to_int() {
    let result = test_eval("x <int> := True\nx");
    test_integer(&result, 1);
}

#[test]
fn test_typed_walrus_declaration_converts_int_to_float() {
    let result = test_eval("x <float> := 5\nx");
    test_float(&result, 5.0);
}

#[test]
fn test_typed_walrus_declaration_converts_int_to_bool() {
    let result = test_eval("x <bool> := 42\nx");
    test_boolean(&result, true);

    let result2 = test_eval("x <bool> := 0\nx");
    test_boolean(&result2, false);
}

#[test]
fn test_typed_walrus_declaration_impossible_conversion() {
    let result = test_eval("x <array> := 5");
    match result.as_ref() {
        Object::Error(msg) => assert!(msg.contains("cannot convert"), "got: {}", msg),
        _ => panic!("Expected error, got {:?}", result),
    }
}

#[test]
fn test_typed_walrus_string_to_array() {
    let result = test_eval("list_str <array> := \"hello\"\nlist_str");
    match result.as_ref() {
        Object::Array(elements) => {
            assert_eq!(elements.len(), 5);
            match elements[0].as_ref() {
                Object::String(s) => assert_eq!(s, "h"),
                other => panic!("Expected String(\"h\"), got {:?}", other),
            }
            match elements[4].as_ref() {
                Object::String(s) => assert_eq!(s, "o"),
                other => panic!("Expected String(\"o\"), got {:?}", other),
            }
        }
        _ => panic!("Expected array, got {:?}", result),
    }
}

// --- as-declare (mutable value + immutable type, zero value) ---

#[test]
fn test_typed_declare_zero_values() {
    test_integer(&test_eval("x as <int>\nx"), 0);
    test_float(&test_eval("x as <float>\nx"), 0.0);
    test_string(&test_eval("x as <str>\nx"), "");
    test_char(&test_eval("x as <char>\nx"), '\0');
    test_boolean(&test_eval("x as <bool>\nx"), false);

    let arr = test_eval("x as <array>\nx");
    match arr.as_ref() {
        Object::Array(elements) => assert!(elements.is_empty()),
        _ => panic!("Expected empty array, got {:?}", arr),
    }
}

#[test]
fn test_as_declare_is_mutable() {
    let result = test_eval("x as <int>\nx = 5\nx");
    test_integer(&result, 5);
}

#[test]
fn test_as_declare_postfix_works() {
    let result = test_eval("x as <int>\nx = 5\nx++\nx");
    test_integer(&result, 6);
}

// --- Immutable value: = blocked, ++/-- blocked ---

#[test]
fn test_immutable_assign_blocked() {
    let result = test_eval("x <int> = 10\nx = 20");
    match result.as_ref() {
        Object::Error(msg) => {
            assert!(msg.contains("cannot reassign immutable"), "got: {}", msg)
        }
        _ => panic!("Expected immutability error, got {:?}", result),
    }
}

#[test]
fn test_immutable_postfix_increment_blocked() {
    let result = test_eval("x <int> = 5\nx++");
    match result.as_ref() {
        Object::Error(msg) => assert!(msg.contains("cannot mutate immutable"), "got: {}", msg),
        _ => panic!("Expected immutability error, got {:?}", result),
    }
}

#[test]
fn test_immutable_postfix_decrement_blocked() {
    let result = test_eval("x <int> = 5\nx--");
    match result.as_ref() {
        Object::Error(msg) => assert!(msg.contains("cannot mutate immutable"), "got: {}", msg),
        _ => panic!("Expected immutability error, got {:?}", result),
    }
}

// --- Walrus := overrides value immutability, but type stays locked ---

#[test]
fn test_walrus_overrides_immutable_value_same_type() {
    // := can change the value if the type matches
    let result = test_eval("x <int> = 10\nx := 20\nx");
    test_integer(&result, 20);
}

#[test]
fn test_walrus_on_immutable_rejects_wrong_type() {
    // := overrides value immutability but type is still locked
    let result = test_eval("x <int> = 10\nx := \"hello\"");
    match result.as_ref() {
        Object::Error(msg) => assert!(
            msg.contains("type mismatch") && msg.contains("locked"),
            "got: {}",
            msg
        ),
        _ => panic!("Expected type locked error, got {:?}", result),
    }
}

#[test]
fn test_walrus_on_immutable_rejects_float_no_conversion() {
    // No implicit conversion on walrus reassignment — type is locked
    let result = test_eval("x <int> = 10\nx := 3.7");
    match result.as_ref() {
        Object::Error(msg) => assert!(
            msg.contains("type mismatch") && msg.contains("locked"),
            "got: {}",
            msg
        ),
        _ => panic!("Expected type locked error, got {:?}", result),
    }
}

// --- Explicit re-declaration changes the type ---

#[test]
fn test_explicit_redeclare_changes_type() {
    // x <str> := "20" re-declares x with new type (conversion at declaration)
    let result = test_eval("x <int> = 10\nx <str> := \"20\"\nx");
    test_string(&result, "20");
}

#[test]
fn test_explicit_redeclare_immutable_to_mutable() {
    // Re-declare as mutable, then = works
    let result = test_eval("x <int> = 10\nx <int> := 99\nx = 42\nx");
    test_integer(&result, 42);
}

// --- Mutable typed (:= declares mutable value) ---

#[test]
fn test_mutable_typed_assign_succeeds() {
    let result = test_eval("x <int> := 10\nx = 20\nx");
    test_integer(&result, 20);
}

#[test]
fn test_mutable_typed_assign_rejects_wrong_type() {
    let result = test_eval("x <int> := 10\nx = \"hello\"");
    match result.as_ref() {
        Object::Error(msg) => assert!(msg.contains("type mismatch"), "got: {}", msg),
        _ => panic!("Expected error, got {:?}", result),
    }
}

#[test]
fn test_mutable_typed_postfix_increment() {
    let result = test_eval("x <int> := 5\nx++\nx");
    test_integer(&result, 6);
}

#[test]
fn test_mutable_typed_postfix_decrement() {
    let result = test_eval("x <int> := 5\nx--\nx");
    test_integer(&result, 4);
}

#[test]
fn test_mutable_typed_walrus_reassign_same_type() {
    // := reassignment on mutable typed: strict type check, no conversion
    let result = test_eval("x <int> := 10\nx := 42\nx");
    test_integer(&result, 42);
}

#[test]
fn test_mutable_typed_walrus_reassign_wrong_type() {
    // Type is locked even on mutable typed variables
    let result = test_eval("x <int> := 10\nx := \"hello\"");
    match result.as_ref() {
        Object::Error(msg) => assert!(
            msg.contains("type mismatch") && msg.contains("locked"),
            "got: {}",
            msg
        ),
        _ => panic!("Expected type locked error, got {:?}", result),
    }
}

// --- Untyped / undeclared errors ---

#[test]
fn test_assign_untyped_variable_errors() {
    let result = test_eval("x := 10\nx = 20");
    match result.as_ref() {
        Object::Error(msg) => assert!(msg.contains("requires typed variable"), "got: {}", msg),
        _ => panic!("Expected error, got {:?}", result),
    }
}

#[test]
fn test_assign_undeclared_variable_errors() {
    let result = test_eval("x = 20");
    match result.as_ref() {
        Object::Error(msg) => assert!(msg.contains("identifier not found"), "got: {}", msg),
        _ => panic!("Expected error, got {:?}", result),
    }
}

// --- Shadowing in inner scope ---

#[test]
fn test_typed_shadowing_inner_scope() {
    let input = r#"
            x <int> = 10
            each i in [1] {
                x <str> = "hello"
            }
            x
        "#;
    let result = test_eval(input);
    test_integer(&result, 10);
}

#[test]
fn test_immutable_shadowed_in_inner_scope_ok() {
    let input = r#"
            x <int> = 10
            each i in [1] {
                x <int> := 99
                x = 42
            }
            x
        "#;
    let result = test_eval(input);
    test_integer(&result, 10);
}

// --- Self-conversion: re-declare a variable with typed walrus to convert in place ---

#[test]
fn test_self_conversion_str_to_int() {
    // Simulates: user_input comes in as string, re-declare as int
    let input = r#"
            user_input := "42"
            user_input <int> := user_input
            user_input
        "#;
    let result = test_eval(input);
    test_integer(&result, 42);
}

#[test]
fn test_self_conversion_str_to_float() {
    let input = r#"
            val := "3.14"
            val <float> := val
            val
        "#;
    let result = test_eval(input);
    test_float(&result, 3.14);
}

#[test]
fn test_self_conversion_float_to_int() {
    let input = r#"
            x := 3.9
            x <int> := x
            x
        "#;
    let result = test_eval(input);
    test_integer(&result, 3);
}

#[test]
fn test_self_conversion_int_to_str() {
    let input = r#"
            num := 100
            num <str> := num
            num
        "#;
    let result = test_eval(input);
    test_string(&result, "100");
}

#[test]
fn test_self_conversion_bad_string_to_int_errors() {
    let input = r#"
            user_input := "hello"
            user_input <int> := user_input
        "#;
    let result = test_eval(input);
    match result.as_ref() {
        Object::Error(msg) => assert!(msg.contains("cannot convert"), "got: {}", msg),
        _ => panic!("Expected conversion error, got {:?}", result),
    }
}

#[test]
fn test_self_conversion_locks_type() {
    // After re-declaration, type is locked
    let input = r#"
            x := "42"
            x <int> := x
            x := "hello"
        "#;
    let result = test_eval(input);
    match result.as_ref() {
        Object::Error(msg) => assert!(msg.contains("locked"), "got: {}", msg),
        _ => panic!("Expected type locked error, got {:?}", result),
    }
}

// ==================== IS_MUT TESTS ====================

#[test]
fn test_is_mut_untyped_variable() {
    let result = test_eval("x := 10\nis_mut(x)");
    test_boolean(&result, true);
}

#[test]
fn test_is_mut_immutable_typed() {
    let result = test_eval("x <int> = 10\nis_mut(x)");
    test_boolean(&result, false);
}

#[test]
fn test_is_mut_mutable_typed() {
    let result = test_eval("x <int> := 10\nis_mut(x)");
    test_boolean(&result, true);
}

#[test]
fn test_is_mut_as_declare() {
    let result = test_eval("x as <int>\nis_mut(x)");
    test_boolean(&result, true);
}

#[test]
fn test_is_mut_undeclared_errors() {
    let result = test_eval("is_mut(x)");
    match result.as_ref() {
        Object::Error(msg) => assert!(msg.contains("identifier not found"), "got: {}", msg),
        _ => panic!("Expected error, got {:?}", result),
    }
}

#[test]
fn test_is_mut_wrong_arg_count() {
    let result = test_eval("x := 1\ny := 2\nis_mut(x, y)");
    match result.as_ref() {
        Object::Error(msg) => {
            assert!(msg.contains("wrong number of arguments"), "got: {}", msg)
        }
        _ => panic!("Expected error, got {:?}", result),
    }
}

// ==================== IS_TYPE_MUT TESTS ====================

#[test]
fn test_is_type_mut_untyped_variable() {
    let result = test_eval("x := 10\nis_type_mut(x)");
    test_boolean(&result, true);
}

#[test]
fn test_is_type_mut_strict_typed() {
    let result = test_eval("x <int> = 10\nis_type_mut(x)");
    test_boolean(&result, false);
}

#[test]
fn test_is_type_mut_walrus_typed() {
    let result = test_eval("x <int> := 10\nis_type_mut(x)");
    test_boolean(&result, false);
}

#[test]
fn test_is_type_mut_as_declare() {
    let result = test_eval("x as <int>\nis_type_mut(x)");
    test_boolean(&result, false);
}

#[test]
fn test_is_type_mut_undeclared_errors() {
    let result = test_eval("is_type_mut(x)");
    match result.as_ref() {
        Object::Error(msg) => assert!(msg.contains("identifier not found"), "got: {}", msg),
        _ => panic!("Expected error, got {:?}", result),
    }
}

// ==================== FUNCTION TESTS ====================

#[test]
fn test_named_function_call() {
    let input = "fun add(a, b) { a + b }\nadd(3, 4)";
    test_integer(&test_eval(input), 7);
}

#[test]
fn test_anonymous_function_assigned() {
    let input = "d := fun(n) { n * 2 }\nd(5)";
    test_integer(&test_eval(input), 10);
}

#[test]
fn test_function_implicit_return() {
    let input = "fun double(x) { x * 2 }\ndouble(7)";
    test_integer(&test_eval(input), 14);
}

#[test]
fn test_function_early_return_give() {
    let input = r#"
            fun abs(n) {
                give -n when n < 0
                n
            }
            abs(-5)
        "#;
    test_integer(&test_eval(input), 5);
}

#[test]
fn test_give_does_not_leak() {
    let input = r#"
            fun e() { give 10 }
            e() + 5
        "#;
    test_integer(&test_eval(input), 15);
}

#[test]
fn test_closure_captures_env() {
    let input = "x := 10\nfun f(n) { n + x }\nf(5)";
    test_integer(&test_eval(input), 15);
}

#[test]
fn test_higher_order_function() {
    let input = r#"
            fun apply(f, x) { f(x) }
            fun double(n) { n * 2 }
            apply(double, 5)
        "#;
    test_integer(&test_eval(input), 10);
}

#[test]
fn test_closure_factory() {
    let input = r#"
            fun make_adder(x) { fun(y) { x + y } }
            add5 := make_adder(5)
            add5(3)
        "#;
    test_integer(&test_eval(input), 8);
}

#[test]
fn test_recursion_factorial() {
    let input = r#"
            fun factorial(n) {
                give 1 when n <= 1
                n * factorial(n - 1)
            }
            factorial(5)
        "#;
    test_integer(&test_eval(input), 120);
}

#[test]
fn test_function_wrong_arg_count() {
    let input = "fun add(a, b) { a + b }\nadd(1)";
    let result = test_eval(input);
    match result.as_ref() {
        Object::Error(msg) => {
            assert!(msg.contains("wrong number of arguments"), "got: {}", msg)
        }
        _ => panic!("Expected error, got {:?}", result),
    }
}

#[test]
fn test_zero_param_function() {
    let input = "fun greet() { 42 }\ngreet()";
    test_integer(&test_eval(input), 42);
}

#[test]
fn test_iife() {
    let input = "fun(x) { x * x }(7)";
    test_integer(&test_eval(input), 49);
}

#[test]
fn test_give_inside_loop_in_function() {
    let input = r#"
            fun find_first_even(arr) {
                each x in arr {
                    give x when x % 2 == 0
                }
                -1
            }
            find_first_even([1, 3, 4, 6])
        "#;
    test_integer(&test_eval(input), 4);
}

#[test]
fn test_function_indent_mode() {
    let input = "#[indent]\nfun add(a, b):\n    a + b\nadd(3, 4)\n";
    test_integer(&test_eval(input), 7);
}

#[test]
fn test_function_indent_mode_with_give() {
    let input = "#[indent]\nfun abs(n):\n    give -n when n < 0\n    n\nabs(-5)\n";
    test_integer(&test_eval(input), 5);
}

// ==================== STRUCT TESTS ====================

#[test]
fn test_struct_positional_instantiation() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            p := Person("Alice", 30)
            p.name
        "#;
    test_string(&test_eval(input), "Alice");
}

#[test]
fn test_struct_positional_field_access() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            p := Person("Alice", 30)
            p.age
        "#;
    test_integer(&test_eval(input), 30);
}

#[test]
fn test_struct_named_instantiation() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            p := Person { name: "Bob", age: 25 }
            p.name
        "#;
    test_string(&test_eval(input), "Bob");
}

#[test]
fn test_struct_named_field_access() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            p := Person { name: "Bob", age: 25 }
            p.age
        "#;
    test_integer(&test_eval(input), 25);
}

#[test]
fn test_struct_dot_assign() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            p := Person("Alice", 30)
            p.age = 31
            p.age
        "#;
    test_integer(&test_eval(input), 31);
}

#[test]
fn test_struct_dot_assign_type_mismatch() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            p := Person("Alice", 30)
            p.age = "thirty"
        "#;
    let result = test_eval(input);
    match result.as_ref() {
        Object::Error(msg) => assert!(msg.contains("type mismatch"), "got: {}", msg),
        _ => panic!("Expected type mismatch error, got {:?}", result),
    }
}

#[test]
fn test_struct_method_implicit_self() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            Person contains {
                fun greet() { name }
            }
            p := Person("Alice", 30)
            p.greet()
        "#;
    test_string(&test_eval(input), "Alice");
}

#[test]
fn test_struct_method_is_adult() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            Person contains {
                fun is_adult() { age >= 18 }
            }
            p := Person("Alice", 30)
            p.is_adult()
        "#;
    test_boolean(&test_eval(input), true);
}

#[test]
fn test_struct_inheritance_fields() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            struct American(Person) {
                nationality <str>
            }
            a := American("John", 25, "USA")
            a.nationality
        "#;
    test_string(&test_eval(input), "USA");
}

#[test]
fn test_struct_inheritance_parent_fields() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            struct American(Person) {
                nationality <str>
            }
            a := American("John", 25, "USA")
            a.name
        "#;
    test_string(&test_eval(input), "John");
}

#[test]
fn test_struct_inheritance_inherits_methods() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            Person contains {
                fun greet() { name }
            }
            struct American(Person) {
                nationality <str>
            }
            a := American("John", 25, "USA")
            a.greet()
        "#;
    test_string(&test_eval(input), "John");
}

#[test]
fn test_struct_method_override() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            Person contains {
                fun greet() { name }
            }
            struct American(Person) {
                nationality <str>
            }
            American contains {
                fun greet() { name + " from " + nationality }
            }
            a := American("John", 25, "USA")
            a.greet()
        "#;
    test_string(&test_eval(input), "John from USA");
}

#[test]
fn test_struct_wrong_arg_count() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            p := Person("Alice")
        "#;
    let result = test_eval(input);
    match result.as_ref() {
        Object::Error(msg) => assert!(msg.contains("2 fields, got 1"), "got: {}", msg),
        _ => panic!("Expected error, got {:?}", result),
    }
}

#[test]
fn test_struct_unknown_field_named() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            p := Person { name: "Alice", height: 170 }
        "#;
    let result = test_eval(input);
    match result.as_ref() {
        Object::Error(msg) => assert!(msg.contains("unknown field"), "got: {}", msg),
        _ => panic!("Expected error, got {:?}", result),
    }
}

#[test]
fn test_struct_type_builtin() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            p := Person("Alice", 30)
            type(p)
        "#;
    test_string(&test_eval(input), "Person");
}

#[test]
fn test_struct_zero_fields() {
    let input = r#"
            struct Empty {
            }
            e := Empty()
            type(e)
        "#;
    test_string(&test_eval(input), "Empty");
}

#[test]
fn test_struct_method_with_give() {
    let input = r#"
            struct Counter {
                count <int>
            }
            Counter contains {
                fun get_double() { give count * 2 }
            }
            c := Counter(5)
            c.get_double()
        "#;
    test_integer(&test_eval(input), 10);
}

#[test]
fn test_struct_method_mutates_field() {
    let input = r#"
            struct Bag {
                items <array>
            }
            Bag contains {
                fun add(item) { items := push(items, item) }
            }
            b := Bag([])
            b.add(1)
            b.add(2)
            b.add(3)
            len(b.items)
        "#;
    test_integer(&test_eval(input), 3);
}

#[test]
fn test_struct_method_mutates_counter() {
    let input = r#"
            struct Counter {
                count <int>
            }
            Counter contains {
                fun inc() { count := count + 1 }
            }
            c := Counter(0)
            c.inc()
            c.inc()
            c.inc()
            c.count
        "#;
    test_integer(&test_eval(input), 3);
}

#[test]
fn test_struct_dot_chaining() {
    let input = r#"
            struct Inner {
                val <int>
            }
            struct Outer {
                name <str>
            }
            Outer contains {
                fun make_inner() { Inner(42) }
            }
            o := Outer("test")
            o.make_inner().val
        "#;
    test_integer(&test_eval(input), 42);
}

// ==================== STRUCT AS TYPE ANNOTATION TESTS ====================

#[test]
fn test_struct_typed_strict() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            p <Person> = Person("Alice", 30)
            p.name
        "#;
    test_string(&test_eval(input), "Alice");
}

#[test]
fn test_struct_typed_walrus() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            p <Person> := Person("Bob", 25)
            p.age
        "#;
    test_integer(&test_eval(input), 25);
}

#[test]
fn test_struct_typed_strict_wrong_type() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            p <Person> = 42
        "#;
    let result = test_eval(input);
    match result.as_ref() {
        Object::Error(msg) => assert!(msg.contains("type mismatch"), "got: {}", msg),
        _ => panic!("Expected error, got {:?}", result),
    }
}

#[test]
fn test_struct_typed_reassign_same_type() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            p <Person> := Person("Alice", 30)
            p = Person("Bob", 25)
            p.name
        "#;
    test_string(&test_eval(input), "Bob");
}

#[test]
fn test_struct_typed_reassign_wrong_type() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            p <Person> := Person("Alice", 30)
            p = 42
        "#;
    let result = test_eval(input);
    match result.as_ref() {
        Object::Error(msg) => assert!(msg.contains("type mismatch"), "got: {}", msg),
        _ => panic!("Expected error, got {:?}", result),
    }
}

#[test]
fn test_struct_typed_immutable() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            p <Person> = Person("Alice", 30)
            p = Person("Bob", 25)
        "#;
    let result = test_eval(input);
    match result.as_ref() {
        Object::Error(msg) => {
            assert!(msg.contains("cannot reassign immutable"), "got: {}", msg)
        }
        _ => panic!("Expected error, got {:?}", result),
    }
}

#[test]
fn test_struct_typed_walrus_reassign_wrong_struct() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            struct Dog {
                name <str>
            }
            p <Person> := Person("Alice", 30)
            p := Dog("Rex")
        "#;
    let result = test_eval(input);
    match result.as_ref() {
        Object::Error(msg) => assert!(msg.contains("locked"), "got: {}", msg),
        _ => panic!("Expected error, got {:?}", result),
    }
}

#[test]
fn test_struct_field_typed_as_struct() {
    let input = r#"
            struct Address {
                city <str>
            }
            struct Person {
                name <str>
                addr <Address>
            }
            a := Address("NYC")
            p := Person("Alice", a)
            p.addr.city
        "#;
    test_string(&test_eval(input), "NYC");
}

#[test]
fn test_struct_field_typed_wrong_struct() {
    let input = r#"
            struct Address {
                city <str>
            }
            struct Person {
                name <str>
                addr <Address>
            }
            p := Person("Alice", "not an address")
        "#;
    let result = test_eval(input);
    match result.as_ref() {
        Object::Error(msg) => assert!(msg.contains("type mismatch"), "got: {}", msg),
        _ => panic!("Expected error, got {:?}", result),
    }
}

#[test]
fn test_struct_as_declare_zero_values() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            p as <Person>
            p.name
        "#;
    test_string(&test_eval(input), "");
}

#[test]
fn test_struct_as_declare_zero_int() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            p as <Person>
            p.age
        "#;
    test_integer(&test_eval(input), 0);
}

#[test]
fn test_struct_as_declare_then_assign() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            p as <Person>
            p = Person("Alice", 30)
            p.name
        "#;
    test_string(&test_eval(input), "Alice");
}

#[test]
fn test_struct_as_declare_then_assign_wrong_type() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            p as <Person>
            p = 42
        "#;
    let result = test_eval(input);
    match result.as_ref() {
        Object::Error(msg) => assert!(msg.contains("type mismatch"), "got: {}", msg),
        _ => panic!("Expected error, got {:?}", result),
    }
}

#[test]
fn test_struct_as_declare_dot_assign() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            p as <Person>
            p.name = "Bob"
            p.age = 25
            p.name
        "#;
    test_string(&test_eval(input), "Bob");
}

#[test]
fn test_struct_as_declare_type_returned() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            p as <Person>
            type(p)
        "#;
    test_string(&test_eval(input), "Person");
}

// ==================== SHORTHAND TYPED DECLARE TESTS ====================

#[test]
fn test_shorthand_typed_declare_struct() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            p <Person>
            p.name
        "#;
    test_string(&test_eval(input), "");
}

#[test]
fn test_shorthand_typed_declare_struct_age() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            p <Person>
            p.age
        "#;
    test_integer(&test_eval(input), 0);
}

#[test]
fn test_shorthand_typed_declare_then_dot_assign() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            p <Person>
            p.name = "Alice"
            p.age = 30
            p.name
        "#;
    test_string(&test_eval(input), "Alice");
}

#[test]
fn test_shorthand_typed_declare_then_reassign() {
    let input = r#"
            struct Person {
                name <str>
                age <int>
            }
            p <Person>
            p = Person("Bob", 25)
            p.age
        "#;
    test_integer(&test_eval(input), 25);
}

#[test]
fn test_shorthand_typed_declare_builtin() {
    let result = test_eval("x <int>\nx");
    test_integer(&result, 0);
}

#[test]
fn test_shorthand_typed_declare_str() {
    let result = test_eval("x <str>\nx");
    test_string(&result, "");
}

// ==================== BYTE TESTS ====================

#[test]
fn test_byte_builtin() {
    let result = test_eval("byte(65)");
    match result.as_ref() {
        Object::Byte(n) => assert_eq!(*n, 65),
        _ => panic!("Expected Byte, got {:?}", result),
    }
}

#[test]
fn test_byte_type() {
    test_string(&test_eval("type(byte(0))"), "BYTE");
}

#[test]
fn test_byte_typed_declare() {
    let result = test_eval("x <byte>\nx");
    match result.as_ref() {
        Object::Byte(n) => assert_eq!(*n, 0),
        _ => panic!("Expected Byte(0), got {:?}", result),
    }
}

#[test]
fn test_byte_arithmetic_promotes_to_int() {
    test_integer(&test_eval("byte(10) + byte(20)"), 30);
}

#[test]
fn test_byte_out_of_range() {
    let result = test_eval("byte(256)");
    match result.as_ref() {
        Object::Error(msg) => assert!(msg.contains("out of range"), "got: {}", msg),
        _ => panic!("Expected error, got {:?}", result),
    }
}

#[test]
fn test_byte_walrus_conversion() {
    let result = test_eval("x <byte> := 65\nx");
    match result.as_ref() {
        Object::Byte(n) => assert_eq!(*n, 65),
        _ => panic!("Expected Byte(65), got {:?}", result),
    }
}

// ==================== UINT TESTS ====================

#[test]
fn test_uint_builtin() {
    let result = test_eval("uint(42)");
    match result.as_ref() {
        Object::Uint(n) => assert_eq!(*n, 42),
        _ => panic!("Expected Uint, got {:?}", result),
    }
}

#[test]
fn test_uint_type() {
    test_string(&test_eval("type(uint(0))"), "UINT");
}

#[test]
fn test_uint_typed_declare() {
    let result = test_eval("x <uint>\nx");
    match result.as_ref() {
        Object::Uint(n) => assert_eq!(*n, 0),
        _ => panic!("Expected Uint(0), got {:?}", result),
    }
}

#[test]
fn test_uint_arithmetic() {
    let result = test_eval("uint(10) + uint(20)");
    match result.as_ref() {
        Object::Uint(n) => assert_eq!(*n, 30),
        _ => panic!("Expected Uint(30), got {:?}", result),
    }
}

#[test]
fn test_uint_negative_error() {
    let result = test_eval("uint(-1)");
    match result.as_ref() {
        Object::Error(msg) => assert!(msg.contains("negative"), "got: {}", msg),
        _ => panic!("Expected error, got {:?}", result),
    }
}

#[test]
fn test_uint_underflow_error() {
    let result = test_eval("uint(1) - uint(2)");
    match result.as_ref() {
        Object::Error(msg) => assert!(msg.contains("underflow"), "got: {}", msg),
        _ => panic!("Expected error, got {:?}", result),
    }
}

// ==================== TUPLE TESTS ====================

#[test]
fn test_tuple_literal() {
    let result = test_eval("(1, 2, 3)");
    match result.as_ref() {
        Object::Tuple(elements) => {
            assert_eq!(elements.len(), 3);
            test_integer(&elements[0], 1);
            test_integer(&elements[1], 2);
            test_integer(&elements[2], 3);
        }
        _ => panic!("Expected Tuple, got {:?}", result),
    }
}

#[test]
fn test_tuple_empty() {
    let result = test_eval("()");
    match result.as_ref() {
        Object::Tuple(elements) => assert_eq!(elements.len(), 0),
        _ => panic!("Expected empty Tuple, got {:?}", result),
    }
}

#[test]
fn test_tuple_single_trailing_comma() {
    let result = test_eval("(42,)");
    match result.as_ref() {
        Object::Tuple(elements) => {
            assert_eq!(elements.len(), 1);
            test_integer(&elements[0], 42);
        }
        _ => panic!("Expected single-element Tuple, got {:?}", result),
    }
}

#[test]
fn test_tuple_index() {
    test_integer(&test_eval("(10, 20, 30)[1]"), 20);
}

#[test]
fn test_tuple_type() {
    test_string(&test_eval("type((1, 2))"), "TUPLE");
}

#[test]
fn test_tuple_len() {
    test_integer(&test_eval("len((1, 2, 3))"), 3);
}

#[test]
fn test_tuple_concat() {
    let result = test_eval("(1, 2) + (3, 4)");
    match result.as_ref() {
        Object::Tuple(elements) => {
            assert_eq!(elements.len(), 4);
            test_integer(&elements[0], 1);
            test_integer(&elements[3], 4);
        }
        _ => panic!("Expected Tuple, got {:?}", result),
    }
}

#[test]
fn test_tuple_equality() {
    test_boolean(&test_eval("(1, 2) == (1, 2)"), true);
    test_boolean(&test_eval("(1, 2) == (1, 3)"), false);
    test_boolean(&test_eval("(1, 2) != (1, 3)"), true);
}

#[test]
fn test_tuple_typed_declare() {
    let result = test_eval("t <tuple>\nlen(t)");
    test_integer(&result, 0);
}

#[test]
fn test_tuple_each() {
    let input = r#"
            sum := 0
            each x in (1, 2, 3) {
                sum := sum + x
            }
            sum
        "#;
    test_integer(&test_eval(input), 6);
}

#[test]
fn test_tuple_has() {
    test_boolean(&test_eval("has((1, 2, 3), 2)"), true);
    test_boolean(&test_eval("has((1, 2, 3), 5)"), false);
}

// ==================== MAP TESTS ====================

#[test]
fn test_map_literal() {
    let result = test_eval("{\"a\": 1, \"b\": 2}");
    match result.as_ref() {
        Object::Map(entries) => assert_eq!(entries.len(), 2),
        _ => panic!("Expected Map, got {:?}", result),
    }
}

#[test]
fn test_map_empty() {
    let result = test_eval("{}");
    match result.as_ref() {
        Object::Map(entries) => assert_eq!(entries.len(), 0),
        _ => panic!("Expected empty Map, got {:?}", result),
    }
}

#[test]
fn test_map_index() {
    test_integer(&test_eval("{\"a\": 10, \"b\": 20}[\"b\"]"), 20);
}

#[test]
fn test_map_index_missing() {
    test_none(&test_eval("{\"a\": 1}[\"z\"]"));
}

#[test]
fn test_map_type() {
    test_string(&test_eval("type({})"), "MAP");
}

#[test]
fn test_map_len() {
    test_integer(&test_eval("len({\"a\": 1, \"b\": 2})"), 2);
}

#[test]
fn test_map_keys() {
    let result = test_eval("keys({\"x\": 1, \"y\": 2})");
    match result.as_ref() {
        Object::Array(elements) => assert_eq!(elements.len(), 2),
        _ => panic!("Expected Array, got {:?}", result),
    }
}

#[test]
fn test_map_values() {
    let result = test_eval("values({\"x\": 10, \"y\": 20})");
    match result.as_ref() {
        Object::Array(elements) => assert_eq!(elements.len(), 2),
        _ => panic!("Expected Array, got {:?}", result),
    }
}

#[test]
fn test_map_insert() {
    test_integer(
        &test_eval("m := {\"a\": 1}\nm := insert(m, \"b\", 2)\nm[\"b\"]"),
        2,
    );
}

#[test]
fn test_map_remove() {
    test_integer(
        &test_eval("m := {\"a\": 1, \"b\": 2}\nm := remove(m, \"a\")\nlen(m)"),
        1,
    );
}

#[test]
fn test_map_has() {
    test_boolean(&test_eval("has({\"a\": 1}, \"a\")"), true);
    test_boolean(&test_eval("has({\"a\": 1}, \"z\")"), false);
}

#[test]
fn test_map_typed_declare() {
    let result = test_eval("m <map>\nlen(m)");
    test_integer(&result, 0);
}

#[test]
fn test_map_integer_keys() {
    test_string(&test_eval("{1: \"one\", 2: \"two\"}[1]"), "one");
}

#[test]
fn test_map_each_iterates_tuples() {
    let input = r#"
            sum := 0
            each entry in {1: 10, 2: 20} {
                sum := sum + entry[1]
            }
            sum
        "#;
    test_integer(&test_eval(input), 30);
}

// ==================== SET TESTS ====================

#[test]
fn test_set_builtin() {
    let result = test_eval("set(1, 2, 3)");
    match result.as_ref() {
        Object::Set(elements) => assert_eq!(elements.len(), 3),
        _ => panic!("Expected Set, got {:?}", result),
    }
}

#[test]
fn test_set_empty() {
    let result = test_eval("set()");
    match result.as_ref() {
        Object::Set(elements) => assert_eq!(elements.len(), 0),
        _ => panic!("Expected empty Set, got {:?}", result),
    }
}

#[test]
fn test_set_deduplicates() {
    let result = test_eval("set(1, 2, 2, 3, 3)");
    match result.as_ref() {
        Object::Set(elements) => assert_eq!(elements.len(), 3),
        _ => panic!("Expected Set with 3 elements, got {:?}", result),
    }
}

#[test]
fn test_set_type() {
    test_string(&test_eval("type(set())"), "SET");
}

#[test]
fn test_set_len() {
    test_integer(&test_eval("len(set(1, 2, 3))"), 3);
}

#[test]
fn test_set_has() {
    test_boolean(&test_eval("has(set(1, 2, 3), 2)"), true);
    test_boolean(&test_eval("has(set(1, 2, 3), 5)"), false);
}

#[test]
fn test_set_remove() {
    test_integer(
        &test_eval("s := set(1, 2, 3)\ns := remove(s, 2)\nlen(s)"),
        2,
    );
}

#[test]
fn test_set_typed_declare() {
    let result = test_eval("s <set>\nlen(s)");
    test_integer(&result, 0);
}

#[test]
fn test_set_each() {
    let input = r#"
            sum := 0
            each x in set(10, 20, 30) {
                sum := sum + x
            }
            sum
        "#;
    test_integer(&test_eval(input), 60);
}

// ==================== ARRAY ZERO VALUE TEST ====================

#[test]
fn test_array_typed_declare() {
    let result = test_eval("arr <array>\narr");
    match result.as_ref() {
        Object::Array(elements) => assert!(elements.is_empty()),
        _ => panic!("Expected empty array, got {:?}", result),
    }
}

// ==================== CROSS-TYPE TESTS ====================

#[test]
fn test_byte_int_arithmetic() {
    test_integer(&test_eval("byte(10) + 5"), 15);
}

#[test]
fn test_uint_int_arithmetic() {
    test_integer(&test_eval("uint(10) + 5"), 15);
}

#[test]
fn test_has_on_array() {
    test_boolean(&test_eval("has([1, 2, 3], 2)"), true);
    test_boolean(&test_eval("has([1, 2, 3], 5)"), false);
}

// ==================== SLICE TESTS ====================

#[test]
fn test_array_slice_start_end() {
    let result = test_eval("[10, 20, 30, 40, 50][1:3]");
    match result.as_ref() {
        Object::Array(arr) => {
            assert_eq!(arr.len(), 2);
            test_integer(&arr[0], 20);
            test_integer(&arr[1], 30);
        }
        _ => panic!("Expected Array, got {:?}", result),
    }
}

#[test]
fn test_array_slice_start_only() {
    let result = test_eval("[10, 20, 30, 40][2:]");
    match result.as_ref() {
        Object::Array(arr) => {
            assert_eq!(arr.len(), 2);
            test_integer(&arr[0], 30);
            test_integer(&arr[1], 40);
        }
        _ => panic!("Expected Array, got {:?}", result),
    }
}

#[test]
fn test_array_slice_end_only() {
    let result = test_eval("[10, 20, 30, 40][:2]");
    match result.as_ref() {
        Object::Array(arr) => {
            assert_eq!(arr.len(), 2);
            test_integer(&arr[0], 10);
            test_integer(&arr[1], 20);
        }
        _ => panic!("Expected Array, got {:?}", result),
    }
}

#[test]
fn test_array_slice_full() {
    let result = test_eval("[10, 20, 30][:]");
    match result.as_ref() {
        Object::Array(arr) => assert_eq!(arr.len(), 3),
        _ => panic!("Expected Array, got {:?}", result),
    }
}

#[test]
fn test_array_slice_empty_result() {
    let result = test_eval("[10, 20, 30][2:2]");
    match result.as_ref() {
        Object::Array(arr) => assert_eq!(arr.len(), 0),
        _ => panic!("Expected empty Array, got {:?}", result),
    }
}

#[test]
fn test_string_slice() {
    test_string(&test_eval("\"hello world\"[0:5]"), "hello");
}

#[test]
fn test_string_slice_from() {
    test_string(&test_eval("\"hello world\"[6:]"), "world");
}

#[test]
fn test_string_slice_to() {
    test_string(&test_eval("\"hello\"[:3]"), "hel");
}

#[test]
fn test_tuple_slice() {
    let result = test_eval("(10, 20, 30, 40)[1:3]");
    match result.as_ref() {
        Object::Tuple(elements) => {
            assert_eq!(elements.len(), 2);
            test_integer(&elements[0], 20);
            test_integer(&elements[1], 30);
        }
        _ => panic!("Expected Tuple, got {:?}", result),
    }
}

#[test]
fn test_slice_with_variable() {
    let input = r#"
            arr := [1, 2, 3, 4, 5]
            start := 1
            end := 4
            arr[start:end]
        "#;
    let result = test_eval(input);
    match result.as_ref() {
        Object::Array(arr) => {
            assert_eq!(arr.len(), 3);
            test_integer(&arr[0], 2);
            test_integer(&arr[2], 4);
        }
        _ => panic!("Expected Array, got {:?}", result),
    }
}

// ==================== HIDE / SELF / TYPED PARAM TESTS ====================

#[test]
fn test_hidden_field_blocked_externally() {
    let input = r#"
            struct Person {
                hide name <str>
                age <int>
            }
            p <Person>
            p.name
        "#;
    let result = test_eval(input);
    match result.as_ref() {
        Object::Error(msg) => assert!(
            msg.contains("hidden"),
            "Expected hidden error, got: {}",
            msg
        ),
        other => panic!("Expected Error, got {:?}", other),
    }
}

#[test]
fn test_non_hidden_field_accessible() {
    let input = r#"
            struct Person {
                hide name <str>
                age <int>
            }
            p <Person>
            p.age
        "#;
    let result = test_eval(input);
    test_integer(&result, 0);
}

#[test]
fn test_hidden_field_accessible_via_self() {
    let input = r#"
            struct Person {
                hide name <str>
            }
            Person contains {
                fun get_name() { self.name }
            }
            p <Person>
            p.get_name()
        "#;
    let result = test_eval(input);
    test_string(&result, "");
}

#[test]
fn test_self_field_mutation_persists() {
    let input = r#"
            struct Person {
                hide name <str>
            }
            Person contains {
                fun set_name(n <str>) { self.name = n }
                fun get_name() { self.name }
            }
            p <Person>
            p.set_name("Alice")
            p.get_name()
        "#;
    let result = test_eval(input);
    test_string(&result, "Alice");
}

#[test]
fn test_hidden_field_blocks_external_dot_assign() {
    let input = r#"
            struct Person {
                hide name <str>
            }
            p <Person>
            p.name = "test"
        "#;
    let result = test_eval(input);
    match result.as_ref() {
        Object::Error(msg) => assert!(
            msg.contains("hidden"),
            "Expected hidden error, got: {}",
            msg
        ),
        other => panic!("Expected Error, got {:?}", other),
    }
}

#[test]
fn test_typed_param_enforced() {
    let input = r#"
            struct Person {
                hide name <str>
            }
            Person contains {
                fun set_name(n <str>) { self.name = n }
            }
            p <Person>
            p.set_name(42)
        "#;
    let result = test_eval(input);
    match result.as_ref() {
        Object::Error(msg) => assert!(
            msg.contains("type mismatch"),
            "Expected type mismatch error, got: {}",
            msg
        ),
        other => panic!("Expected Error, got {:?}", other),
    }
}

#[test]
fn test_untyped_params_still_work() {
    let input = r#"
            fun add(a, b) { a + b }
            add(3, 4)
        "#;
    let result = test_eval(input);
    test_integer(&result, 7);
}

#[test]
fn test_mixed_typed_untyped_params() {
    let input = r#"
            fun greet(name <str>, times) { name + name }
            greet("hi", 2)
        "#;
    let result = test_eval(input);
    test_string(&result, "hihi");
}

#[test]
fn test_inheritance_with_hide() {
    let input = r#"
            struct Person {
                hide fname <str>
                hide lname <str>
            }
            Person contains {
                fun set_name(fn <str>, ln <str>) {
                    self.fname = fn
                    self.lname = ln
                }
                fun get_fname() { self.fname }
            }
            struct American(Person) {
                state <str>
            }
            a <American>
            a.set_name("John", "Hancock")
            a.state = "Virginia"
            a.get_fname()
        "#;
    let result = test_eval(input);
    test_string(&result, "John");
}

#[test]
fn test_struct_example_runs() {
    let input = r#"
            struct Person {
                hide fname <str>
                hide lname <str>
                hide age <int>
            }
            Person contains {
                fun set_name(fn <str>, ln <str>) {
                    self.fname = fn
                    self.lname = ln
                }
                fun set_age(age <int>) {
                    self.age = age
                }
            }
            struct Nationality(Person) {
                hide country <str>
                hide title <str>
            }
            Nationality contains {
                fun set_country(country <str>) {
                    self.country = country
                }
                fun set_title(title <str>) {
                    self.title = title
                }
            }
            struct American(Nationality) {
                state <str>
            }
            a <American>
            a.set_name("John","Hancock")
            a.set_age(29)
            a.set_country("USA")
            a.set_title("American")
            a.state = "Virginia"
            a.state
        "#;
    let result = test_eval(input);
    test_string(&result, "Virginia");
}

#[test]
fn test_string_interpolation_variable() {
    let input = r#"
            name := "world"
            "hello {name}"
        "#;
    let result = test_eval(input);
    test_string(&result, "hello world");
}

#[test]
fn test_string_interpolation_expression() {
    let input = r#"
            x := 5
            "x is {x + 1}"
        "#;
    let result = test_eval(input);
    test_string(&result, "x is 6");
}

#[test]
fn test_string_interpolation_function_call() {
    let input = r#"
            x := 42
            "the answer is {str(x)}"
        "#;
    let result = test_eval(input);
    test_string(&result, "the answer is 42");
}

#[test]
fn test_string_interpolation_multiple() {
    let input = r#"
            a := "hello"
            b := "world"
            "{a} {b}!"
        "#;
    let result = test_eval(input);
    test_string(&result, "hello world!");
}

#[test]
fn test_string_no_interpolation() {
    let result = test_eval("\"hello world\"");
    test_string(&result, "hello world");
}

#[test]
fn test_string_interpolation_integer() {
    let input = r#"
            x := 10
            y := 14
            "Both {x} and {y} are of type int"
        "#;
    let result = test_eval(input);
    test_string(&result, "Both 10 and 14 are of type int");
}

#[test]
fn test_toml_parse_with_nested_tables() {
    let input = r#"
            introduce toml
            data := toml.parse("title = \"Oxigen\"\nports = [8000, 8001]\ncreated = 1979-05-27T07:32:00Z\n[owner]\nname = \"Javan\"\nactive = true")
            data["owner"]["name"]
        "#;
    let result = test_eval(input);
    test_string(&result, "Javan");
}

#[test]
fn test_toml_parse_datetime_maps_to_string() {
    let input = r#"
            introduce toml
            data := toml.parse("created = 1979-05-27T07:32:00Z")
            data["created"]
        "#;
    let result = test_eval(input);
    test_string(&result, "1979-05-27T07:32:00Z");
}

#[test]
fn test_toml_read_file() {
    let input = r#"
            introduce io
            introduce os
            introduce toml

            path := "/tmp/oxigen_test.toml"
            io.write_file(path, "title = \"Oxigen\"\n[owner]\nname = \"Javan\"")
            data := toml.read(path)
            os.remove_file(path)
            data["owner"]["name"]
        "#;
    let result = test_eval(input);
    test_string(&result, "Javan");
}

#[test]
fn test_toml_parse_error() {
    let input = r#"
            introduce toml
            toml.parse("title = ")
        "#;
    let result = test_eval(input);
    match result.as_ref() {
        Object::Error(msg) => assert!(msg.contains("toml parse error:")),
        _ => panic!("Expected TOML parse error, got {:?}", result),
    }
}

#[test]
fn test_toml_stringify_document_format() {
    let input = r#"
            introduce toml
            data := {"title": "MyApp", "server": {"host": "localhost", "port": 8080}}
            toml.stringify(data)
        "#;
    let result = test_eval(input);
    match result.as_ref() {
        Object::String(s) => {
            assert!(
                s.contains("[server]"),
                "Expected [server] section header, got: {}",
                s
            );
            assert!(
                s.contains("host = \"localhost\""),
                "Expected host key, got: {}",
                s
            );
            assert!(s.contains("port = 8080"), "Expected port key, got: {}", s);
        }
        _ => panic!("Expected STRING, got {:?}", result),
    }
}

#[test]
fn test_toml_stringify_roundtrip() {
    let input = r#"
            introduce toml
            original := {"title": "Test", "server": {"host": "localhost", "port": 8080}}
            reparsed := toml.parse(toml.stringify(original))
            reparsed["server"]["host"]
        "#;
    let result = test_eval(input);
    test_string(&result, "localhost");
}
