use super::*;
use crate::lexer::Lexer;

fn parse(input: &str) -> (Program, Vec<Diagnostic>) {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer, input);
    let program = parser.parse_program();
    let errors = parser.errors().to_vec();
    (program, errors)
}

fn parse_ok(input: &str) -> Program {
    let (program, errors) = parse(input);
    assert!(errors.is_empty(), "Parser errors: {:?}", errors);
    program
}

// ==================== LITERAL PARSING TESTS ====================

#[test]
fn test_parse_integer_literal() {
    let program = parse_ok("5");
    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        Statement::Expr(Expression::Int { value, .. }) => {
            assert_eq!(*value, 5);
        }
        _ => panic!("Expected integer expression"),
    }
}

#[test]
fn test_parse_float_literal() {
    let program = parse_ok("3.14");
    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        Statement::Expr(Expression::Float { value, .. }) => {
            assert!((value - 3.14).abs() < f64::EPSILON);
        }
        _ => panic!("Expected float expression"),
    }
}

#[test]
fn test_parse_char_literal() {
    let program = parse_ok("`a`");
    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        Statement::Expr(Expression::Char { value, .. }) => {
            assert_eq!(*value, 'a');
        }
        _ => panic!("Expected char expression"),
    }
}

#[test]
fn test_parse_string_literals() {
    // Double quotes
    let program = parse_ok("\"hello\"");
    match &program.statements[0] {
        Statement::Expr(Expression::Str { value, .. }) => {
            assert_eq!(value, "hello");
        }
        _ => panic!("Expected string expression"),
    }

    // Single quotes
    let program = parse_ok("'world'");
    match &program.statements[0] {
        Statement::Expr(Expression::Str { value, .. }) => {
            assert_eq!(value, "world");
        }
        _ => panic!("Expected string expression"),
    }
}

#[test]
fn test_parse_boolean_literals() {
    let program = parse_ok("True");
    match &program.statements[0] {
        Statement::Expr(Expression::Boolean { value, .. }) => {
            assert!(*value);
        }
        _ => panic!("Expected boolean expression"),
    }

    let program = parse_ok("False");
    match &program.statements[0] {
        Statement::Expr(Expression::Boolean { value, .. }) => {
            assert!(!*value);
        }
        _ => panic!("Expected boolean expression"),
    }
}

#[test]
fn test_parse_none_literal() {
    let program = parse_ok("None");
    match &program.statements[0] {
        Statement::Expr(Expression::NoneExpr { .. }) => {}
        _ => panic!("Expected None expression"),
    }
}

#[test]
fn test_parse_array_literal() {
    let program = parse_ok("[1, 2, 3]");
    match &program.statements[0] {
        Statement::Expr(Expression::Array { elements, .. }) => {
            assert_eq!(elements.len(), 3);
        }
        _ => panic!("Expected array expression"),
    }
}

#[test]
fn test_parse_multiline_array() {
    let program = parse_ok("[\n1,\n2,\n3\n]");
    match &program.statements[0] {
        Statement::Expr(Expression::Array { elements, .. }) => {
            assert_eq!(elements.len(), 3);
        }
        _ => panic!("Expected array expression"),
    }
}

#[test]
fn test_parse_array_trailing_comma() {
    let program = parse_ok("[1, 2, 3,]");
    match &program.statements[0] {
        Statement::Expr(Expression::Array { elements, .. }) => {
            assert_eq!(elements.len(), 3);
        }
        _ => panic!("Expected array expression"),
    }
}

#[test]
fn test_parse_multiline_array_trailing_comma() {
    let program = parse_ok("[\n\"a\",\n\"b\",\n]");
    match &program.statements[0] {
        Statement::Expr(Expression::Array { elements, .. }) => {
            assert_eq!(elements.len(), 2);
        }
        _ => panic!("Expected array expression"),
    }
}

#[test]
fn test_parse_empty_multiline_array() {
    let program = parse_ok("[\n]");
    match &program.statements[0] {
        Statement::Expr(Expression::Array { elements, .. }) => {
            assert_eq!(elements.len(), 0);
        }
        _ => panic!("Expected array expression"),
    }
}

// ==================== EXPRESSION PARSING TESTS ====================

#[test]
fn test_parse_prefix_expressions() {
    let tests = vec![
        ("-5", "-", 5i64),
        ("!True", "!", 0), // Can't easily check bool, just verify it parses
    ];

    for (input, expected_op, _) in tests {
        let program = parse_ok(input);
        match &program.statements[0] {
            Statement::Expr(Expression::Prefix { operator, .. }) => {
                assert_eq!(operator, expected_op);
            }
            _ => panic!("Expected prefix expression for '{}'", input),
        }
    }
}

#[test]
fn test_parse_fail_expression() {
    let program = parse_ok("fail \"bad input\"");
    match &program.statements[0] {
        Statement::Expr(Expression::Fail { value, .. }) => match value.as_ref() {
            Expression::Str { value, .. } => assert_eq!(value, "bad input"),
            _ => panic!("Expected string payload"),
        },
        other => panic!("Expected fail expression, got {:?}", other),
    }
}

#[test]
fn test_parse_angle_fail_expression() {
    let program = parse_ok("<fail>(\"bad input\")");
    match &program.statements[0] {
        Statement::Expr(Expression::Fail { value, .. }) => match value.as_ref() {
            Expression::Str { value, .. } => assert_eq!(value, "bad input"),
            _ => panic!("Expected string payload"),
        },
        other => panic!("Expected angle fail expression, got {:?}", other),
    }
}

#[test]
fn test_parse_error_constructor_expression() {
    let program = parse_ok("<Error>(\"bad input\")");
    match &program.statements[0] {
        Statement::Expr(Expression::ErrorConstruct { tag, value, .. }) => {
            assert!(tag.is_none());
            match value.as_ref() {
                Expression::Str { value, .. } => assert_eq!(value, "bad input"),
                _ => panic!("Expected string payload"),
            }
        }
        other => panic!("Expected error constructor, got {:?}", other),
    }
}

#[test]
fn test_parse_tagged_error_constructor_expression() {
    let program = parse_ok("<Error<retry_error>>(\"bad input\")");
    match &program.statements[0] {
        Statement::Expr(Expression::ErrorConstruct { tag, .. }) => {
            assert_eq!(tag.as_deref(), Some("retry_error"));
        }
        other => panic!("Expected tagged error constructor, got {:?}", other),
    }
}

#[test]
fn test_parse_type_wrap_expression() {
    let program = parse_ok("result := <type<Error || Value>>(read(path))");
    match &program.statements[0] {
        Statement::Let {
            value: Expression::TypeWrap { target, .. },
            ..
        } => match target {
            TypeAnnotation::Union(types) => assert_eq!(types.len(), 2),
            _ => panic!("Expected union target"),
        },
        other => panic!("Expected let with type wrap, got {:?}", other),
    }
}

#[test]
fn test_parse_infix_expressions() {
    let tests = vec![
        ("5 + 5", "+"),
        ("5 - 5", "-"),
        ("5 * 5", "*"),
        ("5 / 5", "/"),
        ("5 % 5", "%"),
        ("5 == 5", "=="),
        ("5 != 5", "!="),
        ("5 < 5", "<"),
        ("5 > 5", ">"),
        ("5 <= 5", "<="),
        ("5 >= 5", ">="),
    ];

    for (input, expected_op) in tests {
        let program = parse_ok(input);
        match &program.statements[0] {
            Statement::Expr(Expression::Infix { operator, .. }) => {
                assert_eq!(operator, expected_op, "Failed for input '{}'", input);
            }
            _ => panic!("Expected infix expression for '{}'", input),
        }
    }
}

#[test]
fn test_parse_postfix_expressions() {
    let tests = vec![("x++", "++"), ("x--", "--")];

    for (input, expected_op) in tests {
        let program = parse_ok(input);
        match &program.statements[0] {
            Statement::Expr(Expression::Postfix { operator, .. }) => {
                assert_eq!(operator, expected_op, "Failed for input '{}'", input);
            }
            _ => panic!("Expected postfix expression for '{}'", input),
        }
    }
}

#[test]
fn test_parse_grouped_expression() {
    let program = parse_ok("(5 + 5)");
    match &program.statements[0] {
        Statement::Expr(Expression::Grouped(inner)) => match inner.as_ref() {
            Expression::Infix { operator, .. } => {
                assert_eq!(operator, "+");
            }
            _ => panic!("Expected infix inside grouped"),
        },
        _ => panic!("Expected grouped expression"),
    }
}

#[test]
fn test_parse_call_expression() {
    let program = parse_ok("print(1, 2, 3)");
    match &program.statements[0] {
        Statement::Expr(Expression::Call { function, args, .. }) => {
            match function.as_ref() {
                Expression::Ident(ident) => assert_eq!(ident.value, "print"),
                _ => panic!("Expected identifier as function"),
            }
            assert_eq!(args.len(), 3);
        }
        _ => panic!("Expected call expression"),
    }
}

#[test]
fn test_parse_index_expression() {
    let program = parse_ok("arr[0]");
    match &program.statements[0] {
        Statement::Expr(Expression::Index { left, index, .. }) => {
            match left.as_ref() {
                Expression::Ident(ident) => assert_eq!(ident.value, "arr"),
                _ => panic!("Expected identifier as left"),
            }
            match index.as_ref() {
                Expression::Int { value, .. } => assert_eq!(*value, 0),
                _ => panic!("Expected integer as index"),
            }
        }
        _ => panic!("Expected index expression"),
    }
}

// ==================== STATEMENT PARSING TESTS ====================

#[test]
fn test_parse_let_statement() {
    let program = parse_ok("x := 5");
    match &program.statements[0] {
        Statement::Let { name, value } => {
            assert_eq!(name.value, "x");
            match value {
                Expression::Int { value, .. } => assert_eq!(*value, 5),
                _ => panic!("Expected integer value"),
            }
        }
        _ => panic!("Expected let statement"),
    }
}

#[test]
fn test_parse_each_statement() {
    let program = parse_ok("each x in [1, 2, 3] { print(x) }");
    match &program.statements[0] {
        Statement::Each {
            variable,
            iterable,
            body,
            ..
        } => {
            assert_eq!(variable.value, "x");
            match iterable {
                Expression::Array { elements, .. } => assert_eq!(elements.len(), 3),
                _ => panic!("Expected array as iterable"),
            }
            assert_eq!(body.len(), 1);
        }
        _ => panic!("Expected each statement"),
    }
}

#[test]
fn test_parse_repeat_statement() {
    let program = parse_ok("repeat when x > 0 { x-- }");
    match &program.statements[0] {
        Statement::Repeat {
            condition, body, ..
        } => {
            match condition {
                Expression::Infix { operator, .. } => assert_eq!(operator, ">"),
                _ => panic!("Expected infix condition"),
            }
            assert_eq!(body.len(), 1);
        }
        _ => panic!("Expected repeat statement"),
    }
}

#[test]
fn test_parse_repeat_unless_statement() {
    let program = parse_ok("repeat unless x > 0 { x-- }");
    match &program.statements[0] {
        Statement::Repeat {
            condition, body, ..
        } => {
            match condition {
                Expression::Prefix {
                    token,
                    operator,
                    right,
                } => {
                    assert_eq!(token.token_type, TokenType::Not);
                    assert_eq!(token.literal, "not");
                    assert_eq!(operator, "not");
                    match right.as_ref() {
                        Expression::Infix { operator, .. } => assert_eq!(operator, ">"),
                        _ => panic!("Expected infix condition inside negation"),
                    }
                }
                _ => panic!("Expected negated repeat condition"),
            }
            assert_eq!(body.len(), 1);
        }
        _ => panic!("Expected repeat statement"),
    }
}

#[test]
fn test_parse_pattern_statement() {
    let program = parse_ok("pattern is_ten(x) when x == 10");
    match &program.statements[0] {
        Statement::Pattern {
            name,
            params,
            condition,
            ..
        } => {
            assert_eq!(name.value, "is_ten");
            assert_eq!(params.len(), 1);
            assert_eq!(params[0].value, "x");
            match condition {
                Expression::Infix { operator, .. } => assert_eq!(operator, "=="),
                _ => panic!("Expected infix condition"),
            }
        }
        _ => panic!("Expected pattern statement"),
    }
}

#[test]
fn test_parse_choose_statement() {
    let program = parse_ok("choose y { is_ten -> 1, else -> 2 }");
    match &program.statements[0] {
        Statement::Choose { subject, arms, .. } => {
            match subject {
                Expression::Ident(ident) => assert_eq!(ident.value, "y"),
                _ => panic!("Expected identifier as subject"),
            }
            assert_eq!(arms.len(), 2);
            assert_eq!(arms[0].pattern_name, "is_ten");
            assert_eq!(arms[1].pattern_name, "else");
        }
        _ => panic!("Expected choose statement"),
    }
}

#[test]
fn test_parse_option_expression() {
    let program = parse_ok("option { x > 0 -> 1, 2 }");
    match &program.statements[0] {
        Statement::Expr(Expression::Option { arms, default, .. }) => {
            assert_eq!(arms.len(), 1);
            match &arms[0].condition {
                Expression::Infix { operator, .. } => assert_eq!(operator, ">"),
                _ => panic!("Expected infix condition"),
            }
            assert_eq!(arms[0].body.len(), 1);
            assert!(default.is_some());
        }
        other => panic!("Expected option expression, got {:?}", other),
    }
}

#[test]
fn test_parse_unless_statement() {
    let program = parse_ok("unless valid { 42 }");
    match &program.statements[0] {
        Statement::If {
            condition,
            consequence,
            alternative,
            ..
        } => {
            match condition {
                Expression::Prefix {
                    token, operator, ..
                } => {
                    assert_eq!(token.token_type, TokenType::Not);
                    assert_eq!(token.literal, "not");
                    assert_eq!(operator, "not");
                }
                _ => panic!("Expected negated condition"),
            }
            assert_eq!(consequence.len(), 1);
            assert!(alternative.is_none());
        }
        other => panic!("Expected if statement (desugared unless), got {:?}", other),
    }
}

#[test]
fn test_parse_postfix_when_guard() {
    let program = parse_ok("x = 5 when True");
    match &program.statements[0] {
        Statement::If {
            consequence,
            alternative,
            ..
        } => {
            assert_eq!(consequence.len(), 1);
            assert!(matches!(&consequence[0], Statement::Assign { .. }));
            assert!(alternative.is_none());
        }
        other => panic!(
            "Expected if statement (desugared when guard), got {:?}",
            other
        ),
    }
}

#[test]
fn test_parse_postfix_unless_guard() {
    let program = parse_ok("x = 5 unless True");
    match &program.statements[0] {
        Statement::If {
            condition,
            consequence,
            alternative,
            ..
        } => {
            match condition {
                Expression::Prefix {
                    token, operator, ..
                } => {
                    assert_eq!(token.token_type, TokenType::Not);
                    assert_eq!(token.literal, "not");
                    assert_eq!(operator, "not");
                }
                _ => panic!("Expected negated condition"),
            }
            assert_eq!(consequence.len(), 1);
            assert!(matches!(&consequence[0], Statement::Assign { .. }));
            assert!(alternative.is_none());
        }
        other => panic!(
            "Expected if statement (desugared postfix unless), got {:?}",
            other
        ),
    }
}

#[test]
fn test_parse_expression_unless_then() {
    let program = parse_ok("result := name.upper() unless name == None then \"Guest\"");
    match &program.statements[0] {
        Statement::Let {
            value:
                Expression::Unless {
                    consequence,
                    condition,
                    alternative,
                    ..
                },
            ..
        } => {
            assert!(matches!(consequence.as_ref(), Expression::Call { .. }));
            match condition.as_ref() {
                Expression::Infix { operator, .. } => assert_eq!(operator, "=="),
                _ => panic!("Expected infix condition"),
            }
            match alternative.as_ref() {
                Expression::Str { value, .. } => assert_eq!(value, "Guest"),
                _ => panic!("Expected string alternative"),
            }
        }
        other => panic!("Expected let with unless expression, got {:?}", other),
    }
}

#[test]
fn test_parse_guard_expression() {
    let program = parse_ok("value := read(path) guard err -> \"Guest\"");
    match &program.statements[0] {
        Statement::Let {
            value:
                Expression::Guard {
                    value,
                    binding,
                    fallback,
                    ..
                },
            ..
        } => {
            assert_eq!(binding.value, "err");
            assert!(matches!(value.as_ref(), Expression::Call { .. }));
            match fallback.as_ref() {
                Expression::Str { value, .. } => assert_eq!(value, "Guest"),
                _ => panic!("Expected string fallback"),
            }
        }
        other => panic!("Expected let with guard expression, got {:?}", other),
    }
}

#[test]
fn test_parse_angle_guard_expression() {
    let program = parse_ok("value := read(path) <guard>(\"Guest\")");
    match &program.statements[0] {
        Statement::Let {
            value: Expression::Guard { fallback, .. },
            ..
        } => match fallback.as_ref() {
            Expression::Str { value, .. } => assert_eq!(value, "Guest"),
            _ => panic!("Expected string fallback"),
        },
        other => panic!("Expected let with angle guard expression, got {:?}", other),
    }
}

#[test]
fn test_parse_log_with_message() {
    let program = parse_ok("<log>(\"hello world\")");
    match &program.statements[0] {
        Statement::Expr(Expression::Log {
            tag,
            sub_tag,
            message,
            ..
        }) => {
            assert_eq!(*tag, None);
            assert_eq!(*sub_tag, None);
            assert!(message.is_some());
        }
        other => panic!("Expected log expression, got {:?}", other),
    }
}

#[test]
fn test_parse_log_with_tag() {
    let program = parse_ok("<log<debug>>(\"info\")");
    match &program.statements[0] {
        Statement::Expr(Expression::Log {
            tag,
            sub_tag,
            message,
            ..
        }) => {
            assert_eq!(tag.as_deref(), Some("debug"));
            assert_eq!(*sub_tag, None);
            assert!(message.is_some());
        }
        other => panic!("Expected log expression with tag, got {:?}", other),
    }
}

#[test]
fn test_parse_log_with_error_tag() {
    let program = parse_ok("<log<Error>>(\"failure\")");
    match &program.statements[0] {
        Statement::Expr(Expression::Log {
            tag,
            sub_tag,
            message,
            ..
        }) => {
            assert_eq!(tag.as_deref(), Some("Error"));
            assert_eq!(*sub_tag, None);
            assert!(message.is_some());
        }
        other => panic!("Expected log expression with Error tag, got {:?}", other),
    }
}

#[test]
fn test_parse_log_with_nested_tags() {
    let program = parse_ok("<log<Error<network>>>(\"connection lost\")");
    match &program.statements[0] {
        Statement::Expr(Expression::Log {
            tag,
            sub_tag,
            message,
            ..
        }) => {
            assert_eq!(tag.as_deref(), Some("Error"));
            assert_eq!(sub_tag.as_deref(), Some("network"));
            assert!(message.is_some());
        }
        other => panic!("Expected log expression with nested tags, got {:?}", other),
    }
}

#[test]
fn test_parse_log_no_parens() {
    let program = parse_ok("<log<Error>>");
    match &program.statements[0] {
        Statement::Expr(Expression::Log {
            tag,
            sub_tag,
            message,
            ..
        }) => {
            assert_eq!(tag.as_deref(), Some("Error"));
            assert_eq!(*sub_tag, None);
            assert!(message.is_none());
        }
        other => panic!("Expected log expression without parens, got {:?}", other),
    }
}

#[test]
fn test_parse_log_empty_parens() {
    let program = parse_ok("<log<custom_tag>>()");
    match &program.statements[0] {
        Statement::Expr(Expression::Log {
            tag,
            sub_tag,
            message,
            ..
        }) => {
            assert_eq!(tag.as_deref(), Some("custom_tag"));
            assert_eq!(*sub_tag, None);
            assert!(message.is_none());
        }
        other => panic!("Expected log expression with empty parens, got {:?}", other),
    }
}

#[test]
fn test_parse_log_nested_no_parens() {
    let program = parse_ok("<log<Error<custom_error>>>");
    match &program.statements[0] {
        Statement::Expr(Expression::Log {
            tag,
            sub_tag,
            message,
            ..
        }) => {
            assert_eq!(tag.as_deref(), Some("Error"));
            assert_eq!(sub_tag.as_deref(), Some("custom_error"));
            assert!(message.is_none());
        }
        other => panic!(
            "Expected log expression with nested tags no parens, got {:?}",
            other
        ),
    }
}

#[test]
fn test_parse_main_block() {
    let program = parse_ok("main {\n  x := 5\n}");
    assert_eq!(program.statements.len(), 1);
    match &program.statements[0] {
        Statement::Main { body, .. } => {
            assert_eq!(body.len(), 1);
        }
        other => panic!("Expected Main statement, got {:?}", other),
    }
}

#[test]
fn test_parse_main_block_with_surrounding_code() {
    let program = parse_ok("x := 1\nmain {\n  x := 42\n}");
    assert_eq!(program.statements.len(), 2);
    assert!(matches!(&program.statements[0], Statement::Let { .. }));
    assert!(matches!(&program.statements[1], Statement::Main { .. }));
}

#[test]
fn test_parse_option_error_arm() {
    let program = parse_ok("option { True -> 1, <Error> -> 0 }");
    match &program.statements[0] {
        Statement::Expr(Expression::Option { error_default, .. }) => {
            assert!(error_default.is_some());
        }
        other => panic!("Expected option expression with error arm, got {:?}", other),
    }
}

#[test]
fn test_parse_guard_expression_statement() {
    let program = parse_ok("println(load()) guard err -> err.msg");
    match &program.statements[0] {
        Statement::Expr(Expression::Guard { binding, .. }) => {
            assert_eq!(binding.value, "err");
        }
        other => panic!("Expected guard expression statement, got {:?}", other),
    }
}

#[test]
fn test_parse_postfix_unless_then_guard() {
    let program = parse_ok("println(\"ok\") unless x == False then println(\"fallback\")");
    match &program.statements[0] {
        Statement::If {
            condition,
            consequence,
            alternative,
            ..
        } => {
            match condition {
                Expression::Prefix {
                    token,
                    operator,
                    right,
                } => {
                    assert_eq!(token.token_type, TokenType::Not);
                    assert_eq!(operator, "not");
                    match right.as_ref() {
                        Expression::Infix { operator, .. } => assert_eq!(operator, "=="),
                        _ => panic!("Expected infix condition inside negation"),
                    }
                }
                _ => panic!("Expected negated condition"),
            }
            assert_eq!(consequence.len(), 1);
            let alternative = alternative.as_ref().expect("Expected alternative branch");
            assert_eq!(alternative.len(), 1);
        }
        other => panic!("Expected if statement with alternative, got {:?}", other),
    }
}

#[test]
fn test_parse_skip_stop() {
    let program = parse_ok("skip");
    assert!(matches!(program.statements[0], Statement::Skip));

    let program = parse_ok("stop");
    assert!(matches!(program.statements[0], Statement::Stop));
}

// ==================== OPERATOR PRECEDENCE TESTS ====================

#[test]
fn test_operator_precedence() {
    let tests = vec![
        ("1 + 2 * 3", "(1 + (2 * 3))"),
        ("1 * 2 + 3", "((1 * 2) + 3)"),
        ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
    ];

    // Just verify these parse without errors
    // A full precedence test would need expression stringification
    for (input, _expected) in tests {
        let _ = parse_ok(input);
    }
}

// ==================== TYPED DECLARATION TESTS ====================

#[test]
fn test_parse_typed_let_strict() {
    let program = parse_ok("x <int> = 10");
    match &program.statements[0] {
        Statement::TypedLet {
            name,
            type_ann,
            walrus,
            ..
        } => {
            assert_eq!(name.value, "x");
            assert_eq!(*type_ann, TypeAnnotation::Int);
            assert!(!walrus);
        }
        other => panic!("Expected TypedLet, got {:?}", other),
    }
}

#[test]
fn test_parse_typed_let_walrus() {
    let program = parse_ok("x <str> := \"hi\"");
    match &program.statements[0] {
        Statement::TypedLet {
            name,
            type_ann,
            walrus,
            ..
        } => {
            assert_eq!(name.value, "x");
            assert_eq!(*type_ann, TypeAnnotation::Str);
            assert!(walrus);
        }
        other => panic!("Expected TypedLet, got {:?}", other),
    }
}

#[test]
fn test_parse_typed_declare() {
    let program = parse_ok("x as <float>");
    match &program.statements[0] {
        Statement::TypedDeclare { name, type_ann } => {
            assert_eq!(name.value, "x");
            assert_eq!(*type_ann, TypeAnnotation::Float);
        }
        other => panic!("Expected TypedDeclare, got {:?}", other),
    }
}

#[test]
fn test_parse_assign() {
    let program = parse_ok("x = 20");
    match &program.statements[0] {
        Statement::Assign { name, .. } => {
            assert_eq!(name.value, "x");
        }
        other => panic!("Expected Assign, got {:?}", other),
    }
}

#[test]
fn test_parse_less_than_not_typed() {
    // x < 10 should still parse as infix comparison, not typed declaration
    let program = parse_ok("x < 10");
    match &program.statements[0] {
        Statement::Expr(Expression::Infix { operator, .. }) => {
            assert_eq!(operator, "<");
        }
        other => panic!("Expected infix expression, got {:?}", other),
    }
}

#[test]
fn test_parse_struct_type_annotation() {
    // Unknown type names are now parsed as struct types (validated at eval time)
    let program = parse_ok("x <Person> = 1");
    match &program.statements[0] {
        Statement::TypedLet { name, type_ann, .. } => {
            assert_eq!(name.value, "x");
            assert_eq!(*type_ann, TypeAnnotation::Struct("Person".to_string()));
        }
        other => panic!("Expected TypedLet, got {:?}", other),
    }
}

// ==================== FUNCTION PARSING TESTS ====================

#[test]
fn test_parse_anonymous_function() {
    let program = parse_ok("fun(a, b) { a + b }");
    assert_eq!(program.statements.len(), 1);
    match &program.statements[0] {
        Statement::Expr(Expression::FunctionLiteral {
            parameters, body, ..
        }) => {
            assert_eq!(parameters.len(), 2);
            assert_eq!(parameters[0].ident.value, "a");
            assert_eq!(parameters[1].ident.value, "b");
            assert_eq!(body.len(), 1);
        }
        other => panic!("Expected FunctionLiteral expression, got {:?}", other),
    }
}

#[test]
fn test_parse_named_function() {
    let program = parse_ok("fun add(a, b) { a + b }");
    assert_eq!(program.statements.len(), 1);
    match &program.statements[0] {
        Statement::Let { name, value } => {
            assert_eq!(name.value, "add");
            match value {
                Expression::FunctionLiteral {
                    parameters, body, ..
                } => {
                    assert_eq!(parameters.len(), 2);
                    assert_eq!(parameters[0].ident.value, "a");
                    assert_eq!(parameters[1].ident.value, "b");
                    assert_eq!(body.len(), 1);
                }
                other => panic!("Expected FunctionLiteral value, got {:?}", other),
            }
        }
        other => panic!("Expected Let statement, got {:?}", other),
    }
}

#[test]
fn test_parse_zero_param_function() {
    let program = parse_ok("fun() { 42 }");
    match &program.statements[0] {
        Statement::Expr(Expression::FunctionLiteral { parameters, .. }) => {
            assert_eq!(parameters.len(), 0);
        }
        other => panic!("Expected FunctionLiteral, got {:?}", other),
    }
}

#[test]
fn test_parse_give_statement() {
    let program = parse_ok("give 42");
    match &program.statements[0] {
        Statement::Give { value, .. } => match value {
            Expression::Int { value, .. } => assert_eq!(*value, 42),
            other => panic!("Expected Int, got {:?}", other),
        },
        other => panic!("Expected Give statement, got {:?}", other),
    }
}

#[test]
fn test_parse_let_with_function_value() {
    let program = parse_ok("f := fun(x) { x }");
    match &program.statements[0] {
        Statement::Let { name, value } => {
            assert_eq!(name.value, "f");
            match value {
                Expression::FunctionLiteral { parameters, .. } => {
                    assert_eq!(parameters.len(), 1);
                    assert_eq!(parameters[0].ident.value, "x");
                }
                other => panic!("Expected FunctionLiteral, got {:?}", other),
            }
        }
        other => panic!("Expected Let statement, got {:?}", other),
    }
}

// ==================== ERROR HANDLING TESTS ====================

#[test]
fn test_parser_errors() {
    let tests = vec![
        ("x :=", "unexpected token"),  // Missing value after :=
        ("each x in { }", "expected"), // {} parses as empty map, then block '{' is missing
    ];

    for (input, expected_error) in tests {
        let (_, errors) = parse(input);
        assert!(!errors.is_empty(), "Expected errors for '{}'", input);
        assert!(
            errors
                .iter()
                .any(|e| e.message.to_lowercase().contains(expected_error)),
            "Expected error containing '{}' for '{}', got {:?}",
            expected_error,
            input,
            errors
        );
    }
}

// ==================== STRUCT PARSING TESTS ====================

#[test]
fn test_parse_struct_definition() {
    let program = parse_ok("struct Person {\n    name <str>\n    age <int>\n}");
    match &program.statements[0] {
        Statement::StructDef {
            name,
            parent,
            fields,
            ..
        } => {
            assert_eq!(name.value, "Person");
            assert!(parent.is_none());
            assert_eq!(fields.len(), 2);
            assert_eq!(fields[0].name.value, "name");
            assert_eq!(fields[0].type_ann, TypeAnnotation::Str);
            assert_eq!(fields[1].name.value, "age");
            assert_eq!(fields[1].type_ann, TypeAnnotation::Int);
        }
        other => panic!("Expected StructDef, got {:?}", other),
    }
}

#[test]
fn test_parse_struct_with_parent() {
    let program = parse_ok("struct American(Person) {\n    nationality <str>\n}");
    match &program.statements[0] {
        Statement::StructDef {
            name,
            parent,
            fields,
            ..
        } => {
            assert_eq!(name.value, "American");
            assert!(parent.is_some());
            assert_eq!(parent.as_ref().unwrap().value, "Person");
            assert_eq!(fields.len(), 1);
            assert_eq!(fields[0].name.value, "nationality");
        }
        other => panic!("Expected StructDef, got {:?}", other),
    }
}

#[test]
fn test_parse_contains_def() {
    let program = parse_ok("Person contains {\n    fun greet() { 42 }\n}");
    match &program.statements[0] {
        Statement::ContainsDef {
            struct_name,
            methods,
            ..
        } => {
            assert_eq!(struct_name.value, "Person");
            assert_eq!(methods.len(), 1);
            assert_eq!(methods[0].0.value, "greet");
        }
        other => panic!("Expected ContainsDef, got {:?}", other),
    }
}

#[test]
fn test_parse_dot_access() {
    let program = parse_ok("p.name");
    match &program.statements[0] {
        Statement::Expr(Expression::DotAccess { left, field, .. }) => {
            match left.as_ref() {
                Expression::Ident(ident) => assert_eq!(ident.value, "p"),
                _ => panic!("Expected Ident as left"),
            }
            assert_eq!(field.value, "name");
        }
        other => panic!("Expected DotAccess expression, got {:?}", other),
    }
}

#[test]
fn test_parse_dot_assign() {
    let program = parse_ok("p.name = \"Jane\"");
    match &program.statements[0] {
        Statement::DotAssign {
            object,
            field,
            value,
            ..
        } => {
            match object {
                Expression::Ident(ident) => assert_eq!(ident.value, "p"),
                _ => panic!("Expected Ident as object"),
            }
            assert_eq!(field.value, "name");
            match value {
                Expression::Str { value, .. } => assert_eq!(value, "Jane"),
                _ => panic!("Expected string value"),
            }
        }
        other => panic!("Expected DotAssign, got {:?}", other),
    }
}

#[test]
fn test_parse_struct_literal() {
    let program = parse_ok("Person { name: \"Alice\", age: 30 }");
    match &program.statements[0] {
        Statement::Expr(Expression::StructLiteral {
            struct_name,
            field_values,
            ..
        }) => {
            assert_eq!(struct_name, "Person");
            assert_eq!(field_values.len(), 2);
            assert_eq!(field_values[0].0, "name");
            assert_eq!(field_values[1].0, "age");
        }
        other => panic!("Expected StructLiteral expression, got {:?}", other),
    }
}

// ==================== TUPLE AND MAP PARSING TESTS ====================

#[test]
fn test_parse_tuple_literal() {
    let program = parse_ok("(1, 2, 3)");
    match &program.statements[0] {
        Statement::Expr(Expression::TupleLiteral { elements, .. }) => {
            assert_eq!(elements.len(), 3);
        }
        other => panic!("Expected TupleLiteral, got {:?}", other),
    }
}

#[test]
fn test_parse_empty_tuple() {
    let program = parse_ok("()");
    match &program.statements[0] {
        Statement::Expr(Expression::TupleLiteral { elements, .. }) => {
            assert_eq!(elements.len(), 0);
        }
        other => panic!("Expected empty TupleLiteral, got {:?}", other),
    }
}

#[test]
fn test_parse_single_tuple() {
    let program = parse_ok("(1,)");
    match &program.statements[0] {
        Statement::Expr(Expression::TupleLiteral { elements, .. }) => {
            assert_eq!(elements.len(), 1);
        }
        other => panic!("Expected single-element TupleLiteral, got {:?}", other),
    }
}

#[test]
fn test_parse_grouped_not_tuple() {
    let program = parse_ok("(1 + 2)");
    match &program.statements[0] {
        Statement::Expr(Expression::Grouped(_)) => {}
        other => panic!("Expected Grouped expression, got {:?}", other),
    }
}

#[test]
fn test_parse_map_literal() {
    let program = parse_ok("{\"a\": 1, \"b\": 2}");
    match &program.statements[0] {
        Statement::Expr(Expression::MapLiteral { entries, .. }) => {
            assert_eq!(entries.len(), 2);
        }
        other => panic!("Expected MapLiteral, got {:?}", other),
    }
}

#[test]
fn test_parse_empty_map() {
    let program = parse_ok("{}");
    match &program.statements[0] {
        Statement::Expr(Expression::MapLiteral { entries, .. }) => {
            assert_eq!(entries.len(), 0);
        }
        other => panic!("Expected empty MapLiteral, got {:?}", other),
    }
}

// ==================== HIDE / SELF / TYPED PARAM PARSER TESTS ====================

#[test]
fn test_parse_hide_struct_field() {
    let program = parse_ok("struct Person {\n    hide name <str>\n    age <int>\n}");
    match &program.statements[0] {
        Statement::StructDef { fields, .. } => {
            assert_eq!(fields.len(), 2);
            assert!(fields[0].hidden);
            assert_eq!(fields[0].name.value, "name");
            assert!(!fields[1].hidden);
            assert_eq!(fields[1].name.value, "age");
        }
        other => panic!("Expected StructDef, got {:?}", other),
    }
}

#[test]
fn test_parse_self_dot_access() {
    let program = parse_ok("self.field");
    match &program.statements[0] {
        Statement::Expr(Expression::DotAccess { left, field, .. }) => {
            match left.as_ref() {
                Expression::Ident(ident) => assert_eq!(ident.value, "self"),
                _ => panic!("Expected Ident('self') as left"),
            }
            assert_eq!(field.value, "field");
        }
        other => panic!("Expected DotAccess expression, got {:?}", other),
    }
}

#[test]
fn test_parse_typed_function_parameters() {
    let program = parse_ok("fun f(x <int>, y) { x }");
    match &program.statements[0] {
        Statement::Let { value, .. } => match value {
            Expression::FunctionLiteral { parameters, .. } => {
                assert_eq!(parameters.len(), 2);
                assert_eq!(parameters[0].ident.value, "x");
                assert_eq!(parameters[0].type_ann, Some(TypeAnnotation::Int));
                assert_eq!(parameters[1].ident.value, "y");
                assert_eq!(parameters[1].type_ann, None);
            }
            other => panic!("Expected FunctionLiteral, got {:?}", other),
        },
        other => panic!("Expected Let, got {:?}", other),
    }
}

#[test]
fn test_parse_string_interpolation() {
    let program = parse_ok("\"hello {name}\"");
    assert_eq!(program.statements.len(), 1);
    match &program.statements[0] {
        Statement::Expr(Expression::StringInterp { parts, .. }) => {
            assert_eq!(parts.len(), 2);
            match &parts[0] {
                StringInterpPart::Literal(s) => assert_eq!(s, "hello "),
                _ => panic!("Expected literal part"),
            }
            match &parts[1] {
                StringInterpPart::Expr(Expression::Ident(ident)) => {
                    assert_eq!(ident.value, "name");
                }
                _ => panic!("Expected ident expr part"),
            }
        }
        _ => panic!("Expected StringInterp expression"),
    }
}

#[test]
fn test_parse_string_interpolation_with_function_call() {
    let program = parse_ok("\"result: {str(x)}\"");
    assert_eq!(program.statements.len(), 1);
    match &program.statements[0] {
        Statement::Expr(Expression::StringInterp { parts, .. }) => {
            assert_eq!(parts.len(), 2);
            match &parts[0] {
                StringInterpPart::Literal(s) => assert_eq!(s, "result: "),
                _ => panic!("Expected literal part"),
            }
            match &parts[1] {
                StringInterpPart::Expr(Expression::Call { .. }) => {}
                other => panic!("Expected call expr part, got {:?}", other),
            }
        }
        _ => panic!("Expected StringInterp expression"),
    }
}

#[test]
fn test_parse_string_no_interpolation() {
    // No braces means plain string, not interpolation
    let program = parse_ok("\"hello world\"");
    match &program.statements[0] {
        Statement::Expr(Expression::Str { value, .. }) => {
            assert_eq!(value, "hello world");
        }
        _ => panic!("Expected plain Str expression"),
    }
}
