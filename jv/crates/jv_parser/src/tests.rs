use super::*;

use test_case::test_case;

#[test]
fn test_simple_val_declaration() {
    let input = "val name = \"hello\"";
    let result = Parser::parse(input).unwrap();

    // Expected: val declaration statement
    assert_eq!(result.statements.len(), 1);
    match &result.statements[0] {
        Statement::ValDeclaration {
            name, initializer, ..
        } => {
            assert_eq!(name, "name");
            match initializer {
                Expression::Literal(Literal::String(s), _) => {
                    assert_eq!(s, "hello");
                }
                _ => panic!("Expected string literal"),
            }
        }
        _ => panic!("Expected val declaration"),
    }
}

#[test]
fn test_function_declaration() {
    let input = r#"
        fun add(a: Int, b: Int): Int {
            return a + b
        }
    "#;
    let result = Parser::parse(input).unwrap();

    assert_eq!(result.statements.len(), 1);
    match &result.statements[0] {
        Statement::FunctionDeclaration {
            name,
            parameters,
            return_type,
            body,
            ..
        } => {
            assert_eq!(name, "add");
            assert_eq!(parameters.len(), 2);
            assert_eq!(parameters[0].name, "a");
            assert_eq!(parameters[1].name, "b");
            assert!(return_type.is_some());

            // Check function body
            match body.as_ref() {
                Expression::Block { statements, .. } => {
                    assert_eq!(statements.len(), 1);
                    match &statements[0] {
                        Statement::Return { .. } => {}
                        _ => panic!("Expected return statement"),
                    }
                }
                _ => panic!("Expected block expression"),
            }
        }
        _ => panic!("Expected function declaration"),
    }
}

#[test]
fn test_data_class_declaration() {
    let input = "data class User(val name: String, var age: Int)";
    let result = Parser::parse(input).unwrap();

    assert_eq!(result.statements.len(), 1);
    match &result.statements[0] {
        Statement::DataClassDeclaration {
            name,
            parameters,
            is_mutable,
            ..
        } => {
            assert_eq!(name, "User");
            assert_eq!(parameters.len(), 2);
            assert_eq!(parameters[0].name, "name");
            assert_eq!(parameters[1].name, "age");
            assert!(!is_mutable); // Data class is immutable by default
        }
        _ => panic!("Expected data class declaration"),
    }
}

#[test]
fn test_when_expression() {
    let input = r#"
        when (x) {
            0 -> "zero"
            1 -> "one"
            else -> "other"
        }
    "#;
    let result = Parser::parse(input).unwrap();

    assert_eq!(result.statements.len(), 1);
    match &result.statements[0] {
        Statement::Expression { expr, .. } => match expr {
            Expression::When {
                expr: subject,
                arms,
                else_arm,
                ..
            } => {
                assert!(subject.is_some());
                assert_eq!(arms.len(), 2);
                assert!(else_arm.is_some());
            }
            _ => panic!("Expected when expression"),
        },
        _ => panic!("Expected expression statement"),
    }
}

#[test]
fn test_null_safety_operators() {
    let input = "val length = user?.name?.length ?: 0";
    let result = Parser::parse(input).unwrap();

    assert_eq!(result.statements.len(), 1);
    match &result.statements[0] {
        Statement::ValDeclaration { initializer, .. } => {
            match initializer {
                Expression::Binary {
                    left, op, right, ..
                } => {
                    assert_eq!(*op, BinaryOp::Elvis);

                    // Left side should be null-safe member access chain
                    match left.as_ref() {
                        Expression::NullSafeMemberAccess { .. } => {}
                        _ => panic!("Expected null-safe member access"),
                    }

                    // Right side should be literal 0
                    match right.as_ref() {
                        Expression::Literal(Literal::Number(n), _) => {
                            assert_eq!(n, "0");
                        }
                        _ => panic!("Expected integer literal 0"),
                    }
                }
                _ => panic!("Expected binary expression with elvis operator"),
            }
        }
        _ => panic!("Expected val declaration"),
    }
}

#[test]
fn test_null_safe_index_access() {
    let input = "val value = items?[index]";
    let result = Parser::parse(input).unwrap();

    assert_eq!(result.statements.len(), 1);
    match &result.statements[0] {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::NullSafeIndexAccess { object, index, .. } => {
                match object.as_ref() {
                    Expression::Identifier(name, _) => assert_eq!(name, "items"),
                    _ => panic!("Expected identifier 'items'"),
                }
                match index.as_ref() {
                    Expression::Identifier(name, _) => assert_eq!(name, "index"),
                    _ => panic!("Expected identifier 'index'"),
                }
            }
            _ => panic!("Expected null-safe index access"),
        },
        _ => panic!("Expected val declaration"),
    }
}

#[test]
fn test_string_interpolation() {
    let input = r#"println("Hello, ${name}!")"#;
    let result = Parser::parse(input).unwrap();

    assert_eq!(result.statements.len(), 1);
    match &result.statements[0] {
        Statement::Expression { expr, .. } => match expr {
            Expression::Call { function, args, .. } => {
                match function.as_ref() {
                    Expression::Identifier(name, _) => {
                        assert_eq!(name, "println");
                    }
                    _ => panic!("Expected println function"),
                }

                assert_eq!(args.len(), 1);
                match &args[0] {
                    Argument::Positional(Expression::StringInterpolation { parts, .. }) => {
                        assert!(parts.len() >= 2); // Should have text and interpolated parts
                    }
                    _ => panic!("Expected string interpolation"),
                }
            }
            _ => panic!("Expected function call"),
        },
        _ => panic!("Expected expression statement"),
    }
}

#[test]
fn test_string_interpolation_leading_expression() {
    let input = r#"val message = "${name}!""#;
    let result = Parser::parse(input);

    assert!(result.is_ok());
    let program = result.unwrap();
    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::StringInterpolation { parts, .. } => {
                assert_eq!(parts.len(), 2);
                match &parts[0] {
                    StringPart::Expression(Expression::Identifier(name, _)) => {
                        assert_eq!(name, "name");
                    }
                    other => panic!("Expected identifier interpolation, found {:?}", other),
                }
                match &parts[1] {
                    StringPart::Text(s) => assert_eq!(s, "!"),
                    other => panic!("Expected trailing text part, found {:?}", other),
                }
            }
            other => panic!("Expected string interpolation, found {:?}", other),
        },
        other => panic!("Expected val declaration, found {:?}", other),
    }
}

#[test]
fn test_string_interpolation_multiple_expressions_no_text() {
    let input = r#"val message = "${first}${last}""#;
    let result = Parser::parse(input).unwrap();

    assert_eq!(result.statements.len(), 1);
    match &result.statements[0] {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::StringInterpolation { parts, .. } => {
                assert_eq!(parts.len(), 2);
                match &parts[0] {
                    StringPart::Expression(Expression::Identifier(name, _)) => {
                        assert_eq!(name, "first");
                    }
                    other => panic!("Expected first identifier expression, found {:?}", other),
                }
                match &parts[1] {
                    StringPart::Expression(Expression::Identifier(name, _)) => {
                        assert_eq!(name, "last");
                    }
                    other => panic!("Expected second identifier expression, found {:?}", other),
                }
            }
            other => panic!("Expected string interpolation, found {:?}", other),
        },
        other => panic!("Expected val declaration, found {:?}", other),
    }
}

#[test]
fn test_extension_function() {
    let input = "fun String.reversed(): String = StringBuilder(this).reverse().toString()";
    let result = Parser::parse(input).unwrap();

    assert_eq!(result.statements.len(), 1);
    match &result.statements[0] {
        Statement::ExtensionFunction(ef) => {
            match &ef.receiver_type {
                TypeAnnotation::Simple(type_name) => {
                    assert_eq!(type_name, "String");
                }
                _ => panic!("Expected simple type annotation"),
            }
            match ef.function.as_ref() {
                Statement::FunctionDeclaration { name, .. } => {
                    assert_eq!(name, "reversed");
                }
                _ => panic!("Expected function declaration inside extension"),
            }
        }
        _ => panic!("Expected extension function"),
    }
}

#[test]
fn test_async_spawn_constructs() {
    let input = r#"
        spawn {
            println("Virtual thread")
        }
    "#;
    let result = Parser::parse(input).unwrap();

    assert_eq!(result.statements.len(), 1);
    match &result.statements[0] {
        Statement::Concurrency(ConcurrencyConstruct::Spawn { body, .. }) => match body.as_ref() {
            Expression::Block { .. } => {}
            _ => panic!("Expected block expression in spawn"),
        },
        _ => panic!("Expected spawn statement"),
    }
}

#[test]
fn test_use_defer_constructs() {
    let input = r#"
        use (resource) {
            resource.process()
        }
    "#;
    let result = Parser::parse(input).unwrap();

    assert_eq!(result.statements.len(), 1);
    match &result.statements[0] {
        Statement::ResourceManagement(ResourceManagement::Use { resource, body, .. }) => {
            match resource.as_ref() {
                Expression::Identifier(name, _) => {
                    assert_eq!(name, "resource");
                }
                _ => panic!("Expected identifier in use statement"),
            }
            match body.as_ref() {
                Expression::Block { .. } => {}
                _ => panic!("Expected block expression in use statement"),
            }
        }
        _ => panic!("Expected use statement"),
    }
}

#[test]
fn test_array_literal() {
    let input = "val numbers = [1, 2, 3]";
    let result = Parser::parse(input).unwrap();

    assert_eq!(result.statements.len(), 1);
    match &result.statements[0] {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::Array { elements, .. } => {
                assert_eq!(elements.len(), 3);
                match &elements[0] {
                    Expression::Literal(Literal::Number(n), _) => assert_eq!(n, "1"),
                    _ => panic!("Expected integer literal 1"),
                }
            }
            _ => panic!("Expected array expression"),
        },
        _ => panic!("Expected val declaration"),
    }
}

#[test]
fn test_lambda_expression() {
    let input = "val doubled = numbers.map { x -> x * 2 }";
    let result = Parser::parse(input).unwrap();

    assert_eq!(result.statements.len(), 1);
    match &result.statements[0] {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::Call { function, args, .. } => {
                match function.as_ref() {
                    Expression::MemberAccess {
                        object, property, ..
                    } => {
                        match object.as_ref() {
                            Expression::Identifier(name, _) => {
                                assert_eq!(name, "numbers");
                            }
                            _ => panic!("Expected identifier"),
                        }
                        assert_eq!(property, "map");
                    }
                    _ => panic!("Expected member access"),
                }

                assert_eq!(args.len(), 1);
                match &args[0] {
                    Argument::Positional(Expression::Lambda { parameters, .. }) => {
                        dbg!(parameters);
                        assert_eq!(parameters.len(), 1, "parameters = {:?}", parameters);
                        assert_eq!(parameters[0].name, "x");
                    }
                    _ => panic!("Expected lambda expression"),
                }
            }
            _ => panic!("Expected function call"),
        },
        _ => panic!("Expected val declaration"),
    }
}

// Error handling tests
#[test]
fn test_parse_error_missing_semicolon() {
    let input = "val x = 42\nval y = 43"; // Missing semicolon
    let result = Parser::parse(input);

    // Should succeed as jv doesn't require semicolons for line-separated statements
    assert!(result.is_ok());
}

#[test]
fn test_parse_error_unmatched_brace() {
    let input = "fun test() { println(\"hello\") "; // Missing closing brace
    let result = Parser::parse(input);

    assert!(result.is_err());
}

#[test]
fn test_parse_error_invalid_syntax() {
    let input = "val = 42"; // Missing variable name
    let result = Parser::parse(input);

    assert!(result.is_err());
}

#[test]
fn test_complex_nested_expression() {
    let input = r#"
        val result = when (getValue()) {
            is String -> str.length
            is Int -> if (it > 0) it * 2 else 0
            null -> 0
            else -> -1
        }
    "#;
    let result = Parser::parse(input);

    assert!(result.is_ok());
    let program = result.unwrap();
    assert_eq!(program.statements.len(), 1);
}

#[test]
fn test_multiple_statements() {
    let input = r#"
        val name = "jv"
        var count = 0

        fun increment() {
            count = count + 1
        }

        data class Point(val x: Double, val y: Double)

        fun main() {
            println("Hello, ${name}!")
            increment()
            val point = Point(1.0, 2.0)
        }
    "#;
    let result = Parser::parse(input);

    assert!(result.is_ok());
    let program = result.unwrap();
    assert!(program.statements.len() >= 5); // Multiple top-level statements
}

// Property-based testing with test_case
#[test_case("val x = 42" ; "integer literal")]
#[test_case("val x = 3.14" ; "float literal")]
#[test_case("val x = true" ; "boolean literal")]
#[test_case("val x = \"hello\"" ; "string literal")]
#[test_case("val x = null" ; "null literal")]
fn test_literal_parsing(input: &str) {
    let result = Parser::parse(input);
    assert!(result.is_ok(), "Failed to parse: {}", input);
}

#[test_case("fun f(): Int" ; "function with return type")]
#[test_case("fun f(x: Int)" ; "function with parameter")]
#[test_case("fun f(x: Int, y: String)" ; "function with multiple parameters")]
#[test_case("fun f(x: Int = 0)" ; "function with default parameter")]
fn test_function_signatures(input: &str) {
    let result = Parser::parse(input);
    assert!(result.is_ok(), "Failed to parse: {}", input);
}

// Performance test
#[test]
#[ignore] // Only run in performance testing
fn test_parse_large_file() {
    let mut large_input = String::new();
    for i in 0..1000 {
        large_input.push_str(&format!("val var{} = {}\n", i, i));
    }

    let start = std::time::Instant::now();
    let result = Parser::parse(&large_input);
    let duration = start.elapsed();

    assert!(result.is_ok());
    println!("Parsed 1000 statements in {:?}", duration);
    // Should parse reasonably fast
    assert!(duration < std::time::Duration::from_secs(1));
}

#[test]
#[should_panic(expected = "not yet implemented")]
fn test_var_declaration_red_phase() {
    // RED: This test should fail
    let input = "var count = 42";
    let result = Parser::parse(input).unwrap();

    assert_eq!(result.statements.len(), 1);
    match &result.statements[0] {
        Statement::VarDeclaration {
            name, initializer, ..
        } => {
            assert_eq!(name, "count");
            match initializer {
                Some(Expression::Literal(Literal::Number(n), _)) => {
                    assert_eq!(n, "42");
                }
                _ => panic!("Expected number literal"),
            }
        }
        _ => panic!("Expected var declaration"),
    }
}

#[test]
#[should_panic(expected = "not yet implemented")]
fn test_function_declaration_red_phase() {
    // RED: This test should fail
    let input = "fun add(x: Int, y: Int): Int = x + y";
    let result = Parser::parse(input).unwrap();

    assert_eq!(result.statements.len(), 1);
    match &result.statements[0] {
        Statement::FunctionDeclaration {
            name,
            parameters,
            return_type,
            body,
            ..
        } => {
            assert_eq!(name, "add");
            assert_eq!(parameters.len(), 2);
            assert_eq!(parameters[0].name, "x");
            assert_eq!(parameters[1].name, "y");
            match return_type {
                Some(TypeAnnotation::Simple(type_name)) => assert_eq!(type_name, "Int"),
                _ => panic!("Expected Int return type"),
            }

            // Body should be a binary expression: x + y
            match body.as_ref() {
                Expression::Binary {
                    op: BinaryOp::Add, ..
                } => {
                    // Expected
                }
                _ => panic!("Expected binary addition expression"),
            }
        }
        _ => panic!("Expected function declaration"),
    }
}

#[test]
#[should_panic(expected = "not yet implemented")]
fn test_data_class_declaration_red_phase() {
    // RED: This test should fail
    let input = "data class User(name: String, age: Int)";
    let result = Parser::parse(input).unwrap();

    assert_eq!(result.statements.len(), 1);
    match &result.statements[0] {
        Statement::DataClassDeclaration {
            name, parameters, ..
        } => {
            assert_eq!(name, "User");
            assert_eq!(parameters.len(), 2);
            assert_eq!(parameters[0].name, "name");
            match parameters[0].type_annotation.as_ref() {
                Some(TypeAnnotation::Simple(type_name)) => assert_eq!(type_name, "String"),
                _ => panic!("Expected String type"),
            }
            assert_eq!(parameters[1].name, "age");
            match parameters[1].type_annotation.as_ref() {
                Some(TypeAnnotation::Simple(type_name)) => assert_eq!(type_name, "Int"),
                _ => panic!("Expected Int type"),
            }
        }
        _ => panic!("Expected data class declaration"),
    }
}

#[test]
#[should_panic(expected = "not yet implemented")]
fn test_binary_expressions_red_phase() {
    // RED: This test should fail
    let input = "val result = a + b * c";
    let result = Parser::parse(input).unwrap();

    assert_eq!(result.statements.len(), 1);
    match &result.statements[0] {
        Statement::ValDeclaration { initializer, .. } => {
            // Should parse as: a + (b * c) due to operator precedence
            match initializer {
                Expression::Binary {
                    op: BinaryOp::Add,
                    left,
                    right,
                    ..
                } => {
                    match left.as_ref() {
                        Expression::Identifier(name, _) => assert_eq!(name, "a"),
                        _ => panic!("Expected identifier 'a'"),
                    }
                    match right.as_ref() {
                        Expression::Binary {
                            op: BinaryOp::Multiply,
                            ..
                        } => {
                            // Expected multiplication
                        }
                        _ => panic!("Expected multiplication expression"),
                    }
                }
                _ => panic!("Expected binary expression"),
            }
        }
        _ => panic!("Expected val declaration"),
    }
}

#[test]
#[should_panic(expected = "not yet implemented")]
fn test_when_expression_red_phase() {
    // RED: This test should fail
    let input = r#"val result = when (x) {
        1 -> "one"
        2 -> "two"
        else -> "other"
    }"#;
    let result = Parser::parse(input).unwrap();

    assert_eq!(result.statements.len(), 1);
    match &result.statements[0] {
        Statement::ValDeclaration { initializer, .. } => {
            match initializer {
                Expression::When {
                    expr,
                    arms,
                    else_arm,
                    ..
                } => {
                    // Check the when expression structure
                    match expr {
                        Some(subject) => match subject.as_ref() {
                            Expression::Identifier(name, _) => assert_eq!(name, "x"),
                            _ => panic!("Expected identifier 'x'"),
                        },
                        None => panic!("Expected when expression subject"),
                    }
                    assert_eq!(arms.len(), 2);
                    assert!(else_arm.is_some());
                }
                _ => panic!("Expected when expression"),
            }
        }
        _ => panic!("Expected val declaration"),
    }
}

#[test]
#[should_panic(expected = "not yet implemented")]
fn test_function_call_red_phase() {
    // RED: This test should fail
    let input = "val result = calculate(x, y, 42)";
    let result = Parser::parse(input).unwrap();

    assert_eq!(result.statements.len(), 1);
    match &result.statements[0] {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::Call { function, args, .. } => {
                match function.as_ref() {
                    Expression::Identifier(name, _) => assert_eq!(name, "calculate"),
                    _ => panic!("Expected function identifier"),
                }
                assert_eq!(args.len(), 3);
            }
            _ => panic!("Expected function call"),
        },
        _ => panic!("Expected val declaration"),
    }
}

#[test]
#[should_panic(expected = "not yet implemented")]
fn test_member_access_red_phase() {
    // RED: This test should fail
    let input = "val name = user.name";
    let result = Parser::parse(input).unwrap();

    assert_eq!(result.statements.len(), 1);
    match &result.statements[0] {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::MemberAccess {
                object, property, ..
            } => {
                match object.as_ref() {
                    Expression::Identifier(name, _) => assert_eq!(name, "user"),
                    _ => panic!("Expected object identifier"),
                }
                assert_eq!(property, "name");
            }
            _ => panic!("Expected member access"),
        },
        _ => panic!("Expected val declaration"),
    }
}

#[test]
#[should_panic(expected = "not yet implemented")]
fn test_null_safe_member_access_red_phase() {
    // RED: This test should fail
    let input = "val name = user?.name";
    let result = Parser::parse(input).unwrap();

    assert_eq!(result.statements.len(), 1);
    match &result.statements[0] {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::NullSafeMemberAccess {
                object, property, ..
            } => {
                match object.as_ref() {
                    Expression::Identifier(name, _) => assert_eq!(name, "user"),
                    _ => panic!("Expected object identifier"),
                }
                assert_eq!(property, "name");
            }
            _ => panic!("Expected null-safe member access"),
        },
        _ => panic!("Expected val declaration"),
    }
}

#[test]
#[should_panic(expected = "not yet implemented")]
fn test_string_interpolation_red_phase() {
    // RED: This test should fail
    let input = r#"val message = "Hello, ${name}!""#;
    let result = Parser::parse(input).unwrap();

    assert_eq!(result.statements.len(), 1);
    match &result.statements[0] {
        Statement::ValDeclaration { initializer, .. } => {
            match initializer {
                Expression::StringInterpolation { parts, .. } => {
                    assert_eq!(parts.len(), 3); // "Hello, ", ${name}, "!"
                    match &parts[0] {
                        StringPart::Text(s) => assert_eq!(s, "Hello, "),
                        _ => panic!("Expected text part"),
                    }
                    match &parts[1] {
                        StringPart::Expression(Expression::Identifier(name, _)) => {
                            assert_eq!(name, "name");
                        }
                        _ => panic!("Expected identifier expression"),
                    }
                    match &parts[2] {
                        StringPart::Text(s) => assert_eq!(s, "!"),
                        _ => panic!("Expected text part"),
                    }
                }
                _ => panic!("Expected string interpolation"),
            }
        }
        _ => panic!("Expected val declaration"),
    }
}

#[test_case("val x = 42" => 1; "single val declaration")]
#[test_case("val x = 42\nvar y = \"test\"" => 2; "val and var declarations")]
#[test_case("fun test() = 42\nval result = test()" => 2; "function and call")]
#[should_panic(expected = "not yet implemented")]
fn test_multiple_statements_red_phase(input: &str) -> usize {
    // RED: This test should fail
    let result = Parser::parse(input).unwrap();
    result.statements.len()
}
