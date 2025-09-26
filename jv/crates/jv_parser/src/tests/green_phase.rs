use super::support::{first_statement, parse_program, parse_program_result};
use jv_ast::{
    Argument, BinaryOp, CallArgumentStyle, ConcurrencyConstruct, Expression, Literal, LoopStrategy,
    ResourceManagement, SequenceDelimiter, Statement, StringPart, TypeAnnotation,
};

use test_case::test_case;

#[test]
fn test_simple_val_declaration() {
    let program = parse_program("val name = \"hello\"");
    let statement = first_statement(&program);

    match statement {
        Statement::ValDeclaration {
            name, initializer, ..
        } => {
            assert_eq!(name, "name");
            match initializer {
                Expression::Literal(Literal::String(value), _) => assert_eq!(value, "hello"),
                other => panic!("expected string literal, found {:?}", other),
            }
        }
        other => panic!("expected val declaration, found {:?}", other),
    }
}

#[test]
fn test_var_declaration() {
    let program = parse_program("var count = 42");
    let statement = first_statement(&program);

    match statement {
        Statement::VarDeclaration {
            name, initializer, ..
        } => {
            assert_eq!(name, "count");
            match initializer {
                Some(Expression::Literal(Literal::Number(value), _)) => assert_eq!(value, "42"),
                other => panic!("expected numeric initializer, found {:?}", other),
            }
        }
        other => panic!("expected var declaration, found {:?}", other),
    }
}

#[test]
fn test_function_declaration_signature_and_body() {
    let program = parse_program(
        r#"
        fun add(a: Int, b: Int): Int {
            return a + b
        }
    "#,
    );
    let statement = first_statement(&program);

    match statement {
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

            match return_type {
                Some(TypeAnnotation::Simple(type_name)) => assert_eq!(type_name, "Int"),
                other => panic!("expected Int return type, found {:?}", other),
            }

            match body.as_ref() {
                Expression::Block { statements, .. } => {
                    assert_eq!(statements.len(), 1);
                    assert!(matches!(statements[0], Statement::Return { .. }));
                }
                other => panic!("expected block body, found {:?}", other),
            }
        }
        other => panic!("expected function declaration, found {:?}", other),
    }
}

#[test]
fn test_extension_function() {
    let program =
        parse_program("fun String.reversed(): String = StringBuilder(this).reverse().toString()");
    let statement = first_statement(&program);

    match statement {
        Statement::ExtensionFunction(extension) => {
            match &extension.receiver_type {
                TypeAnnotation::Simple(type_name) => assert_eq!(type_name, "String"),
                other => panic!("expected simple receiver type, found {:?}", other),
            }

            match extension.function.as_ref() {
                Statement::FunctionDeclaration { name, .. } => assert_eq!(name, "reversed"),
                other => panic!("expected inner function declaration, found {:?}", other),
            }
        }
        other => panic!("expected extension function, found {:?}", other),
    }
}

#[test]
fn test_data_class_declaration() {
    let program = parse_program("data class User(val name: String, var age: Int)");
    let statement = first_statement(&program);

    match statement {
        Statement::DataClassDeclaration {
            name,
            parameters,
            is_mutable,
            ..
        } => {
            assert_eq!(name, "User");
            assert_eq!(parameters.len(), 2);
            assert!(!is_mutable);

            let first = &parameters[0];
            assert_eq!(first.name, "name");
            match first.type_annotation.as_ref() {
                Some(TypeAnnotation::Simple(type_name)) => assert_eq!(type_name, "String"),
                other => panic!("expected String type, found {:?}", other),
            }

            let second = &parameters[1];
            assert_eq!(second.name, "age");
            match second.type_annotation.as_ref() {
                Some(TypeAnnotation::Simple(type_name)) => assert_eq!(type_name, "Int"),
                other => panic!("expected Int type, found {:?}", other),
            }
        }
        other => panic!("expected data class declaration, found {:?}", other),
    }
}

#[test]
fn test_when_expression_statement() {
    let program = parse_program(
        r#"
        when (x) {
            0 -> "zero"
            1 -> "one"
            else -> "other"
        }
    "#,
    );
    let statement = first_statement(&program);

    match statement {
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
            other => panic!("expected when expression, found {:?}", other),
        },
        other => panic!("expected expression statement, found {:?}", other),
    }
}

#[test]
fn test_when_expression_in_val_declaration() {
    let program = parse_program(
        r#"val result = when (x) {
            1 -> "one"
            2 -> "two"
            else -> "other"
        }"#,
    );
    let statement = first_statement(&program);

    match statement {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::When {
                expr,
                arms,
                else_arm,
                ..
            } => {
                match expr {
                    Some(subject) => match subject.as_ref() {
                        Expression::Identifier(name, _) => assert_eq!(name, "x"),
                        other => panic!("expected identifier subject, found {:?}", other),
                    },
                    None => panic!("expected subject for when expression"),
                }
                assert_eq!(arms.len(), 2);
                assert!(else_arm.is_some());
            }
            other => panic!("expected when initializer, found {:?}", other),
        },
        other => panic!("expected val declaration, found {:?}", other),
    }
}

#[test]
fn test_null_safety_operators() {
    let program = parse_program("val length = user?.name?.length ?: 0");
    let statement = first_statement(&program);

    match statement {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::Binary {
                left, op, right, ..
            } => {
                assert_eq!(*op, BinaryOp::Elvis);
                assert!(matches!(
                    left.as_ref(),
                    Expression::NullSafeMemberAccess { .. }
                ));
                match right.as_ref() {
                    Expression::Literal(Literal::Number(value), _) => assert_eq!(value, "0"),
                    other => panic!("expected numeric literal, found {:?}", other),
                }
            }
            other => panic!("expected binary expression, found {:?}", other),
        },
        other => panic!("expected val declaration, found {:?}", other),
    }
}

#[test]
fn test_null_safe_member_access() {
    let program = parse_program("val name = user?.name");
    let statement = first_statement(&program);

    match statement {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::NullSafeMemberAccess {
                object, property, ..
            } => {
                match object.as_ref() {
                    Expression::Identifier(name, _) => assert_eq!(name, "user"),
                    other => panic!("expected object identifier, found {:?}", other),
                }
                assert_eq!(property, "name");
            }
            other => panic!("expected null-safe member access, found {:?}", other),
        },
        other => panic!("expected val declaration, found {:?}", other),
    }
}

#[test]
fn test_null_safe_index_access() {
    let program = parse_program("val value = items?[index]");
    let statement = first_statement(&program);

    match statement {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::NullSafeIndexAccess { object, index, .. } => {
                match object.as_ref() {
                    Expression::Identifier(name, _) => assert_eq!(name, "items"),
                    other => panic!("expected identifier object, found {:?}", other),
                }
                match index.as_ref() {
                    Expression::Identifier(name, _) => assert_eq!(name, "index"),
                    other => panic!("expected identifier index, found {:?}", other),
                }
            }
            other => panic!("expected null-safe index access, found {:?}", other),
        },
        other => panic!("expected val declaration, found {:?}", other),
    }
}

#[test]
fn test_binary_expression_precedence() {
    let program = parse_program("val result = a + b * c");
    let statement = first_statement(&program);

    match statement {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::Binary {
                op, left, right, ..
            } => {
                assert_eq!(*op, BinaryOp::Add);
                match left.as_ref() {
                    Expression::Identifier(name, _) => assert_eq!(name, "a"),
                    other => panic!("expected identifier 'a', found {:?}", other),
                }
                match right.as_ref() {
                    Expression::Binary { op: inner_op, .. } => {
                        assert_eq!(*inner_op, BinaryOp::Multiply)
                    }
                    other => panic!(
                        "expected multiplication on right-hand side, found {:?}",
                        other
                    ),
                }
            }
            other => panic!("expected binary expression, found {:?}", other),
        },
        other => panic!("expected val declaration, found {:?}", other),
    }
}

#[test]
fn test_function_call_expression() {
    let program = parse_program("val result = calculate(x, y, 42)");
    let statement = first_statement(&program);

    match statement {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::Call { function, args, .. } => {
                match function.as_ref() {
                    Expression::Identifier(name, _) => assert_eq!(name, "calculate"),
                    other => panic!("expected function identifier, found {:?}", other),
                }
                assert_eq!(args.len(), 3);
            }
            other => panic!("expected call expression, found {:?}", other),
        },
        other => panic!("expected val declaration, found {:?}", other),
    }
}

#[test]
fn test_member_access_expression() {
    let program = parse_program("val name = user.name");
    let statement = first_statement(&program);

    match statement {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::MemberAccess {
                object, property, ..
            } => {
                match object.as_ref() {
                    Expression::Identifier(name, _) => assert_eq!(name, "user"),
                    other => panic!("expected identifier object, found {:?}", other),
                }
                assert_eq!(property, "name");
            }
            other => panic!("expected member access, found {:?}", other),
        },
        other => panic!("expected val declaration, found {:?}", other),
    }
}

#[test]
fn test_string_interpolation_in_call() {
    let program = parse_program(r#"println("Hello, ${name}!")"#);
    let statement = first_statement(&program);

    match statement {
        Statement::Expression { expr, .. } => match expr {
            Expression::Call { function, args, .. } => {
                match function.as_ref() {
                    Expression::Identifier(name, _) => assert_eq!(name, "println"),
                    other => panic!("expected println identifier, found {:?}", other),
                }
                assert_eq!(args.len(), 1);
                match &args[0] {
                    Argument::Positional(Expression::StringInterpolation { parts, .. }) => {
                        assert!(parts.len() >= 2);
                    }
                    other => panic!("expected string interpolation argument, found {:?}", other),
                }
            }
            other => panic!("expected call expression, found {:?}", other),
        },
        other => panic!("expected expression statement, found {:?}", other),
    }
}

#[test]
fn test_string_interpolation_parts() {
    let program = parse_program(r#"val message = "Hello, ${name}!""#);
    let statement = first_statement(&program);

    match statement {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::StringInterpolation { parts, .. } => {
                assert_eq!(parts.len(), 3);
                match &parts[0] {
                    StringPart::Text(text) => assert_eq!(text, "Hello, "),
                    other => panic!("expected text prefix, found {:?}", other),
                }
                match &parts[1] {
                    StringPart::Expression(Expression::Identifier(name, _)) => {
                        assert_eq!(name, "name")
                    }
                    other => panic!("expected identifier expression, found {:?}", other),
                }
                match &parts[2] {
                    StringPart::Text(text) => assert_eq!(text, "!"),
                    other => panic!("expected text suffix, found {:?}", other),
                }
            }
            other => panic!("expected string interpolation, found {:?}", other),
        },
        other => panic!("expected val declaration, found {:?}", other),
    }
}

#[test]
fn test_string_interpolation_leading_expression() {
    let program = parse_program(r#"val message = "${name}!""#);
    let statement = first_statement(&program);

    match statement {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::StringInterpolation { parts, .. } => {
                assert_eq!(parts.len(), 2);
                match &parts[0] {
                    StringPart::Expression(Expression::Identifier(name, _)) => {
                        assert_eq!(name, "name")
                    }
                    other => panic!("expected identifier expression, found {:?}", other),
                }
                match &parts[1] {
                    StringPart::Text(text) => assert_eq!(text, "!"),
                    other => panic!("expected text suffix, found {:?}", other),
                }
            }
            other => panic!("expected string interpolation, found {:?}", other),
        },
        other => panic!("expected val declaration, found {:?}", other),
    }
}

#[test]
fn test_string_interpolation_multiple_expressions_no_text() {
    let program = parse_program(r#"val message = "${first}${last}""#);
    let statement = first_statement(&program);

    match statement {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::StringInterpolation { parts, .. } => {
                assert_eq!(parts.len(), 2);
                assert!(matches!(
                    &parts[0],
                    StringPart::Expression(Expression::Identifier(name, _)) if name == "first"
                ));
                assert!(matches!(
                    &parts[1],
                    StringPart::Expression(Expression::Identifier(name, _)) if name == "last"
                ));
            }
            other => panic!("expected string interpolation, found {:?}", other),
        },
        other => panic!("expected val declaration, found {:?}", other),
    }
}

#[test]
fn test_async_spawn_constructs() {
    let program = parse_program(
        r#"
        spawn {
            println("Virtual thread")
        }
    "#,
    );
    let statement = first_statement(&program);

    match statement {
        Statement::Concurrency(ConcurrencyConstruct::Spawn { body, .. }) => match body.as_ref() {
            Expression::Block { .. } => {}
            other => panic!("expected block expression, found {:?}", other),
        },
        other => panic!("expected spawn construct, found {:?}", other),
    }
}

#[test]
fn test_use_defer_constructs() {
    let program = parse_program(
        r#"
        use (resource) {
            resource.process()
        }
    "#,
    );
    let statement = first_statement(&program);

    match statement {
        Statement::ResourceManagement(ResourceManagement::Use { resource, body, .. }) => {
            match resource.as_ref() {
                Expression::Identifier(name, _) => assert_eq!(name, "resource"),
                other => panic!("expected identifier resource, found {:?}", other),
            }
            match body.as_ref() {
                Expression::Block { .. } => {}
                other => panic!("expected block body, found {:?}", other),
            }
        }
        other => panic!("expected use construct, found {:?}", other),
    }
}

#[test]
fn test_array_literal() {
    let program = parse_program("val numbers = [1, 2, 3]");
    let statement = first_statement(&program);

    match statement {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::Array { elements, .. } => {
                assert_eq!(elements.len(), 3);
                match &elements[0] {
                    Expression::Literal(Literal::Number(value), _) => assert_eq!(value, "1"),
                    other => panic!("expected numeric literal, found {:?}", other),
                }
            }
            other => panic!("expected array expression, found {:?}", other),
        },
        other => panic!("expected val declaration, found {:?}", other),
    }
}

#[test]
fn test_whitespace_array_literal() {
    let program = parse_program("val numbers = [1 2 3]");
    let statement = first_statement(&program);

    match statement {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::Array {
                elements,
                delimiter,
                ..
            } => {
                assert_eq!(elements.len(), 3);
                assert_eq!(*delimiter, SequenceDelimiter::Whitespace);
            }
            other => panic!("expected array expression, found {:?}", other),
        },
        other => panic!("expected val declaration, found {:?}", other),
    }
}

#[test]
fn test_whitespace_array_literal_with_comment() {
    let program = parse_program("val numbers = [1 /* note */ 2]");
    let statement = first_statement(&program);

    match statement {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::Array {
                elements,
                delimiter,
                ..
            } => {
                assert_eq!(elements.len(), 2);
                assert_eq!(*delimiter, SequenceDelimiter::Whitespace);
            }
            other => panic!("expected array expression, found {:?}", other),
        },
        other => panic!("expected val declaration, found {:?}", other),
    }
}

#[test]
fn test_whitespace_call_arguments() {
    let program = parse_program("val result = plot(1 2 3)");
    let statement = first_statement(&program);

    match statement {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::Call {
                args,
                argument_style,
                ..
            } => {
                assert_eq!(args.len(), 3);
                assert_eq!(*argument_style, CallArgumentStyle::Whitespace);
            }
            other => panic!("expected call expression, found {:?}", other),
        },
        other => panic!("expected val declaration, found {:?}", other),
    }
}

#[test]
fn test_whitespace_call_preserves_trailing_lambda() {
    let program = parse_program("val result = plot(1 2) { sample -> sample * sample }");
    let statement = first_statement(&program);

    match statement {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::Call {
                args,
                argument_style,
                ..
            } => {
                assert_eq!(*argument_style, CallArgumentStyle::Whitespace);
                assert_eq!(args.len(), 3);
                match &args[2] {
                    Argument::Positional(Expression::Lambda { .. }) => {}
                    other => panic!("expected trailing lambda argument, found {:?}", other),
                }
            }
            other => panic!("expected call expression, found {:?}", other),
        },
        other => panic!("expected val declaration, found {:?}", other),
    }
}

#[test]
fn test_lambda_expression() {
    let program = parse_program("val doubled = numbers.map { x -> x * 2 }");
    let statement = first_statement(&program);

    match statement {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::Call { function, args, .. } => {
                match function.as_ref() {
                    Expression::MemberAccess {
                        object, property, ..
                    } => {
                        match object.as_ref() {
                            Expression::Identifier(name, _) => assert_eq!(name, "numbers"),
                            other => panic!("expected identifier receiver, found {:?}", other),
                        }
                        assert_eq!(property, "map");
                    }
                    other => panic!("expected member access, found {:?}", other),
                }

                assert_eq!(args.len(), 1);
                match &args[0] {
                    Argument::Positional(Expression::Lambda { parameters, .. }) => {
                        assert_eq!(parameters.len(), 1);
                        assert_eq!(parameters[0].name, "x");
                    }
                    other => panic!("expected lambda argument, found {:?}", other),
                }
            }
            other => panic!("expected call expression, found {:?}", other),
        },
        other => panic!("expected val declaration, found {:?}", other),
    }
}

#[test]
fn test_parse_error_missing_semicolon() {
    let source = "val x = 42\nval y = 43"; // line-separated statements are valid
    assert!(parse_program_result(source).is_ok());
}

#[test]
fn test_parse_error_unmatched_brace() {
    let source = "fun test() { println(\"hello\") "; // Missing closing brace
    assert!(parse_program_result(source).is_err());
}

#[test]
fn test_parse_error_invalid_syntax() {
    let source = "val = 42"; // Missing variable name
    assert!(parse_program_result(source).is_err());
}

#[test]
fn test_complex_nested_expression() {
    let program = parse_program(
        r#"
        val result = when (getValue()) {
            is String -> str.length
            is Int -> if (it > 0) it * 2 else 0
            null -> 0
            else -> -1
        }
    "#,
    );
    assert_eq!(program.statements.len(), 1);
}

#[test]
fn test_for_in_iterable_statement() {
    let program = parse_program(
        r#"
        for (item in items) {
            val x = item
        }
    "#,
    );
    let statement = first_statement(&program);

    match statement {
        Statement::ForIn(for_in) => {
            assert_eq!(for_in.binding.name, "item");
            assert!(for_in.binding.type_annotation.is_none());
            assert!(matches!(for_in.strategy, LoopStrategy::Iterable));
            match &for_in.iterable {
                Expression::Identifier(name, _) => assert_eq!(name, "items"),
                other => panic!("expected identifier iterable, found {:?}", other),
            }
            if let Expression::Block { statements, .. } = for_in.body.as_ref() {
                assert!(!statements.is_empty());
            } else {
                panic!("expected block body in for-in loop");
            }
        }
        other => panic!("expected for-in statement, found {:?}", other),
    }
}

#[test]
fn test_for_in_numeric_range_statement_exclusive() {
    let program = parse_program(
        r#"
        for (index: Int in 0..10) {
            val current = index
        }
    "#,
    );
    let statement = first_statement(&program);

    match statement {
        Statement::ForIn(for_in) => {
            assert_eq!(for_in.binding.name, "index");
            assert!(matches!(
                for_in.binding.type_annotation,
                Some(TypeAnnotation::Simple(ref name)) if name == "Int"
            ));
            match &for_in.strategy {
                LoopStrategy::NumericRange(meta) => {
                    assert!(!meta.inclusive);
                    match (&meta.start, &meta.end) {
                        (
                            Expression::Literal(Literal::Number(start), _),
                            Expression::Literal(Literal::Number(end), _),
                        ) => {
                            assert_eq!(start, "0");
                            assert_eq!(end, "10");
                        }
                        other => panic!(
                            "expected numeric literals for range bounds, found {:?}",
                            other
                        ),
                    }
                }
                other => panic!("expected numeric range strategy, found {:?}", other),
            }
        }
        other => panic!("expected for-in statement, found {:?}", other),
    }
}

#[test]
fn test_for_in_numeric_range_statement_inclusive() {
    let program = parse_program(
        r#"
        for (day in 1..=7) {
            val label = day
        }
    "#,
    );
    let statement = first_statement(&program);

    match statement {
        Statement::ForIn(for_in) => match &for_in.strategy {
            LoopStrategy::NumericRange(meta) => assert!(meta.inclusive),
            other => panic!("expected inclusive numeric range, found {:?}", other),
        },
        other => panic!("expected for-in statement, found {:?}", other),
    }
}

#[test]
fn test_range_expression_in_val_declaration() {
    let program = parse_program("val range = 1..=10");
    let statement = first_statement(&program);

    match statement {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::Binary { op, .. } => {
                assert!(matches!(op, BinaryOp::RangeInclusive));
            }
            other => panic!("expected binary range expression, found {:?}", other),
        },
        other => panic!("expected val declaration, found {:?}", other),
    }
}

#[test]
fn test_multiple_statements() {
    let program = parse_program(
        r#"
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
    "#,
    );

    assert!(program.statements.len() >= 5);
}

#[test_case("val x = 42" ; "integer literal")]
#[test_case("val x = 3.14" ; "float literal")]
#[test_case("val x = true" ; "boolean literal")]
#[test_case("val x = \"hello\"" ; "string literal")]
#[test_case("val x = null" ; "null literal")]
fn test_literal_parsing(input: &str) {
    assert!(
        parse_program_result(input).is_ok(),
        "failed to parse: {}",
        input
    );
}

#[test_case("fun f(): Int" ; "function with return type")]
#[test_case("fun f(x: Int)" ; "function with parameter")]
#[test_case("fun f(x: Int, y: String)" ; "function with multiple parameters")]
#[test_case("fun f(x: Int = 0)" ; "function with default parameter")]
fn test_function_signatures(input: &str) {
    assert!(
        parse_program_result(input).is_ok(),
        "failed to parse: {}",
        input
    );
}

#[test_case("val x = 42" => 1 ; "single val declaration")]
#[test_case("val x = 42\nvar y = \"test\"" => 2 ; "val and var declarations")]
#[test_case("fun test() = 42\nval result = test()" => 2 ; "function and call")]
fn test_multiple_statements_variants(input: &str) -> usize {
    let program = parse_program(input);
    program.statements.len()
}

#[test]
#[ignore = "performance benchmark"]
fn test_parse_large_file() {
    let mut large_input = String::new();
    for i in 0..1000 {
        large_input.push_str(&format!("val var{} = {}\n", i, i));
    }

    let start = std::time::Instant::now();
    let result = parse_program_result(&large_input);
    let duration = start.elapsed();

    assert!(result.is_ok());
    assert!(
        duration < std::time::Duration::from_secs(1),
        "parsing took {:?}",
        duration
    );
}
