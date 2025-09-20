use super::*;

#[test]
fn test_type_inference() {
    let checker = TypeChecker::new();
    let context = CheckContext::new();

    let string_literal = Expression::Literal(Literal::String("test".to_string()), Span::default());
    let inferred_type = checker.infer_expression_type(&string_literal, &context);

    assert_eq!(inferred_type, InferredType::String);
}

#[test]
fn test_number_type_inference() {
    let checker = TypeChecker::new();
    let context = CheckContext::new();

    let int_literal = Expression::Literal(Literal::Number("42".to_string()), Span::default());
    let double_literal = Expression::Literal(Literal::Number("3.14".to_string()), Span::default());

    let int_type = checker.infer_expression_type(&int_literal, &context);
    let double_type = checker.infer_expression_type(&double_literal, &context);

    assert_eq!(int_type, InferredType::Int);
    assert_eq!(double_type, InferredType::Double);
}

#[test]
fn test_context_variable_scope() {
    let mut context = CheckContext::new();

    context.declare_variable("x".to_string(), InferredType::String);
    assert_eq!(context.get_variable_type("x"), Some(&InferredType::String));

    context.push_scope();
    context.declare_variable("y".to_string(), InferredType::Int);
    assert_eq!(context.get_variable_type("y"), Some(&InferredType::Int));
    assert_eq!(context.get_variable_type("x"), Some(&InferredType::String)); // Should still be visible

    context.pop_scope();
    assert_eq!(context.get_variable_type("x"), Some(&InferredType::String));
    assert_eq!(context.get_variable_type("y"), None); // Should be out of scope
}

#[test]
fn test_boolean_type_inference() {
    let checker = TypeChecker::new();
    let context = CheckContext::new();

    let true_literal = Expression::Literal(Literal::Boolean(true), Span::default());
    let false_literal = Expression::Literal(Literal::Boolean(false), Span::default());

    let true_type = checker.infer_expression_type(&true_literal, &context);
    let false_type = checker.infer_expression_type(&false_literal, &context);

    assert_eq!(true_type, InferredType::Boolean);
    assert_eq!(false_type, InferredType::Boolean);
}

#[test]
fn test_null_type_inference() {
    let checker = TypeChecker::new();
    let context = CheckContext::new();

    let null_literal = Expression::Literal(Literal::Null, Span::default());
    let null_type = checker.infer_expression_type(&null_literal, &context);

    assert_eq!(null_type, InferredType::Null);
}

#[test]
fn test_identifier_type_lookup() {
    let checker = TypeChecker::new();
    let mut context = CheckContext::new();

    context.declare_variable("x".to_string(), InferredType::String);
    context.declare_variable("y".to_string(), InferredType::Int);

    let x_identifier = Expression::Identifier("x".to_string(), Span::default());
    let y_identifier = Expression::Identifier("y".to_string(), Span::default());
    let unknown_identifier = Expression::Identifier("unknown".to_string(), Span::default());

    let x_type = checker.infer_expression_type(&x_identifier, &context);
    let y_type = checker.infer_expression_type(&y_identifier, &context);
    let unknown_type = checker.infer_expression_type(&unknown_identifier, &context);

    assert_eq!(x_type, InferredType::String);
    assert_eq!(y_type, InferredType::Int);
    assert_eq!(unknown_type, InferredType::Unknown);
}

#[test]
fn test_binary_expression_type_inference() {
    let checker = TypeChecker::new();
    let context = CheckContext::new();

    let add_expr = Expression::Binary {
        left: Box::new(Expression::Literal(
            Literal::Number("1".to_string()),
            Span::default(),
        )),
        op: BinaryOp::Add,
        right: Box::new(Expression::Literal(
            Literal::Number("2".to_string()),
            Span::default(),
        )),
        span: Span::default(),
    };

    let result_type = checker.infer_expression_type(&add_expr, &context);
    assert_eq!(result_type, InferredType::Int);
}

#[test]
fn test_type_annotation_conversion() {
    let checker = TypeChecker::new();

    let string_annotation = Some(TypeAnnotation::Simple("String".to_string()));
    let int_annotation = Some(TypeAnnotation::Simple("Int".to_string()));
    let bool_annotation = Some(TypeAnnotation::Simple("Boolean".to_string()));
    let double_annotation = Some(TypeAnnotation::Simple("Double".to_string()));
    let unknown_annotation = Some(TypeAnnotation::Simple("UnknownType".to_string()));
    let none_annotation = None;

    assert_eq!(
        checker.type_annotation_to_inferred(&string_annotation),
        InferredType::String
    );
    assert_eq!(
        checker.type_annotation_to_inferred(&int_annotation),
        InferredType::Int
    );
    assert_eq!(
        checker.type_annotation_to_inferred(&bool_annotation),
        InferredType::Boolean
    );
    assert_eq!(
        checker.type_annotation_to_inferred(&double_annotation),
        InferredType::Double
    );
    assert_eq!(
        checker.type_annotation_to_inferred(&unknown_annotation),
        InferredType::Unknown
    );
    assert_eq!(
        checker.type_annotation_to_inferred(&none_annotation),
        InferredType::Unknown
    );
}

#[test]
fn test_nullable_type_annotation() {
    let checker = TypeChecker::new();

    let nullable_string = Some(TypeAnnotation::Nullable(Box::new(TypeAnnotation::Simple(
        "String".to_string(),
    ))));
    let nullable_int = Some(TypeAnnotation::Nullable(Box::new(TypeAnnotation::Simple(
        "Int".to_string(),
    ))));

    let string_type = checker.type_annotation_to_inferred(&nullable_string);
    let int_type = checker.type_annotation_to_inferred(&nullable_int);

    match string_type {
        InferredType::Optional(inner) => assert_eq!(**inner, InferredType::String),
        _ => panic!("Expected optional string type"),
    }

    match int_type {
        InferredType::Optional(inner) => assert_eq!(**inner, InferredType::Int),
        _ => panic!("Expected optional int type"),
    }
}

#[test]
fn test_variable_declaration_checking() {
    let checker = TypeChecker::new();
    let mut context = CheckContext::new();

    let val_stmt = Statement::ValDeclaration {
        name: "x".to_string(),
        type_annotation: Some(TypeAnnotation::Simple("String".to_string())),
        initializer: Expression::Literal(Literal::String("hello".to_string()), Span::default()),
        span: Span::default(),
    };

    checker.check_statement(&val_stmt, &mut context);

    assert_eq!(context.get_variable_type("x"), Some(&InferredType::String));
    assert!(!context.has_errors());
}

#[test]
fn test_var_declaration_checking() {
    let checker = TypeChecker::new();
    let mut context = CheckContext::new();

    let var_stmt = Statement::VarDeclaration {
        name: "y".to_string(),
        type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
        initializer: Some(Expression::Literal(
            Literal::Number("42".to_string()),
            Span::default(),
        )),
        span: Span::default(),
    };

    checker.check_statement(&var_stmt, &mut context);

    assert_eq!(context.get_variable_type("y"), Some(&InferredType::Int));
    assert!(!context.has_errors());
}

#[test]
fn test_function_declaration_checking() {
    let checker = TypeChecker::new();
    let mut context = CheckContext::new();

    let func_stmt = Statement::FunctionDeclaration {
        name: "test".to_string(),
        type_parameters: vec![],
        parameters: vec![Parameter {
            name: "param".to_string(),
            type_annotation: Some(TypeAnnotation::Simple("String".to_string())),
            default_value: None,
            span: Span::default(),
        }],
        return_type: Some(TypeAnnotation::Simple("Int".to_string())),
        body: Expression::Literal(Literal::Number("42".to_string()), Span::default()),
        modifiers: Modifiers::default(),
        span: Span::default(),
    };

    checker.check_statement(&func_stmt, &mut context);

    assert_eq!(
        context.get_variable_type("test"),
        Some(&InferredType::Unknown)
    );
    assert!(!context.has_errors());
}

#[test]
fn test_program_checking() {
    let checker = TypeChecker::new();

    let program = Program {
        statements: vec![
            Statement::ValDeclaration {
                name: "x".to_string(),
                type_annotation: Some(TypeAnnotation::Simple("String".to_string())),
                initializer: Expression::Literal(
                    Literal::String("hello".to_string()),
                    Span::default(),
                ),
                span: Span::default(),
            },
            Statement::VarDeclaration {
                name: "y".to_string(),
                type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
                initializer: Some(Expression::Literal(
                    Literal::Number("42".to_string()),
                    Span::default(),
                )),
                span: Span::default(),
            },
        ],
        span: Span::default(),
    };

    let result = checker.check_program(&program);
    assert!(result.is_ok());
}

#[test]
fn test_null_safety_checking() {
    let checker = TypeChecker::new();

    let program = Program {
        statements: vec![
            Statement::ValDeclaration {
                name: "nullable".to_string(),
                type_annotation: Some(TypeAnnotation::Simple("String".to_string())),
                initializer: Expression::Literal(Literal::Null, Span::default()),
                span: Span::default(),
            },
            Statement::ValDeclaration {
                name: "normal".to_string(),
                type_annotation: Some(TypeAnnotation::Simple("String".to_string())),
                initializer: Expression::Literal(
                    Literal::String("hello".to_string()),
                    Span::default(),
                ),
                span: Span::default(),
            },
        ],
        span: Span::default(),
    };

    let warnings = checker.check_null_safety(&program);
    assert_eq!(warnings.len(), 1);
    assert!(warnings[0].contains("Assigning null to val declaration"));
}

#[test]
fn test_forbidden_syntax_checking() {
    let checker = TypeChecker::new();
    let program = Program {
        statements: vec![],
        span: Span::default(),
    };

    let violations = checker.check_forbidden_syntax(&program);
    assert_eq!(violations.len(), 0); // Currently no forbidden syntax implemented
}

#[test]
fn test_error_collection() {
    let mut context = CheckContext::new();

    let error1 = CheckError::TypeError("Type mismatch".to_string());
    let error2 = CheckError::UndefinedVariable("x".to_string());

    context.add_error(error1);
    context.add_error(error2);

    assert!(context.has_errors());
    assert_eq!(context.get_errors().len(), 2);
}

#[test]
fn test_warning_collection() {
    let mut context = CheckContext::new();

    context.add_warning("Warning 1".to_string());
    context.add_warning("Warning 2".to_string());

    assert_eq!(context.get_warnings().len(), 2);
    assert_eq!(context.get_warnings()[0], "Warning 1");
    assert_eq!(context.get_warnings()[1], "Warning 2");
}

#[test]
fn test_nested_scopes() {
    let mut context = CheckContext::new();

    // Outer scope
    context.declare_variable("outer".to_string(), InferredType::String);

    // Middle scope
    context.push_scope();
    context.declare_variable("middle".to_string(), InferredType::Int);

    // Inner scope
    context.push_scope();
    context.declare_variable("inner".to_string(), InferredType::Boolean);

    // All variables should be visible
    assert_eq!(
        context.get_variable_type("outer"),
        Some(&InferredType::String)
    );
    assert_eq!(
        context.get_variable_type("middle"),
        Some(&InferredType::Int)
    );
    assert_eq!(
        context.get_variable_type("inner"),
        Some(&InferredType::Boolean)
    );

    // Pop inner scope
    context.pop_scope();
    assert_eq!(
        context.get_variable_type("outer"),
        Some(&InferredType::String)
    );
    assert_eq!(
        context.get_variable_type("middle"),
        Some(&InferredType::Int)
    );
    assert_eq!(context.get_variable_type("inner"), None);

    // Pop middle scope
    context.pop_scope();
    assert_eq!(
        context.get_variable_type("outer"),
        Some(&InferredType::String)
    );
    assert_eq!(context.get_variable_type("middle"), None);
    assert_eq!(context.get_variable_type("inner"), None);
}

#[test]
fn test_variable_shadowing() {
    let mut context = CheckContext::new();

    // Declare variable in outer scope
    context.declare_variable("x".to_string(), InferredType::String);
    assert_eq!(context.get_variable_type("x"), Some(&InferredType::String));

    // Shadow it in inner scope
    context.push_scope();
    context.declare_variable("x".to_string(), InferredType::Int);
    assert_eq!(context.get_variable_type("x"), Some(&InferredType::Int)); // Should see shadowed version

    // Pop scope - should see original again
    context.pop_scope();
    assert_eq!(context.get_variable_type("x"), Some(&InferredType::String));
}

#[test]
fn test_inferred_type_equality() {
    assert_eq!(InferredType::String, InferredType::String);
    assert_eq!(InferredType::Int, InferredType::Int);
    assert_eq!(InferredType::Boolean, InferredType::Boolean);
    assert_eq!(InferredType::Null, InferredType::Null);
    assert_eq!(InferredType::Unknown, InferredType::Unknown);

    assert_ne!(InferredType::String, InferredType::Int);
    assert_ne!(InferredType::Boolean, InferredType::Null);

    let optional_string = InferredType::Optional(Box::new(InferredType::String));
    let optional_string2 = InferredType::Optional(Box::new(InferredType::String));
    let optional_int = InferredType::Optional(Box::new(InferredType::Int));

    assert_eq!(optional_string, optional_string2);
    assert_ne!(optional_string, optional_int);
}

#[test]
fn test_check_error_display() {
    let type_error = CheckError::TypeError("Expected Int, found String".to_string());
    let null_error = CheckError::NullSafetyError("Null dereference".to_string());
    let undefined_error = CheckError::UndefinedVariable("x".to_string());
    let syntax_error = CheckError::SyntaxError("Invalid syntax".to_string());
    let validation_error = CheckError::ValidationError("Validation failed".to_string());

    assert!(type_error.to_string().contains("Type error"));
    assert!(null_error.to_string().contains("Null safety violation"));
    assert!(undefined_error.to_string().contains("Undefined variable"));
    assert!(syntax_error.to_string().contains("Invalid syntax"));
    assert!(validation_error.to_string().contains("Validation error"));
}

#[test]
fn test_type_checker_default() {
    let checker = TypeChecker::default();
    let context = CheckContext::new();

    let literal = Expression::Literal(Literal::String("test".to_string()), Span::default());
    let inferred = checker.infer_expression_type(&literal, &context);

    assert_eq!(inferred, InferredType::String);
}

#[test]
fn test_complex_expression_checking() {
    let checker = TypeChecker::new();
    let mut context = CheckContext::new();

    // Declare some variables
    context.declare_variable("name".to_string(), InferredType::String);
    context.declare_variable("age".to_string(), InferredType::Int);

    // Test complex binary expression
    let complex_expr = Expression::Binary {
        left: Box::new(Expression::Identifier("age".to_string(), Span::default())),
        op: BinaryOp::Greater,
        right: Box::new(Expression::Literal(
            Literal::Number("18".to_string()),
            Span::default(),
        )),
        span: Span::default(),
    };

    let result_type = checker.infer_expression_type(&complex_expr, &context);
    assert_eq!(result_type, InferredType::Boolean);
}

#[test]
fn test_performance_with_many_variables() {
    let mut context = CheckContext::new();

    // Add many variables
    for i in 0..1000 {
        context.declare_variable(format!("var{}", i), InferredType::Int);
    }

    // Test lookup performance
    let start = std::time::Instant::now();
    for i in 0..1000 {
        let var_name = format!("var{}", i);
        assert_eq!(
            context.get_variable_type(&var_name),
            Some(&InferredType::Int)
        );
    }
    let duration = start.elapsed();

    // Should complete reasonably fast
    assert!(duration < std::time::Duration::from_millis(10));
}
