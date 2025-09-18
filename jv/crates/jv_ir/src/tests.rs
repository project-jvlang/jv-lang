#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        convert_type_annotation, desugar_async_expression, desugar_await_expression,
        desugar_data_class, desugar_default_parameters, desugar_defer_expression,
        desugar_elvis_operator, desugar_extension_function, desugar_named_arguments,
        desugar_null_safe_index_access, desugar_null_safe_member_access, desugar_spawn_expression,
        desugar_string_interpolation, desugar_top_level_function, desugar_use_expression,
        desugar_val_declaration, desugar_var_declaration, desugar_when_expression,
        generate_extension_class_name, generate_utility_class_name, infer_java_type,
        transform_expression, transform_program, transform_statement, CompletableFutureOp,
        IrCaseLabel, IrExpression, IrModifiers, IrStatement, IrVisibility, JavaType,
        TransformContext, TransformError, VirtualThreadOp,
    };
    use jv_ast::*;

    fn dummy_span() -> Span {
        Span::dummy()
    }

    // Helper to create a simple transform context
    fn test_context() -> TransformContext {
        TransformContext::new()
    }

    // JavaType construction tests
    #[test]
    fn test_java_type_constructors() {
        assert_eq!(JavaType::int(), JavaType::Primitive("int".to_string()));
        assert_eq!(
            JavaType::boolean(),
            JavaType::Primitive("boolean".to_string())
        );
        assert_eq!(
            JavaType::string(),
            JavaType::Reference {
                name: "String".to_string(),
                generic_args: vec![],
            }
        );
        assert_eq!(JavaType::void(), JavaType::Void);

        assert!(!JavaType::int().is_nullable());
        assert!(JavaType::string().is_nullable());
        assert!(JavaType::object().is_nullable());
    }

    // Transform context tests
    #[test]
    fn test_transform_context_scope_management() {
        let mut context = TransformContext::new();

        // Initial scope
        assert_eq!(context.scope_stack.len(), 1);

        // Add variable to current scope
        context.add_variable("x".to_string(), JavaType::int());
        assert_eq!(context.lookup_variable("x"), Some(&JavaType::int()));

        // Enter new scope
        context.enter_scope();
        assert_eq!(context.scope_stack.len(), 2);

        // Variable from parent scope should be accessible
        assert_eq!(context.lookup_variable("x"), Some(&JavaType::int()));

        // Add variable to inner scope
        context.add_variable("y".to_string(), JavaType::string());
        assert_eq!(context.lookup_variable("y"), Some(&JavaType::string()));

        // Exit scope
        context.exit_scope();
        assert_eq!(context.scope_stack.len(), 1);

        // Inner variable should no longer be accessible
        assert_eq!(context.lookup_variable("y"), None);
        // But parent variable should still be accessible
        assert_eq!(context.lookup_variable("x"), Some(&JavaType::int()));
    }

    // Test for val declaration desugaring
    #[test]
    fn test_desugar_val_declaration_creates_final_variable() {
        let mut context = test_context();
        let initializer = Expression::Literal(Literal::Number("42".to_string()), dummy_span());

        let result = desugar_val_declaration(
            "x".to_string(),
            None,
            initializer,
            Modifiers::default(),
            dummy_span(),
            &mut context,
        )
        .expect("val declaration should desugar successfully");

        match result {
            IrStatement::VariableDeclaration {
                name,
                java_type,
                initializer,
                is_final,
                modifiers,
                ..
            } => {
                assert_eq!(name, "x");
                assert_eq!(java_type, JavaType::int());
                assert!(initializer.is_some());
                assert!(is_final);
                assert_eq!(modifiers.visibility, IrVisibility::Private);
                assert!(modifiers.is_final);
            }
            other => panic!("Expected variable declaration, got {:?}", other),
        }

        assert_eq!(context.lookup_variable("x"), Some(&JavaType::int()));
    }

    // Test for var declaration desugaring with inference
    #[test]
    fn test_desugar_var_declaration_infers_string_type() {
        let mut context = test_context();
        let initializer = Some(Expression::Literal(
            Literal::String("hello".to_string()),
            dummy_span(),
        ));

        let result = desugar_var_declaration(
            "message".to_string(),
            None,
            initializer,
            Modifiers::default(),
            dummy_span(),
            &mut context,
        )
        .expect("var declaration should desugar successfully");

        match result {
            IrStatement::VariableDeclaration {
                name,
                java_type,
                initializer,
                is_final,
                modifiers,
                ..
            } => {
                assert_eq!(name, "message");
                assert_eq!(java_type, JavaType::string());
                assert!(initializer.is_some());
                assert!(!is_final);
                assert_eq!(modifiers.visibility, IrVisibility::Private);
                assert!(!modifiers.is_final);
            }
            other => panic!("Expected variable declaration, got {:?}", other),
        }

        assert_eq!(
            context.lookup_variable("message"),
            Some(&JavaType::string())
        );
    }

    #[test]
    fn test_desugar_var_declaration_requires_type_information() {
        let mut context = test_context();

        let error = desugar_var_declaration(
            "untyped".to_string(),
            None,
            None,
            Modifiers::default(),
            dummy_span(),
            &mut context,
        )
        .expect_err("var declaration without type information should fail");

        match error {
            TransformError::TypeInferenceError { message, .. } => {
                assert!(message.contains("Cannot infer type"));
            }
            other => panic!("Expected type inference error, got {other:?}"),
        }
    }

    // Test for when expression desugaring
    #[test]
    fn test_desugar_when_expression_literals_to_switch() {
        let mut context = test_context();
        context.add_variable("x".to_string(), JavaType::int());

        let subject = Some(Box::new(Expression::Identifier(
            "x".to_string(),
            dummy_span(),
        )));

        let arm1 = WhenArm {
            pattern: Pattern::Literal(Literal::Number("1".to_string()), dummy_span()),
            body: Expression::Literal(Literal::String("one".to_string()), dummy_span()),
            span: dummy_span(),
        };
        let arm2 = WhenArm {
            pattern: Pattern::Literal(Literal::Number("2".to_string()), dummy_span()),
            body: Expression::Literal(Literal::String("two".to_string()), dummy_span()),
            span: dummy_span(),
        };

        let else_arm = Some(Box::new(Expression::Literal(
            Literal::String("other".to_string()),
            dummy_span(),
        )));

        let result = desugar_when_expression(
            subject,
            vec![arm1, arm2],
            else_arm,
            dummy_span(),
            &mut context,
        )
        .expect("when expression should desugar successfully");

        match result {
            IrExpression::Switch {
                discriminant,
                cases,
                java_type,
                ..
            } => {
                match *discriminant {
                    IrExpression::Identifier { ref name, .. } => assert_eq!(name, "x"),
                    other => panic!("Expected identifier discriminant, got {:?}", other),
                }

                assert_eq!(cases.len(), 3);
                match &cases[0].labels[0] {
                    IrCaseLabel::Literal(Literal::Number(value)) => assert_eq!(value, "1"),
                    other => panic!("Expected literal label, got {:?}", other),
                }
                match &cases[2].labels[0] {
                    IrCaseLabel::Default => {}
                    other => panic!("Expected default label, got {:?}", other),
                }

                assert_eq!(java_type, JavaType::string());
            }
            other => panic!("Expected switch expression, got {:?}", other),
        }
    }

    #[test]
    fn test_desugar_when_expression_with_wildcard_default() {
        let mut context = test_context();
        context.add_variable("value".to_string(), JavaType::int());

        let subject = Some(Box::new(Expression::Identifier(
            "value".to_string(),
            dummy_span(),
        )));

        let arm1 = WhenArm {
            pattern: Pattern::Literal(Literal::Number("1".to_string()), dummy_span()),
            body: Expression::Literal(Literal::String("one".to_string()), dummy_span()),
            span: dummy_span(),
        };
        let default_arm = WhenArm {
            pattern: Pattern::Wildcard(dummy_span()),
            body: Expression::Literal(Literal::String("other".to_string()), dummy_span()),
            span: dummy_span(),
        };

        let result = desugar_when_expression(
            subject,
            vec![arm1, default_arm],
            None,
            dummy_span(),
            &mut context,
        )
        .expect("when expression with wildcard should desugar successfully");

        match result {
            IrExpression::Switch { cases, .. } => {
                assert_eq!(cases.len(), 2);
                match &cases[1].labels[0] {
                    IrCaseLabel::Default => {}
                    other => panic!("Expected default label, got {:?}", other),
                }
            }
            other => panic!("Expected switch expression, got {:?}", other),
        }
    }

    #[test]
    fn test_desugar_when_expression_without_subject_is_unsupported() {
        let mut context = test_context();

        let arm = WhenArm {
            pattern: Pattern::Literal(Literal::Number("1".to_string()), dummy_span()),
            body: Expression::Literal(Literal::String("one".to_string()), dummy_span()),
            span: dummy_span(),
        };

        let error = desugar_when_expression(None, vec![arm], None, dummy_span(), &mut context)
            .expect_err("when without subject should be unsupported");

        match error {
            TransformError::UnsupportedConstruct { construct, .. } => {
                assert!(construct.contains("without subject"));
            }
            other => panic!("Expected unsupported construct error, got {:?}", other),
        }
    }

    // Test for extension function desugaring
    #[test]
    fn test_desugar_extension_function_produces_static_method() {
        let mut context = test_context();

        let function_decl = Statement::FunctionDeclaration {
            name: "trimmed".to_string(),
            parameters: vec![],
            return_type: Some(TypeAnnotation::Simple("String".to_string())),
            body: Box::new(Expression::Call {
                function: Box::new(Expression::MemberAccess {
                    object: Box::new(Expression::This(dummy_span())),
                    property: "trim".to_string(),
                    span: dummy_span(),
                }),
                args: vec![],
                span: dummy_span(),
            }),
            modifiers: Modifiers::default(),
            span: dummy_span(),
        };

        let receiver_type = TypeAnnotation::Simple("String".to_string());

        let result = desugar_extension_function(
            receiver_type,
            Box::new(function_decl),
            dummy_span(),
            &mut context,
        )
        .expect("extension function should desugar successfully");

        match result {
            IrStatement::MethodDeclaration {
                parameters,
                modifiers,
                return_type,
                ..
            } => {
                assert!(modifiers.is_static);
                assert_eq!(parameters.len(), 1, "receiver parameter should be present");
                assert_eq!(return_type, JavaType::string());
            }
            other => panic!("Expected method declaration, got {:?}", other),
        }
    }

    // Test for string interpolation desugaring
    #[test]
    fn test_desugar_string_interpolation_to_format_expression() {
        let mut context = test_context();
        context.add_variable("name".to_string(), JavaType::string());

        let parts = vec![
            StringPart::Text("Hello, ".to_string()),
            StringPart::Expression(Expression::Identifier("name".to_string(), dummy_span())),
            StringPart::Text("!".to_string()),
        ];

        let result = desugar_string_interpolation(parts, dummy_span(), &mut context)
            .expect("string interpolation should desugar to format");

        match result {
            IrExpression::StringFormat {
                format_string,
                args,
                ..
            } => {
                assert_eq!(format_string, "Hello, %s!");
                assert_eq!(args.len(), 1);
                match &args[0] {
                    IrExpression::Identifier { name, .. } => assert_eq!(name, "name"),
                    other => panic!("Expected identifier argument, got {:?}", other),
                }
            }
            other => panic!("Expected string format expression, got {:?}", other),
        }
    }

    #[test]
    fn test_desugar_string_interpolation_without_expressions_returns_literal() {
        let mut context = test_context();

        let parts = vec![StringPart::Text("No expressions".to_string())];

        let result = desugar_string_interpolation(parts, dummy_span(), &mut context)
            .expect("pure text interpolation should produce literal");

        match result {
            IrExpression::Literal(Literal::String(text), _) => {
                assert_eq!(text, "No expressions");
            }
            other => panic!("Expected literal string, got {:?}", other),
        }
    }

    #[test]
    fn test_desugar_spawn_expression_produces_virtual_thread() {
        let mut context = test_context();

        let spawn_body = Box::new(Expression::Literal(
            Literal::String("work".to_string()),
            dummy_span(),
        ));

        let result = desugar_spawn_expression(spawn_body, dummy_span(), &mut context)
            .expect("spawn expression should desugar successfully");

        match result {
            IrExpression::VirtualThread {
                operation, args, ..
            } => {
                assert!(matches!(operation, VirtualThreadOp::Start));
                assert_eq!(args.len(), 1);
                if let IrExpression::Lambda {
                    functional_interface,
                    ..
                } = &args[0]
                {
                    assert_eq!(functional_interface, "Runnable");
                } else {
                    panic!("Expected lambda argument for virtual thread");
                }
            }
            other => panic!("Expected virtual thread expression, got {:?}", other),
        }
    }

    #[test]
    fn test_transform_concurrency_spawn_into_expression_statement() {
        let mut context = test_context();

        let spawn_stmt = Statement::Concurrency(ConcurrencyConstruct::Spawn {
            body: Box::new(Expression::Literal(
                Literal::String("task".to_string()),
                dummy_span(),
            )),
            span: dummy_span(),
        });

        let result = transform_statement(spawn_stmt, &mut context)
            .expect("transforming spawn statement should succeed");

        assert_eq!(result.len(), 1);
        match &result[0] {
            IrStatement::Expression { expr, .. } => match expr {
                IrExpression::VirtualThread { .. } => {}
                other => panic!("Expected virtual thread expression, got {:?}", other),
            },
            other => panic!("Expected expression statement, got {:?}", other),
        }
    }

    #[test]
    fn test_transform_concurrency_async_into_expression_statement() {
        let mut context = test_context();

        let async_stmt = Statement::Concurrency(ConcurrencyConstruct::Async {
            body: Box::new(Expression::Literal(
                Literal::String("value".to_string()),
                dummy_span(),
            )),
            span: dummy_span(),
        });

        let result = transform_statement(async_stmt, &mut context)
            .expect("transforming async statement should succeed");

        match &result[0] {
            IrStatement::Expression { expr, .. } => match expr {
                IrExpression::CompletableFuture { operation, .. } => {
                    assert!(matches!(operation, CompletableFutureOp::SupplyAsync));
                }
                other => panic!("Expected CompletableFuture expression, got {:?}", other),
            },
            other => panic!("Expected expression statement, got {:?}", other),
        }
    }

    #[test]
    fn test_transform_concurrency_await_into_expression_statement() {
        let mut context = test_context();
        context.add_variable(
            "future".to_string(),
            JavaType::Reference {
                name: "CompletableFuture".to_string(),
                generic_args: vec![JavaType::object()],
            },
        );

        let await_stmt = Statement::Concurrency(ConcurrencyConstruct::Await {
            expr: Box::new(Expression::Identifier("future".to_string(), dummy_span())),
            span: dummy_span(),
        });

        let result = transform_statement(await_stmt, &mut context)
            .expect("transforming await statement should succeed");

        match &result[0] {
            IrStatement::Expression { expr, .. } => match expr {
                IrExpression::CompletableFuture { operation, .. } => {
                    assert!(matches!(operation, CompletableFutureOp::Get));
                }
                other => panic!("Expected CompletableFuture get expression, got {:?}", other),
            },
            other => panic!("Expected expression statement, got {:?}", other),
        }
    }

    #[test]
    fn test_transform_resource_management_use_into_expression_statement() {
        let mut context = test_context();

        context.add_variable(
            "file".to_string(),
            JavaType::Reference {
                name: "InputStream".to_string(),
                generic_args: vec![],
            },
        );

        let use_stmt = Statement::ResourceManagement(ResourceManagement::Use {
            resource: Box::new(Expression::Identifier("file".to_string(), dummy_span())),
            body: Box::new(Expression::Block {
                statements: vec![],
                span: dummy_span(),
            }),
            span: dummy_span(),
        });

        let result = transform_statement(use_stmt, &mut context)
            .expect("transforming use statement should succeed");

        match &result[0] {
            IrStatement::Expression { expr, .. } => match expr {
                IrExpression::TryWithResources { .. } => {}
                other => panic!("Expected try-with-resources expression, got {:?}", other),
            },
            other => panic!("Expected expression statement, got {:?}", other),
        }
    }

    #[test]
    fn test_transform_resource_management_defer_into_expression_statement() {
        let mut context = test_context();

        let defer_stmt = Statement::ResourceManagement(ResourceManagement::Defer {
            body: Box::new(Expression::Literal(
                Literal::String("cleanup".to_string()),
                dummy_span(),
            )),
            span: dummy_span(),
        });

        let result = transform_statement(defer_stmt, &mut context)
            .expect("transforming defer statement should succeed");

        match &result[0] {
            IrStatement::Expression { expr, .. } => match expr {
                IrExpression::Block { .. } => {}
                other => panic!("Expected block expression, got {:?}", other),
            },
            other => panic!("Expected expression statement, got {:?}", other),
        }
    }

    #[test]
    fn test_block_expression_transforms_statements() {
        let mut context = test_context();

        let block_expr = Expression::Block {
            statements: vec![Statement::Concurrency(ConcurrencyConstruct::Spawn {
                body: Box::new(Expression::Literal(
                    Literal::String("work".to_string()),
                    dummy_span(),
                )),
                span: dummy_span(),
            })],
            span: dummy_span(),
        };

        let result = transform_expression(block_expr, &mut context)
            .expect("block expression should transform successfully");

        match result {
            IrExpression::Block { statements, .. } => {
                assert_eq!(statements.len(), 1);
            }
            other => panic!("Expected block IR expression, got {:?}", other),
        }
    }

    #[test]
    fn test_desugar_use_expression_with_lambda_creates_try_with_resources() {
        let mut context = test_context();

        // resource expression is an existing identifier for simplicity
        context.add_variable(
            "file".to_string(),
            JavaType::Reference {
                name: "InputStream".to_string(),
                generic_args: vec![],
            },
        );

        let resource = Box::new(Expression::Identifier("file".to_string(), dummy_span()));
        let body = Box::new(Expression::Lambda {
            parameters: vec![Parameter {
                name: "f".to_string(),
                type_annotation: None,
                default_value: None,
                span: dummy_span(),
            }],
            body: Box::new(Expression::Literal(
                Literal::String("done".to_string()),
                dummy_span(),
            )),
            span: dummy_span(),
        });

        let result = desugar_use_expression(resource, body, dummy_span(), &mut context)
            .expect("use expression should desugar successfully");

        match result {
            IrExpression::TryWithResources {
                resources,
                body,
                java_type,
                ..
            } => {
                assert_eq!(resources.len(), 1);
                assert_eq!(resources[0].name, "f");
                assert_eq!(java_type, JavaType::string());
                match *body {
                    IrExpression::Literal(Literal::String(ref value), _) => {
                        assert_eq!(value, "done");
                    }
                    other => panic!("Expected literal body, got {:?}", other),
                }
            }
            other => panic!("Expected try-with-resources expression, got {:?}", other),
        }
    }

    #[test]
    fn test_desugar_defer_expression_creates_block() {
        let mut context = test_context();

        let body = Box::new(Expression::Literal(
            Literal::String("cleanup".to_string()),
            dummy_span(),
        ));

        let result = desugar_defer_expression(body, dummy_span(), &mut context)
            .expect("defer expression should desugar successfully");

        match result {
            IrExpression::Block { statements, .. } => {
                assert_eq!(statements.len(), 1);
            }
            other => panic!("Expected block expression, got {:?}", other),
        }
    }

    // Test for use expression desugaring
    // Test for defer expression desugaring
    #[test]
    #[should_panic(expected = "not yet implemented: desugar_defer_expression")]
    fn test_desugar_defer_expression_fails() {
        let mut context = test_context();

        // defer { cleanup() }
        let body = Box::new(Expression::Call {
            function: Box::new(Expression::Identifier("cleanup".to_string(), dummy_span())),
            args: vec![],
            span: dummy_span(),
        });

        // This should fail because desugaring is not implemented yet
        let _result = desugar_defer_expression(body, dummy_span(), &mut context);
    }

    // Test for default parameters desugaring
    #[test]
    #[should_panic(expected = "not yet implemented: desugar_default_parameters")]
    fn test_desugar_default_parameters_fails() {
        let mut context = test_context();

        // fun greet(name: String, greeting: String = "Hello") = "$greeting, $name"
        let param1 = Parameter {
            name: "name".to_string(),
            type_annotation: Some(TypeAnnotation::Simple("String".to_string())),
            default_value: None,
            span: dummy_span(),
        };
        let param2 = Parameter {
            name: "greeting".to_string(),
            type_annotation: Some(TypeAnnotation::Simple("String".to_string())),
            default_value: Some(Expression::Literal(
                Literal::String("Hello".to_string()),
                dummy_span(),
            )),
            span: dummy_span(),
        };

        let body = Box::new(Expression::StringInterpolation {
            parts: vec![
                StringPart::Expression(Expression::Identifier(
                    "greeting".to_string(),
                    dummy_span(),
                )),
                StringPart::Text(", ".to_string()),
                StringPart::Expression(Expression::Identifier("name".to_string(), dummy_span())),
            ],
            span: dummy_span(),
        });

        // This should fail because desugaring is not implemented yet
        let _result = desugar_default_parameters(
            "greet".to_string(),
            vec![param1, param2],
            Some(TypeAnnotation::Simple("String".to_string())),
            body,
            Modifiers::default(),
            dummy_span(),
            &mut context,
        );
    }

    // Test for named arguments desugaring
    #[test]
    #[should_panic(expected = "not yet implemented: desugar_named_arguments")]
    fn test_desugar_named_arguments_fails() {
        let mut context = test_context();

        // greet(name = "World", greeting = "Hi")
        let function = Box::new(Expression::Identifier("greet".to_string(), dummy_span()));
        let args = vec![
            Argument::Named {
                name: "name".to_string(),
                value: Expression::Literal(Literal::String("World".to_string()), dummy_span()),
                span: dummy_span(),
            },
            Argument::Named {
                name: "greeting".to_string(),
                value: Expression::Literal(Literal::String("Hi".to_string()), dummy_span()),
                span: dummy_span(),
            },
        ];

        // This should fail because desugaring is not implemented yet
        let _result = desugar_named_arguments(function, args, dummy_span(), &mut context);
    }

    // Test for top-level function desugaring
    #[test]
    #[should_panic(expected = "not yet implemented: desugar_top_level_function")]
    fn test_desugar_top_level_function_fails() {
        let mut context = test_context();

        // fun topLevelFunction(x: Int): String = x.toString()
        let function = Statement::FunctionDeclaration {
            name: "topLevelFunction".to_string(),
            parameters: vec![Parameter {
                name: "x".to_string(),
                type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
                default_value: None,
                span: dummy_span(),
            }],
            return_type: Some(TypeAnnotation::Simple("String".to_string())),
            body: Box::new(Expression::Call {
                function: Box::new(Expression::MemberAccess {
                    object: Box::new(Expression::Identifier("x".to_string(), dummy_span())),
                    property: "toString".to_string(),
                    span: dummy_span(),
                }),
                args: vec![],
                span: dummy_span(),
            }),
            modifiers: Modifiers::default(),
            span: dummy_span(),
        };

        // This should fail because desugaring is not implemented yet
        let _result = desugar_top_level_function(function, &mut context);
    }

    // Test for data class desugaring
    #[test]
    fn test_desugar_immutable_data_class_creates_record() {
        let mut context = test_context();

        let params = vec![
            Parameter {
                name: "id".to_string(),
                type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
                default_value: None,
                span: dummy_span(),
            },
            Parameter {
                name: "name".to_string(),
                type_annotation: Some(TypeAnnotation::Simple("String".to_string())),
                default_value: None,
                span: dummy_span(),
            },
        ];

        let result = desugar_data_class(
            "User".to_string(),
            params,
            vec!["T".to_string()],
            false,
            Modifiers::default(),
            dummy_span(),
            &mut context,
        )
        .expect("immutable data class should desugar to record");

        match result {
            IrStatement::RecordDeclaration {
                name,
                type_parameters,
                components,
                ..
            } => {
                assert_eq!(name, "User");
                assert_eq!(type_parameters.len(), 1);
                assert_eq!(components.len(), 2);
                assert_eq!(components[0].name, "id");
                assert_eq!(components[1].name, "name");
            }
            other => panic!("Expected record declaration, got {:?}", other),
        }
    }

    #[test]
    fn test_desugar_mutable_data_class_creates_class_with_fields() {
        let mut context = test_context();

        let params = vec![
            Parameter {
                name: "id".to_string(),
                type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
                default_value: None,
                span: dummy_span(),
            },
            Parameter {
                name: "name".to_string(),
                type_annotation: None,
                default_value: Some(Expression::Literal(
                    Literal::String("unknown".to_string()),
                    dummy_span(),
                )),
                span: dummy_span(),
            },
        ];

        let result = desugar_data_class(
            "User".to_string(),
            params,
            vec![],
            true,
            Modifiers::default(),
            dummy_span(),
            &mut context,
        )
        .expect("mutable data class should desugar to class");

        match result {
            IrStatement::ClassDeclaration { fields, .. } => {
                assert_eq!(fields.len(), 2);
                match &fields[1] {
                    IrStatement::FieldDeclaration {
                        name, initializer, ..
                    } => {
                        assert_eq!(name, "name");
                        assert!(initializer.is_some());
                    }
                    other => panic!("Expected field declaration, got {:?}", other),
                }
            }
            other => panic!("Expected class declaration, got {:?}", other),
        }
    }

    // Test for null-safe member access desugaring
    #[test]
    fn test_desugar_null_safe_member_access_creates_operation() {
        let mut context = test_context();
        context.add_variable("obj".to_string(), JavaType::object());

        let result = desugar_null_safe_member_access(
            Box::new(Expression::Identifier("obj".to_string(), dummy_span())),
            "property".to_string(),
            dummy_span(),
            &mut context,
        )
        .expect("null-safe member access should desugar successfully");

        match result {
            IrExpression::NullSafeOperation {
                expr,
                operation,
                default_value,
                java_type,
                ..
            } => {
                match *expr {
                    IrExpression::Identifier { ref name, .. } => {
                        assert_eq!(name, "obj");
                    }
                    other => panic!("Expected identifier receiver, got {:?}", other),
                }

                match *operation {
                    IrExpression::FieldAccess { ref field_name, .. } => {
                        assert_eq!(field_name, "property");
                    }
                    other => panic!("Expected field access, got {:?}", other),
                }

                assert!(default_value.is_some());
                assert_eq!(java_type, JavaType::object());
            }
            other => panic!("Expected null-safe operation, got {:?}", other),
        }
    }

    // Test for null-safe index access desugaring
    #[test]
    fn test_desugar_null_safe_index_access_uses_array_element_type() {
        let mut context = test_context();
        context.add_variable(
            "arr".to_string(),
            JavaType::Array {
                element_type: Box::new(JavaType::string()),
                dimensions: 1,
            },
        );

        let result = desugar_null_safe_index_access(
            Box::new(Expression::Identifier("arr".to_string(), dummy_span())),
            Box::new(Expression::Literal(
                Literal::Number("0".to_string()),
                dummy_span(),
            )),
            dummy_span(),
            &mut context,
        )
        .expect("null-safe index access should desugar successfully");

        match result {
            IrExpression::NullSafeOperation {
                expr,
                operation,
                default_value,
                java_type,
                ..
            } => {
                match *expr {
                    IrExpression::Identifier { ref name, .. } => assert_eq!(name, "arr"),
                    other => panic!("Expected identifier receiver, got {:?}", other),
                }

                match *operation {
                    IrExpression::ArrayAccess {
                        java_type: ref access_type,
                        ..
                    } => {
                        assert_eq!(access_type, &JavaType::string());
                    }
                    other => panic!("Expected array access, got {:?}", other),
                }

                assert!(default_value.is_some());
                assert_eq!(java_type, JavaType::string());
            }
            other => panic!("Expected null-safe operation, got {:?}", other),
        }
    }

    // Test for elvis operator desugaring
    #[test]
    fn test_desugar_elvis_operator_creates_null_safe_operation() {
        let mut context = test_context();
        context.add_variable("value".to_string(), JavaType::string());

        let result = desugar_elvis_operator(
            Box::new(Expression::Identifier("value".to_string(), dummy_span())),
            Box::new(Expression::Literal(
                Literal::String("default".to_string()),
                dummy_span(),
            )),
            dummy_span(),
            &mut context,
        )
        .expect("Elvis operator should desugar successfully");

        match result {
            IrExpression::NullSafeOperation {
                expr,
                operation,
                default_value,
                java_type,
                ..
            } => {
                match *expr {
                    IrExpression::Identifier { ref name, .. } => assert_eq!(name, "value"),
                    other => panic!("Expected identifier receiver, got {:?}", other),
                }

                match *operation {
                    IrExpression::Identifier { ref name, .. } => assert_eq!(name, "value"),
                    other => panic!("Expected identifier operation, got {:?}", other),
                }

                match default_value {
                    Some(boxed) => match *boxed {
                        IrExpression::Literal(Literal::String(ref s), _) => {
                            assert_eq!(s, "default");
                        }
                        other => panic!("Expected string literal default, got {:?}", other),
                    },
                    None => panic!("Expected default value for Elvis operator"),
                }

                assert_eq!(java_type, JavaType::string());
            }
            other => panic!("Expected null-safe operation, got {:?}", other),
        }
    }

    #[test]
    fn test_desugar_elvis_operator_rejects_primitive_left() {
        let mut context = test_context();
        context.add_variable("number".to_string(), JavaType::Primitive("int".to_string()));

        let error = desugar_elvis_operator(
            Box::new(Expression::Identifier("number".to_string(), dummy_span())),
            Box::new(Expression::Literal(
                Literal::Number("0".to_string()),
                dummy_span(),
            )),
            dummy_span(),
            &mut context,
        )
        .expect_err("Primitive left-hand side should be rejected");

        match error {
            TransformError::NullSafetyError { ref message, .. } => {
                assert!(message.contains("Elvis operator"));
            }
            other => panic!("Expected null safety error, got {:?}", other),
        }
    }

    // Test for type inference
    #[test]
    #[should_panic(expected = "not yet implemented: infer_java_type")]
    fn test_infer_java_type_fails() {
        let context = test_context();

        // Should infer String from string literal - convert to IrExpression first
        let initializer = Expression::Literal(Literal::String("hello".to_string()), dummy_span());
        let ir_initializer =
            IrExpression::Literal(Literal::String("hello".to_string()), dummy_span());

        // This should fail because type inference is not implemented yet
        let _result = infer_java_type(None, Some(&ir_initializer), &context);
    }

    // Test for type annotation conversion
    #[test]
    #[should_panic(expected = "not yet implemented: convert_type_annotation")]
    fn test_convert_type_annotation_fails() {
        // Convert jv type annotation to Java type
        let type_annotation = TypeAnnotation::Generic {
            name: "List".to_string(),
            type_args: vec![TypeAnnotation::Simple("String".to_string())],
        };

        // This should fail because type conversion is not implemented yet
        let _result = convert_type_annotation(type_annotation);
    }

    // Test for utility class name generation
    #[test]
    #[should_panic(expected = "not yet implemented: generate_utility_class_name")]
    fn test_generate_utility_class_name_fails() {
        // Should generate something like "com.example.MyFileKt"
        let _result = generate_utility_class_name(Some("com.example"), "MyFile.jv");
    }

    // Test for extension class name generation
    #[test]
    #[should_panic(expected = "not yet implemented: generate_extension_class_name")]
    fn test_generate_extension_class_name_fails() {
        let receiver_type = TypeAnnotation::Simple("String".to_string());

        // Should generate something like "StringExtensions"
        let _result = generate_extension_class_name(&receiver_type);
    }

    // Test for main transformation functions
    #[test]
    #[should_panic(expected = "not yet implemented: transform_program_with_context")]
    fn test_transform_program_fails() {
        // Create a simple program
        let program = Program {
            package: Some("com.example".to_string()),
            imports: vec![],
            statements: vec![Statement::ValDeclaration {
                name: "greeting".to_string(),
                type_annotation: None,
                initializer: Expression::Literal(
                    Literal::String("Hello, World!".to_string()),
                    dummy_span(),
                ),
                modifiers: Modifiers::default(),
                span: dummy_span(),
            }],
            span: dummy_span(),
        };

        // This should fail because transformation is not implemented yet
        let _result = transform_program(program);
    }

    #[test]
    #[should_panic(expected = "not yet implemented: transform_statement")]
    fn test_transform_statement_fails() {
        let mut context = test_context();

        let stmt = Statement::ValDeclaration {
            name: "x".to_string(),
            type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
            initializer: Expression::Literal(Literal::Number("42".to_string()), dummy_span()),
            modifiers: Modifiers::default(),
            span: dummy_span(),
        };

        // This should fail because statement transformation is not implemented yet
        let _result = transform_statement(stmt, &mut context);
    }

    #[test]
    #[should_panic(expected = "not yet implemented: transform_expression")]
    fn test_transform_expression_fails() {
        let mut context = test_context();

        let expr = Expression::Binary {
            left: Box::new(Expression::Literal(
                Literal::Number("1".to_string()),
                dummy_span(),
            )),
            op: BinaryOp::Add,
            right: Box::new(Expression::Literal(
                Literal::Number("2".to_string()),
                dummy_span(),
            )),
            span: dummy_span(),
        };

        // This should fail because expression transformation is not implemented yet
        let _result = transform_expression(expr, &mut context);
    }

    // Complex integration tests for combined desugaring

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn test_complex_when_with_patterns_and_guards_fails() {
        let mut context = test_context();

        // when (value) {
        //     1..10 -> "small"
        //     is String if it.length > 5 -> "long string"
        //     else -> "other"
        // }
        let subject = Some(Box::new(Expression::Identifier(
            "value".to_string(),
            dummy_span(),
        )));

        let range_pattern = Pattern::Range {
            start: Box::new(Expression::Literal(
                Literal::Number("1".to_string()),
                dummy_span(),
            )),
            end: Box::new(Expression::Literal(
                Literal::Number("10".to_string()),
                dummy_span(),
            )),
            span: dummy_span(),
        };

        let guard_pattern = Pattern::Guard {
            pattern: Box::new(Pattern::Constructor {
                name: "String".to_string(),
                patterns: vec![Pattern::Identifier("it".to_string(), dummy_span())],
                span: dummy_span(),
            }),
            condition: Expression::Binary {
                left: Box::new(Expression::MemberAccess {
                    object: Box::new(Expression::Identifier("it".to_string(), dummy_span())),
                    property: "length".to_string(),
                    span: dummy_span(),
                }),
                op: BinaryOp::Greater,
                right: Box::new(Expression::Literal(
                    Literal::Number("5".to_string()),
                    dummy_span(),
                )),
                span: dummy_span(),
            },
            span: dummy_span(),
        };

        let arms = vec![
            WhenArm {
                pattern: range_pattern,
                body: Expression::Literal(Literal::String("small".to_string()), dummy_span()),
                span: dummy_span(),
            },
            WhenArm {
                pattern: guard_pattern,
                body: Expression::Literal(Literal::String("long string".to_string()), dummy_span()),
                span: dummy_span(),
            },
        ];

        let else_arm = Some(Box::new(Expression::Literal(
            Literal::String("other".to_string()),
            dummy_span(),
        )));

        // This should fail because complex pattern desugaring is not implemented yet
        let _result = desugar_when_expression(subject, arms, else_arm, dummy_span(), &mut context);
    }

    #[test]
    fn test_desugar_async_expression_produces_completable_future() {
        let mut context = test_context();

        let async_body = Box::new(Expression::Literal(
            Literal::String("value".to_string()),
            dummy_span(),
        ));

        let result = desugar_async_expression(async_body, dummy_span(), &mut context)
            .expect("async expression should desugar successfully");

        match result {
            IrExpression::CompletableFuture {
                operation, args, ..
            } => {
                assert!(matches!(operation, CompletableFutureOp::SupplyAsync));
                assert_eq!(args.len(), 1);
                match &args[0] {
                    IrExpression::Lambda {
                        functional_interface,
                        ..
                    } => {
                        assert_eq!(functional_interface, "Supplier");
                    }
                    other => panic!("Expected lambda argument, got {:?}", other),
                }
            }
            other => panic!("Expected CompletableFuture expression, got {:?}", other),
        }
    }

    #[test]
    fn test_desugar_await_expression_produces_get_operation() {
        let mut context = test_context();
        context.add_variable(
            "future".to_string(),
            JavaType::Reference {
                name: "CompletableFuture".to_string(),
                generic_args: vec![JavaType::string()],
            },
        );

        let await_target = Box::new(Expression::Identifier("future".to_string(), dummy_span()));

        let result = desugar_await_expression(await_target, dummy_span(), &mut context)
            .expect("await expression should desugar successfully");

        match result {
            IrExpression::CompletableFuture {
                operation,
                args,
                java_type,
                ..
            } => {
                assert!(matches!(operation, CompletableFutureOp::Get));
                assert_eq!(args.len(), 1);
                assert_eq!(java_type, JavaType::string());
            }
            other => panic!("Expected CompletableFuture get expression, got {:?}", other),
        }
    }

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn test_extension_function_with_generics_fails() {
        let mut context = test_context();

        // fun <T> List<T>.secondOrNull(): T? = if (this.size >= 2) this[1] else null
        let receiver_type = TypeAnnotation::Generic {
            name: "List".to_string(),
            type_args: vec![TypeAnnotation::Simple("T".to_string())],
        };

        let function_decl = Statement::FunctionDeclaration {
            name: "secondOrNull".to_string(),
            parameters: vec![],
            return_type: Some(TypeAnnotation::Nullable(Box::new(TypeAnnotation::Simple(
                "T".to_string(),
            )))),
            body: Box::new(Expression::If {
                condition: Box::new(Expression::Binary {
                    left: Box::new(Expression::MemberAccess {
                        object: Box::new(Expression::This(dummy_span())),
                        property: "size".to_string(),
                        span: dummy_span(),
                    }),
                    op: BinaryOp::GreaterEqual,
                    right: Box::new(Expression::Literal(
                        Literal::Number("2".to_string()),
                        dummy_span(),
                    )),
                    span: dummy_span(),
                }),
                then_branch: Box::new(Expression::IndexAccess {
                    object: Box::new(Expression::This(dummy_span())),
                    index: Box::new(Expression::Literal(
                        Literal::Number("1".to_string()),
                        dummy_span(),
                    )),
                    span: dummy_span(),
                }),
                else_branch: Some(Box::new(Expression::Literal(Literal::Null, dummy_span()))),
                span: dummy_span(),
            }),
            modifiers: Modifiers::default(),
            span: dummy_span(),
        };

        // This should fail because generic extension function desugaring is not implemented yet
        let _result = desugar_extension_function(
            receiver_type,
            Box::new(function_decl),
            dummy_span(),
            &mut context,
        );
    }

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn test_nested_null_safe_operations_fails() {
        let mut context = test_context();

        // user?.profile?.address?.street
        let nested_access = Expression::NullSafeMemberAccess {
            object: Box::new(Expression::NullSafeMemberAccess {
                object: Box::new(Expression::NullSafeMemberAccess {
                    object: Box::new(Expression::Identifier("user".to_string(), dummy_span())),
                    property: "profile".to_string(),
                    span: dummy_span(),
                }),
                property: "address".to_string(),
                span: dummy_span(),
            }),
            property: "street".to_string(),
            span: dummy_span(),
        };

        // This should fail because nested null-safe operation desugaring is not implemented yet
        let _result = transform_expression(nested_access, &mut context);
    }

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn test_data_class_with_default_parameters_fails() {
        let mut context = test_context();

        // data class Config(val host: String = "localhost", val port: Int = 8080, val ssl: Boolean = false)
        let params = vec![
            Parameter {
                name: "host".to_string(),
                type_annotation: Some(TypeAnnotation::Simple("String".to_string())),
                default_value: Some(Expression::Literal(
                    Literal::String("localhost".to_string()),
                    dummy_span(),
                )),
                span: dummy_span(),
            },
            Parameter {
                name: "port".to_string(),
                type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
                default_value: Some(Expression::Literal(
                    Literal::Number("8080".to_string()),
                    dummy_span(),
                )),
                span: dummy_span(),
            },
            Parameter {
                name: "ssl".to_string(),
                type_annotation: Some(TypeAnnotation::Simple("Boolean".to_string())),
                default_value: Some(Expression::Literal(Literal::Boolean(false), dummy_span())),
                span: dummy_span(),
            },
        ];

        // This should fail because data class with default parameters desugaring is not implemented yet
        let _result = desugar_data_class(
            "Config".to_string(),
            params,
            vec![],
            false, // immutable
            Modifiers::default(),
            dummy_span(),
            &mut context,
        );
    }

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn test_spawn_with_use_resource_fails() {
        let mut context = test_context();

        // spawn {
        //     use(openConnection()) { conn ->
        //         conn.sendData("Hello")
        //         defer { conn.cleanup() }
        //     }
        // }
        let use_expr = Expression::Block {
            statements: vec![Statement::ResourceManagement(ResourceManagement::Use {
                resource: Box::new(Expression::Call {
                    function: Box::new(Expression::Identifier(
                        "openConnection".to_string(),
                        dummy_span(),
                    )),
                    args: vec![],
                    span: dummy_span(),
                }),
                body: Box::new(Expression::Lambda {
                    parameters: vec![Parameter {
                        name: "conn".to_string(),
                        type_annotation: None,
                        default_value: None,
                        span: dummy_span(),
                    }],
                    body: Box::new(Expression::Block {
                        statements: vec![
                            Statement::Expression {
                                expr: Expression::Call {
                                    function: Box::new(Expression::MemberAccess {
                                        object: Box::new(Expression::Identifier(
                                            "conn".to_string(),
                                            dummy_span(),
                                        )),
                                        property: "sendData".to_string(),
                                        span: dummy_span(),
                                    }),
                                    args: vec![Argument::Positional(Expression::Literal(
                                        Literal::String("Hello".to_string()),
                                        dummy_span(),
                                    ))],
                                    span: dummy_span(),
                                },
                                span: dummy_span(),
                            },
                            Statement::ResourceManagement(ResourceManagement::Defer {
                                body: Box::new(Expression::Call {
                                    function: Box::new(Expression::MemberAccess {
                                        object: Box::new(Expression::Identifier(
                                            "conn".to_string(),
                                            dummy_span(),
                                        )),
                                        property: "cleanup".to_string(),
                                        span: dummy_span(),
                                    }),
                                    args: vec![],
                                    span: dummy_span(),
                                }),
                                span: dummy_span(),
                            }),
                        ],
                        span: dummy_span(),
                    }),
                    span: dummy_span(),
                }),
                span: dummy_span(),
            })],
            span: dummy_span(),
        };

        // This should fail because complex resource management in spawn is not implemented yet
        let _result = desugar_spawn_expression(Box::new(use_expr), dummy_span(), &mut context);
    }

    // Error type tests
    #[test]
    fn test_transform_error_types() {
        let span = dummy_span();

        let type_error = TransformError::TypeInferenceError {
            message: "Cannot infer type".to_string(),
            span: span.clone(),
        };
        assert!(type_error.to_string().contains("Type inference failed"));

        let unsupported_error = TransformError::UnsupportedConstruct {
            construct: "goto statement".to_string(),
            span: span.clone(),
        };
        assert!(unsupported_error
            .to_string()
            .contains("Unsupported construct"));

        let pattern_error = TransformError::InvalidPattern {
            message: "Invalid range pattern".to_string(),
            span: span.clone(),
        };
        assert!(pattern_error.to_string().contains("Invalid pattern"));
    }

    // Integration test for IR node structure validation
    #[test]
    fn test_ir_node_structure_completeness() {
        // Test that all IR nodes can be created and have the expected structure

        // Test JavaType variants
        let primitive_type = JavaType::Primitive("int".to_string());
        let reference_type = JavaType::Reference {
            name: "String".to_string(),
            generic_args: vec![],
        };
        let array_type = JavaType::Array {
            element_type: Box::new(JavaType::int()),
            dimensions: 1,
        };
        let functional_type = JavaType::Functional {
            interface_name: "Function".to_string(),
            param_types: vec![JavaType::string()],
            return_type: Box::new(JavaType::string()),
        };

        assert_eq!(primitive_type, JavaType::Primitive("int".to_string()));
        assert!(matches!(reference_type, JavaType::Reference { .. }));
        assert!(matches!(array_type, JavaType::Array { .. }));
        assert!(matches!(functional_type, JavaType::Functional { .. }));

        // Test IrExpression variants
        let literal_expr = IrExpression::Literal(Literal::Number("42".to_string()), dummy_span());
        assert!(matches!(literal_expr, IrExpression::Literal(_, _)));

        let method_call = IrExpression::MethodCall {
            receiver: None,
            method_name: "staticMethod".to_string(),
            args: vec![],
            java_type: JavaType::void(),
            span: dummy_span(),
        };
        assert!(matches!(method_call, IrExpression::MethodCall { .. }));

        // Test IrStatement variants
        let var_decl = IrStatement::VariableDeclaration {
            name: "x".to_string(),
            java_type: JavaType::int(),
            initializer: Some(IrExpression::Literal(
                Literal::Number("42".to_string()),
                dummy_span(),
            )),
            is_final: true,
            modifiers: IrModifiers::default(),
            span: dummy_span(),
        };
        assert!(matches!(var_decl, IrStatement::VariableDeclaration { .. }));

        // Test modifiers
        let modifiers = IrModifiers {
            visibility: IrVisibility::Public,
            is_static: true,
            is_final: false,
            ..Default::default()
        };
        assert_eq!(modifiers.visibility, IrVisibility::Public);
        assert!(modifiers.is_static);
    }
}
