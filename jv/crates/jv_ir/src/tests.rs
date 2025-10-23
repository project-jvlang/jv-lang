#[cfg(test)]
mod tests {
    use crate::context::{RegisteredMethodCall, RegisteredMethodDeclaration, SequenceStyleCache};
    use crate::{
        convert_type_annotation, desugar_async_expression, desugar_await_expression,
        desugar_data_class, desugar_default_parameters, desugar_defer_expression,
        desugar_elvis_operator, desugar_extension_function, desugar_named_arguments,
        desugar_null_safe_index_access, desugar_null_safe_member_access, desugar_spawn_expression,
        desugar_string_interpolation, desugar_top_level_function, desugar_use_expression,
        desugar_val_declaration, desugar_var_declaration, desugar_when_expression,
        generate_extension_class_name, generate_utility_class_name, infer_java_type,
        naming::method_erasure::apply_method_erasure, transform_expression, transform_program,
        transform_program_with_context, transform_program_with_context_profiled,
        transform_statement, CompletableFutureOp, DataFormat, IrCaseLabel,
        IrDeconstructionComponent, IrDeconstructionPattern, IrExpression, IrForEachKind,
        IrForLoopMetadata, IrImplicitWhenEnd, IrModifiers, IrNumericRangeLoop, IrParameter,
        IrResolvedMethodTarget, IrStatement, IrVisibility, JavaType, PipelineShape, SampleMode,
        SampleSourceKind, Schema, SequencePipeline, SequenceSource, SequenceStage,
        SequenceTerminal, SequenceTerminalEvaluation, SequenceTerminalKind, TransformContext,
        TransformError, TransformPools, TransformProfiler, VirtualThreadOp,
    };
    use jv_ast::*;
    use jv_parser_frontend::ParserPipeline;
    use jv_parser_rowan::frontend::RowanPipeline;
    use sha2::{Digest, Sha256};
    use std::fs;
    use std::time::Duration;
    use tempfile::tempdir;

    fn dummy_span() -> Span {
        Span::dummy()
    }

    fn parse_program(source: &str) -> Program {
        RowanPipeline::default()
            .parse(source)
            .expect("snippet should parse")
            .into_program()
    }

    fn parse_when_expression(source: &str) -> Expression {
        let program = parse_program(source);
        for statement in program.statements {
            match statement {
                Statement::ValDeclaration { initializer, .. } => {
                    if let Expression::When { .. } = initializer {
                        return initializer;
                    }
                }
                Statement::Expression { expr, .. } => {
                    if let Expression::When { .. } = expr {
                        return expr;
                    }
                }
                Statement::VarDeclaration {
                    initializer: Some(initializer),
                    ..
                } => {
                    if let Expression::When { .. } = initializer {
                        return initializer;
                    }
                }
                _ => {}
            }
        }
        panic!("expected when expression in snippet");
    }

    #[test]
    fn transform_profiler_collects_metrics() {
        let program = simple_program();
        let mut context = TransformContext::new();
        let mut profiler = TransformProfiler::new();

        let (ir_program, metrics) =
            transform_program_with_context_profiled(program, &mut context, &mut profiler)
                .expect("profiled lowering succeeds");

        assert_eq!(ir_program.type_declarations.len(), 1);
        assert!(metrics.total_duration() >= Duration::ZERO);
        assert!(metrics.stage("lowering").is_some());
        assert!(profiler.latest_metrics().is_some());
    }

    #[test]
    fn transform_expression_creates_regex_pattern_variant() {
        let span = dummy_span();
        let literal = RegexLiteral {
            pattern: "\\d+".to_string(),
            raw: "/\\d+/".to_string(),
            span: span.clone(),
        };
        let mut context = TransformContext::new();
        let ir_expr = transform_expression(Expression::RegexLiteral(literal.clone()), &mut context)
            .expect("regex literal lowering");

        match ir_expr {
            IrExpression::RegexPattern {
                pattern,
                java_type,
                span: ir_span,
            } => {
                assert_eq!(pattern, "\\d+");
                assert_eq!(ir_span, span);
                match java_type {
                    JavaType::Reference { name, generic_args } => {
                        assert_eq!(name, "java.util.regex.Pattern");
                        assert!(
                            generic_args.is_empty(),
                            "pattern type should not carry generics"
                        );
                    }
                    other => panic!("expected pattern reference type, got {:?}", other),
                }
            }
            other => panic!("expected regex pattern ir expression, got {:?}", other),
        }
    }

    #[test]
    fn map_get_infers_value_type_from_map_generics() {
        let mut context = TransformContext::new();
        context.add_variable(
            "result".to_string(),
            JavaType::Reference {
                name: "java.util.LinkedHashMap".to_string(),
                generic_args: vec![
                    JavaType::Reference {
                        name: "String".to_string(),
                        generic_args: vec![],
                    },
                    JavaType::Reference {
                        name: "java.util.List".to_string(),
                        generic_args: vec![JavaType::Reference {
                            name: "String".to_string(),
                            generic_args: vec![],
                        }],
                    },
                ],
            },
        );
        context.add_variable(
            "key".to_string(),
            JavaType::Reference {
                name: "String".to_string(),
                generic_args: vec![],
            },
        );

        let call_expression = Expression::Call {
            function: Box::new(Expression::MemberAccess {
                object: Box::new(Expression::Identifier("result".to_string(), dummy_span())),
                property: "get".to_string(),
                span: dummy_span(),
            }),
            args: vec![Argument::Positional(Expression::Identifier(
                "key".to_string(),
                dummy_span(),
            ))],
            type_arguments: Vec::new(),
            argument_metadata: CallArgumentMetadata::default(),
            span: dummy_span(),
        };

        let ir =
            transform_expression(call_expression, &mut context).expect("map get call should lower");

        match ir {
            IrExpression::MethodCall { java_type, .. } => {
                let expected = JavaType::Reference {
                    name: "java.util.List".to_string(),
                    generic_args: vec![JavaType::Reference {
                        name: "String".to_string(),
                        generic_args: vec![],
                    }],
                };
                assert_eq!(java_type, expected);
            }
            other => panic!("expected method call IR, got {:?}", other),
        }
    }

    #[test]
    fn map_compute_if_absent_preserves_value_type() {
        let mut context = TransformContext::new();
        context.add_variable(
            "result".to_string(),
            JavaType::Reference {
                name: "java.util.LinkedHashMap".to_string(),
                generic_args: vec![
                    JavaType::Reference {
                        name: "String".to_string(),
                        generic_args: vec![],
                    },
                    JavaType::Reference {
                        name: "java.util.List".to_string(),
                        generic_args: vec![JavaType::Reference {
                            name: "String".to_string(),
                            generic_args: vec![],
                        }],
                    },
                ],
            },
        );
        context.add_variable(
            "key".to_string(),
            JavaType::Reference {
                name: "String".to_string(),
                generic_args: vec![],
            },
        );

        let lambda_param = Parameter {
            name: "ignored".to_string(),
            type_annotation: None,
            default_value: None,
            modifiers: ParameterModifiers::default(),
            span: dummy_span(),
        };
        let lambda_body = Expression::Call {
            function: Box::new(Expression::Identifier(
                "ArrayList".to_string(),
                dummy_span(),
            )),
            args: vec![],
            type_arguments: vec![TypeAnnotation::Simple("String".to_string())],
            argument_metadata: CallArgumentMetadata::default(),
            span: dummy_span(),
        };
        let lambda = Expression::Lambda {
            parameters: vec![lambda_param],
            body: Box::new(lambda_body),
            span: dummy_span(),
        };

        let call_expression = Expression::Call {
            function: Box::new(Expression::MemberAccess {
                object: Box::new(Expression::Identifier("result".to_string(), dummy_span())),
                property: "computeIfAbsent".to_string(),
                span: dummy_span(),
            }),
            args: vec![
                Argument::Positional(Expression::Identifier("key".to_string(), dummy_span())),
                Argument::Positional(lambda),
            ],
            type_arguments: Vec::new(),
            argument_metadata: CallArgumentMetadata::default(),
            span: dummy_span(),
        };

        let ir = transform_expression(call_expression, &mut context)
            .expect("computeIfAbsent call should lower");

        match ir {
            IrExpression::MethodCall {
                args, java_type, ..
            } => {
                let list_of_string = JavaType::Reference {
                    name: "java.util.List".to_string(),
                    generic_args: vec![JavaType::Reference {
                        name: "String".to_string(),
                        generic_args: vec![],
                    }],
                };
                assert_eq!(java_type, list_of_string);

                assert_eq!(args.len(), 2);

                match &args[0] {
                    IrExpression::Identifier { java_type, .. } => {
                        let string_type = JavaType::Reference {
                            name: "String".to_string(),
                            generic_args: vec![],
                        };
                        assert_eq!(*java_type, string_type);
                    }
                    other => panic!("expected identifier for key argument, got {:?}", other),
                }

                match &args[1] {
                    IrExpression::Lambda {
                        functional_interface,
                        param_types,
                        java_type,
                        ..
                    } => {
                        assert_eq!(functional_interface, "java.util.function.Function");
                        let string_type = JavaType::Reference {
                            name: "String".to_string(),
                            generic_args: vec![],
                        };
                        assert_eq!(param_types, &vec![string_type.clone()]);

                        match java_type {
                            JavaType::Functional {
                                interface_name,
                                param_types: params,
                                return_type,
                            } => {
                                assert_eq!(interface_name, "java.util.function.Function");
                                assert_eq!(params, &vec![string_type.clone()]);
                                assert_eq!(**return_type, list_of_string);
                            }
                            other => panic!("expected functional type for lambda, got {:?}", other),
                        }
                    }
                    other => panic!(
                        "expected lambda argument for computeIfAbsent, got {:?}",
                        other
                    ),
                }
            }
            other => panic!("expected method call IR, got {:?}", other),
        }

        assert_eq!(context.method_calls.len(), 1);
        let record = &context.method_calls[0];
        assert_eq!(record.argument_types.len(), 2);

        let string_type = JavaType::Reference {
            name: "String".to_string(),
            generic_args: vec![],
        };
        assert_eq!(record.argument_types[0], string_type);

        match &record.argument_types[1] {
            JavaType::Functional {
                interface_name,
                param_types,
                return_type,
            } => {
                assert_eq!(interface_name, "java.util.function.Function");
                assert_eq!(param_types, &vec![string_type.clone()]);
                assert_eq!(
                    **return_type,
                    JavaType::Reference {
                        name: "java.util.List".to_string(),
                        generic_args: vec![string_type],
                    }
                );
            }
            other => panic!(
                "expected functional type recorded for lambda, got {:?}",
                other
            ),
        }
    }

    #[test]
    fn transform_literal_regex_variant_also_creates_regex_pattern() {
        let span = dummy_span();
        let literal = RegexLiteral {
            pattern: "^[a-z]+$".to_string(),
            raw: "/^[a-z]+$/".to_string(),
            span: span.clone(),
        };
        let mut context = TransformContext::new();
        let ir_expr = transform_expression(
            Expression::Literal(Literal::Regex(literal.clone()), span.clone()),
            &mut context,
        )
        .expect("regex literal lowering");

        match ir_expr {
            IrExpression::RegexPattern {
                pattern,
                java_type,
                span: ir_span,
            } => {
                assert_eq!(pattern, "^[a-z]+$");
                assert_eq!(ir_span, literal.span);
                match java_type {
                    JavaType::Reference { name, generic_args } => {
                        assert_eq!(name, "java.util.regex.Pattern");
                        assert!(
                            generic_args.is_empty(),
                            "pattern type should not include generic arguments"
                        );
                    }
                    other => panic!("expected pattern reference type, got {:?}", other),
                }
            }
            other => panic!("expected regex pattern ir expression, got {:?}", other),
        }
    }

    fn simple_program() -> Program {
        Program {
            package: Some("demo".to_string()),
            imports: Vec::new(),
            statements: vec![Statement::Expression {
                expr: Expression::Literal(Literal::Number("1".to_string()), dummy_span()),
                span: dummy_span(),
            }],
            span: dummy_span(),
        }
    }

    fn sha256_hex(bytes: &[u8]) -> String {
        let mut hasher = Sha256::new();
        hasher.update(bytes);
        format!("{:x}", hasher.finalize())
    }

    #[test]
    fn sample_annotation_desugars_to_ir_declaration() {
        let temp_dir = tempdir().expect("create temp dir");
        let sample_path = temp_dir.path().join("users.json");
        fs::write(&sample_path, r#"[{"id":1,"name":"Alice"}]"#).expect("write sample data");

        let mut context = TransformContext::new();
        context.sample_options_mut().base_dir = Some(temp_dir.path().to_path_buf());

        let annotation = Annotation {
            name: AnnotationName::new(vec!["Sample".to_string()], dummy_span()),
            arguments: vec![AnnotationArgument::Positional {
                value: AnnotationValue::Literal(Literal::String("users.json".to_string())),
                span: dummy_span(),
            }],
            span: dummy_span(),
        };

        let mut modifiers = Modifiers::default();
        modifiers.annotations.push(annotation);

        let initializer = Expression::Literal(Literal::Null, dummy_span());

        let result = desugar_val_declaration(
            "users".to_string(),
            None,
            initializer,
            modifiers,
            ValBindingOrigin::ExplicitKeyword,
            dummy_span(),
            &mut context,
        )
        .expect("desugar sample annotation");

        match result {
            IrStatement::SampleDeclaration(decl) => {
                assert_eq!(decl.variable_name, "users");
                assert_eq!(decl.mode, SampleMode::Embed);
                assert_eq!(decl.format, DataFormat::Json);
                assert!(
                    decl.embedded_data.as_ref().is_some(),
                    "embed mode stores data"
                );
                let list_type = decl.java_type.clone();
                match list_type {
                    JavaType::Reference { name, generic_args } => {
                        assert_eq!(name, "java.util.List");
                        assert_eq!(generic_args.len(), 1);
                        match &generic_args[0] {
                            JavaType::Reference { name, .. } => {
                                assert_eq!(name, "UserSample");
                            }
                            other => panic!("unexpected element type: {:?}", other),
                        }
                    }
                    other => panic!("expected list type, got {:?}", other),
                }

                assert!(
                    decl.records
                        .iter()
                        .any(|record| record.name == "UserSample"),
                    "expected inferred record descriptor"
                );
            }
            other => panic!("expected sample declaration, got {:?}", other),
        }

        let registered = context
            .lookup_variable("users")
            .expect("variable registered in context");
        match registered {
            JavaType::Reference { name, .. } => assert_eq!(name, "java.util.List"),
            other => panic!("unexpected registered type: {:?}", other),
        }
    }

    #[test]
    fn sample_annotation_embed_mode_embeds_data_and_generates_records() {
        let temp_dir = tempdir().expect("temp dir");
        let file_name = "users.json";
        let file_path = temp_dir.path().join(file_name);
        let payload = r#"[{"id":1,"name":"Alice"},{"id":2,"name":"Bob"}]"#;
        fs::write(&file_path, payload).expect("write payload");
        let file_bytes = fs::read(&file_path).expect("read payload");
        let expected_sha = sha256_hex(&file_bytes);

        let mut context = TransformContext::new();
        {
            let options = context.sample_options_mut();
            options.base_dir = Some(temp_dir.path().to_path_buf());
            options.cache_dir = Some(temp_dir.path().join("cache"));
        }

        let annotation = Annotation {
            name: AnnotationName::new(vec!["Sample".to_string()], dummy_span()),
            arguments: vec![
                AnnotationArgument::Positional {
                    value: AnnotationValue::Literal(Literal::String(file_name.to_string())),
                    span: dummy_span(),
                },
                AnnotationArgument::Named {
                    name: "sha256".to_string(),
                    value: AnnotationValue::Literal(Literal::String(expected_sha.clone())),
                    span: dummy_span(),
                },
            ],
            span: dummy_span(),
        };

        let mut modifiers = Modifiers::default();
        modifiers.annotations.push(annotation);

        let ir = desugar_val_declaration(
            "users".to_string(),
            None,
            Expression::Literal(Literal::Null, dummy_span()),
            modifiers,
            ValBindingOrigin::ExplicitKeyword,
            dummy_span(),
            &mut context,
        )
        .expect("desugar embed sample");

        let IrStatement::SampleDeclaration(decl) = ir else {
            panic!("expected sample declaration");
        };

        assert_eq!(decl.variable_name, "users");
        assert_eq!(decl.mode, SampleMode::Embed);
        assert_eq!(decl.format, DataFormat::Json);
        assert_eq!(decl.source, file_name);
        assert_eq!(decl.source_kind, SampleSourceKind::LocalFile);
        assert_eq!(decl.sha256, expected_sha);
        assert_eq!(decl.embedded_data.as_deref(), Some(file_bytes.as_slice()));

        let cache_path = decl.cache_path.as_ref().expect("cache path");
        assert!(cache_path.as_path().exists());
        assert_eq!(
            cache_path.file_name().and_then(|name| name.to_str()),
            Some(decl.sha256.as_str())
        );

        assert_eq!(context.lookup_variable("users"), Some(&decl.java_type));

        match &decl.java_type {
            JavaType::Reference { name, generic_args } => {
                assert_eq!(name, "java.util.List");
                assert_eq!(generic_args.len(), 1);
                match &generic_args[0] {
                    JavaType::Reference { name, .. } => {
                        assert!(name.ends_with("Sample"));
                    }
                    other => panic!("unexpected element type: {:?}", other),
                }
            }
            other => panic!("expected list type, got {:?}", other),
        }

        let root_name = decl.root_record_name.as_ref().expect("root record name");
        let descriptor = decl
            .records
            .iter()
            .find(|record| &record.name == root_name)
            .expect("root record descriptor");

        let id_field = descriptor
            .fields
            .iter()
            .find(|field| field.name == "id")
            .expect("id field");
        assert!(!id_field.is_optional);

        let name_field = descriptor
            .fields
            .iter()
            .find(|field| field.name == "name")
            .expect("name field");
        assert!(!name_field.is_optional);

        match &decl.schema {
            Schema::Array { element_type } => match element_type.as_ref() {
                Schema::Object { fields, required } => {
                    assert!(required.contains("id"));
                    assert!(required.contains("name"));
                    assert!(fields.contains_key("id"));
                    assert!(fields.contains_key("name"));
                }
                other => panic!("unexpected element schema: {:?}", other),
            },
            other => panic!("unexpected schema: {:?}", other),
        }
    }

    #[test]
    fn inline_json_literal_desugars_to_sample_declaration() {
        let source = r#"
            val payload = {
              "user": { "name": "Alice" },
              "count": 2
            }
        "#;

        let program = parse_program(source);
        let mut context = TransformContext::new();
        let ir_program =
            transform_program_with_context(program, &mut context).expect("lower inline json");

        assert_eq!(ir_program.type_declarations.len(), 1);
        match &ir_program.type_declarations[0] {
            IrStatement::SampleDeclaration(declaration) => {
                assert_eq!(declaration.variable_name, "payload");
                assert_eq!(declaration.format, DataFormat::Json);
                assert_eq!(declaration.mode, SampleMode::Embed);
                assert_eq!(declaration.source_kind, SampleSourceKind::Inline);
                assert!(declaration
                    .embedded_data
                    .as_ref()
                    .is_some_and(|data| !data.is_empty()));
                assert!(!declaration.records.is_empty());

                let registered = context
                    .lookup_variable("payload")
                    .expect("payload type registered");
                match registered {
                    JavaType::Reference { name, .. } => {
                        assert!(name.contains("Payload"));
                    }
                    other => panic!("expected reference type for payload, got {:?}", other),
                }
            }
            other => panic!("expected sample declaration, got {:?}", other),
        }
    }

    #[test]
    fn transform_pools_capture_reuse_between_runs() {
        let pools = TransformPools::with_chunk_capacity(8 * 1024);
        let mut context = TransformContext::with_pools(pools);
        let program = simple_program();

        transform_program_with_context(program.clone(), &mut context)
            .expect("first lowering succeeds");

        let _first_session = context
            .last_pool_session()
            .expect("first session metrics recorded");
        // Metrics snapshot existence verifies pools were engaged.
        assert_eq!(context.last_pool_warm_start(), Some(false));

        transform_program_with_context(program, &mut context).expect("second lowering succeeds");

        let metrics = context.pool_metrics().expect("pooled metrics available");
        assert_eq!(metrics.sessions, 2);
        assert!(metrics.warm_sessions >= 1);
        assert!(context.last_pool_warm_start().unwrap_or(false));
        let reuse_ratio = context.pool_reuse_ratio().unwrap_or(0.0);
        assert!(reuse_ratio >= 0.5);
    }

    #[test]
    fn sample_annotation_load_mode_skips_embedding_and_respects_limit_bytes() {
        let temp_dir = tempdir().expect("temp dir");
        let file_name = "metrics.csv";
        let file_path = temp_dir.path().join(file_name);
        let payload = "name,requests\napi,42\napi,43\n";
        fs::write(&file_path, payload).expect("write payload");
        let file_bytes = fs::read(&file_path).expect("read payload");
        let expected_sha = sha256_hex(&file_bytes);

        let mut context = TransformContext::new();
        context.sample_options_mut().base_dir = Some(temp_dir.path().to_path_buf());

        let annotation = Annotation {
            name: AnnotationName::new(vec!["Sample".to_string()], dummy_span()),
            arguments: vec![
                AnnotationArgument::Positional {
                    value: AnnotationValue::Literal(Literal::String(file_name.to_string())),
                    span: dummy_span(),
                },
                AnnotationArgument::Named {
                    name: "mode".to_string(),
                    value: AnnotationValue::EnumConstant {
                        type_path: vec![],
                        constant: "Load".to_string(),
                    },
                    span: dummy_span(),
                },
                AnnotationArgument::Named {
                    name: "limitBytes".to_string(),
                    value: AnnotationValue::Literal(Literal::Number("128".to_string())),
                    span: dummy_span(),
                },
                AnnotationArgument::Named {
                    name: "format".to_string(),
                    value: AnnotationValue::Literal(Literal::String("csv".to_string())),
                    span: dummy_span(),
                },
            ],
            span: dummy_span(),
        };

        let mut modifiers = Modifiers::default();
        modifiers.annotations.push(annotation);

        let ir = desugar_val_declaration(
            "metrics".to_string(),
            None,
            Expression::Literal(Literal::Null, dummy_span()),
            modifiers,
            ValBindingOrigin::ExplicitKeyword,
            dummy_span(),
            &mut context,
        )
        .expect("desugar load sample");

        let IrStatement::SampleDeclaration(decl) = ir else {
            panic!("expected sample declaration");
        };

        assert_eq!(decl.variable_name, "metrics");
        assert_eq!(decl.mode, SampleMode::Load);
        assert_eq!(decl.format, DataFormat::Csv);
        assert_eq!(decl.limit_bytes, Some(128));
        assert!(decl.embedded_data.is_none());
        assert!(decl.cache_path.is_none());
        assert_eq!(decl.source, file_name);
        assert_eq!(decl.source_kind, SampleSourceKind::LocalFile);
        assert_eq!(decl.sha256, expected_sha);
        assert_eq!(context.lookup_variable("metrics"), Some(&decl.java_type));

        match &decl.java_type {
            JavaType::Reference { name, generic_args } => {
                assert_eq!(name, "java.util.List");
                assert_eq!(generic_args.len(), 1);
            }
            other => panic!("expected list type, got {:?}", other),
        }

        match &decl.schema {
            Schema::Array { element_type } => match element_type.as_ref() {
                Schema::Object { fields, required } => {
                    assert!(required.contains("name"));
                    assert!(required.contains("requests"));
                    assert!(fields.contains_key("name"));
                    assert!(fields.contains_key("requests"));
                }
                other => panic!("unexpected element schema: {:?}", other),
            },
            other => panic!("unexpected schema: {:?}", other),
        }
    }

    #[test]
    fn sample_annotation_rejects_explicit_type_annotation() {
        let mut context = TransformContext::new();

        let mut modifiers = Modifiers::default();
        modifiers.annotations.push(Annotation {
            name: AnnotationName::new(vec!["Sample".to_string()], dummy_span()),
            arguments: Vec::new(),
            span: dummy_span(),
        });

        let error = desugar_val_declaration(
            "data".to_string(),
            Some(TypeAnnotation::Simple("String".to_string())),
            Expression::Literal(Literal::Null, dummy_span()),
            modifiers,
            ValBindingOrigin::ExplicitKeyword,
            dummy_span(),
            &mut context,
        )
        .expect_err("type annotation should be rejected");

        match error {
            TransformError::SampleAnnotationError { message, .. } => {
                assert!(message.contains("型注釈"));
            }
            other => panic!("unexpected error: {:?}", other),
        }
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
            ValBindingOrigin::ExplicitKeyword,
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

    #[test]
    fn test_desugar_implicit_val_is_final() {
        let mut context = test_context();
        let initializer = Expression::Literal(Literal::Number("7".to_string()), dummy_span());

        let result = desugar_val_declaration(
            "implicit_total".to_string(),
            None,
            initializer,
            Modifiers::default(),
            ValBindingOrigin::Implicit,
            dummy_span(),
            &mut context,
        )
        .expect("implicit val should desugar successfully");

        let IrStatement::VariableDeclaration {
            is_final,
            modifiers,
            ..
        } = result
        else {
            panic!("expected variable declaration");
        };
        assert!(is_final, "implicit val should produce final variables");
        assert!(modifiers.is_final);
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
            guard: None,
            body: Expression::Literal(Literal::String("one".to_string()), dummy_span()),
            span: dummy_span(),
        };
        let arm2 = WhenArm {
            pattern: Pattern::Literal(Literal::Number("2".to_string()), dummy_span()),
            guard: None,
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
            None,
            dummy_span(),
            &mut context,
        )
        .expect("when expression should desugar successfully");

        match result {
            IrExpression::Switch {
                discriminant,
                cases,
                java_type,
                strategy_description,
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
                let description = strategy_description
                    .expect("strategy description should be present for switch lowering");
                assert!(description.contains("strategy=Switch"));
                assert!(description.contains("guards=0"));
            }
            other => panic!("Expected switch expression, got {:?}", other),
        }

        let strategies = context.when_strategies();
        assert_eq!(strategies.len(), 1);
        assert!(strategies[0].description.contains("strategy=Switch"));
    }

    #[test]
    fn test_desugar_when_expression_with_guard_emits_guard_ir() {
        let mut context = test_context();
        context.add_variable("x".to_string(), JavaType::int());

        let subject = Some(Box::new(Expression::Identifier(
            "x".to_string(),
            dummy_span(),
        )));

        let guard_expr = Expression::Literal(Literal::Boolean(true), dummy_span());
        let arm = WhenArm {
            pattern: Pattern::Literal(Literal::Number("1".to_string()), dummy_span()),
            guard: Some(guard_expr),
            body: Expression::Literal(Literal::String("one".to_string()), dummy_span()),
            span: dummy_span(),
        };

        let else_arm = Some(Box::new(Expression::Literal(
            Literal::String("other".to_string()),
            dummy_span(),
        )));

        let result = desugar_when_expression(
            subject,
            vec![arm],
            else_arm,
            None,
            dummy_span(),
            &mut context,
        )
        .expect("when expression with guard should desugar successfully");

        match result {
            IrExpression::Switch {
                cases,
                strategy_description,
                ..
            } => {
                let guard_ir = cases[0]
                    .guard
                    .as_ref()
                    .expect("guard expression should be preserved");
                match guard_ir {
                    IrExpression::Literal(Literal::Boolean(true), _) => {}
                    other => panic!("Expected boolean literal guard, got {:?}", other),
                }

                let description = strategy_description
                    .expect("strategy description should be recorded for guard lowering");
                assert!(description.contains("strategy=Hybrid"));
                assert!(description.contains("guards=1"));
            }
            other => panic!("Expected switch expression, got {:?}", other),
        }

        let strategies = context.when_strategies();
        assert_eq!(strategies.len(), 1);
        assert!(strategies[0].description.contains("strategy=Hybrid"));
    }

    #[test]
    fn test_desugar_when_expression_with_range_pattern_uses_hybrid_strategy() {
        let mut context = test_context();
        context.add_variable("x".to_string(), JavaType::int());

        let subject = Some(Box::new(Expression::Identifier(
            "x".to_string(),
            dummy_span(),
        )));

        let range_arm = WhenArm {
            pattern: Pattern::Range {
                start: Box::new(Expression::Literal(
                    Literal::Number("0".to_string()),
                    dummy_span(),
                )),
                end: Box::new(Expression::Literal(
                    Literal::Number("10".to_string()),
                    dummy_span(),
                )),
                inclusive_end: false,
                span: dummy_span(),
            },
            guard: None,
            body: Expression::Literal(Literal::String("small".to_string()), dummy_span()),
            span: dummy_span(),
        };

        let else_arm = Some(Box::new(Expression::Literal(
            Literal::String("other".to_string()),
            dummy_span(),
        )));

        let result = desugar_when_expression(
            subject,
            vec![range_arm],
            else_arm,
            None,
            dummy_span(),
            &mut context,
        )
        .expect("range pattern should lower successfully");

        match result {
            IrExpression::Switch {
                cases,
                strategy_description,
                ..
            } => {
                assert_eq!(cases.len(), 2);
                match &cases[0].labels[0] {
                    IrCaseLabel::Range {
                        type_name,
                        variable,
                        inclusive_end,
                        ..
                    } => {
                        assert_eq!(type_name, "int");
                        assert!(variable.starts_with("it"));
                        assert!(!*inclusive_end);
                    }
                    other => panic!("Expected range label, got {:?}", other),
                }
                let guard = cases[0]
                    .guard
                    .as_ref()
                    .expect("range guard should be generated");
                match guard {
                    IrExpression::Binary {
                        op: BinaryOp::And, ..
                    } => {}
                    other => panic!("Expected guard to be boolean AND, got {:?}", other),
                }
                let description = strategy_description
                    .expect("strategy description should exist for hybrid lowering");
                assert!(description.contains("strategy=Hybrid"));
                assert!(description.contains("guards=1"));
            }
            other => panic!("Expected switch expression, got {:?}", other),
        }

        let strategies = context.when_strategies();
        assert_eq!(strategies.len(), 1);
        assert!(strategies[0].description.contains("strategy=Hybrid"));
    }

    #[test]
    fn hybrid_strategy_records_full_metadata() {
        let expr = parse_when_expression(
            "val value = 4\n             val label = when (value) {\n             in 0..10 -> \"small\"\n             else -> \"other\"\n        }\n",
        );

        let Expression::When {
            expr: subject,
            arms,
            else_arm,
            implicit_end,
            span,
        } = expr
        else {
            panic!("expected when expression");
        };

        let recorded_span = span.clone();
        let mut context = TransformContext::new();
        context.add_variable("value".to_string(), JavaType::int());

        let ir = desugar_when_expression(
            subject,
            arms,
            else_arm,
            implicit_end,
            span.clone(),
            &mut context,
        )
        .expect("hybrid lowering should succeed");

        let IrExpression::Switch {
            cases,
            strategy_description,
            ..
        } = ir
        else {
            panic!("expected switch expression for hybrid lowering");
        };

        let metadata = strategy_description.expect("strategy description should be present");
        let guard_count = cases.iter().filter(|case| case.guard.is_some()).count();
        let has_default = cases.iter().any(|case| {
            case.labels
                .iter()
                .any(|label| matches!(label, IrCaseLabel::Default))
        });
        let expected = format!(
            "strategy=Hybrid arms={} guards={} default={} exhaustive=unknown",
            cases.len(),
            guard_count,
            has_default
        );
        assert_eq!(metadata, expected);

        let strategies = context.take_when_strategies();
        assert_eq!(strategies.len(), 1);
        assert_eq!(strategies[0].description, expected);
        assert_eq!(strategies[0].span, recorded_span);
    }

    #[test]
    fn test_desugar_when_expression_with_constructor_pattern_uses_switch_strategy() {
        let mut context = test_context();
        context.add_variable("value".to_string(), JavaType::string());

        let subject = Some(Box::new(Expression::Identifier(
            "value".to_string(),
            dummy_span(),
        )));

        let arm = WhenArm {
            pattern: Pattern::Constructor {
                name: "String".to_string(),
                patterns: vec![],
                span: dummy_span(),
            },
            guard: None,
            body: Expression::Literal(Literal::String("text".to_string()), dummy_span()),
            span: dummy_span(),
        };

        let else_arm = Some(Box::new(Expression::Literal(
            Literal::String("other".to_string()),
            dummy_span(),
        )));

        let result = desugar_when_expression(
            subject,
            vec![arm],
            else_arm,
            None,
            dummy_span(),
            &mut context,
        )
        .expect("constructor pattern should lower successfully");

        match result {
            IrExpression::Switch {
                cases,
                strategy_description,
                ..
            } => {
                match &cases[0].labels[0] {
                    IrCaseLabel::TypePattern { type_name, .. } => assert_eq!(type_name, "String"),
                    other => panic!("Expected type pattern label, got {:?}", other),
                }
                let description =
                    strategy_description.expect("strategy description should be present");
                assert!(description.contains("strategy=Switch"));
            }
            other => panic!("Expected switch expression, got {:?}", other),
        }

        let strategies = context.when_strategies();
        assert_eq!(strategies.len(), 1);
        assert!(strategies[0].description.contains("strategy=Switch"));
    }

    #[test]
    fn test_desugar_when_expression_records_implicit_end() {
        let mut context = test_context();
        context.add_variable("x".to_string(), JavaType::int());

        let subject = Some(Box::new(Expression::Identifier(
            "x".to_string(),
            dummy_span(),
        )));

        let arm = WhenArm {
            pattern: Pattern::Literal(Literal::Number("1".to_string()), dummy_span()),
            guard: None,
            body: Expression::Literal(Literal::String("one".to_string()), dummy_span()),
            span: dummy_span(),
        };

        let implicit_end = Some(ImplicitWhenEnd::Unit { span: dummy_span() });

        let result = desugar_when_expression(
            subject,
            vec![arm],
            None,
            implicit_end,
            dummy_span(),
            &mut context,
        )
        .expect("when expression should desugar successfully");

        match result {
            IrExpression::Switch { implicit_end, .. } => match implicit_end {
                Some(IrImplicitWhenEnd::Unit { .. }) => {}
                other => panic!("Expected implicit unit end, got {:?}", other),
            },
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
            guard: None,
            body: Expression::Literal(Literal::String("one".to_string()), dummy_span()),
            span: dummy_span(),
        };
        let default_arm = WhenArm {
            pattern: Pattern::Wildcard(dummy_span()),
            guard: None,
            body: Expression::Literal(Literal::String("other".to_string()), dummy_span()),
            span: dummy_span(),
        };

        let result = desugar_when_expression(
            subject,
            vec![arm1, default_arm],
            None,
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
    fn test_subjectless_when_expression_lowers_to_if_chain() {
        let mut context = test_context();

        let arm = WhenArm {
            pattern: Pattern::Guard {
                pattern: Box::new(Pattern::Wildcard(dummy_span())),
                condition: Expression::Literal(Literal::Boolean(true), dummy_span()),
                span: dummy_span(),
            },
            guard: None,
            body: Expression::Literal(Literal::String("matched".to_string()), dummy_span()),
            span: dummy_span(),
        };

        let else_arm = Some(Box::new(Expression::Literal(
            Literal::String("fallback".to_string()),
            dummy_span(),
        )));

        let result =
            desugar_when_expression(None, vec![arm], else_arm, None, dummy_span(), &mut context)
                .expect("subjectless when should lower to conditional chain");

        match result {
            IrExpression::Conditional { java_type, .. } => {
                assert_eq!(java_type, JavaType::string());
            }
            other => panic!("Expected conditional expression, got {:?}", other),
        }

        let strategies = context.when_strategies();
        assert_eq!(strategies.len(), 1);
        assert!(strategies[0].description.contains("strategy=IfChain"));
    }

    #[test]
    fn test_desugar_when_expression_preserves_case_spans() {
        let mut context = test_context();
        context.add_variable("subject".to_string(), JavaType::int());

        let subject = Some(Box::new(Expression::Identifier(
            "subject".to_string(),
            Span::new(2, 1, 2, 8),
        )));

        let pattern_span = Span::new(2, 5, 2, 6);
        let body_span = Span::new(2, 10, 2, 13);
        let arm_span = Span::new(2, 5, 2, 13);
        let when_span = Span::new(2, 1, 2, 20);

        let arm = WhenArm {
            pattern: Pattern::Literal(Literal::Number("1".to_string()), pattern_span.clone()),
            guard: None,
            body: Expression::Literal(Literal::String("one".to_string()), body_span),
            span: arm_span.clone(),
        };

        let else_arm = Some(Box::new(Expression::Literal(
            Literal::String("other".to_string()),
            Span::new(2, 16, 2, 21),
        )));

        let result = desugar_when_expression(
            subject,
            vec![arm],
            else_arm,
            None,
            when_span.clone(),
            &mut context,
        )
        .expect("when expression should desugar successfully");

        match result {
            IrExpression::Switch { span, cases, .. } => {
                assert_eq!(span, when_span);
                assert_eq!(cases.len(), 2);
                assert_eq!(cases[0].span, arm_span);
                assert_eq!(cases[1].span, when_span);
            }
            other => panic!("Expected switch expression with spans, got {:?}", other),
        }
    }

    #[test]
    fn test_desugar_when_expression_error_uses_arm_span() {
        let mut context = test_context();
        context.add_variable("subject".to_string(), JavaType::int());

        let subject = Some(Box::new(Expression::Identifier(
            "subject".to_string(),
            Span::new(3, 1, 3, 8),
        )));

        let arm_span = Span::new(3, 5, 3, 12);
        let arm = WhenArm {
            pattern: Pattern::Identifier("it".to_string(), arm_span.clone()),
            guard: None,
            body: Expression::Literal(Literal::String("body".to_string()), Span::new(3, 10, 3, 14)),
            span: arm_span.clone(),
        };

        let error = desugar_when_expression(
            subject,
            vec![arm],
            None,
            None,
            Span::new(3, 1, 3, 18),
            &mut context,
        )
        .expect_err("unsupported when pattern should produce an error");

        match error {
            TransformError::UnsupportedConstruct { span, .. } => assert_eq!(span, arm_span),
            other => panic!(
                "Expected unsupported construct error with span, got {:?}",
                other
            ),
        }
    }

    #[test]
    fn test_desugar_null_safe_member_access_span_propagation() {
        let mut context = test_context();
        context.add_variable("user".to_string(), JavaType::object());

        let receiver_span = Span::new(5, 3, 5, 7);
        let expression_span = Span::new(5, 3, 5, 15);

        let result = desugar_null_safe_member_access(
            Box::new(Expression::Identifier("user".to_string(), receiver_span)),
            "name".to_string(),
            expression_span.clone(),
            &mut context,
        )
        .expect("null-safe member access should succeed");

        match result {
            IrExpression::NullSafeOperation {
                span,
                operation,
                default_value,
                ..
            } => {
                assert_eq!(span, expression_span);
                match *operation {
                    IrExpression::FieldAccess {
                        span: field_span, ..
                    } => {
                        assert_eq!(field_span, expression_span);
                    }
                    other => panic!(
                        "Expected field access as operation span carrier, got {:?}",
                        other
                    ),
                }

                let default = default_value.expect("expected default value for nullable type");
                match *default {
                    IrExpression::Literal(_, literal_span) => {
                        assert_eq!(literal_span, expression_span);
                    }
                    other => panic!("Expected literal default preserving span, got {:?}", other),
                }
            }
            other => panic!("Expected null-safe operation with spans, got {:?}", other),
        }
    }

    #[test]
    fn test_desugar_null_safe_member_access_error_span() {
        let mut context = test_context();
        context.add_variable("number".to_string(), JavaType::int());

        let span = Span::new(6, 1, 6, 12);

        let error = desugar_null_safe_member_access(
            Box::new(Expression::Identifier("number".to_string(), span.clone())),
            "length".to_string(),
            span.clone(),
            &mut context,
        )
        .expect_err("null-safe member access on primitive should fail");

        match error {
            TransformError::NullSafetyError { span: err_span, .. } => assert_eq!(err_span, span),
            other => panic!("Expected null safety error carrying span, got {:?}", other),
        }
    }

    // Test for extension function desugaring
    #[test]
    fn test_desugar_extension_function_produces_static_method() {
        let mut context = test_context();

        let function_decl = Statement::FunctionDeclaration {
            name: "trimmed".to_string(),
            type_parameters: Vec::new(),
            generic_signature: None,
            where_clause: None,
            parameters: vec![],
            return_type: Some(TypeAnnotation::Simple("String".to_string())),
            primitive_return: None,
            body: Box::new(Expression::Call {
                function: Box::new(Expression::MemberAccess {
                    object: Box::new(Expression::This(dummy_span())),
                    property: "trim".to_string(),
                    span: dummy_span(),
                }),
                args: vec![],
                type_arguments: Vec::new(),
                argument_metadata: CallArgumentMetadata::with_style(CallArgumentStyle::Comma),
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

    #[test]
    fn test_extension_call_on_receiver_normalized_to_static_sequence_call() {
        let mut context = test_context();

        // Register a simple Iterable.toStream extension.
        let to_stream_decl = Statement::FunctionDeclaration {
            name: "toStream".to_string(),
            type_parameters: Vec::new(),
            generic_signature: None,
            where_clause: None,
            parameters: vec![],
            return_type: Some(TypeAnnotation::Simple("java.lang.Iterable".to_string())),
            primitive_return: None,
            body: Box::new(Expression::This(dummy_span())),
            modifiers: Modifiers::default(),
            span: dummy_span(),
        };

        let iterable_type = TypeAnnotation::Simple("java.lang.Iterable".to_string());
        desugar_extension_function(
            iterable_type.clone(),
            Box::new(to_stream_decl),
            dummy_span(),
            &mut context,
        )
        .expect("toStream extension should desugar");

        // Extension that invokes receiver.toStream() should be rewritten to a static call.
        let call_to_stream = Expression::Call {
            function: Box::new(Expression::MemberAccess {
                object: Box::new(Expression::This(dummy_span())),
                property: "toStream".to_string(),
                span: dummy_span(),
            }),
            args: Vec::new(),
            type_arguments: Vec::new(),
            argument_metadata: CallArgumentMetadata::with_style(CallArgumentStyle::Whitespace),
            span: dummy_span(),
        };

        let wrapper_decl = Statement::FunctionDeclaration {
            name: "wrap".to_string(),
            type_parameters: Vec::new(),
            generic_signature: None,
            where_clause: None,
            parameters: vec![],
            return_type: Some(TypeAnnotation::Simple(
                "java.util.stream.Stream".to_string(),
            )),
            primitive_return: None,
            body: Box::new(call_to_stream),
            modifiers: Modifiers::default(),
            span: dummy_span(),
        };

        let result = desugar_extension_function(
            iterable_type,
            Box::new(wrapper_decl),
            dummy_span(),
            &mut context,
        )
        .expect("extension wrapper should desugar");

        match result {
            IrStatement::MethodDeclaration { body, .. } => {
                let body = body.expect("extension method should have a body");
                match body {
                    IrExpression::MethodCall {
                        receiver,
                        method_name,
                        args,
                        ..
                    } => {
                        assert!(
                            receiver.is_none(),
                            "extension invocation should be converted to static call"
                        );
                        assert_eq!(method_name, "toStream");
                        assert_eq!(args.len(), 1);
                        match &args[0] {
                            IrExpression::Identifier { name, .. } => {
                                assert_eq!(name, "receiver");
                            }
                            other => panic!(
                                "expected receiver identifier as first argument, got {:?}",
                                other
                            ),
                        }
                    }
                    other => panic!("expected method call body, got {:?}", other),
                }
            }
            other => panic!("expected method declaration, got {:?}", other),
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
    fn transform_multiline_string_with_interpolation_produces_string_format() {
        let mut context = test_context();
        context.add_variable("user".to_string(), JavaType::string());
        context.add_variable("status".to_string(), JavaType::string());

        let literal = MultilineStringLiteral {
            kind: MultilineKind::TripleQuote,
            normalized: "Hello,  says ".to_string(),
            raw: "Hello,  says ".to_string(),
            parts: vec![
                StringPart::Text("Hello, ".to_string()),
                StringPart::Expression(Expression::Identifier("user".to_string(), dummy_span())),
                StringPart::Text(" says ".to_string()),
                StringPart::Expression(Expression::Identifier("status".to_string(), dummy_span())),
            ],
            indent: None,
            span: dummy_span(),
        };

        let result = transform_expression(Expression::MultilineString(literal), &mut context)
            .expect("multiline interpolation should lower successfully");

        match result {
            IrExpression::StringFormat {
                format_string,
                args,
                ..
            } => {
                assert_eq!(format_string, "Hello, %s says %s");
                assert_eq!(args.len(), 2);
                match &args[0] {
                    IrExpression::Identifier { name, .. } => assert_eq!(name, "user"),
                    other => panic!("expected first argument identifier, got {:?}", other),
                }
                match &args[1] {
                    IrExpression::Identifier { name, .. } => assert_eq!(name, "status"),
                    other => panic!("expected second argument identifier, got {:?}", other),
                }
            }
            other => panic!(
                "multiline interpolation should desugar to string format, got {:?}",
                other
            ),
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
                modifiers: ParameterModifiers::default(),
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
    fn test_desugar_defer_expression_propagates_body_errors() {
        let mut context = test_context();

        let body = Box::new(Expression::Identifier("missing".to_string(), dummy_span()));

        let error = desugar_defer_expression(body, dummy_span(), &mut context)
            .expect_err("defer expression should propagate inner transform errors");

        match error {
            TransformError::ScopeError { message, .. } => {
                assert!(message.contains("missing"));
            }
            other => panic!("Expected scope error, got {:?}", other),
        }
    }

    // Test for default parameters desugaring
    #[test]
    fn test_desugar_default_parameters_creates_overloads() {
        let mut context = test_context();

        // fun greet(name: String, greeting: String = "Hello") = "$greeting, $name"
        let param1 = Parameter {
            name: "name".to_string(),
            type_annotation: Some(TypeAnnotation::Simple("String".to_string())),
            default_value: None,
            modifiers: ParameterModifiers::default(),
            span: dummy_span(),
        };
        let param2 = Parameter {
            name: "greeting".to_string(),
            type_annotation: Some(TypeAnnotation::Simple("String".to_string())),
            default_value: Some(Expression::Literal(
                Literal::String("Hello".to_string()),
                dummy_span(),
            )),
            modifiers: ParameterModifiers::default(),
            span: dummy_span(),
        };

        let body = Box::new(Expression::Literal(
            Literal::String("greeting + name".to_string()),
            dummy_span(),
        ));

        let result = desugar_default_parameters(
            "greet".to_string(),
            vec![param1, param2],
            Some(TypeAnnotation::Simple("String".to_string())),
            body,
            Modifiers::default(),
            dummy_span(),
            &mut context,
        )
        .expect("default parameters should desugar successfully");

        // Should generate two overloads: greet(name) and greet(name, greeting)
        assert_eq!(result.len(), 2);

        // First overload: greet(name) - calls greet(name, "Hello")
        assert_eq!(result[0].parameters.len(), 1);
        assert_eq!(result[0].parameters[0].name, "name");

        // Second overload: greet(name, greeting) - original implementation
        assert_eq!(result[1].parameters.len(), 2);
        assert_eq!(result[1].parameters[0].name, "name");
        assert_eq!(result[1].parameters[1].name, "greeting");
    }

    // Test for named arguments desugaring
    #[test]
    fn test_desugar_named_arguments_creates_method_call() {
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

        let result = desugar_named_arguments(function, args, dummy_span(), &mut context)
            .expect("named arguments should desugar successfully");

        match result {
            IrExpression::MethodCall {
                receiver,
                method_name,
                args,
                ..
            } => {
                assert!(receiver.is_none()); // Static function call
                assert_eq!(method_name, "greet");
                assert_eq!(args.len(), 2); // Two named arguments converted to positional
            }
            other => panic!("Expected method call, got {:?}", other),
        }
    }

    // Test for top-level function desugaring
    #[test]
    fn test_desugar_top_level_function_creates_static_method() {
        let mut context = test_context();

        // fun topLevelFunction(x: Int): String = x.toString()
        let function = Statement::FunctionDeclaration {
            name: "topLevelFunction".to_string(),
            type_parameters: Vec::new(),
            generic_signature: None,
            where_clause: None,
            parameters: vec![Parameter {
                name: "x".to_string(),
                type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
                default_value: None,
                modifiers: ParameterModifiers::default(),
                span: dummy_span(),
            }],
            return_type: Some(TypeAnnotation::Simple("String".to_string())),
            primitive_return: None,
            body: Box::new(Expression::Literal(
                Literal::String("x.toString()".to_string()),
                dummy_span(),
            )),
            modifiers: Modifiers::default(),
            span: dummy_span(),
        };

        let result = desugar_top_level_function(function, &mut context)
            .expect("top-level function should desugar successfully");

        match result {
            IrStatement::MethodDeclaration {
                type_parameters,
                name,
                parameters,
                return_type,
                modifiers,
                body,
                ..
            } => {
                assert!(type_parameters.is_empty());
                assert_eq!(name, "topLevelFunction");
                assert_eq!(parameters.len(), 1);
                assert_eq!(parameters[0].name, "x");
                assert_eq!(return_type, JavaType::string());
                assert!(modifiers.is_static); // Top-level functions become static
                assert!(body.is_some()); // Should have a body
            }
            other => panic!("Expected method declaration, got {:?}", other),
        }
    }

    #[test]
    fn test_desugar_top_level_function_carries_type_parameters() {
        let mut context = test_context();

        let function = Statement::FunctionDeclaration {
            name: "identity".to_string(),
            type_parameters: vec!["T".to_string()],
            generic_signature: None,
            where_clause: None,
            parameters: vec![Parameter {
                name: "value".to_string(),
                type_annotation: Some(TypeAnnotation::Simple("T".to_string())),
                default_value: None,
                modifiers: ParameterModifiers::default(),
                span: dummy_span(),
            }],
            return_type: Some(TypeAnnotation::Simple("T".to_string())),
            primitive_return: None,
            body: Box::new(Expression::Identifier("value".to_string(), dummy_span())),
            modifiers: Modifiers::default(),
            span: dummy_span(),
        };

        let result = desugar_top_level_function(function, &mut context)
            .expect("generic top-level function should desugar successfully");

        match result {
            IrStatement::MethodDeclaration {
                type_parameters,
                parameters,
                return_type,
                ..
            } => {
                assert_eq!(type_parameters.len(), 1);
                assert_eq!(type_parameters[0].name, "T");
                assert_eq!(parameters.len(), 1);
                assert_eq!(
                    parameters[0].java_type,
                    JavaType::Reference {
                        name: "T".to_string(),
                        generic_args: vec![],
                    }
                );
                assert_eq!(
                    return_type,
                    JavaType::Reference {
                        name: "T".to_string(),
                        generic_args: vec![],
                    }
                );
            }
            other => panic!("Expected method declaration, got {:?}", other),
        }
    }

    #[test]
    fn desugar_top_level_function_preserves_primitive_return_metadata() {
        let mut context = test_context();

        let primitive_return = PrimitiveReturnMetadata {
            reference: PrimitiveTypeReference {
                primitive: PrimitiveTypeName::Int,
                source: PrimitiveTypeSource::PrimitiveKeyword,
                raw_path: Vec::new(),
                span: dummy_span(),
            },
        };

        let function = Statement::FunctionDeclaration {
            name: "sumValues".to_string(),
            type_parameters: Vec::new(),
            generic_signature: None,
            where_clause: None,
            parameters: Vec::new(),
            return_type: None,
            primitive_return: Some(primitive_return.clone()),
            body: Box::new(Expression::Literal(
                Literal::Number("0".to_string()),
                dummy_span(),
            )),
            modifiers: Modifiers::default(),
            span: dummy_span(),
        };

        let result = desugar_top_level_function(function, &mut context)
            .expect("primitive metadata should survive lowering");

        match result {
            IrStatement::MethodDeclaration {
                primitive_return: actual,
                ..
            } => {
                assert_eq!(actual, Some(primitive_return));
            }
            other => panic!("Expected method declaration, got {:?}", other),
        }
    }

    #[test]
    fn test_transform_statement_preserves_return() {
        let mut context = test_context();

        let stmt = Statement::Return {
            value: Some(Expression::Literal(
                Literal::String("result".to_string()),
                dummy_span(),
            )),
            span: dummy_span(),
        };

        let lowered = transform_statement(stmt, &mut context)
            .expect("return statement should lower successfully");

        assert_eq!(lowered.len(), 1);
        match &lowered[0] {
            IrStatement::Return {
                value: Some(expr), ..
            } => match expr {
                IrExpression::Literal(Literal::String(text), _) => {
                    assert_eq!(text, "result")
                }
                other => panic!("unexpected return expression: {:?}", other),
            },
            other => panic!("expected return statement, got {:?}", other),
        }
    }

    #[test]
    fn test_lower_call_expression_from_identifier() {
        let mut context = test_context();
        context
            .type_info
            .insert("greet".to_string(), JavaType::string());

        let call = Expression::Call {
            function: Box::new(Expression::Identifier("greet".to_string(), dummy_span())),
            args: vec![Argument::Positional(Expression::Literal(
                Literal::String("World".to_string()),
                dummy_span(),
            ))],
            type_arguments: Vec::new(),
            argument_metadata: CallArgumentMetadata::default(),
            span: dummy_span(),
        };

        let ir = transform_expression(call, &mut context)
            .expect("call expression should lower successfully");

        match ir {
            IrExpression::MethodCall {
                receiver,
                method_name,
                args,
                java_type,
                ..
            } => {
                assert!(receiver.is_none());
                assert_eq!(method_name, "greet");
                assert_eq!(java_type, JavaType::string());
                assert_eq!(args.len(), 1);
                match &args[0] {
                    IrExpression::Literal(Literal::String(value), _) => {
                        assert_eq!(value, "World")
                    }
                    other => panic!("unexpected argument expression: {:?}", other),
                }
            }
            other => panic!("expected method call, got {:?}", other),
        }
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
                modifiers: ParameterModifiers::default(),
                span: dummy_span(),
            },
            Parameter {
                name: "name".to_string(),
                type_annotation: Some(TypeAnnotation::Simple("String".to_string())),
                default_value: None,
                modifiers: ParameterModifiers::default(),
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
                modifiers: ParameterModifiers::default(),
                span: dummy_span(),
            },
            Parameter {
                name: "name".to_string(),
                type_annotation: None,
                default_value: Some(Expression::Literal(
                    Literal::String("unknown".to_string()),
                    dummy_span(),
                )),
                modifiers: ParameterModifiers::default(),
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
    fn test_infer_java_type_uses_initializer_type() {
        let mut context = test_context();

        let ir_initializer =
            IrExpression::Literal(Literal::String("hello".to_string()), dummy_span());

        let inferred = infer_java_type(None, Some(&ir_initializer), &mut context)
            .expect("type inference should succeed for string literal");

        assert_eq!(inferred, JavaType::string());
    }

    #[test]
    fn test_whitespace_array_homogeneous_allows_inference() {
        let mut context = test_context();

        let whitespace_array = IrExpression::ArrayCreation {
            element_type: JavaType::int(),
            dimensions: Vec::new(),
            initializer: Some(vec![
                IrExpression::Literal(Literal::Number("1".to_string()), dummy_span()),
                IrExpression::Literal(Literal::Number("2".to_string()), dummy_span()),
            ]),
            delimiter: SequenceDelimiter::Whitespace,
            span: dummy_span(),
        };

        let inferred = infer_java_type(None, Some(&whitespace_array), &mut context)
            .expect("homogeneous whitespace array should infer type");

        assert_eq!(
            inferred,
            JavaType::Reference {
                name: "java.util.List".to_string(),
                generic_args: vec![JavaType::Reference {
                    name: "Integer".to_string(),
                    generic_args: vec![],
                }],
            }
        );
    }

    #[test]
    fn test_whitespace_array_mixed_type_returns_error() {
        let mut context = test_context();

        let whitespace_array = IrExpression::ArrayCreation {
            element_type: JavaType::object(),
            dimensions: Vec::new(),
            initializer: Some(vec![
                IrExpression::Literal(Literal::Number("1".to_string()), dummy_span()),
                IrExpression::Literal(Literal::String("oops".to_string()), dummy_span()),
            ]),
            delimiter: SequenceDelimiter::Whitespace,
            span: dummy_span(),
        };

        let error = infer_java_type(None, Some(&whitespace_array), &mut context)
            .expect_err("mixed whitespace array should error");

        match error {
            TransformError::WhitespaceSequenceTypeMismatch {
                expected, found, ..
            } => {
                assert!(expected.contains("int") || expected.contains("Int"));
                assert!(found.contains("String"));
            }
            other => panic!(
                "expected whitespace sequence mismatch error, got {:?}",
                other
            ),
        }
    }

    #[test]
    fn transform_whitespace_array_literal_lowers_to_list_creation() {
        let mut context = test_context();

        let expr = Expression::Array {
            elements: vec![
                Expression::Literal(Literal::Number("1".to_string()), dummy_span()),
                Expression::Literal(Literal::Number("2".to_string()), dummy_span()),
            ],
            delimiter: SequenceDelimiter::Whitespace,
            span: dummy_span(),
        };

        let ir = transform_expression(expr, &mut context)
            .expect("whitespace array lowering should succeed");

        match ir {
            IrExpression::ArrayCreation {
                element_type,
                dimensions,
                initializer,
                delimiter,
                ..
            } => {
                assert_eq!(element_type, JavaType::int());
                assert!(
                    dimensions.is_empty(),
                    "whitespace arrays should not emit dimensions"
                );
                assert_eq!(delimiter, SequenceDelimiter::Whitespace);
                let init = initializer.expect("initializer expected for array literal");
                assert_eq!(init.len(), 2);
                for (index, element) in init.into_iter().enumerate() {
                    match element {
                        IrExpression::Literal(Literal::Number(value), _) => {
                            assert_eq!(value, (index + 1).to_string());
                        }
                        other => panic!("expected numeric literal, got {:?}", other),
                    }
                }
            }
            other => panic!("expected array creation, got {:?}", other),
        }
    }

    #[test]
    fn transform_comma_array_literal_preserves_dimension_metadata() {
        let mut context = test_context();

        let expr = Expression::Array {
            elements: vec![
                Expression::Literal(Literal::Number("1".to_string()), dummy_span()),
                Expression::Literal(Literal::Number("2".to_string()), dummy_span()),
            ],
            delimiter: SequenceDelimiter::Comma,
            span: dummy_span(),
        };

        let ir =
            transform_expression(expr, &mut context).expect("comma array lowering should succeed");

        match ir {
            IrExpression::ArrayCreation {
                element_type,
                dimensions,
                initializer,
                delimiter,
                ..
            } => {
                assert_eq!(element_type, JavaType::int());
                assert_eq!(dimensions.len(), 1);
                assert!(dimensions.iter().all(|dim| dim.is_none()));
                assert_eq!(delimiter, SequenceDelimiter::Comma);
                let init = initializer.expect("initializer expected for array literal");
                assert_eq!(init.len(), 2);
            }
            other => panic!("expected array creation, got {:?}", other),
        }
    }

    #[test]
    fn test_whitespace_call_mixed_type_is_permitted() {
        let mut context = test_context();

        let whitespace_call = IrExpression::MethodCall {
            receiver: None,
            method_name: "printAll".to_string(),
            java_name: None,
            resolved_target: None,
            args: vec![
                IrExpression::Literal(Literal::Number("1".to_string()), dummy_span()),
                IrExpression::Literal(Literal::Number("2".to_string()), dummy_span()),
                IrExpression::Literal(Literal::String("oops".to_string()), dummy_span()),
            ],
            argument_style: CallArgumentStyle::Whitespace,
            java_type: JavaType::void(),
            span: dummy_span(),
        };

        let inferred = infer_java_type(None, Some(&whitespace_call), &mut context)
            .expect("mixed whitespace call should be accepted");

        assert_eq!(inferred, JavaType::void());
    }

    // Test for type annotation conversion
    #[test]
    fn test_convert_type_annotation_handles_generic_erasure() {
        let type_annotation = TypeAnnotation::Generic {
            name: "List".to_string(),
            type_args: vec![TypeAnnotation::Simple("String".to_string())],
        };

        let java_type = convert_type_annotation(type_annotation)
            .expect("generic annotations should preserve type parameters");

        // Generic types are now preserved, not erased to Object
        assert_eq!(
            java_type,
            JavaType::Reference {
                name: "List".to_string(),
                generic_args: vec![JavaType::Reference {
                    name: "String".to_string(),
                    generic_args: vec![],
                }],
            }
        );
    }

    // Test for utility class name generation
    #[test]
    fn test_generate_utility_class_name_formats_with_suffix() {
        let class_name =
            generate_utility_class_name(Some("com.example"), "src/foo/bar-user_profile.jv");
        assert_eq!(class_name, "com.example.BarUserProfileKt");

        let default_package = generate_utility_class_name(None, "2d_scene.jv");
        assert_eq!(default_package, "_2dSceneKt");
    }

    // Test for extension class name generation
    #[test]
    fn test_generate_extension_class_name_handles_complex_types() {
        let simple = TypeAnnotation::Simple("String".to_string());
        assert_eq!(generate_extension_class_name(&simple), "StringExtensions");

        let complex = TypeAnnotation::Nullable(Box::new(TypeAnnotation::Array(Box::new(
            TypeAnnotation::Generic {
                name: "java.util.List".to_string(),
                type_args: vec![TypeAnnotation::Simple("User".to_string())],
            },
        ))));
        assert_eq!(
            generate_extension_class_name(&complex),
            "ListArrayExtensions"
        );
    }

    // Test for main transformation functions
    #[test]
    fn test_transform_program_produces_ir_program() {
        let program = Program {
            package: Some("com.example".to_string()),
            imports: vec![],
            statements: vec![Statement::ValDeclaration {
                name: "greeting".to_string(),
                binding: None,

                type_annotation: None,
                initializer: Expression::Literal(
                    Literal::String("Hello, World!".to_string()),
                    dummy_span(),
                ),
                modifiers: Modifiers::default(),
                origin: ValBindingOrigin::ExplicitKeyword,
                span: dummy_span(),
            }],
            span: dummy_span(),
        };

        let ir_program = transform_program(program).expect("program transformation should succeed");
        assert_eq!(ir_program.type_declarations.len(), 1);
    }

    #[test]
    fn casts_in_normalize_int_family_value_preserve_ir_cast_nodes() {
        let source = r#"
fun sample(value: Any): Int {
    val character = value as Character
    val number = value as Number
    return 0
}
"#;

        let program = parse_program(source);
        let mut context = TransformContext::new();
        let ir_program = transform_program_with_context(program, &mut context)
            .expect("sample lowering succeeds");

        let method_body = ir_program
            .type_declarations
            .iter()
            .find_map(|decl| {
                if let IrStatement::MethodDeclaration {
                    name,
                    body: Some(body),
                    ..
                } = decl
                {
                    if name == "sample" {
                        return Some(body);
                    }
                }
                None
            })
            .expect("sample method present in IR");

        let statements = match method_body {
            IrExpression::Block { statements, .. } => statements,
            other => panic!("expected block body, found {:?}", other),
        };

        let mut saw_character_cast = false;
        let mut saw_number_cast = false;

        for statement in statements {
            if let IrStatement::VariableDeclaration {
                name,
                java_type,
                initializer,
                ..
            } = statement
            {
                match name.as_str() {
                    "character" => {
                        let expected_type = JavaType::Reference {
                            name: "Character".to_string(),
                            generic_args: Vec::new(),
                        };
                        assert_eq!(java_type, &expected_type);
                        let Some(IrExpression::Cast {
                            expr, target_type, ..
                        }) = initializer
                        else {
                            panic!("character initializer should be a cast")
                        };
                        assert_eq!(target_type, &expected_type);
                        match expr.as_ref() {
                            IrExpression::Identifier { name, .. } => {
                                assert_eq!(name, "value", "cast should wrap original identifier")
                            }
                            other => {
                                panic!("expected cast operand to be identifier, found {:?}", other)
                            }
                        }
                        saw_character_cast = true;
                    }
                    "number" => {
                        let expected_type = JavaType::Reference {
                            name: "Number".to_string(),
                            generic_args: Vec::new(),
                        };
                        assert_eq!(java_type, &expected_type);
                        let Some(IrExpression::Cast {
                            expr, target_type, ..
                        }) = initializer
                        else {
                            panic!("number initializer should be a cast")
                        };
                        assert_eq!(target_type, &expected_type);
                        match expr.as_ref() {
                            IrExpression::Identifier { name, .. } => {
                                assert_eq!(name, "value", "cast should wrap original identifier")
                            }
                            other => {
                                panic!("expected cast operand to be identifier, found {:?}", other)
                            }
                        }
                        saw_number_cast = true;
                    }
                    _ => {}
                }
            }
        }

        assert!(
            saw_character_cast,
            "character declaration should include cast"
        );
        assert!(saw_number_cast, "number declaration should include cast");
    }

    #[test]
    fn test_transform_statement_handles_val_declaration() {
        let mut context = test_context();

        let stmt = Statement::ValDeclaration {
            name: "x".to_string(),
            binding: None,

            type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
            initializer: Expression::Literal(Literal::Number("42".to_string()), dummy_span()),
            modifiers: Modifiers::default(),
            origin: ValBindingOrigin::ExplicitKeyword,
            span: dummy_span(),
        };

        let result = transform_statement(stmt, &mut context)
            .expect("statement transformation should succeed");
        assert_eq!(result.len(), 1);
    }

    #[test]
    fn test_transform_expression_binary_operation_creates_ir() {
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

        let ir_expr = transform_expression(expr, &mut context)
            .expect("expression transformation should succeed");

        match ir_expr {
            IrExpression::Binary { .. } => {}
            other => panic!("Expected binary IR expression, got {:?}", other),
        }
    }

    #[test]
    fn test_implicit_println_lowering_uses_java_lang_system_out() {
        let mut context = test_context();
        context.add_variable("greeting".to_string(), JavaType::string());

        let expr = Expression::Call {
            function: Box::new(Expression::Identifier("println".to_string(), dummy_span())),
            args: vec![Argument::Positional(Expression::Identifier(
                "greeting".to_string(),
                dummy_span(),
            ))],
            type_arguments: Vec::new(),
            argument_metadata: CallArgumentMetadata::default(),
            span: dummy_span(),
        };

        let ir_expr =
            transform_expression(expr, &mut context).expect("println lowering should succeed");

        match ir_expr {
            IrExpression::MethodCall {
                receiver: Some(receiver),
                method_name,
                args,
                ..
            } => {
                assert_eq!(method_name, "println");
                assert_eq!(args.len(), 1);
                match &args[0] {
                    IrExpression::Identifier { name, .. } => assert_eq!(name, "greeting"),
                    other => panic!("expected greeting identifier, got {:?}", other),
                }

                match *receiver {
                    IrExpression::FieldAccess {
                        receiver: inner_receiver,
                        field_name,
                        ..
                    } => {
                        assert_eq!(field_name, "out");
                        match *inner_receiver {
                            IrExpression::Identifier { name, .. } => {
                                assert_eq!(name, "System");
                            }
                            other => panic!("expected system identifier, got {:?}", other),
                        }
                    }
                    other => panic!("expected field access receiver, got {:?}", other),
                }
            }
            other => panic!("expected println method call, got {:?}", other),
        }
    }

    #[test]
    fn test_explicit_system_in_call_uses_fully_qualified_receiver() {
        let mut context = test_context();

        let expr = Expression::Call {
            function: Box::new(Expression::MemberAccess {
                object: Box::new(Expression::MemberAccess {
                    object: Box::new(Expression::Identifier("system".to_string(), dummy_span())),
                    property: "in".to_string(),
                    span: dummy_span(),
                }),
                property: "read".to_string(),
                span: dummy_span(),
            }),
            args: Vec::new(),
            type_arguments: Vec::new(),
            argument_metadata: CallArgumentMetadata::default(),
            span: dummy_span(),
        };

        let ir_expr = transform_expression(expr, &mut context)
            .expect("system.in.read lowering should succeed");

        match ir_expr {
            IrExpression::MethodCall {
                receiver: Some(receiver),
                method_name,
                java_type,
                ..
            } => {
                assert_eq!(method_name, "read");
                assert_eq!(java_type, JavaType::Primitive("int".to_string()));

                match *receiver {
                    IrExpression::FieldAccess {
                        receiver: inner_receiver,
                        field_name,
                        ..
                    } => {
                        assert_eq!(field_name, "in");
                        match *inner_receiver {
                            IrExpression::Identifier { name, .. } => {
                                assert_eq!(name, "System");
                            }
                            other => panic!("expected system identifier, got {:?}", other),
                        }
                    }
                    other => panic!("expected field access receiver, got {:?}", other),
                }
            }
            other => panic!("expected system.in.read method call, got {:?}", other),
        }
    }

    // Complex integration tests for combined desugaring

    #[test]
    fn test_complex_when_with_patterns_and_guards_returns_error() {
        let mut context = test_context();
        context.add_variable("value".to_string(), JavaType::int());

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
            inclusive_end: false,
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
                guard: None,
                body: Expression::Literal(Literal::String("small".to_string()), dummy_span()),
                span: dummy_span(),
            },
            WhenArm {
                pattern: guard_pattern,
                guard: None,
                body: Expression::Literal(Literal::String("long string".to_string()), dummy_span()),
                span: dummy_span(),
            },
        ];

        let else_arm = Some(Box::new(Expression::Literal(
            Literal::String("other".to_string()),
            dummy_span(),
        )));

        let result =
            desugar_when_expression(subject, arms, else_arm, None, dummy_span(), &mut context)
                .expect("complex patterns should now be supported");

        let IrExpression::Switch { cases, .. } = result else {
            panic!("expected switch lowering for complex pattern");
        };

        assert_eq!(cases.len(), 3, "expected two explicit arms plus default");

        match &cases[0].labels[0] {
            IrCaseLabel::Range {
                type_name,
                inclusive_end,
                ..
            } => {
                assert_eq!(type_name, "int");
                assert!(!inclusive_end, "range should remain exclusive");
            }
            other => panic!("expected range label, got {other:?}"),
        }

        match &cases[1].labels[0] {
            IrCaseLabel::TypePattern {
                type_name,
                deconstruction: Some(pattern),
                ..
            } => {
                assert_eq!(type_name, "String");
                assert_eq!(pattern.components.len(), 1);
            }
            other => panic!("expected type pattern label with deconstruction, got {other:?}"),
        }
        assert!(
            cases[1].guard.is_some(),
            "guard expression should be preserved"
        );

        assert!(matches!(cases[2].labels[0], IrCaseLabel::Default));
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
    fn test_desugar_extension_function_with_generics_succeeds() {
        let mut context = test_context();

        let receiver_type = TypeAnnotation::Generic {
            name: "List".to_string(),
            type_args: vec![TypeAnnotation::Simple("T".to_string())],
        };

        let function_decl = Statement::FunctionDeclaration {
            name: "secondOrNull".to_string(),
            type_parameters: vec!["T".to_string()],
            generic_signature: None,
            where_clause: None,
            parameters: vec![],
            return_type: Some(TypeAnnotation::Nullable(Box::new(TypeAnnotation::Simple(
                "T".to_string(),
            )))),
            primitive_return: None,
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

        let method = desugar_extension_function(
            receiver_type,
            Box::new(function_decl),
            dummy_span(),
            &mut context,
        )
        .expect("generic extension functions should be desugared");

        match method {
            IrStatement::MethodDeclaration {
                type_parameters,
                parameters,
                return_type,
                ..
            } => {
                assert_eq!(parameters.len(), 1);
                match return_type {
                    JavaType::Reference { ref name, .. } => assert_eq!(name, "T"),
                    other => panic!("Expected reference return type, got {:?}", other),
                }
                assert_eq!(type_parameters.len(), 1);
                assert_eq!(type_parameters[0].name, "T");
            }
            other => panic!("Expected method declaration, got {:?}", other),
        }
    }

    #[test]
    fn test_transform_expression_handles_nested_null_safe_operations() {
        let mut context = test_context();
        context.add_variable("user".to_string(), JavaType::object());

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

        let result = transform_expression(nested_access, &mut context)
            .expect("nested null-safe member access should desugar");

        fn assert_null_safe_chain(expr: &IrExpression, depth: usize) {
            if depth == 0 {
                return;
            }
            match expr {
                IrExpression::NullSafeOperation { operation, .. } => {
                    assert_null_safe_chain(operation.as_ref(), depth - 1);
                }
                IrExpression::FieldAccess { .. } => {}
                other => panic!("Expected null-safe chain, got {:?}", other),
            }
        }

        assert_null_safe_chain(&result, 3);
    }

    #[test]
    fn test_desugar_data_class_with_default_parameters_creates_record() {
        let mut context = test_context();

        let params = vec![
            Parameter {
                name: "host".to_string(),
                type_annotation: Some(TypeAnnotation::Simple("String".to_string())),
                default_value: Some(Expression::Literal(
                    Literal::String("localhost".to_string()),
                    dummy_span(),
                )),
                modifiers: ParameterModifiers::default(),
                span: dummy_span(),
            },
            Parameter {
                name: "port".to_string(),
                type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
                default_value: Some(Expression::Literal(
                    Literal::Number("8080".to_string()),
                    dummy_span(),
                )),
                modifiers: ParameterModifiers::default(),
                span: dummy_span(),
            },
            Parameter {
                name: "ssl".to_string(),
                type_annotation: Some(TypeAnnotation::Simple("Boolean".to_string())),
                default_value: Some(Expression::Literal(Literal::Boolean(false), dummy_span())),
                modifiers: ParameterModifiers::default(),
                span: dummy_span(),
            },
        ];

        let result = desugar_data_class(
            "Config".to_string(),
            params,
            vec![],
            false,
            Modifiers::default(),
            dummy_span(),
            &mut context,
        )
        .expect("data class with defaults should desugar to record");

        match result {
            IrStatement::RecordDeclaration { components, .. } => {
                assert_eq!(components.len(), 3);
                assert_eq!(components[0].name, "host");
                assert_eq!(components[1].name, "port");
                assert_eq!(components[2].name, "ssl");
            }
            other => panic!("Expected record declaration, got {:?}", other),
        }
    }

    #[test]
    fn test_spawn_with_use_resource_produces_virtual_thread() {
        let mut context = test_context();

        context.add_variable("openConnection".to_string(), JavaType::object());

        let use_expr = Expression::Block {
            statements: vec![Statement::ResourceManagement(ResourceManagement::Use {
                resource: Box::new(Expression::Call {
                    function: Box::new(Expression::Identifier(
                        "openConnection".to_string(),
                        dummy_span(),
                    )),
                    args: vec![],
                    type_arguments: Vec::new(),
                    argument_metadata: CallArgumentMetadata::with_style(CallArgumentStyle::Comma),
                    span: dummy_span(),
                }),
                body: Box::new(Expression::Lambda {
                    parameters: vec![Parameter {
                        name: "conn".to_string(),
                        type_annotation: None,
                        default_value: None,
                        modifiers: ParameterModifiers::default(),
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
                                    type_arguments: Vec::new(),
                                    argument_metadata: CallArgumentMetadata::with_style(
                                        CallArgumentStyle::Comma,
                                    ),
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
                                    type_arguments: Vec::new(),
                                    argument_metadata: CallArgumentMetadata::with_style(
                                        CallArgumentStyle::Comma,
                                    ),
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

        let ir = desugar_spawn_expression(Box::new(use_expr), dummy_span(), &mut context)
            .expect("spawn with use should desugar");

        match ir {
            IrExpression::VirtualThread { args, .. } => {
                assert_eq!(args.len(), 1);
                match &args[0] {
                    IrExpression::Lambda { body, .. } => match body.as_ref() {
                        IrExpression::Block { statements, .. } => {
                            assert_eq!(statements.len(), 1);
                            match &statements[0] {
                                IrStatement::Expression { expr, .. } => match expr {
                                    IrExpression::TryWithResources { .. } => {}
                                    other => panic!("Expected try-with-resources, got {:?}", other),
                                },
                                other => panic!("Expected expression statement, got {:?}", other),
                            }
                        }
                        other => panic!("Expected lambda body block, got {:?}", other),
                    },
                    other => panic!("Expected lambda argument, got {:?}", other),
                }
            }
            other => panic!("Expected virtual thread expression, got {:?}", other),
        }
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

    #[test]
    fn method_declaration_serialization_roundtrip_keeps_java_name() {
        let method = IrStatement::MethodDeclaration {
            name: "flatMap".to_string(),
            java_name: Some("flatMap".to_string()),
            type_parameters: vec![],
            parameters: vec![],
            primitive_return: None,
            return_type: JavaType::void(),
            body: None,
            modifiers: IrModifiers::default(),
            throws: vec![],
            span: dummy_span(),
        };

        let json = serde_json::to_string(&method).expect("method should serialize");
        let restored: IrStatement = serde_json::from_str(&json).expect("method should deserialize");

        assert_eq!(restored, method);
    }

    #[test]
    fn method_call_serialization_roundtrip_keeps_resolution() {
        let call = IrExpression::MethodCall {
            receiver: Some(Box::new(IrExpression::Identifier {
                name: "sequence".to_string(),
                java_type: JavaType::Reference {
                    name: "jv.collections.Sequence".to_string(),
                    generic_args: vec![],
                },
                span: dummy_span(),
            })),
            method_name: "flatMap".to_string(),
            java_name: Some("flatMap".to_string()),
            resolved_target: Some(IrResolvedMethodTarget {
                owner: Some("jv.collections.Sequence".to_string()),
                original_name: Some("flatMap".to_string()),
                java_name: Some("flatMap".to_string()),
                erased_parameters: vec!["java.util.function.Function".to_string()],
            }),
            args: vec![],
            argument_style: CallArgumentStyle::Comma,
            java_type: JavaType::void(),
            span: dummy_span(),
        };

        let json = serde_json::to_string(&call).expect("call should serialize");
        let restored: IrExpression = serde_json::from_str(&json).expect("call should deserialize");

        assert_eq!(restored, call);
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
            java_name: None,
            resolved_target: None,
            args: vec![],
            argument_style: CallArgumentStyle::Comma,
            java_type: JavaType::void(),
            span: dummy_span(),
        };
        assert!(matches!(method_call, IrExpression::MethodCall { .. }));

        let lambda_ir = IrExpression::Lambda {
            functional_interface: "java.util.function.Function".to_string(),
            param_names: vec!["x".to_string()],
            param_types: vec![JavaType::object()],
            body: Box::new(IrExpression::Literal(
                Literal::Number("0".to_string()),
                dummy_span(),
            )),
            java_type: JavaType::object(),
            span: dummy_span(),
        };

        let mut pipeline = SequencePipeline {
            source: SequenceSource::Collection {
                expr: Box::new(IrExpression::Identifier {
                    name: "numbers".to_string(),
                    java_type: JavaType::object(),
                    span: dummy_span(),
                }),
                element_hint: None,
            },
            stages: vec![SequenceStage::Map {
                lambda: Box::new(lambda_ir.clone()),
                result_hint: None,
                span: dummy_span(),
            }],
            terminal: Some(SequenceTerminal {
                kind: SequenceTerminalKind::ToList,
                evaluation: SequenceTerminalEvaluation::Collector,
                requires_non_empty_source: false,
                specialization_hint: None,
                canonical_adapter: None,
                span: dummy_span(),
            }),
            lazy: false,
            span: dummy_span(),
            shape: PipelineShape::default(),
        };
        pipeline.recompute_shape();

        let sequence_expr = IrExpression::SequencePipeline {
            pipeline,
            java_type: JavaType::Reference {
                name: "java.util.List".to_string(),
                generic_args: vec![],
            },
            span: dummy_span(),
        };
        assert!(matches!(
            sequence_expr,
            IrExpression::SequencePipeline { .. }
        ));

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

    #[test]
    fn sequence_style_cache_reuses_array_entries_until_cleared() {
        let mut cache = SequenceStyleCache::with_capacity();
        let span = Span::new(1, 1, 1, 15);

        let first_insert = cache.lookup_or_insert_array(&span, JavaType::int());
        assert!(first_insert.is_none(), "first insert should miss cache");

        let reused = cache.lookup_or_insert_array(&span, JavaType::string());
        assert_eq!(reused, Some(JavaType::int()));

        cache.clear();

        let after_clear = cache.lookup_or_insert_array(&span, JavaType::string());
        assert!(
            after_clear.is_none(),
            "cache should forget entry after clear"
        );
    }

    #[test]
    fn sequence_style_cache_reuses_call_entries_until_cleared() {
        let mut cache = SequenceStyleCache::with_capacity();
        let span = Span::new(2, 4, 2, 12);

        let first_insert = cache.lookup_or_insert_call(&span, JavaType::int());
        assert!(first_insert.is_none());

        let reused = cache.lookup_or_insert_call(&span, JavaType::string());
        assert_eq!(reused, Some(JavaType::int()));

        cache.clear();

        let after_clear = cache.lookup_or_insert_call(&span, JavaType::string());
        assert!(after_clear.is_none());
    }

    #[test]
    fn sequence_pipeline_detection_recognizes_map_to_list() {
        let mut context = TransformContext::new();
        context.add_variable(
            "numbers".to_string(),
            JavaType::Reference {
                name: "java.util.List".to_string(),
                generic_args: vec![],
            },
        );

        let lambda_span = dummy_span();
        let lambda_expr = Expression::Lambda {
            parameters: vec![Parameter {
                name: "x".to_string(),
                type_annotation: None,
                default_value: None,
                modifiers: ParameterModifiers::default(),
                span: lambda_span.clone(),
            }],
            body: Box::new(Expression::Identifier("x".to_string(), lambda_span.clone())),
            span: lambda_span.clone(),
        };

        let map_call = Expression::Call {
            function: Box::new(Expression::MemberAccess {
                object: Box::new(Expression::Identifier("numbers".to_string(), dummy_span())),
                property: "map".to_string(),
                span: dummy_span(),
            }),
            args: vec![Argument::Positional(lambda_expr)],
            type_arguments: Vec::new(),
            argument_metadata: CallArgumentMetadata::default(),
            span: dummy_span(),
        };

        let to_list_call = Expression::Call {
            function: Box::new(Expression::MemberAccess {
                object: Box::new(map_call),
                property: "toList".to_string(),
                span: dummy_span(),
            }),
            args: vec![],
            type_arguments: Vec::new(),
            argument_metadata: CallArgumentMetadata::default(),
            span: dummy_span(),
        };

        let result = transform_expression(to_list_call, &mut context)
            .expect("pipeline transformation should succeed");

        match result {
            IrExpression::SequencePipeline {
                pipeline,
                java_type,
                ..
            } => {
                assert_eq!(pipeline.stages.len(), 1);
                assert!(matches!(pipeline.stages[0], SequenceStage::Map { .. }));

                let terminal = pipeline
                    .terminal
                    .as_ref()
                    .expect("pipeline should have terminal");
                assert!(matches!(terminal.kind, SequenceTerminalKind::ToList));
                assert!(!pipeline.lazy);

                assert_eq!(
                    java_type,
                    JavaType::Reference {
                        name: "java.util.List".to_string(),
                        generic_args: vec![],
                    }
                );
            }
            other => panic!("expected sequence pipeline, got {:?}", other),
        }
    }

    #[test]
    fn sequence_pipeline_shape_flags() {
        let make_map_stage = || SequenceStage::Map {
            lambda: Box::new(IrExpression::Identifier {
                name: "mapper".to_string(),
                java_type: JavaType::object(),
                span: dummy_span(),
            }),
            result_hint: None,
            span: dummy_span(),
        };

        let make_filter_stage = || SequenceStage::Filter {
            predicate: Box::new(IrExpression::Identifier {
                name: "predicate".to_string(),
                java_type: JavaType::object(),
                span: dummy_span(),
            }),
            span: dummy_span(),
        };

        let terminal_to_list = SequenceTerminal {
            kind: SequenceTerminalKind::ToList,
            evaluation: SequenceTerminalEvaluation::Collector,
            requires_non_empty_source: false,
            specialization_hint: None,
            canonical_adapter: None,
            span: dummy_span(),
        };

        let mut single_stage = SequencePipeline {
            source: SequenceSource::Collection {
                expr: Box::new(IrExpression::Identifier {
                    name: "numbers".to_string(),
                    java_type: JavaType::object(),
                    span: dummy_span(),
                }),
                element_hint: None,
            },
            stages: vec![make_map_stage()],
            terminal: Some(terminal_to_list.clone()),
            lazy: false,
            span: dummy_span(),
            shape: PipelineShape::default(),
        };
        single_stage.recompute_shape();
        assert!(matches!(single_stage.shape, PipelineShape::SingleStageMap));

        let mut multi_stage = SequencePipeline {
            source: SequenceSource::Collection {
                expr: Box::new(IrExpression::Identifier {
                    name: "numbers".to_string(),
                    java_type: JavaType::object(),
                    span: dummy_span(),
                }),
                element_hint: None,
            },
            stages: vec![make_map_stage(), make_filter_stage()],
            terminal: Some(terminal_to_list.clone()),
            lazy: false,
            span: dummy_span(),
            shape: PipelineShape::default(),
        };
        multi_stage.recompute_shape();
        match multi_stage.shape {
            PipelineShape::MultiStage {
                stages,
                repeated_transforms,
                has_terminal,
            } => {
                assert_eq!(stages, 2);
                assert!(!repeated_transforms);
                assert!(has_terminal);
            }
            other => panic!("unexpected shape {:?}", other),
        }

        let mut repeated = SequencePipeline {
            source: SequenceSource::Collection {
                expr: Box::new(IrExpression::Identifier {
                    name: "numbers".to_string(),
                    java_type: JavaType::object(),
                    span: dummy_span(),
                }),
                element_hint: None,
            },
            stages: vec![make_map_stage(), make_map_stage()],
            terminal: None,
            lazy: true,
            span: dummy_span(),
            shape: PipelineShape::default(),
        };
        repeated.recompute_shape();
        match repeated.shape {
            PipelineShape::MultiStage {
                repeated_transforms,
                ..
            } => assert!(repeated_transforms),
            other => panic!("unexpected shape {:?}", other),
        }

        let mut explicit_source = SequencePipeline {
            source: SequenceSource::JavaStream {
                expr: Box::new(IrExpression::Identifier {
                    name: "stream".to_string(),
                    java_type: JavaType::object(),
                    span: dummy_span(),
                }),
                element_hint: None,
                auto_close: true,
            },
            stages: vec![make_map_stage()],
            terminal: Some(terminal_to_list),
            lazy: false,
            span: dummy_span(),
            shape: PipelineShape::default(),
        };
        explicit_source.recompute_shape();
        assert!(matches!(
            explicit_source.shape,
            PipelineShape::ExplicitSequenceSource
        ));
    }
    #[test]
    fn numeric_range_lowering_produces_metadata_and_conditions() {
        let mut context = TransformContext::new();
        let span = dummy_span();

        let binding = LoopBinding {
            name: "idx".to_string(),
            pattern: None,

            type_annotation: None,
            span: span.clone(),
        };

        let range = NumericRangeLoop {
            start: Expression::Literal(Literal::Number("0".to_string()), span.clone()),
            end: Expression::Literal(Literal::Number("5".to_string()), span.clone()),
            inclusive: false,
            span: span.clone(),
        };

        let body_expr = Expression::Block {
            statements: vec![Statement::Expression {
                expr: Expression::Literal(Literal::Number("1".to_string()), span.clone()),
                span: span.clone(),
            }],
            span: span.clone(),
        };

        let stmt = Statement::ForIn(ForInStatement {
            binding,
            iterable: Expression::Literal(Literal::Null, span.clone()),
            strategy: LoopStrategy::NumericRange(range),
            body: Box::new(body_expr),
            span: span.clone(),
        });

        let ir = transform_statement(stmt, &mut context).expect("transform numeric range");
        assert_eq!(ir.len(), 1, "expected outer block");

        let block_statements = match &ir[0] {
            IrStatement::Block { statements, .. } => statements,
            other => panic!("expected block, got {:?}", other),
        };
        assert_eq!(
            block_statements.len(),
            2,
            "range lowering should emit temp + loop"
        );

        match &block_statements[0] {
            IrStatement::VariableDeclaration { is_final, .. } => assert!(*is_final),
            other => panic!("expected range bound declaration, got {:?}", other),
        }

        match &block_statements[1] {
            IrStatement::For {
                condition,
                metadata,
                ..
            } => {
                let metadata = metadata
                    .as_ref()
                    .expect("numeric range should attach metadata");
                let IrForLoopMetadata::NumericRange(IrNumericRangeLoop {
                    inclusive, binding, ..
                }) = metadata;
                assert!(!inclusive, "exclusive range should mark inclusive=false");
                assert_eq!(binding, "idx");

                let condition = condition.as_ref().expect("loop condition required");
                match condition {
                    IrExpression::Binary { op, .. } => assert!(matches!(op, BinaryOp::Less)),
                    other => panic!("expected binary comparison, got {:?}", other),
                }
            }
            other => panic!("expected for statement, got {:?}", other),
        }
    }

    #[test]
    fn inclusive_range_uses_less_equal() {
        let mut context = TransformContext::new();
        let span = dummy_span();

        let stmt = Statement::ForIn(ForInStatement {
            binding: LoopBinding {
                name: "idx".to_string(),
                pattern: None,

                type_annotation: None,
                span: span.clone(),
            },
            iterable: Expression::Literal(Literal::Null, span.clone()),
            strategy: LoopStrategy::NumericRange(NumericRangeLoop {
                start: Expression::Literal(Literal::Number("0".to_string()), span.clone()),
                end: Expression::Literal(Literal::Number("5".to_string()), span.clone()),
                inclusive: true,
                span: span.clone(),
            }),
            body: Box::new(Expression::Literal(
                Literal::Number("2".to_string()),
                span.clone(),
            )),
            span: span.clone(),
        });

        let ir = transform_statement(stmt, &mut context).expect("transform inclusive range");
        let block_statements = match &ir[0] {
            IrStatement::Block { statements, .. } => statements,
            other => panic!("expected block, got {:?}", other),
        };
        let condition = match &block_statements[1] {
            IrStatement::For {
                condition,
                metadata,
                ..
            } => {
                if let Some(IrForLoopMetadata::NumericRange(meta)) = metadata {
                    assert!(meta.inclusive);
                } else {
                    panic!("expected numeric range metadata");
                }
                condition.as_ref().expect("condition present")
            }
            other => panic!("expected for statement, got {:?}", other),
        };

        match condition {
            IrExpression::Binary { op, .. } => assert!(matches!(op, BinaryOp::LessEqual)),
            other => panic!("expected binary condition, got {:?}", other),
        }
    }

    #[test]
    fn lazy_sequence_lowering_wraps_with_temp_and_metadata() {
        let mut context = TransformContext::new();
        let span = dummy_span();
        context.add_variable("items".to_string(), JavaType::object());

        let stmt = Statement::ForIn(ForInStatement {
            binding: LoopBinding {
                name: "item".to_string(),
                pattern: None,

                type_annotation: None,
                span: span.clone(),
            },
            iterable: Expression::Identifier("items".to_string(), span.clone()),
            strategy: LoopStrategy::LazySequence {
                needs_cleanup: true,
            },
            body: Box::new(Expression::Literal(Literal::Null, span.clone())),
            span: span.clone(),
        });

        let ir = transform_statement(stmt, &mut context).expect("transform lazy sequence");
        assert_eq!(ir.len(), 1);
        let block_statements = match &ir[0] {
            IrStatement::Block { statements, .. } => statements,
            other => panic!("expected block, got {:?}", other),
        };
        assert_eq!(block_statements.len(), 2, "temp + foreach expected");

        let foreach_kind = match &block_statements[1] {
            IrStatement::ForEach { iterable_kind, .. } => iterable_kind,
            other => panic!("expected foreach, got {:?}", other),
        };

        match foreach_kind {
            IrForEachKind::LazySequence { needs_cleanup } => assert!(*needs_cleanup),
            other => panic!("expected lazy sequence metadata, got {:?}", other),
        }
    }

    #[test]
    fn iterable_strategy_returns_single_foreach_statement() {
        let mut context = TransformContext::new();
        let span = dummy_span();
        context.add_variable("values".to_string(), JavaType::object());

        let stmt = Statement::ForIn(ForInStatement {
            binding: LoopBinding {
                name: "value".to_string(),
                pattern: None,

                type_annotation: None,
                span: span.clone(),
            },
            iterable: Expression::Identifier("values".to_string(), span.clone()),
            strategy: LoopStrategy::Iterable,
            body: Box::new(Expression::Literal(Literal::Null, span.clone())),
            span: span.clone(),
        });

        let ir = transform_statement(stmt, &mut context).expect("transform iterable");
        assert_eq!(ir.len(), 1);
        match &ir[0] {
            IrStatement::ForEach { iterable_kind, .. } => match iterable_kind {
                IrForEachKind::Iterable => {}
                other => panic!("expected iterable kind, got {:?}", other),
            },
            other => panic!("expected foreach, got {:?}", other),
        }
    }

    #[test]
    fn when_expression_with_subject_desugars_to_switch() {
        let expr = parse_when_expression(
            "val value = \"input\"\n             val label = when (value) {\n             is String -> \"string\"\n             else -> \"fallback\"\n        }\n",
        );

        let Expression::When {
            expr: subject,
            arms,
            else_arm,
            implicit_end,
            span,
        } = expr
        else {
            panic!("expected when expression");
        };

        let mut context = TransformContext::new();
        context.add_variable("value".to_string(), JavaType::object());
        let ir = desugar_when_expression(
            subject,
            arms,
            else_arm,
            implicit_end,
            span.clone(),
            &mut context,
        )
        .expect("lowering should succeed");

        match ir {
            IrExpression::Switch {
                strategy_description,
                ..
            } => {
                assert!(
                    strategy_description
                        .as_deref()
                        .unwrap_or_default()
                        .starts_with("strategy=Switch"),
                    "strategy metadata should describe switch lowering",
                );
            }
            other => panic!("expected switch expression, got {other:?}"),
        }

        let strategies = context.take_when_strategies();
        assert_eq!(strategies.len(), 1);
        assert!(
            strategies[0]
                .description
                .starts_with("strategy=Switch arms=2"),
            "context should record switch lowering strategy",
        );
    }

    #[test]
    fn subjectless_when_desugars_to_conditional_chain() {
        let expr = parse_when_expression(
            "val status = when {\n             isLarge() -> \"Large\"\n             else -> \"Small\"\n        }\n",
        );

        let Expression::When {
            expr: subject,
            arms,
            else_arm,
            implicit_end,
            span,
        } = expr
        else {
            panic!("expected when expression");
        };

        assert!(subject.is_none(), "subjectless when should have no subject");

        let mut context = TransformContext::new();
        let ir = desugar_when_expression(
            subject,
            arms,
            else_arm,
            implicit_end,
            span.clone(),
            &mut context,
        )
        .expect("lowering should succeed");

        matches!(ir, IrExpression::Conditional { .. })
            .then_some(())
            .expect("subjectless when should become conditional chain");

        let strategies = context.take_when_strategies();
        assert_eq!(strategies.len(), 1);
        assert!(
            strategies[0].description.starts_with("strategy=IfChain"),
            "context should record subjectless lowering metadata",
        );
    }

    #[test]
    fn subjectless_when_records_if_chain_metadata() {
        let expr = parse_when_expression(
            "val status = when {\n             checkPrimary() -> \"primary\"\n             else -> \"fallback\"\n        }\n",
        );

        let Expression::When {
            expr: subject,
            arms,
            else_arm,
            implicit_end,
            span,
        } = expr
        else {
            panic!("expected when expression");
        };

        assert!(
            subject.is_none(),
            "subjectless when should not have a discriminant"
        );
        let arm_count = arms.len();
        let recorded_span = span.clone();

        let mut context = TransformContext::new();
        let ir = desugar_when_expression(
            subject,
            arms,
            else_arm,
            implicit_end,
            span.clone(),
            &mut context,
        )
        .expect("subjectless lowering should succeed");

        matches!(ir, IrExpression::Conditional { .. })
            .then_some(())
            .expect("subjectless when should lower to conditional chain");

        let strategies = context.take_when_strategies();
        assert_eq!(strategies.len(), 1);
        let expected = format!("strategy=IfChain arms={} exhaustive=unknown", arm_count);
        assert_eq!(strategies[0].description, expected);
        assert_eq!(strategies[0].span, recorded_span);
    }

    #[test]
    fn nested_guard_in_destructuring_emits_jv3199() {
        let span = dummy_span();
        let when_expr = Expression::When {
            expr: Some(Box::new(Expression::Identifier(
                "value".to_string(),
                span.clone(),
            ))),
            arms: vec![WhenArm {
                pattern: Pattern::Constructor {
                    name: "Container".to_string(),
                    patterns: vec![Pattern::Guard {
                        pattern: Box::new(Pattern::Constructor {
                            name: "Point".to_string(),
                            patterns: vec![
                                Pattern::Identifier("x".to_string(), span.clone()),
                                Pattern::Identifier("y".to_string(), span.clone()),
                            ],
                            span: span.clone(),
                        }),
                        condition: Expression::Literal(Literal::Boolean(true), span.clone()),
                        span: span.clone(),
                    }],
                    span: span.clone(),
                },
                guard: None,
                body: Expression::Literal(Literal::Number("1".to_string()), span.clone()),
                span: span.clone(),
            }],
            else_arm: Some(Box::new(Expression::Literal(
                Literal::Number("0".to_string()),
                span.clone(),
            ))),
            implicit_end: None,
            span: span.clone(),
        };

        let Expression::When {
            expr: subject,
            arms,
            else_arm,
            implicit_end,
            span,
        } = when_expr
        else {
            unreachable!("constructed expression should be a when expression");
        };

        let mut context = TransformContext::new();
        context.add_variable("value".to_string(), JavaType::object());
        let result = desugar_when_expression(
            subject,
            arms,
            else_arm,
            implicit_end,
            span.clone(),
            &mut context,
        );

        let Err(TransformError::UnsupportedConstruct { construct, .. }) = result else {
            panic!("expected nested guard to raise unsupported construct error");
        };
        assert!(
            construct.contains("JV3199"),
            "JV3199 diagnostic should be embedded in unsupported construct message"
        );
        assert!(
            construct.contains("--explain JV3199"),
            "JV3199 explanation metadata should be present"
        );
    }

    #[test]
    fn nested_range_in_destructuring_emits_jv3199() {
        let span = dummy_span();
        let when_expr = Expression::When {
            expr: Some(Box::new(Expression::Identifier(
                "value".to_string(),
                span.clone(),
            ))),
            arms: vec![WhenArm {
                pattern: Pattern::Constructor {
                    name: "Container".to_string(),
                    patterns: vec![
                        Pattern::Range {
                            start: Box::new(Expression::Literal(
                                Literal::Number("0".to_string()),
                                span.clone(),
                            )),
                            end: Box::new(Expression::Literal(
                                Literal::Number("10".to_string()),
                                span.clone(),
                            )),
                            inclusive_end: false,
                            span: span.clone(),
                        },
                        Pattern::Identifier("rest".to_string(), span.clone()),
                    ],
                    span: span.clone(),
                },
                guard: None,
                body: Expression::Literal(Literal::Number("1".to_string()), span.clone()),
                span: span.clone(),
            }],
            else_arm: Some(Box::new(Expression::Literal(
                Literal::Number("0".to_string()),
                span.clone(),
            ))),
            implicit_end: None,
            span: span.clone(),
        };

        let Expression::When {
            expr: subject,
            arms,
            else_arm,
            implicit_end,
            span,
        } = when_expr
        else {
            unreachable!("constructed expression should be a when expression");
        };

        let mut context = TransformContext::new();
        context.add_variable("value".to_string(), JavaType::object());
        let result = desugar_when_expression(
            subject,
            arms,
            else_arm,
            implicit_end,
            span.clone(),
            &mut context,
        );

        let Err(TransformError::UnsupportedConstruct { construct, .. }) = result else {
            panic!("expected nested range to raise unsupported construct error");
        };
        assert!(
            construct.contains("JV3199"),
            "JV3199 diagnostic should be embedded in unsupported construct message"
        );
        assert!(
            construct.contains("--explain JV3199"),
            "JV3199 explanation metadata should be present"
        );
    }

    fn nested_constructor_pattern(depth: usize, span: &Span) -> Pattern {
        assert!(depth >= 1, "depth must be at least 1");
        let mut current = Pattern::Identifier("leaf".to_string(), span.clone());
        for level in (1..=depth).rev() {
            current = Pattern::Constructor {
                name: format!("Ctor{level}"),
                patterns: vec![current],
                span: span.clone(),
            };
        }
        current
    }

    fn nested_deconstruction_depth(pattern: &IrDeconstructionPattern) -> usize {
        let mut depth = 1;
        let mut current = pattern;
        while let Some(IrDeconstructionComponent::Type {
            pattern: Some(inner),
            ..
        }) = current.components.first()
        {
            depth += 1;
            current = inner;
        }
        depth
    }

    #[test]
    fn depth_ten_destructuring_lowering_succeeds() {
        let span = dummy_span();
        let when_expr = Expression::When {
            expr: Some(Box::new(Expression::Identifier(
                "value".to_string(),
                span.clone(),
            ))),
            arms: vec![WhenArm {
                pattern: nested_constructor_pattern(10, &span),
                guard: None,
                body: Expression::Literal(Literal::Number("1".to_string()), span.clone()),
                span: span.clone(),
            }],
            else_arm: None,
            implicit_end: None,
            span: span.clone(),
        };

        let Expression::When {
            expr: subject,
            arms,
            else_arm,
            implicit_end,
            span,
        } = when_expr
        else {
            unreachable!("constructed expression should be a when expression");
        };

        let mut context = TransformContext::new();
        context.add_variable("value".to_string(), JavaType::object());
        let result = desugar_when_expression(
            subject,
            arms,
            else_arm,
            implicit_end,
            span.clone(),
            &mut context,
        )
        .expect("depth-10 destructuring should lower successfully");

        let IrExpression::Switch { cases, .. } = result else {
            panic!("expected when lowering to produce a switch expression");
        };

        assert_eq!(
            cases.len(),
            1,
            "single arm should lower to single switch case"
        );
        let IrCaseLabel::TypePattern { deconstruction, .. } = &cases[0].labels[0] else {
            panic!("expected type pattern label for destructuring case");
        };

        let pattern = deconstruction
            .as_ref()
            .expect("top-level case should record deconstruction metadata");
        assert_eq!(
            nested_deconstruction_depth(pattern),
            10,
            "deconstruction metadata should preserve full depth"
        );
    }

    #[test]
    fn depth_eleven_destructuring_emits_jv3199() {
        let span = dummy_span();
        let when_expr = Expression::When {
            expr: Some(Box::new(Expression::Identifier(
                "value".to_string(),
                span.clone(),
            ))),
            arms: vec![WhenArm {
                pattern: nested_constructor_pattern(11, &span),
                guard: None,
                body: Expression::Literal(Literal::Number("1".to_string()), span.clone()),
                span: span.clone(),
            }],
            else_arm: None,
            implicit_end: None,
            span: span.clone(),
        };

        let Expression::When {
            expr: subject,
            arms,
            else_arm,
            implicit_end,
            span,
        } = when_expr
        else {
            unreachable!("constructed expression should be a when expression");
        };

        let mut context = TransformContext::new();
        context.add_variable("value".to_string(), JavaType::object());
        let result = desugar_when_expression(
            subject,
            arms,
            else_arm,
            implicit_end,
            span.clone(),
            &mut context,
        );

        let Err(TransformError::UnsupportedConstruct { construct, .. }) = result else {
            panic!("expected depth-11 destructuring to raise JV3199");
        };
        assert!(
            construct.contains("JV3199"),
            "depth-11 error should embed JV3199 diagnostic code"
        );
        assert!(
            construct.contains("--explain JV3199"),
            "depth-11 error should include explain metadata"
        );
    }

    #[test]
    fn transform_filters_jv_only_comments() {
        let span = dummy_span();
        let program = Program {
            package: None,
            imports: vec![],
            statements: vec![
                Statement::Comment(CommentStatement {
                    kind: CommentKind::Line,
                    visibility: CommentVisibility::Passthrough,
                    text: "// keep".to_string(),
                    span: span.clone(),
                }),
                Statement::Comment(CommentStatement {
                    kind: CommentKind::Line,
                    visibility: CommentVisibility::JvOnly,
                    text: "/// drop".to_string(),
                    span: span.clone(),
                }),
                Statement::Expression {
                    expr: Expression::Literal(Literal::Number("1".to_string()), span.clone()),
                    span: span.clone(),
                },
            ],
            span: span.clone(),
        };

        let mut context = TransformContext::new();
        let ir = transform_program_with_context(program, &mut context)
            .expect("transform program with comments");

        let rendered_comments: Vec<_> = ir
            .type_declarations
            .iter()
            .filter_map(|stmt| match stmt {
                IrStatement::Comment { text, .. } => Some(text.clone()),
                _ => None,
            })
            .collect();

        assert_eq!(rendered_comments, vec!["// keep".to_string()]);
    }

    #[test]
    fn method_registry_assigns_unique_java_names() {
        let mut context = TransformContext::new();
        let owner = Some("jv.collections.SequenceExtensions".to_string());

        let span_decl_one = Span::new(1, 1, 1, 10);
        let span_decl_two = Span::new(2, 1, 2, 10);
        let span_call_one = Span::new(10, 1, 10, 10);
        let span_call_two = Span::new(11, 1, 11, 10);

        let sequence_type = JavaType::Reference {
            name: "jv.collections.Sequence".to_string(),
            generic_args: vec![],
        };
        let iterable_type = JavaType::Reference {
            name: "java.lang.Iterable".to_string(),
            generic_args: vec![],
        };

        let function_seq = JavaType::Reference {
            name: "java.util.function.Function".to_string(),
            generic_args: vec![sequence_type.clone(), sequence_type.clone()],
        };
        let function_iter = JavaType::Reference {
            name: "java.util.function.Function".to_string(),
            generic_args: vec![sequence_type.clone(), iterable_type.clone()],
        };

        context
            .method_declarations
            .push(RegisteredMethodDeclaration {
                owner: owner.clone(),
                name: "flatMap".to_string(),
                java_name: "flatMap".to_string(),
                parameter_types: vec![function_seq.clone()],
                return_type: sequence_type.clone(),
                is_static: true,
                primitive_return: None,
                span: span_decl_one.clone(),
            });
        context
            .method_declarations
            .push(RegisteredMethodDeclaration {
                owner: owner.clone(),
                name: "flatMap".to_string(),
                java_name: "flatMap".to_string(),
                parameter_types: vec![function_iter.clone()],
                return_type: sequence_type.clone(),
                is_static: true,
                primitive_return: None,
                span: span_decl_two.clone(),
            });

        context.method_calls.push(RegisteredMethodCall {
            owner: owner.clone(),
            original_name: "flatMap".to_string(),
            java_name: "flatMap".to_string(),
            receiver_type: None,
            argument_types: vec![function_seq.clone()],
            return_type: sequence_type.clone(),
            argument_style: CallArgumentStyle::Comma,
            span: span_call_one.clone(),
        });
        context.method_calls.push(RegisteredMethodCall {
            owner: owner.clone(),
            original_name: "flatMap".to_string(),
            java_name: "flatMap".to_string(),
            receiver_type: None,
            argument_types: vec![function_iter.clone()],
            return_type: sequence_type.clone(),
            argument_style: CallArgumentStyle::Comma,
            span: span_call_two.clone(),
        });

        let static_modifiers = |is_static| IrModifiers {
            is_static,
            ..IrModifiers::default()
        };

        let method_decl_one = IrStatement::MethodDeclaration {
            name: "flatMap".to_string(),
            java_name: None,
            type_parameters: vec![],
            parameters: vec![IrParameter {
                name: "mapper".to_string(),
                java_type: function_seq.clone(),
                modifiers: IrModifiers::default(),
                span: span_decl_one.clone(),
            }],
            primitive_return: None,
            return_type: sequence_type.clone(),
            body: None,
            modifiers: static_modifiers(true),
            throws: vec![],
            span: span_decl_one.clone(),
        };

        let method_decl_two = IrStatement::MethodDeclaration {
            name: "flatMap".to_string(),
            java_name: None,
            type_parameters: vec![],
            parameters: vec![IrParameter {
                name: "mapper".to_string(),
                java_type: function_iter.clone(),
                modifiers: IrModifiers::default(),
                span: span_decl_two.clone(),
            }],
            primitive_return: None,
            return_type: sequence_type.clone(),
            body: None,
            modifiers: static_modifiers(true),
            throws: vec![],
            span: span_decl_two.clone(),
        };

        let call_expr_one = IrExpression::MethodCall {
            receiver: None,
            method_name: "flatMap".to_string(),
            java_name: None,
            resolved_target: None,
            args: vec![IrExpression::Identifier {
                name: "mapperSeq".to_string(),
                java_type: function_seq.clone(),
                span: span_call_one.clone(),
            }],
            argument_style: CallArgumentStyle::Comma,
            java_type: sequence_type.clone(),
            span: span_call_one.clone(),
        };

        let call_expr_two = IrExpression::MethodCall {
            receiver: None,
            method_name: "flatMap".to_string(),
            java_name: None,
            resolved_target: None,
            args: vec![IrExpression::Identifier {
                name: "mapperIter".to_string(),
                java_type: function_iter.clone(),
                span: span_call_two.clone(),
            }],
            argument_style: CallArgumentStyle::Comma,
            java_type: sequence_type.clone(),
            span: span_call_two.clone(),
        };

        let mut statements = vec![
            method_decl_one,
            method_decl_two,
            IrStatement::Expression {
                expr: call_expr_one,
                span: span_call_one.clone(),
            },
            IrStatement::Expression {
                expr: call_expr_two,
                span: span_call_two.clone(),
            },
        ];

        apply_method_erasure(&mut statements, &context);

        let first_method_name = match &statements[0] {
            IrStatement::MethodDeclaration { java_name, .. } => java_name
                .as_ref()
                .expect("expected java_name on first declaration")
                .clone(),
            other => panic!("unexpected statement variant: {other:?}"),
        };
        let second_method_name = match &statements[1] {
            IrStatement::MethodDeclaration { java_name, .. } => java_name
                .as_ref()
                .expect("expected java_name on second declaration")
                .clone(),
            other => panic!("unexpected statement variant: {other:?}"),
        };

        assert_eq!(first_method_name, "flatMap");
        assert!(
            second_method_name.starts_with("flatMap$"),
            "expected suffixed java name, got {second_method_name}"
        );

        let (call_one_name, call_one_target) = match &statements[2] {
            IrStatement::Expression { expr, .. } => match expr {
                IrExpression::MethodCall {
                    java_name,
                    resolved_target,
                    ..
                } => (
                    java_name
                        .as_ref()
                        .expect("expected java_name on first call")
                        .clone(),
                    resolved_target.clone().expect("expected resolved target"),
                ),
                other => panic!("unexpected expression variant: {other:?}"),
            },
            other => panic!("unexpected statement variant: {other:?}"),
        };

        let (call_two_name, call_two_target) = match &statements[3] {
            IrStatement::Expression { expr, .. } => match expr {
                IrExpression::MethodCall {
                    java_name,
                    resolved_target,
                    ..
                } => (
                    java_name
                        .as_ref()
                        .expect("expected java_name on second call")
                        .clone(),
                    resolved_target.clone().expect("expected resolved target"),
                ),
                other => panic!("unexpected expression variant: {other:?}"),
            },
            other => panic!("unexpected statement variant: {other:?}"),
        };

        assert_eq!(call_one_name, first_method_name);
        assert_eq!(call_two_name, second_method_name);
        assert_eq!(
            call_one_target.erased_parameters,
            vec!["java.util.function.Function".to_string()]
        );
        assert_eq!(
            call_two_target.erased_parameters,
            vec!["java.util.function.Function".to_string()]
        );
        assert_eq!(
            call_two_target.java_name.as_deref(),
            Some(second_method_name.as_str())
        );
        assert_eq!(call_two_target.owner, owner);
    }
    #[test]
    fn method_registry_is_general_purpose_for_arbitrary_owners() {
        let mut context = TransformContext::new();
        let owner = Some("com.example.Utility".to_string());

        let span_decl_one = Span::new(5, 1, 5, 20);
        let span_decl_two = Span::new(6, 1, 6, 20);
        let span_call_one = Span::new(15, 1, 15, 20);
        let span_call_two = Span::new(16, 1, 16, 20);

        let optional_string = JavaType::Reference {
            name: "java.util.Optional".to_string(),
            generic_args: vec![JavaType::Reference {
                name: "java.lang.String".to_string(),
                generic_args: vec![],
            }],
        };
        let optional_integer = JavaType::Reference {
            name: "java.util.Optional".to_string(),
            generic_args: vec![JavaType::Reference {
                name: "java.lang.Integer".to_string(),
                generic_args: vec![],
            }],
        };

        context
            .method_declarations
            .push(RegisteredMethodDeclaration {
                owner: owner.clone(),
                name: "transform".to_string(),
                java_name: "transform".to_string(),
                parameter_types: vec![optional_string.clone()],
                return_type: JavaType::Reference {
                    name: "java.lang.String".to_string(),
                    generic_args: vec![],
                },
                is_static: true,
                primitive_return: None,
                span: span_decl_one.clone(),
            });
        context
            .method_declarations
            .push(RegisteredMethodDeclaration {
                owner: owner.clone(),
                name: "transform".to_string(),
                java_name: "transform".to_string(),
                parameter_types: vec![optional_integer.clone()],
                return_type: JavaType::Reference {
                    name: "java.lang.Integer".to_string(),
                    generic_args: vec![],
                },
                is_static: true,
                primitive_return: None,
                span: span_decl_two.clone(),
            });

        context.method_calls.push(RegisteredMethodCall {
            owner: owner.clone(),
            original_name: "transform".to_string(),
            java_name: "transform".to_string(),
            receiver_type: None,
            argument_types: vec![optional_string.clone()],
            return_type: JavaType::Reference {
                name: "java.lang.String".to_string(),
                generic_args: vec![],
            },
            argument_style: CallArgumentStyle::Comma,
            span: span_call_one.clone(),
        });
        context.method_calls.push(RegisteredMethodCall {
            owner: owner.clone(),
            original_name: "transform".to_string(),
            java_name: "transform".to_string(),
            receiver_type: None,
            argument_types: vec![optional_integer.clone()],
            return_type: JavaType::Reference {
                name: "java.lang.Integer".to_string(),
                generic_args: vec![],
            },
            argument_style: CallArgumentStyle::Comma,
            span: span_call_two.clone(),
        });

        let modifiers = |is_static| IrModifiers {
            is_static,
            ..IrModifiers::default()
        };

        let method_decl_one = IrStatement::MethodDeclaration {
            name: "transform".to_string(),
            java_name: None,
            type_parameters: vec![],
            parameters: vec![IrParameter {
                name: "value".to_string(),
                java_type: optional_string.clone(),
                modifiers: IrModifiers::default(),
                span: span_decl_one.clone(),
            }],
            primitive_return: None,
            return_type: JavaType::Reference {
                name: "java.lang.String".to_string(),
                generic_args: vec![],
            },
            body: None,
            modifiers: modifiers(true),
            throws: vec![],
            span: span_decl_one.clone(),
        };

        let method_decl_two = IrStatement::MethodDeclaration {
            name: "transform".to_string(),
            java_name: None,
            type_parameters: vec![],
            parameters: vec![IrParameter {
                name: "value".to_string(),
                java_type: optional_integer.clone(),
                modifiers: IrModifiers::default(),
                span: span_decl_two.clone(),
            }],
            primitive_return: None,
            return_type: JavaType::Reference {
                name: "java.lang.Integer".to_string(),
                generic_args: vec![],
            },
            body: None,
            modifiers: modifiers(true),
            throws: vec![],
            span: span_decl_two.clone(),
        };

        let call_expr_one = IrStatement::Expression {
            expr: IrExpression::MethodCall {
                receiver: None,
                method_name: "transform".to_string(),
                java_name: None,
                resolved_target: None,
                args: vec![IrExpression::Identifier {
                    name: "stringValue".to_string(),
                    java_type: optional_string.clone(),
                    span: span_call_one.clone(),
                }],
                argument_style: CallArgumentStyle::Comma,
                java_type: JavaType::Reference {
                    name: "java.lang.String".to_string(),
                    generic_args: vec![],
                },
                span: span_call_one.clone(),
            },
            span: span_call_one.clone(),
        };

        let call_expr_two = IrStatement::Expression {
            expr: IrExpression::MethodCall {
                receiver: None,
                method_name: "transform".to_string(),
                java_name: None,
                resolved_target: None,
                args: vec![IrExpression::Identifier {
                    name: "intValue".to_string(),
                    java_type: optional_integer.clone(),
                    span: span_call_two.clone(),
                }],
                argument_style: CallArgumentStyle::Comma,
                java_type: JavaType::Reference {
                    name: "java.lang.Integer".to_string(),
                    generic_args: vec![],
                },
                span: span_call_two.clone(),
            },
            span: span_call_two.clone(),
        };

        let mut statements_first = vec![
            method_decl_one.clone(),
            method_decl_two.clone(),
            call_expr_one.clone(),
            call_expr_two.clone(),
        ];
        let mut statements_second = vec![
            method_decl_one,
            method_decl_two,
            call_expr_one,
            call_expr_two,
        ];

        apply_method_erasure(&mut statements_first, &context);
        apply_method_erasure(&mut statements_second, &context);

        let extract_names = |stmts: &[IrStatement]| {
            let decl_names: Vec<String> = stmts
                .iter()
                .take(2)
                .map(|stmt| match stmt {
                    IrStatement::MethodDeclaration { java_name, .. } => java_name
                        .as_ref()
                        .expect("declaration should have java name")
                        .clone(),
                    other => panic!("unexpected statement variant: {other:?}"),
                })
                .collect();

            let call_names: Vec<String> = stmts
                .iter()
                .skip(2)
                .map(|stmt| match stmt {
                    IrStatement::Expression { expr, .. } => match expr {
                        IrExpression::MethodCall { java_name, .. } => java_name
                            .as_ref()
                            .expect("call should have java name")
                            .clone(),
                        other => panic!("unexpected expression variant: {other:?}"),
                    },
                    other => panic!("unexpected statement variant: {other:?}"),
                })
                .collect();

            (decl_names, call_names)
        };

        let (decl_first, call_first) = extract_names(&statements_first);
        let (decl_second, call_second) = extract_names(&statements_second);

        assert_eq!(decl_first[0], "transform");
        assert!(
            decl_first[1].starts_with("transform$"),
            "second overload should receive deterministic suffix, got {}",
            decl_first[1]
        );
        assert_ne!(
            decl_first[0], decl_first[1],
            "overloads must diverge after erasure"
        );
        assert_eq!(call_first[0], decl_first[0]);
        assert_eq!(call_first[1], decl_first[1]);
        assert_eq!(
            decl_first, decl_second,
            "rename must be deterministic across runs"
        );
        assert_eq!(
            call_first, call_second,
            "call rewriting must be deterministic"
        );
    }
}
