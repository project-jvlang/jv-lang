use jv_ast::types::PrimitiveTypeName;
use jv_ast::{BinaryOp, Literal, Span};
use jv_codegen_java::{JavaCodeGenConfig, JavaCodeGenerator, JavaTarget};
use jv_ir::{
    IrExpression, IrResolvedMethodTarget, JavaType, PipelineShape, PrimitiveSpecializationHint,
    SequencePipeline, SequenceSource, SequenceStage, SequenceTerminal, SequenceTerminalEvaluation,
    SequenceTerminalKind,
};

fn dummy_span() -> Span {
    Span::dummy()
}

fn identifier(name: &str) -> IrExpression {
    IrExpression::Identifier {
        name: name.to_string(),
        java_type: JavaType::object(),
        span: dummy_span(),
    }
}

fn identity_lambda() -> IrExpression {
    IrExpression::Lambda {
        functional_interface: "java.util.function.Function".to_string(),
        param_names: vec!["x".to_string()],
        param_types: vec![JavaType::object()],
        body: Box::new(identifier("x")),
        java_type: JavaType::object(),
        span: dummy_span(),
    }
}

fn predicate_lambda() -> IrExpression {
    IrExpression::Lambda {
        functional_interface: "java.util.function.Predicate".to_string(),
        param_names: vec!["x".to_string()],
        param_types: vec![JavaType::object()],
        body: Box::new(boolean_literal(true)),
        java_type: JavaType::Primitive("boolean".to_string()),
        span: dummy_span(),
    }
}

fn reducer_lambda() -> IrExpression {
    IrExpression::Lambda {
        functional_interface: "java.util.function.BinaryOperator".to_string(),
        param_names: vec!["acc".to_string(), "value".to_string()],
        param_types: vec![JavaType::object(), JavaType::object()],
        body: Box::new(IrExpression::Binary {
            left: Box::new(identifier("acc")),
            op: BinaryOp::Add,
            right: Box::new(identifier("value")),
            java_type: JavaType::object(),
            span: dummy_span(),
        }),
        java_type: JavaType::object(),
        span: dummy_span(),
    }
}

fn map_stage() -> SequenceStage {
    SequenceStage::Map {
        lambda: Box::new(identity_lambda()),
        result_hint: None,
        span: dummy_span(),
    }
}

fn filter_stage() -> SequenceStage {
    SequenceStage::Filter {
        predicate: Box::new(predicate_lambda()),
        span: dummy_span(),
    }
}

fn flat_map_stage() -> SequenceStage {
    SequenceStage::FlatMap {
        lambda: Box::new(identity_lambda()),
        element_hint: None,
        flatten_depth: 1,
        span: dummy_span(),
    }
}

fn flat_map_list_of_stage() -> SequenceStage {
    let list_type = JavaType::Reference {
        name: "java.util.List".to_string(),
        generic_args: vec![JavaType::object()],
    };
    let body = IrExpression::MethodCall {
        receiver: None,
        method_name: "of".to_string(),
        java_name: None,
        resolved_target: Some(IrResolvedMethodTarget {
            owner: Some("java.util.List".to_string()),
            original_name: Some("of".to_string()),
            java_name: Some("of".to_string()),
            erased_parameters: vec!["Ljava/lang/Object;".to_string()],
        }),
        args: vec![IrExpression::Identifier {
            name: "value".to_string(),
            java_type: JavaType::object(),
            span: dummy_span(),
        }],
        argument_style: jv_ast::CallArgumentStyle::Comma,
        java_type: JavaType::object(),
        span: dummy_span(),
    };

    let lambda = IrExpression::Lambda {
        functional_interface: "java.util.function.Function".to_string(),
        param_names: vec!["value".to_string()],
        param_types: vec![JavaType::object()],
        body: Box::new(body),
        java_type: JavaType::object(),
        span: dummy_span(),
    };

    SequenceStage::FlatMap {
        lambda: Box::new(lambda),
        element_hint: Some(list_type),
        flatten_depth: 1,
        span: dummy_span(),
    }
}

fn flat_map_set_of_stage_without_hint() -> SequenceStage {
    let body = IrExpression::MethodCall {
        receiver: None,
        method_name: "of".to_string(),
        java_name: None,
        resolved_target: Some(IrResolvedMethodTarget {
            owner: Some("java.util.Set".to_string()),
            original_name: Some("of".to_string()),
            java_name: Some("of".to_string()),
            erased_parameters: vec!["Ljava/lang/Object;".to_string()],
        }),
        args: vec![IrExpression::Identifier {
            name: "value".to_string(),
            java_type: JavaType::object(),
            span: dummy_span(),
        }],
        argument_style: jv_ast::CallArgumentStyle::Comma,
        java_type: JavaType::object(),
        span: dummy_span(),
    };

    let lambda = IrExpression::Lambda {
        functional_interface: "java.util.function.Function".to_string(),
        param_names: vec!["value".to_string()],
        param_types: vec![JavaType::object()],
        body: Box::new(body),
        java_type: JavaType::object(),
        span: dummy_span(),
    };

    SequenceStage::FlatMap {
        lambda: Box::new(lambda),
        element_hint: None,
        flatten_depth: 1,
        span: dummy_span(),
    }
}

fn number_literal(value: &str) -> IrExpression {
    IrExpression::Literal(Literal::Number(value.to_string()), dummy_span())
}

fn cast_to_long(expr: IrExpression) -> IrExpression {
    let span = expr.span();
    IrExpression::Cast {
        expr: Box::new(expr),
        target_type: JavaType::Primitive("long".to_string()),
        span,
    }
}

fn boolean_literal(value: bool) -> IrExpression {
    IrExpression::Literal(Literal::Boolean(value), dummy_span())
}

fn stream_identifier(name: &str) -> IrExpression {
    IrExpression::Identifier {
        name: name.to_string(),
        java_type: JavaType::Reference {
            name: "java.util.stream.Stream".to_string(),
            generic_args: vec![JavaType::object()],
        },
        span: dummy_span(),
    }
}

fn collectors_to_list_expression() -> IrExpression {
    IrExpression::MethodCall {
        receiver: None,
        method_name: "toList".to_string(),
        java_name: Some("toList".to_string()),
        resolved_target: Some(IrResolvedMethodTarget {
            owner: Some("java.util.stream.Collectors".to_string()),
            original_name: Some("toList".to_string()),
            java_name: Some("toList".to_string()),
            erased_parameters: vec![],
        }),
        args: Vec::new(),
        argument_style: jv_ast::CallArgumentStyle::Comma,
        java_type: JavaType::object(),
        span: dummy_span(),
    }
}

#[test]
fn cast_expression_renders_reference_cast_short_name() {
    let cast = IrExpression::Cast {
        expr: Box::new(identifier("value")),
        target_type: JavaType::Reference {
            name: "Character".to_string(),
            generic_args: Vec::new(),
        },
        span: dummy_span(),
    };

    let mut generator = JavaCodeGenerator::new();
    let rendered = generator
        .generate_expression(&cast)
        .expect("cast expression renders");

    assert_eq!(rendered, "(Character) value");
}

#[test]
fn cast_expression_renders_reference_cast_fully_qualified() {
    let cast = IrExpression::Cast {
        expr: Box::new(identifier("value")),
        target_type: JavaType::Reference {
            name: "java.lang.Character".to_string(),
            generic_args: Vec::new(),
        },
        span: dummy_span(),
    };

    let mut generator = JavaCodeGenerator::new();
    let rendered = generator
        .generate_expression(&cast)
        .expect("cast expression renders");

    assert_eq!(rendered, "(java.lang.Character) value");
}

#[test]
fn cast_expression_renders_number_cast_short_name() {
    let cast = IrExpression::Cast {
        expr: Box::new(identifier("value")),
        target_type: JavaType::Reference {
            name: "Number".to_string(),
            generic_args: Vec::new(),
        },
        span: dummy_span(),
    };

    let mut generator = JavaCodeGenerator::new();
    let rendered = generator
        .generate_expression(&cast)
        .expect("cast expression renders");

    assert_eq!(rendered, "(Number) value");
}

#[test]
fn cast_expression_renders_number_cast_fully_qualified() {
    let cast = IrExpression::Cast {
        expr: Box::new(identifier("value")),
        target_type: JavaType::Reference {
            name: "java.lang.Number".to_string(),
            generic_args: Vec::new(),
        },
        span: dummy_span(),
    };

    let mut generator = JavaCodeGenerator::new();
    let rendered = generator
        .generate_expression(&cast)
        .expect("cast expression renders");

    assert_eq!(rendered, "(java.lang.Number) value");
}

fn take_stage(value: &str) -> SequenceStage {
    SequenceStage::Take {
        count: Box::new(cast_to_long(number_literal(value))),
        span: dummy_span(),
    }
}

fn drop_stage(value: &str) -> SequenceStage {
    SequenceStage::Drop {
        count: Box::new(cast_to_long(number_literal(value))),
        span: dummy_span(),
    }
}

fn sorted_stage() -> SequenceStage {
    SequenceStage::Sorted {
        comparator: None,
        span: dummy_span(),
    }
}

fn sorted_by_stage() -> SequenceStage {
    SequenceStage::Sorted {
        comparator: Some(Box::new(identifier("VALUE_COMPARATOR"))),
        span: dummy_span(),
    }
}

fn map_double_stage() -> SequenceStage {
    let lambda = IrExpression::Lambda {
        functional_interface: "java.util.function.Function".to_string(),
        param_names: vec!["value".to_string()],
        param_types: vec![JavaType::object()],
        body: Box::new(IrExpression::Binary {
            left: Box::new(identifier("value")),
            op: BinaryOp::Multiply,
            right: Box::new(number_literal("2")),
            java_type: JavaType::object(),
            span: dummy_span(),
        }),
        java_type: JavaType::object(),
        span: dummy_span(),
    };

    SequenceStage::Map {
        lambda: Box::new(lambda),
        result_hint: None,
        span: dummy_span(),
    }
}

fn filter_even_stage() -> SequenceStage {
    let modulo_expr = IrExpression::Binary {
        left: Box::new(identifier("candidate")),
        op: BinaryOp::Modulo,
        right: Box::new(number_literal("2")),
        java_type: JavaType::object(),
        span: dummy_span(),
    };
    let comparison = IrExpression::Binary {
        left: Box::new(modulo_expr),
        op: BinaryOp::Equal,
        right: Box::new(number_literal("0")),
        java_type: JavaType::Primitive("boolean".to_string()),
        span: dummy_span(),
    };
    let predicate = IrExpression::Lambda {
        functional_interface: "java.util.function.Predicate".to_string(),
        param_names: vec!["candidate".to_string()],
        param_types: vec![JavaType::object()],
        body: Box::new(comparison),
        java_type: JavaType::Primitive("boolean".to_string()),
        span: dummy_span(),
    };

    SequenceStage::Filter {
        predicate: Box::new(predicate),
        span: dummy_span(),
    }
}

fn string_concatenation_map_stage() -> SequenceStage {
    let suffix = IrExpression::Literal(Literal::String("-guide".to_string()), dummy_span());
    let concatenation = IrExpression::Binary {
        left: Box::new(identifier("value")),
        op: BinaryOp::Add,
        right: Box::new(suffix),
        java_type: JavaType::object(),
        span: dummy_span(),
    };

    let lambda = IrExpression::Lambda {
        functional_interface: "java.util.function.Function".to_string(),
        param_names: vec!["value".to_string()],
        param_types: vec![JavaType::object()],
        body: Box::new(concatenation),
        java_type: JavaType::object(),
        span: dummy_span(),
    };

    SequenceStage::Map {
        lambda: Box::new(lambda),
        result_hint: None,
        span: dummy_span(),
    }
}

fn string_length_filter_stage() -> SequenceStage {
    let length_access = IrExpression::FieldAccess {
        receiver: Box::new(identifier("candidate")),
        field_name: "length".to_string(),
        java_type: JavaType::Primitive("int".to_string()),
        span: dummy_span(),
        is_record_component: false,
    };
    let comparison = IrExpression::Binary {
        left: Box::new(length_access),
        op: BinaryOp::Greater,
        right: Box::new(number_literal("6")),
        java_type: JavaType::Primitive("boolean".to_string()),
        span: dummy_span(),
    };
    let predicate = IrExpression::Lambda {
        functional_interface: "java.util.function.Predicate".to_string(),
        param_names: vec!["candidate".to_string()],
        param_types: vec![JavaType::object()],
        body: Box::new(comparison),
        java_type: JavaType::Primitive("boolean".to_string()),
        span: dummy_span(),
    };

    SequenceStage::Filter {
        predicate: Box::new(predicate),
        span: dummy_span(),
    }
}

fn collection_source(name: &str) -> SequenceSource {
    SequenceSource::Collection {
        expr: Box::new(identifier(name)),
        element_hint: None,
    }
}

fn java_stream_source(name: &str, auto_close: bool) -> SequenceSource {
    SequenceSource::JavaStream {
        expr: Box::new(identifier(name)),
        element_hint: None,
        auto_close,
    }
}

fn list_literal_source(values: &[&str]) -> SequenceSource {
    let elements = values.iter().map(|value| number_literal(value)).collect();

    SequenceSource::ListLiteral {
        elements,
        element_hint: None,
        span: dummy_span(),
    }
}

fn build_pipeline(
    source: SequenceSource,
    stages: Vec<SequenceStage>,
    terminal: SequenceTerminal,
    result_type: JavaType,
) -> IrExpression {
    let mut pipeline = SequencePipeline {
        source,
        stages,
        terminal: Some(terminal),
        lazy: false,
        span: dummy_span(),
        shape: PipelineShape::default(),
    };
    pipeline.recompute_shape();

    IrExpression::SequencePipeline {
        pipeline,
        java_type: result_type,
        span: dummy_span(),
    }
}

#[test]
fn java25_sequence_to_list_uses_stream_to_list() {
    let terminal = SequenceTerminal {
        kind: SequenceTerminalKind::ToList,
        evaluation: SequenceTerminalEvaluation::Collector,
        requires_non_empty_source: false,
        specialization_hint: None,
        canonical_adapter: None,
        span: dummy_span(),
    };

    let expr = build_pipeline(
        collection_source("numbers"),
        vec![map_stage()],
        terminal,
        JavaType::Reference {
            name: "java.util.List".to_string(),
            generic_args: vec![],
        },
    );

    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let rendered = generator
        .generate_expression(&expr)
        .expect("sequence pipeline should render for Java 25");

    assert_eq!(rendered, "(numbers).stream().map((x) -> x).toList()");
}

#[test]
fn java25_sequence_list_literal_source_streams_from_list_of() {
    let terminal = SequenceTerminal {
        kind: SequenceTerminalKind::ToList,
        evaluation: SequenceTerminalEvaluation::Collector,
        requires_non_empty_source: false,
        specialization_hint: None,
        canonical_adapter: None,
        span: dummy_span(),
    };

    let expr = build_pipeline(
        list_literal_source(&["1", "2", "3"]),
        vec![map_stage()],
        terminal,
        JavaType::Reference {
            name: "java.util.List".to_string(),
            generic_args: vec![],
        },
    );

    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let rendered = generator
        .generate_expression(&expr)
        .expect("list literal sequence should render for Java 25");

    assert_eq!(
        rendered,
        "(List.of(1, 2, 3)).stream().map((x) -> x).toList()",
    );
}

#[test]
fn java25_sequence_flat_map_wraps_iterable_results_into_stream() {
    let terminal = SequenceTerminal {
        kind: SequenceTerminalKind::ToList,
        evaluation: SequenceTerminalEvaluation::Collector,
        requires_non_empty_source: false,
        specialization_hint: None,
        canonical_adapter: None,
        span: dummy_span(),
    };

    let expr = build_pipeline(
        collection_source("numbers"),
        vec![flat_map_list_of_stage()],
        terminal,
        JavaType::Reference {
            name: "java.util.List".to_string(),
            generic_args: vec![],
        },
    );

    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let rendered = generator
        .generate_expression(&expr)
        .expect("flatMap pipeline should render for Java 25");

    assert!(
        rendered.contains("Sequence.toStream(List.of(value))"),
        "expected Sequence.toStream wrapper in flatMap lambda, got {rendered}"
    );
}

#[test]
fn java25_sequence_flat_map_uses_resolved_owner_fallback_for_set_of() {
    let terminal = SequenceTerminal {
        kind: SequenceTerminalKind::ToList,
        evaluation: SequenceTerminalEvaluation::Collector,
        requires_non_empty_source: false,
        specialization_hint: None,
        canonical_adapter: None,
        span: dummy_span(),
    };

    let expr = build_pipeline(
        collection_source("numbers"),
        vec![flat_map_set_of_stage_without_hint()],
        terminal,
        JavaType::Reference {
            name: "java.util.List".to_string(),
            generic_args: vec![],
        },
    );

    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let rendered = generator
        .generate_expression(&expr)
        .expect("flatMap pipeline with Set.of should render for Java 25");

    assert!(
        rendered.contains("Sequence.toStream(Set.of(value))"),
        "expected Sequence.toStream wrapper to use resolved owner fallback, got {rendered}"
    );
}

#[test]
fn java25_sequence_renders_full_stage_chain() {
    let terminal = SequenceTerminal {
        kind: SequenceTerminalKind::ToList,
        evaluation: SequenceTerminalEvaluation::Collector,
        requires_non_empty_source: false,
        specialization_hint: None,
        canonical_adapter: None,
        span: dummy_span(),
    };

    let expr = build_pipeline(
        collection_source("numbers"),
        vec![
            map_stage(),
            filter_stage(),
            flat_map_stage(),
            take_stage("3"),
            drop_stage("1"),
            sorted_stage(),
            sorted_by_stage(),
        ],
        terminal,
        JavaType::Reference {
            name: "java.util.List".to_string(),
            generic_args: vec![],
        },
    );

    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let rendered = generator
        .generate_expression(&expr)
        .expect("complex sequence chain renders for Java 25");

    let expected = "(numbers).stream().map((x) -> x).filter((x) -> true).flatMap((x) -> x).limit((long) 3).skip((long) 1).sorted().sorted(VALUE_COMPARATOR).toList()";
    assert_eq!(rendered, expected);
}

#[test]
fn lazy_sequence_pipeline_produces_sequence_wrapper() {
    let mut pipeline = SequencePipeline {
        source: collection_source("numbers"),
        stages: vec![map_stage()],
        terminal: None,
        lazy: true,
        span: dummy_span(),
        shape: PipelineShape::default(),
    };
    pipeline.recompute_shape();

    let expr = IrExpression::SequencePipeline {
        pipeline,
        java_type: JavaType::sequence(),
        span: dummy_span(),
    };

    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let rendered = generator
        .generate_expression(&expr)
        .expect("lazy pipeline should render");

    assert_eq!(
        rendered,
        "Sequence.sequenceFromStream((numbers).stream().map((x) -> x))"
    );
}

#[test]
fn java21_sequence_to_list_falls_back_to_collectors() {
    let terminal = SequenceTerminal {
        kind: SequenceTerminalKind::ToList,
        evaluation: SequenceTerminalEvaluation::Collector,
        requires_non_empty_source: false,
        specialization_hint: None,
        canonical_adapter: None,
        span: dummy_span(),
    };

    let expr = build_pipeline(
        collection_source("numbers"),
        vec![map_stage()],
        terminal,
        JavaType::Reference {
            name: "java.util.List".to_string(),
            generic_args: vec![],
        },
    );

    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java21));
    let rendered = generator
        .generate_expression(&expr)
        .expect("sequence pipeline should render for Java 21");

    assert_eq!(
        rendered,
        "(numbers).stream().map((x) -> x).collect(Collectors.toList())"
    );
}

#[test]
fn java21_sequence_list_literal_source_uses_arrays_stream_fallback() {
    let terminal = SequenceTerminal {
        kind: SequenceTerminalKind::ToList,
        evaluation: SequenceTerminalEvaluation::Collector,
        requires_non_empty_source: false,
        specialization_hint: None,
        canonical_adapter: None,
        span: dummy_span(),
    };

    let expr = build_pipeline(
        list_literal_source(&["1", "2", "3"]),
        vec![map_stage()],
        terminal,
        JavaType::Reference {
            name: "java.util.List".to_string(),
            generic_args: vec![],
        },
    );

    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java21));
    let rendered = generator
        .generate_expression(&expr)
        .expect("list literal sequence should render for Java 21");

    assert_eq!(
        rendered,
        "(Arrays.asList(1, 2, 3).stream().toList()).stream().map((x) -> x).collect(Collectors.toList())",
    );
}

#[test]
fn java21_sequence_to_set_falls_back_to_collectors() {
    let terminal = SequenceTerminal {
        kind: SequenceTerminalKind::ToSet,
        evaluation: SequenceTerminalEvaluation::Collector,
        requires_non_empty_source: false,
        specialization_hint: None,
        canonical_adapter: None,
        span: dummy_span(),
    };

    let expr = build_pipeline(
        collection_source("numbers"),
        vec![map_stage()],
        terminal,
        JavaType::Reference {
            name: "java.util.Set".to_string(),
            generic_args: vec![],
        },
    );

    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java21));
    let rendered = generator
        .generate_expression(&expr)
        .expect("sequence pipeline should render for Java 21");

    assert_eq!(
        rendered,
        "(numbers).stream().map((x) -> x).collect(Collectors.toSet())"
    );
}

#[test]
fn stream_collect_to_list_prefers_tolist_in_java25() {
    let expr = IrExpression::MethodCall {
        receiver: Some(Box::new(stream_identifier("stream"))),
        method_name: "collect".to_string(),
        java_name: Some("collect".to_string()),
        resolved_target: None,
        args: vec![collectors_to_list_expression()],
        argument_style: jv_ast::CallArgumentStyle::Comma,
        java_type: JavaType::list(),
        span: dummy_span(),
    };

    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let rendered = generator
        .generate_expression(&expr)
        .expect("collect should render");

    assert_eq!(rendered, "stream.toList()");
}

#[test]
fn stream_collect_to_list_keeps_collectors_in_java21() {
    let expr = IrExpression::MethodCall {
        receiver: Some(Box::new(stream_identifier("stream"))),
        method_name: "collect".to_string(),
        java_name: Some("collect".to_string()),
        resolved_target: None,
        args: vec![collectors_to_list_expression()],
        argument_style: jv_ast::CallArgumentStyle::Comma,
        java_type: JavaType::list(),
        span: dummy_span(),
    };

    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java21));
    let rendered = generator
        .generate_expression(&expr)
        .expect("collect should render");

    assert_eq!(rendered, "stream.collect(Collectors.toList())");
}

#[test]
fn map_filter_pipeline_renders_expected_lambdas() {
    let terminal = SequenceTerminal {
        kind: SequenceTerminalKind::ToList,
        evaluation: SequenceTerminalEvaluation::Collector,
        requires_non_empty_source: false,
        specialization_hint: None,
        canonical_adapter: None,
        span: dummy_span(),
    };

    let stages = vec![map_double_stage(), filter_even_stage()];

    let expr_java25 = build_pipeline(
        collection_source("numbers"),
        stages.clone(),
        terminal.clone(),
        JavaType::list(),
    );

    let mut generator25 =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let rendered25 = generator25
        .generate_expression(&expr_java25)
        .expect("map/filter pipeline renders for Java 25");

    assert!(
        rendered25.contains("map((value) -> value * 2)"),
        "expected Java 25 pipeline to include doubled map lambda, got: {rendered25}"
    );
    assert!(
        rendered25.contains("filter((candidate) -> candidate % 2 == 0)"),
        "expected Java 25 pipeline to include even filter lambda, got: {rendered25}"
    );
    assert!(
        rendered25.contains(".toList()"),
        "expected Java 25 pipeline to collect to list, got: {rendered25}"
    );
    assert!(
        !rendered25.contains("JvSequence"),
        "expected Java 25 pipeline to avoid helper sequence, got: {rendered25}"
    );

    let expr_java21 = build_pipeline(
        collection_source("numbers"),
        stages,
        terminal,
        JavaType::list(),
    );

    let mut generator21 =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java21));
    let rendered21 = generator21
        .generate_expression(&expr_java21)
        .expect("map/filter pipeline renders for Java 21");

    assert!(
        rendered21.contains("map((value) -> value * 2)"),
        "expected Java 21 pipeline to include doubled map lambda, got: {rendered21}"
    );
    assert!(
        rendered21.contains("filter((candidate) -> candidate % 2 == 0)"),
        "expected Java 21 pipeline to include even filter lambda, got: {rendered21}"
    );
    assert!(
        rendered21.contains("Collectors.toList()"),
        "expected Java 21 pipeline to collect to list, got: {rendered21}"
    );
}

#[test]
fn string_length_filter_prefers_accessor_call() {
    let terminal = SequenceTerminal {
        kind: SequenceTerminalKind::ToList,
        evaluation: SequenceTerminalEvaluation::Collector,
        requires_non_empty_source: false,
        specialization_hint: None,
        canonical_adapter: None,
        span: dummy_span(),
    };

    let expr = build_pipeline(
        collection_source("chapters"),
        vec![
            string_concatenation_map_stage(),
            string_length_filter_stage(),
        ],
        terminal,
        JavaType::list(),
    );

    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let rendered = generator
        .generate_expression(&expr)
        .expect("string length filter pipeline renders for Java 25");

    assert!(
        rendered.contains("candidate.length()"),
        "expected filter to call String::length accessor, got: {rendered}"
    );
    assert!(
        !rendered.contains("candidate.length >"),
        "filter should not use field-style access for String length: {rendered}"
    );
}

#[test]
fn reduce_terminal_returns_optional_without_guard() {
    let terminal = SequenceTerminal {
        kind: SequenceTerminalKind::Reduce {
            accumulator: Box::new(reducer_lambda()),
        },
        evaluation: SequenceTerminalEvaluation::Reducer,
        requires_non_empty_source: false,
        specialization_hint: None,
        canonical_adapter: None,
        span: dummy_span(),
    };

    let expr = build_pipeline(
        collection_source("numbers"),
        vec![],
        terminal,
        JavaType::Reference {
            name: "java.util.Optional".to_string(),
            generic_args: vec![JavaType::object()],
        },
    );

    let mut generator = JavaCodeGenerator::new();
    let rendered = generator
        .generate_expression(&expr)
        .expect("reduce pipeline should render");

    assert!(rendered.contains(".reduce("));
    assert!(!rendered.contains("orElseThrow"));
}

#[test]
fn autocloseable_stream_pipeline_wraps_with_try_resource() {
    let terminal = SequenceTerminal {
        kind: SequenceTerminalKind::Count,
        evaluation: SequenceTerminalEvaluation::Aggregator,
        requires_non_empty_source: false,
        specialization_hint: None,
        canonical_adapter: None,
        span: dummy_span(),
    };

    let expr = build_pipeline(
        java_stream_source("resourceStream", true),
        vec![],
        terminal,
        JavaType::Primitive("long".to_string()),
    );

    let mut generator = JavaCodeGenerator::new();
    let rendered = generator
        .generate_expression(&expr)
        .expect("auto-closeable pipeline should render");

    assert!(rendered.starts_with("new Object()"));
    assert!(rendered.contains("try (var __jvStream = resourceStream)"));
    assert!(!rendered.contains("JvSequence"));
    assert!(rendered.contains("return __jvStream.count();"));
}

#[test]
fn string_format_arguments_materialize_sequences() {
    let terminal = SequenceTerminal {
        kind: SequenceTerminalKind::ToList,
        evaluation: SequenceTerminalEvaluation::Collector,
        requires_non_empty_source: false,
        specialization_hint: None,
        canonical_adapter: None,
        span: dummy_span(),
    };

    let format_expr_java25 = IrExpression::StringFormat {
        format_string: "inline=%s".to_string(),
        args: vec![build_pipeline(
            collection_source("numbers"),
            vec![map_double_stage()],
            terminal.clone(),
            JavaType::list(),
        )],
        span: dummy_span(),
    };

    let mut generator25 =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let rendered25 = generator25
        .generate_expression(&format_expr_java25)
        .expect("string format pipeline renders for Java 25");

    assert_eq!(
        rendered25,
        "String.format(\"inline=%s\", (numbers).stream().map((value) -> value * 2).toList())"
    );

    let format_expr_java21 = IrExpression::StringFormat {
        format_string: "inline=%s".to_string(),
        args: vec![build_pipeline(
            collection_source("numbers"),
            vec![map_double_stage()],
            terminal,
            JavaType::list(),
        )],
        span: dummy_span(),
    };

    let mut generator21 =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java21));
    let rendered21 = generator21
        .generate_expression(&format_expr_java21)
        .expect("string format pipeline renders for Java 21");

    assert_eq!(
        rendered21,
        "String.format(\"inline=%s\", (numbers).stream().map((value) -> value * 2).collect(Collectors.toList()))"
    );
}

#[test]
fn sum_terminal_emits_map_to_long_sum() {
    let terminal = SequenceTerminal {
        kind: SequenceTerminalKind::Sum,
        evaluation: SequenceTerminalEvaluation::Aggregator,
        requires_non_empty_source: false,
        specialization_hint: None,
        canonical_adapter: None,
        span: dummy_span(),
    };

    let expr = build_pipeline(
        collection_source("numbers"),
        vec![],
        terminal,
        JavaType::Primitive("long".to_string()),
    );

    let mut generator = JavaCodeGenerator::new();
    let rendered = generator
        .generate_expression(&expr)
        .expect("sum pipeline renders");

    assert_eq!(
        rendered,
        "(numbers).stream().mapToLong(value -> ((Number) value).longValue()).sum()"
    );
}

#[test]
fn sum_terminal_with_int_hint_uses_map_to_int() {
    let mut pipeline = SequencePipeline {
        source: collection_source("numbers"),
        stages: Vec::new(),
        terminal: Some(SequenceTerminal {
            kind: SequenceTerminalKind::Sum,
            evaluation: SequenceTerminalEvaluation::Aggregator,
            requires_non_empty_source: false,
            specialization_hint: Some(PrimitiveSpecializationHint {
                type_param: "T".to_string(),
                canonical: PrimitiveTypeName::Int,
                aliases: vec![PrimitiveTypeName::Char, PrimitiveTypeName::Short],
                span: dummy_span(),
            }),
            canonical_adapter: None,
            span: dummy_span(),
        }),
        lazy: false,
        span: dummy_span(),
        shape: PipelineShape::default(),
    };
    pipeline.recompute_shape();
    pipeline.apply_specialization_hint();

    let expr = IrExpression::SequencePipeline {
        pipeline,
        java_type: JavaType::Primitive("int".to_string()),
        span: dummy_span(),
    };

    let mut generator = JavaCodeGenerator::new();
    let rendered = generator
        .generate_expression(&expr)
        .expect("specialized sum pipeline renders");

    assert!(rendered.contains(".mapToInt("));
    assert!(rendered.ends_with(".sum()"));
    assert!(rendered.contains("Character"));
    assert!(
        rendered.contains("(java.lang.Character) __jvIntFamilyValue"),
        "lambda should cast Character aliases explicitly. Rendered expression: {}",
        rendered
    );
    assert!(
        rendered.contains("(int) (java.lang.Character)"),
        "lambda should cast Character aliases to int. Rendered expression: {}",
        rendered
    );
    assert!(
        rendered.contains("instanceof java.lang.Character"),
        "lambda should guard Character instances"
    );
    assert!(
        rendered.contains(".intValue()"),
        "lambda should fall back to Number::intValue for non-Character aliases"
    );
    assert!(
        rendered.contains("(java.lang.Number) __jvIntFamilyValue"),
        "lambda should cast remaining aliases to Number. Rendered expression: {}",
        rendered
    );
}

#[test]
fn count_terminal_emits_stream_count_call() {
    let terminal = SequenceTerminal {
        kind: SequenceTerminalKind::Count,
        evaluation: SequenceTerminalEvaluation::Aggregator,
        requires_non_empty_source: false,
        specialization_hint: None,
        canonical_adapter: None,
        span: dummy_span(),
    };

    let expr = build_pipeline(
        collection_source("numbers"),
        vec![map_stage()],
        terminal,
        JavaType::Primitive("long".to_string()),
    );

    let mut generator = JavaCodeGenerator::new();
    let rendered = generator
        .generate_expression(&expr)
        .expect("count pipeline renders");

    assert_eq!(rendered, "(numbers).stream().map((x) -> x).count()");
}
