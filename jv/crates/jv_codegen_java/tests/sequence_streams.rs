use jv_ast::{BinaryOp, Literal, Span};
use jv_codegen_java::{JavaCodeGenConfig, JavaCodeGenerator, JavaTarget};
use jv_ir::{
    IrExpression, JavaType, SequencePipeline, SequenceSource, SequenceStage, SequenceTerminal,
    SequenceTerminalEvaluation, SequenceTerminalKind,
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

fn number_literal(value: &str) -> IrExpression {
    IrExpression::Literal(Literal::Number(value.to_string()), dummy_span())
}

fn boolean_literal(value: bool) -> IrExpression {
    IrExpression::Literal(Literal::Boolean(value), dummy_span())
}

fn take_stage(value: &str) -> SequenceStage {
    SequenceStage::Take {
        count: Box::new(number_literal(value)),
        span: dummy_span(),
    }
}

fn drop_stage(value: &str) -> SequenceStage {
    SequenceStage::Drop {
        count: Box::new(number_literal(value)),
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
    IrExpression::SequencePipeline {
        pipeline: SequencePipeline {
            source,
            stages,
            terminal: Some(terminal),
            lazy: false,
            span: dummy_span(),
        },
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

    assert_eq!(rendered, "(numbers).stream().map(x -> x).toList()");
}

#[test]
fn java25_sequence_list_literal_source_streams_from_list_of() {
    let terminal = SequenceTerminal {
        kind: SequenceTerminalKind::ToList,
        evaluation: SequenceTerminalEvaluation::Collector,
        requires_non_empty_source: false,
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

    assert_eq!(rendered, "(List.of(1, 2, 3)).stream().map(x -> x).toList()",);
}

#[test]
fn java25_sequence_renders_full_stage_chain() {
    let terminal = SequenceTerminal {
        kind: SequenceTerminalKind::ToList,
        evaluation: SequenceTerminalEvaluation::Collector,
        requires_non_empty_source: false,
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

    assert_eq!(
        rendered,
        "(numbers).stream().map(x -> x).filter(x -> true).flatMap(x -> x).limit(3).skip(1).sorted().sorted(VALUE_COMPARATOR).toList()"
    );
}

#[test]
fn java21_sequence_to_list_falls_back_to_collectors() {
    let terminal = SequenceTerminal {
        kind: SequenceTerminalKind::ToList,
        evaluation: SequenceTerminalEvaluation::Collector,
        requires_non_empty_source: false,
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
        "(numbers).stream().map(x -> x).collect(Collectors.toList())"
    );
}

#[test]
fn java21_sequence_list_literal_source_uses_arrays_stream_fallback() {
    let terminal = SequenceTerminal {
        kind: SequenceTerminalKind::ToList,
        evaluation: SequenceTerminalEvaluation::Collector,
        requires_non_empty_source: false,
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
        "(Arrays.asList(1, 2, 3).stream().toList()).stream().map(x -> x).collect(Collectors.toList())",
    );
}

#[test]
fn java21_sequence_to_set_falls_back_to_collectors() {
    let terminal = SequenceTerminal {
        kind: SequenceTerminalKind::ToSet,
        evaluation: SequenceTerminalEvaluation::Collector,
        requires_non_empty_source: false,
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
        "(numbers).stream().map(x -> x).collect(Collectors.toSet())"
    );
}

#[test]
fn reduce_terminal_emits_illegal_argument_guard() {
    let terminal = SequenceTerminal {
        kind: SequenceTerminalKind::Reduce {
            accumulator: Box::new(reducer_lambda()),
        },
        evaluation: SequenceTerminalEvaluation::Reducer,
        requires_non_empty_source: true,
        span: dummy_span(),
    };

    let expr = build_pipeline(
        collection_source("numbers"),
        vec![],
        terminal,
        JavaType::object(),
    );

    let mut generator = JavaCodeGenerator::new();
    let rendered = generator
        .generate_expression(&expr)
        .expect("reduce pipeline should render");

    assert!(rendered.contains(
        ".orElseThrow(() -> new IllegalArgumentException(\"Sequence reduce() on empty source\"))"
    ));
}

#[test]
fn autocloseable_stream_pipeline_wraps_with_try_resource() {
    let terminal = SequenceTerminal {
        kind: SequenceTerminalKind::Count,
        evaluation: SequenceTerminalEvaluation::Aggregator,
        requires_non_empty_source: false,
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
    assert!(rendered.contains("return __jvStream.count();"));
}

#[test]
fn sum_terminal_emits_map_to_long_sum() {
    let terminal = SequenceTerminal {
        kind: SequenceTerminalKind::Sum,
        evaluation: SequenceTerminalEvaluation::Aggregator,
        requires_non_empty_source: false,
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
fn count_terminal_emits_stream_count_call() {
    let terminal = SequenceTerminal {
        kind: SequenceTerminalKind::Count,
        evaluation: SequenceTerminalEvaluation::Aggregator,
        requires_non_empty_source: false,
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

    assert_eq!(rendered, "(numbers).stream().map(x -> x).count()");
}
