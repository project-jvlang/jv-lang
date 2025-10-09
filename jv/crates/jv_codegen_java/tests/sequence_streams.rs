use jv_ast::{BinaryOp, Span};
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
