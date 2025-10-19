use jv_ast::{BinaryOp, CallArgumentStyle, Literal, Span};
use jv_codegen_java::{JavaCodeGenConfig, JavaCodeGenerator, JavaTarget};
use jv_ir::{
    IrExpression, JavaType, PipelineShape, SequencePipeline, SequenceSource, SequenceStage,
    SequenceTerminal, SequenceTerminalEvaluation, SequenceTerminalKind,
};

#[derive(Debug)]
pub struct SequenceGoldenCase {
    pub name: &'static str,
    pub target: JavaTarget,
    pub expression: IrExpression,
    pub expected: &'static str,
}

pub fn run_case(case: SequenceGoldenCase) {
    let SequenceGoldenCase {
        name,
        target,
        expression,
        expected,
    } = case;

    let mut generator = JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(target));
    let rendered = generator
        .generate_expression(&expression)
        .unwrap_or_else(|err| panic!("failed to render {name}: {err}"));

    let actual = rendered.trim_end_matches('\n');
    let expected = expected.trim_end_matches('\n');

    assert_eq!(actual, expected, "golden mismatch for {name}");
}

pub(crate) fn collection_source(name: &str) -> SequenceSource {
    SequenceSource::Collection {
        expr: Box::new(identifier(name)),
        element_hint: None,
    }
}

pub(crate) fn sequence_factory_stream_source(name: &str) -> SequenceSource {
    let call = IrExpression::MethodCall {
        receiver: Some(Box::new(IrExpression::Identifier {
            name: "Sequence".to_string(),
            java_type: JavaType::Reference {
                name: "jv.collections.Sequence".to_string(),
                generic_args: vec![],
            },
            span: dummy_span(),
        })),
        method_name: "sequenceFromIterable".to_string(),
        java_name: None,
        resolved_target: None,
        args: vec![identifier(name)],
        argument_style: CallArgumentStyle::Comma,
        java_type: JavaType::Reference {
            name: "java.util.stream.Stream".to_string(),
            generic_args: vec![],
        },
        span: dummy_span(),
    };

    SequenceSource::JavaStream {
        expr: Box::new(call),
        element_hint: None,
        auto_close: false,
    }
}

pub(crate) fn map_double_stage() -> SequenceStage {
    SequenceStage::Map {
        lambda: Box::new(IrExpression::Lambda {
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
        }),
        result_hint: None,
        span: dummy_span(),
    }
}

pub(crate) fn map_identity_stage() -> SequenceStage {
    SequenceStage::Map {
        lambda: Box::new(IrExpression::Lambda {
            functional_interface: "java.util.function.Function".to_string(),
            param_names: vec!["value".to_string()],
            param_types: vec![JavaType::object()],
            body: Box::new(identifier("value")),
            java_type: JavaType::object(),
            span: dummy_span(),
        }),
        result_hint: None,
        span: dummy_span(),
    }
}

pub(crate) fn filter_true_stage() -> SequenceStage {
    SequenceStage::Filter {
        predicate: Box::new(IrExpression::Lambda {
            functional_interface: "java.util.function.Predicate".to_string(),
            param_names: vec!["value".to_string()],
            param_types: vec![JavaType::object()],
            body: Box::new(boolean_literal(true)),
            java_type: JavaType::Primitive("boolean".to_string()),
            span: dummy_span(),
        }),
        span: dummy_span(),
    }
}

pub(crate) fn reduce_terminal() -> SequenceTerminal {
    SequenceTerminal {
        kind: SequenceTerminalKind::Reduce {
            accumulator: Box::new(reduce_lambda()),
        },
        evaluation: SequenceTerminalEvaluation::Reducer,
        requires_non_empty_source: false,
        specialization_hint: None,
        canonical_adapter: None,
        span: dummy_span(),
    }
}

pub(crate) fn to_list_terminal() -> SequenceTerminal {
    SequenceTerminal {
        kind: SequenceTerminalKind::ToList,
        evaluation: SequenceTerminalEvaluation::Collector,
        requires_non_empty_source: false,
        specialization_hint: None,
        canonical_adapter: None,
        span: dummy_span(),
    }
}

pub(crate) fn java_list_type() -> JavaType {
    JavaType::Reference {
        name: "java.util.List".to_string(),
        generic_args: vec![],
    }
}

pub(crate) fn build_pipeline_expression(
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

pub(crate) fn dummy_span() -> Span {
    Span::dummy()
}

fn identifier(name: &str) -> IrExpression {
    IrExpression::Identifier {
        name: name.to_string(),
        java_type: JavaType::object(),
        span: dummy_span(),
    }
}

fn number_literal(value: &str) -> IrExpression {
    IrExpression::Literal(Literal::Number(value.to_string()), dummy_span())
}

fn boolean_literal(value: bool) -> IrExpression {
    IrExpression::Literal(Literal::Boolean(value), dummy_span())
}

fn reduce_lambda() -> IrExpression {
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

pub mod sequence_helper_from_factory;
pub mod sequence_helper_multistage;
pub mod sequence_inline_map;
pub mod sequence_inline_reduce;
