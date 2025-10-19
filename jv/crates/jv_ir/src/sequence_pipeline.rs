use crate::context::TransformContext;
use crate::error::TransformError;
use crate::transform::{extract_java_type, transform_expression};
use crate::types::{IrExpression, JavaType};
use jv_ast::{
    types::PrimitiveTypeName, Argument, CallArgumentMetadata, CallArgumentStyle, Expression,
    SequenceDelimiter, Span,
};
use serde::{Deserialize, Serialize};
use std::mem;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum PipelineShape {
    SingleStageMap,
    SingleStageFilter,
    SingleStageReduce,
    MultiStage {
        stages: usize,
        repeated_transforms: bool,
        has_terminal: bool,
    },
    ExplicitSequenceSource,
}

impl Default for PipelineShape {
    fn default() -> Self {
        Self::MultiStage {
            stages: 0,
            repeated_transforms: false,
            has_terminal: false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SequencePipeline {
    pub source: SequenceSource,
    pub stages: Vec<SequenceStage>,
    pub terminal: Option<SequenceTerminal>,
    pub lazy: bool,
    pub span: Span,
    pub shape: PipelineShape,
}

impl SequencePipeline {
    pub fn recompute_shape(&mut self) {
        self.shape = PipelineShape::classify(&self.source, &self.stages, self.terminal.as_ref());
    }

    pub fn apply_specialization_hint(&mut self) {
        let Some(terminal) = self.terminal.as_mut() else {
            return;
        };

        let Some(hint) = terminal.specialization_hint.as_ref() else {
            return;
        };

        if terminal.canonical_adapter.is_some() {
            return;
        }

        match hint.canonical {
            PrimitiveTypeName::Int => {
                let adapter = build_int_canonical_adapter(&hint.span, &hint.aliases);
                terminal.canonical_adapter = Some(Box::new(adapter));
            }
            _ => {}
        }
    }
}

fn build_int_canonical_adapter(span: &Span, aliases: &[PrimitiveTypeName]) -> IrExpression {
    let param_name = "__jvIntFamilyValue".to_string();
    let lambda_param_types = vec![JavaType::object()];

    let identifier = || IrExpression::Identifier {
        name: param_name.clone(),
        java_type: JavaType::object(),
        span: span.clone(),
    };

    let number_cast = |value: IrExpression| IrExpression::Cast {
        expr: Box::new(value),
        target_type: JavaType::Reference {
            name: "java.lang.Number".to_string(),
            generic_args: vec![],
        },
        span: span.clone(),
    };

    let number_int_value = IrExpression::MethodCall {
        receiver: Some(Box::new(number_cast(identifier()))),
        method_name: "intValue".to_string(),
        java_name: Some("intValue".to_string()),
        resolved_target: None,
        args: Vec::new(),
        argument_style: CallArgumentStyle::Comma,
        java_type: JavaType::int(),
        span: span.clone(),
    };

    let body = if aliases
        .iter()
        .any(|alias| matches!(alias, PrimitiveTypeName::Char))
    {
        let char_cast = |value: IrExpression| IrExpression::Cast {
            expr: Box::new(value),
            target_type: JavaType::Reference {
                name: "java.lang.Character".to_string(),
                generic_args: vec![],
            },
            span: span.clone(),
        };

        let char_value = IrExpression::MethodCall {
            receiver: Some(Box::new(char_cast(identifier()))),
            method_name: "charValue".to_string(),
            java_name: Some("charValue".to_string()),
            resolved_target: None,
            args: Vec::new(),
            argument_style: CallArgumentStyle::Comma,
            java_type: JavaType::Primitive("char".to_string()),
            span: span.clone(),
        };

        let condition = IrExpression::InstanceOf {
            expr: Box::new(identifier()),
            target_type: JavaType::Reference {
                name: "java.lang.Character".to_string(),
                generic_args: vec![],
            },
            span: span.clone(),
        };

        let char_value_cast = IrExpression::Cast {
            expr: Box::new(char_value),
            target_type: JavaType::int(),
            span: span.clone(),
        };

        IrExpression::Conditional {
            condition: Box::new(condition),
            then_expr: Box::new(char_value_cast),
            else_expr: Box::new(number_int_value),
            java_type: JavaType::int(),
            span: span.clone(),
        }
    } else {
        number_int_value
    };

    IrExpression::Lambda {
        functional_interface: "java.util.function.ToIntFunction".to_string(),
        param_names: vec![param_name],
        param_types: lambda_param_types.clone(),
        body: Box::new(body),
        java_type: JavaType::Functional {
            interface_name: "java.util.function.ToIntFunction".to_string(),
            param_types: lambda_param_types,
            return_type: Box::new(JavaType::int()),
        },
        span: span.clone(),
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SequenceSource {
    Collection {
        expr: Box<IrExpression>,
        element_hint: Option<JavaType>,
    },
    Array {
        expr: Box<IrExpression>,
        element_hint: Option<JavaType>,
        dimensions: usize,
    },
    ListLiteral {
        elements: Vec<IrExpression>,
        element_hint: Option<JavaType>,
        span: Span,
    },
    JavaStream {
        expr: Box<IrExpression>,
        element_hint: Option<JavaType>,
        auto_close: bool,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SequenceStage {
    Map {
        lambda: Box<IrExpression>,
        result_hint: Option<JavaType>,
        span: Span,
    },
    Filter {
        predicate: Box<IrExpression>,
        span: Span,
    },
    FlatMap {
        lambda: Box<IrExpression>,
        element_hint: Option<JavaType>,
        flatten_depth: usize,
        span: Span,
    },
    Take {
        count: Box<IrExpression>,
        span: Span,
    },
    Drop {
        count: Box<IrExpression>,
        span: Span,
    },
    Sorted {
        comparator: Option<Box<IrExpression>>,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SequenceTerminal {
    pub kind: SequenceTerminalKind,
    pub evaluation: SequenceTerminalEvaluation,
    /// Whether the terminal requires a non-empty source for safe evaluation.
    pub requires_non_empty_source: bool,
    #[serde(default)]
    pub specialization_hint: Option<crate::types::PrimitiveSpecializationHint>,
    #[serde(default)]
    pub canonical_adapter: Option<Box<IrExpression>>,
    pub span: Span,
}

impl SequenceTerminal {
    fn new(
        kind: SequenceTerminalKind,
        evaluation: SequenceTerminalEvaluation,
        requires_non_empty_source: bool,
        span: Span,
    ) -> Self {
        Self {
            kind,
            evaluation,
            requires_non_empty_source,
            specialization_hint: None,
            canonical_adapter: None,
            span,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum SequenceTerminalEvaluation {
    Collector,
    Reducer,
    Aggregator,
    Consumer,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SequenceTerminalKind {
    ToList,
    ToSet,
    Fold {
        initial: Box<IrExpression>,
        accumulator: Box<IrExpression>,
    },
    Reduce {
        accumulator: Box<IrExpression>,
    },
    GroupBy {
        key_selector: Box<IrExpression>,
    },
    Associate {
        pair_selector: Box<IrExpression>,
    },
    Count,
    Sum,
    ForEach {
        action: Box<IrExpression>,
    },
}

impl PipelineShape {
    fn classify(
        source: &SequenceSource,
        stages: &[SequenceStage],
        terminal: Option<&SequenceTerminal>,
    ) -> Self {
        if matches!(source, SequenceSource::JavaStream { .. }) {
            return PipelineShape::ExplicitSequenceSource;
        }

        let has_terminal = terminal.is_some();
        let stage_count = stages.len();

        if stage_count == 0 {
            if terminal.is_some_and(|t| matches!(t.kind, SequenceTerminalKind::Reduce { .. })) {
                return PipelineShape::SingleStageReduce;
            }

            return PipelineShape::MultiStage {
                stages: 0,
                repeated_transforms: false,
                has_terminal,
            };
        }

        if stage_count == 1 {
            match stages.first().unwrap() {
                SequenceStage::Map { .. } => return PipelineShape::SingleStageMap,
                SequenceStage::Filter { .. } => return PipelineShape::SingleStageFilter,
                _ => {}
            }
        }

        PipelineShape::MultiStage {
            stages: stage_count,
            repeated_transforms: has_repeated_transform(stages),
            has_terminal,
        }
    }
}

pub fn try_lower_sequence_call(
    function: Expression,
    args: Vec<Argument>,
    _metadata: CallArgumentMetadata,
    span: Span,
    context: &mut TransformContext,
) -> Result<Option<IrExpression>, TransformError> {
    let mut segments = Vec::new();
    let mut current_function = function;
    let mut current_args = args;
    let mut current_span = span.clone();
    let source_expr = loop {
        let Expression::MemberAccess {
            object,
            property,
            span: _member_span,
        } = current_function
        else {
            return Ok(None);
        };

        segments.push(Segment {
            method: property.clone(),
            args: current_args,
            span: current_span.clone(),
        });

        match *object {
            Expression::Call {
                function,
                args,
                type_arguments: _,
                argument_metadata: _,
                span,
            } => {
                current_function = *function;
                current_args = args;
                current_span = span;
            }
            other => break other,
        }
    };

    if segments.is_empty() {
        return Ok(None);
    }

    segments.reverse();

    let source = lower_sequence_source(source_expr, context)?;
    let mut pipeline = SequencePipeline {
        source,
        stages: Vec::new(),
        terminal: None,
        lazy: true,
        span: span.clone(),
        shape: PipelineShape::default(),
    };

    for segment in segments {
        match classify_method(&segment.method) {
            MethodKind::Stage(stage_kind) => {
                let stage = match build_stage(stage_kind, &segment, context) {
                    Ok(stage) => stage,
                    Err(_) => return Ok(None),
                };
                pipeline.stages.push(stage);
            }
            MethodKind::Terminal(kind) => {
                if pipeline.terminal.is_some() {
                    return Ok(None);
                }
                let terminal = match build_terminal(kind, &segment, context) {
                    Ok(terminal) => terminal,
                    Err(_) => return Ok(None),
                };
                pipeline.lazy = false;
                pipeline.terminal = Some(terminal);
                break;
            }
            MethodKind::Unsupported => return Ok(None),
        }
    }

    if pipeline.stages.is_empty() && pipeline.terminal.is_none() {
        return Ok(None);
    }

    pipeline.recompute_shape();

    if is_disallowed_sequence_source(&pipeline.source) {
        return Ok(None);
    }

    let java_type = determine_java_type(&pipeline);

    Ok(Some(IrExpression::SequencePipeline {
        pipeline,
        java_type,
        span,
    }))
}

fn lower_sequence_source(
    expr: Expression,
    context: &mut TransformContext,
) -> Result<SequenceSource, TransformError> {
    match expr {
        Expression::Array {
            elements,
            delimiter: SequenceDelimiter::Whitespace,
            span,
        } => {
            let mut lowered = Vec::with_capacity(elements.len());
            for element in elements {
                lowered.push(transform_expression(element, context)?);
            }

            Ok(SequenceSource::ListLiteral {
                elements: lowered,
                element_hint: None,
                span,
            })
        }
        other => {
            let source_ir = transform_expression(other, context)?;
            let inferred_type = extract_java_type(&source_ir);

            if inferred_type
                .as_ref()
                .is_some_and(|ty| is_java_stream_type(ty))
            {
                let element_hint = inferred_type.as_ref().and_then(stream_element_hint);

                return Ok(SequenceSource::JavaStream {
                    expr: Box::new(source_ir),
                    element_hint,
                    auto_close: false,
                });
            }

            Ok(SequenceSource::Collection {
                expr: Box::new(source_ir),
                element_hint: None,
            })
        }
    }
}

fn is_java_stream_type(java_type: &JavaType) -> bool {
    match java_type {
        JavaType::Reference { name, .. } => is_java_stream_name(name),
        _ => false,
    }
}

fn stream_element_hint(java_type: &JavaType) -> Option<JavaType> {
    match java_type {
        JavaType::Reference { name, generic_args } if is_java_stream_name(name) => {
            generic_args.first().cloned()
        }
        _ => None,
    }
}

fn default_java_stream_type() -> JavaType {
    JavaType::Reference {
        name: "java.util.stream.Stream".to_string(),
        generic_args: vec![JavaType::object()],
    }
}

fn is_java_stream_name(name: &str) -> bool {
    matches!(name, "java.util.stream.Stream" | "Stream")
}

fn is_disallowed_sequence_source(source: &SequenceSource) -> bool {
    match source {
        SequenceSource::Collection { expr, .. } => {
            extract_java_type(expr.as_ref()).map_or(false, is_disallowed_sequence_source_type)
        }
        _ => false,
    }
}

fn is_disallowed_sequence_source_type(java_type: JavaType) -> bool {
    match java_type {
        JavaType::Reference { name, .. } => {
            matches!(name.as_str(), "java.util.stream.Collectors" | "Collectors")
        }
        _ => false,
    }
}

#[derive(Clone)]
struct Segment {
    method: String,
    args: Vec<Argument>,
    span: Span,
}

enum MethodKind {
    Stage(StageKind),
    Terminal(TerminalKind),
    Unsupported,
}

#[derive(Clone, Copy)]
enum StageKind {
    Map,
    Filter,
    FlatMap,
    Take,
    Drop,
    Sorted,
    SortedBy,
}

#[derive(Clone, Copy)]
enum TerminalKind {
    ToList,
    ToSet,
    Fold,
    Reduce,
    GroupBy,
    Associate,
    Count,
    Sum,
    ForEach,
}

fn classify_method(name: &str) -> MethodKind {
    match name {
        "map" => MethodKind::Stage(StageKind::Map),
        "filter" => MethodKind::Stage(StageKind::Filter),
        "flatMap" => MethodKind::Stage(StageKind::FlatMap),
        "take" => MethodKind::Stage(StageKind::Take),
        "drop" => MethodKind::Stage(StageKind::Drop),
        "sorted" => MethodKind::Stage(StageKind::Sorted),
        "sortedBy" => MethodKind::Stage(StageKind::SortedBy),
        "toList" => MethodKind::Terminal(TerminalKind::ToList),
        "toSet" => MethodKind::Terminal(TerminalKind::ToSet),
        "fold" => MethodKind::Terminal(TerminalKind::Fold),
        "reduce" => MethodKind::Terminal(TerminalKind::Reduce),
        "groupBy" => MethodKind::Terminal(TerminalKind::GroupBy),
        "associate" => MethodKind::Terminal(TerminalKind::Associate),
        "count" => MethodKind::Terminal(TerminalKind::Count),
        "sum" => MethodKind::Terminal(TerminalKind::Sum),
        "forEach" => MethodKind::Terminal(TerminalKind::ForEach),
        _ => MethodKind::Unsupported,
    }
}

fn build_stage(
    kind: StageKind,
    segment: &Segment,
    context: &mut TransformContext,
) -> Result<SequenceStage, TransformError> {
    match kind {
        StageKind::Map => {
            let lambda = expect_single_positional(&segment.args)?;
            let lambda_ir = transform_expression(lambda, context)?;
            Ok(SequenceStage::Map {
                lambda: Box::new(lambda_ir),
                result_hint: None,
                span: segment.span.clone(),
            })
        }
        StageKind::Filter => {
            let predicate = expect_single_positional(&segment.args)?;
            let predicate_ir = transform_expression(predicate, context)?;
            Ok(SequenceStage::Filter {
                predicate: Box::new(predicate_ir),
                span: segment.span.clone(),
            })
        }
        StageKind::FlatMap => {
            let lambda = expect_single_positional(&segment.args)?;
            let lambda_ir = transform_expression(lambda, context)?;
            Ok(SequenceStage::FlatMap {
                lambda: Box::new(lambda_ir),
                element_hint: None,
                flatten_depth: 1,
                span: segment.span.clone(),
            })
        }
        StageKind::Take => {
            let count = expect_single_positional(&segment.args)?;
            let count_ir = transform_expression(count, context)?;
            Ok(SequenceStage::Take {
                count: Box::new(count_ir),
                span: segment.span.clone(),
            })
        }
        StageKind::Drop => {
            let count = expect_single_positional(&segment.args)?;
            let count_ir = transform_expression(count, context)?;
            Ok(SequenceStage::Drop {
                count: Box::new(count_ir),
                span: segment.span.clone(),
            })
        }
        StageKind::Sorted => {
            if segment.args.is_empty() {
                Ok(SequenceStage::Sorted {
                    comparator: None,
                    span: segment.span.clone(),
                })
            } else {
                let comparator = expect_single_positional(&segment.args)?;
                let comparator_ir = transform_expression(comparator, context)?;
                Ok(SequenceStage::Sorted {
                    comparator: Some(Box::new(comparator_ir)),
                    span: segment.span.clone(),
                })
            }
        }
        StageKind::SortedBy => {
            let comparator = expect_single_positional(&segment.args)?;
            let comparator_ir = transform_expression(comparator, context)?;
            Ok(SequenceStage::Sorted {
                comparator: Some(Box::new(comparator_ir)),
                span: segment.span.clone(),
            })
        }
    }
}

fn build_terminal(
    kind: TerminalKind,
    segment: &Segment,
    context: &mut TransformContext,
) -> Result<SequenceTerminal, TransformError> {
    match kind {
        TerminalKind::ToList if segment.args.is_empty() => Ok(SequenceTerminal::new(
            SequenceTerminalKind::ToList,
            SequenceTerminalEvaluation::Collector,
            false,
            segment.span.clone(),
        )),
        TerminalKind::ToSet if segment.args.is_empty() => Ok(SequenceTerminal::new(
            SequenceTerminalKind::ToSet,
            SequenceTerminalEvaluation::Collector,
            false,
            segment.span.clone(),
        )),
        TerminalKind::Fold => {
            let (initial, lambda) = expect_two_positional(&segment.args)?;
            let initial_ir = transform_expression(initial, context)?;
            let accumulator_ir = transform_expression(lambda, context)?;
            Ok(SequenceTerminal::new(
                SequenceTerminalKind::Fold {
                    initial: Box::new(initial_ir),
                    accumulator: Box::new(accumulator_ir),
                },
                SequenceTerminalEvaluation::Reducer,
                false,
                segment.span.clone(),
            ))
        }
        TerminalKind::Reduce => {
            let accumulator = expect_single_positional(&segment.args)?;
            let accumulator_ir = transform_expression(accumulator, context)?;
            Ok(SequenceTerminal::new(
                SequenceTerminalKind::Reduce {
                    accumulator: Box::new(accumulator_ir),
                },
                SequenceTerminalEvaluation::Reducer,
                true,
                segment.span.clone(),
            ))
        }
        TerminalKind::GroupBy => {
            let selector = expect_single_positional(&segment.args)?;
            let selector_ir = transform_expression(selector, context)?;
            Ok(SequenceTerminal::new(
                SequenceTerminalKind::GroupBy {
                    key_selector: Box::new(selector_ir),
                },
                SequenceTerminalEvaluation::Collector,
                false,
                segment.span.clone(),
            ))
        }
        TerminalKind::Associate => {
            let selector = expect_single_positional(&segment.args)?;
            let selector_ir = transform_expression(selector, context)?;
            Ok(SequenceTerminal::new(
                SequenceTerminalKind::Associate {
                    pair_selector: Box::new(selector_ir),
                },
                SequenceTerminalEvaluation::Collector,
                false,
                segment.span.clone(),
            ))
        }
        TerminalKind::Count if segment.args.is_empty() => Ok(SequenceTerminal::new(
            SequenceTerminalKind::Count,
            SequenceTerminalEvaluation::Aggregator,
            false,
            segment.span.clone(),
        )),
        TerminalKind::Sum if segment.args.is_empty() => Ok(SequenceTerminal::new(
            SequenceTerminalKind::Sum,
            SequenceTerminalEvaluation::Aggregator,
            false,
            segment.span.clone(),
        )),
        TerminalKind::ForEach => {
            let action = expect_single_positional(&segment.args)?;
            let action_ir = transform_expression(action, context)?;
            Ok(SequenceTerminal::new(
                SequenceTerminalKind::ForEach {
                    action: Box::new(action_ir),
                },
                SequenceTerminalEvaluation::Consumer,
                false,
                segment.span.clone(),
            ))
        }
        _ => Err(TransformError::TypeInferenceError {
            message: format!(
                "Unsupported terminal combination for method '{}'.",
                segment.method
            ),
            span: segment.span.clone(),
        }),
    }
}

fn determine_java_type(pipeline: &SequencePipeline) -> JavaType {
    if let Some(terminal) = pipeline.terminal.as_ref() {
        return match &terminal.kind {
            SequenceTerminalKind::ToList => JavaType::list(),
            SequenceTerminalKind::ToSet => JavaType::Reference {
                name: "java.util.Set".to_string(),
                generic_args: vec![],
            },
            SequenceTerminalKind::Fold { .. } | SequenceTerminalKind::Reduce { .. } => {
                JavaType::object()
            }
            SequenceTerminalKind::GroupBy { .. } | SequenceTerminalKind::Associate { .. } => {
                JavaType::Reference {
                    name: "java.util.Map".to_string(),
                    generic_args: vec![],
                }
            }
            SequenceTerminalKind::Count | SequenceTerminalKind::Sum => {
                JavaType::Primitive("long".to_string())
            }
            SequenceTerminalKind::ForEach { .. } => JavaType::void(),
        };
    }

    match &pipeline.source {
        SequenceSource::JavaStream { expr, .. } => extract_java_type(expr.as_ref())
            .filter(|ty| is_java_stream_type(ty))
            .unwrap_or_else(default_java_stream_type),
        _ => JavaType::sequence(),
    }
}

fn expect_single_positional(args: &[Argument]) -> Result<Expression, TransformError> {
    if args.len() != 1 {
        return Err(TransformError::TypeInferenceError {
            message: "Sequence stage expects exactly one argument".to_string(),
            span: args.first().map(argument_span).unwrap_or_else(Span::dummy),
        });
    }

    match &args[0] {
        Argument::Positional(expr) => Ok(expr.clone()),
        Argument::Named { span, .. } => Err(TransformError::TypeInferenceError {
            message: "Named arguments are not supported in sequence pipelines".to_string(),
            span: span.clone(),
        }),
    }
}

fn expect_two_positional(args: &[Argument]) -> Result<(Expression, Expression), TransformError> {
    if args.len() != 2 {
        return Err(TransformError::TypeInferenceError {
            message: "Sequence terminal expects exactly two positional arguments".to_string(),
            span: args.first().map(argument_span).unwrap_or_else(Span::dummy),
        });
    }

    match (&args[0], &args[1]) {
        (Argument::Positional(first), Argument::Positional(second)) => {
            Ok((first.clone(), second.clone()))
        }
        _ => Err(TransformError::TypeInferenceError {
            message: "Named arguments are not supported in sequence pipelines".to_string(),
            span: argument_span(&args[0]),
        }),
    }
}

fn argument_span(arg: &Argument) -> Span {
    match arg {
        Argument::Positional(expr) => expression_span(expr),
        Argument::Named { span, .. } => span.clone(),
    }
}

fn expression_span(expr: &Expression) -> Span {
    match expr {
        Expression::Literal(_, span)
        | Expression::Identifier(_, span)
        | Expression::Binary { span, .. }
        | Expression::Unary { span, .. }
        | Expression::Call { span, .. }
        | Expression::MemberAccess { span, .. }
        | Expression::NullSafeMemberAccess { span, .. }
        | Expression::IndexAccess { span, .. }
        | Expression::NullSafeIndexAccess { span, .. }
        | Expression::StringInterpolation { span, .. }
        | Expression::Array { span, .. }
        | Expression::Lambda { span, .. }
        | Expression::Block { span, .. }
        | Expression::When { span, .. }
        | Expression::If { span, .. }
        | Expression::Try { span, .. }
        | Expression::This(span)
        | Expression::Super(span) => span.clone(),
        Expression::RegexLiteral(regex) => regex.span.clone(),
        Expression::JsonLiteral(literal) => literal.span.clone(),
        Expression::MultilineString(literal) => literal.span.clone(),
    }
}

fn has_repeated_transform(stages: &[SequenceStage]) -> bool {
    let mut fingerprints = Vec::new();

    for stage in stages {
        let discriminant = mem::discriminant(stage);
        if fingerprints.iter().any(|seen| *seen == discriminant) {
            return true;
        }
        fingerprints.push(discriminant);
    }

    false
}
