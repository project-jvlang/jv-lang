use crate::context::TransformContext;
use crate::error::TransformError;
use crate::transform::{
    extract_java_type, normalize_whitespace_array_elements, transform_expression,
};
use crate::types::{IrExpression, IrProgram, IrResolvedMethodTarget, IrStatement, JavaType};
use jv_ast::{
    Argument, BinaryOp, CallArgumentMetadata, CallArgumentStyle, Expression, SequenceDelimiter,
    Span, types::PrimitiveTypeName,
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
    #[serde(default)]
    pub source_element_type: Option<JavaType>,
    #[serde(default)]
    pub stage_element_types: Vec<Option<JavaType>>,
}

impl SequencePipeline {
    pub fn recompute_shape(&mut self) {
        self.shape = PipelineShape::classify(&self.source, &self.stages, self.terminal.as_ref());
        self.recompute_element_types();
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

    pub fn recompute_element_types(&mut self) {
        self.source_element_type = source_element_type(&self.source);
        let mut current = self.source_element_type.clone();
        if current.is_none() {
            current = Some(JavaType::object());
        }

        self.stage_element_types.clear();
        self.stage_element_types.reserve(self.stages.len());

        for stage in self.stages.iter_mut() {
            match stage {
                SequenceStage::Map {
                    lambda,
                    result_hint,
                    ..
                } => {
                    let input_type = current.clone().unwrap_or_else(JavaType::object);
                    let output_type =
                        infer_map_result_hint(lambda.as_ref()).unwrap_or_else(JavaType::object);
                    update_single_param_lambda(
                        lambda.as_mut(),
                        "java.util.function.Function",
                        input_type.clone(),
                        output_type.clone(),
                    );
                    *result_hint = Some(output_type.clone());
                    current = Some(output_type);
                }
                SequenceStage::Filter { predicate, .. } => {
                    let input_type = current.clone().unwrap_or_else(JavaType::object);
                    update_single_param_lambda(
                        predicate.as_mut(),
                        "java.util.function.Predicate",
                        input_type,
                        JavaType::boolean(),
                    );
                }
                SequenceStage::FlatMap {
                    lambda,
                    element_hint,
                    ..
                } => {
                    let input_type = current.clone().unwrap_or_else(JavaType::object);
                    let lambda_return =
                        infer_lambda_return_type(lambda.as_ref()).unwrap_or_else(JavaType::object);
                    update_single_param_lambda(
                        lambda.as_mut(),
                        "java.util.function.Function",
                        input_type,
                        lambda_return.clone(),
                    );

                    let container_type = infer_flat_map_element_hint(lambda.as_ref())
                        .or_else(|| element_hint.clone());
                    if container_type != *element_hint {
                        *element_hint = container_type.clone();
                    }

                    let element_type = container_type
                        .as_ref()
                        .and_then(|ty| extract_iterable_element_type(ty))
                        .unwrap_or_else(JavaType::object);
                    current = Some(element_type);
                }
                SequenceStage::Take { .. } | SequenceStage::Drop { .. } => {
                    if current.is_none() {
                        current = Some(JavaType::object());
                    }
                }
                SequenceStage::Sorted { comparator, .. } => {
                    if let Some(comparator) = comparator.as_mut() {
                        let element_type = current.clone().unwrap_or_else(JavaType::object);
                        update_bi_param_lambda(
                            comparator,
                            "java.util.Comparator",
                            &element_type,
                            &element_type,
                            JavaType::int(),
                        );
                    }
                    if current.is_none() {
                        current = Some(JavaType::object());
                    }
                }
            }

            self.stage_element_types.push(current.clone());
        }
    }

    pub fn element_type(&self) -> Option<&JavaType> {
        self.stage_element_types
            .iter()
            .rev()
            .find_map(|entry| entry.as_ref())
            .or_else(|| self.source_element_type.as_ref())
    }

    pub fn element_type_before_stage(&self, index: usize) -> Option<&JavaType> {
        if index == 0 {
            return self.source_element_type.as_ref();
        }

        self.stage_element_types
            .get(index - 1)
            .and_then(|entry| entry.as_ref())
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
        source_element_type: None,
        stage_element_types: Vec::new(),
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
            let elements = normalize_whitespace_array_elements(elements);
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
            let result_hint = infer_map_result_hint(&lambda_ir);
            Ok(SequenceStage::Map {
                lambda: Box::new(lambda_ir),
                result_hint,
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
            let element_hint = infer_flat_map_element_hint(&lambda_ir);
            Ok(SequenceStage::FlatMap {
                lambda: Box::new(lambda_ir),
                element_hint,
                flatten_depth: 1,
                span: segment.span.clone(),
            })
        }
        StageKind::Take => {
            let count = expect_single_positional(&segment.args)?;
            let count_ir = transform_expression(count, context)?;
            let count_ir = ensure_long_count_expression(count_ir);
            Ok(SequenceStage::Take {
                count: Box::new(count_ir),
                span: segment.span.clone(),
            })
        }
        StageKind::Drop => {
            let count = expect_single_positional(&segment.args)?;
            let count_ir = transform_expression(count, context)?;
            let count_ir = ensure_long_count_expression(count_ir);
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

fn infer_flat_map_element_hint(lambda: &IrExpression) -> Option<JavaType> {
    match lambda {
        IrExpression::Lambda { body, .. } => infer_iterable_like_type(body.as_ref()),
        _ => None,
    }
}

fn infer_map_result_hint(lambda: &IrExpression) -> Option<JavaType> {
    match lambda {
        IrExpression::Lambda { body, .. } => infer_expression_type(body.as_ref()),
        _ => None,
    }
}

fn infer_expression_type(expr: &IrExpression) -> Option<JavaType> {
    match expr {
        IrExpression::Binary {
            op: BinaryOp::Add,
            left,
            right,
            ..
        } => {
            let left_ty = infer_expression_type(left).or_else(|| extract_java_type(left));
            let right_ty = infer_expression_type(right).or_else(|| extract_java_type(right));

            if left_ty.as_ref().map(is_string_like_type).unwrap_or(false)
                || right_ty.as_ref().map(is_string_like_type).unwrap_or(false)
            {
                return Some(JavaType::string());
            }

            extract_java_type(expr)
        }
        IrExpression::Conditional {
            then_expr,
            else_expr,
            ..
        } => {
            let then_ty = infer_expression_type(then_expr).or_else(|| extract_java_type(then_expr));
            let else_ty = infer_expression_type(else_expr).or_else(|| extract_java_type(else_expr));

            match (then_ty, else_ty) {
                (Some(a), Some(b)) if a == b => Some(a),
                (Some(a), None) => Some(a),
                (None, Some(b)) => Some(b),
                _ => extract_java_type(expr),
            }
        }
        IrExpression::Block {
            statements,
            java_type,
            ..
        } => {
            if let Some(last) = statements.last() {
                match last {
                    IrStatement::Expression { expr, .. }
                    | IrStatement::Return {
                        value: Some(expr), ..
                    } => infer_expression_type(expr).or_else(|| extract_java_type(expr)),
                    _ => Some(java_type.clone()),
                }
            } else {
                Some(java_type.clone())
            }
        }
        _ => extract_java_type(expr),
    }
}

fn is_string_like_type(java_type: &JavaType) -> bool {
    match java_type {
        JavaType::Reference { name, .. } => matches!(
            name.as_str(),
            "String"
                | "java.lang.String"
                | "CharSequence"
                | "java.lang.CharSequence"
                | "StringBuilder"
                | "java.lang.StringBuilder"
                | "StringBuffer"
                | "java.lang.StringBuffer"
        ),
        _ => false,
    }
}

fn ensure_long_count_expression(expr: IrExpression) -> IrExpression {
    let span = expr.span();
    let java_type = extract_java_type(&expr);

    match java_type {
        Some(JavaType::Primitive(name)) => match name.as_str() {
            "long" => expr,
            "int" | "short" | "byte" | "char" => build_long_cast(expr, span),
            _ => expr,
        },
        Some(JavaType::Reference { name, .. }) if is_number_like_type(&name) => {
            build_number_long_value(expr, span)
        }
        Some(JavaType::Reference { name, .. }) if name == "java.lang.Character" => {
            build_character_long_value(expr, span)
        }
        _ => expr,
    }
}

fn build_long_cast(expr: IrExpression, span: Span) -> IrExpression {
    IrExpression::Cast {
        expr: Box::new(expr),
        target_type: JavaType::Primitive("long".to_string()),
        span,
    }
}

fn build_number_long_value(expr: IrExpression, span: Span) -> IrExpression {
    IrExpression::MethodCall {
        receiver: Some(Box::new(expr)),
        method_name: "longValue".to_string(),
        java_name: Some("longValue".to_string()),
        resolved_target: None,
        args: Vec::new(),
        argument_style: CallArgumentStyle::Comma,
        java_type: JavaType::Primitive("long".to_string()),
        span,
    }
}

fn build_character_long_value(expr: IrExpression, span: Span) -> IrExpression {
    let char_value = IrExpression::MethodCall {
        receiver: Some(Box::new(expr)),
        method_name: "charValue".to_string(),
        java_name: Some("charValue".to_string()),
        resolved_target: None,
        args: Vec::new(),
        argument_style: CallArgumentStyle::Comma,
        java_type: JavaType::Primitive("char".to_string()),
        span: span.clone(),
    };

    build_long_cast(char_value, span)
}

fn is_number_like_type(name: &str) -> bool {
    matches!(
        name,
        "java.lang.Number"
            | "java.lang.Byte"
            | "java.lang.Short"
            | "java.lang.Integer"
            | "java.lang.Long"
            | "java.lang.Float"
            | "java.lang.Double"
    )
}

fn infer_iterable_like_type(expr: &IrExpression) -> Option<JavaType> {
    if let Some(java_type) = extract_java_type(expr) {
        if is_iterable_like_java_type(&java_type) {
            return Some(java_type);
        }
    }

    match expr {
        IrExpression::Conditional {
            then_expr,
            else_expr,
            java_type,
            ..
        } => {
            if is_iterable_like_java_type(java_type) {
                return Some(java_type.clone());
            }

            let then_hint = infer_iterable_like_type(then_expr);
            let else_hint = infer_iterable_like_type(else_expr);
            match (then_hint, else_hint) {
                (Some(hint), Some(other)) if hint == other => Some(hint),
                (Some(hint), None) => Some(hint),
                (None, Some(hint)) => Some(hint),
                _ => None,
            }
        }
        IrExpression::Block { java_type, .. }
        | IrExpression::Cast {
            target_type: java_type,
            ..
        } => {
            if is_iterable_like_java_type(java_type) {
                Some(java_type.clone())
            } else {
                None
            }
        }
        IrExpression::MethodCall {
            method_name,
            resolved_target,
            receiver,
            args,
            java_type,
            ..
        } => {
            if is_iterable_like_java_type(java_type) {
                return Some(java_type.clone());
            }

            if let Some(hint) = infer_iterable_like_from_method_call(
                method_name,
                resolved_target.as_ref(),
                receiver.as_deref(),
                args.as_slice(),
                java_type,
            ) {
                return Some(hint);
            }

            receiver
                .as_deref()
                .and_then(|recv| extract_java_type(recv))
                .filter(is_iterable_like_java_type)
        }
        IrExpression::ObjectCreation {
            class_name,
            generic_args,
            ..
        } => {
            if is_iterable_like_name(class_name) {
                Some(JavaType::Reference {
                    name: class_name.clone(),
                    generic_args: generic_args.clone(),
                })
            } else {
                None
            }
        }
        IrExpression::ArrayCreation {
            element_type,
            delimiter,
            ..
        } => {
            if *delimiter == SequenceDelimiter::Whitespace {
                Some(JavaType::Reference {
                    name: "java.util.List".to_string(),
                    generic_args: vec![element_type.clone()],
                })
            } else {
                None
            }
        }
        IrExpression::Identifier { java_type, .. }
        | IrExpression::FieldAccess { java_type, .. }
        | IrExpression::ArrayAccess { java_type, .. }
        | IrExpression::Assignment { java_type, .. }
        | IrExpression::Switch { java_type, .. }
        | IrExpression::NullSafeOperation { java_type, .. }
        | IrExpression::SequencePipeline { java_type, .. }
        | IrExpression::TryWithResources { java_type, .. }
        | IrExpression::CompletableFuture { java_type, .. }
        | IrExpression::VirtualThread { java_type, .. } => {
            if is_iterable_like_java_type(java_type) {
                Some(java_type.clone())
            } else {
                None
            }
        }
        _ => None,
    }
}

fn infer_iterable_like_from_method_call(
    method_name: &str,
    resolved_target: Option<&IrResolvedMethodTarget>,
    receiver: Option<&IrExpression>,
    args: &[IrExpression],
    java_type: &JavaType,
) -> Option<JavaType> {
    if !returns_object_like(java_type) {
        return None;
    }

    if let Some(target) = resolved_target {
        if let Some(owner) = target.owner.as_deref() {
            if is_iterable_like_name(owner) && is_iterable_factory_method(owner, method_name) {
                return Some(build_iterable_type_hint(owner, java_type, args));
            }
        }
    }

    if let Some(receiver_expr) = receiver {
        if let Some(receiver_type) = extract_java_type(receiver_expr) {
            if is_iterable_like_java_type(&receiver_type) {
                if let Some(owner) = iterable_type_name(&receiver_type) {
                    if is_iterable_factory_method(owner, method_name) {
                        return Some(build_iterable_type_hint(owner, java_type, args));
                    }
                }
            }
        }
    }

    None
}

fn iterable_type_name(java_type: &JavaType) -> Option<&str> {
    match java_type {
        JavaType::Reference { name, .. } => Some(name.as_str()),
        _ => None,
    }
}

fn returns_object_like(java_type: &JavaType) -> bool {
    matches!(
        java_type,
        JavaType::Reference { name, .. }
            if name == "java.lang.Object" || name == "Object"
    )
}

fn is_iterable_factory_method(owner: &str, method_name: &str) -> bool {
    match method_name {
        "of" | "copyOf" => true,
        "from" | "fromIterable" | "fromSequence" | "fromStream" => is_sequence_owner(owner),
        _ => false,
    }
}

fn is_sequence_owner(name: &str) -> bool {
    let simple = name.rsplit('.').next().unwrap_or(name);
    simple == "Sequence"
}

fn build_iterable_type_hint(owner: &str, java_type: &JavaType, args: &[IrExpression]) -> JavaType {
    if let JavaType::Reference {
        generic_args: existing,
        ..
    } = java_type
    {
        if !existing.is_empty() {
            return JavaType::Reference {
                name: owner.to_string(),
                generic_args: existing.clone(),
            };
        }
    }

    let element_type = args
        .iter()
        .find_map(|arg| extract_java_type(arg))
        .map(|ty| boxed_type(&ty))
        .unwrap_or_else(JavaType::object);

    JavaType::Reference {
        name: owner.to_string(),
        generic_args: vec![element_type],
    }
}

fn is_iterable_like_java_type(java_type: &JavaType) -> bool {
    match java_type {
        JavaType::Reference { name, .. } => is_iterable_like_name(name),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jv_ast::CallArgumentStyle;
    use jv_ast::Span;

    fn dummy_span() -> Span {
        Span::dummy()
    }

    fn lambda_returning_factory(owner: &str) -> IrExpression {
        IrExpression::Lambda {
            functional_interface: "java.util.function.Function".to_string(),
            param_names: vec!["value".to_string()],
            param_types: vec![JavaType::object()],
            body: Box::new(IrExpression::MethodCall {
                receiver: None,
                method_name: "of".to_string(),
                java_name: None,
                resolved_target: Some(IrResolvedMethodTarget {
                    owner: Some(owner.to_string()),
                    original_name: Some("of".to_string()),
                    java_name: Some("of".to_string()),
                    erased_parameters: vec!["Ljava/lang/Object;".to_string()],
                }),
                args: vec![IrExpression::Identifier {
                    name: "value".to_string(),
                    java_type: JavaType::object(),
                    span: dummy_span(),
                }],
                argument_style: CallArgumentStyle::Comma,
                java_type: JavaType::object(),
                span: dummy_span(),
            }),
            java_type: JavaType::object(),
            span: dummy_span(),
        }
    }

    #[test]
    fn flat_map_hint_infers_list_of_factory() {
        let lambda = lambda_returning_factory("java.util.List");
        let hint = infer_flat_map_element_hint(&lambda);
        assert_eq!(
            hint,
            Some(JavaType::Reference {
                name: "java.util.List".to_string(),
                generic_args: vec![JavaType::object()],
            })
        );
    }

    #[test]
    fn flat_map_hint_infers_set_of_factory() {
        let lambda = lambda_returning_factory("java.util.Set");
        let hint = infer_flat_map_element_hint(&lambda);
        assert_eq!(
            hint,
            Some(JavaType::Reference {
                name: "java.util.Set".to_string(),
                generic_args: vec![JavaType::object()],
            })
        );
    }
}

fn is_iterable_like_name(name: &str) -> bool {
    let simple = name.rsplit('.').next().unwrap_or(name);
    matches!(
        simple,
        "Iterable"
            | "Collection"
            | "List"
            | "Set"
            | "Queue"
            | "Deque"
            | "ArrayList"
            | "LinkedList"
            | "LinkedHashSet"
            | "HashSet"
            | "Sequence"
    ) || matches!(
        name,
        "java.lang.Iterable"
            | "java.util.Collection"
            | "java.util.List"
            | "java.util.Set"
            | "java.util.Queue"
            | "java.util.Deque"
            | "java.util.ArrayList"
            | "java.util.LinkedList"
            | "java.util.LinkedHashSet"
            | "java.util.HashSet"
            | "jv.collections.Sequence"
    )
}

fn infer_lambda_return_type(lambda: &IrExpression) -> Option<JavaType> {
    match lambda {
        IrExpression::Lambda { body, .. } => extract_java_type(body.as_ref()),
        _ => extract_java_type(lambda),
    }
}

fn update_single_param_lambda(
    lambda: &mut IrExpression,
    interface: &str,
    param_type: JavaType,
    return_type: JavaType,
) {
    if let IrExpression::Lambda {
        functional_interface,
        param_types,
        java_type,
        param_names,
        body,
        ..
    } = lambda
    {
        *functional_interface = interface.to_string();

        if param_types.len() != 1 || param_types[0] != param_type {
            param_types.clear();
            param_types.push(param_type.clone());
        }

        *java_type = JavaType::Functional {
            interface_name: interface.to_string(),
            param_types: vec![param_type.clone()],
            return_type: Box::new(return_type.clone()),
        };

        if let Some(name) = param_names.get(0) {
            update_identifier_usage(body.as_mut(), name, &param_type);
        }
    }
}

fn update_bi_param_lambda(
    lambda: &mut IrExpression,
    interface: &str,
    first_param: &JavaType,
    second_param: &JavaType,
    return_type: JavaType,
) {
    if let IrExpression::Lambda {
        functional_interface,
        param_types,
        java_type,
        param_names,
        body,
        ..
    } = lambda
    {
        *functional_interface = interface.to_string();

        if param_types.len() != 2
            || param_types[0] != *first_param
            || param_types[1] != *second_param
        {
            param_types.clear();
            param_types.push(first_param.clone());
            param_types.push(second_param.clone());
        }

        *java_type = JavaType::Functional {
            interface_name: interface.to_string(),
            param_types: vec![first_param.clone(), second_param.clone()],
            return_type: Box::new(return_type.clone()),
        };

        if let Some(name) = param_names.get(0) {
            update_identifier_usage(body.as_mut(), name, first_param);
        }
        if let Some(name) = param_names.get(1) {
            update_identifier_usage(body.as_mut(), name, second_param);
        }
    }
}

fn update_identifier_usage(expr: &mut IrExpression, target: &str, java_type: &JavaType) {
    match expr {
        IrExpression::Identifier {
            name,
            java_type: ty,
            ..
        } if name == target => {
            *ty = java_type.clone();
        }
        IrExpression::MethodCall { receiver, args, .. } => {
            if let Some(recv) = receiver.as_mut() {
                update_identifier_usage(recv, target, java_type);
            }
            for arg in args.iter_mut() {
                update_identifier_usage(arg, target, java_type);
            }
        }
        IrExpression::FieldAccess { receiver, .. } => {
            update_identifier_usage(receiver, target, java_type);
        }
        IrExpression::ArrayAccess { array, index, .. } => {
            update_identifier_usage(array, target, java_type);
            update_identifier_usage(index, target, java_type);
        }
        IrExpression::Binary { left, right, .. } => {
            update_identifier_usage(left, target, java_type);
            update_identifier_usage(right, target, java_type);
        }
        IrExpression::Unary { operand, .. } => {
            update_identifier_usage(operand, target, java_type);
        }
        IrExpression::Assignment {
            target: assign,
            value,
            ..
        } => {
            update_identifier_usage(assign, target, java_type);
            update_identifier_usage(value, target, java_type);
        }
        IrExpression::Conditional {
            condition,
            then_expr,
            else_expr,
            ..
        } => {
            update_identifier_usage(condition, target, java_type);
            update_identifier_usage(then_expr, target, java_type);
            update_identifier_usage(else_expr, target, java_type);
        }
        IrExpression::Block { statements, .. } => {
            for stmt in statements.iter_mut() {
                update_identifier_usage_in_statement(stmt, target, java_type);
            }
        }
        IrExpression::ObjectCreation { args, .. } => {
            for arg in args.iter_mut() {
                update_identifier_usage(arg, target, java_type);
            }
        }
        IrExpression::Lambda {
            param_names, body, ..
        } => {
            if param_names.iter().any(|name| name == target) {
                return;
            }
            update_identifier_usage(body.as_mut(), target, java_type);
        }
        IrExpression::SequencePipeline { pipeline, .. } => {
            update_sequence_pipeline_identifiers(pipeline, target, java_type);
        }
        IrExpression::Switch {
            discriminant,
            cases,
            ..
        } => {
            update_identifier_usage(discriminant, target, java_type);
            for case in cases.iter_mut() {
                update_switch_case_identifiers(case, target, java_type);
            }
        }
        IrExpression::Cast { expr, .. } => {
            update_identifier_usage(expr, target, java_type);
        }
        IrExpression::InstanceOf { expr, .. } => {
            update_identifier_usage(expr, target, java_type);
        }
        IrExpression::ArrayCreation {
            dimensions,
            initializer,
            ..
        } => {
            for dimension in dimensions.iter_mut().flatten() {
                update_identifier_usage(dimension, target, java_type);
            }
            if let Some(elements) = initializer.as_mut() {
                for element in elements.iter_mut() {
                    update_identifier_usage(element, target, java_type);
                }
            }
        }
        IrExpression::NullSafeOperation {
            expr,
            operation,
            default_value,
            ..
        } => {
            update_identifier_usage(expr, target, java_type);
            update_identifier_usage(operation, target, java_type);
            if let Some(default) = default_value.as_mut() {
                update_identifier_usage(default, target, java_type);
            }
        }
        IrExpression::StringFormat { args, .. }
        | IrExpression::CompletableFuture { args, .. }
        | IrExpression::VirtualThread { args, .. } => {
            for arg in args.iter_mut() {
                update_identifier_usage(arg, target, java_type);
            }
        }
        IrExpression::TryWithResources {
            resources, body, ..
        } => {
            for resource in resources.iter_mut() {
                update_resource_identifiers(resource, target, java_type);
            }
            update_identifier_usage(body.as_mut(), target, java_type);
        }
        IrExpression::RegexPattern { .. }
        | IrExpression::Literal(..)
        | IrExpression::This { .. }
        | IrExpression::Super { .. } => {}
        _ => {}
    }
}

fn update_identifier_usage_in_statement(
    statement: &mut IrStatement,
    target: &str,
    java_type: &JavaType,
) {
    match statement {
        IrStatement::VariableDeclaration { initializer, .. } => {
            if let Some(expr) = initializer.as_mut() {
                update_identifier_usage(expr, target, java_type);
            }
        }
        IrStatement::Expression { expr, .. } => {
            update_identifier_usage(expr, target, java_type);
        }
        IrStatement::Return { value, .. } => {
            if let Some(expr) = value.as_mut() {
                update_identifier_usage(expr, target, java_type);
            }
        }
        IrStatement::If {
            condition,
            then_stmt,
            else_stmt,
            ..
        } => {
            update_identifier_usage(condition, target, java_type);
            update_identifier_usage_in_statement(then_stmt.as_mut(), target, java_type);
            if let Some(else_stmt) = else_stmt.as_mut() {
                update_identifier_usage_in_statement(else_stmt, target, java_type);
            }
        }
        IrStatement::While {
            condition, body, ..
        } => {
            update_identifier_usage(condition, target, java_type);
            update_identifier_usage_in_statement(body.as_mut(), target, java_type);
        }
        IrStatement::ForEach {
            variable,
            iterable,
            body,
            ..
        } => {
            update_identifier_usage(iterable, target, java_type);
            if variable != target {
                update_identifier_usage_in_statement(body.as_mut(), target, java_type);
            }
        }
        IrStatement::For {
            init,
            condition,
            update,
            body,
            ..
        } => {
            if let Some(init_stmt) = init.as_mut() {
                update_identifier_usage_in_statement(init_stmt, target, java_type);
            }
            if let Some(cond) = condition.as_mut() {
                update_identifier_usage(cond, target, java_type);
            }
            if let Some(update_expr) = update.as_mut() {
                update_identifier_usage(update_expr, target, java_type);
            }
            update_identifier_usage_in_statement(body.as_mut(), target, java_type);
        }
        IrStatement::Switch {
            discriminant,
            cases,
            ..
        } => {
            update_identifier_usage(discriminant, target, java_type);
            for case in cases.iter_mut() {
                update_switch_case_identifiers(case, target, java_type);
            }
        }
        IrStatement::Try {
            body,
            catch_clauses,
            finally_block,
            ..
        } => {
            update_identifier_usage_in_statement(body.as_mut(), target, java_type);
            for clause in catch_clauses.iter_mut() {
                if clause.variable_name != target {
                    update_identifier_usage_in_statement(&mut clause.body, target, java_type);
                }
            }
            if let Some(finally_stmt) = finally_block.as_mut() {
                update_identifier_usage_in_statement(finally_stmt, target, java_type);
            }
        }
        IrStatement::TryWithResources {
            resources,
            body,
            catch_clauses,
            finally_block,
            ..
        } => {
            for resource in resources.iter_mut() {
                update_resource_identifiers(resource, target, java_type);
            }
            update_identifier_usage_in_statement(body.as_mut(), target, java_type);
            for clause in catch_clauses.iter_mut() {
                if clause.variable_name != target {
                    update_identifier_usage_in_statement(&mut clause.body, target, java_type);
                }
            }
            if let Some(finally_stmt) = finally_block.as_mut() {
                update_identifier_usage_in_statement(finally_stmt, target, java_type);
            }
        }
        IrStatement::Throw { expr, .. } => {
            update_identifier_usage(expr, target, java_type);
        }
        IrStatement::Block { statements, .. } => {
            for stmt in statements.iter_mut() {
                update_identifier_usage_in_statement(stmt, target, java_type);
            }
        }
        IrStatement::ClassDeclaration {
            fields,
            methods,
            nested_classes,
            ..
        } => {
            for field in fields.iter_mut() {
                update_identifier_usage_in_statement(field, target, java_type);
            }
            for method in methods.iter_mut() {
                update_identifier_usage_in_statement(method, target, java_type);
            }
            for nested in nested_classes.iter_mut() {
                update_identifier_usage_in_statement(nested, target, java_type);
            }
        }
        IrStatement::InterfaceDeclaration {
            fields,
            methods,
            default_methods,
            nested_types,
            ..
        } => {
            for field in fields.iter_mut() {
                update_identifier_usage_in_statement(field, target, java_type);
            }
            for method in methods.iter_mut() {
                update_identifier_usage_in_statement(method, target, java_type);
            }
            for default_method in default_methods.iter_mut() {
                update_identifier_usage_in_statement(default_method, target, java_type);
            }
            for nested in nested_types.iter_mut() {
                update_identifier_usage_in_statement(nested, target, java_type);
            }
        }
        IrStatement::RecordDeclaration { methods, .. } => {
            for method in methods.iter_mut() {
                update_identifier_usage_in_statement(method, target, java_type);
            }
        }
        IrStatement::MethodDeclaration { body, .. } => {
            if let Some(body_expr) = body.as_mut() {
                update_identifier_usage(body_expr, target, java_type);
            }
        }
        IrStatement::FieldDeclaration { initializer, .. } => {
            if let Some(expr) = initializer.as_mut() {
                update_identifier_usage(expr, target, java_type);
            }
        }
        IrStatement::SampleDeclaration(_)
        | IrStatement::Comment { .. }
        | IrStatement::Commented { .. }
        | IrStatement::Break { .. }
        | IrStatement::Continue { .. }
        | IrStatement::Import(_)
        | IrStatement::Package { .. } => {}
    }
}

fn update_switch_case_identifiers(
    case: &mut crate::types::IrSwitchCase,
    target: &str,
    java_type: &JavaType,
) {
    for label in case.labels.iter_mut() {
        update_case_label_identifiers(label, target, java_type);
    }
    if let Some(guard) = case.guard.as_mut() {
        update_identifier_usage(guard, target, java_type);
    }
    update_identifier_usage(&mut case.body, target, java_type);
}

fn update_case_label_identifiers(
    label: &mut crate::types::IrCaseLabel,
    target: &str,
    java_type: &JavaType,
) {
    match label {
        crate::types::IrCaseLabel::Range { lower, upper, .. } => {
            update_identifier_usage(lower, target, java_type);
            update_identifier_usage(upper, target, java_type);
        }
        _ => {}
    }
}

fn update_resource_identifiers(
    resource: &mut crate::types::IrResource,
    target: &str,
    java_type: &JavaType,
) {
    update_identifier_usage(&mut resource.initializer, target, java_type);
}

fn update_sequence_pipeline_identifiers(
    pipeline: &mut SequencePipeline,
    target: &str,
    java_type: &JavaType,
) {
    update_sequence_source_identifiers(&mut pipeline.source, target, java_type);
    for stage in pipeline.stages.iter_mut() {
        update_sequence_stage_identifiers(stage, target, java_type);
    }
    if let Some(terminal) = pipeline.terminal.as_mut() {
        update_sequence_terminal_identifiers(terminal, target, java_type);
    }
}

fn update_sequence_source_identifiers(
    source: &mut SequenceSource,
    target: &str,
    java_type: &JavaType,
) {
    match source {
        SequenceSource::Collection { expr, .. }
        | SequenceSource::Array { expr, .. }
        | SequenceSource::JavaStream { expr, .. } => {
            update_identifier_usage(expr, target, java_type);
        }
        SequenceSource::ListLiteral { elements, .. } => {
            for element in elements.iter_mut() {
                update_identifier_usage(element, target, java_type);
            }
        }
    }
}

fn update_sequence_stage_identifiers(
    stage: &mut SequenceStage,
    target: &str,
    java_type: &JavaType,
) {
    match stage {
        SequenceStage::Map { lambda, .. } | SequenceStage::FlatMap { lambda, .. } => {
            update_identifier_usage(lambda.as_mut(), target, java_type);
        }
        SequenceStage::Filter { predicate, .. } => {
            update_identifier_usage(predicate.as_mut(), target, java_type);
        }
        SequenceStage::Take { count, .. } | SequenceStage::Drop { count, .. } => {
            update_identifier_usage(count.as_mut(), target, java_type);
        }
        SequenceStage::Sorted { comparator, .. } => {
            if let Some(comp) = comparator.as_mut() {
                update_identifier_usage(comp.as_mut(), target, java_type);
            }
        }
    }
}

fn update_sequence_terminal_identifiers(
    terminal: &mut SequenceTerminal,
    target: &str,
    java_type: &JavaType,
) {
    use SequenceTerminalKind::*;

    match &mut terminal.kind {
        Fold {
            initial,
            accumulator,
        } => {
            update_identifier_usage(initial.as_mut(), target, java_type);
            update_identifier_usage(accumulator.as_mut(), target, java_type);
        }
        Reduce { accumulator } => {
            update_identifier_usage(accumulator.as_mut(), target, java_type);
        }
        GroupBy { key_selector } => {
            update_identifier_usage(key_selector.as_mut(), target, java_type);
        }
        Associate { pair_selector } => {
            update_identifier_usage(pair_selector.as_mut(), target, java_type);
        }
        ForEach { action } => {
            update_identifier_usage(action.as_mut(), target, java_type);
        }
        ToList | ToSet | Count | Sum => {}
    }
}

fn infer_pipeline_element_type(pipeline: &SequencePipeline) -> Option<JavaType> {
    pipeline.element_type().cloned()
}

fn source_element_type(source: &SequenceSource) -> Option<JavaType> {
    match source {
        SequenceSource::Collection { expr, element_hint } => element_hint.clone().or_else(|| {
            extract_java_type(expr.as_ref()).and_then(|ty| match ty {
                JavaType::Reference { generic_args, .. } if !generic_args.is_empty() => {
                    Some(generic_args[0].clone())
                }
                JavaType::Array { element_type, .. } => Some(*element_type.clone()),
                _ => None,
            })
        }),
        SequenceSource::Array {
            expr, element_hint, ..
        } => element_hint.clone().or_else(|| {
            extract_java_type(expr.as_ref()).and_then(|ty| match ty {
                JavaType::Array { element_type, .. } => Some(*element_type.clone()),
                _ => None,
            })
        }),
        SequenceSource::ListLiteral {
            elements,
            element_hint,
            ..
        } => {
            if let Some(hint) = element_hint {
                Some(hint.clone())
            } else {
                elements.first().and_then(|expr| extract_java_type(expr))
            }
        }
        SequenceSource::JavaStream { element_hint, .. } => element_hint.clone(),
    }
}

fn extract_iterable_element_type(java_type: &JavaType) -> Option<JavaType> {
    match java_type {
        JavaType::Reference { generic_args, .. } => generic_args.first().cloned(),
        JavaType::Array { element_type, .. } => Some(*element_type.clone()),
        _ => None,
    }
}

fn boxed_type(java_type: &JavaType) -> JavaType {
    match java_type {
        JavaType::Primitive(name) => {
            let boxed = match name.as_str() {
                "int" => "Integer",
                "boolean" => "Boolean",
                "char" => "Character",
                "double" => "Double",
                "float" => "Float",
                "long" => "Long",
                "byte" => "Byte",
                "short" => "Short",
                _ => "Object",
            };

            JavaType::Reference {
                name: boxed.to_string(),
                generic_args: Vec::new(),
            }
        }
        JavaType::Void => JavaType::object(),
        _ => java_type.clone(),
    }
}

fn java_optional_type(inner: JavaType) -> JavaType {
    JavaType::Reference {
        name: "java.util.Optional".to_string(),
        generic_args: vec![inner],
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
                false,
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
            SequenceTerminalKind::Fold { .. } => JavaType::object(),
            SequenceTerminalKind::Reduce { .. } => {
                let element_type = infer_pipeline_element_type(pipeline)
                    .map(|ty| boxed_type(&ty))
                    .unwrap_or_else(JavaType::object);
                java_optional_type(element_type)
            }
            SequenceTerminalKind::GroupBy { .. } | SequenceTerminalKind::Associate { .. } => {
                JavaType::Reference {
                    name: "java.util.Map".to_string(),
                    generic_args: vec![],
                }
            }
            SequenceTerminalKind::Count => JavaType::Primitive("long".to_string()),
            SequenceTerminalKind::Sum => determine_sum_result_type(pipeline, terminal),
            SequenceTerminalKind::ForEach { .. } => JavaType::void(),
        };
    }

    match &pipeline.source {
        SequenceSource::JavaStream { expr, .. } => extract_java_type(expr.as_ref())
            .filter(|ty| is_java_stream_type(ty))
            .unwrap_or_else(default_java_stream_type),
        _ => JavaType::stream(),
    }
}

fn determine_sum_result_type(pipeline: &SequencePipeline, terminal: &SequenceTerminal) -> JavaType {
    if let Some(hint) = terminal.specialization_hint.as_ref() {
        return match hint.canonical {
            PrimitiveTypeName::Int | PrimitiveTypeName::Char => {
                JavaType::Primitive("int".to_string())
            }
            PrimitiveTypeName::Long => JavaType::Primitive("long".to_string()),
            PrimitiveTypeName::Double | PrimitiveTypeName::Float => {
                JavaType::Primitive("double".to_string())
            }
            _ => JavaType::Primitive("long".to_string()),
        };
    }

    if let Some(element_type) = infer_pipeline_element_type(pipeline) {
        return match element_type {
            JavaType::Primitive(ref name)
                if matches!(name.as_str(), "int" | "short" | "byte" | "char") =>
            {
                JavaType::Primitive("int".to_string())
            }
            JavaType::Primitive(ref name) if name == "long" => {
                JavaType::Primitive("long".to_string())
            }
            JavaType::Primitive(ref name) if name == "float" || name == "double" => {
                JavaType::Primitive("double".to_string())
            }
            _ => JavaType::Primitive("long".to_string()),
        };
    }

    JavaType::Primitive("int".to_string())
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
        | Expression::TypeCast { span, .. }
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

pub fn enforce_list_terminals(program: &mut IrProgram) {
    let mut enforcer = ListTerminalEnforcer {
        return_type_stack: Vec::new(),
    };
    enforcer.visit_program(program);
}

struct ListTerminalEnforcer {
    return_type_stack: Vec<Option<JavaType>>,
}

impl ListTerminalEnforcer {
    fn visit_program(&mut self, program: &mut IrProgram) {
        for stmt in &mut program.type_declarations {
            self.visit_statement(stmt);
        }
    }

    fn visit_statement(&mut self, stmt: &mut IrStatement) {
        match stmt {
            IrStatement::VariableDeclaration {
                java_type,
                initializer,
                ..
            }
            | IrStatement::FieldDeclaration {
                java_type,
                initializer,
                ..
            } => {
                if let Some(expr) = initializer {
                    self.visit_expression(expr, Some(java_type));
                }
            }
            IrStatement::Return { value, .. } => {
                if let Some(expr) = value {
                    let expected_type = self.return_type_stack.last().and_then(|ty| ty.clone());
                    let expected_ref = expected_type.as_ref();
                    self.visit_expression(expr, expected_ref);
                }
            }
            IrStatement::Expression { expr, .. } => {
                self.visit_expression(expr, None);
            }
            IrStatement::Block { statements, .. } => {
                for stmt in statements {
                    self.visit_statement(stmt);
                }
            }
            IrStatement::If {
                condition,
                then_stmt,
                else_stmt,
                ..
            } => {
                self.visit_expression(condition, None);
                self.visit_statement(then_stmt);
                if let Some(else_branch) = else_stmt {
                    self.visit_statement(else_branch);
                }
            }
            IrStatement::While {
                condition, body, ..
            } => {
                self.visit_expression(condition, None);
                self.visit_statement(body);
            }
            IrStatement::For {
                init,
                condition,
                update,
                body,
                ..
            } => {
                if let Some(init_stmt) = init {
                    self.visit_statement(init_stmt);
                }
                if let Some(cond) = condition {
                    self.visit_expression(cond, None);
                }
                if let Some(update_expr) = update {
                    self.visit_expression(update_expr, None);
                }
                self.visit_statement(body);
            }
            IrStatement::ForEach { iterable, body, .. } => {
                self.visit_expression(iterable, None);
                self.visit_statement(body);
            }
            IrStatement::Switch {
                discriminant,
                cases,
                ..
            } => {
                self.visit_expression(discriminant, None);
                for case in cases {
                    self.visit_expression(&mut case.body, None);
                }
            }
            IrStatement::Try {
                body,
                catch_clauses,
                finally_block,
                ..
            } => {
                self.visit_statement(body);
                for clause in catch_clauses {
                    self.visit_statement(&mut clause.body);
                }
                if let Some(finally_stmt) = finally_block {
                    self.visit_statement(finally_stmt);
                }
            }
            IrStatement::MethodDeclaration {
                return_type, body, ..
            } => {
                self.return_type_stack.push(Some(return_type.clone()));
                if let Some(expr) = body {
                    self.visit_expression(expr, Some(return_type));
                }
                self.return_type_stack.pop();
            }
            IrStatement::ClassDeclaration {
                fields,
                methods,
                nested_classes,
                ..
            } => {
                for field in fields {
                    self.visit_statement(field);
                }
                for method in methods {
                    self.visit_statement(method);
                }
                for nested in nested_classes {
                    self.visit_statement(nested);
                }
            }
            IrStatement::InterfaceDeclaration {
                fields,
                methods,
                default_methods,
                nested_types,
                ..
            } => {
                for field in fields {
                    self.visit_statement(field);
                }
                for method in methods {
                    self.visit_statement(method);
                }
                for default_method in default_methods {
                    self.visit_statement(default_method);
                }
                for nested in nested_types {
                    self.visit_statement(nested);
                }
            }
            IrStatement::RecordDeclaration { methods, .. } => {
                for method in methods {
                    self.visit_statement(method);
                }
            }
            IrStatement::TryWithResources {
                resources,
                body,
                catch_clauses,
                finally_block,
                ..
            } => {
                for resource in resources {
                    self.visit_expression(&mut resource.initializer, None);
                }
                self.visit_statement(body);
                for clause in catch_clauses {
                    self.visit_statement(&mut clause.body);
                }
                if let Some(finally_stmt) = finally_block {
                    self.visit_statement(finally_stmt);
                }
            }
            _ => {}
        }
    }

    fn visit_expression(&mut self, expr: &mut IrExpression, expected: Option<&JavaType>) {
        if let Some(expected_type) = expected {
            if Self::is_list_type(expected_type) {
                if let IrExpression::SequencePipeline {
                    pipeline,
                    java_type,
                    ..
                } = expr
                {
                    if pipeline.terminal.is_none() {
                        let terminal = SequenceTerminal {
                            kind: SequenceTerminalKind::ToList,
                            evaluation: SequenceTerminalEvaluation::Collector,
                            requires_non_empty_source: false,
                            specialization_hint: None,
                            canonical_adapter: None,
                            span: pipeline.span.clone(),
                        };
                        pipeline.terminal = Some(terminal);
                        pipeline.lazy = false;
                        pipeline.recompute_shape();
                    }
                    *java_type = expected_type.clone();
                }
            }
        }

        match expr {
            IrExpression::SequencePipeline { .. }
            | IrExpression::Literal(..)
            | IrExpression::RegexPattern { .. }
            | IrExpression::Identifier { .. }
            | IrExpression::InstanceOf { .. }
            | IrExpression::This { .. }
            | IrExpression::Super { .. } => {}
            IrExpression::CharToString(conversion) => {
                self.visit_expression(&mut conversion.value, None);
            }
            IrExpression::MethodCall { receiver, args, .. } => {
                if let Some(recv) = receiver.as_mut() {
                    self.visit_expression(recv, None);
                }
                for arg in args {
                    self.visit_expression(arg, None);
                }
            }
            IrExpression::FieldAccess { receiver, .. } => {
                self.visit_expression(receiver, None);
            }
            IrExpression::ArrayAccess { array, index, .. } => {
                self.visit_expression(array, None);
                self.visit_expression(index, None);
            }
            IrExpression::Binary { left, right, .. } => {
                self.visit_expression(left, None);
                self.visit_expression(right, None);
            }
            IrExpression::RegexMatch { subject, pattern, .. } => {
                self.visit_expression(subject, None);
                self.visit_expression(pattern, None);
            }
            IrExpression::Unary { operand, .. } => {
                self.visit_expression(operand, None);
            }
            IrExpression::Assignment { target, value, .. } => {
                let expected_type = extract_java_type(target);
                let expected_ref = expected_type.as_ref();
                self.visit_expression(value, expected_ref);
                self.visit_expression(target, None);
            }
            IrExpression::Conditional {
                condition,
                then_expr,
                else_expr,
                java_type,
                ..
            } => {
                self.visit_expression(condition, None);
                self.visit_expression(then_expr, Some(java_type));
                self.visit_expression(else_expr, Some(java_type));
            }
            IrExpression::Block { statements, .. } => {
                for stmt in statements {
                    self.visit_statement(stmt);
                }
            }
            IrExpression::ArrayCreation { initializer, .. } => {
                if let Some(values) = initializer {
                    for value in values {
                        self.visit_expression(value, None);
                    }
                }
            }
            IrExpression::ObjectCreation { args, .. } => {
                for arg in args {
                    self.visit_expression(arg, None);
                }
            }
            IrExpression::Lambda { body, .. } => {
                self.visit_expression(body, None);
            }
            IrExpression::Switch {
                discriminant,
                cases,
                ..
            } => {
                self.visit_expression(discriminant, None);
                for case in cases {
                    self.visit_expression(&mut case.body, None);
                }
            }
            IrExpression::Cast {
                expr: inner,
                target_type,
                ..
            } => {
                self.visit_expression(inner, Some(target_type));
            }
            IrExpression::TryWithResources {
                resources, body, ..
            } => {
                for resource in resources {
                    self.visit_expression(&mut resource.initializer, None);
                }
                self.visit_expression(body, None);
            }
            IrExpression::CompletableFuture { args, .. }
            | IrExpression::VirtualThread { args, .. } => {
                for arg in args {
                    self.visit_expression(arg, None);
                }
            }
            IrExpression::NullSafeOperation {
                expr,
                operation,
                default_value,
                ..
            } => {
                self.visit_expression(expr, None);
                self.visit_expression(operation, None);
                if let Some(default) = default_value {
                    self.visit_expression(default, None);
                }
            }
            IrExpression::StringFormat { args, .. } => {
                for arg in args {
                    self.visit_expression(arg, None);
                }
            }
        }
    }

    fn is_list_type(java_type: &JavaType) -> bool {
        match java_type {
            JavaType::Reference { name, .. } => matches!(name.as_str(), "java.util.List" | "List"),
            _ => false,
        }
    }
}
