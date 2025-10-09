use crate::context::TransformContext;
use crate::error::TransformError;
use crate::transform::transform_expression;
use crate::types::{IrExpression, JavaType};
use jv_ast::{Argument, CallArgumentMetadata, Expression, Span};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SequencePipeline {
    pub source: SequenceSource,
    pub stages: Vec<SequenceStage>,
    pub terminal: Option<SequenceTerminal>,
    pub lazy: bool,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SequenceSource {
    Expression {
        expr: Box<IrExpression>,
        element_hint: Option<JavaType>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SequenceStage {
    Map { lambda: Box<IrExpression>, span: Span },
    Filter { predicate: Box<IrExpression>, span: Span },
    FlatMap { lambda: Box<IrExpression>, span: Span },
    Take { count: Box<IrExpression>, span: Span },
    Drop { count: Box<IrExpression>, span: Span },
    Sorted {
        comparator: Option<Box<IrExpression>>,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SequenceTerminal {
    pub kind: SequenceTerminalKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SequenceTerminalKind {
    ToList,
    ToSet,
    Count,
    Sum,
    ForEach { action: Box<IrExpression> },
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

    let source_ir = transform_expression(source_expr, context)?;
    let mut pipeline = SequencePipeline {
        source: SequenceSource::Expression {
            expr: Box::new(source_ir),
            element_hint: None,
        },
        stages: Vec::new(),
        terminal: None,
        lazy: true,
        span: span.clone(),
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
                if pipeline.stages.is_empty() || pipeline.terminal.is_some() {
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

    if pipeline.stages.is_empty() {
        return Ok(None);
    }

    let java_type = determine_java_type(pipeline.terminal.as_ref());

    Ok(Some(IrExpression::SequencePipeline {
        pipeline,
        java_type,
        span,
    }))
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
        TerminalKind::ToList if segment.args.is_empty() => Ok(SequenceTerminal {
            kind: SequenceTerminalKind::ToList,
            span: segment.span.clone(),
        }),
        TerminalKind::ToSet if segment.args.is_empty() => Ok(SequenceTerminal {
            kind: SequenceTerminalKind::ToSet,
            span: segment.span.clone(),
        }),
        TerminalKind::Count if segment.args.is_empty() => Ok(SequenceTerminal {
            kind: SequenceTerminalKind::Count,
            span: segment.span.clone(),
        }),
        TerminalKind::Sum if segment.args.is_empty() => Ok(SequenceTerminal {
            kind: SequenceTerminalKind::Sum,
            span: segment.span.clone(),
        }),
        TerminalKind::ForEach => {
            let action = expect_single_positional(&segment.args)?;
            let action_ir = transform_expression(action, context)?;
            Ok(SequenceTerminal {
                kind: SequenceTerminalKind::ForEach {
                    action: Box::new(action_ir),
                },
                span: segment.span.clone(),
            })
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

fn determine_java_type(terminal: Option<&SequenceTerminal>) -> JavaType {
    match terminal.map(|t| &t.kind) {
        Some(SequenceTerminalKind::ToList) => JavaType::Reference {
            name: "java.util.List".to_string(),
            generic_args: vec![],
        },
        Some(SequenceTerminalKind::ToSet) => JavaType::Reference {
            name: "java.util.Set".to_string(),
            generic_args: vec![],
        },
        Some(SequenceTerminalKind::Count) | Some(SequenceTerminalKind::Sum) => {
            JavaType::Primitive("long".to_string())
        }
        Some(SequenceTerminalKind::ForEach { .. }) => JavaType::void(),
        None => JavaType::Reference {
            name: "jv.collections.Sequence".to_string(),
            generic_args: vec![],
        },
    }
}

fn expect_single_positional(args: &[Argument]) -> Result<Expression, TransformError> {
    if args.len() != 1 {
        return Err(TransformError::TypeInferenceError {
            message: "Sequence stage expects exactly one argument".to_string(),
            span: args
                .first()
                .map(argument_span)
                .unwrap_or_else(Span::dummy),
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
