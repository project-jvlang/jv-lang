use super::transform_expression;
use crate::context::TransformContext;
use crate::error::TransformError;
use crate::sequence_pipeline::{
    SequenceTerminal, SequenceTerminalEvaluation, SequenceTerminalKind,
};
use crate::transform::utils::{extract_java_type, ir_expression_span};
use crate::types::{IrExpression, JavaType};
use jv_ast::{CallArgumentStyle, Literal, Span, StringPart};

pub fn desugar_string_interpolation(
    parts: Vec<StringPart>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let mut has_expression = false;
    let mut format_string = String::new();
    let mut raw_text = String::new();
    let mut args = Vec::new();

    for part in parts {
        match part {
            StringPart::Text(text) => {
                format_string.push_str(&escape_format_text(&text));
                raw_text.push_str(&text);
            }
            StringPart::Expression(expr) => {
                has_expression = true;
                format_string.push_str("%s");
                let ir_expr = transform_expression(expr, context)?;
                let ir_expr = materialize_sequence_if_needed(ir_expr);
                args.push(ir_expr);
            }
        }
    }

    if !has_expression {
        return Ok(IrExpression::Literal(Literal::String(raw_text), span));
    }

    Ok(IrExpression::StringFormat {
        format_string,
        args,
        span,
    })
}

fn escape_format_text(text: &str) -> String {
    text.replace('%', "%%")
}

fn materialize_sequence_if_needed(expr: IrExpression) -> IrExpression {
    let expr_span = ir_expression_span(&expr);
    let type_hint = extract_java_type(&expr);

    match expr {
        IrExpression::SequencePipeline {
            mut pipeline,
            java_type,
            span,
        } if is_sequence_type(&java_type) && pipeline.terminal.is_none() => {
            pipeline.terminal = Some(SequenceTerminal {
                kind: SequenceTerminalKind::ToList,
                evaluation: SequenceTerminalEvaluation::Collector,
                requires_non_empty_source: false,
                specialization_hint: None,
                span: span.clone(),
            });
            pipeline.lazy = false;
            pipeline.recompute_shape();
            IrExpression::SequencePipeline {
                pipeline,
                java_type: JavaType::list(),
                span,
            }
        }
        IrExpression::SequencePipeline {
            pipeline,
            java_type,
            span,
        } => IrExpression::SequencePipeline {
            pipeline,
            java_type,
            span,
        },
        other if type_hint.as_ref().is_some_and(is_sequence_type) => IrExpression::MethodCall {
            receiver: Some(Box::new(other)),
            method_name: "toList".to_string(),
            java_name: None,
            resolved_target: None,
            args: Vec::new(),
            argument_style: CallArgumentStyle::Comma,
            java_type: JavaType::list(),
            span: expr_span,
        },
        other => other,
    }
}

fn is_sequence_type(java_type: &JavaType) -> bool {
    matches!(
        java_type,
        JavaType::Reference { name, .. } if name == "jv.collections.SequenceCore"
    )
}
