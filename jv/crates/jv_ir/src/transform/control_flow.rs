use super::transform_expression;
use super::utils::extract_java_type;
use crate::context::TransformContext;
use crate::error::TransformError;
use crate::types::{IrCaseLabel, IrExpression, IrImplicitWhenEnd, IrSwitchCase, JavaType};
use jv_ast::{Expression, ImplicitWhenEnd, Pattern, Span, WhenArm};

pub fn desugar_when_expression(
    expr: Option<Box<Expression>>,
    arms: Vec<WhenArm>,
    else_arm: Option<Box<Expression>>,
    implicit_end: Option<ImplicitWhenEnd>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let subject_expr = expr.ok_or_else(|| TransformError::UnsupportedConstruct {
        construct: "when expressions without subject".to_string(),
        span: span.clone(),
    })?;

    let discriminant = transform_expression(*subject_expr, context)?;

    let mut cases = Vec::new();
    let mut result_type: Option<JavaType> = None;
    let mut has_default_case = false;

    let mut implicit_ir_end = implicit_end.map(convert_implicit_end);

    for arm in arms {
        let (mut labels, is_default) = convert_when_pattern(&arm.pattern, arm.span.clone())?;

        if is_default {
            if has_default_case {
                return Err(TransformError::UnsupportedConstruct {
                    construct: "Multiple default patterns in when expression".to_string(),
                    span: arm.span,
                });
            }
            has_default_case = true;
            if labels.is_empty() {
                labels.push(IrCaseLabel::Default);
            }
        }

        let guard = match arm.guard {
            Some(guard_expr) => Some(transform_expression(guard_expr, context)?),
            None => None,
        };

        let body = transform_expression(arm.body, context)?;
        if result_type.is_none() {
            result_type = extract_java_type(&body);
        }

        cases.push(IrSwitchCase {
            labels,
            guard,
            body,
            span: arm.span,
        });
    }

    if let Some(else_expr) = else_arm {
        if has_default_case {
            return Err(TransformError::UnsupportedConstruct {
                construct: "when expression cannot have both default pattern and else arm"
                    .to_string(),
                span,
            });
        }

        let body = transform_expression(*else_expr, context)?;
        if result_type.is_none() {
            result_type = extract_java_type(&body);
        }

        cases.push(IrSwitchCase {
            labels: vec![IrCaseLabel::Default],
            guard: None,
            body,
            span: span.clone(),
        });
        implicit_ir_end = None;
    }

    if cases.is_empty() {
        return Err(TransformError::UnsupportedConstruct {
            construct: "when expression must have at least one arm".to_string(),
            span,
        });
    }

    let java_type = result_type.unwrap_or_else(JavaType::object);

    Ok(IrExpression::Switch {
        discriminant: Box::new(discriminant),
        cases,
        java_type,
        implicit_end: implicit_ir_end,
        span,
    })
}

fn convert_implicit_end(end: ImplicitWhenEnd) -> IrImplicitWhenEnd {
    match end {
        ImplicitWhenEnd::Unit { span } => IrImplicitWhenEnd::Unit { span },
    }
}

fn convert_when_pattern(
    pattern: &Pattern,
    span: Span,
) -> Result<(Vec<IrCaseLabel>, bool), TransformError> {
    match pattern {
        Pattern::Wildcard(_) => Ok((vec![IrCaseLabel::Default], true)),
        Pattern::Literal(literal, _) => Ok((vec![IrCaseLabel::Literal(literal.clone())], false)),
        Pattern::Identifier(_, _)
        | Pattern::Constructor { .. }
        | Pattern::Range { .. }
        | Pattern::Guard { .. } => Err(TransformError::UnsupportedConstruct {
            construct: "Unsupported when pattern".to_string(),
            span,
        }),
    }
}
