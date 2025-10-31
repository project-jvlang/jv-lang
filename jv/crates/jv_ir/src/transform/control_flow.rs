use super::when_lowering_planner::WhenLoweringPlanner;
use crate::context::TransformContext;
use crate::error::TransformError;
use crate::types::IrExpression;
use jv_ast::{Expression, ImplicitWhenEnd, Span, WhenArm};

pub fn desugar_when_expression(
    expr: Option<Box<Expression>>,
    arms: Vec<WhenArm>,
    else_arm: Option<Box<Expression>>,
    implicit_end: Option<ImplicitWhenEnd>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    desugar_when_expression_with_label(expr, arms, else_arm, implicit_end, None, span, context)
}

pub fn desugar_when_expression_with_label(
    expr: Option<Box<Expression>>,
    arms: Vec<WhenArm>,
    else_arm: Option<Box<Expression>>,
    implicit_end: Option<ImplicitWhenEnd>,
    label: Option<String>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let recorded_span = span.clone();
    let planner = WhenLoweringPlanner::new(expr, arms, else_arm, implicit_end, label, span);
    let plan = planner.plan(context)?;
    context.record_when_strategy(recorded_span, plan.description.clone());
    Ok(plan.ir)
}
