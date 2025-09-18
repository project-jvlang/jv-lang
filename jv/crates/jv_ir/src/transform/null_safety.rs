use super::transform_expression;
use super::utils::extract_java_type;
use crate::context::TransformContext;
use crate::error::TransformError;
use crate::types::{IrExpression, JavaType};
use jv_ast::{Expression, Literal, Span};

pub fn desugar_null_safe_member_access(
    object: Box<Expression>,
    property: String,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let ir_object = transform_expression(*object, context)?;
    let object_type = extract_java_type(&ir_object).unwrap_or_else(JavaType::object);

    if matches!(object_type, JavaType::Primitive(_)) {
        return Err(TransformError::NullSafetyError {
            message: "Cannot apply null-safe access to primitive types".to_string(),
            span,
        });
    }

    let result_type = JavaType::object();
    let operation = IrExpression::FieldAccess {
        receiver: Box::new(ir_object.clone()),
        field_name: property,
        java_type: result_type.clone(),
        span: span.clone(),
    };

    let default_value = Some(Box::new(IrExpression::Literal(Literal::Null, span.clone())));

    Ok(IrExpression::NullSafeOperation {
        expr: Box::new(ir_object),
        operation: Box::new(operation),
        default_value,
        java_type: result_type,
        span,
    })
}

pub fn desugar_null_safe_index_access(
    object: Box<Expression>,
    index: Box<Expression>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let ir_object = transform_expression(*object, context)?;
    let object_type = extract_java_type(&ir_object).unwrap_or_else(JavaType::object);

    if matches!(object_type, JavaType::Primitive(_)) {
        return Err(TransformError::NullSafetyError {
            message: "Cannot apply null-safe index access to primitive types".to_string(),
            span,
        });
    }

    let ir_index = transform_expression(*index, context)?;

    let element_type = match &object_type {
        JavaType::Array { element_type, .. } => *element_type.clone(),
        _ => JavaType::object(),
    };

    let operation = IrExpression::ArrayAccess {
        array: Box::new(ir_object.clone()),
        index: Box::new(ir_index),
        java_type: element_type.clone(),
        span: span.clone(),
    };

    let default_value = if matches!(element_type, JavaType::Primitive(_)) {
        None
    } else {
        Some(Box::new(IrExpression::Literal(Literal::Null, span.clone())))
    };

    Ok(IrExpression::NullSafeOperation {
        expr: Box::new(ir_object),
        operation: Box::new(operation),
        default_value,
        java_type: element_type,
        span,
    })
}

pub fn desugar_elvis_operator(
    left: Box<Expression>,
    right: Box<Expression>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let left_expr = transform_expression(*left, context)?;
    let right_expr = transform_expression(*right, context)?;

    if let Some(JavaType::Primitive(_)) = extract_java_type(&left_expr) {
        return Err(TransformError::NullSafetyError {
            message: "Elvis operator requires nullable left-hand expression".to_string(),
            span,
        });
    }

    let left_type = extract_java_type(&left_expr);
    let right_type = extract_java_type(&right_expr);
    let result_type = left_type
        .or(right_type.clone())
        .unwrap_or_else(JavaType::object);

    Ok(IrExpression::NullSafeOperation {
        expr: Box::new(left_expr.clone()),
        operation: Box::new(left_expr),
        default_value: Some(Box::new(right_expr)),
        java_type: result_type,
        span,
    })
}
