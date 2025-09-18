use super::transform_expression;
use super::utils::extract_java_type;
use crate::context::TransformContext;
use crate::error::TransformError;
use crate::types::{CompletableFutureOp, IrExpression, JavaType, VirtualThreadOp};
use jv_ast::{Expression, Span};

pub fn desugar_spawn_expression(
    body: Box<Expression>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let body_ir = transform_expression(*body, context)?;

    let lambda = IrExpression::Lambda {
        functional_interface: "Runnable".to_string(),
        param_names: Vec::new(),
        param_types: Vec::new(),
        body: Box::new(body_ir),
        java_type: JavaType::Reference {
            name: "Runnable".to_string(),
            generic_args: vec![],
        },
        span: span.clone(),
    };

    Ok(IrExpression::VirtualThread {
        operation: VirtualThreadOp::Start,
        args: vec![lambda],
        java_type: JavaType::Reference {
            name: "Thread".to_string(),
            generic_args: vec![],
        },
        span,
    })
}

pub fn desugar_async_expression(
    body: Box<Expression>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let body_ir = transform_expression(*body, context)?;
    let result_type = extract_java_type(&body_ir).unwrap_or_else(JavaType::object);

    let lambda = IrExpression::Lambda {
        functional_interface: "Supplier".to_string(),
        param_names: Vec::new(),
        param_types: Vec::new(),
        body: Box::new(body_ir),
        java_type: JavaType::Reference {
            name: "Supplier".to_string(),
            generic_args: vec![result_type.clone()],
        },
        span: span.clone(),
    };

    Ok(IrExpression::CompletableFuture {
        operation: CompletableFutureOp::SupplyAsync,
        args: vec![lambda],
        java_type: JavaType::Reference {
            name: "CompletableFuture".to_string(),
            generic_args: vec![result_type],
        },
        span,
    })
}

pub fn desugar_await_expression(
    expr: Box<Expression>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let future_ir = transform_expression(*expr, context)?;

    let awaited_type = match extract_java_type(&future_ir) {
        Some(JavaType::Reference { name, generic_args })
            if name == "CompletableFuture" && !generic_args.is_empty() =>
        {
            generic_args[0].clone()
        }
        _ => JavaType::object(),
    };

    Ok(IrExpression::CompletableFuture {
        operation: CompletableFutureOp::Get,
        args: vec![future_ir],
        java_type: awaited_type,
        span,
    })
}
