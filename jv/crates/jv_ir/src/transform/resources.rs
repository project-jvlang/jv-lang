use super::transform_expression;
use super::type_system::convert_type_annotation;
use super::utils::extract_java_type;
use crate::context::TransformContext;
use crate::error::TransformError;
use crate::types::{IrExpression, IrResource, IrStatement, JavaType};
use jv_ast::{Expression, Span};

pub fn desugar_use_expression(
    resource: Box<Expression>,
    body: Box<Expression>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let resource_ir = transform_expression(*resource, context)?;
    let mut resource_name = match &resource_ir {
        IrExpression::Identifier { name, .. } => name.clone(),
        _ => "__resource".to_string(),
    };
    let mut resource_type = extract_java_type(&resource_ir).unwrap_or_else(JavaType::object);

    let body_ir = match *body {
        Expression::Lambda {
            parameters,
            body: lambda_body,
            ..
        } => {
            let param = parameters.get(0);
            if let Some(param) = param {
                resource_name = param.name.clone();
                if let Some(annotation) = &param.type_annotation {
                    resource_type = convert_type_annotation(annotation.clone())?;
                }
            }
            context.enter_scope();
            context.add_variable(resource_name.clone(), resource_type.clone());
            let body_ir = transform_expression(*lambda_body, context)?;
            context.exit_scope();
            body_ir
        }
        other => transform_expression(other, context)?,
    };

    let result_type = extract_java_type(&body_ir).unwrap_or_else(JavaType::void);

    Ok(IrExpression::TryWithResources {
        resources: vec![IrResource {
            name: resource_name,
            initializer: resource_ir,
            java_type: resource_type,
            span: span.clone(),
        }],
        body: Box::new(body_ir),
        java_type: result_type,
        span,
    })
}

pub fn desugar_defer_expression(
    body: Box<Expression>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let body_ir = transform_expression(*body, context)?;

    Ok(IrExpression::Block {
        statements: vec![IrStatement::Expression {
            expr: body_ir,
            span: span.clone(),
        }],
        java_type: JavaType::void(),
        span,
    })
}
