use crate::context::TransformContext;
use crate::error::TransformError;
use crate::types::{IrExpression, IrProgram, IrStatement, JavaType};
use jv_ast::{
    BinaryOp, ConcurrencyConstruct, Expression, Literal, Program, ResourceManagement, Span,
    Statement,
};

mod concurrency;
mod control_flow;
mod declarations;
mod functions;
mod null_safety;
mod resources;
mod sample;
mod strings;
mod type_system;
mod utils;

pub use crate::types::{
    DataFormat, IrSampleDeclaration, PrimitiveType, SampleFetchError, SampleFetchRequest,
    SampleFetchResult, SampleMode, SampleRecordDescriptor, SampleRecordField, SampleSourceKind,
    Schema, SchemaError,
};
pub use concurrency::{
    desugar_async_expression, desugar_await_expression, desugar_spawn_expression,
};
pub use control_flow::desugar_when_expression;
pub use declarations::{
    desugar_data_class, desugar_extension_function, desugar_val_declaration,
    desugar_var_declaration,
};
pub use functions::{
    desugar_default_parameters, desugar_named_arguments, desugar_top_level_function,
};
pub use null_safety::{
    desugar_elvis_operator, desugar_null_safe_index_access, desugar_null_safe_member_access,
};
pub use resources::{desugar_defer_expression, desugar_use_expression};
pub use sample::{fetch_sample_data, infer_schema};
pub use strings::desugar_string_interpolation;
pub use type_system::{convert_type_annotation, infer_java_type};
pub use utils::{generate_extension_class_name, generate_utility_class_name};

pub fn transform_program(program: Program) -> Result<IrProgram, TransformError> {
    let mut context = TransformContext::new();
    transform_program_with_context(program, &mut context)
}

pub fn transform_program_with_context(
    program: Program,
    context: &mut TransformContext,
) -> Result<IrProgram, TransformError> {
    let mut ir_statements = Vec::new();

    for stmt in program.statements {
        let mut transformed = transform_statement(stmt, context)?;
        ir_statements.append(&mut transformed);
    }

    Ok(IrProgram {
        package: program.package,
        imports: Vec::new(),
        type_declarations: ir_statements,
        span: program.span,
    })
}

pub fn transform_statement(
    stmt: Statement,
    context: &mut TransformContext,
) -> Result<Vec<IrStatement>, TransformError> {
    match stmt {
        Statement::ValDeclaration {
            name,
            type_annotation,
            initializer,
            modifiers,
            span,
        } => Ok(vec![desugar_val_declaration(
            name,
            type_annotation,
            initializer,
            modifiers,
            span,
            context,
        )?]),
        Statement::VarDeclaration {
            name,
            type_annotation,
            initializer,
            modifiers,
            span,
        } => Ok(vec![desugar_var_declaration(
            name,
            type_annotation,
            initializer,
            modifiers,
            span,
            context,
        )?]),
        Statement::Expression { expr, span } => {
            let ir_expr = transform_expression(expr, context)?;
            Ok(vec![IrStatement::Expression {
                expr: ir_expr,
                span,
            }])
        }
        Statement::Concurrency(construct) => match construct {
            ConcurrencyConstruct::Spawn { body, span } => {
                let expression_span = span.clone();
                let ir_expr = desugar_spawn_expression(body, span, context)?;
                Ok(vec![IrStatement::Expression {
                    expr: ir_expr,
                    span: expression_span,
                }])
            }
            ConcurrencyConstruct::Async { body, span } => {
                let expression_span = span.clone();
                let ir_expr = desugar_async_expression(body, span, context)?;
                Ok(vec![IrStatement::Expression {
                    expr: ir_expr,
                    span: expression_span,
                }])
            }
            ConcurrencyConstruct::Await { expr, span } => {
                let expression_span = span.clone();
                let ir_expr = desugar_await_expression(expr, span, context)?;
                Ok(vec![IrStatement::Expression {
                    expr: ir_expr,
                    span: expression_span,
                }])
            }
        },
        Statement::ResourceManagement(management) => match management {
            ResourceManagement::Use {
                resource,
                body,
                span,
            } => {
                let expression_span = span.clone();
                let ir_expr = desugar_use_expression(resource, body, span, context)?;
                Ok(vec![IrStatement::Expression {
                    expr: ir_expr,
                    span: expression_span,
                }])
            }
            ResourceManagement::Defer { body, span } => {
                let expression_span = span.clone();
                let ir_expr = desugar_defer_expression(body, span, context)?;
                Ok(vec![IrStatement::Expression {
                    expr: ir_expr,
                    span: expression_span,
                }])
            }
        },
        _ => Ok(vec![]),
    }
}

pub fn transform_expression(
    expr: Expression,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    match expr {
        Expression::Literal(lit, span) => Ok(IrExpression::Literal(lit, span)),
        Expression::Identifier(name, span) => {
            if let Some(java_type) = context.lookup_variable(&name).cloned() {
                Ok(IrExpression::Identifier {
                    name,
                    java_type,
                    span,
                })
            } else {
                Err(TransformError::ScopeError {
                    message: format!("Unknown identifier '{name}'"),
                    span,
                })
            }
        }
        Expression::NullSafeMemberAccess {
            object,
            property,
            span,
        } => desugar_null_safe_member_access(object, property, span, context),
        Expression::NullSafeIndexAccess {
            object,
            index,
            span,
        } => desugar_null_safe_index_access(object, index, span, context),
        Expression::StringInterpolation { parts, span } => {
            desugar_string_interpolation(parts, span, context)
        }
        Expression::Block { statements, span } => {
            context.enter_scope();
            let mut ir_statements = Vec::new();
            for stmt in statements {
                let mut transformed = transform_statement(stmt, context)?;
                ir_statements.append(&mut transformed);
            }
            context.exit_scope();
            Ok(IrExpression::Block {
                statements: ir_statements,
                java_type: JavaType::void(),
                span,
            })
        }
        Expression::This(span) => {
            if let Some(java_type) = context.lookup_variable("this").cloned() {
                Ok(IrExpression::This { java_type, span })
            } else {
                Err(TransformError::ScopeError {
                    message: "'this' is not available in the current context".to_string(),
                    span,
                })
            }
        }
        Expression::Binary {
            left,
            op,
            right,
            span,
        } => {
            if matches!(op, BinaryOp::Elvis) {
                desugar_elvis_operator(left, right, span, context)
            } else {
                let left_ir = transform_expression(*left, context)?;
                let right_ir = transform_expression(*right, context)?;
                let java_type = JavaType::Primitive("int".to_string());
                Ok(IrExpression::Binary {
                    left: Box::new(left_ir),
                    op,
                    right: Box::new(right_ir),
                    java_type,
                    span,
                })
            }
        }
        _ => Ok(IrExpression::Literal(Literal::Null, Span::default())),
    }
}
