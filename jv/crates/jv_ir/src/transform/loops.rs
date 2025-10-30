use super::transform_expression;
use super::type_system::convert_type_annotation;
use super::utils::{extract_java_type, ir_expression_span};
use crate::context::TransformContext;
use crate::error::TransformError;
use crate::types::{
    IrExpression, IrForEachKind, IrForLoopMetadata, IrModifiers, IrNumericRangeLoop, IrStatement,
    JavaType,
};
use jv_ast::{BinaryOp, Literal};
use jv_ast::{Expression, ForInStatement, LoopBinding, LoopStrategy, NumericRangeLoop, Span};

pub fn desugar_for_in_statement(
    for_in: ForInStatement,
    context: &mut TransformContext,
) -> Result<Vec<IrStatement>, TransformError> {
    let ForInStatement {
        binding,
        iterable,
        strategy,
        body,
        span,
        label,
    } = for_in;

    match strategy {
        LoopStrategy::NumericRange(range) => {
            lower_numeric_range(binding, *body, range, label, span, context)
        }
        LoopStrategy::Iterable | LoopStrategy::Unknown => {
            lower_iterable(binding, iterable, *body, label, span, context)
        }
        LoopStrategy::LazySequence { needs_cleanup } => lower_lazy_sequence(
            binding,
            iterable,
            *body,
            label,
            span,
            needs_cleanup,
            context,
        ),
    }
}

fn lower_numeric_range(
    binding: LoopBinding,
    body_expr: Expression,
    range: NumericRangeLoop,
    label: Option<String>,
    span: Span,
    context: &mut TransformContext,
) -> Result<Vec<IrStatement>, TransformError> {
    context.enter_scope();
    let result = (|| -> Result<Vec<IrStatement>, TransformError> {
        let mut statements = Vec::new();

        let end_ir = transform_expression(range.end, context)?;
        let end_type = extract_java_type(&end_ir).unwrap_or_else(JavaType::int);

        let end_var_name = context.fresh_identifier("__jv_range_end_");
        statements.push(IrStatement::VariableDeclaration {
            name: end_var_name.clone(),
            java_type: end_type.clone(),
            initializer: Some(end_ir),
            is_final: true,
            modifiers: IrModifiers::default(),
            span: range.span.clone(),
        });
        context.add_variable(end_var_name.clone(), end_type.clone());

        let start_ir = transform_expression(range.start, context)?;
        let binding_type = resolve_binding_type(&binding, Some(&start_ir), JavaType::int())?;
        let binding_name = binding.name.clone();
        let binding_span = binding.span.clone();

        let body_stmt = {
            context.enter_scope();
            context.add_variable(binding_name.clone(), binding_type.clone());
            let result = lower_loop_body(body_expr, context);
            context.exit_scope();
            result?
        };

        let condition_op = if range.inclusive {
            BinaryOp::LessEqual
        } else {
            BinaryOp::Less
        };

        let condition_expr = IrExpression::Binary {
            left: Box::new(make_identifier(
                &binding_name,
                &binding_type,
                binding_span.clone(),
            )),
            op: condition_op,
            right: Box::new(make_identifier(
                &end_var_name,
                &end_type,
                range.span.clone(),
            )),
            java_type: JavaType::boolean(),
            span: range.span.clone(),
        };

        let update_expr = IrExpression::Assignment {
            target: Box::new(make_identifier(
                &binding_name,
                &binding_type,
                binding_span.clone(),
            )),
            value: Box::new(IrExpression::Binary {
                left: Box::new(make_identifier(
                    &binding_name,
                    &binding_type,
                    binding_span.clone(),
                )),
                op: BinaryOp::Add,
                right: Box::new(IrExpression::Literal(
                    Literal::Number("1".to_string()),
                    range.span.clone(),
                )),
                java_type: binding_type.clone(),
                span: range.span.clone(),
            }),
            java_type: binding_type.clone(),
            span: range.span.clone(),
        };

        let init_stmt = IrStatement::VariableDeclaration {
            name: binding_name.clone(),
            java_type: binding_type.clone(),
            initializer: Some(start_ir),
            is_final: false,
            modifiers: IrModifiers::default(),
            span: binding_span.clone(),
        };

        let for_stmt = IrStatement::For {
            label: label.clone(),
            init: Some(Box::new(init_stmt)),
            condition: Some(condition_expr),
            update: Some(update_expr),
            body: Box::new(body_stmt),
            metadata: Some(IrForLoopMetadata::NumericRange(IrNumericRangeLoop {
                binding: binding_name,
                binding_type: binding_type.clone(),
                end_variable: end_var_name,
                end_type: end_type,
                inclusive: range.inclusive,
                span: range.span.clone(),
            })),
            span: span.clone(),
        };

        statements.push(for_stmt);

        Ok(vec![IrStatement::Block {
            label: None,
            statements,
            span: span.clone(),
        }])
    })();
    context.exit_scope();
    result
}

fn lower_iterable(
    binding: LoopBinding,
    iterable_expr: Expression,
    body_expr: Expression,
    label: Option<String>,
    span: Span,
    context: &mut TransformContext,
) -> Result<Vec<IrStatement>, TransformError> {
    let iterable_ir = transform_expression(iterable_expr, context)?;
    let binding_type = resolve_binding_type(&binding, None, JavaType::object())?;
    let binding_name = binding.name.clone();

    let body_stmt = {
        context.enter_scope();
        context.add_variable(binding_name.clone(), binding_type.clone());
        let result = lower_loop_body(body_expr, context);
        context.exit_scope();
        result?
    };

    Ok(vec![IrStatement::ForEach {
        label,
        variable: binding_name,
        variable_type: binding_type,
        iterable: iterable_ir,
        body: Box::new(body_stmt),
        iterable_kind: IrForEachKind::Iterable,
        span,
    }])
}

fn lower_lazy_sequence(
    binding: LoopBinding,
    iterable_expr: Expression,
    body_expr: Expression,
    label: Option<String>,
    span: Span,
    needs_cleanup: bool,
    context: &mut TransformContext,
) -> Result<Vec<IrStatement>, TransformError> {
    context.enter_scope();
    let result = (|| -> Result<Vec<IrStatement>, TransformError> {
        let mut statements = Vec::new();

        let iterable_ir = transform_expression(iterable_expr, context)?;
        let iterable_type = extract_java_type(&iterable_ir).unwrap_or_else(JavaType::object);
        let temp_name = context.fresh_identifier("__jv_iterable_");

        statements.push(IrStatement::VariableDeclaration {
            name: temp_name.clone(),
            java_type: iterable_type.clone(),
            initializer: Some(iterable_ir),
            is_final: true,
            modifiers: IrModifiers::default(),
            span: span.clone(),
        });
        context.add_variable(temp_name.clone(), iterable_type.clone());

        let binding_type = resolve_binding_type(&binding, None, JavaType::object())?;
        let binding_name = binding.name.clone();
        let binding_span = binding.span.clone();

        let body_stmt = {
            context.enter_scope();
            context.add_variable(binding_name.clone(), binding_type.clone());
            let result = lower_loop_body(body_expr, context);
            context.exit_scope();
            result?
        };

        statements.push(IrStatement::ForEach {
            label: label.clone(),
            variable: binding_name,
            variable_type: binding_type,
            iterable: make_identifier(&temp_name, &iterable_type, binding_span.clone()),
            body: Box::new(body_stmt),
            iterable_kind: IrForEachKind::LazySequence { needs_cleanup },
            span: span.clone(),
        });

        Ok(vec![IrStatement::Block {
            label: None,
            statements,
            span: span.clone(),
        }])
    })();
    context.exit_scope();
    result
}

fn lower_loop_body(
    body_expr: Expression,
    context: &mut TransformContext,
) -> Result<IrStatement, TransformError> {
    let ir_expr = transform_expression(body_expr, context)?;
    Ok(match ir_expr {
        IrExpression::Block {
            statements, span, ..
        } => IrStatement::Block {
            label: None,
            statements,
            span,
        },
        other => {
            let span = ir_expression_span(&other);
            IrStatement::Expression { expr: other, span }
        }
    })
}

fn resolve_binding_type(
    binding: &LoopBinding,
    fallback: Option<&IrExpression>,
    default_type: JavaType,
) -> Result<JavaType, TransformError> {
    if let Some(annotation) = &binding.type_annotation {
        return convert_type_annotation(annotation.clone());
    }

    if let Some(expr) = fallback {
        if let Some(java_type) = extract_java_type(expr) {
            return Ok(java_type);
        }
    }

    Ok(default_type)
}

fn make_identifier(name: &str, java_type: &JavaType, span: Span) -> IrExpression {
    IrExpression::Identifier {
        name: name.to_string(),
        java_type: java_type.clone(),
        span,
    }
}
