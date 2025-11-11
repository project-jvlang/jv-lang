use crate::context::TransformContext;
use crate::error::{TestLoweringDiagnostic, TransformError};
use crate::transform::transform_expression;
use crate::transform::utils::{extract_java_type, ir_expression_span};
use crate::types::{AssertionPattern, IrExpression, IrStatement, JavaType};
use jv_ast::{BinaryOp, CallArgumentStyle, Expression, Span};

pub(super) fn lower_test_body(
    body: Expression,
    context: &mut TransformContext,
) -> Result<(IrExpression, Vec<AssertionPattern>), TransformError> {
    let lowered = transform_expression(body, context)?;
    apply_assertions(lowered)
}

fn apply_assertions(
    expr: IrExpression,
) -> Result<(IrExpression, Vec<AssertionPattern>), TransformError> {
    match expr {
        IrExpression::Block {
            statements,
            java_type,
            span,
        } => {
            let mut patterns = Vec::new();
            let mut rewritten = Vec::with_capacity(statements.len());

            for statement in statements {
                rewritten.push(rewrite_assertion_statement(statement, &mut patterns)?);
            }

            Ok((
                IrExpression::Block {
                    statements: rewritten,
                    java_type,
                    span,
                },
                patterns,
            ))
        }
        other => {
            let span = ir_expression_span(&other);
            let block = IrExpression::Block {
                statements: vec![IrStatement::Expression {
                    expr: other,
                    span: span.clone(),
                }],
                java_type: JavaType::void(),
                span,
            };
            apply_assertions(block)
        }
    }
}

fn rewrite_assertion_statement(
    statement: IrStatement,
    patterns: &mut Vec<AssertionPattern>,
) -> Result<IrStatement, TransformError> {
    match statement {
        IrStatement::Expression { expr, span } => match expr {
            IrExpression::Binary {
                left,
                right,
                op: BinaryOp::Equal,
                span: expr_span,
                ..
            } => {
                let actual = *left;
                let expected = *right;
                patterns.push(AssertionPattern::Equals {
                    actual: actual.clone(),
                    expected: expected.clone(),
                    span: expr_span.clone(),
                });

                Ok(IrStatement::Expression {
                    expr: build_assertion_call("assertEquals", vec![expected, actual], &span),
                    span,
                })
            }
            IrExpression::Binary {
                left,
                right,
                op: BinaryOp::NotEqual,
                span: expr_span,
                ..
            } => {
                let actual = *left;
                let expected = *right;
                patterns.push(AssertionPattern::NotEquals {
                    actual: actual.clone(),
                    expected: expected.clone(),
                    span: expr_span.clone(),
                });

                Ok(IrStatement::Expression {
                    expr: build_assertion_call("assertNotEquals", vec![expected, actual], &span),
                    span,
                })
            }
            other_expr => {
                let java_type = extract_java_type(&other_expr);
                let bool_like = java_type
                    .as_ref()
                    .map(|java_type| is_boolean_type(java_type))
                    .unwrap_or(false);

                if bool_like {
                    patterns.push(AssertionPattern::Truthy {
                        expr: other_expr.clone(),
                        span: span.clone(),
                    });
                    Ok(IrStatement::Expression {
                        expr: build_assertion_call("assertTrue", vec![other_expr], &span),
                        span,
                    })
                } else if java_type
                    .as_ref()
                    .map(|java_type| is_void_type(java_type))
                    .unwrap_or(true)
                {
                    Ok(IrStatement::Expression {
                        expr: other_expr,
                        span,
                    })
                } else {
                    Err(TransformError::TestLoweringError {
                        code: "JV5305",
                        message: "非boolean式は自動的にJUnitアサーションへ変換できません"
                            .to_string(),
                        span,
                        details: Some(TestLoweringDiagnostic::AssertionRewriteRequired),
                    })
                }
            }
        },
        other => Ok(other),
    }
}

fn build_assertion_call(method: &str, args: Vec<IrExpression>, span: &Span) -> IrExpression {
    IrExpression::MethodCall {
        receiver: Some(Box::new(assertions_identifier(span))),
        method_name: method.to_string(),
        java_name: None,
        resolved_target: None,
        args,
        argument_style: CallArgumentStyle::Comma,
        java_type: JavaType::void(),
        span: span.clone(),
    }
}

fn assertions_identifier(span: &Span) -> IrExpression {
    IrExpression::Identifier {
        name: "Assertions".to_string(),
        java_type: JavaType::Reference {
            name: "org.junit.jupiter.api.Assertions".to_string(),
            generic_args: vec![],
        },
        span: span.clone(),
    }
}

fn is_boolean_type(java_type: &JavaType) -> bool {
    match java_type {
        JavaType::Primitive(name) => name == "boolean",
        JavaType::Reference { name, .. } => name == "Boolean" || name == "java.lang.Boolean",
        _ => false,
    }
}

fn is_void_type(java_type: &JavaType) -> bool {
    matches!(java_type, JavaType::Void)
}
