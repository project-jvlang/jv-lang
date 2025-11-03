use crate::context::TransformContext;
use crate::error::TransformError;
use crate::naming::test_identifiers::{NormalizedName, normalize_dataset, normalize_method};
use crate::sequence_pipeline::TestSuitePlanner;
use crate::transform::transform_expression;
use crate::transform::type_system::convert_type_annotation;
use crate::transform::utils::{convert_modifiers, extract_java_type, ir_expression_span};
use crate::types::{
    AssertionPattern, IrAnnotation, IrAnnotationArgument, IrAnnotationValue, IrExpression,
    IrModifiers, IrParameter, IrStatement, IrVisibility, JavaType, Literal,
};
use jv_ast::types::Modifiers;
use jv_ast::{
    Annotation, AnnotationArgument, AnnotationName, AnnotationValue, BinaryOp, BindingPatternKind,
    CallArgumentStyle, Expression, Span, TestDataset, TestDatasetRow, TestDeclaration,
    TestParameter,
};

pub fn lower_test_declaration(
    declaration: TestDeclaration,
    context: &mut TransformContext,
) -> Result<Vec<IrStatement>, TransformError> {
    let normalized_method = normalize_method(&declaration.display_name, &declaration.span);
    let class_name = derive_class_name(&normalized_method, &declaration.span);
    let mut planner = TestSuitePlanner::new(class_name, declaration.span.clone());

    let (parameters, scope_guard) = lower_test_parameters(&declaration.parameters, context)?;
    let (body_ir, assertion_patterns) = lower_test_body(declaration.body, context)?;
    drop(scope_guard);

    let method_name = normalized_method.identifier().to_string();
    let mut method_modifiers = base_method_modifiers(&declaration.annotations);
    method_modifiers.visibility = IrVisibility::Public;

    let mut annotations = vec![display_name_annotation(
        &declaration.display_name,
        &declaration.span,
    )];

    let dataset = lower_dataset(&declaration, &parameters, context, &method_name)?;
    annotations.extend(dataset.annotations);
    method_modifiers.annotations.extend(annotations);

    let method = IrStatement::MethodDeclaration {
        name: method_name,
        java_name: None,
        type_parameters: Vec::new(),
        parameters,
        primitive_return: None,
        return_type: JavaType::void(),
        body: Some(body_ir),
        modifiers: method_modifiers,
        throws: Vec::new(),
        assertion_patterns,
        span: declaration.span.clone(),
    };

    planner.push_method(method);
    if let Some(provider) = dataset.provider {
        planner.push_method(provider);
    }
    if let Some(sample) = dataset.sample_declaration {
        planner.push_sample(sample);
    }

    Ok(planner.build())
}

struct DatasetArtifacts {
    annotations: Vec<IrAnnotation>,
    provider: Option<IrStatement>,
    sample_declaration: Option<IrStatement>,
}

fn lower_dataset(
    declaration: &TestDeclaration,
    parameters: &[IrParameter],
    context: &mut TransformContext,
    method_name: &str,
) -> Result<DatasetArtifacts, TransformError> {
    match &declaration.dataset {
        None => {
            if !parameters.is_empty() {
                return Err(TransformError::TestLoweringError {
                    code: "JV5303",
                    message: "テストパラメータを使用する場合はデータセットを指定してください"
                        .to_string(),
                    span: declaration.span.clone(),
                });
            }

            Ok(DatasetArtifacts {
                annotations: vec![test_annotation(&declaration.span)],
                provider: None,
                sample_declaration: None,
            })
        }
        Some(TestDataset::InlineArray { rows, span }) => {
            if rows.is_empty() {
                return Err(TransformError::TestLoweringError {
                    code: "JV5304",
                    message: "データセットに少なくとも1行は必要です".to_string(),
                    span: span.clone(),
                });
            }

            let dataset_name = normalize_dataset(&declaration.display_name, &declaration.span);
            let provider_name = dataset_name.identifier().to_string();
            let provider =
                build_inline_dataset_provider(rows, parameters, context, &provider_name, span)?;

            Ok(DatasetArtifacts {
                annotations: vec![
                    parameterized_annotation(&declaration.span),
                    method_source_annotation(&provider_name, &declaration.span),
                ],
                provider: Some(provider),
                sample_declaration: None,
            })
        }
        Some(TestDataset::Sample(metadata)) => Err(TransformError::TestLoweringError {
            code: "JV5306",
            message: "@Sample を利用したデータセットのローワリングは未実装です".to_string(),
            span: metadata.span.clone(),
        }),
    }
}

fn base_method_modifiers(user_annotations: &[Annotation]) -> IrModifiers {
    if user_annotations.is_empty() {
        return IrModifiers::default();
    }

    let mut modifiers = Modifiers::default();
    modifiers.annotations = user_annotations.to_vec();
    convert_modifiers(&modifiers)
}

fn lower_test_parameters<'ctx>(
    parameters: &[TestParameter],
    context: &'ctx mut TransformContext,
) -> Result<(Vec<IrParameter>, ParameterScope<'ctx>), TransformError> {
    let guard = ParameterScope::new(context);
    let mut ir_parameters = Vec::with_capacity(parameters.len());

    for parameter in parameters {
        let name = match &parameter.pattern {
            BindingPatternKind::Identifier { name, .. } => name.clone(),
            BindingPatternKind::Wildcard { span, .. } => {
                return Err(TransformError::TestLoweringError {
                    code: "JV5302",
                    message: "ワイルドカードのテストパラメータはサポートされていません".to_string(),
                    span: span.clone(),
                });
            }
            other => {
                return Err(TransformError::TestLoweringError {
                    code: "JV5302",
                    message: format!(
                        "複雑なテストパラメータパターン '{other:?}' はまだサポートされていません"
                    ),
                    span: parameter.span.clone(),
                });
            }
        };

        let java_type = match &parameter.type_annotation {
            Some(annotation) => convert_type_annotation(annotation.clone())?,
            None => JavaType::object(),
        };

        guard.context.add_variable(name.clone(), java_type.clone());

        ir_parameters.push(IrParameter {
            name,
            java_type,
            modifiers: IrModifiers::default(),
            span: parameter.span.clone(),
        });
    }

    Ok((ir_parameters, guard))
}

fn lower_test_body(
    body: Expression,
    context: &mut TransformContext,
) -> Result<(IrExpression, Vec<AssertionPattern>), TransformError> {
    let lowered = transform_expression(body, context)?;
    Ok(apply_assertions(lowered))
}

fn apply_assertions(expr: IrExpression) -> (IrExpression, Vec<AssertionPattern>) {
    match expr {
        IrExpression::Block {
            mut statements,
            java_type,
            span,
        } => {
            let mut patterns = Vec::new();
            let mut rewritten = Vec::with_capacity(statements.len());

            for statement in statements {
                rewritten.push(rewrite_assertion_statement(statement, &mut patterns));
            }

            (
                IrExpression::Block {
                    statements: rewritten,
                    java_type,
                    span,
                },
                patterns,
            )
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
) -> IrStatement {
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

                IrStatement::Expression {
                    expr: build_assertion_call("assertEquals", vec![expected, actual], &span),
                    span,
                }
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

                IrStatement::Expression {
                    expr: build_assertion_call("assertNotEquals", vec![expected, actual], &span),
                    span,
                }
            }
            other_expr => {
                let bool_like = extract_java_type(&other_expr)
                    .map(is_boolean_type)
                    .unwrap_or(false);

                if bool_like {
                    patterns.push(AssertionPattern::Truthy {
                        expr: other_expr.clone(),
                        span: span.clone(),
                    });
                    IrStatement::Expression {
                        expr: build_assertion_call("assertTrue", vec![other_expr], &span),
                        span,
                    }
                } else {
                    IrStatement::Expression {
                        expr: other_expr,
                        span,
                    }
                }
            }
        },
        other => other,
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

fn build_inline_dataset_provider(
    rows: &[TestDatasetRow],
    parameters: &[IrParameter],
    context: &mut TransformContext,
    provider_name: &str,
    dataset_span: &Span,
) -> Result<IrStatement, TransformError> {
    let lowered_rows = lower_inline_rows(rows, context)?;

    for (row_span, values) in &lowered_rows {
        if values.len() != parameters.len() {
            return Err(TransformError::TestLoweringError {
                code: "JV5301",
                message: format!(
                    "データセット列数({})とパラメータ数({})が一致しません",
                    values.len(),
                    parameters.len()
                ),
                span: row_span.clone(),
            });
        }
    }

    let arguments_type = JavaType::Reference {
        name: "org.junit.jupiter.params.provider.Arguments".to_string(),
        generic_args: vec![],
    };
    let stream_type = JavaType::Reference {
        name: "java.util.stream.Stream".to_string(),
        generic_args: vec![arguments_type.clone()],
    };

    let mut stream_args = Vec::with_capacity(lowered_rows.len());
    for (row_span, values) in lowered_rows {
        stream_args.push(IrExpression::MethodCall {
            receiver: Some(Box::new(arguments_identifier(&row_span))),
            method_name: "of".to_string(),
            java_name: None,
            resolved_target: None,
            args: values,
            argument_style: CallArgumentStyle::Comma,
            java_type: arguments_type.clone(),
            span: row_span,
        });
    }

    let stream_call = IrExpression::MethodCall {
        receiver: Some(Box::new(stream_identifier(dataset_span))),
        method_name: "of".to_string(),
        java_name: None,
        resolved_target: None,
        args: stream_args,
        argument_style: CallArgumentStyle::Comma,
        java_type: stream_type.clone(),
        span: dataset_span.clone(),
    };

    let body = IrExpression::Block {
        statements: vec![IrStatement::Return {
            value: Some(stream_call),
            span: dataset_span.clone(),
        }],
        java_type: stream_type.clone(),
        span: dataset_span.clone(),
    };

    Ok(IrStatement::MethodDeclaration {
        name: provider_name.to_string(),
        java_name: None,
        type_parameters: Vec::new(),
        parameters: Vec::new(),
        primitive_return: None,
        return_type: stream_type,
        body: Some(body),
        modifiers: IrModifiers {
            visibility: IrVisibility::Private,
            is_static: true,
            ..IrModifiers::default()
        },
        throws: Vec::new(),
        assertion_patterns: Vec::new(),
        span: dataset_span.clone(),
    })
}

fn lower_inline_rows(
    rows: &[TestDatasetRow],
    context: &mut TransformContext,
) -> Result<Vec<(Span, Vec<IrExpression>)>, TransformError> {
    let mut lowered = Vec::with_capacity(rows.len());
    for row in rows {
        let mut values = Vec::with_capacity(row.values.len());
        for value in &row.values {
            values.push(transform_expression(value.clone(), context)?);
        }
        lowered.push((row.span.clone(), values));
    }
    Ok(lowered)
}

fn display_name_annotation(name: &str, span: &Span) -> IrAnnotation {
    IrAnnotation {
        name: AnnotationName::new(vec!["DisplayName".to_string()], span.clone()),
        arguments: vec![IrAnnotationArgument::Positional(
            IrAnnotationValue::Literal(Literal::String(name.to_string())),
        )],
        span: span.clone(),
    }
}

fn test_annotation(span: &Span) -> IrAnnotation {
    IrAnnotation {
        name: AnnotationName::new(vec!["Test".to_string()], span.clone()),
        arguments: Vec::new(),
        span: span.clone(),
    }
}

fn parameterized_annotation(span: &Span) -> IrAnnotation {
    IrAnnotation {
        name: AnnotationName::new(vec!["ParameterizedTest".to_string()], span.clone()),
        arguments: Vec::new(),
        span: span.clone(),
    }
}

fn method_source_annotation(source: &str, span: &Span) -> IrAnnotation {
    IrAnnotation {
        name: AnnotationName::new(vec!["MethodSource".to_string()], span.clone()),
        arguments: vec![IrAnnotationArgument::Positional(
            IrAnnotationValue::Literal(Literal::String(source.to_string())),
        )],
        span: span.clone(),
    }
}

fn arguments_identifier(span: &Span) -> IrExpression {
    IrExpression::Identifier {
        name: "Arguments".to_string(),
        java_type: JavaType::Reference {
            name: "org.junit.jupiter.params.provider.Arguments".to_string(),
            generic_args: vec![],
        },
        span: span.clone(),
    }
}

fn stream_identifier(span: &Span) -> IrExpression {
    IrExpression::Identifier {
        name: "Stream".to_string(),
        java_type: JavaType::Reference {
            name: "java.util.stream.Stream".to_string(),
            generic_args: vec![],
        },
        span: span.clone(),
    }
}

fn derive_class_name(name: &NormalizedName, span: &Span) -> String {
    let base = name.base().trim_start_matches("test_");
    let mut pascal = base
        .split('_')
        .filter(|segment| !segment.is_empty())
        .map(|segment| {
            let mut chars = segment.chars();
            match chars.next() {
                Some(first) => {
                    let mut out = String::new();
                    for upper in first.to_uppercase() {
                        out.push(upper);
                    }
                    out.push_str(chars.as_str());
                    out
                }
                None => String::new(),
            }
        })
        .collect::<String>();

    if pascal.is_empty() {
        pascal.push_str("Generated");
    }

    let mut suffix = name
        .hash_suffix()
        .trim_start_matches('_')
        .to_ascii_uppercase();
    if suffix.is_empty() {
        suffix = "00000000".to_string();
    }

    format!("{pascal}{suffix}Test")
}

struct ParameterScope<'ctx> {
    context: &'ctx mut TransformContext,
}

impl<'ctx> ParameterScope<'ctx> {
    fn new(context: &'ctx mut TransformContext) -> Self {
        context.enter_scope();
        Self { context }
    }
}

impl<'ctx> Drop for ParameterScope<'ctx> {
    fn drop(&mut self) {
        self.context.exit_scope();
    }
}

fn is_boolean_type(java_type: &JavaType) -> bool {
    match java_type {
        JavaType::Primitive(name) => name == "boolean",
        JavaType::Reference { name, .. } => name == "Boolean" || name == "java.lang.Boolean",
        _ => false,
    }
}
