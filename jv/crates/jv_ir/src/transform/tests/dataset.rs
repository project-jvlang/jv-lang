use crate::context::TransformContext;
use crate::error::{TestLoweringDiagnostic, TransformError};
use crate::naming::test_identifiers::normalize_dataset;
use crate::transform::transform_expression;
use crate::types::{
    IrAnnotation, IrAnnotationArgument, IrAnnotationValue, IrExpression, IrModifiers, IrParameter,
    IrStatement, IrVisibility, JavaType,
};
use jv_ast::{AnnotationName, CallArgumentStyle, Literal, Span, TestDataset, TestDatasetRow};

pub(super) struct DatasetArtifacts {
    pub(super) annotations: Vec<IrAnnotation>,
    pub(super) provider: Option<IrStatement>,
    pub(super) sample_declaration: Option<IrStatement>,
}

pub(super) fn lower_dataset(
    dataset: &Option<TestDataset>,
    display_name: &str,
    span: &Span,
    parameters: &[IrParameter],
    context: &mut TransformContext,
) -> Result<DatasetArtifacts, TransformError> {
    match dataset {
        None => {
            if !parameters.is_empty() {
                return Err(TransformError::TestLoweringError {
                    code: "JV5303",
                    message: "テストパラメータを使用する場合はデータセットを指定してください"
                        .to_string(),
                    span: span.clone(),
                    details: None,
                });
            }

            Ok(DatasetArtifacts {
                annotations: vec![test_annotation(span)],
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
                    details: None,
                });
            }

            let dataset_name = normalize_dataset(display_name, span);
            let provider_name = dataset_name.identifier().to_string();
            let provider =
                build_inline_dataset_provider(rows, parameters, context, &provider_name, span)?;

            Ok(DatasetArtifacts {
                annotations: vec![
                    parameterized_annotation(span),
                    method_source_annotation(&provider_name, span),
                ],
                provider: Some(provider),
                sample_declaration: None,
            })
        }
        Some(TestDataset::Sample(metadata)) => Err(TransformError::TestLoweringError {
            code: "JV5306",
            message: "@Sample を利用したデータセットのローワリングは未実装です".to_string(),
            span: metadata.span.clone(),
            details: None,
        }),
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
                details: Some(TestLoweringDiagnostic::DatasetColumnMismatch {
                    parameter_count: parameters.len(),
                    column_count: values.len(),
                }),
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
