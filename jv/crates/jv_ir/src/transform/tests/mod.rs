use crate::context::TransformContext;
use crate::error::TransformError;
use crate::naming::test_identifiers::{NormalizedName, normalize_method};
use crate::sequence_pipeline::TestSuitePlanner;
use crate::transform::type_system::convert_type_annotation;
use crate::transform::utils::convert_modifiers;
use crate::types::{
    IrAnnotation, IrAnnotationArgument, IrAnnotationValue, IrModifiers, IrParameter, IrStatement,
    IrVisibility, JavaType,
};
use jv_ast::types::Modifiers;
use jv_ast::{
    Annotation, AnnotationName, BindingPatternKind, Literal, Span, TestDeclaration, TestParameter,
};

mod assertions;
mod dataset;

use assertions::lower_test_body;
use dataset::lower_dataset;

pub fn lower_test_declaration(
    declaration: TestDeclaration,
    context: &mut TransformContext,
) -> Result<Vec<IrStatement>, TransformError> {
    let TestDeclaration {
        display_name,
        dataset,
        parameters: ast_parameters,
        annotations,
        body,
        span,
        ..
    } = declaration;

    let normalized_method = normalize_method(&display_name, &span);
    let class_name = derive_class_name(&normalized_method, &span);
    let mut planner = TestSuitePlanner::new(class_name, span.clone());

    let (parameters, body_ir, assertion_patterns) = {
        context.enter_scope();
        let result = (|| -> Result<_, TransformError> {
            let parameters = lower_test_parameters(&ast_parameters, context)?;
            let (body_ir, assertion_patterns) = lower_test_body(body, context)?;
            Ok((parameters, body_ir, assertion_patterns))
        })();
        context.exit_scope();
        result?
    };

    let method_name = normalized_method.identifier().to_string();
    let mut method_modifiers = base_method_modifiers(&annotations);
    method_modifiers.visibility = IrVisibility::Public;

    let mut method_annotations = vec![display_name_annotation(&display_name, &span)];
    let dataset = lower_dataset(&dataset, &display_name, &span, &parameters, context)?;
    method_annotations.extend(dataset.annotations);
    method_modifiers.annotations.extend(method_annotations);

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
        span: span.clone(),
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

fn base_method_modifiers(user_annotations: &[Annotation]) -> IrModifiers {
    if user_annotations.is_empty() {
        return IrModifiers::default();
    }

    let mut modifiers = Modifiers::default();
    modifiers.annotations = user_annotations.to_vec();
    convert_modifiers(&modifiers)
}

fn lower_test_parameters(
    parameters: &[TestParameter],
    context: &mut TransformContext,
) -> Result<Vec<IrParameter>, TransformError> {
    let mut ir_parameters = Vec::with_capacity(parameters.len());

    for parameter in parameters {
        let name = match &parameter.pattern {
            BindingPatternKind::Identifier { name, .. } => name.clone(),
            BindingPatternKind::Wildcard { span, .. } => {
                return Err(TransformError::TestLoweringError {
                    code: "JV5302",
                    message: "ワイルドカードのテストパラメータはサポートされていません".to_string(),
                    span: span.clone(),
                    details: None,
                });
            }
            other => {
                return Err(TransformError::TestLoweringError {
                    code: "JV5302",
                    message: format!(
                        "複雑なテストパラメータパターン '{other:?}' はまだサポートされていません"
                    ),
                    span: parameter.span.clone(),
                    details: None,
                });
            }
        };

        let java_type = match &parameter.type_annotation {
            Some(annotation) => convert_type_annotation(annotation.clone())?,
            None => JavaType::object(),
        };

        context.add_variable(name.clone(), java_type.clone());

        ir_parameters.push(IrParameter {
            name,
            java_type,
            modifiers: IrModifiers::default(),
            span: parameter.span.clone(),
        });
    }

    Ok(ir_parameters)
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

fn derive_class_name(name: &NormalizedName, _span: &Span) -> String {
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
