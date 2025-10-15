use super::utils::{boxed_java_type, extract_java_type, ir_expression_span};
use crate::context::TransformContext;
use crate::error::TransformError;
use crate::types::{IrExpression, JavaType, JavaWildcardKind};
use jv_ast::{CallArgumentStyle, SequenceDelimiter, Span, TypeAnnotation};

pub fn infer_java_type(
    type_annotation: Option<TypeAnnotation>,
    initializer: Option<&IrExpression>,
    context: &mut TransformContext,
) -> Result<JavaType, TransformError> {
    if let Some(ta) = type_annotation {
        return convert_type_annotation(ta);
    }

    if let Some(expr) = initializer {
        validate_sequence_styles(expr, context)?;

        if let Some(java_type) = extract_java_type(expr) {
            return Ok(java_type);
        }

        return Err(TransformError::TypeInferenceError {
            message: "Unable to infer Java type from initializer".to_string(),
            span: ir_expression_span(expr),
        });
    }

    Err(TransformError::TypeInferenceError {
        message: "Unable to infer Java type without type annotation or initializer".to_string(),
        span: Span::default(),
    })
}

pub fn convert_type_annotation(
    type_annotation: TypeAnnotation,
) -> Result<JavaType, TransformError> {
    match type_annotation {
        TypeAnnotation::Simple(name) => Ok(convert_simple_type(&name)),
        TypeAnnotation::Generic { name, type_args } => {
            let mut generic_args = Vec::with_capacity(type_args.len());
            for arg in type_args {
                let java_arg = convert_type_annotation(arg)?;
                generic_args.push(boxed_java_type(&java_arg));
            }

            Ok(JavaType::Reference { name, generic_args })
        }
        TypeAnnotation::Nullable(inner) => convert_type_annotation(*inner),
        TypeAnnotation::Array(element_type) => {
            let element_java_type = convert_type_annotation(*element_type)?;
            Ok(JavaType::Array {
                element_type: Box::new(element_java_type),
                dimensions: 1,
            })
        }
        _ => Ok(JavaType::Reference {
            name: "Object".to_string(),
            generic_args: vec![],
        }),
    }
}

fn convert_simple_type(name: &str) -> JavaType {
    match name {
        "Int" | "int" => JavaType::Primitive("int".to_string()),
        "Boolean" | "boolean" => JavaType::Primitive("boolean".to_string()),
        "Double" | "double" => JavaType::Primitive("double".to_string()),
        "Float" | "float" => JavaType::Primitive("float".to_string()),
        "Long" | "long" => JavaType::Primitive("long".to_string()),
        "Char" | "char" => JavaType::Primitive("char".to_string()),
        "Byte" | "byte" => JavaType::Primitive("byte".to_string()),
        "Short" | "short" => JavaType::Primitive("short".to_string()),
        "String" => JavaType::Reference {
            name: "String".to_string(),
            generic_args: vec![],
        },
        _ => JavaType::Reference {
            name: name.to_string(),
            generic_args: vec![],
        },
    }
}

fn validate_sequence_styles(
    expr: &IrExpression,
    context: &mut TransformContext,
) -> Result<(), TransformError> {
    match expr {
        IrExpression::ArrayCreation {
            initializer,
            delimiter,
            span,
            ..
        } => {
            if *delimiter == SequenceDelimiter::Whitespace {
                if let Some(elements) = initializer {
                    validate_whitespace_array(elements, span, context)?;
                }
            }

            if let Some(elements) = initializer {
                for element in elements {
                    validate_sequence_styles(element, context)?;
                }
            }
        }
        IrExpression::MethodCall {
            args,
            argument_style,
            span,
            ..
        } => {
            if *argument_style == CallArgumentStyle::Whitespace {
                validate_whitespace_call(args, span, context)?;
            }

            for arg in args {
                validate_sequence_styles(arg, context)?;
            }
        }
        _ => {}
    }

    Ok(())
}

fn validate_whitespace_array(
    elements: &[IrExpression],
    span: &Span,
    context: &mut TransformContext,
) -> Result<(), TransformError> {
    let mut canonical: Option<JavaType> = None;

    for element in elements {
        if let Some(element_type) = extract_java_type(element) {
            match &canonical {
                Some(expected) if *expected != element_type => {
                    return Err(sequence_type_mismatch(expected, &element_type, span));
                }
                None => canonical = Some(element_type.clone()),
                _ => {}
            }
        }
    }

    if let Some(element_type) = canonical {
        let cache = context.sequence_style_cache_mut();
        if let Some(existing) = cache.lookup_or_insert_array(span, element_type.clone()) {
            if existing != element_type {
                return Err(sequence_type_mismatch(&existing, &element_type, span));
            }
        }
    }

    Ok(())
}

fn validate_whitespace_call(
    args: &[IrExpression],
    span: &Span,
    context: &mut TransformContext,
) -> Result<(), TransformError> {
    let mut canonical: Option<JavaType> = None;
    let mut heterogeneous = false;

    for arg in args {
        if let Some(arg_type) = extract_java_type(arg) {
            match &canonical {
                Some(expected) if *expected != arg_type => {
                    heterogeneous = true;
                    break;
                }
                None => canonical = Some(arg_type.clone()),
                _ => {}
            }
        }
    }

    if heterogeneous {
        return Ok(());
    }

    if let Some(arg_type) = canonical {
        let cache = context.sequence_style_cache_mut();
        if let Some(existing) = cache.lookup_or_insert_call(span, arg_type.clone()) {
            if existing != arg_type {
                return Err(sequence_type_mismatch(&existing, &arg_type, span));
            }
        }
    }

    Ok(())
}

fn sequence_type_mismatch(expected: &JavaType, found: &JavaType, span: &Span) -> TransformError {
    TransformError::WhitespaceSequenceTypeMismatch {
        expected: describe_java_type(expected),
        found: describe_java_type(found),
        span: span.clone(),
    }
}

fn describe_java_type(java_type: &JavaType) -> String {
    match java_type {
        JavaType::Primitive(name) => name.clone(),
        JavaType::Reference { name, generic_args } => {
            if generic_args.is_empty() {
                name.clone()
            } else {
                let args = generic_args
                    .iter()
                    .map(describe_java_type)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}<{}>", name, args)
            }
        }
        JavaType::Array {
            element_type,
            dimensions,
        } => {
            let mut descriptor = describe_java_type(element_type);
            for _ in 0..*dimensions {
                descriptor.push_str("[]");
            }
            descriptor
        }
        JavaType::Functional { interface_name, .. } => interface_name.clone(),
        JavaType::Wildcard { kind, bound } => match kind {
            JavaWildcardKind::Unbounded => "?".to_string(),
            JavaWildcardKind::Extends => {
                let ty = bound
                    .as_ref()
                    .map(|inner| describe_java_type(inner))
                    .unwrap_or_else(|| "Object".to_string());
                format!("? extends {}", ty)
            }
            JavaWildcardKind::Super => {
                let ty = bound
                    .as_ref()
                    .map(|inner| describe_java_type(inner))
                    .unwrap_or_else(|| "Object".to_string());
                format!("? super {}", ty)
            }
        },
        JavaType::Void => "void".to_string(),
    }
}
