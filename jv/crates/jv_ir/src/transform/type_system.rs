use super::utils::{extract_java_type, ir_expression_span};
use crate::context::TransformContext;
use crate::error::TransformError;
use crate::types::{IrExpression, JavaType};
use jv_ast::{Span, TypeAnnotation};

pub fn infer_java_type(
    type_annotation: Option<TypeAnnotation>,
    initializer: Option<&IrExpression>,
    _context: &TransformContext,
) -> Result<JavaType, TransformError> {
    if let Some(ta) = type_annotation {
        return convert_type_annotation(ta);
    }

    if let Some(expr) = initializer {
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
        TypeAnnotation::Simple(name) => Ok(match name.as_str() {
            "Int" | "int" => JavaType::Primitive("int".to_string()),
            "String" => JavaType::Reference {
                name: "String".to_string(),
                generic_args: vec![],
            },
            "Boolean" | "boolean" => JavaType::Primitive("boolean".to_string()),
            "Double" | "double" => JavaType::Primitive("double".to_string()),
            "Float" | "float" => JavaType::Primitive("float".to_string()),
            "Long" | "long" => JavaType::Primitive("long".to_string()),
            "Char" | "char" => JavaType::Primitive("char".to_string()),
            "Byte" | "byte" => JavaType::Primitive("byte".to_string()),
            "Short" | "short" => JavaType::Primitive("short".to_string()),
            _ => JavaType::Reference {
                name,
                generic_args: vec![],
            },
        }),
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
