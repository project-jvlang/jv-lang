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
        TypeAnnotation::Function {
            params,
            return_type,
        } => {
            let mut param_types = Vec::with_capacity(params.len());
            for param in params {
                param_types.push(convert_type_annotation(param)?);
            }
            let return_type = convert_type_annotation(*return_type)?;
            Ok(functional_interface_type(&param_types, &return_type))
        }
    }
}

fn convert_simple_type(name: &str) -> JavaType {
    let trimmed = name.trim();
    let lower = trimmed.to_ascii_lowercase();
    match lower.as_str() {
        "any" | "object" => JavaType::Reference {
            name: "Object".to_string(),
            generic_args: vec![],
        },
        "int" => JavaType::Primitive("int".to_string()),
        "boolean" => JavaType::Primitive("boolean".to_string()),
        "double" => JavaType::Primitive("double".to_string()),
        "float" => JavaType::Primitive("float".to_string()),
        "long" => JavaType::Primitive("long".to_string()),
        "char" => JavaType::Primitive("char".to_string()),
        "byte" => JavaType::Primitive("byte".to_string()),
        "short" => JavaType::Primitive("short".to_string()),
        "unit" => JavaType::Void,
        "string" | "java.lang.string" => JavaType::Reference {
            name: "String".to_string(),
            generic_args: vec![],
        },
        "iterator" | "java.util.iterator" => JavaType::Reference {
            name: "java.util.Iterator".to_string(),
            generic_args: vec![],
        },
        "iterable" | "java.lang.iterable" => JavaType::Reference {
            name: "java.lang.Iterable".to_string(),
            generic_args: vec![],
        },
        "collection" | "java.util.collection" => JavaType::Reference {
            name: "java.util.Collection".to_string(),
            generic_args: vec![],
        },
        "list" | "java.util.list" => JavaType::Reference {
            name: "java.util.List".to_string(),
            generic_args: vec![],
        },
        "set" | "java.util.set" => JavaType::Reference {
            name: "java.util.Set".to_string(),
            generic_args: vec![],
        },
        "map" | "java.util.map" => JavaType::Reference {
            name: "java.util.Map".to_string(),
            generic_args: vec![],
        },
        "queue" | "java.util.queue" => JavaType::Reference {
            name: "java.util.Queue".to_string(),
            generic_args: vec![],
        },
        "deque" | "java.util.deque" => JavaType::Reference {
            name: "java.util.Deque".to_string(),
            generic_args: vec![],
        },
        "navigableset" | "java.util.navigableset" => JavaType::Reference {
            name: "java.util.NavigableSet".to_string(),
            generic_args: vec![],
        },
        "sortedset" | "java.util.sortedset" => JavaType::Reference {
            name: "java.util.SortedSet".to_string(),
            generic_args: vec![],
        },
        "navigablemap" | "java.util.navigablemap" => JavaType::Reference {
            name: "java.util.NavigableMap".to_string(),
            generic_args: vec![],
        },
        "sortedmap" | "java.util.sortedmap" => JavaType::Reference {
            name: "java.util.SortedMap".to_string(),
            generic_args: vec![],
        },
        "concurrentmap" | "java.util.concurrentmap" => JavaType::Reference {
            name: "java.util.concurrent.ConcurrentMap".to_string(),
            generic_args: vec![],
        },
        "arraylist" | "java.util.arraylist" => JavaType::Reference {
            name: "java.util.ArrayList".to_string(),
            generic_args: vec![],
        },
        "linkedlist" | "java.util.linkedlist" => JavaType::Reference {
            name: "java.util.LinkedList".to_string(),
            generic_args: vec![],
        },
        "hashmap" | "java.util.hashmap" => JavaType::Reference {
            name: "java.util.HashMap".to_string(),
            generic_args: vec![],
        },
        "linkedhashmap" | "java.util.linkedhashmap" => JavaType::Reference {
            name: "java.util.LinkedHashMap".to_string(),
            generic_args: vec![],
        },
        "treemap" | "java.util.treemap" => JavaType::Reference {
            name: "java.util.TreeMap".to_string(),
            generic_args: vec![],
        },
        "hashset" | "java.util.hashset" => JavaType::Reference {
            name: "java.util.HashSet".to_string(),
            generic_args: vec![],
        },
        "linkedhashset" | "java.util.linkedhashset" => JavaType::Reference {
            name: "java.util.LinkedHashSet".to_string(),
            generic_args: vec![],
        },
        "treeset" | "java.util.treeset" => JavaType::Reference {
            name: "java.util.TreeSet".to_string(),
            generic_args: vec![],
        },
        "arraydeque" | "java.util.arraydeque" => JavaType::Reference {
            name: "java.util.ArrayDeque".to_string(),
            generic_args: vec![],
        },
        "concurrenthashmap" | "java.util.concurrenthashmap" => JavaType::Reference {
            name: "java.util.concurrent.ConcurrentHashMap".to_string(),
            generic_args: vec![],
        },
        _ => JavaType::Reference {
            name: trimmed.to_string(),
            generic_args: vec![],
        },
    }
}

fn functional_interface_type(params: &[JavaType], return_type: &JavaType) -> JavaType {
    match params.len() {
        0 => JavaType::Reference {
            name: "java.util.function.Supplier".to_string(),
            generic_args: vec![boxed_java_type(return_type)],
        },
        1 => {
            let param = boxed_java_type(&params[0]);
            match return_type {
                JavaType::Primitive(name) if name == "boolean" => JavaType::Reference {
                    name: "java.util.function.Predicate".to_string(),
                    generic_args: vec![param],
                },
                JavaType::Void => JavaType::Reference {
                    name: "java.util.function.Consumer".to_string(),
                    generic_args: vec![param],
                },
                _ => JavaType::Reference {
                    name: "java.util.function.Function".to_string(),
                    generic_args: vec![param, boxed_java_type(return_type)],
                },
            }
        }
        2 => {
            let left = boxed_java_type(&params[0]);
            let right = boxed_java_type(&params[1]);
            if matches!(return_type, JavaType::Void) {
                JavaType::Reference {
                    name: "java.util.function.BiConsumer".to_string(),
                    generic_args: vec![left, right],
                }
            } else {
                JavaType::Reference {
                    name: "java.util.function.BiFunction".to_string(),
                    generic_args: vec![left, right, boxed_java_type(return_type)],
                }
            }
        }
        _ => JavaType::Reference {
            name: "java.util.function.Function".to_string(),
            generic_args: vec![JavaType::object(), boxed_java_type(return_type)],
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn 小文字コレクション名をfqcnへ正規化できる() {
        let deque = convert_simple_type("deque");
        if let JavaType::Reference { name, .. } = deque {
            assert_eq!(name, "java.util.Deque");
        } else {
            panic!("deque は参照型として扱われるべき");
        }

        let array_list = convert_simple_type("arraylist");
        if let JavaType::Reference { name, .. } = array_list {
            assert_eq!(name, "java.util.ArrayList");
        } else {
            panic!("arraylist は参照型として扱われるべき");
        }
    }

    #[test]
    fn fqcn風表記も正規化できる() {
        let result = convert_simple_type("java.util.concurrenthashmap");
        if let JavaType::Reference { name, .. } = result {
            assert_eq!(name, "java.util.concurrent.ConcurrentHashMap");
        } else {
            panic!("ConcurrentHashMap は参照型として扱われるべき");
        }
    }
}
