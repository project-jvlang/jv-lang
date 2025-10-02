use crate::types::{
    IrAnnotation, IrAnnotationArgument, IrAnnotationValue, IrExpression, IrModifiers, IrVisibility,
    JavaType,
};
use jv_ast::{
    Annotation, AnnotationArgument, AnnotationName, AnnotationValue, Literal, Modifiers, Span,
    TypeAnnotation, Visibility,
};
use std::path::Path;

pub(crate) fn convert_modifiers(modifiers: &Modifiers) -> IrModifiers {
    let mut ir_modifiers = IrModifiers::default();

    ir_modifiers.visibility = match modifiers.visibility {
        Visibility::Public => IrVisibility::Public,
        Visibility::Protected => IrVisibility::Protected,
        Visibility::Private => IrVisibility::Private,
        Visibility::Internal => IrVisibility::Package,
    };

    ir_modifiers.is_static = modifiers.is_static;
    ir_modifiers.is_final = modifiers.is_final;
    ir_modifiers.is_abstract = modifiers.is_abstract;

    for annotation in &modifiers.annotations {
        ir_modifiers
            .annotations
            .push(convert_annotation(annotation));
    }

    if modifiers.is_override
        && !ir_modifiers
            .annotations
            .iter()
            .any(|a| a.name.simple_name() == "Override")
    {
        ir_modifiers.annotations.push(IrAnnotation {
            name: AnnotationName::new(vec!["Override".to_string()], Span::dummy()),
            arguments: Vec::new(),
            span: Span::dummy(),
        });
    }

    ir_modifiers
}

fn convert_annotation(annotation: &Annotation) -> IrAnnotation {
    IrAnnotation {
        name: annotation.name.clone(),
        arguments: annotation
            .arguments
            .iter()
            .map(convert_annotation_argument)
            .collect(),
        span: annotation.span.clone(),
    }
}

fn convert_annotation_argument(argument: &AnnotationArgument) -> IrAnnotationArgument {
    match argument {
        AnnotationArgument::Positional { value, .. } => {
            IrAnnotationArgument::Positional(convert_annotation_value(value))
        }
        AnnotationArgument::Named { name, value, .. } => IrAnnotationArgument::Named {
            name: name.clone(),
            value: convert_annotation_value(value),
        },
    }
}

fn convert_annotation_value(value: &AnnotationValue) -> IrAnnotationValue {
    match value {
        AnnotationValue::Literal(literal) => IrAnnotationValue::Literal(literal.clone()),
        AnnotationValue::EnumConstant {
            type_path,
            constant,
        } => {
            let type_name = if type_path.is_empty() {
                String::new()
            } else {
                type_path.join(".")
            };
            IrAnnotationValue::EnumConstant {
                type_name,
                constant: constant.clone(),
            }
        }
        AnnotationValue::Array(values) => {
            IrAnnotationValue::Array(values.iter().map(convert_annotation_value).collect())
        }
        AnnotationValue::ClassLiteral { type_path } => {
            IrAnnotationValue::ClassLiteral(type_path.join("."))
        }
        AnnotationValue::NestedAnnotation(annotation) => {
            IrAnnotationValue::Nested(convert_annotation(annotation))
        }
    }
}

pub(crate) fn extract_java_type(expr: &IrExpression) -> Option<JavaType> {
    match expr {
        IrExpression::Literal(literal, _) => Some(literal_to_java_type(literal)),
        IrExpression::Identifier { java_type, .. }
        | IrExpression::MethodCall { java_type, .. }
        | IrExpression::FieldAccess { java_type, .. }
        | IrExpression::ArrayAccess { java_type, .. }
        | IrExpression::Binary { java_type, .. }
        | IrExpression::Unary { java_type, .. }
        | IrExpression::Assignment { java_type, .. }
        | IrExpression::Conditional { java_type, .. }
        | IrExpression::Block { java_type, .. }
        | IrExpression::ObjectCreation { java_type, .. }
        | IrExpression::Lambda { java_type, .. }
        | IrExpression::Switch { java_type, .. }
        | IrExpression::NullSafeOperation { java_type, .. }
        | IrExpression::CompletableFuture { java_type, .. }
        | IrExpression::VirtualThread { java_type, .. }
        | IrExpression::TryWithResources { java_type, .. }
        | IrExpression::This { java_type, .. }
        | IrExpression::Super { java_type, .. } => Some(java_type.clone()),
        IrExpression::Cast { target_type, .. } => Some(target_type.clone()),
        IrExpression::InstanceOf { .. } => Some(JavaType::boolean()),
        IrExpression::ArrayCreation {
            element_type,
            dimensions,
            ..
        } => Some(JavaType::Array {
            element_type: Box::new(element_type.clone()),
            dimensions: dimensions.len(),
        }),
        IrExpression::StringFormat { .. } => Some(JavaType::string()),
    }
}

pub(crate) fn ir_expression_span(expr: &IrExpression) -> Span {
    match expr {
        IrExpression::Literal(_, span)
        | IrExpression::Identifier { span, .. }
        | IrExpression::MethodCall { span, .. }
        | IrExpression::FieldAccess { span, .. }
        | IrExpression::ArrayAccess { span, .. }
        | IrExpression::Binary { span, .. }
        | IrExpression::Unary { span, .. }
        | IrExpression::Assignment { span, .. }
        | IrExpression::Conditional { span, .. }
        | IrExpression::Block { span, .. }
        | IrExpression::ArrayCreation { span, .. }
        | IrExpression::ObjectCreation { span, .. }
        | IrExpression::Lambda { span, .. }
        | IrExpression::Switch { span, .. }
        | IrExpression::Cast { span, .. }
        | IrExpression::InstanceOf { span, .. }
        | IrExpression::This { span, .. }
        | IrExpression::Super { span, .. }
        | IrExpression::StringFormat { span, .. }
        | IrExpression::NullSafeOperation { span, .. }
        | IrExpression::CompletableFuture { span, .. }
        | IrExpression::VirtualThread { span, .. }
        | IrExpression::TryWithResources { span, .. } => span.clone(),
    }
}

fn literal_to_java_type(literal: &Literal) -> JavaType {
    match literal {
        Literal::String(_) => JavaType::string(),
        Literal::Number(_) => JavaType::Primitive("int".to_string()),
        Literal::Boolean(_) => JavaType::boolean(),
        Literal::Character(_) => JavaType::Primitive("char".to_string()),
        Literal::Null => JavaType::object(),
    }
}

pub fn generate_utility_class_name(package: Option<&str>, source_file: &str) -> String {
    let file_stem = Path::new(source_file)
        .file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or(source_file);

    let mut class_name = sanitize_type_identifier(file_stem);
    if !class_name.ends_with("Kt") {
        class_name.push_str("Kt");
    }

    match package {
        Some(pkg) if !pkg.is_empty() => format!("{}.{}", pkg, class_name),
        _ => class_name,
    }
}

pub fn generate_extension_class_name(receiver_type: &TypeAnnotation) -> String {
    let base = type_annotation_base_name(receiver_type);
    let mut class_name = sanitize_type_identifier(&base);
    class_name.push_str("Extensions");
    class_name
}

fn type_annotation_base_name(annotation: &TypeAnnotation) -> String {
    match annotation {
        TypeAnnotation::Simple(name) => simple_type_name(name),
        TypeAnnotation::Nullable(inner) => type_annotation_base_name(inner),
        TypeAnnotation::Generic { name, .. } => simple_type_name(name),
        TypeAnnotation::Function { .. } => "Function".to_string(),
        TypeAnnotation::Array(inner) => format!("{}Array", type_annotation_base_name(inner)),
    }
}

fn simple_type_name(name: &str) -> String {
    name.rsplit('.').next().unwrap_or(name).to_string()
}

fn sanitize_type_identifier(raw: &str) -> String {
    let mut result = String::new();
    let mut current = String::new();

    for ch in raw.chars() {
        if ch.is_alphanumeric() {
            current.push(ch);
        } else if !current.is_empty() {
            push_component(&mut result, &current);
            current.clear();
        }
    }

    if !current.is_empty() {
        push_component(&mut result, &current);
    }

    if result.is_empty() {
        result.push_str("Generated");
    }

    if result
        .chars()
        .next()
        .map(|c| c.is_ascii_digit())
        .unwrap_or(false)
    {
        result.insert(0, '_');
    }

    result
}

fn push_component(result: &mut String, component: &str) {
    let mut chars = component.chars();
    if let Some(first) = chars.next() {
        result.extend(first.to_uppercase());
        result.extend(chars);
    }
}
