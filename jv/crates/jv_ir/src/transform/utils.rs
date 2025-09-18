use crate::types::{IrExpression, IrModifiers, IrVisibility, JavaType};
use jv_ast::{Literal, Modifiers, Span, TypeAnnotation, Visibility};

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

    if modifiers.is_override {
        ir_modifiers.annotations.push("Override".to_string());
    }

    ir_modifiers
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

pub fn generate_utility_class_name(_package: Option<&str>, _source_file: &str) -> String {
    panic!("not yet implemented: generate_utility_class_name")
}

pub fn generate_extension_class_name(_receiver_type: &TypeAnnotation) -> String {
    panic!("not yet implemented: generate_extension_class_name")
}
