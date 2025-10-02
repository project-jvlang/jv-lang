// jv_ast/utils - Utility functions and helper implementations
use crate::expression::*;
use crate::statement::*;
use crate::types::*;

// Helper traits and implementations
impl Expression {
    pub fn span(&self) -> &Span {
        match self {
            Expression::Literal(_, span) => span,
            Expression::Identifier(_, span) => span,
            Expression::Binary { span, .. } => span,
            Expression::Unary { span, .. } => span,
            Expression::Call { span, .. } => span,
            Expression::MemberAccess { span, .. } => span,
            Expression::NullSafeMemberAccess { span, .. } => span,
            Expression::IndexAccess { span, .. } => span,
            Expression::NullSafeIndexAccess { span, .. } => span,
            Expression::StringInterpolation { span, .. } => span,
            Expression::MultilineString(literal) => &literal.span,
            Expression::JsonLiteral(literal) => &literal.span,
            Expression::When { span, .. } => span,
            Expression::If { span, .. } => span,
            Expression::Block { span, .. } => span,
            Expression::Array { span, .. } => span,
            Expression::Lambda { span, .. } => span,
            Expression::Try { span, .. } => span,
            Expression::This(span) => span,
            Expression::Super(span) => span,
        }
    }
}

impl Statement {
    pub fn span(&self) -> &Span {
        match self {
            Statement::ValDeclaration { span, .. } => span,
            Statement::VarDeclaration { span, .. } => span,
            Statement::FunctionDeclaration { span, .. } => span,
            Statement::ClassDeclaration { span, .. } => span,
            Statement::DataClassDeclaration { span, .. } => span,
            Statement::InterfaceDeclaration { span, .. } => span,
            Statement::ExtensionFunction(ext) => &ext.span,
            Statement::Expression { span, .. } => span,
            Statement::Return { span, .. } => span,
            Statement::Assignment { span, .. } => span,
            Statement::ForIn(for_in) => &for_in.span,
            Statement::Break(span) => span,
            Statement::Continue(span) => span,
            Statement::Import { span, .. } => span,
            Statement::Package { span, .. } => span,
            Statement::Comment(comment) => &comment.span,
            Statement::Concurrency(construct) => match construct {
                ConcurrencyConstruct::Spawn { span, .. } => span,
                ConcurrencyConstruct::Async { span, .. } => span,
                ConcurrencyConstruct::Await { span, .. } => span,
            },
            Statement::ResourceManagement(rm) => match rm {
                ResourceManagement::Use { span, .. } => span,
                ResourceManagement::Defer { span, .. } => span,
            },
        }
    }
}

/// Helper function to determine if an expression requires null-safety checking
pub fn requires_null_safety_check(expr: &Expression) -> bool {
    match expr {
        Expression::NullSafeMemberAccess { .. } | Expression::NullSafeIndexAccess { .. } => true,
        Expression::MemberAccess { object, .. } | Expression::IndexAccess { object, .. } => {
            requires_null_safety_check(object)
        }
        Expression::Call { function, .. } => requires_null_safety_check(function),
        _ => false,
    }
}

/// Helper function to analyze expressions for null safety
pub fn analyze_null_safety(expr: &Expression) -> bool {
    match expr {
        Expression::Literal(Literal::Null, _) => false, // Null literal is inherently not null-safe
        Expression::NullSafeMemberAccess { .. } | Expression::NullSafeIndexAccess { .. } => true,
        Expression::MemberAccess { object, .. } | Expression::IndexAccess { object, .. } => {
            analyze_null_safety(object)
        }
        Expression::Call { function, args, .. } => {
            analyze_null_safety(function)
                && args.iter().all(|arg| match arg {
                    Argument::Positional(expr) => analyze_null_safety(expr),
                    Argument::Named { value, .. } => analyze_null_safety(value),
                })
        }
        _ => false, // Conservative: assume other expressions are not null-safe
    }
}

/// Resolve the scope of an expression (simplified implementation)
pub fn resolve_scope(expr: &Expression) -> String {
    match expr {
        Expression::Identifier(name, _) => {
            // In a real implementation, this would consult a symbol table
            // For now, we'll provide basic scope classification
            if name
                .chars()
                .next()
                .map(|c| c.is_uppercase())
                .unwrap_or(false)
            {
                "global".to_string() // Assume uppercase identifiers are global/type names
            } else {
                "local".to_string() // Assume lowercase identifiers are local variables
            }
        }
        Expression::MemberAccess {
            object, property, ..
        } => {
            // Member access inherits scope from object
            let object_scope = resolve_scope(object);
            format!("{}.{}", object_scope, property)
        }
        Expression::Call { function, .. } => {
            // Function calls have the scope of the function being called
            resolve_scope(function)
        }
        Expression::This(_) => "instance".to_string(),
        _ => "expression".to_string(), // Other expressions don't have meaningful scope
    }
}

/// Resolve default parameter values
pub fn resolve_default_parameter(param: &Parameter) -> Expression {
    match &param.default_value {
        Some(default_expr) => {
            // In a real implementation, this would resolve variables and validate types
            // For now, we'll clone the default expression and potentially transform it
            match default_expr {
                Expression::Identifier(name, span) => {
                    // Could resolve to a constant or transform to a literal
                    Expression::Identifier(name.clone(), span.clone())
                }
                Expression::Literal(literal, span) => {
                    // Validate that literal matches parameter type if specified
                    match (&param.type_annotation, literal) {
                        (Some(TypeAnnotation::Simple(type_name)), _) => {
                            match (type_name.as_str(), literal) {
                                ("Int", Literal::Number(_)) => default_expr.clone(),
                                ("String", Literal::String(_)) => default_expr.clone(),
                                ("Boolean", Literal::Boolean(_)) => default_expr.clone(),
                                _ => Expression::Literal(Literal::Null, span.clone()), // Type mismatch
                            }
                        }
                        _ => default_expr.clone(),
                    }
                }
                _ => default_expr.clone(),
            }
        }
        None => {
            // No default value provided, generate appropriate default based on type
            let span = param.span.clone();
            match &param.type_annotation {
                Some(TypeAnnotation::Simple(type_name)) => match type_name.as_str() {
                    "Int" => Expression::Literal(Literal::Number("0".to_string()), span),
                    "String" => Expression::Literal(Literal::String("".to_string()), span),
                    "Boolean" => Expression::Literal(Literal::Boolean(false), span),
                    _ => Expression::Literal(Literal::Null, span),
                },
                _ => Expression::Literal(Literal::Null, span),
            }
        }
    }
}
