use crate::context::TransformContext;
use crate::error::TransformError;
use crate::types::{IrExpression, IrStatement, JavaType, MethodOverload};
use jv_ast::{Argument, Expression, Modifiers, Parameter, Span, Statement, TypeAnnotation};

pub fn desugar_default_parameters(
    function_name: String,
    parameters: Vec<Parameter>,
    return_type: Option<TypeAnnotation>,
    body: Box<Expression>,
    modifiers: Modifiers,
    span: Span,
    context: &mut TransformContext,
) -> Result<Vec<MethodOverload>, TransformError> {
    use crate::types::*;

    let mut overloads = Vec::new();

    // Convert modifiers to IR modifiers
    let ir_modifiers = IrModifiers {
        visibility: match modifiers.visibility {
            jv_ast::Visibility::Public => IrVisibility::Public,
            jv_ast::Visibility::Private => IrVisibility::Private,
            jv_ast::Visibility::Protected => IrVisibility::Protected,
            jv_ast::Visibility::Internal => IrVisibility::Package,
        },
        is_static: modifiers.is_static,
        is_final: modifiers.is_final,
        is_abstract: modifiers.is_abstract,
        ..Default::default()
    };

    // Convert return type
    let java_return_type = match return_type {
        Some(type_ann) => convert_type_annotation(type_ann)?,
        None => JavaType::void(),
    };

    // Find parameters with default values
    let mut required_params = Vec::new();
    let mut optional_params = Vec::new();

    for param in parameters {
        if param.default_value.is_some() {
            optional_params.push(param);
        } else {
            required_params.push(param);
        }
    }

    // Generate overloads for each combination of parameters
    // Start with all required parameters only
    let mut current_params = required_params.clone();

    // Convert parameters to IR
    let ir_params: Result<Vec<IrParameter>, TransformError> = current_params
        .iter()
        .map(|p| {
            Ok(IrParameter {
                name: p.name.clone(),
                java_type: match &p.type_annotation {
                    Some(type_ann) => convert_type_annotation(type_ann.clone())?,
                    None => JavaType::object(), // Default to Object if no type specified
                },
                modifiers: IrModifiers::default(),
                span: p.span.clone(),
            })
        })
        .collect();

    let ir_params = ir_params?;

    // Convert body to IR expression
    let ir_body = convert_expression_to_ir(*body, context)?;

    // Create base overload with required parameters only
    overloads.push(MethodOverload {
        name: function_name.clone(),
        parameters: ir_params,
        return_type: java_return_type.clone(),
        body: ir_body.clone(),
        modifiers: ir_modifiers.clone(),
        span: span.clone(),
    });

    // Create additional overloads by adding optional parameters one by one
    for optional_param in optional_params {
        current_params.push(optional_param.clone());

        let ir_params: Result<Vec<IrParameter>, TransformError> = current_params
            .iter()
            .map(|p| {
                Ok(IrParameter {
                    name: p.name.clone(),
                    java_type: match &p.type_annotation {
                        Some(type_ann) => convert_type_annotation(type_ann.clone())?,
                        None => JavaType::object(),
                    },
                    modifiers: IrModifiers::default(),
                    span: p.span.clone(),
                })
            })
            .collect();

        let ir_params = ir_params?;

        // Create a delegating body that calls the full method with default values
        let delegating_body = create_delegating_call(
            &function_name,
            &current_params,
            &optional_param.default_value,
            context,
        )?;

        overloads.push(MethodOverload {
            name: function_name.clone(),
            parameters: ir_params,
            return_type: java_return_type.clone(),
            body: delegating_body,
            modifiers: ir_modifiers.clone(),
            span: span.clone(),
        });
    }

    Ok(overloads)
}

// Helper function to convert TypeAnnotation to JavaType
fn convert_type_annotation(type_ann: TypeAnnotation) -> Result<JavaType, TransformError> {
    match type_ann {
        TypeAnnotation::Simple(name) => match name.as_str() {
            "int" => Ok(JavaType::int()),
            "boolean" => Ok(JavaType::boolean()),
            "String" => Ok(JavaType::string()),
            "void" => Ok(JavaType::void()),
            _ => Ok(JavaType::Reference {
                name,
                generic_args: vec![],
            }),
        },
        TypeAnnotation::Nullable(inner) => {
            // For now, just convert the inner type (Java's type system handles nullability)
            convert_type_annotation(*inner)
        }
        TypeAnnotation::Generic { name, type_args } => {
            let generic_args: Result<Vec<JavaType>, TransformError> =
                type_args.into_iter().map(convert_type_annotation).collect();
            Ok(JavaType::Reference {
                name,
                generic_args: generic_args?,
            })
        }
        TypeAnnotation::Array(element_type) => Ok(JavaType::Array {
            element_type: Box::new(convert_type_annotation(*element_type)?),
            dimensions: 1,
        }),
        TypeAnnotation::Function {
            params,
            return_type,
        } => {
            let param_types: Result<Vec<JavaType>, TransformError> =
                params.into_iter().map(convert_type_annotation).collect();
            Ok(JavaType::Functional {
                interface_name: "Function".to_string(), // Default functional interface
                param_types: param_types?,
                return_type: Box::new(convert_type_annotation(*return_type)?),
            })
        }
    }
}

// Helper function to convert Expression to IrExpression (simplified)
fn convert_expression_to_ir(
    expr: Expression,
    _context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    match expr {
        Expression::Literal(lit, span) => Ok(IrExpression::Literal(lit, span)),
        Expression::Identifier(name, span) => Ok(IrExpression::Identifier {
            name,
            java_type: JavaType::object(), // Type inference would determine this
            span,
        }),
        // For now, return a placeholder for complex expressions
        _ => Ok(IrExpression::Literal(
            jv_ast::Literal::String("/* TODO: complex expression */".to_string()),
            Span::dummy(),
        )),
    }
}

// Helper function to create delegating method calls
fn create_delegating_call(
    function_name: &str,
    current_params: &[Parameter],
    default_value: &Option<Expression>,
    _context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    // Create method call arguments from current parameters
    let mut args = Vec::new();

    for param in current_params {
        args.push(IrExpression::Identifier {
            name: param.name.clone(),
            java_type: JavaType::object(),
            span: param.span.clone(),
        });
    }

    // Add default value if provided
    if let Some(default_expr) = default_value {
        args.push(convert_expression_to_ir(default_expr.clone(), _context)?);
    }

    Ok(IrExpression::MethodCall {
        receiver: None, // Static call to overloaded method
        method_name: function_name.to_string(),
        args,
        java_type: JavaType::object(), // Return type would be inferred
        span: Span::dummy(),
    })
}

pub fn desugar_named_arguments(
    function: Box<Expression>,
    args: Vec<Argument>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    use crate::types::*;

    // Convert function expression to IR
    let ir_function = convert_expression_to_ir(*function, context)?;

    // Separate positional and named arguments
    let mut positional_args = Vec::new();
    let mut named_args = Vec::new();

    for arg in args {
        match arg {
            Argument::Positional(expr) => {
                positional_args.push(convert_expression_to_ir(expr, context)?);
            }
            Argument::Named { name, value, .. } => {
                named_args.push((name, convert_expression_to_ir(value, context)?));
            }
        }
    }

    // For now, we'll assume we need to reorder arguments based on parameter names
    // In a full implementation, this would require looking up function signatures
    // and matching named arguments to parameter positions

    // Create a combined argument list starting with positional arguments
    let mut final_args = positional_args;

    // Add named arguments (for now, in the order they appear)
    // A full implementation would reorder these based on the function signature
    for (_name, expr) in named_args {
        final_args.push(expr);
    }

    // Convert the function call based on the type of function expression
    match ir_function {
        IrExpression::Identifier { name, .. } => {
            // Function call by name
            Ok(IrExpression::MethodCall {
                receiver: None, // Static function call
                method_name: name,
                args: final_args,
                java_type: JavaType::object(), // Would be inferred from function signature
                span,
            })
        }
        IrExpression::FieldAccess {
            receiver,
            field_name,
            ..
        } => {
            // Method call on an object
            Ok(IrExpression::MethodCall {
                receiver: Some(receiver),
                method_name: field_name,
                args: final_args,
                java_type: JavaType::object(),
                span,
            })
        }
        _ => {
            // For other function expressions, wrap in a method call
            // This is a simplified approach - a full implementation would handle
            // function references, lambdas, etc. more sophisticatedly
            let mut all_args = vec![ir_function];
            all_args.extend(final_args);

            Ok(IrExpression::MethodCall {
                receiver: None,
                method_name: "call".to_string(), // Generic call method
                args: all_args,
                java_type: JavaType::object(),
                span,
            })
        }
    }
}

pub fn desugar_top_level_function(
    function: Statement,
    context: &mut TransformContext,
) -> Result<IrStatement, TransformError> {
    use crate::types::*;

    match function {
        Statement::FunctionDeclaration {
            name,
            parameters,
            return_type,
            body,
            modifiers,
            span,
        } => {
            // Convert function parameters to IR parameters
            let ir_parameters: Result<Vec<IrParameter>, TransformError> = parameters
                .iter()
                .map(|p| {
                    Ok(IrParameter {
                        name: p.name.clone(),
                        java_type: match &p.type_annotation {
                            Some(type_ann) => convert_type_annotation(type_ann.clone())?,
                            None => JavaType::object(),
                        },
                        modifiers: IrModifiers::default(),
                        span: p.span.clone(),
                    })
                })
                .collect();

            let ir_parameters = ir_parameters?;

            // Convert return type
            let java_return_type = match return_type {
                Some(type_ann) => convert_type_annotation(type_ann)?,
                None => JavaType::void(),
            };

            // Convert modifiers to IR modifiers
            let ir_modifiers = IrModifiers {
                visibility: match modifiers.visibility {
                    jv_ast::Visibility::Public => IrVisibility::Public,
                    jv_ast::Visibility::Private => IrVisibility::Private,
                    jv_ast::Visibility::Protected => IrVisibility::Protected,
                    jv_ast::Visibility::Internal => IrVisibility::Package,
                },
                is_static: true, // Top-level functions become static methods
                is_final: modifiers.is_final,
                is_abstract: modifiers.is_abstract,
                ..Default::default()
            };

            // Convert function body to IR expression
            let ir_body = convert_expression_to_ir(*body, context)?;

            // Create method declaration
            Ok(IrStatement::MethodDeclaration {
                name,
                parameters: ir_parameters,
                return_type: java_return_type,
                body: Some(ir_body),
                modifiers: ir_modifiers,
                throws: vec![], // No throws clause for now
                span,
            })
        }
        _ => {
            // Return an error for non-function statements
            Err(TransformError::UnsupportedConstruct {
                construct: "Expected function statement for top-level function desugaring"
                    .to_string(),
                span: Span::dummy(),
            })
        }
    }
}
