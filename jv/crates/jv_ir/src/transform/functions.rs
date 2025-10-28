use super::transform_expression;
use super::type_system::convert_type_annotation;
use super::utils::extract_java_type;
use crate::context::TransformContext;
use crate::error::TransformError;
use crate::types::{IrExpression, IrStatement, JavaType, MethodOverload};
use jv_ast::{
    Argument, CallArgumentStyle, Expression, Literal, Modifiers, Parameter, Span, Statement,
    TypeAnnotation, VarianceMarker,
};

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

    let infer_return_type = return_type.is_none();

    // Convert return type
    let mut java_return_type = match return_type {
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
    let ir_body = transform_expression(*body, context)?;

    if infer_return_type {
        if let Some(inferred) = infer_return_type_from_ir_expression(&ir_body) {
            java_return_type = inferred;
        }
    }

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
            &java_return_type,
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

// Helper function to create delegating method calls
fn create_delegating_call(
    function_name: &str,
    current_params: &[Parameter],
    default_value: &Option<Expression>,
    return_type: &JavaType,
    context: &mut TransformContext,
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
        args.push(convert_expression_to_ir(default_expr.clone(), context)?);
    }

    let argument_types: Vec<JavaType> = args
        .iter()
        .map(|expr| extract_java_type(expr).unwrap_or_else(JavaType::object))
        .collect();

    let mut call = IrExpression::MethodCall {
        receiver: None, // Static call to overloaded method
        method_name: function_name.to_string(),
        java_name: None,
        resolved_target: None,
        args,
        argument_style: CallArgumentStyle::Comma,
        java_type: return_type.clone(),
        span: Span::dummy(),
    };

    context.bind_method_call(&mut call, None, None, argument_types);

    Ok(call)
}

fn infer_return_type_from_ir_expression(body: &IrExpression) -> Option<JavaType> {
    if let Some(java_type) = extract_java_type(body) {
        if java_type != JavaType::void() {
            return Some(java_type);
        }
    }

    let mut accumulator = ReturnTypeAccumulator::default();
    gather_return_types_from_expression(body, &mut accumulator);
    accumulator
        .into_option()
        .filter(|java_type| *java_type != JavaType::void())
}

fn gather_return_types_from_expression(expr: &IrExpression, acc: &mut ReturnTypeAccumulator) {
    if acc.is_conflicted() {
        return;
    }

    match expr {
        IrExpression::Block { statements, .. } => {
            for statement in statements {
                gather_return_types_from_statement(statement, acc);
                if acc.is_conflicted() {
                    break;
                }
            }
        }
        IrExpression::Lambda { body, .. } => gather_return_types_from_expression(body, acc),
        IrExpression::NullSafeOperation {
            operation,
            default_value,
            ..
        } => {
            gather_return_types_from_expression(operation, acc);
            if let Some(default) = default_value.as_ref() {
                gather_return_types_from_expression(default, acc);
            }
        }
        IrExpression::Conditional {
            condition,
            then_expr,
            else_expr,
            ..
        } => {
            gather_return_types_from_expression(condition, acc);
            gather_return_types_from_expression(then_expr, acc);
            gather_return_types_from_expression(else_expr, acc);
        }
        IrExpression::Switch {
            discriminant,
            cases,
            ..
        } => {
            gather_return_types_from_expression(discriminant, acc);
            for case in cases {
                if acc.is_conflicted() {
                    break;
                }
                gather_return_types_from_expression(&case.body, acc);
            }
        }
        IrExpression::TryWithResources {
            resources, body, ..
        } => {
            for resource in resources {
                gather_return_types_from_expression(&resource.initializer, acc);
                if acc.is_conflicted() {
                    return;
                }
            }
            gather_return_types_from_expression(body, acc);
        }
        _ => {}
    }
}

fn gather_return_types_from_statement(stmt: &IrStatement, acc: &mut ReturnTypeAccumulator) {
    if acc.is_conflicted() {
        return;
    }

    match stmt {
        IrStatement::Commented { statement, .. } => {
            gather_return_types_from_statement(statement, acc);
        }
        IrStatement::VariableDeclaration { initializer, .. }
        | IrStatement::FieldDeclaration { initializer, .. } => {
            if let Some(expr) = initializer.as_ref() {
                gather_return_types_from_expression(expr, acc);
            }
        }
        IrStatement::Expression { expr, .. } => gather_return_types_from_expression(expr, acc),
        IrStatement::Return { value, .. } => {
            let candidate = match value {
                Some(expr) => extract_java_type(expr),
                None => Some(JavaType::void()),
            };

            match candidate {
                Some(java_type) => acc.push(java_type),
                None => acc.mark_unknown(),
            }
        }
        IrStatement::If {
            condition,
            then_stmt,
            else_stmt,
            ..
        } => {
            gather_return_types_from_expression(condition, acc);
            gather_return_types_from_statement(then_stmt, acc);
            if let Some(else_branch) = else_stmt.as_ref() {
                gather_return_types_from_statement(else_branch, acc);
            }
        }
        IrStatement::While {
            condition, body, ..
        } => {
            gather_return_types_from_expression(condition, acc);
            gather_return_types_from_statement(body, acc);
        }
        IrStatement::ForEach { iterable, body, .. } => {
            gather_return_types_from_expression(iterable, acc);
            gather_return_types_from_statement(body, acc);
        }
        IrStatement::For {
            init,
            condition,
            update,
            body,
            ..
        } => {
            if let Some(init_stmt) = init.as_ref() {
                gather_return_types_from_statement(init_stmt, acc);
            }
            if let Some(cond) = condition.as_ref() {
                gather_return_types_from_expression(cond, acc);
            }
            if let Some(update_expr) = update.as_ref() {
                gather_return_types_from_expression(update_expr, acc);
            }
            gather_return_types_from_statement(body, acc);
        }
        IrStatement::Switch {
            discriminant,
            cases,
            ..
        } => {
            gather_return_types_from_expression(discriminant, acc);
            for case in cases {
                if acc.is_conflicted() {
                    break;
                }
                gather_return_types_from_expression(&case.body, acc);
            }
        }
        IrStatement::Try {
            body,
            catch_clauses,
            finally_block,
            ..
        } => {
            gather_return_types_from_statement(body, acc);
            for clause in catch_clauses {
                gather_return_types_from_statement(&clause.body, acc);
            }
            if let Some(finally_stmt) = finally_block.as_ref() {
                gather_return_types_from_statement(finally_stmt, acc);
            }
        }
        IrStatement::TryWithResources {
            resources,
            body,
            catch_clauses,
            finally_block,
            ..
        } => {
            for resource in resources {
                gather_return_types_from_expression(&resource.initializer, acc);
                if acc.is_conflicted() {
                    return;
                }
            }
            gather_return_types_from_statement(body, acc);
            for clause in catch_clauses {
                gather_return_types_from_statement(&clause.body, acc);
            }
            if let Some(finally_stmt) = finally_block.as_ref() {
                gather_return_types_from_statement(finally_stmt, acc);
            }
        }
        IrStatement::Throw { expr, .. } => gather_return_types_from_expression(expr, acc),
        IrStatement::Block { statements, .. } => {
            for statement in statements {
                gather_return_types_from_statement(statement, acc);
                if acc.is_conflicted() {
                    break;
                }
            }
        }
        IrStatement::ClassDeclaration {
            fields,
            methods,
            nested_classes,
            ..
        } => {
            for field in fields {
                gather_return_types_from_statement(field, acc);
            }
            for method in methods {
                gather_return_types_from_statement(method, acc);
            }
            for nested in nested_classes {
                gather_return_types_from_statement(nested, acc);
            }
        }
        IrStatement::InterfaceDeclaration {
            fields,
            methods,
            default_methods,
            nested_types,
            ..
        } => {
            for field in fields {
                gather_return_types_from_statement(field, acc);
            }
            for method in methods {
                gather_return_types_from_statement(method, acc);
            }
            for default_method in default_methods {
                gather_return_types_from_statement(default_method, acc);
            }
            for nested in nested_types {
                gather_return_types_from_statement(nested, acc);
            }
        }
        IrStatement::RecordDeclaration { methods, .. } => {
            for method in methods {
                gather_return_types_from_statement(method, acc);
            }
        }
        IrStatement::MethodDeclaration { body, .. } => {
            if let Some(body_expr) = body.as_ref() {
                gather_return_types_from_expression(body_expr, acc);
            }
        }
        IrStatement::SampleDeclaration(_)
        | IrStatement::Comment { .. }
        | IrStatement::Break { .. }
        | IrStatement::Continue { .. }
        | IrStatement::Import(_)
        | IrStatement::Package { .. } => {}
    }
}

#[derive(Default)]
struct ReturnTypeAccumulator {
    value: Option<JavaType>,
    conflicted: bool,
}

impl ReturnTypeAccumulator {
    fn push(&mut self, candidate: JavaType) {
        if self.conflicted {
            return;
        }

        match &self.value {
            Some(existing) => {
                if *existing != candidate {
                    self.conflicted = true;
                }
            }
            None => {
                self.value = Some(candidate);
            }
        }
    }

    fn mark_unknown(&mut self) {
        self.conflicted = true;
    }

    fn is_conflicted(&self) -> bool {
        self.conflicted
    }

    fn into_option(self) -> Option<JavaType> {
        if self.conflicted {
            None
        } else {
            self.value
        }
    }
}

fn convert_expression_to_ir(
    expr: Expression,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    match transform_expression(expr.clone(), context) {
        Ok(ir) => Ok(ir),
        Err(err) => match expr {
            Expression::Literal(lit, span) => {
                if let Literal::Regex(regex) = &lit {
                    return Ok(IrExpression::RegexPattern {
                        pattern: regex.pattern.clone(),
                        java_type: JavaType::pattern(),
                        span: regex.span.clone(),
                    });
                }
                Ok(IrExpression::Literal(lit, span))
            }
            Expression::RegexLiteral(literal) => Ok(IrExpression::RegexPattern {
                pattern: literal.pattern.clone(),
                java_type: JavaType::pattern(),
                span: literal.span.clone(),
            }),
            Expression::Identifier(name, span) => Ok(IrExpression::Identifier {
                name,
                java_type: JavaType::object(),
                span,
            }),
            _ => Err(err),
        },
    }
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
                java_name: None,
                resolved_target: None,
                args: final_args,
                argument_style: CallArgumentStyle::Comma,
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
                java_name: None,
                resolved_target: None,
                args: final_args,
                argument_style: CallArgumentStyle::Comma,
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
                java_name: None,
                resolved_target: None,
                args: all_args,
                argument_style: CallArgumentStyle::Comma,
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
            primitive_return,
            body,
            modifiers,
            span,
            type_parameters,
            generic_signature,
            ..
        } => {
            let should_infer_return = return_type.is_none();

            // Convert return type
            let mut java_return_type = match return_type {
                Some(type_ann) => convert_type_annotation(type_ann)?,
                None => JavaType::void(),
            };

            let mut ir_type_parameters = type_parameters
                .into_iter()
                .map(|tp| crate::types::IrTypeParameter::new(tp.clone(), span.clone()))
                .collect::<Vec<_>>();

            if let Some(signature) = generic_signature {
                use std::collections::HashMap;

                let index_by_name: HashMap<_, _> = ir_type_parameters
                    .iter()
                    .enumerate()
                    .map(|(idx, param)| (param.name.clone(), idx))
                    .collect();

                for parameter in &signature.parameters {
                    if let Some(&index) = index_by_name.get(&parameter.name) {
                        let bounds = parameter
                            .bounds
                            .iter()
                            .map(|annotation: &TypeAnnotation| {
                                convert_type_annotation(annotation.clone())
                            })
                            .collect::<Result<Vec<_>, _>>()?;
                        ir_type_parameters[index].bounds = bounds;

                        if let Some(variance) = parameter.variance.clone() {
                            ir_type_parameters[index].variance = match variance {
                                VarianceMarker::Covariant => crate::types::IrVariance::Covariant,
                                VarianceMarker::Contravariant => {
                                    crate::types::IrVariance::Contravariant
                                }
                            };
                        }

                        if let Some(kind) = parameter.kind.clone() {
                            ir_type_parameters[index].kind = Some(kind);
                        }
                    }
                }
            }

            // Register the function's return type so subsequent calls can resolve it.
            context
                .type_info
                .insert(name.clone(), java_return_type.clone());

            // Convert modifiers to IR modifiers
            let mut ir_modifiers = IrModifiers {
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

            let mut throws = Vec::new();

            context.enter_scope();

            let mut ir_parameters = Vec::new();

            if name == "main" && parameters.is_empty() {
                ir_modifiers.visibility = IrVisibility::Public;

                let args_type = JavaType::Array {
                    element_type: Box::new(JavaType::Reference {
                        name: "String".to_string(),
                        generic_args: vec![],
                    }),
                    dimensions: 1,
                };

                context.add_variable("args".to_string(), args_type.clone());

                ir_parameters.push(IrParameter {
                    name: "args".to_string(),
                    java_type: args_type,
                    modifiers: IrModifiers::default(),
                    span: span.clone(),
                });

                throws.push("java.lang.Exception".to_string());
            } else {
                for param in parameters.into_iter() {
                    let java_type = match param.type_annotation {
                        Some(type_ann) => convert_type_annotation(type_ann)?,
                        None => JavaType::object(),
                    };

                    context.add_variable(param.name.clone(), java_type.clone());

                    ir_parameters.push(IrParameter {
                        name: param.name,
                        java_type,
                        modifiers: IrModifiers::default(),
                        span: param.span,
                    });
                }
            }

            let signature_params: Vec<JavaType> = ir_parameters
                .iter()
                .map(|param| param.java_type.clone())
                .collect();
            context.register_function_signature(name.clone(), signature_params);

            // Convert function body to IR expression
            let body_ir_result = convert_expression_to_ir(*body, context);
            context.exit_scope();
            let ir_body = body_ir_result?;

            if should_infer_return {
                if let Some(inferred) = infer_return_type_from_ir_expression(&ir_body) {
                    java_return_type = inferred;
                    context
                        .type_info
                        .insert(name.clone(), java_return_type.clone());
                }
            }

            // Create method declaration
            let mut method = IrStatement::MethodDeclaration {
                name,
                java_name: None,
                type_parameters: ir_type_parameters,
                parameters: ir_parameters,
                primitive_return,
                return_type: java_return_type,
                body: Some(ir_body),
                modifiers: ir_modifiers,
                throws,
                span,
            };

            context.bind_method_declaration(&mut method, None);

            Ok(method)
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
