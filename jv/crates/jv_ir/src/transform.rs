use crate::context::TransformContext;
use crate::error::TransformError;
use crate::types::*;
use jv_ast::*;

// Main transformation function
pub fn transform_program(program: Program) -> Result<IrProgram, TransformError> {
    let mut context = TransformContext::new();
    transform_program_with_context(program, &mut context)
}

pub fn transform_program_with_context(
    program: Program,
    context: &mut TransformContext,
) -> Result<IrProgram, TransformError> {
    let mut ir_statements = Vec::new();

    for stmt in program.statements {
        let mut transformed = transform_statement(stmt, context)?;
        ir_statements.append(&mut transformed);
    }

    Ok(IrProgram {
        package: program.package,
        imports: Vec::new(), // TODO: handle imports
        type_declarations: ir_statements,
        span: program.span,
    })
}

// Transformation functions for different AST nodes (all will panic in Red phase)
pub fn transform_statement(
    stmt: Statement,
    context: &mut TransformContext,
) -> Result<Vec<IrStatement>, TransformError> {
    match stmt {
        Statement::ValDeclaration {
            name,
            type_annotation,
            initializer,
            modifiers,
            span,
        } => Ok(vec![desugar_val_declaration(
            name,
            type_annotation,
            initializer,
            modifiers,
            span,
            context,
        )?]),
        Statement::VarDeclaration {
            name,
            type_annotation,
            initializer,
            modifiers,
            span,
        } => Ok(vec![desugar_var_declaration(
            name,
            type_annotation,
            initializer,
            modifiers,
            span,
            context,
        )?]),
        Statement::Expression { expr, span } => {
            let ir_expr = transform_expression(expr, context)?;
            Ok(vec![IrStatement::Expression {
                expr: ir_expr,
                span,
            }])
        }
        Statement::Concurrency(construct) => match construct {
            ConcurrencyConstruct::Spawn { body, span } => {
                let expression_span = span.clone();
                let ir_expr = desugar_spawn_expression(body, span, context)?;
                Ok(vec![IrStatement::Expression {
                    expr: ir_expr,
                    span: expression_span,
                }])
            }
            ConcurrencyConstruct::Async { body, span } => {
                let expression_span = span.clone();
                let ir_expr = desugar_async_expression(body, span, context)?;
                Ok(vec![IrStatement::Expression {
                    expr: ir_expr,
                    span: expression_span,
                }])
            }
            ConcurrencyConstruct::Await { expr, span } => {
                let expression_span = span.clone();
                let ir_expr = desugar_await_expression(expr, span, context)?;
                Ok(vec![IrStatement::Expression {
                    expr: ir_expr,
                    span: expression_span,
                }])
            }
        },
        Statement::ResourceManagement(management) => match management {
            ResourceManagement::Use {
                resource,
                body,
                span,
            } => {
                let expression_span = span.clone();
                let ir_expr = desugar_use_expression(resource, body, span, context)?;
                Ok(vec![IrStatement::Expression {
                    expr: ir_expr,
                    span: expression_span,
                }])
            }
            ResourceManagement::Defer { body, span } => {
                let expression_span = span.clone();
                let ir_expr = desugar_defer_expression(body, span, context)?;
                Ok(vec![IrStatement::Expression {
                    expr: ir_expr,
                    span: expression_span,
                }])
            }
        },
        _ => {
            // For now, return empty for unimplemented statements
            Ok(vec![])
        }
    }
}

pub fn transform_expression(
    expr: Expression,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    match expr {
        Expression::Literal(lit, span) => {
            let _java_type = match &lit {
                Literal::String(_) => JavaType::Reference {
                    name: "String".to_string(),
                    generic_args: vec![],
                },
                Literal::Number(_) => JavaType::Primitive("int".to_string()),
                Literal::Boolean(_) => JavaType::Primitive("boolean".to_string()),
                Literal::Character(_) => JavaType::Primitive("char".to_string()),
                Literal::Null => JavaType::Reference {
                    name: "Object".to_string(),
                    generic_args: vec![],
                },
            };
            Ok(IrExpression::Literal(lit, span))
        }
        Expression::Identifier(name, span) => {
            if let Some(java_type) = context.lookup_variable(&name).cloned() {
                Ok(IrExpression::Identifier {
                    name,
                    java_type,
                    span,
                })
            } else {
                Err(TransformError::ScopeError {
                    message: format!("Unknown identifier '{name}'"),
                    span,
                })
            }
        }
        Expression::NullSafeMemberAccess {
            object,
            property,
            span,
        } => desugar_null_safe_member_access(object, property, span, context),
        Expression::NullSafeIndexAccess {
            object,
            index,
            span,
        } => desugar_null_safe_index_access(object, index, span, context),
        Expression::StringInterpolation { parts, span } => {
            desugar_string_interpolation(parts, span, context)
        }
        Expression::Block { statements, span } => {
            context.enter_scope();
            let mut ir_statements = Vec::new();
            for stmt in statements {
                let mut transformed = transform_statement(stmt, context)?;
                ir_statements.append(&mut transformed);
            }
            context.exit_scope();
            Ok(IrExpression::Block {
                statements: ir_statements,
                java_type: JavaType::void(),
                span,
            })
        }
        Expression::This(span) => {
            if let Some(java_type) = context.lookup_variable("this").cloned() {
                Ok(IrExpression::This { java_type, span })
            } else {
                Err(TransformError::ScopeError {
                    message: "'this' is not available in the current context".to_string(),
                    span,
                })
            }
        }
        Expression::Binary {
            left,
            op,
            right,
            span,
        } => {
            if matches!(op, BinaryOp::Elvis) {
                desugar_elvis_operator(left, right, span, context)
            } else {
                let left_ir = transform_expression(*left, context)?;
                let right_ir = transform_expression(*right, context)?;
                let java_type = JavaType::Primitive("int".to_string()); // TODO: proper type inference
                Ok(IrExpression::Binary {
                    left: Box::new(left_ir),
                    op,
                    right: Box::new(right_ir),
                    java_type,
                    span,
                })
            }
        }
        _ => {
            // For now, create a placeholder for unimplemented expressions
            Ok(IrExpression::Literal(Literal::Null, Span::default()))
        }
    }
}

// Desugaring functions for specific constructs
fn convert_modifiers(modifiers: &Modifiers) -> IrModifiers {
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

pub fn desugar_val_declaration(
    name: String,
    type_annotation: Option<TypeAnnotation>,
    initializer: Expression,
    modifiers: Modifiers,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrStatement, TransformError> {
    let ir_initializer = transform_expression(initializer, context)?;
    let java_type = infer_java_type(type_annotation, Some(&ir_initializer), context)?;

    let mut ir_modifiers = convert_modifiers(&modifiers);
    ir_modifiers.is_final = true;

    context.add_variable(name.clone(), java_type.clone());

    Ok(IrStatement::VariableDeclaration {
        name,
        java_type,
        initializer: Some(ir_initializer),
        is_final: true,
        modifiers: ir_modifiers,
        span,
    })
}

pub fn desugar_var_declaration(
    name: String,
    mut type_annotation: Option<TypeAnnotation>,
    initializer: Option<Expression>,
    modifiers: Modifiers,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrStatement, TransformError> {
    if type_annotation.is_none() && initializer.is_none() {
        return Err(TransformError::TypeInferenceError {
            message:
                "Cannot infer type for 'var' declaration without type annotation or initializer"
                    .to_string(),
            span,
        });
    }

    let ir_initializer = match initializer {
        Some(expr) => Some(transform_expression(expr, context)?),
        None => None,
    };

    let java_type = infer_java_type(type_annotation.take(), ir_initializer.as_ref(), context)?;

    let mut ir_modifiers = convert_modifiers(&modifiers);
    ir_modifiers.is_final = modifiers.is_final;

    context.add_variable(name.clone(), java_type.clone());

    Ok(IrStatement::VariableDeclaration {
        name,
        java_type,
        initializer: ir_initializer,
        is_final: false,
        modifiers: ir_modifiers,
        span,
    })
}

pub fn desugar_when_expression(
    expr: Option<Box<Expression>>,
    arms: Vec<WhenArm>,
    else_arm: Option<Box<Expression>>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let subject_expr = expr.ok_or_else(|| TransformError::UnsupportedConstruct {
        construct: "when expressions without subject".to_string(),
        span: span.clone(),
    })?;

    let discriminant = transform_expression(*subject_expr, context)?;

    let mut cases = Vec::new();
    let mut result_type: Option<JavaType> = None;
    let mut has_default_case = false;

    for arm in arms {
        let (labels, is_default) = convert_when_pattern(&arm.pattern, arm.span.clone())?;

        if is_default {
            if has_default_case {
                return Err(TransformError::UnsupportedConstruct {
                    construct: "Multiple default patterns in when expression".to_string(),
                    span: arm.span,
                });
            }
            has_default_case = true;
        }

        let body = transform_expression(arm.body, context)?;
        if result_type.is_none() {
            result_type = extract_java_type(&body);
        }

        cases.push(IrSwitchCase {
            labels,
            guard: None,
            body,
            span: arm.span,
        });
    }

    if let Some(else_expr) = else_arm {
        if has_default_case {
            return Err(TransformError::UnsupportedConstruct {
                construct: "when expression cannot have both default pattern and else arm"
                    .to_string(),
                span,
            });
        }

        let body = transform_expression(*else_expr, context)?;
        if result_type.is_none() {
            result_type = extract_java_type(&body);
        }

        cases.push(IrSwitchCase {
            labels: vec![IrCaseLabel::Default],
            guard: None,
            body,
            span: span.clone(),
        });
        has_default_case = true;
    }

    if cases.is_empty() {
        return Err(TransformError::UnsupportedConstruct {
            construct: "when expression must have at least one arm".to_string(),
            span,
        });
    }

    if !has_default_case {
        cases.push(IrSwitchCase {
            labels: vec![IrCaseLabel::Default],
            guard: None,
            body: IrExpression::Literal(Literal::Null, span.clone()),
            span: span.clone(),
        });
        if result_type.is_none() {
            result_type = Some(JavaType::object());
        }
    }

    let java_type = result_type.unwrap_or_else(JavaType::object);

    Ok(IrExpression::Switch {
        discriminant: Box::new(discriminant),
        cases,
        java_type,
        span,
    })
}

pub fn desugar_extension_function(
    receiver_type: TypeAnnotation,
    function: Box<Statement>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrStatement, TransformError> {
    let receiver_java_type = convert_type_annotation(receiver_type)?;

    let (
        function_name,
        function_parameters,
        function_return_type,
        function_body,
        function_modifiers,
        function_span,
    ) = match *function {
        Statement::FunctionDeclaration {
            name,
            parameters,
            return_type,
            body,
            modifiers,
            span,
        } => (name, parameters, return_type, body, modifiers, span),
        _ => {
            return Err(TransformError::ExtensionFunctionError {
                message: "Extension function must wrap a function declaration".to_string(),
                span,
            })
        }
    };

    context.enter_scope();

    let receiver_param_name = "receiver".to_string();
    context.add_variable(receiver_param_name.clone(), receiver_java_type.clone());
    context.add_variable("this".to_string(), receiver_java_type.clone());

    let mut ir_parameters = Vec::new();
    ir_parameters.push(IrParameter {
        name: receiver_param_name.clone(),
        java_type: receiver_java_type.clone(),
        modifiers: IrModifiers::default(),
        span,
    });

    for param in function_parameters.into_iter() {
        let java_type = match param.type_annotation {
            Some(annotation) => convert_type_annotation(annotation)?,
            None => JavaType::object(),
        };

        context.add_variable(param.name.clone(), java_type.clone());

        let mut param_modifiers = IrModifiers::default();
        param_modifiers.is_final = true;

        ir_parameters.push(IrParameter {
            name: param.name,
            java_type,
            modifiers: param_modifiers,
            span: param.span,
        });
    }

    let body_ir_result = transform_expression(*function_body, context);
    context.exit_scope();
    let body_ir = body_ir_result?;
    let body_ir = replace_this_ir_expression(body_ir, &receiver_param_name, &receiver_java_type);

    let mut method_modifiers = convert_modifiers(&function_modifiers);
    method_modifiers.is_static = true;

    let return_java_type = match function_return_type {
        Some(annotation) => convert_type_annotation(annotation)?,
        None => JavaType::void(),
    };

    Ok(IrStatement::MethodDeclaration {
        name: function_name,
        parameters: ir_parameters,
        return_type: return_java_type,
        body: Some(body_ir),
        modifiers: method_modifiers,
        throws: Vec::new(),
        span: function_span,
    })
}

pub fn desugar_string_interpolation(
    parts: Vec<StringPart>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let mut has_expression = false;
    let mut format_string = String::new();
    let mut raw_text = String::new();
    let mut args = Vec::new();

    for part in parts {
        match part {
            StringPart::Text(text) => {
                format_string.push_str(&escape_format_text(&text));
                raw_text.push_str(&text);
            }
            StringPart::Expression(expr) => {
                has_expression = true;
                format_string.push_str("%s");
                let ir_expr = transform_expression(expr, context)?;
                args.push(ir_expr);
            }
        }
    }

    if !has_expression {
        return Ok(IrExpression::Literal(Literal::String(raw_text), span));
    }

    Ok(IrExpression::StringFormat {
        format_string,
        args,
        span,
    })
}

pub fn desugar_spawn_expression(
    body: Box<Expression>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let body_ir = transform_expression(*body, context)?;

    let lambda = IrExpression::Lambda {
        functional_interface: "Runnable".to_string(),
        param_names: Vec::new(),
        param_types: Vec::new(),
        body: Box::new(body_ir),
        java_type: JavaType::Reference {
            name: "Runnable".to_string(),
            generic_args: vec![],
        },
        span: span.clone(),
    };

    Ok(IrExpression::VirtualThread {
        operation: VirtualThreadOp::Start,
        args: vec![lambda],
        java_type: JavaType::Reference {
            name: "Thread".to_string(),
            generic_args: vec![],
        },
        span,
    })
}

pub fn desugar_async_expression(
    body: Box<Expression>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let body_ir = transform_expression(*body, context)?;
    let result_type = extract_java_type(&body_ir).unwrap_or_else(JavaType::object);

    let lambda = IrExpression::Lambda {
        functional_interface: "Supplier".to_string(),
        param_names: Vec::new(),
        param_types: Vec::new(),
        body: Box::new(body_ir),
        java_type: JavaType::Reference {
            name: "Supplier".to_string(),
            generic_args: vec![result_type.clone()],
        },
        span: span.clone(),
    };

    Ok(IrExpression::CompletableFuture {
        operation: CompletableFutureOp::SupplyAsync,
        args: vec![lambda],
        java_type: JavaType::Reference {
            name: "CompletableFuture".to_string(),
            generic_args: vec![result_type],
        },
        span,
    })
}

pub fn desugar_await_expression(
    expr: Box<Expression>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let future_ir = transform_expression(*expr, context)?;

    let awaited_type = match extract_java_type(&future_ir) {
        Some(JavaType::Reference { name, generic_args })
            if name == "CompletableFuture" && !generic_args.is_empty() =>
        {
            generic_args[0].clone()
        }
        _ => JavaType::object(),
    };

    Ok(IrExpression::CompletableFuture {
        operation: CompletableFutureOp::Get,
        args: vec![future_ir],
        java_type: awaited_type,
        span,
    })
}

pub fn desugar_use_expression(
    resource: Box<Expression>,
    body: Box<Expression>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let resource_ir = transform_expression(*resource, context)?;
    let mut resource_name = match &resource_ir {
        IrExpression::Identifier { name, .. } => name.clone(),
        _ => "__resource".to_string(),
    };
    let mut resource_type = extract_java_type(&resource_ir).unwrap_or_else(JavaType::object);

    let body_ir = match *body {
        Expression::Lambda {
            parameters,
            body: lambda_body,
            ..
        } => {
            let param = parameters.get(0);
            if let Some(param) = param {
                resource_name = param.name.clone();
                if let Some(annotation) = &param.type_annotation {
                    resource_type = convert_type_annotation(annotation.clone())?;
                }
            }
            context.enter_scope();
            context.add_variable(resource_name.clone(), resource_type.clone());
            let body_ir = transform_expression(*lambda_body, context)?;
            context.exit_scope();
            body_ir
        }
        other => transform_expression(other, context)?,
    };

    let result_type = extract_java_type(&body_ir).unwrap_or_else(JavaType::void);

    Ok(IrExpression::TryWithResources {
        resources: vec![IrResource {
            name: resource_name,
            initializer: resource_ir,
            java_type: resource_type,
            span: span.clone(),
        }],
        body: Box::new(body_ir),
        java_type: result_type,
        span,
    })
}

pub fn desugar_defer_expression(
    body: Box<Expression>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let body_ir = transform_expression(*body, context)?;

    Ok(IrExpression::Block {
        statements: vec![IrStatement::Expression {
            expr: body_ir,
            span: span.clone(),
        }],
        java_type: JavaType::void(),
        span,
    })
}

pub fn desugar_default_parameters(
    function_name: String,
    parameters: Vec<Parameter>,
    return_type: Option<TypeAnnotation>,
    body: Box<Expression>,
    modifiers: Modifiers,
    span: Span,
    context: &mut TransformContext,
) -> Result<Vec<MethodOverload>, TransformError> {
    panic!("not yet implemented: desugar_default_parameters")
}

pub fn desugar_named_arguments(
    function: Box<Expression>,
    args: Vec<Argument>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    panic!("not yet implemented: desugar_named_arguments")
}

pub fn desugar_top_level_function(
    function: Statement,
    context: &mut TransformContext,
) -> Result<IrStatement, TransformError> {
    panic!("not yet implemented: desugar_top_level_function")
}

pub fn desugar_data_class(
    name: String,
    parameters: Vec<Parameter>,
    type_parameters: Vec<String>,
    is_mutable: bool,
    modifiers: Modifiers,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrStatement, TransformError> {
    let ir_modifiers = convert_modifiers(&modifiers);
    let ir_type_parameters = type_parameters
        .into_iter()
        .map(|tp| IrTypeParameter {
            name: tp,
            bounds: Vec::new(),
            span: span.clone(),
        })
        .collect::<Vec<_>>();

    if !is_mutable {
        let mut components = Vec::new();

        for param in parameters {
            let java_type = match param.type_annotation {
                Some(annotation) => convert_type_annotation(annotation)?,
                None => JavaType::object(),
            };

            components.push(IrRecordComponent {
                name: param.name,
                java_type,
                span: param.span,
            });
        }

        return Ok(IrStatement::RecordDeclaration {
            name,
            type_parameters: ir_type_parameters,
            components,
            interfaces: Vec::new(),
            methods: Vec::new(),
            modifiers: ir_modifiers,
            span,
        });
    }

    let mut fields = Vec::new();

    for param in parameters {
        let initializer_ir = match param.default_value {
            Some(expr) => Some(transform_expression(expr, context)?),
            None => None,
        };

        let java_type = match param.type_annotation {
            Some(annotation) => convert_type_annotation(annotation)?,
            None => initializer_ir
                .as_ref()
                .and_then(|expr| extract_java_type(expr))
                .unwrap_or_else(JavaType::object),
        };

        let mut field_modifiers = IrModifiers::default();
        field_modifiers.is_final = false;

        fields.push(IrStatement::FieldDeclaration {
            name: param.name,
            java_type,
            initializer: initializer_ir,
            modifiers: field_modifiers,
            span: param.span,
        });
    }

    Ok(IrStatement::ClassDeclaration {
        name,
        type_parameters: ir_type_parameters,
        superclass: None,
        interfaces: Vec::new(),
        fields,
        methods: Vec::new(),
        nested_classes: Vec::new(),
        modifiers: ir_modifiers,
        span,
    })
}

pub fn desugar_null_safe_member_access(
    object: Box<Expression>,
    property: String,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let ir_object = transform_expression(*object, context)?;
    let object_type = extract_java_type(&ir_object).unwrap_or_else(JavaType::object);

    if matches!(object_type, JavaType::Primitive(_)) {
        return Err(TransformError::NullSafetyError {
            message: "Cannot apply null-safe access to primitive types".to_string(),
            span,
        });
    }

    let result_type = JavaType::object();
    let operation = IrExpression::FieldAccess {
        receiver: Box::new(ir_object.clone()),
        field_name: property,
        java_type: result_type.clone(),
        span: span.clone(),
    };

    let default_value = Some(Box::new(IrExpression::Literal(Literal::Null, span.clone())));

    Ok(IrExpression::NullSafeOperation {
        expr: Box::new(ir_object),
        operation: Box::new(operation),
        default_value,
        java_type: result_type,
        span,
    })
}

pub fn desugar_null_safe_index_access(
    object: Box<Expression>,
    index: Box<Expression>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let ir_object = transform_expression(*object, context)?;
    let object_type = extract_java_type(&ir_object).unwrap_or_else(JavaType::object);

    if matches!(object_type, JavaType::Primitive(_)) {
        return Err(TransformError::NullSafetyError {
            message: "Cannot apply null-safe index access to primitive types".to_string(),
            span,
        });
    }

    let ir_index = transform_expression(*index, context)?;

    let element_type = match &object_type {
        JavaType::Array { element_type, .. } => *element_type.clone(),
        _ => JavaType::object(),
    };

    let operation = IrExpression::ArrayAccess {
        array: Box::new(ir_object.clone()),
        index: Box::new(ir_index),
        java_type: element_type.clone(),
        span: span.clone(),
    };

    let default_value = if matches!(element_type, JavaType::Primitive(_)) {
        None
    } else {
        Some(Box::new(IrExpression::Literal(Literal::Null, span.clone())))
    };

    Ok(IrExpression::NullSafeOperation {
        expr: Box::new(ir_object),
        operation: Box::new(operation),
        default_value,
        java_type: element_type,
        span,
    })
}

pub fn desugar_elvis_operator(
    left: Box<Expression>,
    right: Box<Expression>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let left_expr = transform_expression(*left, context)?;
    let right_expr = transform_expression(*right, context)?;

    if let Some(JavaType::Primitive(_)) = extract_java_type(&left_expr) {
        return Err(TransformError::NullSafetyError {
            message: "Elvis operator requires nullable left-hand expression".to_string(),
            span,
        });
    }

    let left_type = extract_java_type(&left_expr);
    let right_type = extract_java_type(&right_expr);
    let result_type = left_type
        .or(right_type.clone())
        .unwrap_or_else(JavaType::object);

    Ok(IrExpression::NullSafeOperation {
        expr: Box::new(left_expr.clone()),
        operation: Box::new(left_expr),
        default_value: Some(Box::new(right_expr)),
        java_type: result_type,
        span,
    })
}

// Type inference and conversion
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
        TypeAnnotation::Nullable(inner) => {
            // For now, treat nullable as the same as non-nullable since Java has nullable references by default
            convert_type_annotation(*inner)
        }
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

// Utility functions
fn extract_java_type(expr: &IrExpression) -> Option<JavaType> {
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

fn literal_to_java_type(literal: &Literal) -> JavaType {
    match literal {
        Literal::String(_) => JavaType::string(),
        Literal::Number(_) => JavaType::Primitive("int".to_string()),
        Literal::Boolean(_) => JavaType::boolean(),
        Literal::Character(_) => JavaType::Primitive("char".to_string()),
        Literal::Null => JavaType::object(),
    }
}

fn ir_expression_span(expr: &IrExpression) -> Span {
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
        | IrExpression::NullSafeOperation { span, .. }
        | IrExpression::StringFormat { span, .. }
        | IrExpression::CompletableFuture { span, .. }
        | IrExpression::VirtualThread { span, .. }
        | IrExpression::TryWithResources { span, .. } => span.clone(),
    }
}

fn convert_when_pattern(
    pattern: &Pattern,
    span: Span,
) -> Result<(Vec<IrCaseLabel>, bool), TransformError> {
    match pattern {
        Pattern::Literal(literal, _) => Ok((vec![IrCaseLabel::Literal(literal.clone())], false)),
        Pattern::Wildcard(_) => Ok((vec![IrCaseLabel::Default], true)),
        _ => Err(TransformError::UnsupportedConstruct {
            construct: "Unsupported when pattern".to_string(),
            span,
        }),
    }
}

fn escape_format_text(text: &str) -> String {
    text.replace('%', "%%")
}

fn replace_this_ir_expression(
    expr: IrExpression,
    replacement_name: &str,
    replacement_type: &JavaType,
) -> IrExpression {
    match expr {
        IrExpression::This { span, .. } => IrExpression::Identifier {
            name: replacement_name.to_string(),
            java_type: replacement_type.clone(),
            span,
        },
        IrExpression::MethodCall {
            receiver,
            method_name,
            args,
            java_type,
            span,
        } => IrExpression::MethodCall {
            receiver: receiver.map(|inner| {
                Box::new(replace_this_ir_expression(
                    *inner,
                    replacement_name,
                    replacement_type,
                ))
            }),
            method_name,
            args: args
                .into_iter()
                .map(|arg| replace_this_ir_expression(arg, replacement_name, replacement_type))
                .collect(),
            java_type,
            span,
        },
        IrExpression::FieldAccess {
            receiver,
            field_name,
            java_type,
            span,
        } => IrExpression::FieldAccess {
            receiver: Box::new(replace_this_ir_expression(
                *receiver,
                replacement_name,
                replacement_type,
            )),
            field_name,
            java_type,
            span,
        },
        IrExpression::ArrayAccess {
            array,
            index,
            java_type,
            span,
        } => IrExpression::ArrayAccess {
            array: Box::new(replace_this_ir_expression(
                *array,
                replacement_name,
                replacement_type,
            )),
            index: Box::new(replace_this_ir_expression(
                *index,
                replacement_name,
                replacement_type,
            )),
            java_type,
            span,
        },
        IrExpression::Binary {
            left,
            op,
            right,
            java_type,
            span,
        } => IrExpression::Binary {
            left: Box::new(replace_this_ir_expression(
                *left,
                replacement_name,
                replacement_type,
            )),
            op,
            right: Box::new(replace_this_ir_expression(
                *right,
                replacement_name,
                replacement_type,
            )),
            java_type,
            span,
        },
        IrExpression::Unary {
            op,
            operand,
            java_type,
            span,
        } => IrExpression::Unary {
            op,
            operand: Box::new(replace_this_ir_expression(
                *operand,
                replacement_name,
                replacement_type,
            )),
            java_type,
            span,
        },
        IrExpression::Assignment {
            target,
            value,
            java_type,
            span,
        } => IrExpression::Assignment {
            target: Box::new(replace_this_ir_expression(
                *target,
                replacement_name,
                replacement_type,
            )),
            value: Box::new(replace_this_ir_expression(
                *value,
                replacement_name,
                replacement_type,
            )),
            java_type,
            span,
        },
        IrExpression::Conditional {
            condition,
            then_expr,
            else_expr,
            java_type,
            span,
        } => IrExpression::Conditional {
            condition: Box::new(replace_this_ir_expression(
                *condition,
                replacement_name,
                replacement_type,
            )),
            then_expr: Box::new(replace_this_ir_expression(
                *then_expr,
                replacement_name,
                replacement_type,
            )),
            else_expr: Box::new(replace_this_ir_expression(
                *else_expr,
                replacement_name,
                replacement_type,
            )),
            java_type,
            span,
        },
        IrExpression::Block {
            statements,
            java_type,
            span,
        } => IrExpression::Block {
            statements: statements
                .into_iter()
                .map(|stmt| replace_this_in_statement(stmt, replacement_name, replacement_type))
                .collect(),
            java_type,
            span,
        },
        IrExpression::ArrayCreation {
            element_type,
            dimensions,
            initializer,
            span,
        } => IrExpression::ArrayCreation {
            element_type,
            dimensions: dimensions
                .into_iter()
                .map(|dim| {
                    dim.map(|expr| {
                        replace_this_ir_expression(expr, replacement_name, replacement_type)
                    })
                })
                .collect(),
            initializer: initializer.map(|values| {
                values
                    .into_iter()
                    .map(|expr| {
                        replace_this_ir_expression(expr, replacement_name, replacement_type)
                    })
                    .collect()
            }),
            span,
        },
        IrExpression::ObjectCreation {
            class_name,
            generic_args,
            args,
            java_type,
            span,
        } => IrExpression::ObjectCreation {
            class_name,
            generic_args,
            args: args
                .into_iter()
                .map(|expr| replace_this_ir_expression(expr, replacement_name, replacement_type))
                .collect(),
            java_type,
            span,
        },
        IrExpression::Lambda {
            functional_interface,
            param_names,
            param_types,
            body,
            java_type,
            span,
        } => IrExpression::Lambda {
            functional_interface,
            param_names,
            param_types,
            body: Box::new(replace_this_ir_expression(
                *body,
                replacement_name,
                replacement_type,
            )),
            java_type,
            span,
        },
        IrExpression::Switch {
            discriminant,
            cases,
            java_type,
            span,
        } => IrExpression::Switch {
            discriminant: Box::new(replace_this_ir_expression(
                *discriminant,
                replacement_name,
                replacement_type,
            )),
            cases: cases
                .into_iter()
                .map(|case| IrSwitchCase {
                    labels: case.labels,
                    guard: case.guard.map(|expr| {
                        replace_this_ir_expression(expr, replacement_name, replacement_type)
                    }),
                    body: replace_this_ir_expression(case.body, replacement_name, replacement_type),
                    span: case.span,
                })
                .collect(),
            java_type,
            span,
        },
        IrExpression::Cast {
            expr,
            target_type,
            span,
        } => IrExpression::Cast {
            expr: Box::new(replace_this_ir_expression(
                *expr,
                replacement_name,
                replacement_type,
            )),
            target_type,
            span,
        },
        IrExpression::InstanceOf {
            expr,
            target_type,
            span,
        } => IrExpression::InstanceOf {
            expr: Box::new(replace_this_ir_expression(
                *expr,
                replacement_name,
                replacement_type,
            )),
            target_type,
            span,
        },
        IrExpression::NullSafeOperation {
            expr,
            operation,
            default_value,
            java_type,
            span,
        } => IrExpression::NullSafeOperation {
            expr: Box::new(replace_this_ir_expression(
                *expr,
                replacement_name,
                replacement_type,
            )),
            operation: Box::new(replace_this_ir_expression(
                *operation,
                replacement_name,
                replacement_type,
            )),
            default_value: default_value.map(|expr| {
                Box::new(replace_this_ir_expression(
                    *expr,
                    replacement_name,
                    replacement_type,
                ))
            }),
            java_type,
            span,
        },
        IrExpression::StringFormat {
            format_string,
            args,
            span,
        } => IrExpression::StringFormat {
            format_string,
            args: args
                .into_iter()
                .map(|expr| replace_this_ir_expression(expr, replacement_name, replacement_type))
                .collect(),
            span,
        },
        IrExpression::CompletableFuture {
            operation,
            args,
            java_type,
            span,
        } => IrExpression::CompletableFuture {
            operation,
            args: args
                .into_iter()
                .map(|expr| replace_this_ir_expression(expr, replacement_name, replacement_type))
                .collect(),
            java_type,
            span,
        },
        IrExpression::VirtualThread {
            operation,
            args,
            java_type,
            span,
        } => IrExpression::VirtualThread {
            operation,
            args: args
                .into_iter()
                .map(|expr| replace_this_ir_expression(expr, replacement_name, replacement_type))
                .collect(),
            java_type,
            span,
        },
        IrExpression::TryWithResources {
            resources,
            body,
            java_type,
            span,
        } => IrExpression::TryWithResources {
            resources: resources
                .into_iter()
                .map(|res| IrResource {
                    name: res.name,
                    initializer: replace_this_ir_expression(
                        res.initializer,
                        replacement_name,
                        replacement_type,
                    ),
                    java_type: res.java_type,
                    span: res.span,
                })
                .collect(),
            body: Box::new(replace_this_ir_expression(
                *body,
                replacement_name,
                replacement_type,
            )),
            java_type,
            span,
        },
        other => other,
    }
}

fn replace_this_in_statement(
    stmt: IrStatement,
    replacement_name: &str,
    replacement_type: &JavaType,
) -> IrStatement {
    match stmt {
        IrStatement::Expression { expr, span } => IrStatement::Expression {
            expr: replace_this_ir_expression(expr, replacement_name, replacement_type),
            span,
        },
        IrStatement::Return { value, span } => IrStatement::Return {
            value: value
                .map(|expr| replace_this_ir_expression(expr, replacement_name, replacement_type)),
            span,
        },
        IrStatement::VariableDeclaration {
            name,
            java_type,
            initializer,
            is_final,
            modifiers,
            span,
        } => IrStatement::VariableDeclaration {
            name,
            java_type,
            initializer: initializer
                .map(|expr| replace_this_ir_expression(expr, replacement_name, replacement_type)),
            is_final,
            modifiers,
            span,
        },
        other => other,
    }
}

pub fn generate_utility_class_name(package: Option<&str>, source_file: &str) -> String {
    panic!("not yet implemented: generate_utility_class_name")
}

pub fn generate_extension_class_name(receiver_type: &TypeAnnotation) -> String {
    panic!("not yet implemented: generate_extension_class_name")
}
