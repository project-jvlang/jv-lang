use super::sample::{desugar_sample_annotation, inline_json_declaration};
use super::transform_expression;
use super::type_system::{convert_type_annotation, infer_java_type};
use super::utils::{convert_modifiers, extract_java_type};
use crate::context::TransformContext;
use crate::error::TransformError;
use crate::types::{
    IrExpression, IrModifiers, IrParameter, IrRecordComponent, IrResource, IrStatement,
    IrSwitchCase, IrTypeParameter, JavaType,
};
use jv_ast::{Expression, Modifiers, Parameter, Span, Statement, TypeAnnotation, ValBindingOrigin};

pub fn desugar_val_declaration(
    name: String,
    type_annotation: Option<TypeAnnotation>,
    initializer: Expression,
    modifiers: Modifiers,
    origin: ValBindingOrigin,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrStatement, TransformError> {
    let sample_annotations: Vec<_> = modifiers
        .annotations
        .iter()
        .filter(|annotation| annotation.name.simple_name() == "Sample")
        .cloned()
        .collect();

    if !sample_annotations.is_empty() {
        if sample_annotations.len() > 1 {
            return Err(TransformError::SampleAnnotationError {
                message: "@Sample は一度だけ指定してください".to_string(),
                span: span.clone(),
            });
        }

        let declaration = desugar_sample_annotation(
            name,
            type_annotation,
            initializer,
            &modifiers,
            sample_annotations[0].clone(),
            span,
            context,
        )?;

        return Ok(IrStatement::SampleDeclaration(declaration));
    }

    if let Expression::JsonLiteral(literal) = initializer.clone() {
        let declaration = inline_json_declaration(
            name.clone(),
            type_annotation.clone(),
            literal,
            span.clone(),
            context,
        )?;

        return Ok(IrStatement::SampleDeclaration(declaration));
    }

    let ir_initializer = transform_expression(initializer, context)?;
    let java_type = infer_java_type(type_annotation, Some(&ir_initializer), context)?;

    let mut ir_modifiers = convert_modifiers(&modifiers);
    let is_final = match origin {
        ValBindingOrigin::ExplicitKeyword
        | ValBindingOrigin::Implicit
        | ValBindingOrigin::ImplicitTyped => true,
    };
    ir_modifiers.is_final = is_final;

    context.add_variable(name.clone(), java_type.clone());

    Ok(IrStatement::VariableDeclaration {
        name,
        java_type,
        initializer: Some(ir_initializer),
        is_final,
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
    ir_modifiers.is_final = false;

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

pub fn desugar_extension_function(
    receiver_type: TypeAnnotation,
    function: Box<Statement>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrStatement, TransformError> {
    let receiver_java_type = convert_type_annotation(receiver_type)?;

    let (
        function_name,
        function_type_parameters,
        function_parameters,
        function_return_type,
        function_body,
        function_modifiers,
        function_span,
    ) = match *function {
        Statement::FunctionDeclaration {
            name,
            type_parameters,
            parameters,
            return_type,
            body,
            modifiers,
            span,
            ..
        } => (
            name,
            type_parameters,
            parameters,
            return_type,
            body,
            modifiers,
            span,
        ),
        _ => {
            return Err(TransformError::ExtensionFunctionError {
                message: "Extension function must wrap a function declaration".to_string(),
                span,
            })
        }
    };

    let ir_type_parameters = function_type_parameters
        .into_iter()
        .map(|tp| IrTypeParameter::new(tp, function_span.clone()))
        .collect::<Vec<_>>();

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
        type_parameters: ir_type_parameters,
        parameters: ir_parameters,
        return_type: return_java_type,
        body: Some(body_ir),
        modifiers: method_modifiers,
        throws: Vec::new(),
        span: function_span,
    })
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
        .map(|tp| IrTypeParameter::new(tp, span.clone()))
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
            argument_style,
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
            argument_style,
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
            delimiter,
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
            delimiter,
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
            implicit_end,
            span,
            strategy_description,
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
            implicit_end,
            strategy_description,
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
