use crate::context::TransformContext;
use crate::error::TransformError;
use crate::naming::method_erasure::apply_method_erasure;
use crate::profiling::{PerfMetrics, TransformProfiler};
use crate::sequence_pipeline;
use crate::types::{
    DoublebraceBaseStrategy, DoublebraceCopySourceStrategy, DoublebraceFieldUpdate,
    DoublebraceLoweringKind, DoublebraceLoweringPlan, DoublebraceLoweringStep,
    DoublebraceMethodInvocation, IrCommentKind, IrDoublebraceCopyPlan, IrDoublebraceFieldUpdate,
    IrDoublebraceMethodInvocation, IrDoublebraceMutatePlan, IrDoublebraceMutation,
    IrDoublebracePlan, IrExpression, IrImport, IrImportDetail, IrProgram, IrStatement, JavaType,
    JavaWildcardKind,
};
use jv_ast::{
    Argument, BinaryOp, BindingPatternKind, CallArgumentMetadata, CallArgumentStyle, CallKind,
    CommentKind, ConcurrencyConstruct, DoublebraceInit, Expression, Literal, Modifiers, Program,
    ResourceManagement, SequenceDelimiter, Span, Statement, TypeAnnotation, UnaryOp,
    ValBindingOrigin,
};

mod concurrency;
mod control_flow;
mod declarations;
mod functions;
mod loops;
mod null_safety;
mod resources;
mod sample;
mod strings;
mod type_system;
mod utils;
mod when_lowering_planner;

use self::utils::boxed_java_type;
pub(crate) use self::utils::{extract_java_type, ir_expression_span};
pub use crate::types::{
    DataFormat, IrSampleDeclaration, PrimitiveType, SampleFetchError, SampleFetchRequest,
    SampleFetchResult, SampleMode, SampleRecordDescriptor, SampleRecordField, SampleSourceKind,
    Schema, SchemaError,
};
pub use concurrency::{
    desugar_async_expression, desugar_await_expression, desugar_spawn_expression,
};
pub use control_flow::desugar_when_expression;
pub use declarations::{
    desugar_data_class, desugar_extension_function, desugar_val_declaration,
    desugar_var_declaration,
};
pub use functions::{
    desugar_default_parameters, desugar_named_arguments, desugar_top_level_function,
};
pub use null_safety::{
    desugar_elvis_operator, desugar_null_safe_index_access, desugar_null_safe_member_access,
};
pub use resources::{desugar_defer_expression, desugar_use_expression};
pub use sample::{fetch_sample_data, infer_json_value_schema, infer_schema};
pub use strings::desugar_string_interpolation;
pub use type_system::{convert_type_annotation, infer_java_type};
pub use utils::{generate_extension_class_name, generate_utility_class_name};

pub fn transform_program(program: Program) -> Result<IrProgram, TransformError> {
    let mut context = TransformContext::new();
    transform_program_with_context(program, &mut context)
}

pub fn transform_program_with_context(
    program: Program,
    context: &mut TransformContext,
) -> Result<IrProgram, TransformError> {
    lower_program(program, context)
}

/// Lowers a program while collecting profiling metrics.
pub fn transform_program_profiled(
    program: Program,
    profiler: &mut TransformProfiler,
) -> Result<(IrProgram, PerfMetrics), TransformError> {
    let mut context = TransformContext::new();
    transform_program_with_context_profiled(program, &mut context, profiler)
}

/// Lowers a program using the provided context while recording metrics into the profiler.
pub fn transform_program_with_context_profiled(
    program: Program,
    context: &mut TransformContext,
    profiler: &mut TransformProfiler,
) -> Result<(IrProgram, PerfMetrics), TransformError> {
    profiler.begin_session();
    let result = profiler.measure_stage("lowering", || lower_program(program, context));

    match result {
        Ok(ir_program) => {
            let metrics = profiler.finish_session(context);
            Ok((ir_program, metrics))
        }
        Err(err) => {
            profiler.abort_session();
            Err(err)
        }
    }
}

pub fn transform_statement(
    stmt: Statement,
    context: &mut TransformContext,
) -> Result<Vec<IrStatement>, TransformError> {
    match stmt {
        Statement::Comment(comment) => {
            if comment.is_passthrough() {
                Ok(vec![IrStatement::Comment {
                    kind: match comment.kind {
                        CommentKind::Line => IrCommentKind::Line,
                        CommentKind::Block => IrCommentKind::Block,
                    },
                    text: comment.text.clone(),
                    span: comment.span.clone(),
                }])
            } else {
                Ok(Vec::new())
            }
        }
        Statement::ValDeclaration {
            name,
            type_annotation,
            initializer,
            modifiers,
            origin,
            span,
            ..
        } => Ok(vec![desugar_val_declaration(
            name,
            type_annotation,
            initializer,
            modifiers,
            origin,
            span,
            context,
        )?]),
        Statement::VarDeclaration {
            name,
            type_annotation,
            initializer,
            modifiers,
            span,
            ..
        } => Ok(vec![desugar_var_declaration(
            name,
            type_annotation,
            initializer,
            modifiers,
            span,
            context,
        )?]),
        Statement::Return { value, span } => {
            let ir_value = value
                .map(|expr| transform_expression(expr, context))
                .transpose()?;
            Ok(vec![IrStatement::Return {
                value: ir_value,
                span,
            }])
        }
        Statement::Throw { expr, span } => {
            let ir_expr = transform_expression(expr, context)?;
            Ok(vec![IrStatement::Throw {
                expr: ir_expr,
                span,
            }])
        }
        Statement::FunctionDeclaration {
            name,
            parameters,
            return_type,
            primitive_return,
            body,
            modifiers,
            span,
            type_parameters,
            where_clause,
            generic_signature,
        } => Ok(vec![desugar_top_level_function(
            Statement::FunctionDeclaration {
                name,
                parameters,
                return_type,
                primitive_return,
                body,
                modifiers,
                span,
                type_parameters,
                where_clause,
                generic_signature,
            },
            context,
        )?]),
        Statement::DataClassDeclaration {
            name,
            parameters,
            type_parameters,
            is_mutable,
            modifiers,
            span,
            ..
        } => Ok(vec![desugar_data_class(
            name,
            parameters,
            type_parameters,
            is_mutable,
            modifiers,
            span,
            context,
        )?]),
        Statement::ExtensionFunction(extension) => Ok(vec![desugar_extension_function(
            extension.receiver_type,
            extension.function,
            extension.span,
            context,
        )?]),
        Statement::Expression {
            expr: Expression::Identifier(name, _),
            ..
        } if name.eq_ignore_ascii_case("as") || is_constructor_like(&name) => Ok(Vec::new()),
        Statement::Expression { expr, span } => {
            let ir_expr = transform_expression(expr, context)?;
            Ok(vec![IrStatement::Expression {
                expr: ir_expr,
                span,
            }])
        }
        Statement::Assignment {
            target,
            value,
            binding_pattern,
            span,
        } => {
            if let Some(binding_name) =
                infer_implicit_binding_name(binding_pattern.as_ref(), &target)
            {
                if context.lookup_variable(&binding_name).is_none() {
                    return Ok(vec![desugar_implicit_val_assignment(
                        binding_name,
                        value,
                        span,
                        context,
                    )?]);
                }
            }

            let ir_target = transform_expression(target, context)?;
            let java_type = extract_java_type(&ir_target).ok_or_else(|| {
                TransformError::TypeInferenceError {
                    message: "Cannot determine assignment target type".to_string(),
                    span: span.clone(),
                }
            })?;

            let ir_value = transform_expression(value, context)?;

            Ok(vec![IrStatement::Expression {
                expr: IrExpression::Assignment {
                    target: Box::new(ir_target),
                    value: Box::new(ir_value),
                    java_type,
                    span: span.clone(),
                },
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
        Statement::ForIn(for_in) => loops::desugar_for_in_statement(for_in, context),
        _ => Ok(vec![]),
    }
}

fn desugar_implicit_val_assignment(
    name: String,
    initializer: Expression,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrStatement, TransformError> {
    desugar_val_declaration(
        name,
        None,
        initializer,
        Modifiers::default(),
        ValBindingOrigin::Implicit,
        span,
        context,
    )
}

fn infer_implicit_binding_name(
    binding_pattern: Option<&BindingPatternKind>,
    target: &Expression,
) -> Option<String> {
    match (binding_pattern, target) {
        (Some(BindingPatternKind::Identifier { name, .. }), Expression::Identifier(target, _))
            if target == name =>
        {
            Some(name.clone())
        }
        (None, Expression::Identifier(name, _)) => Some(name.clone()),
        _ => None,
    }
}

fn lower_program(
    program: Program,
    context: &mut TransformContext,
) -> Result<IrProgram, TransformError> {
    let Program {
        package,
        imports,
        statements,
        span,
        ..
    } = program;

    let pool_guard = context.begin_lowering_session();
    let mut ir_statements = Vec::new();

    for stmt in statements {
        let mut transformed = transform_statement(stmt, context)?;
        ir_statements.append(&mut transformed);
    }

    drop(pool_guard);
    context.finish_lowering_session();

    let mut type_declarations = attach_trailing_comments(ir_statements);
    apply_method_erasure(&mut type_declarations, context);

    let ir_imports = if context.has_resolved_imports() {
        context
            .take_resolved_imports()
            .into_iter()
            .map(IrStatement::Import)
            .collect()
    } else {
        imports
            .into_iter()
            .filter_map(|statement| {
                if let Statement::Import {
                    path,
                    alias,
                    is_wildcard,
                    span,
                } = statement
                {
                    let detail = if is_wildcard {
                        IrImportDetail::Package { name: path.clone() }
                    } else {
                        IrImportDetail::Type { fqcn: path.clone() }
                    };

                    Some(IrStatement::Import(IrImport {
                        original: path,
                        alias,
                        detail,
                        module_dependency: None,
                        span,
                    }))
                } else {
                    None
                }
            })
            .collect()
    };

    Ok(IrProgram {
        package,
        imports: ir_imports,
        type_declarations,
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span,
    })
}

fn attach_trailing_comments(statements: Vec<IrStatement>) -> Vec<IrStatement> {
    let mut result = Vec::with_capacity(statements.len());

    for statement in statements {
        match statement {
            IrStatement::Comment { kind, text, span } => {
                if let Some(last) = result.pop() {
                    if should_attach_trailing_comment(&last, &span) {
                        result.push(IrStatement::Commented {
                            statement: Box::new(last),
                            comment: text,
                            kind,
                            comment_span: span,
                        });
                        continue;
                    } else {
                        result.push(last);
                    }
                }

                result.push(IrStatement::Comment { kind, text, span });
            }
            other => result.push(other),
        }
    }

    result
}

fn should_attach_trailing_comment(statement: &IrStatement, comment_span: &Span) -> bool {
    let base = unwrap_commented(statement);
    if matches!(base, IrStatement::Comment { .. }) {
        return false;
    }

    if let Some(span) = ir_statement_span(base) {
        if span.end_line == comment_span.start_line {
            return comment_span.start_column >= span.end_column;
        }
    }

    false
}

fn unwrap_commented<'a>(statement: &'a IrStatement) -> &'a IrStatement {
    match statement {
        IrStatement::Commented { statement, .. } => unwrap_commented(statement),
        other => other,
    }
}

fn ir_statement_span(statement: &IrStatement) -> Option<Span> {
    match statement {
        IrStatement::VariableDeclaration { span, .. }
        | IrStatement::MethodDeclaration { span, .. }
        | IrStatement::ClassDeclaration { span, .. }
        | IrStatement::InterfaceDeclaration { span, .. }
        | IrStatement::RecordDeclaration { span, .. }
        | IrStatement::FieldDeclaration { span, .. }
        | IrStatement::Expression { span, .. }
        | IrStatement::Return { span, .. }
        | IrStatement::If { span, .. }
        | IrStatement::While { span, .. }
        | IrStatement::ForEach { span, .. }
        | IrStatement::For { span, .. }
        | IrStatement::Switch { span, .. }
        | IrStatement::Try { span, .. }
        | IrStatement::TryWithResources { span, .. }
        | IrStatement::Throw { span, .. }
        | IrStatement::Break { span, .. }
        | IrStatement::Continue { span, .. }
        | IrStatement::Block { span, .. }
        | IrStatement::Package { span, .. }
        | IrStatement::Comment { span, .. } => Some(span.clone()),
        IrStatement::Import(import) => Some(import.span.clone()),
        IrStatement::SampleDeclaration(decl) => Some(decl.span.clone()),
        IrStatement::Commented { statement, .. } => ir_statement_span(statement),
    }
}

pub fn transform_expression(
    expr: Expression,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    match expr {
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
        Expression::Identifier(name, span) => {
            if name.is_empty() {
                return Ok(IrExpression::Literal(Literal::String(String::new()), span));
            }
            if let Some(java_type) = context.lookup_variable(&name).cloned() {
                return Ok(IrExpression::Identifier {
                    name,
                    java_type,
                    span,
                });
            }

            if is_constructor_like(&name) {
                let fqcn = if let Some(package) = context.current_package.as_deref() {
                    format!("{package}.{name}")
                } else {
                    name.clone()
                };
                return Ok(IrExpression::Identifier {
                    name,
                    java_type: JavaType::Reference {
                        name: fqcn,
                        generic_args: vec![],
                    },
                    span,
                });
            }

            Err(TransformError::ScopeError {
                message: format!("Unknown identifier '{name}'"),
                span,
            })
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
        Expression::TypeCast { expr, target, span } => {
            let lowered = transform_expression(*expr, context)?;
            let target_type = convert_type_annotation(target)?;
            Ok(IrExpression::Cast {
                expr: Box::new(lowered),
                target_type,
                span,
            })
        }
        Expression::StringInterpolation { parts, span } => {
            desugar_string_interpolation(parts, span, context)
        }
        Expression::MultilineString(literal) => {
            if literal.parts.is_empty() {
                Ok(IrExpression::Literal(
                    Literal::String(literal.normalized),
                    literal.span,
                ))
            } else {
                desugar_string_interpolation(literal.parts, literal.span, context)
            }
        }
        Expression::Block { statements, span } => {
            context.enter_scope();
            let mut ir_statements = Vec::new();
            for stmt in statements {
                let mut transformed = transform_statement(stmt, context)?;
                ir_statements.append(&mut transformed);
            }
            context.exit_scope();
            let statements = attach_trailing_comments(ir_statements);
            Ok(IrExpression::Block {
                statements,
                java_type: JavaType::void(),
                span,
            })
        }
        Expression::DoublebraceInit(init) => lower_doublebrace_expression(init, context),
        Expression::Array {
            elements,
            delimiter,
            span,
        } => {
            let elements = if delimiter == SequenceDelimiter::Whitespace {
                normalize_whitespace_array_elements(elements)
            } else {
                elements
            };

            let mut lowered_elements = Vec::with_capacity(elements.len());
            let mut element_type: Option<JavaType> = None;

            for element in elements {
                let lowered = transform_expression(element, context)?;

                if let Some(new_type) = extract_java_type(&lowered) {
                    let updated = match element_type.take() {
                        Some(current) if current == new_type => current,
                        Some(_) => JavaType::object(),
                        None => new_type,
                    };
                    element_type = Some(updated);
                }

                lowered_elements.push(lowered);
            }

            let element_type = element_type.unwrap_or_else(JavaType::object);
            let dimensions = if delimiter == SequenceDelimiter::Whitespace {
                Vec::new()
            } else {
                vec![None]
            };

            Ok(IrExpression::ArrayCreation {
                element_type,
                dimensions,
                initializer: Some(lowered_elements),
                delimiter,
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
                let left_type = extract_java_type(&left_ir);
                let right_type = extract_java_type(&right_ir);
                let java_type = match op {
                    BinaryOp::Equal
                    | BinaryOp::NotEqual
                    | BinaryOp::Less
                    | BinaryOp::LessEqual
                    | BinaryOp::Greater
                    | BinaryOp::GreaterEqual
                    | BinaryOp::And
                    | BinaryOp::Or
                    | BinaryOp::Is => JavaType::boolean(),
                    BinaryOp::Add
                        if left_type
                            .as_ref()
                            .map(is_string_like_java_type)
                            .unwrap_or(false)
                            || right_type
                                .as_ref()
                                .map(is_string_like_java_type)
                                .unwrap_or(false) =>
                    {
                        JavaType::string()
                    }
                    BinaryOp::Add
                    | BinaryOp::Subtract
                    | BinaryOp::Multiply
                    | BinaryOp::Divide
                    | BinaryOp::Modulo => {
                        binary_numeric_result_type(left_type.as_ref(), right_type.as_ref())
                    }
                    BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor => {
                        integral_binary_result_type(left_type.as_ref(), right_type.as_ref())
                    }
                    _ => JavaType::int(),
                };
                Ok(IrExpression::Binary {
                    left: Box::new(left_ir),
                    op,
                    right: Box::new(right_ir),
                    java_type,
                    span,
                })
            }
        }
        Expression::Unary { op, operand, span } => {
            let operand_ir = transform_expression(*operand, context)?;
            let operand_type = extract_java_type(&operand_ir);

            let java_type = match op {
                UnaryOp::Not => JavaType::boolean(),
                UnaryOp::Plus | UnaryOp::Minus | UnaryOp::BitNot => {
                    operand_type.unwrap_or_else(JavaType::int)
                }
            };

            Ok(IrExpression::Unary {
                op,
                operand: Box::new(operand_ir),
                java_type,
                span,
            })
        }
        Expression::MemberAccess {
            object,
            property,
            span,
        } => {
            if let Some((field_expr, _)) =
                lower_system_field((*object).clone(), property.clone(), span.clone())
            {
                return Ok(field_expr);
            }

            if let Some(segments) = flatten_member_access_chain(&Expression::MemberAccess {
                object: object.clone(),
                property: property.clone(),
                span: span.clone(),
            }) {
                if !segments.is_empty() && context.lookup_variable(&segments[0]).is_none() {
                    let name = segments.join(".");
                    let java_type = context.lookup_variable(&name).cloned().unwrap_or_else(|| {
                        JavaType::Reference {
                            name: name.clone(),
                            generic_args: vec![],
                        }
                    });
                    return Ok(IrExpression::Identifier {
                        name,
                        java_type,
                        span,
                    });
                }
            }

            let receiver_expr = transform_expression(*object, context)?;
            let field_name = property;
            let receiver_type = extract_java_type(&receiver_expr).unwrap_or_else(JavaType::object);
            let mut field_java_type = receiver_type.clone();
            let mut is_record_component = false;

            if let JavaType::Reference { name, .. } = &receiver_type {
                if let Some(component_type) = context.record_component_type(name, &field_name) {
                    field_java_type = component_type;
                    is_record_component = true;
                }
            }

            Ok(IrExpression::FieldAccess {
                receiver: Box::new(receiver_expr),
                field_name,
                java_type: field_java_type,
                span,
                is_record_component,
            })
        }
        Expression::Call {
            function,
            args,
            type_arguments,
            argument_metadata,
            call_kind,
            span,
        } => {
            if let Some(sequence_expr) = sequence_pipeline::try_lower_sequence_call(
                (*function).clone(),
                args.clone(),
                argument_metadata.clone(),
                span.clone(),
                context,
            )? {
                return Ok(sequence_expr);
            }

            lower_call_expression(
                *function,
                args,
                type_arguments,
                argument_metadata,
                call_kind,
                span,
                context,
            )
        }
        Expression::Lambda {
            parameters,
            body,
            span,
        } => {
            context.enter_scope();
            let mut param_names = Vec::with_capacity(parameters.len());
            let mut param_types = Vec::with_capacity(parameters.len());

            for param in &parameters {
                let java_type = JavaType::object();
                context.add_variable(param.name.clone(), java_type.clone());
                param_names.push(param.name.clone());
                param_types.push(java_type);
            }

            let result = transform_expression(*body, context);
            context.exit_scope();

            let body_ir = result?;

            Ok(IrExpression::Lambda {
                functional_interface: "java.util.function.Function".to_string(),
                param_names,
                param_types,
                body: Box::new(body_ir),
                java_type: JavaType::object(),
                span,
            })
        }
        Expression::When {
            expr: subject,
            arms,
            else_arm,
            implicit_end,
            span,
        } => desugar_when_expression(subject, arms, else_arm, implicit_end, span, context),
        _ => Ok(IrExpression::Literal(Literal::Null, Span::default())),
    }
}

fn is_string_like_java_type(java_type: &JavaType) -> bool {
    match java_type {
        JavaType::Reference { name, .. } => matches!(
            name.as_str(),
            "String"
                | "java.lang.String"
                | "CharSequence"
                | "java.lang.CharSequence"
                | "StringBuilder"
                | "java.lang.StringBuilder"
                | "StringBuffer"
                | "java.lang.StringBuffer"
        ),
        _ => false,
    }
}

const SYSTEM_OUT_METHODS: &[&str] = &[
    "append", "format", "print", "printf", "println", "write", "flush", "close",
];

const SYSTEM_IN_METHODS: &[&str] = &[
    "available",
    "close",
    "mark",
    "marksupported",
    "read",
    "readallbytes",
    "readnbytes",
    "reset",
    "skip",
    "transferto",
];

fn lower_doublebrace_expression(
    init: DoublebraceInit,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let span = init.span.clone();
    let base_expression = init.base.map(|expr| *expr);
    let plan = context
        .take_doublebrace_plan(&span)
        .or_else(|| context.doublebrace_plan(&span).cloned())
        .ok_or_else(|| TransformError::TypeInferenceError {
            message: "Doublebrace 初期化式のプラン情報が見つかりません".to_string(),
            span: span.clone(),
        })?;

    let receiver_type = java_type_from_fqcn(&plan.receiver_fqcn);

    let ir_plan = lower_doublebrace_plan(plan, context)?;
    let needs_synthesized = match &ir_plan {
        IrDoublebracePlan::Mutate(mutate) => {
            matches!(mutate.base, DoublebraceBaseStrategy::SynthesizedInstance)
        }
        IrDoublebracePlan::Copy(copy) => {
            matches!(
                copy.source,
                DoublebraceCopySourceStrategy::SynthesizedInstance
            )
        }
    };

    let mut base_ir = match base_expression {
        Some(base_expr) => {
            let lowered = if needs_synthesized {
                lower_doublebrace_synthesized_base(base_expr, &receiver_type, context)?
            } else {
                transform_expression(base_expr, context)?
            };
            Some(Box::new(lowered))
        }
        None => None,
    };

    if base_ir.is_none() && needs_synthesized {
        base_ir = Some(Box::new(synthesize_receiver_instance(
            &receiver_type,
            &span,
        )));
    }

    Ok(IrExpression::DoublebraceInit {
        base: base_ir,
        receiver_type: receiver_type.clone(),
        plan: ir_plan,
        java_type: receiver_type,
        span,
    })
}

fn lower_doublebrace_plan(
    plan: DoublebraceLoweringPlan,
    context: &mut TransformContext,
) -> Result<IrDoublebracePlan, TransformError> {
    match plan.kind {
        DoublebraceLoweringKind::Mutate(mutate) => {
            let mut lowered_steps = Vec::with_capacity(mutate.steps.len());
            for step in mutate.steps {
                match step {
                    DoublebraceLoweringStep::FieldAssignment(update) => {
                        let ir_update = lower_field_update(update, context)?;
                        lowered_steps.push(IrDoublebraceMutation::FieldAssignment(ir_update));
                    }
                    DoublebraceLoweringStep::MethodCall(call) => {
                        let ir_call = lower_method_invocation(call, context)?;
                        lowered_steps.push(IrDoublebraceMutation::MethodCall(ir_call));
                    }
                    DoublebraceLoweringStep::Other(statement) => {
                        let statements = transform_statement(statement, context)?;
                        lowered_steps.push(IrDoublebraceMutation::Statement(statements));
                    }
                }
            }
            Ok(IrDoublebracePlan::Mutate(IrDoublebraceMutatePlan {
                base: mutate.base,
                steps: lowered_steps,
            }))
        }
        DoublebraceLoweringKind::Copy(copy) => {
            let mut lowered_updates = Vec::with_capacity(copy.updates.len());
            for update in copy.updates {
                lowered_updates.push(lower_field_update(update, context)?);
            }
            Ok(IrDoublebracePlan::Copy(IrDoublebraceCopyPlan {
                source: copy.source,
                updates: lowered_updates,
            }))
        }
    }
}

fn lower_field_update(
    update: DoublebraceFieldUpdate,
    context: &mut TransformContext,
) -> Result<IrDoublebraceFieldUpdate, TransformError> {
    let value = transform_expression(update.value, context)?;
    Ok(IrDoublebraceFieldUpdate {
        name: update.name,
        value,
        span: update.span,
    })
}

fn lower_method_invocation(
    invocation: DoublebraceMethodInvocation,
    context: &mut TransformContext,
) -> Result<IrDoublebraceMethodInvocation, TransformError> {
    let arguments = lower_call_arguments(invocation.arguments, context)?;
    Ok(IrDoublebraceMethodInvocation {
        name: invocation.name,
        arguments,
        argument_style: invocation.metadata.style,
        metadata: invocation.metadata,
        span: invocation.span,
    })
}

fn java_type_from_fqcn(fqcn: &str) -> JavaType {
    parse_java_type(fqcn.trim())
}

fn synthesize_receiver_instance(receiver_type: &JavaType, span: &Span) -> IrExpression {
    match receiver_type {
        JavaType::Reference { name, generic_args } => {
            let instantiation = preferred_doublebrace_instantiation(name).unwrap_or(name);
            IrExpression::ObjectCreation {
                class_name: instantiation.to_string(),
                generic_args: generic_args.clone(),
                args: Vec::new(),
                java_type: receiver_type.clone(),
                span: span.clone(),
            }
        }
        _ => IrExpression::Literal(Literal::Null, span.clone()),
    }
}

fn lower_doublebrace_synthesized_base(
    base_expr: Expression,
    receiver_type: &JavaType,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    match base_expr {
        Expression::Call {
            function,
            args,
            type_arguments,
            argument_metadata,
            call_kind,
            span,
        } => {
            if let CallKind::Constructor { .. } = &call_kind {
                if let JavaType::Reference { name, generic_args } = receiver_type {
                    let instantiation =
                        preferred_doublebrace_instantiation(name).unwrap_or(name.as_str());
                    let ir_args = lower_call_arguments(args, context)?;
                    return Ok(IrExpression::ObjectCreation {
                        class_name: instantiation.to_string(),
                        generic_args: generic_args.clone(),
                        args: ir_args,
                        java_type: receiver_type.clone(),
                        span,
                    });
                }
            }

            lower_call_expression(
                *function,
                args,
                type_arguments,
                argument_metadata,
                call_kind,
                span,
                context,
            )
        }
        other => transform_expression(other, context),
    }
}

fn preferred_doublebrace_instantiation(name: &str) -> Option<&'static str> {
    let simple = name.rsplit('.').next().unwrap_or(name);
    match simple {
        "Iterable" | "Collection" | "List" => Some("java.util.ArrayList"),
        "Set" => Some("java.util.LinkedHashSet"),
        "SortedSet" | "NavigableSet" => Some("java.util.TreeSet"),
        "Queue" | "Deque" => Some("java.util.ArrayDeque"),
        "Map" => Some("java.util.LinkedHashMap"),
        "SortedMap" | "NavigableMap" => Some("java.util.TreeMap"),
        _ => None,
    }
}

fn parse_java_type(spec: &str) -> JavaType {
    let mut base = spec.trim().to_string();
    if base.is_empty() {
        return JavaType::object();
    }

    let mut dimensions = 0;
    while base.ends_with("[]") {
        dimensions += 1;
        base.truncate(base.len().saturating_sub(2));
        base = base.trim_end().to_string();
    }

    let core = if dimensions > 0 { base.trim() } else { &base };
    let mut ty = parse_non_array_type(core);

    if dimensions > 0 {
        ty = JavaType::Array {
            element_type: Box::new(ty),
            dimensions,
        };
    }

    ty
}

fn parse_non_array_type(spec: &str) -> JavaType {
    let trimmed = spec.trim();
    if trimmed.is_empty() {
        return JavaType::object();
    }

    if trimmed == "void" {
        return JavaType::Void;
    }

    if trimmed.starts_with('?') {
        return parse_wildcard_type(trimmed);
    }

    if let Some(start) = trimmed.find('<') {
        let base = trimmed[..start].trim();
        let args_section = trimmed[start + 1..]
            .rsplit_once('>')
            .map(|(inner, _)| inner)
            .unwrap_or_default();
        let args = split_generic_arguments(args_section);
        let parsed_args = args.into_iter().map(parse_java_type).collect::<Vec<_>>();
        return JavaType::Reference {
            name: base.to_string(),
            generic_args: parsed_args,
        };
    }

    if is_primitive_name(trimmed) {
        return JavaType::Primitive(trimmed.to_string());
    }

    JavaType::Reference {
        name: trimmed.to_string(),
        generic_args: Vec::new(),
    }
}

fn parse_wildcard_type(spec: &str) -> JavaType {
    let remainder = spec.trim_start_matches('?').trim();
    if remainder.is_empty() {
        return JavaType::Wildcard {
            kind: JavaWildcardKind::Unbounded,
            bound: None,
        };
    }

    if let Some(rest) = remainder.strip_prefix("extends") {
        let bound = parse_java_type(rest);
        return JavaType::Wildcard {
            kind: JavaWildcardKind::Extends,
            bound: Some(Box::new(bound)),
        };
    }

    if let Some(rest) = remainder.strip_prefix("super") {
        let bound = parse_java_type(rest);
        return JavaType::Wildcard {
            kind: JavaWildcardKind::Super,
            bound: Some(Box::new(bound)),
        };
    }

    JavaType::Wildcard {
        kind: JavaWildcardKind::Unbounded,
        bound: None,
    }
}

fn split_generic_arguments(spec: &str) -> Vec<&str> {
    let mut args = Vec::new();
    let mut depth = 0;
    let mut last = 0;

    for (idx, ch) in spec.char_indices() {
        match ch {
            '<' => depth += 1,
            '>' => {
                if depth > 0 {
                    depth -= 1;
                }
            }
            ',' if depth == 0 => {
                let part = spec[last..idx].trim();
                if !part.is_empty() {
                    args.push(part);
                }
                last = idx + ch.len_utf8();
            }
            _ => {}
        }
    }

    if last < spec.len() {
        let part = spec[last..].trim();
        if !part.is_empty() {
            args.push(part);
        }
    }

    args
}

fn is_primitive_name(name: &str) -> bool {
    matches!(
        name,
        "boolean" | "byte" | "short" | "int" | "long" | "float" | "double" | "char"
    )
}

fn lower_call_expression(
    function: Expression,
    args: Vec<Argument>,
    type_arguments: Vec<TypeAnnotation>,
    argument_metadata: CallArgumentMetadata,
    call_kind: CallKind,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let _ = &call_kind;
    let argument_style = argument_metadata.style;
    let mut ir_args = lower_call_arguments(args, context)?;

    if let CallKind::Constructor { type_name, fqcn } = &call_kind {
        let resolved_fqcn = fqcn.clone().or_else(|| {
            context.lookup_variable(type_name).and_then(|ty| match ty {
                JavaType::Reference { name, .. } => Some(name.clone()),
                _ => None,
            })
        });
        let fqcn = resolved_fqcn.unwrap_or_else(|| type_name.clone());
        let class_name = fqcn
            .rsplit('.')
            .next()
            .unwrap_or(type_name.as_str())
            .to_string();
        let generic_args = type_arguments
            .iter()
            .map(|annotation| {
                let java = type_system::convert_type_annotation(annotation.clone())?;
                Ok(boxed_java_type(&java))
            })
            .collect::<Result<Vec<_>, TransformError>>()?;
        let java_type = JavaType::Reference {
            name: fqcn,
            generic_args: generic_args.clone(),
        };

        return Ok(IrExpression::ObjectCreation {
            class_name,
            generic_args,
            args: ir_args,
            java_type,
            span,
        });
    }

    match function {
        Expression::Identifier(name, fn_span) => {
            if let Some(receiver_kind) = resolve_implicit_system_method(&name) {
                let receiver_expr = create_system_field_access(receiver_kind, fn_span);
                let return_type = system_method_return_type(receiver_kind, &name);
                let mut call = IrExpression::MethodCall {
                    receiver: Some(Box::new(receiver_expr)),
                    method_name: name,
                    java_name: None,
                    resolved_target: None,
                    args: ir_args,
                    argument_style,
                    java_type: return_type,
                    span,
                };
                register_call_metadata(context, &mut call, None);
                return Ok(call);
            }

            if let Some(param_types) = context.function_signature(&name) {
                if param_types.len() == ir_args.len() {
                    let param_types_vec: Vec<JavaType> = param_types.to_vec();
                    ir_args =
                        adjust_call_arguments_for_signature(ir_args, &param_types_vec, context);
                }
            }

            let resolved_type = context.lookup_variable(&name).cloned();

            if resolved_type.is_none() && is_constructor_like(&name) {
                let class_name = name.clone();
                let fqcn = if let Some(package) = context.current_package.as_deref() {
                    format!("{package}.{class_name}")
                } else {
                    class_name.clone()
                };
                let generic_args = type_arguments
                    .into_iter()
                    .map(|annotation| {
                        let java = type_system::convert_type_annotation(annotation)?;
                        Ok(boxed_java_type(&java))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                return Ok(IrExpression::ObjectCreation {
                    class_name,
                    generic_args: generic_args.clone(),
                    args: ir_args,
                    java_type: JavaType::Reference {
                        name: fqcn,
                        generic_args,
                    },
                    span,
                });
            }

            let java_type = resolved_type.unwrap_or_else(JavaType::object);

            if let Some((method_name, return_type)) = functional_interface_method(&java_type) {
                let receiver = IrExpression::Identifier {
                    name: name.clone(),
                    java_type: java_type.clone(),
                    span: span.clone(),
                };
                let mut call = IrExpression::MethodCall {
                    receiver: Some(Box::new(receiver)),
                    method_name,
                    java_name: None,
                    resolved_target: None,
                    args: ir_args,
                    argument_style,
                    java_type: return_type,
                    span,
                };
                register_call_metadata(context, &mut call, None);
                return Ok(call);
            }

            let mut call = IrExpression::MethodCall {
                receiver: None,
                method_name: name,
                java_name: None,
                resolved_target: None,
                args: ir_args,
                argument_style,
                java_type,
                span,
            };
            register_call_metadata(context, &mut call, None);
            Ok(call)
        }
        Expression::MemberAccess {
            object,
            property,
            span: _fn_span,
        } => {
            if let Some((receiver_expr, receiver_kind)) =
                lower_system_receiver_expression((*object).clone())
            {
                let return_type = system_method_return_type(receiver_kind, &property);
                let mut call = IrExpression::MethodCall {
                    receiver: Some(Box::new(receiver_expr)),
                    method_name: property,
                    java_name: None,
                    resolved_target: None,
                    args: ir_args,
                    argument_style,
                    java_type: return_type,
                    span,
                };
                register_call_metadata(context, &mut call, None);
                return Ok(call);
            }

            let receiver_expr = transform_expression(*object, context)?;

            if let Some(mut call) = promote_extension_call_if_applicable(
                &receiver_expr,
                &property,
                &ir_args,
                argument_style,
                &span,
                context,
            ) {
                register_call_metadata(context, &mut call, None);
                return Ok(call);
            }

            let mut call = IrExpression::MethodCall {
                receiver: Some(Box::new(receiver_expr)),
                method_name: property,
                java_name: None,
                resolved_target: None,
                args: ir_args,
                argument_style,
                java_type: JavaType::object(),
                span,
            };
            register_call_metadata(context, &mut call, None);
            Ok(call)
        }
        other => {
            let function_expr = transform_expression(other, context)?;
            match function_expr {
                IrExpression::FieldAccess {
                    receiver,
                    field_name,
                    ..
                } => {
                    let mut call = IrExpression::MethodCall {
                        receiver: Some(receiver),
                        method_name: field_name,
                        java_name: None,
                        resolved_target: None,
                        args: ir_args,
                        argument_style,
                        java_type: JavaType::object(),
                        span,
                    };
                    register_call_metadata(context, &mut call, None);
                    Ok(call)
                }
                IrExpression::MethodCall {
                    receiver,
                    method_name,
                    java_name,
                    resolved_target,
                    args: existing_args,
                    argument_style: existing_style,
                    java_type,
                    span: inner_span,
                } if ir_args.is_empty() && existing_args.is_empty() => {
                    let mut call = IrExpression::MethodCall {
                        receiver,
                        method_name,
                        java_name,
                        resolved_target,
                        args: existing_args,
                        argument_style: existing_style,
                        java_type,
                        span: inner_span,
                    };
                    register_call_metadata(context, &mut call, None);
                    Ok(call)
                }
                _ => {
                    let mut call = IrExpression::MethodCall {
                        receiver: Some(Box::new(function_expr)),
                        method_name: "call".to_string(),
                        java_name: None,
                        resolved_target: None,
                        args: ir_args,
                        argument_style,
                        java_type: JavaType::object(),
                        span,
                    };
                    register_call_metadata(context, &mut call, None);
                    Ok(call)
                }
            }
        }
    }
}

fn lower_call_arguments(
    args: Vec<Argument>,
    context: &mut TransformContext,
) -> Result<Vec<IrExpression>, TransformError> {
    let mut lowered = Vec::with_capacity(args.len());
    for arg in args {
        let expr = match arg {
            Argument::Positional(expr) => expr,
            Argument::Named { value, .. } => value,
        };
        lowered.push(transform_expression(expr, context)?);
    }
    Ok(lowered)
}

fn adjust_call_arguments_for_signature(
    args: Vec<IrExpression>,
    param_types: &[JavaType],
    context: &mut TransformContext,
) -> Vec<IrExpression> {
    if args.len() != param_types.len() {
        return args;
    }

    args.into_iter()
        .zip(param_types.iter())
        .map(|(arg, param)| coerce_argument_for_parameter(arg, param, context))
        .collect()
}

fn coerce_argument_for_parameter(
    arg: IrExpression,
    param_type: &JavaType,
    context: &mut TransformContext,
) -> IrExpression {
    if !is_stream_reference_type(param_type) {
        return arg;
    }

    match extract_java_type(&arg) {
        Some(JavaType::Reference { name, .. }) if is_iterable_like(&name) => {
            let mut call = make_sequence_helper_call("toStream", arg, param_type.clone());
            register_call_metadata(
                context,
                &mut call,
                Some("jv.collections.Sequence".to_string()),
            );
            call
        }
        Some(JavaType::Reference { name, .. }) if is_stream_type(&name) => arg,
        _ => arg,
    }
}

fn make_sequence_helper_call(
    method: &str,
    argument: IrExpression,
    result_type: JavaType,
) -> IrExpression {
    let span = ir_expression_span(&argument);
    let factory_identifier = IrExpression::Identifier {
        name: "Sequence".to_string(),
        java_type: JavaType::Reference {
            name: "jv.collections.Sequence".to_string(),
            generic_args: vec![],
        },
        span: span.clone(),
    };

    IrExpression::MethodCall {
        receiver: Some(Box::new(factory_identifier)),
        method_name: method.to_string(),
        java_name: None,
        resolved_target: None,
        args: vec![argument],
        argument_style: CallArgumentStyle::Whitespace,
        java_type: result_type,
        span,
    }
}

fn promote_extension_call_if_applicable(
    receiver_expr: &IrExpression,
    method_name: &str,
    args: &[IrExpression],
    argument_style: CallArgumentStyle,
    span: &Span,
    context: &TransformContext,
) -> Option<IrExpression> {
    let receiver_type = extract_java_type(receiver_expr)?;

    if context.is_current_extension_scope(method_name, &receiver_type) {
        return None;
    }

    let metadata = context.lookup_extension_method(method_name, &receiver_type, args.len())?;

    let mut promoted_args = Vec::with_capacity(args.len() + 1);
    promoted_args.push(receiver_expr.clone());
    promoted_args.extend(args.iter().cloned());

    Some(IrExpression::MethodCall {
        receiver: None,
        method_name: method_name.to_string(),
        java_name: None,
        resolved_target: None,
        args: promoted_args,
        argument_style,
        java_type: metadata.return_type.clone(),
        span: span.clone(),
    })
}

fn register_call_metadata(
    context: &mut TransformContext,
    call: &mut IrExpression,
    owner: Option<String>,
) {
    if let IrExpression::MethodCall { receiver, args, .. } = call {
        let receiver_type = receiver.as_ref().and_then(|expr| extract_java_type(expr));
        let argument_types = args
            .iter()
            .map(|arg| extract_java_type(arg).unwrap_or_else(JavaType::object))
            .collect();
        context.bind_method_call(call, owner, receiver_type, argument_types);
    }
}

fn is_stream_reference_type(java_type: &JavaType) -> bool {
    match java_type {
        JavaType::Reference { name, .. } => is_stream_type(name),
        _ => false,
    }
}

fn is_iterable_like(name: &str) -> bool {
    matches!(
        name,
        "java.util.List" | "java.lang.Iterable" | "java.util.Collection"
    )
}

fn is_stream_type(name: &str) -> bool {
    matches!(name, "java.util.stream.Stream" | "Stream")
}

fn binary_numeric_result_type(left: Option<&JavaType>, right: Option<&JavaType>) -> JavaType {
    const RANK_INT: u8 = 1;
    const RANK_LONG: u8 = 2;
    const RANK_FLOAT: u8 = 3;
    const RANK_DOUBLE: u8 = 4;

    let left_rank = left.and_then(numeric_rank);
    let right_rank = right.and_then(numeric_rank);

    let rank = match (left_rank, right_rank) {
        (Some(l), Some(r)) => l.max(r),
        (Some(l), None) | (None, Some(l)) => l,
        (None, None) => RANK_INT,
    };

    match rank {
        RANK_DOUBLE => JavaType::Primitive("double".to_string()),
        RANK_FLOAT => JavaType::Primitive("float".to_string()),
        RANK_LONG => JavaType::Primitive("long".to_string()),
        _ => JavaType::Primitive("int".to_string()),
    }
}

fn integral_binary_result_type(left: Option<&JavaType>, right: Option<&JavaType>) -> JavaType {
    const RANK_INT: u8 = 1;
    const RANK_LONG: u8 = 2;

    let left_rank = left.and_then(integral_rank);
    let right_rank = right.and_then(integral_rank);

    let rank = match (left_rank, right_rank) {
        (Some(l), Some(r)) => l.max(r),
        (Some(l), None) | (None, Some(l)) => l,
        (None, None) => RANK_INT,
    };

    match rank {
        RANK_LONG => JavaType::Primitive("long".to_string()),
        _ => JavaType::Primitive("int".to_string()),
    }
}

fn numeric_rank(java_type: &JavaType) -> Option<u8> {
    match java_type {
        JavaType::Primitive(name) => primitive_numeric_rank(name),
        JavaType::Reference { name, .. } => wrapper_numeric_rank(name),
        _ => None,
    }
}

fn integral_rank(java_type: &JavaType) -> Option<u8> {
    match java_type {
        JavaType::Primitive(name) => primitive_integral_rank(name),
        JavaType::Reference { name, .. } => wrapper_integral_rank(name),
        _ => None,
    }
}

fn primitive_numeric_rank(name: &str) -> Option<u8> {
    match name {
        "double" => Some(4),
        "float" => Some(3),
        "long" => Some(2),
        "int" | "short" | "byte" | "char" => Some(1),
        _ => None,
    }
}

fn primitive_integral_rank(name: &str) -> Option<u8> {
    match name {
        "long" => Some(2),
        "int" | "short" | "byte" | "char" => Some(1),
        _ => None,
    }
}

fn wrapper_numeric_rank(name: &str) -> Option<u8> {
    match name {
        "Double" | "java.lang.Double" => Some(4),
        "Float" | "java.lang.Float" => Some(3),
        "Long" | "java.lang.Long" => Some(2),
        "Integer"
        | "java.lang.Integer"
        | "Short"
        | "java.lang.Short"
        | "Byte"
        | "java.lang.Byte"
        | "Character"
        | "java.lang.Character" => Some(1),
        _ => None,
    }
}

fn wrapper_integral_rank(name: &str) -> Option<u8> {
    match name {
        "Long" | "java.lang.Long" => Some(2),
        "Integer"
        | "java.lang.Integer"
        | "Short"
        | "java.lang.Short"
        | "Byte"
        | "java.lang.Byte"
        | "Character"
        | "java.lang.Character" => Some(1),
        _ => None,
    }
}

fn flatten_member_access_chain(expr: &Expression) -> Option<Vec<String>> {
    match expr {
        Expression::Identifier(name, _) => Some(vec![name.clone()]),
        Expression::MemberAccess {
            object, property, ..
        } => {
            let mut segments = flatten_member_access_chain(object)?;
            segments.push(property.clone());
            Some(segments)
        }
        _ => None,
    }
}

fn is_constructor_like(name: &str) -> bool {
    name.chars()
        .next()
        .map(|ch| ch.is_ascii_uppercase())
        .unwrap_or(false)
}

fn functional_interface_method(java_type: &JavaType) -> Option<(String, JavaType)> {
    match java_type {
        JavaType::Reference { name, generic_args } => match name.as_str() {
            "java.util.function.Function" => {
                let return_type = generic_args
                    .get(1)
                    .cloned()
                    .unwrap_or_else(JavaType::object);
                Some(("apply".to_string(), return_type))
            }
            "java.util.function.Predicate" => Some((
                "test".to_string(),
                JavaType::Primitive("boolean".to_string()),
            )),
            "java.util.function.Consumer" => Some(("accept".to_string(), JavaType::Void)),
            "java.util.function.Supplier" => {
                let return_type = generic_args
                    .get(0)
                    .cloned()
                    .unwrap_or_else(JavaType::object);
                Some(("get".to_string(), return_type))
            }
            "java.util.function.BiFunction" => {
                let return_type = generic_args
                    .get(2)
                    .cloned()
                    .unwrap_or_else(JavaType::object);
                Some(("apply".to_string(), return_type))
            }
            "java.util.function.BiConsumer" => Some(("accept".to_string(), JavaType::Void)),
            _ => None,
        },
        _ => None,
    }
}

fn lower_system_receiver_expression(expr: Expression) -> Option<(IrExpression, SystemReceiver)> {
    match expr {
        Expression::MemberAccess {
            object,
            property,
            span,
        } => lower_system_field(*object, property, span),
        _ => None,
    }
}

fn lower_system_field(
    object: Expression,
    property: String,
    span: Span,
) -> Option<(IrExpression, SystemReceiver)> {
    match object {
        Expression::Identifier(name, system_span) if name.eq_ignore_ascii_case("system") => {
            let property_lower = property.to_ascii_lowercase();
            let receiver_kind = match property_lower.as_str() {
                "out" => SystemReceiver::Out,
                "in" => SystemReceiver::In,
                "err" => SystemReceiver::Err,
                _ => return None,
            };

            let field_expr = IrExpression::FieldAccess {
                receiver: Box::new(system_identifier(system_span)),
                field_name: receiver_kind.field_name().to_string(),
                java_type: receiver_kind.java_type(),
                span,
                is_record_component: false,
            };

            Some((field_expr, receiver_kind))
        }
        _ => None,
    }
}

fn resolve_implicit_system_method(name: &str) -> Option<SystemReceiver> {
    let lower = name.to_ascii_lowercase();
    if SYSTEM_OUT_METHODS.contains(&lower.as_str()) {
        Some(SystemReceiver::Out)
    } else if SYSTEM_IN_METHODS.contains(&lower.as_str()) {
        Some(SystemReceiver::In)
    } else {
        None
    }
}

fn system_method_return_type(receiver: SystemReceiver, method_name: &str) -> JavaType {
    match receiver {
        SystemReceiver::Out | SystemReceiver::Err => system_out_method_return_type(method_name),
        SystemReceiver::In => system_in_method_return_type(method_name),
    }
}

fn system_out_method_return_type(method_name: &str) -> JavaType {
    let lower = method_name.to_ascii_lowercase();
    match lower.as_str() {
        "append" | "format" | "printf" => JavaType::Reference {
            name: "java.io.PrintStream".to_string(),
            generic_args: vec![],
        },
        _ => JavaType::void(),
    }
}

fn system_in_method_return_type(method_name: &str) -> JavaType {
    let lower = method_name.to_ascii_lowercase();
    match lower.as_str() {
        "read" | "available" => JavaType::Primitive("int".to_string()),
        "skip" | "transferto" => JavaType::Primitive("long".to_string()),
        "marksupported" => JavaType::Primitive("boolean".to_string()),
        "readallbytes" | "readnbytes" => JavaType::Array {
            element_type: Box::new(JavaType::Primitive("byte".to_string())),
            dimensions: 1,
        },
        "close" | "mark" | "reset" => JavaType::void(),
        _ => JavaType::object(),
    }
}

fn system_identifier(span: Span) -> IrExpression {
    IrExpression::Identifier {
        name: "System".to_string(),
        java_type: JavaType::Reference {
            name: "java.lang.System".to_string(),
            generic_args: vec![],
        },
        span,
    }
}

pub(crate) fn normalize_whitespace_array_elements(elements: Vec<Expression>) -> Vec<Expression> {
    let mut normalized = Vec::with_capacity(elements.len());

    for expr in elements.into_iter() {
        match expr {
            Expression::Unary { op, operand, span }
                if matches!(op, UnaryOp::Plus | UnaryOp::Minus) =>
            {
                if let Some(prev) = normalized.pop() {
                    if matches!(
                        &prev,
                        Expression::Unary {
                            op: UnaryOp::Plus,
                            ..
                        } | Expression::Unary {
                            op: UnaryOp::Minus,
                            ..
                        }
                    ) {
                        normalized.push(prev);
                        normalized.push(Expression::Unary { op, operand, span });
                    } else {
                        let prev_span = prev.span().clone();
                        let merged_span = prev_span.merge(&span);
                        let binary_op = match op {
                            UnaryOp::Plus => BinaryOp::Add,
                            UnaryOp::Minus => BinaryOp::Subtract,
                            _ => unreachable!(),
                        };

                        normalized.push(Expression::Binary {
                            left: Box::new(prev),
                            op: binary_op,
                            right: operand,
                            span: merged_span,
                        });
                    }
                } else {
                    normalized.push(Expression::Unary { op, operand, span });
                }
            }
            other => normalized.push(other),
        }
    }

    normalized
}

fn create_system_field_access(receiver: SystemReceiver, span: Span) -> IrExpression {
    IrExpression::FieldAccess {
        receiver: Box::new(system_identifier(span.clone())),
        field_name: receiver.field_name().to_string(),
        java_type: receiver.java_type(),
        span,
        is_record_component: false,
    }
}

#[derive(Clone, Copy)]
enum SystemReceiver {
    Out,
    In,
    Err,
}

impl SystemReceiver {
    fn field_name(self) -> &'static str {
        match self {
            SystemReceiver::Out => "out",
            SystemReceiver::In => "in",
            SystemReceiver::Err => "err",
        }
    }

    fn java_type(self) -> JavaType {
        match self {
            SystemReceiver::Out | SystemReceiver::Err => JavaType::Reference {
                name: "java.io.PrintStream".to_string(),
                generic_args: vec![],
            },
            SystemReceiver::In => JavaType::Reference {
                name: "java.io.InputStream".to_string(),
                generic_args: vec![],
            },
        }
    }
}
