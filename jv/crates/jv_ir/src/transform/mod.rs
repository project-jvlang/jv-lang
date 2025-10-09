use self::utils::extract_java_type;
use crate::context::TransformContext;
use crate::error::TransformError;
use crate::profiling::{PerfMetrics, TransformProfiler};
use crate::sequence_pipeline;
use crate::types::{IrCommentKind, IrExpression, IrProgram, IrStatement, JavaType};
use jv_ast::{
    Argument, BinaryOp, CallArgumentMetadata, CommentKind, ConcurrencyConstruct, Expression,
    Literal, Program, ResourceManagement, Span, Statement,
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
        Statement::FunctionDeclaration {
            name,
            parameters,
            return_type,
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
                body,
                modifiers,
                span,
                type_parameters,
                where_clause,
                generic_signature,
            },
            context,
        )?]),
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
            span,
        } => {
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

fn lower_program(
    program: Program,
    context: &mut TransformContext,
) -> Result<IrProgram, TransformError> {
    let Program {
        package,
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

    let type_declarations = attach_trailing_comments(ir_statements);

    Ok(IrProgram {
        package,
        imports: Vec::new(),
        type_declarations,
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
        | IrStatement::Import { span, .. }
        | IrStatement::Package { span, .. }
        | IrStatement::Comment { span, .. } => Some(span.clone()),
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
            let statements = attach_trailing_comments(ir_statements);
            Ok(IrExpression::Block {
                statements,
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
        Expression::MemberAccess {
            object,
            property,
            span,
        } => {
            if let Some((field_expr, _)) = lower_system_field(*object, property, span.clone()) {
                Ok(field_expr)
            } else {
                Ok(IrExpression::Literal(Literal::Null, Span::default()))
            }
        }
        Expression::Call {
            function,
            args,
            argument_metadata,
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

            lower_call_expression(*function, args, argument_metadata, span, context)
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

fn lower_call_expression(
    function: Expression,
    args: Vec<Argument>,
    argument_metadata: CallArgumentMetadata,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let argument_style = argument_metadata.style;
    let ir_args = lower_call_arguments(args, context)?;

    match function {
        Expression::Identifier(name, fn_span) => {
            if let Some(receiver_kind) = resolve_implicit_system_method(&name) {
                let receiver_expr = create_system_field_access(receiver_kind, fn_span);
                let return_type = system_method_return_type(receiver_kind, &name);
                return Ok(IrExpression::MethodCall {
                    receiver: Some(Box::new(receiver_expr)),
                    method_name: name,
                    args: ir_args,
                    argument_style,
                    java_type: return_type,
                    span,
                });
            }

            let java_type = context
                .lookup_variable(&name)
                .cloned()
                .unwrap_or_else(JavaType::object);

            Ok(IrExpression::MethodCall {
                receiver: None,
                method_name: name,
                args: ir_args,
                argument_style,
                java_type,
                span,
            })
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
                return Ok(IrExpression::MethodCall {
                    receiver: Some(Box::new(receiver_expr)),
                    method_name: property,
                    args: ir_args,
                    argument_style,
                    java_type: return_type,
                    span,
                });
            }

            let receiver_expr = transform_expression(*object, context)?;

            Ok(IrExpression::MethodCall {
                receiver: Some(Box::new(receiver_expr)),
                method_name: property,
                args: ir_args,
                argument_style,
                java_type: JavaType::object(),
                span,
            })
        }
        other => {
            let function_expr = transform_expression(other, context)?;
            Ok(IrExpression::MethodCall {
                receiver: Some(Box::new(function_expr)),
                method_name: "call".to_string(),
                args: ir_args,
                argument_style,
                java_type: JavaType::object(),
                span,
            })
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

fn create_system_field_access(receiver: SystemReceiver, span: Span) -> IrExpression {
    IrExpression::FieldAccess {
        receiver: Box::new(system_identifier(span.clone())),
        field_name: receiver.field_name().to_string(),
        java_type: receiver.java_type(),
        span,
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
