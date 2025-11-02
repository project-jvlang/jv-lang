use super::*;
use jv_ast::CallArgumentStyle;
use jv_ir::{
    LogGuardKind, LogInvocationItem, LogInvocationPlan, LogLevel, LogMessage, LoggerFieldId,
    LoggerFieldSpec, LoggingFrameworkKind, LoggingMetadata,
};

const LOGGER_ID: LoggerFieldId = LoggerFieldId(1);

#[test]
fn slf4j_debug_emits_guard_and_brace_placeholders() {
    let plan = log_plan(
        LogLevel::Debug,
        Some(LogGuardKind::DebugEnabled),
        vec![statement_value_length("value")],
        string_format_message("value %s", vec![identifier_expr("value")]),
    );

    let source = render_logging_program(
        LoggingFrameworkKind::Slf4j,
        plan,
        "org.slf4j.Logger",
        &["value"],
        false,
    );

    assert!(
        source.contains("if (LOGGER.isDebugEnabled())"),
        "expected debug guard in source:\n{source}"
    );
    assert!(
        source.contains("value.length();"),
        "expected preparatory statement inside guard:\n{source}"
    );
    assert!(
        source.contains("LOGGER.debug(\"value {}\", value);"),
        "expected SLF4J placeholder with braces:\n{source}"
    );
}

#[test]
fn slf4j_trace_context_injects_mdc_when_enabled() {
    let plan = log_plan(
        LogLevel::Info,
        None,
        vec![statement_value_length("value")],
        string_format_message("value %s", vec![identifier_expr("value")]),
    );

    let source = render_logging_program(
        LoggingFrameworkKind::Slf4j,
        plan,
        "org.slf4j.Logger",
        &["value"],
        true,
    );

    assert!(
        source.contains("Span.current()"),
        "expected current span acquisition:\n{source}"
    );
    assert!(
        source.contains("MDC.put(\"traceId\""),
        "expected traceId injection into MDC:\n{source}"
    );
    assert!(
        source.contains("MDC.remove(\"spanId\");"),
        "expected MDC cleanup for spanId:\n{source}"
    );
    assert!(
        source.contains("} finally {"),
        "expected try/finally for MDC cleanup:\n{source}"
    );
}

#[test]
fn jboss_logging_uses_percent_format_specifiers() {
    let plan = log_plan(
        LogLevel::Error,
        Some(LogGuardKind::ErrorEnabled),
        Vec::new(),
        string_format_message(
            "%s -> %s",
            vec![identifier_expr("from"), identifier_expr("to")],
        ),
    );

    let source = render_logging_program(
        LoggingFrameworkKind::JbossLogging,
        plan,
        "org.jboss.logging.Logger",
        &["from", "to"],
        false,
    );

    assert!(
        source.contains("LOGGER.errorf(\"%s -> %s\", from, to);"),
        "expected JBoss Logging formatted call:\n{source}"
    );
}

#[test]
fn commons_logging_concatenates_strings() {
    let plan = log_plan(
        LogLevel::Info,
        None,
        Vec::new(),
        string_format_message("Hello %s!", vec![identifier_expr("name")]),
    );

    let source = render_logging_program(
        LoggingFrameworkKind::CommonsLogging,
        plan,
        "org.apache.commons.logging.Log",
        &["name"],
        false,
    );

    assert!(
        source.contains("LOGGER.info(\"Hello \" + name + \"!\");"),
        "expected Commons Logging concatenation path:\n{source}"
    );
}

#[test]
fn jul_logging_uses_numbered_placeholders() {
    let plan = log_plan(
        LogLevel::Debug,
        Some(LogGuardKind::DebugEnabled),
        Vec::new(),
        string_format_message(
            "[%s] %s",
            vec![identifier_expr("traceId"), identifier_expr("message")],
        ),
    );

    let source = render_logging_program(
        LoggingFrameworkKind::Jul,
        plan,
        "java.util.logging.Logger",
        &["traceId", "message"],
        false,
    );

    assert!(
        source.contains("if (LOGGER.isLoggable(Level.FINE))"),
        "expected JUL guard using isLoggable(Level.FINE):\n{source}"
    );
    assert!(
        source.contains("LOGGER.log(Level.FINE, \"[{0}] {1}\", traceId, message);"),
        "expected JUL numbered placeholder call:\n{source}"
    );
}

fn log_plan(
    level: LogLevel,
    guard: Option<LogGuardKind>,
    mut statements: Vec<LogInvocationItem>,
    message: LogMessage,
) -> LogInvocationPlan {
    let mut items = Vec::new();
    items.append(&mut statements);
    items.push(LogInvocationItem::Message(message));

    LogInvocationPlan {
        class_id: None,
        logger_field: LOGGER_ID,
        level,
        uses_default_level: false,
        guard_kind: guard,
        items,
        span: dummy_span(),
    }
}

fn render_logging_program(
    framework: LoggingFrameworkKind,
    plan: LogInvocationPlan,
    field_type_name: &str,
    param_names: &[&str],
    trace_context: bool,
) -> String {
    let logger_field = IrStatement::FieldDeclaration {
        name: "LOGGER".to_string(),
        java_type: reference_type(field_type_name),
        initializer: None,
        modifiers: IrModifiers {
            visibility: IrVisibility::Private,
            is_static: true,
            is_final: true,
            ..IrModifiers::default()
        },
        span: dummy_span(),
    };

    let method = log_method(plan, param_names);

    let class = IrStatement::ClassDeclaration {
        name: "LoggingSample".to_string(),
        type_parameters: vec![],
        superclass: None,
        interfaces: vec![],
        fields: vec![logger_field],
        methods: vec![method],
        nested_classes: vec![],
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        span: dummy_span(),
    };

    let metadata = LoggingMetadata {
        logger_fields: vec![LoggerFieldSpec {
            id: LOGGER_ID,
            owner_hint: Some("example.LoggingSample".to_string()),
            field_name: "LOGGER".to_string(),
            class_id: None,
        }],
        framework,
        trace_context,
    };

    let program = IrProgram {
        package: Some("example".to_string()),
        imports: vec![],
        type_declarations: vec![class],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        logging: metadata,
        span: dummy_span(),
    };

    let mut generator = JavaCodeGenerator::new();
    let unit = generator
        .generate_compilation_unit(&program)
        .expect("logging code generation should succeed");
    unit.to_source(&JavaCodeGenConfig::default())
}

fn log_method(plan: LogInvocationPlan, param_names: &[&str]) -> IrStatement {
    let log_expression = IrExpression::LogInvocation {
        plan: Box::new(plan),
        java_type: JavaType::void(),
        span: dummy_span(),
    };

    let body = IrExpression::Block {
        statements: vec![IrStatement::Expression {
            expr: log_expression,
            span: dummy_span(),
        }],
        java_type: JavaType::void(),
        span: dummy_span(),
    };

    let parameters: Vec<IrParameter> = param_names
        .iter()
        .map(|name| IrParameter {
            name: name.to_string(),
            java_type: string_type(),
            modifiers: IrModifiers::default(),
            span: dummy_span(),
        })
        .collect();

    IrStatement::MethodDeclaration {
        name: "log".to_string(),
        java_name: None,
        type_parameters: vec![],
        parameters,
        primitive_return: None,
        return_type: JavaType::void(),
        body: Some(body),
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        throws: vec![],
        span: dummy_span(),
    }
}

fn statement_value_length(param_name: &str) -> LogInvocationItem {
    let call = IrExpression::MethodCall {
        receiver: Some(Box::new(identifier_expr(param_name))),
        method_name: "length".to_string(),
        java_name: None,
        resolved_target: None,
        args: vec![],
        argument_style: CallArgumentStyle::Comma,
        java_type: JavaType::Primitive("int".to_string()),
        span: dummy_span(),
    };
    LogInvocationItem::Statement(IrStatement::Expression {
        expr: call,
        span: dummy_span(),
    })
}

fn string_format_message(template: &str, args: Vec<IrExpression>) -> LogMessage {
    LogMessage {
        expression: IrExpression::StringFormat {
            format_string: template.to_string(),
            args,
            span: dummy_span(),
        },
        span: dummy_span(),
    }
}

fn identifier_expr(name: &str) -> IrExpression {
    IrExpression::Identifier {
        name: name.to_string(),
        java_type: string_type(),
        span: dummy_span(),
    }
}
