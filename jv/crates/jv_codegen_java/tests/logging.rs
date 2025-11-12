use jv_ast::Span;
use jv_codegen_java::{JavaCodeGenConfig, JavaCodeGenerator};
use jv_ir::{
    IrExpression, IrModifiers, IrParameter, IrProgram, IrStatement, IrVisibility, JavaType,
    LogGuardKind, LogInvocationItem, LogInvocationPlan, LogLevel, LogMessage, LoggerFieldId,
    LoggerFieldSpec, LoggingFrameworkKind, LoggingMetadata,
};

const LOGGER_ID: LoggerFieldId = LoggerFieldId(0);

fn dummy_span() -> Span {
    Span::dummy()
}

fn string_type() -> JavaType {
    JavaType::Reference {
        name: "String".to_string(),
        generic_args: Vec::new(),
    }
}

fn reference_type(name: &str) -> JavaType {
    JavaType::Reference {
        name: name.to_string(),
        generic_args: Vec::new(),
    }
}

fn identifier_expr(name: &str) -> IrExpression {
    IrExpression::Identifier {
        name: name.to_string(),
        java_type: string_type(),
        span: dummy_span(),
    }
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

fn log_plan(
    level: LogLevel,
    guard: Option<LogGuardKind>,
    mut items: Vec<LogInvocationItem>,
    message: LogMessage,
) -> LogInvocationPlan {
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

fn build_log_method(plans: Vec<LogInvocationPlan>, param_names: &[&str]) -> IrStatement {
    let expression_statements: Vec<IrStatement> = plans
        .into_iter()
        .map(|plan| IrStatement::Expression {
            expr: IrExpression::LogInvocation {
                plan: Box::new(plan),
                java_type: JavaType::void(),
                span: dummy_span(),
            },
            span: dummy_span(),
        })
        .collect();

    let body = IrExpression::Block {
        statements: expression_statements,
        java_type: JavaType::void(),
        span: dummy_span(),
    };

    let parameters: Vec<IrParameter> = param_names
        .iter()
        .map(|name| IrParameter {
            name: (*name).to_string(),
            java_type: string_type(),
            modifiers: IrModifiers::default(),
            span: dummy_span(),
        })
        .collect();

    IrStatement::MethodDeclaration {
        name: "log".to_string(),
        java_name: None,
        type_parameters: Vec::new(),
        parameters,
        primitive_return: None,
        return_type: JavaType::void(),
        body: Some(body),
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        throws: Vec::new(),
        assertion_patterns: Vec::new(),
        span: dummy_span(),
    }
}

fn render_logging_source(
    framework: LoggingFrameworkKind,
    plans: Vec<LogInvocationPlan>,
    logger_type: &str,
    param_names: &[&str],
    trace_context: bool,
) -> String {
    let logger_field = IrStatement::FieldDeclaration {
        name: "LOGGER".to_string(),
        java_type: reference_type(logger_type),
        initializer: None,
        modifiers: IrModifiers {
            visibility: IrVisibility::Private,
            is_static: true,
            is_final: true,
            ..IrModifiers::default()
        },
        span: dummy_span(),
    };

    let method = build_log_method(plans, param_names);

    let class = IrStatement::ClassDeclaration {
        name: "LoggingSample".to_string(),
        type_parameters: Vec::new(),
        superclass: None,
        interfaces: Vec::new(),
        fields: vec![logger_field],
        methods: vec![method],
        nested_classes: Vec::new(),
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
        imports: Vec::new(),
        type_declarations: vec![class],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        logging: metadata,
        tuple_record_plans: Vec::new(),
        span: dummy_span(),
    };

    let mut generator = JavaCodeGenerator::new();
    let unit = generator
        .generate_compilation_unit(&program)
        .expect("ロギングのコード生成に成功するはずです");
    unit.to_source(&JavaCodeGenConfig::default())
}

#[test]
fn slf4j_debug_emits_guard_and_brace_placeholders() {
    let plan = log_plan(
        LogLevel::Debug,
        Some(LogGuardKind::DebugEnabled),
        Vec::new(),
        string_format_message("value %s", vec![identifier_expr("value")]),
    );

    let source = render_logging_source(
        LoggingFrameworkKind::Slf4j,
        vec![plan],
        "org.slf4j.Logger",
        &["value"],
        false,
    );

    assert!(
        source.contains("if (LOGGER.isDebugEnabled())"),
        "SLF4J の DEBUG ログではガードが必要です:\n{source}"
    );
    assert!(
        source.contains("LOGGER.debug(\"value {}\", value);"),
        "SLF4J では波括弧プレースホルダーが期待されます:\n{source}"
    );
}

#[test]
fn jboss_logging_uses_percent_placeholders() {
    let plan = log_plan(
        LogLevel::Error,
        None,
        Vec::new(),
        string_format_message(
            "%s -> %s",
            vec![identifier_expr("from"), identifier_expr("to")],
        ),
    );

    let source = render_logging_source(
        LoggingFrameworkKind::JbossLogging,
        vec![plan],
        "org.jboss.logging.Logger",
        &["from", "to"],
        false,
    );

    assert!(
        source.contains("LOGGER.errorf(\"%s -> %s\", from, to);"),
        "JBoss Logging は printf 形式を維持するはずです:\n{source}"
    );
}

#[test]
fn jul_logging_emits_numbered_placeholders_and_guard() {
    let plan = log_plan(
        LogLevel::Debug,
        Some(LogGuardKind::DebugEnabled),
        Vec::new(),
        string_format_message(
            "[%s] %s",
            vec![identifier_expr("traceId"), identifier_expr("message")],
        ),
    );

    let source = render_logging_source(
        LoggingFrameworkKind::Jul,
        vec![plan],
        "java.util.logging.Logger",
        &["traceId", "message"],
        false,
    );

    assert!(
        source.contains("if (LOGGER.isLoggable(Level.FINE))"),
        "JUL では isLoggable(Level.FINE) ガードが必要です:\n{source}"
    );
    assert!(
        source.contains("LOGGER.log(Level.FINE, \"[{0}] {1}\", traceId, message);"),
        "JUL は番号付きプレースホルダーを利用するはずです:\n{source}"
    );
}
