use super::*;
use crate::log_message::LogMessageInterpolator;

pub(super) fn emit_log_plan(
    generator: &mut JavaCodeGenerator,
    plan: &LogInvocationPlan,
) -> Result<String, CodeGenError> {
    let logger_name = generator
        .resolve_logger_field_name(plan.logger_field)
        .map(str::to_string)
        .ok_or_else(|| CodeGenError::UnsupportedConstruct {
            construct: format!(
                "Logger field with id {} is not registered in logging metadata",
                plan.logger_field.raw()
            ),
            span: Some(plan.span.clone()),
        })?;

    let strategy = StrategyRegistry::resolve(&generator.logging_framework).ok_or_else(|| {
        CodeGenError::UnsupportedConstruct {
            construct: format!(
                "Unsupported logging framework '{:?}' during Java emission",
                generator.logging_framework
            ),
            span: Some(plan.span.clone()),
        }
    })?;

    let mut builder = generator.builder();
    let mut has_guard = false;

    if let Some(kind) = plan.guard_kind {
        if let Some(condition) =
            strategy.guard_condition(generator, &logger_name, plan.level, kind)?
        {
            builder.push_line(&format!("if ({condition}) {{"));
            builder.indent();
            has_guard = true;
        }
    }

    for item in &plan.items {
        match item {
            LogInvocationItem::Statement(statement) => {
                let rendered = generator.generate_statement(statement)?;
                if !rendered.trim().is_empty() {
                    JavaCodeGenerator::push_lines(&mut builder, &rendered);
                }
            }
            LogInvocationItem::Message(message) => {
                let rendered =
                    strategy.emit_message(generator, plan.level, message, &logger_name)?;
                if !rendered.trim().is_empty() {
                    JavaCodeGenerator::push_lines(&mut builder, &rendered);
                }
            }
            LogInvocationItem::Nested(nested) => {
                let rendered = emit_log_plan(generator, nested)?;
                if !rendered.trim().is_empty() {
                    JavaCodeGenerator::push_lines(&mut builder, &rendered);
                }
            }
        }
    }

    if has_guard {
        builder.dedent();
        builder.push_line("}");
    }

    Ok(builder.build())
}

trait LoggingFrameworkStrategy {
    fn guard_condition(
        &self,
        generator: &mut JavaCodeGenerator,
        logger_name: &str,
        level: LogLevel,
        kind: LogGuardKind,
    ) -> Result<Option<String>, CodeGenError>;

    fn emit_message(
        &self,
        generator: &mut JavaCodeGenerator,
        level: LogLevel,
        message: &LogMessage,
        logger_name: &str,
    ) -> Result<String, CodeGenError>;
}

struct StrategyRegistry;

impl StrategyRegistry {
    fn resolve(kind: &LoggingFrameworkKind) -> Option<&'static dyn LoggingFrameworkStrategy> {
        match kind {
            LoggingFrameworkKind::Slf4j => Some(&SLF4J_STRATEGY),
            LoggingFrameworkKind::Log4j2 => Some(&LOG4J2_STRATEGY),
            LoggingFrameworkKind::JbossLogging => Some(&JBOSS_STRATEGY),
            LoggingFrameworkKind::CommonsLogging => Some(&COMMONS_STRATEGY),
            LoggingFrameworkKind::Jul => Some(&JUL_STRATEGY),
            LoggingFrameworkKind::Custom { .. } => None,
        }
    }
}

static SLF4J_STRATEGY: Slf4jStrategy = Slf4jStrategy;
static LOG4J2_STRATEGY: Log4j2Strategy = Log4j2Strategy;
static JBOSS_STRATEGY: JbossStrategy = JbossStrategy;
static COMMONS_STRATEGY: CommonsStrategy = CommonsStrategy;
static JUL_STRATEGY: JulStrategy = JulStrategy;

struct Slf4jStrategy;
struct Log4j2Strategy;
struct JbossStrategy;
struct CommonsStrategy;
struct JulStrategy;

impl LoggingFrameworkStrategy for Slf4jStrategy {
    fn guard_condition(
        &self,
        _generator: &mut JavaCodeGenerator,
        logger_name: &str,
        _level: LogLevel,
        kind: LogGuardKind,
    ) -> Result<Option<String>, CodeGenError> {
        Ok(Some(format!(
            "{logger}.{method}()",
            logger = logger_name,
            method = guard_method_name(kind)
        )))
    }

    fn emit_message(
        &self,
        generator: &mut JavaCodeGenerator,
        level: LogLevel,
        message: &LogMessage,
        logger_name: &str,
    ) -> Result<String, CodeGenError> {
        let method = log_level_method(level);
        emit_placeholder_call(
            generator,
            logger_name,
            method,
            message,
            PlaceholderStyle::Braces,
        )
    }
}

impl LoggingFrameworkStrategy for Log4j2Strategy {
    fn guard_condition(
        &self,
        _generator: &mut JavaCodeGenerator,
        logger_name: &str,
        _level: LogLevel,
        kind: LogGuardKind,
    ) -> Result<Option<String>, CodeGenError> {
        Ok(Some(format!(
            "{logger}.{method}()",
            logger = logger_name,
            method = guard_method_name(kind)
        )))
    }

    fn emit_message(
        &self,
        generator: &mut JavaCodeGenerator,
        level: LogLevel,
        message: &LogMessage,
        logger_name: &str,
    ) -> Result<String, CodeGenError> {
        let method = log_level_method(level);
        emit_placeholder_call(
            generator,
            logger_name,
            method,
            message,
            PlaceholderStyle::Braces,
        )
    }
}

impl LoggingFrameworkStrategy for JbossStrategy {
    fn guard_condition(
        &self,
        _generator: &mut JavaCodeGenerator,
        logger_name: &str,
        _level: LogLevel,
        kind: LogGuardKind,
    ) -> Result<Option<String>, CodeGenError> {
        Ok(Some(format!(
            "{logger}.{method}()",
            logger = logger_name,
            method = guard_method_name(kind)
        )))
    }

    fn emit_message(
        &self,
        generator: &mut JavaCodeGenerator,
        level: LogLevel,
        message: &LogMessage,
        logger_name: &str,
    ) -> Result<String, CodeGenError> {
        let method = jboss_level_method(level);
        emit_placeholder_call(
            generator,
            logger_name,
            method,
            message,
            PlaceholderStyle::Percent,
        )
    }
}

impl LoggingFrameworkStrategy for CommonsStrategy {
    fn guard_condition(
        &self,
        _generator: &mut JavaCodeGenerator,
        logger_name: &str,
        _level: LogLevel,
        kind: LogGuardKind,
    ) -> Result<Option<String>, CodeGenError> {
        Ok(Some(format!(
            "{logger}.{method}()",
            logger = logger_name,
            method = guard_method_name(kind)
        )))
    }

    fn emit_message(
        &self,
        generator: &mut JavaCodeGenerator,
        level: LogLevel,
        message: &LogMessage,
        logger_name: &str,
    ) -> Result<String, CodeGenError> {
        let method = log_level_method(level);
        emit_concatenated_call(generator, logger_name, method, message)
    }
}

impl LoggingFrameworkStrategy for JulStrategy {
    fn guard_condition(
        &self,
        generator: &mut JavaCodeGenerator,
        logger_name: &str,
        level: LogLevel,
        _kind: LogGuardKind,
    ) -> Result<Option<String>, CodeGenError> {
        let level_const = jul_level_constant(level);
        generator.add_import("java.util.logging.Level");
        Ok(Some(format!(
            "{logger}.isLoggable({level_const})",
            logger = logger_name
        )))
    }

    fn emit_message(
        &self,
        generator: &mut JavaCodeGenerator,
        level: LogLevel,
        message: &LogMessage,
        logger_name: &str,
    ) -> Result<String, CodeGenError> {
        emit_jul_log_call(generator, logger_name, level, message)
    }
}

enum PlaceholderStyle {
    Braces,
    Percent,
}

fn emit_placeholder_call(
    generator: &mut JavaCodeGenerator,
    logger_name: &str,
    method: &str,
    message: &LogMessage,
    style: PlaceholderStyle,
) -> Result<String, CodeGenError> {
    let interpolator = LogMessageInterpolator::new(message);

    if let Some(text) = interpolator.literal_text() {
        return Ok(format!(
            "{logger}.{method}(\"{literal}\");",
            logger = logger_name,
            method = method,
            literal = JavaCodeGenerator::escape_string(text)
        ));
    }

    let placeholder = match style {
        PlaceholderStyle::Braces => interpolator.braces_pattern(),
        PlaceholderStyle::Percent => interpolator.percent_pattern(),
    };

    if let Some(result) = placeholder {
        let escaped_pattern = JavaCodeGenerator::escape_string(result.pattern());
        let mut invocation = format!(
            "{logger}.{method}(\"{pattern}\"",
            logger = logger_name,
            method = method,
            pattern = escaped_pattern
        );
        let mut rendered_args = Vec::with_capacity(result.args().len());
        for arg in result.args() {
            rendered_args.push(generator.generate_expression(arg)?);
        }
        if !rendered_args.is_empty() {
            invocation.push_str(", ");
            invocation.push_str(&rendered_args.join(", "));
        }
        invocation.push_str(");");
        return Ok(invocation);
    }

    let expr = interpolator
        .expression()
        .expect("ログメッセージの式が必要です");
    let rendered = generator.generate_expression(expr)?;
    match style {
        PlaceholderStyle::Percent => Ok(format!(
            "{logger}.{method}(\"%s\", {value});",
            logger = logger_name,
            method = method,
            value = rendered
        )),
        PlaceholderStyle::Braces => Ok(format!(
            "{logger}.{method}(\"{{}}\", {value});",
            logger = logger_name,
            method = method,
            value = rendered
        )),
    }
}

fn emit_concatenated_call(
    generator: &mut JavaCodeGenerator,
    logger_name: &str,
    method: &str,
    message: &LogMessage,
) -> Result<String, CodeGenError> {
    let interpolator = LogMessageInterpolator::new(message);

    let expression = if let Some(text) = interpolator.literal_text() {
        format!("\"{}\"", JavaCodeGenerator::escape_string(text))
    } else if let Some(segments) = interpolator.concatenation_segments() {
        build_concatenation(generator, segments.segments(), segments.args())?
    } else {
        let expr = interpolator
            .expression()
            .expect("ログメッセージの式が必要です");
        let rendered = generator.generate_expression(expr)?;
        ensure_string_value(generator, expr, rendered)
    };
    Ok(format!(
        "{logger}.{method}({expr});",
        logger = logger_name,
        method = method,
        expr = expression
    ))
}

fn emit_jul_log_call(
    generator: &mut JavaCodeGenerator,
    logger_name: &str,
    level: LogLevel,
    message: &LogMessage,
) -> Result<String, CodeGenError> {
    let level_const = jul_level_constant(level);
    generator.add_import("java.util.logging.Level");

    let interpolator = LogMessageInterpolator::new(message);

    if let Some(text) = interpolator.literal_text() {
        return Ok(format!(
            "{logger}.log({level}, \"{literal}\");",
            logger = logger_name,
            level = level_const,
            literal = JavaCodeGenerator::escape_string(text)
        ));
    }

    if let Some(result) = interpolator.numbered_pattern() {
        let escaped = JavaCodeGenerator::escape_string(result.pattern());
        let mut invocation = format!(
            "{logger}.log({level}, \"{pattern}\"",
            logger = logger_name,
            level = level_const,
            pattern = escaped
        );
        let mut rendered_args = Vec::with_capacity(result.args().len());
        for arg in result.args() {
            rendered_args.push(generator.generate_expression(arg)?);
        }
        if !rendered_args.is_empty() {
            invocation.push_str(", ");
            invocation.push_str(&rendered_args.join(", "));
        }
        invocation.push_str(");");
        return Ok(invocation);
    }

    let expr = interpolator
        .expression()
        .expect("ログメッセージの式が必要です");
    let rendered = generator.generate_expression(expr)?;
    let wrapped = ensure_string_value(generator, expr, rendered);
    Ok(format!(
        "{logger}.log({level}, {value});",
        logger = logger_name,
        level = level_const,
        value = wrapped
    ))
}

fn build_concatenation(
    generator: &mut JavaCodeGenerator,
    segments: &[String],
    args: &[&IrExpression],
) -> Result<String, CodeGenError> {
    let mut parts = Vec::new();
    for (idx, arg) in args.iter().enumerate() {
        let literal = &segments[idx];
        if !literal.is_empty() {
            parts.push(format!("\"{}\"", JavaCodeGenerator::escape_string(literal)));
        }
        let rendered = generator.generate_expression(arg)?;
        parts.push(ensure_string_value(generator, arg, rendered));
    }
    if let Some(last) = segments.last() {
        if !last.is_empty() {
            parts.push(format!("\"{}\"", JavaCodeGenerator::escape_string(last)));
        }
    }
    if parts.is_empty() {
        parts.push("\"\"".to_string());
    }

    let mut iter = parts.into_iter();
    let mut expression = iter.next().unwrap_or_else(|| "\"\"".to_string());
    for part in iter {
        expression.push_str(" + ");
        expression.push_str(&part);
    }
    Ok(expression)
}

fn ensure_string_value(
    generator: &mut JavaCodeGenerator,
    expr: &IrExpression,
    rendered: String,
) -> String {
    match JavaCodeGenerator::expression_java_type(expr) {
        Some(JavaType::Reference { name, .. })
            if name == "java.lang.String" || name == "String" =>
        {
            rendered
        }
        _ => {
            generator.add_import("java.lang.String");
            format!("String.valueOf({rendered})")
        }
    }
}

fn log_level_method(level: LogLevel) -> &'static str {
    match level {
        LogLevel::Trace => "trace",
        LogLevel::Debug => "debug",
        LogLevel::Info => "info",
        LogLevel::Warn => "warn",
        LogLevel::Error => "error",
    }
}

fn jboss_level_method(level: LogLevel) -> &'static str {
    match level {
        LogLevel::Trace => "tracef",
        LogLevel::Debug => "debugf",
        LogLevel::Info => "infof",
        LogLevel::Warn => "warnf",
        LogLevel::Error => "errorf",
    }
}

fn guard_method_name(kind: LogGuardKind) -> &'static str {
    match kind {
        LogGuardKind::TraceEnabled => "isTraceEnabled",
        LogGuardKind::DebugEnabled => "isDebugEnabled",
        LogGuardKind::InfoEnabled => "isInfoEnabled",
        LogGuardKind::WarnEnabled => "isWarnEnabled",
        LogGuardKind::ErrorEnabled => "isErrorEnabled",
    }
}

fn jul_level_constant(level: LogLevel) -> &'static str {
    match level {
        LogLevel::Trace => "Level.FINEST",
        LogLevel::Debug => "Level.FINE",
        LogLevel::Info => "Level.INFO",
        LogLevel::Warn => "Level.WARNING",
        LogLevel::Error => "Level.SEVERE",
    }
}
