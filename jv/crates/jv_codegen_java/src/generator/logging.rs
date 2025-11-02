use super::*;

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
    match analyze_message(message) {
        MessageFormat::Literal(text) => Ok(format!(
            "{logger}.{method}(\"{literal}\");",
            logger = logger_name,
            method = method,
            literal = JavaCodeGenerator::escape_string(&text)
        )),
        MessageFormat::Interpolated {
            format,
            segments,
            args,
        } => {
            if args.is_empty() {
                return Ok(format!(
                    "{logger}.{method}(\"{literal}\");",
                    logger = logger_name,
                    method = method,
                    literal = JavaCodeGenerator::escape_string(&segments[0])
                ));
            }

            let pattern = match style {
                PlaceholderStyle::Braces => build_brace_pattern(&segments, args.len()),
                PlaceholderStyle::Percent => format.clone(),
            };
            let escaped_pattern = JavaCodeGenerator::escape_string(&pattern);
            let mut invocation = format!(
                "{logger}.{method}(\"{pattern}\"",
                logger = logger_name,
                method = method,
                pattern = escaped_pattern
            );
            let mut rendered_args = Vec::with_capacity(args.len());
            for arg in args {
                rendered_args.push(generator.generate_expression(arg)?);
            }
            if !rendered_args.is_empty() {
                invocation.push_str(", ");
                invocation.push_str(&rendered_args.join(", "));
            }
            invocation.push_str(");");
            Ok(invocation)
        }
        MessageFormat::Expression(expr) => {
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
    }
}

fn emit_concatenated_call(
    generator: &mut JavaCodeGenerator,
    logger_name: &str,
    method: &str,
    message: &LogMessage,
) -> Result<String, CodeGenError> {
    let expression = match analyze_message(message) {
        MessageFormat::Literal(text) => format!("\"{}\"", JavaCodeGenerator::escape_string(&text)),
        MessageFormat::Interpolated { segments, args, .. } => {
            build_concatenation(generator, &segments, &args)?
        }
        MessageFormat::Expression(expr) => {
            let rendered = generator.generate_expression(expr)?;
            ensure_string_value(generator, expr, rendered)
        }
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

    match analyze_message(message) {
        MessageFormat::Literal(text) => Ok(format!(
            "{logger}.log({level}, \"{literal}\");",
            logger = logger_name,
            level = level_const,
            literal = JavaCodeGenerator::escape_string(&text)
        )),
        MessageFormat::Interpolated { segments, args, .. } => {
            if args.is_empty() {
                return Ok(format!(
                    "{logger}.log({level}, \"{literal}\");",
                    logger = logger_name,
                    level = level_const,
                    literal = JavaCodeGenerator::escape_string(&segments[0])
                ));
            }
            let pattern = build_numbered_pattern(&segments, args.len());
            let escaped = JavaCodeGenerator::escape_string(&pattern);
            let mut invocation = format!(
                "{logger}.log({level}, \"{pattern}\"",
                logger = logger_name,
                level = level_const,
                pattern = escaped
            );
            let mut rendered_args = Vec::with_capacity(args.len());
            for arg in args {
                rendered_args.push(generator.generate_expression(arg)?);
            }
            invocation.push_str(", ");
            invocation.push_str(&rendered_args.join(", "));
            invocation.push_str(");");
            Ok(invocation)
        }
        MessageFormat::Expression(expr) => {
            let rendered = generator.generate_expression(expr)?;
            let wrapped = ensure_string_value(generator, expr, rendered);
            Ok(format!(
                "{logger}.log({level}, {value});",
                logger = logger_name,
                level = level_const,
                value = wrapped
            ))
        }
    }
}

enum MessageFormat<'a> {
    Literal(String),
    Interpolated {
        format: String,
        segments: Vec<String>,
        args: Vec<&'a IrExpression>,
    },
    Expression(&'a IrExpression),
}

fn analyze_message(message: &LogMessage) -> MessageFormat<'_> {
    let expr = &message.expression;
    match expr {
        IrExpression::Literal(Literal::String(text), _) => MessageFormat::Literal(text.clone()),
        IrExpression::StringFormat {
            format_string,
            args,
            ..
        } => {
            if let Some(segments) = split_format_segments(format_string, args.len()) {
                MessageFormat::Interpolated {
                    format: format_string.clone(),
                    segments,
                    args: args.iter().collect(),
                }
            } else {
                MessageFormat::Expression(expr)
            }
        }
        _ => MessageFormat::Expression(expr),
    }
}

fn split_format_segments(format: &str, arg_count: usize) -> Option<Vec<String>> {
    let mut segments = Vec::new();
    let mut remaining = format;
    let mut placeholders = 0usize;

    while let Some(index) = remaining.find("%s") {
        let (prefix, rest) = remaining.split_at(index);
        segments.push(unescape_percent(prefix));
        remaining = &rest[2..];
        placeholders += 1;
    }
    segments.push(unescape_percent(remaining));

    if placeholders == arg_count {
        Some(segments)
    } else {
        None
    }
}

fn unescape_percent(segment: &str) -> String {
    let mut result = String::with_capacity(segment.len());
    let mut chars = segment.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '%' {
            if matches!(chars.peek(), Some('%')) {
                chars.next();
                result.push('%');
                continue;
            }
        }
        result.push(ch);
    }
    result
}

fn build_brace_pattern(segments: &[String], arg_count: usize) -> String {
    let mut pattern = String::new();
    for index in 0..arg_count {
        pattern.push_str(&segments[index]);
        pattern.push_str("{}");
    }
    pattern.push_str(&segments[arg_count]);
    pattern
}

fn build_numbered_pattern(segments: &[String], arg_count: usize) -> String {
    let mut pattern = String::new();
    for index in 0..arg_count {
        pattern.push_str(&segments[index]);
        pattern.push('{');
        pattern.push_str(&index.to_string());
        pattern.push('}');
    }
    pattern.push_str(&segments[arg_count]);
    pattern
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
