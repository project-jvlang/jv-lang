use super::{transform_expression, transform_statement};
use crate::context::TransformContext;
use crate::error::TransformError;
use crate::types::{
    IrExpression, JavaType, LogGuardKind, LogInvocationItem, LogInvocationPlan, LogLevel,
    LogMessage,
};
use jv_ast::{LogBlock, LogBlockLevel, LogItem};

pub(crate) fn lower_log_block_expression(
    block: LogBlock,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let span = block.span.clone();
    match build_plan(block, context)? {
        Some(plan) => Ok(IrExpression::LogInvocation {
            plan: Box::new(plan),
            java_type: JavaType::void(),
            span,
        }),
        None => Ok(IrExpression::Block {
            statements: Vec::new(),
            java_type: JavaType::void(),
            span,
        }),
    }
}

fn build_plan(
    block: LogBlock,
    context: &mut TransformContext,
) -> Result<Option<LogInvocationPlan>, TransformError> {
    let span = block.span.clone();
    let options = context.logging_options().clone();
    let (level, uses_default_level) = resolve_block_level(block.level, &options);
    let threshold = options.active_level;

    if !uses_default_level && !level.is_enabled(threshold) {
        return Ok(None);
    }

    let logger_field = context.allocate_logger_field(None);
    let guard_kind = guard_for_level(level);

    let mut items = Vec::new();

    for item in block.items {
        match item {
            LogItem::Statement(stmt) => {
                let mut lowered = transform_statement(stmt, context)?;
                for statement in lowered.drain(..) {
                    items.push(LogInvocationItem::Statement(statement));
                }
            }
            LogItem::Expression(expr) => {
                let lowered = transform_expression(expr, context)?;
                let message_span = lowered.span();
                items.push(LogInvocationItem::Message(LogMessage {
                    expression: lowered,
                    span: message_span,
                }));
            }
            LogItem::Nested(inner) => {
                if let Some(nested_plan) = build_plan(inner, context)? {
                    items.push(LogInvocationItem::Nested(Box::new(nested_plan)));
                }
            }
        }
    }

    Ok(Some(LogInvocationPlan {
        class_id: None,
        logger_field,
        level,
        uses_default_level,
        guard_kind,
        items,
        span,
    }))
}

fn resolve_block_level(
    level: LogBlockLevel,
    options: &crate::context::LoggingOptions,
) -> (LogLevel, bool) {
    match level {
        LogBlockLevel::Default => (options.default_level, true),
        LogBlockLevel::Trace => (LogLevel::Trace, false),
        LogBlockLevel::Debug => (LogLevel::Debug, false),
        LogBlockLevel::Info => (LogLevel::Info, false),
        LogBlockLevel::Warn => (LogLevel::Warn, false),
        LogBlockLevel::Error => (LogLevel::Error, false),
    }
}

fn guard_for_level(level: LogLevel) -> Option<LogGuardKind> {
    match level {
        LogLevel::Trace | LogLevel::Debug => LogGuardKind::for_level(level),
        _ => None,
    }
}
