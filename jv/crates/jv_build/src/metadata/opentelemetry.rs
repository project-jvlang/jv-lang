use jv_ir::types::LoggingFrameworkKind;

/// OpenTelemetry span accessor class.
pub const SPAN_CLASS: &str = "io.opentelemetry.api.trace.Span";
/// OpenTelemetry span context class.
pub const SPAN_CONTEXT_CLASS: &str = "io.opentelemetry.api.trace.SpanContext";
/// MDC/NDC key for trace identifier.
pub const TRACE_ID_KEY: &str = "traceId";
/// MDC/NDC key for span identifier.
pub const SPAN_ID_KEY: &str = "spanId";

/// Framework-specific accessor that exposes MDC/NDC mutation APIs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TraceContextAccessor {
    pub import: &'static str,
    pub type_name: &'static str,
    pub put_method: &'static str,
    pub remove_method: &'static str,
}

impl TraceContextAccessor {
    pub const fn new(
        import: &'static str,
        type_name: &'static str,
        put_method: &'static str,
        remove_method: &'static str,
    ) -> Self {
        Self {
            import,
            type_name,
            put_method,
            remove_method,
        }
    }
}

/// Resolve trace context accessor for the configured logging framework.
pub fn trace_context_accessor(framework: &LoggingFrameworkKind) -> Option<TraceContextAccessor> {
    match framework {
        LoggingFrameworkKind::Slf4j => Some(TraceContextAccessor::new(
            "org.slf4j.MDC",
            "MDC",
            "put",
            "remove",
        )),
        LoggingFrameworkKind::Log4j2 => Some(TraceContextAccessor::new(
            "org.apache.logging.log4j.ThreadContext",
            "ThreadContext",
            "put",
            "remove",
        )),
        LoggingFrameworkKind::JbossLogging => Some(TraceContextAccessor::new(
            "org.jboss.logging.MDC",
            "MDC",
            "put",
            "remove",
        )),
        LoggingFrameworkKind::CommonsLogging => Some(TraceContextAccessor::new(
            "org.slf4j.MDC",
            "MDC",
            "put",
            "remove",
        )),
        LoggingFrameworkKind::Jul => None,
        LoggingFrameworkKind::Custom { .. } => None,
    }
}
