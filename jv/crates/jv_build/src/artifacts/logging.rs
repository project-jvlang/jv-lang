use super::GeneratedArtifact;
use jv_pm::{LoggingConfig, LoggingFramework, OtelProtocol};

/// ロギング構成のテンプレートを生成する。
///
/// OpenTelemetry が有効な場合、フレームワーク別に構成ファイルを生成する。
/// 対応フレームワーク:
/// - SLF4J（logback.xml）
/// - Log4j2（log4j2.xml）
/// - JUL （logging.properties）
///
/// その他のフレームワークでは OpenTelemetry 側のテンプレートを提供しない。
pub fn generate_logging_artifacts(config: &LoggingConfig) -> Vec<GeneratedArtifact> {
    if !config.opentelemetry.enabled {
        return Vec::new();
    }

    match &config.framework {
        LoggingFramework::Slf4j => vec![GeneratedArtifact {
            filename: "logback.xml",
            contents: render_logback(config),
        }],
        LoggingFramework::Log4j2 => vec![GeneratedArtifact {
            filename: "log4j2.xml",
            contents: render_log4j2(config),
        }],
        LoggingFramework::Jul => vec![GeneratedArtifact {
            filename: "logging.properties",
            contents: render_jul(config),
        }],
        // 現時点ではテンプレートが未定義。
        LoggingFramework::JbossLogging
        | LoggingFramework::CommonsLogging
        | LoggingFramework::Custom(_) => Vec::new(),
    }
}

fn render_logback(config: &LoggingConfig) -> String {
    let mut lines = Vec::new();
    lines.push("<configuration scan=\"true\">".to_string());
    lines.push(format!(
        "  <appender name=\"OTEL\" class=\"io.opentelemetry.instrumentation.logback.appender.v1_0.OpenTelemetryAppender\">"
    ));
    lines.push(format!(
        "    <otelExporter endpoint=\"{}\" protocol=\"{}\"/>",
        xml_escape(endpoint(config)),
        otel_protocol(&config.opentelemetry.protocol)
    ));

    if !config.opentelemetry.resource.is_empty() {
        lines.push("    <otelResource>".to_string());
        for (key, value) in sorted_entries(&config.opentelemetry.resource) {
            lines.push(format!(
                "      <attribute key=\"{}\" value=\"{}\"/>",
                xml_escape(key),
                xml_escape(value)
            ));
        }
        lines.push("    </otelResource>".to_string());
    }

    if !config.opentelemetry.attributes.is_empty() {
        lines.push("    <otelAttributes>".to_string());
        for (key, value) in sorted_entries(&config.opentelemetry.attributes) {
            lines.push(format!(
                "      <attribute key=\"{}\" value=\"{}\"/>",
                xml_escape(key),
                xml_escape(value)
            ));
        }
        lines.push("    </otelAttributes>".to_string());
    }

    lines.push("  </appender>".to_string());
    lines.push(format!(
        "  <root level=\"{}\">",
        xml_escape(config.log_level.to_string())
    ));
    lines.push("    <appender-ref ref=\"OTEL\"/>".to_string());
    lines.push("  </root>".to_string());
    lines.push("</configuration>".to_string());

    lines.join("\n")
}

fn render_log4j2(config: &LoggingConfig) -> String {
    let mut lines = Vec::new();
    lines.push("<Configuration status=\"WARN\">".to_string());
    lines.push("  <Appenders>".to_string());
    lines.push(format!(
        "    <OpenTelemetry name=\"OTEL\" endpoint=\"{}\" protocol=\"{}\" traceContext=\"{}\">",
        xml_escape(endpoint(config)),
        otel_protocol(&config.opentelemetry.protocol),
        if config.opentelemetry.trace_context {
            "enable"
        } else {
            "disable"
        }
    ));

    if !config.opentelemetry.resource.is_empty() {
        lines.push("      <ResourceAttributes>".to_string());
        for (key, value) in sorted_entries(&config.opentelemetry.resource) {
            lines.push(format!(
                "        <Attribute key=\"{}\" value=\"{}\"/>",
                xml_escape(key),
                xml_escape(value)
            ));
        }
        lines.push("      </ResourceAttributes>".to_string());
    }

    if !config.opentelemetry.attributes.is_empty() {
        lines.push("      <LogAttributes>".to_string());
        for (key, value) in sorted_entries(&config.opentelemetry.attributes) {
            lines.push(format!(
                "        <Attribute key=\"{}\" value=\"{}\"/>",
                xml_escape(key),
                xml_escape(value)
            ));
        }
        lines.push("      </LogAttributes>".to_string());
    }

    lines.push("    </OpenTelemetry>".to_string());
    lines.push("  </Appenders>".to_string());
    lines.push("  <Loggers>".to_string());
    lines.push(format!(
        "    <Root level=\"{}\">",
        xml_escape(config.log_level.to_string())
    ));
    lines.push("      <AppenderRef ref=\"OTEL\"/>".to_string());
    lines.push("    </Root>".to_string());
    lines.push("  </Loggers>".to_string());
    lines.push("</Configuration>".to_string());
    lines.join("\n")
}

fn render_jul(config: &LoggingConfig) -> String {
    let mut lines = Vec::new();
    let handler = "io.opentelemetry.instrumentation.logging.appender.javautil.OpenTelemetryHandler";
    lines.push(format!("handlers = {}", handler));
    lines.push(format!("{}.level = {}", handler, config.log_level));
    lines.push(format!(
        "{}.endpoint = {}",
        handler,
        properties_escape(endpoint(config))
    ));
    lines.push(format!(
        "{}.protocol = {}",
        handler,
        otel_protocol(&config.opentelemetry.protocol)
    ));
    lines.push(format!(
        "{}.traceContext = {}",
        handler,
        if config.opentelemetry.trace_context {
            "true"
        } else {
            "false"
        }
    ));

    if !config.opentelemetry.resource.is_empty() {
        for (key, value) in sorted_entries(&config.opentelemetry.resource) {
            lines.push(format!(
                "{}.resource.{} = {}",
                handler,
                properties_escape(key),
                properties_escape(value)
            ));
        }
    }

    if !config.opentelemetry.attributes.is_empty() {
        for (key, value) in sorted_entries(&config.opentelemetry.attributes) {
            lines.push(format!(
                "{}.attribute.{} = {}",
                handler,
                properties_escape(key),
                properties_escape(value)
            ));
        }
    }

    lines.join("\n")
}

fn endpoint(config: &LoggingConfig) -> &str {
    config
        .opentelemetry
        .endpoint
        .as_deref()
        .unwrap_or("http://localhost:4317")
}

fn sorted_entries<'a>(
    map: &'a std::collections::HashMap<String, String>,
) -> Vec<(&'a str, &'a str)> {
    let mut entries: Vec<(&str, &str)> =
        map.iter().map(|(k, v)| (k.as_str(), v.as_str())).collect();
    entries.sort_by(|left, right| left.0.cmp(right.0));
    entries
}

fn otel_protocol(protocol: &OtelProtocol) -> &'static str {
    match protocol {
        OtelProtocol::Grpc => "grpc",
        OtelProtocol::Http => "http",
    }
}

fn xml_escape(value: impl AsRef<str>) -> String {
    value
        .as_ref()
        .replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&apos;")
}

fn properties_escape(value: impl AsRef<str>) -> String {
    let mut result = String::new();
    for ch in value.as_ref().chars() {
        match ch {
            '\\' => result.push_str("\\\\"),
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            '=' => result.push_str("\\="),
            ':' => result.push_str("\\:"),
            '#' => result.push_str("\\#"),
            '!' => result.push_str("\\!"),
            ' ' => result.push_str("\\ "),
            other => result.push(other),
        }
    }
    result
}
