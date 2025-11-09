use crate::deps::{MavenDependency, collect_opentelemetry_dependencies};
use jv_pm::{LoggingConfig, LoggingFramework, OpenTelemetryConfig, OtelProtocol};

const OTEL_VERSION: &str = "1.32.0";
const OTEL_INSTRUMENTATION_VERSION: &str = "1.32.0-alpha";

#[test]
fn otel_disabled_produces_no_dependencies() {
    let mut config = LoggingConfig::default();
    config.framework = LoggingFramework::Slf4j;
    config.opentelemetry.enabled = false;

    let dependencies = collect_opentelemetry_dependencies(&config);
    assert!(dependencies.is_empty());
}

#[test]
fn slf4j_includes_logback_appender() {
    let config = enabled_config(LoggingFramework::Slf4j);
    let dependencies = collect_opentelemetry_dependencies(&config);

    let expected = vec![
        MavenDependency::new("io.opentelemetry", "opentelemetry-api", OTEL_VERSION),
        MavenDependency::new("io.opentelemetry", "opentelemetry-sdk", OTEL_VERSION),
        MavenDependency::new(
            "io.opentelemetry",
            "opentelemetry-exporter-otlp",
            OTEL_VERSION,
        ),
        MavenDependency::new(
            "io.opentelemetry.instrumentation",
            "opentelemetry-logback-appender-1.0",
            OTEL_INSTRUMENTATION_VERSION,
        ),
    ];

    assert_eq!(dependencies, expected);
}

#[test]
fn log4j2_includes_log4j_appender() {
    let config = enabled_config(LoggingFramework::Log4j2);
    let dependencies = collect_opentelemetry_dependencies(&config);

    let expected = vec![
        MavenDependency::new("io.opentelemetry", "opentelemetry-api", OTEL_VERSION),
        MavenDependency::new("io.opentelemetry", "opentelemetry-sdk", OTEL_VERSION),
        MavenDependency::new(
            "io.opentelemetry",
            "opentelemetry-exporter-otlp",
            OTEL_VERSION,
        ),
        MavenDependency::new(
            "io.opentelemetry.instrumentation",
            "opentelemetry-log4j-appender-2.17",
            OTEL_INSTRUMENTATION_VERSION,
        ),
    ];

    assert_eq!(dependencies, expected);
}

#[test]
fn jul_includes_java_util_appender() {
    let config = enabled_config(LoggingFramework::Jul);
    let dependencies = collect_opentelemetry_dependencies(&config);

    let expected = vec![
        MavenDependency::new("io.opentelemetry", "opentelemetry-api", OTEL_VERSION),
        MavenDependency::new("io.opentelemetry", "opentelemetry-sdk", OTEL_VERSION),
        MavenDependency::new(
            "io.opentelemetry",
            "opentelemetry-exporter-otlp",
            OTEL_VERSION,
        ),
        MavenDependency::new(
            "io.opentelemetry.instrumentation",
            "opentelemetry-logging-appender",
            OTEL_INSTRUMENTATION_VERSION,
        ),
    ];

    assert_eq!(dependencies, expected);
}

#[test]
fn custom_framework_uses_base_dependencies_only() {
    let config = enabled_config(LoggingFramework::Custom(Default::default()));
    let dependencies = collect_opentelemetry_dependencies(&config);

    let expected = vec![
        MavenDependency::new("io.opentelemetry", "opentelemetry-api", OTEL_VERSION),
        MavenDependency::new("io.opentelemetry", "opentelemetry-sdk", OTEL_VERSION),
        MavenDependency::new(
            "io.opentelemetry",
            "opentelemetry-exporter-otlp",
            OTEL_VERSION,
        ),
    ];

    assert_eq!(dependencies, expected);
}

fn enabled_config(framework: LoggingFramework) -> LoggingConfig {
    let mut config = LoggingConfig::default();
    config.framework = framework;
    config.opentelemetry = OpenTelemetryConfig {
        enabled: true,
        endpoint: Some("https://collector.example.com:4317".to_string()),
        protocol: OtelProtocol::Grpc,
        trace_context: true,
        resource: Default::default(),
        attributes: Default::default(),
    };
    config
}
