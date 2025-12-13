use crate::artifacts::logging::generate_logging_artifacts;
use jv_pm::{LogLevel, LoggingConfig, LoggingFramework, OpenTelemetryConfig, OtelProtocol};

fn base_logging_config() -> LoggingConfig {
    LoggingConfig {
        log_level: LogLevel::Debug,
        default_level: LogLevel::Info,
        opentelemetry: OpenTelemetryConfig {
            enabled: true,
            endpoint: Some("https://collector.example.com:4317".to_string()),
            protocol: OtelProtocol::Grpc,
            trace_context: true,
            resource: [("service.name".to_string(), "order-service".to_string())]
                .into_iter()
                .collect(),
            attributes: [
                ("deployment.environment".to_string(), "staging".to_string()),
                ("team".to_string(), "observability".to_string()),
            ]
            .into_iter()
            .collect(),
        },
        ..LoggingConfig::default()
    }
}

#[test]
fn logback_template_snapshot() {
    let mut config = base_logging_config();
    config.framework = LoggingFramework::Slf4j;

    let artifacts = generate_logging_artifacts(&config);
    assert_eq!(artifacts.len(), 1);

    insta::assert_snapshot!("logback_template", &artifacts[0].contents);
    assert_eq!(artifacts[0].filename, "logback.xml");
}

#[test]
fn log4j2_template_snapshot() {
    let mut config = base_logging_config();
    config.framework = LoggingFramework::Log4j2;

    let artifacts = generate_logging_artifacts(&config);
    assert_eq!(artifacts.len(), 1);

    insta::assert_snapshot!("log4j2_template", &artifacts[0].contents);
    assert_eq!(artifacts[0].filename, "log4j2.xml");
}

#[test]
fn jul_template_snapshot() {
    let mut config = base_logging_config();
    config.framework = LoggingFramework::Jul;
    config.opentelemetry.protocol = OtelProtocol::Http;

    let artifacts = generate_logging_artifacts(&config);
    assert_eq!(artifacts.len(), 1);

    insta::assert_snapshot!("jul_template", &artifacts[0].contents);
    assert_eq!(artifacts[0].filename, "logging.properties");
}

#[test]
fn disabled_opentelemetry_produces_no_artifacts() {
    let config = LoggingConfig {
        framework: LoggingFramework::Slf4j,
        opentelemetry: OpenTelemetryConfig {
            enabled: false,
            ..OpenTelemetryConfig::default()
        },
        ..LoggingConfig::default()
    };

    let artifacts = generate_logging_artifacts(&config);
    assert!(artifacts.is_empty());
}
