use std::collections::BTreeSet;

use jv_pm::{LoggingConfig, LoggingFramework};

/// Maven 依存関係の識別子。
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MavenDependency {
    pub group: &'static str,
    pub artifact: &'static str,
    pub version: &'static str,
}

impl MavenDependency {
    pub const fn new(group: &'static str, artifact: &'static str, version: &'static str) -> Self {
        Self {
            group,
            artifact,
            version,
        }
    }
}

const OTEL_VERSION: &str = "1.32.0";
const OTEL_INSTRUMENTATION_VERSION: &str = "1.32.0-alpha";

const BASE_DEPENDENCIES: &[MavenDependency] = &[
    MavenDependency::new("io.opentelemetry", "opentelemetry-api", OTEL_VERSION),
    MavenDependency::new("io.opentelemetry", "opentelemetry-sdk", OTEL_VERSION),
    MavenDependency::new("io.opentelemetry", "opentelemetry-exporter-otlp", OTEL_VERSION),
];

const LOGBACK_APPENDER: MavenDependency = MavenDependency::new(
    "io.opentelemetry.instrumentation",
    "opentelemetry-logback-appender-1.0",
    OTEL_INSTRUMENTATION_VERSION,
);

const LOG4J2_APPENDER: MavenDependency = MavenDependency::new(
    "io.opentelemetry.instrumentation",
    "opentelemetry-log4j-appender-2.17",
    OTEL_INSTRUMENTATION_VERSION,
);

const JUL_APPENDER: MavenDependency = MavenDependency::new(
    "io.opentelemetry.instrumentation",
    "opentelemetry-logging-appender",
    OTEL_INSTRUMENTATION_VERSION,
);

/// OpenTelemetry 統合に必要な Maven 依存関係を収集する。
///
/// OpenTelemetry が無効な場合は空のリストを返す。
pub fn collect_opentelemetry_dependencies(config: &LoggingConfig) -> Vec<MavenDependency> {
    if !config.opentelemetry.enabled {
        return Vec::new();
    }

    let mut seen = BTreeSet::new();
    let mut result = Vec::new();

    for dependency in BASE_DEPENDENCIES {
        push_unique(*dependency, &mut result, &mut seen);
    }

    match &config.framework {
        LoggingFramework::Slf4j => {
            push_unique(LOGBACK_APPENDER, &mut result, &mut seen);
        }
        LoggingFramework::Log4j2 => {
            push_unique(LOG4J2_APPENDER, &mut result, &mut seen);
        }
        LoggingFramework::Jul => {
            push_unique(JUL_APPENDER, &mut result, &mut seen);
        }
        LoggingFramework::JbossLogging
        | LoggingFramework::CommonsLogging
        | LoggingFramework::Custom(_) => {}
    }

    result
}

fn push_unique(
    dependency: MavenDependency,
    result: &mut Vec<MavenDependency>,
    seen: &mut BTreeSet<(&'static str, &'static str)>,
) {
    if seen.insert((dependency.group, dependency.artifact)) {
        result.push(dependency);
    }
}
