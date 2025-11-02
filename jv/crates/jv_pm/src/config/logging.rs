use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;
use std::str::FromStr;
use thiserror::Error;

/// ロギング設定の解析時に発生するエラー。
#[derive(Debug, Error, PartialEq, Eq)]
pub enum LoggingConfigError {
    #[error(
        "未対応のロギングフレームワーク: {invalid}. 利用可能候補: {choices}",
        choices = .available.join(", ")
    )]
    UnknownFramework {
        invalid: String,
        available: &'static [&'static str],
    },
    #[error(
        "未対応のログレベル: {invalid}. 利用可能候補: {choices}",
        choices = .available.join(", ")
    )]
    UnknownLogLevel {
        invalid: String,
        available: &'static [&'static str],
    },
    #[error(
        "未対応の OpenTelemetry プロトコル: {invalid}. 利用可能候補: {choices}",
        choices = .available.join(", ")
    )]
    UnknownProtocol {
        invalid: String,
        available: &'static [&'static str],
    },
}

/// ログレベル。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LogLevel {
    Trace,
    Debug,
    Info,
    Warn,
    Error,
}

impl LogLevel {
    pub const fn as_str(self) -> &'static str {
        match self {
            LogLevel::Trace => "TRACE",
            LogLevel::Debug => "DEBUG",
            LogLevel::Info => "INFO",
            LogLevel::Warn => "WARN",
            LogLevel::Error => "ERROR",
        }
    }

    pub const fn variants() -> &'static [&'static str] {
        &["TRACE", "DEBUG", "INFO", "WARN", "ERROR"]
    }
}

impl Default for LogLevel {
    fn default() -> Self {
        LogLevel::Info
    }
}

impl fmt::Display for LogLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl FromStr for LogLevel {
    type Err = LoggingConfigError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let normalised = s.trim().to_ascii_lowercase();
        match normalised.as_str() {
            "trace" => Ok(LogLevel::Trace),
            "debug" => Ok(LogLevel::Debug),
            "info" => Ok(LogLevel::Info),
            "warn" | "warning" => Ok(LogLevel::Warn),
            "error" => Ok(LogLevel::Error),
            other => Err(LoggingConfigError::UnknownLogLevel {
                invalid: other.to_string(),
                available: LogLevel::variants(),
            }),
        }
    }
}

impl Serialize for LogLevel {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(self.as_str())
    }
}

impl<'de> Deserialize<'de> for LogLevel {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        LogLevel::from_str(&value).map_err(serde::de::Error::custom)
    }
}

/// サポートするロギングフレームワーク。
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LoggingFramework {
    Slf4j,
    Log4j2,
    JbossLogging,
    CommonsLogging,
    Jul,
    Custom(CustomLoggingFramework),
}

impl LoggingFramework {
    pub const fn as_str(&self) -> &'static str {
        match self {
            LoggingFramework::Slf4j => "slf4j",
            LoggingFramework::Log4j2 => "log4j2",
            LoggingFramework::JbossLogging => "jboss-logging",
            LoggingFramework::CommonsLogging => "commons-logging",
            LoggingFramework::Jul => "jul",
            LoggingFramework::Custom(_) => "custom",
        }
    }

    pub const fn variants() -> &'static [&'static str] {
        &[
            "slf4j",
            "log4j2",
            "jboss-logging",
            "commons-logging",
            "jul",
            "custom",
        ]
    }
}

impl Default for LoggingFramework {
    fn default() -> Self {
        LoggingFramework::Slf4j
    }
}

impl fmt::Display for LoggingFramework {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoggingFramework::Custom(custom) => {
                if let Some(identifier) = &custom.identifier {
                    write!(f, "custom:{identifier}")
                } else {
                    f.write_str(self.as_str())
                }
            }
            _ => f.write_str(self.as_str()),
        }
    }
}

impl FromStr for LoggingFramework {
    type Err = LoggingConfigError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let trimmed = s.trim();
        let normalised = trimmed.to_ascii_lowercase();
        match normalised.as_str() {
            "slf4j" => Ok(LoggingFramework::Slf4j),
            "log4j2" => Ok(LoggingFramework::Log4j2),
            "jboss-logging" => Ok(LoggingFramework::JbossLogging),
            "commons-logging" => Ok(LoggingFramework::CommonsLogging),
            "jul" => Ok(LoggingFramework::Jul),
            "custom" => Ok(LoggingFramework::Custom(CustomLoggingFramework::default())),
            _ if normalised.starts_with("custom:") => {
                let identifier = trimmed[7..].trim();
                let identifier = if identifier.is_empty() {
                    None
                } else {
                    Some(identifier.to_string())
                };
                Ok(LoggingFramework::Custom(CustomLoggingFramework {
                    identifier,
                }))
            }
            other => Err(LoggingConfigError::UnknownFramework {
                invalid: other.to_string(),
                available: LoggingFramework::variants(),
            }),
        }
    }
}

impl Serialize for LoggingFramework {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for LoggingFramework {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        LoggingFramework::from_str(&value).map_err(serde::de::Error::custom)
    }
}

/// カスタムフレームワーク設定。
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct CustomLoggingFramework {
    /// 利用者が識別用に指定する任意の文字列。
    pub identifier: Option<String>,
}

/// OpenTelemetry の接続プロトコル。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OtelProtocol {
    Grpc,
    Http,
}

impl OtelProtocol {
    pub const fn as_str(self) -> &'static str {
        match self {
            OtelProtocol::Grpc => "grpc",
            OtelProtocol::Http => "http",
        }
    }

    pub const fn variants() -> &'static [&'static str] {
        &["grpc", "http"]
    }
}

impl Default for OtelProtocol {
    fn default() -> Self {
        OtelProtocol::Grpc
    }
}

impl fmt::Display for OtelProtocol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl FromStr for OtelProtocol {
    type Err = LoggingConfigError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let normalised = s.trim().to_ascii_lowercase();
        match normalised.as_str() {
            "grpc" => Ok(OtelProtocol::Grpc),
            "http" => Ok(OtelProtocol::Http),
            other => Err(LoggingConfigError::UnknownProtocol {
                invalid: other.to_string(),
                available: OtelProtocol::variants(),
            }),
        }
    }
}

impl Serialize for OtelProtocol {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(self.as_str())
    }
}

impl<'de> Deserialize<'de> for OtelProtocol {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        OtelProtocol::from_str(&value).map_err(serde::de::Error::custom)
    }
}

/// OpenTelemetry 関連設定。
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(default)]
pub struct OpenTelemetryConfig {
    pub enabled: bool,
    pub endpoint: Option<String>,
    pub protocol: OtelProtocol,
    pub trace_context: bool,
    #[serde(default)]
    pub resource: HashMap<String, String>,
    #[serde(default)]
    pub attributes: HashMap<String, String>,
}

impl Default for OpenTelemetryConfig {
    fn default() -> Self {
        Self {
            enabled: false,
            endpoint: None,
            protocol: OtelProtocol::Grpc,
            trace_context: false,
            resource: HashMap::new(),
            attributes: HashMap::new(),
        }
    }
}

impl OpenTelemetryConfig {
    fn apply_layer(&mut self, layer: &OpenTelemetryLayer) {
        if let Some(enabled) = layer.enabled {
            self.enabled = enabled;
        }
        if let Some(endpoint_override) = &layer.endpoint {
            self.endpoint = endpoint_override.clone();
        }
        if let Some(protocol) = layer.protocol {
            self.protocol = protocol;
        }
        if let Some(trace_context) = layer.trace_context {
            self.trace_context = trace_context;
        }
        if let Some(resource) = &layer.resource {
            self.resource = resource.clone();
        }
        if let Some(attributes) = &layer.attributes {
            self.attributes = attributes.clone();
        }
    }
}

/// OpenTelemetry の上書き値。
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct OpenTelemetryLayer {
    pub enabled: Option<bool>,
    pub endpoint: Option<Option<String>>,
    pub protocol: Option<OtelProtocol>,
    pub trace_context: Option<bool>,
    pub resource: Option<HashMap<String, String>>,
    pub attributes: Option<HashMap<String, String>>,
}

impl OpenTelemetryLayer {
    pub fn is_empty(&self) -> bool {
        self.enabled.is_none()
            && self.endpoint.is_none()
            && self.protocol.is_none()
            && self.trace_context.is_none()
            && self.resource.is_none()
            && self.attributes.is_none()
    }
}

/// ロギング設定本体。
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(default)]
pub struct LoggingConfig {
    pub framework: LoggingFramework,
    pub log_level: LogLevel,
    pub default_level: LogLevel,
    pub opentelemetry: OpenTelemetryConfig,
}

impl Default for LoggingConfig {
    fn default() -> Self {
        Self {
            framework: LoggingFramework::default(),
            log_level: LogLevel::default(),
            default_level: LogLevel::default(),
            opentelemetry: OpenTelemetryConfig::default(),
        }
    }
}

impl LoggingConfig {
    /// マニフェストに基づき設定を初期化する。
    pub fn from_manifest(manifest_value: Option<Self>) -> Self {
        manifest_value.unwrap_or_default()
    }

    /// 指定された優先順位で上書き層を適用する。
    pub fn with_layers(mut self, layers: &[LoggingConfigLayer]) -> Self {
        for layer in layers {
            self.apply_layer(layer);
        }
        self
    }

    fn apply_layer(&mut self, layer: &LoggingConfigLayer) {
        if let Some(framework) = layer.framework.clone() {
            self.framework = framework;
        }
        if let Some(log_level) = layer.log_level {
            self.log_level = log_level;
        }
        if let Some(default_level) = layer.default_level {
            self.default_level = default_level;
        }
        if let Some(otel_layer) = &layer.opentelemetry {
            if !otel_layer.is_empty() {
                self.opentelemetry.apply_layer(otel_layer);
            }
        }
    }
}

/// マニフェスト以外の環境変数・CLI 上書き値を保持する層。
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct LoggingConfigLayer {
    pub framework: Option<LoggingFramework>,
    pub log_level: Option<LogLevel>,
    pub default_level: Option<LogLevel>,
    pub opentelemetry: Option<OpenTelemetryLayer>,
}

impl LoggingConfigLayer {
    pub fn is_empty(&self) -> bool {
        self.framework.is_none()
            && self.log_level.is_none()
            && self.default_level.is_none()
            && self
                .opentelemetry
                .as_ref()
                .map_or(true, OpenTelemetryLayer::is_empty)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn log_level_from_str_is_case_insensitive() {
        assert_eq!(LogLevel::from_str("TRACE").unwrap(), LogLevel::Trace);
        assert_eq!(LogLevel::from_str("debug").unwrap(), LogLevel::Debug);
        assert_eq!(LogLevel::from_str("Info").unwrap(), LogLevel::Info);
        assert_eq!(LogLevel::from_str("warn").unwrap(), LogLevel::Warn);
        assert_eq!(LogLevel::from_str("warning").unwrap(), LogLevel::Warn);
        assert_eq!(LogLevel::from_str("ERROR").unwrap(), LogLevel::Error);
    }

    #[test]
    fn logging_framework_from_str_supports_custom_identifier() {
        assert_eq!(
            LoggingFramework::from_str("slf4j").unwrap(),
            LoggingFramework::Slf4j
        );
        assert_eq!(
            LoggingFramework::from_str("custom").unwrap(),
            LoggingFramework::Custom(CustomLoggingFramework::default())
        );
        assert_eq!(
            LoggingFramework::from_str("custom:com.example.CustomLogger").unwrap(),
            LoggingFramework::Custom(CustomLoggingFramework {
                identifier: Some("com.example.CustomLogger".to_string())
            })
        );
    }

    #[test]
    fn otel_protocol_parsing() {
        assert_eq!(OtelProtocol::from_str("grpc").unwrap(), OtelProtocol::Grpc);
        assert_eq!(OtelProtocol::from_str("HTTP").unwrap(), OtelProtocol::Http);
    }

    #[test]
    fn logging_config_defaults() {
        let config = LoggingConfig::default();
        assert_eq!(config.framework, LoggingFramework::Slf4j);
        assert_eq!(config.log_level, LogLevel::Info);
        assert_eq!(config.default_level, LogLevel::Info);
        assert_eq!(config.opentelemetry.protocol, OtelProtocol::Grpc);
        assert!(!config.opentelemetry.enabled);
    }

    #[test]
    fn logging_config_deserializes_with_defaults() {
        let toml = r#"
log_level = "warn"
[opentelemetry]
enabled = true
"#;
        let config: LoggingConfig = toml::from_str(toml).expect("toml 解析");
        assert_eq!(config.framework, LoggingFramework::Slf4j);
        assert_eq!(config.log_level, LogLevel::Warn);
        assert_eq!(config.default_level, LogLevel::Info);
        assert!(config.opentelemetry.enabled);
        assert_eq!(config.opentelemetry.protocol, OtelProtocol::Grpc);
    }

    #[test]
    fn merge_layers_respects_priority() {
        let manifest = LoggingConfig {
            framework: LoggingFramework::Slf4j,
            log_level: LogLevel::Info,
            default_level: LogLevel::Info,
            opentelemetry: OpenTelemetryConfig::default(),
        };

        let env_layer = LoggingConfigLayer {
            log_level: Some(LogLevel::Warn),
            opentelemetry: Some(OpenTelemetryLayer {
                enabled: Some(true),
                endpoint: Some(Some("http://env-collector".to_string())),
                protocol: Some(OtelProtocol::Http),
                trace_context: None,
                resource: None,
                attributes: None,
            }),
            ..LoggingConfigLayer::default()
        };

        let cli_layer = LoggingConfigLayer {
            framework: Some(LoggingFramework::Log4j2),
            log_level: Some(LogLevel::Debug),
            opentelemetry: Some(OpenTelemetryLayer {
                endpoint: Some(Some("http://cli-collector".to_string())),
                trace_context: Some(true),
                ..OpenTelemetryLayer::default()
            }),
            ..LoggingConfigLayer::default()
        };

        let merged = manifest.with_layers(&[env_layer, cli_layer]);
        assert_eq!(merged.framework, LoggingFramework::Log4j2);
        assert_eq!(merged.log_level, LogLevel::Debug);
        assert_eq!(merged.default_level, LogLevel::Info);
        assert!(merged.opentelemetry.enabled);
        assert_eq!(
            merged.opentelemetry.endpoint.as_deref(),
            Some("http://cli-collector")
        );
        assert_eq!(merged.opentelemetry.protocol, OtelProtocol::Http);
        assert!(merged.opentelemetry.trace_context);
    }

    #[test]
    fn open_telemetry_layer_can_clear_endpoint() {
        let manifest = LoggingConfig {
            opentelemetry: OpenTelemetryConfig {
                enabled: true,
                endpoint: Some("http://manifest".to_string()),
                protocol: OtelProtocol::Grpc,
                trace_context: false,
                resource: HashMap::new(),
                attributes: HashMap::new(),
            },
            ..LoggingConfig::default()
        };

        let cli_layer = LoggingConfigLayer {
            opentelemetry: Some(OpenTelemetryLayer {
                endpoint: Some(None),
                ..OpenTelemetryLayer::default()
            }),
            ..LoggingConfigLayer::default()
        };

        let merged = manifest.with_layers(&[cli_layer]);
        assert!(merged.opentelemetry.endpoint.is_none());
    }

    #[test]
    fn empty_layers_do_not_change_config() {
        let manifest = LoggingConfig::default();
        let empty_layer = LoggingConfigLayer::default();
        let merged = manifest.with_layers(&[empty_layer]);
        assert_eq!(merged, LoggingConfig::default());
    }
}
