//! jv.toml のロギング設定を検証し、既定値を含む構造体へ変換するモジュール。
//!
//! # 概要
//! - `[logging]` セクションの許可されたキーをチェックし、余分な項目を早期に検出する。
//! - ロギングフレームワークやログレベルがサポート外の場合は、人間が読める診断を生成する。
//! - OpenTelemetry 設定が有効化された際に必須項目が欠落していないかを検証する。
//! - CLI や環境変数との優先順位表を公開し、今後の統合処理で再利用できるようにする。

use std::fmt;
use std::str::FromStr;

use jv_pm::{LogLevel, LoggingConfig, LoggingFramework, OtelProtocol};
use toml::Value;
use toml::value::Table;

/// `[logging]` セクションで許可されるトップレベルキー。
const LOGGING_KEYS: &[&str] = &["framework", "log_level", "default_level", "opentelemetry"];

/// `[logging.opentelemetry]` セクションで許可されるキー。
const OTEL_KEYS: &[&str] = &[
    "enabled",
    "endpoint",
    "protocol",
    "trace_context",
    "resource",
    "attributes",
];

/// ロギング設定の優先順位表 1 行を表す。
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoggingPriorityRow {
    /// 設定対象の論理名（例: `framework`）。
    pub item: &'static str,
    /// マニフェスト（`jv.toml`）で設定するキー。
    pub manifest_key: &'static str,
    /// 環境毎のマニフェスト（例: `jv-dev.toml`）で参照するキー。
    pub environment_key: &'static str,
    /// 環境変数の既定名。
    pub env_var: &'static str,
    /// CLI オプションの名称。
    pub cli_flag: &'static str,
}

/// ロギング設定に関する優先順位表。
pub const LOGGING_PRIORITY_TABLE: &[LoggingPriorityRow] = &[
    LoggingPriorityRow {
        item: "framework",
        manifest_key: "logging.framework",
        environment_key: "logging.framework",
        env_var: "JV_LOG_FRAMEWORK",
        cli_flag: "--log-framework",
    },
    LoggingPriorityRow {
        item: "log_level",
        manifest_key: "logging.log_level",
        environment_key: "logging.log_level",
        env_var: "JV_LOG_LEVEL",
        cli_flag: "--log-level",
    },
    LoggingPriorityRow {
        item: "default_level",
        manifest_key: "logging.default_level",
        environment_key: "logging.default_level",
        env_var: "JV_LOG_DEFAULT_LEVEL",
        cli_flag: "--log-default-level",
    },
    LoggingPriorityRow {
        item: "opentelemetry.enabled",
        manifest_key: "logging.opentelemetry.enabled",
        environment_key: "logging.opentelemetry.enabled",
        env_var: "JV_OTEL_ENABLED",
        cli_flag: "--otel-enabled",
    },
    LoggingPriorityRow {
        item: "opentelemetry.endpoint",
        manifest_key: "logging.opentelemetry.endpoint",
        environment_key: "logging.opentelemetry.endpoint",
        env_var: "JV_OTEL_ENDPOINT",
        cli_flag: "--otel-endpoint",
    },
    LoggingPriorityRow {
        item: "opentelemetry.protocol",
        manifest_key: "logging.opentelemetry.protocol",
        environment_key: "logging.opentelemetry.protocol",
        env_var: "JV_OTEL_PROTOCOL",
        cli_flag: "--otel-protocol",
    },
    LoggingPriorityRow {
        item: "opentelemetry.trace_context",
        manifest_key: "logging.opentelemetry.trace_context",
        environment_key: "logging.opentelemetry.trace_context",
        env_var: "JV_OTEL_TRACE_CONTEXT",
        cli_flag: "--otel-trace-context",
    },
];

/// ロギング設定読み込み中に発生し得るエラー。
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LoggingManifestError {
    UnknownKey {
        context: &'static str,
        key: String,
        allowed: &'static [&'static str],
    },
    InvalidType {
        field: &'static str,
        expected: &'static str,
    },
    InvalidValue {
        field: &'static str,
        value: String,
        detail: String,
    },
    InvalidMapValue {
        field: &'static str,
        key: String,
    },
    MissingOtelEndpoint,
    DeserializeError {
        detail: String,
    },
}

impl LoggingManifestError {
    fn unknown_key(
        context: &'static str,
        key: impl Into<String>,
        allowed: &'static [&'static str],
    ) -> Self {
        Self::UnknownKey {
            context,
            key: key.into(),
            allowed,
        }
    }

    fn invalid_type(field: &'static str, expected: &'static str) -> Self {
        Self::InvalidType { field, expected }
    }

    fn invalid_value(
        field: &'static str,
        value: impl Into<String>,
        detail: impl Into<String>,
    ) -> Self {
        Self::InvalidValue {
            field,
            value: value.into(),
            detail: detail.into(),
        }
    }

    fn invalid_map_value(field: &'static str, key: impl Into<String>) -> Self {
        Self::InvalidMapValue {
            field,
            key: key.into(),
        }
    }

    fn deserialize(detail: impl Into<String>) -> Self {
        Self::DeserializeError {
            detail: detail.into(),
        }
    }
}

impl fmt::Display for LoggingManifestError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoggingManifestError::UnknownKey {
                context,
                key,
                allowed,
            } => {
                let joined = allowed.join(", ");
                write!(
                    f,
                    "[{}] に未知のキー '{}' が含まれています。利用可能なキー: {}",
                    context, key, joined
                )
            }
            LoggingManifestError::InvalidType { field, expected } => {
                write!(f, "[{}] には {} を指定してください。", field, expected)
            }
            LoggingManifestError::InvalidValue {
                field,
                value,
                detail,
            } => {
                write!(f, "[{}] の値 '{}' は無効です: {}", field, value, detail)
            }
            LoggingManifestError::InvalidMapValue { field, key } => write!(
                f,
                "[{}] のキー '{}' には文字列値のみを指定してください。",
                field, key
            ),
            LoggingManifestError::MissingOtelEndpoint => write!(
                f,
                "[logging.opentelemetry] を有効にした場合は endpoint を指定してください。"
            ),
            LoggingManifestError::DeserializeError { detail } => {
                write!(f, "ロギング設定の読み込みに失敗しました: {}", detail)
            }
        }
    }
}

impl std::error::Error for LoggingManifestError {}

/// マニフェスト全体の `toml::Value` からロギング設定を解析する。
pub fn parse_logging_config(value: &Value) -> Result<LoggingConfig, LoggingManifestError> {
    let table = value
        .as_table()
        .ok_or_else(|| LoggingManifestError::invalid_type("root", "テーブル"))?;
    parse_logging_config_from_table(table)
}

/// マニフェストのトップレベルテーブルからロギング設定を解析する。
pub fn parse_logging_config_from_table(
    table: &Table,
) -> Result<LoggingConfig, LoggingManifestError> {
    match table.get("logging") {
        None => Ok(LoggingConfig::default()),
        Some(Value::Table(logging_table)) => parse_logging_table(logging_table),
        Some(_) => Err(LoggingManifestError::invalid_type("logging", "テーブル")),
    }
}

fn parse_logging_table(table: &Table) -> Result<LoggingConfig, LoggingManifestError> {
    ensure_allowed_keys(table, LOGGING_KEYS, "logging")?;
    validate_logging_items(table)?;

    let mut normalized = table.clone();
    normalize_logging_table(&mut normalized)?;

    let value = Value::Table(normalized);
    let config: LoggingConfig = value
        .try_into()
        .map_err(|err| LoggingManifestError::deserialize(err.to_string()))?;

    if config.opentelemetry.enabled {
        let endpoint_missing = config
            .opentelemetry
            .endpoint
            .as_ref()
            .map(|s| s.trim().is_empty())
            .unwrap_or(true);
        if endpoint_missing {
            return Err(LoggingManifestError::MissingOtelEndpoint);
        }
    }

    Ok(config)
}

fn normalize_logging_table(table: &mut Table) -> Result<(), LoggingManifestError> {
    if let Some(Value::Table(otel)) = table.get_mut("opentelemetry") {
        if let Some(value) = otel.get_mut("resource") {
            flatten_string_map_value(value, "logging.opentelemetry.resource")?;
        }
        if let Some(value) = otel.get_mut("attributes") {
            flatten_string_map_value(value, "logging.opentelemetry.attributes")?;
        }
    }
    Ok(())
}

fn validate_logging_items(table: &Table) -> Result<(), LoggingManifestError> {
    if let Some(value) = table.get("framework") {
        let Some(framework) = value.as_str() else {
            return Err(LoggingManifestError::invalid_type(
                "logging.framework",
                "文字列",
            ));
        };
        LoggingFramework::from_str(framework).map_err(|error| {
            LoggingManifestError::invalid_value("logging.framework", framework, error.to_string())
        })?;
    }

    if let Some(value) = table.get("log_level") {
        let Some(level) = value.as_str() else {
            return Err(LoggingManifestError::invalid_type(
                "logging.log_level",
                "文字列",
            ));
        };
        LogLevel::from_str(level).map_err(|error| {
            LoggingManifestError::invalid_value("logging.log_level", level, error.to_string())
        })?;
    }

    if let Some(value) = table.get("default_level") {
        let Some(level) = value.as_str() else {
            return Err(LoggingManifestError::invalid_type(
                "logging.default_level",
                "文字列",
            ));
        };
        LogLevel::from_str(level).map_err(|error| {
            LoggingManifestError::invalid_value("logging.default_level", level, error.to_string())
        })?;
    }

    if let Some(otel) = table.get("opentelemetry") {
        let otel_table = otel.as_table().ok_or_else(|| {
            LoggingManifestError::invalid_type("logging.opentelemetry", "テーブル")
        })?;
        ensure_allowed_keys(otel_table, OTEL_KEYS, "logging.opentelemetry")?;
        validate_otel_items(otel_table)?;
    }

    Ok(())
}

fn validate_otel_items(table: &Table) -> Result<(), LoggingManifestError> {
    if let Some(value) = table.get("enabled") {
        if !value.is_bool() {
            return Err(LoggingManifestError::invalid_type(
                "logging.opentelemetry.enabled",
                "真偽値",
            ));
        }
    }

    if let Some(value) = table.get("endpoint") {
        if !value.is_str() {
            return Err(LoggingManifestError::invalid_type(
                "logging.opentelemetry.endpoint",
                "文字列",
            ));
        }
    }

    if let Some(value) = table.get("protocol") {
        let Some(protocol) = value.as_str() else {
            return Err(LoggingManifestError::invalid_type(
                "logging.opentelemetry.protocol",
                "文字列",
            ));
        };
        OtelProtocol::from_str(protocol).map_err(|error| {
            LoggingManifestError::invalid_value(
                "logging.opentelemetry.protocol",
                protocol,
                error.to_string(),
            )
        })?;
    }

    if let Some(value) = table.get("trace_context") {
        if !value.is_bool() {
            return Err(LoggingManifestError::invalid_type(
                "logging.opentelemetry.trace_context",
                "真偽値",
            ));
        }
    }

    if let Some(resource) = table.get("resource") {
        let resource_table = resource.as_table().ok_or_else(|| {
            LoggingManifestError::invalid_type(
                "logging.opentelemetry.resource",
                "文字列値のテーブル",
            )
        })?;
        ensure_string_map(resource_table, "logging.opentelemetry.resource")?;
    }

    if let Some(attributes) = table.get("attributes") {
        let attr_table = attributes.as_table().ok_or_else(|| {
            LoggingManifestError::invalid_type(
                "logging.opentelemetry.attributes",
                "文字列値のテーブル",
            )
        })?;
        ensure_string_map(attr_table, "logging.opentelemetry.attributes")?;
    }

    Ok(())
}

fn ensure_allowed_keys(
    table: &Table,
    allowed: &'static [&'static str],
    context: &'static str,
) -> Result<(), LoggingManifestError> {
    for key in table.keys() {
        if !allowed.contains(&key.as_str()) {
            return Err(LoggingManifestError::unknown_key(context, key, allowed));
        }
    }
    Ok(())
}

fn ensure_string_map(table: &Table, field: &'static str) -> Result<(), LoggingManifestError> {
    for (key, value) in table {
        match value {
            Value::String(_) => {}
            Value::Table(nested) => ensure_string_map(nested, field)?,
            _ => {
                return Err(LoggingManifestError::invalid_map_value(field, key));
            }
        }
    }
    Ok(())
}

fn flatten_string_map_value(
    value: &mut Value,
    field: &'static str,
) -> Result<(), LoggingManifestError> {
    let table = value
        .as_table()
        .ok_or_else(|| LoggingManifestError::invalid_type(field, "テーブル"))?
        .clone();

    let mut flattened = Table::new();
    flatten_string_map_recursive(&table, "", &mut flattened, field)?;
    *value = Value::Table(flattened);
    Ok(())
}

fn flatten_string_map_recursive(
    table: &Table,
    prefix: &str,
    out: &mut Table,
    field: &'static str,
) -> Result<(), LoggingManifestError> {
    for (key, value) in table {
        let full_key = if prefix.is_empty() {
            key.clone()
        } else {
            format!("{prefix}.{key}")
        };

        match value {
            Value::String(s) => {
                out.insert(full_key, Value::String(s.clone()));
            }
            Value::Table(nested) => {
                flatten_string_map_recursive(nested, &full_key, out, field)?;
            }
            _ => {
                return Err(LoggingManifestError::invalid_map_value(field, full_key));
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(value: &str) -> Result<LoggingConfig, LoggingManifestError> {
        let root: Value = value.parse().expect("toml");
        parse_logging_config(&root)
    }

    #[test]
    fn logging_section_absent_returns_default() {
        let config = parse(
            r#"[package]
name = "sample"
version = "0.1.0"
"#,
        )
        .expect("既定値");
        assert_eq!(config.framework, LoggingFramework::Slf4j);
        assert_eq!(config.log_level, LogLevel::Info);
    }

    #[test]
    fn logging_unknown_key_reports_error() {
        let root: Value = r#"[logging]
framework = "slf4j"
unsupported = true
"#
        .parse()
        .expect("toml");
        let error = parse_logging_config(&root).expect_err("未知キー");
        assert!(matches!(
            error,
            LoggingManifestError::UnknownKey {
                context: "logging",
                ..
            }
        ));
    }

    #[test]
    fn logging_invalid_log_level_reports_error() {
        let root: Value = r#"[logging]
log_level = "verbose"
"#
        .parse()
        .expect("toml");
        let error = parse_logging_config(&root).expect_err("log_level");
        assert!(matches!(
            error,
            LoggingManifestError::InvalidValue {
                field: "logging.log_level",
                ..
            }
        ));
    }

    #[test]
    fn logging_opentelemetry_missing_endpoint_is_error() {
        let root: Value = r#"[logging.opentelemetry]
enabled = true
"#
        .parse()
        .expect("toml");
        let error = parse_logging_config(&root).expect_err("endpoint 欠落");
        assert!(matches!(error, LoggingManifestError::MissingOtelEndpoint));
    }

    #[test]
    fn logging_opentelemetry_unknown_key_reports_error() {
        let root: Value = r#"[logging.opentelemetry]
enabled = false
extra = "x"
"#
        .parse()
        .expect("toml");
        let error = parse_logging_config(&root).expect_err("未知キー");
        assert!(matches!(
            error,
            LoggingManifestError::UnknownKey {
                context: "logging.opentelemetry",
                ..
            }
        ));
    }

    #[test]
    fn logging_valid_configuration_is_accepted() {
        let config = parse(
            r#"[logging]
framework = "log4j2"
log_level = "warn"
default_level = "info"

[logging.opentelemetry]
enabled = true
endpoint = "http://localhost:4317"
protocol = "grpc"
trace_context = true

[logging.opentelemetry.resource]
service.name = "example"
"#,
        )
        .expect("設定");

        assert_eq!(config.framework, LoggingFramework::Log4j2);
        assert_eq!(config.log_level, LogLevel::Warn);
        assert!(config.opentelemetry.enabled);
        assert_eq!(
            config.opentelemetry.endpoint.as_deref(),
            Some("http://localhost:4317")
        );
    }
}
