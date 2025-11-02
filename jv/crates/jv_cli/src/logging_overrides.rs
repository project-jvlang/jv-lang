use std::str::FromStr;

use anyhow::Result;
use jv_pm::{LogLevel, LoggingConfigLayer, LoggingFramework, OpenTelemetryLayer, OtelProtocol};

/// CLI オプションからロギング設定の上書き層を構築する。
pub fn build_cli_logging_layer(
    framework: Option<&str>,
    log_level: Option<&str>,
    default_level: Option<&str>,
    otel_enabled: Option<&str>,
    otel_endpoint: Option<&str>,
    otel_protocol: Option<&str>,
    otel_trace_context: Option<&str>,
) -> Result<LoggingConfigLayer> {
    let mut layer = LoggingConfigLayer::default();

    if let Some(value) = framework {
        let trimmed = value.trim();
        if !trimmed.is_empty() {
            let parsed = LoggingFramework::from_str(trimmed).map_err(|error| {
                anyhow::anyhow!(
                    "--log-framework に無効な値 '{}' が指定されました: {}",
                    trimmed,
                    error
                )
            })?;
            layer.framework = Some(parsed);
        }
    }

    if let Some(value) = log_level {
        let trimmed = value.trim();
        if !trimmed.is_empty() {
            let parsed = LogLevel::from_str(trimmed).map_err(|error| {
                anyhow::anyhow!(
                    "--log-level に無効な値 '{}' が指定されました: {}",
                    trimmed,
                    error
                )
            })?;
            layer.log_level = Some(parsed);
        }
    }

    if let Some(value) = default_level {
        let trimmed = value.trim();
        if !trimmed.is_empty() {
            let parsed = LogLevel::from_str(trimmed).map_err(|error| {
                anyhow::anyhow!(
                    "--log-default-level に無効な値 '{}' が指定されました: {}",
                    trimmed,
                    error
                )
            })?;
            layer.default_level = Some(parsed);
        }
    }

    let mut otel_layer = OpenTelemetryLayer::default();
    let mut has_otel_override = false;

    if let Some(value) = otel_enabled {
        let trimmed = value.trim();
        if !trimmed.is_empty() {
            otel_layer.enabled = Some(parse_bool_flag(trimmed, "--otel-enabled")?);
            has_otel_override = true;
        }
    }

    if let Some(value) = otel_endpoint {
        let endpoint = if value.trim().is_empty() {
            None
        } else {
            Some(value.trim().to_string())
        };
        otel_layer.endpoint = Some(endpoint);
        has_otel_override = true;
    }

    if let Some(value) = otel_protocol {
        let trimmed = value.trim();
        if !trimmed.is_empty() {
            let parsed = OtelProtocol::from_str(trimmed).map_err(|error| {
                anyhow::anyhow!(
                    "--otel-protocol に無効な値 '{}' が指定されました: {}",
                    trimmed,
                    error
                )
            })?;
            otel_layer.protocol = Some(parsed);
            has_otel_override = true;
        }
    }

    if let Some(value) = otel_trace_context {
        let trimmed = value.trim();
        if !trimmed.is_empty() {
            otel_layer.trace_context = Some(parse_bool_flag(trimmed, "--otel-trace-context")?);
            has_otel_override = true;
        }
    }

    if has_otel_override {
        layer.opentelemetry = Some(otel_layer);
    }

    Ok(layer)
}

/// 環境変数からロギング設定の上書き層を構築する。
pub fn read_env_logging_layer() -> Result<LoggingConfigLayer> {
    let mut layer = LoggingConfigLayer::default();

    if let Ok(value) = std::env::var("JV_LOG_FRAMEWORK") {
        let trimmed = value.trim();
        if !trimmed.is_empty() {
            let parsed = LoggingFramework::from_str(trimmed).map_err(|error| {
                anyhow::anyhow!(
                    "環境変数 JV_LOG_FRAMEWORK の値 '{}' は無効です: {}",
                    trimmed,
                    error
                )
            })?;
            layer.framework = Some(parsed);
        }
    }

    if let Ok(value) = std::env::var("JV_LOG_LEVEL") {
        let trimmed = value.trim();
        if !trimmed.is_empty() {
            let parsed = LogLevel::from_str(trimmed).map_err(|error| {
                anyhow::anyhow!(
                    "環境変数 JV_LOG_LEVEL の値 '{}' は無効です: {}",
                    trimmed,
                    error
                )
            })?;
            layer.log_level = Some(parsed);
        }
    }

    if let Ok(value) = std::env::var("JV_LOG_DEFAULT_LEVEL") {
        let trimmed = value.trim();
        if !trimmed.is_empty() {
            let parsed = LogLevel::from_str(trimmed).map_err(|error| {
                anyhow::anyhow!(
                    "環境変数 JV_LOG_DEFAULT_LEVEL の値 '{}' は無効です: {}",
                    trimmed,
                    error
                )
            })?;
            layer.default_level = Some(parsed);
        }
    }

    let mut otel_layer = OpenTelemetryLayer::default();
    let mut has_otel_override = false;

    if let Ok(value) = std::env::var("JV_OTEL_ENABLED") {
        let trimmed = value.trim();
        if !trimmed.is_empty() {
            otel_layer.enabled = Some(parse_bool_flag(trimmed, "JV_OTEL_ENABLED")?);
            has_otel_override = true;
        }
    }

    if let Ok(value) = std::env::var("JV_OTEL_ENDPOINT") {
        let trimmed = value.trim();
        otel_layer.endpoint = Some(if trimmed.is_empty() {
            None
        } else {
            Some(trimmed.to_string())
        });
        has_otel_override = true;
    }

    if let Ok(value) = std::env::var("JV_OTEL_PROTOCOL") {
        let trimmed = value.trim();
        if !trimmed.is_empty() {
            let parsed = OtelProtocol::from_str(trimmed).map_err(|error| {
                anyhow::anyhow!(
                    "環境変数 JV_OTEL_PROTOCOL の値 '{}' は無効です: {}",
                    trimmed,
                    error
                )
            })?;
            otel_layer.protocol = Some(parsed);
            has_otel_override = true;
        }
    }

    if let Ok(value) = std::env::var("JV_OTEL_TRACE_CONTEXT") {
        let trimmed = value.trim();
        if !trimmed.is_empty() {
            otel_layer.trace_context = Some(parse_bool_flag(trimmed, "JV_OTEL_TRACE_CONTEXT")?);
            has_otel_override = true;
        }
    }

    if has_otel_override {
        layer.opentelemetry = Some(otel_layer);
    }

    Ok(layer)
}

fn parse_bool_flag(value: &str, context: &str) -> Result<bool> {
    let trimmed = value.trim();
    let normalized = trimmed.to_ascii_lowercase();
    match normalized.as_str() {
        "true" | "1" | "yes" | "on" => Ok(true),
        "false" | "0" | "no" | "off" => Ok(false),
        _ => anyhow::bail!(
            "{} には true/false のいずれかを指定してください (指定値: '{}')",
            context,
            trimmed
        ),
    }
}
