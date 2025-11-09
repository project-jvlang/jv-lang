use std::collections::HashSet;

use crate::{CompletionItemData, CompletionKind, Position};

const LOG_SNIPPETS: &[LogSnippet] = &[
    LogSnippet {
        keyword: "LOG",
        label: r#"LOG { "メッセージ" }"#,
        snippet: "LOG {\n    \"$0\"\n}",
        detail: "defaultレベルのログブロック",
    },
    LogSnippet {
        keyword: "TRACE",
        label: r#"TRACE { "メッセージ" }"#,
        snippet: "TRACE {\n    \"$0\"\n}",
        detail: "TRACEレベルのログブロック",
    },
    LogSnippet {
        keyword: "DEBUG",
        label: r#"DEBUG { "メッセージ" }"#,
        snippet: "DEBUG {\n    \"$0\"\n}",
        detail: "DEBUGレベルのログブロック",
    },
    LogSnippet {
        keyword: "INFO",
        label: r#"INFO { "メッセージ" }"#,
        snippet: "INFO {\n    \"$0\"\n}",
        detail: "INFOレベルのログブロック",
    },
    LogSnippet {
        keyword: "WARN",
        label: r#"WARN { "メッセージ" }"#,
        snippet: "WARN {\n    \"$0\"\n}",
        detail: "WARNレベルのログブロック",
    },
    LogSnippet {
        keyword: "ERROR",
        label: r#"ERROR { "メッセージ" }"#,
        snippet: "ERROR {\n    \"$0\"\n}",
        detail: "ERRORレベルのログブロック",
    },
];

const LOGGING_TOP_LEVEL_KEYS: &[&str] =
    &["framework", "log_level", "default_level", "opentelemetry"];
const LOGGING_OTEL_KEYS: &[&str] = &[
    "enabled",
    "endpoint",
    "protocol",
    "trace_context",
    "resource",
    "attributes",
];
const LOGGING_FRAMEWORK_VALUES: &[&str] = &[
    "slf4j",
    "log4j2",
    "jboss-logging",
    "commons-logging",
    "jul",
    "custom",
];
const LOGGING_LEVEL_VALUES: &[&str] = &["TRACE", "DEBUG", "INFO", "WARN", "ERROR"];
const LOGGING_PROTOCOL_VALUES: &[&str] = &["grpc", "http"];
const BOOLEAN_VALUES: &[&str] = &["true", "false"];

pub fn log_block_snippet_completions(
    content: &str,
    position: &Position,
) -> Vec<CompletionItemData> {
    let mut snippets: Vec<&LogSnippet> = Vec::new();

    if let Some((_, _, word)) = crate::word_at_position(content, position) {
        let upper = word.to_ascii_uppercase();
        for snippet in LOG_SNIPPETS {
            if snippet.keyword.starts_with(&upper) {
                snippets.push(snippet);
            }
        }
    }

    if snippets.is_empty() {
        snippets.extend(LOG_SNIPPETS.iter());
    }

    snippets
        .into_iter()
        .map(|snippet| {
            CompletionItemData::snippet(snippet.label, snippet.snippet)
                .with_detail(snippet.detail)
                .with_kind(CompletionKind::Snippet)
        })
        .collect()
}

pub fn manifest_logging_completions(content: &str, position: &Position) -> Vec<CompletionItemData> {
    let Some(section) = current_section(content, position.line as usize) else {
        return Vec::new();
    };

    if !section.starts_with("logging") {
        return Vec::new();
    }

    let line = current_line_prefix(content, position);
    let existing_keys = gather_section_keys(content, section.as_str());
    let prefix = line.trim_start();
    let mut completions = Vec::new();

    if let Some(eq_index) = prefix.find('=') {
        let key = prefix[..eq_index].trim();
        let value_prefix = prefix[eq_index + 1..].trim_start_matches(|c: char| c.is_whitespace());
        completions.extend(value_completions_for(section.as_str(), key, value_prefix));
    } else {
        let candidates = if section == "logging" {
            LOGGING_TOP_LEVEL_KEYS
                .iter()
                .filter(|key| !existing_keys.contains(&key.to_string()))
                .copied()
                .collect::<Vec<_>>()
        } else if section == "logging.opentelemetry" {
            LOGGING_OTEL_KEYS
                .iter()
                .filter(|key| !existing_keys.contains(&key.to_string()))
                .copied()
                .collect::<Vec<_>>()
        } else {
            Vec::new()
        };

        for candidate in candidates {
            let label = candidate.to_string();
            if label.starts_with(prefix) || prefix.is_empty() {
                completions.push(
                    CompletionItemData::keyword(label.clone())
                        .with_kind(CompletionKind::Field)
                        .with_documentation(format!("`{}` セクションのキー候補です。", section)),
                );
            }
        }
    }

    completions
}

fn current_line_prefix(content: &str, position: &Position) -> String {
    let line_index = position.line as usize;
    let lines: Vec<&str> = content.lines().collect();
    if line_index >= lines.len() {
        return String::new();
    }

    let line = lines[line_index];
    let char_index = position.character as usize;
    if char_index >= line.len() {
        line.to_string()
    } else {
        line[..char_index].to_string()
    }
}

fn current_section(content: &str, line_index: usize) -> Option<String> {
    let mut section = None;
    for (idx, raw_line) in content.lines().enumerate() {
        if idx > line_index {
            break;
        }
        let line = raw_line.trim();
        if line.starts_with('#') || line.is_empty() {
            continue;
        }
        if let Some(name) = parse_section_header(line) {
            section = Some(name);
        }
    }
    section
}

fn parse_section_header(line: &str) -> Option<String> {
    if !line.starts_with('[') || !line.ends_with(']') {
        return None;
    }
    if line.starts_with("[[") {
        return None;
    }
    let name = line.trim_matches(['[', ']']);
    if name.is_empty() {
        None
    } else {
        Some(name.to_string())
    }
}

fn gather_section_keys(content: &str, section: &str) -> HashSet<String> {
    let mut current = None;
    let mut keys = HashSet::new();
    for raw_line in content.lines() {
        let trimmed = raw_line.trim();
        if trimmed.starts_with('#') || trimmed.is_empty() {
            continue;
        }
        if let Some(name) = parse_section_header(trimmed) {
            current = Some(name);
            continue;
        }
        if current.as_deref() != Some(section) {
            continue;
        }
        if let Some(eq_pos) = trimmed.find('=') {
            let key = trimmed[..eq_pos].trim();
            if !key.is_empty() {
                keys.insert(key.to_string());
            }
        }
    }
    keys
}

fn value_completions_for(section: &str, key: &str, value_prefix: &str) -> Vec<CompletionItemData> {
    let mut completions = Vec::new();
    let mut normalized = value_prefix.trim_start_matches(|c: char| c.is_whitespace());
    normalized = normalized.trim_start_matches(|c| c == '"' || c == '\'');
    match (section, key) {
        ("logging", "framework") => {
            completions.extend(build_value_items(LOGGING_FRAMEWORK_VALUES, normalized));
        }
        ("logging", "log_level") | ("logging", "default_level") => {
            completions.extend(build_value_items(LOGGING_LEVEL_VALUES, normalized));
        }
        ("logging", "opentelemetry") => {
            completions.push(
                CompletionItemData::keyword("opentelemetry".to_string())
                    .with_kind(CompletionKind::Field)
                    .with_documentation("サブテーブル `[logging.opentelemetry]` を定義します。"),
            );
        }
        ("logging.opentelemetry", "enabled") | ("logging.opentelemetry", "trace_context") => {
            completions.extend(build_value_items(BOOLEAN_VALUES, normalized));
        }
        ("logging.opentelemetry", "protocol") => {
            completions.extend(build_value_items(LOGGING_PROTOCOL_VALUES, normalized));
        }
        ("logging.opentelemetry", "endpoint") => {
            let snippet = CompletionItemData::snippet(
                r#"endpoint = "http://localhost:4317""#,
                r#"endpoint = "${1:http://localhost:4317}""#,
            )
            .with_documentation("OTLP Collector のエンドポイントURLを指定します。")
            .with_kind(CompletionKind::Value);
            if snippet.label.starts_with(normalized) || normalized.is_empty() {
                completions.push(snippet);
            }
        }
        ("logging.opentelemetry", "resource") => {
            let snippet = CompletionItemData::snippet(
                r#"resource = { "service.name" = "sample" }"#,
                r#"resource = { "service.name" = "${1:demo-service}" }"#,
            )
            .with_documentation("OTLP リソース属性を定義します。")
            .with_kind(CompletionKind::Value);
            if snippet.label.starts_with(normalized) || normalized.is_empty() {
                completions.push(snippet);
            }
        }
        ("logging.opentelemetry", "attributes") => {
            let snippet = CompletionItemData::snippet(
                r#"attributes = { "env" = "dev" }"#,
                r#"attributes = { "env" = "${1:dev}" }"#,
            )
            .with_documentation("ログに付与するカスタム属性を定義します。")
            .with_kind(CompletionKind::Value);
            if snippet.label.starts_with(normalized) || normalized.is_empty() {
                completions.push(snippet);
            }
        }
        _ => {}
    }
    completions
}

fn build_value_items(candidates: &[&str], prefix: &str) -> Vec<CompletionItemData> {
    candidates
        .iter()
        .filter(|candidate| candidate.starts_with(prefix) || prefix.is_empty())
        .map(|candidate| {
            CompletionItemData::keyword(candidate.to_string())
                .with_kind(CompletionKind::Value)
                .with_documentation("候補値を選択してください。")
        })
        .collect()
}

struct LogSnippet {
    keyword: &'static str,
    label: &'static str,
    snippet: &'static str,
    detail: &'static str,
}
