use jv_dsl_api::{BlockPlugin, DslBlock, DslError, DslPlugin, TokenPlugin};
use serde_json::json;

#[derive(Clone)]
pub struct LogPlugin;

const SUPPORTED_LEVELS: &[&str] = &["trace", "debug", "info", "warn", "error"];

impl DslPlugin for LogPlugin {
    fn name(&self) -> &'static str {
        "log"
    }

    fn registered_keywords(&self) -> &'static [&'static str] {
        &["LOG", "TRACE", "DEBUG", "INFO", "WARN", "ERROR"]
    }

    fn feature_flag(&self) -> Option<&'static str> {
        Some("dsl-log")
    }
}

impl TokenPlugin for LogPlugin {}

impl BlockPlugin for LogPlugin {
    fn keyword(&self) -> &'static str {
        "LOG"
    }

    fn parse_block_body(&self, body: &str) -> Result<DslBlock, DslError> {
        let mut statements = Vec::new();
        for raw in body.split(|c| c == '\n' || c == ',') {
            let stmt = raw.trim();
            if stmt.is_empty() {
                continue;
            }
            let (level, message) = parse_log_call(stmt)?;
            statements.push(json!({ "level": level, "message": message }));
        }

        Ok(DslBlock::new(
            self.keyword(),
            json!({ "statements": statements }),
            self.is_scope_transparent(),
            self.max_nesting(),
        ))
    }
}

fn parse_log_call(text: &str) -> Result<(&'static str, String), DslError> {
    let open = text
        .find('(')
        .ok_or_else(|| DslError::new("log call missing '('"))?;
    let close = text
        .rfind(')')
        .ok_or_else(|| DslError::new("log call missing ')'"))?;

    let level_raw = text[..open].trim().to_ascii_lowercase();
    let level = SUPPORTED_LEVELS
        .iter()
        .copied()
        .find(|l| *l == level_raw)
        .ok_or_else(|| DslError::new(format!("unsupported log level: {level_raw}")))?;

    let message = text[open + 1..close].trim().trim_matches('"').to_string();
    Ok((level, message))
}
