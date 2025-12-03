use jv_dsl_api::{BlockPlugin, DslBlock, DslError, DslPlugin};
use serde_json::json;

#[derive(Clone)]
pub struct IoPlugin;

impl DslPlugin for IoPlugin {
    fn name(&self) -> &'static str {
        "io"
    }

    fn registered_keywords(&self) -> &'static [&'static str] {
        &["IO", "IO.FILE", "IO.STD"]
    }

    fn feature_flag(&self) -> Option<&'static str> {
        Some("dsl-io")
    }
}

impl BlockPlugin for IoPlugin {
    fn keyword(&self) -> &'static str {
        "IO"
    }

    fn parse_block_body(&self, body: &str) -> Result<DslBlock, DslError> {
        let mut operations = Vec::new();
        for raw in body.split(|c| c == '\n' || c == ',') {
            let stmt = raw.trim();
            if stmt.is_empty() {
                continue;
            }
            let (op, arg) = parse_io_call(stmt)?;
            operations.push(json!({ "op": op, "arg": arg }));
        }

        Ok(DslBlock::new(
            self.keyword(),
            json!({ "operations": operations }),
            self.is_scope_transparent(),
            self.max_nesting(),
        ))
    }
}

fn parse_io_call(text: &str) -> Result<(String, String), DslError> {
    let open = text
        .find('(')
        .ok_or_else(|| DslError::new("IO call missing '('"))?;
    let close = text
        .rfind(')')
        .ok_or_else(|| DslError::new("IO call missing ')'"))?;

    let op = text[..open]
        .trim()
        .trim_start_matches("IO.")
        .trim_start_matches("io.")
        .trim()
        .to_ascii_lowercase();
    if op.is_empty() {
        return Err(DslError::new("IO operation name is empty"));
    }

    let arg = text[open + 1..close].trim().trim_matches('"').to_string();
    Ok((op, arg))
}
