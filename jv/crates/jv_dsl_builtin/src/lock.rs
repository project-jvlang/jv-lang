use jv_dsl_api::{BlockPlugin, DslBlock, DslError, DslPlugin};
use serde_json::json;

#[derive(Clone)]
pub struct LockPlugin;

impl DslPlugin for LockPlugin {
    fn name(&self) -> &'static str {
        "lock"
    }

    fn registered_keywords(&self) -> &'static [&'static str] {
        &["LOCK"]
    }

    fn feature_flag(&self) -> Option<&'static str> {
        Some("dsl-lock")
    }
}

impl BlockPlugin for LockPlugin {
    fn keyword(&self) -> &'static str {
        "LOCK"
    }

    fn is_scope_transparent(&self) -> bool {
        false
    }

    fn parse_block_body(&self, body: &str) -> Result<DslBlock, DslError> {
        let steps: Vec<_> = body
            .lines()
            .map(str::trim)
            .filter(|line| !line.is_empty())
            .map(|line| json!({ "action": line }))
            .collect();

        if steps.is_empty() {
            return Err(DslError::new("LOCK block must contain at least one action"));
        }

        Ok(DslBlock::new(
            self.keyword(),
            json!({ "steps": steps }),
            self.is_scope_transparent(),
            self.max_nesting(),
        ))
    }
}
