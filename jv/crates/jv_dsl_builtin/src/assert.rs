use jv_dsl_api::{BlockPlugin, DslBlock, DslError, DslPlugin, TokenPlugin};
use serde_json::json;

#[derive(Clone)]
pub struct AssertPlugin;

impl DslPlugin for AssertPlugin {
    fn name(&self) -> &'static str {
        "assert"
    }

    fn registered_keywords(&self) -> &'static [&'static str] {
        &["ASSERT"]
    }

    fn feature_flag(&self) -> Option<&'static str> {
        Some("dsl-assert")
    }
}

impl TokenPlugin for AssertPlugin {}

impl BlockPlugin for AssertPlugin {
    fn keyword(&self) -> &'static str {
        "ASSERT"
    }

    fn max_nesting(&self) -> usize {
        4
    }

    fn parse_block_body(&self, body: &str) -> Result<DslBlock, DslError> {
        let mut checks = Vec::new();
        for line in body.lines().map(str::trim) {
            if line.is_empty() {
                continue;
            }
            checks.push(json!({ "expr": line }));
        }

        if checks.is_empty() {
            return Err(DslError::new("ASSERT block cannot be empty"));
        }

        Ok(DslBlock::new(
            self.keyword(),
            json!({ "checks": checks }),
            self.is_scope_transparent(),
            self.max_nesting(),
        ))
    }
}
