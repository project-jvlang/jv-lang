use jv_dsl_api::{
    Associativity, BlockPlugin, DslBlock, DslError, DslPlugin, GlobalOperator, GlobalOperatorPlugin,
};
use serde_json::json;

#[derive(Clone)]
pub struct CronPlugin;

impl DslPlugin for CronPlugin {
    fn name(&self) -> &'static str {
        "cron"
    }

    fn registered_keywords(&self) -> &'static [&'static str] {
        &["CRON"]
    }

    fn feature_flag(&self) -> Option<&'static str> {
        Some("dsl-cron")
    }
}

impl BlockPlugin for CronPlugin {
    fn keyword(&self) -> &'static str {
        "CRON"
    }

    fn parse_block_body(&self, body: &str) -> Result<DslBlock, DslError> {
        let mut lines = body.lines().map(str::trim).filter(|l| !l.is_empty());
        let schedule_line = lines
            .next()
            .ok_or_else(|| DslError::new("CRON block requires schedule"))?;

        let schedule = extract_schedule(schedule_line)?;
        let tasks: Vec<_> = lines.map(|line| json!({ "task": line })).collect();

        Ok(DslBlock::new(
            self.keyword(),
            json!({ "schedule": schedule, "tasks": tasks }),
            self.is_scope_transparent(),
            self.max_nesting(),
        ))
    }
}

impl GlobalOperatorPlugin for CronPlugin {
    fn register_operators(&self) -> &'static [GlobalOperator] {
        const OPS: &[GlobalOperator] = &[GlobalOperator {
            symbol: "EVERY",
            precedence: 15,
            associativity: Associativity::Left,
        }];
        OPS
    }
}

fn extract_schedule(line: &str) -> Result<String, DslError> {
    if let Some(start) = line.find('"') {
        if let Some(end) = line[start + 1..].find('"') {
            let end_idx = start + 1 + end;
            return Ok(line[start + 1..end_idx].to_string());
        }
    }
    Ok(line.to_string())
}
