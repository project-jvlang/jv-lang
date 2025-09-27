use serde::{Deserialize, Serialize};

/// Budget thresholds applied when validating AST→IR performance.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PerfBudget {
    pub max_elapsed_ms: f64,
    pub max_peak_rss_mb: f64,
    pub min_reuse_ratio: f64,
}

impl PerfBudget {
    pub const fn new(max_elapsed_ms: f64, max_peak_rss_mb: f64, min_reuse_ratio: f64) -> Self {
        Self {
            max_elapsed_ms,
            max_peak_rss_mb,
            min_reuse_ratio,
        }
    }
}

/// Single profiling sample captured during a run.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct RunSample {
    pub iteration: usize,
    pub parse_ms: f64,
    pub lowering_ms: f64,
    pub total_ms: f64,
    pub warm_start: bool,
}

/// Aggregated summary derived from collected samples.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Summary {
    pub cold_total_ms: f64,
    pub warm_average_ms: f64,
    pub warm_min_ms: f64,
    pub warm_max_ms: f64,
    pub reuse_ratio: f64,
    pub peak_rss_mb: Option<f64>,
    pub sessions: u64,
    pub warm_sessions: u64,
}

/// Result of evaluating the captured metrics against budget thresholds.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct BudgetChecks {
    pub cold_within_budget: bool,
    pub warm_within_budget: bool,
    pub reuse_ratio_ok: bool,
    pub peak_rss_ok: Option<bool>,
}

/// Canonical JSON payload persisted by the AST→IR profiler.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PerfReport {
    pub report_version: u32,
    pub fixture: String,
    pub iterations: usize,
    pub runs: Vec<RunSample>,
    pub budget: PerfBudget,
    pub summary: Summary,
    pub checks: BudgetChecks,
    pub pass: bool,
    pub timestamp_ms: u128,
    pub report_path: String,
}
