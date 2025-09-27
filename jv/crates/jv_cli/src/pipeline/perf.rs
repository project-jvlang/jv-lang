use std::fs;
use std::path::{Path, PathBuf};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use anyhow::{Context, Result};
use jv_ir::PerfMetrics;
use jv_support::perf::report::{BudgetChecks, PerfBudget, PerfReport, RunSample, Summary};

pub const PERF_REPORT_FILENAME: &str = "ast-ir-phase1.json";
const PERF_REPORT_VERSION: u32 = 1;
const MAX_ELAPSED_MS: f64 = 3_000.0;
const MAX_PEAK_RSS_MB: f64 = 100.0;
const MIN_REUSE_RATIO: f64 = 0.90;

#[derive(Debug, Clone)]
pub struct PerfCapture {
    pub report: PerfReport,
    pub report_path: PathBuf,
}

pub fn persist_single_run_report(
    workspace_root: &Path,
    entrypoint: &Path,
    parse_duration: Duration,
    metrics: &PerfMetrics,
) -> Result<PerfCapture> {
    let report_dir = workspace_root.join("target/perf-reports");
    fs::create_dir_all(&report_dir).with_context(|| {
        format!(
            "Failed to create perf report directory: {}",
            report_dir.display()
        )
    })?;

    let report_path = report_dir.join(PERF_REPORT_FILENAME);

    let parse_ms = parse_duration.as_secs_f64() * 1_000.0;
    let lowering_ms = metrics
        .stage("lowering")
        .map(|stage| stage.elapsed_millis())
        .unwrap_or_else(|| metrics.total_millis());
    let total_ms = parse_ms + lowering_ms;
    let warm_start = metrics.was_warm_start().unwrap_or(false);

    let reuse_ratio = metrics.reuse_ratio().unwrap_or(0.0);
    let peak_rss_mb = metrics
        .peak_rss_bytes()
        .map(|bytes| bytes as f64 / (1024.0 * 1024.0));

    let (sessions, warm_sessions) = metrics
        .pool_metrics()
        .map(|aggregate| (aggregate.sessions, aggregate.warm_sessions))
        .unwrap_or_else(|| {
            let warm_count = if warm_start { 1 } else { 0 };
            let session_count = if warm_start || metrics.pool_session().is_some() {
                1
            } else {
                0
            };
            (session_count, warm_count)
        });

    let runs = vec![RunSample {
        iteration: sessions as usize,
        parse_ms,
        lowering_ms,
        total_ms,
        warm_start,
    }];

    let warm_average_ms = if warm_sessions > 0 {
        total_ms
    } else {
        total_ms
    };
    let warm_min_ms = warm_average_ms;
    let warm_max_ms = warm_average_ms;

    let summary = Summary {
        cold_total_ms: total_ms,
        warm_average_ms,
        warm_min_ms,
        warm_max_ms,
        reuse_ratio,
        peak_rss_mb,
        sessions,
        warm_sessions,
    };

    let cold_within_budget = total_ms <= MAX_ELAPSED_MS;
    let warm_within_budget = if warm_sessions > 0 || warm_start {
        total_ms <= MAX_ELAPSED_MS
    } else {
        true
    };
    let reuse_ratio_ok = reuse_ratio >= MIN_REUSE_RATIO;
    let peak_rss_ok = peak_rss_mb.map(|mb| mb <= MAX_PEAK_RSS_MB);

    let checks = BudgetChecks {
        cold_within_budget,
        warm_within_budget,
        reuse_ratio_ok,
        peak_rss_ok,
    };

    let pass = cold_within_budget
        && warm_within_budget
        && reuse_ratio_ok
        && peak_rss_ok.unwrap_or(true);

    let budget = PerfBudget::new(MAX_ELAPSED_MS, MAX_PEAK_RSS_MB, MIN_REUSE_RATIO);

    let fixture = entrypoint
        .strip_prefix(workspace_root)
        .unwrap_or(entrypoint)
        .to_string_lossy()
        .into_owned();
    let report_location = report_path
        .strip_prefix(workspace_root)
        .unwrap_or(&report_path)
        .to_string_lossy()
        .into_owned();

    let timestamp_ms = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis();

    let report = PerfReport {
        report_version: PERF_REPORT_VERSION,
        fixture,
        iterations: runs.len(),
        runs,
        budget,
        summary,
        checks,
        pass,
        timestamp_ms,
        report_path: report_location,
    };

    let payload = serde_json::to_string_pretty(&report)
        .context("Failed to serialise performance report to JSON")?;
    fs::write(&report_path, payload).with_context(|| {
        format!(
            "Failed to write performance report: {}",
            report_path.display()
        )
    })?;

    Ok(PerfCapture {
        report,
        report_path,
    })
}
