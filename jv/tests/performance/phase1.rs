use std::fs;
use std::path::{Path, PathBuf};
use std::time::{Instant, SystemTime, UNIX_EPOCH};

use jv_ir::{
    TransformContext, TransformPools, TransformProfiler, transform_program_with_context_profiled,
};
use jv_parser_frontend::{Parser2Pipeline, ParserPipeline};
use jv_support::perf::report::{BudgetChecks, PerfBudget, PerfReport, RunSample, Summary};

const ITERATIONS: usize = 12;
const REPORT_FILENAME: &str = "ast-ir-phase1.json";

const MAX_ELAPSED_MS: f64 = 3_000.0;
const MAX_PEAK_RSS_MB: f64 = 100.0;
const MIN_REUSE_RATIO: f64 = 0.90;

#[test]
#[ignore]
fn perf_phase1() {
    let workspace = workspace_root();
    let fixture_path = workspace.join("tests/performance/phase1.jv");
    let report_dir = workspace.join("target/perf-reports");
    let report_path = report_dir.join(REPORT_FILENAME);

    let source = fs::read_to_string(&fixture_path)
        .unwrap_or_else(|err| panic!("failed to read fixture {}: {}", fixture_path.display(), err));

    fs::create_dir_all(&report_dir).expect("failed to create perf report directory");

    let budget = PerfBudget {
        max_elapsed_ms: MAX_ELAPSED_MS,
        max_peak_rss_mb: MAX_PEAK_RSS_MB,
        min_reuse_ratio: MIN_REUSE_RATIO,
    };

    let mut runs = Vec::with_capacity(ITERATIONS);
    let mut total_times = Vec::with_capacity(ITERATIONS);

    let pools = TransformPools::with_chunk_capacity(256 * 1024);
    let mut context = TransformContext::with_pools(pools.clone());
    let mut profiler = TransformProfiler::new();
    let pipeline = Parser2Pipeline::default();

    let mut latest_reuse_ratio = None;
    let mut latest_peak_rss = None;
    let mut sessions = 0;
    let mut warm_sessions = 0;

    for iteration in 0..ITERATIONS {
        let parse_start = Instant::now();
        let program = pipeline
            .parse(&source)
            .expect("fixture should parse")
            .into_program();
        let parse_ms = duration_to_millis(parse_start.elapsed());

        let (_ir, metrics) =
            transform_program_with_context_profiled(program, &mut context, &mut profiler)
                .expect("fixture should lower to IR");

        let lowering_ms = metrics
            .stage("lowering")
            .map(|stage| stage.elapsed_millis())
            .unwrap_or_else(|| metrics.total_millis());
        let total_ms = parse_ms + lowering_ms;
        let warm = metrics.was_warm_start().unwrap_or(false);

        if let Some(pool_metrics) = metrics.pool_metrics() {
            sessions = pool_metrics.sessions;
            warm_sessions = pool_metrics.warm_sessions;
            latest_reuse_ratio = Some(pool_metrics.reuse_ratio());
        }
        if let Some(bytes) = metrics.peak_rss_bytes() {
            latest_peak_rss = Some(bytes as f64 / (1024.0 * 1024.0));
        }

        runs.push(RunSample {
            iteration: iteration + 1,
            parse_ms,
            lowering_ms,
            total_ms,
            warm_start: warm,
        });
        total_times.push(total_ms);
    }

    let cold_total_ms = total_times
        .first()
        .copied()
        .expect("at least one iteration should run");
    let warm_totals = &total_times[1..];

    let warm_average_ms = if warm_totals.is_empty() {
        cold_total_ms
    } else {
        warm_totals.iter().copied().sum::<f64>() / warm_totals.len() as f64
    };
    let (warm_min_ms, warm_max_ms) = if warm_totals.is_empty() {
        (cold_total_ms, cold_total_ms)
    } else {
        let min = warm_totals.iter().copied().fold(f64::INFINITY, f64::min);
        let max = warm_totals
            .iter()
            .copied()
            .fold(f64::NEG_INFINITY, f64::max);
        (min, max)
    };

    let warm_within_budget = warm_totals.iter().all(|ms| *ms <= budget.max_elapsed_ms);

    let checks = BudgetChecks {
        cold_within_budget: cold_total_ms <= budget.max_elapsed_ms,
        warm_within_budget,
        reuse_ratio_ok: latest_reuse_ratio.map_or(true, |ratio| ratio >= budget.min_reuse_ratio),
        peak_rss_ok: latest_peak_rss.map(|mb| mb <= budget.max_peak_rss_mb),
    };

    let summary = Summary {
        cold_total_ms,
        warm_average_ms,
        warm_min_ms,
        warm_max_ms,
        reuse_ratio: latest_reuse_ratio.unwrap_or_default(),
        peak_rss_mb: latest_peak_rss,
        sessions,
        warm_sessions,
    };

    let pass = checks.cold_within_budget
        && checks.warm_within_budget
        && checks.reuse_ratio_ok
        && checks.peak_rss_ok.unwrap_or(true);

    let report = PerfReport {
        report_version: 1,
        fixture: fixture_path
            .strip_prefix(&workspace)
            .unwrap_or(&fixture_path)
            .to_string_lossy()
            .into_owned(),
        iterations: ITERATIONS,
        runs,
        budget,
        summary,
        checks,
        pass,
        timestamp_ms: current_timestamp_ms(),
        report_path: report_path
            .strip_prefix(&workspace)
            .unwrap_or(&report_path)
            .to_string_lossy()
            .into_owned(),
    };

    let payload = serde_json::to_string_pretty(&report).expect("serialize perf report");
    fs::write(&report_path, payload).expect("write perf report");

    println!(
        "phase1 AST→IR performance: cold={:.2}ms warm_avg={:.2}ms reuse={:.3}",
        report.summary.cold_total_ms, report.summary.warm_average_ms, report.summary.reuse_ratio
    );

    assert!(
        report.pass,
        "phase1 performance budget failed: {:?}",
        report.checks
    );
}

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("..").join("..")
}

fn duration_to_millis(duration: std::time::Duration) -> f64 {
    duration.as_secs_f64() * 1_000.0
}

fn current_timestamp_ms() -> u128 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system clock should be after unix epoch")
        .as_millis()
}
