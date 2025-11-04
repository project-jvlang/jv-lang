use std::fs;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

use crate::{JavaCodeGenConfig, JavaTarget, generate_java_source_with_config};
use jv_ir::transform::transform_program_with_context_profiled;
use jv_ir::{TransformContext, TransformProfiler};
use jv_parser_frontend::ParserPipeline;
use jv_parser_rowan::frontend::RowanPipeline;

const ITERATIONS: usize = 6;
const WARMUP_SKIP: usize = 1;
const LOWERING_STAGE: &str = "lowering";
const MAX_SLOWDOWN_RATIO: f64 = 1.10;
const BASELINE_FILE: &str = "baseline.jv";
const JUNIT_FILE: &str = "junit5_suite.jv";
const EXAMPLE_DIR: &str = "examples/junit5_benchmark/src";

#[test]
#[ignore]
fn junit5_codegen() {
    let report = assert_junit5_within_threshold(MAX_SLOWDOWN_RATIO);
    log_summary(&report);
}

fn assert_junit5_within_threshold(max_ratio: f64) -> BenchReport {
    let report = run_junit5_bench(ITERATIONS);
    let total_ratio = report.total_ratio();
    assert!(
        total_ratio <= max_ratio,
        "テストDSLパイプラインの合計時間が許容値を超えました: {:.2}% (基準 {:.2}ms / テスト {:.2}ms)",
        (total_ratio - 1.0) * 100.0,
        report.baseline.total_ms_avg,
        report.junit.total_ms_avg,
    );

    let lowering_ratio = report.lowering_ratio();
    assert!(
        lowering_ratio <= max_ratio,
        "AST→IR 変換時間が許容値を超えました: {:.2}% (基準 {:.2}ms / テスト {:.2}ms)",
        (lowering_ratio - 1.0) * 100.0,
        report.baseline.lowering_ms_avg,
        report.junit.lowering_ms_avg,
    );

    report
}

fn run_junit5_bench(iterations: usize) -> BenchReport {
    let (baseline_source, junit_source) = load_sample_sources();
    let pipeline = RowanPipeline::default();
    let config = JavaCodeGenConfig::for_target(JavaTarget::Java25);

    let baseline_samples = run_iterations(&baseline_source, iterations, &pipeline, &config);
    let junit_samples = run_iterations(&junit_source, iterations, &pipeline, &config);

    BenchReport {
        baseline: aggregate_metrics(&baseline_samples),
        junit: aggregate_metrics(&junit_samples),
    }
}

fn run_iterations(
    source: &str,
    iterations: usize,
    pipeline: &RowanPipeline,
    config: &JavaCodeGenConfig,
) -> Vec<PipelineRunMetrics> {
    let mut runs = Vec::with_capacity(iterations);
    for _ in 0..iterations {
        runs.push(measure_single_iteration(source, pipeline, config));
    }
    runs
}

fn measure_single_iteration(
    source: &str,
    pipeline: &RowanPipeline,
    config: &JavaCodeGenConfig,
) -> PipelineRunMetrics {
    let parse_start = Instant::now();
    let program = pipeline
        .parse(source)
        .expect("ベンチマークソースの解析に失敗しました")
        .into_program();
    let parse_ms = duration_to_millis(parse_start.elapsed());

    let mut context = TransformContext::new();
    let mut profiler = TransformProfiler::new();
    let (ir, metrics) =
        transform_program_with_context_profiled(program, &mut context, &mut profiler)
            .expect("AST から IR への変換に失敗しました");
    let lowering_ms = metrics
        .stage_millis(LOWERING_STAGE)
        .unwrap_or_else(|| metrics.total_millis());

    let codegen_start = Instant::now();
    generate_java_source_with_config(&ir, config).expect("Java コード生成に失敗しました");
    let codegen_ms = duration_to_millis(codegen_start.elapsed());

    PipelineRunMetrics {
        parse_ms,
        lowering_ms,
        codegen_ms,
        total_ms: parse_ms + lowering_ms + codegen_ms,
    }
}

fn aggregate_metrics(samples: &[PipelineRunMetrics]) -> AggregateMetrics {
    let trimmed = trim_samples(samples);
    AggregateMetrics {
        parse_ms_avg: average(trimmed, |sample| sample.parse_ms),
        lowering_ms_avg: average(trimmed, |sample| sample.lowering_ms),
        codegen_ms_avg: average(trimmed, |sample| sample.codegen_ms),
        total_ms_avg: average(trimmed, |sample| sample.total_ms),
    }
}

fn trim_samples<'a>(samples: &'a [PipelineRunMetrics]) -> &'a [PipelineRunMetrics] {
    if samples.len() > WARMUP_SKIP {
        &samples[WARMUP_SKIP..]
    } else {
        samples
    }
}

fn average(samples: &[PipelineRunMetrics], extract: impl Fn(&PipelineRunMetrics) -> f64) -> f64 {
    if samples.is_empty() {
        return 0.0;
    }
    let sum: f64 = samples.iter().map(extract).sum();
    sum / samples.len() as f64
}

fn load_sample_sources() -> (String, String) {
    (read_example(BASELINE_FILE), read_example(JUNIT_FILE))
}

fn read_example(name: &str) -> String {
    let path = fixture_path(name);
    fs::read_to_string(&path)
        .unwrap_or_else(|err| panic!("{} の読み込みに失敗しました: {}", path.display(), err))
}

fn fixture_path(name: &str) -> PathBuf {
    workspace_root().join(EXAMPLE_DIR).join(name)
}

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("crates ディレクトリが必要です")
        .parent()
        .expect("jv ディレクトリが必要です")
        .parent()
        .expect("ワークスペースルートが必要です")
        .to_path_buf()
}

fn duration_to_millis(duration: Duration) -> f64 {
    duration.as_secs_f64() * 1_000.0
}

fn log_summary(report: &BenchReport) {
    println!(
        "ベンチマーク結果: 基準 {:.3}ms / テスト {:.3}ms (合計倍率 {:.3})",
        report.baseline.total_ms_avg,
        report.junit.total_ms_avg,
        report.total_ratio(),
    );
    println!(
        "ローワリング: 基準 {:.3}ms / テスト {:.3}ms (倍率 {:.3})",
        report.baseline.lowering_ms_avg,
        report.junit.lowering_ms_avg,
        report.lowering_ratio(),
    );
    println!(
        "パース: 基準 {:.3}ms / テスト {:.3}ms",
        report.baseline.parse_ms_avg, report.junit.parse_ms_avg,
    );
    println!(
        "コード生成: 基準 {:.3}ms / テスト {:.3}ms",
        report.baseline.codegen_ms_avg, report.junit.codegen_ms_avg,
    );
}

#[derive(Debug, Clone)]
struct PipelineRunMetrics {
    parse_ms: f64,
    lowering_ms: f64,
    codegen_ms: f64,
    total_ms: f64,
}

#[derive(Debug, Clone)]
struct AggregateMetrics {
    parse_ms_avg: f64,
    lowering_ms_avg: f64,
    codegen_ms_avg: f64,
    total_ms_avg: f64,
}

#[derive(Debug, Clone)]
struct BenchReport {
    baseline: AggregateMetrics,
    junit: AggregateMetrics,
}

impl BenchReport {
    fn total_ratio(&self) -> f64 {
        if self.baseline.total_ms_avg <= f64::EPSILON {
            1.0
        } else {
            self.junit.total_ms_avg / self.baseline.total_ms_avg
        }
    }

    fn lowering_ratio(&self) -> f64 {
        if self.baseline.lowering_ms_avg <= f64::EPSILON {
            1.0
        } else {
            self.junit.lowering_ms_avg / self.baseline.lowering_ms_avg
        }
    }
}
