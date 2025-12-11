use crate::harness::{BenchCorpus, PipelineKind, PipelineSwitcher, current_rss_kb};
use criterion::{Criterion, black_box};
use std::path::PathBuf;
use std::process::Command;

pub fn bench_memory(c: &mut Criterion) {
    let corpus = BenchCorpus::load().expect("corpus loads");
    let mut group = c.benchmark_group("memory");

    let sample = corpus
        .synthetic_by_lines(2000)
        .cloned()
        .expect("synthetic-2000 corpus present");

    let use_process_probe = std::env::var("JV_BENCH_USE_RSS_PROBE").is_ok();
    let corpus_path = sample_path();

    // Helper: run once with fresh pipeline to avoid accumulating salsa DB state across iters.
    let run_delta = |kind: PipelineKind| -> u64 {
        if use_process_probe {
            return run_delta_via_rss_probe(kind, &corpus_path);
        }

        let before = current_rss_kb().unwrap_or(0);
        // cacheless + lightweight for in-process measurement
        let harness =
            PipelineSwitcher::with_cache_mode(jv_parser_salsa::pipeline::CacheMode::Ephemeral);
        if let Err(err) = harness.run(kind, black_box(sample.source.as_str())) {
            black_box(err);
        }
        let after = current_rss_kb().unwrap_or(before);
        after.saturating_sub(before)
    };

    group.bench_function("salsa_fast/rss_delta", |b| {
        b.iter(|| {
            let delta = run_delta(PipelineKind::SalsaFast);
            black_box(delta);
        });
    });

    group.bench_function("salsa_full/rss_delta", |b| {
        b.iter(|| {
            let delta = run_delta(PipelineKind::SalsaFull);
            black_box(delta);
        });
    });

    group.bench_function("rowan/rss_delta", |b| {
        b.iter(|| {
            let delta = run_delta(PipelineKind::Rowan);
            black_box(delta);
        });
    });

    group.finish();
}

fn sample_path() -> PathBuf {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    manifest_dir
        .join("benches")
        .join("corpus")
        .join("synthetic")
        .join("synthetic-2000.jv")
}

/// 呼び出しごとに `rss_probe` を別プロセスで実行し、出力から RSS 差分を取得する。
fn run_delta_via_rss_probe(kind: PipelineKind, corpus_path: &PathBuf) -> u64 {
    let pipeline_arg = match kind {
        PipelineKind::SalsaFast => "salsa_fast",
        PipelineKind::SalsaFull => "salsa_full",
        PipelineKind::Rowan => "rowan",
    };

    let output = Command::new("cargo")
        .args([
            "run",
            "-q",
            "-p",
            "jv_parser_salsa",
            "--release",
            "--example",
            "rss_probe",
            "--",
            "--pipeline",
            pipeline_arg,
            "--corpus",
            corpus_path
                .to_str()
                .unwrap_or("benches/corpus/synthetic/synthetic-2000.jv"),
            "--cache-mode",
            "cacheless",
        ])
        .output()
        .expect("rss_probe process should run");

    let stdout = String::from_utf8_lossy(&output.stdout);
    stdout
        .lines()
        .find_map(|line| {
            line.split_whitespace()
                .find(|part| part.starts_with("rss_delta_kib="))
                .and_then(|part| part.strip_prefix("rss_delta_kib="))
                .and_then(|val| val.parse::<u64>().ok())
        })
        .expect("rss_probe output should contain rss_delta_kib")
}
