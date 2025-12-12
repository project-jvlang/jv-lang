use crate::harness::{
    BenchCorpus, CorpusEntry, PipelineKind, PipelineSwitcher, current_rss_kb,
    load_jdk_modules_entries,
};
use criterion::{Criterion, black_box};
use std::env;
use std::path::PathBuf;
use std::process::Command;

pub fn bench_memory(c: &mut Criterion) {
    let corpus = BenchCorpus::load().expect("corpus loads");
    let mut group = c.benchmark_group("memory");

    let sample = corpus
        .synthetic_by_lines(2000)
        .cloned()
        .expect("synthetic-2000 corpus present");

    let use_process_probe =
        cfg!(feature = "bench-rss-probe") || env::var("JV_BENCH_USE_RSS_PROBE").is_ok();
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

    let (modules_path, jdk_modules) = load_jdk_modules_entries().unwrap_or_else(|err| {
        panic!(
            "failed to load JDK modules corpus: {err}. \
             Set JV_BENCH_JDK_MODULES or provision toolchains/jdk25/lib/modules."
        )
    });

    group.bench_function("salsa_fast/jdk_modules_rss_delta", |b| {
        b.iter(|| {
            let delta = run_modules_delta(
                PipelineKind::SalsaFast,
                &jdk_modules,
                use_process_probe,
                &modules_path,
            );
            black_box(delta);
        });
    });

    group.bench_function("salsa_full/jdk_modules_rss_delta", |b| {
        b.iter(|| {
            let delta = run_modules_delta(
                PipelineKind::SalsaFull,
                &jdk_modules,
                use_process_probe,
                &modules_path,
            );
            black_box(delta);
        });
    });

    println!(
        "JDK modules memory benchmark source: {}",
        modules_path.display()
    );

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

fn run_modules_delta(
    kind: PipelineKind,
    modules: &[CorpusEntry],
    use_process_probe: bool,
    modules_path: &PathBuf,
) -> u64 {
    if use_process_probe {
        panic!(
            "rss_probe path driver does not yet support JDK module images ({}). \
             Implement streaming support in rss_probe before enabling JV_BENCH_USE_RSS_PROBE.",
            modules_path.display()
        );
    }

    let before = current_rss_kb().unwrap_or(0);
    let harness =
        PipelineSwitcher::with_cache_mode(jv_parser_salsa::pipeline::CacheMode::Ephemeral);
    for entry in modules {
        if let Err(err) = harness.run(kind, black_box(entry.source.as_str())) {
            black_box(err);
        }
    }
    let after = current_rss_kb().unwrap_or(before);
    after.saturating_sub(before)
}
