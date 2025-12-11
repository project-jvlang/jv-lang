use crate::harness::{BenchCorpus, PipelineKind, PipelineSwitcher, current_rss_kb};
use criterion::{Criterion, black_box};

pub fn bench_memory(c: &mut Criterion) {
    let corpus = BenchCorpus::load().expect("corpus loads");
    let mut group = c.benchmark_group("memory");

    let sample = corpus
        .synthetic_by_lines(2000)
        .cloned()
        .expect("synthetic-2000 corpus present");

    // Helper: run once with fresh pipeline to avoid accumulating salsa DB state across iters.
    let run_delta = |kind: PipelineKind| -> u64 {
        let before = current_rss_kb().unwrap_or(0);
        let harness = PipelineSwitcher::new();
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
