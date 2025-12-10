use crate::harness::{PipelineKind, bench_state, current_rss_kb};
use criterion::{Criterion, black_box};

pub fn bench_memory(c: &mut Criterion) {
    let (harness, corpus) = bench_state();
    let mut group = c.benchmark_group("memory");

    let sample = corpus
        .synthetic_by_lines(2000)
        .cloned()
        .expect("synthetic-2000 corpus present");

    group.bench_function("salsa_full/rss_delta", |b| {
        b.iter(|| {
            let before = current_rss_kb().unwrap_or(0);
            harness
                .run(PipelineKind::SalsaFull, black_box(sample.source.as_str()))
                .expect("parse succeeds");
            let after = current_rss_kb().unwrap_or(before);
            black_box(after.saturating_sub(before));
        });
    });

    group.bench_function("salsa_fast/rss_delta", |b| {
        b.iter(|| {
            let before = current_rss_kb().unwrap_or(0);
            harness
                .run(PipelineKind::SalsaFast, black_box(sample.source.as_str()))
                .expect("parse succeeds");
            let after = current_rss_kb().unwrap_or(before);
            black_box(after.saturating_sub(before));
        });
    });

    group.finish();
}
