use crate::harness::{PipelineKind, bench_state};
use criterion::{Criterion, black_box};

pub fn bench_lsp(c: &mut Criterion) {
    let (harness, corpus) = bench_state();
    let mut group = c.benchmark_group("lsp");

    let file_500 = corpus
        .synthetic_by_lines(500)
        .cloned()
        .expect("synthetic-500 corpus present");

    group.bench_function("completion/500_lines/salsa_fast", |b| {
        b.iter(|| {
            if let Err(err) =
                harness.run(PipelineKind::SalsaFast, black_box(file_500.source.as_str()))
            {
                black_box(err);
            }
        });
    });

    group.bench_function("diagnostics/500_lines/salsa_full", |b| {
        b.iter(|| {
            if let Err(err) =
                harness.run(PipelineKind::SalsaFull, black_box(file_500.source.as_str()))
            {
                black_box(err);
            }
        });
    });

    group.bench_function("completion/500_lines/rowan", |b| {
        b.iter(|| {
            if let Err(err) = harness.run(PipelineKind::Rowan, black_box(file_500.source.as_str()))
            {
                black_box(err);
            }
        });
    });

    group.finish();
}
