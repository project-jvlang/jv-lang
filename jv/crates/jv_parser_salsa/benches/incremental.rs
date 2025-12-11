use crate::harness::{PipelineKind, bench_state, estimate_hit_rate};
use criterion::{Criterion, black_box};

pub fn bench_incremental(c: &mut Criterion) {
    let (harness, corpus) = bench_state();
    let mut group = c.benchmark_group("incremental");

    let base = corpus
        .synthetic_by_lines(500)
        .cloned()
        .expect("synthetic-500 corpus present");
    let edited = base.source.replace("synthetic_0", "synthetic_0_edited");
    let hit_rate = estimate_hit_rate(&base.source, &edited);

    group.bench_function("salsa_full/single_line_edit", |b| {
        b.iter(|| {
            if let Err(err) = harness.run(PipelineKind::SalsaFull, black_box(base.source.as_str()))
            {
                black_box(err);
            }
            if let Err(err) = harness.run(PipelineKind::SalsaFull, black_box(edited.as_str())) {
                black_box(err);
            }
        });
    });

    group.bench_function("salsa_fast/cache_hit_estimate", |b| {
        b.iter(|| black_box(hit_rate));
    });

    if let Some(stdlib) = corpus.stdlib().first().cloned() {
        group.bench_function("rowan/unchanged_reparse", |b| {
            b.iter(|| {
                if let Err(err) =
                    harness.run(PipelineKind::Rowan, black_box(stdlib.source.as_str()))
                {
                    black_box(err);
                }
            });
        });
    }

    group.finish();
}
