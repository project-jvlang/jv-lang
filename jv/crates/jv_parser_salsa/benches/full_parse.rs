use crate::harness::{PipelineKind, PipelineSwitcher, bench_state};
use criterion::{Criterion, black_box};

pub fn bench_full_parse(c: &mut Criterion) {
    let mut group = c.benchmark_group("full_parse");
    let (_harness, corpus) = bench_state();

    let stdlib = corpus.stdlib().to_vec();
    group.bench_function("salsa_fast/stdlib", |b| {
        b.iter(|| {
            let harness = PipelineSwitcher::new();
            for entry in &stdlib {
                if let Err(err) =
                    harness.run(PipelineKind::SalsaFast, black_box(entry.source.as_str()))
                {
                    black_box(err);
                }
            }
        });
    });

    group.bench_function("salsa_full/stdlib", |b| {
        b.iter(|| {
            let harness = PipelineSwitcher::new();
            for entry in &stdlib {
                if let Err(err) =
                    harness.run(PipelineKind::SalsaFull, black_box(entry.source.as_str()))
                {
                    black_box(err);
                }
            }
        });
    });

    group.bench_function("rowan/stdlib", |b| {
        b.iter(|| {
            let harness = PipelineSwitcher::new();
            for entry in &stdlib {
                if let Err(err) = harness.run(PipelineKind::Rowan, black_box(entry.source.as_str()))
                {
                    black_box(err);
                }
            }
        });
    });

    if let Some(syn100) = corpus.synthetic_by_lines(100).cloned() {
        group.bench_function("salsa_fast/synthetic_100", |b| {
            b.iter(|| {
                let harness = PipelineSwitcher::new();
                if let Err(err) =
                    harness.run(PipelineKind::SalsaFast, black_box(syn100.source.as_str()))
                {
                    black_box(err);
                }
            });
        });

        group.bench_function("rowan/synthetic_100", |b| {
            b.iter(|| {
                let harness = PipelineSwitcher::new();
                if let Err(err) =
                    harness.run(PipelineKind::Rowan, black_box(syn100.source.as_str()))
                {
                    black_box(err);
                }
            });
        });
    }

    if let Some(syn2000) = corpus.synthetic_by_lines(2000).cloned() {
        group.bench_function("salsa_full/synthetic_2000", |b| {
            b.iter(|| {
                let harness = PipelineSwitcher::new();
                if let Err(err) =
                    harness.run(PipelineKind::SalsaFull, black_box(syn2000.source.as_str()))
                {
                    black_box(err);
                }
            });
        });

        group.bench_function("rowan/synthetic_2000", |b| {
            b.iter(|| {
                let harness = PipelineSwitcher::new();
                if let Err(err) =
                    harness.run(PipelineKind::Rowan, black_box(syn2000.source.as_str()))
                {
                    black_box(err);
                }
            });
        });
    }

    group.finish();
}
