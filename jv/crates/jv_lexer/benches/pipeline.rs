use criterion::{BatchSize, Criterion, black_box, criterion_group, criterion_main};
use jv_lexer::{Lexer, pipeline};

fn sample_source() -> String {
    let snippet = r#"
val items = [1 2 3 4 5]
fun transform(value: Int) -> Int {
    when (value % 2) {
        0 -> value / 2
        else -> value * 3 + 1
    }
}
"#;
    snippet.repeat(128)
}

fn run_pipeline_baseline(input: String) {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize().expect("baseline pipeline runs");
    black_box(tokens);
}

fn run_pipeline_new(input: String) {
    pipeline::trace::enable();
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize().expect("traced pipeline runs");
    pipeline::trace::disable();
    black_box(tokens);
}

fn bench_pipeline(c: &mut Criterion) {
    let sample = sample_source();
    let mut group = c.benchmark_group("lexer_pipeline");

    group.bench_function("pipeline_baseline", |b| {
        b.iter_batched(
            || sample.clone(),
            run_pipeline_baseline,
            BatchSize::SmallInput,
        )
    });

    group.bench_function("pipeline_new", |b| {
        b.iter_batched(|| sample.clone(), run_pipeline_new, BatchSize::SmallInput)
    });

    group.finish();
}

criterion_group!(lexer_pipeline, bench_pipeline);
criterion_main!(lexer_pipeline);
