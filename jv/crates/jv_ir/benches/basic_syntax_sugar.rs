use criterion::{criterion_group, criterion_main, Criterion};
use jv_ir::{transform::transform_program_with_context, TransformContext};
use jv_parser::Parser;

const SOURCE: &str = r#"
    val payload = {
        "user": {
            "name": "Alice",
            "age": 30
        },
        "tags": ["admin", "core", "ops"],
        "status": "active"
    }

    val template = """
        Hello, ${payload.user.name}!
        Tags: ${payload.tags.join(", ")}
    """

    val summary = SUM(1 2 3 4 5)
"#;

fn bench_basic_syntax_sugar(c: &mut Criterion) {
    c.bench_function("basic_syntax_sugar_pipeline", |b| {
        b.iter(|| {
            let program = Parser::parse(SOURCE).expect("source parses");
            let mut context = TransformContext::new();
            transform_program_with_context(program, &mut context).expect("lowering succeeds");
        });
    });
}

criterion_group!(benches, bench_basic_syntax_sugar);
criterion_main!(benches);
