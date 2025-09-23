use std::time::Instant;

use jv_ast::{Expression, Program, Span, Statement};
use jv_ir::{transform_program_with_context, JavaType, TransformContext};
use jv_parser::Parser;

fn first_whitespace_array_span(program: &Program) -> Option<Span> {
    program.statements.iter().find_map(|statement| {
        if let Statement::ValDeclaration { initializer, .. } = statement {
            if let Expression::Array { delimiter, span, .. } = initializer {
                if matches!(delimiter, jv_ast::SequenceDelimiter::Whitespace) {
                    return Some(span.clone());
                }
            }
        }
        None
    })
}

fn parse_and_transform(source: &str) -> usize {
    let program = Parser::parse(source).expect("parser should accept whitespace sequences");
    let span = first_whitespace_array_span(&program);
    let mut context = TransformContext::new();
    let _ir = transform_program_with_context(program, &mut context)
        .expect("transform should succeed for whitespace sequences");

    let mut touched_sequences = 0usize;

    if let Some(span) = span {
        let cache = context.sequence_style_cache_mut();
        let miss = cache.lookup_or_insert_array(&span, JavaType::int());
        assert!(miss.is_none(), "first lookup should miss cache");
        let hit = cache.lookup_or_insert_array(&span, JavaType::string());
        assert_eq!(hit, Some(JavaType::int()));
        cache.clear();
        let after_clear = cache.lookup_or_insert_array(&span, JavaType::string());
        assert!(after_clear.is_none(), "cache should be reset after clear");
        touched_sequences += 1;
    }

    touched_sequences
}

fn main() {
    let iterations = std::env::var("SEQUENCE_LAYOUT_BENCH_ITERS")
        .ok()
        .and_then(|value| value.parse().ok())
        .unwrap_or(250);

    let source = r#"
val sample = [1 2 3 4 5]

fun plot(numbers, extra) {
    val _placeholder = numbers
    val _unused = extra
}

fun numbers() {
    val layout = [1 2 3 4 5]
    val comma = [6, 7, 8, 9, 10]
    plot(layout, comma)
}
"#;

    // Warm-up iteration to amortize parser allocations from first run.
    let mut total_sequences = parse_and_transform(source);

    let start = Instant::now();
    for _ in 0..iterations {
        total_sequences += parse_and_transform(source);
    }
    let elapsed = start.elapsed();

    println!(
        "sequence_layout_bench iterations={} sequences_touched={} elapsed={:?}",
        iterations, total_sequences, elapsed
    );
}
