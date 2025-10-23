use std::time::Instant;

use jv_codegen_java::{JavaCodeGenConfig, JavaCodeGenerator};
use jv_ir::transform_program;
use jv_parser_frontend::ParserPipeline;
use jv_parser_rowan::frontend::RowanPipeline;
use jv_pm::JavaTarget;

const BASELINE_MS: u128 = 200;

#[test]
fn loop_pipeline_stays_within_performance_budget() {
    let source = r#"
fun main() {
    var exclusive = 0
    for (value in 0..32) {
        exclusive = exclusive + value
    }

    var inclusive = 0
    for (value in 0..=16) {
        inclusive = inclusive + value
    }

    val total = exclusive + inclusive
    val sentinel = total
}
"#;

    let start = Instant::now();

    let program = RowanPipeline::default()
        .parse(source)
        .expect("loop sample should parse")
        .into_program();

    let ir = transform_program(program).expect("loop sample should lower to IR");

    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    generator
        .generate_compilation_unit(&ir)
        .expect("loop sample should generate Java");

    let elapsed_us = start.elapsed().as_micros();
    let baseline_us = BASELINE_MS * 1_000;
    let permitted_us = ((baseline_us as f64) * 1.02) as u128;

    assert!(
        elapsed_us <= permitted_us,
        "loop pipeline regression: took {}µs (budget {}µs)",
        elapsed_us,
        permitted_us
    );
}
