use jv_codegen_java::{JavaCodeGenConfig, JavaCodeGenerator, JavaTarget};
use jv_ir::transform::transform_program_with_context;
use jv_ir::TransformContext;
use jv_parser_frontend::ParserPipeline;
use jv_parser_rowan::frontend::RowanPipeline;

fn lower_program(source: &str) -> jv_ir::types::IrProgram {
    let program = RowanPipeline::default()
        .parse(source)
        .expect("source parses")
        .into_program();
    let mut context = TransformContext::new();
    transform_program_with_context(program, &mut context).expect("lowering succeeds")
}

fn render_for_target(ir_program: &jv_ir::types::IrProgram, target: JavaTarget) -> String {
    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(target));
    let unit = generator
        .generate_compilation_unit(ir_program)
        .expect("java generation succeeds");
    unit.to_source(&JavaCodeGenConfig::for_target(target))
}

#[test]
fn multiline_literals_emit_text_block_for_java25() {
    let source = r#"
        fun main(): Unit {
            val template = """
SELECT *
FROM logs
WHERE level = "WARN"
"""
            println(template)
        }
    "#;

    let ir_program = lower_program(source);
    let java25 = render_for_target(&ir_program, JavaTarget::Java25);
    assert!(
        java25.contains("\"\"\""),
        "Java 25 target should emit text block literal:\n{}",
        java25
    );
    assert!(
        java25.contains("SELECT *"),
        "text block should retain content without escaping:\n{}",
        java25
    );
    assert!(
        !java25.contains("\\nFROM"),
        "text block must not escape embedded newlines:\n{}",
        java25
    );

    let java21 = render_for_target(&ir_program, JavaTarget::Java21);
    assert!(
        java21.contains("\\nFROM"),
        "Java 21 fallback should keep escaped newlines:\n{}",
        java21
    );
    assert!(
        !java21.contains("\"\"\""),
        "Java 21 fallback must not emit text block literal:\n{}",
        java21
    );
}

#[test]
fn multiline_literals_still_escape_when_target_is_java21() {
    let source = r#"
        fun main(): Unit {
            val template = """
lineA
lineB
"""
            println(template)
        }
    "#;

    let ir_program = lower_program(source);
    let java21 = render_for_target(&ir_program, JavaTarget::Java21);
    assert!(java21.contains("\\nlineB"), "newline should be escaped: {java21}");

    let java25 = render_for_target(&ir_program, JavaTarget::Java25);
    assert!(java25.contains("lineA"), "content should remain in text block: {java25}");
    assert!(
        java25.contains("\"\"\""),
        "text block literal expected for Java 25 target: {java25}"
    );
}
