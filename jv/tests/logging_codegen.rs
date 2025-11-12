use jv_codegen_java::{JavaCodeGenConfig, JavaCodeGenerator};
use jv_ir::{
    transform_program_with_context, LogLevel as IrLogLevel, LoggingFrameworkKind, TransformContext,
};
use jv_parser_frontend::ParserPipeline;
use jv_parser_rowan::frontend::RowanPipeline;

fn lower_with_logging(
    source: &str,
    framework: LoggingFrameworkKind,
    trace_context: bool,
) -> jv_ir::types::IrProgram {
    let pipeline = RowanPipeline::default();
    let output = pipeline.parse(source).expect("ソースコードの解析に成功するはずです");
    let diagnostics = output.diagnostics().final_diagnostics();
    assert!(
        diagnostics.is_empty(),
        "解析診断が発生しました: {:?}",
        diagnostics
    );
    let program = output.into_program();

    let mut context = TransformContext::new();
    context.set_logging_framework(framework);
    {
        let options = context.logging_options_mut();
        options.active_level = IrLogLevel::Trace;
        options.default_level = IrLogLevel::Info;
    }
    context.set_trace_context_enabled(trace_context);

    transform_program_with_context(program, &mut context)
        .expect("ローワリングに成功するはずです")
}

fn generate_java(ir_program: &jv_ir::types::IrProgram) -> String {
    let mut generator = JavaCodeGenerator::new();
    let unit = generator
        .generate_compilation_unit(ir_program)
        .expect("Javaコード生成に成功するはずです");
    unit.to_source(&JavaCodeGenConfig::default())
}

#[test]
fn slf4j_log_block_generates_info_invocation() {
    let source = r#"
        package example

        fun main {
            LOG {
                "start"
            }
        }
    "#;

    let ir_program =
        lower_with_logging(source, LoggingFrameworkKind::Slf4j, /*trace_context*/ false);
    let java = generate_java(&ir_program);

    assert!(
        java.contains("LOGGER.info(\"start\");"),
        "生成されたJavaコードに INFO 呼び出しが含まれるべきです:\n{java}"
    );
    assert!(
        java.contains("private static final org.slf4j.Logger LOGGER"),
        "ロガーフィールドが生成されるべきです:\n{java}"
    );
}

#[test]
fn trace_context_enabled_injects_mdc_operations() {
    let source = r#"
        package example

        fun main {
            LOG {
                "trace ready"
            }
        }
    "#;

    let ir_program = lower_with_logging(source, LoggingFrameworkKind::Slf4j, true);
    let java = generate_java(&ir_program);

    assert!(
        java.contains("Span.current()"),
        "トレースコンテキスト取得が必要です:\n{java}"
    );
    assert!(
        java.contains("MDC.put(\"traceId\""),
        "traceId を MDC に格納するコードが必要です:\n{java}"
    );
    assert!(
        java.contains("MDC.remove(\"traceId\")") && java.contains("MDC.remove(\"spanId\")"),
        "MDC クリーンアップが必要です:\n{java}"
    );
}
