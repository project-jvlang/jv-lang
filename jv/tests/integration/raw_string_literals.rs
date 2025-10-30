use jv_codegen_java::{JavaCodeGenConfig, JavaCodeGenerator};
use jv_ir::TransformContext;
use jv_ir::transform::transform_program_with_context;
use jv_parser_frontend::ParserPipeline;
use jv_parser_rowan::frontend::RowanPipeline;
use jv_pm::JavaTarget;

#[path = "../fixture_runner.rs"]
mod fixture_runner;

use fixture_runner::integration_support::{compile_java_with_javac, detect_javac_major};

fn parse_and_lower(source: &str) -> jv_ir::types::IrProgram {
    let program = RowanPipeline::default()
        .parse(source)
        .expect("ソースの構文解析に失敗しました")
        .into_program();
    let mut context = TransformContext::new();
    transform_program_with_context(program, &mut context).expect("IRへのローワリングに失敗しました")
}

fn generate_java_source(ir_program: &jv_ir::types::IrProgram, target: JavaTarget) -> String {
    let config = JavaCodeGenConfig::for_target(target);
    let mut generator = JavaCodeGenerator::with_config(config.clone());
    let unit = generator
        .generate_compilation_unit(ir_program)
        .expect("Javaコード生成に失敗しました");
    unit.to_source(&config)
}

fn compile_with_javac_if_available(
    java_source: &str,
    required: u32,
    detected: Option<u32>,
    label: &str,
) {
    if let Some(version) = detected {
        let (class_name, renamed_source) = rename_public_class(java_source, label)
            .unwrap_or_else(|| panic!("publicクラス名を抽出できませんでした:\n{}", java_source));
        compile_java_with_javac(&renamed_source, required, version, &class_name, label)
            .unwrap_or_else(|error| panic!("javacコンパイルに失敗しました: {error:?}"));
    }
}

fn rename_public_class(java_source: &str, label: &str) -> Option<(String, String)> {
    for line in java_source.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("public ") {
            if let Some(index) = trimmed.find("class ") {
                let prefix = &trimmed[..index + "class ".len()];
                let after = &trimmed[index + "class ".len()..];
                let original = after
                    .chars()
                    .take_while(|ch| ch.is_ascii_alphanumeric() || *ch == '_')
                    .collect::<String>();
                if original.is_empty() {
                    continue;
                }
                let renamed = format!("{}_{}", original, label);
                let target = format!("{}{}", prefix, original);
                let replacement = format!("{}{}", prefix, renamed);
                let rewritten = java_source.replacen(&target, &replacement, 1);
                return Some((original, rewritten));
            }
        }
    }
    None
}

#[test]
fn raw_single_literals_lower_to_expected_java() {
    let source = r#"
        fun main(): Unit {
            val rawChar = 'A'
            val rawPath = 'C:\Users\jv'
            val literalInterpolation = '${value}'

            val combined = rawPath + literalInterpolation
            println(combined)
        }
    "#;

    let ir_program = parse_and_lower(source);
    let detected = detect_javac_major();

    for target in [JavaTarget::Java25, JavaTarget::Java21] {
        let java_source = generate_java_source(&ir_program, target);
        assert!(
            java_source.contains("rawChar = 'A'"),
            "生文字列1文字はcharリテラルとして出力されるべきです:\n{}",
            java_source
        );
        assert!(
            java_source.contains("char rawChar"),
            "単一文字の生文字列はchar型として宣言されるべきです:\n{}",
            java_source
        );
        assert!(
            java_source.contains(r#"rawPath = "C:\\Users\\jv""#),
            "バックスラッシュを含む生文字列はエスケープ済みのStringとして出力されるべきです:\n{}",
            java_source
        );
        assert!(
            java_source.contains("String rawPath"),
            "複数文字の生文字列はString型として宣言されるべきです:\n{}",
            java_source
        );
        assert!(
            java_source.contains(r#"literalInterpolation = "${value}""#),
            "補間構文を含む生文字列は文字列としてそのまま保持されるべきです:\n{}",
            java_source
        );

        compile_with_javac_if_available(
            &java_source,
            target.required_release(),
            detected,
            target.label(),
        );
    }
}

#[test]
fn raw_triple_literal_emits_text_block_and_compiles() {
    let source = r#"
        fun main(): Unit {
            val query = '''
SELECT *
FROM logs
WHERE level = 'ERROR'
'''

            val copy = query
            println(copy)
        }
    "#;

    let ir_program = parse_and_lower(source);
    let detected = detect_javac_major();

    for target in [JavaTarget::Java25, JavaTarget::Java21] {
        let java_source = generate_java_source(&ir_program, target);
        assert!(
            java_source.contains("SELECT *"),
            "SQL 文が生成コードに含まれていません:\n{}",
            java_source
        );
        assert!(
            java_source.contains("WHERE level = 'ERROR'")
                || java_source.contains("WHERE level = \\\"ERROR\\\""),
            "WHERE句が生成コードに含まれていません:\n{}",
            java_source
        );

        compile_with_javac_if_available(
            &java_source,
            target.required_release(),
            detected,
            target.label(),
        );
    }
}

trait JavaTargetExt {
    fn required_release(&self) -> u32;
    fn label(&self) -> &'static str;
}

impl JavaTargetExt for JavaTarget {
    fn required_release(&self) -> u32 {
        match self {
            JavaTarget::Java21 => 21,
            JavaTarget::Java25 => 25,
        }
    }

    fn label(&self) -> &'static str {
        match self {
            JavaTarget::Java21 => "Java21",
            JavaTarget::Java25 => "Java25",
        }
    }
}
