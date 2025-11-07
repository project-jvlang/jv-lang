use std::fs;
use std::path::{Path, PathBuf};

use jv_checker::TypeChecker;
use jv_cli::pipeline::{
    generics::apply_type_facts,
    type_facts_bridge::{preload_tuple_plans_into_context, preload_type_facts_into_context},
};
use jv_codegen_java::{generate_java_source_with_config, JavaCodeGenConfig};
use jv_ir::{TransformContext, transform::transform_program_with_context};
use jv_parser_frontend::ParserPipeline;
use jv_parser_rowan::frontend::RowanPipeline;
use jv_pm::JavaTarget;

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("..")
}

fn fixture_path(name: &str) -> PathBuf {
    workspace_root()
        .join("tests")
        .join("data")
        .join("tuple")
        .join(format!("{name}.jv"))
}

fn expected_path(name: &str, target: JavaTarget) -> PathBuf {
    workspace_root()
        .join("tests")
        .join("data")
        .join("tuple")
        .join("expected")
        .join(format!("java{}", target.as_str()))
        .join(format!("{name}.java"))
}

fn normalize(text: &str) -> String {
    text.replace("\r\n", "\n").trim_end().to_string()
}

fn generate_java_source(name: &str, target: JavaTarget) -> String {
    let source =
        fs::read_to_string(fixture_path(name)).expect("フィクスチャの読み込みに失敗しました");
    let program = RowanPipeline::default()
        .parse(&source)
        .expect("ソース解析に失敗しました")
        .into_program();

    let mut checker = TypeChecker::new();
    checker.set_java_target(target);
    checker
        .check_program(&program)
        .expect("型チェックに失敗しました");

    let tuple_plans = checker.tuple_record_plans().to_vec();
    let type_facts_snapshot = checker.type_facts().cloned();

    let lowering_input = checker
        .take_normalized_program()
        .unwrap_or_else(|| program.clone());

    let mut context = TransformContext::new();
    if let Some(facts) = type_facts_snapshot.as_ref() {
        preload_type_facts_into_context(&mut context, facts);
    }
    preload_tuple_plans_into_context(&mut context, &tuple_plans);

    let mut ir_program = transform_program_with_context(lowering_input, &mut context)
        .expect("IR変換に失敗しました");
    ir_program.tuple_record_plans = tuple_plans;

    if let Some(facts) = type_facts_snapshot.as_ref() {
        apply_type_facts(&mut ir_program, facts);
    }

    let config = JavaCodeGenConfig::for_target(target);
    generate_java_source_with_config(&ir_program, &config)
        .expect("Javaコード生成に失敗しました")
}

#[test]
fn tuple_fixtures_match_expected_java() {
    let fixtures = ["divmod", "find_user", "calculate_stats"];
    for target in [JavaTarget::Java25, JavaTarget::Java21] {
        for fixture in fixtures {
            let actual = generate_java_source(fixture, target);
            let expected_file = expected_path(fixture, target);
            let expected = fs::read_to_string(&expected_file).unwrap_or_else(|_| {
                panic!(
                    "ゴールデンファイルが存在しません: {}\n--- 実際の出力 ---\n{}",
                    expected_file.display(),
                    actual
                )
            });

            assert_eq!(
                normalize(&actual),
                normalize(&expected),
                "ゴールデン比較が一致しません (fixture: {}, target: java{})",
                fixture,
                target.as_str()
            );
        }
    }
}
