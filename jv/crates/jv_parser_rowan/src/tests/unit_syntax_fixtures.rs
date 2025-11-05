use std::fs;
use std::path::{Path, PathBuf};

use crate::frontend::RowanPipeline;

fn fixture_root() -> PathBuf {
    let crate_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    crate_dir
        .parent()
        .and_then(|dir| dir.parent())
        .expect("workspace layout should have ../../tests/fixtures")
        .join("tests")
        .join("fixtures")
        .join("unit_syntax")
}

fn load_fixture(path: &Path) -> String {
    fs::read_to_string(path)
        .unwrap_or_else(|error| panic!("フィクスチャ {:?} の読み込みに失敗しました: {}", path, error))
}

fn collect_fixtures(subdir: &str) -> Vec<PathBuf> {
    let dir = fixture_root().join(subdir);
    fs::read_dir(&dir)
        .unwrap_or_else(|error| panic!("ディレクトリ {:?} の列挙に失敗しました: {}", dir, error))
        .filter_map(|entry| {
            let path = entry.ok()?.path();
            if path.extension().and_then(|ext| ext.to_str()) == Some("jv") {
                Some(path)
            } else {
                None
            }
        })
        .collect()
}

#[test]
fn 単位構文フィクスチャの正常系が診断なしで通過する() {
    let pipeline = RowanPipeline::new();
    let categories = ["basic", "literals", "type_annotations"];

    for category in categories {
        for fixture in collect_fixtures(category) {
            let source = load_fixture(&fixture);
            let debug = pipeline
                .execute_with_debug(&source)
                .unwrap_or_else(|error| {
                    panic!(
                        "フィクスチャ {:?} の解析に失敗しました: {:?}",
                        fixture, error
                    )
                });

            assert!(
                debug.pipeline_error().is_none(),
                "フィクスチャ {:?} で予期しないパイプラインエラーが発生しました: {:?}",
                fixture,
                debug.pipeline_error()
            );

            let diagnostics = debug.artifacts().diagnostics().final_diagnostics();
            assert!(
                diagnostics.is_empty(),
                "フィクスチャ {:?} で想定外の診断が出力されました: {:?}",
                fixture,
                diagnostics
                    .iter()
                    .map(|diag| diag.message())
                    .collect::<Vec<_>>()
            );
        }
    }
}

fn expected_code_for_error_fixture(path: &Path) -> &'static str {
    let name = path
        .file_name()
        .and_then(|value| value.to_str())
        .unwrap_or_default();
    if name.contains("スペース") {
        "JV_UNIT_001"
    } else if name.contains("右辺") {
        "JV_UNIT_003"
    } else if name.contains("変換") {
        "JV_UNIT_004"
    } else {
        panic!("未知のフィクスチャ名です: {}", name);
    }
}

#[test]
fn 単位構文フィクスチャのエラーが期待した診断を報告する() {
    let pipeline = RowanPipeline::new();

    for fixture in collect_fixtures("errors") {
        let source = load_fixture(&fixture);
        let expected_code = expected_code_for_error_fixture(&fixture);

        let debug = pipeline
            .execute_with_debug(&source)
            .unwrap_or_else(|error| {
                panic!(
                    "フィクスチャ {:?} の解析に失敗しました: {:?}",
                    fixture, error
                )
            });

        let diagnostics = debug.artifacts().diagnostics().final_diagnostics();
        let messages: Vec<&str> = diagnostics.iter().map(|diag| diag.message()).collect();

        assert!(
            messages
                .iter()
                .any(|message| message.contains(expected_code)),
            "フィクスチャ {:?} で期待した診断コード {} が見つかりません。実際のメッセージ: {:?}",
            fixture,
            expected_code,
            messages
        );
    }
}
