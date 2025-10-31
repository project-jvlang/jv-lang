//! 正規表現 `is` 判定に関する型推論と null 安全解析の統合テスト。

use jv_ast::{Program, RegexGuardStrategy};
use jv_checker::{CheckError, TypeChecker};
use jv_parser_frontend::ParserPipeline;
use jv_parser_rowan::frontend::RowanPipeline;

fn parse_program(source: &str) -> Program {
    RowanPipeline::default()
        .parse(source)
        .expect("サンプルコードが構文解析できること")
        .into_program()
}

fn optional_subject_program() -> Program {
    parse_program(
        r#"
val maybe: String? = null
val judged = maybe is /\d+/
"#,
    )
}

fn pattern_expression_program() -> Program {
    parse_program(
        r#"
val pattern = java.util.regex.Pattern.compile("\\d+")
val text: String = "123"
val matched = text is pattern
"#,
    )
}

#[test]
fn optional左辺ではガード戦略と警告が記録される() {
    let program = optional_subject_program();

    let mut checker = TypeChecker::new();
    checker
        .check_program(&program)
        .expect("Optional 左辺の正規表現判定が型推論に成功すること");

    let snapshot = checker
        .inference_snapshot()
        .expect("型推論スナップショットが取得できること")
        .clone();
    let typings = snapshot.regex_typings();

    assert_eq!(
        typings.len(),
        1,
        "正規表現 is 判定が1件だけ記録される想定です: {typings:?}"
    );
    let typing = &typings[0];
    assert!(
        matches!(
            typing.guard_strategy,
            RegexGuardStrategy::CaptureAndGuard { .. }
        ),
        "Optional 左辺では一時変数を伴うガード戦略が設定される想定です: {:?}",
        typing.guard_strategy
    );
    assert_eq!(
        typing.warnings.len(),
        1,
        "Optional 左辺向けの警告が1件付与されることを期待しました: {typing:?}"
    );
    let warning = &typing.warnings[0];
    assert_eq!(
        warning.code, "JV_REGEX_W001",
        "Optional 左辺に対する警告コードが JV_REGEX_W001 である必要があります"
    );

    let diagnostics = checker.check_null_safety(&program, Some(&snapshot));
    assert!(
        diagnostics.iter().any(|error| matches!(
            error,
            CheckError::ValidationError { message, .. }
                if message.contains("JV_REGEX_W001")
        )),
        "null 安全解析でも JV_REGEX_W001 が警告として提示される想定です: {diagnostics:?}"
    );
    assert!(
        diagnostics
            .iter()
            .all(|error| !matches!(error, CheckError::NullSafetyError(_))),
        "正規表現判定で null 安全エラーが発生しないことを期待しました: {diagnostics:?}"
    );
}

#[test]
fn pattern式を用いたis判定では警告なしでガード戦略がnoneになる() {
    let program = pattern_expression_program();

    let mut checker = TypeChecker::new();
    checker
        .check_program(&program)
        .expect("Pattern 型を使用した判定が成功すること");

    let snapshot = checker
        .inference_snapshot()
        .expect("型推論スナップショットが取得できること")
        .clone();
    let typings = snapshot.regex_typings();

    assert_eq!(
        typings.len(),
        1,
        "Pattern 型を使用した判定も1件の解析記録になる想定です: {typings:?}"
    );
    let typing = &typings[0];
    assert!(
        matches!(typing.guard_strategy, RegexGuardStrategy::None),
        "Pattern 型の右辺では追加のガードが不要になる想定です: {:?}",
        typing.guard_strategy
    );
    assert!(
        typing.warnings.is_empty(),
        "Pattern 型の右辺では警告が発生しない想定です: {typing:?}"
    );

    let diagnostics = checker.check_null_safety(&program, Some(&snapshot));
    assert!(
        diagnostics.iter().all(|diagnostic| !matches!(
            diagnostic,
            CheckError::ValidationError { message, .. }
                if message.contains("JV_REGEX")
        )),
        "Pattern 型を使った場合に追加の正規表現警告が出ないことを期待しました: {diagnostics:?}"
    );
}
