//! RegexCommand の型推論と診断結果を統合的に検証するテスト。

use jv_ast::Program;
use jv_ast::RegexCommandMode;
use jv_checker::CheckError;
use jv_checker::inference::regex::RegexCommandTyping;
use jv_checker::inference::types::PrimitiveType;
use jv_checker::{TypeChecker, TypeKind};
use jv_parser_frontend::ParserPipeline;
use jv_parser_rowan::frontend::RowanPipeline;

fn parse_program(source: &str) -> Program {
    RowanPipeline::default()
        .parse(source)
        .expect("ソースコードを構文解析できること")
        .into_program()
}

fn typecheck_regex_command(source: &str) -> RegexCommandTyping {
    let program = parse_program(source);
    let mut checker = TypeChecker::new();
    checker
        .check_program(&program)
        .expect("正規表現コマンドが型推論に成功すること");

    checker
        .inference_snapshot()
        .expect("推論スナップショットが存在すること")
        .regex_command_typings()
        .first()
        .cloned()
        .expect("RegexCommandTyping が記録されること")
}

fn typecheck_with_snapshot(source: &str) -> (RegexCommandTyping, TypeChecker, Program) {
    let program = parse_program(source);
    let mut checker = TypeChecker::new();
    checker
        .check_program(&program)
        .expect("正規表現コマンドが型推論に成功すること");

    let typing = checker
        .inference_snapshot()
        .expect("推論スナップショットが存在すること")
        .regex_command_typings()
        .first()
        .cloned()
        .expect("RegexCommandTyping が記録されること");
    (typing, checker, program)
}

#[test]
fn all_mode_returns_java_lang_string() {
    let typing = typecheck_regex_command(
        r#"
val subject = "value123"
val replaced = a/subject/\d+/"X"/
"#,
    );

    assert_eq!(typing.mode, RegexCommandMode::All);
    assert_eq!(
        typing.return_type,
        TypeKind::reference("java.lang.String"),
        "replaceAll 相当は String を返す想定です"
    );
    assert!(
        typing.diagnostics.is_empty(),
        "正常ケースでは診断が発生しない想定です: {:?}",
        typing.diagnostics
    );
}

#[test]
fn first_mode_also_returns_string() {
    let typing = typecheck_regex_command(
        r#"
val subject = "value123"
val replaced = f/subject/\d+/"only"/
"#,
    );

    assert_eq!(typing.mode, RegexCommandMode::First);
    assert_eq!(
        typing.return_type,
        TypeKind::reference("java.lang.String"),
        "replaceFirst 相当も String を返す想定です"
    );
}

#[test]
fn match_mode_returns_boxed_boolean_and_detects_flag_confusion() {
    let typing = typecheck_regex_command(
        r#"
val subject = "value123"
val matched = m/subject/^\d+$/m
"#,
    );

    assert_eq!(typing.mode, RegexCommandMode::Match);
    assert_eq!(
        typing.return_type,
        TypeKind::boxed(PrimitiveType::Boolean),
        "matches モードは java.lang.Boolean を返す想定です"
    );
    assert!(
        typing
            .diagnostics
            .iter()
            .all(|diag| diag.code != "JV_REGEX_E101"),
        "未知フラグ診断が誤って発生しないことを確認します: {:?}",
        typing.diagnostics
    );
}

#[test]
fn default_match_mode_reports_ambiguous_intent() {
    let typing = typecheck_regex_command(
        r#"
val subject = "value123"
val matched = /subject/\d+/
"#,
    );

    assert_eq!(typing.mode, RegexCommandMode::Match);
    assert!(
        typing
            .diagnostics
            .iter()
            .any(|diag| diag.code == "JV_REGEX_I002"),
        "モード省略時には JV_REGEX_I002 が提示される想定です: {:?}",
        typing.diagnostics
    );
}

#[test]
fn split_mode_returns_string_array() {
    let typing = typecheck_regex_command(
        r#"
val subject = "a,b,c"
val parts = s/subject/,\\s*/d
"#,
    );

    assert_eq!(typing.mode, RegexCommandMode::Split);
    assert_eq!(
        typing.return_type,
        TypeKind::reference("java.lang.String[]"),
        "Split モードは String[] を返す必要があります"
    );
}

#[test]
fn iterate_mode_without_replacement_requires_materialization() {
    let (typing, mut checker, program) = typecheck_with_snapshot(
        r#"
val subject = "value123"
val stream = i/subject/\d+/
"#,
    );

    assert_eq!(typing.mode, RegexCommandMode::Iterate);
    assert_eq!(
        typing.return_type,
        TypeKind::reference("java.util.stream.Stream"),
        "Iterate モードは Stream<MatchResult> の推論メタとして Stream を返す想定です"
    );
    assert!(
        typing.requires_stream_materialization,
        "置換なしの Iterate は requires_stream_materialization が true になる想定です"
    );

    // null safety 解析でも診断が発生しないことを念のため確認する
    let snapshot = checker.inference_snapshot().cloned();
    let diagnostics = checker.check_null_safety(&program, snapshot.as_ref());
    assert!(
        diagnostics
            .iter()
            .all(|diag| !matches!(diag, CheckError::NullSafetyError(_))),
        "Iterate モードは null safety エラーを引き起こさない想定です: {diagnostics:?}"
    );
}

#[test]
fn iterate_mode_with_literal_replacement_returns_string() {
    let typing = typecheck_regex_command(
        r#"
val subject = "value123"
val rebuilt = i/subject/\d+/"->"/
"#,
    );

    assert_eq!(typing.mode, RegexCommandMode::Iterate);
    assert_eq!(
        typing.return_type,
        TypeKind::reference("java.lang.String"),
        "置換付き Iterate は String を返す想定です"
    );
    assert!(
        !typing.requires_stream_materialization,
        "置換付き Iterate では materialization フラグが不要になる想定です"
    );
}
