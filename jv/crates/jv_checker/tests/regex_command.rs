//! RegexCommand の型推論と診断結果を統合的に検証するテスト。

use jv_ast::{Program, RegexCommandMode};
use jv_checker::diagnostics::{DiagnosticSeverity, messages::JAVA_REGEX_DOC_URL};
use jv_checker::inference::regex::{CATEGORY_REGEX_MODE, RegexCommandTyping};
use jv_checker::inference::types::PrimitiveType;
use jv_checker::{CheckError, TypeChecker, TypeKind};
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
    let confusion = typing
        .diagnostics
        .iter()
        .find(|diag| diag.code == "JV_REGEX_I001")
        .expect("モードとフラグの混同を検出する情報診断が必要です");
    assert_eq!(
        confusion.severity,
        DiagnosticSeverity::Information,
        "JV_REGEX_I001 は情報レベルの診断である必要があります"
    );
    assert_eq!(
        confusion.category, CATEGORY_REGEX_MODE,
        "JV_REGEX_I001 は regex.mode カテゴリに属する必要があります"
    );
    assert_eq!(
        confusion.span, typing.span,
        "モード混同診断はコマンド全体のスパンを指し示す必要があります"
    );
    assert_eq!(
        confusion.documentation_url.as_deref(),
        Some(JAVA_REGEX_DOC_URL),
        "診断には Java 正規表現ドキュメントへのリンクが含まれる必要があります"
    );
    assert!(
        confusion
            .suggestions
            .iter()
            .any(|entry| entry.contains("Quick Fix")),
        "診断には Quick Fix 案内が含まれる必要があります: {:?}",
        confusion.suggestions
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
    let intent = typing
        .diagnostics
        .iter()
        .find(|diag| diag.code == "JV_REGEX_I002")
        .expect("モード省略診断が存在する必要があります");
    assert_eq!(
        intent.severity,
        DiagnosticSeverity::Information,
        "JV_REGEX_I002 は情報レベルである必要があります"
    );
    assert_eq!(
        intent.category, CATEGORY_REGEX_MODE,
        "JV_REGEX_I002 は regex.mode カテゴリに分類される必要があります"
    );
    assert_eq!(
        intent.span, typing.span,
        "モード省略診断はコマンド式全体を指す必要があります"
    );
    assert_eq!(
        intent.documentation_url.as_deref(),
        Some(JAVA_REGEX_DOC_URL),
        "診断には Java 正規表現ドキュメントへのリンクが含まれる必要があります"
    );
    assert!(
        intent
            .suggestions
            .iter()
            .any(|entry| entry.contains("Quick Fix")),
        "診断には Quick Fix 案内が含まれる必要があります: {:?}",
        intent.suggestions
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

#[test]
fn telemetry_records_single_regex_command_metrics() {
    let source = r#"
val subject = "value123"
val replaced = a/subject/\d+/"X"/
"#;

    let program = parse_program(source);
    let mut checker = TypeChecker::new();
    checker
        .check_program(&program)
        .expect("RegexCommand を含むプログラムが型検査に成功すること");

    let snapshot = checker
        .inference_snapshot()
        .expect("推論スナップショットが存在すること");
    assert_eq!(
        snapshot.regex_command_typings().len(),
        1,
        "RegexCommand が1件収集される想定です"
    );

    let telemetry = checker.telemetry();
    let metrics = &telemetry.regex_command;

    assert_eq!(metrics.total, 1, "RegexCommand の総数が1件になる想定です");
    assert_eq!(
        metrics.modes.get("all"),
        Some(&1),
        "replaceAll モードが1件記録される想定です"
    );
    assert_eq!(
        metrics.return_types.get("java.lang.String"),
        Some(&1),
        "String 戻り値が1件記録される想定です"
    );
    assert_eq!(
        metrics.guard_strategies.get("none"),
        Some(&1),
        "非オプショナル subject はガード無しで記録される想定です"
    );
    assert_eq!(
        metrics.materialize_streams, 0,
        "materialize_streams は replaceAll では 0 の想定です"
    );
    assert!(
        metrics.diagnostics.is_empty(),
        "正常ケースでは診断メトリクスが空である想定です: {:?}",
        metrics.diagnostics
    );
}

#[test]
fn telemetry_records_iterate_optional_metrics() {
    let source = r#"
val subject: String? = "value123"
val stream = i/subject/\d+/
"#;

    let program = parse_program(source);
    let mut checker = TypeChecker::new();
    checker
        .check_program(&program)
        .expect("Iterate モードの RegexCommand が型検査に成功すること");

    let snapshot = checker
        .inference_snapshot()
        .expect("推論スナップショットが存在すること");
    assert_eq!(
        snapshot.regex_command_typings().len(),
        1,
        "Iterate モードの RegexCommand が1件収集される想定です"
    );

    let telemetry = checker.telemetry();
    let metrics = &telemetry.regex_command;

    assert_eq!(
        metrics.total, 1,
        "Iterate モードでも RegexCommand の総数は1件になる想定です"
    );
    assert_eq!(
        metrics.modes.get("iterate"),
        Some(&1),
        "iterate モードが1件記録される想定です"
    );
    assert_eq!(
        metrics.return_types.get("java.util.stream.Stream"),
        Some(&1),
        "Stream 戻り値が1件記録される想定です"
    );
    assert_eq!(
        metrics.guard_strategies.get("capture_and_guard"),
        Some(&1),
        "Optional subject は capture_and_guard として記録される想定です"
    );
    assert_eq!(
        metrics.materialize_streams, 1,
        "materialize_streams は Iterate (置換無し) で 1 になる想定です"
    );
    assert!(
        metrics.diagnostics.is_empty(),
        "Iterate 正常ケースでは診断メトリクスが空である想定です: {:?}",
        metrics.diagnostics
    );
}
