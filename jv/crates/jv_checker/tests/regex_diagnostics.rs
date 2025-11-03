//! 正規表現診断のメタデータと多言語メッセージを検証するテスト群。

use jv_ast::{Program, Span};
use jv_checker::CheckError;
use jv_checker::diagnostics::{DiagnosticSeverity, messages};
use jv_checker::regex::RegexValidator;
use jv_parser_frontend::ParserPipeline;
use jv_parser_rowan::frontend::RowanPipeline;

fn parse_program(source: &str) -> Program {
    RowanPipeline::default()
        .parse(source)
        .expect("ソースコードを構文解析できること")
        .into_program()
}

#[test]
fn validator_populates_regex_escape_metadata() {
    let program = parse_program(
        r#"
val problematic = /value\q/
"#,
    );

    let mut validator = RegexValidator::new();
    let errors = validator.validate_program(&program);
    assert!(
        errors.iter().any(|error| matches!(
            error,
            CheckError::ValidationError { message, .. }
                if message.contains("JV_REGEX_E203")
        )),
        "RegexValidator は JV_REGEX_E203 エラーを報告する必要があります: {errors:?}"
    );

    let analyses = validator.take_analyses();
    assert_eq!(
        analyses.len(),
        1,
        "正規表現解析は 1 件のはずです: {analyses:?}"
    );
    let analysis = &analyses[0];
    let diagnostic = analysis
        .diagnostics
        .iter()
        .find(|entry| entry.code == "JV_REGEX_E220")
        .expect("静的検証の昇格診断が含まれる必要があります");
    assert!(
        diagnostic.message.contains("JV_REGEX_E203"),
        "元となる JV_REGEX_E203 エラーコードがメッセージに含まれる必要があります: {}",
        diagnostic.message
    );

    assert_eq!(
        diagnostic.severity,
        DiagnosticSeverity::Error,
        "不正エスケープはエラー重大度で報告される必要があります"
    );
    assert!(
        diagnostic
            .categories
            .iter()
            .any(|category| category == &"regex.literal.escape"),
        "診断カテゴリは regex.literal.escape を含む必要があります: {:?}",
        diagnostic.categories
    );
    assert!(
        diagnostic.span.is_some(),
        "診断には正確な Span が必須です: {:?}",
        diagnostic.span
    );
    assert!(
        diagnostic
            .suggestions
            .iter()
            .any(|suggestion| suggestion.contains("\\q")),
        "無効なエスケープを置き換える提案が含まれる必要があります: {:?}",
        diagnostic.suggestions
    );
    assert!(
        diagnostic.message.contains(messages::JAVA_REGEX_DOC_URL),
        "診断メッセージは Java 正規表現ドキュメントの URL を含む必要があります: {}",
        diagnostic.message
    );
}

#[test]
fn bilingual_regex_messages_are_inlined() {
    let message = messages::regex_unterminated_literal_message();
    assert!(
        message.contains("正規表現リテラルが `/` で閉じられていません"),
        "日本語メッセージが含まれる必要があります: {message}"
    );
    assert!(
        message.contains("The regex literal is missing its closing `/`"),
        "英語メッセージが含まれる必要があります: {message}"
    );
    assert!(
        message.contains(messages::JAVA_REGEX_DOC_URL),
        "メッセージは Java 正規表現ドキュメントの URL を示す必要があります: {message}"
    );
}

#[test]
fn regex_command_issue_retains_metadata() {
    use jv_checker::diagnostics::messages::JAVA_REGEX_DOC_URL;
    use jv_checker::inference::regex::RegexCommandIssue;

    let span = Span::new(3, 5, 3, 12);
    let issue = RegexCommandIssue::new(
        "JV_REGEX_E103",
        "モードとフラグの衝突を検出しました",
        span.clone(),
        DiagnosticSeverity::Warning,
        "regex.flag",
    )
    .with_suggestions(["Quick Fix: regex.mode.resolve-conflict -> remove flag m"]);

    assert_eq!(issue.code, "JV_REGEX_E103");
    assert_eq!(issue.severity, DiagnosticSeverity::Warning);
    assert_eq!(issue.category, "regex.flag");
    assert_eq!(issue.span, span);
    assert!(
        issue
            .documentation_url
            .as_deref()
            .is_some_and(|url| url == JAVA_REGEX_DOC_URL),
        "RegexCommandIssue は Java 正規表現ドキュメントへのリンクを既定で含む必要があります: {:?}",
        issue.documentation_url
    );
    assert!(
        issue
            .suggestions
            .iter()
            .any(|suggestion| suggestion.contains("Quick Fix")),
        "正規表現コマンド診断には Quick Fix 文面が含まれる必要があります: {:?}",
        issue.suggestions
    );
}
