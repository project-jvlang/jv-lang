use jv_lsp::JvLanguageServer;

#[test]
fn regex_diagnostic_includes_categories_and_quick_fix() {
    let mut server = JvLanguageServer::new();
    let uri = "file:///regex_diagnostic.jv".to_string();
    server.open_document(uri.clone(), "val broken = /abc\\q/\n".to_string());

    let diagnostics = server.get_diagnostics(&uri);
    let diagnostic = diagnostics
        .iter()
        .find(|entry| entry.code.as_deref() == Some("JV_REGEX_E220"))
        .expect("JV_REGEX_E220 診断が生成される必要があります");

    let data = diagnostic
        .data
        .as_ref()
        .expect("LSP 診断データが付与される必要があります");
    assert!(
        diagnostic.message.contains("JV_REGEX_E203"),
        "昇格診断は元のエラーコードをメッセージ内に含める必要があります: {}",
        diagnostic.message
    );
    assert!(
        data.categories
            .iter()
            .any(|category| category == "regex.literal.escape"),
        "カテゴリに regex.literal.escape を含める必要があります: {:?}",
        data.categories
    );
    assert!(
        data.documentation_urls
            .iter()
            .any(|url| url.contains("java/util/regex/Pattern.html")),
        "ドキュメントURLは Java 正規表現仕様を指す必要があります: {:?}",
        data.documentation_urls
    );
    assert!(
        !data.code_actions.is_empty(),
        "Quick Fix を含む CodeAction が必要です: {:?}",
        data.code_actions
    );
    assert!(
        data.code_actions
            .iter()
            .all(|action| action.kind.as_deref() == Some("quickfix")),
        "CodeAction の kind は quickfix である必要があります: {:?}",
        data.code_actions
    );
    assert!(
        data.code_actions
            .iter()
            .any(|action| action.title.contains("\\q")),
        "Quick Fix 文面は対象エスケープシーケンスを含む必要があります: {:?}",
        data.code_actions
    );
    assert!(
        !diagnostic.suggestions.is_empty(),
        "LSP 診断にも Quick Fix 文字列が含まれる必要があります"
    );
    assert_eq!(
        diagnostic.range.start.line, 0,
        "LSP 範囲は 0-based の行番号である必要があります: {:?}",
        diagnostic.range
    );
    assert_eq!(
        diagnostic.range.start.character, 13,
        "開始列は 0-based で 13（1-based 14 相当）である必要があります: {:?}",
        diagnostic.range
    );
    assert_eq!(
        diagnostic.range.end.character, 20,
        "終了列は 0-based で 20（1-based 21 相当）である必要があります: {:?}",
        diagnostic.range
    );
}
