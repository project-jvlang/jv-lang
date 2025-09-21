use std::fmt::Write;

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct DeferredSyntaxEntry {
    /// 構文ギャップ名（英語でも可）
    syntax: &'static str,
    /// 暫定対応や現状の扱い。
    mitigation: &'static str,
    /// フォローアップ手段や参照 ID。
    follow_up: &'static str,
}

/// 現時点で延期されている構文の一覧。
/// 完了した構文はここから削除し、ドキュメントの同期を取る。
const INVENTORY: &[DeferredSyntaxEntry] = &[];

/// Deferred syntax ドキュメントのマーカー区間へ出力する Markdown を生成する。
pub(crate) fn render_markdown_table(entries: &[DeferredSyntaxEntry]) -> String {
    if entries.is_empty() {
        return String::from(
            "現時点では延期中の構文はありません。\nNo syntax is currently deferred.",
        );
    }

    let mut table = String::from(
        "| Syntax / 構文 | Mitigation / 暫定対応 | Follow-up / フォローアップ |\n| --- | --- | --- |\n",
    );

    for entry in entries {
        let _ = writeln!(
            table,
            "| {} | {} | {} |",
            entry.syntax, entry.mitigation, entry.follow_up
        );
    }

    table.trim_end().to_string()
}

#[test]
fn deferred_syntax_doc_is_in_sync_with_inventory() {
    const START_MARKER: &str = "<!-- deferred-syntax:begin -->";
    const END_MARKER: &str = "<!-- deferred-syntax:end -->";
    let doc = include_str!("../../docs/deferred-syntax.md");

    let start = doc
        .find(START_MARKER)
        .expect("deferred-syntax doc is missing start marker");
    let end = doc
        .find(END_MARKER)
        .expect("deferred-syntax doc is missing end marker");
    assert!(start < end, "deferred-syntax markers are inverted");

    let actual_section = doc[start + START_MARKER.len()..end].trim();
    let expected_section = render_markdown_table(INVENTORY);

    assert_eq!(
        actual_section,
        expected_section.trim(),
        "deferred-syntax doc is out of sync with INVENTORY; update docs/deferred-syntax.md using render_markdown_table()"
    );
}
