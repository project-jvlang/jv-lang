const LANGUAGE_GUIDE: &str = include_str!("../../../../docs/language-guide-en.md");

const EXPECTED_TOC: &[(&str, &str)] = &[
    ("Variables and Types", "#variables-and-types"),
    ("Functions", "#functions"),
    ("Classes and Data Classes", "#classes-and-data-classes"),
    ("Null Safety", "#null-safety"),
    ("Control Flow", "#control-flow"),
    ("Collections", "#collections"),
    ("String Interpolation", "#string-interpolation"),
    ("Concurrency", "#concurrency"),
    ("Resource Management", "#resource-management"),
    ("Extension Functions", "#extension-functions"),
    ("Java Interop", "#java-interop"),
    (
        "JSON Literals & POJO Generation",
        "#json-literals--pojo-generation",
    ),
];

fn parse_table_of_contents(doc: &str) -> Vec<(String, String)> {
    let mut lines = doc.lines();
    while let Some(line) = lines.next() {
        if line.trim() == "## Table of Contents" {
            break;
        }
    }

    let mut entries = Vec::new();
    for line in lines {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            if entries.is_empty() {
                continue;
            } else {
                break;
            }
        }

        if !trimmed
            .chars()
            .next()
            .map(|ch| ch.is_ascii_digit())
            .unwrap_or(false)
        {
            continue;
        }

        let Some((prefix, suffix)) = trimmed.split_once("](") else {
            panic!("Table of Contents entryが `](...)` 形式になっていません: {trimmed}");
        };
        let Some(open_bracket) = prefix.find('[') else {
            panic!("Table of Contents entryに `[` が含まれていません: {trimmed}");
        };
        let title = &prefix[open_bracket + 1..];
        let Some(close_paren) = suffix.find(')') else {
            panic!("Table of Contents entryのアンカーに閉じ括弧がありません: {trimmed}");
        };
        let anchor = &suffix[..close_paren];
        entries.push((title.to_string(), anchor.to_string()));
    }

    entries
}

#[test]
fn language_guide_table_of_contents_is_in_sync() {
    let actual = parse_table_of_contents(LANGUAGE_GUIDE);
    assert!(
        !actual.is_empty(),
        "docs/language-guide-en.md からTable of Contentsを抽出できませんでした。見出しの形式を確認してください"
    );

    let expected: Vec<(String, String)> = EXPECTED_TOC
        .iter()
        .map(|(title, anchor)| (title.to_string(), anchor.to_string()))
        .collect();

    assert_eq!(
        actual, expected,
        "docs/language-guide-en.md のTable of Contentsが期待リストと一致しません。ドキュメントを更新するか、EXPECTED_TOCを再同期してください"
    );

    for (title, _) in EXPECTED_TOC {
        let heading = format!("## {title}");
        assert!(
            LANGUAGE_GUIDE.contains(&heading),
            "docs/language-guide-en.md に見出し `{heading}` が存在しません"
        );
    }
}
