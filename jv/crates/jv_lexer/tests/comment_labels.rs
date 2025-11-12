//! コメントラベル機能の検証。

use jv_lexer::{FieldNameLabelErrorKind, FieldNameLabelKind, Lexer, TokenDiagnostic, TokenType};

fn 字句解析結果(ソース: &str) -> Vec<jv_lexer::Token> {
    let mut 字句解析器 = Lexer::new(ソース.to_string());
    字句解析器
        .tokenize()
        .expect("字句解析が失敗しないことを期待")
}

#[test]
fn 行コメントからラベルを抽出する() {
    let ソース = "value // result";
    let トークン列 = 字句解析結果(ソース);

    let (ラベルトークン, ペイロード) = トークン列
        .iter()
        .find_map(|トークン| match &トークン.token_type {
            TokenType::FieldNameLabel(ペイロード) => Some((トークン, ペイロード)),
            _ => None,
        })
        .expect("FieldNameLabelトークンが生成されること");

    assert_eq!(
        ペイロード.primary.as_deref(),
        Some("result"),
        "行コメントから識別子のみを抽出できること"
    );

    let 主スパン = ペイロード
        .primary_span
        .as_ref()
        .expect("主ラベルのスパン情報を保持すること");
    assert_eq!(主スパン.kind, FieldNameLabelKind::LineComment);
    assert_eq!(
        主スパン.token_distance,
        Some(1),
        "直前の実トークンとの差分が1であること"
    );
    assert_eq!(ラベルトークン.line, 主スパン.line);
    assert_eq!(ラベルトークン.column, 主スパン.column);
    assert_eq!(ラベルトークン.lexeme, "result");
}

#[test]
fn ブロックコメントからラベルを抽出する() {
    let ソース = "value /* result */";
    let トークン列 = 字句解析結果(ソース);

    let (_, ペイロード) = トークン列
        .iter()
        .find_map(|トークン| match &トークン.token_type {
            TokenType::FieldNameLabel(ペイロード) => Some((トークン, ペイロード)),
            _ => None,
        })
        .expect("ブロックコメントでもラベルが生成されること");

    assert_eq!(
        ペイロード.primary.as_deref(),
        Some("result"),
        "ブロックコメントから識別子のみを抽出できること"
    );
    let 主スパン = ペイロード
        .primary_span
        .as_ref()
        .expect("主ラベルのスパン情報を保持すること");
    assert_eq!(主スパン.kind, FieldNameLabelKind::BlockComment);
}

#[test]
fn 複数のコメントラベルで距離優先度を保持する() {
    let ソース = "value // primary\n// secondary\nnext";
    let トークン列 = 字句解析結果(ソース);

    let ラベル一覧: Vec<_> = トークン列
        .iter()
        .filter_map(|トークン| match &トークン.token_type {
            TokenType::FieldNameLabel(ペイロード) => Some((トークン, ペイロード)),
            _ => None,
        })
        .collect();

    assert_eq!(ラベル一覧.len(), 2, "2つのラベルが検出されること");
    assert_eq!(
        ラベル一覧[0].1.primary.as_deref(),
        Some("primary"),
        "直近のコメントが優先されること"
    );
    assert_eq!(
        ラベル一覧[1].1.primary.as_deref(),
        Some("secondary"),
        "より遠いコメントも候補として保持されること"
    );

    let 距離一覧: Vec<_> = ラベル一覧
        .iter()
        .map(|(_, ペイロード)| {
            ペイロード
                .primary_span
                .as_ref()
                .and_then(|スパン| スパン.token_distance)
        })
        .collect();
    assert_eq!(
        距離一覧,
        vec![Some(1), Some(2)],
        "直近コメントほど距離が小さく記録されること"
    );
}

#[test]
fn 識別子以外は診断を報告する() {
    let ソース = "value // 1invalid";
    let トークン列 = 字句解析結果(ソース);

    let コメントトークン = トークン列
        .iter()
        .find(|トークン| matches!(トークン.token_type, TokenType::LineComment(_)))
        .expect("コメントトークンが存在すること");
    assert!(
        !トークン列
            .iter()
            .any(|トークン| matches!(トークン.token_type, TokenType::FieldNameLabel(_))),
        "無効な識別子の場合はFieldNameLabelが生成されないこと"
    );

    let 診断 = コメントトークン
        .diagnostic
        .as_ref()
        .expect("無効な識別子は診断を伴うこと");
    match 診断 {
        TokenDiagnostic::InvalidFieldNameLabel { reason, text, .. } => {
            assert_eq!(reason, &FieldNameLabelErrorKind::InvalidIdentifier);
            assert_eq!(text, "1invalid");
        }
        _ => panic!("期待外の診断が報告された: {:?}", 診断),
    }
}

#[test]
fn 余分なテキストも診断対象とする() {
    let ソース = "value // not valid";
    let トークン列 = 字句解析結果(ソース);

    let コメントトークン = トークン列
        .iter()
        .find(|トークン| matches!(トークン.token_type, TokenType::LineComment(_)))
        .expect("コメントトークンが存在すること");
    assert!(
        !トークン列
            .iter()
            .any(|トークン| matches!(トークン.token_type, TokenType::FieldNameLabel(_))),
        "余分なテキストを含む場合はラベル化しないこと"
    );

    let 診断 = コメントトークン
        .diagnostic
        .as_ref()
        .expect("余分なテキストも診断対象となること");
    match 診断 {
        TokenDiagnostic::InvalidFieldNameLabel { reason, text, .. } => {
            assert_eq!(reason, &FieldNameLabelErrorKind::ExtraText);
            assert_eq!(text, "not valid");
        }
        _ => panic!("期待外の診断が報告された: {:?}", 診断),
    }
}
