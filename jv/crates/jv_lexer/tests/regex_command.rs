//! RegexCommand を lex する際のトークン分解を検証する統合テスト。

use jv_lexer::{Lexer, Token, TokenMetadata, TokenType};

fn tokenize(source: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(source.to_string());
    lexer
        .tokenize()
        .expect("正規表現コマンドが lex できること")
}

fn extract_regex_token(tokens: &[Token]) -> &Token {
    tokens
        .iter()
        .find(|token| matches!(token.token_type, TokenType::RegexLiteral(_)))
        .expect("パターン部の RegexLiteral トークンが生成されること")
}

#[test]
fn all_mode_with_literal_replacement_and_flags() {
    let tokens = tokenize(r#"a/text/\d+/"X"/im"#);

    // 先頭にモード指定が識別子として lex されること。
    assert!(
        matches!(tokens.first().map(|t| &t.token_type), Some(TokenType::Identifier(value)) if value == "a"),
        "モード指定が Identifier として lex される想定です: {tokens:?}"
    );

    // subject とパターンの区切りが TokenType::Divide で保持されること。
    assert!(
        tokens
            .iter()
            .any(|token| matches!(token.token_type, TokenType::Divide)),
        "subject と pattern の境界に `/` トークンが出力される想定です: {tokens:?}"
    );

    let regex_token = extract_regex_token(&tokens);
    assert_eq!(regex_token.lexeme, "\\d+");

    let (raw, pattern) = regex_token
        .metadata
        .iter()
        .find_map(|meta| match meta {
            TokenMetadata::RegexLiteral { raw, pattern } => Some((raw, pattern)),
            _ => None,
        })
        .expect("RegexLiteral メタデータが付与されること");
    assert_eq!(raw, "/\\d+/");
    assert_eq!(pattern, "\\d+");

    assert!(
        tokens.iter().any(|token| matches!(&token.token_type, TokenType::String(value) if value == "X")),
        "置換部の文字列が String トークンとして lex されることを期待しました: {tokens:?}"
    );
    assert!(
        tokens.iter().any(|token| matches!(&token.token_type, TokenType::Identifier(value) if value == "im")),
        "フラグ列 `im` が Identifier として保持されることを期待しました: {tokens:?}"
    );
}

#[test]
fn first_mode_without_replacement_preserves_subject_expression() {
    let tokens = tokenize("f/(subject.trim())/[a-z]+/");

    // subject の括弧や呼び出しが command 判定の中で保持されること。
    let regex_index = tokens
        .iter()
        .position(|token| matches!(token.token_type, TokenType::RegexLiteral(_)))
        .expect("RegexLiteral トークンが存在すること");
    let subject_tokens: Vec<_> = tokens[..regex_index]
        .iter()
        .map(|token| token.token_type.clone())
        .collect();
    assert!(
        subject_tokens.iter().any(|token| matches!(token, TokenType::Identifier(name) if name == "subject")),
        "subject の識別子が識別子トークンとして残る想定です: {subject_tokens:?}"
    );
    assert!(
        subject_tokens.iter().any(|token| matches!(token, TokenType::Identifier(name) if name == "trim")),
        "メソッド呼び出しの識別子 `trim` が保持される必要があります: {subject_tokens:?}"
    );

    let regex_token = extract_regex_token(&tokens);
    assert_eq!(regex_token.lexeme, "[a-z]+");
}

#[test]
fn iterate_mode_with_lambda_replacement_keeps_pattern_literal() {
    let tokens = tokenize(r#"i/text/\w+/${ it.group(1).toUpperCase() }/"#);

    // パターン部が単一の RegexLiteral トークンとして検出されること。
    let regex_token = extract_regex_token(&tokens);
    assert_eq!(regex_token.lexeme, "\\w+");

    // ラムダ置換の先頭トークン（`${`）が tokenize 済みであることを簡易的に確認する。
    assert!(
        tokens.iter().any(|token| matches!(&token.token_type, TokenType::Identifier(value) if value == "it")),
        "ラムダ本体内の `it` が通常の識別子として lex される想定です: {tokens:?}"
    );
}
