use super::lex;
use crate::run;
use jv_lexer::{LayoutSequenceKind, TokenMetadata, TokenType};

fn preprocess_tokens(source: &str) -> Vec<jv_lexer::Token> {
    let tokens = lex(source);
    let (tokens, diagnostics, halted_stage) = run(tokens).into_parts();

    assert!(
        diagnostics.is_empty(),
        "前処理段階で診断が発生しないこと: {:?}",
        diagnostics
    );
    assert!(
        halted_stage.is_none(),
        "前処理パイプラインが中断しないこと: {:?}",
        halted_stage
    );

    tokens
}

fn extract_layout_commas(tokens: &[jv_lexer::Token]) -> Vec<&jv_lexer::Token> {
    tokens
        .iter()
        .filter(|token| matches!(token.token_type, TokenType::LayoutComma))
        .collect()
}

#[test]
fn array_layout_commas_carry_array_metadata() {
    let tokens = preprocess_tokens("val numbers = [1 2 3]");
    let layout_commas = extract_layout_commas(&tokens);

    assert_eq!(
        layout_commas.len(),
        2,
        "配列内の空白区切り要素には2個のレイアウトコンマが挿入されるべき"
    );

    for synthetic in layout_commas {
        let metadata = synthetic
            .metadata
            .iter()
            .find_map(|meta| match meta {
                TokenMetadata::LayoutComma(layout) => Some(layout),
                _ => None,
            })
            .expect("LayoutCommaメタデータが付与されていること");
        assert_eq!(
            metadata.sequence,
            LayoutSequenceKind::Array,
            "配列コンテキストがLayoutSequenceKind::Arrayとして記録されること"
        );
    }
}

#[test]
fn explicit_commas_are_preserved_before_layout_commas() {
    let tokens = preprocess_tokens("[1, 2 3]");

    let mut explicit_indices = Vec::new();
    let mut layout_indices = Vec::new();

    for (idx, token) in tokens.iter().enumerate() {
        match token.token_type {
            TokenType::Comma => explicit_indices.push(idx),
            TokenType::LayoutComma => layout_indices.push(idx),
            _ => {}
        }
    }

    assert_eq!(explicit_indices.len(), 1, "明示的コンマは1個のみのはず");
    assert_eq!(
        layout_indices.len(),
        1,
        "合成レイアウトコンマは1個のみのはず"
    );

    let explicit = explicit_indices[0];
    let synthetic = layout_indices[0];
    assert!(
        synthetic > explicit,
        "合成レイアウトコンマは明示的コンマの後ろに挿入されるべき"
    );
}

#[test]
fn comments_inside_call_sequences_are_preserved() {
    let tokens = preprocess_tokens("sum(1 /* keep */ 2)");

    let literal_two = tokens
        .iter()
        .find(|token| token.lexeme == "2")
        .expect("リテラル'2'トークンが存在すること");

    assert!(
        literal_two.leading_trivia.comments,
        "呼び出し引数内のコメントはトリビアとして保持されること"
    );
}

#[test]
fn when_arms_receive_layout_commas_with_when_metadata() {
    let tokens = preprocess_tokens(
        r#"
when (value) {
    1 -> foo()
    2 -> bar()
}
"#,
    );

    let when_commas: Vec<_> = extract_layout_commas(&tokens)
        .into_iter()
        .filter(|token| {
            token.metadata.iter().any(|meta| match meta {
                TokenMetadata::LayoutComma(layout) => layout.sequence == LayoutSequenceKind::When,
                _ => false,
            })
        })
        .collect();

    assert_eq!(
        when_commas.len(),
        2,
        "whenアームごとにレイアウトコンマが付与されるべき"
    );
}
