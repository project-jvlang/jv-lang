use crate::preprocess;
use jv_lexer::{LayoutSequenceKind, Lexer, TokenMetadata, TokenType};

fn preprocess_tokens(source: &str) -> Vec<jv_lexer::Token> {
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().expect("lexing should succeed");
    let (tokens, diagnostics, halted_stage) = preprocess::run(tokens).into_parts();

    assert!(
        diagnostics.is_empty(),
        "expected no preprocess diagnostics, found {:?}",
        diagnostics
    );
    assert!(
        halted_stage.is_none(),
        "pipeline should not halt for fixture `{}`",
        source
    );

    tokens
}

#[test]
fn layout_stage_inserts_layout_commas_in_arrays() {
    let tokens = preprocess_tokens("val numbers = [1 2 3]");

    let layout_commas: Vec<_> = tokens
        .iter()
        .filter(|token| matches!(token.token_type, TokenType::LayoutComma))
        .collect();

    assert_eq!(
        layout_commas.len(),
        2,
        "expected two synthetic layout commas, found {:?}",
        layout_commas
    );

    for synthetic in layout_commas {
        let metadata = synthetic
            .metadata
            .iter()
            .find_map(|meta| match meta {
                TokenMetadata::LayoutComma(metadata) => Some(metadata),
                _ => None,
            })
            .expect("layout comma tokens should carry layout metadata");
        assert_eq!(
            metadata.sequence,
            LayoutSequenceKind::Array,
            "layout metadata must record array context"
        );
    }
}

#[test]
fn comments_stage_marks_comments_inside_call_sequences() {
    let tokens = preprocess_tokens("sum(1 /* keep */ 2)");

    let number_two = tokens
        .iter()
        .find(|token| token.lexeme == "2")
        .expect("expected to find literal `2` in token stream");

    assert!(
        number_two.leading_trivia.comments,
        "trailing comment in call arguments should be recorded as trivia"
    );
}

#[test]
fn layout_commas_respect_existing_separators() {
    let tokens = preprocess_tokens("[1, 2 3]");

    let mut layout_indices = Vec::new();
    let mut comma_indices = Vec::new();

    for (idx, token) in tokens.iter().enumerate() {
        match token.token_type {
            TokenType::LayoutComma => layout_indices.push(idx),
            TokenType::Comma => comma_indices.push(idx),
            _ => {}
        }
    }

    assert_eq!(
        comma_indices.len(),
        1,
        "fixture should contain exactly one explicit comma"
    );
    assert_eq!(
        layout_indices.len(),
        1,
        "pipeline should insert a single synthetic comma after the explicit separator"
    );

    let explicit_index = comma_indices[0];
    let synthetic_index = layout_indices[0];
    assert!(
        synthetic_index > explicit_index,
        "synthetic layout comma must appear after the explicit separator"
    );
}
