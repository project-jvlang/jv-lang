use jv_lexer::{Lexer, Token};
use jv_parser::preprocess;

fn preprocess_source(source: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().expect("lexing fixture should succeed");
    let (tokens, diagnostics, halted_stage) = preprocess::run(tokens).into_parts();

    assert!(
        diagnostics.is_empty(),
        "stage0 diagnostics should remain empty for fixture: {:?}",
        diagnostics
    );
    assert!(
        halted_stage.is_none(),
        "stage0 pipeline should not halt for fixture input"
    );

    tokens
}

fn describe_tokens(tokens: &[Token]) -> String {
    tokens
        .iter()
        .enumerate()
        .map(|(index, token)| {
            let metadata = if token.metadata.is_empty() {
                "[]".to_string()
            } else {
                let parts: Vec<String> = token
                    .metadata
                    .iter()
                    .map(|meta| format!("{:?}", meta))
                    .collect();
                format!("[{}]", parts.join(", "))
            };

            format!(
                "{index:02}: {:?} `{}` comments={} metadata={}",
                token.token_type, token.lexeme, token.leading_trivia.comments, metadata
            )
        })
        .collect::<Vec<_>>()
        .join("\n")
}

#[test]
fn stage0_array_and_call_tokens_snapshot() {
    let source = include_str!("fixtures/preprocess/layout_array.jv");
    let tokens = preprocess_source(source);
    let snapshot = describe_tokens(&tokens);

    insta::assert_snapshot!("stage0_layout_tokens", snapshot);
}
