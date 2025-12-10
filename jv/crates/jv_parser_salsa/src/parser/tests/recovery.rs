use crate::{
    Span,
    lexer::{TokenDiagnostic, TokenKind, TokenMetadata, TokenTrivia},
    parser::{OwnedToken, parse},
};

fn tok(kind: TokenKind, lexeme: &str) -> OwnedToken {
    OwnedToken {
        kind,
        span: Span { start: 0, end: 0 },
        lexeme: lexeme.to_string(),
        leading_trivia: TokenTrivia::default(),
        metadata: Vec::<TokenMetadata>::new(),
        diagnostic: None::<TokenDiagnostic>,
    }
}

#[test]
fn recovers_on_unexpected_tokens() {
    let tokens = vec![
        tok(TokenKind::RightBrace, "}"), // unexpected
        tok(TokenKind::Val, "val"),
        tok(TokenKind::Identifier, "x"),
        tok(TokenKind::Assign, "="),
        tok(TokenKind::Number, "1"),
        tok(TokenKind::Semicolon, ";"),
        tok(TokenKind::Eof, ""),
    ];

    let result = parse(tokens);
    assert!(
        result.output.recovered,
        "parser should mark recovered when encountering stray brace"
    );
}
