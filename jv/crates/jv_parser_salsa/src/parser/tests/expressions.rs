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
fn respects_basic_precedence() {
    // 1 + 2 * 3
    let tokens = vec![
        tok(TokenKind::Number, "1"),
        tok(TokenKind::Plus, "+"),
        tok(TokenKind::Number, "2"),
        tok(TokenKind::Multiply, "*"),
        tok(TokenKind::Number, "3"),
        tok(TokenKind::Eof, ""),
    ];

    let result = parse(tokens);
    assert!(
        !result.output.recovered,
        "expression parsing should not trigger recovery"
    );
    assert!(
        result.errors.is_empty(),
        "lexing errors should be empty for expression test"
    );
}
