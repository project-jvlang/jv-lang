use crate::{
    lexer::{TokenDiagnostic, TokenKind, TokenMetadata, TokenTrivia},
    parser::{parse, ParseEvent, OwnedToken, SyntaxKind},
    Span,
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
fn parses_basic_statements() {
    let tokens = vec![
        tok(TokenKind::Package, "package"),
        tok(TokenKind::Identifier, "main"),
        tok(TokenKind::Semicolon, ";"),
        tok(TokenKind::Import, "import"),
        tok(TokenKind::Identifier, "java"),
        tok(TokenKind::Dot, "."),
        tok(TokenKind::Identifier, "util"),
        tok(TokenKind::Semicolon, ";"),
        tok(TokenKind::Val, "val"),
        tok(TokenKind::Identifier, "x"),
        tok(TokenKind::Assign, "="),
        tok(TokenKind::Number, "1"),
        tok(TokenKind::Semicolon, ";"),
        tok(TokenKind::Eof, ""),
    ];

    let result = parse(tokens);
    assert!(
        result.errors.is_empty(),
        "lexing errors should be empty: {:?}",
        result.errors
    );
    assert!(
        result
            .output
            .events
            .iter()
            .any(|e| matches!(e, ParseEvent::StartNode { kind: SyntaxKind::PackageDeclaration })),
        "package node should be emitted"
    );
    assert!(
        result
            .output
            .events
            .iter()
            .any(|e| matches!(e, ParseEvent::StartNode { kind: SyntaxKind::ValDeclaration })),
        "val declaration should be parsed"
    );
}

#[test]
fn parses_function_and_class() {
    let tokens = vec![
        tok(TokenKind::Fun, "fun"),
        tok(TokenKind::Identifier, "main"),
        tok(TokenKind::LeftParen, "("),
        tok(TokenKind::RightParen, ")"),
        tok(TokenKind::LeftBrace, "{"),
        tok(TokenKind::RightBrace, "}"),
        tok(TokenKind::Class, "class"),
        tok(TokenKind::Identifier, "User"),
        tok(TokenKind::LeftBrace, "{"),
        tok(TokenKind::RightBrace, "}"),
        tok(TokenKind::Eof, ""),
    ];

    let result = parse(tokens);
    let kinds: Vec<SyntaxKind> = result
        .output
        .events
        .iter()
        .filter_map(|e| match e {
            ParseEvent::StartNode { kind } => Some(kind.clone()),
            _ => None,
        })
        .collect();
    assert!(kinds.contains(&SyntaxKind::FunctionDeclaration));
    assert!(kinds.contains(&SyntaxKind::ClassDeclaration));
}
