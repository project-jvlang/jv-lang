use crate::{
    Span,
    db::{Database, FileInput, preprocess},
    lexer::{TokenDiagnostic, TokenKind, TokenMetadata, TokenTrivia},
    parser::{DiagnosticSeverity, OwnedToken, ParseEvent, SyntaxKind, parse},
};
use std::sync::Arc;

fn tok(kind: TokenKind, lexeme: &str) -> OwnedToken {
    OwnedToken {
        kind,
        span: Span { start: 0, end: 0 },
        lexeme: lexeme.to_string().into(),
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
        result.output.events.iter().any(|e| matches!(
            e,
            ParseEvent::StartNode {
                kind: SyntaxKind::PackageDeclaration
            }
        )),
        "package node should be emitted"
    );
    assert!(
        result.output.events.iter().any(|e| matches!(
            e,
            ParseEvent::StartNode {
                kind: SyntaxKind::ValDeclaration
            }
        )),
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

#[test]
fn parses_function_without_params_list() {
    let tokens = vec![
        tok(TokenKind::Fun, "fun"),
        tok(TokenKind::Identifier, "main"),
        tok(TokenKind::LeftBrace, "{"),
        tok(TokenKind::RightBrace, "}"),
        tok(TokenKind::Eof, ""),
    ];

    let result = parse(tokens);
    assert!(
        result.errors.is_empty(),
        "lexing errors should be empty: {:?}",
        result.errors
    );
    assert!(
        result.output.events.iter().any(|e| matches!(
            e,
            ParseEvent::StartNode {
                kind: SyntaxKind::FunctionDeclaration
            }
        )),
        "function declaration should be parsed"
    );
}

#[test]
fn reports_error_for_if_expression() {
    let tokens = vec![
        tok(TokenKind::If, "if"),
        tok(TokenKind::BooleanTrue, "true"),
        tok(TokenKind::LeftBrace, "{"),
        tok(TokenKind::RightBrace, "}"),
        tok(TokenKind::Eof, ""),
    ];

    let result = parse(tokens);
    assert!(
        result
            .output
            .diagnostics
            .iter()
            .any(|d| d.message.contains("JV3103")),
        "expected JV3103 diagnostic, got {:?}",
        result.output.diagnostics
    );
    assert!(
        result.output.recovered,
        "parser should mark recovered on unsupported if"
    );
}

#[test]
fn debug_preprocess_token_kinds_for_when_is_pattern() {
    let source = r#"
fun provide(): String? = null

maybe = provide()
val fallback = "fallback"
label = when (maybe) {
    is String -> maybe
    else -> fallback
}
"#;

    let db = Database::new();
    let file = FileInput::new(&db, Arc::from("<memory>"), Arc::from(source));
    let output = preprocess(&db, file);

    let is_positions: Vec<_> = output
        .tokens
        .iter()
        .enumerate()
        .filter_map(|(idx, tok)| {
            if tok.lexeme.as_ref() == "is" {
                Some((idx, tok.line, tok.column, tok.token_type.clone()))
            } else {
                None
            }
        })
        .collect();
    assert!(
        !is_positions.is_empty(),
        "expected to find `is` token, got {:?}",
        output.tokens
    );

    for (idx, line, column, token_type) in is_positions {
        assert!(
            matches!(token_type, jv_lexer::TokenType::Identifier(_)),
            "expected `is` to be lexed as Identifier, got {token_type:?} at {line}:{column}; around={:?}",
            output
                .tokens
                .get(idx.saturating_sub(3)..=(idx + 3).min(output.tokens.len() - 1))
        );
    }

    let at_branch_start: Vec<_> = output
        .tokens
        .iter()
        .enumerate()
        .filter_map(|(idx, tok)| {
            if tok.line == 7 && tok.column == 5 {
                Some((idx, tok.lexeme.as_ref(), tok.token_type.clone()))
            } else {
                None
            }
        })
        .collect();
    assert_eq!(
        at_branch_start.len(),
        2,
        "expected LayoutComma + `is` tokens at 7:5, got {at_branch_start:?}"
    );
    assert!(
        matches!(at_branch_start[0].2, jv_lexer::TokenType::LayoutComma),
        "expected LayoutComma at 7:5, got {at_branch_start:?}"
    );
    assert!(
        at_branch_start[1].1 == "is",
        "expected `is` at 7:5, got {at_branch_start:?}"
    );

    let owned_tokens: Vec<OwnedToken> = output
        .tokens
        .iter()
        .map(|tok| OwnedToken {
            kind: crate::lexer::kind_from_token_type(&tok.token_type),
            span: Span { start: 0, end: 0 },
            lexeme: tok.lexeme.clone(),
            leading_trivia: tok.leading_trivia.clone(),
            metadata: tok.metadata.clone(),
            diagnostic: tok.diagnostic.clone(),
        })
        .collect();
    let result = parse(owned_tokens);
    let started_when = result.output.events.iter().any(|event| {
        matches!(
            event,
            ParseEvent::StartNode {
                kind: SyntaxKind::WhenStatement
            }
        )
    });
    let when_branch_count = result
        .output
        .events
        .iter()
        .filter(|event| {
            matches!(
                event,
                ParseEvent::StartNode {
                    kind: SyntaxKind::WhenBranch
                }
            )
        })
        .count();
    let error = result
        .output
        .diagnostics
        .iter()
        .find(|d| d.severity == DiagnosticSeverity::Error);
    assert!(
        error.is_none(),
        "expected no parser errors, got {error:?}; started_when={started_when}, when_branch_count={when_branch_count}, around={:?}",
        error.map(|d| {
            let start = d.span.start;
            result
                .tokens
                .get(start.saturating_sub(5)..(start + 5).min(result.tokens.len()))
        })
    );
}
