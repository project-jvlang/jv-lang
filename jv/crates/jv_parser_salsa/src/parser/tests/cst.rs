use crate::{
    lexer::{TokenDiagnostic, TokenKind, TokenMetadata, TokenTrivia},
    parser::{cst::CstBuilder, OwnedToken, SyntaxKind},
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
fn builds_lossless_tree() {
    let mut builder = CstBuilder::new();
    builder.start_node(SyntaxKind::Root);
    builder.push_token(tok(TokenKind::Identifier, "x"));
    builder.finish_node();
    let tree = builder.build();
    assert!(tree.is_some(), "builder should produce a root node");
    let root = tree.unwrap();
    assert_eq!(root.kind, SyntaxKind::Root);
    assert!(
        !root.children.is_empty(),
        "root should contain the pushed token"
    );
}
