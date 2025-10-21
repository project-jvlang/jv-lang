use crate::lowering::{lower_program, LoweringDiagnosticSeverity};
use crate::parser::{parse, ParseEvent};
use crate::{JvLanguage, ParseBuilder, SyntaxKind};
use jv_lexer::{Lexer, Token};
use rowan::SyntaxNode;

fn lex(source: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(source.to_string());
    lexer.tokenize().expect("lexing to succeed")
}

fn build_root(tokens: &[Token], events: &[ParseEvent]) -> SyntaxNode<JvLanguage> {
    let mut builder = ParseBuilder::new();
    for event in events {
        match event {
            ParseEvent::StartNode { kind } => builder.start_node(*kind),
            ParseEvent::FinishNode => builder.finish_node(),
            ParseEvent::Token { kind, token_index } => {
                let text = tokens
                    .get(*token_index)
                    .map(|token| token.lexeme.as_str())
                    .unwrap_or_default();
                builder.builder().token((*kind).into(), text);
            }
            ParseEvent::Error { .. } => {}
        }
    }

    SyntaxNode::new_root(builder.finish())
}

#[test]
fn returns_unimplemented_diagnostics_for_supported_nodes() {
    let source = r#"
        package demo.core
        val answer: Int = 42
    "#;
    let tokens = lex(source);
    let output = parse(&tokens);
    assert!(
        output.diagnostics.is_empty(),
        "expected parser diagnostics to be empty: {:?}",
        output.diagnostics
    );
    let root = build_root(&tokens, &output.events);

    let lowering = lower_program(&root, &tokens);
    assert!(lowering.statements.is_empty());
    let kinds: Vec<_> = lowering
        .diagnostics
        .iter()
        .map(|diag| diag.node_kind)
        .collect();
    assert!(kinds.contains(&SyntaxKind::PackageDeclaration));
    assert!(kinds.contains(&SyntaxKind::ValDeclaration));
    for diagnostic in &lowering.diagnostics {
        assert_eq!(diagnostic.severity, LoweringDiagnosticSeverity::Error);
        assert!(
            diagnostic.span.is_some(),
            "expected span for {:?}",
            diagnostic.node_kind
        );
    }
}

#[test]
fn reports_diagnostic_for_invalid_statement() {
    let source = "val = 0";
    let tokens = lex(source);
    let output = parse(&tokens);
    let root = build_root(&tokens, &output.events);

    let lowering = lower_program(&root, &tokens);
    assert!(
        lowering.statements.is_empty(),
        "expected no lowered statements, got {:?}",
        lowering.statements
    );
    assert!(
        lowering
            .diagnostics
            .iter()
            .any(|diag| diag.node_kind == SyntaxKind::Error),
        "expected error node diagnostic, got {:?}",
        lowering.diagnostics
    );
}
