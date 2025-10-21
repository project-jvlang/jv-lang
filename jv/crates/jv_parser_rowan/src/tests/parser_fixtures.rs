use jv_lexer::Lexer;

use crate::parser::{parse, ParseEvent};
use crate::syntax::SyntaxKind;

fn lex(input: &str) -> Vec<jv_lexer::Token> {
    let mut lexer = Lexer::new(input.to_string());
    lexer.tokenize().expect("lexing to succeed")
}

#[test]
fn parses_core_statements() {
    let source = r#"
        package com.example
        import foo.bar

        val answer: Int = 42

        fun greet(name: String) {
            if (name == null) {
                return
            } else if (name == "admin") {
                break
            }

            when (name) {
                "guest" -> return
                else -> throw IllegalStateException()
            }
        }
    "#;

    let tokens = lex(source);
    let output = parse(&tokens);

    assert!(
        output.diagnostics.is_empty(),
        "expected no diagnostics, got {:?}",
        output.diagnostics
    );

    let started: Vec<SyntaxKind> = output
        .events
        .iter()
        .filter_map(|event| match event {
            ParseEvent::StartNode { kind } => Some(*kind),
            _ => None,
        })
        .collect();
    assert!(started.contains(&SyntaxKind::PackageDeclaration));
    assert!(started.contains(&SyntaxKind::ImportDeclaration));
    assert!(started.contains(&SyntaxKind::ValDeclaration));
    assert!(started.contains(&SyntaxKind::FunctionDeclaration));
    assert!(started.contains(&SyntaxKind::IfStatement));
    assert!(started.contains(&SyntaxKind::WhenStatement));
}

#[test]
fn recovers_from_invalid_val() {
    let source = r#"
        val = 0
        return
    "#;

    let tokens = lex(source);
    let output = parse(&tokens);

    assert!(
        output.recovered,
        "expected parser to mark recovery when encountering invalid val"
    );
    assert!(
        !output.diagnostics.is_empty(),
        "expected diagnostics for invalid val declaration"
    );

    let has_error_node = output.events.iter().any(|event| match event {
        ParseEvent::StartNode { kind } => {
            matches!(kind, SyntaxKind::Error | SyntaxKind::BlockError)
        }
        _ => false,
    });
    assert!(has_error_node, "expected error node in event stream");
}
