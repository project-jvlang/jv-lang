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

#[test]
fn parses_deeply_nested_constructs() {
    let source = r#"
        package deep.example.core
        import foo.bar.*
        import util.logger

        val threshold: Int = 10

        class Complex {
            fun process(items: List<Int>) {
                for (item in items) {
                    when (item) {
                        0 -> continue
                        else -> {
                            var flag = false
                            do {
                                val sentinel = threshold
                            } while (flag)

                            if (flag) {
                                throw IllegalStateException()
                            }
                        }
                    }
                }

                while (true) {
                    break
                }

                return
            }

            class Nested {
                fun excite() {
                    val message = "ready"
                }
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
    assert!(
        !output.recovered,
        "expected parser to avoid recovery for complex constructs"
    );

    let started: Vec<SyntaxKind> = output
        .events
        .iter()
        .filter_map(|event| match event {
            ParseEvent::StartNode { kind } => Some(*kind),
            _ => None,
        })
        .collect();

    for expected in [
        SyntaxKind::PackageDeclaration,
        SyntaxKind::ImportDeclaration,
        SyntaxKind::ValDeclaration,
        SyntaxKind::ClassDeclaration,
        SyntaxKind::FunctionDeclaration,
        SyntaxKind::ForStatement,
        SyntaxKind::WhenStatement,
        SyntaxKind::DoWhileStatement,
        SyntaxKind::IfStatement,
        SyntaxKind::WhileStatement,
        SyntaxKind::ReturnStatement,
        SyntaxKind::ThrowStatement,
        SyntaxKind::BreakStatement,
        SyntaxKind::ContinueStatement,
    ] {
        assert!(
            started.contains(&expected),
            "expected {:?} node in complex program",
            expected
        );
    }
}
