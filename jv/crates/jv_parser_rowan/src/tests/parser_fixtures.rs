use jv_lexer::Lexer;
use rowan::SyntaxNode;

use crate::parser::parse;
use crate::syntax::SyntaxKind;
use crate::{JvLanguage, ParseBuilder, ParseEvent};

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

#[test]
fn build_tree_from_events_handles_deep_nesting() {
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
    assert!(output.diagnostics.is_empty(), "unexpected diagnostics: {:?}", output.diagnostics);

    let tree: SyntaxNode<JvLanguage> = SyntaxNode::new_root(ParseBuilder::build_from_events(&output.events, &tokens));

    assert_eq!(tree.kind(), SyntaxKind::Root, "root node kind mismatch");

    // Root should contain at least one StatementList node.
    assert!(tree.descendants().any(|node| node.kind() == SyntaxKind::StatementList));

    // Verify nested control flow constructs remain beneath the function declaration.
    let when_node = tree
        .descendants()
        .find(|node| node.kind() == SyntaxKind::WhenStatement)
        .expect("expected when statement in nested sample");
    assert!(
        when_node
            .ancestors()
            .any(|ancestor| ancestor.kind() == SyntaxKind::FunctionDeclaration),
        "when statement should be nested within a function declaration"
    );

    let do_while_node = tree
        .descendants()
        .find(|node| node.kind() == SyntaxKind::DoWhileStatement)
        .expect("expected do-while statement in nested sample");
    assert!(
        do_while_node
            .ancestors()
            .any(|ancestor| ancestor.kind() == SyntaxKind::FunctionDeclaration),
        "do-while statement should be nested within a function declaration"
    );

    // Ensure class nesting is preserved (inner class inside outer class body).
    assert!(
        tree.descendants().any(|node| {
            node.kind() == SyntaxKind::ClassDeclaration
                && node
                    .ancestors()
                    .skip(1)
                    .any(|ancestor| ancestor.kind() == SyntaxKind::ClassDeclaration)
        }),
        "expected to find a class declaration nested within another class"
    );
}

#[test]
fn build_tree_from_events_preserves_error_nodes() {
    let tokens = lex(
        r#"
        val = 0
        return
    "#,
    );

    let output = parse(&tokens);
    assert!(output.recovered, "parser should have marked recovery for invalid input");

    let tree: SyntaxNode<JvLanguage> = SyntaxNode::new_root(ParseBuilder::build_from_events(&output.events, &tokens));
    assert!(
        tree.descendants()
            .any(|node| matches!(node.kind(), SyntaxKind::Error | SyntaxKind::BlockError)),
        "expected error nodes to be present in the constructed tree"
    );
}
