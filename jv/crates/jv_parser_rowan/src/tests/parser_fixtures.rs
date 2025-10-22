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

        fun greet(names: List<String>) {
            for (name in names) {
                when (name) {
                    "guest" -> continue
                    "admin" -> return
                    else -> {
                        throw IllegalStateException()
                    }
                }

                when (name.length) {
                    0 -> break
                    else -> {}
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
    assert!(started.contains(&SyntaxKind::ForStatement));
    assert!(started.contains(&SyntaxKind::WhenStatement));
    assert!(started.contains(&SyntaxKind::BreakStatement));
    assert!(started.contains(&SyntaxKind::ContinueStatement));
    assert!(started.contains(&SyntaxKind::ReturnStatement));
    assert!(started.contains(&SyntaxKind::ThrowStatement));
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

    let started: Vec<SyntaxKind> = output
        .events
        .iter()
        .filter_map(|event| match event {
            ParseEvent::StartNode { kind } => Some(*kind),
            _ => None,
        })
        .collect();
    assert!(
        started.contains(&SyntaxKind::ReturnStatement),
        "parser should resume and emit the return statement after recovery"
    );
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
                    when (item % 3) {
                        0 -> continue
                        1 -> {
                            var attempts = 0
                            for (candidate in items) {
                                attempts = attempts + candidate
                                when (attempts) {
                                    5 -> break
                                    else -> {}
                                }
                            }
                        }
                        else -> {
                            val doubled = item * 2
                            when (doubled) {
                                10 -> return
                                else -> throw IllegalArgumentException("invalid")
                            }
                        }
                    }
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
                    when (item % 3) {
                        0 -> continue
                        1 -> {
                            var attempts = 0
                            for (candidate in items) {
                                attempts = attempts + candidate
                                when (attempts) {
                                    5 -> break
                                    else -> {}
                                }
                            }
                        }
                        else -> {
                            val doubled = item * 2
                            when (doubled) {
                                10 -> return
                                else -> throw IllegalArgumentException("invalid")
                            }
                        }
                    }
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
        "unexpected diagnostics: {:?}",
        output.diagnostics
    );

    let tree: SyntaxNode<JvLanguage> =
        SyntaxNode::new_root(ParseBuilder::build_from_events(&output.events, &tokens));

    assert_eq!(tree.kind(), SyntaxKind::Root, "root node kind mismatch");

    // Root should contain at least one StatementList node.
    assert!(tree
        .descendants()
        .any(|node| node.kind() == SyntaxKind::StatementList));

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

    assert!(
        tree.descendants().any(|node| {
            node.kind() == SyntaxKind::ForStatement
                && node
                    .ancestors()
                    .skip(1)
                    .any(|ancestor| ancestor.kind() == SyntaxKind::ForStatement)
        }),
        "expected to find a nested for statement within another for loop"
    );

    assert!(
        tree.descendants().any(|node| {
            node.kind() == SyntaxKind::ThrowStatement
                && node
                    .ancestors()
                    .any(|ancestor| ancestor.kind() == SyntaxKind::FunctionDeclaration)
        }),
        "expected throw statement within function declaration"
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
fn parses_destructuring_and_expression_body() {
    let source = r#"
        val [first second] = point
        var (left right) = pair
        [name age] = user
        fun greet(name: String): String = "Hello, ${name}!"
    "#;

    let tokens = lex(source);
    let output = parse(&tokens);

    assert!(
        output.diagnostics.is_empty(),
        "expected no diagnostics, got {:?}",
        output.diagnostics
    );

    let mut seen_list_pattern = false;
    let mut seen_tuple_pattern = false;
    let mut seen_expression_body = false;

    for event in &output.events {
        if let ParseEvent::StartNode { kind } = event {
            match kind {
                SyntaxKind::BindingListPattern => seen_list_pattern = true,
                SyntaxKind::BindingTuplePattern => seen_tuple_pattern = true,
                SyntaxKind::FunctionDeclaration => seen_expression_body = true,
                _ => {}
            }
        }
    }

    assert!(seen_list_pattern, "expected destructuring list pattern");
    assert!(seen_tuple_pattern, "expected tuple pattern");
    assert!(
        seen_expression_body,
        "expected function declaration with expression body"
    );

    // Recordパターンはサポート外でエラーとなることを確認する。
    let record_source = r#"val {name: first age: second} = user"#;
    let record_tokens = lex(record_source);
    let record_output = parse(&record_tokens);
    assert!(
        !record_output.diagnostics.is_empty(),
        "expected diagnostics for record pattern"
    );
}

#[test]
fn build_tree_from_events_preserves_error_nodes() {
    let tokens = lex(r#"
        val = 0
        return
    "#);

    let output = parse(&tokens);
    assert!(
        output.recovered,
        "parser should have marked recovery for invalid input"
    );

    let tree: SyntaxNode<JvLanguage> =
        SyntaxNode::new_root(ParseBuilder::build_from_events(&output.events, &tokens));
    assert!(
        tree.descendants()
            .any(|node| matches!(node.kind(), SyntaxKind::Error | SyntaxKind::BlockError)),
        "expected error nodes to be present in the constructed tree"
    );
}

#[test]
fn nested_when_branch_emits_expected_event_sequence() {
    let source = r#"
        fun evaluate(flag: Boolean, items: List<Int>) {
            when (flag) {
                true -> for (item in items) {
                    when (item) {
                        0 -> return
                        else -> {
                            for (candidate in items) {
                                val snapshot = candidate * 2
                                when (snapshot) {
                                    0 -> break
                                    else -> {}
                                }
                            }
                        }
                    }
                }
                else -> throw IllegalStateException()
            }
        }
    "#;

    let tokens = lex(source);
    let output = parse(&tokens);

    assert!(
        output.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        output.diagnostics
    );

    let start_sequence = start_node_sequence(&output.events);
    let filtered_sequence: Vec<SyntaxKind> = start_sequence
        .iter()
        .copied()
        .filter(|kind| *kind != SyntaxKind::Expression)
        .collect();

    assert!(
        contains_subsequence(
            &filtered_sequence,
            &[
                SyntaxKind::WhenStatement,
                SyntaxKind::WhenBranch,
                SyntaxKind::ForStatement,
                SyntaxKind::WhenStatement,
                SyntaxKind::WhenBranch,
                SyntaxKind::ForStatement
            ]
        ),
        "expected nested when branch subsequence in {:?}",
        start_sequence
    );

    assert!(
        contains_subsequence(
            &filtered_sequence,
            &[
                SyntaxKind::FunctionDeclaration,
                SyntaxKind::WhenStatement,
                SyntaxKind::ThrowStatement
            ]
        ),
        "expected throw branch subsequence in {:?}",
        filtered_sequence
    );
}

#[test]
fn return_and_throw_when_expressions_are_parsed_as_single_expressions() {
    let source = r#"
        fun dispatch(value: Int): Int {
            return when (value) {
                0 -> 0
                1 -> value + 1
                else -> {
                    val doubled = value * 2
                    doubled
                }
            }
        }

        fun escalate(code: Int) {
            throw when (code) {
                0 -> IllegalStateException()
                1 -> IllegalArgumentException("bad code")
                else -> when (code % 2) {
                    0 -> RuntimeException("even")
                    else -> RuntimeException("odd")
                }
            }
        }
    "#;

    let tokens = lex(source);
    let output = parse(&tokens);

    assert!(
        output.diagnostics.is_empty(),
        "unexpected diagnostics while parsing return/throw when sample: {:?}",
        output.diagnostics
    );
    assert!(
        !output.recovered,
        "parser should not need recovery for return/throw when sample"
    );

    let tree: SyntaxNode<JvLanguage> =
        SyntaxNode::new_root(ParseBuilder::build_from_events(&output.events, &tokens));

    let return_contains_when = tree
        .descendants()
        .filter(|node| node.kind() == SyntaxKind::ReturnStatement)
        .any(|node| {
            node.descendants_with_tokens().any(|element| {
                element
                    .into_token()
                    .map(|token| token.kind() == SyntaxKind::WhenKw)
                    .unwrap_or(false)
            })
        });
    assert!(
        return_contains_when,
        "expected return statement to include a when expression"
    );

    let throw_contains_when = tree
        .descendants()
        .filter(|node| node.kind() == SyntaxKind::ThrowStatement)
        .any(|node| {
            node.descendants_with_tokens().any(|element| {
                element
                    .into_token()
                    .map(|token| token.kind() == SyntaxKind::WhenKw)
                    .unwrap_or(false)
            })
        });
    assert!(
        throw_contains_when,
        "expected throw statement to include a when expression"
    );
}

#[test]
fn missing_branch_closing_brace_recovers_with_block_error() {
    let source = r#"
        fun example(value: Int) {
            when (value) {
                else -> {
                    when (value % 2) {
                        0 -> return
                        else -> {}
                    }
                // missing brace on purpose
            }
            return
        }
    "#;

    let tokens = lex(source);
    let output = parse(&tokens);

    assert!(
        output.recovered,
        "parser should mark recovery when branch brace is missing"
    );

    assert!(
        output
            .diagnostics
            .iter()
            .any(|diag| diag.message.contains("'}' が必要です")),
        "expected diagnostics about missing closing brace, got {:?}",
        output.diagnostics
    );

    let has_block_error = output.events.iter().any(|event| match event {
        ParseEvent::StartNode { kind } => *kind == SyntaxKind::BlockError,
        _ => false,
    });
    assert!(has_block_error, "expected BlockError node to be emitted");

    let tree: SyntaxNode<JvLanguage> =
        SyntaxNode::new_root(ParseBuilder::build_from_events(&output.events, &tokens));
    let block_error = tree
        .descendants()
        .find(|node| node.kind() == SyntaxKind::BlockError)
        .expect("expected BlockError node in syntax tree");
    let mut ancestor_kinds: Vec<SyntaxKind> =
        block_error.ancestors().map(|node| node.kind()).collect();
    ancestor_kinds.retain(|kind| !matches!(kind, SyntaxKind::BlockError));
    assert!(
        ancestor_kinds.contains(&SyntaxKind::Block),
        "BlockError should be emitted inside a block, got ancestry {:?}",
        ancestor_kinds
    );
    assert!(
        ancestor_kinds.contains(&SyntaxKind::FunctionDeclaration),
        "BlockError should remain scoped to the surrounding function, got ancestry {:?}",
        ancestor_kinds
    );
}

#[test]
fn unsupported_if_statement_reports_error() {
    let source = r#"
        fun demo() {
            if (true) {
                return
            }
        }
    "#;

    let tokens = lex(source);
    let output = parse(&tokens);

    assert!(
        output
            .diagnostics
            .iter()
            .any(|diag| diag.message.contains("`if`")),
        "expected diagnostic about unsupported if statement, got {:?}",
        output.diagnostics
    );

    assert!(
        output.events.iter().any(|event| match event {
            ParseEvent::StartNode { kind } => {
                matches!(kind, SyntaxKind::Error | SyntaxKind::BlockError)
            }
            _ => false,
        }),
        "expected error or block error node emitted for unsupported if statement"
    );
}

#[test]
fn type_annotation_preserves_optional_generic_tokens() {
    let source = r#"
        val cache: List<List<String?>> = rebuild()
        var current: List<String?> = mutate(cache)
    "#;

    let tokens = lex(source);
    let output = parse(&tokens);

    assert!(
        output.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        output.diagnostics
    );

    let tree: SyntaxNode<JvLanguage> =
        SyntaxNode::new_root(ParseBuilder::build_from_events(&output.events, &tokens));

    let mut annotations = tree
        .descendants()
        .filter(|node| node.kind() == SyntaxKind::TypeAnnotation);

    let first = annotations
        .next()
        .expect("expected first type annotation to be present");
    let second = annotations
        .next()
        .expect("expected second type annotation to be present");

    for annotation in [first, second] {
        let token_kinds: Vec<SyntaxKind> = annotation
            .descendants_with_tokens()
            .filter_map(|element| element.into_token().map(|token| token.kind()))
            .filter(|kind| !matches!(kind, SyntaxKind::Whitespace | SyntaxKind::Newline))
            .collect();

        assert_eq!(
            token_kinds.first().copied(),
            Some(SyntaxKind::Colon),
            "type annotation should start with colon token, got {:?}",
            token_kinds
        );
        assert!(
            token_kinds.iter().any(|kind| *kind == SyntaxKind::Less),
            "expected '<' token inside annotation {:?}",
            annotation
        );
        assert!(
            token_kinds.iter().any(|kind| *kind == SyntaxKind::Greater),
            "expected '>' token inside annotation {:?}",
            annotation
        );
        assert!(
            token_kinds.iter().any(|kind| *kind == SyntaxKind::Question),
            "expected '?' token inside annotation {:?}",
            annotation
        );
    }
}

#[test]
fn root_wraps_statements_in_statement_list() {
    let source = r#"
        package demo.example
        import foo.bar

        fun ready() {}
    "#;

    let tokens = lex(source);
    let output = parse(&tokens);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected diagnostics while parsing top-level statements: {:?}",
        output.diagnostics
    );

    let tree: SyntaxNode<JvLanguage> =
        SyntaxNode::new_root(ParseBuilder::build_from_events(&output.events, &tokens));

    let mut root_children = tree.children();
    let statement_list = root_children
        .next()
        .expect("root should have at least one child node");
    assert_eq!(
        statement_list.kind(),
        SyntaxKind::StatementList,
        "root should wrap statements in a StatementList node"
    );
    assert!(
        root_children.next().is_none(),
        "root should contain only a single StatementList node"
    );

    assert!(
        statement_list
            .children()
            .any(|node| node.kind() == SyntaxKind::PackageDeclaration),
        "statement list should include the package declaration node"
    );
    assert!(
        statement_list
            .children()
            .any(|node| node.kind() == SyntaxKind::ImportDeclaration),
        "statement list should include the import declaration node"
    );
    assert!(
        statement_list
            .children()
            .any(|node| node.kind() == SyntaxKind::FunctionDeclaration),
        "statement list should include the function declaration node"
    );
}

#[test]
fn function_without_body_recovers_and_continues() {
    let source = r#"
        fun broken(name: String)
        val fallback = 42
    "#;

    let tokens = lex(source);
    let output = parse(&tokens);

    assert!(
        output.recovered,
        "parser should mark recovery when function body is missing"
    );
    assert!(
        output
            .diagnostics
            .iter()
            .any(|diag| diag.message.contains("関数本体が必要です")),
        "expected diagnostic about missing function body, got {:?}",
        output.diagnostics
    );

    let started = start_node_sequence(&output.events);
    assert!(
        contains_subsequence(
            &started,
            &[SyntaxKind::FunctionDeclaration, SyntaxKind::ValDeclaration]
        ),
        "expected function declaration followed by val declaration in {:?}",
        started
    );

    assert!(
        output.events.iter().any(
            |event| matches!(event, ParseEvent::StartNode { kind } if *kind == SyntaxKind::Error)
        ),
        "expected error node to be emitted for missing function body"
    );
}

#[test]
fn class_missing_closing_brace_emits_block_error() {
    let source = r#"
        class Incomplete {
            fun value() {
                return
            }
            fun other() => 1

        val after = 1
    "#;

    let tokens = lex(source);
    let output = parse(&tokens);

    assert!(
        output.recovered,
        "parser should recover when class closing brace is missing"
    );
    assert!(
        output
            .diagnostics
            .iter()
            .any(|diag| diag.message.contains("'}' が必要です")),
        "expected diagnostic about missing closing brace, got {:?}",
        output.diagnostics
    );

    let started = start_node_sequence(&output.events);
    assert!(
        contains_subsequence(
            &started,
            &[SyntaxKind::ClassDeclaration, SyntaxKind::ValDeclaration]
        ),
        "expected class declaration followed by val declaration in {:?}",
        started
    );

    assert!(
        output
            .events
            .iter()
            .any(|event| matches!(event, ParseEvent::StartNode { kind } if *kind == SyntaxKind::BlockError)),
        "expected BlockError node for unclosed class body"
    );
}

#[test]
fn data_class_constructor_recovers_from_missing_parameter() {
    let source = r#"
        data class Person(: String) {}
        val done = true
    "#;

    let tokens = lex(source);
    let output = parse(&tokens);

    assert!(
        output.recovered,
        "parser should recover from missing constructor parameter identifier"
    );
    assert!(
        output.diagnostics.iter().any(|diag| diag
            .message
            .contains("コンストラクタパラメータ名が必要です")),
        "expected diagnostic about missing constructor parameter name, got {:?}",
        output.diagnostics
    );

    let started = start_node_sequence(&output.events);
    assert!(
        contains_subsequence(
            &started,
            &[SyntaxKind::ClassDeclaration, SyntaxKind::ValDeclaration]
        ),
        "expected class declaration followed by val declaration in {:?}",
        started
    );

    assert!(
        output.events.iter().any(
            |event| matches!(event, ParseEvent::StartNode { kind } if *kind == SyntaxKind::Error)
        ),
        "expected error node when constructor parameter is missing"
    );
}

fn start_node_sequence(events: &[ParseEvent]) -> Vec<SyntaxKind> {
    events
        .iter()
        .filter_map(|event| match event {
            ParseEvent::StartNode { kind } => Some(*kind),
            _ => None,
        })
        .collect()
}

fn contains_subsequence(haystack: &[SyntaxKind], needle: &[SyntaxKind]) -> bool {
    if needle.is_empty() {
        return true;
    }

    let mut idx = 0usize;
    for kind in haystack {
        if *kind == needle[idx] {
            idx += 1;
            if idx == needle.len() {
                return true;
            }
        }
    }

    false
}
