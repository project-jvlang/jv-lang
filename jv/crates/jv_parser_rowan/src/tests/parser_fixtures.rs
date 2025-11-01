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
    eprintln!("tokens.len() = {}", tokens.len());
    for token in &tokens {
        eprintln!(
            "token: {:?} @{}:{}",
            token.token_type, token.line, token.column
        );
    }
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
fn function_parameters_accept_default_values() {
    let source = r#"
        fun greet(name: String, prefix: String = "Hello", punctuation: String = "!") {
            println("${prefix}, ${name}${punctuation}")
        }
    "#;

    let tokens = lex(source);
    let output = parse(&tokens);

    assert!(
        output.diagnostics.is_empty(),
        "expected parser to accept default parameter initializers, got {:?}",
        output.diagnostics
    );
    assert!(
        !output.recovered,
        "parser should not need recovery for default parameter initializers"
    );
    let has_initializer_clause = output.events.iter().any(|event| match event {
        ParseEvent::StartNode { kind } => *kind == SyntaxKind::InitializerClause,
        _ => false,
    });
    assert!(
        has_initializer_clause,
        "parameter default should produce an InitializerClause node"
    );
}

#[test]
fn function_without_parentheses_forms_empty_parameter_list() {
    let source = r#"
        fun main {
            println("hello")
        }
    "#;

    let tokens = lex(source);
    let output = parse(&tokens);

    assert!(
        output.diagnostics.is_empty(),
        "expected parser to accept omitted parentheses, got {:?}",
        output.diagnostics
    );
    assert!(
        !output.recovered,
        "parser should not require recovery when parentheses are omitted"
    );

    let tree: SyntaxNode<JvLanguage> =
        SyntaxNode::new_root(ParseBuilder::build_from_events(&output.events, &tokens));
    let statement_list = tree
        .children()
        .find(|node| node.kind() == SyntaxKind::StatementList)
        .expect("parsed tree should contain a StatementList");
    let function = statement_list
        .children()
        .find(|node| node.kind() == SyntaxKind::FunctionDeclaration)
        .expect("function declaration should be emitted");
    let params = function
        .children()
        .find(|node| node.kind() == SyntaxKind::FunctionParameterList)
        .expect("parameter list node should be present");
    assert!(
        !params
            .children()
            .any(|child| child.kind() == SyntaxKind::FunctionParameter),
        "parameter list should be empty when parentheses are omitted"
    );
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
fn parses_labeled_for_statement_and_break_labels() {
    let source = r#"
        #outer for (item in items) {
            when (item) {
                else -> break #outer
            }
        }
    "#;

    let tokens = lex(source);
    let output = parse(&tokens);

    assert!(
        output.diagnostics.is_empty(),
        "ハッシュラベル対応で診断が出力されないことを期待しました: {:?}",
        output.diagnostics
    );

    let green = ParseBuilder::build_from_events(&output.events, &tokens);
    let tree: SyntaxNode<JvLanguage> = SyntaxNode::new_root(green);
    let statement_list = tree
        .children()
        .find(|node| node.kind() == SyntaxKind::StatementList)
        .expect("ステートメント列ノードが存在するはずです");
    let labeled = statement_list
        .children()
        .find(|node| node.kind() == SyntaxKind::LabeledStatement)
        .expect("ラベル付きfor文が出力されるはずです");
    let has_label_token = labeled.children_with_tokens().any(|child| {
        child
            .as_token()
            .map(|token| token.kind() == SyntaxKind::HashLabel)
            .unwrap_or(false)
    });
    assert!(
        has_label_token,
        "ラベル付きノードにHashLabelトークンが含まれるべきです"
    );
    let for_statement = labeled
        .children()
        .find(|node| node.kind() == SyntaxKind::ForStatement)
        .expect("ラベルの直後にfor文が存在するはずです");
    let break_has_label = for_statement
        .descendants()
        .find(|node| node.kind() == SyntaxKind::BreakStatement)
        .map(|node| {
            node.children_with_tokens().any(|child| {
                child
                    .as_token()
                    .map(|token| token.kind() == SyntaxKind::HashLabel)
                    .unwrap_or(false)
            })
        })
        .unwrap_or(false);
    assert!(
        break_has_label,
        "break文にハッシュラベルが付与されていることを期待しました"
    );
}

#[test]
fn parses_nested_labeled_statements() {
    let source = r#"
        #outer for (item in items) {
            #inner {
                return #outer
            }
        }
    "#;

    let tokens = lex(source);
    let output = parse(&tokens);

    assert!(
        output.diagnostics.is_empty(),
        "ネストしたラベルで診断が発生しないことを期待しました: {:?}",
        output.diagnostics
    );

    let green = ParseBuilder::build_from_events(&output.events, &tokens);
    let tree: SyntaxNode<JvLanguage> = SyntaxNode::new_root(green);
    let statement_list = tree
        .children()
        .find(|node| node.kind() == SyntaxKind::StatementList)
        .expect("トップレベルにステートメント列が必要です");
    let labeled_nodes: Vec<_> = statement_list
        .descendants()
        .filter(|node| node.kind() == SyntaxKind::LabeledStatement)
        .collect();
    assert_eq!(
        labeled_nodes.len(),
        2,
        "外側と内側で2つのラベル付きステートメントが生成されるはずです"
    );
    let return_has_label = labeled_nodes
        .iter()
        .flat_map(|node| node.descendants())
        .find(|node| node.kind() == SyntaxKind::ReturnStatement)
        .map(|node| {
            node.children_with_tokens().any(|child| {
                child
                    .as_token()
                    .map(|token| token.kind() == SyntaxKind::HashLabel)
                    .unwrap_or(false)
            })
        })
        .unwrap_or(false);
    assert!(
        return_has_label,
        "return文がハッシュラベルを保持しているべきです"
    );
}

#[test]
fn labeled_for_after_unicode_string_retains_label() {
    let source = r#"
        val members = []
        var firstActive = "未検出"
        #outer for (user in members) {
            break #outer
        }
    "#;

    let tokens = lex(source);
    let output = parse(&tokens);

    assert!(
        output.diagnostics.is_empty(),
        "ハッシュラベルの後続処理で診断が発生しないことを期待しました: {:?}",
        output.diagnostics
    );

    let green = ParseBuilder::build_from_events(&output.events, &tokens);
    let tree: SyntaxNode<JvLanguage> = SyntaxNode::new_root(green);
    let statement_list = tree
        .children()
        .find(|node| node.kind() == SyntaxKind::StatementList)
        .expect("ステートメント列が構築されるはずです");

    let labeled = statement_list
        .children()
        .find(|node| node.kind() == SyntaxKind::LabeledStatement)
        .expect("ラベル付き for 文が生成されるはずです");
    let label_token = labeled
        .children_with_tokens()
        .find_map(|child| child.as_token().cloned())
        .filter(|token| token.kind() == SyntaxKind::HashLabel)
        .expect("HashLabel トークンが存在するはずです");
    assert_eq!(label_token.text(), "outer");

    let break_statement = labeled
        .descendants()
        .find(|node| node.kind() == SyntaxKind::BreakStatement)
        .expect("break 文が存在するはずです");
    let break_has_label = break_statement.children_with_tokens().any(|child| {
        child
            .as_token()
            .map(|token| token.kind() == SyntaxKind::HashLabel)
            .unwrap_or(false)
    });
    assert!(break_has_label, "break #outer が解析結果に含まれるべきです");
}

#[test]
fn hash_label_without_target_falls_back_to_comment() {
    let source = r#"
        #meta
        val value = 1
    "#;

    let tokens = lex(source);
    let output = parse(&tokens);

    assert!(
        output.diagnostics.is_empty(),
        "ラベルフォールバック時に診断が発生しないことを期待しました: {:?}",
        output.diagnostics
    );

    let green = ParseBuilder::build_from_events(&output.events, &tokens);
    let tree: SyntaxNode<JvLanguage> = SyntaxNode::new_root(green);
    let mut statements = tree
        .children()
        .find(|node| node.kind() == SyntaxKind::StatementList)
        .expect("トップレベルのステートメント列が必要です")
        .children();
    let first = statements
        .next()
        .expect("少なくとも1つのステートメントが存在するはずです");
    assert_eq!(
        first.kind(),
        SyntaxKind::CommentStatement,
        "不正な位置のハッシュラベルはコメント扱いになるべきです"
    );
    let comment_contains_hash_label = first.children_with_tokens().any(|child| {
        child
            .as_token()
            .map(|token| token.kind() == SyntaxKind::HashLabel)
            .unwrap_or(false)
    });
    assert!(
        comment_contains_hash_label,
        "フォールバックコメントにHashLabelトークンが含まれるべきです"
    );
    let second = statements
        .next()
        .expect("コメントの後にval宣言が続くはずです");
    assert_eq!(
        second.kind(),
        SyntaxKind::ValDeclaration,
        "フォールバック後も通常の宣言が解析されるべきです"
    );
}

#[test]
fn hash_label_attaches_to_trailing_lambda_in_expression() {
    let source = r#"
        processor.map #finish {
            break #finish
        }
    "#;

    let tokens = lex(source);
    let output = parse(&tokens);

    assert!(
        output.diagnostics.is_empty(),
        "ラベル付きトレーリングラムダで診断が発生しないことを期待しました: {:?}",
        output.diagnostics
    );

    let green = ParseBuilder::build_from_events(&output.events, &tokens);
    let tree: SyntaxNode<JvLanguage> = SyntaxNode::new_root(green);
    let statement_list = tree
        .children()
        .find(|node| node.kind() == SyntaxKind::StatementList)
        .expect("トップレベルのステートメント列が必要です");
    let expression = statement_list
        .children()
        .find(|node| node.kind() == SyntaxKind::Expression)
        .expect("式ステートメントが生成されるはずです");
    let labeled = expression
        .descendants()
        .find(|node| node.kind() == SyntaxKind::LabeledStatement)
        .expect("トレーリングラムダにラベル付きノードが付与されるはずです");
    assert!(
        labeled.children_with_tokens().any(|child| child
            .as_token()
            .map(|token| token.kind() == SyntaxKind::HashLabel)
            .unwrap_or(false)),
        "ラベル付きノードにHashLabelトークンが含まれるべきです"
    );
    let block_exists = labeled
        .children()
        .any(|child| child.kind() == SyntaxKind::Block);
    assert!(block_exists, "ラベル付きラムダはBlockノードを含むべきです");
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
fn unsupported_while_statement_reports_error() {
    let source = r#"
        fun demo() {
            while (true) {
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
            .any(|diag| diag.message.contains("`while`")),
        "expected diagnostic about unsupported while statement, got {:?}",
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
fn function_arrow_body_emits_diagnostic() {
    let source = r#"fun compute(x: Int): Int -> x + 1"#;

    let tokens = lex(source);
    let output = parse(&tokens);

    assert!(
        output
            .diagnostics
            .iter()
            .any(|diag| diag.message.contains("`->` 構文は廃止されました")),
        "expected diagnostic about deprecated arrow syntax, got {:?}",
        output.diagnostics
    );

    let started = start_node_sequence(&output.events);
    assert!(
        started
            .iter()
            .any(|kind| *kind == SyntaxKind::FunctionDeclaration),
        "expected function declaration to still be produced"
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
