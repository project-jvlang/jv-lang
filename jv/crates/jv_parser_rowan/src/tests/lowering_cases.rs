use crate::lowering::{lower_program, LoweringDiagnosticSeverity, LoweringResult};
use crate::{JvLanguage, ParseBuilder, SyntaxKind};
use jv_ast::{
    statement::{LoopStrategy, ValBindingOrigin},
    types::{Literal, Modifiers, TypeAnnotation},
    Expression, Statement,
};
use jv_lexer::{Token, TokenTrivia, TokenType};
use rowan::SyntaxNode;

fn make_token(column: &mut usize, token_type: TokenType, lexeme: &str) -> Token {
    let token = Token {
        token_type,
        lexeme: lexeme.to_string(),
        line: 1,
        column: *column,
        leading_trivia: TokenTrivia::default(),
        diagnostic: None,
        metadata: Vec::new(),
    };
    *column += lexeme.len().max(1);
    token
}

fn build_tree(
    tokens: &[Token],
    build: impl FnOnce(&mut ParseBuilder, &[Token]),
) -> SyntaxNode<JvLanguage> {
    let mut builder = ParseBuilder::new();
    builder.start_node(SyntaxKind::Root);
    build(&mut builder, tokens);
    builder.finish_node();
    SyntaxNode::new_root(builder.finish())
}

fn sample_package_val() -> (SyntaxNode<JvLanguage>, Vec<Token>) {
    let mut column = 0usize;
    let mut tokens = Vec::new();
    tokens.push(make_token(&mut column, TokenType::Package, "package"));
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("demo".to_string()),
        "demo",
    ));
    tokens.push(make_token(&mut column, TokenType::Dot, "."));
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("core".to_string()),
        "core",
    ));
    tokens.push(make_token(&mut column, TokenType::Val, "val"));
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("answer".to_string()),
        "answer",
    ));
    tokens.push(make_token(&mut column, TokenType::Colon, ":"));
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("Int".to_string()),
        "Int",
    ));
    tokens.push(make_token(&mut column, TokenType::Assign, "="));
    tokens.push(make_token(
        &mut column,
        TokenType::Number("42".to_string()),
        "42",
    ));
    tokens.push(make_token(&mut column, TokenType::Eof, ""));

    let node = build_tree(&tokens, |builder, tokens| {
        // package demo.core
        builder.start_node(SyntaxKind::PackageDeclaration);
        builder.push_token(&tokens[0]);
        builder.start_node(SyntaxKind::PackageName);
        builder.start_node(SyntaxKind::QualifiedName);
        builder.start_node(SyntaxKind::QualifiedNameSegment);
        builder.push_token(&tokens[1]);
        builder.finish_node();
        builder.push_token(&tokens[2]);
        builder.start_node(SyntaxKind::QualifiedNameSegment);
        builder.push_token(&tokens[3]);
        builder.finish_node();
        builder.finish_node(); // QualifiedName
        builder.finish_node(); // PackageName
        builder.finish_node(); // PackageDeclaration

        // val answer: Int = 42
        builder.start_node(SyntaxKind::ValDeclaration);
        builder.push_token(&tokens[4]);
        builder.start_node(SyntaxKind::BindingPattern);
        builder.push_token(&tokens[5]);
        builder.finish_node();
        builder.start_node(SyntaxKind::TypeAnnotation);
        builder.push_token(&tokens[6]);
        builder.start_node(SyntaxKind::Expression);
        builder.push_token(&tokens[7]);
        builder.finish_node();
        builder.finish_node();
        builder.start_node(SyntaxKind::InitializerClause);
        builder.push_token(&tokens[8]);
        builder.start_node(SyntaxKind::Expression);
        builder.push_token(&tokens[9]);
        builder.finish_node();
        builder.finish_node();
        builder.finish_node(); // ValDeclaration

        builder.push_token(&tokens[10]); // EOF
    });

    (node, tokens)
}

fn error_val_tokens() -> (SyntaxNode<JvLanguage>, Vec<Token>) {
    let mut column = 0usize;
    let mut tokens = Vec::new();
    tokens.push(make_token(&mut column, TokenType::Val, "val"));
    tokens.push(make_token(&mut column, TokenType::Assign, "="));
    tokens.push(make_token(
        &mut column,
        TokenType::Number("0".to_string()),
        "0",
    ));
    tokens.push(make_token(&mut column, TokenType::Eof, ""));

    let node = build_tree(&tokens, |builder, tokens| {
        builder.start_node(SyntaxKind::Error);
        builder.push_token(&tokens[0]);
        builder.push_token(&tokens[1]);
        builder.push_token(&tokens[2]);
        builder.finish_node();
        builder.push_token(&tokens[3]);
    });

    (node, tokens)
}

fn function_tokens() -> (SyntaxNode<JvLanguage>, Vec<Token>) {
    let mut column = 0usize;
    let mut tokens = Vec::new();
    tokens.push(make_token(&mut column, TokenType::Fun, "fun"));
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("greet".to_string()),
        "greet",
    ));
    tokens.push(make_token(&mut column, TokenType::LeftParen, "("));
    tokens.push(make_token(&mut column, TokenType::RightParen, ")"));
    tokens.push(make_token(&mut column, TokenType::LeftBrace, "{"));
    tokens.push(make_token(&mut column, TokenType::RightBrace, "}"));
    tokens.push(make_token(&mut column, TokenType::Eof, ""));

    let node = build_tree(&tokens, |builder, tokens| {
        builder.start_node(SyntaxKind::FunctionDeclaration);
        builder.push_token(&tokens[0]);
        builder.push_token(&tokens[1]);
        builder.start_node(SyntaxKind::FunctionParameterList);
        builder.push_token(&tokens[2]);
        builder.push_token(&tokens[3]);
        builder.finish_node();
        builder.start_node(SyntaxKind::Block);
        builder.push_token(&tokens[4]);
        builder.push_token(&tokens[5]);
        builder.finish_node();
        builder.finish_node(); // FunctionDeclaration
        builder.push_token(&tokens[6]);
    });

    (node, tokens)
}

fn for_tokens() -> (SyntaxNode<JvLanguage>, Vec<Token>) {
    let mut column = 0usize;
    let mut tokens = Vec::new();
    tokens.push(make_token(&mut column, TokenType::For, "for"));
    tokens.push(make_token(&mut column, TokenType::LeftParen, "("));
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("item".into()),
        "item",
    ));
    tokens.push(make_token(&mut column, TokenType::In, "in"));
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("items".into()),
        "items",
    ));
    tokens.push(make_token(&mut column, TokenType::RightParen, ")"));
    tokens.push(make_token(&mut column, TokenType::LeftBrace, "{"));
    tokens.push(make_token(&mut column, TokenType::RightBrace, "}"));
    tokens.push(make_token(&mut column, TokenType::Eof, ""));

    let node = build_tree(&tokens, |builder, tokens| {
        builder.start_node(SyntaxKind::ForStatement);
        builder.push_token(&tokens[0]);
        builder.push_token(&tokens[1]);
        builder.start_node(SyntaxKind::BindingPattern);
        builder.push_token(&tokens[2]);
        builder.finish_node();
        builder.push_token(&tokens[3]); // in
        builder.start_node(SyntaxKind::Expression);
        builder.push_token(&tokens[4]); // items
        builder.finish_node();
        builder.push_token(&tokens[5]); // )
        builder.start_node(SyntaxKind::Block);
        builder.push_token(&tokens[6]);
        builder.push_token(&tokens[7]);
        builder.finish_node();
        builder.finish_node(); // ForStatement
        builder.push_token(&tokens[8]);
    });

    (node, tokens)
}

fn comment_tokens() -> (SyntaxNode<JvLanguage>, Vec<Token>) {
    let mut column = 0usize;
    let mut tokens = Vec::new();
    tokens.push(make_token(
        &mut column,
        TokenType::LineComment("// visible".to_string()),
        "// visible",
    ));
    tokens.push(make_token(
        &mut column,
        TokenType::LineComment("/// internal".to_string()),
        "/// internal",
    ));
    tokens.push(make_token(
        &mut column,
        TokenType::BlockComment(" block ".to_string()),
        "/* block */",
    ));
    tokens.push(make_token(&mut column, TokenType::Eof, ""));

    let node = build_tree(&tokens, |builder, tokens| {
        builder.start_node(SyntaxKind::CommentStatement);
        builder.push_token(&tokens[0]);
        builder.finish_node();

        builder.start_node(SyntaxKind::CommentStatement);
        builder.push_token(&tokens[1]);
        builder.finish_node();

        builder.start_node(SyntaxKind::CommentStatement);
        builder.push_token(&tokens[2]);
        builder.finish_node();

        builder.push_token(&tokens[3]);
    });

    (node, tokens)
}

fn assignment_tokens() -> (SyntaxNode<JvLanguage>, Vec<Token>) {
    let mut column = 0usize;
    let mut tokens = Vec::new();
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("user".to_string()),
        "user",
    ));
    tokens.push(make_token(&mut column, TokenType::Dot, "."));
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("name".to_string()),
        "name",
    ));
    tokens.push(make_token(&mut column, TokenType::Assign, "="));
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("value".to_string()),
        "value",
    ));
    tokens.push(make_token(&mut column, TokenType::Eof, ""));

    let node = build_tree(&tokens, |builder, tokens| {
        builder.start_node(SyntaxKind::AssignmentStatement);
        builder.start_node(SyntaxKind::AssignmentTarget);
        builder.push_token(&tokens[0]);
        builder.push_token(&tokens[1]);
        builder.push_token(&tokens[2]);
        builder.finish_node();
        builder.push_token(&tokens[3]);
        builder.start_node(SyntaxKind::Expression);
        builder.push_token(&tokens[4]);
        builder.finish_node();
        builder.finish_node();
        builder.push_token(&tokens[5]);
    });

    (node, tokens)
}

fn return_tokens() -> (SyntaxNode<JvLanguage>, Vec<Token>) {
    let mut column = 0usize;
    let mut tokens = Vec::new();
    tokens.push(make_token(&mut column, TokenType::Return, "return"));
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("value".into()),
        "value",
    ));
    tokens.push(make_token(&mut column, TokenType::Eof, ""));

    let node = build_tree(&tokens, |builder, tokens| {
        builder.start_node(SyntaxKind::ReturnStatement);
        builder.push_token(&tokens[0]);
        builder.start_node(SyntaxKind::Expression);
        builder.push_token(&tokens[1]);
        builder.finish_node();
        builder.finish_node();
        builder.push_token(&tokens[2]);
    });

    (node, tokens)
}

fn for_without_block_tokens() -> (SyntaxNode<JvLanguage>, Vec<Token>) {
    let mut column = 0usize;
    let mut tokens = Vec::new();
    tokens.push(make_token(&mut column, TokenType::For, "for"));
    tokens.push(make_token(&mut column, TokenType::LeftParen, "("));
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("item".into()),
        "item",
    ));
    tokens.push(make_token(&mut column, TokenType::In, "in"));
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("items".into()),
        "items",
    ));
    tokens.push(make_token(&mut column, TokenType::RightParen, ")"));
    tokens.push(make_token(&mut column, TokenType::Eof, ""));

    let node = build_tree(&tokens, |builder, tokens| {
        builder.start_node(SyntaxKind::ForStatement);
        builder.push_token(&tokens[0]);
        builder.push_token(&tokens[1]);
        builder.start_node(SyntaxKind::BindingPattern);
        builder.push_token(&tokens[2]);
        builder.finish_node();
        builder.push_token(&tokens[3]);
        builder.start_node(SyntaxKind::Expression);
        builder.push_token(&tokens[4]);
        builder.finish_node();
        builder.push_token(&tokens[5]);
        builder.finish_node();
        builder.push_token(&tokens[6]);
    });

    (node, tokens)
}

fn complex_expression_tokens() -> (SyntaxNode<JvLanguage>, Vec<Token>) {
    let mut column = 0usize;
    let mut tokens = Vec::new();
    tokens.push(make_token(&mut column, TokenType::Val, "val"));
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("result".into()),
        "result",
    ));
    tokens.push(make_token(&mut column, TokenType::Assign, "="));
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("foo".into()),
        "foo",
    ));
    tokens.push(make_token(&mut column, TokenType::Plus, "+"));
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("bar".into()),
        "bar",
    ));
    tokens.push(make_token(&mut column, TokenType::Eof, ""));

    let node = build_tree(&tokens, |builder, tokens| {
        builder.start_node(SyntaxKind::ValDeclaration);
        builder.push_token(&tokens[0]);
        builder.start_node(SyntaxKind::BindingPattern);
        builder.push_token(&tokens[1]);
        builder.finish_node();
        builder.start_node(SyntaxKind::InitializerClause);
        builder.push_token(&tokens[2]);
        builder.start_node(SyntaxKind::Expression);
        builder.push_token(&tokens[3]);
        builder.push_token(&tokens[4]);
        builder.push_token(&tokens[5]);
        builder.finish_node();
        builder.finish_node();
        builder.finish_node();
        builder.push_token(&tokens[6]);
    });

    (node, tokens)
}

fn class_with_when_tokens() -> (SyntaxNode<JvLanguage>, Vec<Token>) {
    let mut column = 0usize;
    let mut tokens = Vec::new();
    tokens.push(make_token(&mut column, TokenType::Class, "class"));
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("Sample".into()),
        "Sample",
    ));
    tokens.push(make_token(&mut column, TokenType::LeftBrace, "{"));
    tokens.push(make_token(&mut column, TokenType::When, "when"));
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("cond".into()),
        "cond",
    ));
    tokens.push(make_token(&mut column, TokenType::LeftBrace, "{"));
    tokens.push(make_token(&mut column, TokenType::RightBrace, "}"));
    tokens.push(make_token(&mut column, TokenType::RightBrace, "}"));
    tokens.push(make_token(&mut column, TokenType::Eof, ""));

    let node = build_tree(&tokens, |builder, tokens| {
        builder.start_node(SyntaxKind::ClassDeclaration);
        builder.push_token(&tokens[0]);
        builder.push_token(&tokens[1]);
        builder.start_node(SyntaxKind::ClassBody);
        builder.push_token(&tokens[2]);
        builder.start_node(SyntaxKind::StatementList);
        builder.start_node(SyntaxKind::WhenStatement);
        builder.push_token(&tokens[3]);
        builder.start_node(SyntaxKind::Expression);
        builder.push_token(&tokens[4]);
        builder.finish_node();
        builder.start_node(SyntaxKind::Block);
        builder.push_token(&tokens[5]);
        builder.push_token(&tokens[6]);
        builder.finish_node();
        builder.finish_node();
        builder.finish_node();
        builder.push_token(&tokens[7]);
        builder.finish_node();
        builder.finish_node();
        builder.push_token(&tokens[8]);
    });

    (node, tokens)
}

struct LoweringCase<'a> {
    build: fn() -> (SyntaxNode<JvLanguage>, Vec<Token>),
    verify: Box<dyn Fn(&LoweringResult) + 'a>,
}

#[test]
fn lowering_table_driven_cases() {
    let cases: Vec<LoweringCase<'_>> = vec![
        LoweringCase {
            build: sample_package_val,
            verify: Box::new(|result| {
                assert!(
                    result.diagnostics.is_empty(),
                    "expected no diagnostics, got {:?}",
                    result.diagnostics
                );
                assert_eq!(result.statements.len(), 2, "expected two statements");
                match &result.statements[0] {
                    Statement::Package { name, .. } => assert_eq!(name, "demo.core"),
                    other => panic!("expected package statement, got {:?}", other),
                }
                match &result.statements[1] {
                    Statement::ValDeclaration {
                        name,
                        type_annotation,
                        initializer,
                        modifiers,
                        origin,
                        ..
                    } => {
                        assert_eq!(name, "answer");
                        assert_eq!(origin, &ValBindingOrigin::ExplicitKeyword);
                        assert_eq!(modifiers, &Modifiers::default());
                        assert_eq!(type_annotation, &Some(TypeAnnotation::Simple("Int".into())));
                        match initializer {
                            Expression::Literal(Literal::Number(value), _) => {
                                assert_eq!(value, "42")
                            }
                            other => panic!("expected numeric literal, got {:?}", other),
                        }
                    }
                    other => panic!("expected val declaration, got {:?}", other),
                }
            }),
        },
        LoweringCase {
            build: error_val_tokens,
            verify: Box::new(|result| {
                assert!(
                    result.statements.is_empty(),
                    "expected no statements, got {:?}",
                    result.statements
                );
                let diagnostic = result
                    .diagnostics
                    .first()
                    .expect("expected at least one diagnostic");
                assert_eq!(diagnostic.node_kind, SyntaxKind::Error);
                assert_eq!(diagnostic.severity, LoweringDiagnosticSeverity::Error);
            }),
        },
        LoweringCase {
            build: function_tokens,
            verify: Box::new(|result| {
                assert!(
                    result.diagnostics.is_empty(),
                    "unexpected diagnostics: {:?}",
                    result.diagnostics
                );
                match result
                    .statements
                    .first()
                    .expect("expected function statement")
                {
                    Statement::FunctionDeclaration {
                        name, parameters, ..
                    } => {
                        assert_eq!(name, "greet");
                        assert!(parameters.is_empty());
                    }
                    other => panic!("expected function declaration, got {:?}", other),
                }
            }),
        },
        LoweringCase {
            build: for_without_block_tokens,
            verify: Box::new(|result| {
                match result
                    .statements
                    .first()
                    .expect("expected for-in statement")
                {
                    Statement::ForIn(stmt) => {
                        assert_eq!(stmt.binding.name, "item");
                        assert_eq!(stmt.strategy, LoopStrategy::Iterable);
                    }
                    other => panic!("expected for-in statement, got {:?}", other),
                }
                let warning = result
                    .diagnostics
                    .iter()
                    .find(|diag| diag.severity == LoweringDiagnosticSeverity::Warning)
                    .expect("expected warning diagnostic for missing block");
                assert!(
                    warning.message.contains("空ブロックとして処理します"),
                    "unexpected warning message: {}",
                    warning.message
                );
            }),
        },
        LoweringCase {
            build: complex_expression_tokens,
            verify: Box::new(|result| {
                match result.statements.first().expect("expected val statement") {
                    Statement::ValDeclaration { initializer, .. } => match initializer {
                        Expression::Identifier(text, _) => assert_eq!(text, "foo+bar"),
                        other => panic!("expected identifier fallback, got {:?}", other),
                    },
                    other => panic!("expected val declaration, got {:?}", other),
                }
                let warning = result
                    .diagnostics
                    .iter()
                    .find(|diag| diag.severity == LoweringDiagnosticSeverity::Warning)
                    .expect("expected warning for complex expression fallback");
                assert!(
                    warning.message.contains("式ローワリング"),
                    "unexpected warning: {}",
                    warning.message
                );
            }),
        },
        LoweringCase {
            build: class_with_when_tokens,
            verify: Box::new(|result| {
                match result.statements.first().expect("expected class statement") {
                    Statement::ClassDeclaration {
                        properties,
                        methods,
                        ..
                    } => {
                        assert!(properties.is_empty());
                        assert!(methods.is_empty());
                    }
                    other => panic!("expected class declaration, got {:?}", other),
                }
                let warning = result
                    .diagnostics
                    .iter()
                    .find(|diag| diag.severity == LoweringDiagnosticSeverity::Warning)
                    .expect("expected warning for unsupported class member");
                assert!(
                    warning.message.contains("未対応のノード"),
                    "unexpected warning message: {}",
                    warning.message
                );
            }),
        },
        LoweringCase {
            build: return_tokens,
            verify: Box::new(|result| {
                assert!(
                    result.diagnostics.is_empty(),
                    "unexpected diagnostics: {:?}",
                    result.diagnostics
                );
                match result
                    .statements
                    .first()
                    .expect("expected return statement")
                {
                    Statement::Return { value, .. } => {
                        let expr = value.as_ref().expect("expected return value");
                        match expr {
                            Expression::Identifier(name, _) => assert_eq!(name, "value"),
                            other => panic!("expected identifier expression, got {:?}", other),
                        }
                    }
                    other => panic!("expected return statement, got {:?}", other),
                }
            }),
        },
        LoweringCase {
            build: assignment_tokens,
            verify: Box::new(|result| {
                assert!(
                    result.diagnostics.is_empty(),
                    "unexpected diagnostics: {:?}",
                    result.diagnostics
                );
                let statement = result
                    .statements
                    .first()
                    .expect("expected assignment statement");
                match statement {
                    Statement::Assignment { target, value, .. } => {
                        match target {
                            Expression::MemberAccess {
                                object, property, ..
                            } => {
                                assert_eq!(property, "name");
                                match object.as_ref() {
                                    Expression::Identifier(name, _) => assert_eq!(name, "user"),
                                    other => panic!(
                                        "expected identifier object for member access, got {:?}",
                                        other
                                    ),
                                }
                            }
                            other => panic!("expected member access target, got {:?}", other),
                        }
                        match value {
                            Expression::Identifier(name, _) => assert_eq!(name, "value"),
                            other => panic!("expected identifier rhs, got {:?}", other),
                        }
                    }
                    other => panic!("expected assignment statement, got {:?}", other),
                }
            }),
        },
        LoweringCase {
            build: comment_tokens,
            verify: Box::new(|result| {
                assert!(
                    result.diagnostics.is_empty(),
                    "unexpected diagnostics: {:?}",
                    result.diagnostics
                );
                assert_eq!(
                    result.statements.len(),
                    3,
                    "expected three comment statements"
                );
                match &result.statements[0] {
                    Statement::Comment(comment) => {
                        assert_eq!(comment.kind, jv_ast::comments::CommentKind::Line);
                        assert_eq!(
                            comment.visibility,
                            jv_ast::comments::CommentVisibility::Passthrough
                        );
                        assert_eq!(comment.text, "// visible");
                    }
                    other => panic!("expected line comment statement, got {:?}", other),
                }
                match &result.statements[1] {
                    Statement::Comment(comment) => {
                        assert_eq!(comment.kind, jv_ast::comments::CommentKind::Line);
                        assert_eq!(
                            comment.visibility,
                            jv_ast::comments::CommentVisibility::JvOnly
                        );
                        assert_eq!(comment.text, "/// internal");
                    }
                    other => panic!("expected jv-only comment, got {:?}", other),
                }
                match &result.statements[2] {
                    Statement::Comment(comment) => {
                        assert_eq!(comment.kind, jv_ast::comments::CommentKind::Block);
                        assert_eq!(
                            comment.visibility,
                            jv_ast::comments::CommentVisibility::Passthrough
                        );
                        assert_eq!(comment.text, "/* block */");
                    }
                    other => panic!("expected block comment, got {:?}", other),
                }
            }),
        },
        LoweringCase {
            build: for_tokens,
            verify: Box::new(|result| {
                assert!(
                    result.diagnostics.is_empty(),
                    "unexpected diagnostics: {:?}",
                    result.diagnostics
                );
                match result
                    .statements
                    .first()
                    .expect("expected for-in statement")
                {
                    Statement::ForIn(for_in) => {
                        assert_eq!(for_in.binding.name, "item");
                        assert_eq!(for_in.strategy, LoopStrategy::Iterable);
                        match &for_in.iterable {
                            Expression::Identifier(name, _) => assert_eq!(name, "items"),
                            other => panic!("expected iterable identifier, got {:?}", other),
                        }
                    }
                    other => panic!("expected for-in statement, got {:?}", other),
                }
            }),
        },
    ];

    for case in cases {
        let (root, tokens) = (case.build)();
        let result = lower_program(&root, &tokens);
        (case.verify)(&result);
    }
}
