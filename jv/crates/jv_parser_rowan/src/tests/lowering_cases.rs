use crate::lowering::{lower_program, LoweringDiagnosticSeverity};
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
    tokens.push(make_token(&mut column, TokenType::Identifier("item".into()), "item"));
    tokens.push(make_token(&mut column, TokenType::In, "in"));
    tokens.push(make_token(&mut column, TokenType::Identifier("items".into()), "items"));
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

#[test]
fn lowers_package_and_val_declaration() {
    let (root, tokens) = sample_package_val();
    let lowering = lower_program(&root, &tokens);
    assert!(
        lowering.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        lowering.diagnostics
    );
    assert_eq!(lowering.statements.len(), 2);

    match &lowering.statements[0] {
        Statement::Package { name, .. } => assert_eq!(name, "demo.core"),
        other => panic!("expected package statement, got {:?}", other),
    }

    match &lowering.statements[1] {
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
            assert_eq!(
                type_annotation,
                &Some(TypeAnnotation::Simple("Int".to_string()))
            );
            match initializer {
                Expression::Literal(Literal::Number(value), _) => assert_eq!(value, "42"),
                other => panic!("expected numeric literal, got {:?}", other),
            }
        }
        other => panic!("expected val declaration, got {:?}", other),
    }
}

#[test]
fn reports_diagnostic_for_invalid_statement() {
    let (root, tokens) = error_val_tokens();
    let lowering = lower_program(&root, &tokens);
    assert!(lowering.statements.is_empty());
    let diagnostic = lowering
        .diagnostics
        .first()
        .expect("expected at least one diagnostic");
    assert_eq!(diagnostic.node_kind, SyntaxKind::Error);
    assert_eq!(diagnostic.severity, LoweringDiagnosticSeverity::Error);
}

#[test]
fn lowers_function_declaration() {
    let (root, tokens) = function_tokens();
    let lowering = lower_program(&root, &tokens);
    assert!(
        lowering.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        lowering.diagnostics
    );
    assert_eq!(lowering.statements.len(), 1);
    match &lowering.statements[0] {
        Statement::FunctionDeclaration { name, parameters, .. } => {
            assert_eq!(name, "greet");
            assert!(parameters.is_empty());
        }
        other => panic!("expected function declaration, got {:?}", other),
    }
}

#[test]
fn lowers_for_statement() {
    let (root, tokens) = for_tokens();
    let lowering = lower_program(&root, &tokens);
    assert!(
        lowering.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        lowering.diagnostics
    );
    assert_eq!(lowering.statements.len(), 1);
    match &lowering.statements[0] {
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
}

#[test]
fn lowers_return_statement() {
    let (root, tokens) = return_tokens();
    let lowering = lower_program(&root, &tokens);
    assert!(
        lowering.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        lowering.diagnostics
    );
    assert_eq!(lowering.statements.len(), 1);
    match &lowering.statements[0] {
        Statement::Return { value, .. } => {
            let expr = value.as_ref().expect("expected return value");
            match expr {
                Expression::Identifier(name, _) => assert_eq!(name, "value"),
                other => panic!("expected identifier expression, got {:?}", other),
            }
        }
        other => panic!("expected return statement, got {:?}", other),
    }
}
