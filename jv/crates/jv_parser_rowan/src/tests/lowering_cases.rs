use crate::lowering::{lower_program, LoweringDiagnosticSeverity, LoweringResult};
use crate::parser::parse;
use crate::verification::StatementKindKey;
use crate::{JvLanguage, ParseBuilder, SyntaxKind};
use jv_ast::strings::MultilineKind;
use jv_ast::{
    expression::{Argument, Parameter, ParameterProperty, StringPart},
    json::{JsonLiteral, JsonValue},
    statement::{ConcurrencyConstruct, LoopStrategy, ResourceManagement, ValBindingOrigin},
    types::{BinaryOp, Literal, Modifiers, Pattern, TypeAnnotation},
    BindingPatternKind, Expression, Statement,
};
use jv_lexer::{Lexer, Token, TokenTrivia, TokenType};
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

fn lower_source(source: &str) -> LoweringResult {
    let tokens = Lexer::new(source.to_string())
        .tokenize()
        .expect("lex source");
    let parse_output = parse(&tokens);
    assert!(
        parse_output.diagnostics.is_empty(),
        "expected parser diagnostics to be empty, got {:?}",
        parse_output.diagnostics
    );
    let green = ParseBuilder::build_from_events(&parse_output.events, &tokens);
    let syntax: SyntaxNode<JvLanguage> = SyntaxNode::new_root(green);
    lower_program(&syntax, &tokens)
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

fn invalid_type_annotation_tokens() -> (SyntaxNode<JvLanguage>, Vec<Token>) {
    let mut column = 0usize;
    let mut tokens = Vec::new();
    tokens.push(make_token(&mut column, TokenType::Val, "val"));
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("answer".to_string()),
        "answer",
    ));
    tokens.push(make_token(&mut column, TokenType::Colon, ":"));
    tokens.push(make_token(
        &mut column,
        TokenType::Number("123".to_string()),
        "123",
    ));
    tokens.push(make_token(&mut column, TokenType::Assign, "="));
    tokens.push(make_token(
        &mut column,
        TokenType::Number("42".to_string()),
        "42",
    ));
    tokens.push(make_token(&mut column, TokenType::Eof, ""));

    let node = build_tree(&tokens, |builder, tokens| {
        builder.start_node(SyntaxKind::ValDeclaration);
        builder.push_token(&tokens[0]);
        builder.start_node(SyntaxKind::BindingPattern);
        builder.push_token(&tokens[1]);
        builder.finish_node();
        builder.start_node(SyntaxKind::TypeAnnotation);
        builder.push_token(&tokens[2]);
        builder.start_node(SyntaxKind::Expression);
        builder.push_token(&tokens[3]);
        builder.finish_node();
        builder.finish_node();
        builder.start_node(SyntaxKind::InitializerClause);
        builder.push_token(&tokens[4]);
        builder.start_node(SyntaxKind::Expression);
        builder.push_token(&tokens[5]);
        builder.finish_node();
        builder.finish_node();
        builder.finish_node();
        builder.push_token(&tokens[6]);
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

#[test]
fn val_declaration_captures_line_doc_comment() {
    let result = lower_source(
        "/// Adds two numbers
val total = 1 + 2",
    );
    assert!(
        result
            .statements
            .iter()
            .any(|stmt| matches!(stmt, Statement::Comment(_))),
        "line doc comments should still surface as comment statements"
    );
    let modifiers = result
        .statements
        .iter()
        .find_map(|statement| match statement {
            Statement::ValDeclaration { modifiers, .. } => Some(modifiers),
            _ => None,
        })
        .expect("val declaration should exist");
    let documentation = modifiers
        .documentation
        .as_ref()
        .expect("documentation metadata should be present");
    assert_eq!(documentation.text.trim(), "Adds two numbers");
    assert!(
        !modifiers.jv_comments.is_empty(),
        "raw jv-only comments should still be retained"
    );
}

#[test]
fn function_declaration_captures_block_doc_comment() {
    let result = lower_source(
        "/** Greets the user. */
fun greet(name: String): String = \"Hello, ${name}\"",
    );
    let modifiers = result
        .statements
        .iter()
        .find_map(|statement| match statement {
            Statement::FunctionDeclaration { modifiers, .. } => Some(modifiers),
            _ => None,
        })
        .expect("function declaration should be present");
    let documentation = modifiers
        .documentation
        .as_ref()
        .expect("block doc should be attached");
    assert!(
        documentation.text.contains("Greets the user"),
        "documentation text should retain block content"
    );
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

fn deprecated_arrow_function_tokens() -> (SyntaxNode<JvLanguage>, Vec<Token>) {
    let mut column = 0usize;
    let mut tokens = Vec::new();
    tokens.push(make_token(&mut column, TokenType::Fun, "fun"));
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("legacy".to_string()),
        "legacy",
    ));
    tokens.push(make_token(&mut column, TokenType::LeftParen, "("));
    tokens.push(make_token(&mut column, TokenType::RightParen, ")"));
    tokens.push(make_token(&mut column, TokenType::Colon, ":"));
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("Int".to_string()),
        "Int",
    ));
    tokens.push(make_token(&mut column, TokenType::Arrow, "->"));
    tokens.push(make_token(
        &mut column,
        TokenType::Number("1".to_string()),
        "1",
    ));
    tokens.push(make_token(&mut column, TokenType::Eof, ""));

    let node = build_tree(&tokens, |builder, tokens| {
        builder.start_node(SyntaxKind::FunctionDeclaration);
        builder.push_token(&tokens[0]);
        builder.push_token(&tokens[1]);
        builder.start_node(SyntaxKind::FunctionParameterList);
        builder.push_token(&tokens[2]);
        builder.push_token(&tokens[3]);
        builder.finish_node();
        builder.start_node(SyntaxKind::FunctionReturnType);
        builder.push_token(&tokens[4]);
        builder.start_node(SyntaxKind::Expression);
        builder.push_token(&tokens[5]);
        builder.finish_node();
        builder.finish_node();
        builder.push_token(&tokens[6]);
        builder.start_node(SyntaxKind::Expression);
        builder.push_token(&tokens[7]);
        builder.finish_node();
        builder.finish_node();
        builder.push_token(&tokens[8]);
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

fn assignment_member_rhs_tokens() -> (SyntaxNode<JvLanguage>, Vec<Token>) {
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
        TokenType::Identifier("other".to_string()),
        "other",
    ));
    tokens.push(make_token(&mut column, TokenType::Dot, "."));
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("profile".to_string()),
        "profile",
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
        builder.push_token(&tokens[5]);
        builder.push_token(&tokens[6]);
        builder.finish_node();
        builder.finish_node();
        builder.push_token(&tokens[7]);
    });

    (node, tokens)
}

fn destructuring_assignment_tokens() -> (SyntaxNode<JvLanguage>, Vec<Token>) {
    let mut column = 0usize;
    let mut tokens = Vec::new();
    tokens.push(make_token(&mut column, TokenType::LeftBracket, "["));
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("first".to_string()),
        "first",
    ));
    tokens.push(make_token(&mut column, TokenType::LayoutComma, ","));
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("second".to_string()),
        "second",
    ));
    tokens.push(make_token(&mut column, TokenType::RightBracket, "]"));
    tokens.push(make_token(&mut column, TokenType::Assign, "="));
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("point".to_string()),
        "point",
    ));
    tokens.push(make_token(&mut column, TokenType::Eof, ""));

    let node = build_tree(&tokens, |builder, tokens| {
        builder.start_node(SyntaxKind::AssignmentStatement);
        builder.start_node(SyntaxKind::AssignmentTarget);
        builder.start_node(SyntaxKind::BindingPattern);
        builder.start_node(SyntaxKind::BindingListPattern);
        builder.push_token(&tokens[0]);

        builder.start_node(SyntaxKind::BindingPattern);
        builder.push_token(&tokens[1]);
        builder.finish_node();

        builder.push_token(&tokens[2]);

        builder.start_node(SyntaxKind::BindingPattern);
        builder.push_token(&tokens[3]);
        builder.finish_node();

        builder.push_token(&tokens[4]);
        builder.finish_node(); // BindingListPattern
        builder.finish_node(); // BindingPattern
        builder.finish_node(); // AssignmentTarget

        builder.push_token(&tokens[5]);
        builder.start_node(SyntaxKind::Expression);
        builder.push_token(&tokens[6]);
        builder.finish_node();
        builder.finish_node();
        builder.push_token(&tokens[7]);
    });

    (node, tokens)
}

fn destructuring_val_tokens() -> (SyntaxNode<JvLanguage>, Vec<Token>) {
    let mut column = 0usize;
    let mut tokens = Vec::new();
    tokens.push(make_token(&mut column, TokenType::Val, "val"));
    tokens.push(make_token(&mut column, TokenType::LeftBracket, "["));
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("first".to_string()),
        "first",
    ));
    tokens.push(make_token(&mut column, TokenType::LayoutComma, ","));
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("second".to_string()),
        "second",
    ));
    tokens.push(make_token(&mut column, TokenType::RightBracket, "]"));
    tokens.push(make_token(&mut column, TokenType::Assign, "="));
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("pair".to_string()),
        "pair",
    ));
    tokens.push(make_token(&mut column, TokenType::Eof, ""));

    let node = build_tree(&tokens, |builder, tokens| {
        builder.start_node(SyntaxKind::ValDeclaration);
        builder.push_token(&tokens[0]);
        builder.start_node(SyntaxKind::BindingPattern);
        builder.start_node(SyntaxKind::BindingListPattern);
        builder.push_token(&tokens[1]);

        builder.start_node(SyntaxKind::BindingPattern);
        builder.push_token(&tokens[2]);
        builder.finish_node();

        builder.push_token(&tokens[3]);

        builder.start_node(SyntaxKind::BindingPattern);
        builder.push_token(&tokens[4]);
        builder.finish_node();

        builder.push_token(&tokens[5]);
        builder.finish_node(); // BindingListPattern
        builder.finish_node(); // BindingPattern

        builder.start_node(SyntaxKind::InitializerClause);
        builder.push_token(&tokens[6]);
        builder.start_node(SyntaxKind::Expression);
        builder.push_token(&tokens[7]);
        builder.finish_node();
        builder.finish_node(); // InitializerClause

        builder.finish_node(); // ValDeclaration
        builder.push_token(&tokens[8]);
    });

    (node, tokens)
}

fn use_statement_tokens() -> (SyntaxNode<JvLanguage>, Vec<Token>) {
    let mut column = 0usize;
    let mut tokens = Vec::new();
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("use".to_string()),
        "use",
    ));
    tokens.push(make_token(&mut column, TokenType::LeftParen, "("));
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("resource".to_string()),
        "resource",
    ));
    tokens.push(make_token(&mut column, TokenType::RightParen, ")"));
    tokens.push(make_token(&mut column, TokenType::LeftBrace, "{"));
    tokens.push(make_token(&mut column, TokenType::RightBrace, "}"));
    tokens.push(make_token(&mut column, TokenType::Eof, ""));

    let node = build_tree(&tokens, |builder, tokens| {
        builder.start_node(SyntaxKind::UseStatement);
        builder.push_token(&tokens[0]);
        builder.push_token(&tokens[1]);
        builder.start_node(SyntaxKind::Expression);
        builder.push_token(&tokens[2]);
        builder.finish_node();
        builder.push_token(&tokens[3]);
        builder.start_node(SyntaxKind::Block);
        builder.push_token(&tokens[4]);
        builder.push_token(&tokens[5]);
        builder.finish_node();
        builder.finish_node();
        builder.push_token(&tokens[6]);
    });

    (node, tokens)
}

fn defer_statement_tokens() -> (SyntaxNode<JvLanguage>, Vec<Token>) {
    let mut column = 0usize;
    let mut tokens = Vec::new();
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("defer".to_string()),
        "defer",
    ));
    tokens.push(make_token(&mut column, TokenType::LeftBrace, "{"));
    tokens.push(make_token(&mut column, TokenType::RightBrace, "}"));
    tokens.push(make_token(&mut column, TokenType::Eof, ""));

    let node = build_tree(&tokens, |builder, tokens| {
        builder.start_node(SyntaxKind::DeferStatement);
        builder.push_token(&tokens[0]);
        builder.start_node(SyntaxKind::Block);
        builder.push_token(&tokens[1]);
        builder.push_token(&tokens[2]);
        builder.finish_node();
        builder.finish_node();
        builder.push_token(&tokens[3]);
    });

    (node, tokens)
}

fn spawn_statement_tokens() -> (SyntaxNode<JvLanguage>, Vec<Token>) {
    let mut column = 0usize;
    let mut tokens = Vec::new();
    tokens.push(make_token(
        &mut column,
        TokenType::Identifier("spawn".to_string()),
        "spawn",
    ));
    tokens.push(make_token(&mut column, TokenType::LeftBrace, "{"));
    tokens.push(make_token(&mut column, TokenType::RightBrace, "}"));
    tokens.push(make_token(&mut column, TokenType::Eof, ""));

    let node = build_tree(&tokens, |builder, tokens| {
        builder.start_node(SyntaxKind::SpawnStatement);
        builder.push_token(&tokens[0]);
        builder.start_node(SyntaxKind::Block);
        builder.push_token(&tokens[1]);
        builder.push_token(&tokens[2]);
        builder.finish_node();
        builder.finish_node();
        builder.push_token(&tokens[3]);
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
                        binding,
                        type_annotation,
                        initializer,
                        modifiers,
                        origin,
                        ..
                    } => {
                        assert_eq!(name, "answer");
                        let pattern = binding
                            .as_ref()
                            .expect("expected binding pattern for val declaration");
                        match pattern {
                            BindingPatternKind::Identifier { name, .. } => {
                                assert_eq!(name, "answer")
                            }
                            other => panic!("expected identifier binding, got {:?}", other),
                        }
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
            build: invalid_type_annotation_tokens,
            verify: Box::new(|result| {
                assert_eq!(result.statements.len(), 1, "expected single statement");
                assert_eq!(
                    result.diagnostics.len(),
                    1,
                    "expected a diagnostic for invalid type annotation"
                );
                let diagnostic = &result.diagnostics[0];
                assert_eq!(diagnostic.severity, LoweringDiagnosticSeverity::Error);
                assert!(
                    diagnostic.message.contains("想定外")
                        || diagnostic.message.contains("型注釈")
                        || diagnostic.message.contains("識別子"),
                    "unexpected diagnostic message: {}",
                    diagnostic.message
                );
                match &result.statements[0] {
                    Statement::ValDeclaration {
                        type_annotation, ..
                    } => {
                        assert!(
                            type_annotation.is_none(),
                            "type annotation should be dropped on failure"
                        );
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
            build: deprecated_arrow_function_tokens,
            verify: Box::new(|result| {
                assert_eq!(
                    result.diagnostics.len(),
                    1,
                    "expected a diagnostic for deprecated arrow syntax"
                );
                let diagnostic = &result.diagnostics[0];
                assert_eq!(diagnostic.severity, LoweringDiagnosticSeverity::Error);
                assert!(
                    diagnostic
                        .message
                        .contains("`->` 式ボディはサポートされません"),
                    "unexpected diagnostic message: {}",
                    diagnostic.message
                );
                assert_eq!(
                    result.statements.len(),
                    1,
                    "function should still be lowered for recovery"
                );
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
                        let pattern = stmt
                            .binding
                            .pattern
                            .as_ref()
                            .expect("expected binding pattern on loop binding");
                        match pattern {
                            BindingPatternKind::Identifier { name, .. } => assert_eq!(name, "item"),
                            other => panic!("expected identifier loop binding, got {:?}", other),
                        }
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
                assert!(
                    result.diagnostics.is_empty(),
                    "unexpected diagnostics: {:?}",
                    result.diagnostics
                );
                match result.statements.first().expect("expected val statement") {
                    Statement::ValDeclaration { initializer, .. } => match initializer {
                        Expression::Binary {
                            op, left, right, ..
                        } => {
                            assert_eq!(op, &BinaryOp::Add);
                            match left.as_ref() {
                                Expression::Identifier(name, _) => assert_eq!(name, "foo"),
                                other => panic!(
                                    "expected identifier lhs for binary expression, got {:?}",
                                    other
                                ),
                            }
                            match right.as_ref() {
                                Expression::Identifier(name, _) => assert_eq!(name, "bar"),
                                other => panic!(
                                    "expected identifier rhs for binary expression, got {:?}",
                                    other
                                ),
                            }
                        }
                        other => panic!("expected binary addition, got {:?}", other),
                    },
                    other => panic!("expected val declaration, got {:?}", other),
                }
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
            build: assignment_member_rhs_tokens,
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
                    Statement::Assignment { value, .. } => match value {
                        Expression::MemberAccess {
                            object, property, ..
                        } => {
                            assert_eq!(property, "profile");
                            match object.as_ref() {
                                Expression::Identifier(name, _) => assert_eq!(name, "other"),
                                other => panic!(
                                    "expected identifier object for rhs member access, got {:?}",
                                    other
                                ),
                            }
                        }
                        other => panic!("expected member access rhs, got {:?}", other),
                    },
                    other => panic!("expected assignment statement, got {:?}", other),
                }
            }),
        },
        LoweringCase {
            build: destructuring_assignment_tokens,
            verify: Box::new(|result| {
                assert!(
                    result.diagnostics.is_empty(),
                    "unexpected diagnostics for destructuring: {:?}",
                    result.diagnostics
                );
                let statement = result
                    .statements
                    .first()
                    .expect("expected assignment statement for destructuring");
                match statement {
                    Statement::Assignment {
                        target,
                        binding_pattern,
                        value,
                        ..
                    } => {
                        let pattern = binding_pattern
                            .as_ref()
                            .expect("expected binding pattern for destructuring assignment");
                        match pattern {
                            BindingPatternKind::List { elements, .. } => {
                                assert_eq!(elements.len(), 2);
                                match &elements[0] {
                                    BindingPatternKind::Identifier { name, .. } => {
                                        assert_eq!(name, "first")
                                    }
                                    other => {
                                        panic!("unexpected first binding element: {:?}", other)
                                    }
                                }
                                match &elements[1] {
                                    BindingPatternKind::Identifier { name, .. } => {
                                        assert_eq!(name, "second")
                                    }
                                    other => {
                                        panic!("unexpected second binding element: {:?}", other)
                                    }
                                }
                            }
                            other => panic!("expected list binding pattern, got {:?}", other),
                        }
                        match target {
                            Expression::Identifier(name, _) => assert_eq!(name, "first"),
                            other => panic!("unexpected lowered target expression: {:?}", other),
                        }
                        match value {
                            Expression::Identifier(name, _) => assert_eq!(name, "point"),
                            other => {
                                panic!("unexpected destructuring value expression: {:?}", other)
                            }
                        }
                    }
                    other => panic!("expected assignment statement, got {:?}", other),
                }
            }),
        },
        LoweringCase {
            build: destructuring_val_tokens,
            verify: Box::new(|result| {
                assert!(
                    result.diagnostics.is_empty(),
                    "unexpected diagnostics for destructuring val: {:?}",
                    result.diagnostics
                );
                let statement = result
                    .statements
                    .first()
                    .expect("expected val declaration for destructuring");
                match statement {
                    Statement::ValDeclaration {
                        name,
                        binding,
                        initializer,
                        ..
                    } => {
                        assert_eq!(name, "first");
                        let pattern = binding
                            .as_ref()
                            .expect("expected binding pattern for destructuring val");
                        match pattern {
                            BindingPatternKind::List { elements, .. } => {
                                assert_eq!(elements.len(), 2);
                                match &elements[0] {
                                    BindingPatternKind::Identifier { name, .. } => {
                                        assert_eq!(name, "first")
                                    }
                                    other => {
                                        panic!("unexpected first binding element: {:?}", other)
                                    }
                                }
                                match &elements[1] {
                                    BindingPatternKind::Identifier { name, .. } => {
                                        assert_eq!(name, "second")
                                    }
                                    other => {
                                        panic!("unexpected second binding element: {:?}", other)
                                    }
                                }
                            }
                            other => panic!("expected list binding pattern, got {:?}", other),
                        }
                        match initializer {
                            Expression::Identifier(name, _) => assert_eq!(name, "pair"),
                            other => panic!("unexpected destructuring initializer: {:?}", other),
                        }
                    }
                    other => panic!("expected val declaration, got {:?}", other),
                }
            }),
        },
        LoweringCase {
            build: use_statement_tokens,
            verify: Box::new(|result| {
                assert!(
                    result.diagnostics.is_empty(),
                    "unexpected diagnostics: {:?}",
                    result.diagnostics
                );
                match result
                    .statements
                    .first()
                    .expect("expected resource management statement")
                {
                    Statement::ResourceManagement(ResourceManagement::Use {
                        resource,
                        body,
                        ..
                    }) => {
                        match resource.as_ref() {
                            Expression::Identifier(name, _) => assert_eq!(name, "resource"),
                            other => panic!("expected identifier resource, got {:?}", other),
                        }
                        match body.as_ref() {
                            Expression::Block { .. } => {}
                            other => panic!("expected block body, got {:?}", other),
                        }
                    }
                    other => panic!(
                        "expected use resource management statement, got {:?}",
                        other
                    ),
                }
            }),
        },
        LoweringCase {
            build: defer_statement_tokens,
            verify: Box::new(|result| {
                assert!(
                    result.diagnostics.is_empty(),
                    "unexpected diagnostics: {:?}",
                    result.diagnostics
                );
                match result.statements.first().expect("expected defer statement") {
                    Statement::ResourceManagement(ResourceManagement::Defer { body, .. }) => {
                        match body.as_ref() {
                            Expression::Block { .. } => {}
                            other => panic!("expected block body for defer, got {:?}", other),
                        }
                    }
                    other => panic!(
                        "expected defer resource management statement, got {:?}",
                        other
                    ),
                }
            }),
        },
        LoweringCase {
            build: spawn_statement_tokens,
            verify: Box::new(|result| {
                assert!(
                    result.diagnostics.is_empty(),
                    "unexpected diagnostics: {:?}",
                    result.diagnostics
                );
                match result.statements.first().expect("expected spawn statement") {
                    Statement::Concurrency(ConcurrencyConstruct::Spawn { body, .. }) => {
                        match body.as_ref() {
                            Expression::Block { .. } => {}
                            other => panic!("expected block body for spawn, got {:?}", other),
                        }
                    }
                    other => panic!("expected spawn concurrency statement, got {:?}", other),
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

#[test]
fn expression_lowering_respects_operator_precedence() {
    let result = lower_source("val result = 1 + 2 * 3");
    assert!(
        result.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        result.diagnostics
    );

    let initializer = match result.statements.first().expect("expected val statement") {
        Statement::ValDeclaration {
            name, initializer, ..
        } => {
            assert_eq!(name, "result");
            initializer
        }
        other => panic!("expected val declaration, got {:?}", other),
    };

    match initializer {
        Expression::Binary {
            op, left, right, ..
        } => {
            assert_eq!(op, &BinaryOp::Add);
            match left.as_ref() {
                Expression::Literal(Literal::Number(value), _) => assert_eq!(value, "1"),
                other => panic!("expected numeric literal lhs, got {:?}", other),
            }
            match right.as_ref() {
                Expression::Binary {
                    op: inner_op,
                    left: mul_left,
                    right: mul_right,
                    ..
                } => {
                    assert_eq!(inner_op, &BinaryOp::Multiply);
                    match mul_left.as_ref() {
                        Expression::Literal(Literal::Number(value), _) => assert_eq!(value, "2"),
                        other => {
                            panic!("expected numeric literal for multiply lhs, got {:?}", other)
                        }
                    }
                    match mul_right.as_ref() {
                        Expression::Literal(Literal::Number(value), _) => assert_eq!(value, "3"),
                        other => {
                            panic!("expected numeric literal for multiply rhs, got {:?}", other)
                        }
                    }
                }
                other => panic!("expected multiplicative rhs, got {:?}", other),
            }
        }
        other => panic!("expected additive binary expression, got {:?}", other),
    }
}

#[test]
fn expression_lowering_handles_lambda_literal() {
    let result = lower_source("val twice = { x -> x * 2 }");
    assert!(
        result.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        result.diagnostics
    );

    let initializer = match result.statements.first().expect("expected val statement") {
        Statement::ValDeclaration {
            name, initializer, ..
        } => {
            assert_eq!(name, "twice");
            initializer
        }
        other => panic!("expected val declaration, got {:?}", other),
    };

    match initializer {
        Expression::Lambda {
            parameters, body, ..
        } => {
            assert_eq!(parameters.len(), 1);
            let Parameter { name, .. } = &parameters[0];
            assert_eq!(name, "x");
            match body.as_ref() {
                Expression::Binary {
                    op, left, right, ..
                } => {
                    assert_eq!(op, &BinaryOp::Multiply);
                    match left.as_ref() {
                        Expression::Identifier(name, _) => assert_eq!(name, "x"),
                        other => panic!("expected identifier lhs, got {:?}", other),
                    }
                    match right.as_ref() {
                        Expression::Literal(Literal::Number(value), _) => assert_eq!(value, "2"),
                        other => panic!("expected numeric literal rhs, got {:?}", other),
                    }
                }
                other => panic!("expected binary multiply body, got {:?}", other),
            }
        }
        other => panic!("expected lambda expression, got {:?}", other),
    }
}

#[test]
fn expression_lowering_handles_trailing_lambda_call() {
    let result = lower_source("val mapped = items.map { value -> value + 1 }");
    assert!(
        result.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        result.diagnostics
    );

    let initializer = match result.statements.first().expect("expected val statement") {
        Statement::ValDeclaration {
            name, initializer, ..
        } => {
            assert_eq!(name, "mapped");
            initializer
        }
        other => panic!("expected val declaration, got {:?}", other),
    };

    match initializer {
        Expression::Call { function, args, .. } => {
            match function.as_ref() {
                Expression::MemberAccess {
                    object, property, ..
                } => {
                    match object.as_ref() {
                        Expression::Identifier(name, _) => assert_eq!(name, "items"),
                        other => panic!("expected identifier receiver, got {:?}", other),
                    }
                    assert_eq!(property, "map");
                }
                other => panic!("expected member access function, got {:?}", other),
            }

            assert_eq!(args.len(), 1, "expected trailing lambda argument only");
            match &args[0] {
                Argument::Positional(expr) => match expr {
                    Expression::Lambda {
                        parameters, body, ..
                    } => {
                        assert_eq!(parameters.len(), 1);
                        assert_eq!(parameters[0].name, "value");
                        match body.as_ref() {
                            Expression::Binary {
                                op, left, right, ..
                            } => {
                                assert_eq!(op, &BinaryOp::Add);
                                match left.as_ref() {
                                    Expression::Identifier(name, _) => assert_eq!(name, "value"),
                                    other => panic!(
                                        "expected identifier lambda body lhs, got {:?}",
                                        other
                                    ),
                                }
                                match right.as_ref() {
                                    Expression::Literal(Literal::Number(value), _) => {
                                        assert_eq!(value, "1")
                                    }
                                    other => panic!(
                                        "expected numeric literal lambda body rhs, got {:?}",
                                        other
                                    ),
                                }
                            }
                            other => {
                                panic!("expected additive lambda body expression, got {:?}", other)
                            }
                        }
                    }
                    other => panic!("expected lambda argument, got {:?}", other),
                },
                other => panic!("expected positional argument, got {:?}", other),
            }
        }
        other => panic!("expected call expression, got {:?}", other),
    }
}

#[test]
fn expression_trailing_lambda_appends_to_parenthesized_arguments() {
    let result = lower_source("val bucket = cache.computeIfAbsent(key) { value -> value + 1 }");
    assert!(
        result.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        result.diagnostics
    );

    let initializer = match result.statements.first().expect("expected val statement") {
        Statement::ValDeclaration {
            name, initializer, ..
        } => {
            assert_eq!(name, "bucket");
            initializer
        }
        other => panic!("expected val declaration, got {:?}", other),
    };

    match initializer {
        Expression::Call { function, args, .. } => {
            match function.as_ref() {
                Expression::MemberAccess {
                    object, property, ..
                } => {
                    match object.as_ref() {
                        Expression::Identifier(name, _) => assert_eq!(name, "cache"),
                        other => panic!("expected identifier receiver, got {:?}", other),
                    }
                    assert_eq!(property, "computeIfAbsent");
                }
                other => panic!("expected member access function, got {:?}", other),
            }

            assert_eq!(
                args.len(),
                2,
                "expected existing argument plus trailing lambda"
            );
            match &args[0] {
                Argument::Positional(expr) => match expr {
                    Expression::Identifier(name, _) => assert_eq!(name, "key"),
                    other => panic!("expected identifier key argument, got {:?}", other),
                },
                other => panic!("expected positional argument, got {:?}", other),
            }
            match &args[1] {
                Argument::Positional(expr) => match expr {
                    Expression::Lambda { parameters, .. } => {
                        assert_eq!(parameters.len(), 1, "lambda should have one parameter");
                        assert_eq!(parameters[0].name, "value");
                    }
                    other => panic!("expected lambda trailing argument, got {:?}", other),
                },
                other => panic!("expected positional lambda argument, got {:?}", other),
            }
        }
        other => panic!("expected call expression initializer, got {:?}", other),
    }
}

#[test]
fn expression_trailing_lambda_without_parameters_wraps_body() {
    let result = lower_source("val bucket = cache.computeIfAbsent(key) { ArrayList<String>() }");
    assert!(
        result.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        result.diagnostics
    );

    let initializer = match result.statements.first().expect("expected val statement") {
        Statement::ValDeclaration { initializer, .. } => initializer,
        other => panic!("expected val declaration, got {:?}", other),
    };

    match initializer {
        Expression::Call { args, .. } => {
            assert_eq!(args.len(), 2, "expected key and trailing lambda arguments");
            match &args[1] {
                Argument::Positional(expr) => match expr {
                    Expression::Lambda { parameters, .. } => {
                        assert!(
                            parameters.is_empty(),
                            "implicit trailing lambda should not synthesize parameters yet"
                        );
                    }
                    other => panic!("expected trailing lambda expression, got {:?}", other),
                },
                other => panic!("expected positional trailing lambda, got {:?}", other),
            }
        }
        other => panic!("expected call expression initializer, got {:?}", other),
    }
}

#[test]
fn expression_lowering_handles_when_with_subject_and_guard() {
    let source = r#"
    package demo
    val value = 0
    val result = when (value) {
        is String && value.startsWith("A") -> value.length
        in 0..10 -> 42
        else -> 0
    }
    "#;

    let result = lower_source(source);
    assert!(
        result.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        result.diagnostics
    );

    let when_expr = result
        .statements
        .iter()
        .find_map(|statement| {
            if let Statement::ValDeclaration {
                name, initializer, ..
            } = statement
            {
                if name == "result" {
                    return Some(initializer);
                }
            }
            None
        })
        .expect("expected val result declaration");

    let (subject_expr, arms, else_branch) = match when_expr {
        Expression::When {
            expr,
            arms,
            else_arm,
            ..
        } => (expr, arms, else_arm),
        other => panic!("expected when expression, got {:?}", other),
    };

    match subject_expr.as_ref().map(|expr| expr.as_ref()) {
        Some(Expression::Identifier(name, _)) => assert_eq!(name, "value"),
        other => panic!("expected identifier subject, got {:?}", other),
    }

    assert_eq!(arms.len(), 2, "expected two when arms");

    let first_arm = &arms[0];
    match &first_arm.pattern {
        Pattern::Constructor { name, .. } => assert_eq!(name, "String"),
        other => panic!("expected constructor pattern, got {:?}", other),
    }
    assert!(
        first_arm.guard.is_some(),
        "expected guard expression on first arm"
    );
    match &first_arm.body {
        Expression::MemberAccess { property, .. } => assert_eq!(property, "length"),
        other => panic!("unexpected first arm body {:?}", other),
    }

    let second_arm = &arms[1];
    match &second_arm.pattern {
        Pattern::Range { inclusive_end, .. } => assert!(!inclusive_end),
        other => panic!("expected range pattern, got {:?}", other),
    }
    assert!(
        second_arm.guard.is_none(),
        "range arm should not have guard"
    );
    match &second_arm.body {
        Expression::Literal(Literal::Number(value), _) => assert_eq!(value, "42"),
        other => panic!("unexpected second arm body {:?}", other),
    }

    let else_expr = else_branch
        .as_ref()
        .expect("expected else branch expression")
        .as_ref();
    match else_expr {
        Expression::Literal(Literal::Number(value), _) => assert_eq!(value, "0"),
        other => panic!("unexpected else branch {:?}", other),
    }
}

#[test]
fn expression_lowering_handles_subjectless_when_expression() {
    let source = r#"
    package demo
    val value = 5
    val label = when {
        value > 10 -> "large"
        else -> "small"
    }
    "#;

    let result = lower_source(source);
    assert!(
        result.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        result.diagnostics
    );

    let when_expr = result
        .statements
        .iter()
        .find_map(|statement| {
            if let Statement::ValDeclaration {
                name, initializer, ..
            } = statement
            {
                if name == "label" {
                    return Some(initializer);
                }
            }
            None
        })
        .expect("expected val label declaration");

    let (subject_expr, arms, else_branch) = match when_expr {
        Expression::When {
            expr,
            arms,
            else_arm,
            ..
        } => (expr, arms, else_arm),
        other => panic!("expected when expression, got {:?}", other),
    };

    assert!(
        subject_expr.is_none(),
        "subjectless when should not have a subject expression"
    );
    assert_eq!(arms.len(), 1, "expected single conditional arm");

    let arm = &arms[0];
    match &arm.pattern {
        Pattern::Wildcard(_) => {}
        other => panic!("expected wildcard pattern, got {:?}", other),
    }
    let guard = arm.guard.as_ref().expect("expected guard expression");
    match guard {
        Expression::Binary { op, .. } => assert_eq!(op, &BinaryOp::Greater),
        other => panic!("unexpected guard expression {:?}", other),
    }
    match &arm.body {
        Expression::Literal(Literal::String(text), _) => assert_eq!(text, "large"),
        other => panic!("unexpected arm body {:?}", other),
    }

    let else_expr = else_branch
        .as_ref()
        .expect("expected else branch expression")
        .as_ref();
    match else_expr {
        Expression::Literal(Literal::String(text), _) => assert_eq!(text, "small"),
        other => panic!("unexpected else branch {:?}", other),
    }
}

#[test]
fn string_interpolation_lowering_builds_parts() {
    let source = include_str!(
        "../../../../../tests/parser_rowan_specs/fixtures/string_interpolation_and_json.jv"
    );

    let result = lower_source(source);
    assert!(
        result.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        result.diagnostics
    );

    let function_body = result
        .statements
        .iter()
        .find_map(|statement| match statement {
            Statement::FunctionDeclaration { name, body, .. } if name == "literalsDemo" => {
                Some(body.as_ref())
            }
            _ => None,
        })
        .expect("expected literalsDemo function declaration");

    let block_statements = match function_body {
        Expression::Block { statements, .. } => statements,
        other => panic!("expected block body, got {:?}", other),
    };

    let kinds: Vec<StatementKindKey> = block_statements
        .iter()
        .map(StatementKindKey::from_statement)
        .collect();
    assert_eq!(
        kinds,
        vec![
            StatementKindKey::ValDeclaration,
            StatementKindKey::ValDeclaration,
            StatementKindKey::Expression,
            StatementKindKey::ValDeclaration,
            StatementKindKey::Expression,
            StatementKindKey::ValDeclaration,
            StatementKindKey::VarDeclaration,
            StatementKindKey::VarDeclaration,
            StatementKindKey::Expression,
            StatementKindKey::Expression,
            StatementKindKey::Expression,
            StatementKindKey::Expression,
            StatementKindKey::Expression,
            StatementKindKey::Expression,
            StatementKindKey::Expression,
            StatementKindKey::Expression
        ],
        "unexpected block statement kinds"
    );

    let message_expr = block_statements
        .iter()
        .find_map(|statement| match statement {
            Statement::ValDeclaration {
                name, initializer, ..
            } if name == "message" => Some(initializer),
            _ => None,
        })
        .expect("expected message val declaration");

    match message_expr {
        Expression::StringInterpolation { parts, .. } => {
            assert_eq!(parts.len(), 3, "expected text, expression, text segments");
            match &parts[0] {
                StringPart::Text(text) => assert_eq!(text, "Hello, "),
                other => panic!("unexpected first string part {:?}", other),
            }
            match &parts[1] {
                StringPart::Expression(expr) => match expr {
                    Expression::Identifier(identifier, _) => assert_eq!(identifier, "name"),
                    other => panic!("unexpected interpolation expression {:?}", other),
                },
                other => panic!("expected expression part, got {:?}", other),
            }
            match &parts[2] {
                StringPart::Text(text) => assert_eq!(text, "!"),
                other => panic!("unexpected final string part {:?}", other),
            }
        }
        other => panic!("expected string interpolation, got {:?}", other),
    }

    let template_expr = block_statements
        .iter()
        .find_map(|statement| match statement {
            Statement::ValDeclaration {
                name, initializer, ..
            } if name == "template" => Some(initializer),
            _ => None,
        })
        .expect("expected template val declaration");

    match template_expr {
        Expression::MultilineString(literal) => {
            assert_eq!(literal.kind, MultilineKind::TripleQuote);
            assert!(
                literal.parts.iter().any(|part| matches!(
                    part,
                    StringPart::Expression(Expression::Identifier(identifier, _)) if identifier == "name"
                )),
                "expected embedded expression referencing name"
            );
        }
        other => panic!("expected multiline string literal, got {:?}", other),
    }
}

#[test]
fn multiline_string_interpolation_handles_complex_patterns() {
    let source =
        include_str!("../../../../../tests/parser_rowan_specs/fixtures/multiline_interpolation.jv");

    let result = lower_source(source);
    assert!(
        result.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        result.diagnostics
    );

    let function_body = result
        .statements
        .iter()
        .find_map(|statement| match statement {
            Statement::FunctionDeclaration { name, body, .. }
                if name == "multilineInterpolationDemo" =>
            {
                Some(body.as_ref())
            }
            _ => None,
        })
        .expect("expected multilineInterpolationDemo function declaration");

    let block_statements = match function_body {
        Expression::Block { statements, .. } => statements,
        other => panic!("expected block body, got {:?}", other),
    };

    let find_initializer = |name: &str| {
        block_statements
            .iter()
            .find_map(|statement| match statement {
                Statement::ValDeclaration {
                    name: binding_name,
                    initializer,
                    ..
                } if binding_name == name => Some(initializer),
                _ => None,
            })
            .unwrap_or_else(|| panic!("expected val declaration for {name}"))
    };

    let greeting_expr = find_initializer("greeting");
    match greeting_expr {
        Expression::MultilineString(literal) => {
            assert!(
                literal.parts.iter().any(|part| matches!(
                    part,
                    StringPart::Expression(Expression::Identifier(identifier, _))
                        if identifier == "userName"
                )),
                "greeting should reference userName"
            );
            assert!(
                literal.parts.iter().any(|part| matches!(
                    part,
                    StringPart::Text(text) if text.contains("Welcome to the pipeline.")
                )),
                "greeting should include trailing text after interpolation"
            );
        }
        other => panic!("expected multiline string literal, got {:?}", other),
    }

    let tag_summary_expr = find_initializer("tagSummary");
    match tag_summary_expr {
        Expression::MultilineString(literal) => {
            let expr_parts: Vec<&Expression> = literal
                .parts
                .iter()
                .filter_map(|part| match part {
                    StringPart::Expression(expr) => Some(expr),
                    _ => None,
                })
                .collect();
            assert_eq!(
                expr_parts.len(),
                3,
                "expected three expressions in tagSummary"
            );

            match expr_parts[0] {
                Expression::Call { .. } => {}
                other => panic!("expected join call expression, got {:?}", other),
            }
            match expr_parts[1] {
                Expression::IndexAccess { .. } => {}
                other => panic!("expected index access expression, got {:?}", other),
            }
            match expr_parts[2] {
                Expression::Call { .. } => {}
                other => panic!("expected chained method call expression, got {:?}", other),
            }
        }
        other => panic!("expected multiline string literal, got {:?}", other),
    }

    let inline_expr = find_initializer("inline");
    match inline_expr {
        Expression::MultilineString(literal) => {
            let expr_parts: Vec<&Expression> = literal
                .parts
                .iter()
                .filter_map(|part| match part {
                    StringPart::Expression(expr) => Some(expr),
                    _ => None,
                })
                .collect();
            assert_eq!(expr_parts.len(), 2, "inline should contain two expressions");
            match expr_parts[0] {
                Expression::Identifier(identifier, _) if identifier == "userName" => {}
                other => panic!("expected userName identifier, got {:?}", other),
            }
            match expr_parts[1] {
                Expression::Identifier(identifier, _) if identifier == "status" => {}
                other => panic!("expected status identifier, got {:?}", other),
            }
            assert!(
                literal.parts.iter().any(|part| matches!(
                    part,
                    StringPart::Text(text) if text.ends_with("suffix")
                )),
                "inline string should retain trailing literal text"
            );
        }
        other => panic!("expected multiline string literal, got {:?}", other),
    }

    let tight_expr = find_initializer("tight");
    match tight_expr {
        Expression::MultilineString(literal) => {
            let expr_parts: Vec<&Expression> = literal
                .parts
                .iter()
                .filter_map(|part| match part {
                    StringPart::Expression(expr) => Some(expr),
                    _ => None,
                })
                .collect();
            assert_eq!(
                expr_parts.len(),
                3,
                "tight should contain three expressions"
            );
            match expr_parts[0] {
                Expression::Identifier(identifier, _) if identifier == "userName" => {}
                other => panic!("expected userName identifier, got {:?}", other),
            }
            match expr_parts[1] {
                Expression::IndexAccess { .. } => {}
                other => panic!("expected first index access, got {:?}", other),
            }
            match expr_parts[2] {
                Expression::IndexAccess { .. } => {}
                other => panic!("expected second index access, got {:?}", other),
            }
            assert!(
                literal.parts.iter().any(|part| matches!(
                    part,
                    StringPart::Text(text) if text.contains("tail")
                )),
                "tight multiline string should preserve trailing segment"
            );
        }
        other => panic!("expected multiline string literal, got {:?}", other),
    }
}

#[test]
fn nested_package_fixture_preserves_interpolation_identifiers() {
    let source = include_str!("../../../../../jv/tests/fixtures/package/nested_package.jv");
    let result = lower_source(source);
    assert!(
        result.diagnostics.is_empty(),
        "unexpected diagnostics while lowering nested_package.jv: {:?}",
        result.diagnostics
    );

    let function_body = result
        .statements
        .iter()
        .find_map(|statement| match statement {
            Statement::FunctionDeclaration { name, body, .. } if name == "greet" => Some(body),
            _ => None,
        })
        .expect("expected greet function in nested_package fixture");

    let return_expr = match &**function_body {
        Expression::Block { statements, .. } => statements
            .iter()
            .find_map(|statement| match statement {
                Statement::Return {
                    value: Some(expr), ..
                } => Some(expr),
                _ => None,
            })
            .expect("expected return statement in greet body"),
        other => panic!("expected block body for greet, got {:?}", other),
    };

    match return_expr {
        Expression::StringInterpolation { parts, .. } => match parts.get(1) {
            Some(StringPart::Expression(Expression::Identifier(identifier, _))) => {
                assert_eq!(
                    identifier, "name",
                    "interpolation identifier should match parameter name"
                );
            }
            other => panic!("expected identifier interpolation part, got {:?}", other),
        },
        other => panic!(
            "expected string interpolation as return value, got {:?}",
            other
        ),
    }
}

#[test]
fn json_literal_lowering_builds_object_tree() {
    let source = r#"
    package fixtures
    val config = {
      "name": "jv",
      "enabled": true,
      "values": [1, 2, 3]
    }
    "#;

    let result = lower_source(source);
    assert!(
        result.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        result.diagnostics
    );

    let config_expr = result
        .statements
        .iter()
        .find_map(|statement| match statement {
            Statement::ValDeclaration {
                name, initializer, ..
            } if name == "config" => Some(initializer),
            _ => None,
        })
        .expect("expected config val declaration");

    let literal: &JsonLiteral = match config_expr {
        Expression::JsonLiteral(literal) => literal,
        other => panic!("expected json literal expression, got {:?}", other),
    };

    match &literal.value {
        JsonValue::Object { entries, .. } => {
            assert_eq!(entries.len(), 3, "expected three object entries");

            let mut name_found = false;
            let mut enabled_found = false;
            let mut values_found = false;

            for entry in entries {
                match entry.key.as_str() {
                    "name" => {
                        name_found = true;
                        match &entry.value {
                            JsonValue::String { value, .. } => assert_eq!(value, "jv"),
                            other => panic!("expected string value for name, got {:?}", other),
                        }
                    }
                    "enabled" => {
                        enabled_found = true;
                        match &entry.value {
                            JsonValue::Boolean { value, .. } => assert!(*value),
                            other => panic!("expected boolean value for enabled, got {:?}", other),
                        }
                    }
                    "values" => {
                        values_found = true;
                        match &entry.value {
                            JsonValue::Array { elements, .. } => {
                                let literals: Vec<&str> = elements
                                    .iter()
                                    .map(|element| match element {
                                        JsonValue::Number { literal, .. } => literal.as_str(),
                                        other => panic!(
                                            "expected numeric array elements, got {:?}",
                                            other
                                        ),
                                    })
                                    .collect();
                                assert_eq!(literals, vec!["1", "2", "3"]);
                            }
                            other => panic!("expected array value for values, got {:?}", other),
                        }
                    }
                    other => panic!("unexpected JSON entry key {:?}", other),
                }
            }

            assert!(name_found, "missing name entry");
            assert!(enabled_found, "missing enabled entry");
            assert!(values_found, "missing values entry");
        }
        other => panic!("expected JSON object literal, got {:?}", other),
    }

    assert!(
        literal.leading_comments.is_empty() && literal.trailing_comments.is_empty(),
        "unexpected JSON comments in literal"
    );
}

#[test]
fn data_class_lowering_produces_constructor_properties() {
    let source = "data class Point(val x: Double, var y: Double)";

    let result = lower_source(source);
    assert!(
        result.diagnostics.is_empty(),
        "unexpected lowering diagnostics: {:?}",
        result.diagnostics
    );
    assert_eq!(
        result.statements.len(),
        1,
        "expected exactly one top-level statement"
    );

    let data_class = result
        .statements
        .iter()
        .find_map(|statement| match statement {
            Statement::DataClassDeclaration { .. } => Some(statement),
            _ => None,
        })
        .expect("data class declaration to be lowered");

    match data_class {
        Statement::DataClassDeclaration {
            name,
            parameters,
            is_mutable,
            span,
            ..
        } => {
            assert_eq!(name, "Point");
            assert!(
                *is_mutable,
                "expected mutable data class due to var parameter"
            );
            assert!(
                span.start_line > 0 || span.end_line > 0,
                "data class span should not be dummy"
            );

            assert_eq!(parameters.len(), 2, "expected two constructor parameters");

            let first = &parameters[0];
            assert_eq!(first.name, "x");
            assert_eq!(first.modifiers.property, ParameterProperty::Val);
            match &first.type_annotation {
                Some(TypeAnnotation::Simple(name)) => assert_eq!(name, "Double"),
                other => panic!("expected Double annotation for x, got {:?}", other),
            }

            let second = &parameters[1];
            assert_eq!(second.name, "y");
            assert_eq!(second.modifiers.property, ParameterProperty::Var);
            match &second.type_annotation {
                Some(TypeAnnotation::Simple(name)) => assert_eq!(name, "Double"),
                other => panic!("expected Double annotation for y, got {:?}", other),
            }
        }
        _ => unreachable!("matched in find_map above"),
    }
}
