use jv_parser2::{Arena, Parser, Source, lexer::Lexer, parser::incremental::IncrementalCache};
use jv_ast::Statement;

#[test]
fn parse_smoke_empty_program() {
    let source = Source::from_str("");
    let lexer = Lexer::new(source);
    let arena = Arena::new();
    let mut parser = Parser::new(lexer, &arena);
    let result = parser.parse();
    assert!(result.diagnostics.is_empty());
    assert!(result.ast.is_some());
}

#[test]
fn parse_incremental_delegates() {
    let source = Source::from_str("val x = 1");
    let lexer = Lexer::new(source);
    let arena = Arena::new();
    let parser = Parser::new(lexer, &arena);
    let mut cache = IncrementalCache::default();
    let change = jv_parser2::parser::incremental::TextChange {
        start: 0,
        old_end: 0,
        new_text: String::new(),
        start_line: 0,
        end_line: 0,
    };
    jv_parser2::parser::incremental::parse_incremental(parser, &change, &mut cache);
}

#[test]
fn spans_respect_newlines() {
    let source = Source::from_str("val a = 1\nval b = 2");
    let lexer = Lexer::new(source);
    let arena = Arena::new();
    let mut parser = Parser::new(lexer, &arena);
    let result = parser.parse();
    let prog = result.ast.unwrap().to_owned();
    assert_eq!(prog.statements.len(), 2, "two statements expected");
    let second = &prog.statements[1];
    assert!(
        second.span().start_line > 0,
        "line info should reflect newline, got {:?}",
        second.span()
    );
}

#[test]
fn expression_spans_respect_newlines() {
    let source = Source::from_str("val a = 1\nval b = 2");
    let lexer = Lexer::new(source);
    let arena = Arena::new();
    let mut parser = Parser::new(lexer, &arena);
    let result = parser.parse();
    let prog = result.ast.unwrap().to_owned();
    match &prog.statements[1] {
        jv_ast::statement::Statement::ValDeclaration { initializer, .. } => match initializer {
            expr => match expr {
                jv_ast::Expression::Literal(_, span) => {
                    assert!(
                        span.start_line > 0,
                        "literal span should reflect newline, got {:?}",
                        span
                    );
                }
                other => panic!("expected literal initializer, got {:?}", other),
            },
        },
        other => panic!("expected val declaration, got {:?}", other),
    }
}

#[test]
fn parses_package_with_function() {
    let code = r#"package org.jv.test

fun main() {
    println("test")
}
"#;
    let source = Source::from_str(code);
    let lexer = Lexer::new(source);
    let arena = Arena::new();
    let mut parser = Parser::new(lexer, &arena);
    let result = parser.parse();

    assert!(result.diagnostics.is_empty(), "diagnostics: {:?}", result.diagnostics);
    let prog = result.ast.expect("should parse").to_owned();
    assert_eq!(prog.package.as_deref(), Some("org.jv.test"), "package should be parsed");
    assert_eq!(prog.statements.len(), 1, "should have one function statement");

    match &prog.statements[0] {
        Statement::FunctionDeclaration { name, .. } => {
            assert_eq!(name, "main", "function name should be main");
        }
        other => panic!("expected FunctionDeclaration, got {:?}", other),
    }
}

#[test]
fn parses_typed_assignment_as_implicit_typed() {
    let code = r#"result = 1
typed: Int = 2
val explicit = result + typed
var counter = 0
counter = counter + explicit"#;
    let source = Source::from_str(code);
    let lexer = Lexer::new(source);
    let arena = Arena::new();
    let mut parser = Parser::new(lexer, &arena);
    let result = parser.parse();

    if !result.diagnostics.is_empty() {
        for diag in &result.diagnostics {
            eprintln!("Diagnostic: {} at {:?}", diag.message, diag.span);
        }
    }

    let prog = result.ast.expect("should parse").to_owned();
    eprintln!("\nParsed statements:");
    for (i, stmt) in prog.statements.iter().enumerate() {
        eprintln!("[{}] {:?}", i, stmt);
    }

    // Statement 0: result = 1 - should be Assignment
    assert!(matches!(&prog.statements[0], Statement::Assignment { .. }),
            "result = 1 should be Assignment, got {:?}", prog.statements[0]);

    // Statement 1: typed: Int = 2 - should be ValDeclaration with ImplicitTyped origin
    match &prog.statements[1] {
        Statement::ValDeclaration { name, origin, type_annotation, .. } => {
            assert_eq!(name, "typed");
            assert!(type_annotation.is_some(), "should have type annotation");
            assert_eq!(*origin, jv_ast::ValBindingOrigin::ImplicitTyped,
                       "typed: Int = 2 should have ImplicitTyped origin");
        }
        other => panic!("typed: Int = 2 should be ValDeclaration, got {:?}", other),
    }

    // Statement 2: val explicit = ... - should be ValDeclaration with ExplicitKeyword origin
    match &prog.statements[2] {
        Statement::ValDeclaration { name, origin, .. } => {
            assert_eq!(name, "explicit");
            assert_eq!(*origin, jv_ast::ValBindingOrigin::ExplicitKeyword,
                       "val explicit should have ExplicitKeyword origin");
        }
        other => panic!("val explicit should be ValDeclaration, got {:?}", other),
    }

    // Statement 3: var counter = 0 - should be VarDeclaration
    assert!(matches!(&prog.statements[3], Statement::VarDeclaration { .. }),
            "var counter should be VarDeclaration, got {:?}", prog.statements[3]);

    // Statement 4: counter = counter + explicit - should be Assignment
    assert!(matches!(&prog.statements[4], Statement::Assignment { .. }),
            "counter = ... should be Assignment, got {:?}", prog.statements[4]);
}

#[test]
fn parses_string_interpolation_with_member_access() {
    let code = r#"fun render(item) {
    return "${item.name} - ${status}"
}"#;

    let source = Source::from_str(code);
    let lexer = Lexer::new(source);
    let arena = Arena::new();
    let mut parser = Parser::new(lexer, &arena);
    let result = parser.parse();

    if !result.diagnostics.is_empty() {
        for diag in &result.diagnostics {
            eprintln!("Diagnostic: {} at {:?}", diag.message, diag.span);
        }
    }

    let prog = result.ast.expect("should parse").to_owned();
    assert_eq!(prog.statements.len(), 1, "should have one function statement");

    // Check we parsed it without producing underscore placeholders
    match &prog.statements[0] {
        Statement::FunctionDeclaration { body, .. } => {
            // The body should contain a return statement with string interpolation
            let body_str = format!("{:?}", body);
            assert!(!body_str.contains(r#"Identifier("_""#),
                    "should not contain underscore placeholder identifiers: {}", body_str);
        }
        other => panic!("expected FunctionDeclaration, got {:?}", other),
    }
}
