use jv_ast::Statement;
use jv_parser2::{Arena, Parser, Source, lexer::Lexer, parser::incremental::IncrementalCache};

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

    assert!(
        result.diagnostics.is_empty(),
        "diagnostics: {:?}",
        result.diagnostics
    );
    let prog = result.ast.expect("should parse").to_owned();
    assert_eq!(
        prog.package.as_deref(),
        Some("org.jv.test"),
        "package should be parsed"
    );
    assert_eq!(
        prog.statements.len(),
        1,
        "should have one function statement"
    );

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
    assert!(
        matches!(&prog.statements[0], Statement::Assignment { .. }),
        "result = 1 should be Assignment, got {:?}",
        prog.statements[0]
    );

    // Statement 1: typed: Int = 2 - should be ValDeclaration with ImplicitTyped origin
    match &prog.statements[1] {
        Statement::ValDeclaration {
            name,
            origin,
            type_annotation,
            ..
        } => {
            assert_eq!(name, "typed");
            assert!(type_annotation.is_some(), "should have type annotation");
            assert_eq!(
                *origin,
                jv_ast::ValBindingOrigin::ImplicitTyped,
                "typed: Int = 2 should have ImplicitTyped origin"
            );
        }
        other => panic!("typed: Int = 2 should be ValDeclaration, got {:?}", other),
    }

    // Statement 2: val explicit = ... - should be ValDeclaration with ExplicitKeyword origin
    match &prog.statements[2] {
        Statement::ValDeclaration { name, origin, .. } => {
            assert_eq!(name, "explicit");
            assert_eq!(
                *origin,
                jv_ast::ValBindingOrigin::ExplicitKeyword,
                "val explicit should have ExplicitKeyword origin"
            );
        }
        other => panic!("val explicit should be ValDeclaration, got {:?}", other),
    }

    // Statement 3: var counter = 0 - should be VarDeclaration
    assert!(
        matches!(&prog.statements[3], Statement::VarDeclaration { .. }),
        "var counter should be VarDeclaration, got {:?}",
        prog.statements[3]
    );

    // Statement 4: counter = counter + explicit - should be Assignment
    assert!(
        matches!(&prog.statements[4], Statement::Assignment { .. }),
        "counter = ... should be Assignment, got {:?}",
        prog.statements[4]
    );
}

#[test]
fn parses_unit_syntax_source() {
    let source = r#"
@ 長さ(Double) m! {
    基準 := 1
    @Conversion {}
}

val 歩行距離 = 1250 @ m
val 室温: Double@[K] = 298.15
"#;
    let src = Source::from_str(source);
    let lexer = Lexer::new(src);
    let arena = Arena::new();
    let mut parser = Parser::new(lexer, &arena);
    let result = parser.parse();

    if !result.diagnostics.is_empty() {
        for diag in &result.diagnostics {
            eprintln!("Diagnostic: {} at {:?}", diag.message, diag.span);
        }
    }

    let prog = result.ast.expect("should parse").to_owned();
    eprintln!("\nParsed {} statements:", prog.statements.len());
    for (i, stmt) in prog.statements.iter().enumerate() {
        eprintln!("[{}] {:?}", i, std::mem::discriminant(stmt));
    }

    // We expect: UnitTypeDefinition + 2 ValDeclarations = 3 statements
    assert!(
        prog.statements.len() >= 3,
        "expected at least 3 statements, got {}",
        prog.statements.len()
    );

    // First should be UnitTypeDefinition
    assert!(
        matches!(&prog.statements[0], Statement::UnitTypeDefinition { .. }),
        "first statement should be UnitTypeDefinition, got {:?}",
        std::mem::discriminant(&prog.statements[0])
    );

    // Check for val declarations
    let val_count = prog
        .statements
        .iter()
        .filter(|s| matches!(s, Statement::ValDeclaration { .. }))
        .count();
    assert!(
        val_count >= 2,
        "expected at least 2 ValDeclarations, got {}",
        val_count
    );
}

#[test]
fn parses_multi_parameter_lambda_without_parens() {
    let code = r#"fun demo() {
    val result = items.fold(0) { acc value -> acc + value }
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
    assert_eq!(
        prog.statements.len(),
        1,
        "should have one function statement"
    );

    // Extract the lambda from the fold call
    match &prog.statements[0] {
        Statement::FunctionDeclaration { body, .. } => {
            // Find the lambda in the body
            let body_str = format!("{:?}", body);
            eprintln!("Body: {}", body_str);
            // The lambda should have 2 parameters: acc and value
            assert!(
                body_str.contains(r#"Lambda"#),
                "should contain a Lambda expression: {}",
                body_str
            );
            // Verify both parameter names appear - if only one, lambda parsed incorrectly
            assert!(
                body_str.contains(r#"name: "acc""#),
                "lambda should have 'acc' parameter"
            );
            assert!(
                body_str.contains(r#"name: "value""#),
                "lambda should have 'value' parameter"
            );
        }
        other => panic!("expected FunctionDeclaration, got {:?}", other),
    }
}

#[test]
fn parses_reduce_style_lambda() {
    // Test the exact pattern from stdlib: { left right -> ... }
    let code = r#"fun demo() {
    return this.reduce { left right ->
        operation(
            left
            right
        )
    }
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
    let body_str = format!("{:?}", prog);
    eprintln!("Parsed: {}", body_str);

    // Should have both 'left' and 'right' as lambda parameters
    assert!(
        body_str.contains(r#"name: "left""#),
        "lambda should have 'left' parameter: {}",
        body_str
    );
    assert!(
        body_str.contains(r#"name: "right""#),
        "lambda should have 'right' parameter: {}",
        body_str
    );
}

#[test]
fn parses_trailing_lambda_after_member_access() {
    // Test: this.toStream().flatMap { value -> value }
    // Should parse flatMap as a call with trailing lambda argument
    let code = r#"fun demo() {
    return this.toStream().flatMap { value -> value }
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
    let body_str = format!("{:?}", prog);
    eprintln!("Parsed: {}", body_str);

    // The flatMap should be parsed as a Call, not a MemberAccess
    // It should have a Lambda argument with parameter "value"
    assert!(
        body_str.contains(r#"name: "value""#),
        "lambda should have 'value' parameter: {}",
        body_str
    );

    // The call should be to flatMap, not just a member access
    // Check that there's a Call with function being MemberAccess to "flatMap"
    assert!(
        body_str.contains(r#"property: "flatMap""#),
        "should have flatMap member access: {}",
        body_str
    );
    assert!(
        body_str.contains("Lambda"),
        "should have a lambda as argument: {}",
        body_str
    );
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
    assert_eq!(
        prog.statements.len(),
        1,
        "should have one function statement"
    );

    // Check we parsed it without producing underscore placeholders
    match &prog.statements[0] {
        Statement::FunctionDeclaration { body, .. } => {
            // The body should contain a return statement with string interpolation
            let body_str = format!("{:?}", body);
            assert!(
                !body_str.contains(r#"Identifier("_""#),
                "should not contain underscore placeholder identifiers: {}",
                body_str
            );
        }
        other => panic!("expected FunctionDeclaration, got {:?}", other),
    }
}

#[test]
fn typed_assignment_uses_implicit_typed_origin() {
    use jv_ast::ValBindingOrigin;

    let source = Source::from_str("typed: Int = 2");
    let lexer = Lexer::new(source);
    let arena = Arena::new();
    let mut parser = Parser::new(lexer, &arena);
    let result = parser.parse();
    assert!(
        result.diagnostics.is_empty(),
        "no errors: {:?}",
        result.diagnostics
    );
    let prog = result.ast.unwrap().to_owned();
    assert_eq!(prog.statements.len(), 1);
    match &prog.statements[0] {
        Statement::ValDeclaration { name, origin, .. } => {
            assert_eq!(name, "typed");
            assert_eq!(
                *origin,
                ValBindingOrigin::ImplicitTyped,
                "expected ImplicitTyped origin but got {:?}",
                origin
            );
        }
        other => panic!("expected ValDeclaration but got {:?}", other),
    }
}

#[test]
fn full_binding_test_source_produces_correct_origins() {
    use jv_ast::ValBindingOrigin;

    let source = Source::from_str(
        r#"result = 1
typed: Int = 2
val explicit = result + typed
var counter = 0
counter = counter + explicit
"#,
    );
    let lexer = Lexer::new(source);
    let arena = Arena::new();
    let mut parser = Parser::new(lexer, &arena);
    let result = parser.parse();
    let prog = result.ast.unwrap().to_owned();

    eprintln!("Parsed {} statements", prog.statements.len());
    for (i, stmt) in prog.statements.iter().enumerate() {
        match stmt {
            Statement::ValDeclaration { name, origin, .. } => {
                eprintln!(
                    "Statement {}: ValDeclaration {} origin={:?}",
                    i, name, origin
                );
            }
            Statement::VarDeclaration { name, .. } => {
                eprintln!("Statement {}: VarDeclaration {}", i, name);
            }
            Statement::Assignment { target, .. } => {
                eprintln!("Statement {}: Assignment to {:?}", i, target);
            }
            other => {
                eprintln!("Statement {}: {:?}", i, std::any::type_name_of_val(other));
            }
        }
    }

    // Check statement 0: result = 1 should be an Assignment (will be normalized later)
    assert!(
        matches!(&prog.statements[0], Statement::Assignment { .. }),
        "Statement 0 should be Assignment"
    );

    // Check statement 1: typed: Int = 2 should be ValDeclaration with ImplicitTyped
    match &prog.statements[1] {
        Statement::ValDeclaration { name, origin, .. } => {
            assert_eq!(name, "typed");
            assert_eq!(
                *origin,
                ValBindingOrigin::ImplicitTyped,
                "typed should have ImplicitTyped origin, got {:?}",
                origin
            );
        }
        other => panic!("Statement 1 should be ValDeclaration but got {:?}", other),
    }

    // Check statement 2: val explicit = ... should be ValDeclaration with ExplicitKeyword
    match &prog.statements[2] {
        Statement::ValDeclaration { name, origin, .. } => {
            assert_eq!(name, "explicit");
            assert_eq!(
                *origin,
                ValBindingOrigin::ExplicitKeyword,
                "explicit should have ExplicitKeyword origin"
            );
        }
        other => panic!("Statement 2 should be ValDeclaration but got {:?}", other),
    }

    // Check statement 3: var counter = 0 should be VarDeclaration
    assert!(
        matches!(&prog.statements[3], Statement::VarDeclaration { .. }),
        "Statement 3 should be VarDeclaration"
    );

    // Check statement 4: counter = counter + explicit should be Assignment (reassignment)
    assert!(
        matches!(&prog.statements[4], Statement::Assignment { .. }),
        "Statement 4 should be Assignment"
    );
}

#[test]
fn parses_as_type_cast_in_lambda() {
    let code = r#"val f = { candidate -> (candidate as Number).longValue() }"#;
    let source = Source::from_str(code);
    let lexer = Lexer::new(source);
    let arena = Arena::new();
    let mut parser = Parser::new(lexer, &arena);
    let result = parser.parse();

    eprintln!("Diagnostics: {:?}", result.diagnostics);
    let prog = result.ast.unwrap().to_owned();

    eprintln!("Parsed {} statements", prog.statements.len());
    for (i, stmt) in prog.statements.iter().enumerate() {
        eprintln!("Statement {}: {:?}", i, stmt);
    }

    assert!(
        result.diagnostics.is_empty(),
        "Should parse without diagnostics: {:?}",
        result.diagnostics
    );
    assert_eq!(prog.statements.len(), 1, "Should have one statement");

    match &prog.statements[0] {
        Statement::ValDeclaration {
            name, initializer, ..
        } => {
            assert_eq!(name, "f");
            match initializer {
                jv_ast::Expression::Lambda {
                    parameters, body, ..
                } => {
                    assert_eq!(parameters.len(), 1, "Lambda should have one parameter");
                    assert_eq!(
                        parameters[0].name, "candidate",
                        "Parameter name should be 'candidate'"
                    );
                    eprintln!("Lambda body: {:?}", body);
                    // The body should NOT be an identifier "_"
                    match body.as_ref() {
                        jv_ast::Expression::Identifier(name, _) if name == "_" => {
                            panic!(
                                "Lambda body should not be underscore placeholder! Body: {:?}",
                                body
                            );
                        }
                        _ => {
                            // Expected - body is something other than underscore
                        }
                    }
                }
                other => panic!("Initializer should be Lambda but got {:?}", other),
            }
        }
        other => panic!("Statement should be ValDeclaration but got {:?}", other),
    }
}

#[test]
fn parses_if_expression() {
    let code = r#"val value = if (true) 1 else 0"#;
    let source = Source::from_str(code);
    let lexer = Lexer::new(source);
    let arena = Arena::new();
    let mut parser = Parser::new(lexer, &arena);
    let result = parser.parse();

    eprintln!("Diagnostics: {:?}", result.diagnostics);
    let prog = result.ast.unwrap().to_owned();

    eprintln!("Parsed {} statements", prog.statements.len());
    for (i, stmt) in prog.statements.iter().enumerate() {
        eprintln!("Statement {}: {:?}", i, stmt);
    }

    assert_eq!(prog.statements.len(), 1, "Should have one statement");

    match &prog.statements[0] {
        Statement::ValDeclaration {
            name, initializer, ..
        } => {
            assert_eq!(name, "value");
            eprintln!("Initializer: {:?}", initializer);
            // The initializer should be an If expression
            match initializer {
                jv_ast::Expression::If {
                    condition,
                    then_branch,
                    else_branch,
                    ..
                } => {
                    eprintln!("If condition: {:?}", condition);
                    eprintln!("If then: {:?}", then_branch);
                    eprintln!("If else: {:?}", else_branch);
                }
                jv_ast::Expression::Identifier(name, _) if name == "_" => {
                    panic!(
                        "Initializer should not be underscore placeholder! Got: {:?}",
                        initializer
                    );
                }
                other => {
                    eprintln!("Expected If expression but got: {:?}", other);
                }
            }
        }
        other => panic!("Statement should be ValDeclaration but got {:?}", other),
    }
}
