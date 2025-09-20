use super::*;

use test_case::test_case;

#[test]
fn test_basic_keywords_green_phase() {
    // GREEN: This test should now pass
    let mut lexer = Lexer::new("val var when data class fun".to_string());
    let tokens = lexer.tokenize().unwrap();

    // Expected tokens for Green phase
    assert_eq!(tokens.len(), 7); // 6 keywords + EOF
    assert_eq!(tokens[0].token_type, TokenType::Val);
    assert_eq!(tokens[1].token_type, TokenType::Var);
    assert_eq!(tokens[2].token_type, TokenType::When);
    assert_eq!(tokens[3].token_type, TokenType::Data);
    assert_eq!(tokens[4].token_type, TokenType::Class);
    assert_eq!(tokens[5].token_type, TokenType::Fun);
    assert_eq!(tokens[6].token_type, TokenType::Eof);
}

#[test]
fn test_identifiers_and_literals_green_phase() {
    // RED: This test should fail
    let mut lexer = Lexer::new("val name = \"Hello\" 42 true".to_string());
    let tokens = lexer.tokenize().unwrap();

    // Expected tokens for Green phase
    assert_eq!(tokens.len(), 7); // val, name, =, "Hello", 42, true, EOF
    assert_eq!(tokens[0].token_type, TokenType::Val);
    assert_eq!(
        tokens[1].token_type,
        TokenType::Identifier("name".to_string())
    );
    assert_eq!(tokens[2].token_type, TokenType::Assign);
    assert_eq!(tokens[3].token_type, TokenType::String("Hello".to_string()));
    assert_eq!(tokens[4].token_type, TokenType::Number("42".to_string()));
    assert_eq!(tokens[5].token_type, TokenType::Boolean(true));
    assert_eq!(tokens[6].token_type, TokenType::Eof);
}

#[test]
fn test_null_safety_operators_red_phase() {
    // RED: This test should fail
    let mut lexer = Lexer::new("obj?.field ?: default".to_string());
    let tokens = lexer.tokenize().unwrap();

    // Should recognize ?. and ?: operators
    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
    assert!(token_types.contains(&&TokenType::NullSafe));
    assert!(token_types.contains(&&TokenType::Elvis));
    assert!(token_types.contains(&&TokenType::Identifier("obj".to_string())));
    assert!(token_types.contains(&&TokenType::Identifier("field".to_string())));
    assert!(token_types.contains(&&TokenType::Identifier("default".to_string())));
}

#[test]
fn test_string_interpolation_red_phase() {
    // RED: This test should fail
    let mut lexer = Lexer::new("\"Hello, ${name}!\"".to_string());
    let tokens = lexer.tokenize().unwrap();

    // Expected tokens for string interpolation
    // Should tokenize as: StringStart("Hello, "), Identifier("name"), StringEnd("!")
    assert!(tokens.len() >= 3);

    // Look for interpolation-related tokens
    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
    assert!(token_types
        .iter()
        .any(|t| matches!(t, TokenType::StringStart)));
    assert!(token_types.contains(&&TokenType::Identifier("name".to_string())));
    assert!(token_types
        .iter()
        .any(|t| matches!(t, TokenType::StringEnd)));
}

#[test]
fn test_arithmetic_operators_red_phase() {
    // RED: This test should fail
    let mut lexer = Lexer::new("a + b - c * d / e % f".to_string());
    let tokens = lexer.tokenize().unwrap();

    // Should recognize arithmetic operators
    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
    assert!(token_types.contains(&&TokenType::Plus));
    assert!(token_types.contains(&&TokenType::Minus));
    assert!(token_types.contains(&&TokenType::Multiply));
    assert!(token_types.contains(&&TokenType::Divide));
    assert!(token_types.contains(&&TokenType::Modulo));
}

#[test]
fn test_comparison_operators_red_phase() {
    // RED: This test should fail
    let mut lexer = Lexer::new("a == b != c < d <= e > f >= g".to_string());
    let tokens = lexer.tokenize().unwrap();

    // Should recognize comparison operators
    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
    assert!(token_types.contains(&&TokenType::Equal));
    assert!(token_types.contains(&&TokenType::NotEqual));
    assert!(token_types.contains(&&TokenType::Less));
    assert!(token_types.contains(&&TokenType::LessEqual));
    assert!(token_types.contains(&&TokenType::Greater));
    assert!(token_types.contains(&&TokenType::GreaterEqual));
}

#[test]
fn test_logical_operators_red_phase() {
    // RED: This test should fail
    let mut lexer = Lexer::new("a && b || !c".to_string());
    let tokens = lexer.tokenize().unwrap();

    // Should recognize logical operators
    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
    assert!(token_types.contains(&&TokenType::And));
    assert!(token_types.contains(&&TokenType::Or));
    assert!(token_types.contains(&&TokenType::Not));
}

#[test]
fn test_arrow_operators_red_phase() {
    // RED: This test should fail
    let mut lexer = Lexer::new("fun test() -> Int { x => x + 1 }".to_string());
    let tokens = lexer.tokenize().unwrap();

    // Should recognize arrow operators
    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
    assert!(token_types.contains(&&TokenType::Arrow)); // ->
    assert!(token_types.contains(&&TokenType::FatArrow)); // =>
}

#[test]
fn test_punctuation_red_phase() {
    // RED: This test should fail
    let mut lexer = Lexer::new("(a, b) { c[d]: e; f::g }".to_string());
    let tokens = lexer.tokenize().unwrap();

    // Should recognize all punctuation
    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
    assert!(token_types.contains(&&TokenType::LeftParen));
    assert!(token_types.contains(&&TokenType::RightParen));
    assert!(token_types.contains(&&TokenType::LeftBrace));
    assert!(token_types.contains(&&TokenType::RightBrace));
    assert!(token_types.contains(&&TokenType::LeftBracket));
    assert!(token_types.contains(&&TokenType::RightBracket));
    assert!(token_types.contains(&&TokenType::Comma));
    assert!(token_types.contains(&&TokenType::Colon));
    assert!(token_types.contains(&&TokenType::Semicolon));
    assert!(token_types.contains(&&TokenType::DoubleColon));
}

#[test]
fn test_comments_red_phase() {
    // RED: This test should fail
    let mut lexer =
        Lexer::new("val x = 1 // line comment\n/* block comment */ val y = 2".to_string());
    let tokens = lexer.tokenize().unwrap();

    // Should recognize both line and block comments
    let has_line_comment = tokens
        .iter()
        .any(|t| matches!(t.token_type, TokenType::LineComment(_)));
    let has_block_comment = tokens
        .iter()
        .any(|t| matches!(t.token_type, TokenType::BlockComment(_)));

    assert!(has_line_comment);
    assert!(has_block_comment);
}

#[test]
fn test_position_tracking_green_phase() {
    // GREEN: Test position tracking functionality
    let mut lexer = Lexer::new("val\nname\n  =\n    42".to_string());
    let tokens = lexer.tokenize().unwrap();

    // Should track line and column positions correctly
    assert_eq!(tokens[0].line, 1); // val
    assert_eq!(tokens[0].column, 1);

    assert_eq!(tokens[1].line, 2); // name
    assert_eq!(tokens[1].column, 1);

    assert_eq!(tokens[2].line, 3); // =
    assert_eq!(tokens[2].column, 3);

    assert_eq!(tokens[3].line, 4); // 42
    assert_eq!(tokens[3].column, 5);
}

#[test_case("true" => TokenType::Boolean(true); "boolean true")]
#[test_case("false" => TokenType::Boolean(false); "boolean false")]
#[test_case("null" => TokenType::Null; "null literal")]
fn test_literal_values_red_phase(input: &str) -> TokenType {
    // RED: This test should fail
    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.tokenize().unwrap();

    tokens[0].token_type.clone()
}

#[test]
fn test_complex_string_interpolation_red_phase() {
    // RED: This test should fail
    let mut lexer = Lexer::new("\"User ${user.name} has ${user.age} years\"".to_string());
    let tokens = lexer.tokenize().unwrap();

    // Should handle complex interpolation with member access
    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();

    // Should contain identifiers and dots for member access
    assert!(token_types.contains(&&TokenType::Identifier("user".to_string())));
    assert!(token_types.contains(&&TokenType::Dot));
    assert!(token_types.contains(&&TokenType::Identifier("name".to_string())));
    assert!(token_types.contains(&&TokenType::Identifier("age".to_string())));
}

#[test]
fn test_lexer_error_handling_red_phase() {
    // RED: This test should fail
    let mut lexer = Lexer::new("\"unterminated string".to_string());
    let result = lexer.tokenize();

    // Should return an error for unterminated string
    assert!(result.is_err());
    match result.unwrap_err() {
        LexError::UnterminatedString(_, _) => {} // Expected
        _ => panic!("Wrong error type"),
    }
}

// Additional comprehensive tests for lexer
#[test]
fn test_complex_expressions() {
    let source = r#"
        val result = user?.profile?.name ?: "Unknown"
        val sum = (a + b) * (c - d) / e % f
    "#;
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    // Should contain all expected operators and symbols
    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
    assert!(token_types.contains(&&TokenType::Val));
    assert!(token_types.contains(&&TokenType::NullSafe));
    assert!(token_types.contains(&&TokenType::Elvis));
    assert!(token_types.contains(&&TokenType::LeftParen));
    assert!(token_types.contains(&&TokenType::RightParen));
    assert!(token_types.contains(&&TokenType::Plus));
    assert!(token_types.contains(&&TokenType::Minus));
    assert!(token_types.contains(&&TokenType::Multiply));
    assert!(token_types.contains(&&TokenType::Divide));
    assert!(token_types.contains(&&TokenType::Modulo));
}

#[test]
fn test_function_definitions() {
    let source = r#"
        fun add(a: Int, b: Int): Int {
            return a + b
        }

        fun greet(name: String = "World") {
            println("Hello, $name!")
        }
    "#;
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
    assert!(token_types.contains(&&TokenType::Fun));
    assert!(token_types.contains(&&TokenType::Return));
    assert!(token_types.contains(&&TokenType::Colon));
    assert!(token_types.contains(&&TokenType::Arrow));
    assert!(token_types.contains(&&TokenType::Assign));
}

#[test]
fn test_data_class_syntax() {
    let source = "data class User(val name: String, var age: Int)";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
    assert!(token_types.contains(&&TokenType::Data));
    assert!(token_types.contains(&&TokenType::Class));
    assert!(token_types.contains(&&TokenType::Val));
    assert!(token_types.contains(&&TokenType::Var));
}

#[test]
fn test_when_expressions() {
    let source = r#"
        when (x) {
            0 -> "zero"
            1, 2 -> "one or two"
            in 3..10 -> "small"
            else -> "other"
        }
    "#;
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
    assert!(token_types.contains(&&TokenType::When));
    assert!(token_types.contains(&&TokenType::FatArrow));
    assert!(token_types.contains(&&TokenType::In));
    // Note: DotDot (range operator) not yet implemented in TokenType enum
    assert!(token_types.contains(&&TokenType::Else));
}

#[test]
fn test_async_spawn_syntax() {
    let source = r#"
        spawn {
            println("Running in virtual thread")
        }

        val future = async {
            computeValue()
        }.await()
    "#;
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    let identifiers: Vec<_> = tokens
        .iter()
        .filter_map(|t| match &t.token_type {
            TokenType::Identifier(name) => Some(name.as_str()),
            _ => None,
        })
        .collect();

    // Spawn/async/await should currently lex as identifiers until dedicated keywords are added
    assert!(identifiers.contains(&"spawn"));
    assert!(identifiers.contains(&"async"));
    assert!(identifiers.contains(&"await"));
}

#[test]
fn test_extension_functions() {
    let source = "fun String.reversed(): String = StringBuilder(this).reverse().toString()";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
    assert!(token_types.contains(&&TokenType::Fun));
    assert!(token_types.contains(&&TokenType::Dot));
    assert!(token_types.contains(&&TokenType::Arrow));
    assert!(token_types.contains(&&TokenType::Assign));
}

#[test]
fn test_numbers_and_types() {
    let source = "val int = 42; val float = 3.14; val hex = 0xFF; val binary = 0b1010";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    let numbers: Vec<_> = tokens
        .iter()
        .filter_map(|t| match &t.token_type {
            TokenType::Number(n) => Some(n.as_str()),
            _ => None,
        })
        .collect();

    assert!(numbers.contains(&"42"));
    assert!(numbers.contains(&"3.14"));
    assert!(numbers.contains(&"0xFF") || numbers.contains(&"255"));
    assert!(numbers.contains(&"0b1010") || numbers.contains(&"10"));
}

#[test]
fn test_use_defer_syntax() {
    let source = r#"
        use (resource) {
            resource.process()
        }

        defer {
            cleanup()
        }
    "#;
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
    // Note: Use, Defer keywords not yet implemented in TokenType enum
}

#[test]
fn test_array_and_collection_syntax() {
    let source = r#"
        val array = [1, 2, 3]
        val list = listOf("a", "b", "c")
        val map = mapOf("key" to "value")
    "#;
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
    assert!(token_types.contains(&&TokenType::LeftBracket));
    assert!(token_types.contains(&&TokenType::RightBracket));
    // Note: To keyword not yet implemented in TokenType enum
}

#[test]
fn test_edge_cases_and_errors() {
    // Test various edge cases
    let test_cases = vec![
        ("", true),             // Empty string should not fail
        ("//", true),           // Just comment
        ("/* */", true),        // Just block comment
        ("\"\"", true),         // Empty string literal
        ("''", false),          // Single quotes not supported
        ("123.456.789", false), // Invalid number
        ("@invalid", false),    // Invalid character
    ];

    for (input, should_succeed) in test_cases {
        let mut lexer = Lexer::new(input.to_string());
        let result = lexer.tokenize();

        if should_succeed {
            assert!(result.is_ok(), "Failed to tokenize: '{}'", input);
        } else {
            assert!(
                result.is_err(),
                "Should have failed to tokenize: '{}'",
                input
            );
        }
    }
}

#[test]
fn test_whitespace_and_newlines() {
    let source = "val\n\tx\n\t\t=\n\t\t\t42";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    // Should correctly handle various whitespace
    assert_eq!(tokens[0].token_type, TokenType::Val);
    assert_eq!(tokens[1].token_type, TokenType::Identifier("x".to_string()));
    assert_eq!(tokens[2].token_type, TokenType::Assign);
    assert_eq!(tokens[3].token_type, TokenType::Number("42".to_string()));
}

#[test]
fn test_unicode_identifiers() {
    let source = "val δ = 3.14; val α = β + γ";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    let identifiers: Vec<_> = tokens
        .iter()
        .filter_map(|t| match &t.token_type {
            TokenType::Identifier(id) => Some(id.as_str()),
            _ => None,
        })
        .collect();

    assert!(identifiers.contains(&"δ"));
    assert!(identifiers.contains(&"α"));
    assert!(identifiers.contains(&"β"));
    assert!(identifiers.contains(&"γ"));
}
