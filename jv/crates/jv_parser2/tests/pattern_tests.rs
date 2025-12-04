use jv_parser2::{
    Diagnostic, Parser, Source, allocator::Arena, lexer::Lexer, parser::pattern::parse_pattern,
    token::TokenKind,
};

fn parse_pattern_str(
    input: &str,
) -> (
    Option<jv_ast::binding_pattern::BindingPatternKind>,
    Vec<Diagnostic>,
    TokenKind,
) {
    let source = Source::from_str(input);
    let lexer = Lexer::new(source);
    let arena = Arena::new();
    let mut parser = Parser::new(lexer, &arena);
    let pat = parse_pattern(&mut parser);
    let diags = parser.diagnostics().clone();
    let remaining = parser.current().kind;
    (pat, diags, remaining)
}

#[test]
fn parses_literal_number_pattern() {
    let (pat, diags, remaining) = parse_pattern_str("42");
    assert!(diags.is_empty());
    assert_eq!(remaining, TokenKind::Eof);
    match pat {
        Some(jv_ast::binding_pattern::BindingPatternKind::Literal { .. }) => {}
        other => panic!("expected literal pattern, got {:?}", other),
    }
}

#[test]
fn parses_guard_pattern_and_consumes_guard_expression() {
    let (pat, diags, remaining) = parse_pattern_str("x if true");
    assert!(diags.is_empty(), "diagnostics: {:?}", diags);
    assert_eq!(remaining, TokenKind::Eof, "guard should be consumed");
    match pat {
        Some(jv_ast::binding_pattern::BindingPatternKind::Identifier { name, .. }) => {
            assert_eq!(name, "x");
        }
        other => panic!("expected identifier pattern, got {:?}", other),
    }
}

#[test]
fn reports_missing_paren_in_tuple_pattern() {
    let (_pat, diags, _remaining) = parse_pattern_str("(x");
    assert!(
        diags
            .iter()
            .any(|d| d.message.contains("expected ')' to close tuple pattern")),
        "diagnostic missing: {:?}",
        diags
    );
}

#[test]
fn reports_missing_bracket_in_list_pattern() {
    let (_pat, diags, _remaining) = parse_pattern_str("[x");
    assert!(
        diags
            .iter()
            .any(|d| d.message.contains("expected ']' to close list pattern")),
        "diagnostic missing: {:?}",
        diags
    );
}

#[test]
fn reports_missing_paren_when_empty_tuple_not_closed() {
    let (_pat, diags, _remaining) = parse_pattern_str("(");
    assert!(
        diags
            .iter()
            .any(|d| d.message.contains("expected ')' to close tuple pattern")),
        "diagnostic missing: {:?}",
        diags
    );
}
