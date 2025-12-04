use jv_parser2::{Parser, Source, allocator::Arena, lexer::Lexer, parser::pattern::parse_pattern};

fn parse_pattern_str(
    input: &str,
) -> (
    Option<jv_ast::binding_pattern::BindingPatternKind>,
    Vec<jv_parser2::Diagnostic>,
) {
    let source = Source::from_str(input);
    let lexer = Lexer::new(source);
    let arena = Arena::new();
    let mut parser = Parser::new(lexer, &arena);
    let pat = parse_pattern(&mut parser);
    (pat, parser.diagnostics().clone())
}

#[test]
fn parses_literal_number_pattern() {
    let (pat, diags) = parse_pattern_str("42");
    assert!(diags.is_empty());
    match pat {
        Some(jv_ast::binding_pattern::BindingPatternKind::Literal { .. }) => {}
        other => panic!("expected literal pattern, got {:?}", other),
    }
}

#[test]
fn reports_missing_paren_in_tuple_pattern() {
    let (_pat, diags) = parse_pattern_str("(x");
    assert!(!diags.is_empty());
}

#[test]
fn reports_missing_bracket_in_list_pattern() {
    let (_pat, diags) = parse_pattern_str("[x");
    assert!(!diags.is_empty());
}
