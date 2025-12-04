use jv_parser2::{lexer::Lexer, parser::incremental::IncrementalCache, Parser, Source, Arena};

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
    let mut parser = Parser::new(lexer, &arena);
    let mut cache = IncrementalCache::default();
    let change = jv_parser2::parser::incremental::TextChange {
        start: 0,
        old_end: 0,
        new_text: String::new(),
        start_line: 0,
        end_line: 0,
    };
    let result = jv_parser2::parser::incremental::parse_incremental(&mut parser, &change, &mut cache);
    assert!(result.ast.is_some());
}
