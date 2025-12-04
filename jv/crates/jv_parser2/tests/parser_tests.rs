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
