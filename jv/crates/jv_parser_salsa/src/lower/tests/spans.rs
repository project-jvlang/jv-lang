use crate::lexer::Lexer;
use crate::lower::context::LoweringContext;

#[test]
fn computes_span_with_leading_trivia_and_newlines() {
    let source = "// leading comment\npackage main\n";
    let lexer = Lexer::new(source).expect("lex");
    let tokens = lexer.collect_owned_tokens();
    let ctx = LoweringContext::new(source, &tokens);
    let pkg_token = tokens
        .iter()
        .find(|t| t.kind == crate::lexer::TokenKind::Package)
        .expect("package token");
    let span = ctx.span_for_token(pkg_token);
    assert_eq!(span.start_line, 2);
    assert_eq!(span.start_column, 1);
}
