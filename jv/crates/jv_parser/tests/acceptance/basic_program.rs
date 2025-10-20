use jv_ast::Program;
use jv_lexer::Token;
use jv_parser::Parser;
use serde_json::json;

fn parse_fixture(name: &str, source: &str) -> (Program, Vec<Token>) {
    let output = Parser::parse(source).unwrap_or_else(|err| {
        panic!("failed to parse acceptance fixture `{name}`: {err:?}")
    });
    let diagnostics = output.diagnostics().finalized();
    assert!(
        diagnostics.is_empty(),
        "expected no diagnostics for `{name}`, found {:?}",
        diagnostics
    );
    let tokens = output.tokens().to_vec();
    let program = output.into_program();
    (program, tokens)
}

fn summarize_tokens(tokens: &[Token]) -> serde_json::Value {
    let layout_commas = tokens
        .iter()
        .filter(|token| matches!(token.token_type, jv_lexer::TokenType::LayoutComma))
        .count();
    let comment_trivia = tokens
        .iter()
        .filter(|token| token.leading_trivia.comments)
        .count();
    json!({
        "total": tokens.len(),
        "layout_commas": layout_commas,
        "comment_trivia": comment_trivia,
    })
}

fn render_snapshot_payload(name: &str, program: &Program, tokens: &[Token]) -> String {
    let payload = json!({
        "fixture": name,
        "program": program,
        "token_metrics": summarize_tokens(tokens),
    });
    serde_json::to_string_pretty(&payload).expect("snapshot payload serialization should succeed")
}

#[test]
fn acceptance_basic_program_ast() {
    let source = include_str!("../fixtures/acceptance/basic_program.jv");
    let (program, tokens) = parse_fixture("basic_program", source);
    let payload = render_snapshot_payload("basic_program", &program, &tokens);

    insta::assert_snapshot!("acceptance_basic_program", payload);
}

#[test]
fn acceptance_pattern_pipeline_ast() {
    let source = include_str!("../fixtures/acceptance/pattern_pipeline.jv");
    let (program, tokens) = parse_fixture("pattern_pipeline", source);
    let payload = render_snapshot_payload("pattern_pipeline", &program, &tokens);

    insta::assert_snapshot!("acceptance_pattern_pipeline", payload);
}
