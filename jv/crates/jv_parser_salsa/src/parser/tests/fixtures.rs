use crate::{
    Span,
    db::{Database, FileInput, preprocess},
    parser::{OwnedToken, parse},
};
use std::sync::Arc;

fn parse_source(source: &str) -> (Vec<crate::parser::ParserDiagnostic>, Vec<jv_lexer::Token>) {
    let db = Database::new();
    let file = FileInput::new(&db, Arc::from("<memory>"), Arc::from(source));
    let output = preprocess(&db, file);
    let owned_tokens: Vec<OwnedToken> = output
        .tokens
        .iter()
        .map(|tok| OwnedToken {
            kind: crate::lexer::kind_from_token_type(&tok.token_type),
            span: Span { start: 0, end: 0 },
            lexeme: tok.lexeme.clone(),
            leading_trivia: tok.leading_trivia.clone(),
            metadata: tok.metadata.clone(),
            diagnostic: tok.diagnostic.clone(),
        })
        .collect();
    let result = parse(owned_tokens);
    (result.output.diagnostics, output.tokens.clone())
}

#[test]
fn parses_primitive_sum_specialization_fixture_source() {
    let source = r#"
import jv.collections.*

fun main(): Unit {
    val characters = ['A' 'B' 'C']

    val charSum = characters.sum()
    println(charSum)

    val shorts = [5s 7s 9s]

    val shortSum = shorts.sum()
    println(shortSum)

    val numbers = [10 20 30]
    val intSum = numbers
        .map { value -> value }
        .sum()
    println(intSum)

    val longs = [100L 200L 300L]
    val longSum = longs.sum()
    println(longSum)
}
"#;

    let (diagnostics, tokens) = parse_source(source);
    if diagnostics.is_empty() {
        return;
    }

    let mut rendered = String::new();
    for diag in diagnostics {
        let idx = diag.span.start;
        let tok = tokens.get(idx);
        let around = tokens.get(idx.saturating_sub(3)..(idx + 4).min(tokens.len()));
        rendered.push_str(&format!(
            "parser diagnostic: {diag:?}; token={tok:?}; around={around:?}\n"
        ));
    }
    panic!("{rendered}");
}
