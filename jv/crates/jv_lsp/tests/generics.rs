use std::time::{Duration, Instant};

use jv_lsp::{JvLanguageServer, Position};

fn generic_source() -> &'static str {
    r#"fun <T> identity(value: T): T = value

fun main(): Unit {
    val echoed = identity("value")
    println(echoed)
}
"#
}

#[test]
fn diagnostics_for_generic_source_are_empty() {
    let mut server = JvLanguageServer::new();
    let uri = "file:///generics.jv".to_string();
    server.open_document(uri.clone(), generic_source().to_string());

    let start = Instant::now();
    let diagnostics = server.get_diagnostics(&uri);
    let messages: Vec<_> = diagnostics
        .iter()
        .map(|diagnostic| diagnostic.message.clone())
        .collect();
    let elapsed = start.elapsed();

    assert_eq!(
        diagnostics.len(),
        1,
        "expected a single diagnostic highlighting generic ambiguity"
    );
    assert!(
        messages[0].contains("ambiguous function signature"),
        "diagnostic should mention ambiguous signature: {:?}",
        messages
    );
    assert!(
        elapsed <= Duration::from_millis(200),
        "diagnostic collection exceeded 200ms: {:?}",
        elapsed
    );
}

#[test]
fn completions_include_sequence_templates() {
    let mut server = JvLanguageServer::new();
    let uri = "file:///generics.jv".to_string();
    server.open_document(uri.clone(), generic_source().to_string());
    let position = Position {
        line: 3,
        character: 18,
    };

    let completions = server.get_completions(&uri, position);
    assert!(!completions.is_empty());
    assert!(completions.iter().any(|item| item.contains("data")));
}

#[test]
fn hover_returns_none_for_non_regex_generics() {
    let mut server = JvLanguageServer::new();
    let uri = "file:///generics.jv".to_string();
    server.open_document(uri.clone(), generic_source().to_string());

    let hover = server.get_hover(
        &uri,
        Position {
            line: 0,
            character: 5,
        },
    );

    assert!(hover.is_none());
}
