use std::time::Instant;

use jv_lsp::{JvLanguageServer, Position};

#[test]
fn lsp_responses_stay_under_latency_budget() {
    let mut server = JvLanguageServer::new();
    let uri = "file:///latency.jv".to_string();
    let source = "fun <T> identity(value: T): T {\n    value\n}\n".to_string();

    server.open_document(uri.clone(), source);

    let diagnostics_duration = {
        let start = Instant::now();
        let diagnostics = server.get_diagnostics(&uri);
        assert!(
            diagnostics.len() <= 4,
            "unexpected number of diagnostics: {}",
            diagnostics.len()
        );
        start.elapsed().as_millis()
    };
    assert!(
        diagnostics_duration <= 1000,
        "diagnostics exceeded latency budget: {} ms",
        diagnostics_duration
    );

    let position = Position {
        line: 0,
        character: 8,
    };

    let hover_duration = {
        let start = Instant::now();
        let hover = server
            .get_hover(&uri, position.clone())
            .expect("hover should be available for identity");
        assert!(hover.contents.contains("型パラメータ"));
        start.elapsed().as_millis()
    };
    assert!(
        hover_duration <= 1000,
        "hover exceeded latency budget: {} ms",
        hover_duration
    );

    let completion_duration = {
        let start = Instant::now();
        let completions = server.get_completions(&uri, position);
        assert!(
            completions
                .iter()
                .any(|entry| entry.label.contains("identity")),
            "expected identity completion entry"
        );
        start.elapsed().as_millis()
    };
    assert!(
        completion_duration <= 1000,
        "completion exceeded latency budget: {} ms",
        completion_duration
    );
}
