use super::{Lexer, TokenKind, compat};

fn kinds_with_reference(source: &str) -> (Vec<TokenKind>, Vec<TokenKind>) {
    let mut baseline = jv_lexer::Lexer::new(source.to_string());
    let baseline_tokens = baseline.tokenize().expect("baseline lexing should succeed");
    let baseline_kinds: Vec<TokenKind> = baseline_tokens
        .iter()
        .map(|tok| compat::kind_from_token_type(&tok.token_type))
        .collect();

    let mut lexer = Lexer::new(source).expect("salsa lexer should succeed");
    let ours: Vec<TokenKind> = std::iter::from_fn(|| lexer.next_token())
        .map(|t| t.kind)
        .collect();

    (baseline_kinds, ours)
}

#[test]
fn maps_token_kinds_for_basic_program() {
    let source = "package main\nval x = 1 + 2";
    let (baseline, ours) = kinds_with_reference(source);
    assert_eq!(baseline, ours);
    assert!(
        ours.contains(&TokenKind::Package),
        "should contain package keyword"
    );
}

#[test]
fn handles_unicode_identifiers_and_string_escapes() {
    let source = r#"val π = "line:\n😀""#;
    let (baseline, ours) = kinds_with_reference(source);
    assert_eq!(baseline, ours);
    assert!(
        ours.iter().any(|k| matches!(k, TokenKind::Identifier)),
        "unicode identifier should lex as Identifier"
    );
    assert!(
        ours.iter().any(|k| matches!(k, TokenKind::String)),
        "string literal should lex as String"
    );
}

#[test]
fn matches_jv_lexer_on_stdlib_corpus() {
    let root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let corpus_list = root.join("benches").join("corpus").join("stdlib.txt");
    let content = std::fs::read_to_string(&corpus_list)
        .unwrap_or_else(|err| panic!("failed to read {corpus_list:?}: {err}"));

    for path in content.lines().filter(|line| !line.trim().is_empty()) {
        let rel = path.trim();
        let candidates = [
            root.join(rel),
            root.join("..").join(rel),
            root.join("..").join("..").join(rel),
        ];
        let abs = candidates
            .iter()
            .find(|p| p.exists())
            .cloned()
            .unwrap_or_else(|| candidates.last().unwrap().clone());
        let source = std::fs::read_to_string(&abs)
            .unwrap_or_else(|err| panic!("failed to read {:?}: {}", abs, err));
        let mut baseline_lexer = jv_lexer::Lexer::new(source.to_string());
        let baseline_tokens = match baseline_lexer.tokenize() {
            Ok(tokens) => tokens,
            Err(_) => continue,
        };
        let baseline: Vec<TokenKind> = baseline_tokens
            .iter()
            .map(|tok| compat::kind_from_token_type(&tok.token_type))
            .collect();
        let mut ours_lexer = Lexer::new(&source).expect("salsa lexer should succeed");
        let ours: Vec<TokenKind> = std::iter::from_fn(|| ours_lexer.next_token())
            .map(|t| t.kind)
            .collect();
        assert_eq!(
            baseline, ours,
            "token kinds should match for corpus file {:?}",
            abs
        );
    }
}
