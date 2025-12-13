use crate::lower::{LoweringContext, LoweringDiagnostic};
use crate::parser::OwnedToken;

/// テスト名を抽出する。文字列リテラルまたは識別子を優先。
pub fn extract_test_name(tokens: &[OwnedToken], source: &str) -> Option<String> {
    tokens.iter().skip(1).find_map(|tok| match tok.kind {
        crate::lexer::TokenKind::String => Some(value_from_span(tok, source)),
        crate::lexer::TokenKind::Identifier => Some(tok.lexeme_string()),
        _ => None,
    })
}

/// データセット名を抽出する。`dataset <name>` パターンを単純に検出。
pub fn extract_dataset(tokens: &[OwnedToken], source: &str) -> Option<String> {
    tokens.windows(2).find_map(|win| {
        let [first, second] = win else { return None };
        if first.lexeme.eq_ignore_ascii_case("dataset") {
            match second.kind {
                crate::lexer::TokenKind::String => Some(value_from_span(second, source)),
                crate::lexer::TokenKind::Identifier => Some(second.lexeme_string()),
                _ => None,
            }
        } else {
            None
        }
    })
}

pub fn ensure_test_name(
    ctx: &mut LoweringContext<'_>,
    keyword: &OwnedToken,
    name: &Option<String>,
) {
    if name.is_none() {
        ctx.push_diagnostic(LoweringDiagnostic::error(
            "JV-DSL-006: テスト名が必要です",
            Some(ctx.span_for_token(keyword)),
        ));
    }
}

pub fn ensure_test_body(ctx: &mut LoweringContext<'_>, keyword: &OwnedToken, has_body: bool) {
    if !has_body {
        ctx.push_diagnostic(LoweringDiagnostic::error(
            "JV-DSL-007: テスト本体が必要です",
            Some(ctx.span_for_token(keyword)),
        ));
    }
}

fn normalize_lexeme(text: &str) -> String {
    text.trim_matches('"').trim_matches('\'').to_string()
}

fn value_from_span(token: &OwnedToken, source: &str) -> String {
    let normalized_lexeme = normalize_lexeme(token.lexeme.as_ref());
    let span_value = source
        .get(token.span.start as usize..token.span.end as usize)
        .map(normalize_lexeme);

    match span_value {
        Some(value) if value.len() >= normalized_lexeme.len() => value,
        _ => normalized_lexeme,
    }
}
