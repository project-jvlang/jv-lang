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
    let from_dataset_keyword = tokens.windows(2).find_map(|win| {
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
    });
    if from_dataset_keyword.is_some() {
        return from_dataset_keyword;
    }

    fn is_trivia(kind: crate::lexer::TokenKind) -> bool {
        matches!(
            kind,
            crate::lexer::TokenKind::Whitespace
                | crate::lexer::TokenKind::Newline
                | crate::lexer::TokenKind::LayoutComma
                | crate::lexer::TokenKind::FieldNameLabel
                | crate::lexer::TokenKind::LineComment
                | crate::lexer::TokenKind::BlockComment
                | crate::lexer::TokenKind::JavaDocComment
        )
    }

    // `[@Sample("cases.json", ...)]` 形式のデータセット指定を抽出する。
    let mut idx = 0usize;
    while idx < tokens.len() {
        if tokens[idx].kind != crate::lexer::TokenKind::At {
            idx += 1;
            continue;
        }

        let mut name_idx = idx + 1;
        while tokens.get(name_idx).is_some_and(|t| is_trivia(t.kind)) {
            name_idx += 1;
        }
        let Some(name_tok) = tokens.get(name_idx) else {
            break;
        };
        if name_tok.kind != crate::lexer::TokenKind::Identifier || !name_tok.lexeme_eq("Sample") {
            idx += 1;
            continue;
        }

        let mut paren_idx = name_idx + 1;
        while tokens.get(paren_idx).is_some_and(|t| is_trivia(t.kind)) {
            paren_idx += 1;
        }
        let Some(paren_tok) = tokens.get(paren_idx) else {
            break;
        };
        if paren_tok.kind != crate::lexer::TokenKind::LeftParen {
            idx = name_idx + 1;
            continue;
        }

        let after_paren = tokens.get(paren_idx + 1..).unwrap_or(&[]);
        if let Some(arg) = after_paren
            .iter()
            .find(|t| t.kind == crate::lexer::TokenKind::String)
        {
            return Some(value_from_span(arg, source));
        }

        idx = paren_idx + 1;
    }

    None
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
