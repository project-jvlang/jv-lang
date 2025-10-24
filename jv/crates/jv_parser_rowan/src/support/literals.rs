use jv_ast::{RegexLiteral, Span};
use jv_lexer::{Token, TokenMetadata, TokenType};

/// Construct a [`RegexLiteral`] from the originating [`Token`] and span.
pub fn regex_literal_from_token(token: &Token, span: Span) -> RegexLiteral {
    let normalized = match &token.token_type {
        TokenType::RegexLiteral(value) => value.clone(),
        _ => token.lexeme.clone(),
    };

    let (raw, pattern) = token
        .metadata
        .iter()
        .find_map(|metadata| match metadata {
            TokenMetadata::RegexLiteral { raw, pattern } => {
                Some((raw.clone(), pattern.clone()))
            }
            _ => None,
        })
        .unwrap_or_else(|| (format!("/{normalized}/"), normalized.clone()));

    let mut computed_span = span;
    computed_span.end_column = computed_span.start_column + raw.chars().count();

    RegexLiteral {
        pattern,
        raw,
        span: computed_span,
    }
}
