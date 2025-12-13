use super::context::LoweringContext;
use crate::lexer::TokenKind;
use crate::parser::OwnedToken;
use jv_ast::expression::Expression;
use jv_ast::types::{Literal, RegexLiteral};

/// 単純リテラルをローワリングする。
#[allow(dead_code)]
pub fn lower_literal(tok: &OwnedToken, ctx: &LoweringContext<'_>) -> Option<Expression> {
    match tok.kind {
        TokenKind::Number => Some(Expression::Literal(
            Literal::Number(tok.lexeme_string()),
            ctx.span_for_token(tok),
        )),
        TokenKind::String | TokenKind::StringInterpolation => Some(Expression::Literal(
            Literal::String(strip_quotes(&tok.lexeme)),
            ctx.span_for_token(tok),
        )),
        TokenKind::Character => {
            let ch = tok.lexeme.chars().next().unwrap_or_default();
            Some(Expression::Literal(
                Literal::Character(ch),
                ctx.span_for_token(tok),
            ))
        }
        TokenKind::BooleanTrue => Some(Expression::Literal(
            Literal::Boolean(true),
            ctx.span_for_token(tok),
        )),
        TokenKind::BooleanFalse => Some(Expression::Literal(
            Literal::Boolean(false),
            ctx.span_for_token(tok),
        )),
        TokenKind::Null => Some(Expression::Literal(Literal::Null, ctx.span_for_token(tok))),
        TokenKind::RegexLiteral => Some(Expression::RegexLiteral(RegexLiteral {
            pattern: tok.lexeme_string(),
            raw: tok.lexeme_string(),
            span: ctx.span_for_token(tok),
            origin: None,
            const_key: None,
            template_segments: Vec::new(),
        })),
        _ => None,
    }
}

#[allow(dead_code)]
fn strip_quotes(raw: &str) -> String {
    if raw.len() >= 2 {
        let first = raw.chars().next().unwrap_or('"');
        let last = raw.chars().last().unwrap_or('"');
        if (first == '"' && last == '"') || (first == '\'' && last == '\'') {
            return raw[1..raw.len().saturating_sub(1)].to_string();
        }
    }
    raw.to_string()
}
