use super::context::{LoweringContext, LoweringDiagnostic};
use crate::lexer::{TokenKind, token_type_from_kind};
use crate::parser::OwnedToken;
use jv_ast::types::TypeAnnotation;

/// 型注釈トークンのスライスを jv_type_inference_java へ渡して解釈する。
pub fn lower_type_annotation(
    ctx: &mut LoweringContext<'_>,
    tokens: &[OwnedToken],
) -> Option<TypeAnnotation> {
    if tokens.is_empty() {
        return None;
    }
    let end = find_type_boundary(tokens);
    let slice = &tokens[..end];
    let converted = convert_tokens(ctx, tokens);
    match jv_type_inference_java::lower_type_annotation_from_tokens(&converted) {
        Ok(lowered) => Some(lowered.into_annotation()),
        Err(err) => {
            let span = slice.first().map(|t| ctx.span_for_token(t));
            let message = format!("型注釈の解釈に失敗しました: {:?}", err.kind());
            ctx.push_diagnostic(LoweringDiagnostic::error(message, span));
            None
        }
    }
}

fn find_type_boundary(tokens: &[OwnedToken]) -> usize {
    let mut angle = 0usize;
    let mut paren = 0usize;
    let mut bracket = 0usize;
    for (idx, tok) in tokens.iter().enumerate() {
        match tok.kind {
            TokenKind::Less => angle += 1,
            TokenKind::Greater => angle = angle.saturating_sub(1),
            TokenKind::LeftParen => paren += 1,
            TokenKind::RightParen => {
                if paren == 0 {
                    return idx;
                }
                paren -= 1;
            }
            TokenKind::LeftBracket => bracket += 1,
            TokenKind::RightBracket => {
                if bracket == 0 {
                    return idx;
                }
                bracket -= 1;
            }
            TokenKind::Comma | TokenKind::Semicolon | TokenKind::Assign
                if angle == 0 && paren == 0 && bracket == 0 =>
            {
                return idx;
            }
            _ => {}
        }
    }
    tokens.len()
}

fn convert_tokens(ctx: &LoweringContext<'_>, tokens: &[OwnedToken]) -> Vec<jv_lexer::Token> {
    tokens
        .iter()
        .map(|tok| {
            let span = ctx.span_for_token(tok);
            jv_lexer::Token {
                token_type: token_type_from_kind(tok.kind),
                lexeme: tok.lexeme.clone(),
                line: span.start_line,
                column: span.start_column,
                leading_trivia: tok.leading_trivia.clone(),
                diagnostic: tok.diagnostic.clone(),
                metadata: tok.metadata.clone(),
            }
        })
        .collect()
}
