use super::context::{LoweringContext, LoweringDiagnostic};
use crate::lexer::{TokenKind, token_type_from_kind};
use crate::parser::OwnedToken;
use jv_ast::types::TypeAnnotation;
use jv_lexer::TokenType;

/// 型注釈トークンのスライスを jv_type_inference_java へ渡して解釈する。
pub fn lower_type_annotation(
    ctx: &mut LoweringContext<'_>,
    tokens: &[OwnedToken],
) -> Option<TypeAnnotation> {
    if let Some(tuple_raw) = try_extract_tuple_annotation(tokens) {
        return Some(TypeAnnotation::Simple(tuple_raw));
    }

    let filtered: Vec<_> = tokens
        .iter()
        .filter(|&tok| {
            !matches!(
                tok.kind,
                TokenKind::Whitespace
                    | TokenKind::Newline
                    | TokenKind::LineComment
                    | TokenKind::BlockComment
                    | TokenKind::JavaDocComment
            )
        })
        .cloned()
        .collect();

    if filtered.is_empty() {
        return None;
    }
    let mut end = find_type_boundary(&filtered);
    let mut last_error = None;
    while end > 0 {
        let slice = &filtered[..end];
        let converted = convert_tokens(ctx, slice);
        match jv_type_inference_java::lower_type_annotation_from_tokens(&converted) {
            Ok(lowered) => return Some(lowered.into_annotation()),
            Err(err) => {
                last_error = Some((slice.to_vec(), err.kind()));
                end = end.saturating_sub(1);
            }
        }
    }

    if let Some((slice, kind)) = last_error {
        let span = slice.first().map(|t| ctx.span_for_token(t));
        let message = format!("型注釈の解釈に失敗しました: {:?}", kind);
        ctx.push_diagnostic(LoweringDiagnostic::error(message, span));
    }
    None
}

fn try_extract_tuple_annotation(tokens: &[OwnedToken]) -> Option<String> {
    let start = tokens.iter().position(|tok| {
        !matches!(
            tok.kind,
            TokenKind::Whitespace
                | TokenKind::Newline
                | TokenKind::LayoutComma
                | TokenKind::FieldNameLabel
                | TokenKind::LineComment
                | TokenKind::BlockComment
                | TokenKind::JavaDocComment
        )
    })?;
    let end = tokens.iter().rposition(|tok| {
        !matches!(
            tok.kind,
            TokenKind::Whitespace
                | TokenKind::Newline
                | TokenKind::LayoutComma
                | TokenKind::FieldNameLabel
                | TokenKind::LineComment
                | TokenKind::BlockComment
                | TokenKind::JavaDocComment
        )
    })?;

    let start_tok = tokens.get(start)?;
    let end_tok = tokens.get(end)?;
    if start_tok.kind != TokenKind::LeftParen || end_tok.kind != TokenKind::RightParen {
        return None;
    }

    let mut paren_depth = 0usize;
    let mut angle_depth = 0usize;
    let mut bracket_depth = 0usize;
    let mut elements = 0usize;
    let mut current_has_content = false;

    for tok in tokens.get(start..=end)? {
        match tok.kind {
            TokenKind::Arrow => return None,
            TokenKind::LeftParen => {
                paren_depth += 1;
                current_has_content = true;
            }
            TokenKind::RightParen => {
                if paren_depth == 0 {
                    return None;
                }
                paren_depth = paren_depth.saturating_sub(1);
                current_has_content = true;
            }
            TokenKind::Less => {
                angle_depth += 1;
                current_has_content = true;
            }
            TokenKind::Greater => {
                if angle_depth == 0 {
                    return None;
                }
                angle_depth = angle_depth.saturating_sub(1);
                current_has_content = true;
            }
            TokenKind::LeftBracket => {
                bracket_depth += 1;
                current_has_content = true;
            }
            TokenKind::RightBracket => {
                if bracket_depth == 0 {
                    return None;
                }
                bracket_depth = bracket_depth.saturating_sub(1);
                current_has_content = true;
            }
            TokenKind::Comma
            | TokenKind::LayoutComma
            | TokenKind::Whitespace
            | TokenKind::Newline
                if paren_depth == 1 && angle_depth == 0 && bracket_depth == 0 =>
            {
                if current_has_content {
                    elements += 1;
                    current_has_content = false;
                }
            }
            TokenKind::LineComment
            | TokenKind::BlockComment
            | TokenKind::JavaDocComment
            | TokenKind::LayoutComma
            | TokenKind::FieldNameLabel
            | TokenKind::Whitespace
            | TokenKind::Newline => {}
            _ => current_has_content = true,
        }
    }

    if paren_depth != 0 || angle_depth != 0 || bracket_depth != 0 {
        return None;
    }
    if current_has_content {
        elements += 1;
    }
    if elements < 2 {
        return None;
    }

    let raw = tokens
        .get(start..=end)?
        .iter()
        .map(|tok| match tok.kind {
            TokenKind::LayoutComma => {
                let lexeme = tok.lexeme.as_ref();
                if lexeme.is_empty() { " " } else { lexeme }
            }
            _ => tok.lexeme.as_ref(),
        })
        .collect::<String>()
        .trim()
        .to_string();
    Some(raw)
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
            TokenKind::RightParen => paren = paren.saturating_sub(1),
            TokenKind::LeftBracket => bracket += 1,
            TokenKind::RightBracket => bracket = bracket.saturating_sub(1),
            _ => {}
        }

        if angle == 0 && paren == 0 && bracket == 0 {
            match tok.kind {
                TokenKind::Comma | TokenKind::Semicolon | TokenKind::Assign => return idx,
                TokenKind::Where | TokenKind::Colon => return idx,
                TokenKind::RightParen => {
                    if matches!(
                        tokens.get(idx + 1).map(|t| t.kind),
                        Some(TokenKind::Colon | TokenKind::Where)
                    ) {
                        return idx;
                    }
                }
                _ => {}
            }
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
                token_type: token_type_with_lexeme(tok),
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

fn token_type_with_lexeme(tok: &OwnedToken) -> TokenType {
    match tok.kind {
        TokenKind::Identifier => TokenType::Identifier(tok.lexeme_string()),
        TokenKind::Number => TokenType::Number(tok.lexeme_string()),
        TokenKind::String => TokenType::String(tok.lexeme_string()),
        TokenKind::StringInterpolation => TokenType::StringInterpolation(tok.lexeme_string()),
        TokenKind::RegexLiteral => TokenType::RegexLiteral(tok.lexeme_string()),
        TokenKind::Character => TokenType::Character(tok.lexeme.chars().next().unwrap_or('\0')),
        _ => token_type_from_kind(tok.kind),
    }
}
