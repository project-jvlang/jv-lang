use super::context::LoweringContext;
use super::expressions::lower_expression;
use super::types::lower_type_annotation;
use crate::parser::OwnedToken;
use jv_ast::expression::{Parameter, ParameterModifiers, ParameterProperty};
use jv_ast::types::TypeAnnotation;

/// 簡易なパラメータパーサ。カンマ区切りでトークンを分割し、val/var 修飾・型注釈を抽出する。
pub fn parse_parameters(ctx: &mut LoweringContext<'_>, tokens: &[OwnedToken]) -> Vec<Parameter> {
    let mut params = Vec::new();
    let mut depth = 0usize;
    let mut start = 0usize;
    for (idx, tok) in tokens.iter().enumerate() {
        match tok.kind {
            crate::lexer::TokenKind::LeftParen
            | crate::lexer::TokenKind::LeftBracket
            | crate::lexer::TokenKind::Less => depth += 1,
            crate::lexer::TokenKind::RightParen
            | crate::lexer::TokenKind::RightBracket
            | crate::lexer::TokenKind::Greater => depth = depth.saturating_sub(1),
            crate::lexer::TokenKind::Comma if depth == 0 => {
                push_param(ctx, &tokens[start..idx], &mut params);
                start = idx + 1;
            }
            _ => {}
        }
    }
    if start < tokens.len() {
        push_param(ctx, &tokens[start..], &mut params);
    }
    params
}

fn push_param(ctx: &mut LoweringContext<'_>, slice: &[OwnedToken], out: &mut Vec<Parameter>) {
    if slice.is_empty() {
        return;
    }
    let mut iter = slice.iter();
    let first = match iter.next() {
        Some(f) => f,
        None => return,
    };

    let mut modifiers = ParameterModifiers::default();
    let mut name_token = first;
    if first.lexeme_eq("val") {
        modifiers.property = ParameterProperty::Val;
        name_token = iter.next().unwrap_or(first);
    } else if first.lexeme_eq("var") {
        modifiers.property = ParameterProperty::Var;
        name_token = iter.next().unwrap_or(first);
    }

    let mut type_annotation: Option<TypeAnnotation> = None;
    if let Some(colon_idx) = slice
        .iter()
        .position(|t| t.kind == crate::lexer::TokenKind::Colon)
    {
        let ty_tokens = &slice[colon_idx + 1..];
        if !ty_tokens.is_empty() {
            type_annotation = lower_type_annotation(ctx, ty_tokens);
        }
    }

    let default_value = if let Some(assign_idx) = slice
        .iter()
        .position(|t| t.kind == crate::lexer::TokenKind::Assign)
    {
        let value_tokens = &slice[assign_idx + 1..];
        lower_expression(ctx, value_tokens)
    } else {
        None
    };

    out.push(Parameter {
        name: name_token.lexeme_string(),
        type_annotation,
        default_value,
        modifiers,
        span: ctx.span_for_range(
            slice.first().unwrap_or(name_token),
            slice.last().unwrap_or(name_token),
        ),
    });
}
