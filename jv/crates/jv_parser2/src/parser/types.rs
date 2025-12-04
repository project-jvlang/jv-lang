//! 型アノテーションのパース（簡易版）。

use super::Parser;
use crate::token::TokenKind;
use jv_ast::types::{TypeAnnotation, UnitSymbol};

pub(crate) fn parse_type<'src, 'alloc>(
    parser: &mut Parser<'src, 'alloc>,
) -> Option<TypeAnnotation> {
    let mut ty = parse_primary_type(parser)?;

    // 後置: nullable
    if parser.consume_if(TokenKind::Question) {
        ty = TypeAnnotation::Nullable(Box::new(ty));
    }

    // 後置: 単位型（@unit）
    if parser.consume_if(TokenKind::At) {
        if let Some((unit_name, span)) = parse_ident(parser) {
            let unit = UnitSymbol {
                name: unit_name,
                is_bracketed: false,
                has_default_marker: false,
                span: to_ast_span(span),
            };
            ty = TypeAnnotation::Unit {
                base: Box::new(ty),
                unit,
                implicit: false,
            };
        }
    }

    Some(ty)
}

fn parse_primary_type<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) -> Option<TypeAnnotation> {
    let (name, name_span) = parse_ident(parser)?;
    let mut ty = TypeAnnotation::Simple(name);

    // ジェネリクス <T, U>
    if parser.consume_if(TokenKind::Less) {
        let mut args = Vec::new();
        loop {
            let arg = parse_type(parser).unwrap_or_else(|| TypeAnnotation::Simple("_".into()));
            args.push(arg);
            if parser.consume_if(TokenKind::Comma) {
                continue;
            }
            let _ = parser.consume_if(TokenKind::Greater);
            break;
        }
        ty = TypeAnnotation::Generic {
            name: match ty {
                TypeAnnotation::Simple(n) => n,
                _ => "_".into(),
            },
            type_args: args,
        };
    }

    // 配列型 T[]
    if parser.consume_if(TokenKind::LeftBracket) {
        let _ = parser.consume_if(TokenKind::RightBracket);
        ty = TypeAnnotation::Array(Box::new(ty));
    }

    // スパンを考慮した拡張が必要になった場合はここで包む
    let _ = name_span;
    Some(ty)
}

fn parse_ident<'src, 'alloc>(
    parser: &mut Parser<'src, 'alloc>,
) -> Option<(String, crate::span::Span)> {
    let token = parser.current();
    if token.kind == TokenKind::Identifier {
        let name = parser
            .lexeme(token.span)
            .map(|s| s.to_string())
            .unwrap_or_else(|| format!("_id{}", token.span.start));
        parser.advance();
        Some((name, token.span))
    } else {
        None
    }
}

fn to_ast_span(span: crate::span::Span) -> jv_ast::Span {
    jv_ast::Span {
        start_line: 0,
        start_column: span.start as usize,
        end_line: 0,
        end_column: span.end as usize,
    }
}
