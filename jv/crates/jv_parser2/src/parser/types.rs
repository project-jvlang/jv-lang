//! 型アノテーションのパース（簡易版）。

use super::Parser;
use crate::token::TokenKind;
use jv_ast::types::{TypeAnnotation, UnitSymbol};

#[cfg(test)]
use crate::{allocator::Arena, lexer::Lexer, source::Source};

pub(crate) fn parse_type<'src, 'alloc>(
    parser: &mut Parser<'src, 'alloc>,
) -> Option<TypeAnnotation> {
    let mut ty = parse_primary_type(parser)?;

    // 後置: [] / ? / @unit を複数回処理する
    loop {
        if parser.consume_if(TokenKind::LeftBracket) {
            let _ = parser.consume_if(TokenKind::RightBracket);
            ty = TypeAnnotation::Array(Box::new(ty));
            continue;
        }

        if parser.consume_if(TokenKind::Question) {
            ty = TypeAnnotation::Nullable(Box::new(ty));
            continue;
        }

        if parser.consume_if(TokenKind::At) {
            if let Some((unit_name, span)) = parse_ident(parser) {
                let unit = UnitSymbol {
                    name: unit_name,
                    is_bracketed: false,
                    has_default_marker: false,
                    span: parser.ast_span(span),
                };
                ty = TypeAnnotation::Unit {
                    base: Box::new(ty),
                    unit,
                    implicit: false,
                };
            } else {
                let span = parser.current().span;
                parser.push_diagnostic(crate::diagnostics::Diagnostic::new(
                    "unit name expected after @",
                    span,
                ));
            }
            continue;
        }

        break;
    }

    Some(ty)
}

fn parse_primary_type<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) -> Option<TypeAnnotation> {
    // 関数型またはタプル型: (Type1, Type2) -> ReturnType または (Type1 Type2)
    if parser.current().kind == TokenKind::LeftParen {
        return parse_paren_type(parser);
    }

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

/// 括弧で始まる型をパース: 関数型 `(T) -> R` またはタプル型 `(T U)`
fn parse_paren_type<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) -> Option<TypeAnnotation> {
    let _start_span = parser.advance().span; // consume '('

    // 空の括弧 () の場合
    if parser.current().kind == TokenKind::RightParen {
        parser.advance(); // consume ')'
        // () -> R は引数なし関数型
        if parser.current().kind == TokenKind::Arrow {
            parser.advance(); // consume '->'
            let return_type = parse_type(parser)?;
            return Some(TypeAnnotation::Function {
                params: Vec::new(),
                return_type: Box::new(return_type),
            });
        }
        // 単なる () は空タプル
        return Some(TypeAnnotation::Simple("()".to_string()));
    }

    // 括弧内の型を収集
    let mut param_types = Vec::new();
    loop {
        // カンマをスキップ
        let _ = parser.consume_if(TokenKind::Comma);

        if parser.current().kind == TokenKind::RightParen {
            break;
        }
        if parser.current().kind == TokenKind::Eof {
            break;
        }

        // 内部の型をパース
        if let Some(inner_ty) = parse_type(parser) {
            param_types.push(inner_ty);
        } else {
            // パース失敗時はスキップ
            parser.advance();
        }
    }

    // ')' を消費
    let _ = parser.consume_if(TokenKind::RightParen);

    // '->' が続くなら関数型
    if parser.current().kind == TokenKind::Arrow {
        parser.advance(); // consume '->'
        let return_type = parse_type(parser).unwrap_or_else(|| TypeAnnotation::Simple("_".into()));
        return Some(TypeAnnotation::Function {
            params: param_types,
            return_type: Box::new(return_type),
        });
    }

    // そうでなければタプル型（文字列形式で返す）
    if param_types.is_empty() {
        return None;
    }

    // タプル型を "(Type1 Type2)" 形式の文字列として返す
    let elements: Vec<String> = param_types
        .iter()
        .map(|ty| type_annotation_to_string(ty))
        .collect();
    let tuple_str = format!("({})", elements.join(" "));
    Some(TypeAnnotation::Simple(tuple_str))
}

/// TypeAnnotation を文字列に変換（タプル型表現用）
fn type_annotation_to_string(ty: &TypeAnnotation) -> String {
    match ty {
        TypeAnnotation::Simple(s) => s.clone(),
        TypeAnnotation::Nullable(inner) => format!("{}?", type_annotation_to_string(inner)),
        TypeAnnotation::Array(inner) => format!("{}[]", type_annotation_to_string(inner)),
        TypeAnnotation::Generic { name, type_args } => {
            let args: Vec<String> = type_args.iter().map(type_annotation_to_string).collect();
            format!("{}<{}>", name, args.join(", "))
        }
        TypeAnnotation::Function { params, return_type } => {
            let param_strs: Vec<String> = params.iter().map(type_annotation_to_string).collect();
            format!("({}) -> {}", param_strs.join(", "), type_annotation_to_string(return_type))
        }
        TypeAnnotation::Unit { base, unit, .. } => {
            format!("{}@{}", type_annotation_to_string(base), unit.name)
        }
    }
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
        parser.push_diagnostic(crate::diagnostics::Diagnostic::new(
            "identifier expected",
            token.span,
        ));
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_type_str(input: &str) -> (Option<TypeAnnotation>, usize) {
        let source = Source::from_str(input);
        let lexer = Lexer::new(source);
        let arena = Arena::new();
        let mut parser = Parser::new(lexer, &arena);
        let ty = parse_type(&mut parser);
        (ty, parser.diagnostics.len())
    }

    #[test]
    fn parses_repeated_postfix_arrays_and_nullable() {
        let (ty, _) = parse_type_str("T[][]?");
        let expected = TypeAnnotation::Nullable(Box::new(TypeAnnotation::Array(Box::new(
            TypeAnnotation::Array(Box::new(TypeAnnotation::Simple("T".into()))),
        ))));
        assert_eq!(ty.unwrap(), expected);
    }

    #[test]
    fn reports_missing_identifier() {
        let (ty, diags) = parse_type_str("?");
        assert!(ty.is_none());
        assert!(diags > 0);
    }

    #[test]
    fn reports_missing_unit_name() {
        let (ty, diags) = parse_type_str("Int@");
        assert!(ty.is_some());
        assert!(diags > 0);
    }

    #[test]
    fn parses_tuple_type() {
        let (ty, diags) = parse_type_str("(Int String)");
        assert_eq!(diags, 0, "no diagnostics expected");
        let expected = TypeAnnotation::Simple("(Int String)".to_string());
        assert_eq!(ty, Some(expected));
    }

    #[test]
    fn parses_tuple_with_comma() {
        let (ty, _) = parse_type_str("(Int, String)");
        let expected = TypeAnnotation::Simple("(Int String)".to_string());
        assert_eq!(ty, Some(expected));
    }
}
