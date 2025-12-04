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
                    span: to_ast_span(span),
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
        parser.push_diagnostic(crate::diagnostics::Diagnostic::new(
            "identifier expected",
            token.span,
        ));
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
}
