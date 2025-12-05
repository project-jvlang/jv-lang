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
    // タプル型: (Type1 Type2 ...) または (Type1, Type2, ...)
    if parser.current().kind == TokenKind::LeftParen {
        return parse_tuple_type(parser);
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

/// タプル型をパース: (Type1 Type2) または (Type1, Type2)
/// TypeAnnotation::Simple("(Type1 Type2)") として返す
fn parse_tuple_type<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) -> Option<TypeAnnotation> {
    let start_span = parser.advance().span; // consume '('
    let mut elements = Vec::new();
    let mut depth = 1;

    // 括弧内の各型要素を収集
    while depth > 0 && parser.current().kind != TokenKind::Eof {
        match parser.current().kind {
            TokenKind::LeftParen => {
                depth += 1;
                // ネストしたタプルの場合は再帰的にパース
                if let Some(nested) = parse_tuple_type(parser) {
                    if let TypeAnnotation::Simple(s) = nested {
                        elements.push(s);
                    }
                }
            }
            TokenKind::RightParen => {
                depth -= 1;
                if depth == 0 {
                    parser.advance(); // consume ')'
                    break;
                }
            }
            TokenKind::Comma => {
                // カンマは無視（スペース区切りと同じ扱い）
                parser.advance();
            }
            TokenKind::Identifier => {
                // 型名を取得
                if let Some((type_name, _)) = parse_ident(parser) {
                    // ジェネリクス引数をチェック
                    let mut full_type = type_name;
                    if parser.current().kind == TokenKind::Less {
                        full_type.push('<');
                        parser.advance();
                        let mut generic_depth = 1;
                        while generic_depth > 0 && parser.current().kind != TokenKind::Eof {
                            let tok = parser.current();
                            if tok.kind == TokenKind::Less {
                                generic_depth += 1;
                            } else if tok.kind == TokenKind::Greater {
                                generic_depth -= 1;
                            }
                            if let Some(text) = parser.lexeme(tok.span) {
                                full_type.push_str(text);
                            }
                            parser.advance();
                            if generic_depth > 0 && tok.kind == TokenKind::Comma {
                                full_type.push(' ');
                            }
                        }
                    }
                    // 配列マーカー[]
                    while parser.current().kind == TokenKind::LeftBracket {
                        parser.advance();
                        if parser.current().kind == TokenKind::RightBracket {
                            parser.advance();
                        }
                        full_type.push_str("[]");
                    }
                    // Nullable ?
                    while parser.current().kind == TokenKind::Question {
                        parser.advance();
                        full_type.push('?');
                    }
                    elements.push(full_type);
                }
            }
            _ => {
                // 予期しないトークンはスキップ
                parser.advance();
            }
        }
    }

    if elements.is_empty() {
        return None;
    }

    // "(Type1 Type2)" 形式の文字列として返す
    let tuple_str = format!("({elements})", elements = elements.join(" "));
    Some(TypeAnnotation::Simple(tuple_str))
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
