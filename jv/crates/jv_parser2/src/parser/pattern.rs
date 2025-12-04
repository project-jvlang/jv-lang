//! パターンマッチング構文のパース（簡易版）。

use super::Parser;
use crate::token::TokenKind;
use jv_ast::binding_pattern::BindingPatternKind;

pub(crate) fn parse_pattern<'src, 'alloc>(
    parser: &mut Parser<'src, 'alloc>,
) -> Option<BindingPatternKind> {
    match parser.current().kind {
        TokenKind::Identifier => parse_ident_pattern(parser),
        TokenKind::Underscore => {
            let span = parser.current().span;
            parser.advance();
            Some(BindingPatternKind::Wildcard {
                span: to_ast_span(span),
            })
        }
        TokenKind::LeftParen => parse_tuple_pattern(parser),
        TokenKind::LeftBracket => parse_list_pattern(parser),
        _ => None,
    }
}

fn parse_ident_pattern<'src, 'alloc>(
    parser: &mut Parser<'src, 'alloc>,
) -> Option<BindingPatternKind> {
    let token = parser.current();
    if token.kind != TokenKind::Identifier {
        return None;
    }
    let name = parser
        .lexeme(token.span)
        .map(|s| s.to_string())
        .unwrap_or_else(|| format!("_id{}", token.span.start));
    parser.advance();
    Some(BindingPatternKind::Identifier {
        name,
        span: to_ast_span(token.span),
    })
}

fn parse_tuple_pattern<'src, 'alloc>(
    parser: &mut Parser<'src, 'alloc>,
) -> Option<BindingPatternKind> {
    let open = parser.advance().span; // consume '('
    let mut elements = Vec::new();

    if parser.consume_if(TokenKind::RightParen) {
        return Some(BindingPatternKind::Tuple {
            elements,
            span: to_ast_span(open),
        });
    }

    loop {
        if let Some(pat) = parse_pattern(parser) {
            elements.push(pat);
        } else {
            break;
        }
        if parser.consume_if(TokenKind::Comma) {
            continue;
        }
        let _ = parser.consume_if(TokenKind::RightParen);
        break;
    }

    let end_span = parser.current().span;
    let span = open.merge(end_span);
    Some(BindingPatternKind::Tuple {
        elements,
        span: to_ast_span(span),
    })
}

fn parse_list_pattern<'src, 'alloc>(
    parser: &mut Parser<'src, 'alloc>,
) -> Option<BindingPatternKind> {
    let open = parser.advance().span; // consume '['
    let mut elements = Vec::new();

    if parser.consume_if(TokenKind::RightBracket) {
        return Some(BindingPatternKind::List {
            elements,
            span: to_ast_span(open),
        });
    }

    loop {
        if let Some(pat) = parse_pattern(parser) {
            elements.push(pat);
        } else {
            break;
        }
        if parser.consume_if(TokenKind::Comma) {
            continue;
        }
        let _ = parser.consume_if(TokenKind::RightBracket);
        break;
    }

    let end_span = parser.current().span;
    let span = open.merge(end_span);
    Some(BindingPatternKind::List {
        elements,
        span: to_ast_span(span),
    })
}

fn to_ast_span(span: crate::span::Span) -> jv_ast::Span {
    jv_ast::Span {
        start_line: 0,
        start_column: span.start as usize,
        end_line: 0,
        end_column: span.end as usize,
    }
}
