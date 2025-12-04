//! パターンマッチング構文のパース（簡易版）。

use super::{Parser, expression::parse_expression, recovery};
use crate::token::TokenKind;
use jv_ast::binding_pattern::BindingPatternKind;
use jv_ast::types::Literal;

pub fn parse_pattern<'src, 'alloc>(
    parser: &mut Parser<'src, 'alloc>,
) -> Option<BindingPatternKind> {
    let pat = match parser.current().kind {
        TokenKind::Identifier => parse_ident_pattern(parser),
        TokenKind::Underscore => {
            let span = parser.current().span;
            parser.advance();
            Some(BindingPatternKind::Wildcard {
                span: parser.ast_span(span),
            })
        }
        TokenKind::Number
        | TokenKind::String
        | TokenKind::TrueKw
        | TokenKind::FalseKw
        | TokenKind::NullKw => parse_literal_pattern(parser),
        TokenKind::LeftParen => parse_tuple_pattern(parser),
        TokenKind::LeftBracket => parse_list_pattern(parser),
        _ => None,
    }?;

    Some(parse_guard(parser, pat))
}

fn parse_guard<'src, 'alloc>(
    parser: &mut Parser<'src, 'alloc>,
    pat: BindingPatternKind,
) -> BindingPatternKind {
    if !parser.consume_if(TokenKind::If) {
        return pat;
    }

    let guard_expr = parse_expression(parser);
    if guard_expr.is_none() {
        let span = parser.current().span;
        parser.push_diagnostic(crate::diagnostics::Diagnostic::new(
            "guard expression expected after pattern",
            span,
        ));
        recovery::recover_to_sync_point(parser);
    }

    pat
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
        span: parser.ast_span(token.span),
    })
}

fn parse_tuple_pattern<'src, 'alloc>(
    parser: &mut Parser<'src, 'alloc>,
) -> Option<BindingPatternKind> {
    let open = parser.advance().span; // consume '('
    let mut elements = Vec::new();
    let mut closed = false;
    let mut end_span = open;
    let mut emitted_diag = false;

    if parser.consume_if(TokenKind::RightParen) {
        return Some(BindingPatternKind::Tuple {
            elements,
            span: parser.ast_span(open),
        });
    }

    loop {
        if let Some(pat) = parse_pattern(parser) {
            let pat_span = pat.span().clone();
            end_span = parser.span_from_ast(&pat_span);
            elements.push(pat);
        } else {
            break;
        }
        if parser.consume_if(TokenKind::Comma) {
            continue;
        }
        if parser.current().kind == TokenKind::RightParen {
            let close = parser.advance().span;
            end_span = close;
            closed = true;
            break;
        } else {
            // missing ')'
            let err_span = parser.current().span;
            parser.push_diagnostic(crate::diagnostics::Diagnostic::new(
                "expected ')' to close tuple pattern",
                err_span,
            ));
            emitted_diag = true;
            break;
        }
    }

    if !closed {
        if !emitted_diag {
            let span = parser.current().span;
            parser.push_diagnostic(crate::diagnostics::Diagnostic::new(
                "expected ')' to close tuple pattern",
                span,
            ));
        }
        recovery::recover_to_sync_point(parser);
    }

    let span = parser.ast_span(open.merge(end_span));
    Some(BindingPatternKind::Tuple { elements, span })
}

fn parse_list_pattern<'src, 'alloc>(
    parser: &mut Parser<'src, 'alloc>,
) -> Option<BindingPatternKind> {
    let open = parser.advance().span; // consume '['
    let mut elements = Vec::new();
    let mut closed = false;
    let mut end_span = open;
    let mut emitted_diag = false;

    if parser.consume_if(TokenKind::RightBracket) {
        return Some(BindingPatternKind::List {
            elements,
            span: parser.ast_span(open),
        });
    }

    loop {
        if let Some(pat) = parse_pattern(parser) {
            let pat_span = pat.span().clone();
            end_span = parser.span_from_ast(&pat_span);
            elements.push(pat);
        } else {
            break;
        }
        if parser.consume_if(TokenKind::Comma) {
            continue;
        }
        if parser.current().kind == TokenKind::RightBracket {
            let close = parser.advance().span;
            end_span = close;
            closed = true;
            break;
        } else {
            let err_span = parser.current().span;
            parser.push_diagnostic(crate::diagnostics::Diagnostic::new(
                "expected ']' to close list pattern",
                err_span,
            ));
            emitted_diag = true;
            break;
        }
    }

    if !closed {
        if !emitted_diag {
            let span = parser.current().span;
            parser.push_diagnostic(crate::diagnostics::Diagnostic::new(
                "expected ']' to close list pattern",
                span,
            ));
        }
        recovery::recover_to_sync_point(parser);
    }

    let span = parser.ast_span(open.merge(end_span));
    Some(BindingPatternKind::List { elements, span })
}

fn parse_literal_pattern<'src, 'alloc>(
    parser: &mut Parser<'src, 'alloc>,
) -> Option<BindingPatternKind> {
    let tok = parser.current();
    let lit = match tok.kind {
        TokenKind::Number => Literal::Number(parser.lexeme(tok.span).unwrap_or("0").to_string()),
        TokenKind::String => Literal::String(parser.lexeme(tok.span).unwrap_or("").to_string()),
        TokenKind::TrueKw => Literal::Boolean(true),
        TokenKind::FalseKw => Literal::Boolean(false),
        TokenKind::NullKw => Literal::Null,
        _ => return None,
    };
    parser.advance();
    Some(BindingPatternKind::Literal {
        literal: lit,
        span: parser.ast_span(tok.span),
    })
}
