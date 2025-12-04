//! Pratt 構文解析による式パーサー（簡易版）。

use super::Parser;
use crate::token::TokenKind;
use jv_ast::expression::BinaryMetadata;
use jv_ast::types::{BinaryOp, Literal, UnaryOp};
use jv_ast::{Expression, Span as AstSpan};

pub(crate) fn parse_expression<'src, 'alloc>(
    parser: &mut Parser<'src, 'alloc>,
) -> Option<Expression> {
    parse_precedence(parser, 0)
}

fn parse_precedence<'src, 'alloc>(
    parser: &mut Parser<'src, 'alloc>,
    min_bp: u8,
) -> Option<Expression> {
    let mut left = parse_prefix(parser)?;

    loop {
        let token = parser.current();
        let Some((l_bp, r_bp, op)) = infix_binding_power(token.kind) else {
            break;
        };
        if l_bp < min_bp {
            break;
        }
        parser.advance();
        let right = parse_precedence(parser, r_bp).unwrap_or_else(|| dummy_expr(token.span));
        let span = token.span.merge(span_of_expr(&left));
        left = Expression::Binary {
            left: Box::new(left),
            op,
            right: Box::new(right),
            span: to_ast_span(span),
            metadata: BinaryMetadata::default(),
        };
    }

    Some(left)
}

fn parse_prefix<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) -> Option<Expression> {
    let token = parser.current();
    match token.kind {
        TokenKind::Identifier => {
            parser.advance();
            let name = parser
                .lexeme(token.span)
                .map(|s| s.to_string())
                .unwrap_or_else(|| format!("_id{}", token.span.start));
            Some(Expression::Identifier(name, to_ast_span(token.span)))
        }
        TokenKind::Number => {
            parser.advance();
            let text = parser
                .lexeme(token.span)
                .map(|s| s.to_string())
                .unwrap_or_else(|| "0".to_string());
            Some(Expression::Literal(
                Literal::Number(text),
                to_ast_span(token.span),
            ))
        }
        TokenKind::String => {
            parser.advance();
            let text = parser
                .lexeme(token.span)
                .map(|s| s.to_string())
                .unwrap_or_default();
            Some(Expression::Literal(
                Literal::String(text),
                to_ast_span(token.span),
            ))
        }
        TokenKind::TrueKw => {
            parser.advance();
            Some(Expression::Literal(
                Literal::Boolean(true),
                to_ast_span(token.span),
            ))
        }
        TokenKind::FalseKw => {
            parser.advance();
            Some(Expression::Literal(
                Literal::Boolean(false),
                to_ast_span(token.span),
            ))
        }
        TokenKind::NullKw => {
            parser.advance();
            Some(Expression::Literal(Literal::Null, to_ast_span(token.span)))
        }
        TokenKind::LeftParen => {
            parser.advance();
            let expr = parse_expression(parser).unwrap_or_else(|| dummy_expr(token.span));
            let _ = parser.consume_if(TokenKind::RightParen);
            Some(expr)
        }
        TokenKind::Minus => {
            parser.advance();
            let rhs = parse_precedence(parser, 70).unwrap_or_else(|| dummy_expr(token.span));
            Some(Expression::Unary {
                op: UnaryOp::Minus,
                operand: Box::new(rhs),
                span: to_ast_span(token.span),
            })
        }
        TokenKind::Plus => {
            parser.advance();
            let rhs = parse_precedence(parser, 70).unwrap_or_else(|| dummy_expr(token.span));
            Some(Expression::Unary {
                op: UnaryOp::Plus,
                operand: Box::new(rhs),
                span: to_ast_span(token.span),
            })
        }
        TokenKind::Not => {
            parser.advance();
            let rhs = parse_precedence(parser, 70).unwrap_or_else(|| dummy_expr(token.span));
            Some(Expression::Unary {
                op: UnaryOp::Not,
                operand: Box::new(rhs),
                span: to_ast_span(token.span),
            })
        }
        _ => {
            // 不明なプレフィックスはダミー式で継続。
            parser.advance();
            Some(dummy_expr(token.span))
        }
    }
}

fn infix_binding_power(kind: TokenKind) -> Option<(u8, u8, BinaryOp)> {
    // l_bp, r_bp
    let res = match kind {
        TokenKind::Multiply | TokenKind::Divide | TokenKind::Modulo => (70, 71, BinaryOp::Multiply),
        TokenKind::Plus => (60, 61, BinaryOp::Add),
        TokenKind::Minus => (60, 61, BinaryOp::Subtract),
        TokenKind::Less => (50, 51, BinaryOp::Less),
        TokenKind::LessEqual => (50, 51, BinaryOp::LessEqual),
        TokenKind::Greater => (50, 51, BinaryOp::Greater),
        TokenKind::GreaterEqual => (50, 51, BinaryOp::GreaterEqual),
        TokenKind::Equal => (40, 41, BinaryOp::Equal),
        TokenKind::NotEqual => (40, 41, BinaryOp::NotEqual),
        TokenKind::RangeExclusive => (35, 36, BinaryOp::RangeExclusive),
        TokenKind::RangeInclusive => (35, 36, BinaryOp::RangeInclusive),
        TokenKind::Elvis => (30, 31, BinaryOp::Elvis),
        TokenKind::And => (25, 26, BinaryOp::And),
        TokenKind::Or => (20, 21, BinaryOp::Or),
        _ => return None,
    };
    Some(res)
}

fn dummy_expr(span: crate::span::Span) -> Expression {
    Expression::Identifier("_".into(), to_ast_span(span))
}

fn span_of_expr(expr: &Expression) -> crate::span::Span {
    match expr {
        Expression::Identifier(_, span)
        | Expression::Literal(_, span)
        | Expression::Unary { span, .. }
        | Expression::Binary { span, .. } => {
            crate::span::Span::new(span.start_column as u32, span.end_column as u32)
        }
        _ => crate::span::Span::new(0, 0),
    }
}

fn to_ast_span(span: crate::span::Span) -> AstSpan {
    AstSpan {
        start_line: 0,
        start_column: span.start as usize,
        end_line: 0,
        end_column: span.end as usize,
    }
}
