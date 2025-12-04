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
        // 後置演算子（呼び出し・メンバアクセス・インデックス）を処理。
        if let Some(next) = parse_postfix(parser, &mut left) {
            left = next;
            continue;
        }

        // 三項 ?: （右結合）
        if parser.current().kind == TokenKind::Question {
            let l_bp = 15;
            if l_bp < min_bp {
                break;
            }
            parser.advance(); // consume '?'
            let then_expr = parse_precedence(parser, l_bp + 1)
                .unwrap_or_else(|| dummy_expr(parser.current().span));
            if !parser.consume_if(TokenKind::Colon) {
                // colon missing, continue with dummy else
            }
            let else_expr =
                parse_precedence(parser, l_bp).unwrap_or_else(|| dummy_expr(parser.current().span));
            let span = span_of_expr(&left)
                .merge(span_of_expr(&then_expr))
                .merge(span_of_expr(&else_expr));
            left = Expression::If {
                condition: Box::new(left),
                then_branch: Box::new(then_expr),
                else_branch: Some(Box::new(else_expr)),
                span: to_ast_span(span),
            };
            continue;
        }

        let token = parser.current();
        let Some((l_bp, r_bp, op)) = infix_binding_power(token.kind) else {
            break;
        };
        if l_bp < min_bp {
            break;
        }
        parser.advance();
        let right = parse_precedence(parser, r_bp).unwrap_or_else(|| dummy_expr(token.span));
        let span = span_of_expr(&left)
            .merge(span_of_expr(&right))
            .merge(token.span);
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
            let rhs = parse_precedence(parser, 90).unwrap_or_else(|| dummy_expr(token.span));
            Some(Expression::Unary {
                op: UnaryOp::Minus,
                operand: Box::new(rhs),
                span: to_ast_span(token.span),
            })
        }
        TokenKind::Plus => {
            parser.advance();
            let rhs = parse_precedence(parser, 90).unwrap_or_else(|| dummy_expr(token.span));
            Some(Expression::Unary {
                op: UnaryOp::Plus,
                operand: Box::new(rhs),
                span: to_ast_span(token.span),
            })
        }
        TokenKind::Not => {
            parser.advance();
            let rhs = parse_precedence(parser, 90).unwrap_or_else(|| dummy_expr(token.span));
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
        | Expression::Binary { span, .. } => ast_span_to_span(span),
        Expression::MemberAccess { span, .. }
        | Expression::NullSafeMemberAccess { span, .. }
        | Expression::Call { span, .. }
        | Expression::IndexAccess { span, .. }
        | Expression::NullSafeIndexAccess { span, .. } => ast_span_to_span(span),
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

fn ast_span_to_span(span: &AstSpan) -> crate::span::Span {
    crate::span::Span::new(span.start_column as u32, span.end_column as u32)
}

fn parse_postfix<'src, 'alloc>(
    parser: &mut Parser<'src, 'alloc>,
    left: &mut Expression,
) -> Option<Expression> {
    let token = parser.current();
    match token.kind {
        TokenKind::Dot | TokenKind::NullSafe => {
            let null_safe = token.kind == TokenKind::NullSafe;
            parser.advance(); // consume . or ?.
            let ident_token = parser.current();
            if ident_token.kind != TokenKind::Identifier {
                return None;
            }
            let name = parser
                .lexeme(ident_token.span)
                .map(|s| s.to_string())
                .unwrap_or_else(|| format!("_id{}", ident_token.span.start));
            parser.advance();
            let combined = span_of_expr(left).merge(token.span).merge(ident_token.span);
            let span = to_ast_span(combined);
            if null_safe {
                Some(Expression::NullSafeMemberAccess {
                    object: Box::new(left.clone()),
                    property: name,
                    span,
                })
            } else {
                Some(Expression::MemberAccess {
                    object: Box::new(left.clone()),
                    property: name,
                    span,
                })
            }
        }
        TokenKind::LeftParen => {
            let open = parser.advance().span;
            let mut args = Vec::new();
            let mut last_span = open;
            if !parser.consume_if(TokenKind::RightParen) {
                loop {
                    let arg = parse_expression(parser).unwrap_or_else(|| dummy_expr(open));
                    last_span = span_of_expr(&arg);
                    args.push(jv_ast::expression::Argument::Positional(arg));
                    if parser.consume_if(TokenKind::Comma) {
                        continue;
                    }
                    let _ = parser.consume_if(TokenKind::RightParen);
                    break;
                }
            }
            let combined = span_of_expr(left).merge(open).merge(last_span);
            Some(Expression::Call {
                function: Box::new(left.clone()),
                args,
                type_arguments: Vec::new(),
                argument_metadata: Default::default(),
                span: to_ast_span(combined),
            })
        }
        TokenKind::LeftBracket => {
            let open = parser.advance().span;
            let index = parse_expression(parser).unwrap_or_else(|| dummy_expr(open));
            let _ = parser.consume_if(TokenKind::RightBracket);
            let combined = span_of_expr(left).merge(open).merge(span_of_expr(&index));
            Some(Expression::IndexAccess {
                object: Box::new(left.clone()),
                index: Box::new(index),
                span: to_ast_span(combined),
            })
        }
        _ => None,
    }
}
