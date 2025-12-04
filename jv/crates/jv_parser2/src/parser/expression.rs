//! Pratt 構文解析による式パーサー（簡易版）。

use super::Parser;
use crate::token::TokenKind;
use jv_ast::expression::BinaryMetadata;
use jv_ast::types::{BinaryOp, Literal, UnaryOp};
use jv_ast::{Expression, Pattern, WhenArm};

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
            let then_expr = match parse_precedence(parser, l_bp + 1) {
                Some(expr) => expr,
                None => {
                    let span = parser.current().span;
                    dummy_expr(parser, span)
                }
            };
            if !parser.consume_if(TokenKind::Colon) {
                // colon missing, continue with dummy else
            }
            let else_expr = match parse_precedence(parser, l_bp.saturating_sub(1)) {
                Some(expr) => expr,
                None => {
                    let span = parser.current().span;
                    dummy_expr(parser, span)
                }
            };
            let span = span_of_expr(parser, &left)
                .merge(span_of_expr(parser, &then_expr))
                .merge(span_of_expr(parser, &else_expr));
            left = Expression::If {
                condition: Box::new(left),
                then_branch: Box::new(then_expr),
                else_branch: Some(Box::new(else_expr)),
                span: parser.ast_span(span),
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
        let right =
            parse_precedence(parser, r_bp).unwrap_or_else(|| dummy_expr(parser, token.span));
        let span = span_of_expr(parser, &left)
            .merge(span_of_expr(parser, &right))
            .merge(token.span);
        left = Expression::Binary {
            left: Box::new(left),
            op,
            right: Box::new(right),
            span: parser.ast_span(span),
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
            Some(Expression::Identifier(name, parser.ast_span(token.span)))
        }
        TokenKind::Number => {
            parser.advance();
            let text = parser
                .lexeme(token.span)
                .map(|s| s.to_string())
                .unwrap_or_else(|| "0".to_string());
            Some(Expression::Literal(
                Literal::Number(text),
                parser.ast_span(token.span),
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
                parser.ast_span(token.span),
            ))
        }
        TokenKind::TrueKw => {
            parser.advance();
            Some(Expression::Literal(
                Literal::Boolean(true),
                parser.ast_span(token.span),
            ))
        }
        TokenKind::FalseKw => {
            parser.advance();
            Some(Expression::Literal(
                Literal::Boolean(false),
                parser.ast_span(token.span),
            ))
        }
        TokenKind::NullKw => {
            parser.advance();
            Some(Expression::Literal(
                Literal::Null,
                parser.ast_span(token.span),
            ))
        }
        TokenKind::When => Some(parse_when_expression(parser)),
        TokenKind::LeftParen => {
            parser.advance();
            let expr = parse_expression(parser).unwrap_or_else(|| dummy_expr(parser, token.span));
            let _ = parser.consume_if(TokenKind::RightParen);
            Some(expr)
        }
        TokenKind::Minus => {
            parser.advance();
            let rhs =
                parse_precedence(parser, 90).unwrap_or_else(|| dummy_expr(parser, token.span));
            Some(Expression::Unary {
                op: UnaryOp::Minus,
                operand: Box::new(rhs),
                span: parser.ast_span(token.span),
            })
        }
        TokenKind::Plus => {
            parser.advance();
            let rhs =
                parse_precedence(parser, 90).unwrap_or_else(|| dummy_expr(parser, token.span));
            Some(Expression::Unary {
                op: UnaryOp::Plus,
                operand: Box::new(rhs),
                span: parser.ast_span(token.span),
            })
        }
        TokenKind::Not => {
            parser.advance();
            let rhs =
                parse_precedence(parser, 90).unwrap_or_else(|| dummy_expr(parser, token.span));
            Some(Expression::Unary {
                op: UnaryOp::Not,
                operand: Box::new(rhs),
                span: parser.ast_span(token.span),
            })
        }
        TokenKind::LeftBrace => parse_block_expression(parser),
        _ => {
            // 不明なプレフィックスはダミー式で継続。
            parser.advance();
            Some(dummy_expr(parser, token.span))
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

fn dummy_expr<'src, 'alloc>(parser: &Parser<'src, 'alloc>, span: crate::span::Span) -> Expression {
    Expression::Identifier("_".into(), parser.ast_span(span))
}

pub(crate) fn span_of_expr(parser: &Parser<'_, '_>, expr: &Expression) -> crate::span::Span {
    match expr {
        Expression::Identifier(_, span)
        | Expression::Literal(_, span)
        | Expression::Unary { span, .. }
        | Expression::Binary { span, .. } => parser.span_from_ast(span),
        Expression::MemberAccess { span, .. }
        | Expression::NullSafeMemberAccess { span, .. }
        | Expression::Call { span, .. }
        | Expression::IndexAccess { span, .. }
        | Expression::NullSafeIndexAccess { span, .. } => parser.span_from_ast(span),
        _ => crate::span::Span::new(0, 0),
    }
}

fn parse_when_expression<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) -> Expression {
    let when_span = parser.advance().span;
    let subject = if parser.consume_if(TokenKind::LeftParen) {
        let expr = parse_expression(parser);
        let _ = parser.consume_if(TokenKind::RightParen);
        expr.map(Box::new)
    } else {
        None
    };

    let mut arms = Vec::new();
    let mut else_arm = None;
    let mut end_span = when_span;

    if parser.consume_if(TokenKind::LeftBrace) {
        loop {
            let token = parser.current();
            if token.kind == TokenKind::RightBrace || token.kind == TokenKind::Eof {
                end_span = token.span;
                if token.kind == TokenKind::RightBrace {
                    parser.advance();
                }
                break;
            }

            if token.kind == TokenKind::Else {
                parser.advance();
                let _ = parser.consume_if(TokenKind::FatArrow) || parser.consume_if(TokenKind::Arrow);
                let body =
                    parse_expression(parser).unwrap_or_else(|| dummy_expr(parser, token.span));
                else_arm = Some(Box::new(body));
            } else if let Some(arm) = parse_when_arm(parser) {
                arms.push(arm);
            } else {
                // 進まないと無限ループになるので一つ消費する。
                parser.advance();
            }

            let _ = parser.consume_if(TokenKind::Comma);
        }
    }

    let span = parser.ast_span(when_span.merge(end_span));
    Expression::When {
        expr: subject,
        arms,
        else_arm,
        implicit_end: None,
        span,
    }
}

fn parse_when_arm<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) -> Option<WhenArm> {
    let pattern = parse_when_pattern(parser)?;

    if !(parser.consume_if(TokenKind::FatArrow) || parser.consume_if(TokenKind::Arrow)) {
        // 矢印が無ければ不正な when arm として継続。
    }

    let fallback_span = pattern_span(parser, &pattern);
    let body = parse_expression(parser).unwrap_or_else(|| dummy_expr(parser, fallback_span));

    let pattern_span = pattern_span(parser, &pattern);
    let arm_span = parser
        .ast_span(pattern_span.merge(span_of_expr(parser, &body)));
    Some(WhenArm {
        pattern,
        guard: None,
        body,
        span: arm_span,
    })
}

fn parse_when_pattern<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) -> Option<Pattern> {
    let token = parser.current();
    match token.kind {
        TokenKind::Identifier => {
            let name = parser
                .lexeme(token.span)
                .map(|s| s.to_string())
                .unwrap_or_else(|| format!("_id{}", token.span.start));
            parser.advance();

            if name == "is" {
                let type_token = parser.current();
                if type_token.kind == TokenKind::Identifier {
                    let ty = parser
                        .lexeme(type_token.span)
                        .map(|s| s.to_string())
                        .unwrap_or_else(|| format!("_id{}", type_token.span.start));
                    let span = token.span.merge(type_token.span);
                    parser.advance();
                    Some(Pattern::Constructor {
                        name: ty,
                        patterns: Vec::new(),
                        span: parser.ast_span(span),
                    })
                } else {
                    Some(Pattern::Identifier(name, parser.ast_span(token.span)))
                }
            } else {
                Some(Pattern::Identifier(name, parser.ast_span(token.span)))
            }
        }
        TokenKind::Underscore => {
            parser.advance();
            Some(Pattern::Wildcard(parser.ast_span(token.span)))
        }
        TokenKind::Number => {
            let text = parser
                .lexeme(token.span)
                .map(|s| s.to_string())
                .unwrap_or_else(|| "0".to_string());
            parser.advance();
            Some(Pattern::Literal(
                Literal::Number(text),
                parser.ast_span(token.span),
            ))
        }
        TokenKind::String => {
            let text = parser
                .lexeme(token.span)
                .map(|s| s.to_string())
                .unwrap_or_default();
            parser.advance();
            Some(Pattern::Literal(
                Literal::String(text),
                parser.ast_span(token.span),
            ))
        }
        TokenKind::TrueKw | TokenKind::FalseKw => {
            parser.advance();
            let value = token.kind == TokenKind::TrueKw;
            Some(Pattern::Literal(
                Literal::Boolean(value),
                parser.ast_span(token.span),
            ))
        }
        TokenKind::NullKw => {
            parser.advance();
            Some(Pattern::Literal(
                Literal::Null,
                parser.ast_span(token.span),
            ))
        }
        _ => None,
    }
}

fn pattern_span(parser: &Parser<'_, '_>, pattern: &Pattern) -> crate::span::Span {
    match pattern {
        Pattern::Literal(_, span)
        | Pattern::Identifier(_, span)
        | Pattern::Wildcard(span) => parser.span_from_ast(span),
        Pattern::Constructor { span, .. } | Pattern::Range { span, .. } | Pattern::Guard { span, .. } => {
            parser.span_from_ast(span)
        }
    }
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
            let combined = span_of_expr(parser, left)
                .merge(token.span)
                .merge(ident_token.span);
            let span = parser.ast_span(combined);
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
            let mut close_span = open;
            if !parser.consume_if(TokenKind::RightParen) {
                loop {
                    let arg = parse_expression(parser).unwrap_or_else(|| dummy_expr(parser, open));
                    last_span = span_of_expr(parser, &arg);
                    args.push(jv_ast::expression::Argument::Positional(arg));
                    if parser.consume_if(TokenKind::Comma) {
                        continue;
                    }
                    if parser.current().kind == TokenKind::RightParen {
                        close_span = parser.current().span;
                        parser.advance();
                    }
                    break;
                }
            } else {
                // consume_if で進んだので current は次トークン。閉じ括弧は open に隣接する位置。
                close_span = open;
            }
            let combined = span_of_expr(parser, left)
                .merge(open)
                .merge(last_span)
                .merge(close_span);
            Some(Expression::Call {
                function: Box::new(left.clone()),
                args,
                type_arguments: Vec::new(),
                argument_metadata: Default::default(),
                span: parser.ast_span(combined),
            })
        }
        TokenKind::LeftBracket => {
            let open = parser.advance().span;
            let index = parse_expression(parser).unwrap_or_else(|| dummy_expr(parser, open));
            let mut close_span = open;
            if parser.current().kind == TokenKind::RightBracket {
                close_span = parser.current().span;
                parser.advance();
            }
            let combined = span_of_expr(parser, left)
                .merge(open)
                .merge(span_of_expr(parser, &index))
                .merge(close_span);
            Some(Expression::IndexAccess {
                object: Box::new(left.clone()),
                index: Box::new(index),
                span: parser.ast_span(combined),
            })
        }
        _ => None,
    }
}

fn parse_block_expression<'src, 'alloc>(
    parser: &mut Parser<'src, 'alloc>,
) -> Option<Expression> {
    let open_span = parser.advance().span;
    let mut depth = 1usize;
    let mut last_span = open_span;

    while depth > 0 {
        let tok = parser.advance();
        last_span = tok.span;
        match tok.kind {
            TokenKind::LeftBrace => depth += 1,
            TokenKind::RightBrace => depth = depth.saturating_sub(1),
            TokenKind::Eof => break,
            _ => {}
        }
    }

    let span = parser.ast_span(open_span.merge(last_span));
    Some(Expression::Block {
        statements: Vec::new(),
        span,
    })
}
