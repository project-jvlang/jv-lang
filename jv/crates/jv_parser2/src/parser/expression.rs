//! Pratt 構文解析による式パーサー（簡易版）。

use super::{Parser, statement::parse_statement};
use crate::token::TokenKind;
use crate::parser::statement::parse_identifier;
use jv_ast::json::{JsonEntry, JsonLiteral, JsonValue};
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
        TokenKind::LeftBrace => parse_json_or_block(parser),
        TokenKind::LeftBracket => parse_array_literal(parser),
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
            let mut has_comma = false;
            let mut first_comma_span: Option<crate::span::Span> = None;
            if !parser.consume_if(TokenKind::RightParen) {
                loop {
                    let arg = parse_expression(parser).unwrap_or_else(|| dummy_expr(parser, open));
                    last_span = span_of_expr(parser, &arg);
                    args.push(jv_ast::expression::Argument::Positional(arg));
                    if parser.current().kind == TokenKind::Comma {
                        if !has_comma {
                            first_comma_span = Some(parser.current().span);
                        }
                        has_comma = true;
                        parser.advance();
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

            // Emit JV2102 diagnostic if commas were used in function call arguments
            if has_comma {
                let diagnostic_span = first_comma_span.unwrap_or(open);
                parser.push_diagnostic(crate::diagnostics::Diagnostic::new(
                    "JV2102: 関数呼び出しでカンマ区切りはサポートされません。位置引数は空白または改行で区切ってください。\nJV2102: Function calls do not support comma separators. Separate positional arguments with whitespace or newlines.",
                    diagnostic_span,
                ));
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
    let mut statements = Vec::new();

    while parser.current().kind != TokenKind::RightBrace && parser.current().kind != TokenKind::Eof {
        match parse_statement(parser) {
            Some(stmt) => statements.push(stmt),
            None => {
                parser.advance();
            }
        }
    }

    let end_span = if parser.current().kind == TokenKind::RightBrace {
        parser.advance().span
    } else {
        open_span
    };

    let span = parser.ast_span(open_span.merge(end_span));
    Some(Expression::Block { statements, span })
}

fn parse_json_or_block<'src, 'alloc>(
    parser: &mut Parser<'src, 'alloc>,
) -> Option<Expression> {
    // Peek ahead: if { is followed by a statement keyword, it's a block, not JSON.
    // This prevents misinterpreting `{ val x = ... }` as a JSON object with key "val".
    let next_token = parser.peek_next().map(|t| t.kind);
    if matches!(
        next_token,
        Some(
            TokenKind::Val
                | TokenKind::Var
                | TokenKind::Fun
                | TokenKind::Class
                | TokenKind::Data
                | TokenKind::If
                | TokenKind::For
                | TokenKind::While
                | TokenKind::Return
                | TokenKind::Throw
                | TokenKind::Break
                | TokenKind::Continue
                | TokenKind::When
                | TokenKind::Test
        )
    ) {
        return parse_block_expression(parser);
    }

    let checkpoint = parser.checkpoint();
    if let Some(json) = parse_json_value(parser) {
        let span = match &json {
            JsonValue::Object { span, .. } | JsonValue::Array { span, .. } | JsonValue::String { span, .. } | JsonValue::Number { span, .. } | JsonValue::Boolean { span, .. } | JsonValue::Null { span, .. } => span.clone(),
        };
        return Some(Expression::JsonLiteral(JsonLiteral {
            value: json,
            leading_comments: Vec::new(),
            trailing_comments: Vec::new(),
            span,
            inferred_schema: None,
        }));
    }
    parser.rewind(checkpoint);
    parse_block_expression(parser)
}

fn parse_array_literal<'src, 'alloc>(
    parser: &mut Parser<'src, 'alloc>,
) -> Option<Expression> {
    let checkpoint = parser.checkpoint();
    if let Some(JsonValue::Array { elements, delimiter, span }) = parse_json_array(parser) {
        return Some(Expression::JsonLiteral(JsonLiteral {
            value: JsonValue::Array {
                elements,
                delimiter,
                span: span.clone(),
            },
            leading_comments: Vec::new(),
            trailing_comments: Vec::new(),
            span,
            inferred_schema: None,
        }));
    }
    parser.rewind(checkpoint);
    None
}

fn parse_json_value<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) -> Option<JsonValue> {
    let token = parser.current();
    match token.kind {
        TokenKind::LeftBrace => parse_json_object(parser),
        TokenKind::LeftBracket => parse_json_array(parser),
        TokenKind::String => {
            let lexeme = parser.lexeme(token.span).unwrap_or("");
            let value = lexeme.trim_matches('"').to_string();
            parser.advance();
            Some(JsonValue::String {
                value,
                span: parser.ast_span(token.span),
            })
        }
        TokenKind::Number => {
            let lexeme = parser.lexeme(token.span).unwrap_or("0").to_string();
            parser.advance();
            Some(JsonValue::Number {
                literal: lexeme,
                grouping: Default::default(),
                span: parser.ast_span(token.span),
            })
        }
        TokenKind::TrueKw | TokenKind::FalseKw => {
            parser.advance();
            Some(JsonValue::Boolean {
                value: token.kind == TokenKind::TrueKw,
                span: parser.ast_span(token.span),
            })
        }
        TokenKind::NullKw => {
            parser.advance();
            Some(JsonValue::Null {
                span: parser.ast_span(token.span),
            })
        }
        _ => None,
    }
}

fn parse_json_object<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) -> Option<JsonValue> {
    let start = parser.advance().span;
    let mut entries = Vec::new();
    let mut end_span = start;

    while parser.current().kind != TokenKind::RightBrace && parser.current().kind != TokenKind::Eof
    {
        // Key
        let token = parser.current();
        let key = match token.kind {
            TokenKind::String | TokenKind::Identifier => {
                let lexeme = parser.lexeme(token.span).unwrap_or("");
                parser.advance();
                lexeme.trim_matches('"').to_string()
            }
            _ => {
                parser.advance();
                continue;
            }
        };

        let key_span = token.span;

        // Optional colon
        let _ = parser.consume_if(TokenKind::Colon);

        let value = parse_json_value(parser).unwrap_or(JsonValue::Null {
            span: parser.ast_span(key_span),
        });

        let value_span = match &value {
            JsonValue::Object { span, .. }
            | JsonValue::Array { span, .. }
            | JsonValue::String { span, .. }
            | JsonValue::Number { span, .. }
            | JsonValue::Boolean { span, .. }
            | JsonValue::Null { span, .. } => span.clone(),
        };

        let entry_span = parser.ast_span(key_span.merge(parser.span_from_ast(&value_span)));
        entries.push(JsonEntry {
            key,
            comments: Vec::new(),
            value,
            span: entry_span.clone(),
        });
        end_span = parser.span_from_ast(&entry_span);

        let _ = parser.consume_if(TokenKind::Comma);
    }

    if parser.current().kind == TokenKind::RightBrace {
        end_span = parser.current().span;
        parser.advance();
    }

    Some(JsonValue::Object {
        entries,
        span: parser.ast_span(start.merge(end_span)),
    })
}

fn parse_json_array<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) -> Option<JsonValue> {
    let start = parser.advance().span;
    let mut elements = Vec::new();
    let mut end_span = start;
    let mut has_comma = false;
    let mut first_comma_span: Option<crate::span::Span> = None;

    while parser.current().kind != TokenKind::RightBracket && parser.current().kind != TokenKind::Eof
    {
        if let Some(value) = parse_json_value(parser) {
            end_span = match &value {
                JsonValue::Object { span, .. }
                | JsonValue::Array { span, .. }
                | JsonValue::String { span, .. }
                | JsonValue::Number { span, .. }
                | JsonValue::Boolean { span, .. }
                | JsonValue::Null { span, .. } => parser.span_from_ast(span),
            };
            elements.push(value);
        } else {
            parser.advance();
        }

        if parser.current().kind == TokenKind::Comma {
            if !has_comma {
                first_comma_span = Some(parser.current().span);
            }
            has_comma = true;
            parser.advance();
            continue;
        }
    }

    if parser.current().kind == TokenKind::RightBracket {
        end_span = parser.current().span;
        parser.advance();
    }

    // Emit JV2101 diagnostic if commas were used in array literal
    if has_comma {
        let diagnostic_span = first_comma_span.unwrap_or(start);
        parser.push_diagnostic(crate::diagnostics::Diagnostic::new(
            "JV2101: 配列リテラルでカンマ区切りはサポートされません。空白または改行のみで要素を分けてください。\nJV2101: Array literals do not support comma separators. Use whitespace or newlines between elements.",
            diagnostic_span,
        ));
    }

    Some(JsonValue::Array {
        elements,
        delimiter: Default::default(),
        span: parser.ast_span(start.merge(end_span)),
    })
}
