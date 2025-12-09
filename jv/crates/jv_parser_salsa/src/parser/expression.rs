use crate::lexer::TokenKind;

use super::{ParserContext, SyntaxKind};

/// Pratt パーサーのコア実装。
pub fn parse_expression_bp(ctx: &mut ParserContext, min_bp: u8) -> bool {
    if ctx.is_eof() {
        return false;
    }

    ctx.start_node(SyntaxKind::Expression);
    let mut ok = parse_prefix(ctx);

    loop {
        let op = match ctx.peek_kind() {
            Some(kind) => kind,
            None => break,
        };

        if let Some((l_bp, r_bp)) = infix_binding_power(op) {
            if l_bp < min_bp {
                break;
            }
            ctx.bump(); // operator
            ok = parse_expression_bp(ctx, r_bp) && ok;
            continue;
        }

        if is_postfix(op) {
            ok = parse_postfix(ctx) && ok;
            continue;
        }

        break;
    }

    ctx.finish_node();
    ok
}

fn parse_prefix(ctx: &mut ParserContext) -> bool {
    match ctx.peek_kind() {
        Some(kind) if is_literal(kind) => {
            ctx.bump();
            true
        }
        Some(TokenKind::Identifier) => {
            ctx.bump();
            true
        }
        Some(TokenKind::LeftParen) => {
            ctx.bump();
            let _ = parse_expression_bp(ctx, 0);
            if ctx.peek_kind() == Some(TokenKind::RightParen) {
                ctx.bump();
            }
            true
        }
        Some(op) if is_prefix_operator(op) => {
            let (_, r_bp) = prefix_binding_power(op);
            ctx.bump();
            parse_expression_bp(ctx, r_bp)
        }
        _ => false,
    }
}

fn parse_postfix(ctx: &mut ParserContext) -> bool {
    match ctx.peek_kind() {
        Some(TokenKind::LeftParen) => {
            ctx.bump(); // (
            // 引数リスト
            while !ctx.is_eof() && ctx.peek_kind() != Some(TokenKind::RightParen) {
                let _ = parse_expression_bp(ctx, 0);
                if ctx.peek_kind() == Some(TokenKind::Comma) {
                    ctx.bump();
                } else {
                    break;
                }
            }
            if ctx.peek_kind() == Some(TokenKind::RightParen) {
                ctx.bump();
            }
            true
        }
        Some(TokenKind::Dot) | Some(TokenKind::NullSafe) => {
            ctx.bump(); // . or ?.
            if ctx.peek_kind() == Some(TokenKind::Identifier) {
                ctx.bump();
            }
            true
        }
        Some(TokenKind::LeftBracket) => {
            ctx.bump();
            let _ = parse_expression_bp(ctx, 0);
            if ctx.peek_kind() == Some(TokenKind::RightBracket) {
                ctx.bump();
            }
            true
        }
        _ => false,
    }
}

fn is_literal(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::Number
            | TokenKind::String
            | TokenKind::BooleanTrue
            | TokenKind::BooleanFalse
            | TokenKind::Null
            | TokenKind::RegexLiteral
            | TokenKind::Character
            | TokenKind::StringInterpolation
    )
}

fn is_prefix_operator(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::Plus | TokenKind::Minus | TokenKind::Not | TokenKind::Question
    )
}

fn is_postfix(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::LeftParen | TokenKind::Dot | TokenKind::NullSafe | TokenKind::LeftBracket
    )
}

fn prefix_binding_power(kind: TokenKind) -> (u8, u8) {
    match kind {
        TokenKind::Plus | TokenKind::Minus | TokenKind::Not | TokenKind::Question => (8, 9),
        _ => (0, 0),
    }
}

fn infix_binding_power(op: TokenKind) -> Option<(u8, u8)> {
    let power = match op {
        TokenKind::Or => (1, 2),
        TokenKind::And => (2, 3),
        TokenKind::Equal | TokenKind::NotEqual => (3, 4),
        TokenKind::Less | TokenKind::LessEqual | TokenKind::Greater | TokenKind::GreaterEqual => {
            (4, 5)
        }
        TokenKind::RangeExclusive | TokenKind::RangeInclusive => (5, 6),
        TokenKind::Plus | TokenKind::Minus => (6, 7),
        TokenKind::Multiply | TokenKind::Divide | TokenKind::Modulo => (7, 8),
        _ => return None,
    };
    Some(power)
}
