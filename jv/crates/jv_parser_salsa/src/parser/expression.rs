use crate::lexer::TokenKind;

use super::{OwnedToken, ParserContext, SyntaxKind, recovery::recover_statement};

/// Pratt パーサーのコア実装。
pub fn parse_expression_bp(ctx: &mut ParserContext, min_bp: u8) -> bool {
    if ctx.is_eof() {
        return false;
    }

    ctx.start_node(SyntaxKind::Expression);
    let mut ok = parse_prefix(ctx);
    if !ok {
        recover_statement(ctx);
    }

    loop {
        let op_tok = match ctx.peek(0) {
            Some(tok) => tok.clone(),
            None => break,
        };

        if op_tok.kind == TokenKind::Less && should_parse_type_args(ctx) {
            consume_type_args(ctx);
            continue;
        }

        if let Some((l_bp, r_bp)) = infix_binding_power(&op_tok) {
            if l_bp < min_bp {
                break;
            }
            ctx.bump(); // operator
            ok = parse_expression_bp(ctx, r_bp) && ok;
            continue;
        }

        if is_postfix(&op_tok) {
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
        Some(TokenKind::LeftBracket) => {
            ctx.bump(); // [
            loop {
                ctx.bump_while(|kind| {
                    matches!(
                        kind,
                        TokenKind::Comma | TokenKind::LayoutComma | TokenKind::Newline
                    )
                });
                if ctx.is_eof() || ctx.peek_kind() == Some(TokenKind::RightBracket) {
                    break;
                }
                let before = ctx.cursor;
                let _ = parse_expression_bp(ctx, 0);
                if ctx.cursor == before {
                    break;
                }
            }
            if ctx.peek_kind() == Some(TokenKind::RightBracket) {
                ctx.bump();
            }
            true
        }
        Some(TokenKind::LeftBrace) => {
            consume_brace_block(ctx);
            true
        }
        Some(op) if is_prefix_operator(op) => {
            let (_, r_bp) = prefix_binding_power(op);
            ctx.bump();
            parse_expression_bp(ctx, r_bp)
        }
        _ => {
            ctx.error("式が必要です");
            ctx.bump(); // consume to avoid infinite loop
            false
        }
    }
}

fn parse_postfix(ctx: &mut ParserContext) -> bool {
    let tok = match ctx.peek(0) {
        Some(t) => t.clone(),
        None => return false,
    };

    match tok.kind {
        TokenKind::LeftParen => {
            ctx.bump(); // (
            // 引数リスト
            loop {
                // レイアウトカンマや改行は区切りとして扱う。
                ctx.bump_while(|kind| {
                    matches!(
                        kind,
                        TokenKind::Comma | TokenKind::LayoutComma | TokenKind::Newline
                    )
                });

                if ctx.is_eof() || ctx.peek_kind() == Some(TokenKind::RightParen) {
                    break;
                }

                let before = ctx.cursor;
                let _ = parse_expression_bp(ctx, 0);

                // 進捗がなければ無限ループを避けるために抜ける。
                if ctx.cursor == before {
                    break;
                }
            }
            if ctx.peek_kind() == Some(TokenKind::RightParen) {
                ctx.bump();
            }
            true
        }
        TokenKind::Dot => {
            ctx.bump(); // .
            if ctx.peek_kind() == Some(TokenKind::Identifier) {
                ctx.bump();
            } else {
                ctx.error("メンバ名が必要です");
            }
            true
        }
        TokenKind::NullSafe => {
            ctx.bump(); // ?.
            if ctx.peek_kind() == Some(TokenKind::LeftBracket) {
                ctx.bump(); // [
                let _ = parse_expression_bp(ctx, 0);
                if ctx.peek_kind() == Some(TokenKind::RightBracket) {
                    ctx.bump();
                } else {
                    ctx.error("インデックス式を `]` で閉じてください");
                }
            } else if ctx.peek_kind() == Some(TokenKind::Identifier) {
                ctx.bump();
            } else {
                ctx.error("null 安全アクセスのターゲットが必要です");
            }
            true
        }
        TokenKind::LeftBracket => {
            ctx.bump();
            let _ = parse_expression_bp(ctx, 0);
            if ctx.peek_kind() == Some(TokenKind::RightBracket) {
                ctx.bump();
            } else {
                ctx.error("インデックス式を `]` で閉じてください");
            }
            true
        }
        TokenKind::LeftBrace => {
            consume_brace_block(ctx);
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

fn is_postfix(tok: &OwnedToken) -> bool {
    matches!(
        tok.kind,
        TokenKind::LeftParen
            | TokenKind::Dot
            | TokenKind::NullSafe
            | TokenKind::LeftBracket
            | TokenKind::LeftBrace
    )
}

fn prefix_binding_power(kind: TokenKind) -> (u8, u8) {
    match kind {
        TokenKind::Plus | TokenKind::Minus | TokenKind::Not | TokenKind::Question => (8, 9),
        _ => (0, 0),
    }
}

fn infix_binding_power(op: &OwnedToken) -> Option<(u8, u8)> {
    let power = match op.kind {
        TokenKind::Or => (1, 2),
        TokenKind::And => (2, 3),
        TokenKind::Elvis => (3, 4),
        TokenKind::Equal | TokenKind::NotEqual => (4, 5),
        // NOTE: Rowan 版も is/as を専用 TokenKind にせず lexeme ベースで判定しているため、ここも同等の扱いにする。
        // TODO: TokenKind に is/as を追加して lexeme 依存を排除する。
        TokenKind::Identifier if op.lexeme_eq("is") => (4, 5),
        TokenKind::Identifier if op.lexeme_eq("as") => (4, 5), // equality 相当の優先度
        TokenKind::Less | TokenKind::LessEqual | TokenKind::Greater | TokenKind::GreaterEqual => {
            (5, 6)
        }
        TokenKind::RangeExclusive | TokenKind::RangeInclusive => (5, 6),
        TokenKind::Plus | TokenKind::Minus => (6, 7),
        TokenKind::Multiply | TokenKind::Divide | TokenKind::Modulo => (7, 8),
        _ => return None,
    };
    Some(power)
}

fn should_parse_type_args(ctx: &ParserContext) -> bool {
    let Some(close_idx) = find_matching_angle(ctx) else {
        return false;
    };
    // <T> のように最低1トークンを含む必要がある。
    if close_idx == 0 {
        return false;
    }
    // `>` の次に呼び出しやアクセスが続く場合のみ型引数とみなす。
    matches!(
        ctx.peek(close_idx + 1).map(|tok| tok.kind),
        Some(
            TokenKind::LeftParen
                | TokenKind::Dot
                | TokenKind::LeftBracket
                | TokenKind::LeftBrace
                | TokenKind::Comma
                | TokenKind::RightParen
        )
    )
}

fn find_matching_angle(ctx: &ParserContext) -> Option<usize> {
    let mut depth: isize = 0;
    let mut i = 0;
    while let Some(tok) = ctx.peek(i) {
        match tok.kind {
            TokenKind::Less => {
                depth += 1;
            }
            TokenKind::Greater => {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                } else if depth < 0 {
                    return None;
                }
            }
            TokenKind::Eof | TokenKind::Semicolon => return None,
            _ => {}
        }
        i += 1;
    }
    None
}

fn consume_type_args(ctx: &mut ParserContext) {
    // 前提: peek(0) は `<`
    let mut depth: isize = 0;
    while let Some(kind) = ctx.peek_kind() {
        match kind {
            TokenKind::Less => {
                depth += 1;
                ctx.bump();
            }
            TokenKind::Greater => {
                depth -= 1;
                ctx.bump();
                if depth <= 0 {
                    break;
                }
            }
            TokenKind::Eof => break,
            _ => {
                ctx.bump();
            }
        }
    }
}

fn consume_brace_block(ctx: &mut ParserContext) {
    if ctx.peek_kind() != Some(TokenKind::LeftBrace) {
        return;
    }
    let mut depth: usize = 0;
    ctx.bump(); // {
    while let Some(kind) = ctx.peek_kind() {
        match kind {
            TokenKind::LeftBrace => {
                depth += 1;
                ctx.bump();
            }
            TokenKind::RightBrace => {
                ctx.bump();
                if depth == 0 {
                    break;
                }
                depth = depth.saturating_sub(1);
            }
            TokenKind::Eof => break,
            _ => {
                ctx.bump();
            }
        }
    }
}
