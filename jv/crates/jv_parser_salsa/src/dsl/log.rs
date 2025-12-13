use crate::lexer::TokenKind;
use crate::lower::expressions::lower_expression;
use crate::lower::{LoweringContext, LoweringDiagnostic};
use crate::parser::OwnedToken;
use jv_ast::expression::{Expression, LogBlock, LogBlockLevel, LogItem};

/// ログブロックをローワリングする。`tokens` にはキーワードを含める。
pub fn lower_log_block(
    ctx: &mut LoweringContext<'_>,
    tokens: &[OwnedToken],
    depth: usize,
) -> Option<Expression> {
    let first = tokens.first()?;
    let level = log_level(first.kind)?;

    // 先行トリビアを除いたブロック本体を抽出する。
    let mut iter = tokens.iter().enumerate().skip(1);
    let Some((brace_idx, brace_tok)) = iter.find(|(_, tok)| tok.kind == TokenKind::LeftBrace)
    else {
        ctx.push_diagnostic(LoweringDiagnostic::error(
            "JV-DSL-002: LOG ブロックは `{` で開始する必要があります",
            Some(ctx.span_for_token(first)),
        ));
        return Some(Expression::LogBlock(LogBlock {
            level,
            items: Vec::new(),
            span: ctx.span_for_token(first),
        }));
    };

    let mut depth_counter = 1usize;
    let mut body_tokens = Vec::new();
    for tok in tokens.iter().skip(brace_idx + 1) {
        match tok.kind {
            TokenKind::LeftBrace => depth_counter += 1,
            TokenKind::RightBrace => {
                depth_counter = depth_counter.saturating_sub(1);
                if depth_counter == 0 {
                    break;
                }
            }
            _ => {}
        }
        if depth_counter > 0 {
            body_tokens.push(tok.clone());
        }
    }

    if depth_counter != 0 {
        ctx.push_diagnostic(LoweringDiagnostic::error(
            "JV-DSL-002: LOG ブロックは `}` で閉じる必要があります",
            Some(ctx.span_for_token(brace_tok)),
        ));
    }

    let mut items = Vec::new();
    for chunk in split_log_items(&body_tokens) {
        let expr = if chunk
            .first()
            .map(|tok| log_level(tok.kind).is_some())
            .unwrap_or(false)
        {
            lower_log_block(ctx, chunk, depth + 1)
        } else {
            lower_expression(ctx, chunk)
        };

        if let Some(expr) = expr {
            if matches!(expr, Expression::LogBlock(_)) && depth > 0 {
                ctx.push_diagnostic(LoweringDiagnostic::error(
                    "JV-DSL-001: ログブロックのネストは1段までです",
                    Some(expr.span().clone()),
                ));
            }
            match expr {
                Expression::LogBlock(block) => items.push(LogItem::Nested(block)),
                other => items.push(LogItem::Expression(other)),
            }
        }
    }

    let span = ctx.span_for_range(first, tokens.last().unwrap_or(first));

    Some(Expression::LogBlock(LogBlock { level, items, span }))
}

fn log_level(kind: TokenKind) -> Option<LogBlockLevel> {
    match kind {
        TokenKind::Log => Some(LogBlockLevel::Default),
        TokenKind::Trace => Some(LogBlockLevel::Trace),
        TokenKind::Debug => Some(LogBlockLevel::Debug),
        TokenKind::Info => Some(LogBlockLevel::Info),
        TokenKind::Warn => Some(LogBlockLevel::Warn),
        TokenKind::Error => Some(LogBlockLevel::Error),
        _ => None,
    }
}

fn split_log_items(tokens: &[OwnedToken]) -> Vec<&[OwnedToken]> {
    let mut parts = Vec::new();
    let mut start = 0usize;
    for (idx, tok) in tokens.iter().enumerate() {
        if matches!(
            tok.kind,
            TokenKind::Semicolon | TokenKind::Newline | TokenKind::LayoutComma
        ) {
            if start < idx {
                parts.push(&tokens[start..idx]);
            }
            start = idx + 1;
        }
    }
    if start < tokens.len() {
        parts.push(&tokens[start..]);
    }
    parts
}
