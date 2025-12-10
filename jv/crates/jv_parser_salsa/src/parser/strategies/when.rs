use crate::lexer::TokenKind;

use super::{ParserContext, StatementStrategy, SyntaxKind, control::parse_block};

pub struct WhenStrategy;

impl StatementStrategy for WhenStrategy {
    fn name(&self) -> &'static str {
        "when"
    }

    fn matches(&self, _ctx: &ParserContext, lookahead: TokenKind) -> bool {
        lookahead == TokenKind::When
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        ctx.start_node(SyntaxKind::WhenStatement);
        ctx.bump(); // when
        let _ = ctx.parse_expression();
        if ctx.peek_kind() == Some(TokenKind::LeftBrace) {
            ctx.bump(); // {
            while !ctx.is_eof() && ctx.peek_kind() != Some(TokenKind::RightBrace) {
                parse_branch(ctx);
            }
            if ctx.peek_kind() == Some(TokenKind::RightBrace) {
                ctx.bump();
            } else {
                ctx.error("when ブロックが `}` で閉じていません");
            }
        } else {
            ctx.error("when ブロックの `{` が必要です");
        }
        ctx.finish_node();
        true
    }
}

fn parse_branch(ctx: &mut ParserContext) {
    ctx.start_node(SyntaxKind::WhenBranch);
    if ctx.peek_kind() == Some(TokenKind::Else) {
        ctx.bump();
    } else {
        // 旧パーサー同様、`->` に到達するまで式としてパターンを読む。
        while !ctx.is_eof()
            && !matches!(
                ctx.peek_kind(),
                Some(TokenKind::Arrow) | Some(TokenKind::FatArrow)
            )
        {
            let progressed = ctx.parse_expression();
            if !progressed {
                ctx.error("when ブランチのパターンが必要です");
                ctx.bump();
                break;
            }
            if ctx.peek_kind() == Some(TokenKind::Comma) {
                ctx.bump();
                continue;
            }
        }
    }
    if ctx.peek_kind() == Some(TokenKind::Arrow) || ctx.peek_kind() == Some(TokenKind::FatArrow) {
        ctx.bump();
    } else {
        ctx.error("when ブランチに `->` が必要です");
        ctx.bump(); // 少なくとも1トークン進める
    }
    // ブランチ本体: 単式またはブロック
    if ctx.peek_kind() == Some(TokenKind::LeftBrace) {
        parse_block(ctx);
    } else {
        let _ = ctx.parse_expression();
    }
    ctx.finish_node();
}
