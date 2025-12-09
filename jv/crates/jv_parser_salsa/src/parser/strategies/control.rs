use crate::lexer::TokenKind;

use super::{ParserContext, StatementStrategy, SyntaxKind};

pub struct IfStrategy;

impl StatementStrategy for IfStrategy {
    fn name(&self) -> &'static str {
        "if"
    }

    fn matches(&self, _ctx: &ParserContext, lookahead: TokenKind) -> bool {
        lookahead == TokenKind::If
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        ctx.start_node(SyntaxKind::IfStatement);
        ctx.bump(); // if
        let _ = ctx.parse_expression();
        parse_block(ctx);
        if ctx.peek_kind() == Some(TokenKind::Else) {
            ctx.bump();
            if ctx.peek_kind() == Some(TokenKind::If) {
                let _ = self.parse(ctx);
            } else {
                parse_block(ctx);
            }
        }
        ctx.finish_node();
        true
    }
}

pub fn parse_block(ctx: &mut ParserContext) {
    if ctx.peek_kind() == Some(TokenKind::LeftBrace) {
        ctx.start_node(SyntaxKind::Block);
        // 先頭の `{` を消費
        ctx.bump();
        let mut depth = 0usize;
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
                    depth -= 1;
                }
                _ => {
                    if !ctx.bump() {
                        break;
                    }
                }
            }
        }
        ctx.finish_node();
    } else {
        // 単文形式
        let _ = ctx.parse_expression();
    }
}
