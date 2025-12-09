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
    if !ctx.parse_block() {
        // 単文形式
        let _ = ctx.parse_expression();
    }
}
