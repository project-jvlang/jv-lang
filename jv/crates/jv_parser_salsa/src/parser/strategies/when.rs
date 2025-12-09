use crate::lexer::TokenKind;

use super::{control::parse_block, ParserContext, StatementStrategy, SyntaxKind};

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
        parse_block(ctx);
        ctx.finish_node();
        true
    }
}
