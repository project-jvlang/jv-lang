use crate::lexer::TokenKind;

use super::{control::parse_block, ParserContext, StatementStrategy, SyntaxKind};

pub struct TestStrategy;

impl StatementStrategy for TestStrategy {
    fn name(&self) -> &'static str {
        "test"
    }

    fn matches(&self, ctx: &ParserContext, lookahead: TokenKind) -> bool {
        lookahead == TokenKind::Identifier
            && matches!(ctx.current(), Some(tok) if tok.lexeme == "test")
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        ctx.start_node(SyntaxKind::TestDeclaration);
        ctx.bump(); // test
        if ctx.peek_kind() == Some(TokenKind::Identifier) {
            ctx.start_node(SyntaxKind::Identifier);
            ctx.bump();
            ctx.finish_node();
        }
        parse_block(ctx);
        ctx.finish_node();
        true
    }
}
