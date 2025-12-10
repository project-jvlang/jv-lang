use crate::lexer::TokenKind;

use super::{ParserContext, StatementStrategy, SyntaxKind, control::parse_block};

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
        if matches!(
            ctx.peek_kind(),
            Some(TokenKind::Identifier | TokenKind::String)
        ) {
            ctx.bump();
        }
        if matches!(
            ctx.current(),
            Some(tok) if tok.lexeme.eq_ignore_ascii_case("dataset")
        ) {
            ctx.bump(); // dataset
            if matches!(
                ctx.peek_kind(),
                Some(TokenKind::Identifier | TokenKind::String)
            ) {
                ctx.bump();
            }
        }
        parse_block(ctx);
        ctx.finish_node();
        true
    }
}
