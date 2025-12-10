use crate::lexer::TokenKind;

use super::{ParserContext, StatementStrategy, SyntaxKind, control::parse_block};

pub struct SpawnStrategy;
pub struct AsyncStrategy;

fn matches_ident(ctx: &ParserContext, text: &str) -> bool {
    matches!(ctx.current(), Some(tok) if tok.lexeme == text)
}

impl StatementStrategy for SpawnStrategy {
    fn name(&self) -> &'static str {
        "spawn"
    }

    fn matches(&self, ctx: &ParserContext, lookahead: TokenKind) -> bool {
        lookahead == TokenKind::Identifier && matches_ident(ctx, "spawn")
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        ctx.start_node(SyntaxKind::SpawnStatement);
        ctx.bump(); // spawn
        parse_block(ctx);
        ctx.finish_node();
        true
    }
}

impl StatementStrategy for AsyncStrategy {
    fn name(&self) -> &'static str {
        "async"
    }

    fn matches(&self, ctx: &ParserContext, lookahead: TokenKind) -> bool {
        lookahead == TokenKind::Identifier && matches_ident(ctx, "async")
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        ctx.start_node(SyntaxKind::AsyncStatement);
        ctx.bump(); // async
        parse_block(ctx);
        ctx.finish_node();
        true
    }
}
