use crate::lexer::TokenKind;

use super::{ParserContext, StatementStrategy, SyntaxKind, control::parse_block};

pub struct UseStrategy;
pub struct DeferStrategy;

fn is_keyword(ctx: &ParserContext, kw: &str) -> bool {
    matches!(ctx.current(), Some(tok) if tok.lexeme == kw)
}

impl StatementStrategy for UseStrategy {
    fn name(&self) -> &'static str {
        "use"
    }

    fn matches(&self, ctx: &ParserContext, lookahead: TokenKind) -> bool {
        lookahead == TokenKind::Identifier && is_keyword(ctx, "use")
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        ctx.start_node(SyntaxKind::UseStatement);
        ctx.bump(); // use
        parse_block(ctx);
        ctx.finish_node();
        true
    }
}

impl StatementStrategy for DeferStrategy {
    fn name(&self) -> &'static str {
        "defer"
    }

    fn matches(&self, ctx: &ParserContext, lookahead: TokenKind) -> bool {
        lookahead == TokenKind::Identifier && is_keyword(ctx, "defer")
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        ctx.start_node(SyntaxKind::DeferStatement);
        ctx.bump(); // defer
        parse_block(ctx);
        ctx.finish_node();
        true
    }
}
