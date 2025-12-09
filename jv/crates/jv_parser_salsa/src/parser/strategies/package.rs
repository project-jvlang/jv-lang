use crate::lexer::TokenKind;

use super::{ParserContext, StatementStrategy, SyntaxKind};

pub struct PackageStrategy;

impl StatementStrategy for PackageStrategy {
    fn name(&self) -> &'static str {
        "package"
    }

    fn matches(&self, _ctx: &ParserContext, lookahead: TokenKind) -> bool {
        lookahead == TokenKind::Package
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        ctx.start_node(SyntaxKind::PackageDeclaration);
        ctx.bump(); // package
        ctx.start_node(SyntaxKind::Identifier);
        ctx.bump(); // first segment
        ctx.finish_node();
        while ctx.peek_kind() == Some(TokenKind::Dot) {
            ctx.bump(); // dot
            ctx.start_node(SyntaxKind::Identifier);
            ctx.bump(); // segment
            ctx.finish_node();
        }
        ctx.finish_node();
        true
    }
}
