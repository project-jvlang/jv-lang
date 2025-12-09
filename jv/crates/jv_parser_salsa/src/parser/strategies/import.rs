use crate::lexer::TokenKind;

use super::{ParserContext, StatementStrategy, SyntaxKind};

pub struct ImportStrategy;

impl StatementStrategy for ImportStrategy {
    fn name(&self) -> &'static str {
        "import"
    }

    fn matches(&self, _ctx: &ParserContext, lookahead: TokenKind) -> bool {
        lookahead == TokenKind::Import
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        ctx.start_node(SyntaxKind::ImportDeclaration);
        ctx.bump(); // import
        ctx.start_node(SyntaxKind::Identifier);
        ctx.bump(); // first
        ctx.finish_node();
        while ctx.peek_kind() == Some(TokenKind::Dot) {
            ctx.bump();
            ctx.start_node(SyntaxKind::Identifier);
            ctx.bump();
            ctx.finish_node();
        }
        if ctx.peek_kind() == Some(TokenKind::Multiply) {
            ctx.bump();
        }
        ctx.finish_node();
        true
    }
}
