use crate::lexer::TokenKind;

use super::{control::parse_block, ParserContext, StatementStrategy, SyntaxKind};

pub struct ForStrategy;
pub struct WhileStrategy;

impl StatementStrategy for ForStrategy {
    fn name(&self) -> &'static str {
        "for"
    }

    fn matches(&self, _ctx: &ParserContext, lookahead: TokenKind) -> bool {
        lookahead == TokenKind::For
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        ctx.start_node(SyntaxKind::ForStatement);
        ctx.bump(); // for
        // 簡易: in までを消費
        ctx.bump_while(|k| k != TokenKind::In && k != TokenKind::LeftBrace);
        if ctx.peek_kind() == Some(TokenKind::In) {
            ctx.bump();
            let _ = ctx.parse_expression();
        }
        parse_block(ctx);
        ctx.finish_node();
        true
    }
}

impl StatementStrategy for WhileStrategy {
    fn name(&self) -> &'static str {
        "while"
    }

    fn matches(&self, _ctx: &ParserContext, lookahead: TokenKind) -> bool {
        lookahead == TokenKind::While || lookahead == TokenKind::Do
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        ctx.start_node(SyntaxKind::WhileStatement);
        ctx.bump(); // while/do
        let _ = ctx.parse_expression();
        parse_block(ctx);
        ctx.finish_node();
        true
    }
}
