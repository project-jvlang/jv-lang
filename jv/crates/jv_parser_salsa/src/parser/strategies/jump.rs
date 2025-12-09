use crate::lexer::TokenKind;

use super::{ParserContext, StatementStrategy, SyntaxKind};

pub struct ReturnStrategy;
pub struct BreakStrategy;
pub struct ContinueStrategy;

impl StatementStrategy for ReturnStrategy {
    fn name(&self) -> &'static str {
        "return"
    }

    fn matches(&self, _ctx: &ParserContext, lookahead: TokenKind) -> bool {
        matches!(lookahead, TokenKind::Return | TokenKind::Throw)
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        ctx.start_node(SyntaxKind::ReturnStatement);
        ctx.bump(); // return/throw
        // optional expression
        if !matches!(
            ctx.peek_kind(),
            None | Some(TokenKind::Semicolon | TokenKind::Newline | TokenKind::RightBrace)
        ) {
            let _ = ctx.parse_expression();
        }
        ctx.finish_node();
        true
    }
}

impl StatementStrategy for BreakStrategy {
    fn name(&self) -> &'static str {
        "break"
    }

    fn matches(&self, _ctx: &ParserContext, lookahead: TokenKind) -> bool {
        lookahead == TokenKind::Break
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        ctx.start_node(SyntaxKind::BreakStatement);
        ctx.bump();
        ctx.finish_node();
        true
    }
}

impl StatementStrategy for ContinueStrategy {
    fn name(&self) -> &'static str {
        "continue"
    }

    fn matches(&self, _ctx: &ParserContext, lookahead: TokenKind) -> bool {
        lookahead == TokenKind::Continue
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        ctx.start_node(SyntaxKind::ContinueStatement);
        ctx.bump();
        ctx.finish_node();
        true
    }
}
