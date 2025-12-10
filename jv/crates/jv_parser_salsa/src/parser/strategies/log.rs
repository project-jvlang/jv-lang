use crate::lexer::TokenKind;

use super::{ParserContext, StatementStrategy, SyntaxKind, control::parse_block};

pub struct LogStrategy;

fn is_log_keyword(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::Log
            | TokenKind::Trace
            | TokenKind::Debug
            | TokenKind::Info
            | TokenKind::Warn
            | TokenKind::Error
    )
}

impl StatementStrategy for LogStrategy {
    fn name(&self) -> &'static str {
        "log-block"
    }

    fn matches(&self, _ctx: &ParserContext, lookahead: TokenKind) -> bool {
        is_log_keyword(lookahead)
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        let Some(kind) = ctx.peek_kind() else {
            return false;
        };
        if !is_log_keyword(kind) {
            return false;
        }

        ctx.start_node(SyntaxKind::LogBlockExpression);
        ctx.bump(); // keyword
        parse_block(ctx);
        ctx.finish_node();
        true
    }
}
