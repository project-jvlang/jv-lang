use crate::lexer::TokenKind;
use crate::parser::recovery::recover_statement;

use super::{ParserContext, StatementStrategy};

pub struct IfStrategy;

impl StatementStrategy for IfStrategy {
    fn name(&self) -> &'static str {
        "if"
    }

    fn matches(&self, _ctx: &ParserContext, lookahead: TokenKind) -> bool {
        lookahead == TokenKind::If
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        let message = "JV3103: `if` expressions are not supported / `if` 式はサポートされていません。\n条件分岐は `when` 式を使用してください。Quick Fix: when.convert.if. / Use a `when` expression for branching. Quick Fix: when.convert.if. (--explain JV3103)";
        ctx.error(message);
        recover_statement(ctx);
        true
    }
}

pub fn parse_block(ctx: &mut ParserContext) {
    if !ctx.parse_block() {
        // 単文形式
        let _ = ctx.parse_expression();
    }
}
