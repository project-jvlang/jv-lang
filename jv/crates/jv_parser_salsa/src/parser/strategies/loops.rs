use crate::lexer::TokenKind;
use crate::parser::recovery::recover_statement;

use super::{ParserContext, StatementStrategy, SyntaxKind, control::parse_block};

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
        let message = "E_LOOP_001: `while`/`do-while` loops have been removed from the language / `while`/`do-while` ループはサポートされていません。\n`for (item in ...)` ループへ書き換えてください。/ Replace legacy loops with `for (item in ...)`. (--explain E_LOOP_001)";
        ctx.error(message);
        recover_statement(ctx);
        true
    }
}
