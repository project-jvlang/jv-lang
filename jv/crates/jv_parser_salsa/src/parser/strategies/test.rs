use crate::lexer::TokenKind;

use super::{ParserContext, StatementStrategy, SyntaxKind, control::parse_block};

pub struct TestStrategy;

impl StatementStrategy for TestStrategy {
    fn name(&self) -> &'static str {
        "test"
    }

    fn matches(&self, ctx: &ParserContext, lookahead: TokenKind) -> bool {
        lookahead == TokenKind::Identifier
            && matches!(ctx.current(), Some(tok) if tok.lexeme_eq("test"))
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        ctx.start_node(SyntaxKind::TestDeclaration);
        ctx.bump(); // test
        // Name / dataset / parameter list are lowered from tokens; here we only need to
        // consume them up to the `{ ... }` block to avoid syntax errors.
        while let Some(kind) = ctx.peek_kind() {
            if matches!(kind, TokenKind::LeftBrace | TokenKind::Eof) {
                break;
            }
            ctx.bump();
        }
        if ctx.peek_kind() == Some(TokenKind::LeftBrace) {
            parse_block(ctx);
        } else {
            ctx.error("テスト本体の `{}` ブロックが必要です");
        }
        ctx.finish_node();
        true
    }
}
