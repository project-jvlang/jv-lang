use crate::lexer::TokenKind;

use super::{ParserContext, SyntaxKind};

/// ステートメント同期用トークン集合。
pub const SYNC_TOKENS: &[TokenKind] = &[
    TokenKind::Semicolon,
    TokenKind::Newline,
    TokenKind::RightBrace,
    TokenKind::Package,
    TokenKind::Import,
    TokenKind::Val,
    TokenKind::Var,
    TokenKind::Fun,
    TokenKind::Class,
    TokenKind::If,
    TokenKind::When,
    TokenKind::For,
    TokenKind::Return,
    TokenKind::Break,
    TokenKind::Continue,
    TokenKind::Eof,
];

/// SYNC_TOKENS まで読み飛ばしてリカバリする。
pub fn recover_statement(ctx: &mut ParserContext) {
    ctx.recovered = true;
    ctx.start_node(SyntaxKind::Error);
    ctx.bump(); // consume unexpected
    while let Some(kind) = ctx.peek_kind() {
        if SYNC_TOKENS.contains(&kind) {
            break;
        }
        ctx.bump();
    }
    ctx.finish_node();
}
