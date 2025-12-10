use crate::lexer::TokenKind;

use super::{ParserContext, StatementStrategy, SyntaxKind, control::parse_block};

pub struct FunctionStrategy;

impl StatementStrategy for FunctionStrategy {
    fn name(&self) -> &'static str {
        "function"
    }

    fn matches(&self, _ctx: &ParserContext, lookahead: TokenKind) -> bool {
        lookahead == TokenKind::Fun
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        ctx.start_node(SyntaxKind::FunctionDeclaration);
        ctx.bump(); // fun
        if ctx.peek_kind() == Some(TokenKind::Identifier) {
            ctx.start_node(SyntaxKind::Identifier);
            ctx.bump();
            ctx.finish_node();
        }
        parse_params(ctx);
        // 戻り値型注釈をスキップ
        if ctx.peek_kind() == Some(TokenKind::Arrow) || ctx.peek_kind() == Some(TokenKind::FatArrow)
        {
            ctx.bump();
        }
        // ボディ
        if ctx.peek_kind() == Some(TokenKind::LeftBrace) {
            parse_block(ctx);
        } else if ctx.peek_kind() == Some(TokenKind::FatArrow) {
            ctx.bump();
            let _ = ctx.parse_expression();
        }
        ctx.finish_node();
        true
    }
}

fn parse_params(ctx: &mut ParserContext) {
    if ctx.peek_kind() != Some(TokenKind::LeftParen) {
        return;
    }
    ctx.bump(); // (
    while !ctx.is_eof() && ctx.peek_kind() != Some(TokenKind::RightParen) {
        if ctx.peek_kind() == Some(TokenKind::Identifier) {
            ctx.start_node(SyntaxKind::Identifier);
            ctx.bump();
            ctx.finish_node();
        }
        if ctx.peek_kind() == Some(TokenKind::Colon) {
            ctx.bump();
            ctx.bump_while(|k| k != TokenKind::Comma && k != TokenKind::RightParen);
        }
        if ctx.peek_kind() == Some(TokenKind::Comma) {
            ctx.bump();
        } else {
            break;
        }
    }
    if ctx.peek_kind() == Some(TokenKind::RightParen) {
        ctx.bump();
    }
}
