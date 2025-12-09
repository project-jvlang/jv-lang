use crate::lexer::TokenKind;

use super::{control::parse_block, ParserContext, StatementStrategy, SyntaxKind};

pub struct ClassStrategy;
pub struct DataClassStrategy;

impl StatementStrategy for ClassStrategy {
    fn name(&self) -> &'static str {
        "class"
    }

    fn matches(&self, _ctx: &ParserContext, lookahead: TokenKind) -> bool {
        lookahead == TokenKind::Class
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        parse_class(ctx, SyntaxKind::ClassDeclaration)
    }
}

impl StatementStrategy for DataClassStrategy {
    fn name(&self) -> &'static str {
        "data-class"
    }

    fn matches(&self, _ctx: &ParserContext, lookahead: TokenKind) -> bool {
        lookahead == TokenKind::Data
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        parse_class(ctx, SyntaxKind::DataClassDeclaration)
    }
}

fn parse_class(ctx: &mut ParserContext, kind: SyntaxKind) -> bool {
    ctx.start_node(kind);
    ctx.bump(); // class/data
    if ctx.peek_kind() == Some(TokenKind::Identifier) {
        ctx.start_node(SyntaxKind::Identifier);
        ctx.bump();
        ctx.finish_node();
    }
    // ジェネリクスや extends/implements はここではスキップ。
    while let Some(k) = ctx.peek_kind() {
        if k == TokenKind::LeftBrace {
            break;
        }
        if k == TokenKind::Eof {
            break;
        }
        ctx.bump();
    }
    if ctx.peek_kind() == Some(TokenKind::LeftBrace) {
        parse_block(ctx);
    }
    ctx.finish_node();
    true
}
