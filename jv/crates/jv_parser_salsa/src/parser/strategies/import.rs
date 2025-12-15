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
        ctx.start_node(SyntaxKind::ImportPath);
        ctx.start_node(SyntaxKind::Identifier);
        ctx.bump(); // first
        ctx.finish_node();
        while ctx.peek_kind() == Some(TokenKind::Dot) {
            ctx.bump();
            ctx.start_node(SyntaxKind::Identifier);
            ctx.bump();
            ctx.finish_node();
        }
        let has_wildcard = if ctx.peek_kind() == Some(TokenKind::Multiply) {
            ctx.bump();
            true
        } else {
            false
        };
        ctx.finish_node(); // ImportPath

        // alias: import path as Alias
        if !has_wildcard {
            if let Some(tok) = ctx.peek(0)
                && tok.kind == TokenKind::Identifier
                && tok.lexeme.eq_ignore_ascii_case("as")
            {
                ctx.start_node(SyntaxKind::ImportClause);
                ctx.bump(); // as
                ctx.start_node(SyntaxKind::ImportAlias);
                if ctx.peek_kind() == Some(TokenKind::Identifier) {
                    ctx.bump();
                } else {
                    ctx.error("import alias には識別子が必要です");
                }
                ctx.finish_node(); // ImportAlias
                ctx.finish_node(); // ImportClause
            }
        } else if let Some(tok) = ctx.peek(0)
            && tok.kind == TokenKind::Identifier
            && tok.lexeme.eq_ignore_ascii_case("as")
        {
            ctx.error("ワイルドカード import に別名は付けられません");
            ctx.start_node(SyntaxKind::ImportClause);
            ctx.start_node(SyntaxKind::ImportAlias);
            ctx.bump(); // as (consume to avoid loop)
            ctx.finish_node(); // ImportAlias
            ctx.finish_node(); // ImportClause
        }
        ctx.finish_node();
        true
    }
}
