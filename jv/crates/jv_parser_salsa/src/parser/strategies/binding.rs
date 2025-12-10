use crate::lexer::TokenKind;

use super::{ParserContext, StatementStrategy, SyntaxKind};

pub struct ValStrategy;
pub struct VarStrategy;
pub struct AssignmentStrategy;

impl StatementStrategy for ValStrategy {
    fn name(&self) -> &'static str {
        "val"
    }

    fn matches(&self, _ctx: &ParserContext, lookahead: TokenKind) -> bool {
        lookahead == TokenKind::Val
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        parse_binding(ctx, SyntaxKind::ValDeclaration)
    }
}

impl StatementStrategy for VarStrategy {
    fn name(&self) -> &'static str {
        "var"
    }

    fn matches(&self, _ctx: &ParserContext, lookahead: TokenKind) -> bool {
        lookahead == TokenKind::Var
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        parse_binding(ctx, SyntaxKind::VarDeclaration)
    }
}

impl StatementStrategy for AssignmentStrategy {
    fn name(&self) -> &'static str {
        "assignment"
    }

    fn matches(&self, ctx: &ParserContext, lookahead: TokenKind) -> bool {
        lookahead == TokenKind::Identifier
            && matches!(ctx.peek(1).map(|t| t.kind), Some(TokenKind::Assign))
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        ctx.start_node(SyntaxKind::AssignmentStatement);
        ctx.bump(); // identifier
        ctx.bump(); // =
        let _ = ctx.parse_expression();
        ctx.finish_node();
        true
    }
}

fn parse_binding(ctx: &mut ParserContext, kind: SyntaxKind) -> bool {
    ctx.start_node(kind);
    ctx.bump(); // val/var
    parse_pattern(ctx);
    if ctx.peek_kind() == Some(TokenKind::Colon) {
        ctx.bump(); // :
        ctx.bump_while(|k| {
            !matches!(
                k,
                TokenKind::Assign | TokenKind::Semicolon | TokenKind::Newline
            )
        });
    }
    if ctx.peek_kind() == Some(TokenKind::Assign) {
        ctx.bump();
        let _ = ctx.parse_expression();
    }
    ctx.finish_node();
    true
}

fn parse_pattern(ctx: &mut ParserContext) {
    if ctx.peek_kind() == Some(TokenKind::LeftParen) {
        ctx.start_node(SyntaxKind::DestructuringPattern);
        ctx.bump(); // (
        while ctx.peek_kind() != Some(TokenKind::RightParen) && !ctx.is_eof() {
            ctx.start_node(SyntaxKind::PatternElement);
            if ctx.peek_kind() == Some(TokenKind::Identifier)
                || ctx.peek_kind() == Some(TokenKind::Underscore)
            {
                ctx.bump();
            } else {
                ctx.error("パターン要素には識別子または _ が必要です");
                ctx.bump();
            }
            ctx.finish_node();
            if ctx.peek_kind() == Some(TokenKind::Comma) {
                ctx.bump();
                continue;
            } else {
                break;
            }
        }
        if ctx.peek_kind() == Some(TokenKind::RightParen) {
            ctx.bump();
        } else {
            ctx.error("デストラクトパターンを `)` で閉じてください");
        }
        ctx.finish_node();
    } else if ctx.peek_kind() == Some(TokenKind::Identifier) {
        ctx.start_node(SyntaxKind::Identifier);
        ctx.bump();
        ctx.finish_node();
    }
}
