use crate::parser::context::ParserContext;
use crate::syntax::{SyntaxKind, TokenKind};

use super::StatementStrategy;

/// リソース管理／並行構文戦略。
pub(crate) struct ResourceStrategy;

pub(crate) static RESOURCE_STRATEGY: ResourceStrategy = ResourceStrategy;

impl StatementStrategy for ResourceStrategy {
    fn name(&self) -> &'static str {
        "resource"
    }

    fn matches(&self, _ctx: &ParserContext<'_>, lookahead: TokenKind) -> bool {
        matches!(
            lookahead,
            TokenKind::UseKw | TokenKind::DeferKw | TokenKind::SpawnKw
        )
    }

    fn parse(&self, ctx: &mut ParserContext<'_>) -> bool {
        match ctx.peek_significant_kind() {
            Some(TokenKind::UseKw) => parse_use(ctx),
            Some(TokenKind::DeferKw) => parse_defer(ctx),
            Some(TokenKind::SpawnKw) => parse_spawn(ctx),
            _ => false,
        }
    }
}

fn parse_use(ctx: &mut ParserContext<'_>) -> bool {
    let start = ctx.position();
    ctx.consume_trivia();
    ctx.start_node(SyntaxKind::UseStatement);
    ctx.bump_expected(TokenKind::UseKw, "`use` キーワードが必要です");

    if ctx.bump_expected(
        TokenKind::LeftParen,
        "`use` リソース指定は '(' で開始します",
    ) {
        ctx.parse_expression_until(&[TokenKind::RightParen], false);
        ctx.bump_expected(
            TokenKind::RightParen,
            "`use` リソースを閉じる ')' が必要です",
        );
    } else {
        ctx.recover_statement("`use` リソース指定の解析に失敗しました", start);
    }

    if !ctx.parse_block() {
        ctx.recover_statement("`use` ブロックが必要です", start);
    }

    ctx.finish_node();
    true
}

fn parse_defer(ctx: &mut ParserContext<'_>) -> bool {
    let start = ctx.position();
    ctx.consume_trivia();
    ctx.start_node(SyntaxKind::DeferStatement);
    ctx.bump_expected(TokenKind::DeferKw, "`defer` キーワードが必要です");

    if !ctx.parse_block() {
        ctx.recover_statement("`defer` ブロックが必要です", start);
    }

    ctx.finish_node();
    true
}

fn parse_spawn(ctx: &mut ParserContext<'_>) -> bool {
    let start = ctx.position();
    ctx.consume_trivia();
    ctx.start_node(SyntaxKind::SpawnStatement);
    ctx.bump_expected(TokenKind::SpawnKw, "`spawn` キーワードが必要です");

    if !ctx.parse_block() {
        ctx.recover_statement("`spawn` ブロックが必要です", start);
    }

    ctx.finish_node();
    true
}
