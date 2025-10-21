use crate::parser::context::ParserContext;
use crate::syntax::{SyntaxKind, TokenKind};

use super::StatementStrategy;

/// 制御構文戦略。
pub(crate) struct ControlStrategy;

pub(crate) static CONTROL_STRATEGY: ControlStrategy = ControlStrategy;

impl StatementStrategy for ControlStrategy {
    fn name(&self) -> &'static str {
        "control"
    }

    fn matches(&self, ctx: &ParserContext<'_>, lookahead: TokenKind) -> bool {
        let _ = ctx;
        matches!(
            lookahead,
            TokenKind::IfKw
                | TokenKind::WhenKw
                | TokenKind::ForKw
                | TokenKind::WhileKw
                | TokenKind::DoKw
                | TokenKind::ReturnKw
                | TokenKind::ThrowKw
                | TokenKind::BreakKw
                | TokenKind::ContinueKw
        )
    }

    fn parse(&self, ctx: &mut ParserContext<'_>) -> bool {
        match ctx.peek_significant_kind() {
            Some(TokenKind::IfKw) => parse_if(ctx),
            Some(TokenKind::WhenKw) => parse_when(ctx),
            Some(TokenKind::ForKw) => parse_for(ctx),
            Some(TokenKind::WhileKw) => parse_while(ctx),
            Some(TokenKind::DoKw) => parse_do_while(ctx),
            Some(TokenKind::ReturnKw) => parse_return(ctx),
            Some(TokenKind::ThrowKw) => parse_throw(ctx),
            Some(TokenKind::BreakKw) => parse_break(ctx),
            Some(TokenKind::ContinueKw) => parse_continue(ctx),
            _ => false,
        }
    }
}

fn parse_if(ctx: &mut ParserContext<'_>) -> bool {
    let start = ctx.position();
    ctx.consume_trivia();
    ctx.start_node(SyntaxKind::IfStatement);
    ctx.bump_expected(TokenKind::IfKw, "`if` キーワードが必要です");
    ctx.parse_expression_until(
        &[
            TokenKind::LeftBrace,
            TokenKind::ElseKw,
            TokenKind::Semicolon,
            TokenKind::Newline,
        ],
        true,
    );

    if ctx.peek_significant_kind() == Some(TokenKind::LeftBrace) {
        ctx.parse_block();
    } else {
        ctx.parse_expression_until(
            &[
                TokenKind::ElseKw,
                TokenKind::Semicolon,
                TokenKind::Newline,
                TokenKind::RightBrace,
            ],
            true,
        );
    }

    parse_else_clause(ctx, start);
    ctx.finish_node();
    true
}

fn parse_else_clause(ctx: &mut ParserContext<'_>, start: usize) {
    ctx.consume_trivia();
    if ctx.peek_significant_kind() != Some(TokenKind::ElseKw) {
        return;
    }

    ctx.start_node(SyntaxKind::ElseClause);
    ctx.bump_raw(); // else
    ctx.consume_trivia();

    match ctx.peek_significant_kind() {
        Some(TokenKind::IfKw) => {
            parse_if(ctx);
        }
        Some(TokenKind::LeftBrace) => {
            ctx.parse_block();
        }
        _ => {
            ctx.parse_expression_until(
                &[
                    TokenKind::Semicolon,
                    TokenKind::Newline,
                    TokenKind::RightBrace,
                ],
                true,
            );
        }
    }

    if ctx.position() == start {
        ctx.recover_statement("else 節の解析に失敗しました", start);
    }

    ctx.finish_node();
}

fn parse_when(ctx: &mut ParserContext<'_>) -> bool {
    let start = ctx.position();
    ctx.consume_trivia();
    ctx.start_node(SyntaxKind::WhenStatement);
    ctx.bump_expected(TokenKind::WhenKw, "`when` キーワードが必要です");

    if ctx.bump_if(TokenKind::LeftParen) {
        ctx.parse_expression_until(&[TokenKind::RightParen], false);
        ctx.bump_expected(TokenKind::RightParen, "`when` 条件を閉じる ')' が必要です");
    } else {
        ctx.parse_expression_until(&[TokenKind::LeftBrace], true);
    }

    ctx.consume_trivia();
    if ctx.peek_significant_kind() != Some(TokenKind::LeftBrace) {
        ctx.recover_statement("when ブロックが必要です", start);
        ctx.finish_node();
        return true;
    }

    ctx.bump_raw(); // '{'
    loop {
        ctx.consume_trivia();
        if ctx.peek_significant_kind() == Some(TokenKind::RightBrace) {
            ctx.bump_raw();
            break;
        }
        if ctx.is_eof() {
            ctx.recover_statement("when ブロックが閉じられていません", start);
            break;
        }

        parse_when_branch(ctx);
    }

    ctx.finish_node();
    true
}

fn parse_when_branch(ctx: &mut ParserContext<'_>) {
    let branch_start = ctx.position();
    ctx.start_node(SyntaxKind::WhenBranch);

    ctx.consume_trivia();
    if ctx.peek_significant_kind() == Some(TokenKind::ElseKw) {
        ctx.bump_raw();
    } else {
        ctx.parse_expression_until(&[TokenKind::Arrow], false);
    }

    ctx.bump_expected(TokenKind::Arrow, "`->` が必要です");

    parse_when_branch_body(ctx);

    if ctx.position() == branch_start {
        ctx.recover_statement("when 分岐の解析に失敗しました", branch_start);
    }

    ctx.finish_node();
}

fn parse_when_branch_body(ctx: &mut ParserContext<'_>) {
    ctx.consume_trivia();
    match ctx.peek_significant_kind() {
        Some(TokenKind::LeftBrace) => {
            ctx.parse_block();
        }
        Some(TokenKind::ReturnKw) => {
            parse_return(ctx);
        }
        Some(TokenKind::ThrowKw) => {
            parse_throw(ctx);
        }
        Some(TokenKind::BreakKw) => {
            parse_break(ctx);
        }
        Some(TokenKind::ContinueKw) => {
            parse_continue(ctx);
        }
        Some(TokenKind::IfKw) => {
            parse_if(ctx);
        }
        Some(TokenKind::WhenKw) => {
            parse_when(ctx);
        }
        Some(TokenKind::ForKw) => {
            parse_for(ctx);
        }
        Some(TokenKind::WhileKw) => {
            parse_while(ctx);
        }
        Some(TokenKind::DoKw) => {
            parse_do_while(ctx);
        }
        Some(_) => {
            ctx.parse_expression_until(
                &[
                    TokenKind::Semicolon,
                    TokenKind::Newline,
                    TokenKind::RightBrace,
                ],
                false,
            );
        }
        None => {}
    }
}

fn parse_for(ctx: &mut ParserContext<'_>) -> bool {
    let start = ctx.position();
    ctx.consume_trivia();
    ctx.start_node(SyntaxKind::ForStatement);
    ctx.bump_expected(TokenKind::ForKw, "`for` キーワードが必要です");

    if ctx.bump_expected(TokenKind::LeftParen, "`for` ヘッダは '(' で開始します") {
        ctx.parse_binding_pattern();
        ctx.bump_expected(TokenKind::InKw, "`in` キーワードが必要です");
        ctx.parse_expression_until(&[TokenKind::RightParen], false);
        ctx.bump_expected(TokenKind::RightParen, "`for` ヘッダを閉じる ')' が必要です");
    } else {
        ctx.recover_statement("`for` ヘッダの解析に失敗しました", start);
    }

    if !ctx.parse_block() {
        ctx.recover_statement("`for` 本体が必要です", start);
    }

    ctx.finish_node();
    true
}

fn parse_while(ctx: &mut ParserContext<'_>) -> bool {
    let start = ctx.position();
    ctx.consume_trivia();
    ctx.start_node(SyntaxKind::WhileStatement);
    ctx.bump_expected(TokenKind::WhileKw, "`while` キーワードが必要です");
    ctx.parse_expression_until(&[TokenKind::LeftBrace], true);

    if !ctx.parse_block() {
        ctx.recover_statement("`while` 本体が必要です", start);
    }

    ctx.finish_node();
    true
}

fn parse_do_while(ctx: &mut ParserContext<'_>) -> bool {
    let start = ctx.position();
    ctx.consume_trivia();
    ctx.start_node(SyntaxKind::DoWhileStatement);
    ctx.bump_expected(TokenKind::DoKw, "`do` キーワードが必要です");

    if !ctx.parse_block() {
        ctx.recover_statement("`do` ブロックが必要です", start);
    }

    ctx.bump_expected(TokenKind::WhileKw, "`while` キーワードが必要です");
    ctx.parse_expression_until(
        &[
            TokenKind::Semicolon,
            TokenKind::Newline,
            TokenKind::RightBrace,
        ],
        true,
    );
    ctx.finish_node();
    true
}

fn parse_return(ctx: &mut ParserContext<'_>) -> bool {
    ctx.consume_trivia();
    ctx.start_node(SyntaxKind::ReturnStatement);
    ctx.bump_expected(TokenKind::ReturnKw, "`return` キーワードが必要です");
    ctx.parse_expression_until(
        &[
            TokenKind::Semicolon,
            TokenKind::Newline,
            TokenKind::RightBrace,
        ],
        true,
    );
    ctx.finish_node();
    true
}

fn parse_throw(ctx: &mut ParserContext<'_>) -> bool {
    ctx.consume_trivia();
    ctx.start_node(SyntaxKind::ThrowStatement);
    ctx.bump_expected(TokenKind::ThrowKw, "`throw` キーワードが必要です");
    ctx.parse_expression_until(
        &[
            TokenKind::Semicolon,
            TokenKind::Newline,
            TokenKind::RightBrace,
        ],
        true,
    );
    ctx.finish_node();
    true
}

fn parse_break(ctx: &mut ParserContext<'_>) -> bool {
    ctx.consume_trivia();
    ctx.start_node(SyntaxKind::BreakStatement);
    ctx.bump_expected(TokenKind::BreakKw, "`break` キーワードが必要です");
    ctx.finish_node();
    true
}

fn parse_continue(ctx: &mut ParserContext<'_>) -> bool {
    ctx.consume_trivia();
    ctx.start_node(SyntaxKind::ContinueStatement);
    ctx.bump_expected(TokenKind::ContinueKw, "`continue` キーワードが必要です");
    ctx.finish_node();
    true
}
