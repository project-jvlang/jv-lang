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
            TokenKind::WhenKw
                | TokenKind::ForKw
                | TokenKind::ReturnKw
                | TokenKind::ThrowKw
                | TokenKind::BreakKw
                | TokenKind::ContinueKw
                | TokenKind::IfKw
                | TokenKind::WhileKw
                | TokenKind::DoKw
        )
    }

    fn parse(&self, ctx: &mut ParserContext<'_>) -> bool {
        match ctx.peek_significant_kind() {
            Some(TokenKind::WhenKw) => parse_when(ctx),
            Some(TokenKind::ForKw) => parse_for(ctx),
            Some(TokenKind::ReturnKw) => parse_return(ctx),
            Some(TokenKind::ThrowKw) => parse_throw(ctx),
            Some(TokenKind::BreakKw) => parse_break(ctx),
            Some(TokenKind::ContinueKw) => parse_continue(ctx),
            Some(TokenKind::IfKw) => report_unsupported_control(ctx, TokenKind::IfKw),
            Some(TokenKind::WhileKw) => report_unsupported_control(ctx, TokenKind::WhileKw),
            Some(TokenKind::DoKw) => report_unsupported_control(ctx, TokenKind::DoKw),
            _ => false,
        }
    }
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
        match ctx.peek_significant_kind() {
            Some(TokenKind::LayoutComma) | Some(TokenKind::Comma) => {
                ctx.bump_raw();
                continue;
            }
            Some(TokenKind::RightBrace) => {
                ctx.bump_raw();
                break;
            }
            _ => {}
        }
        if ctx.is_eof() {
            ctx.recover_statement("when ブロックが閉じられていません", start);
            break;
        }

        let before_branch = ctx.position();
        parse_when_branch(ctx);

        // 進捗がない場合、強制的にトークンを消費して無限ループを回避
        if ctx.position() == before_branch {
            ctx.bump_raw(); // 強制的に次へ進む
        }
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

    let before_arrow = ctx.position();
    if !ctx.bump_expected(TokenKind::Arrow, "`->` が必要です") {
        // Arrow がない場合、これは不正な分岐なので、
        // 少なくともトークンを1つ進めてから回復する
        if ctx.position() == before_arrow && !ctx.is_eof() {
            ctx.bump_raw(); // 強制的に進める
        }
    }

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
        Some(TokenKind::WhenKw) => {
            parse_when(ctx);
        }
        Some(TokenKind::ForKw) => {
            parse_for(ctx);
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

fn report_unsupported_control(ctx: &mut ParserContext<'_>, keyword: TokenKind) -> bool {
    let message = match keyword {
        TokenKind::IfKw => "JV3103: `if` expressions are not supported / `if` 式はサポートされていません。\n条件分岐は `when` 式を使用してください。Quick Fix: when.convert.if. / Use a `when` expression for branching. Quick Fix: when.convert.if. (--explain JV3103)",
        TokenKind::WhileKw | TokenKind::DoKw => "E_LOOP_001: `while`/`do-while` loops have been removed from the language / `while`/`do-while` ループはサポートされていません。\n`for (item in ...)` ループへ書き換えてください。/ Replace legacy loops with `for (item in ...)`. (--explain E_LOOP_001)",
        _ => "JVF000: Unsupported control construct encountered.\n指定の制御構文は現在サポートされていません。/ This control construct is not supported.",
    };
    ctx.consume_trivia();
    let start = ctx.position();
    ctx.recover_statement(message, start);
    true
}
