use crate::lexer::TokenKind;

use super::{ParserContext, StatementStrategy, SyntaxKind, control::parse_block};

pub struct WhenStrategy;

impl StatementStrategy for WhenStrategy {
    fn name(&self) -> &'static str {
        "when"
    }

    fn matches(&self, _ctx: &ParserContext, lookahead: TokenKind) -> bool {
        lookahead == TokenKind::When
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        parse_when_statement(ctx)
    }
}

pub(crate) fn parse_when_statement(ctx: &mut ParserContext) -> bool {
    ctx.start_node(SyntaxKind::WhenStatement);
    ctx.bump(); // when

    ctx.bump_while(is_when_trivia);
    // `when { ... }` (subjectless) を許可する。
    if ctx.peek_kind() != Some(TokenKind::LeftBrace) {
        let prev_allow_trailing_block = ctx.allow_trailing_block;
        ctx.allow_trailing_block = false;
        let _ = ctx.parse_expression();
        ctx.allow_trailing_block = prev_allow_trailing_block;
    }

    ctx.bump_while(is_when_trivia);
    if ctx.peek_kind() == Some(TokenKind::LeftBrace) {
        ctx.bump(); // {
        loop {
            ctx.bump_while(is_when_trivia);
            if ctx.is_eof() || ctx.peek_kind() == Some(TokenKind::RightBrace) {
                break;
            }
            parse_branch(ctx);
        }
        if ctx.peek_kind() == Some(TokenKind::RightBrace) {
            ctx.bump();
        } else {
            ctx.error("when ブロックが `}` で閉じていません");
        }
    } else {
        ctx.error("when ブロックの `{` が必要です");
    }

    ctx.finish_node();
    true
}

fn parse_branch(ctx: &mut ParserContext) {
    ctx.start_node(SyntaxKind::WhenBranch);
    ctx.bump_while(is_when_trivia);
    if ctx.peek_kind() == Some(TokenKind::Else) {
        ctx.bump();
    } else {
        // `in` / `is` / `&&` などパターン特有の記法は式パーサが扱えないため、
        // トップレベルの `->` までトークンをそのまま取り込む。
        let mut brace_depth: isize = 0;
        let mut paren_depth: isize = 0;
        let mut bracket_depth: isize = 0;
        let mut consumed_any = false;
        while !ctx.is_eof() {
            match ctx.peek_kind() {
                Some(TokenKind::Arrow | TokenKind::FatArrow)
                    if brace_depth == 0 && paren_depth == 0 && bracket_depth == 0 =>
                {
                    break;
                }
                _ => {}
            }

            match ctx.peek_kind() {
                Some(TokenKind::LeftBrace) => brace_depth += 1,
                Some(TokenKind::RightBrace) => brace_depth -= 1,
                Some(TokenKind::LeftParen) => paren_depth += 1,
                Some(TokenKind::RightParen) => paren_depth -= 1,
                Some(TokenKind::LeftBracket) => bracket_depth += 1,
                Some(TokenKind::RightBracket) => bracket_depth -= 1,
                _ => {}
            }

            ctx.bump();
            consumed_any = true;
        }

        if !consumed_any {
            ctx.error("when ブランチのパターンが必要です");
        }
    }
    ctx.bump_while(is_when_trivia);
    if ctx.peek_kind() == Some(TokenKind::Arrow) || ctx.peek_kind() == Some(TokenKind::FatArrow) {
        ctx.bump();
    } else {
        ctx.error("when ブランチに `->` が必要です");
        ctx.bump(); // 少なくとも1トークン進める
    }
    // ブランチ本体: 単式またはブロック
    ctx.bump_while(is_when_trivia);
    if ctx.peek_kind() == Some(TokenKind::LeftBrace) {
        parse_block(ctx);
    } else {
        let _ = ctx.parse_expression();
    }
    ctx.finish_node();
}

fn is_when_trivia(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::Whitespace | TokenKind::Newline | TokenKind::LayoutComma | TokenKind::Semicolon
    )
}
