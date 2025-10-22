use crate::parser::context::ParserContext;
use crate::syntax::{SyntaxKind, TokenKind};

use super::StatementStrategy;

/// 代入ステートメント戦略。
pub(crate) struct AssignmentStrategy;

pub(crate) static ASSIGNMENT_STRATEGY: AssignmentStrategy = AssignmentStrategy;

impl StatementStrategy for AssignmentStrategy {
    fn name(&self) -> &'static str {
        "assignment"
    }

    fn matches(&self, ctx: &ParserContext<'_>, lookahead: TokenKind) -> bool {
        if lookahead != TokenKind::Identifier {
            return false;
        }

        let mut offset = 1usize;
        while let Some((_, kind)) = ctx.peek_significant_kind_n(offset) {
            match kind {
                TokenKind::Assign => return true,
                TokenKind::Dot => {
                    if let Some((_, next)) = ctx.peek_significant_kind_n(offset + 1) {
                        if next == TokenKind::Identifier {
                            offset += 2;
                            continue;
                        }
                    }
                    return false;
                }
                _ => return false,
            }
        }
        false
    }

    fn parse(&self, ctx: &mut ParserContext<'_>) -> bool {
        let start = ctx.position();
        ctx.consume_trivia();
        ctx.start_node(SyntaxKind::AssignmentStatement);

        ctx.start_node(SyntaxKind::AssignmentTarget);
        if !parse_assignment_target(ctx) {
            ctx.finish_node(); // AssignmentTarget
            ctx.finish_node(); // AssignmentStatement
            ctx.recover_statement("代入ターゲットの解析に失敗しました", start);
            return true;
        }
        ctx.finish_node(); // AssignmentTarget

        if !ctx.bump_expected(TokenKind::Assign, "`=` が必要です") {
            ctx.finish_node(); // AssignmentStatement
            return true;
        }

        if !ctx.parse_expression_until(
            &[
                TokenKind::Semicolon,
                TokenKind::Newline,
                TokenKind::RightBrace,
            ],
            true,
        ) {
            ctx.finish_node();
            ctx.recover_statement("代入式の右辺が必要です", start);
            return true;
        }

        ctx.finish_node();
        true
    }
}

fn parse_assignment_target(ctx: &mut ParserContext<'_>) -> bool {
    ctx.consume_trivia();
    if ctx.peek_significant_kind() != Some(TokenKind::Identifier) {
        return false;
    }
    ctx.bump_raw();

    loop {
        ctx.consume_trivia();
        if ctx.peek_significant_kind() != Some(TokenKind::Dot) {
            break;
        }

        if ctx.peek_significant_kind_n(1).map(|(_, kind)| kind) != Some(TokenKind::Identifier) {
            return false;
        }

        ctx.bump_raw(); // '.'
        ctx.consume_trivia();
        ctx.bump_raw(); // identifier
    }

    true
}
