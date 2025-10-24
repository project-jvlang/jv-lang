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
        match lookahead {
            TokenKind::Identifier => self.matches_identifier_target(ctx),
            TokenKind::LeftBracket | TokenKind::LeftParen => {
                self.matches_pattern_target(ctx, lookahead)
            }
            _ => false,
        }
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
    match ctx.peek_significant_kind() {
        Some(TokenKind::Identifier) => {
            if ctx.peek_significant_kind_n(1).map(|(_, kind)| kind) == Some(TokenKind::Dot) {
                ctx.bump_raw();
                loop {
                    ctx.consume_trivia();
                    if ctx.peek_significant_kind() != Some(TokenKind::Dot) {
                        break;
                    }

                    if ctx.peek_significant_kind_n(1).map(|(_, kind)| kind)
                        != Some(TokenKind::Identifier)
                    {
                        return false;
                    }

                    ctx.bump_raw(); // '.'
                    ctx.consume_trivia();
                    ctx.bump_raw(); // identifier
                }
                ctx.parse_optional_type_annotation();
                true
            } else {
                let parsed = ctx.parse_binding_pattern();
                if parsed {
                    ctx.parse_optional_type_annotation();
                }
                parsed
            }
        }
        Some(TokenKind::LeftBracket) | Some(TokenKind::LeftParen) => ctx.parse_binding_pattern(),
        _ => false,
    }
}

impl AssignmentStrategy {
    fn matches_identifier_target(&self, ctx: &ParserContext<'_>) -> bool {
        let mut offset = 1usize;
        while let Some((_, kind)) = ctx.peek_significant_kind_n(offset) {
            match kind {
                TokenKind::Assign => return true,
                TokenKind::Colon => {
                    let next_offset = self.skip_type_annotation(ctx, offset + 1);
                    if next_offset == offset + 1 {
                        return false;
                    }
                    offset = next_offset;
                    continue;
                }
                TokenKind::Dot => {
                    if let Some((_, next)) = ctx.peek_significant_kind_n(offset + 1) {
                        if next == TokenKind::Identifier {
                            offset += 2;
                            continue;
                        }
                    }
                    return false;
                }
                TokenKind::Semicolon
                | TokenKind::Newline
                | TokenKind::RightBrace
                | TokenKind::Eof => return false,
                _ => return false,
            }
        }
        false
    }

    fn skip_type_annotation(&self, ctx: &ParserContext<'_>, mut offset: usize) -> usize {
        let mut depth_paren = 0usize;
        let mut depth_bracket = 0usize;
        let mut depth_angle = 0usize;

        while let Some((_, kind)) = ctx.peek_significant_kind_n(offset) {
            match kind {
                TokenKind::LeftParen => depth_paren = depth_paren.saturating_add(1),
                TokenKind::RightParen => {
                    if depth_paren == 0 {
                        break;
                    }
                    depth_paren -= 1;
                }
                TokenKind::LeftBracket => depth_bracket = depth_bracket.saturating_add(1),
                TokenKind::RightBracket => {
                    if depth_bracket == 0 {
                        break;
                    }
                    depth_bracket -= 1;
                }
                TokenKind::Less => depth_angle = depth_angle.saturating_add(1),
                TokenKind::Greater => {
                    if depth_angle > 0 {
                        depth_angle -= 1;
                    }
                }
                TokenKind::Assign
                | TokenKind::LeftBrace
                | TokenKind::Semicolon
                | TokenKind::RightBrace
                | TokenKind::Comma
                | TokenKind::WhereKw
                    if depth_paren == 0 && depth_bracket == 0 && depth_angle == 0 =>
                {
                    break;
                }
                _ => {}
            }

            offset += 1;
        }

        offset
    }

    fn matches_pattern_target(&self, ctx: &ParserContext<'_>, opening: TokenKind) -> bool {
        let Some(initial_closer) = closing_delimiter(opening) else {
            return false;
        };

        let mut offset = 1usize;
        let mut stack = vec![initial_closer];

        while let Some((_, kind)) = ctx.peek_significant_kind_n(offset) {
            if let Some(expected) = stack.last() {
                if kind == *expected {
                    stack.pop();
                    offset += 1;
                    if stack.is_empty() {
                        break;
                    }
                    continue;
                }
            }

            match kind {
                TokenKind::LeftBracket => stack.push(TokenKind::RightBracket),
                TokenKind::LeftParen => stack.push(TokenKind::RightParen),
                TokenKind::Assign if stack.is_empty() => return true,
                TokenKind::Semicolon | TokenKind::Newline | TokenKind::RightBrace
                    if stack.is_empty() =>
                {
                    return false;
                }
                _ => {}
            }

            offset += 1;
        }

        if stack.is_empty() {
            while let Some((_, kind)) = ctx.peek_significant_kind_n(offset) {
                match kind {
                    TokenKind::Assign => return true,
                    TokenKind::Semicolon | TokenKind::Newline | TokenKind::RightBrace => {
                        return false
                    }
                    TokenKind::Comma | TokenKind::LayoutComma => {
                        offset += 1;
                    }
                    _ => {
                        offset += 1;
                    }
                }
            }
        }

        false
    }
}

fn closing_delimiter(opening: TokenKind) -> Option<TokenKind> {
    match opening {
        TokenKind::LeftBracket => Some(TokenKind::RightBracket),
        TokenKind::LeftParen => Some(TokenKind::RightParen),
        _ => None,
    }
}
