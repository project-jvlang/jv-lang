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
        lookahead == TokenKind::Identifier && find_assignment_operator(ctx).is_some()
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        if find_assignment_operator(ctx).is_none() {
            return false;
        }
        ctx.start_node(SyntaxKind::AssignmentStatement);
        let _ = ctx.parse_expression(); // target
        ctx.bump_while(|kind| {
            matches!(
                kind,
                TokenKind::Whitespace
                    | TokenKind::Newline
                    | TokenKind::LayoutComma
                    | TokenKind::FieldNameLabel
                    | TokenKind::LineComment
                    | TokenKind::BlockComment
                    | TokenKind::JavaDocComment
            )
        });
        if ctx.peek_kind() == Some(TokenKind::Colon) {
            // `name: Type = value` のような暗黙の型注釈付き束縛を許可する。
            ctx.bump(); // :
            ctx.bump_while(|k| {
                !matches!(
                    k,
                    TokenKind::Assign
                        | TokenKind::Semicolon
                        | TokenKind::Newline
                        | TokenKind::LayoutComma
                )
            });
            ctx.bump_while(|kind| {
                matches!(
                    kind,
                    TokenKind::Whitespace
                        | TokenKind::Newline
                        | TokenKind::LayoutComma
                        | TokenKind::FieldNameLabel
                        | TokenKind::LineComment
                        | TokenKind::BlockComment
                        | TokenKind::JavaDocComment
                )
            });
        }
        if ctx.peek_kind() == Some(TokenKind::Assign) {
            ctx.bump(); // =
        } else {
            ctx.error("代入には `=` が必要です");
        }
        let _ = ctx.parse_expression(); // value
        ctx.finish_node();
        true
    }
}

fn find_assignment_operator(ctx: &ParserContext) -> Option<usize> {
    // `settings.theme = ...` のようにターゲット式の後ろに `=` が現れるケースも assignment として扱う。
    // ただし `f(a = b)` のように括弧内で現れる `=` は除外するため、ネスト深さを追跡する。
    let mut paren_depth: usize = 0;
    let mut bracket_depth: usize = 0;
    let mut brace_depth: usize = 0;
    let mut i = 0usize;
    while let Some(tok) = ctx.peek(i) {
        if i > 0
            && tok.leading_trivia.newlines > 0
            && paren_depth == 0
            && bracket_depth == 0
            && brace_depth == 0
        {
            // 物理改行を跨いで `=` を探すと、次のステートメントの代入を誤検出する。
            break;
        }
        match tok.kind {
            TokenKind::LeftParen => paren_depth += 1,
            TokenKind::RightParen => paren_depth = paren_depth.saturating_sub(1),
            TokenKind::LeftBracket => bracket_depth += 1,
            TokenKind::RightBracket => bracket_depth = bracket_depth.saturating_sub(1),
            TokenKind::Assign if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 => {
                return Some(i);
            }
            TokenKind::LeftBrace if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 => {
                // トレーリングブロック（ラムダ/ブロック引数）を含む式は、
                // ブロック内の `=` を誤検出しないよう assignment 判定から除外する。
                break;
            }
            TokenKind::LeftBrace => brace_depth += 1,
            TokenKind::RightBrace if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 => {
                break;
            }
            TokenKind::RightBrace => brace_depth = brace_depth.saturating_sub(1),
            TokenKind::Newline | TokenKind::Semicolon | TokenKind::LayoutComma
                if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 =>
            {
                break;
            }
            TokenKind::Eof => break,
            _ => {}
        }
        i += 1;
    }
    None
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
        while !ctx.is_eof() {
            ctx.bump_while(|kind| {
                matches!(
                    kind,
                    TokenKind::Whitespace
                        | TokenKind::Newline
                        | TokenKind::LayoutComma
                        | TokenKind::FieldNameLabel
                        | TokenKind::LineComment
                        | TokenKind::BlockComment
                        | TokenKind::JavaDocComment
                )
            });
            if ctx.peek_kind() == Some(TokenKind::RightParen) {
                break;
            }
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

            ctx.bump_while(|kind| {
                matches!(
                    kind,
                    TokenKind::Whitespace
                        | TokenKind::Newline
                        | TokenKind::LayoutComma
                        | TokenKind::FieldNameLabel
                        | TokenKind::LineComment
                        | TokenKind::BlockComment
                        | TokenKind::JavaDocComment
                )
            });
            if ctx.peek_kind() == Some(TokenKind::Comma) {
                ctx.bump();
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
