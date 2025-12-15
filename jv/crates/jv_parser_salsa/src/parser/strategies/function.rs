use crate::lexer::TokenKind;

use super::{ParserContext, StatementStrategy, SyntaxKind, control::parse_block};

pub struct FunctionStrategy;

impl StatementStrategy for FunctionStrategy {
    fn name(&self) -> &'static str {
        "function"
    }

    fn matches(&self, _ctx: &ParserContext, lookahead: TokenKind) -> bool {
        lookahead == TokenKind::Fun
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        ctx.start_node(SyntaxKind::FunctionDeclaration);
        ctx.bump(); // fun
        consume_until_params(ctx);
        parse_params(ctx);
        consume_return_type_and_where(ctx);
        // ボディ
        if ctx.peek_kind() == Some(TokenKind::LeftBrace) {
            parse_block(ctx);
        } else if ctx.peek_kind() == Some(TokenKind::Assign) {
            ctx.bump();
            let _ = ctx.parse_expression();
        } else if ctx.peek_kind() == Some(TokenKind::FatArrow) {
            ctx.bump();
            let _ = ctx.parse_expression();
        }
        ctx.finish_node();
        true
    }
}

fn parse_params(ctx: &mut ParserContext) {
    if ctx.peek_kind() != Some(TokenKind::LeftParen) {
        return;
    }
    ctx.bump(); // (
    loop {
        // 改行やレイアウトカンマはセパレータとして扱う。
        ctx.bump_while(|k| matches!(k, TokenKind::Newline | TokenKind::LayoutComma));

        if ctx.is_eof() || ctx.peek_kind() == Some(TokenKind::RightParen) {
            break;
        }

        if ctx.peek_kind() == Some(TokenKind::Identifier) {
            ctx.start_node(SyntaxKind::Identifier);
            ctx.bump();
            ctx.finish_node();
        }
        if ctx.peek_kind() == Some(TokenKind::Colon) {
            ctx.bump();
            ctx.bump_while(|k| {
                !matches!(
                    k,
                    TokenKind::Comma
                        | TokenKind::LayoutComma
                        | TokenKind::Newline
                        | TokenKind::RightParen
                        | TokenKind::Eof
                )
            });
        }

        // 次のパラメータ区切りを消費
        if matches!(
            ctx.peek_kind(),
            Some(TokenKind::Comma | TokenKind::LayoutComma | TokenKind::Newline)
        ) {
            ctx.bump();
        }
    }
    if ctx.peek_kind() == Some(TokenKind::RightParen) {
        ctx.bump();
    }
}

/// 型パラメータやレシーバ、関数名などを引数リスト開始までスキップする。
fn consume_until_params(ctx: &mut ParserContext) {
    while let Some(kind) = ctx.peek_kind() {
        if matches!(
            kind,
            TokenKind::LeftParen
                | TokenKind::LeftBrace
                | TokenKind::Assign
                | TokenKind::FatArrow
                | TokenKind::Colon
                | TokenKind::Where
                | TokenKind::Eof
        ) {
            break;
        }
        ctx.bump();
    }
}

/// 戻り値型と where 句をスキップして関数本体の開始まで進める。
fn consume_return_type_and_where(ctx: &mut ParserContext) {
    loop {
        match ctx.peek_kind() {
            Some(
                TokenKind::LeftBrace | TokenKind::Assign | TokenKind::FatArrow | TokenKind::Eof,
            ) => {
                break;
            }
            Some(TokenKind::Colon) => {
                ctx.bump(); // :
                // 戻り値型。`where` や `{` 手前まで読み飛ばす。
                while let Some(kind) = ctx.peek_kind() {
                    if matches!(
                        kind,
                        TokenKind::LeftBrace
                            | TokenKind::Assign
                            | TokenKind::FatArrow
                            | TokenKind::Where
                            | TokenKind::Eof
                    ) {
                        break;
                    }
                    ctx.bump();
                }
            }
            Some(TokenKind::Where) => {
                ctx.bump(); // where
                while let Some(kind) = ctx.peek_kind() {
                    if matches!(
                        kind,
                        TokenKind::LeftBrace
                            | TokenKind::Assign
                            | TokenKind::FatArrow
                            | TokenKind::Eof
                    ) {
                        break;
                    }
                    ctx.bump();
                }
            }
            Some(TokenKind::Newline | TokenKind::Comma | TokenKind::Semicolon) => {
                ctx.bump();
            }
            Some(_) => {
                ctx.bump();
            }
            None => break,
        }
    }
}
