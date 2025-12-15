use crate::lexer::TokenKind;

use super::{ParserContext, StatementStrategy, SyntaxKind};

/// 単位型定義の簡易パーサー（設計順序に合わせたプレースホルダー）。
pub struct UnitTypeDefStrategy;

impl StatementStrategy for UnitTypeDefStrategy {
    fn name(&self) -> &'static str {
        "unit-type-definition"
    }

    fn matches(&self, ctx: &ParserContext, lookahead: TokenKind) -> bool {
        lookahead == TokenKind::At
            && matches!(ctx.peek(1).map(|t| t.kind), Some(TokenKind::Identifier))
            && matches!(ctx.peek(2).map(|t| t.kind), Some(TokenKind::LeftParen))
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        ctx.start_node(SyntaxKind::UnitTypeDefinition);
        ctx.bump(); // @

        // ヘッダー: @ <category> ( <base> )
        ctx.start_node(SyntaxKind::UnitCategory);
        if ctx.peek_kind() == Some(TokenKind::Identifier) {
            ctx.bump();
        } else {
            ctx.error("単位カテゴリを識別子で指定してください");
        }
        ctx.finish_node();

        if ctx.peek_kind() == Some(TokenKind::LeftParen) {
            ctx.bump();
            let _ = ctx.parse_expression();
            if ctx.peek_kind() == Some(TokenKind::RightParen) {
                ctx.bump();
            } else {
                ctx.error("基底型の丸括弧が閉じていません");
            }
        } else {
            ctx.error("基底型を丸括弧で指定してください");
        }

        // シンボル: `@ 温度(Double) ℃ { ... }` の `℃` のように、
        // 基底型の後ろに単位シンボルが続くケースを許容する。
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
        while let Some(kind) = ctx.peek_kind() {
            if kind == TokenKind::LeftBrace {
                break;
            }
            if matches!(
                kind,
                TokenKind::Newline | TokenKind::Semicolon | TokenKind::Eof
            ) {
                break;
            }
            // 単位シンボル（例: `℃`）は Identifier としてトークナイズされないことがあるため、
            // ブロック開始までのトークン列を許容して読み飛ばす。
            ctx.bump();
        }

        // 本体: { ... }
        // 単位定義ブロック内の詳細構文は DSL に委譲するため、ここでは波括弧の対応のみを取りながら
        // トークンを消費して構文エラーを避ける。
        if ctx.peek_kind() == Some(TokenKind::LeftBrace) {
            ctx.start_node(SyntaxKind::Block);
            let mut depth: usize = 0;
            ctx.bump(); // {
            while let Some(kind) = ctx.peek_kind() {
                match kind {
                    TokenKind::LeftBrace => {
                        depth += 1;
                        ctx.bump();
                    }
                    TokenKind::RightBrace => {
                        ctx.bump();
                        if depth == 0 {
                            break;
                        }
                        depth = depth.saturating_sub(1);
                    }
                    TokenKind::Eof => break,
                    _ => {
                        ctx.bump();
                    }
                }
            }
            ctx.finish_node(); // Block
        } else {
            ctx.error("単位定義の本体 `{` が必要です");
        }

        ctx.finish_node();
        // TODO: ここでは構文スケルトンのみ。design.md の UnitTypeDefinition 規約に沿ってヘッダー/依存/変換ブロックを詳細実装する。
        true
    }
}
