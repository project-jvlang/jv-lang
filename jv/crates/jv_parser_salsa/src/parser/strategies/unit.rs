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

        // 本体: { ... }
        if !ctx.parse_block() {
            ctx.error("単位定義の本体 `{` が必要です");
        }

        ctx.finish_node();
        // TODO: ここでは構文スケルトンのみ。design.md の UnitTypeDefinition 規約に沿ってヘッダー/依存/変換ブロックを詳細実装する。
        true
    }
}
