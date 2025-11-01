use crate::parser::context::ParserContext;
use crate::syntax::TokenKind;

use super::StatementStrategy;

/// 式ステートメント戦略。
pub(crate) struct ExpressionStrategy;

pub(crate) static EXPRESSION_STRATEGY: ExpressionStrategy = ExpressionStrategy;

impl StatementStrategy for ExpressionStrategy {
    fn name(&self) -> &'static str {
        "expression"
    }

    fn matches(&self, _ctx: &ParserContext<'_>, lookahead: TokenKind) -> bool {
        !matches!(
            lookahead,
            TokenKind::PackageKw
                | TokenKind::ImportKw
                | TokenKind::ValKw
                | TokenKind::VarKw
                | TokenKind::FunKw
                | TokenKind::ClassKw
                | TokenKind::DataKw
                | TokenKind::ElseKw
                | TokenKind::WhenKw
                | TokenKind::ForKw
                | TokenKind::WhileKw
                | TokenKind::DoKw
                | TokenKind::ReturnKw
                | TokenKind::ThrowKw
                | TokenKind::BreakKw
                | TokenKind::ContinueKw
                | TokenKind::Semicolon
                | TokenKind::RightBrace
                | TokenKind::RightParen
                | TokenKind::RightBracket
                | TokenKind::Comma
                | TokenKind::Eof
        )
    }

    fn parse(&self, ctx: &mut ParserContext<'_>) -> bool {
        let start = ctx.position();
        if ctx.parse_expression_until(
            &[
                TokenKind::Semicolon,
                TokenKind::Newline,
                TokenKind::RightBrace,
            ],
            true,
        ) {
            true
        } else {
            ctx.recover_statement("式ステートメントの解析に失敗しました", start);
            true
        }
    }
}
