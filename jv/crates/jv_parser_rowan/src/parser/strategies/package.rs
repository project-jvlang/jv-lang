use crate::parser::context::ParserContext;
use crate::syntax::{SyntaxKind, TokenKind};

use super::StatementStrategy;

/// `package` 戦略。
pub(crate) struct PackageStrategy;

pub(crate) static PACKAGE_STRATEGY: PackageStrategy = PackageStrategy;

impl StatementStrategy for PackageStrategy {
    fn name(&self) -> &'static str {
        "package"
    }

    fn matches(&self, ctx: &ParserContext<'_>, lookahead: TokenKind) -> bool {
        let _ = ctx;
        lookahead == TokenKind::PackageKw
    }

    fn parse(&self, ctx: &mut ParserContext<'_>) -> bool {
        let start = ctx.position();
        ctx.consume_trivia();
        ctx.start_node(SyntaxKind::PackageDeclaration);
        ctx.bump_expected(TokenKind::PackageKw, "`package` キーワードが必要です");
        if !ctx.parse_qualified_name(SyntaxKind::PackageName) {
            ctx.recover_statement("package 名が必要です", start);
        }
        ctx.finish_node();
        true
    }
}
