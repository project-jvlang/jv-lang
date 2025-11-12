use crate::parser::context::ParserContext;
use crate::syntax::{SyntaxKind, TokenKind};

use super::StatementStrategy;

/// `import` 戦略。
pub(crate) struct ImportStrategy;

pub(crate) static IMPORT_STRATEGY: ImportStrategy = ImportStrategy;

impl StatementStrategy for ImportStrategy {
    fn name(&self) -> &'static str {
        "import"
    }

    fn matches(&self, ctx: &ParserContext<'_>, lookahead: TokenKind) -> bool {
        let _ = ctx;
        lookahead == TokenKind::ImportKw
    }

    fn parse(&self, ctx: &mut ParserContext<'_>) -> bool {
        let start = ctx.position();
        ctx.consume_trivia();
        ctx.start_node(SyntaxKind::ImportDeclaration);
        ctx.bump_expected(TokenKind::ImportKw, "`import` キーワードが必要です");

        if !ctx.parse_qualified_name(SyntaxKind::ImportPath) {
            ctx.recover_statement("import パスが必要です", start);
            ctx.finish_node();
            return true;
        }

        ctx.consume_trivia();
        if ctx.peek_significant_kind() == Some(TokenKind::Dot) {
            if let Some((_, TokenKind::Star)) = ctx.peek_significant_kind_n(1) {
                ctx.start_node(SyntaxKind::ImportClause);
                ctx.start_node(SyntaxKind::ImportWildcard);
                ctx.bump_raw(); // dot
                ctx.bump_raw(); // star
                ctx.finish_node(); // ImportWildcard
                ctx.finish_node(); // ImportClause
            }
        } else if ctx.peek_significant_kind() == Some(TokenKind::Star) {
            ctx.start_node(SyntaxKind::ImportClause);
            ctx.start_node(SyntaxKind::ImportWildcard);
            ctx.bump_raw(); // star
            ctx.finish_node(); // ImportWildcard
            ctx.finish_node(); // ImportClause
        } else if let Some(token) = ctx.peek_significant_token_n(0) {
            if TokenKind::from_token(token) == TokenKind::Identifier
                && token.lexeme.eq_ignore_ascii_case("as")
            {
                ctx.start_node(SyntaxKind::ImportClause);
                ctx.bump_raw(); // alias keyword
                ctx.consume_inline_whitespace();
                if ctx.peek_significant_kind() == Some(TokenKind::Identifier) {
                    ctx.start_node(SyntaxKind::ImportAlias);
                    ctx.bump_raw(); // alias name
                    ctx.finish_node(); // ImportAlias
                } else {
                    ctx.report_error(
                        "import aliasには識別子を指定してください",
                        ctx.position(),
                        ctx.position(),
                    );
                }
                ctx.finish_node(); // ImportClause
            }
        }
        ctx.finish_node();
        true
    }
}
