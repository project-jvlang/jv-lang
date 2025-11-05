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
        } else if let Some((index, TokenKind::Identifier)) = ctx.peek_significant_kind_n(0) {
            let token = &ctx.tokens[index];
            if token.lexeme == "as" {
                ctx.start_node(SyntaxKind::ImportClause);
                ctx.bump_raw(); // 'as'
                ctx.consume_trivia();
                ctx.start_node(SyntaxKind::ImportAlias);
                if !ctx.bump_expected(TokenKind::Identifier, "import 別名が必要です") {
                    ctx.recover_statement("import 別名が必要です", start);
                    ctx.finish_node(); // ImportAlias
                    ctx.finish_node(); // ImportClause
                    ctx.finish_node();
                    return true;
                }
                ctx.finish_node(); // ImportAlias
                ctx.finish_node(); // ImportClause
            }
        }

        ctx.finish_node();
        true
    }
}
