use crate::parser::context::ParserContext;
use crate::syntax::{SyntaxKind, TokenKind};

use super::StatementStrategy;

/// `val`/`var` ステートメント戦略。
pub(crate) struct VariableStrategy {
    name: &'static str,
    keyword: TokenKind,
    node_kind: SyntaxKind,
    require_initializer: bool,
}

pub(crate) static VAL_STRATEGY: VariableStrategy = VariableStrategy {
    name: "val",
    keyword: TokenKind::ValKw,
    node_kind: SyntaxKind::ValDeclaration,
    require_initializer: true,
};

pub(crate) static VAR_STRATEGY: VariableStrategy = VariableStrategy {
    name: "var",
    keyword: TokenKind::VarKw,
    node_kind: SyntaxKind::VarDeclaration,
    require_initializer: false,
};

impl StatementStrategy for VariableStrategy {
    fn name(&self) -> &'static str {
        self.name
    }

    fn matches(&self, ctx: &ParserContext<'_>, lookahead: TokenKind) -> bool {
        let _ = ctx;
        lookahead == self.keyword
    }

    fn parse(&self, ctx: &mut ParserContext<'_>) -> bool {
        let start = ctx.position();
        ctx.consume_trivia();
        ctx.start_node(self.node_kind);
        ctx.bump_expected(self.keyword, "宣言キーワードが必要です");

        if !ctx.parse_binding_pattern() {
            ctx.recover_statement("バインディング名が必要です", start);
            ctx.finish_node();
            return true;
        }

        ctx.parse_optional_type_annotation();
        ctx.parse_initializer_clause(self.require_initializer);
        ctx.finish_node();
        true
    }
}
