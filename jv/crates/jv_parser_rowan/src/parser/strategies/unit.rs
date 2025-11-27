use crate::frontend::{
    DIAGNOSTIC_JV_UNIT_001_MISSING_SPACE, DIAGNOSTIC_JV_UNIT_003_DEPENDENCY_MISSING_RHS,
    DIAGNOSTIC_JV_UNIT_004_UNKNOWN_DIRECTIVE,
};
use crate::parser::context::ParserContext;
use crate::syntax::{SyntaxKind, TokenKind};
use jv_lexer::TokenType;

use super::StatementStrategy;

/// `@ 単位系(基底型) 名称 { ... }` を解釈するステートメント戦略。
pub(crate) struct UnitTypeDefinitionStrategy;

pub(crate) static UNIT_TYPE_DEF_STRATEGY: UnitTypeDefinitionStrategy = UnitTypeDefinitionStrategy;

impl StatementStrategy for UnitTypeDefinitionStrategy {
    fn name(&self) -> &'static str {
        "unit-type-definition"
    }

    fn matches(&self, ctx: &ParserContext<'_>, lookahead: TokenKind) -> bool {
        if lookahead != TokenKind::At {
            return false;
        }

        let Some((_, TokenKind::Identifier)) = ctx.peek_significant_kind_n(1) else {
            return false;
        };
        let Some((left_paren_index, TokenKind::LeftParen)) = ctx.peek_significant_kind_n(2) else {
            return false;
        };

        let mut depth = 0usize;
        let mut index = left_paren_index + 1;
        while index < ctx.tokens.len() {
            let token = &ctx.tokens[index];
            let kind = TokenKind::from_token(token);
            match kind {
                TokenKind::LeftParen => depth = depth.saturating_add(1),
                TokenKind::RightParen => {
                    if depth == 0 {
                        if let Some((_, next_kind)) =
                            Self::next_significant_kind_from(ctx, index + 1)
                        {
                            return matches!(next_kind, TokenKind::Identifier | TokenKind::Unknown);
                        }
                        return false;
                    }
                    depth -= 1;
                }
                TokenKind::Eof => return false,
                _ => {}
            }
            index += 1;
        }

        false
    }

    fn parse(&self, ctx: &mut ParserContext<'_>) -> bool {
        let start = ctx.position();
        ctx.start_node(SyntaxKind::UnitTypeDefinition);

        if !self.parse_header(ctx, start) {
            ctx.finish_node();
            ctx.recover_statement("単位定義のヘッダー解析に失敗しました", start);
            return true;
        }

        if !self.parse_body(ctx, start) {
            ctx.finish_node();
            ctx.recover_statement("単位定義の本体解析に失敗しました", start);
            return true;
        }

        ctx.finish_node();
        true
    }
}

impl UnitTypeDefinitionStrategy {
    fn next_significant_kind_from(
        ctx: &ParserContext<'_>,
        start_index: usize,
    ) -> Option<(usize, TokenKind)> {
        let mut index = start_index;
        while index < ctx.tokens.len() {
            let token = &ctx.tokens[index];
            let kind = TokenKind::from_token(token);
            if kind.is_trivia() {
                index += 1;
                continue;
            }
            return Some((index, kind));
        }
        None
    }

    fn bump_unit_symbol(&self, ctx: &mut ParserContext<'_>) -> bool {
        let Some(token) = ctx.peek_significant_token_n(0) else {
            ctx.report_error(
                "単位名を識別子で指定してください",
                ctx.position(),
                ctx.position(),
            );
            return false;
        };
        let kind = TokenKind::from_token(token);
        if kind == TokenKind::Identifier || matches!(token.token_type, TokenType::Invalid(_)) {
            ctx.bump_raw();
            true
        } else {
            ctx.report_error(
                "単位名を識別子で指定してください",
                ctx.position(),
                ctx.position(),
            );
            false
        }
    }

    fn parse_header(&self, ctx: &mut ParserContext<'_>, start: usize) -> bool {
        ctx.start_node(SyntaxKind::UnitHeader);

        if !ctx.bump_expected(TokenKind::At, "単位定義は `@` で始まります") {
            ctx.finish_node();
            return false;
        }

        let has_space = ctx.consume_inline_whitespace();
        if !has_space {
            ctx.report_error(DIAGNOSTIC_JV_UNIT_001_MISSING_SPACE, start, ctx.position());
        }

        ctx.start_node(SyntaxKind::UnitCategory);
        if !ctx.bump_expected(
            TokenKind::Identifier,
            "単位カテゴリを識別子で指定してください",
        ) {
            ctx.finish_node(); // UnitCategory
            ctx.finish_node(); // UnitHeader
            return false;
        }
        ctx.finish_node(); // UnitCategory

        ctx.consume_inline_whitespace();

        ctx.start_node(SyntaxKind::UnitBaseType);
        if !ctx.bump_expected(TokenKind::LeftParen, "基底型は丸括弧で囲んでください")
        {
            ctx.finish_node(); // UnitBaseType
            ctx.finish_node(); // UnitHeader
            return false;
        }

        let expr_start = ctx.position();
        ctx.start_node(SyntaxKind::Expression);
        ctx.parse_type_expression_until(&[TokenKind::RightParen]);
        let consumed = ctx.position() > expr_start;
        ctx.finish_node(); // Expression
        if !consumed {
            ctx.report_error("基底型を表す式が必要です", expr_start, ctx.position());
        }

        if !ctx.bump_expected(TokenKind::RightParen, "基底型の丸括弧が閉じていません")
        {
            ctx.finish_node(); // UnitBaseType
            ctx.finish_node(); // UnitHeader
            return false;
        }
        ctx.finish_node(); // UnitBaseType

        ctx.consume_inline_whitespace();

        ctx.start_node(SyntaxKind::UnitName);
        if !self.bump_unit_symbol(ctx) {
            ctx.finish_node(); // UnitName
            ctx.finish_node(); // UnitHeader
            return false;
        }
        ctx.finish_node(); // UnitName

        let _ = ctx.consume_inline_whitespace();
        if ctx.peek_significant_kind() == Some(TokenKind::Bang) {
            ctx.start_node(SyntaxKind::UnitDefaultMarker);
            ctx.bump_raw();
            ctx.finish_node();
        }

        ctx.finish_node();
        true
    }

    fn parse_body(&self, ctx: &mut ParserContext<'_>, start: usize) -> bool {
        ctx.consume_whitespace();
        ctx.start_node(SyntaxKind::UnitBody);

        if !ctx.bump_expected(TokenKind::LeftBrace, "単位定義の本体は `{` で始まります")
        {
            ctx.finish_node();
            return false;
        }

        let mut ok = true;
        loop {
            if ctx.consume_whitespace() {
                continue;
            }

            match ctx.peek_significant_kind() {
                Some(TokenKind::RightBrace) => {
                    ctx.bump_raw();
                    break;
                }
                Some(TokenKind::Identifier) => {
                    if !self.parse_dependency_or_relation(ctx) {
                        ok = false;
                    }
                }
                Some(TokenKind::At) => {
                    if !self.parse_conversion_block(ctx) {
                        ok = false;
                    }
                }
                Some(TokenKind::Eof) | None => {
                    ctx.report_error("単位定義の本体が途中で終了しました", start, ctx.position());
                    ok = false;
                    break;
                }
                _ => {
                    let error_start = ctx.position();
                    ctx.report_error(
                        "単位定義の本体で解釈できないトークンです",
                        error_start,
                        error_start,
                    );
                    ctx.bump_raw();
                    ok = false;
                }
            }
        }

        ctx.finish_node();
        ok
    }

    fn parse_dependency_or_relation(&self, ctx: &mut ParserContext<'_>) -> bool {
        let start = ctx.position();
        match ctx.peek_significant_kind_n(1).map(|(_, kind)| kind) {
            Some(TokenKind::Colon) => self.parse_dependency(ctx, start),
            Some(TokenKind::Arrow) => self.parse_relation(ctx),
            _ => {
                ctx.report_error(
                    "単位定義の本体では `:=` か `->` を使用してください",
                    start,
                    start,
                );
                ctx.bump_raw();
                false
            }
        }
    }

    fn parse_dependency(&self, ctx: &mut ParserContext<'_>, start: usize) -> bool {
        ctx.start_node(SyntaxKind::UnitDependency);
        ctx.bump_raw(); // 識別子
        ctx.consume_whitespace();

        let colon_ok =
            ctx.bump_expected(TokenKind::Colon, "単位依存定義には `:=` を使用してください");
        ctx.consume_whitespace();
        let assign_ok = ctx.bump_expected(
            TokenKind::Assign,
            "単位依存定義には `:=` を使用してください",
        );
        ctx.consume_whitespace();

        let has_expression = ctx.parse_expression_until(
            &[
                TokenKind::RightBrace,
                TokenKind::Newline,
                TokenKind::Semicolon,
            ],
            true,
        );
        if !has_expression {
            ctx.report_error(
                DIAGNOSTIC_JV_UNIT_003_DEPENDENCY_MISSING_RHS,
                start,
                ctx.position(),
            );
        }

        ctx.finish_node();
        colon_ok && assign_ok && has_expression
    }

    fn parse_relation(&self, ctx: &mut ParserContext<'_>) -> bool {
        ctx.start_node(SyntaxKind::UnitRelation);
        ctx.bump_raw(); // 左辺識別子
        ctx.consume_whitespace();

        let arrow_ok =
            ctx.bump_expected(TokenKind::Arrow, "単位間の関係には `->` を使用してください");
        ctx.consume_whitespace();
        let rhs_ok = ctx.bump_expected(
            TokenKind::Identifier,
            "単位間の関係には右辺の単位名が必要です",
        );

        ctx.finish_node();
        arrow_ok && rhs_ok
    }

    fn parse_conversion_block(&self, ctx: &mut ParserContext<'_>) -> bool {
        let start = ctx.position();
        let directive = ctx
            .peek_significant_token_n(1)
            .filter(|token| TokenKind::from_token(token) == TokenKind::Identifier)
            .map(|token| token.lexeme.as_str());

        let node_kind = match directive {
            Some("ReverseConversion") => SyntaxKind::UnitReverseConversionBlock,
            _ => SyntaxKind::UnitConversionBlock,
        };

        ctx.start_node(node_kind);
        ctx.bump_raw(); // '@'
        let _ = ctx.consume_inline_whitespace();

        let ident_token = if ctx.peek_significant_kind() == Some(TokenKind::Identifier) {
            ctx.bump_raw()
        } else {
            ctx.report_error(
                "単位変換ディレクティブ名を指定してください",
                start,
                ctx.position(),
            );
            None
        };

        if let Some(token) = ident_token {
            let name = token.lexeme.as_str();
            if name != "Conversion" && name != "ReverseConversion" {
                ctx.report_error(
                    DIAGNOSTIC_JV_UNIT_004_UNKNOWN_DIRECTIVE,
                    start,
                    ctx.position(),
                );
            }
        } else {
            ctx.finish_node();
            return false;
        }

        let block_ok = ctx.parse_block();
        if !block_ok {
            ctx.report_error(
                "単位変換ブロックは `{}` で囲む必要があります",
                start,
                ctx.position(),
            );
        }

        ctx.finish_node();
        block_ok
    }
}
