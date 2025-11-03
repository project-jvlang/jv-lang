use crate::parser::context::ParserContext;
use crate::syntax::{SyntaxKind, TokenKind};
use jv_lexer::TokenType;

use super::StatementStrategy;

/// `test` 戦略。
pub(crate) struct TestStrategy;

pub(crate) static TEST_STRATEGY: TestStrategy = TestStrategy;

impl StatementStrategy for TestStrategy {
    fn name(&self) -> &'static str {
        "test"
    }

    fn matches(&self, ctx: &ParserContext<'_>, lookahead: TokenKind) -> bool {
        if lookahead != TokenKind::Identifier {
            return false;
        }

        let Some(token) = ctx.peek_significant_token() else {
            return false;
        };

        matches!(
            &token.token_type,
            TokenType::Identifier(name) if name == "test"
        )
    }

    fn parse(&self, ctx: &mut ParserContext<'_>) -> bool {
        let start = ctx.position();
        ctx.consume_trivia();
        ctx.start_node(SyntaxKind::TestDeclaration);

        parse_annotation_list(ctx);

        if !consume_test_keyword(ctx) {
            ctx.finish_node();
            return true;
        }

        ctx.consume_trivia();
        if !ctx.bump_expected(
            TokenKind::StringLiteral,
            "テスト名の文字列リテラルが必要です",
        ) {
            ctx.finish_node();
            return true;
        }

        parse_test_dataset(ctx);
        parse_test_parameter_list(ctx);

        if !ctx.parse_block() {
            ctx.recover_statement("テスト本体のブロックが必要です", start);
        }

        ctx.finish_node();
        true
    }
}

fn consume_test_keyword(ctx: &mut ParserContext<'_>) -> bool {
    ctx.consume_trivia();
    if ctx.peek_significant_kind() != Some(TokenKind::Identifier) {
        return false;
    }

    let Some(token) = ctx.peek_significant_token() else {
        return false;
    };

    let TokenType::Identifier(name) = &token.token_type else {
        return false;
    };

    if name != "test" {
        return false;
    }

    ctx.bump_raw();
    true
}

fn parse_annotation_list(ctx: &mut ParserContext<'_>) -> bool {
    let mut started = false;
    let mut parsed_any = false;

    loop {
        ctx.consume_trivia();
        if ctx.peek_significant_kind() != Some(TokenKind::At) {
            break;
        }

        if !started {
            ctx.start_node(SyntaxKind::AnnotationList);
            started = true;
        }

        if !parse_annotation(ctx) {
            break;
        }
        parsed_any = true;
    }

    if started {
        ctx.finish_node();
    }

    parsed_any
}

fn parse_annotation(ctx: &mut ParserContext<'_>) -> bool {
    let start = ctx.position();
    ctx.start_node(SyntaxKind::Annotation);

    if !ctx.bump_expected(TokenKind::At, "`@` が必要です") {
        ctx.finish_node();
        return false;
    }

    if !parse_annotation_name(ctx) {
        ctx.finish_node();
        ctx.recover_statement("アノテーション名が必要です", start);
        return false;
    }

    parse_annotation_argument_list(ctx);

    ctx.finish_node();
    true
}

fn parse_annotation_name(ctx: &mut ParserContext<'_>) -> bool {
    ctx.consume_trivia();
    if ctx.peek_significant_kind() != Some(TokenKind::Identifier) {
        return false;
    }

    ctx.start_node(SyntaxKind::QualifiedName);
    loop {
        ctx.start_node(SyntaxKind::QualifiedNameSegment);
        ctx.bump_raw();
        ctx.finish_node();

        ctx.consume_trivia();
        if ctx.peek_significant_kind() == Some(TokenKind::Dot) {
            ctx.bump_raw();
            ctx.consume_trivia();
            if ctx.peek_significant_kind() == Some(TokenKind::Identifier) {
                continue;
            }
        }
        break;
    }
    ctx.finish_node();
    true
}

fn parse_annotation_argument_list(ctx: &mut ParserContext<'_>) -> bool {
    ctx.consume_trivia();
    if ctx.peek_significant_kind() != Some(TokenKind::LeftParen) {
        return false;
    }

    let start = ctx.position();
    ctx.start_node(SyntaxKind::AnnotationArgumentList);
    ctx.bump_raw();

    loop {
        ctx.consume_trivia();
        match ctx.peek_significant_kind() {
            Some(TokenKind::RightParen) => {
                ctx.bump_raw();
                break;
            }
            Some(TokenKind::Eof) | None => {
                ctx.finish_node();
                ctx.recover_statement("')' が必要です", start);
                return true;
            }
            _ => {}
        }

        if !parse_annotation_argument(ctx) {
            ctx.recover_statement("アノテーション引数の解析に失敗しました", start);
            break;
        }

        ctx.consume_trivia();
        match ctx.peek_significant_kind() {
            Some(TokenKind::Comma) | Some(TokenKind::LayoutComma) => {
                ctx.bump_raw();
            }
            Some(TokenKind::RightParen) => {}
            Some(TokenKind::Eof) | None => {
                ctx.recover_statement("アノテーション引数の区切りが必要です", start);
                break;
            }
            _ => {}
        }
    }

    ctx.finish_node();
    true
}

fn parse_annotation_argument(ctx: &mut ParserContext<'_>) -> bool {
    ctx.start_node(SyntaxKind::AnnotationArgument);
    ctx.consume_trivia();

    let is_named = ctx.peek_significant_kind() == Some(TokenKind::Identifier)
        && ctx
            .peek_significant_kind_n(1)
            .map(|(_, kind)| kind == TokenKind::Assign)
            .unwrap_or(false);

    if is_named {
        ctx.bump_raw();
        ctx.consume_trivia();
        ctx.bump_raw();
    }

    let parsed = ctx.parse_expression_until(
        &[
            TokenKind::Comma,
            TokenKind::LayoutComma,
            TokenKind::RightParen,
        ],
        false,
    );

    ctx.finish_node();
    parsed
}

fn parse_test_dataset(ctx: &mut ParserContext<'_>) -> bool {
    ctx.consume_trivia();
    if ctx.peek_significant_kind() != Some(TokenKind::LeftBracket) {
        return false;
    }

    let start = ctx.position();
    ctx.start_node(SyntaxKind::TestDataset);
    ctx.bump_raw();

    loop {
        ctx.consume_trivia();
        match ctx.peek_significant_kind() {
            Some(TokenKind::RightBracket) => {
                ctx.bump_raw();
                break;
            }
            Some(TokenKind::Eof) | None => {
                ctx.finish_node();
                ctx.recover_statement("データセットを閉じる ']' が必要です", start);
                return true;
            }
            _ => {}
        }

        if !parse_dataset_row(ctx) {
            ctx.recover_statement("データセット行の解析に失敗しました", start);
            break;
        }

        ctx.consume_trivia();
        match ctx.peek_significant_kind() {
            Some(TokenKind::Comma) | Some(TokenKind::LayoutComma) => {
                ctx.bump_raw();
            }
            Some(TokenKind::RightBracket) => {}
            Some(TokenKind::Eof) | None => {
                ctx.recover_statement("データセット行の区切りが必要です", start);
                break;
            }
            _ => {}
        }
    }

    ctx.finish_node();
    true
}

fn parse_dataset_row(ctx: &mut ParserContext<'_>) -> bool {
    ctx.start_node(SyntaxKind::TestDatasetRow);
    let parsed = if ctx.peek_significant_kind() == Some(TokenKind::At) {
        parse_annotation(ctx)
    } else {
        ctx.parse_expression_until(
            &[
                TokenKind::Comma,
                TokenKind::LayoutComma,
                TokenKind::RightBracket,
            ],
            false,
        )
    };
    ctx.finish_node();
    parsed
}

fn parse_test_parameter_list(ctx: &mut ParserContext<'_>) -> bool {
    ctx.consume_trivia();
    if ctx.peek_significant_kind() != Some(TokenKind::LeftParen) {
        return false;
    }

    let start = ctx.position();
    ctx.start_node(SyntaxKind::TestParameterList);
    ctx.bump_raw();

    loop {
        ctx.consume_trivia();
        match ctx.peek_significant_kind() {
            Some(TokenKind::RightParen) => {
                ctx.bump_raw();
                break;
            }
            Some(TokenKind::Eof) | None => {
                ctx.finish_node();
                ctx.recover_statement("パラメータリストを閉じる ')' が必要です", start);
                return true;
            }
            _ => {}
        }

        ctx.start_node(SyntaxKind::TestParameter);
        if !ctx.parse_binding_pattern() {
            ctx.finish_node();
            ctx.recover_statement("テストパラメータのバインディングが必要です", start);
            break;
        }
        ctx.parse_optional_type_annotation();
        ctx.finish_node();

        ctx.consume_trivia();
        match ctx.peek_significant_kind() {
            Some(TokenKind::Comma) | Some(TokenKind::LayoutComma) => {
                ctx.bump_raw();
            }
            Some(TokenKind::RightParen) => {}
            Some(TokenKind::Eof) | None => {
                ctx.recover_statement("テストパラメータの区切りが必要です", start);
                break;
            }
            _ => {}
        }
    }

    ctx.finish_node();
    true
}
