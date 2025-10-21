use crate::parser::context::ParserContext;
use crate::syntax::{SyntaxKind, TokenKind};

use super::StatementStrategy;

/// `class` / `data` 戦略。
pub(crate) struct ClassStrategy;

pub(crate) static CLASS_STRATEGY: ClassStrategy = ClassStrategy;

impl StatementStrategy for ClassStrategy {
    fn name(&self) -> &'static str {
        "class"
    }

    fn matches(&self, ctx: &ParserContext<'_>, lookahead: TokenKind) -> bool {
        let _ = ctx;
        matches!(lookahead, TokenKind::ClassKw | TokenKind::DataKw)
    }

    fn parse(&self, ctx: &mut ParserContext<'_>) -> bool {
        let start = ctx.position();
        ctx.consume_trivia();
        ctx.start_node(SyntaxKind::ClassDeclaration);

        let mut saw_keyword = false;
        if ctx.bump_if(TokenKind::ClassKw) {
            saw_keyword = true;
        } else if ctx.bump_if(TokenKind::DataKw) {
            saw_keyword = true;
            let _ = ctx.bump_if(TokenKind::ClassKw);
        }

        if !saw_keyword {
            ctx.bump_expected(TokenKind::ClassKw, "`class` もしくは `data` が必要です");
        }

        if !ctx.bump_expected(TokenKind::Identifier, "クラス名が必要です") {
            ctx.recover_statement("クラス名が必要です", start);
            ctx.finish_node();
            return true;
        }

        parse_primary_constructor(ctx);

        if !ctx.parse_class_body() {
            ctx.recover_statement("クラスボディが必要です", start);
        }

        ctx.finish_node();
        true
    }
}

fn parse_primary_constructor(ctx: &mut ParserContext<'_>) {
    ctx.consume_trivia();
    if ctx.peek_significant_kind() != Some(TokenKind::LeftParen) {
        return;
    }

    ctx.start_node(SyntaxKind::FunctionParameterList);
    ctx.bump_raw(); // (

    loop {
        ctx.consume_trivia();
        if ctx.peek_significant_kind() == Some(TokenKind::RightParen) {
            ctx.bump_raw();
            break;
        }

        let param_start = ctx.position();
        ctx.start_node(SyntaxKind::FunctionParameter);
        if !ctx.parse_binding_pattern() {
            ctx.recover_statement("コンストラクタパラメータ名が必要です", param_start);
            ctx.finish_node();
            break;
        }
        ctx.parse_optional_type_annotation();
        ctx.finish_node();

        ctx.consume_trivia();
        match ctx.peek_significant_kind() {
            Some(TokenKind::Comma) => {
                ctx.bump_raw();
            }
            Some(TokenKind::RightParen) => {
                ctx.bump_raw();
                break;
            }
            Some(TokenKind::Eof) | None => {
                ctx.recover_statement("')' が必要です", param_start);
                break;
            }
            _ => {
                ctx.recover_statement("パラメータ区切りが必要です", param_start);
                break;
            }
        }
    }

    ctx.finish_node();
}
