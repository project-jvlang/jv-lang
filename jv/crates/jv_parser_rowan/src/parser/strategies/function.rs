use crate::parser::context::ParserContext;
use crate::syntax::{SyntaxKind, TokenKind};

use super::StatementStrategy;

/// `fun` 戦略。
pub(crate) struct FunctionStrategy;

pub(crate) static FUNCTION_STRATEGY: FunctionStrategy = FunctionStrategy;

impl StatementStrategy for FunctionStrategy {
    fn name(&self) -> &'static str {
        "fun"
    }

    fn matches(&self, ctx: &ParserContext<'_>, lookahead: TokenKind) -> bool {
        let _ = ctx;
        lookahead == TokenKind::FunKw
    }

    fn parse(&self, ctx: &mut ParserContext<'_>) -> bool {
        let start = ctx.position();
        ctx.consume_trivia();
        ctx.start_node(SyntaxKind::FunctionDeclaration);
        ctx.bump_expected(TokenKind::FunKw, "`fun` キーワードが必要です");

        if !ctx.bump_expected(TokenKind::Identifier, "関数名が必要です") {
            ctx.recover_statement("関数名が必要です", start);
            ctx.finish_node();
            return true;
        }

        if !parse_parameter_list(ctx) {
            ctx.recover_statement("パラメータリストが必要です", start);
        }

        parse_return_type(ctx);

        if ctx.bump_if(TokenKind::FatArrow) {
            ctx.parse_expression_until(
                &[
                    TokenKind::Semicolon,
                    TokenKind::Newline,
                    TokenKind::RightBrace,
                ],
                true,
            );
        } else if !ctx.parse_block() {
            ctx.recover_statement("関数本体が必要です", start);
        }

        ctx.finish_node();
        true
    }
}

fn parse_parameter_list(ctx: &mut ParserContext<'_>) -> bool {
    ctx.consume_trivia();
    if ctx.peek_significant_kind() != Some(TokenKind::LeftParen) {
        return false;
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
        parse_parameter_modifier(ctx);

        if !ctx.parse_binding_pattern() {
            ctx.recover_statement("パラメータ名が必要です", param_start);
            ctx.finish_node();
            break;
        }

        ctx.parse_optional_type_annotation();
        ctx.finish_node(); // parameter

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

    ctx.finish_node(); // parameter list
    true
}

fn parse_parameter_modifier(ctx: &mut ParserContext<'_>) {
    ctx.consume_trivia();
    let modifier = match ctx.peek_significant_kind() {
        Some(TokenKind::ValKw) | Some(TokenKind::VarKw) => ctx.peek_significant_kind(),
        _ => None,
    };

    if let Some(kind) = modifier {
        ctx.start_node(SyntaxKind::ParameterModifierList);
        ctx.start_node(SyntaxKind::ParameterModifier);
        ctx.bump_raw(); // val/var
        ctx.finish_node();
        ctx.finish_node();
        if kind == TokenKind::VarKw {
            // `var` パラメータは追加で `var` 以外の修飾も受け付けない。
        }
    }
}

fn parse_return_type(ctx: &mut ParserContext<'_>) {
    ctx.consume_trivia();
    if ctx.peek_significant_kind() != Some(TokenKind::Colon) {
        return;
    }

    ctx.start_node(SyntaxKind::FunctionReturnType);
    ctx.bump_raw(); // :
    ctx.parse_expression_until(
        &[
            TokenKind::FatArrow,
            TokenKind::LeftBrace,
            TokenKind::Semicolon,
            TokenKind::Newline,
        ],
        false,
    );
    ctx.finish_node();
}
