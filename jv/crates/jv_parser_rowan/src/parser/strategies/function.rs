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

        parse_type_parameter_list(ctx);
        let _ = parse_function_receiver(ctx);

        if !ctx.bump_expected(TokenKind::Identifier, "関数名が必要です") {
            ctx.recover_statement("関数名が必要です", start);
            ctx.finish_node();
            return true;
        }

        if !parse_parameter_list(ctx) {
            ctx.recover_statement("パラメータリストが必要です", start);
        }

        parse_return_type(ctx);
        parse_where_clause(ctx);

        if ctx.bump_if(TokenKind::Assign) {
            ctx.parse_expression_until(
                &[
                    TokenKind::Semicolon,
                    TokenKind::Newline,
                    TokenKind::RightBrace,
                ],
                true,
            );
        } else {
            ctx.consume_trivia();
            if ctx.peek_significant_kind() == Some(TokenKind::Arrow) {
                let arrow_start = ctx.position();
                ctx.bump_raw();
                ctx.report_error(
                    "関数宣言での `->` 構文は廃止されました。`fun name(...) = expr` を使用してください",
                    arrow_start,
                    ctx.position(),
                );
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
        }

        ctx.finish_node();
        true
    }
}

fn parse_type_parameter_list(ctx: &mut ParserContext<'_>) -> bool {
    ctx.consume_trivia();
    if ctx.peek_significant_kind() != Some(TokenKind::Less) {
        return false;
    }

    ctx.start_node(SyntaxKind::TypeParameterList);
    ctx.bump_raw(); // '<'

    loop {
        ctx.consume_trivia();
        if ctx.peek_significant_kind() == Some(TokenKind::Greater) {
            ctx.bump_raw();
            break;
        }

        let param_start = ctx.position();
        ctx.start_node(SyntaxKind::TypeParameter);

        if !ctx.bump_expected(TokenKind::Identifier, "型パラメータ名が必要です") {
            ctx.recover_statement("型パラメータ名が必要です", param_start);
            ctx.finish_node();
            break;
        }

        ctx.consume_trivia();
        if ctx.peek_significant_kind() == Some(TokenKind::Colon) {
            ctx.start_node(SyntaxKind::TypeAnnotation);
            ctx.bump_raw();
            ctx.parse_type_expression_until(&[TokenKind::Comma, TokenKind::Greater]);
            ctx.finish_node();
        }

        ctx.finish_node();

        ctx.consume_trivia();
        match ctx.peek_significant_kind() {
            Some(TokenKind::Comma) | Some(TokenKind::LayoutComma) => {
                ctx.bump_raw();
            }
            Some(TokenKind::Greater) => {
                ctx.bump_raw();
                break;
            }
            Some(TokenKind::Eof) | None => {
                ctx.recover_statement("`>` が必要です", param_start);
                break;
            }
            _ => {
                ctx.recover_statement("型パラメータ区切りが必要です", param_start);
                break;
            }
        }
    }

    ctx.finish_node();
    true
}

fn parse_function_receiver(ctx: &mut ParserContext<'_>) -> bool {
    ctx.consume_trivia();
    let Some((_, first_kind)) = ctx.peek_significant_kind_n(0) else {
        return false;
    };

    if first_kind != TokenKind::Identifier {
        return false;
    }

    let mut offset = 0usize;
    let mut generic_depth = 0usize;
    let mut dot_seen: Option<usize> = None;

    loop {
        let Some((_, kind)) = ctx.peek_significant_kind_n(offset) else {
            break;
        };

        match kind {
            TokenKind::LeftParen | TokenKind::Assign | TokenKind::Arrow => break,
            TokenKind::Less => generic_depth = generic_depth.saturating_add(1),
            TokenKind::Greater => {
                if generic_depth == 0 {
                    break;
                }
                generic_depth = generic_depth.saturating_sub(1);
            }
            TokenKind::Dot if generic_depth == 0 => {
                if let Some((_, next_kind)) = ctx.peek_significant_kind_n(offset + 1) {
                    if next_kind == TokenKind::Identifier {
                        dot_seen = Some(offset);
                    }
                }
                break;
            }
            _ => {}
        }

        offset += 1;
    }

    if dot_seen.is_none() {
        return false;
    }

    ctx.start_node(SyntaxKind::TypeAnnotation);
    ctx.parse_type_expression_until(&[TokenKind::Dot]);
    ctx.finish_node();
    ctx.bump_expected(TokenKind::Dot, "関数名の前に'.'が必要です");
    true
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
        ctx.parse_parameter_modifiers();

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

fn parse_return_type(ctx: &mut ParserContext<'_>) {
    ctx.consume_trivia();
    if ctx.peek_significant_kind() != Some(TokenKind::Colon) {
        return;
    }

    ctx.start_node(SyntaxKind::FunctionReturnType);
    ctx.bump_raw(); // :
    ctx.parse_expression_until(
        &[
            TokenKind::Assign,
            TokenKind::Arrow,
            TokenKind::LeftBrace,
            TokenKind::Semicolon,
            TokenKind::Newline,
        ],
        false,
    );
    ctx.finish_node();
}

fn parse_where_clause(ctx: &mut ParserContext<'_>) -> bool {
    ctx.consume_trivia();
    if ctx.peek_significant_kind() != Some(TokenKind::WhereKw) {
        return false;
    }

    ctx.start_node(SyntaxKind::WhereClause);
    ctx.bump_raw(); // where

    loop {
        ctx.consume_trivia();
        let predicate_start = ctx.position();

        if ctx.peek_significant_kind() != Some(TokenKind::Identifier) {
            ctx.recover_statement("`where` 句には識別子が必要です", predicate_start);
            break;
        }

        ctx.start_node(SyntaxKind::WherePredicate);
        ctx.bump_raw(); // identifier

        ctx.consume_trivia();
        if !ctx.bump_expected(TokenKind::Colon, "`where` 句には ':' が必要です") {
            ctx.finish_node();
            break;
        }

        ctx.parse_expression_until(
            &[
                TokenKind::Comma,
                TokenKind::LayoutComma,
                TokenKind::LeftBrace,
                TokenKind::Assign,
                TokenKind::Semicolon,
                TokenKind::Newline,
            ],
            false,
        );
        ctx.finish_node();

        ctx.consume_trivia();
        match ctx.peek_significant_kind() {
            Some(TokenKind::Comma) | Some(TokenKind::LayoutComma) => {
                ctx.bump_raw();
                continue;
            }
            _ => break,
        }
    }

    ctx.finish_node();
    true
}
