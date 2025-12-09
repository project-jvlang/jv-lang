use crate::lexer::TokenKind;

use super::{ParserContext, StatementStrategy, SyntaxKind};

#[derive(Debug)]
struct ParamInfo {
    token_index: usize,
    is_var: bool,
}

pub struct ClassStrategy;
pub struct DataClassStrategy;

impl StatementStrategy for ClassStrategy {
    fn name(&self) -> &'static str {
        "class"
    }

    fn matches(&self, _ctx: &ParserContext, lookahead: TokenKind) -> bool {
        lookahead == TokenKind::Class
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        parse_class(ctx, SyntaxKind::ClassDeclaration, false)
    }
}

impl StatementStrategy for DataClassStrategy {
    fn name(&self) -> &'static str {
        "data-class"
    }

    fn matches(&self, _ctx: &ParserContext, lookahead: TokenKind) -> bool {
        lookahead == TokenKind::Data
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        parse_class(ctx, SyntaxKind::DataClassDeclaration, true)
    }
}

fn parse_class(ctx: &mut ParserContext, kind: SyntaxKind, is_data_class: bool) -> bool {
    ctx.start_node(kind);
    ctx.bump(); // class/data
    parse_modifiers(ctx);
    if ctx.peek_kind() == Some(TokenKind::Identifier) {
        ctx.start_node(SyntaxKind::Identifier);
        ctx.bump();
        ctx.finish_node();
    }
    // 型パラメータの簡易サポート: <T, U>
    if ctx.peek_kind() == Some(TokenKind::Less) {
        ctx.bump();
        while ctx.peek_kind() != Some(TokenKind::Greater) && !ctx.is_eof() {
            if ctx.peek_kind() == Some(TokenKind::Identifier) {
                ctx.bump();
            } else {
                ctx.error("型パラメータに識別子が必要です");
                ctx.bump();
            }
            if ctx.peek_kind() == Some(TokenKind::Comma) {
                ctx.bump();
                continue;
            } else {
                break;
            }
        }
        if ctx.peek_kind() == Some(TokenKind::Greater) {
            ctx.bump();
        } else {
            ctx.error("型パラメータリストを `>` で閉じてください");
        }
    }

    // コンストラクタパラメータ (簡易): ( [modifiers] pattern [: Type] [= default] {, ...} )
    let params = parse_primary_constructor(ctx);

    // 継承/実装 (簡易): : Base, Trait
    if ctx.peek_kind() == Some(TokenKind::Colon) {
        ctx.bump();
        loop {
            if ctx.peek_kind() == Some(TokenKind::Identifier) {
                ctx.bump();
            } else {
                ctx.error("継承/実装ターゲットが必要です");
                break;
            }
            if ctx.peek_kind() == Some(TokenKind::Comma) {
                ctx.bump();
                continue;
            }
            break;
        }
    }

    // メンバーはステートメント戦略を流用して解析する（フィールド/メソッド/ネストクラスをカバー）。
    if ctx.peek_kind() == Some(TokenKind::LeftBrace) {
        parse_class_body(ctx, is_data_class, &params);
    } else if is_data_class {
        // data class でボディがなくてもパラメータ由来のメンバーを生成する
        ctx.start_node(SyntaxKind::ClassBody);
        emit_synthesized_members(ctx, &params);
        ctx.finish_node();
    }
    ctx.finish_node();
    true
}

fn parse_class_body(ctx: &mut ParserContext, is_data_class: bool, params: &[ParamInfo]) {
    ctx.start_node(SyntaxKind::ClassBody);
    if ctx.peek_kind() == Some(TokenKind::LeftBrace) {
        ctx.bump(); // {
        if is_data_class {
            emit_synthesized_members(ctx, params);
        }
        while !ctx.is_eof() && ctx.peek_kind() != Some(TokenKind::RightBrace) {
            parse_modifiers(ctx);
            let look = ctx.peek_kind();
            let parsed = match look {
                Some(TokenKind::Val) => super::binding::ValStrategy.parse(ctx),
                Some(TokenKind::Var) => super::binding::VarStrategy.parse(ctx),
                Some(TokenKind::Fun) => super::function::FunctionStrategy.parse(ctx),
                Some(TokenKind::Class) => ClassStrategy.parse(ctx),
                Some(TokenKind::Data) => DataClassStrategy.parse(ctx),
                _ => {
                    ctx.error("クラスメンバーとして解釈できないトークンです");
                    ctx.bump();
                    false
                }
            };
            if !parsed && ctx.peek_kind() == Some(TokenKind::Semicolon) {
                ctx.bump();
            }
        }
        if ctx.peek_kind() == Some(TokenKind::RightBrace) {
            ctx.bump();
        } else {
            ctx.error("クラス本体を `}` で閉じてください");
        }
    } else {
        ctx.error("クラス本体の `{` が必要です");
    }
    ctx.finish_node();
}

fn parse_modifiers(ctx: &mut ParserContext) {
    const MODS: &[&str] = &[
        "public", "private", "protected", "internal", "open", "sealed", "abstract", "final",
        "override", "static", "mut", "ref",
    ];
    while let Some(tok) = ctx.current() {
        if tok.kind == TokenKind::Identifier && MODS.iter().any(|m| tok.lexeme == *m) {
            ctx.start_node(SyntaxKind::Modifier);
            ctx.bump();
            ctx.finish_node();
        } else {
            break;
        }
    }
}

fn parse_parameter_pattern(ctx: &mut ParserContext, params: &mut Vec<ParamInfo>, is_var: bool) {
    if ctx.peek_kind() == Some(TokenKind::LeftParen) {
        // デストラクト
        ctx.start_node(SyntaxKind::DestructuringPattern);
        ctx.bump();
        while ctx.peek_kind() != Some(TokenKind::RightParen) && !ctx.is_eof() {
            ctx.start_node(SyntaxKind::PatternElement);
            if ctx.peek_kind() == Some(TokenKind::Identifier) || ctx.peek_kind() == Some(TokenKind::Underscore) {
                ctx.bump();
            } else {
                ctx.error("パターン要素には識別子または _ が必要です");
                ctx.bump();
            }
            ctx.finish_node();
            if ctx.peek_kind() == Some(TokenKind::Comma) {
                ctx.bump();
                continue;
            } else {
                break;
            }
        }
        if ctx.peek_kind() == Some(TokenKind::RightParen) {
            ctx.bump();
        } else {
            ctx.error("パターンを `)` で閉じてください");
        }
        ctx.finish_node();
    } else if ctx.peek_kind() == Some(TokenKind::Identifier) {
        let idx = ctx.cursor;
        ctx.bump();
        params.push(ParamInfo {
            token_index: idx,
            is_var,
        });
    } else {
        ctx.error("コンストラクタパラメータ名が必要です");
        ctx.bump();
    }
}

fn parse_primary_constructor(ctx: &mut ParserContext) -> Vec<ParamInfo> {
    let mut params = Vec::new();
    if ctx.peek_kind() == Some(TokenKind::LeftParen) {
        ctx.start_node(SyntaxKind::ClassParameterList);
        ctx.bump(); // (
        while ctx.peek_kind() != Some(TokenKind::RightParen) && !ctx.is_eof() {
            ctx.start_node(SyntaxKind::ClassParameter);
            parse_modifiers(ctx);
            let mut is_var = false;
            if ctx.peek_kind() == Some(TokenKind::Val) {
                ctx.bump();
            } else if ctx.peek_kind() == Some(TokenKind::Var) {
                is_var = true;
                ctx.bump();
            }
            parse_parameter_pattern(ctx, &mut params, is_var);
            if ctx.peek_kind() == Some(TokenKind::Colon) {
                ctx.bump();
                ctx.bump_while(|k| {
                    !matches!(k, TokenKind::Comma | TokenKind::RightParen | TokenKind::LeftBrace | TokenKind::Assign)
                });
            }
            if ctx.peek_kind() == Some(TokenKind::Assign) {
                ctx.bump();
                let _ = ctx.parse_expression();
            }
            ctx.finish_node(); // ClassParameter
            if ctx.peek_kind() == Some(TokenKind::Comma) {
                ctx.bump();
                continue;
            } else {
                break;
            }
        }
        if ctx.peek_kind() == Some(TokenKind::RightParen) {
            ctx.bump();
        } else {
            ctx.error("コンストラクタパラメータリストを `)` で閉じてください");
        }
        ctx.finish_node(); // ClassParameterList
    }
    params
}

fn emit_synthesized_members(ctx: &mut ParserContext, params: &[ParamInfo]) {
    for param in params {
        let decl_kind = if param.is_var {
            SyntaxKind::VarDeclaration
        } else {
            SyntaxKind::ValDeclaration
        };
        ctx.start_node(decl_kind);
        ctx.start_node(SyntaxKind::Identifier);
        ctx.events.push(super::super::ParseEvent::Token {
            kind: SyntaxKind::Identifier,
            token_index: param.token_index,
        });
        ctx.finish_node(); // Identifier
        ctx.finish_node(); // Val/Var
    }
}
