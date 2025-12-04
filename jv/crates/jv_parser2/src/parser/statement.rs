//! 文のパース処理（簡易実装）。

use super::{Parser, expression::parse_expression, recovery, types::parse_type};
use crate::{diagnostics::Diagnostic, token::TokenKind};
use jv_ast::statement::{
    ForInStatement, LoopBinding, LoopStrategy, Program, Statement, ValBindingOrigin,
};
use jv_ast::types::Modifiers;
use jv_ast::{Expression, Span as AstSpan};

/// プログラム全体をパースする。
pub(crate) fn parse_program<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) -> Option<Program> {
    let mut statements = Vec::new();
    while parser.current().kind != TokenKind::Eof {
        match parse_statement(parser) {
            Some(stmt) => statements.push(stmt),
            None => {
                // 回復して次へ。
                recovery::recover_to_sync_point(parser);
                if parser.current().kind == TokenKind::Semicolon {
                    parser.advance();
                }
            }
        }
    }

    Some(Program {
        package: None,
        imports: Vec::new(),
        statements,
        span: AstSpan::default(),
    })
}

fn parse_statement<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) -> Option<Statement> {
    match parser.current().kind {
        TokenKind::Val => parse_val(parser, true),
        TokenKind::Var => parse_val(parser, false),
        TokenKind::Fun => parse_fun(parser),
        TokenKind::Class => parse_class(parser, false),
        TokenKind::Data => {
            parser.advance(); // consume data
            if parser.current().kind == TokenKind::Class {
                parse_class(parser, true)
            } else {
                parse_class_bodyless(parser, true)
            }
        }
        TokenKind::For => parse_for(parser),
        // when をステートメントとして扱う場合は式ステートメントにする。
        TokenKind::When => parse_expression_statement(parser),
        TokenKind::Return => parse_return(parser),
        TokenKind::Throw => parse_throw(parser),
        TokenKind::Break => {
            let span = to_ast_span(parser.advance().span);
            Some(Statement::Break(span))
        }
        TokenKind::Continue => {
            let span = to_ast_span(parser.advance().span);
            Some(Statement::Continue(span))
        }
        _ => parse_expression_statement(parser),
    }
}

fn parse_val<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>, is_val: bool) -> Option<Statement> {
    let kw_span = parser.advance().span;
    let (name, name_span) = parse_identifier(parser)?;

    let type_annotation = if parser.consume_if(TokenKind::Colon) {
        parse_type(parser)
    } else {
        None
    };

    let initializer = if parser.consume_if(TokenKind::Assign) {
        parse_expression(parser).unwrap_or_else(|| dummy_expr(name_span))
    } else {
        dummy_expr(name_span)
    };

    let span = kw_span.merge(name_span);
    if is_val {
        Some(Statement::ValDeclaration {
            name,
            binding: None,
            type_annotation,
            initializer,
            modifiers: Modifiers::default(),
            origin: ValBindingOrigin::ExplicitKeyword,
            span: to_ast_span(span),
        })
    } else {
        Some(Statement::VarDeclaration {
            name,
            binding: None,
            type_annotation,
            initializer: Some(initializer),
            modifiers: Modifiers::default(),
            span: to_ast_span(span),
        })
    }
}

fn parse_fun<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) -> Option<Statement> {
    let fn_span = parser.advance().span;
    let (name, name_span) = parse_identifier(parser)?;

    // 引数・戻り値は後続タスクで詳細実装。ここでは括弧をスキップ。
    if parser.consume_if(TokenKind::LeftParen) {
        skip_group(parser, TokenKind::LeftParen, TokenKind::RightParen);
    }

    let return_type = if parser.consume_if(TokenKind::Arrow) {
        parse_type(parser)
    } else {
        None
    };

    let body = parse_expression(parser).unwrap_or_else(|| dummy_expr(fn_span));

    let span = fn_span.merge(name_span);
    Some(Statement::FunctionDeclaration {
        name,
        type_parameters: Vec::new(),
        generic_signature: None,
        where_clause: None,
        parameters: Vec::new(),
        return_type,
        primitive_return: None,
        body: Box::new(body),
        modifiers: Modifiers::default(),
        span: to_ast_span(span),
    })
}

fn parse_class<'src, 'alloc>(
    parser: &mut Parser<'src, 'alloc>,
    is_data: bool,
) -> Option<Statement> {
    let class_kw = parser.advance().span;
    let (name, name_span) = parse_identifier(parser)?;
    let class_span = class_kw.merge(name_span);
    if is_data {
        Some(Statement::DataClassDeclaration {
            name,
            parameters: Vec::new(),
            type_parameters: Vec::new(),
            generic_signature: None,
            is_mutable: false,
            modifiers: Modifiers::default(),
            span: to_ast_span(class_span),
        })
    } else {
        Some(Statement::ClassDeclaration {
            name,
            type_parameters: Vec::new(),
            generic_signature: None,
            superclass: None,
            interfaces: Vec::new(),
            properties: Vec::new(),
            methods: Vec::new(),
            modifiers: Modifiers::default(),
            span: to_ast_span(class_span),
        })
    }
}

fn parse_class_bodyless<'src, 'alloc>(
    _parser: &mut Parser<'src, 'alloc>,
    is_data: bool,
) -> Option<Statement> {
    let span = AstSpan::default();
    if is_data {
        Some(Statement::DataClassDeclaration {
            name: String::new(),
            parameters: Vec::new(),
            type_parameters: Vec::new(),
            generic_signature: None,
            is_mutable: false,
            modifiers: Modifiers::default(),
            span,
        })
    } else {
        Some(Statement::ClassDeclaration {
            name: String::new(),
            type_parameters: Vec::new(),
            generic_signature: None,
            superclass: None,
            interfaces: Vec::new(),
            properties: Vec::new(),
            methods: Vec::new(),
            modifiers: Modifiers::default(),
            span,
        })
    }
}

fn parse_for<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) -> Option<Statement> {
    let for_span = parser.advance().span;
    let (name, name_span) = parse_identifier(parser)?;

    let binding = LoopBinding {
        name,
        pattern: None,
        type_annotation: None,
        span: to_ast_span(name_span),
    };

    if !parser.consume_if(TokenKind::In) {
        parser.push_diagnostic(Diagnostic::new("\"in\" expected in for-in loop", for_span));
    }

    let iterable = parse_expression(parser).unwrap_or_else(|| dummy_expr(for_span));
    let body = parse_expression(parser).unwrap_or_else(|| dummy_expr(for_span));

    let span = for_span.merge(name_span);
    Some(Statement::ForIn(ForInStatement {
        binding,
        iterable,
        strategy: LoopStrategy::Unknown,
        body: Box::new(body),
        span: to_ast_span(span),
    }))
}

fn parse_return<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) -> Option<Statement> {
    let kw = parser.advance().span;
    let value = if parser.current().kind != TokenKind::Semicolon {
        parse_expression(parser)
    } else {
        None
    };
    Some(Statement::Return {
        value,
        span: to_ast_span(kw),
    })
}

fn parse_throw<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) -> Option<Statement> {
    let kw = parser.advance().span;
    let expr = parse_expression(parser).unwrap_or_else(|| dummy_expr(kw));
    Some(Statement::Throw {
        expr,
        span: to_ast_span(kw),
    })
}

fn parse_expression_statement<'src, 'alloc>(
    parser: &mut Parser<'src, 'alloc>,
) -> Option<Statement> {
    let expr = parse_expression(parser).unwrap_or_else(|| dummy_expr(parser.current().span));
    Some(Statement::Expression {
        expr,
        span: AstSpan::default(),
    })
}

fn parse_identifier<'src, 'alloc>(
    parser: &mut Parser<'src, 'alloc>,
) -> Option<(String, crate::span::Span)> {
    let token = parser.current();
    if token.kind == TokenKind::Identifier {
        parser.advance();
        Some((token_text_placeholder(&token), token.span))
    } else {
        parser.push_diagnostic(Diagnostic::new("identifier expected", token.span));
        None
    }
}

fn token_text_placeholder(token: &crate::token::Token) -> String {
    format!("_id{}", token.span.start)
}

fn dummy_expr(span: crate::span::Span) -> Expression {
    Expression::Identifier("_".into(), to_ast_span(span))
}

fn to_ast_span(span: crate::span::Span) -> AstSpan {
    AstSpan {
        start_line: 0,
        start_column: span.start as usize,
        end_line: 0,
        end_column: span.end as usize,
    }
}

fn skip_group<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>, open: TokenKind, close: TokenKind) {
    let mut depth = 1;
    while depth > 0 {
        let tok = parser.advance();
        if tok.kind == TokenKind::Eof {
            break;
        }
        if tok.kind == open {
            depth += 1;
        } else if tok.kind == close {
            depth -= 1;
        }
    }
}
