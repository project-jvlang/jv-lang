//! 文のパース処理（簡易実装）。

use super::{
    Parser,
    expression::{parse_expression, span_of_expr},
    recovery,
    types::parse_type,
};
use crate::{diagnostics::Diagnostic, token::TokenKind};
use jv_ast::binding_pattern::BindingPatternKind;
use jv_ast::statement::{
    ForInStatement, LoopBinding, LoopStrategy, NumericRangeLoop, Program, Statement, TestDataset,
    TestDatasetRow, TestDeclaration, TestParameter, ValBindingOrigin,
};
use jv_ast::types::{BinaryOp, Literal, Modifiers};
use jv_ast::expression::{Parameter, ParameterModifiers};
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
                parser.recovery_metrics.recovered += 1;
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

pub(crate) fn parse_statement<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) -> Option<Statement> {
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
            let span = {
                let tok = parser.advance();
                parser.ast_span(tok.span)
            };
            Some(Statement::Break(span))
        }
        TokenKind::Continue => {
            let span = {
                let tok = parser.advance();
                parser.ast_span(tok.span)
            };
            Some(Statement::Continue(span))
        }
        TokenKind::Test => parse_test(parser),
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
        parse_expression(parser).unwrap_or_else(|| dummy_expr(parser, name_span))
    } else {
        dummy_expr(parser, name_span)
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
            span: parser.ast_span(span),
        })
    } else {
        Some(Statement::VarDeclaration {
            name,
            binding: None,
            type_annotation,
            initializer: Some(initializer),
            modifiers: Modifiers::default(),
            span: parser.ast_span(span),
        })
    }
}

fn parse_fun<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) -> Option<Statement> {
    let fn_span = parser.advance().span; // consume 'fun'

    // Parse optional type parameters: fun <T, R> ...
    let type_parameters = if parser.consume_if(TokenKind::Less) {
        parse_type_parameters(parser)
    } else {
        Vec::new()
    };

    // Parse function name (possibly with receiver: Type.method)
    let (name, name_span) = parse_function_name(parser)?;

    // Parse function parameters
    let parameters = if parser.consume_if(TokenKind::LeftParen) {
        parse_parameters(parser)
    } else {
        Vec::new()
    };

    let return_type = if parser.consume_if(TokenKind::Colon) {
        parse_type(parser)
    } else if parser.consume_if(TokenKind::Arrow) {
        parse_type(parser)
    } else {
        None
    };

    let _ = parser.consume_if(TokenKind::Assign);

    let body = parse_expression(parser).unwrap_or_else(|| dummy_expr(parser, fn_span));

    let span = fn_span.merge(name_span);
    Some(Statement::FunctionDeclaration {
        name,
        type_parameters,
        generic_signature: None,
        where_clause: None,
        parameters,
        return_type,
        primitive_return: None,
        body: Box::new(body),
        modifiers: Modifiers::default(),
        span: parser.ast_span(span),
    })
}

/// Parse type parameters: <T, R : Bound, ...>
fn parse_type_parameters<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) -> Vec<String> {
    let mut params = Vec::new();

    loop {
        let token = parser.current();
        if token.kind == TokenKind::Identifier {
            let name = parser
                .lexeme(token.span)
                .map(|s| s.to_string())
                .unwrap_or_else(|| format!("_T{}", params.len()));
            parser.advance();
            params.push(name);

            // Skip type bound: `: Bound<T>`
            if parser.consume_if(TokenKind::Colon) {
                skip_type_bound(parser);
            }
        }

        if parser.consume_if(TokenKind::Comma) {
            continue;
        }
        if parser.consume_if(TokenKind::Greater) {
            break;
        }
        if parser.current().kind == TokenKind::Eof {
            break;
        }
        // Skip unexpected tokens
        parser.advance();
    }

    params
}

/// Skip a type bound like `Comparable<R>` in `<R : Comparable<R>>`
fn skip_type_bound<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) {
    // Consume the bound type name
    if parser.current().kind == TokenKind::Identifier {
        parser.advance();
    }

    // Handle generic bounds: Comparable<R>
    if parser.consume_if(TokenKind::Less) {
        let mut depth = 1;
        while depth > 0 && parser.current().kind != TokenKind::Eof {
            let tok = parser.current();
            if tok.kind == TokenKind::Less {
                depth += 1;
            } else if tok.kind == TokenKind::Greater {
                depth -= 1;
            }
            parser.advance();
            if depth == 0 {
                break;
            }
        }
    }
}

/// Parse function name, handling receiver syntax: Type<T>.methodName
fn parse_function_name<'src, 'alloc>(
    parser: &mut Parser<'src, 'alloc>,
) -> Option<(String, crate::span::Span)> {
    let token = parser.current();
    if token.kind != TokenKind::Identifier {
        parser.push_diagnostic(Diagnostic::new("function name expected", token.span));
        return None;
    }

    let mut name = parser
        .lexeme(token.span)
        .map(|s| s.to_string())
        .unwrap_or_else(|| format!("_fn{}", token.span.start));
    let start_span = token.span;
    parser.advance();

    // Handle generic type: Type<T>
    if parser.consume_if(TokenKind::Less) {
        name.push('<');
        let mut depth = 1;
        while depth > 0 && parser.current().kind != TokenKind::Eof {
            let tok = parser.current();
            if tok.kind == TokenKind::Less {
                depth += 1;
            } else if tok.kind == TokenKind::Greater {
                depth -= 1;
            }
            if let Some(text) = parser.lexeme(tok.span) {
                name.push_str(text);
            }
            parser.advance();
        }
    }

    // Handle receiver: .methodName
    if parser.consume_if(TokenKind::Dot) {
        name.push('.');
        let method_token = parser.current();
        if method_token.kind == TokenKind::Identifier {
            if let Some(method_name) = parser.lexeme(method_token.span) {
                name.push_str(method_name);
            }
            parser.advance();
        }
    }

    Some((name, start_span))
}

/// Parse function parameters: `(name: Type = default, ...)`
fn parse_parameters<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) -> Vec<Parameter> {
    let mut parameters = Vec::new();

    // Handle empty parameter list
    if parser.current().kind == TokenKind::RightParen {
        parser.advance();
        return parameters;
    }

    loop {
        let param_start = parser.current().span;

        // Parse parameter name
        let (name, name_span) = match parse_identifier(parser) {
            Some(id) => id,
            None => {
                // Skip to next comma or closing paren
                while !matches!(
                    parser.current().kind,
                    TokenKind::Comma | TokenKind::RightParen | TokenKind::Eof
                ) {
                    parser.advance();
                }
                if parser.consume_if(TokenKind::Comma) {
                    continue;
                }
                break;
            }
        };

        // Track end span - starts at name, may extend with type or default
        let mut param_end = name_span;

        // Parse optional type annotation `: Type`
        let type_annotation = if parser.consume_if(TokenKind::Colon) {
            let type_start = parser.current().span;
            let ty = parse_type(parser);
            if ty.is_some() {
                // Extend span through the type
                param_end = type_start;
            }
            ty
        } else {
            None
        };

        // Parse optional default value `= expr`
        let default_value = if parser.consume_if(TokenKind::Assign) {
            let expr = parse_expression(parser);
            if let Some(ref e) = expr {
                // Extend span through the expression
                param_end = param_end.merge(span_of_expr(parser, e));
            }
            expr
        } else {
            None
        };

        let param_span = param_start.merge(param_end);

        parameters.push(Parameter {
            name,
            type_annotation,
            default_value,
            modifiers: ParameterModifiers::default(),
            span: parser.ast_span(param_span),
        });

        // Check for comma (more parameters) or end
        if parser.consume_if(TokenKind::Comma) {
            // Handle trailing comma
            if parser.current().kind == TokenKind::RightParen {
                break;
            }
            continue;
        }
        break;
    }

    // Consume closing paren
    let _ = parser.consume_if(TokenKind::RightParen);

    parameters
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
            span: parser.ast_span(class_span),
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
            span: parser.ast_span(class_span),
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

    // jv syntax: for (binding in iterable) body
    // Consume optional opening parenthesis
    let has_paren = parser.consume_if(TokenKind::LeftParen);

    let (name, name_span) = parse_identifier(parser)?;

    let binding = LoopBinding {
        name,
        pattern: None,
        type_annotation: None,
        span: parser.ast_span(name_span),
    };

    if !parser.consume_if(TokenKind::In) {
        parser.push_diagnostic(Diagnostic::new("\"in\" expected in for-in loop", for_span));
    }

    let iterable = parse_expression(parser).unwrap_or_else(|| dummy_expr(parser, for_span));

    // Consume closing parenthesis if we had an opening one
    if has_paren && !parser.consume_if(TokenKind::RightParen) {
        parser.push_diagnostic(Diagnostic::new("\")\" expected in for-in loop", for_span));
    }

    let body = parse_expression(parser).unwrap_or_else(|| dummy_expr(parser, for_span));

    // Detect numeric range expressions and set appropriate strategy
    let strategy = detect_loop_strategy(&iterable);

    let span = for_span.merge(name_span);
    Some(Statement::ForIn(ForInStatement {
        binding,
        iterable,
        strategy,
        body: Box::new(body),
        span: parser.ast_span(span),
    }))
}

/// Detect if the iterable expression is a numeric range and return appropriate strategy.
fn detect_loop_strategy(iterable: &Expression) -> LoopStrategy {
    match iterable {
        Expression::Binary {
            left,
            op,
            right,
            span,
            ..
        } => match op {
            BinaryOp::RangeExclusive => LoopStrategy::NumericRange(NumericRangeLoop {
                start: (**left).clone(),
                end: (**right).clone(),
                inclusive: false,
                span: span.clone(),
            }),
            BinaryOp::RangeInclusive => LoopStrategy::NumericRange(NumericRangeLoop {
                start: (**left).clone(),
                end: (**right).clone(),
                inclusive: true,
                span: span.clone(),
            }),
            _ => LoopStrategy::Unknown,
        },
        _ => LoopStrategy::Unknown,
    }
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
        span: parser.ast_span(kw),
    })
}

fn parse_throw<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) -> Option<Statement> {
    let kw = parser.advance().span;
    let expr = parse_expression(parser).unwrap_or_else(|| dummy_expr(parser, kw));
    Some(Statement::Throw {
        expr,
        span: parser.ast_span(kw),
    })
}

fn parse_expression_statement<'src, 'alloc>(
    parser: &mut Parser<'src, 'alloc>,
) -> Option<Statement> {
    if let Some(assignment) = parse_assignment_statement(parser) {
        return Some(assignment);
    }

    let expr = match parse_expression(parser) {
        Some(expr) => expr,
        None => {
            let span = parser.current().span;
            dummy_expr(parser, span)
        }
    };
    Some(Statement::Expression {
        expr,
        span: AstSpan::default(),
    })
}

fn parse_assignment_statement<'src, 'alloc>(
    parser: &mut Parser<'src, 'alloc>,
) -> Option<Statement> {
    let checkpoint = parser.checkpoint();
    let target = match parse_expression(parser) {
        Some(expr) => expr,
        None => {
            parser.rewind(checkpoint);
            return None;
        }
    };

    if !parser.consume_if(TokenKind::Assign) {
        parser.rewind(checkpoint);
        return None;
    }

    let fallback_span = span_of_expr(parser, &target);
    let value = parse_expression(parser).unwrap_or_else(|| dummy_expr(parser, fallback_span));
    let span = parser
        .ast_span(span_of_expr(parser, &target).merge(span_of_expr(parser, &value)));

    Some(Statement::Assignment {
        target,
        binding_pattern: None,
        value,
        span,
    })
}

fn parse_test<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) -> Option<Statement> {
    let test_kw_span = parser.advance().span;

    // Display name
    let display_name = if parser.current().kind == TokenKind::String {
        let tok = parser.advance();
        parser
            .lexeme(tok.span)
            .map(|s| s.trim_matches('"').to_string())
            .unwrap_or_else(|| "_".into())
    } else {
        "_".into()
    };

    // Dataset (inline only for now)
    let dataset = if parser.current().kind == TokenKind::LeftBracket {
        Some(parse_inline_dataset(parser))
    } else {
        None
    };

    // Parameters
    let mut parameters = Vec::new();
    if parser.consume_if(TokenKind::LeftParen) {
        loop {
            if parser.current().kind == TokenKind::RightParen || parser.current().kind == TokenKind::Eof {
                let _ = parser.consume_if(TokenKind::RightParen);
                break;
            }

            let (name, name_span) = match parse_identifier(parser) {
                Some(pair) => pair,
                None => {
                    parser.advance();
                    continue;
                }
            };
            let type_annotation = if parser.consume_if(TokenKind::Colon) {
                parse_type(parser)
            } else {
                None
            };
            let param_span = parser.ast_span(name_span);
            parameters.push(TestParameter {
                pattern: BindingPatternKind::Identifier { name, span: param_span.clone() },
                type_annotation,
                span: param_span,
            });

            if parser.consume_if(TokenKind::Comma) {
                continue;
            }
        }
    }

    // Body
    let body = parse_expression(parser).unwrap_or_else(|| dummy_expr(parser, test_kw_span));
    let body_span = span_of_expr(parser, &body);
    let span = parser.ast_span(test_kw_span.merge(body_span));

    Some(Statement::TestDeclaration(TestDeclaration {
        display_name,
        normalized: None,
        dataset,
        parameters,
        annotations: Vec::new(),
        body,
        span,
    }))
}

fn parse_inline_dataset<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) -> TestDataset {
    let start_span = parser.advance().span; // consume '['
    let mut rows = Vec::new();

    loop {
        let token = parser.current();
        if token.kind == TokenKind::RightBracket || token.kind == TokenKind::Eof {
            let end_span = if token.kind == TokenKind::RightBracket {
                parser.advance().span
            } else {
                token.span
            };
            let span = parser.ast_span(start_span.merge(end_span));
            return TestDataset::InlineArray { rows, span };
        }

        if token.kind == TokenKind::LeftBracket {
            rows.push(parse_dataset_row(parser));
        } else {
            parser.advance();
        }
    }
}

fn parse_dataset_row<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) -> TestDatasetRow {
    let row_start = parser.advance().span; // consume '['
    let mut values = Vec::new();
    let mut last_span = row_start;

    loop {
        if parser.current().kind == TokenKind::RightBracket || parser.current().kind == TokenKind::Eof {
            if parser.current().kind == TokenKind::RightBracket {
                last_span = parser.current().span;
                parser.advance();
            }
            break;
        }

        // Parse dataset values (literals only, including negative numbers)
        if let Some(expr) = parse_dataset_value(parser) {
            last_span = span_of_expr(parser, &expr);
            values.push(expr);
        } else {
            // Skip unknown token
            last_span = parser.current().span;
            parser.advance();
        }

        if parser.consume_if(TokenKind::Comma) {
            continue;
        }
    }

    let span = parser.ast_span(row_start.merge(last_span));
    TestDatasetRow { values, span }
}

/// Parse a single literal value for dataset rows.
/// Supports: string, number (including negative), boolean, null
fn parse_dataset_value<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) -> Option<Expression> {
    let token = parser.current();
    match token.kind {
        TokenKind::String => {
            parser.advance();
            let text = parser
                .lexeme(token.span)
                .map(|s| {
                    // Remove surrounding quotes from string literal
                    if s.starts_with('"') && s.ends_with('"') && s.len() >= 2 {
                        s[1..s.len() - 1].to_string()
                    } else {
                        s.to_string()
                    }
                })
                .unwrap_or_default();
            Some(Expression::Literal(
                Literal::String(text),
                parser.ast_span(token.span),
            ))
        }
        TokenKind::Number => {
            parser.advance();
            let text = parser
                .lexeme(token.span)
                .map(|s| s.to_string())
                .unwrap_or_else(|| "0".to_string());
            Some(Expression::Literal(
                Literal::Number(text),
                parser.ast_span(token.span),
            ))
        }
        TokenKind::Minus => {
            // Negative number: consume '-' and expect number
            let minus_span = parser.advance().span;
            if parser.current().kind == TokenKind::Number {
                let num_token = parser.current();
                parser.advance();
                let text = parser
                    .lexeme(num_token.span)
                    .map(|s| format!("-{}", s))
                    .unwrap_or_else(|| "-0".to_string());
                let span = parser.ast_span(minus_span.merge(num_token.span));
                Some(Expression::Literal(Literal::Number(text), span))
            } else {
                // Just a minus without number - treat as error, return dummy
                Some(Expression::Literal(
                    Literal::Number("-0".to_string()),
                    parser.ast_span(minus_span),
                ))
            }
        }
        TokenKind::TrueKw => {
            parser.advance();
            Some(Expression::Literal(
                Literal::Boolean(true),
                parser.ast_span(token.span),
            ))
        }
        TokenKind::FalseKw => {
            parser.advance();
            Some(Expression::Literal(
                Literal::Boolean(false),
                parser.ast_span(token.span),
            ))
        }
        TokenKind::NullKw => {
            parser.advance();
            Some(Expression::Literal(
                Literal::Null,
                parser.ast_span(token.span),
            ))
        }
        TokenKind::Identifier => {
            // Identifiers in datasets are typically used as references
            parser.advance();
            let name = parser
                .lexeme(token.span)
                .map(|s| s.to_string())
                .unwrap_or_else(|| "_".to_string());
            Some(Expression::Identifier(name, parser.ast_span(token.span)))
        }
        _ => None,
    }
}

pub(crate) fn parse_identifier<'src, 'alloc>(
    parser: &mut Parser<'src, 'alloc>,
) -> Option<(String, crate::span::Span)> {
    let token = parser.current();
    if token.kind == TokenKind::Identifier {
        parser.advance();
        let text = parser
            .lexeme(token.span)
            .map(|s| s.to_string())
            .unwrap_or_else(|| token_text_placeholder(&token));
        Some((text, token.span))
    } else if token.kind == TokenKind::Underscore {
        // `_` wildcard pattern is also valid as an identifier
        parser.advance();
        Some(("_".to_string(), token.span))
    } else {
        parser.push_diagnostic(Diagnostic::new("identifier expected", token.span));
        None
    }
}

fn token_text_placeholder(token: &crate::token::Token) -> String {
    format!("_id{}", token.span.start)
}

fn dummy_expr<'src, 'alloc>(parser: &Parser<'src, 'alloc>, span: crate::span::Span) -> Expression {
    Expression::Identifier("_".into(), parser.ast_span(span))
}

#[cfg(test)]
mod statement_tests {
    use super::*;
    use crate::{allocator::Arena, lexer::Lexer, source::Source};
    use crate::parser::Parser;

    fn parse_source(input: &str) -> Vec<Statement> {
        let source = Source::from_str(input);
        let lexer = Lexer::new(source);
        let arena = Arena::new();
        let mut parser = Parser::new(lexer, &arena);
        let program = parse_program(&mut parser).unwrap();
        program.statements
    }

    #[test]
    fn parses_function_with_tuple_return_type() {
        let stmts = parse_source(r#"fun produce(): (Int String) {
    (1 "ok")
}"#);
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Statement::FunctionDeclaration { name, return_type, body, .. } => {
                assert_eq!(name, "produce");
                assert!(return_type.is_some(), "return type should be present");
                let ret = return_type.as_ref().unwrap();
                match ret {
                    jv_ast::TypeAnnotation::Simple(s) => {
                        assert!(s.starts_with('('), "should start with '(': {}", s);
                    }
                    other => panic!("expected Simple, got {:?}", other),
                }
                // Body should be a Block containing a Tuple expression
                match body.as_ref() {
                    Expression::Block { statements, .. } => {
                        assert_eq!(statements.len(), 1, "block should have one statement");
                    }
                    other => panic!("expected Block, got {:?}", other),
                }
            }
            other => panic!("expected FunctionDeclaration, got {:?}", other),
        }
    }
}

#[allow(dead_code)]
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
