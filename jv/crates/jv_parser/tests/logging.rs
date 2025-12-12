use jv_ast::{
    expression::{Expression, LogBlockLevel, LogItem},
    statement::Statement,
};
use jv_lexer::TokenType;
use jv_parser::{ParseError, Parser};

#[test]
fn log_block_items_are_preserved_in_ast_order() {
    let source = r#"
        fun main {
            LOG {
                val user = loadUser()
                TRACE {
                    "nested"
                }
                "done"
            }
        }
    "#;

    let output = Parser::parse(source).expect("ログ構文のパースが成功するはずです");
    let (program, tokens, diagnostics) = output.into_parts();

    assert!(
        diagnostics.final_diagnostics().is_empty(),
        "診断が空であること: {:?}",
        diagnostics.final_diagnostics()
    );

    assert!(
        tokens
            .iter()
            .any(|token| matches!(token.token_type, TokenType::Log)),
        "LOG キーワードがトークン列に存在するべきです"
    );
    assert!(
        tokens
            .iter()
            .any(|token| matches!(token.token_type, TokenType::Trace)),
        "TRACE キーワードがトークン列に存在するべきです"
    );
    assert!(
        tokens
            .iter()
            .any(|token| matches!(token.token_type, TokenType::String(_))),
        "文字列メッセージがトークン化されているべきです"
    );

    let function_body = match program.statements.first() {
        Some(Statement::FunctionDeclaration { body, .. }) => body.as_ref(),
        other => panic!("関数宣言を期待しましたが {:?} でした", other),
    };

    let statements = match function_body {
        Expression::Block { statements, .. } => statements,
        other => panic!("関数本体はブロック式のはずです: {:?}", other),
    };

    let log_expr = match statements.first() {
        Some(Statement::Expression { expr, .. }) => expr,
        other => panic!("最初のステートメントは式のはずです: {:?}", other),
    };

    let log_block = match log_expr {
        Expression::LogBlock(block) => block,
        other => panic!("LOG ブロック式を期待しましたが {:?} でした", other),
    };

    assert_eq!(log_block.level, LogBlockLevel::Default);
    assert_eq!(
        log_block.items.len(),
        3,
        "宣言・ネスト・メッセージの3要素になるはずです"
    );

    match log_block.items.first() {
        Some(LogItem::Statement(_)) => {}
        other => panic!("最初の要素はステートメントのはずです: {:?}", other),
    }

    match log_block.items.get(1) {
        Some(LogItem::Nested(inner)) => assert_eq!(inner.level, LogBlockLevel::Trace),
        other => panic!("2番目の要素は TRACE ブロックのはずです: {:?}", other),
    }

    match log_block.items.get(2) {
        Some(LogItem::Expression(Expression::Literal(_, _))) => {}
        other => panic!("3番目の要素はメッセージ式のはずです: {:?}", other),
    }

    let nested_blocks = 1
        + log_block
            .items
            .iter()
            .filter(|item| matches!(item, LogItem::Nested(_)))
            .count();
    assert_eq!(
        nested_blocks, 2,
        "親子2つの LogBlock が生成されるべきです"
    );
}

#[test]
fn log_block_reports_diagnostic_when_nested_too_deep() {
    let source = r#"
        fun main {
            LOG {
                TRACE {
                    DEBUG {
                        "too deep"
                    }
                }
            }
        }
    "#;

    let error =
        Parser::parse(source).expect_err("過剰なネストではパースエラーが発生するはずです");

    match error {
        ParseError::Syntax { message, .. } => {
            assert!(
                message.contains("ログブロックのネストは1段までです")
                    || message.contains("JV-DSL-001"),
                "過剰なネスト診断が必要ですが {message:?} でした"
            );
        }
        other => panic!("構文エラーを期待しましたが {:?} を受け取りました", other),
    }
}
