use jv_parser_rowan::{
    frontend::RowanPipeline,
    parser::{self, ParseEvent},
    syntax::SyntaxKind,
};

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

    let pipeline = RowanPipeline::default();
    let debug = pipeline
        .execute_with_debug(source)
        .expect("ログ構文のパースが成功するはずです");
    if let Some(error) = debug.pipeline_error() {
        panic!("パーサがエラーを報告しました: {error:?}");
    }

    let artifacts = debug.artifacts();
    let tokens = artifacts.tokens();

    assert!(
        tokens
            .iter()
            .any(|token| matches!(token.token_type, jv_lexer::TokenType::Log)),
        "LOG キーワードがトークン列に存在するべきです"
    );
    assert!(
        tokens
            .iter()
            .any(|token| matches!(token.token_type, jv_lexer::TokenType::Trace)),
        "TRACE キーワードがトークン列に存在するべきです"
    );
    assert!(
        tokens
            .iter()
            .any(|token| matches!(token.token_type, jv_lexer::TokenType::String(_))),
        "文字列メッセージがトークン化されているべきです"
    );

    let parse_output = parser::parse(tokens);
    assert!(
        parse_output
            .diagnostics
            .iter()
            .all(|diagnostic| !matches!(diagnostic.severity, parser::DiagnosticSeverity::Error)),
        "ログ構文には構文エラーが発生しないはずです: {:?}",
        parse_output.diagnostics
    );

    let log_block_nodes = parse_output
        .events
        .iter()
        .filter(|event| {
            matches!(
                event,
                ParseEvent::StartNode {
                    kind: SyntaxKind::LogBlockExpression,
                }
            )
        })
        .count();
    assert_eq!(
        log_block_nodes, 2,
        "親子2つの LogBlockExpression ノードが生成されるべきです"
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

    let pipeline = RowanPipeline::default();
    let debug = pipeline
        .execute_with_debug(source)
        .expect("ネスト上限テストのパース実行に失敗しました");
    assert!(
        debug.pipeline_error().is_some(),
        "過剰なネストではパイプラインエラーが発生するはずです"
    );
    let artifacts = debug.into_artifacts();
    let frontend = artifacts.into_frontend_output();
    let diagnostics = frontend.diagnostics().final_diagnostics();

    assert!(
        diagnostics.iter().any(|diagnostic| diagnostic
            .message()
            .contains("ログブロックのネストは1段までです")),
        "過剰なネストに対する診断が必要ですが {:?} でした",
        diagnostics
    );
}
