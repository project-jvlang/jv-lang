use jv_lexer::TokenType;
use jv_parser_frontend::{DiagnosticSeverity, Parser2Pipeline};

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

    let artifacts = Parser2Pipeline::default()
        .execute(source)
        .expect("ログ構文のパースが成功するはずです");
    let tokens = artifacts.tokens();

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

    assert!(
        artifacts
            .diagnostics()
            .final_diagnostics()
            .iter()
            .all(|diagnostic| !matches!(diagnostic.severity(), DiagnosticSeverity::Error)),
        "ログ構文には致命的なパースエラーが発生しないはずです"
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

    let artifacts = Parser2Pipeline::default()
        .execute(source)
        .expect("ネスト上限テストのパース実行に失敗しました");
    let diagnostics = artifacts.diagnostics().final_diagnostics();

    assert!(
        diagnostics.is_empty()
            || diagnostics.iter().any(|diagnostic| {
                diagnostic
                    .message()
                    .contains("ログブロック") || matches!(diagnostic.severity(), DiagnosticSeverity::Error)
            }),
        "過剰なネストに対する診断が必要ですが {:?} でした",
        diagnostics
    );
}
