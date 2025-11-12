use crate::frontend::RowanPipeline;
use crate::parser::{parse, TokenSpan};
use jv_ast::Span;
use jv_lexer::{Lexer, Token};
use jv_parser_frontend::ParseError;

fn lex(source: &str) -> Vec<Token> {
    Lexer::new(source.to_string())
        .tokenize()
        .expect("字句解析に成功すること")
}

fn token_span_to_span(tokens: &[Token], span: TokenSpan) -> Span {
    if tokens.is_empty() {
        return Span::dummy();
    }

    let max_index = tokens.len().saturating_sub(1);
    let start_index = span.start.min(max_index);
    let raw_end = if span.end == 0 {
        0
    } else {
        span.end.saturating_sub(1)
    };
    let end_index = raw_end.min(max_index).max(start_index);

    let start = &tokens[start_index];
    let end = &tokens[end_index];
    let start_span = Span::from_token_lexeme(start.line, start.column, &start.lexeme);
    let end_span = Span::from_token_lexeme(end.line, end.column, &end.lexeme);
    start_span.merge(&end_span)
}

#[test]
fn parser_reports_span_for_missing_binding_name() {
    let tokens = lex("val=");
    let output = parse(&tokens);

    assert_eq!(
        output.diagnostics.len(),
        1,
        "バインディング名不足の診断が1件だけ発生すること"
    );

    let diagnostic = &output.diagnostics[0];
    assert_eq!(diagnostic.message, "バインディング名が必要です");

    let span = token_span_to_span(&tokens, diagnostic.span);
    assert_eq!(span.start_line, 1);
    assert_eq!(span.end_line, 1);
    assert_eq!(span.start_column, 1);
    assert_eq!(span.end_column, 5);
}

#[test]
fn parser_reports_span_for_missing_initializer_expression() {
    let tokens = lex("val x");
    let output = parse(&tokens);

    assert_eq!(
        output.diagnostics.len(),
        1,
        "初期化子欠落の診断が1件だけ発生すること"
    );

    let diagnostic = &output.diagnostics[0];
    assert_eq!(diagnostic.message, "初期化子が必要です");

    let span = token_span_to_span(&tokens, diagnostic.span);
    assert_eq!(span.start_line, 1);
    assert_eq!(span.end_line, 1);
    assert_eq!(span.start_column, 6);
    assert_eq!(span.end_column, 6);
}

#[test]
fn lowering_reports_jv2101_diagnostic_with_precise_span() {
    let pipeline = RowanPipeline::new();
    let debug = pipeline
        .execute_with_debug("val numbers = [1, 2 3]")
        .expect("Rowanパイプラインが実行できること");

    let pipeline_error = debug
        .pipeline_error()
        .expect("レイアウト違反はパイプラインエラーとして伝搬すること");
    match pipeline_error {
        ParseError::Syntax { span, .. } => {
            assert_eq!(span.start_line, 1);
            assert_eq!(span.end_line, 1);
            assert_eq!(span.start_column, 15);
            assert_eq!(span.end_column, 23);
        }
        other => panic!("想定外のエラー種別: {other:?}"),
    }

    let diagnostics = debug.artifacts().diagnostics().final_diagnostics();

    let array_diag = diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code() == Some("JV2101"))
        .expect("JV2101診断が生成されること");

    assert_eq!(array_diag.source().name(), "rowan-lowering");
    assert!(
        array_diag.message().contains("配列リテラル"),
        "配列リテラルに関する説明が含まれること"
    );

    let span = array_diag
        .span()
        .expect("レイアウト診断にはスパンが付与されること");
    assert_eq!(span.start_line, 1);
    assert_eq!(span.end_line, 1);
    assert_eq!(span.start_column, 15);
    assert_eq!(span.end_column, 23);
}
