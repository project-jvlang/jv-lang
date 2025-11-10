use super::*;

use crate::pipeline::pipeline::{CharScannerStage, ClassifierStage, EmitterStage, NormalizerStage};
use fastrand::Rng;
use serde_json::{from_str, to_string};
use std::fs;
use std::path::{Path, PathBuf};
use test_case::test_case;

#[test]
fn test_basic_keywords_green_phase() {
    // GREEN: This test should now pass
    let mut lexer = Lexer::new("val var when data class fun".to_string());
    let tokens = lexer.tokenize().unwrap();

    // Expected tokens for Green phase
    assert_eq!(tokens.len(), 7); // 6 keywords + EOF
    assert_eq!(tokens[0].token_type, TokenType::Val);
    assert_eq!(tokens[1].token_type, TokenType::Var);
    assert_eq!(tokens[2].token_type, TokenType::When);
    assert_eq!(tokens[3].token_type, TokenType::Data);
    assert_eq!(tokens[4].token_type, TokenType::Class);
    assert_eq!(tokens[5].token_type, TokenType::Fun);
    assert_eq!(tokens[6].token_type, TokenType::Eof);
}

#[test]
fn test_logging_keywords_are_classified() {
    let mut lexer = Lexer::new("LOG TRACE DEBUG INFO WARN ERROR".to_string());
    let tokens = lexer.tokenize().expect("lexing success");

    let kinds: Vec<_> = tokens.iter().map(|token| &token.token_type).collect();
    assert!(kinds.contains(&&TokenType::Log));
    assert!(kinds.contains(&&TokenType::Trace));
    assert!(kinds.contains(&&TokenType::Debug));
    assert!(kinds.contains(&&TokenType::Info));
    assert!(kinds.contains(&&TokenType::Warn));
    assert!(kinds.contains(&&TokenType::Error));
}

#[test]
fn test_lowercase_logging_words_are_identifiers() {
    let mut lexer = Lexer::new("log trace debug info warn error".to_string());
    let tokens = lexer.tokenize().expect("lexing success");

    for expected in ["log", "trace", "debug", "info", "warn", "error"] {
        assert!(tokens.iter().any(|token| matches!(
            &token.token_type,
            TokenType::Identifier(text) if text == expected
        )));
    }
}

#[test]
fn test_identifiers_and_literals_green_phase() {
    // RED: This test should fail
    let mut lexer = Lexer::new("val name = \"Hello\" 42 true".to_string());
    let tokens = lexer.tokenize().unwrap();

    // Expected tokens for Green phase
    assert_eq!(tokens.len(), 7); // val, name, =, "Hello", 42, true, EOF
    assert_eq!(tokens[0].token_type, TokenType::Val);
    assert_eq!(
        tokens[1].token_type,
        TokenType::Identifier("name".to_string())
    );
    assert_eq!(tokens[2].token_type, TokenType::Assign);
    assert_eq!(tokens[3].token_type, TokenType::String("Hello".to_string()));
    assert_eq!(tokens[4].token_type, TokenType::Number("42".to_string()));
    assert_eq!(tokens[5].token_type, TokenType::Boolean(true));
    assert_eq!(tokens[6].token_type, TokenType::Eof);
}

#[test]
fn test_null_safety_operators_red_phase() {
    // RED: This test should fail
    let mut lexer = Lexer::new("obj?.field ?: default".to_string());
    let tokens = lexer.tokenize().unwrap();

    // Should recognize ?. and ?: operators
    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
    assert!(token_types.contains(&&TokenType::NullSafe));
    assert!(token_types.contains(&&TokenType::Elvis));
    assert!(token_types.contains(&&TokenType::Identifier("obj".to_string())));
    assert!(token_types.contains(&&TokenType::Identifier("field".to_string())));
    assert!(token_types.contains(&&TokenType::Identifier("default".to_string())));
}

#[test]
fn test_string_interpolation_red_phase() {
    // RED: This test should fail
    let mut lexer = Lexer::new("\"Hello, ${name}!\"".to_string());
    let tokens = lexer.tokenize().unwrap();

    // Expected tokens for string interpolation
    // Should tokenize as: StringStart("Hello, "), Identifier("name"), StringEnd("!")
    assert!(tokens.len() >= 3);

    // Look for interpolation-related tokens
    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
    assert!(
        token_types
            .iter()
            .any(|t| matches!(t, TokenType::StringStart))
    );
    assert!(token_types.contains(&&TokenType::Identifier("name".to_string())));
    assert!(
        token_types
            .iter()
            .any(|t| matches!(t, TokenType::StringEnd))
    );
}

#[test]
fn test_triple_quote_multiline_string_metadata() {
    let source = "\"\"\"Hello, ${name}!\nWelcome.\n\"\"\"";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    let string_start = tokens
        .iter()
        .find(|token| matches!(token.token_type, TokenType::StringStart))
        .expect("expected string start token");

    let metadata = string_start
        .metadata
        .iter()
        .find_map(|meta| match meta {
            TokenMetadata::StringLiteral(info) => Some(info),
            _ => None,
        })
        .expect("string metadata missing");

    assert_eq!(metadata.delimiter, StringDelimiterKind::TripleQuote);
    assert!(metadata.allows_interpolation);
    assert!(metadata.normalize_indentation);
}

#[test]
fn test_backtick_multiline_string_metadata() {
    let source = "```\nvalue: ${value}\n```";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    let string_start = tokens
        .iter()
        .find(|token| matches!(token.token_type, TokenType::StringStart))
        .expect("expected string start token");

    let metadata = string_start
        .metadata
        .iter()
        .find_map(|meta| match meta {
            TokenMetadata::StringLiteral(info) => Some(info),
            _ => None,
        })
        .expect("string metadata missing");

    assert_eq!(metadata.delimiter, StringDelimiterKind::BacktickBlock);
    assert!(metadata.allows_interpolation);
    assert!(!metadata.normalize_indentation);
}

#[test]
fn test_triple_quote_simple_string_metadata() {
    let source = "\"\"\"plain text\"\"\"";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    let string_token = tokens
        .iter()
        .find(|token| matches!(token.token_type, TokenType::String(_)))
        .expect("expected plain string token");

    let metadata = string_token
        .metadata
        .iter()
        .find_map(|meta| match meta {
            TokenMetadata::StringLiteral(info) => Some(info),
            _ => None,
        })
        .expect("string metadata missing");

    assert_eq!(metadata.delimiter, StringDelimiterKind::TripleQuote);
    assert!(metadata.normalize_indentation);
}

#[test]
fn layout_comma_metadata_survives_multiline_array() {
    let source = "[1\n  2]";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    assert!(
        !tokens
            .iter()
            .any(|token| matches!(token.token_type, TokenType::Comma)),
        "lexer should not synthesize explicit commas for whitespace-delimited arrays",
    );

    let second_element = tokens
        .iter()
        .find(|token| matches!(token.token_type, TokenType::Number(ref value) if value == "2"))
        .expect("expected to find second numeric literal");

    assert_eq!(second_element.leading_trivia.newlines, 1);
    assert!(
        second_element.leading_trivia.spaces >= 2,
        "indentation should be preserved so parser can infer layout commas",
    );
}

#[test]
fn layout_comma_metadata_survives_commented_call_arguments() {
    let source = "plot(1 /*hint*/\n2)";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    assert!(
        !tokens
            .iter()
            .any(|token| matches!(token.token_type, TokenType::Comma)),
        "lexer should keep call arguments whitespace-delimited without inserting commas",
    );

    let last_argument = tokens
        .iter()
        .find(|token| matches!(token.token_type, TokenType::Number(ref value) if value == "2"))
        .expect("expected to find second positional argument");

    assert_eq!(last_argument.leading_trivia.newlines, 1);
    assert!(
        last_argument.leading_trivia.comments,
        "leading trivia should remember inline block comment crossing the newline",
    );
    assert!(
        last_argument
            .leading_trivia
            .passthrough_comments
            .iter()
            .any(|comment| comment.text == "/*hint*/"),
        "passthrough comment should be preserved for downstream layout analysis",
    );
}

#[test]
fn implicit_params_in_arrays_keep_json_metadata_and_trivia() {
    let source = "[\"_label\" _2]";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer
        .tokenize()
        .expect("配列内の暗黙引数を含むソースの字句解析に失敗しました");

    let bracket = tokens
        .iter()
        .find(|token| matches!(token.token_type, TokenType::LeftBracket))
        .expect("開きブラケットが見つかりません");
    let json_confidence = bracket.metadata.iter().find_map(|metadata| match metadata {
        TokenMetadata::PotentialJsonStart { confidence } => Some(*confidence),
        _ => None,
    });
    assert!(
        json_confidence.is_some(),
        "JSON候補メタデータが欠落しています"
    );

    let implicit_param = tokens
        .iter()
        .find(|token| matches!(token.token_type, TokenType::ImplicitParam(2)))
        .expect("暗黙引数トークンを取得できません");
    assert!(
        implicit_param.leading_trivia.spaces > 0,
        "暗黙引数直前の空白がトリビアとして保持されていません"
    );
    let underscore_info = implicit_param
        .metadata
        .iter()
        .find_map(|meta| match meta {
            TokenMetadata::UnderscoreInfo(info) => Some(info),
            _ => None,
        })
        .expect("Underscoreメタデータが欠落しています");
    assert!(underscore_info.is_implicit);
    assert_eq!(underscore_info.number, Some(2));
}

#[test]
fn implicit_params_preserve_comment_trivia_and_metadata() {
    let source = "// carry\n_7";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer
        .tokenize()
        .expect("コメント付き暗黙引数の字句解析に失敗しました");

    let implicit_param = tokens
        .iter()
        .find(|token| matches!(token.token_type, TokenType::ImplicitParam(7)))
        .expect("暗黙引数トークンが見つかりません");

    assert!(implicit_param.leading_trivia.comments);
    assert_eq!(implicit_param.leading_trivia.passthrough_comments.len(), 1);
    let carry_text = &implicit_param.leading_trivia.passthrough_comments[0].text;
    assert!(carry_text.contains("carry"));

    let underscore_info = implicit_param
        .metadata
        .iter()
        .find_map(|meta| match meta {
            TokenMetadata::UnderscoreInfo(info) => Some(info),
            _ => None,
        })
        .expect("暗黙引数メタデータが見つかりません");
    assert_eq!(underscore_info.line, 2);
    assert_eq!(underscore_info.column, 1);
    assert_eq!(underscore_info.number, Some(7));
}

#[test]
fn invalid_implicit_param_emits_token_diagnostic() {
    let source = "_0";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer
        .tokenize()
        .expect("_0の診断検証用トークナイズに失敗しました");

    let invalid_token = tokens
        .iter()
        .find(|token| matches!(&token.token_type, TokenType::Invalid(value) if value == "_0"))
        .expect("Invalidトークンが見つかりません");

    let diagnostic = invalid_token
        .diagnostic
        .as_ref()
        .expect("診断が添付されていません");
    match diagnostic {
        TokenDiagnostic::InvalidImplicitParam { reason, suggested } => {
            assert_eq!(*reason, InvalidImplicitParamReason::LeadingZero);
            assert_eq!(suggested.as_deref(), Some("_1"));
        }
        other => panic!("想定外の診断: {other:?}"),
    }

    assert!(
        invalid_token
            .metadata
            .iter()
            .any(|meta| matches!(meta, TokenMetadata::UnderscoreInfo(_))),
        "InvalidトークンにUnderscoreメタデータが残っていません"
    );
}

#[test]
fn lex_wildcard_and_multiple_implicit_params() {
    let source = "_ _1 _999";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer
        .tokenize()
        .expect("暗黙引数の列を含むソースの字句解析に失敗しました");

    let essential: Vec<TokenType> = tokens
        .iter()
        .filter_map(|token| match &token.token_type {
            TokenType::Whitespace(_) | TokenType::Newline => None,
            other => Some(other.clone()),
        })
        .collect();

    assert!(matches!(essential.get(0), Some(TokenType::Underscore)));
    assert!(matches!(
        essential.get(1),
        Some(TokenType::ImplicitParam(1))
    ));
    assert!(matches!(
        essential.get(2),
        Some(TokenType::ImplicitParam(999))
    ));
    assert!(matches!(essential.last(), Some(TokenType::Eof)));
}

#[test]
fn alphanumeric_suffix_remains_identifier() {
    let source = "_1value";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer
        .tokenize()
        .expect("英数字混合識別子の字句解析に失敗しました");

    let identifier = tokens
        .iter()
        .find(|token| matches!(token.token_type, TokenType::Identifier(_)))
        .expect("識別子トークンが見つかりません");
    assert!(matches!(identifier.token_type, TokenType::Identifier(ref name) if name == "_1value"));
    assert!(
        tokens
            .iter()
            .all(|token| !matches!(token.token_type, TokenType::ImplicitParam(_)))
    );
}

#[test]
fn underscores_inside_strings_remain_literals() {
    let source = "\"prefix _1 suffix\"";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer
        .tokenize()
        .expect("文字列リテラルの字句解析に失敗しました");

    assert!(tokens.iter().any(|token| {
        matches!(token.token_type, TokenType::String(ref value) if value.contains("_1"))
    }));
    assert!(
        tokens
            .iter()
            .all(|token| !matches!(token.token_type, TokenType::ImplicitParam(_)))
    );
}

#[test]
fn underscores_inside_comments_do_not_emit_tokens() {
    let source = "// _3は無視\n_4";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer
        .tokenize()
        .expect("コメントを含むソースの字句解析に失敗しました");

    assert!(tokens.iter().any(|token| matches!(
        token.token_type,
        TokenType::LineComment(ref text) if text.contains("_3")
    )));
    assert!(
        tokens
            .iter()
            .filter(|token| matches!(token.token_type, TokenType::ImplicitParam(_)))
            .count()
            == 1
    );
}

#[test]
fn overflow_implicit_param_emits_diagnostic() {
    let source = "_4294967296";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer
        .tokenize()
        .expect("オーバーフロー検証用ソースの字句解析に失敗しました");

    let invalid_token = tokens
        .iter()
        .find(|token| matches!(&token.token_type, TokenType::Invalid(value) if value == "_4294967296"))
        .expect("オーバーフロー診断トークンが見つかりません");
    let diagnostic = invalid_token
        .diagnostic
        .as_ref()
        .expect("診断が添付されていません");
    match diagnostic {
        TokenDiagnostic::InvalidImplicitParam { reason, suggested } => {
            assert_eq!(*reason, InvalidImplicitParamReason::Overflow);
            assert!(suggested.is_none());
        }
        other => panic!("想定外の診断: {other:?}"),
    }
}

#[test]
fn test_arithmetic_operators_red_phase() {
    // RED: This test should fail
    let mut lexer = Lexer::new("a + b - c * d / e % f".to_string());
    let tokens = lexer.tokenize().unwrap();

    // Should recognize arithmetic operators
    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
    assert!(token_types.contains(&&TokenType::Plus));
    assert!(token_types.contains(&&TokenType::Minus));
    assert!(token_types.contains(&&TokenType::Multiply));
    assert!(token_types.contains(&&TokenType::Divide));
    assert!(token_types.contains(&&TokenType::Modulo));
}

#[test]
fn test_comparison_operators_red_phase() {
    // RED: This test should fail
    let mut lexer = Lexer::new("a == b != c < d <= e > f >= g".to_string());
    let tokens = lexer.tokenize().unwrap();

    // Should recognize comparison operators
    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
    assert!(token_types.contains(&&TokenType::Equal));
    assert!(token_types.contains(&&TokenType::NotEqual));
    assert!(token_types.contains(&&TokenType::Less));
    assert!(token_types.contains(&&TokenType::LessEqual));
    assert!(token_types.contains(&&TokenType::Greater));
    assert!(token_types.contains(&&TokenType::GreaterEqual));
}

#[test]
fn test_logical_operators_red_phase() {
    // RED: This test should fail
    let mut lexer = Lexer::new("a && b || !c".to_string());
    let tokens = lexer.tokenize().unwrap();

    // Should recognize logical operators
    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
    assert!(token_types.contains(&&TokenType::And));
    assert!(token_types.contains(&&TokenType::Or));
    assert!(token_types.contains(&&TokenType::Not));
}

#[test]
fn test_arrow_operators_red_phase() {
    // RED: This test should fail
    let mut lexer = Lexer::new("fun test() -> Int { x => x + 1 }".to_string());
    let tokens = lexer.tokenize().unwrap();

    // Should recognize arrow operators
    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
    assert!(token_types.contains(&&TokenType::Arrow)); // ->
    assert!(token_types.contains(&&TokenType::FatArrow)); // =>
}

#[test]
fn test_punctuation_red_phase() {
    // RED: This test should fail
    let mut lexer = Lexer::new("(a, b) { c[d]: e; f::g }".to_string());
    let tokens = lexer.tokenize().unwrap();

    // Should recognize all punctuation
    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
    assert!(token_types.contains(&&TokenType::LeftParen));
    assert!(token_types.contains(&&TokenType::RightParen));
    assert!(token_types.contains(&&TokenType::LeftBrace));
    assert!(token_types.contains(&&TokenType::RightBrace));
    assert!(token_types.contains(&&TokenType::LeftBracket));
    assert!(token_types.contains(&&TokenType::RightBracket));
    assert!(token_types.contains(&&TokenType::Comma));
    assert!(token_types.contains(&&TokenType::Colon));
    assert!(token_types.contains(&&TokenType::Semicolon));
    assert!(token_types.contains(&&TokenType::DoubleColon));
}

#[test]
fn test_comments_red_phase() {
    // RED: This test should fail
    let mut lexer =
        Lexer::new("val x = 1 // line comment\n/* block comment */ val y = 2".to_string());
    let tokens = lexer.tokenize().unwrap();

    // Should recognize both line and block comments
    let has_line_comment = tokens
        .iter()
        .any(|t| matches!(t.token_type, TokenType::LineComment(_)));
    let has_block_comment = tokens
        .iter()
        .any(|t| matches!(t.token_type, TokenType::BlockComment(_)));

    assert!(has_line_comment);
    assert!(has_block_comment);
}
#[test]
fn test_comment_trivia_passthrough_and_jv_only() {
    let source = "// keep
val keep = 1
/// drop
val drop = 2
//* star
val star = 3
";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().expect("tokenize comment sample");

    let mut val_tokens = tokens
        .iter()
        .filter(|token| matches!(token.token_type, TokenType::Val));

    let keep_val = val_tokens.next().expect("expected first val token");
    assert!(keep_val.leading_trivia.comments);
    assert_eq!(keep_val.leading_trivia.passthrough_comments.len(), 1);
    assert_eq!(
        keep_val.leading_trivia.passthrough_comments[0].text,
        "// keep"
    );
    assert!(keep_val.leading_trivia.jv_comments.is_empty());

    let drop_val = val_tokens.next().expect("expected second val token");
    assert!(drop_val.leading_trivia.comments);
    assert!(drop_val.leading_trivia.passthrough_comments.is_empty());
    assert_eq!(drop_val.leading_trivia.jv_comments.len(), 1);
    assert_eq!(drop_val.leading_trivia.jv_comments[0].text, "/// drop");

    let star_val = val_tokens.next().expect("expected third val token");
    assert!(star_val.leading_trivia.comments);
    assert!(star_val.leading_trivia.passthrough_comments.is_empty());
    assert_eq!(star_val.leading_trivia.jv_comments.len(), 1);
    assert_eq!(star_val.leading_trivia.jv_comments[0].text, "//* star");
}

#[test]
fn test_block_comment_trivia_passthrough() {
    let source = "/* keep */
val value = 1";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().expect("tokenize block comment sample");

    let val_token = tokens
        .iter()
        .find(|token| matches!(token.token_type, TokenType::Val))
        .expect("expected val token");

    assert!(val_token.leading_trivia.comments);
    assert_eq!(val_token.leading_trivia.passthrough_comments.len(), 1);
    let trivia = &val_token.leading_trivia.passthrough_comments[0];
    assert!(trivia.text.starts_with("/*"));
    assert!(trivia.text.ends_with("*/"));
    assert!(trivia.text.contains("keep"));
    assert_eq!(trivia.kind, SourceCommentKind::Block);
    assert!(val_token.leading_trivia.jv_comments.is_empty());
}

#[test]
fn test_multiline_jv_only_block_comment() {
    let source = "//* comment start
line in comment
*//
val value = 1";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().expect("tokenize jv-only block comment");

    let comment_token = tokens
        .iter()
        .find(|token| matches!(token.token_type, TokenType::LineComment(_)))
        .expect("expected comment token");

    if let TokenType::LineComment(text) = &comment_token.token_type {
        assert!(text.starts_with("/*"));
        assert!(text.ends_with("*//"));
        assert!(text.contains("line in comment"));
    } else {
        panic!("expected line comment token");
    }

    let val_token = tokens
        .iter()
        .find(|token| matches!(token.token_type, TokenType::Val))
        .expect("expected val token after comment");

    assert!(val_token.leading_trivia.passthrough_comments.is_empty());
    assert!(val_token.leading_trivia.jv_comments.is_empty());
}

#[test]
fn test_javadoc_comment_trivia_pass_through() {
    let source = "/**\n * Sample doc.\n */\nval answer = 42";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().expect("tokenize source with javadoc");

    let javadoc_token = tokens
        .iter()
        .find(|token| matches!(token.token_type, TokenType::JavaDocComment(_)))
        .expect("expected javadoc comment token");

    if let TokenType::JavaDocComment(content) = &javadoc_token.token_type {
        assert!(content.starts_with("**"));
        assert!(content.contains("Sample doc."));
    }

    let val_token = tokens
        .iter()
        .find(|token| matches!(token.token_type, TokenType::Val))
        .expect("expected val token following javadoc");

    let doc_comment = val_token
        .leading_trivia
        .doc_comment
        .as_ref()
        .expect("javadoc should attach to following token");
    assert!(doc_comment.starts_with("**"));
    assert!(doc_comment.contains("Sample doc."));
    assert!(val_token.leading_trivia.comments);
    assert!(val_token.leading_trivia.json_comments.is_empty());
}

#[test]
fn test_json_comment_trivia_attached_to_following_token() {
    let source = "{ // user config\n  \"key\": 1 }";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    let string_token = tokens
        .iter()
        .find(|token| matches!(token.token_type, TokenType::String(_)))
        .expect("string token should exist");

    assert!(string_token.leading_trivia.comments);
    assert_eq!(string_token.leading_trivia.json_comments.len(), 1);
    let trivia = &string_token.leading_trivia.json_comments[0];
    assert_eq!(trivia.kind, JsonCommentTriviaKind::Line);
    assert_eq!(trivia.line, 1);
    assert_eq!(trivia.text.trim(), "user config");
}

#[test]
fn test_potential_json_start_metadata_for_brace_and_bracket() {
    let source = "{\"key\": 1}\n[1, 2]";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    let brace_confidence = tokens
        .iter()
        .find(|token| matches!(token.token_type, TokenType::LeftBrace))
        .and_then(|token| {
            token.metadata.iter().find_map(|metadata| match metadata {
                TokenMetadata::PotentialJsonStart { confidence } => Some(*confidence),
                _ => None,
            })
        });

    assert!(matches!(brace_confidence, Some(JsonConfidence::High)));

    let bracket_confidence = tokens
        .iter()
        .find(|token| matches!(token.token_type, TokenType::LeftBracket))
        .and_then(|token| {
            token.metadata.iter().find_map(|metadata| match metadata {
                TokenMetadata::PotentialJsonStart { confidence } => Some(*confidence),
                _ => None,
            })
        });

    assert!(matches!(
        bracket_confidence,
        Some(JsonConfidence::Medium | JsonConfidence::High)
    ));
}

#[test]
fn test_block_brace_has_no_json_metadata() {
    let source = "{ val x = 1 }";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    let brace_metadata = tokens
        .iter()
        .find(|token| matches!(token.token_type, TokenType::LeftBrace))
        .map(|token| token.metadata.iter().collect::<Vec<_>>())
        .unwrap_or_default();

    assert!(
        brace_metadata
            .iter()
            .all(|metadata| !matches!(metadata, TokenMetadata::PotentialJsonStart { .. }))
    );
}

#[test]
fn test_position_tracking_green_phase() {
    // GREEN: Test position tracking functionality
    let mut lexer = Lexer::new("val\nname\n  =\n    42".to_string());
    let tokens = lexer.tokenize().unwrap();

    // Should track line and column positions correctly
    assert_eq!(tokens[0].line, 1); // val
    assert_eq!(tokens[0].column, 1);

    assert_eq!(tokens[1].line, 2); // name
    assert_eq!(tokens[1].column, 1);

    assert_eq!(tokens[2].line, 3); // =
    assert_eq!(tokens[2].column, 3);

    assert_eq!(tokens[3].line, 4); // 42
    assert_eq!(tokens[3].column, 5);
}

#[test_case("true" => TokenType::Boolean(true); "boolean true")]
#[test_case("false" => TokenType::Boolean(false); "boolean false")]
#[test_case("null" => TokenType::Null; "null literal")]
fn test_literal_values_red_phase(input: &str) -> TokenType {
    // RED: This test should fail
    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.tokenize().unwrap();

    tokens[0].token_type.clone()
}

#[test]
fn test_complex_string_interpolation_red_phase() {
    // RED: This test should fail
    let mut lexer = Lexer::new("\"User ${user.name} has ${user.age} years\"".to_string());
    let tokens = lexer.tokenize().unwrap();

    // Should handle complex interpolation with member access
    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();

    // Should contain identifiers and dots for member access
    assert!(token_types.contains(&&TokenType::Identifier("user".to_string())));
    assert!(token_types.contains(&&TokenType::Dot));
    assert!(token_types.contains(&&TokenType::Identifier("name".to_string())));
    assert!(token_types.contains(&&TokenType::Identifier("age".to_string())));
}

#[test]
fn test_lexer_error_handling_red_phase() {
    // RED: This test should fail
    let mut lexer = Lexer::new("\"unterminated string".to_string());
    let result = lexer.tokenize();

    // Should return an error for unterminated string
    assert!(result.is_err());
    match result.unwrap_err() {
        LexError::UnterminatedString(_, _) => {} // Expected
        _ => panic!("Wrong error type"),
    }
}

// Additional comprehensive tests for lexer
#[test]
fn test_complex_expressions() {
    let source = r#"
        val result = user?.profile?.name ?: "Unknown"
        val sum = (a + b) * (c - d) / e % f
    "#;
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    // Should contain all expected operators and symbols
    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
    assert!(token_types.contains(&&TokenType::Val));
    assert!(token_types.contains(&&TokenType::NullSafe));
    assert!(token_types.contains(&&TokenType::Elvis));
    assert!(token_types.contains(&&TokenType::LeftParen));
    assert!(token_types.contains(&&TokenType::RightParen));
    assert!(token_types.contains(&&TokenType::Plus));
    assert!(token_types.contains(&&TokenType::Minus));
    assert!(token_types.contains(&&TokenType::Multiply));
    assert!(token_types.contains(&&TokenType::Divide));
    assert!(token_types.contains(&&TokenType::Modulo));
}

#[test]
fn test_function_definitions() {
    let source = r#"
        fun add(a: Int, b: Int): Int {
            return a + b
        }

        fun greet(name: String = "World") {
            println("Hello, $name!")
        }
    "#;
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
    assert!(token_types.contains(&&TokenType::Fun));
    assert!(token_types.contains(&&TokenType::Return));
    assert!(token_types.contains(&&TokenType::Colon));
    assert!(token_types.contains(&&TokenType::Assign));
}

#[test]
fn test_data_class_syntax() {
    let source = "data class User(val name: String, var age: Int)";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
    assert!(token_types.contains(&&TokenType::Data));
    assert!(token_types.contains(&&TokenType::Class));
    assert!(token_types.contains(&&TokenType::Val));
    assert!(token_types.contains(&&TokenType::Var));
}

#[test]
fn test_when_expressions() {
    let source = r#"
        when (x) {
            0 -> "zero"
            1, 2 -> "one or two"
            in 3..10 -> "small"
            else -> "other"
        }
    "#;
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
    assert!(token_types.contains(&&TokenType::When));
    assert!(token_types.contains(&&TokenType::Arrow));
    assert!(token_types.contains(&&TokenType::In));
    assert!(token_types.contains(&&TokenType::RangeExclusive));
    assert!(token_types.contains(&&TokenType::Else));
}

#[test]
fn test_range_tokens() {
    let source = "for (i in 0..10) { a[i] } for (j in 0..=limit) { use(j) }";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    assert!(
        tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::RangeExclusive))
    );
    assert!(
        tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::RangeInclusive))
    );
}

#[test]
fn test_legacy_loop_diagnostic_metadata() {
    let source = "while (true) { break } do { continue } while (false)";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    let while_token = tokens
        .iter()
        .find(|t| matches!(t.token_type, TokenType::While))
        .expect("while token missing");
    assert!(matches!(
        while_token.diagnostic,
        Some(TokenDiagnostic::LegacyLoop {
            keyword: LegacyLoopKeyword::While,
        })
    ));

    let do_token = tokens
        .iter()
        .find(|t| matches!(t.token_type, TokenType::Do))
        .expect("do token missing");
    assert!(matches!(
        do_token.diagnostic,
        Some(TokenDiagnostic::LegacyLoop {
            keyword: LegacyLoopKeyword::Do,
        })
    ));
}

#[test]
fn test_async_spawn_syntax() {
    let source = r#"
        spawn {
            println("Running in virtual thread")
        }

        val future = async {
            computeValue()
        }.await()
    "#;
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    let identifiers: Vec<_> = tokens
        .iter()
        .filter_map(|t| match &t.token_type {
            TokenType::Identifier(name) => Some(name.as_str()),
            _ => None,
        })
        .collect();

    // Spawn/async/await should currently lex as identifiers until dedicated keywords are added
    assert!(identifiers.contains(&"spawn"));
    assert!(identifiers.contains(&"async"));
    assert!(identifiers.contains(&"await"));
}

#[test]
fn test_extension_functions() {
    let source = "fun String.reversed(): String = StringBuilder(this).reverse().toString()";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
    assert!(token_types.contains(&&TokenType::Fun));
    assert!(token_types.contains(&&TokenType::Dot));
    assert!(token_types.contains(&&TokenType::Assign));
}

#[test]
fn test_numbers_and_types() {
    let source = "val int = 42; val float = 3.14; val hex = 0xFF; val binary = 0b1010";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    let numbers: Vec<_> = tokens
        .iter()
        .filter_map(|t| match &t.token_type {
            TokenType::Number(n) => Some(n.as_str()),
            _ => None,
        })
        .collect();

    assert!(numbers.contains(&"42"));
    assert!(numbers.contains(&"3.14"));
    assert!(numbers.contains(&"0xFF") || numbers.contains(&"255"));
    assert!(numbers.contains(&"0b1010") || numbers.contains(&"10"));
}

#[test]
fn test_number_grouping_metadata() {
    let source = "val sum = SUM(1,234 5_678)";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    let comma_token = tokens
        .iter()
        .find(|token| matches!(&token.token_type, TokenType::Number(n) if n == "1234"))
        .expect("expected normalized comma number");
    let underscore_token = tokens
        .iter()
        .find(|token| matches!(&token.token_type, TokenType::Number(n) if n == "5678"))
        .expect("expected normalized underscore number");

    let comma_meta = comma_token
        .metadata
        .iter()
        .find_map(|meta| match meta {
            TokenMetadata::NumberLiteral(info) => Some(info),
            _ => None,
        })
        .expect("comma grouping metadata missing");
    assert_eq!(comma_meta.grouping, NumberGroupingKind::Comma);
    assert_eq!(comma_meta.original_lexeme, "1,234");

    let underscore_meta = underscore_token
        .metadata
        .iter()
        .find_map(|meta| match meta {
            TokenMetadata::NumberLiteral(info) => Some(info),
            _ => None,
        })
        .expect("underscore grouping metadata missing");
    assert_eq!(underscore_meta.grouping, NumberGroupingKind::Underscore);
    assert_eq!(underscore_meta.original_lexeme, "5_678");
}

#[test]
fn test_use_defer_syntax() {
    let source = r#"
        use (resource) {
            resource.process()
        }

        defer {
            cleanup()
        }
    "#;
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    let _token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
    // Note: Use, Defer keywords not yet implemented in TokenType enum
}

#[test]
fn test_array_and_collection_syntax() {
    let source = r#"
        val array = [1, 2, 3]
        val list = listOf("a", "b", "c")
        val map = mapOf("key" to "value")
    "#;
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
    assert!(token_types.contains(&&TokenType::LeftBracket));
    assert!(token_types.contains(&&TokenType::RightBracket));
    // Note: To keyword not yet implemented in TokenType enum
}

#[test]
fn test_edge_cases_and_errors() {
    // Test various edge cases
    let test_cases = vec![
        ("", true),             // Empty string should not fail
        ("//", true),           // Just comment
        ("/* */", true),        // Just block comment
        ("\"\"", true),         // Empty string literal
        ("'A'", true),          // Character literal
        ("''", false),          // Empty character literal should fail
        ("123.456.789", false), // Invalid number
        ("@invalid", true),     // Annotation tokens should be supported
    ];

    for (input, should_succeed) in test_cases {
        let mut lexer = Lexer::new(input.to_string());
        let result = lexer.tokenize();

        if should_succeed {
            assert!(result.is_ok(), "Failed to tokenize: '{}'", input);
        } else {
            assert!(
                result.is_err(),
                "Should have failed to tokenize: '{}'",
                input
            );
        }
    }
}

#[test]
fn test_character_literal_tokenization() {
    let mut lexer = Lexer::new("'A'".to_string());
    let tokens = lexer.tokenize().expect("character literal should tokenize");
    assert!(
        matches!(
            tokens.first().map(|t| &t.token_type),
            Some(TokenType::Character('A'))
        ),
        "unexpected token sequence: {:?}",
        tokens
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_annotation_tokens() {
    let source = "@Sample(\"data/users.json\", mode = Load)";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    assert_eq!(tokens[0].token_type, TokenType::At);
    assert_eq!(
        tokens[1].token_type,
        TokenType::Identifier("Sample".to_string())
    );

    let has_mode_identifier = tokens.iter().any(|token| match &token.token_type {
        TokenType::Identifier(name) => name == "mode",
        _ => false,
    });
    assert!(
        has_mode_identifier,
        "Expected to find 'mode' identifier token"
    );
}

#[test]
fn test_whitespace_and_newlines() {
    let source = "val\n\tx\n\t\t=\n\t\t\t42";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    // Should correctly handle various whitespace
    assert_eq!(tokens[0].token_type, TokenType::Val);
    assert_eq!(tokens[1].token_type, TokenType::Identifier("x".to_string()));
    assert_eq!(tokens[2].token_type, TokenType::Assign);
    assert_eq!(tokens[3].token_type, TokenType::Number("42".to_string()));
}

#[test]
fn test_unicode_identifiers() {
    let source = "val δ = 3.14; val α = β + γ";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().unwrap();

    let identifiers: Vec<_> = tokens
        .iter()
        .filter_map(|t| match &t.token_type {
            TokenType::Identifier(id) => Some(id.as_str()),
            _ => None,
        })
        .collect();

    assert!(identifiers.contains(&"δ"));
    assert!(identifiers.contains(&"α"));
    assert!(identifiers.contains(&"β"));
    assert!(identifiers.contains(&"γ"));
}

#[test]
fn trivia_records_whitespace_for_array_elements() {
    let mut lexer = Lexer::new("[1 2 3]".to_string());
    let tokens = lexer.tokenize().unwrap();
    let numbers: Vec<&Token> = tokens
        .iter()
        .filter(|token| matches!(token.token_type, TokenType::Number(_)))
        .collect();

    assert_eq!(numbers.len(), 3);
    assert_eq!(numbers[0].leading_trivia.spaces, 0);
    assert!(numbers[1].leading_trivia.spaces > 0);
    assert_eq!(numbers[1].leading_trivia.newlines, 0);
    assert!(numbers[2].leading_trivia.spaces > 0);
}

#[test]
fn layout_commas_preserve_whitespace_array_of_expressions() {
    let source = "[i (i +1) i +2 i+3 i +4]";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().expect("lex whitespace-delimited array");

    assert!(
        !tokens
            .iter()
            .any(|token| matches!(token.token_type, TokenType::Comma)),
        "explicit commas should not be synthesized for whitespace arrays"
    );

    let spaced_element_starts = tokens
        .iter()
        .filter(|token| {
            token.leading_trivia.newlines == 0
                && token.leading_trivia.spaces > 0
                && matches!(
                    token.token_type,
                    TokenType::Identifier(_) | TokenType::LeftParen
                )
        })
        .count();
    assert_eq!(
        spaced_element_starts, 4,
        "whitespace gaps between elements should be preserved via trivia"
    );

    let identifier_count = tokens
        .iter()
        .filter(|token| matches!(token.token_type, TokenType::Identifier(ref name) if name == "i"))
        .count();
    assert!(
        identifier_count >= 5,
        "expected repeated identifier tokens for each element"
    );
}

#[test]
fn trivia_records_absence_of_whitespace_for_comma_arrays() {
    let mut lexer = Lexer::new("[1,2,3]".to_string());
    let tokens = lexer.tokenize().unwrap();
    let numbers: Vec<&Token> = tokens
        .iter()
        .filter(|token| matches!(token.token_type, TokenType::Number(_)))
        .collect();

    assert_eq!(numbers.len(), 3);
    assert_eq!(numbers[1].leading_trivia.spaces, 0);
    assert_eq!(numbers[1].leading_trivia.newlines, 0);
    assert!(!numbers[1].leading_trivia.comments);
    assert_eq!(numbers[2].leading_trivia.spaces, 0);
}

#[test]
fn trivia_records_newlines_between_array_elements() {
    let mut lexer = Lexer::new("[1\n 2]".to_string());
    let tokens = lexer.tokenize().unwrap();
    let numbers: Vec<&Token> = tokens
        .iter()
        .filter(|token| matches!(token.token_type, TokenType::Number(_)))
        .collect();

    assert_eq!(numbers.len(), 2);
    assert!(numbers[1].leading_trivia.newlines > 0);
    assert!(numbers[1].leading_trivia.spaces > 0);
    assert!(!numbers[1].leading_trivia.comments);
}

#[test]
fn trivia_records_comments_between_array_elements() {
    let mut lexer = Lexer::new("[1 /* note */ 2]".to_string());
    let tokens = lexer.tokenize().unwrap();
    let numbers: Vec<&Token> = tokens
        .iter()
        .filter(|token| matches!(token.token_type, TokenType::Number(_)))
        .collect();

    assert_eq!(numbers.len(), 2);
    assert!(numbers[1].leading_trivia.comments);
    assert!(numbers[1].leading_trivia.spaces > 0);
}

#[test]
fn trivia_records_whitespace_for_call_arguments() {
    let mut lexer = Lexer::new("plot(1 2 3)".to_string());
    let tokens = lexer.tokenize().unwrap();
    let numbers: Vec<&Token> = tokens
        .iter()
        .filter(|token| matches!(token.token_type, TokenType::Number(_)))
        .collect();

    assert_eq!(numbers.len(), 3);
    assert_eq!(numbers[0].leading_trivia.spaces, 0);
    assert!(numbers[1].leading_trivia.spaces > 0);
    assert!(numbers[2].leading_trivia.spaces > 0);
}

#[test]
fn trivia_records_absence_of_whitespace_for_comma_arguments() {
    let mut lexer = Lexer::new("plot(1,2,3)".to_string());
    let tokens = lexer.tokenize().unwrap();
    let numbers: Vec<&Token> = tokens
        .iter()
        .filter(|token| matches!(token.token_type, TokenType::Number(_)))
        .collect();

    assert_eq!(numbers.len(), 3);
    assert_eq!(numbers[1].leading_trivia.spaces, 0);
    assert_eq!(numbers[2].leading_trivia.spaces, 0);
}

#[test]
fn char_scanner_handles_large_input_without_overflow() {
    let mut scanner = pipeline::CharScanner::new();
    let source = "a".repeat(10 * 1024 * 1024 + 128);
    let mut context = pipeline::LexerContext::new(&source);

    let checkpoint = scanner.save_position();
    let token = scanner
        .scan_next_token(&mut context)
        .expect("large scan succeeds");

    assert_eq!(token.kind, pipeline::RawTokenKind::Identifier);
    assert_eq!(token.text.len(), source.len());

    scanner.commit_position();
    scanner.discard_checkpoint(checkpoint);
}

#[test]
fn char_scanner_captures_trivia_for_identifiers() {
    let mut scanner = pipeline::CharScanner::new();
    let source = " \tfoo";
    let mut context = pipeline::LexerContext::new(source);

    let token = scanner
        .scan_next_token(&mut context)
        .expect("scan succeeds");

    assert_eq!(token.text, "foo");
    assert_eq!(token.kind, pipeline::RawTokenKind::Identifier);

    let trivia = token.trivia.expect("trivia expected");
    assert!(trivia.spaces >= 1);
    assert_eq!(trivia.newlines, 0);
}

#[test]
fn char_scanner_supports_checkpoint_restore_cycle() {
    let mut scanner = pipeline::CharScanner::new();
    let source = "foo bar";
    let mut context = pipeline::LexerContext::new(source);

    let checkpoint = scanner.save_position();

    let first = scanner.scan_next_token(&mut context).expect("first token");
    assert_eq!(first.text, "foo");

    scanner.restore_position(checkpoint);
    let second = scanner
        .scan_next_token(&mut context)
        .expect("token after restore");
    assert_eq!(second.text, "foo");

    scanner.discard_checkpoint(checkpoint);
}

#[test]
fn normalizer_records_number_grouping_metadata() {
    let mut scanner = pipeline::CharScanner::new();
    let mut normalizer = pipeline::Normalizer::new();
    let source = "1_234,567";
    let mut context = pipeline::LexerContext::new(source);

    let raw = scanner
        .scan_next_token(&mut context)
        .expect("raw token is produced");
    assert_eq!(raw.kind, pipeline::RawTokenKind::NumberCandidate);

    let normalized = normalizer
        .normalize(raw, &mut context)
        .expect("normalization succeeds");

    assert_eq!(normalized.normalized_text, "1234567");

    let grouping = normalized
        .metadata
        .provisional_metadata
        .iter()
        .find_map(|meta| match meta {
            TokenMetadata::NumberLiteral(info) => Some(info.grouping),
            _ => None,
        })
        .expect("number metadata present");

    assert_eq!(grouping, NumberGroupingKind::Mixed);
}

#[test]
fn normalizer_handles_unicode_escape_sequences() {
    let mut scanner = pipeline::CharScanner::new();
    let mut normalizer = pipeline::Normalizer::new();
    let source = "\"smile \\u{1F600}\"";
    let mut context = pipeline::LexerContext::new(source);

    let raw = scanner
        .scan_next_token(&mut context)
        .expect("raw token is produced");

    let normalized = normalizer
        .normalize(raw, &mut context)
        .expect("normalization succeeds");

    assert_eq!(normalized.normalized_text, "smile 😀");

    let delimiter = normalized
        .metadata
        .provisional_metadata
        .iter()
        .find_map(|meta| match meta {
            TokenMetadata::StringLiteral(info) => Some(info.delimiter),
            _ => None,
        })
        .expect("string metadata present");

    assert_eq!(delimiter, StringDelimiterKind::DoubleQuote);
}

#[test]
fn normalizer_preserves_multiline_strings() {
    let mut scanner = pipeline::CharScanner::new();
    let mut normalizer = pipeline::Normalizer::new();
    let source = "\"first line\nsecond line\"";
    let mut context = pipeline::LexerContext::new(source);

    let raw = scanner
        .scan_next_token(&mut context)
        .expect("raw token is produced");

    let normalized = normalizer
        .normalize(raw, &mut context)
        .expect("normalization succeeds");

    assert_eq!(normalized.normalized_text, "first line\nsecond line");

    let string_metadata = normalized
        .metadata
        .provisional_metadata
        .iter()
        .find_map(|meta| match meta {
            TokenMetadata::StringLiteral(info) => Some(info.clone()),
            _ => None,
        })
        .expect("string metadata present");

    assert_eq!(string_metadata.delimiter, StringDelimiterKind::DoubleQuote);
    assert!(!string_metadata.normalize_indentation);
}

fn span_with_len(len: usize) -> pipeline::Span {
    let start = pipeline::ScannerPosition::default();
    let mut end = start;
    end.advance(len, 0, len);
    pipeline::Span::new(0..len, start, end)
}

#[test]
fn classifier_resolves_keywords_and_identifiers() {
    let mut classifier = pipeline::Classifier::new();
    let mut ctx_keyword = pipeline::LexerContext::new("val");
    let raw_keyword = pipeline::RawToken {
        kind: pipeline::RawTokenKind::Identifier,
        text: "val",
        span: span_with_len(3),
        trivia: None,
        carry_over: None,
    };
    let normalized_keyword = pipeline::NormalizedToken::new(
        raw_keyword,
        "val".to_string(),
        pipeline::PreMetadata::default(),
    );
    let keyword = classifier
        .classify(normalized_keyword, &mut ctx_keyword)
        .expect("classify keyword");
    assert_eq!(keyword.token_type, TokenType::Val);

    let mut classifier = pipeline::Classifier::new();
    let mut ctx_identifier = pipeline::LexerContext::new("value");
    let raw_identifier = pipeline::RawToken {
        kind: pipeline::RawTokenKind::Identifier,
        text: "value",
        span: span_with_len(5),
        trivia: None,
        carry_over: None,
    };
    let normalized_identifier = pipeline::NormalizedToken::new(
        raw_identifier,
        "value".to_string(),
        pipeline::PreMetadata::default(),
    );
    let identifier = classifier
        .classify(normalized_identifier, &mut ctx_identifier)
        .expect("classify identifier");
    match identifier.token_type {
        TokenType::Identifier(ref name) => assert_eq!(name, "value"),
        other => panic!("expected identifier, got {:?}", other),
    }
}

#[test]
fn classifier_marks_string_interpolation_plan() {
    let mut classifier = pipeline::Classifier::new();
    let source = "\"Hello, ${name}!\"";
    let mut ctx = pipeline::LexerContext::new(source);
    let mut metadata = pipeline::PreMetadata::default();
    metadata
        .provisional_metadata
        .push(TokenMetadata::StringLiteral(StringLiteralMetadata {
            delimiter: StringDelimiterKind::DoubleQuote,
            allows_interpolation: true,
            normalize_indentation: false,
        }));
    metadata
        .provisional_metadata
        .push(TokenMetadata::StringInterpolation {
            segments: vec![
                StringInterpolationSegment::Literal("Hello, ".to_string()),
                StringInterpolationSegment::Expression("name".to_string()),
                StringInterpolationSegment::Literal("!".to_string()),
            ],
        });
    let raw = pipeline::RawToken {
        kind: pipeline::RawTokenKind::Symbol,
        text: source,
        span: span_with_len(source.len()),
        trivia: Some(TokenTrivia::default()),
        carry_over: None,
    };
    let normalized = pipeline::NormalizedToken::new(raw, source.to_string(), metadata);
    let classified = classifier
        .classify(normalized, &mut ctx)
        .expect("classify interpolation");
    assert!(matches!(
        classified.emission_plan,
        pipeline::EmissionPlan::StringInterpolation { .. }
    ));
    match classified.token_type {
        TokenType::StringInterpolation(ref text) => {
            assert!(text.contains("${name}"));
        }
        other => panic!("expected string interpolation token, got {:?}", other),
    }
}

#[test]
fn emitter_merges_comment_carry_into_trivia() {
    let mut emitter = pipeline::Emitter::new();
    let carry = CommentCarryOverMetadata {
        passthrough: vec![SourceCommentTrivia {
            kind: SourceCommentKind::Line,
            text: "// keep".to_string(),
            line: 1,
            column: 1,
        }],
        jv_only: Vec::new(),
        json: Vec::new(),
        doc_comment: Some("Doc".to_string()),
    };
    let metadata = vec![TokenMetadata::CommentCarryOver(carry)];
    let raw = pipeline::RawToken {
        kind: pipeline::RawTokenKind::Identifier,
        text: "value",
        span: span_with_len(5),
        trivia: Some(TokenTrivia::default()),
        carry_over: None,
    };
    let normalized =
        pipeline::NormalizedToken::new(raw, "value".to_string(), pipeline::PreMetadata::default());
    let classified = pipeline::ClassifiedToken::with_plan(
        normalized,
        TokenType::Identifier("value".to_string()),
        Vec::new(),
        metadata,
        pipeline::EmissionPlan::Direct,
    );
    let mut ctx = pipeline::LexerContext::new("value");
    let emitted = emitter
        .emit(classified, &mut ctx)
        .expect("emit identifier token");
    assert_eq!(emitted.len(), 1);
    let token = &emitted[0];
    assert!(token.leading_trivia.comments);
    assert_eq!(token.leading_trivia.passthrough_comments.len(), 1);
    assert_eq!(token.leading_trivia.passthrough_comments[0].text, "// keep");
    assert_eq!(token.leading_trivia.doc_comment.as_deref(), Some("Doc"));
    assert!(token.metadata.is_empty());
}

#[test]
fn pipeline_token_sequence_matches_expected_assignment() {
    let source = "val greeting = \"Hello, ${name}!\"";
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer
        .tokenize()
        .expect("tokenize greeting assignment through pipeline");
    let token_types: Vec<TokenType> = tokens.into_iter().map(|token| token.token_type).collect();
    assert_eq!(
        token_types,
        vec![
            TokenType::Val,
            TokenType::Identifier("greeting".to_string()),
            TokenType::Assign,
            TokenType::StringStart,
            TokenType::Identifier("name".to_string()),
            TokenType::StringEnd,
            TokenType::Eof,
        ]
    );
}

#[test]
fn no_layout_comma_inside_string_interpolation_expressions() {
    let source = r#"
        fun main() {
            val x = 10
            val y = 20
            println("${x} + ${y} = ${x + y}")
        }
    "#;
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer
        .tokenize()
        .expect("tokenize function with interpolation");

    let layout_commas = tokens
        .iter()
        .filter(|token| matches!(token.token_type, TokenType::LayoutComma))
        .count();
    assert_eq!(
        layout_commas, 0,
        "layout commas should not appear inside interpolation expressions"
    );
}

#[test]
fn regex_literal_basic_tokenization() {
    let mut lexer = Lexer::new("val pattern = /abc/".to_string());
    let tokens = lexer.tokenize().unwrap();

    let regex_token = tokens
        .iter()
        .find(|token| matches!(token.token_type, TokenType::RegexLiteral(_)))
        .expect("expected regex literal token");

    assert_eq!(regex_token.lexeme, "abc");
    assert_eq!(
        regex_token.token_type,
        TokenType::RegexLiteral("abc".to_string())
    );

    let metadata = regex_token
        .metadata
        .iter()
        .find_map(|meta| match meta {
            TokenMetadata::RegexLiteral { raw, pattern } => Some((raw, pattern)),
            _ => None,
        })
        .expect("regex metadata should be present");

    assert_eq!(metadata.0, "/abc/");
    assert_eq!(metadata.1, "abc");
}

#[test]
fn regex_literal_preserves_escaped_slash() {
    let mut lexer = Lexer::new("val pattern = /a\\/b/".to_string());
    let tokens = lexer.tokenize().unwrap();

    let regex_token = tokens
        .iter()
        .find(|token| matches!(token.token_type, TokenType::RegexLiteral(_)))
        .expect("expected regex literal token");

    assert_eq!(
        regex_token.token_type,
        TokenType::RegexLiteral("a/b".to_string())
    );
}

#[test]
fn regex_literal_reports_unterminated_pattern() {
    let mut lexer = Lexer::new("/abc".to_string());
    let error = lexer
        .tokenize()
        .expect_err("expected unterminated regex error");

    assert!(matches!(error, LexError::UnterminatedRegex { .. }));
}

#[test]
fn regex_literal_rejects_tab_character() {
    let mut lexer = Lexer::new("/a\tb/".to_string());
    let error = lexer
        .tokenize()
        .expect_err("expected invalid regex character error");

    assert!(matches!(
        error,
        LexError::InvalidRegexCharacter {
            character: '\t',
            ..
        }
    ));
}

#[test]
fn string_literal_with_regex_delimiters_remains_string() {
    let mut lexer = Lexer::new("val message = \"/not/a/regex/\"".to_string());
    let tokens = lexer
        .tokenize()
        .expect("string literal containing slashes should tokenize");

    let string_token = tokens.iter().find_map(|token| match &token.token_type {
        TokenType::String(value) => Some(value.clone()),
        _ => None,
    });

    assert_eq!(
        string_token.as_deref(),
        Some("/not/a/regex/"),
        "string literal contents should be preserved"
    );

    assert!(
        tokens
            .iter()
            .all(|token| !matches!(token.token_type, TokenType::RegexLiteral(_))),
        "string literal should not emit RegexLiteral tokens"
    );
}

#[test]
fn random_string_literals_never_lex_as_regex_literals() {
    const ALPHABET: &[char] = &[
        '/', '\\', ' ', '-', '_', '.', ':', ';', ',', '+', '*', '?', '=', '#', '0', '1', '2', '3',
        '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l',
        'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D',
        'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',
        'W', 'X', 'Y', 'Z',
    ];

    let mut rng = Rng::with_seed(0xDEAD_5A5Eu64);
    for iteration in 0..256 {
        let length = rng.usize(0..24);
        let mut literal = String::with_capacity(length);
        for _ in 0..length {
            let idx = rng.usize(0..ALPHABET.len());
            literal.push(ALPHABET[idx]);
        }

        let escaped = literal.replace('\\', "\\\\").replace('"', "\\\"");
        let source = format!("val sample = \"{escaped}\"");

        let mut lexer = Lexer::new(source.clone());
        let tokens = lexer.tokenize().unwrap_or_else(|error| {
            panic!("iteration {iteration}: failed to tokenize {source:?}: {error:?}")
        });

        let string_token = tokens.iter().find_map(|token| match &token.token_type {
            TokenType::String(value) => Some(value.clone()),
            _ => None,
        });

        assert_eq!(
            string_token.as_deref(),
            Some(literal.as_str()),
            "iteration {iteration}: string literal contents should match input"
        );

        assert!(
            tokens
                .iter()
                .all(|token| !matches!(token.token_type, TokenType::RegexLiteral(_))),
            "iteration {iteration}: no RegexLiteral tokens should be produced"
        );
    }
}

#[test]
fn arithmetic_division_remains_unchanged() {
    let mut lexer = Lexer::new("result = 4 / 2".to_string());
    let tokens = lexer.tokenize().unwrap();

    assert!(
        tokens
            .iter()
            .any(|token| matches!(token.token_type, TokenType::Divide)),
        "division operator should be preserved"
    );

    assert!(
        tokens
            .iter()
            .all(|token| !matches!(token.token_type, TokenType::RegexLiteral(_))),
        "regex literal token should not appear in arithmetic expression"
    );
}

fn fixtures_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../tests/fixtures")
}

fn collect_fixture_files(dir: &Path, files: &mut Vec<PathBuf>) {
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.file_name().is_some_and(|name| name == "target") {
                continue;
            }
            if path.is_dir() {
                collect_fixture_files(&path, files);
            } else if path.extension().is_some_and(|ext| ext == "jv") {
                files.push(path);
            }
        }
    }
}

#[test]
fn tokenize_repository_fixtures() {
    let root = fixtures_root();
    let mut files = Vec::new();
    collect_fixture_files(&root, &mut files);
    assert!(
        !files.is_empty(),
        "expected to discover at least one fixture under {:?}",
        root
    );

    let mut failures = Vec::new();
    for path in files {
        let source = match fs::read_to_string(&path) {
            Ok(content) => content,
            Err(err) => {
                failures.push((path.display().to_string(), format!("read error: {err}")));
                continue;
            }
        };
        let mut lexer = Lexer::new(source);
        if let Err(err) = lexer.tokenize() {
            failures.push((path.display().to_string(), err.to_string()));
        }
    }

    if !failures.is_empty() {
        let details = failures
            .into_iter()
            .map(|(path, err)| format!("{path}: {err}"))
            .collect::<Vec<_>>()
            .join("\n");
        panic!("lexer failed to tokenize fixtures:\n{details}");
    }
}

#[test]
fn token_type_serde_roundtrip_supports_underscore_variants() {
    let cases = vec![
        TokenType::Underscore,
        TokenType::ImplicitParam(42),
        TokenType::ImplicitParam(u32::MAX),
    ];

    for token in cases {
        let json = to_string(&token).expect("serialize token type");
        let restored: TokenType = from_str(&json).expect("deserialize token type");
        assert_eq!(restored, token);
    }
}

#[test]
fn token_metadata_roundtrip_preserves_underscore_info() {
    let metadata = TokenMetadata::UnderscoreInfo(UnderscoreInfoMetadata {
        raw: "_123".to_string(),
        is_implicit: true,
        number: Some(123),
        line: 12,
        column: 4,
        length: 4,
        in_non_code_region: false,
    });

    let json = to_string(&metadata).expect("serialize underscore metadata");
    let restored: TokenMetadata = from_str(&json).expect("deserialize underscore metadata");
    assert_eq!(restored, metadata);
}

#[test]
fn token_diagnostic_roundtrip_covers_invalid_implicit_param() {
    let diagnostic = TokenDiagnostic::InvalidImplicitParam {
        reason: InvalidImplicitParamReason::LeadingZero,
        suggested: Some("_1".to_string()),
    };

    let json = to_string(&diagnostic).expect("serialize diagnostic");
    let restored: TokenDiagnostic = from_str(&json).expect("deserialize diagnostic");
    assert_eq!(restored, diagnostic);
}
