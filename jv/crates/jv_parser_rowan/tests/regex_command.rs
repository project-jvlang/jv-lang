//! 簡潔な正規表現コマンド構文のパーサ統合テスト。

use jv_ast::{
    Expression, RegexCommand, RegexCommandMode, RegexCommandModeOrigin, RegexFlag, Statement,
};
use jv_parser_frontend::ParserPipeline;
use jv_parser_rowan::frontend::RowanPipeline;

fn extract_regex_command(source: &str) -> RegexCommand {
    let program = RowanPipeline::default()
        .parse(source)
        .expect("ソースコードが構文解析できること")
        .into_program();

    for statement in program.statements {
        let candidate = match statement {
            Statement::ValDeclaration { initializer, .. }
            | Statement::Expression {
                expr: initializer, ..
            } => initializer,
            Statement::VarDeclaration {
                initializer: Some(initializer),
                ..
            } => initializer,
            _ => continue,
        };
        if let Expression::RegexCommand(command) = candidate {
            return (*command).clone();
        }
    }

    panic!("対象となる式を検出できませんでした: {source}");
}

#[test]
fn default_match_mode_is_inferred() {
    let source = r#"
val subject = "value123"
val matched = /subject/\d+/
"#;

    let command = extract_regex_command(source);

    assert_eq!(command.mode, RegexCommandMode::Match);
    assert!(matches!(
        command.mode_origin,
        RegexCommandModeOrigin::DefaultMatch
    ));
    assert!(command.replacement.is_none());
    assert!(command.flags.is_empty());
}

#[test]
fn short_mode_with_replacement_and_flags_parses() {
    let source = r#"
val replaced = a/sub/\d+/"X"/ims
"#;

    let command = extract_regex_command(source);

    assert_eq!(command.mode, RegexCommandMode::All);
    assert!(matches!(
        command.mode_origin,
        RegexCommandModeOrigin::ShortMode
    ));
    let replacement = command
        .replacement
        .as_ref()
        .expect("置換情報が存在すること");
    match replacement {
        jv_ast::RegexReplacement::Literal(literal) => {
            assert_eq!(literal.normalized, "X");
        }
        other => panic!("リテラル置換を期待しました: {other:?}"),
    }

    let flags: Vec<RegexFlag> = command.flags.clone();
    assert!(flags.contains(&RegexFlag::CaseInsensitive));
    assert!(flags.contains(&RegexFlag::Multiline));
    assert!(flags.contains(&RegexFlag::DotAll));
}

#[test]
fn first_mode_literal_replacement_is_preserved() {
    let source = r#"
val once = f/text/\d+/"first"/
"#;

    let command = extract_regex_command(source);

    assert_eq!(command.mode, RegexCommandMode::First);
    assert!(matches!(
        command.mode_origin,
        RegexCommandModeOrigin::ShortMode
    ));

    let replacement = command
        .replacement
        .as_ref()
        .expect("first モードでも置換部が存在すること");
    match replacement {
        jv_ast::RegexReplacement::Literal(literal) => {
            assert_eq!(literal.normalized, "first");
            assert_eq!(literal.raw, "\"first\"");
        }
        other => panic!("リテラル置換を期待しました: {other:?}"),
    }
    assert!(command.flags.is_empty(), "明示フラグが無い場合は空ベクタ");
}

#[test]
fn split_mode_without_replacement_has_no_replacement() {
    let source = r#"
val text = "a,b,c"
val parts = s/text/\s+/
"#;

    let command = extract_regex_command(source);

    assert_eq!(command.mode, RegexCommandMode::Split);
    assert!(
        command.replacement.is_none(),
        "Split モードでは置換が不要です"
    );
    assert_eq!(
        command.pattern.pattern, "\\s+",
        "パターンの正規化結果が保持されること"
    );
    assert!(
        command.flags.is_empty(),
        "明示フラグが無い場合は空集合として扱う想定です: {:?}",
        command.flags
    );
}

#[test]
fn iterate_mode_lambda_replacement_is_captured() {
    let source = r#"
val text = "value123"
val joined = i/text/\w+/{ match -> match.group(1).toUpperCase() }/
"#;

    let command = extract_regex_command(source);

    assert_eq!(command.mode, RegexCommandMode::Iterate);
    let replacement = command
        .replacement
        .as_ref()
        .expect("ラムダ置換が保持されること");
    match replacement {
        jv_ast::RegexReplacement::Lambda(lambda) => {
            assert_eq!(
                lambda.params.len(),
                1,
                "ラムダ置換には1つのパラメータが存在する想定です: {:?}",
                lambda.params
            );
            if let Some(param) = lambda.params.first() {
                assert_eq!(
                    param.name, "match",
                    "先頭パラメータ名が match である想定です: {:?}",
                    param
                );
            }
            assert!(
                matches!(
                    lambda.body.as_ref(),
                    Expression::Call { .. }
                        | Expression::MemberAccess { .. }
                        | Expression::Binary { .. }
                        | Expression::Identifier(_, _)
                ),
                "ラムダ本体が式として保持される必要があります: {:?}",
                lambda.body
            );
        }
        other => panic!("ラムダ置換を期待しました: {other:?}"),
    }
}
