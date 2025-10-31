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
