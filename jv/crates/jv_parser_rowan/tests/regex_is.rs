//! `when` 分岐と正規表現リテラルの統合を検証するテスト。

use jv_ast::{Expression, Literal, Statement, types::Pattern};
use jv_parser_frontend::ParserPipeline;
use jv_parser_rowan::frontend::RowanPipeline;

fn extract_when_expression(source: &str) -> Expression {
    let program = RowanPipeline::default()
        .parse(source)
        .expect("ソースコードが構文解析できること")
        .into_program();

    for statement in program.statements {
        match statement {
            Statement::ValDeclaration { initializer, .. }
            | Statement::Expression { expr: initializer, .. } => {
                if matches!(initializer, Expression::When { .. }) {
                    return initializer;
                }
            }
            Statement::VarDeclaration {
                initializer: Some(initializer),
                ..
            } if matches!(initializer, Expression::When { .. }) => {
                return initializer;
            }
            _ => continue,
        }
    }

    panic!("when 式を含む文を検出できませんでした: {source}");
}

#[test]
fn when分岐で正規表現リテラルがパターンとして扱われる() {
    let source = r#"
val text: String = "123"
val result = when (text) {
    /\d+/ -> true
    else -> false
}
"#;

    let when_expr = extract_when_expression(source);

    let Expression::When { arms, .. } = when_expr else {
        panic!("when 式が生成されることを期待しました");
    };

    let first_arm = arms.first().expect("1つ目の分岐が存在すること");

    match &first_arm.pattern {
        Pattern::Literal(Literal::Regex(literal), _) => {
            assert_eq!(
                literal.pattern, "\\d+",
                "正規表現リテラルのパターンが保持される想定です: {:?}",
                literal
            );
        }
        other => panic!(
            "正規表現リテラルが Pattern::Literal として保存される想定です: {:?}",
            other
        ),
    }
    assert!(
        first_arm.guard.is_none(),
        "リテラルパターンでは追加のガードが不要な想定です"
    );
}
