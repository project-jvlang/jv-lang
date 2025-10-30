//! `when` 分岐と `is` 演算子の正規表現リテラル統合を検証するテスト。

use jv_ast::{
    Expression, IsTestKind, Literal, RegexGuardStrategy, Statement, types::Pattern, BinaryOp,
};
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

fn extract_is_expression(source: &str) -> Expression {
    let program = RowanPipeline::default()
        .parse(source)
        .expect("ソースコードが構文解析できること")
        .into_program();

    for statement in program.statements {
        match statement {
            Statement::ValDeclaration { initializer, .. }
            | Statement::Expression { expr: initializer, .. } => {
                if matches!(
                    initializer,
                    Expression::Binary {
                        op: BinaryOp::Is, ..
                    }
                ) {
                    return initializer;
                }
            }
            Statement::VarDeclaration {
                initializer: Some(initializer),
                ..
            } if matches!(
                initializer,
                Expression::Binary {
                    op: BinaryOp::Is, ..
                }
            ) =>
            {
                return initializer;
            }
            _ => continue,
        }
    }

    panic!("`is` 演算子を含む二項式を検出できませんでした: {source}");
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

#[test]
fn is演算子の正規表現判定にメタデータが付与される() {
    let source = r#"
val maybe: String? = null
val judged = maybe is /\d+/
"#;

    let expr = extract_is_expression(source);

    let Expression::Binary { metadata, .. } = expr else {
        panic!("二項式の生成を期待しました");
    };
    let Some(is_test) = metadata.is_test else {
        panic!("`is` 判定のメタデータが存在することを期待しました: {metadata:?}");
    };

    assert!(
        matches!(is_test.kind, IsTestKind::RegexLiteral),
        "`is` 判定は正規表現リテラルとして分類される想定です: {:?}",
        is_test.kind
    );

    let regex = is_test
        .regex
        .as_ref()
        .expect("正規表現リテラル情報が格納されている想定です");
    assert_eq!(
        regex.raw, "/\\d+/",
        "正規表現リテラルの raw 表現が保持されている想定です"
    );
    assert_eq!(
        regex.pattern, "\\d+",
        "正規表現リテラルの正規化済みパターンが保持されている想定です"
    );
    assert!(
        is_test.pattern_expr.is_none(),
        "リテラル判定では pattern_expr が空になる想定です"
    );
    assert!(
        matches!(is_test.guard_strategy, RegexGuardStrategy::None),
        "ローアリング段階ではガード戦略が None である想定です: {:?}",
        is_test.guard_strategy
    );
}
