use super::lowering_cases::lower_source;
use jv_ast::expression::Expression;
use jv_ast::types::{Literal, Pattern, RegexLiteral};
use jv_ast::Statement;

fn when_expression_from(source: &str, binding: &str) -> Expression {
    let result = lower_source(source);
    assert!(
        result.diagnostics.is_empty(),
        "予期しない診断が発生しました: {:?}",
        result.diagnostics
    );

    result
        .statements
        .iter()
        .find_map(|statement| match statement {
            Statement::ValDeclaration {
                name, initializer, ..
            } if name == binding => Some(initializer.clone()),
            _ => None,
        })
        .unwrap_or_else(|| {
            let available: Vec<_> = result
                .statements
                .iter()
                .filter_map(|statement| match statement {
                    Statement::ValDeclaration { name, .. } => Some(name.as_str()),
                    Statement::VarDeclaration { name, .. } => Some(name.as_str()),
                    Statement::Expression { .. } => Some("<expression>"),
                    _ => None,
                })
                .collect();
            panic!(
                "{binding} のwhen式が見つかりません: 利用可能な宣言 {:?}",
                available
            );
        })
}

fn assert_range_pattern(
    pattern: &Pattern,
    expected_inclusive: bool,
    start_literal: &str,
    end_literal: &str,
) {
    match pattern {
        Pattern::Range {
            start,
            end,
            inclusive_end,
            ..
        } => {
            assert_eq!(
                *inclusive_end, expected_inclusive,
                "範囲パターンのinclusive_endが期待値と一致しません"
            );
            match (start.as_ref(), end.as_ref()) {
                (
                    Expression::Literal(Literal::Number(start_value), _),
                    Expression::Literal(Literal::Number(end_value), _),
                ) => {
                    assert_eq!(start_value, start_literal, "開始値が一致しません");
                    assert_eq!(end_value, end_literal, "終了値が一致しません");
                }
                other => panic!("数値リテラルの境界ではありません: {:?}", other),
            }
        }
        other => panic!("範囲パターンを期待しましたが {:?} でした", other),
    }
}

#[test]
fn when_範囲パターンの包括性を検証する() {
    let source = r#"
        val rating = when (score) {
            in 0..10 -> "low"
            in 0..=10 -> "mid"
            else -> "high"
        }
    "#;

    let Expression::When { arms, else_arm, .. } = when_expression_from(source, "rating") else {
        panic!("when式が得られませんでした");
    };
    assert_eq!(arms.len(), 2, "条件腕の数が想定と異なります");

    assert_range_pattern(&arms[0].pattern, false, "0", "10");
    assert_range_pattern(&arms[1].pattern, true, "0", "10");

    let else_expr = else_arm
        .as_ref()
        .expect("else腕が保持されているはずです")
        .as_ref();
    match else_expr {
        Expression::Literal(Literal::String(value), _) => assert_eq!(value, "high"),
        other => panic!("else腕の式が期待どおりではありません: {:?}", other),
    }
}

#[test]
fn when_リテラルと正規表現パターンを検証する() {
    let string_expr = when_expression_from(
        "val result = when (value) { \"ok\" -> 1 else -> 0 }",
        "result",
    );
    let Expression::When { arms, .. } = string_expr else {
        panic!("文字列パターンのwhen式が得られませんでした");
    };
    match &arms[0].pattern {
        Pattern::Literal(Literal::String(value), _) => assert_eq!(value, "ok"),
        other => panic!("文字列リテラルパターンを期待しました: {:?}", other),
    }

    let boolean_expr = when_expression_from(
        "val result = when (value) { true -> 1 else -> 0 }",
        "result",
    );
    let Expression::When { arms, .. } = boolean_expr else {
        panic!("真偽値パターンのwhen式が得られませんでした");
    };
    match &arms[0].pattern {
        Pattern::Literal(Literal::Boolean(flag), _) => assert!(*flag),
        other => panic!("真偽値リテラルパターンを期待しました: {:?}", other),
    }

    let null_expr = when_expression_from(
        "val result = when (value) { null -> 1 else -> 0 }",
        "result",
    );
    let Expression::When { arms, .. } = null_expr else {
        panic!("nullパターンのwhen式が得られませんでした");
    };
    match &arms[0].pattern {
        Pattern::Literal(Literal::Null, _) => {}
        other => panic!("nullリテラルパターンを期待しました: {:?}", other),
    }

    let regex_expr = when_expression_from(
        "val result = when (value) { /foo/ -> 1 else -> 0 }",
        "result",
    );
    let Expression::When { arms, .. } = regex_expr else {
        panic!("正規表現パターンのwhen式が得られませんでした");
    };
    match &arms[0].pattern {
        Pattern::Literal(
            Literal::Regex(RegexLiteral {
                pattern,
                raw,
                span,
                ..
            }),
            outer_span,
        ) => {
            assert_eq!(pattern, "foo", "正規表現パターンが一致しません");
            assert_eq!(raw, "/foo/", "正規表現のraw表現が一致しません");
            assert_eq!(
                span, outer_span,
                "RegexLiteralのspanとPattern spanが異なります"
            );
        }
        other => panic!("正規表現パターンを期待しました: {:?}", other),
    }
}

#[test]
fn when_コンストラクタガードのスパンを検証する() {
    let source = r#"
        val label = when (value) {
            is String && value.length > 0 -> value
            else -> "empty"
        }
    "#;

    let Expression::When { arms, .. } = when_expression_from(source, "label") else {
        panic!("when式が得られませんでした");
    };
    let first_arm = arms.first().expect("最初の腕が存在するはずです");

    let Pattern::Constructor {
        span: pattern_span,
        name,
        ..
    } = &first_arm.pattern
    else {
        panic!("コンストラクタパターンを期待しました");
    };
    assert_eq!(name, "String", "`is String` が保持されていません");

    let guard = first_arm
        .guard
        .as_ref()
        .expect("ガード式が存在するはずです");
    let Expression::Binary {
        span: guard_span, ..
    } = guard
    else {
        panic!("ガード式は二項演算となるはずです: {:?}", guard);
    };
    assert!(
        guard_span.start_line >= pattern_span.start_line,
        "ガードの開始行がパターンより前になっています"
    );
    assert!(
        guard_span.start_column > pattern_span.start_column,
        "ガードの開始列がパターンの列を超えていません: pattern={pattern_span:?}, guard={guard_span:?}"
    );
}

#[test]
fn when_主題なしwhenがガードとelseを保持する() {
    let source = r#"
        val flag = when {
            condition() -> true
            else -> false
        }
    "#;

    let Expression::When {
        expr,
        arms,
        else_arm,
        ..
    } = when_expression_from(source, "flag")
    else {
        panic!("when式が得られませんでした");
    };

    assert!(expr.is_none(), "主題なしwhenではexprがNoneであるべきです");
    assert_eq!(arms.len(), 1, "条件腕が1件存在するはずです");
    let else_expr = else_arm
        .as_ref()
        .expect("主題なしwhenでもelseが保持されるはずです")
        .as_ref();
    match else_expr {
        Expression::Literal(Literal::Boolean(flag), _) => assert!(!flag),
        other => panic!("else腕の式が期待どおりではありません: {:?}", other),
    }

    match &arms[0].pattern {
        Pattern::Wildcard(_) => {}
        other => panic!(
            "主題なしwhenの最初の腕はワイルドカードになるはずです: {:?}",
            other
        ),
    }

    match &arms[0].guard {
        Some(Expression::Call { .. }) => {}
        other => panic!("guardが関数呼び出しとして保持されていません: {:?}", other),
    }
}
