use super::lowering_cases::lower_source;
use jv_ast::expression::Expression;
use jv_ast::statement::ValBindingOrigin;
use jv_ast::types::{BinaryOp, Literal, TypeAnnotation};
use jv_ast::{BindingPatternKind, Statement};

#[test]
fn 暗黙型付きval宣言のメタデータを保持する() {
    let result = lower_source("name: String = \"hello\"");
    assert!(
        result.diagnostics.is_empty(),
        "診断が発生しないはずですが {:?} が検出されました",
        result.diagnostics
    );

    let statement = result
        .statements
        .first()
        .expect("ステートメントが1件生成されるはずです");

    match statement {
        Statement::ValDeclaration {
            name,
            binding,
            type_annotation,
            initializer,
            origin: origin_value,
            ..
        } => {
            assert_eq!(name, "name", "binding名が想定と異なります");
            assert_eq!(
                *origin_value,
                ValBindingOrigin::ImplicitTyped,
                "ValBindingOriginが暗黙型付きとして設定されていません"
            );
            match type_annotation {
                Some(TypeAnnotation::Simple(type_name)) => {
                    assert_eq!(type_name, "String", "型注釈が欠落しています")
                }
                other => panic!("型注釈が期待どおりではありません: {:?}", other),
            }
            let pattern = binding
                .as_ref()
                .expect("bindingパターンが付与されているはずです");
            match pattern {
                BindingPatternKind::Identifier {
                    name: pattern_name, ..
                } => assert_eq!(pattern_name, "name", "bindingパターンの識別子が不一致です"),
                other => panic!(
                    "識別子バインディング以外の形状が生成されました: {:?}",
                    other
                ),
            }
            match initializer {
                Expression::Literal(Literal::String(value), _) => {
                    assert_eq!(value, "hello", "文字列リテラルが期待値と異なります")
                }
                other => panic!("初期化式が文字列リテラルではありません: {:?}", other),
            }
        }
        other => panic!("val宣言が得られませんでした: {:?}", other),
    }
}

#[test]
fn 暗黙val宣言を代入構文から復元する() {
    let result = lower_source("message = \"world\"");
    assert!(
        result.diagnostics.is_empty(),
        "診断が発生しないはずですが {:?} が検出されました",
        result.diagnostics
    );

    let statement = result
        .statements
        .first()
        .expect("ステートメントが1件生成されるはずです");

    match statement {
        Statement::ValDeclaration {
            name,
            binding,
            type_annotation,
            initializer,
            origin: origin_value,
            ..
        } => {
            assert_eq!(name, "message", "binding名が想定と異なります");
            let pattern = binding
                .as_ref()
                .expect("暗黙宣言でもバインディングパターンが保持されるはずです");
            match pattern {
                BindingPatternKind::Identifier {
                    name: pattern_name, ..
                } => assert_eq!(
                    pattern_name, "message",
                    "bindingパターンの識別子が不一致です"
                ),
                other => panic!(
                    "識別子バインディング以外の形状が生成されました: {:?}",
                    other
                ),
            }
            assert!(
                type_annotation.is_none(),
                "暗黙宣言では型注釈が付与されないはずです"
            );
            assert_eq!(
                *origin_value,
                ValBindingOrigin::Implicit,
                "ValBindingOriginが暗黙起源として設定されていません"
            );
            match initializer {
                Expression::Literal(Literal::String(value), _) => {
                    assert_eq!(value, "world", "文字列リテラルが期待値と異なります")
                }
                other => panic!("初期化式が文字列リテラルではありません: {:?}", other),
            }
        }
        other => panic!("暗黙val宣言が生成されませんでした: {:?}", other),
    }
}

#[test]
fn null安全演算子チェーンが保持される() {
    let result = lower_source("val length = user?.name?.length ?: 0");
    assert!(
        result.diagnostics.is_empty(),
        "診断が発生しないはずですが {:?} が検出されました",
        result.diagnostics
    );

    let statement = result
        .statements
        .first()
        .expect("ステートメントが1件生成されるはずです");

    match statement {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::Binary {
                left, op, right, ..
            } => {
                assert_eq!(*op, BinaryOp::Elvis, "Elvis演算子が設定されていません");
                match left.as_ref() {
                    Expression::NullSafeMemberAccess {
                        object, property, ..
                    } => {
                        assert_eq!(property, "length", "最終プロパティが不一致です");
                        match object.as_ref() {
                            Expression::NullSafeMemberAccess {
                                object: inner,
                                property: inner_property,
                                ..
                            } => {
                                assert_eq!(inner_property, "name", "中間プロパティが不一致です");
                                match inner.as_ref() {
                                    Expression::Identifier(identifier, _) => {
                                        assert_eq!(identifier, "user", "ルート識別子が不一致です")
                                    }
                                    other => panic!(
                                        "null安全メンバアクセスの基底が識別子ではありません: {:?}",
                                        other
                                    ),
                                }
                            }
                            other => panic!(
                                "null安全メンバアクセスの入れ子が想定どおりではありません: {:?}",
                                other
                            ),
                        }
                    }
                    other => panic!(
                        "Elvis演算子の左辺がnull安全メンバアクセスではありません: {:?}",
                        other
                    ),
                }
                match right.as_ref() {
                    Expression::Literal(Literal::Number(value), _) => {
                        assert_eq!(value, "0", "Elvis右辺の数値が不一致です")
                    }
                    other => panic!("Elvis演算子の右辺が数値リテラルではありません: {:?}", other),
                }
            }
            other => panic!("二項演算としてLoweringされていません: {:?}", other),
        },
        other => panic!("val宣言から初期化式を取得できませんでした: {:?}", other),
    }
}

#[test]
fn null安全インデックスアクセスが保持される() {
    let result = lower_source("val value = items?[index]");
    assert!(
        result.diagnostics.is_empty(),
        "診断が発生しないはずですが {:?} が検出されました",
        result.diagnostics
    );

    let statement = result
        .statements
        .first()
        .expect("ステートメントが1件生成されるはずです");

    match statement {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::NullSafeIndexAccess { object, index, .. } => {
                match object.as_ref() {
                    Expression::Identifier(name, _) => assert_eq!(
                        name, "items",
                        "null安全インデックスの対象識別子が不一致です"
                    ),
                    other => panic!(
                        "null安全インデックス対象が識別子ではありません: {:?}",
                        other
                    ),
                }
                match index.as_ref() {
                    Expression::Identifier(name, _) => assert_eq!(
                        name, "index",
                        "null安全インデックスの添字識別子が不一致です"
                    ),
                    other => panic!(
                        "null安全インデックスの添字が識別子ではありません: {:?}",
                        other
                    ),
                }
            }
            other => panic!(
                "null安全インデックスアクセスとしてLoweringされていません: {:?}",
                other
            ),
        },
        other => panic!("val宣言から初期化式を取得できませんでした: {:?}", other),
    }
}
