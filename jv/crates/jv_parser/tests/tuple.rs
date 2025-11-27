//! タプルリテラルと分割代入検証用テスト群。
#![allow(non_snake_case)]

use jv_ast::{Expression, Statement, binding_pattern::BindingPatternKind};
use jv_parser::{DiagnosticSeverity, FrontendDiagnostics, Parser};

fn 解析結果(ソース: &str) -> (jv_ast::Program, FrontendDiagnostics) {
    let 出力 = Parser::parse(ソース).expect("パーサが致命的エラーを返さないこと");
    let (プログラム, _, 診断) = 出力.into_parts();
    (プログラム, 診断)
}

fn 最終診断一覧<'a>(
    診断: &'a FrontendDiagnostics,
) -> impl Iterator<Item = &'a jv_parser::Diagnostic> {
    診断.final_diagnostics().iter()
}

#[test]
fn 空白区切りの括弧はタプルとして解釈される() {
    let (プログラム, 診断) = 解析結果("val pair = (first second)");

    assert!(
        最終診断一覧(&診断).next().is_none(),
        "正常ケースでは診断を発行しない: {:?}",
        診断.final_diagnostics()
    );

    let 文 = プログラム
        .statements
        .first()
        .expect("トップレベルに1つの宣言が存在すること");

    let 初期化式 = match 文 {
        Statement::ValDeclaration { initializer, .. } => initializer,
        その他 => panic!("val宣言を想定しているが {:?} が得られた", その他),
    };

    let (要素, メタ) = match 初期化式 {
        Expression::Tuple {
            elements,
            fields,
            context,
            ..
        } => {
            assert!(
                !context.in_destructuring_pattern,
                "通常の初期化では分割代入フラグが立たないこと"
            );
            (elements, fields)
        }
        予期しない式 => panic!("タプル式を期待したが {:?} が得られた", 予期しない式),
    };

    assert_eq!(要素.len(), 2, "2要素のタプルとして解析されること");
    assert_eq!(メタ.len(), 2, "各要素ごとにメタ情報が付与されること");

    let mut 名前一覧 = 要素.iter().map(|式| match 式 {
        Expression::Identifier(名前, _) => 名前.as_str(),
        異常 => panic!("識別子要素を期待したが {:?} が得られた", 異常),
    });
    assert_eq!(名前一覧.next(), Some("first"));
    assert_eq!(名前一覧.next(), Some("second"));

    let インデックス: Vec<_> = メタ.iter().map(|meta| meta.fallback_index).collect();
    assert_eq!(
        インデックス,
        vec![1, 2],
        "後退名称は連番で割り当てられること"
    );
}

#[test]
fn コメントラベルがタプルメタデータに反映される() {
    let ソース = r#"
val labeled = (
    // userId
    /* account */
    user
    // balance
    /* amount */
    amount
)
"#;
    let (プログラム, 診断) = 解析結果(ソース);

    assert!(
        最終診断一覧(&診断).next().is_none(),
        "コメントラベル付きタプルでは診断を発行しない: {:?}",
        診断.final_diagnostics()
    );

    let 文 = プログラム
        .statements
        .first()
        .expect("トップレベルに1つの宣言が存在すること");

    let 初期化式 = match 文 {
        Statement::ValDeclaration { initializer, .. } => initializer,
        その他 => panic!("val宣言を想定しているが {:?} が得られた", その他),
    };

    let フィールド = match 初期化式 {
        Expression::Tuple { fields, .. } => fields,
        予期しない式 => panic!("タプル式を期待したが {:?} が得られた", 予期しない式),
    };

    assert_eq!(フィールド.len(), 2, "2要素のタプルとして解析されること");

    let 最初 = &フィールド[0];
    assert_eq!(
        最初.primary_label.as_deref(),
        Some("userId"),
        "最初のラベルがprimaryとして設定されること"
    );
    let 最初の副次ラベル: Vec<_> = 最初
        .secondary_labels
        .iter()
        .map(|ラベル| ラベル.name.as_str())
        .collect();
    assert_eq!(
        最初の副次ラベル,
        vec!["account"],
        "最初の要素にはコメント由来の副次ラベルが付与されること"
    );
    assert_eq!(
        最初.identifier_hint.as_deref(),
        Some("user"),
        "識別子要素はidentifier_hintに記録されること"
    );

    let 二番目 = &フィールド[1];
    assert_eq!(
        二番目.primary_label.as_deref(),
        Some("balance"),
        "二番目のラベルがprimaryとして設定されること"
    );
    let 二番目の副次ラベル: Vec<_> = 二番目
        .secondary_labels
        .iter()
        .map(|ラベル| ラベル.name.as_str())
        .collect();
    assert_eq!(
        二番目の副次ラベル,
        vec!["amount"],
        "二番目の要素にも副次ラベルが引き継がれること"
    );
    assert_eq!(
        二番目.identifier_hint.as_deref(),
        Some("amount"),
        "識別子要素はidentifier_hintに記録されること"
    );
}

#[test]
fn 空タプルは診断を返す() {
    let (_, 診断) = 解析結果("val invalid = ()");
    let エラー = 最終診断一覧(&診断)
        .find(|diag| {
            diag.message()
                .contains("空のタプルや句読点のみのタプルリテラルはサポートされません")
        })
        .expect("空タプルに対するエラーメッセージが報告されること");

    assert_eq!(
        エラー.severity(),
        DiagnosticSeverity::Error,
        "空タプルはエラー扱いになること"
    );
}

#[test]
fn 分割代入ではタプルコンテキストが設定される() {
    let (プログラム, 診断) = 解析結果("val (first second) = (10 20)");

    assert!(
        最終診断一覧(&診断).next().is_none(),
        "要素数が一致する分割代入では診断を発行しない: {:?}",
        診断.final_diagnostics()
    );

    let 文 = プログラム
        .statements
        .first()
        .expect("宣言が1件存在すること");
    let 初期化式 = match 文 {
        Statement::ValDeclaration { initializer, .. } => initializer,
        予期しない => panic!("val宣言を期待したが {:?} を受け取った", 予期しない),
    };

    let 文脈 = match 初期化式 {
        Expression::Tuple { context, .. } => context,
        予期しない式 => panic!("タプル式を期待したが {:?} を取得した", 予期しない式),
    };

    assert!(
        文脈.in_destructuring_pattern,
        "分割代入ではin_destructuring_patternが設定されること"
    );
    assert!(
        !文脈.is_function_return,
        "分割代入ではis_function_returnは偽のままであること"
    );
}

fn パターン名一覧(パターン: &BindingPatternKind) -> Vec<String> {
    match パターン {
        BindingPatternKind::Tuple { elements, .. } | BindingPatternKind::List { elements, .. } => {
            elements
                .iter()
                .map(|要素| match 要素 {
                    BindingPatternKind::Identifier { name, .. } => name.clone(),
                    BindingPatternKind::Wildcard { .. } => "_".to_string(),
                    BindingPatternKind::Tuple { .. } => "(...)".to_string(),
                    BindingPatternKind::List { .. } => "[...]".to_string(),
                })
                .collect()
        }
        BindingPatternKind::Identifier { name, .. } => vec![name.clone()],
        BindingPatternKind::Wildcard { .. } => vec!["_".into()],
    }
}

#[test]
fn 分割代入で不足要素を報告する() {
    let (プログラム, 診断) = 解析結果("val (a b c) = (1 2)");

    let 文 = プログラム
        .statements
        .first()
        .expect("宣言が1件存在すること");
    let パターン = match 文 {
        Statement::ValDeclaration { binding, .. } => binding
            .as_ref()
            .expect("分割代入ではバインディングパターンが存在すること"),
        予期しない => panic!(
            "分割代入を含むval宣言を期待したが {:?} を受け取った",
            予期しない
        ),
    };
    assert_eq!(
        パターン名一覧(パターン),
        vec!["a", "b", "c"],
        "パターンが正しく解析されていること"
    );

    let 診断本体 = 最終診断一覧(&診断)
        .find(|diag| diag.message().contains("分割代入の要素が不足しています"))
        .expect("要素不足の診断が出力されること");
    assert_eq!(
        診断本体.severity(),
        DiagnosticSeverity::Error,
        "不足診断はエラー扱いであること"
    );
}

#[test]
fn 分割代入で過剰要素を報告する() {
    let (プログラム, 診断) = 解析結果("val (a b) = (1 2 3)");

    let 文 = プログラム
        .statements
        .first()
        .expect("宣言が1件存在すること");
    let パターン = match 文 {
        Statement::ValDeclaration { binding, .. } => binding
            .as_ref()
            .expect("分割代入ではバインディングパターンが存在すること"),
        予期しない => panic!(
            "分割代入を含むval宣言を期待したが {:?} を受け取った",
            予期しない
        ),
    };
    assert_eq!(
        パターン名一覧(パターン),
        vec!["a", "b"],
        "パターンが正しく解析されていること"
    );

    let 診断本体 = 最終診断一覧(&診断)
        .find(|diag| diag.message().contains("分割代入の要素が多すぎます"))
        .expect("要素過多の診断が出力されること");
    assert_eq!(
        診断本体.severity(),
        DiagnosticSeverity::Error,
        "過多診断はエラー扱いであること"
    );
}
