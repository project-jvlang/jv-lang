//! タプル型注釈・推論ロジックの統合テスト。
#![allow(non_snake_case)]

use jv_ast::TypeAnnotation;
use jv_checker::{PrimitiveType, TypeChecker, TypeKind, inference::type_parser};
use jv_ir::{TupleRecordStrategy, TupleUsageKind};
use jv_parser_frontend::ParserPipeline;
use jv_parser_rowan::frontend::RowanPipeline;

fn 構文解析(ソース: &str) -> jv_ast::Program {
    RowanPipeline::default()
        .parse(ソース)
        .expect("構文解析が成功すること")
        .into_program()
}

fn 型チェック済み(ソース: &str) -> TypeChecker {
    let プログラム = 構文解析(ソース);
    let mut チェッカー = TypeChecker::new();
    チェッカー
        .check_program(&プログラム)
        .expect("型チェックが成功すること");
    チェッカー
}

#[test]
fn タプル型注釈をTypeKindへ展開できる() {
    let 注釈 = TypeAnnotation::Simple("(Int String)".to_string());
    let 型 = type_parser::parse_type_annotation(&注釈).expect("タプル型注釈が正しく解析されること");

    let TypeKind::Tuple(要素) = 型 else {
        panic!("タプル型を期待したが {:?} が返却された", 型);
    };

    assert_eq!(要素.len(), 2, "2要素タプルとして解釈されること");
    assert_eq!(
        要素[0],
        TypeKind::primitive(PrimitiveType::Int),
        "先頭要素がIntプリミティブとして扱われること"
    );
    assert_eq!(
        要素[1],
        TypeKind::reference("java.lang.String"),
        "2要素目がString参照型として扱われること"
    );
}

#[test]
fn 関数戻り値のタプルは専用レコードを計画する() {
    let ソース = r#"
fun produce(): (Int String) {
    (1 "ok")
}
"#;
    let チェッカー = 型チェック済み(ソース);

    let プラン一覧 = チェッカー.tuple_record_plans();
    assert_eq!(プラン一覧.len(), 1, "タプル計画が1件生成されること");
    let プラン = &プラン一覧[0];
    assert_eq!(
        プラン.strategy,
        TupleRecordStrategy::Specific,
        "戻り値専用のレコード戦略が選択されること"
    );
    assert_eq!(
        プラン.specific_name.as_deref(),
        Some("Produce_Result"),
        "関数名からPascalCaseのレコード名が生成されること"
    );
    assert_eq!(
        プラン.generic_name, "Tuple2_Int_String",
        "型ヒントから汎用名が組み立てられること"
    );
    assert_eq!(プラン.arity, 2);
    assert_eq!(プラン.type_hints, vec!["Int".into(), "String".into()]);
    assert_eq!(プラン.fields.len(), 2, "要素メタ情報が全要素分揃うこと");
    assert_eq!(プラン.usage_sites.len(), 1, "使用箇所が1件記録されること");
    assert_eq!(
        プラン.usage_sites[0].kind,
        TupleUsageKind::FunctionReturn,
        "使用箇所が関数戻り値として記録されること"
    );

    let スナップショット = チェッカー
        .inference_snapshot()
        .expect("推論スナップショットが取得できること");
    let スキーム = スナップショット
        .function_scheme("produce")
        .expect("関数スキームが登録されること");
    let TypeKind::Function(引数型, 戻り型) = &スキーム.ty else {
        panic!("関数スキームを期待したが {:?} が得られた", スキーム.ty);
    };
    assert!(引数型.is_empty(), "パラメータなし関数として推論されること");
    let TypeKind::Tuple(戻り要素) = 戻り型.as_ref() else {
        panic!("戻り値がタプルであることを期待したが {:?} だった", 戻り型);
    };
    assert_eq!(戻り要素.len(), 2);
    assert_eq!(
        戻り要素[0],
        TypeKind::primitive(PrimitiveType::Int),
        "戻り値タプルの先頭要素がIntになること"
    );
    assert_eq!(
        戻り要素[1],
        TypeKind::reference("java.lang.String"),
        "戻り値タプルの2要素目がStringになること"
    );
}

#[test]
fn 複数コンテキストで共有されるタプルは汎用レコードを選択する() {
    let ソース = r#"
fun duplicate(value: Int): (Int Int) {
    val pair: (Int Int) = (value value)
    pair
}
"#;
    let チェッカー = 型チェック済み(ソース);

    let プラン一覧 = チェッカー.tuple_record_plans();
    assert_eq!(プラン一覧.len(), 1, "タプル計画が1件生成されること");
    let プラン = &プラン一覧[0];
    assert_eq!(
        プラン.strategy,
        TupleRecordStrategy::Generic,
        "複数用途では汎用レコード戦略が選ばれること"
    );
    assert!(
        プラン.specific_name.is_none(),
        "専用名は割り当てられないこと"
    );
    assert_eq!(
        プラン.generic_name, "Tuple2_Int_Int",
        "汎用名が型ヒントから構築されること"
    );
    assert_eq!(
        プラン.usage_sites.len(),
        1,
        "タプルリテラルは1箇所の使用として記録されること"
    );
    assert_eq!(
        プラン.usage_sites[0].kind,
        TupleUsageKind::BindingInitializer,
        "分割代入以外の初期化はBindingInitializerとして残ること"
    );
    assert_eq!(プラン.type_hints, vec!["Int".into(), "Int".into()]);
}
