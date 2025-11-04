use jv_ast::{
    CommentVisibility, DocumentationKind, Expression, JsonLiteral, JsonValue, Modifiers, Program,
    Statement,
};
use jv_parser::Parser;

fn parse_program(source: &str) -> Program {
    let output = Parser::parse(source).expect("ソースコードの構文解析に失敗");
    let diagnostics = output.diagnostics();
    assert!(
        diagnostics.final_diagnostics().is_empty(),
        "診断が発生: {:?}",
        diagnostics.final_diagnostics()
    );
    output.into_program()
}

fn find_val_modifiers(program: &Program, name: &str) -> &Modifiers {
    program
        .statements
        .iter()
        .find_map(|statement| match statement {
            Statement::ValDeclaration {
                name: ident,
                modifiers,
                ..
            } if ident == name => Some(modifiers),
            _ => None,
        })
        .expect("指定した val 宣言が存在しない")
}

fn find_json_literal(program: &Program, name: &str) -> &JsonLiteral {
    program
        .statements
        .iter()
        .find_map(|statement| match statement {
            Statement::ValDeclaration {
                name: ident,
                initializer,
                ..
            } if ident == name => match initializer {
                Expression::JsonLiteral(literal) => Some(literal),
                other => panic!("JSON リテラルを期待したが {:?} だった", other),
            },
            _ => None,
        })
        .expect("JSON リテラルを初期化式に持つ val が存在しない")
}

#[test]
fn val_declaration_attaches_line_doc_and_jv_block_comments() {
    let program = parse_program(
        r#"/// 宣言の説明
//* jv専用メモ
詳細は LSP でのみ表示
*//
val annotated = 1
"#,
    );
    let modifiers = find_val_modifiers(&program, "annotated");

    let documentation = modifiers
        .documentation
        .as_ref()
        .expect("ラインドキュメントが存在するはず");
    assert_eq!(documentation.kind, DocumentationKind::Line);
    assert_eq!(documentation.text.trim(), "宣言の説明");

    assert_eq!(modifiers.jv_comments.len(), 1);
    let comment = &modifiers.jv_comments[0];
    assert!(
        comment.text.contains("jv専用メモ"),
        "jv 専用コメント本文が保持されるべき: {}",
        comment.text
    );
    assert!(
        matches!(comment.visibility, CommentVisibility::JvOnly),
        "jv専用コメントだけが modifiers に残るべき"
    );
}

#[test]
fn function_declaration_normalizes_javadoc_whitespace() {
    let program = parse_program(
        r#"/**
 * API 説明
 * - 詳細
 */
fun describe(): Unit {
    println("ok")
}
"#,
    );

    let modifiers = program
        .statements
        .iter()
        .find_map(|statement| match statement {
            Statement::FunctionDeclaration { name, modifiers, .. } if name == "describe" => {
                Some(modifiers)
            }
            _ => None,
        })
        .expect("関数宣言が存在するはず");

    let documentation = modifiers
        .documentation
        .as_ref()
        .expect("JavaDoc が正規化されるべき");
    assert_eq!(documentation.kind, DocumentationKind::Block);
    assert!(
        documentation.text.contains("API 説明"),
        "本文が正規化結果に含まれるべき: {}",
        documentation.text
    );
    assert!(
        documentation.text.contains("- 詳細"),
        "行頭アスタリスクが除去された状態で保持されるべき: {}",
        documentation.text
    );
    assert!(
        !documentation.text.contains("*/"),
        "閉じデリミタは含まれないべき: {}",
        documentation.text
    );
}

#[test]
fn json_literal_preserves_leading_json_metadata_comments() {
    let program = parse_program(
        r#"val payload = {
  //# schema: beta
  "value": 1
}
"#,
    );
    let literal = find_json_literal(&program, "payload");

    assert_eq!(literal.leading_comments.len(), 1);
    let comment = &literal.leading_comments[0];
    assert_eq!(comment.text, "schema: beta");
    assert!(
        matches!(literal.value, JsonValue::Object { .. }),
        "JSON 値がオブジェクトとして構築されるべき"
    );
}
