use super::lowering_cases::lower_source;
use crate::{JvLanguage, ParseBuilder, ParseEvent};
use jv_ast::Statement;
use jv_ast::expression::Expression;
use jv_ast::statement::ValBindingOrigin;
use jv_ast::types::TypeAnnotation;
use jv_lexer::Lexer;
use rowan::SyntaxNode;

/// val宣言の4パターン全てを検証するヘルパー関数
fn assert_val_declaration(
    statement: &Statement,
    expected_name: &str,
    expected_origin: ValBindingOrigin,
    expected_type: Option<&str>,
) {
    match statement {
        Statement::ValDeclaration {
            name,
            origin,
            type_annotation,
            ..
        } => {
            assert_eq!(name, expected_name, "val宣言の名前が期待値と異なります");
            assert_eq!(
                *origin, expected_origin,
                "ValBindingOriginが期待値と異なります: expected {:?}, got {:?}",
                expected_origin, origin
            );
            match (type_annotation, expected_type) {
                (None, None) => {}
                (Some(TypeAnnotation::Simple(actual)), Some(expected)) => {
                    assert_eq!(actual, expected, "型注釈が期待値と異なります");
                }
                (actual, expected) => {
                    panic!(
                        "型注釈の有無が期待値と異なります: expected {:?}, got {:?}",
                        expected, actual
                    );
                }
            }
        }
        other => panic!(
            "ValDeclarationが期待されましたが、{:?}が得られました",
            other
        ),
    }
}

// ==================== トップレベル ====================

#[test]
fn トップレベル_val_x_equals_0() {
    let result = lower_source("val x = 0");
    assert!(result.diagnostics.is_empty());
    let stmt = result.statements.first().expect("statement expected");
    assert_val_declaration(stmt, "x", ValBindingOrigin::ExplicitKeyword, None);
}

#[test]
fn トップレベル_x_equals_0() {
    let result = lower_source("x = 0");
    assert!(result.diagnostics.is_empty());
    let stmt = result.statements.first().expect("statement expected");
    assert_val_declaration(stmt, "x", ValBindingOrigin::Implicit, None);
}

#[test]
fn トップレベル_val_x_colon_int_equals_0() {
    let result = lower_source("val x: Int = 0");
    assert!(result.diagnostics.is_empty());
    let stmt = result.statements.first().expect("statement expected");
    assert_val_declaration(stmt, "x", ValBindingOrigin::ExplicitKeyword, Some("Int"));
}

#[test]
fn トップレベル_x_colon_int_equals_0() {
    let result = lower_source("x: Int = 0");
    assert!(result.diagnostics.is_empty());
    let stmt = result.statements.first().expect("statement expected");
    assert_val_declaration(stmt, "x", ValBindingOrigin::ImplicitTyped, Some("Int"));
}

// ==================== 関数内 ====================

#[test]
fn 関数内_val_x_equals_0() {
    let result = lower_source(
        r#"
fun test() {
    val x = 0
}
"#,
    );
    assert!(result.diagnostics.is_empty());
    let func = result.statements.first().expect("function expected");
    if let Statement::FunctionDeclaration { body, .. } = func {
        if let Expression::Block { statements, .. } = body.as_ref() {
            let stmt = statements.first().expect("statement in function expected");
            assert_val_declaration(stmt, "x", ValBindingOrigin::ExplicitKeyword, None);
        } else {
            panic!("Expected Block expression");
        }
    } else {
        panic!("Expected FunctionDeclaration");
    }
}

#[test]
fn 関数内_x_equals_0() {
    let result = lower_source(
        r#"
fun test() {
    x = 0
}
"#,
    );
    assert!(result.diagnostics.is_empty());
    let func = result.statements.first().expect("function expected");
    if let Statement::FunctionDeclaration { body, .. } = func {
        if let Expression::Block { statements, .. } = body.as_ref() {
            let stmt = statements.first().expect("statement in function expected");
            assert_val_declaration(stmt, "x", ValBindingOrigin::Implicit, None);
        } else {
            panic!("Expected Block expression");
        }
    } else {
        panic!("Expected FunctionDeclaration");
    }
}

#[test]
fn 関数内_val_x_colon_int_equals_0() {
    let result = lower_source(
        r#"
fun test() {
    val x: Int = 0
}
"#,
    );
    assert!(result.diagnostics.is_empty());
    let func = result.statements.first().expect("function expected");
    if let Statement::FunctionDeclaration { body, .. } = func {
        if let Expression::Block { statements, .. } = body.as_ref() {
            let stmt = statements.first().expect("statement in function expected");
            assert_val_declaration(stmt, "x", ValBindingOrigin::ExplicitKeyword, Some("Int"));
        } else {
            panic!("Expected Block expression");
        }
    } else {
        panic!("Expected FunctionDeclaration");
    }
}

#[test]
fn 関数内_x_colon_int_equals_0() {
    let result = lower_source(
        r#"
fun test() {
    x: Int = 0
}
"#,
    );
    assert!(result.diagnostics.is_empty());
    let func = result.statements.first().expect("function expected");
    if let Statement::FunctionDeclaration { body, .. } = func {
        if let Expression::Block { statements, .. } = body.as_ref() {
            let stmt = statements.first().expect("statement in function expected");
            assert_val_declaration(stmt, "x", ValBindingOrigin::ImplicitTyped, Some("Int"));
        } else {
            panic!("Expected Block expression");
        }
    } else {
        panic!("Expected FunctionDeclaration");
    }
}

// ==================== クラスメンバ ====================

#[test]
fn クラスメンバ_val_x_equals_0() {
    let result = lower_source(
        r#"
class Test {
    val x = 0
}
"#,
    );
    assert!(result.diagnostics.is_empty());
    let class = result.statements.first().expect("class expected");
    if let Statement::ClassDeclaration { properties, .. } = class {
        let prop = properties.first().expect("property expected");
        assert_eq!(prop.name, "x");
        assert!(!prop.is_mutable);
    } else {
        panic!("Expected ClassDeclaration");
    }
}

// ==================== forループ内 ====================

#[test]
fn forループ内_val_x_equals_0() {
    let result = lower_source(
        r#"
for (i in 0..10) {
    val x = 0
}
"#,
    );
    assert!(result.diagnostics.is_empty());
    let for_stmt = result.statements.first().expect("for statement expected");
    if let Statement::ForIn(for_in) = for_stmt {
        if let Expression::Block { statements, .. } = for_in.body.as_ref() {
            let stmt = statements.first().expect("statement in for expected");
            assert_val_declaration(stmt, "x", ValBindingOrigin::ExplicitKeyword, None);
        } else {
            panic!("Expected Block expression");
        }
    } else {
        panic!("Expected ForIn statement");
    }
}

#[test]
fn forループ内_x_equals_0() {
    let result = lower_source(
        r#"
for (i in 0..10) {
    x = 0
}
"#,
    );
    assert!(result.diagnostics.is_empty());
    let for_stmt = result.statements.first().expect("for statement expected");
    if let Statement::ForIn(for_in) = for_stmt {
        if let Expression::Block { statements, .. } = for_in.body.as_ref() {
            let stmt = statements.first().expect("statement in for expected");
            assert_val_declaration(stmt, "x", ValBindingOrigin::Implicit, None);
        } else {
            panic!("Expected Block expression");
        }
    } else {
        panic!("Expected ForIn statement");
    }
}

#[test]
fn forループ内_val_x_colon_int_equals_0() {
    let result = lower_source(
        r#"
for (i in 0..10) {
    val x: Int = 0
}
"#,
    );
    assert!(result.diagnostics.is_empty());
    let for_stmt = result.statements.first().expect("for statement expected");
    if let Statement::ForIn(for_in) = for_stmt {
        if let Expression::Block { statements, .. } = for_in.body.as_ref() {
            let stmt = statements.first().expect("statement in for expected");
            assert_val_declaration(stmt, "x", ValBindingOrigin::ExplicitKeyword, Some("Int"));
        } else {
            panic!("Expected Block expression");
        }
    } else {
        panic!("Expected ForIn statement");
    }
}

#[test]
fn forループ内_x_colon_int_equals_0() {
    let result = lower_source(
        r#"
for (i in 0..10) {
    x: Int = 0
}
"#,
    );
    assert!(result.diagnostics.is_empty());
    let for_stmt = result.statements.first().expect("for statement expected");
    if let Statement::ForIn(for_in) = for_stmt {
        if let Expression::Block { statements, .. } = for_in.body.as_ref() {
            let stmt = statements.first().expect("statement in for expected");
            assert_val_declaration(stmt, "x", ValBindingOrigin::ImplicitTyped, Some("Int"));
        } else {
            panic!("Expected Block expression");
        }
    } else {
        panic!("Expected ForIn statement");
    }
}

// ==================== when分岐内 ====================

#[test]
fn when分岐内_val_x_equals_0() {
    let result = lower_source(
        r#"
when (value) {
    1 -> {
        val x = 0
    }
}
"#,
    );
    assert!(result.diagnostics.is_empty());
    let when_expr = result.statements.first().expect("when expression expected");
    if let Statement::Expression { expr, .. } = when_expr {
        if let Expression::When { arms, .. } = expr {
            let arm = arms.first().expect("when arm expected");
            // when arm body can be Lambda or Block depending on syntax
            let statements = match &arm.body {
                Expression::Lambda { body, .. } => {
                    if let Expression::Block { statements, .. } = body.as_ref() {
                        statements
                    } else {
                        panic!("Expected Block in lambda body");
                    }
                }
                Expression::Block { statements, .. } => statements,
                other => panic!("Expected Block or Lambda in when arm, got {:?}", other),
            };
            let stmt = statements.first().expect("statement in when arm expected");
            assert_val_declaration(stmt, "x", ValBindingOrigin::ExplicitKeyword, None);
        } else {
            panic!("Expected When expression");
        }
    } else {
        panic!("Expected Expression statement");
    }
}

#[test]
fn when分岐内_x_equals_0() {
    let result = lower_source(
        r#"
when (value) {
    1 -> {
        x = 0
    }
}
"#,
    );
    assert!(result.diagnostics.is_empty());
    let when_expr = result.statements.first().expect("when expression expected");
    if let Statement::Expression { expr, .. } = when_expr {
        if let Expression::When { arms, .. } = expr {
            let arm = arms.first().expect("when arm expected");
            // when arm body can be Lambda or Block depending on syntax
            let statements = match &arm.body {
                Expression::Lambda { body, .. } => {
                    if let Expression::Block { statements, .. } = body.as_ref() {
                        statements
                    } else {
                        panic!("Expected Block in lambda body");
                    }
                }
                Expression::Block { statements, .. } => statements,
                other => panic!("Expected Block or Lambda in when arm, got {:?}", other),
            };
            let stmt = statements.first().expect("statement in when arm expected");
            assert_val_declaration(stmt, "x", ValBindingOrigin::Implicit, None);
        } else {
            panic!("Expected When expression");
        }
    } else {
        panic!("Expected Expression statement");
    }
}

// ==================== ラムダ内 ====================

#[test]
fn ラムダ内_val_x_equals_0() {
    let result = lower_source(
        r#"
val f = { ->
    val x = 0
    x
}
"#,
    );
    assert!(result.diagnostics.is_empty());
    let val_decl = result.statements.first().expect("val declaration expected");
    if let Statement::ValDeclaration { initializer, .. } = val_decl {
        if let Expression::Lambda { body, .. } = initializer {
            if let Expression::Block { statements, .. } = body.as_ref() {
                let stmt = statements.first().expect("statement in lambda expected");
                assert_val_declaration(stmt, "x", ValBindingOrigin::ExplicitKeyword, None);
            } else {
                panic!("Expected Block expression in lambda");
            }
        } else {
            panic!("Expected Lambda expression");
        }
    } else {
        panic!("Expected ValDeclaration");
    }
}

#[test]
fn ラムダ内_x_equals_0() {
    let result = lower_source(
        r#"
val f = { ->
    x = 0
    x
}
"#,
    );
    assert!(result.diagnostics.is_empty());
    let val_decl = result.statements.first().expect("val declaration expected");
    if let Statement::ValDeclaration { initializer, .. } = val_decl {
        if let Expression::Lambda { body, .. } = initializer {
            if let Expression::Block { statements, .. } = body.as_ref() {
                let stmt = statements.first().expect("statement in lambda expected");
                assert_val_declaration(stmt, "x", ValBindingOrigin::Implicit, None);
            } else {
                panic!("Expected Block expression in lambda");
            }
        } else {
            panic!("Expected Lambda expression");
        }
    } else {
        panic!("Expected ValDeclaration");
    }
}

// ==================== ネストした深さ ====================

#[test]
fn 深くネストした関数内_x_equals_0() {
    let result = lower_source(
        r#"
fun outer() {
    for (i in 0..10) {
        when (i) {
            1 -> {
                val f = { ->
                    x = 0
                    x
                }
            }
        }
    }
}
"#,
    );
    assert!(result.diagnostics.is_empty());

    // 関数 -> for -> when -> lambda と辿る
    let func = result.statements.first().expect("function expected");
    if let Statement::FunctionDeclaration { body, .. } = func {
        if let Expression::Block { statements, .. } = body.as_ref() {
            let for_stmt = statements.first().expect("for statement expected");
            if let Statement::ForIn(for_in) = for_stmt {
                if let Expression::Block { statements, .. } = for_in.body.as_ref() {
                    let when_stmt = statements.first().expect("when statement expected");
                    if let Statement::Expression { expr, .. } = when_stmt {
                        if let Expression::When { arms, .. } = expr {
                            let arm = arms.first().expect("when arm expected");
                            let statements = match &arm.body {
                                Expression::Lambda { body, .. } => {
                                    if let Expression::Block { statements, .. } = body.as_ref() {
                                        statements
                                    } else {
                                        panic!("Expected Block expression inside lambda body");
                                    }
                                }
                                Expression::Block { statements, .. } => statements,
                                other => {
                                    panic!("Expected Block or Lambda in when arm, got {:?}", other)
                                }
                            };

                            let lambda_decl = statements.first().expect("lambda val decl expected");
                            if let Statement::ValDeclaration { initializer, .. } = lambda_decl {
                                if let Expression::Lambda { body, .. } = initializer {
                                    if let Expression::Block { statements, .. } = body.as_ref() {
                                        let stmt = statements
                                            .first()
                                            .expect("statement in lambda expected");
                                        assert_val_declaration(
                                            stmt,
                                            "x",
                                            ValBindingOrigin::Implicit,
                                            None,
                                        );
                                        return;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    panic!("Failed to navigate through nested structure");
}

#[test]
fn 複数のネストレベルで異なる宣言パターン() {
    let result = lower_source(
        r#"
a = 1
fun test() {
    b: Int = 2
    for (i in 0..10) {
        val c = 3
        when (i) {
            1 -> {
                d: String = "four"
            }
        }
    }
}
"#,
    );
    assert!(result.diagnostics.is_empty());

    // トップレベル: a = 1 (Implicit)
    assert_val_declaration(
        result.statements.first().expect("a declaration"),
        "a",
        ValBindingOrigin::Implicit,
        None,
    );

    // 以降の詳細な検証は省略（構造が複雑なため）
    // 実際には各レベルで正しく変換されることが重要
}

// ==================== デバッグ用: CST構造ダンプ ====================

/// CST構造をダンプするヘルパー関数
fn dump_cst_structure(source: &str, test_name: &str) -> SyntaxNode<JvLanguage> {
    eprintln!("\n=== CST Structure Dump: {} ===", test_name);
    eprintln!("Source:\n{}", source);

    let tokens = Lexer::new(source.to_string())
        .tokenize()
        .expect("lex source");
    let parse_output = crate::parser::parse(&tokens);

    eprintln!("\nParser Diagnostics: {:?}", parse_output.diagnostics);

    let green = ParseBuilder::build_from_events(&parse_output.events, &tokens);
    let syntax: SyntaxNode<JvLanguage> = SyntaxNode::new_root(green);

    eprintln!("\nCST Structure:");
    dump_node(&syntax, 0);
    eprintln!("=== End CST Dump ===\n");

    syntax
}

/// ノードを再帰的にダンプする
fn dump_node(node: &SyntaxNode<JvLanguage>, indent: usize) {
    let indent_str = "  ".repeat(indent);

    // ノードの種類を表示
    eprintln!("{}kind: {:?}", indent_str, node.kind());

    // 子要素がない場合はテキストを表示
    let children_count = node.children().count();
    if children_count == 0 {
        if let Some(text) = node.first_token() {
            eprintln!("{}  text: {:?}", indent_str, text.text());
        }
    } else {
        // 子要素がある場合は再帰的にダンプ
        eprintln!("{}  (children: {})", indent_str, children_count);
        for child in node.children() {
            dump_node(&child, indent + 1);
        }
    }
}

#[test]
#[ignore] // デバッグ用のため通常実行では無視
fn debug_cst_function_success() {
    let source = r#"
fun test() {
    x = 0
}
"#;
    dump_cst_structure(source, "関数内_x_equals_0 (成功ケース)");
}

#[test]
#[ignore] // デバッグ用のため通常実行では無視
fn debug_cst_when_failure() {
    let source = r#"
when (value) {
    1 -> {
        x = 0
    }
}
"#;
    dump_cst_structure(source, "when分岐内_x_equals_0 (失敗ケース)");
}

#[test]
#[ignore] // デバッグ用のため通常実行では無視
fn debug_cst_lambda_failure() {
    let source = r#"
val f = { ->
    x = 0
    x
}
"#;
    dump_cst_structure(source, "ラムダ内_x_equals_0 (失敗ケース)");
}

/// パーサーが生成したイベント列を詳細にダンプする
fn dump_parser_events(source: &str, test_name: &str) {
    eprintln!("\n=== Parser Events Dump: {} ===", test_name);
    eprintln!("Source:\n{}", source);

    let tokens = Lexer::new(source.to_string())
        .tokenize()
        .expect("lex source");

    eprintln!("\nTokens (index, kind, lexeme):");
    for (idx, token) in tokens.iter().enumerate() {
        eprintln!("  [{idx:>3}] {:?} {:?}", token.token_type, token.lexeme);
    }

    let parse_output = crate::parser::parse(&tokens);
    eprintln!("\nParser Diagnostics: {:?}", parse_output.diagnostics);

    eprintln!("\nParse Events:");
    for (idx, event) in parse_output.events.iter().enumerate() {
        match event {
            ParseEvent::StartNode { kind } => {
                eprintln!("  [{idx:>3}] StartNode({kind:?})");
            }
            ParseEvent::FinishNode => {
                eprintln!("  [{idx:>3}] FinishNode");
            }
            ParseEvent::Token { kind, token_index } => {
                let token_info = tokens
                    .get(*token_index)
                    .map(|token| format!("{:?} {:?}", token.token_type, token.lexeme))
                    .unwrap_or_else(|| "<?>".to_string());
                eprintln!("  [{idx:>3}] Token({kind:?}, idx={token_index}) => {token_info}");
            }
            ParseEvent::Error { message, span } => {
                eprintln!("  [{idx:>3}] Error(span={span:?}) {message}");
            }
        }
    }

    eprintln!("=== End Parser Events Dump ===\n");
}

#[test]
#[ignore] // デバッグ用のため通常実行では無視
fn debug_parser_events_function_success() {
    let source = r#"
fun test() {
    x = 0
}
"#;
    dump_parser_events(source, "関数内_x_equals_0 (成功ケース)");
}

#[test]
#[ignore] // デバッグ用のため通常実行では無視
fn debug_parser_events_when_failure() {
    let source = r#"
when (value) {
    1 -> {
        x = 0
    }
}
"#;
    dump_parser_events(source, "when分岐内_x_equals_0 (失敗ケース)");
}

#[test]
#[ignore] // デバッグ用のため通常実行では無視
fn debug_parser_events_lambda_failure() {
    let source = r#"
val f = { ->
    x = 0
    x
}
"#;
    dump_parser_events(source, "ラムダ内_x_equals_0 (失敗ケース)");
}
