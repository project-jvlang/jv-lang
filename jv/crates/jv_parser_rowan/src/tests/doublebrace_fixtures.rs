use jv_lexer::Lexer;
use rowan::SyntaxNode;

use crate::parser::parse;
use crate::syntax::SyntaxKind;
use crate::{JvLanguage, ParseBuilder};

fn lex(input: &str) -> Vec<jv_lexer::Token> {
    let mut lexer = Lexer::new(input.to_string());
    lexer.tokenize().expect("字句解析に成功する")
}

fn parse_tree(source: &str) -> (Vec<jv_lexer::Token>, SyntaxNode<JvLanguage>) {
    let tokens = lex(source);
    let output = parse(&tokens);

    assert!(
        output.diagnostics.is_empty(),
        "パーサー診断が発生しないこと: {:?}",
        output.diagnostics
    );
    assert!(
        !output.recovered,
        "リカバリせずに二重ブレースを解析できること"
    );

    let syntax = SyntaxNode::new_root(ParseBuilder::build_from_events(&output.events, &tokens));
    (tokens, syntax)
}

#[test]
fn doublebrace_builder_statements_are_retained() {
    // ビルダー形式の呼び出しとフィールド更新が StatementList に保持されることを確認する。
    let source = r#"
        val config = Builder() {{
            name("main")
            timeout = 5000
            finalize()
        }}
    "#;
    let (_tokens, tree) = parse_tree(source);

    let statement_list = tree
        .descendants()
        .find(|node| node.kind() == SyntaxKind::DoublebraceBlock)
        .and_then(|block| {
            block
                .children()
                .find(|child| child.kind() == SyntaxKind::StatementList)
        })
        .expect("Doublebrace ブロック内に StatementList が存在するはず");

    let mut saw_assignment = false;
    let mut saw_expression = false;
    for node in statement_list.children() {
        match node.kind() {
            SyntaxKind::AssignmentStatement => saw_assignment = true,
            SyntaxKind::Expression => saw_expression = true,
            _ => {}
        }
    }

    assert!(
        saw_expression,
        "メソッド呼び出しが Expression ノードとして保持されること"
    );
    assert!(
        saw_assignment,
        "フィールド更新が AssignmentStatement として保持されること"
    );
}

#[test]
fn doublebrace_records_return_statement_for_diagnostics() {
    // 制御フロー禁止診断のために return 文が構文木へ残ることを検証する。
    let source = r#"
        val list = ArrayList() {{
            add(1)
            return
        }}
    "#;
    let (_, tree) = parse_tree(source);

    let doublebrace = tree
        .descendants()
        .find(|node| node.kind() == SyntaxKind::DoublebraceBlock)
        .expect("Doublebrace ブロックが生成されるはず");
    let statement_list = doublebrace
        .children()
        .find(|child| child.kind() == SyntaxKind::StatementList)
        .expect("Doublebrace ブロック内に StatementList が必要");

    assert!(
        statement_list
            .children()
            .any(|node| node.kind() == SyntaxKind::ReturnStatement),
        "return 文が StatementList に残り、後段でエラー検出できること"
    );
}
