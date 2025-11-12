use crate::lowering::lower_program;
use crate::parser::parse;
use crate::{JvLanguage, ParseBuilder};
use jv_ast::expression::{Argument, CallArgumentStyle};
use jv_ast::{Expression, SequenceDelimiter, Statement};
use jv_lexer::Lexer;
use rowan::SyntaxNode;

fn lower_statements(source: &str) -> Vec<Statement> {
    let tokens = Lexer::new(source.to_string())
        .tokenize()
        .expect("字句解析が成功すること");
    let parse_output = parse(&tokens);
    assert!(
        parse_output.diagnostics.is_empty(),
        "パーサ診断が発生しました: {:?}",
        parse_output.diagnostics
    );
    assert!(
        !parse_output.recovered,
        "Rowan パーサでのエラー回復は不要のはずです"
    );

    let green = ParseBuilder::build_from_events(&parse_output.events, &tokens);
    let syntax: SyntaxNode<JvLanguage> = SyntaxNode::new_root(green);
    let lowering = lower_program(&syntax, &tokens);
    assert!(
        lowering.diagnostics.is_empty(),
        "ローワリング診断が発生しました: {:?}",
        lowering.diagnostics
    );
    lowering.statements
}

#[test]
fn call_expression_preserves_whitespace_argument_style() {
    let statements = lower_statements("val result = sum(1 2 3)");
    let initializer = statements
        .iter()
        .find_map(|statement| match statement {
            Statement::ValDeclaration { initializer, .. } => Some(initializer),
            _ => None,
        })
        .expect("val 宣言が生成されること");

    match initializer {
        Expression::Call {
            args,
            argument_metadata,
            ..
        } => {
            assert_eq!(args.len(), 3, "引数は3件になるはずです");
            for argument in args {
                match argument {
                    Argument::Positional(_) => {}
                    other => panic!("位置引数以外が生成されました: {:?}", other),
                }
            }
            assert_eq!(
                argument_metadata.style,
                CallArgumentStyle::Whitespace,
                "空白レイアウトの引数スタイルを保持する必要があります"
            );
            assert!(
                !argument_metadata.used_commas,
                "カンマを使用しないことをメタデータで記録する必要があります"
            );
        }
        other => panic!("呼び出し式が生成されるはずが: {:?}", other),
    }
}

#[test]
fn array_literal_records_whitespace_delimiter() {
    let statements = lower_statements("val numbers = [1 2 3]");
    let initializer = statements
        .iter()
        .find_map(|statement| match statement {
            Statement::ValDeclaration { initializer, .. } => Some(initializer),
            _ => None,
        })
        .expect("val 宣言が生成されること");

    match initializer {
        Expression::Array {
            elements,
            delimiter,
            ..
        } => {
            assert_eq!(elements.len(), 3, "配列要素は3件になるはずです");
            assert_eq!(
                *delimiter,
                SequenceDelimiter::Whitespace,
                "空白区切りの配列が適切に記録される必要があります"
            );
        }
        other => panic!("配列リテラルが生成されるはずが: {:?}", other),
    }
}
