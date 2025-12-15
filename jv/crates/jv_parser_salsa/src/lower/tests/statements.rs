use crate::lexer::Lexer;
use crate::lower::LoweringResult;
use crate::lower::lower;
use crate::parser::parse;
use jv_ast::statement::Statement;
use jv_ast::types::Literal;

fn lower_source(source: &str) -> LoweringResult {
    let lexer = Lexer::new(source).expect("lexing should succeed");
    let tokens = lexer.collect_owned_tokens();
    let parse_result = parse(tokens);
    lower(source, &parse_result)
}

#[test]
fn lowers_package_and_import() {
    let result = lower_source("package main\nimport foo.bar\n");
    assert!(
        result.diagnostics.is_empty(),
        "diagnostics: {:?}",
        result.diagnostics
    );
    assert_eq!(result.statements.len(), 2);
    match &result.statements[0] {
        Statement::Package { name, .. } => assert_eq!(name, "main"),
        other => panic!("expected package statement, got {:?}", other),
    }
    match &result.statements[1] {
        Statement::Import { path, .. } => assert_eq!(path, "foo.bar"),
        other => panic!("expected import, got {:?}", other),
    }
}

#[test]
fn lowers_val_and_var_declarations() {
    let result = lower_source("val x = 1\nvar y: Int = 2\n");
    assert!(
        result.diagnostics.is_empty(),
        "diagnostics: {:?}",
        result.diagnostics
    );
    assert_eq!(result.statements.len(), 2);
    match &result.statements[0] {
        Statement::ValDeclaration {
            name, initializer, ..
        } => {
            assert_eq!(name, "x");
            assert!(
                matches!(initializer, jv_ast::Expression::Literal(Literal::Number(n), _) if n == "1")
            );
        }
        other => panic!("expected val declaration, got {:?}", other),
    }

    match &result.statements[1] {
        Statement::VarDeclaration {
            name, initializer, ..
        } => {
            assert_eq!(name, "y");
            assert!(initializer.is_some());
        }
        other => panic!("expected var declaration, got {:?}", other),
    }
}

#[test]
fn skips_if_statement_and_reports_parser_error() {
    let source = "if true { }";
    let lexer = Lexer::new(source).expect("lex");
    let tokens = lexer.collect_owned_tokens();
    let parse_result = parse(tokens);
    assert!(
        parse_result
            .output
            .diagnostics
            .iter()
            .any(|d| d.message.contains("JV3103")),
        "expected JV3103 diagnostic, got {:?}",
        parse_result.output.diagnostics
    );
    let lowering = lower(source, &parse_result);
    assert!(
        lowering
            .diagnostics
            .iter()
            .any(|d| d.message.contains("JV3103")),
        "lowering should surface JV3103 diagnostic, got {:?}",
        lowering.diagnostics
    );
    assert!(
        lowering.statements.is_empty(),
        "if should not produce statements, got {:?}",
        lowering.statements
    );
}

#[test]
fn lowers_inline_test_dataset_rows_as_columns() {
    let source = r#"
        test "dataset addition" [
            ["carry" 11 17 28]
            ["negative" -5 3 -2]
        ] (label: String, lhs: Int, rhs: Int, expected: Int) {
            val sum = lhs + rhs
            sum == expected
        }
    "#;
    let result = lower_source(source);
    assert!(
        result.diagnostics.is_empty(),
        "diagnostics: {:?}",
        result.diagnostics
    );
    let stmt = result
        .statements
        .first()
        .expect("should lower into a test statement");
    match stmt {
        Statement::TestDeclaration(decl) => {
            let dataset = decl.dataset.as_ref().expect("dataset should be present");
            match dataset {
                jv_ast::statement::TestDataset::InlineArray { rows, .. } => {
                    assert_eq!(rows.len(), 2);
                    assert_eq!(rows[0].values.len(), 4, "row0: {:?}", rows[0].values);
                    assert_eq!(rows[1].values.len(), 4, "row1: {:?}", rows[1].values);
                }
                other => panic!("expected InlineArray dataset, got {:?}", other),
            }
        }
        other => panic!("expected TestDeclaration, got {:?}", other),
    }
}
