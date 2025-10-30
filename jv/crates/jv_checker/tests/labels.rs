use jv_ast::{
    Expression, ForInStatement, LoopBinding, LoopStrategy, Program, Span, Statement,
    expression::SequenceDelimiter,
};
use jv_checker::{CheckError, TypeChecker};

fn new_span() -> Span {
    Span::dummy()
}

fn empty_iterable(span: &Span) -> Expression {
    Expression::Array {
        elements: Vec::new(),
        delimiter: SequenceDelimiter::Comma,
        span: span.clone(),
    }
}

fn loop_with_label(label: Option<&str>, body: Vec<Statement>) -> Statement {
    let loop_span = new_span();
    Statement::ForIn(ForInStatement {
        binding: LoopBinding {
            name: "value".into(),
            pattern: None,
            type_annotation: None,
            span: loop_span.clone(),
        },
        iterable: empty_iterable(&loop_span),
        strategy: LoopStrategy::Iterable,
        label: label.map(|name| name.to_string()),
        body: Box::new(Expression::Block {
            statements: body,
            label: None,
            span: loop_span.clone(),
        }),
        span: loop_span,
    })
}

fn collect_validation_codes(errors: &[CheckError]) -> Vec<String> {
    errors
        .iter()
        .filter_map(|error| match error {
            CheckError::ValidationError { message, .. } => message
                .split(':')
                .next()
                .map(|code| code.trim().to_string()),
            _ => None,
        })
        .collect()
}

fn wrap(statements: Vec<Statement>) -> Program {
    Program {
        package: None,
        imports: Vec::new(),
        statements,
        span: new_span(),
    }
}

// 未定義ラベル参照が E-LABEL-UNDEFINED を報告することを確認する
#[test]
fn undefined_label_reports_e_label_undefined() {
    let break_stmt = Statement::Break {
        label: Some("missing".into()),
        span: new_span(),
    };
    let program = wrap(vec![loop_with_label(None, vec![break_stmt])]);

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);

    match result {
        Err(errors) => {
            let codes = collect_validation_codes(&errors);
            assert!(
                codes.iter().any(|code| code == "E-LABEL-UNDEFINED"),
                "期待した E-LABEL-UNDEFINED が発生していません: {:?}",
                codes
            );
        }
        Ok(()) => panic!("エラーを期待していましたが検証が成功しました"),
    }
}

// ループ以外のラベルに対する continue が E-LABEL-NON_LOOP_CONTINUE を報告する
#[test]
fn continue_to_non_loop_label_reports_expected_diagnostic() {
    let span = new_span();
    let block_expr = Statement::Expression {
        expr: Expression::Block {
            statements: vec![Statement::Continue {
                label: Some("section".into()),
                span: span.clone(),
            }],
            label: Some("section".into()),
            span: span.clone(),
        },
        span: span.clone(),
    };
    let program = wrap(vec![loop_with_label(None, vec![block_expr])]);

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);

    match result {
        Err(errors) => {
            let codes = collect_validation_codes(&errors);
            assert!(
                codes.iter().any(|code| code == "E-LABEL-NON_LOOP_CONTINUE"),
                "期待した E-LABEL-NON_LOOP_CONTINUE が発生していません: {:?}",
                codes
            );
        }
        Ok(()) => panic!("エラーを期待していましたが検証が成功しました"),
    }
}

// ラムダ以外のラベルへの return が E-LABEL-NON_LAMBDA_RETURN を報告する
#[test]
fn return_to_non_lambda_label_reports_expected_diagnostic() {
    let loop_body = vec![Statement::Return {
        label: Some("outer".into()),
        value: None,
        span: new_span(),
    }];
    let loop_stmt = loop_with_label(Some("outer"), loop_body);

    let lambda_expr = Expression::Lambda {
        parameters: Vec::new(),
        body: Box::new(Expression::Block {
            statements: vec![loop_stmt],
            label: Some("lambda".into()),
            span: new_span(),
        }),
        label: Some("task".into()),
        span: new_span(),
    };

    let program = wrap(vec![Statement::Expression {
        expr: lambda_expr,
        span: new_span(),
    }]);

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);

    match result {
        Err(errors) => {
            let codes = collect_validation_codes(&errors);
            assert!(
                codes.iter().any(|code| code == "E-LABEL-NON_LAMBDA_RETURN"),
                "期待した E-LABEL-NON_LAMBDA_RETURN が発生していません: {:?}",
                codes
            );
        }
        Ok(()) => panic!("エラーを期待していましたが検証が成功しました"),
    }
}

// 同一スコープでラベルを重複させた場合に E-LABEL-REDECLARED が報告される
#[test]
fn duplicated_label_in_scope_reports_e_label_redeclared() {
    let loop_body = vec![Statement::Break {
        label: None,
        span: new_span(),
    }];
    let program = wrap(vec![
        loop_with_label(Some("repeat"), loop_body.clone()),
        loop_with_label(Some("repeat"), loop_body),
    ]);

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);

    match result {
        Err(errors) => {
            let codes = collect_validation_codes(&errors);
            assert!(
                codes.iter().any(|code| code == "E-LABEL-REDECLARED"),
                "期待した E-LABEL-REDECLARED が発生していません: {:?}",
                codes
            );
        }
        Ok(()) => panic!("エラーを期待していましたが検証が成功しました"),
    }
}

// ネストしたラベルのシャドーイングが許容されることを確認する
#[test]
fn label_shadowing_with_nested_loops_is_accepted() {
    let inner = loop_with_label(
        Some("loop"),
        vec![Statement::Break {
            label: Some("loop".into()),
            span: new_span(),
        }],
    );
    let outer = loop_with_label(
        Some("loop"),
        vec![
            inner,
            Statement::Break {
                label: Some("loop".into()),
                span: new_span(),
            },
        ],
    );
    let program = wrap(vec![outer]);

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);

    if let Err(errors) = result {
        panic!(
            "シャドーイング許容テストでエラーが発生しました: {:?}",
            errors
        );
    }
}
