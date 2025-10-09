use jv_ast::{Argument, BinaryOp, Expression, Literal, Parameter, Span};
use jv_ir::{
    transform_expression, IrExpression, JavaType, SequenceSource, SequenceTerminalEvaluation,
    SequenceTerminalKind, TransformContext,
};

fn dummy_span() -> Span {
    Span::dummy()
}

fn register_numbers(context: &mut TransformContext) {
    context.add_variable(
        "numbers".to_string(),
        JavaType::Reference {
            name: "java.util.List".to_string(),
            generic_args: vec![],
        },
    );
}

fn identifier(name: &str) -> Expression {
    Expression::Identifier(name.to_string(), dummy_span())
}

fn lambda(params: &[&str], body: Expression) -> Expression {
    let parameters = params
        .iter()
        .map(|name| Parameter {
            name: (*name).to_string(),
            type_annotation: None,
            default_value: None,
            span: dummy_span(),
        })
        .collect();
    Expression::Lambda {
        parameters,
        body: Box::new(body),
        span: dummy_span(),
    }
}

fn add(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        left: Box::new(lhs),
        op: BinaryOp::Add,
        right: Box::new(rhs),
        span: dummy_span(),
    }
}

fn map_pipeline(source: Expression) -> Expression {
    let lambda_body = add(
        identifier("x"),
        Expression::Literal(Literal::Number("1".into()), dummy_span()),
    );
    let lambda_expr = lambda(&["x"], lambda_body);
    Expression::Call {
        function: Box::new(Expression::MemberAccess {
            object: Box::new(source),
            property: "map".to_string(),
            span: dummy_span(),
        }),
        args: vec![Argument::Positional(lambda_expr)],
        argument_metadata: jv_ast::CallArgumentMetadata::default(),
        span: dummy_span(),
    }
}

fn reduce_expression() -> Expression {
    let map_call = map_pipeline(identifier("numbers"));
    let reduce_lambda_body = add(identifier("acc"), identifier("value"));
    let reduce_lambda = lambda(&["acc", "value"], reduce_lambda_body);
    Expression::Call {
        function: Box::new(Expression::MemberAccess {
            object: Box::new(map_call),
            property: "reduce".to_string(),
            span: dummy_span(),
        }),
        args: vec![Argument::Positional(reduce_lambda)],
        argument_metadata: jv_ast::CallArgumentMetadata::default(),
        span: dummy_span(),
    }
}

fn fold_expression() -> Expression {
    let map_call = map_pipeline(identifier("numbers"));
    let fold_lambda_body = add(identifier("acc"), identifier("value"));
    let fold_lambda = lambda(&["acc", "value"], fold_lambda_body);
    let initial = Expression::Literal(Literal::Number("0".to_string()), dummy_span());
    Expression::Call {
        function: Box::new(Expression::MemberAccess {
            object: Box::new(map_call),
            property: "fold".to_string(),
            span: dummy_span(),
        }),
        args: vec![
            Argument::Positional(initial),
            Argument::Positional(fold_lambda),
        ],
        argument_metadata: jv_ast::CallArgumentMetadata::default(),
        span: dummy_span(),
    }
}

fn count_expression() -> Expression {
    let map_call = map_pipeline(identifier("numbers"));
    Expression::Call {
        function: Box::new(Expression::MemberAccess {
            object: Box::new(map_call),
            property: "count".to_string(),
            span: dummy_span(),
        }),
        args: vec![],
        argument_metadata: jv_ast::CallArgumentMetadata::default(),
        span: dummy_span(),
    }
}

#[test]
fn reduce_terminal_requires_non_empty_source() {
    let mut context = TransformContext::new();
    register_numbers(&mut context);
    let ir = transform_expression(reduce_expression(), &mut context)
        .expect("sequence reduce pipeline transforms");

    let (pipeline, java_type) = match ir {
        IrExpression::SequencePipeline {
            pipeline,
            java_type,
            ..
        } => (pipeline, java_type),
        other => panic!("expected sequence pipeline, got {:?}", other),
    };

    assert!(matches!(pipeline.source, SequenceSource::Collection { .. }));
    let terminal = pipeline
        .terminal
        .as_ref()
        .expect("reduce pipeline should have terminal");
    assert!(matches!(terminal.kind, SequenceTerminalKind::Reduce { .. }));
    assert_eq!(terminal.evaluation, SequenceTerminalEvaluation::Reducer);
    assert!(terminal.requires_non_empty_source);
    assert_eq!(java_type, JavaType::object());
}

#[test]
fn fold_terminal_retains_initial_expression() {
    let mut context = TransformContext::new();
    register_numbers(&mut context);
    let ir = transform_expression(fold_expression(), &mut context)
        .expect("sequence fold pipeline transforms");

    let pipeline = match ir {
        IrExpression::SequencePipeline { pipeline, .. } => pipeline,
        other => panic!("expected sequence pipeline, got {:?}", other),
    };

    let terminal = pipeline
        .terminal
        .as_ref()
        .expect("fold pipeline should have terminal");
    match &terminal.kind {
        SequenceTerminalKind::Fold { initial, .. } => {
            assert!(matches!(**initial, IrExpression::Literal(_, _)));
        }
        other => panic!("expected fold terminal, got {:?}", other),
    }
    assert_eq!(terminal.evaluation, SequenceTerminalEvaluation::Reducer);
    assert!(
        !terminal.requires_non_empty_source,
        "fold should allow empty collections"
    );
}

#[test]
fn count_terminal_uses_aggregate_policy() {
    let mut context = TransformContext::new();
    register_numbers(&mut context);
    let ir = transform_expression(count_expression(), &mut context)
        .expect("sequence count pipeline transforms");

    let pipeline = match ir {
        IrExpression::SequencePipeline { pipeline, .. } => pipeline,
        other => panic!("expected sequence pipeline, got {:?}", other),
    };

    let terminal = pipeline
        .terminal
        .as_ref()
        .expect("count pipeline should have terminal");
    assert!(matches!(terminal.kind, SequenceTerminalKind::Count));
    assert_eq!(terminal.evaluation, SequenceTerminalEvaluation::Aggregator);
    assert!(
        !terminal.requires_non_empty_source,
        "count should not require non-empty source"
    );
}
