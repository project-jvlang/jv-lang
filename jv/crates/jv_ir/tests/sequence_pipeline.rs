use jv_ast::{
    Argument, BinaryOp, Expression, Literal, Parameter, SequenceDelimiter, Span, StringPart,
};
use jv_ir::{
    transform_expression, IrExpression, JavaType, SequenceSource, SequenceStage,
    SequenceTerminalEvaluation, SequenceTerminalKind, TransformContext,
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

fn add_offset(rhs: &str) -> Expression {
    add(identifier("i"), number_literal(rhs))
}

fn whitespace_offset_array() -> Expression {
    array_literal(vec![
        identifier("i"),
        add_offset("1"),
        add_offset("2"),
        add_offset("3"),
        add_offset("4"),
    ])
}

fn multiply(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        left: Box::new(lhs),
        op: BinaryOp::Multiply,
        right: Box::new(rhs),
        span: dummy_span(),
    }
}

fn modulo(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        left: Box::new(lhs),
        op: BinaryOp::Modulo,
        right: Box::new(rhs),
        span: dummy_span(),
    }
}

fn equal(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        left: Box::new(lhs),
        op: BinaryOp::Equal,
        right: Box::new(rhs),
        span: dummy_span(),
    }
}

fn number_literal(value: &str) -> Expression {
    Expression::Literal(Literal::Number(value.to_string()), dummy_span())
}

fn array_literal(elements: Vec<Expression>) -> Expression {
    Expression::Array {
        elements,
        delimiter: SequenceDelimiter::Whitespace,
        span: dummy_span(),
    }
}

fn call_method(receiver: Expression, method: &str, args: Vec<Argument>) -> Expression {
    Expression::Call {
        function: Box::new(Expression::MemberAccess {
            object: Box::new(receiver),
            property: method.to_string(),
            span: dummy_span(),
        }),
        args,
        argument_metadata: jv_ast::CallArgumentMetadata::default(),
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

fn map_expression() -> Expression {
    call_method(
        identifier("numbers"),
        "map",
        vec![Argument::Positional(lambda(
            &["value"],
            multiply(identifier("value"), number_literal("2")),
        ))],
    )
}

fn map_filter_expression() -> Expression {
    let mapped = map_expression();
    call_method(
        mapped,
        "filter",
        vec![Argument::Positional(lambda(
            &["candidate"],
            equal(
                modulo(identifier("candidate"), number_literal("2")),
                number_literal("0"),
            ),
        ))],
    )
}

fn map_filter_to_list_expression() -> Expression {
    let filtered = map_filter_expression();
    call_method(filtered, "toList", vec![])
}

#[test]
fn map_pipeline_without_terminal_stays_lazy_sequence() {
    let mut context = TransformContext::new();
    register_numbers(&mut context);

    let ir =
        transform_expression(map_expression(), &mut context).expect("map pipeline lowers to IR");

    let IrExpression::SequencePipeline {
        pipeline,
        java_type,
        ..
    } = ir
    else {
        panic!("expected sequence pipeline expression");
    };

    assert!(pipeline.lazy, "lazy pipelines should remain marked lazy");
    assert!(
        pipeline.terminal.is_none(),
        "map without terminal should not synthesize a terminal"
    );
    assert_eq!(
        pipeline.stages.len(),
        1,
        "single map call should produce one stage"
    );
    assert_eq!(
        java_type,
        JavaType::sequence(),
        "lazy pipeline should infer Sequence return type"
    );
}

fn complex_sequence_expression() -> Expression {
    let mapped = call_method(
        identifier("numbers"),
        "map",
        vec![Argument::Positional(lambda(
            &["value"],
            multiply(identifier("value"), number_literal("2")),
        ))],
    );
    let filtered = call_method(
        mapped,
        "filter",
        vec![Argument::Positional(lambda(
            &["candidate"],
            equal(
                modulo(identifier("candidate"), number_literal("4")),
                number_literal("0"),
            ),
        ))],
    );
    let flattened = call_method(
        filtered,
        "flatMap",
        vec![Argument::Positional(lambda(
            &["value"],
            array_literal(vec![
                identifier("value"),
                add(identifier("value"), number_literal("1")),
            ]),
        ))],
    );
    let taken = call_method(
        flattened,
        "take",
        vec![Argument::Positional(number_literal("3"))],
    );
    let dropped = call_method(
        taken,
        "drop",
        vec![Argument::Positional(number_literal("1"))],
    );
    let sorted = call_method(dropped, "sorted", vec![]);
    let sorted_by = call_method(
        sorted,
        "sortedBy",
        vec![Argument::Positional(lambda(
            &["value"],
            identifier("value"),
        ))],
    );
    call_method(sorted_by, "toList", vec![])
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

#[test]
fn map_filter_pipeline_is_eager_to_list() {
    let mut context = TransformContext::new();
    register_numbers(&mut context);
    let ir = transform_expression(map_filter_to_list_expression(), &mut context)
        .expect("map/filter pipeline lowers");

    let pipeline = match ir {
        IrExpression::SequencePipeline { pipeline, .. } => pipeline,
        other => panic!("expected sequence pipeline, got {:?}", other),
    };

    assert_eq!(pipeline.stages.len(), 2, "expected map and filter stages");
    assert!(matches!(pipeline.stages[0], SequenceStage::Map { .. }));
    assert!(matches!(pipeline.stages[1], SequenceStage::Filter { .. }));
    assert!(
        !pipeline.lazy,
        "toList terminal should force eager evaluation"
    );

    let terminal = pipeline
        .terminal
        .as_ref()
        .expect("pipeline should include toList terminal");
    assert!(matches!(terminal.kind, SequenceTerminalKind::ToList));
    assert_eq!(
        terminal.evaluation,
        SequenceTerminalEvaluation::Collector,
        "toList terminal should use collector evaluation"
    );
}

#[test]
fn pipeline_detects_complex_stage_chain() {
    let mut context = TransformContext::new();
    register_numbers(&mut context);
    let ir =
        transform_expression(complex_sequence_expression(), &mut context).expect("pipeline lowers");

    let pipeline = match ir {
        IrExpression::SequencePipeline { pipeline, .. } => pipeline,
        other => panic!("expected sequence pipeline, got {:?}", other),
    };

    assert_eq!(
        pipeline.stages.len(),
        7,
        "expected seven intermediate stages"
    );
    assert!(
        matches!(pipeline.stages[0], SequenceStage::Map { .. }),
        "first stage should be map"
    );
    assert!(
        matches!(pipeline.stages[1], SequenceStage::Filter { .. }),
        "second stage should be filter"
    );
    match &pipeline.stages[2] {
        SequenceStage::FlatMap { flatten_depth, .. } => {
            assert_eq!(
                *flatten_depth, 1,
                "flatMap should default to single-level flattening"
            );
        }
        other => panic!("expected flatMap stage, found {:?}", other),
    }
    assert!(
        matches!(pipeline.stages[3], SequenceStage::Take { .. }),
        "fourth stage should be take"
    );
    assert!(
        matches!(pipeline.stages[4], SequenceStage::Drop { .. }),
        "fifth stage should be drop"
    );
    match &pipeline.stages[5] {
        SequenceStage::Sorted { comparator, .. } => {
            assert!(comparator.is_none(), "sorted stage should omit comparator");
        }
        other => panic!(
            "expected sorted stage without comparator, found {:?}",
            other
        ),
    }
    match &pipeline.stages[6] {
        SequenceStage::Sorted { comparator, .. } => {
            assert!(
                comparator.is_some(),
                "sortedBy stage should retain comparator lambda"
            );
        }
        other => panic!("expected sortedBy stage with comparator, found {:?}", other),
    }

    assert!(
        !pipeline.lazy,
        "presence of terminal should mark pipeline as eager"
    );

    let terminal = pipeline
        .terminal
        .as_ref()
        .expect("complex pipeline should end with toList");
    assert!(
        matches!(terminal.kind, SequenceTerminalKind::ToList),
        "terminal must be toList"
    );
    assert_eq!(
        terminal.evaluation,
        SequenceTerminalEvaluation::Collector,
        "toList evaluation should be collector"
    );
}

#[test]
fn sum_terminal_uses_aggregator_evaluation() {
    let mut context = TransformContext::new();
    register_numbers(&mut context);

    let mapped = call_method(
        identifier("numbers"),
        "map",
        vec![Argument::Positional(lambda(
            &["value"],
            identifier("value"),
        ))],
    );
    let sum_expr = call_method(mapped, "sum", vec![]);
    let ir =
        transform_expression(sum_expr, &mut context).expect("sequence sum pipeline transforms");

    let pipeline = match ir {
        IrExpression::SequencePipeline { pipeline, .. } => pipeline,
        other => panic!("expected sequence pipeline, got {:?}", other),
    };

    let terminal = pipeline
        .terminal
        .as_ref()
        .expect("sum pipeline should have terminal");
    assert!(
        matches!(terminal.kind, SequenceTerminalKind::Sum),
        "terminal should be sum"
    );
    assert_eq!(
        terminal.evaluation,
        SequenceTerminalEvaluation::Aggregator,
        "sum should use aggregator evaluation"
    );
    assert!(
        !terminal.requires_non_empty_source,
        "sum should not require non-empty source"
    );
}

#[test]
fn whitespace_list_literal_becomes_list_sequence_source() {
    let mut context = TransformContext::new();

    let literal = array_literal(vec![
        number_literal("1"),
        number_literal("2"),
        number_literal("3"),
    ]);
    let map_expr = map_pipeline(literal);
    let filter_lambda = lambda(
        &["candidate"],
        Expression::Literal(Literal::Boolean(true), dummy_span()),
    );
    let filter_call = call_method(
        map_expr,
        "filter",
        vec![Argument::Positional(filter_lambda)],
    );

    let ir = transform_expression(filter_call, &mut context)
        .expect("list literal pipeline lowers to sequence");

    let pipeline = match ir {
        IrExpression::SequencePipeline { pipeline, .. } => pipeline,
        other => panic!("expected sequence pipeline, got {:?}", other),
    };

    let list_elements = match pipeline.source {
        SequenceSource::ListLiteral { elements, .. } => elements,
        other => panic!("expected list literal source, got {:?}", other),
    };

    assert_eq!(
        list_elements.len(),
        3,
        "list literal should retain elements"
    );
    assert!(
        list_elements
            .iter()
            .all(|element| matches!(element, IrExpression::Literal(_, _))),
        "list literal elements should lower to literal IR nodes",
    );
    assert_eq!(pipeline.stages.len(), 2, "map + filter stages expected");
}

#[test]
fn whitespace_array_with_offsets_lowers_to_array_creation() {
    let mut context = TransformContext::new();
    context.add_variable("i".to_string(), JavaType::int());

    let ir = transform_expression(whitespace_offset_array(), &mut context)
        .expect("offset array literal lowers to IR");

    let IrExpression::ArrayCreation {
        element_type,
        initializer,
        delimiter,
        ..
    } = ir
    else {
        panic!("expected array creation IR");
    };

    let elements = initializer.expect("array literal initializer present");
    assert_eq!(delimiter, SequenceDelimiter::Whitespace);
    assert_eq!(element_type, JavaType::int());
    assert_eq!(elements.len(), 5, "array should preserve all elements");
    assert!(
        matches!(elements[0], IrExpression::Identifier { ref name, .. } if name == "i"),
        "first element should remain an identifier",
    );
    assert!(
        elements.iter().skip(1).all(|element| matches!(
            element,
            IrExpression::Binary {
                op: BinaryOp::Add, ..
            }
        )),
        "subsequent elements should remain binary additions"
    );
}

#[test]
fn whitespace_array_inside_lambda_preserves_array_creation_body() {
    let mut context = TransformContext::new();
    let lambda_expr = lambda(&["i"], whitespace_offset_array());

    let ir =
        transform_expression(lambda_expr, &mut context).expect("lambda lowers to IR expression");

    let IrExpression::Lambda {
        param_names,
        param_types,
        body,
        ..
    } = ir
    else {
        panic!("expected lambda IR");
    };

    assert_eq!(param_names, vec!["i".to_string()]);
    assert_eq!(param_types, vec![JavaType::object()]);

    let IrExpression::ArrayCreation {
        initializer,
        delimiter,
        ..
    } = *body
    else {
        panic!("expected lambda body to remain array creation");
    };

    let elements = initializer.expect("array literal initializer present");
    assert_eq!(delimiter, SequenceDelimiter::Whitespace);
    assert_eq!(elements.len(), 5);
    assert!(
        elements.iter().skip(1).all(|element| matches!(
            element,
            IrExpression::Binary {
                op: BinaryOp::Add, ..
            }
        )),
        "lambda should retain binary additions inside array literal"
    );
}

#[test]
fn string_interpolation_materializes_lazy_sequence() {
    let mut context = TransformContext::new();
    register_numbers(&mut context);

    let interpolation = Expression::StringInterpolation {
        parts: vec![
            StringPart::Text("inline=".to_string()),
            StringPart::Expression(map_expression()),
        ],
        span: dummy_span(),
    };

    let ir = transform_expression(interpolation, &mut context)
        .expect("string interpolation lowers to IR");

    let IrExpression::StringFormat { args, .. } = ir else {
        panic!("expected string format IR expression");
    };

    assert_eq!(args.len(), 1, "expected single interpolation argument");
    let arg = &args[0];

    let IrExpression::SequencePipeline {
        pipeline,
        java_type,
        ..
    } = arg
    else {
        panic!("expected sequence pipeline argument");
    };

    assert_eq!(
        *java_type,
        JavaType::list(),
        "string interpolation should materialize sequences as java.util.List",
    );
    assert!(
        !pipeline.lazy,
        "materialized pipeline should evaluate eagerly for interpolation",
    );

    let terminal = pipeline
        .terminal
        .as_ref()
        .expect("string interpolation should inject toList terminal");
    assert!(matches!(terminal.kind, SequenceTerminalKind::ToList));
    assert_eq!(
        terminal.evaluation,
        SequenceTerminalEvaluation::Collector,
        "materialized terminal should use collector evaluation",
    );
}
