use jv_ast::expression::DoublebraceInit;
use jv_ast::{
    Argument, CallArgumentMetadata, CallArgumentStyle, CallKind, Expression, Literal, Span,
    Statement,
};
use jv_ir::{
    DoublebraceBaseStrategy, DoublebraceCopySourceStrategy, DoublebraceFieldUpdate,
    DoublebraceLoweringCopyPlan, DoublebraceLoweringKind, DoublebraceLoweringMutatePlan,
    DoublebraceLoweringPlan, DoublebraceLoweringStep, DoublebraceMethodInvocation,
    IrDoublebraceMutation, IrDoublebracePlan, IrExpression, JavaType, TransformContext,
    transform_expression,
};

fn ダミーのスパン() -> Span {
    Span::dummy()
}

fn 受信型() -> JavaType {
    JavaType::Reference {
        name: "com.example.Mutable".to_string(),
        generic_args: vec![],
    }
}

#[test]
fn ダブルブレース_ミューテーション変換() {
    let span = ダミーのスパン();
    let statements = vec![
        Statement::Assignment {
            target: Expression::Identifier("value".into(), span.clone()),
            binding_pattern: None,
            value: Expression::Literal(Literal::Number("42".into()), span.clone()),
            span: span.clone(),
        },
        Statement::Expression {
            expr: Expression::Call {
                function: Box::new(Expression::Identifier("push".into(), span.clone())),
                args: vec![Argument::Positional(Expression::Literal(
                    Literal::Number("1".into()),
                    span.clone(),
                ))],
                type_arguments: Vec::new(),
                argument_metadata: CallArgumentMetadata::with_style(CallArgumentStyle::Whitespace),
                call_kind: CallKind::Function,
                span: span.clone(),
            },
            span: span.clone(),
        },
    ];

    let init = DoublebraceInit {
        base: Some(Box::new(Expression::Identifier(
            "source".into(),
            span.clone(),
        ))),
        receiver_hint: None,
        statements,
        span: span.clone(),
    };

    let plan = DoublebraceLoweringPlan {
        receiver_fqcn: "com.example.Mutable".into(),
        kind: DoublebraceLoweringKind::Mutate(DoublebraceLoweringMutatePlan {
            base: DoublebraceBaseStrategy::ExistingInstance,
            steps: vec![
                DoublebraceLoweringStep::FieldAssignment(DoublebraceFieldUpdate {
                    name: "value".into(),
                    value: Expression::Literal(Literal::Number("42".into()), span.clone()),
                    span: span.clone(),
                }),
                DoublebraceLoweringStep::MethodCall(DoublebraceMethodInvocation {
                    name: "push".into(),
                    arguments: vec![Argument::Positional(Expression::Literal(
                        Literal::Number("1".into()),
                        span.clone(),
                    ))],
                    metadata: CallArgumentMetadata::with_style(CallArgumentStyle::Whitespace),
                    span: span.clone(),
                }),
            ],
        }),
    };

    let mut context = TransformContext::new();
    context.add_variable("source".into(), 受信型());
    context.insert_doublebrace_plan(&span, plan);

    let expression = Expression::DoublebraceInit(init);
    let ir = transform_expression(expression, &mut context)
        .expect("Doublebrace のミューテーション変換が成功するはずです");

    match ir {
        IrExpression::DoublebraceInit {
            base,
            receiver_type,
            plan: IrDoublebracePlan::Mutate(mut_plan),
            ..
        } => {
            let base_expr = base.expect("ベース式が存在するはずです");
            match *base_expr {
                IrExpression::Identifier { ref name, .. } => {
                    assert_eq!(name, "source", "ベース識別子が保持されていること");
                }
                other => panic!("識別子が生成されるべきですが {:?} が得られました", other),
            }

            match receiver_type {
                JavaType::Reference { ref name, .. } => assert_eq!(
                    name, "com.example.Mutable",
                    "レシーバー型が計画通りであること"
                ),
                other => panic!("参照型が必要ですが {:?} が返りました", other),
            }

            assert_eq!(
                mut_plan.steps.len(),
                2,
                "ミューテーション手順が 2 件収集されていること"
            );

            match &mut_plan.steps[0] {
                IrDoublebraceMutation::FieldAssignment(update) => {
                    assert_eq!(update.name, "value", "フィールド名が保持されること");
                    match &update.value {
                        IrExpression::Literal(Literal::Number(number), _) => {
                            assert_eq!(number, "42", "フィールド更新にリテラルが使われていること")
                        }
                        other => panic!("数値リテラルが必要ですが {:?} が得られました", other),
                    }
                }
                other => panic!("フィールド更新が必要ですが {:?} が返りました", other),
            }

            match &mut_plan.steps[1] {
                IrDoublebraceMutation::MethodCall(call) => {
                    assert_eq!(call.name, "push", "メソッド名が保持されること");
                    assert_eq!(
                        call.argument_style,
                        CallArgumentStyle::Whitespace,
                        "引数スタイルが伝播すること"
                    );
                    assert_eq!(call.arguments.len(), 1, "引数が 1 件であること");
                    match &call.arguments[0] {
                        IrExpression::Literal(Literal::Number(value), _) => {
                            assert_eq!(value, "1", "引数リテラルが保持されること")
                        }
                        other => panic!("引数が数値リテラルであるべきですが {:?}", other),
                    }
                }
                other => panic!("メソッド呼び出しが必要ですが {:?} が返りました", other),
            }
        }
        other => panic!("DoublebraceInit が生成されるべきですが {:?}", other),
    }
}

#[test]
fn ダブルブレース_IRはジェネリック受信型を保持する() {
    let span = ダミーのスパン();
    let init = DoublebraceInit {
        base: None,
        receiver_hint: None,
        statements: Vec::new(),
        span: span.clone(),
    };

    let plan = DoublebraceLoweringPlan {
        receiver_fqcn: "java.util.ArrayList<java.lang.String>".into(),
        kind: DoublebraceLoweringKind::Mutate(DoublebraceLoweringMutatePlan {
            base: DoublebraceBaseStrategy::SynthesizedInstance,
            steps: Vec::new(),
        }),
    };

    let mut context = TransformContext::new();
    context.insert_doublebrace_plan(&span, plan);

    let expression = Expression::DoublebraceInit(init);
    let ir = transform_expression(expression, &mut context)
        .expect("Doublebrace のジェネリック変換が成功するはずです");

    match ir {
        IrExpression::DoublebraceInit {
            base,
            receiver_type,
            ..
        } => {
            let base_expr = base.expect("Synthesized インスタンスが生成されるはずです");
            match *base_expr {
                IrExpression::ObjectCreation {
                    ref class_name,
                    ref generic_args,
                    ..
                } => {
                    assert_eq!(
                        class_name, "java.util.ArrayList",
                        "既定の具象クラスが使用されること"
                    );
                    assert_eq!(
                        generic_args.len(),
                        1,
                        "生成時にもジェネリック引数が保持されること"
                    );
                }
                other => panic!("ObjectCreation が必要ですが {:?}", other),
            }

            match receiver_type {
                JavaType::Reference { name, generic_args } => {
                    assert_eq!(name, "java.util.ArrayList");
                    assert_eq!(
                        generic_args.len(),
                        1,
                        "ジェネリック引数が 1 件保持されること"
                    );
                    match &generic_args[0] {
                        JavaType::Reference { name, .. } => {
                            assert_eq!(name, "java.lang.String");
                        }
                        other => panic!("ジェネリックは参照型であるべきですが {:?}", other),
                    }
                }
                other => panic!("参照型が必要ですが {:?}", other),
            }
        }
        other => panic!("DoublebraceInit が生成されるべきですが {:?}", other),
    }
}

#[test]
fn ダブルブレース_標準インタフェースにはデフォルト具象を使用する() {
    let span = ダミーのスパン();
    let init = DoublebraceInit {
        base: None,
        receiver_hint: None,
        statements: Vec::new(),
        span: span.clone(),
    };

    let plan = DoublebraceLoweringPlan {
        receiver_fqcn: "java.util.Map<java.lang.String, java.lang.Integer>".into(),
        kind: DoublebraceLoweringKind::Mutate(DoublebraceLoweringMutatePlan {
            base: DoublebraceBaseStrategy::SynthesizedInstance,
            steps: Vec::new(),
        }),
    };

    let mut context = TransformContext::new();
    context.insert_doublebrace_plan(&span, plan);

    let expression = Expression::DoublebraceInit(init);
    let ir = transform_expression(expression, &mut context)
        .expect("Doublebrace のデフォルト具象生成が成功するはずです");

    match ir {
        IrExpression::DoublebraceInit { base, .. } => {
            let base_expr = base.expect("Synthesized インスタンスが必要です");
            match *base_expr {
                IrExpression::ObjectCreation {
                    ref class_name,
                    ref generic_args,
                    ..
                } => {
                    assert_eq!(
                        class_name, "java.util.LinkedHashMap",
                        "Map インタフェースには LinkedHashMap が利用されること"
                    );
                    assert_eq!(
                        generic_args.len(),
                        2,
                        "Map 生成時にキー・値双方の型引数が保持されること"
                    );
                }
                other => panic!("ObjectCreation が必要ですが {:?}", other),
            }
        }
        other => panic!("DoublebraceInit が生成されるべきですが {:?}", other),
    }
}

#[test]
fn ダブルブレース_インタフェースコンストラクタは具象クラスで生成される() {
    let span = ダミーのスパン();
    let base_call = Expression::Call {
        function: Box::new(Expression::Identifier("Map".into(), span.clone())),
        args: vec![Argument::Positional(Expression::Identifier(
            "existing".into(),
            span.clone(),
        ))],
        type_arguments: Vec::new(),
        argument_metadata: CallArgumentMetadata::with_style(CallArgumentStyle::Comma),
        call_kind: CallKind::Constructor {
            type_name: "Map".into(),
            fqcn: Some("java.util.Map".into()),
        },
        span: span.clone(),
    };

    let init = DoublebraceInit {
        base: Some(Box::new(base_call)),
        receiver_hint: None,
        statements: Vec::new(),
        span: span.clone(),
    };

    let plan = DoublebraceLoweringPlan {
        receiver_fqcn: "java.util.LinkedHashMap<java.lang.String, java.lang.Integer>".into(),
        kind: DoublebraceLoweringKind::Mutate(DoublebraceLoweringMutatePlan {
            base: DoublebraceBaseStrategy::SynthesizedInstance,
            steps: Vec::new(),
        }),
    };

    let mut context = TransformContext::new();
    context.add_variable(
        "existing".into(),
        JavaType::Reference {
            name: "java.util.Map".into(),
            generic_args: Vec::new(),
        },
    );
    context.insert_doublebrace_plan(&span, plan);

    let expression = Expression::DoublebraceInit(init);
    let ir = transform_expression(expression, &mut context)
        .expect("Doublebrace インタフェースコンストラクタの具象化に成功するはずです");

    match ir {
        IrExpression::DoublebraceInit {
            base,
            receiver_type,
            ..
        } => {
            let base_expr = base.expect("Synthesized インスタンスが生成されること");
            match *base_expr {
                IrExpression::ObjectCreation {
                    ref class_name,
                    ref args,
                    ref generic_args,
                    ..
                } => {
                    assert_eq!(
                        class_name, "java.util.LinkedHashMap",
                        "Map コンストラクタから LinkedHashMap が生成されること"
                    );
                    assert_eq!(generic_args.len(), 2, "キーと値の型引数が保持されること");
                    assert_eq!(args.len(), 1, "元の引数が保持されること");
                    match &args[0] {
                        IrExpression::Identifier { name, .. } => {
                            assert_eq!(name, "existing", "引数識別子が変換後も保持されること")
                        }
                        other => panic!("Identifier が必要ですが {:?}", other),
                    }
                }
                other => panic!("ObjectCreation が必要ですが {:?}", other),
            }

            match receiver_type {
                JavaType::Reference { name, generic_args } => {
                    assert_eq!(
                        name, "java.util.LinkedHashMap",
                        "レシーバー型が既定具象へ変換されること"
                    );
                    assert_eq!(
                        generic_args.len(),
                        2,
                        "レシーバー型にも型引数が保持されること"
                    );
                }
                other => panic!("参照型が必要ですが {:?}", other),
            }
        }
        other => panic!("DoublebraceInit が生成されるべきですが {:?}", other),
    }
}

#[test]
fn ダブルブレース_コピー変換() {
    let span = ダミーのスパン();
    let statements = vec![Statement::Assignment {
        target: Expression::Identifier("name".into(), span.clone()),
        binding_pattern: None,
        value: Expression::Literal(Literal::String("Alice".into()), span.clone()),
        span: span.clone(),
    }];

    let init = DoublebraceInit {
        base: Some(Box::new(Expression::Identifier(
            "original".into(),
            span.clone(),
        ))),
        receiver_hint: None,
        statements,
        span: span.clone(),
    };

    let plan = DoublebraceLoweringPlan {
        receiver_fqcn: "com.example.Immutable".into(),
        kind: DoublebraceLoweringKind::Copy(DoublebraceLoweringCopyPlan {
            source: DoublebraceCopySourceStrategy::ExistingInstance,
            updates: vec![DoublebraceFieldUpdate {
                name: "name".into(),
                value: Expression::Literal(Literal::String("Alice".into()), span.clone()),
                span: span.clone(),
            }],
        }),
    };

    let mut context = TransformContext::new();
    context.add_variable(
        "original".into(),
        JavaType::Reference {
            name: "com.example.Immutable".into(),
            generic_args: vec![],
        },
    );
    context.insert_doublebrace_plan(&span, plan);

    let expression = Expression::DoublebraceInit(init);
    let ir = transform_expression(expression, &mut context)
        .expect("Doublebrace のコピー変換が成功するはずです");

    match ir {
        IrExpression::DoublebraceInit {
            base,
            receiver_type,
            plan: IrDoublebracePlan::Copy(copy_plan),
            ..
        } => {
            assert!(base.is_some(), "コピー戦略ではベース式が必要です");
            match receiver_type {
                JavaType::Reference { ref name, .. } => assert_eq!(
                    name, "com.example.Immutable",
                    "コピー対象型が保持されること"
                ),
                other => panic!("参照型が必要ですが {:?} が返りました", other),
            }
            assert_eq!(
                copy_plan.updates.len(),
                1,
                "コピー更新が 1 件収集されていること"
            );
            let update = &copy_plan.updates[0];
            assert_eq!(update.name, "name", "フィールド名が維持されること");
            match &update.value {
                IrExpression::Literal(Literal::String(value), _) => {
                    assert_eq!(value, "Alice", "文字列リテラルが保持されること");
                }
                other => panic!("文字列リテラルが必要ですが {:?}", other),
            }
        }
        other => panic!("DoublebraceInit が生成されるべきですが {:?}", other),
    }
}
