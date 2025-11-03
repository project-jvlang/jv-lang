use jv_ast::expression::{CallArgumentMetadata, CallArgumentStyle, CallKind, DoublebraceInit};
use jv_ast::types::{BinaryOp, TypeAnnotation, UnaryOp};
use jv_ast::{
    Argument, Expression, Literal, Modifiers, Program, Span, Statement, ValBindingOrigin,
};
use jv_build::metadata::{JavaMethodSignature, SymbolIndex, TypeEntry};
use jv_checker::TypeKind;
use jv_checker::inference::environment::TypeEnvironment;
use jv_checker::java::{
    CopySource, DoublebracePlan, DoublebracePlanError, MutationStep, PlanBase,
    plan_doublebrace_application, plan_doublebrace_in_program, span_key,
};
use jv_ir::types::JavaType;

fn span() -> Span {
    Span::dummy()
}

#[test]
fn mutate_plan_for_mutable_receiver() {
    // 可変クラスではフィールド代入とメソッド呼び出しがミューテーションとして扱われること。
    let span = span();
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
        statements: statements.clone(),
        span: span.clone(),
    };

    let mut index = SymbolIndex::new(Some(25));
    let mut entry = TypeEntry::new("com.example.Mutable".into(), "com.example".into(), None);
    entry.add_instance_field("value".into());
    entry.add_instance_method(
        "push".into(),
        JavaMethodSignature {
            parameters: Vec::new(),
            return_type: JavaType::Void,
        },
    );
    index.add_type(entry);

    let target_ty = TypeKind::reference("com.example.Mutable");
    let base_ty = Some(TypeKind::reference("com.example.Mutable"));
    let plan = plan_doublebrace_application(base_ty.as_ref(), &target_ty, &init, Some(&index))
        .expect("ミューテーションプランの構築に成功する");

    match plan {
        DoublebracePlan::Mutate(mutate) => {
            assert_eq!(mutate.base, PlanBase::ExistingInstance);
            assert_eq!(mutate.receiver, target_ty);
            assert_eq!(mutate.steps.len(), 2);

            match &mutate.steps[0] {
                MutationStep::FieldAssignment(update) => {
                    assert_eq!(update.name, "value");
                }
                other => panic!("フィールド代入が必要ですが {:?} が返されました", other),
            }

            match &mutate.steps[1] {
                MutationStep::MethodCall(call) => {
                    assert_eq!(call.name, "push");
                    assert_eq!(call.arguments.len(), 1);
                    assert_eq!(call.metadata.style, CallArgumentStyle::Whitespace);
                }
                other => panic!("メソッド呼び出しが必要ですが {:?} が返されました", other),
            }
        }
        other => panic!("ミューテーションプランが返るべきですが {:?}", other),
    }
}

#[test]
fn whitespace_arguments_with_unary_minus_are_normalized() {
    let span = span();
    let statements = vec![Statement::Expression {
        expr: Expression::Call {
            function: Box::new(Expression::Identifier("get".into(), span.clone())),
            args: vec![
                Argument::Positional(Expression::Call {
                    function: Box::new(Expression::Identifier("size".into(), span.clone())),
                    args: Vec::new(),
                    type_arguments: Vec::new(),
                    argument_metadata: CallArgumentMetadata::with_style(CallArgumentStyle::Comma),
                    call_kind: CallKind::Function,
                    span: span.clone(),
                }),
                Argument::Positional(Expression::Unary {
                    op: UnaryOp::Minus,
                    operand: Box::new(Expression::Literal(
                        Literal::Number("1".into()),
                        span.clone(),
                    )),
                    span: span.clone(),
                }),
            ],
            type_arguments: Vec::new(),
            argument_metadata: CallArgumentMetadata::with_style(CallArgumentStyle::Whitespace),
            call_kind: CallKind::Function,
            span: span.clone(),
        },
        span: span.clone(),
    }];

    let init = DoublebraceInit {
        base: Some(Box::new(Expression::Identifier(
            "items".into(),
            span.clone(),
        ))),
        receiver_hint: None,
        statements,
        span: span.clone(),
    };

    let mut index = SymbolIndex::new(Some(16));
    let mut entry = TypeEntry::new("com.example.Menu".into(), "com.example".into(), None);
    entry.add_instance_method(
        "get".into(),
        JavaMethodSignature {
            parameters: vec![JavaType::Primitive("int".into())],
            return_type: JavaType::Reference {
                name: "java.lang.Object".into(),
                generic_args: Vec::new(),
            },
        },
    );
    entry.add_instance_method(
        "size".into(),
        JavaMethodSignature {
            parameters: Vec::new(),
            return_type: JavaType::Primitive("int".into()),
        },
    );
    index.add_type(entry);

    let target_ty = TypeKind::reference("com.example.Menu");
    let base_ty = Some(TypeKind::reference("com.example.Menu"));

    let plan = plan_doublebrace_application(base_ty.as_ref(), &target_ty, &init, Some(&index))
        .expect("normalize whitespace arguments");

    match plan {
        DoublebracePlan::Mutate(mutate) => {
            assert_eq!(mutate.steps.len(), 1);
            match &mutate.steps[0] {
                MutationStep::MethodCall(call) => {
                    assert_eq!(call.metadata.style, CallArgumentStyle::Comma);
                    assert_eq!(call.arguments.len(), 1);
                    match &call.arguments[0] {
                        Argument::Positional(Expression::Binary { op, .. }) => {
                            assert_eq!(op, &BinaryOp::Subtract);
                        }
                        other => panic!("expected binary argument, got {:?}", other),
                    }
                }
                other => panic!("expected method call, got {:?}", other),
            }
        }
        other => panic!("expected mutate plan, got {:?}", other),
    }
}

#[test]
fn constructor_base_uses_synthesized_plan() {
    let span = span();
    let init = DoublebraceInit {
        base: Some(Box::new(Expression::Call {
            function: Box::new(Expression::Identifier("ArrayList".into(), span.clone())),
            args: Vec::new(),
            type_arguments: Vec::new(),
            argument_metadata: CallArgumentMetadata::with_style(CallArgumentStyle::Comma),
            call_kind: CallKind::Constructor {
                type_name: "ArrayList".into(),
                fqcn: Some("java.util.ArrayList".into()),
            },
            span: span.clone(),
        })),
        receiver_hint: None,
        statements: vec![Statement::Expression {
            expr: Expression::Call {
                function: Box::new(Expression::Identifier("add".into(), span.clone())),
                args: vec![Argument::Positional(Expression::Literal(
                    Literal::String("value".into()),
                    span.clone(),
                ))],
                type_arguments: Vec::new(),
                argument_metadata: CallArgumentMetadata::with_style(CallArgumentStyle::Whitespace),
                call_kind: CallKind::Function,
                span: span.clone(),
            },
            span: span.clone(),
        }],
        span: span.clone(),
    };

    let target_ty = TypeKind::reference("java.util.ArrayList");
    let base_ty = Some(TypeKind::reference("java.util.ArrayList"));

    let plan = plan_doublebrace_application(base_ty.as_ref(), &target_ty, &init, None)
        .expect("constructor base should yield mutate plan");

    match plan {
        DoublebracePlan::Mutate(mutate) => {
            assert_eq!(mutate.base, PlanBase::SynthesizedInstance);
        }
        other => panic!("expected mutate plan, received {:?}", other),
    }
}

#[test]
fn factory_base_with_registry_match_prefers_synthesized_instance() {
    let span = span();
    let init = DoublebraceInit {
        base: Some(Box::new(Expression::Call {
            function: Box::new(Expression::Identifier("factory".into(), span.clone())),
            args: Vec::new(),
            type_arguments: Vec::new(),
            argument_metadata: CallArgumentMetadata::with_style(CallArgumentStyle::Comma),
            call_kind: CallKind::Function,
            span: span.clone(),
        })),
        receiver_hint: None,
        statements: Vec::new(),
        span: span.clone(),
    };

    let target_ty = TypeKind::reference("java.util.ArrayList<java.lang.String>");
    let base_ty = Some(TypeKind::reference("java.util.List<java.lang.String>"));

    let plan = plan_doublebrace_application(base_ty.as_ref(), &target_ty, &init, None)
        .expect("factory base should still yield a mutate plan");

    match plan {
        DoublebracePlan::Mutate(mutate) => {
            assert_eq!(mutate.base, PlanBase::SynthesizedInstance);
        }
        other => panic!("expected mutate plan, received {:?}", other),
    }
}

#[test]
fn copy_plan_for_immutable_receiver() {
    // 不変クラスではコピー戦略が選択され、フィールド更新が収集されること。
    let span = span();
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
        statements: statements.clone(),
        span: span.clone(),
    };

    let mut index = SymbolIndex::new(Some(25));
    let mut entry = TypeEntry::new("com.example.Immutable".into(), "com.example".into(), None);
    entry.add_instance_method(
        "copy".into(),
        JavaMethodSignature {
            parameters: Vec::new(),
            return_type: JavaType::Reference {
                name: "com.example.Immutable".into(),
                generic_args: Vec::new(),
            },
        },
    );
    index.add_type(entry);

    let target_ty = TypeKind::reference("com.example.Immutable");
    let base_ty = Some(TypeKind::reference("com.example.Immutable"));

    let plan = plan_doublebrace_application(base_ty.as_ref(), &target_ty, &init, Some(&index))
        .expect("コピー計画の構築に成功する");

    match plan {
        DoublebracePlan::Copy(copy) => {
            assert_eq!(copy.source, CopySource::ExistingInstance);
            assert_eq!(copy.receiver, target_ty);
            assert_eq!(copy.updates.len(), 1);
            assert_eq!(copy.updates[0].name, "name");
        }
        other => panic!("コピー計画が返るべきですが {:?}", other),
    }
}

#[test]
fn copy_plan_requires_existing_base() {
    // コピー元が無い場合はエラーとして扱われること。
    let span = span();
    let statements = vec![Statement::Assignment {
        target: Expression::Identifier("name".into(), span.clone()),
        binding_pattern: None,
        value: Expression::Literal(Literal::String("Alice".into()), span.clone()),
        span: span.clone(),
    }];

    let init = DoublebraceInit {
        base: None,
        receiver_hint: None,
        statements: statements.clone(),
        span: span.clone(),
    };

    let mut index = SymbolIndex::new(Some(25));
    let mut entry = TypeEntry::new("com.example.Immutable".into(), "com.example".into(), None);
    entry.add_instance_method(
        "copy".into(),
        JavaMethodSignature {
            parameters: Vec::new(),
            return_type: JavaType::Reference {
                name: "com.example.Immutable".into(),
                generic_args: Vec::new(),
            },
        },
    );
    index.add_type(entry);

    let target_ty = TypeKind::reference("com.example.Immutable");

    let result = plan_doublebrace_application(None, &target_ty, &init, Some(&index));

    match result {
        Err(DoublebracePlanError::CopyUnavailable { reason, .. }) => {
            assert!(
                reason.contains("既存インスタンス"),
                "エラーメッセージがコピー元不足を指摘するべきですが: {reason}"
            );
        }
        other => panic!("エラーが返るべきですが {:?}", other),
    }
}

#[test]
fn planner_collects_plan_for_val_declaration() {
    let span = span();
    let doublebrace = DoublebraceInit {
        base: None,
        receiver_hint: None,
        statements: vec![Statement::Expression {
            expr: Expression::Call {
                function: Box::new(Expression::Identifier("add".into(), span.clone())),
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
        }],
        span: span.clone(),
    };

    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![Statement::ValDeclaration {
            name: "items".into(),
            binding: None,
            type_annotation: None,
            initializer: Expression::DoublebraceInit(doublebrace),
            modifiers: Modifiers::default(),
            origin: ValBindingOrigin::ExplicitKeyword,
            span: span.clone(),
        }],
        span: span.clone(),
    };

    let mut environment = TypeEnvironment::new();
    environment.define_monotype("items", TypeKind::reference("java.util.ArrayList"));

    let plans =
        plan_doublebrace_in_program(&program, &environment, None).expect("planning succeeds");
    assert!(
        plans.contains_key(&span_key(&span)),
        "doublebrace plan should be recorded"
    );
}

#[test]
fn fallback_planner_recovers_constructor_generics() {
    let span_menu = Span::new(0, 0, 0, 4);
    let span_copy = Span::new(1, 0, 1, 4);

    let constructor_call = Expression::Call {
        function: Box::new(Expression::Identifier(
            "ArrayList".into(),
            span_menu.clone(),
        )),
        args: Vec::new(),
        type_arguments: vec![TypeAnnotation::Simple("String".into())],
        argument_metadata: CallArgumentMetadata::with_style(CallArgumentStyle::Comma),
        call_kind: CallKind::Constructor {
            type_name: "ArrayList".into(),
            fqcn: Some("java.util.ArrayList".into()),
        },
        span: span_menu.clone(),
    };

    let add_statement = Statement::Expression {
        expr: Expression::Call {
            function: Box::new(Expression::Identifier("add".into(), span_menu.clone())),
            args: vec![Argument::Positional(Expression::Literal(
                Literal::String("季節のスープ".into()),
                span_menu.clone(),
            ))],
            type_arguments: Vec::new(),
            argument_metadata: CallArgumentMetadata::with_style(CallArgumentStyle::Whitespace),
            call_kind: CallKind::Function,
            span: span_menu.clone(),
        },
        span: span_menu.clone(),
    };

    let menu_doublebrace = DoublebraceInit {
        base: Some(Box::new(constructor_call)),
        receiver_hint: None,
        statements: vec![add_statement],
        span: span_menu.clone(),
    };

    let copy_doublebrace = DoublebraceInit {
        base: Some(Box::new(Expression::Identifier(
            "menu".into(),
            span_copy.clone(),
        ))),
        receiver_hint: None,
        statements: Vec::new(),
        span: span_copy.clone(),
    };

    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![
            Statement::ValDeclaration {
                name: "menu".into(),
                binding: None,
                type_annotation: None,
                initializer: Expression::DoublebraceInit(menu_doublebrace),
                modifiers: Modifiers::default(),
                origin: ValBindingOrigin::ExplicitKeyword,
                span: span_menu.clone(),
            },
            Statement::ValDeclaration {
                name: "copy".into(),
                binding: None,
                type_annotation: None,
                initializer: Expression::DoublebraceInit(copy_doublebrace),
                modifiers: Modifiers::default(),
                origin: ValBindingOrigin::ExplicitKeyword,
                span: span_copy.clone(),
            },
        ],
        span: Span::new(0, 0, 1, 4),
    };

    let environment = TypeEnvironment::new();
    let plans =
        plan_doublebrace_in_program(&program, &environment, None).expect("fallback planning");

    let menu_plan = plans
        .get(&span_key(&span_menu))
        .expect("menu doublebrace should be planned");
    match menu_plan {
        DoublebracePlan::Mutate(plan) => {
            assert_eq!(
                plan.receiver.describe(),
                "java.util.ArrayList<java.lang.String>",
                "constructor generics should be retained"
            );
            assert_eq!(
                plan.base,
                PlanBase::SynthesizedInstance,
                "constructor base should synthesize an instance"
            );
        }
        other => panic!("expected mutate plan for menu doublebrace, got {:?}", other),
    }

    let copy_plan = plans
        .get(&span_key(&span_copy))
        .expect("copy doublebrace should be planned");
    match copy_plan {
        DoublebracePlan::Mutate(plan) => {
            assert_eq!(
                plan.receiver.describe(),
                "java.util.ArrayList<java.lang.String>",
                "registered bindings should provide generic receiver info"
            );
            assert_eq!(
                plan.base,
                PlanBase::ExistingInstance,
                "identifier base should reuse existing instance"
            );
            assert!(
                plan.steps.is_empty(),
                "empty doublebrace should not record mutation steps"
            );
        }
        other => panic!("expected mutate plan for copy doublebrace, got {:?}", other),
    }
}
