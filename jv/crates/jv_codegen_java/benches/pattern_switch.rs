use criterion::{Criterion, black_box, criterion_group, criterion_main};
use jv_ast::{Literal, Span};
use jv_codegen_java::{JavaCodeGenConfig, JavaCodeGenerator, JavaTarget};
use jv_ir::{
    IrCaseLabel, IrDeconstructionComponent, IrDeconstructionPattern, IrExpression, IrModifiers,
    IrParameter, IrProgram, IrRecordComponent, IrStatement, IrSwitchCase, IrVisibility, JavaType,
};
use std::sync::Arc;
use std::time::{Duration, Instant};

fn dummy_span() -> Span {
    Span::dummy()
}

fn literal_switch_cases(arm_count: usize) -> Vec<IrSwitchCase> {
    let mut cases = Vec::with_capacity(arm_count + 1);

    for index in 0..arm_count {
        cases.push(IrSwitchCase {
            labels: vec![IrCaseLabel::Literal(Literal::Number(index.to_string()))],
            guard: None,
            body: IrExpression::Literal(Literal::String(format!("case_{index}")), dummy_span()),
            span: dummy_span(),
        });
    }

    cases.push(IrSwitchCase {
        labels: vec![IrCaseLabel::Default],
        guard: None,
        body: IrExpression::Literal(Literal::String("fallback".to_string()), dummy_span()),
        span: dummy_span(),
    });

    cases
}

fn sealed_switch_cases(levels: usize) -> Vec<IrSwitchCase> {
    let mut cases = Vec::with_capacity(levels + 1);

    for index in 0..levels {
        let payload_pattern = IrDeconstructionPattern {
            components: vec![
                IrDeconstructionComponent::Binding {
                    name: format!("payload_value_{index}"),
                },
                IrDeconstructionComponent::Binding {
                    name: format!("payload_label_{index}"),
                },
            ],
        };

        let variant_pattern = IrDeconstructionPattern {
            components: vec![
                IrDeconstructionComponent::Binding {
                    name: format!("head_{index}"),
                },
                IrDeconstructionComponent::Type {
                    type_name: "PatternPayload".to_string(),
                    pattern: Some(Box::new(payload_pattern)),
                },
            ],
        };

        cases.push(IrSwitchCase {
            labels: vec![IrCaseLabel::TypePattern {
                type_name: format!("PatternVariant{index}"),
                variable: format!("variant_{index}"),
                deconstruction: Some(variant_pattern),
            }],
            guard: None,
            body: IrExpression::Literal(Literal::String(format!("sealed_{index}")), dummy_span()),
            span: dummy_span(),
        });
    }

    cases.push(IrSwitchCase {
        labels: vec![IrCaseLabel::Default],
        guard: None,
        body: IrExpression::Literal(Literal::String("sealed_default".to_string()), dummy_span()),
        span: dummy_span(),
    });

    cases
}

#[allow(clippy::too_many_arguments)]
fn build_switch_program(
    class_name: &str,
    method_name: &str,
    subject_name: &str,
    subject_type: JavaType,
    result_type: JavaType,
    cases: Vec<IrSwitchCase>,
    strategy_description: Option<String>,
    mut extra_declarations: Vec<IrStatement>,
) -> IrProgram {
    let discriminant_type = subject_type.clone();
    let return_type = result_type.clone();

    let switch_expr = IrExpression::Switch {
        discriminant: Box::new(IrExpression::Identifier {
            name: subject_name.to_string(),
            java_type: discriminant_type,
            span: dummy_span(),
        }),
        cases,
        java_type: return_type.clone(),
        implicit_end: None,
        strategy_description,
        span: dummy_span(),
    };

    let return_statement = IrStatement::Return {
        value: Some(switch_expr),
        span: dummy_span(),
    };

    let method_body = IrExpression::Block {
        statements: vec![return_statement],
        java_type: return_type.clone(),
        span: dummy_span(),
    };

    let match_method = IrStatement::MethodDeclaration {
        name: method_name.to_string(),
        java_name: None,
        type_parameters: vec![],
        parameters: vec![IrParameter {
            name: subject_name.to_string(),
            java_type: subject_type,
            modifiers: IrModifiers::default(),
            span: dummy_span(),
        }],
        primitive_return: None,
        return_type,
        body: Some(method_body),
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            is_static: true,
            ..IrModifiers::default()
        },
        throws: vec![],
        span: dummy_span(),
    };

    let bench_class = IrStatement::ClassDeclaration {
        name: class_name.to_string(),
        type_parameters: vec![],
        superclass: None,
        interfaces: vec![],
        fields: vec![],
        methods: vec![match_method],
        nested_classes: vec![],
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        span: dummy_span(),
    };

    extra_declarations.push(bench_class);

    IrProgram {
        package: Some("benchmarks.patterns".to_string()),
        imports: vec![],
        type_declarations: extra_declarations,
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        logging: Default::default(),
        tuple_record_plans: Vec::new(),
        span: dummy_span(),
    }
}

fn literal_when_program(arm_count: usize) -> IrProgram {
    let cases = literal_switch_cases(arm_count);
    let strategy = Some(format!("scenario=literal arms={arm_count} default=true"));

    build_switch_program(
        "PatternSwitchBench",
        "matchValue",
        "value",
        JavaType::int(),
        JavaType::string(),
        cases,
        strategy,
        Vec::new(),
    )
}

fn sealed_hierarchy_program(levels: usize) -> IrProgram {
    let variant_names: Vec<String> = (0..levels)
        .map(|index| format!("PatternVariant{index}"))
        .collect();

    let node_interface_type = JavaType::Reference {
        name: "PatternNode".to_string(),
        generic_args: vec![],
    };

    let payload_type = JavaType::Reference {
        name: "PatternPayload".to_string(),
        generic_args: vec![],
    };

    let interface_declaration = IrStatement::InterfaceDeclaration {
        name: "PatternNode".to_string(),
        type_parameters: vec![],
        superinterfaces: vec![],
        methods: vec![],
        default_methods: vec![],
        fields: vec![],
        nested_types: vec![],
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            is_sealed: true,
            permitted_types: variant_names.clone(),
            ..IrModifiers::default()
        },
        span: dummy_span(),
    };

    let payload_record = IrStatement::RecordDeclaration {
        name: "PatternPayload".to_string(),
        type_parameters: vec![],
        components: vec![
            IrRecordComponent {
                name: "value".to_string(),
                java_type: JavaType::int(),
                span: dummy_span(),
            },
            IrRecordComponent {
                name: "label".to_string(),
                java_type: JavaType::string(),
                span: dummy_span(),
            },
        ],
        interfaces: vec![],
        methods: vec![],
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        span: dummy_span(),
    };

    let variant_declarations: Vec<IrStatement> = variant_names
        .iter()
        .map(|variant_name| IrStatement::RecordDeclaration {
            name: variant_name.clone(),
            type_parameters: vec![],
            components: vec![
                IrRecordComponent {
                    name: "head".to_string(),
                    java_type: JavaType::int(),
                    span: dummy_span(),
                },
                IrRecordComponent {
                    name: "payload".to_string(),
                    java_type: payload_type.clone(),
                    span: dummy_span(),
                },
            ],
            interfaces: vec![node_interface_type.clone()],
            methods: vec![],
            modifiers: IrModifiers {
                visibility: IrVisibility::Public,
                ..IrModifiers::default()
            },
            span: dummy_span(),
        })
        .collect();

    let mut extra_declarations = vec![interface_declaration, payload_record];
    extra_declarations.extend(variant_declarations);

    let cases = sealed_switch_cases(levels);
    let strategy = Some(format!(
        "scenario=sealed levels={levels} depth=2 default=true"
    ));

    build_switch_program(
        "PatternSealedBench",
        "matchNode",
        "node",
        node_interface_type,
        JavaType::string(),
        cases,
        strategy,
        extra_declarations,
    )
}

fn run_codegen(program: &IrProgram, target: JavaTarget) {
    let mut generator = JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(target));
    let unit = generator
        .generate_compilation_unit(program)
        .expect("pattern switch generation succeeds");
    black_box(unit);
}

fn verify_budget(program: &IrProgram, target: JavaTarget, label: &str, budget: Duration) {
    let start = Instant::now();
    run_codegen(program, target);
    let elapsed = start.elapsed();
    println!(
        "{label} single-run: {:.3}ms (budget {:.3}ms)",
        elapsed.as_secs_f64() * 1_000.0,
        budget.as_secs_f64() * 1_000.0
    );
    assert!(
        elapsed <= budget,
        "{label} exceeded budget: {:?} > {:?}",
        elapsed,
        budget
    );
}

fn target_prefix(target: JavaTarget) -> &'static str {
    match target {
        JavaTarget::Java21 => "java21",
        JavaTarget::Java25 => "java25",
    }
}

fn run_scenario(
    c: &mut Criterion,
    program: &Arc<IrProgram>,
    scenario_label: &str,
    budget: Duration,
) {
    for &target in &[JavaTarget::Java25, JavaTarget::Java21] {
        let label = format!("{}_{}", target_prefix(target), scenario_label);
        verify_budget(program.as_ref(), target, &label, budget);

        let program_for_target = Arc::clone(program);
        c.bench_function(&label, move |b| {
            b.iter(|| run_codegen(black_box(program_for_target.as_ref()), target));
        });
    }
}

fn bench_pattern_switch(c: &mut Criterion) {
    let literal_scenarios = [
        (100usize, Duration::from_millis(10)),
        (500usize, Duration::from_millis(25)),
        (1000usize, Duration::from_millis(50)),
        (5000usize, Duration::from_millis(250)),
    ];

    for &(arms, budget) in &literal_scenarios {
        let program = Arc::new(literal_when_program(arms));
        let scenario_label = format!("when_arms_{arms}");
        run_scenario(c, &program, &scenario_label, budget);
    }

    let sealed_program = Arc::new(sealed_hierarchy_program(10));
    run_scenario(
        c,
        &sealed_program,
        "sealed_depth10",
        Duration::from_millis(100),
    );
}

criterion_group!(pattern_switch, bench_pattern_switch);
criterion_main!(pattern_switch);
