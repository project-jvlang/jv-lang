use criterion::{black_box, criterion_group, criterion_main, Criterion};
use jv_ast::{Literal, Span};
use jv_codegen_java::{JavaCodeGenConfig, JavaCodeGenerator, JavaTarget};
use jv_ir::{
    IrCaseLabel, IrExpression, IrModifiers, IrParameter, IrProgram, IrStatement, IrSwitchCase,
    IrVisibility, JavaType,
};
use std::time::{Duration, Instant};

fn dummy_span() -> Span {
    Span::dummy()
}

fn make_switch_cases(arm_count: usize) -> Vec<IrSwitchCase> {
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

fn when_program(arm_count: usize) -> IrProgram {
    let java_string = JavaType::string();
    let java_int = JavaType::int();

    let cases = make_switch_cases(arm_count);

    let switch_expr = IrExpression::Switch {
        discriminant: Box::new(IrExpression::Identifier {
            name: "value".to_string(),
            java_type: java_int.clone(),
            span: dummy_span(),
        }),
        cases,
        java_type: java_string.clone(),
        implicit_end: None,
        strategy_description: Some(format!(
            "strategy=Switch arms={} guards=0 default=true exhaustive=false",
            arm_count
        )),
        span: dummy_span(),
    };

    let return_statement = IrStatement::Return {
        value: Some(switch_expr),
        span: dummy_span(),
    };

    let method_body = IrExpression::Block {
        statements: vec![return_statement],
        java_type: java_string.clone(),
        span: dummy_span(),
    };

    let match_method = IrStatement::MethodDeclaration {
        name: "matchValue".to_string(),
        parameters: vec![IrParameter {
            name: "value".to_string(),
            java_type: java_int,
            modifiers: IrModifiers::default(),
            span: dummy_span(),
        }],
        return_type: java_string,
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
        name: "PatternSwitchBench".to_string(),
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

    IrProgram {
        package: Some("benchmarks.patterns".to_string()),
        imports: vec![],
        type_declarations: vec![bench_class],
        span: dummy_span(),
    }
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

fn bench_pattern_switch(c: &mut Criterion) {
    let program_1000 = when_program(1000);
    let program_5000 = when_program(5000);

    verify_budget(
        &program_1000,
        JavaTarget::Java25,
        "java25_when_arms_1000",
        Duration::from_millis(50),
    );
    verify_budget(
        &program_5000,
        JavaTarget::Java25,
        "java25_when_arms_5000",
        Duration::from_millis(250),
    );
    verify_budget(
        &program_1000,
        JavaTarget::Java21,
        "java21_when_arms_1000",
        Duration::from_millis(50),
    );
    verify_budget(
        &program_5000,
        JavaTarget::Java21,
        "java21_when_arms_5000",
        Duration::from_millis(250),
    );

    c.bench_function("java25_when_arms_1000", |b| {
        b.iter(|| run_codegen(black_box(&program_1000), JavaTarget::Java25));
    });

    c.bench_function("java25_when_arms_5000", |b| {
        b.iter(|| run_codegen(black_box(&program_5000), JavaTarget::Java25));
    });

    c.bench_function("java21_when_arms_1000", |b| {
        b.iter(|| run_codegen(black_box(&program_1000), JavaTarget::Java21));
    });

    c.bench_function("java21_when_arms_5000", |b| {
        b.iter(|| run_codegen(black_box(&program_5000), JavaTarget::Java21));
    });
}

criterion_group!(pattern_switch, bench_pattern_switch);
criterion_main!(pattern_switch);
