use criterion::{Criterion, black_box, criterion_group, criterion_main};
use jv_ast::Span;
use jv_codegen_java::{JavaCodeGenConfig, JavaCodeGenerator, JavaTarget};
use jv_ir::{IrModifiers, IrProgram, IrStatement, IrVisibility, JavaType};

fn sealed_program() -> IrProgram {
    let sealed_modifiers = IrModifiers {
        visibility: IrVisibility::Public,
        is_sealed: true,
        permitted_types: vec!["Success".to_string(), "Failure".to_string()],
        ..IrModifiers::default()
    };

    let result_class = IrStatement::ClassDeclaration {
        name: "Result".to_string(),
        type_parameters: vec![],
        superclass: None,
        interfaces: vec![],
        fields: vec![],
        methods: vec![],
        nested_classes: vec![],
        modifiers: sealed_modifiers,
        span: Span::dummy(),
    };

    let success_class = IrStatement::ClassDeclaration {
        name: "Success".to_string(),
        type_parameters: vec![],
        superclass: Some(reference_type("Result")),
        interfaces: vec![],
        fields: vec![],
        methods: vec![],
        nested_classes: vec![],
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        span: Span::dummy(),
    };

    let failure_class = IrStatement::ClassDeclaration {
        name: "Failure".to_string(),
        type_parameters: vec![],
        superclass: Some(reference_type("Result")),
        interfaces: vec![],
        fields: vec![],
        methods: vec![],
        nested_classes: vec![],
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        span: Span::dummy(),
    };

    IrProgram {
        package: Some("com.example".to_string()),
        imports: vec![],
        type_declarations: vec![result_class, success_class, failure_class],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        logging: Default::default(),
        span: Span::dummy(),
    }
}

fn reference_type(name: &str) -> JavaType {
    JavaType::Reference {
        name: name.to_string(),
        generic_args: vec![],
    }
}

fn bench_target_matrix(c: &mut Criterion) {
    let program = sealed_program();

    c.bench_function("java25_sealed_generation", |b| {
        b.iter(|| {
            let mut generator =
                JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
            let unit = generator
                .generate_compilation_unit(black_box(&program))
                .expect("java25 generation succeeds");
            black_box(unit);
        });
    });

    c.bench_function("java21_sealed_generation", |b| {
        b.iter(|| {
            let mut generator =
                JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java21));
            let unit = generator
                .generate_compilation_unit(black_box(&program))
                .expect("java21 generation succeeds");
            black_box(unit);
        });
    });
}

criterion_group!(target_matrix, bench_target_matrix);
criterion_main!(target_matrix);
