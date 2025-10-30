//! 正規表現 `is` 演算子に対応する IR から Java コードへの生成結果を検証するテスト。

use super::*;
use jv_ast::RegexGuardStrategy;
use jv_pm::JavaTarget;

fn render_java(program: &IrProgram, target: JavaTarget) -> String {
    let config = JavaCodeGenConfig::for_target(target);
    let mut generator = JavaCodeGenerator::with_config(config.clone());
    let unit = generator
        .generate_compilation_unit(program)
        .expect("コード生成が成功すること");
    unit.to_source(&config)
}

fn regex_literal_match_program() -> IrProgram {
    let span = dummy_span();

    let boolean_return = PrimitiveReturnMetadata {
        reference: PrimitiveTypeReference {
            primitive: PrimitiveTypeName::Boolean,
            source: PrimitiveTypeSource::PrimitiveKeyword,
            raw_path: Vec::new(),
            span: span.clone(),
        },
    };
    let string_ty = string_type();
    let method = IrStatement::MethodDeclaration {
        name: "check".to_string(),
        java_name: None,
        type_parameters: vec![],
        parameters: vec![IrParameter {
            name: "text".to_string(),
            java_type: string_ty.clone(),
            modifiers: IrModifiers::default(),
            span: span.clone(),
        }],
        primitive_return: Some(boolean_return),
        return_type: JavaType::Primitive("boolean".to_string()),
        body: Some(IrExpression::Block {
            statements: vec![IrStatement::Return {
                value: Some(IrExpression::RegexMatch {
                    subject: Box::new(ir_identifier("text", &string_ty)),
                    pattern: Box::new(IrExpression::RegexPattern {
                        pattern: "\\d+".to_string(),
                        java_type: JavaType::pattern(),
                        span: span.clone(),
                    }),
                    guard_strategy: RegexGuardStrategy::None,
                    java_type: JavaType::Primitive("boolean".to_string()),
                    span: span.clone(),
                }),
                span: span.clone(),
            }],
            java_type: JavaType::Primitive("boolean".to_string()),
            span: span.clone(),
        }),
        modifiers: IrModifiers {
            visibility: IrVisibility::Private,
            ..IrModifiers::default()
        },
        throws: vec![],
        span: span.clone(),
    };

    let class = IrStatement::ClassDeclaration {
        name: "RegexSample".to_string(),
        type_parameters: vec![],
        superclass: None,
        interfaces: vec![],
        fields: vec![],
        methods: vec![method],
        nested_classes: vec![],
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        span: span.clone(),
    };

    IrProgram {
        package: Some("regex.sample".to_string()),
        imports: vec![],
        type_declarations: vec![class],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span,
    }
}

fn optional_subject_match_program() -> IrProgram {
    let span = dummy_span();
    let boolean_return = PrimitiveReturnMetadata {
        reference: PrimitiveTypeReference {
            primitive: PrimitiveTypeName::Boolean,
            source: PrimitiveTypeSource::PrimitiveKeyword,
            raw_path: Vec::new(),
            span: span.clone(),
        },
    };
    let char_seq_ty = JavaType::Reference {
        name: "java.lang.CharSequence".to_string(),
        generic_args: vec![],
    };
    let method = IrStatement::MethodDeclaration {
        name: "checkOptional".to_string(),
        java_name: None,
        type_parameters: vec![],
        parameters: vec![IrParameter {
            name: "text".to_string(),
            java_type: char_seq_ty.clone(),
            modifiers: IrModifiers::default(),
            span: span.clone(),
        }],
        primitive_return: Some(boolean_return),
        return_type: JavaType::Primitive("boolean".to_string()),
        body: Some(IrExpression::Block {
            statements: vec![IrStatement::Return {
                value: Some(IrExpression::RegexMatch {
                    subject: Box::new(ir_identifier("text", &char_seq_ty)),
                    pattern: Box::new(IrExpression::RegexPattern {
                        pattern: "[a-zA-Z]+".to_string(),
                        java_type: JavaType::pattern(),
                        span: span.clone(),
                    }),
                    guard_strategy: RegexGuardStrategy::CaptureAndGuard {
                        temp_name: Some("__guard".to_string()),
                    },
                    java_type: JavaType::Primitive("boolean".to_string()),
                    span: span.clone(),
                }),
                span: span.clone(),
            }],
            java_type: JavaType::Primitive("boolean".to_string()),
            span: span.clone(),
        }),
        modifiers: IrModifiers {
            visibility: IrVisibility::Private,
            ..IrModifiers::default()
        },
        throws: vec![],
        span: span.clone(),
    };

    let class = IrStatement::ClassDeclaration {
        name: "RegexGuards".to_string(),
        type_parameters: vec![],
        superclass: None,
        interfaces: vec![],
        fields: vec![],
        methods: vec![method],
        nested_classes: vec![],
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        span: span.clone(),
    };

    IrProgram {
        package: Some("regex.sample".to_string()),
        imports: vec![],
        type_declarations: vec![class],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span,
    }
}

#[test]
fn 正規表現リテラルに対するis判定がmatches呼び出しに展開される() {
    let program = regex_literal_match_program();
    let java25 = render_java(&program, JavaTarget::Java25);

    assert!(
        java25.contains("Pattern.compile(\"\\\\d+\")"),
        "正規表現リテラルの Pattern.compile(...) 呼び出しが生成される想定です:\n{java25}"
    );
    assert!(
        java25.contains(".matcher(text).matches()"),
        "正規表現リテラルは matcher(text).matches() に変換される想定です:\n{java25}"
    );
}

#[test]
fn nullableな左辺では自動ガード付きのmatches呼び出しを生成する() {
    let program = optional_subject_match_program();
    let java25 = render_java(&program, JavaTarget::Java25);

    assert!(
        java25.contains("instanceof java.lang.CharSequence"),
        "Optional 左辺では instanceof ガードが挿入される想定です:\n{java25}"
    );
    assert!(
        java25.contains(".matcher(__guard).matches()"),
        "ガード後に matcher(__guard).matches() 呼び出しが生成される想定です:\n{java25}"
    );
}
