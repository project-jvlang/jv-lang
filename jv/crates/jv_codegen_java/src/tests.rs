use super::*;
use insta::assert_snapshot;
use jv_ast::{
    BinaryOp, CallArgumentStyle, Literal, SequenceDelimiter, Span,
    types::{PrimitiveTypeName, PrimitiveTypeReference, PrimitiveTypeSource},
};
use jv_ir::TransformContext;
use jv_ir::transform::transform_program_with_context;
use jv_ir::{
    CharToStringConversion, DataFormat, IrCaseLabel, IrCommentKind, IrDeconstructionComponent,
    IrDeconstructionPattern, IrExpression, IrImplicitWhenEnd, IrModifiers, IrParameter, IrProgram,
    IrRecordComponent, IrSampleDeclaration, IrStatement, IrSwitchCase, IrTypeParameter,
    IrVisibility, JavaType, MethodOverload, PipelineShape, PrimitiveReturnMetadata,
    PrimitiveSpecializationHint, PrimitiveType, RawStringFlavor, SampleMode,
    SampleRecordDescriptor, SampleRecordField, SampleSourceKind, Schema, SequencePipeline,
    SequenceSource, SequenceTerminal, SequenceTerminalEvaluation, SequenceTerminalKind,
};
use jv_parser_frontend::ParserPipeline;
use jv_parser_rowan::frontend::RowanPipeline;
use serde_json::to_string_pretty;
use std::collections::{BTreeMap, BTreeSet};
use std::path::PathBuf;

fn dummy_span() -> Span {
    Span::dummy()
}

fn string_type() -> JavaType {
    JavaType::Reference {
        name: "String".to_string(),
        generic_args: vec![],
    }
}

fn int_type() -> JavaType {
    JavaType::Primitive("int".to_string())
}

fn reference_type(name: &str) -> JavaType {
    JavaType::Reference {
        name: name.to_string(),
        generic_args: vec![],
    }
}

fn list_type(element: JavaType) -> JavaType {
    JavaType::Reference {
        name: "java.util.List".to_string(),
        generic_args: vec![element],
    }
}

fn user_sample_type() -> JavaType {
    reference_type("UserSample")
}

fn user_sample_list_type() -> JavaType {
    list_type(user_sample_type())
}

fn sample_record_descriptor() -> SampleRecordDescriptor {
    SampleRecordDescriptor {
        name: "UserSample".to_string(),
        fields: vec![
            SampleRecordField {
                name: "id".to_string(),
                java_type: JavaType::Primitive("int".to_string()),
                is_optional: false,
            },
            SampleRecordField {
                name: "name".to_string(),
                java_type: string_type(),
                is_optional: false,
            },
            SampleRecordField {
                name: "email".to_string(),
                java_type: string_type(),
                is_optional: false,
            },
        ],
    }
}

fn sample_schema() -> Schema {
    let mut fields = BTreeMap::new();
    fields.insert("id".to_string(), Schema::Primitive(PrimitiveType::Integer));
    fields.insert("name".to_string(), Schema::Primitive(PrimitiveType::String));
    fields.insert(
        "email".to_string(),
        Schema::Primitive(PrimitiveType::String),
    );

    let mut required = BTreeSet::new();
    required.insert("id".to_string());
    required.insert("name".to_string());
    required.insert("email".to_string());

    Schema::Array {
        element_type: Box::new(Schema::Object { fields, required }),
    }
}

fn sample_embed_bytes() -> Vec<u8> {
    br#"[{"id":1,"name":"Alice","email":"alice@example.com"},{"id":2,"name":"Bob","email":"bob@example.com"}]"#
        .to_vec()
}

fn sample_declaration(records: Vec<SampleRecordDescriptor>) -> IrSampleDeclaration {
    sample_declaration_with(
        records,
        DataFormat::Json,
        "users.json",
        sample_embed_bytes(),
    )
}

fn sample_csv_bytes() -> Vec<u8> {
    b"id,name,email\n1,Alice,alice@example.com\n2,Bob,bob@example.com\n".to_vec()
}

fn sample_tsv_bytes() -> Vec<u8> {
    b"id\tname\temail\n1\tAlice\talice@example.com\n2\tBob\tbob@example.com\n".to_vec()
}

fn sample_declaration_with(
    records: Vec<SampleRecordDescriptor>,
    format: DataFormat,
    source: &str,
    embedded_data: Vec<u8>,
) -> IrSampleDeclaration {
    IrSampleDeclaration {
        variable_name: "users".to_string(),
        java_type: user_sample_list_type(),
        format,
        mode: SampleMode::Embed,
        source: source.to_string(),
        source_kind: SampleSourceKind::LocalFile,
        sha256: "deadbeef".to_string(),
        cache_path: None,
        limit_bytes: Some(1024),
        embedded_data: Some(embedded_data),
        schema: sample_schema(),
        records,
        root_record_name: Some("UserSample".to_string()),
        span: dummy_span(),
    }
}

fn sample_declaration_csv() -> IrSampleDeclaration {
    sample_declaration_with(
        vec![sample_record_descriptor()],
        DataFormat::Csv,
        "users.csv",
        sample_csv_bytes(),
    )
}

fn sample_declaration_tsv() -> IrSampleDeclaration {
    sample_declaration_with(
        vec![sample_record_descriptor()],
        DataFormat::Tsv,
        "users.tsv",
        sample_tsv_bytes(),
    )
}

fn parse_program(source: &str) -> IrProgram {
    let program = RowanPipeline::default()
        .parse(source)
        .expect("IR codegen fixture should parse")
        .into_program();
    let mut context = TransformContext::new();
    transform_program_with_context(program, &mut context).expect("program should lower to IR")
}

fn sample_sha256() -> String {
    "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef".to_string()
}

fn sample_load_declaration_with(
    format: DataFormat,
    source: &str,
    source_kind: SampleSourceKind,
    cache_path: Option<&str>,
    limit_bytes: Option<u64>,
) -> IrSampleDeclaration {
    IrSampleDeclaration {
        variable_name: "users".to_string(),
        java_type: user_sample_list_type(),
        format,
        mode: SampleMode::Load,
        source: source.to_string(),
        source_kind,
        sha256: sample_sha256(),
        cache_path: cache_path.map(PathBuf::from),
        limit_bytes,
        embedded_data: None,
        schema: sample_schema(),
        records: vec![sample_record_descriptor()],
        root_record_name: Some("UserSample".to_string()),
        span: dummy_span(),
    }
}

fn user_sample_constructor(name: &str, age: i32, email: &str) -> IrExpression {
    IrExpression::ObjectCreation {
        class_name: "UserSample".to_string(),
        generic_args: vec![],
        args: vec![
            IrExpression::Literal(Literal::String(name.to_string()), None, dummy_span()),
            IrExpression::Literal(Literal::Number(age.to_string()), None, dummy_span()),
            IrExpression::Literal(Literal::String(email.to_string()), None, dummy_span()),
        ],
        java_type: user_sample_type(),
        span: dummy_span(),
    }
}

fn embed_raw_json_field() -> IrStatement {
    IrStatement::FieldDeclaration {
        name: "RAW_JSON".to_string(),
        java_type: string_type(),
        initializer: Some(IrExpression::Literal(
            Literal::String("{\"users\": 2}".to_string()),
            None,
            dummy_span(),
        )),
        modifiers: IrModifiers {
            visibility: IrVisibility::Private,
            is_static: true,
            is_final: true,
            ..IrModifiers::default()
        },
        span: dummy_span(),
    }
}

fn embed_users_field() -> IrStatement {
    IrStatement::FieldDeclaration {
        name: "USERS".to_string(),
        java_type: user_sample_list_type(),
        initializer: Some(IrExpression::MethodCall {
            receiver: Some(Box::new(IrExpression::Identifier {
                name: "java.util.List".to_string(),
                java_type: reference_type("java.util.List"),
                span: dummy_span(),
            })),
            method_name: "of".to_string(),
            java_name: None,
            resolved_target: None,
            args: vec![
                user_sample_constructor("Alice", 28, "alice@example.com"),
                user_sample_constructor("Bob", 31, "bob@example.com"),
            ],
            argument_style: CallArgumentStyle::Comma,
            java_type: user_sample_list_type(),
            span: dummy_span(),
        }),
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            is_static: true,
            is_final: true,
            ..IrModifiers::default()
        },
        span: dummy_span(),
    }
}

fn embed_data_class() -> IrStatement {
    IrStatement::ClassDeclaration {
        name: "UserSampleData".to_string(),
        type_parameters: vec![],
        superclass: None,
        interfaces: vec![],
        fields: vec![embed_raw_json_field(), embed_users_field()],
        methods: vec![],
        nested_classes: vec![],
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        span: dummy_span(),
    }
}

fn sample_embed_program() -> IrProgram {
    IrProgram {
        package: Some("sample.embed".to_string()),
        imports: vec![],
        type_declarations: vec![user_sample_record(), embed_data_class()],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span: dummy_span(),
    }
}

fn load_path_of_call() -> IrExpression {
    IrExpression::MethodCall {
        receiver: Some(Box::new(IrExpression::Identifier {
            name: "java.nio.file.Path".to_string(),
            java_type: reference_type("java.nio.file.Path"),
            span: dummy_span(),
        })),
        method_name: "of".to_string(),
        java_name: None,
        resolved_target: None,
        args: vec![IrExpression::Literal(
            Literal::String("/data/users.json".to_string()),
            None,
            dummy_span(),
        )],
        argument_style: CallArgumentStyle::Comma,
        java_type: reference_type("java.nio.file.Path"),
        span: dummy_span(),
    }
}

fn load_read_string_call() -> IrExpression {
    IrExpression::MethodCall {
        receiver: Some(Box::new(IrExpression::Identifier {
            name: "java.nio.file.Files".to_string(),
            java_type: reference_type("java.nio.file.Files"),
            span: dummy_span(),
        })),
        method_name: "readString".to_string(),
        java_name: None,
        resolved_target: None,
        args: vec![load_path_of_call()],
        argument_style: CallArgumentStyle::Comma,
        java_type: string_type(),
        span: dummy_span(),
    }
}

fn load_users_method() -> IrStatement {
    IrStatement::MethodDeclaration {
        name: "loadUsers".to_string(),
        java_name: None,
        type_parameters: vec![],
        parameters: vec![],
        primitive_return: None,
        return_type: string_type(),
        body: Some(IrExpression::Block {
            statements: vec![IrStatement::Return {
                value: Some(load_read_string_call()),
                span: dummy_span(),
            }],
            java_type: string_type(),
            span: dummy_span(),
        }),
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            is_static: true,
            ..IrModifiers::default()
        },
        throws: vec!["java.io.IOException".to_string()],
        span: dummy_span(),
    }
}

fn load_helper_class() -> IrStatement {
    IrStatement::ClassDeclaration {
        name: "UserSampleLoader".to_string(),
        type_parameters: vec![],
        superclass: None,
        interfaces: vec![],
        fields: vec![],
        methods: vec![load_users_method()],
        nested_classes: vec![],
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        span: dummy_span(),
    }
}

fn sample_load_program() -> IrProgram {
    IrProgram {
        package: Some("sample.load".to_string()),
        imports: vec![],
        type_declarations: vec![load_helper_class()],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span: dummy_span(),
    }
}

fn user_sample_record() -> IrStatement {
    IrStatement::RecordDeclaration {
        name: "UserSample".to_string(),
        type_parameters: vec![],
        components: vec![
            IrRecordComponent {
                name: "id".to_string(),
                java_type: int_type(),
                span: dummy_span(),
            },
            IrRecordComponent {
                name: "name".to_string(),
                java_type: string_type(),
                span: dummy_span(),
            },
            IrRecordComponent {
                name: "email".to_string(),
                java_type: string_type(),
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
    }
}

fn simple_method() -> IrStatement {
    let return_expression = IrExpression::FieldAccess {
        receiver: Box::new(IrExpression::This {
            java_type: JavaType::Reference {
                name: "Sample".to_string(),
                generic_args: vec![],
            },
            span: dummy_span(),
        }),
        field_name: "greeting".to_string(),
        java_type: string_type(),
        span: dummy_span(),
        is_record_component: false,
    };
    let return_stmt = IrStatement::Return {
        value: Some(return_expression),
        span: dummy_span(),
    };
    let body = IrExpression::Block {
        statements: vec![return_stmt],
        java_type: string_type(),
        span: dummy_span(),
    };
    IrStatement::MethodDeclaration {
        name: "sayHello".to_string(),
        java_name: None,
        type_parameters: vec![],
        parameters: vec![],
        primitive_return: None,
        return_type: string_type(),
        body: Some(body),
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        throws: vec![],
        span: dummy_span(),
    }
}

fn simple_class() -> IrStatement {
    let field = IrStatement::FieldDeclaration {
        name: "greeting".to_string(),
        java_type: string_type(),
        initializer: Some(IrExpression::Literal(
            Literal::String("Hello".to_string()),
            None,
            dummy_span(),
        )),
        modifiers: IrModifiers {
            visibility: IrVisibility::Private,
            is_final: true,
            ..IrModifiers::default()
        },
        span: dummy_span(),
    };

    IrStatement::ClassDeclaration {
        name: "Sample".to_string(),
        type_parameters: vec![],
        superclass: None,
        interfaces: vec![],
        fields: vec![field],
        methods: vec![simple_method()],
        nested_classes: vec![],
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        span: dummy_span(),
    }
}

fn snapshot_program() -> IrProgram {
    let mut block_statements = Vec::new();

    block_statements.push(IrStatement::VariableDeclaration {
        name: "message".to_string(),
        java_type: string_type(),
        initializer: Some(IrExpression::Literal(
            Literal::String("Hello".to_string()),
            None,
            dummy_span(),
        )),
        is_final: true,
        modifiers: IrModifiers::default(),
        span: dummy_span(),
    });

    let comparison = IrExpression::Binary {
        left: Box::new(IrExpression::Identifier {
            name: "input".to_string(),
            java_type: string_type(),
            span: dummy_span(),
        }),
        op: jv_ast::BinaryOp::NotEqual,
        right: Box::new(IrExpression::Literal(
            Literal::String("".to_string()),
            None,
            dummy_span(),
        )),
        java_type: JavaType::Primitive("boolean".to_string()),
        span: dummy_span(),
    };

    let then_block = IrStatement::Block {
        statements: vec![IrStatement::Return {
            value: Some(IrExpression::Identifier {
                name: "input".to_string(),
                java_type: string_type(),
                span: dummy_span(),
            }),
            span: dummy_span(),
        }],
        span: dummy_span(),
    };

    let else_block = IrStatement::Block {
        statements: vec![IrStatement::Return {
            value: Some(IrExpression::Identifier {
                name: "message".to_string(),
                java_type: string_type(),
                span: dummy_span(),
            }),
            span: dummy_span(),
        }],
        span: dummy_span(),
    };

    block_statements.push(IrStatement::If {
        condition: comparison,
        then_stmt: Box::new(then_block),
        else_stmt: Some(Box::new(else_block)),
        span: dummy_span(),
    });

    block_statements.push(IrStatement::Return {
        value: Some(IrExpression::Identifier {
            name: "message".to_string(),
            java_type: string_type(),
            span: dummy_span(),
        }),
        span: dummy_span(),
    });

    let method_body = IrExpression::Block {
        statements: block_statements,
        java_type: string_type(),
        span: dummy_span(),
    };

    let method = IrStatement::MethodDeclaration {
        name: "greet".to_string(),
        java_name: None,
        type_parameters: vec![],
        parameters: vec![IrParameter {
            name: "input".to_string(),
            java_type: string_type(),
            modifiers: IrModifiers::default(),
            span: dummy_span(),
        }],
        primitive_return: None,
        return_type: string_type(),
        body: Some(method_body),
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            is_static: true,
            ..IrModifiers::default()
        },
        throws: vec![],
        span: dummy_span(),
    };

    let class = IrStatement::ClassDeclaration {
        name: "Greeter".to_string(),
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
        span: dummy_span(),
    };

    IrProgram {
        package: Some("example.greeter".to_string()),
        imports: vec![],
        type_declarations: vec![class],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span: dummy_span(),
    }
}

#[test]
fn class_generation_renders_members() {
    let mut generator = JavaCodeGenerator::new();
    let class_code = generator
        .generate_class(&simple_class())
        .expect("class generation should succeed");

    assert!(class_code.contains("public class Sample"));
    assert!(class_code.contains("private final String greeting = \"Hello\";"));
    assert!(class_code.contains("public String sayHello"));
    assert!(class_code.contains("return this.greeting"));
}

#[test]
fn method_generation_renders_generic_type_parameters() {
    let mut generator = JavaCodeGenerator::new();
    let span = dummy_span();
    let iterable_type = JavaType::Reference {
        name: "Iterable".to_string(),
        generic_args: vec![JavaType::Reference {
            name: "T".to_string(),
            generic_args: vec![],
        }],
    };
    let stream_type = JavaType::Reference {
        name: "Stream".to_string(),
        generic_args: vec![JavaType::Reference {
            name: "T".to_string(),
            generic_args: vec![],
        }],
    };

    let method = IrStatement::MethodDeclaration {
        name: "toStream".to_string(),
        java_name: None,
        type_parameters: vec![IrTypeParameter::new("T", span.clone())],
        parameters: vec![IrParameter {
            name: "source".to_string(),
            java_type: iterable_type,
            modifiers: IrModifiers::default(),
            span: span.clone(),
        }],
        primitive_return: None,
        return_type: stream_type.clone(),
        body: Some(IrExpression::Block {
            statements: vec![IrStatement::Return {
                value: Some(IrExpression::Literal(Literal::Null, None, span.clone())),
                span: span.clone(),
            }],
            java_type: stream_type,
            span: span.clone(),
        }),
        modifiers: IrModifiers {
            visibility: IrVisibility::Private,
            is_static: true,
            ..IrModifiers::default()
        },
        throws: vec![],
        span,
    };

    let code = generator
        .generate_method(&method)
        .expect("method generation should succeed");

    let first_line = code
        .lines()
        .next()
        .expect("method should have a signature line");
    assert_eq!(
        first_line,
        "private static <T> Stream<T> toStream(Iterable<T> source) {"
    );
}

#[test]
fn primitive_return_methods_receive_specialized_names() {
    let mut generator = JavaCodeGenerator::new();
    let span = dummy_span();
    let primitive_return = PrimitiveReturnMetadata {
        reference: PrimitiveTypeReference {
            primitive: PrimitiveTypeName::Int,
            source: PrimitiveTypeSource::PrimitiveKeyword,
            raw_path: Vec::new(),
            span: span.clone(),
        },
    };

    let method = IrStatement::MethodDeclaration {
        name: "sum".to_string(),
        java_name: None,
        type_parameters: Vec::new(),
        parameters: Vec::new(),
        primitive_return: Some(primitive_return),
        return_type: JavaType::Primitive("int".to_string()),
        body: Some(IrExpression::Literal(
            Literal::Number("0".to_string()),
            None,
            span.clone(),
        )),
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            is_static: true,
            ..IrModifiers::default()
        },
        throws: Vec::new(),
        span,
    };

    let code = generator
        .generate_method(&method)
        .expect("primitive return method should generate");

    let first_line = code.lines().next().expect("method should have signature");
    assert!(first_line.contains("sum$IntVersion"));
}

#[test]
fn method_generation_renders_where_clause_bounds() {
    let mut generator = JavaCodeGenerator::new();
    let span = dummy_span();
    let mut type_param = IrTypeParameter::new("T", span.clone());
    type_param.bounds.push(JavaType::Reference {
        name: "Number".to_string(),
        generic_args: vec![],
    });

    let type_ref = JavaType::Reference {
        name: "T".to_string(),
        generic_args: vec![],
    };

    let method = IrStatement::MethodDeclaration {
        name: "identity".to_string(),
        java_name: None,
        type_parameters: vec![type_param],
        parameters: vec![IrParameter {
            name: "value".to_string(),
            java_type: type_ref.clone(),
            modifiers: IrModifiers::default(),
            span: span.clone(),
        }],
        primitive_return: None,
        return_type: type_ref.clone(),
        body: Some(IrExpression::Block {
            statements: vec![IrStatement::Return {
                value: Some(IrExpression::Identifier {
                    name: "value".to_string(),
                    java_type: type_ref.clone(),
                    span: span.clone(),
                }),
                span: span.clone(),
            }],
            java_type: type_ref,
            span: span.clone(),
        }),
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        throws: vec![],
        span,
    };

    let code = generator
        .generate_method(&method)
        .expect("where clause bounds should emit extends clause");

    let first_line = code
        .lines()
        .next()
        .expect("method should have a signature line");
    assert!(
        first_line.contains("<T extends Number>"),
        "expected extends clause in signature, got: {first_line}"
    );
    assert!(
        first_line.contains("T identity(T value)"),
        "expected method signature with generic return/parameter, got: {first_line}"
    );
}

#[test]
fn record_extension_method_is_emitted_as_instance_method() {
    let mut generator = JavaCodeGenerator::new();
    let span = dummy_span();
    let record_type = JavaType::Reference {
        name: "StreamWrapper".to_string(),
        generic_args: vec![],
    };

    let record_declaration = IrStatement::RecordDeclaration {
        name: "StreamWrapper".to_string(),
        type_parameters: vec![],
        components: vec![IrRecordComponent {
            name: "value".to_string(),
            java_type: string_type(),
            span: span.clone(),
        }],
        interfaces: vec![],
        methods: vec![],
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        span: span.clone(),
    };

    let mut extension_modifiers = IrModifiers::default();
    extension_modifiers.visibility = IrVisibility::Public;
    extension_modifiers.is_static = true;

    let extension_method = IrStatement::MethodDeclaration {
        name: "identity".to_string(),
        java_name: None,
        type_parameters: vec![],
        parameters: vec![IrParameter {
            name: "receiver".to_string(),
            java_type: record_type.clone(),
            modifiers: IrModifiers::default(),
            span: span.clone(),
        }],
        primitive_return: None,
        return_type: record_type.clone(),
        body: Some(IrExpression::Block {
            statements: vec![IrStatement::Return {
                value: Some(IrExpression::Identifier {
                    name: "receiver".to_string(),
                    java_type: record_type.clone(),
                    span: span.clone(),
                }),
                span: span.clone(),
            }],
            java_type: record_type.clone(),
            span: span.clone(),
        }),
        modifiers: extension_modifiers,
        throws: vec![],
        span: span.clone(),
    };

    let program = IrProgram {
        package: Some("demo.sequence".to_string()),
        imports: vec![],
        type_declarations: vec![record_declaration, extension_method],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span,
    };

    let unit = generator
        .generate_compilation_unit(&program)
        .expect("code generation should succeed");

    assert_eq!(
        unit.type_declarations.len(),
        1,
        "record should own the extension instance methods"
    );

    let record_java = &unit.type_declarations[0];
    assert!(
        record_java.contains("public record StreamWrapper(String value)"),
        "record declaration should be generated"
    );
    assert!(
        record_java.contains("public StreamWrapper identity()"),
        "extension must become an instance method"
    );
    assert!(
        record_java.contains("return this;"),
        "receiver references should be rewritten to `this`"
    );
    assert!(
        !record_java.contains("static StreamWrapper identity"),
        "static modifier must be removed from instance method"
    );
    assert!(
        !record_java.contains("StreamWrapper identity(StreamWrapper receiver)"),
        "receiver parameter must be removed from instance method signature"
    );
}

#[test]
fn top_level_records_drop_private_visibility() {
    let mut generator = JavaCodeGenerator::new();
    let span = dummy_span();
    let mut private_modifiers = IrModifiers::default();
    private_modifiers.visibility = IrVisibility::Private;

    let record_declaration = IrStatement::RecordDeclaration {
        name: "Event".to_string(),
        type_parameters: vec![],
        components: vec![IrRecordComponent {
            name: "label".to_string(),
            java_type: string_type(),
            span: span.clone(),
        }],
        interfaces: vec![],
        methods: vec![],
        modifiers: private_modifiers,
        span: span.clone(),
    };

    let program = IrProgram {
        package: None,
        imports: vec![],
        type_declarations: vec![record_declaration],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span,
    };

    let unit = generator
        .generate_compilation_unit(&program)
        .expect("code generation should succeed");

    assert_eq!(unit.type_declarations.len(), 1);
    let record_java = &unit.type_declarations[0];
    assert!(
        record_java.contains("record Event"),
        "record name should be preserved: {record_java}"
    );
    assert!(
        !record_java.contains("private record"),
        "top-level record should not emit private modifier: {record_java}"
    );
}

#[test]
fn record_field_access_is_lowered_to_accessor_calls() {
    let mut generator = JavaCodeGenerator::new();
    let span = dummy_span();

    let record_declaration = IrStatement::RecordDeclaration {
        name: "Module".to_string(),
        type_parameters: vec![],
        components: vec![
            IrRecordComponent {
                name: "name".to_string(),
                java_type: string_type(),
                span: span.clone(),
            },
            IrRecordComponent {
                name: "done".to_string(),
                java_type: JavaType::Primitive("boolean".to_string()),
                span: span.clone(),
            },
        ],
        interfaces: vec![],
        methods: vec![],
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        span: span.clone(),
    };

    let module_type = JavaType::Reference {
        name: "Module".to_string(),
        generic_args: vec![],
    };

    let module_param = |name: &str| IrParameter {
        name: name.to_string(),
        java_type: module_type.clone(),
        modifiers: IrModifiers::default(),
        span: span.clone(),
    };

    let accessor_block = |field: &str, ty: JavaType| IrExpression::Block {
        statements: vec![IrStatement::Return {
            value: Some(IrExpression::FieldAccess {
                receiver: Box::new(IrExpression::Identifier {
                    name: "module".to_string(),
                    java_type: module_type.clone(),
                    span: span.clone(),
                }),
                field_name: field.to_string(),
                java_type: ty.clone(),
                span: span.clone(),
                is_record_component: true,
            }),
            span: span.clone(),
        }],
        java_type: ty,
        span: span.clone(),
    };

    let name_method = IrStatement::MethodDeclaration {
        name: "moduleName".to_string(),
        java_name: None,
        type_parameters: vec![],
        parameters: vec![module_param("module")],
        primitive_return: None,
        return_type: string_type(),
        body: Some(accessor_block("name", string_type())),
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            is_static: true,
            ..IrModifiers::default()
        },
        throws: vec![],
        span: span.clone(),
    };

    let done_method = IrStatement::MethodDeclaration {
        name: "isModuleComplete".to_string(),
        java_name: None,
        type_parameters: vec![],
        parameters: vec![module_param("module")],
        primitive_return: None,
        return_type: JavaType::Primitive("boolean".to_string()),
        body: Some(accessor_block(
            "done",
            JavaType::Primitive("boolean".to_string()),
        )),
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            is_static: true,
            ..IrModifiers::default()
        },
        throws: vec![],
        span: span.clone(),
    };

    let renderer_class = IrStatement::ClassDeclaration {
        name: "ModuleRenderer".to_string(),
        type_parameters: vec![],
        superclass: None,
        interfaces: vec![],
        fields: vec![],
        methods: vec![name_method, done_method],
        nested_classes: vec![],
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        span: span.clone(),
    };

    let program = IrProgram {
        package: Some("demo.records".to_string()),
        imports: vec![],
        type_declarations: vec![record_declaration, renderer_class],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span,
    };

    let unit = generator
        .generate_compilation_unit(&program)
        .expect("code generation should succeed");

    assert_eq!(unit.type_declarations.len(), 2);
    let renderer_java = &unit.type_declarations[1];

    assert!(
        renderer_java.contains("return module.name();"),
        "record component access should call accessor: {renderer_java}"
    );
    assert!(
        renderer_java.contains("return module.done();"),
        "boolean component access should call accessor: {renderer_java}"
    );
    assert!(
        !renderer_java.contains("module.name;"),
        "record fields must not be accessed directly: {renderer_java}"
    );
}

#[test]
fn class_extension_method_is_emitted_as_instance_method() {
    let mut generator = JavaCodeGenerator::new();
    let span = dummy_span();
    let class_type = JavaType::Reference {
        name: "Greeter".to_string(),
        generic_args: vec![],
    };

    let class_declaration = IrStatement::ClassDeclaration {
        name: "Greeter".to_string(),
        type_parameters: vec![],
        superclass: None,
        interfaces: vec![],
        fields: vec![],
        methods: vec![],
        nested_classes: vec![],
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        span: span.clone(),
    };

    let mut extension_modifiers = IrModifiers::default();
    extension_modifiers.visibility = IrVisibility::Public;
    extension_modifiers.is_static = true;

    let extension_method = IrStatement::MethodDeclaration {
        name: "self".to_string(),
        java_name: None,
        type_parameters: vec![],
        parameters: vec![IrParameter {
            name: "receiver".to_string(),
            java_type: class_type.clone(),
            modifiers: IrModifiers::default(),
            span: span.clone(),
        }],
        primitive_return: None,
        return_type: class_type.clone(),
        body: Some(IrExpression::Block {
            statements: vec![IrStatement::Return {
                value: Some(IrExpression::Identifier {
                    name: "receiver".to_string(),
                    java_type: class_type.clone(),
                    span: span.clone(),
                }),
                span: span.clone(),
            }],
            java_type: class_type.clone(),
            span: span.clone(),
        }),
        modifiers: extension_modifiers,
        throws: vec![],
        span: span.clone(),
    };

    let program = IrProgram {
        package: Some("demo.greeter".to_string()),
        imports: vec![],
        type_declarations: vec![class_declaration, extension_method],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span,
    };

    let unit = generator
        .generate_compilation_unit(&program)
        .expect("code generation should succeed");

    assert_eq!(
        unit.type_declarations.len(),
        1,
        "class should own the extension instance methods"
    );

    let class_java = &unit.type_declarations[0];
    assert!(
        class_java.contains("public class Greeter"),
        "class declaration should be generated"
    );
    assert!(
        class_java.contains("public Greeter self()"),
        "extension must become an instance method on the class"
    );
    assert!(
        class_java.contains("return this;"),
        "receiver references should be rewritten to `this`"
    );
    assert!(
        !class_java.contains("static Greeter self"),
        "static modifier must be removed from class-backed instance method"
    );
    assert!(
        !class_java.contains("Greeter self(Greeter receiver)"),
        "receiver parameter must be removed from class-backed instance method signature"
    );
}

#[test]
fn primitive_specialized_pipeline_renders_character_casts() {
    let mut generator = JavaCodeGenerator::new();
    let span = dummy_span();

    let number_type = reference_type("java.lang.Number");
    let list_of_numbers = list_type(number_type.clone());

    let mut pipeline = SequencePipeline {
        source: SequenceSource::Collection {
            expr: Box::new(IrExpression::Identifier {
                name: "values".to_string(),
                java_type: list_of_numbers.clone(),
                span: span.clone(),
            }),
            element_hint: None,
        },
        stages: Vec::new(),
        terminal: Some(SequenceTerminal {
            kind: SequenceTerminalKind::Sum,
            evaluation: SequenceTerminalEvaluation::Aggregator,
            requires_non_empty_source: false,
            specialization_hint: Some(PrimitiveSpecializationHint {
                type_param: "T".to_string(),
                canonical: PrimitiveTypeName::Int,
                aliases: vec![PrimitiveTypeName::Char, PrimitiveTypeName::Short],
                span: span.clone(),
            }),
            canonical_adapter: None,
            span: span.clone(),
        }),
        lazy: false,
        span: span.clone(),
        shape: PipelineShape::default(),
        source_element_type: None,
        stage_element_types: Vec::new(),
    };
    pipeline.recompute_shape();
    pipeline.apply_specialization_hint();

    let return_type = JavaType::int();

    let method_body = IrExpression::Block {
        statements: vec![IrStatement::Return {
            value: Some(IrExpression::SequencePipeline {
                pipeline,
                java_type: return_type.clone(),
                span: span.clone(),
            }),
            span: span.clone(),
        }],
        java_type: return_type.clone(),
        span: span.clone(),
    };

    let mut method_modifiers = IrModifiers::default();
    method_modifiers.visibility = IrVisibility::Public;
    method_modifiers.is_static = true;

    let method = IrStatement::MethodDeclaration {
        name: "sumCharsAsInt".to_string(),
        java_name: None,
        type_parameters: vec![],
        parameters: vec![IrParameter {
            name: "values".to_string(),
            java_type: list_of_numbers.clone(),
            modifiers: IrModifiers::default(),
            span: span.clone(),
        }],
        primitive_return: Some(PrimitiveReturnMetadata {
            reference: PrimitiveTypeReference {
                primitive: PrimitiveTypeName::Int,
                source: PrimitiveTypeSource::PrimitiveKeyword,
                raw_path: Vec::new(),
                span: span.clone(),
            },
        }),
        return_type: return_type.clone(),
        body: Some(method_body),
        modifiers: method_modifiers,
        throws: vec![],
        span: span.clone(),
    };

    let mut class_modifiers = IrModifiers::default();
    class_modifiers.visibility = IrVisibility::Public;
    class_modifiers.is_final = true;

    let class = IrStatement::ClassDeclaration {
        name: "PrimitiveAdapters".to_string(),
        type_parameters: vec![],
        superclass: None,
        interfaces: vec![],
        fields: vec![],
        methods: vec![method],
        nested_classes: vec![],
        modifiers: class_modifiers,
        span: span.clone(),
    };

    let program = IrProgram {
        package: Some("demo.sequence".to_string()),
        imports: vec![],
        type_declarations: vec![class],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span,
    };

    let unit = generator
        .generate_compilation_unit(&program)
        .expect("primitive specialized pipeline should render");

    assert_eq!(
        unit.type_declarations.len(),
        1,
        "expected single class output"
    );

    let class_java = &unit.type_declarations[0];
    assert!(
        class_java.contains(".mapToInt("),
        "specialized sum should utilize mapToInt"
    );
    assert!(
        class_java.contains("(int) ((java.lang.Character) __jvIntFamilyValue).charValue()"),
        "Character aliases must be cast to int\n\nGenerated output:\n{class_java}"
    );
    assert!(
        class_java.contains("instanceof java.lang.Character"),
        "adapter should guard Character instances"
    );
    assert!(
        class_java.contains("values).stream()"),
        "sequence source should stream the input collection"
    );
    assert!(
        class_java.contains("((java.lang.Number) __jvIntFamilyValue).intValue()"),
        "non-alias branch should fall back to Number::intValue\n\nGenerated output:\n{class_java}"
    );
    assert!(
        class_java.contains("sumCharsAsInt$IntVersion"),
        "primitive return metadata should trigger Int specialization suffix\n\nGenerated output:\n{class_java}"
    );
}

#[test]
fn analyze_mutable_captures_detects_assignments_in_lambda() {
    let mut generator = JavaCodeGenerator::new();

    let accumulator_decl = IrStatement::VariableDeclaration {
        name: "accumulator".to_string(),
        java_type: JavaType::Reference {
            name: "R".to_string(),
            generic_args: vec![],
        },
        initializer: Some(IrExpression::Identifier {
            name: "initial".to_string(),
            java_type: JavaType::Reference {
                name: "R".to_string(),
                generic_args: vec![],
            },
            span: dummy_span(),
        }),
        is_final: false,
        modifiers: IrModifiers::default(),
        span: dummy_span(),
    };

    let assignment = IrExpression::Assignment {
        target: Box::new(IrExpression::Identifier {
            name: "accumulator".to_string(),
            java_type: JavaType::Reference {
                name: "R".to_string(),
                generic_args: vec![],
            },
            span: dummy_span(),
        }),
        value: Box::new(IrExpression::Identifier {
            name: "value".to_string(),
            java_type: JavaType::Reference {
                name: "T".to_string(),
                generic_args: vec![],
            },
            span: dummy_span(),
        }),
        java_type: JavaType::Reference {
            name: "R".to_string(),
            generic_args: vec![],
        },
        span: dummy_span(),
    };

    let lambda = IrExpression::Lambda {
        functional_interface: "java.util.function.Consumer".to_string(),
        param_names: vec!["value".to_string()],
        param_types: vec![JavaType::Reference {
            name: "T".to_string(),
            generic_args: vec![],
        }],
        body: Box::new(IrExpression::Block {
            statements: vec![IrStatement::Expression {
                expr: assignment,
                span: dummy_span(),
            }],
            span: dummy_span(),
            java_type: JavaType::Void,
        }),
        java_type: JavaType::Functional {
            interface_name: "java.util.function.Consumer".to_string(),
            param_types: vec![JavaType::Reference {
                name: "T".to_string(),
                generic_args: vec![],
            }],
            return_type: Box::new(JavaType::Void),
        },
        span: dummy_span(),
    };

    let for_each_call = IrExpression::MethodCall {
        receiver: Some(Box::new(IrExpression::Identifier {
            name: "receiver".to_string(),
            java_type: JavaType::Reference {
                name: "java.util.stream.Stream".to_string(),
                generic_args: vec![],
            },
            span: dummy_span(),
        })),
        method_name: "forEach".to_string(),
        java_name: Some("forEach".to_string()),
        resolved_target: None,
        args: vec![lambda],
        argument_style: CallArgumentStyle::Comma,
        java_type: JavaType::Void,
        span: dummy_span(),
    };

    let method_body = IrExpression::Block {
        statements: vec![
            accumulator_decl,
            IrStatement::Expression {
                expr: for_each_call,
                span: dummy_span(),
            },
            IrStatement::Return {
                value: Some(IrExpression::Identifier {
                    name: "accumulator".to_string(),
                    java_type: JavaType::Reference {
                        name: "R".to_string(),
                        generic_args: vec![],
                    },
                    span: dummy_span(),
                }),
                span: dummy_span(),
            },
        ],
        span: dummy_span(),
        java_type: JavaType::Reference {
            name: "R".to_string(),
            generic_args: vec![],
        },
    };

    let captures = generator.analyze_mutable_captures(&method_body);
    assert!(captures.contains("accumulator"));
}

#[test]
fn external_extension_method_remains_static_utility() {
    let mut generator = JavaCodeGenerator::new();
    let span = dummy_span();
    let list_type = JavaType::Reference {
        name: "java.util.List".to_string(),
        generic_args: vec![],
    };

    let mut modifiers = IrModifiers::default();
    modifiers.visibility = IrVisibility::Public;
    modifiers.is_static = true;

    let method = IrStatement::MethodDeclaration {
        name: "size".to_string(),
        java_name: None,
        type_parameters: vec![],
        parameters: vec![IrParameter {
            name: "receiver".to_string(),
            java_type: list_type.clone(),
            modifiers: IrModifiers::default(),
            span: span.clone(),
        }],
        primitive_return: None,
        return_type: JavaType::Primitive("int".to_string()),
        body: Some(IrExpression::Block {
            statements: vec![IrStatement::Return {
                value: Some(IrExpression::Literal(
                    Literal::Number("0".to_string()),
                    None,
                    span.clone(),
                )),
                span: span.clone(),
            }],
            java_type: JavaType::Primitive("int".to_string()),
            span: span.clone(),
        }),
        modifiers,
        throws: vec![],
        span: span.clone(),
    };

    let program = IrProgram {
        package: Some("demo.collections".to_string()),
        imports: vec![],
        type_declarations: vec![method],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span,
    };

    let unit = generator
        .generate_compilation_unit(&program)
        .expect("code generation should succeed");

    assert_eq!(
        unit.type_declarations.len(),
        1,
        "script utility class should be generated for external extensions"
    );

    let utility_java = &unit.type_declarations[0];
    assert!(
        utility_java.contains("public final class GeneratedMain"),
        "script wrapper class should be emitted"
    );
    assert!(
        utility_java.contains("public static int size(java.util.List receiver)"),
        "external extension must remain a static utility method with receiver parameter"
    );
    assert!(
        !utility_java.contains("public int size()"),
        "external extension must not be converted to an instance method"
    );
    assert!(
        utility_java.contains("return 0;"),
        "method body should remain intact when kept static"
    );
}

#[test]
fn expression_generation_handles_binary_arithmetic() {
    let mut generator = JavaCodeGenerator::new();
    let expression = IrExpression::Binary {
        left: Box::new(IrExpression::Identifier {
            name: "a".to_string(),
            java_type: int_type(),
            span: dummy_span(),
        }),
        op: jv_ast::BinaryOp::Add,
        right: Box::new(IrExpression::Identifier {
            name: "b".to_string(),
            java_type: int_type(),
            span: dummy_span(),
        }),
        java_type: int_type(),
        span: dummy_span(),
    };

    let generated = generator
        .generate_expression(&expression)
        .expect("expression generation should succeed");

    assert_eq!(generated, "a + b");
}

#[test]
fn regex_pattern_expression_renders_pattern_compile() {
    let mut generator = JavaCodeGenerator::new();
    let expression = IrExpression::RegexPattern {
        pattern: "^[a-z]+$".to_string(),
        java_type: JavaType::pattern(),
        span: dummy_span(),
    };

    let rendered = generator
        .generate_expression(&expression)
        .expect("regex expression should render");

    assert_eq!(rendered, "Pattern.compile(\"^[a-z]+$\")");
}

#[test]
fn whitespace_array_uses_list_of_for_java25_target() {
    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));

    let expression = IrExpression::ArrayCreation {
        element_type: int_type(),
        dimensions: vec![],
        initializer: Some(vec![
            IrExpression::Literal(Literal::Number("1".to_string()), None, dummy_span()),
            IrExpression::Literal(Literal::Number("2".to_string()), None, dummy_span()),
            IrExpression::Literal(Literal::Number("3".to_string()), None, dummy_span()),
        ]),
        delimiter: SequenceDelimiter::Whitespace,
        span: dummy_span(),
    };

    let generated = generator
        .generate_expression(&expression)
        .expect("whitespace array should render");

    assert_eq!(generated, "List.of(1, 2, 3)");
}

#[test]
fn whitespace_array_uses_java21_fallback_for_legacy_target() {
    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java21));

    let expression = IrExpression::ArrayCreation {
        element_type: int_type(),
        dimensions: vec![],
        initializer: Some(vec![
            IrExpression::Literal(Literal::Number("1".to_string()), None, dummy_span()),
            IrExpression::Literal(Literal::Number("2".to_string()), None, dummy_span()),
            IrExpression::Literal(Literal::Number("3".to_string()), None, dummy_span()),
        ]),
        delimiter: SequenceDelimiter::Whitespace,
        span: dummy_span(),
    };

    let generated = generator
        .generate_expression(&expression)
        .expect("whitespace array should render with fallback");

    assert_eq!(generated, "Arrays.asList(1, 2, 3).stream().toList()");
}

#[test]
fn whitespace_call_arguments_render_multiline_for_readability() {
    let mut generator = JavaCodeGenerator::new();
    let expression = IrExpression::MethodCall {
        receiver: None,
        method_name: "plot".to_string(),
        java_name: None,
        resolved_target: None,
        args: vec![
            IrExpression::Literal(Literal::Number("1".to_string()), None, dummy_span()),
            IrExpression::Literal(Literal::Number("2".to_string()), None, dummy_span()),
            IrExpression::Literal(Literal::Number("3".to_string()), None, dummy_span()),
        ],
        argument_style: CallArgumentStyle::Whitespace,
        java_type: JavaType::void(),
        span: dummy_span(),
    };

    let generated = generator
        .generate_expression(&expression)
        .expect("whitespace call should render");

    assert_eq!(generated, "plot(\n    1,\n    2,\n    3\n)");
}

#[test]
fn compilation_unit_collects_type_declarations() {
    let program = IrProgram {
        package: Some("com.example".to_string()),
        imports: vec![],
        type_declarations: vec![simple_class()],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span: dummy_span(),
    };

    let unit = generate_java_code(&program).expect("compilation unit generation");
    let source = unit.to_source(&JavaCodeGenConfig::default());

    assert!(source.contains("package com.example;"));
    assert!(source.contains("public class Sample"));
    assert!(source.contains("sayHello"));
}

#[test]
fn script_statements_are_wrapped_in_generated_main() {
    let greeting_decl = IrStatement::VariableDeclaration {
        name: "greeting".to_string(),
        java_type: JavaType::string(),
        initializer: Some(IrExpression::Literal(
            Literal::String("Hello, jv!".to_string()),
            None,
            dummy_span(),
        )),
        is_final: true,
        modifiers: IrModifiers::default(),
        span: dummy_span(),
    };

    let println_call = IrExpression::MethodCall {
        receiver: Some(Box::new(IrExpression::FieldAccess {
            receiver: Box::new(IrExpression::Identifier {
                name: "System".to_string(),
                java_type: JavaType::Reference {
                    name: "System".to_string(),
                    generic_args: vec![],
                },
                span: dummy_span(),
            }),
            field_name: "out".to_string(),
            java_type: JavaType::Reference {
                name: "PrintStream".to_string(),
                generic_args: vec![],
            },
            span: dummy_span(),
            is_record_component: false,
        })),
        method_name: "println".to_string(),
        java_name: None,
        resolved_target: None,
        args: vec![IrExpression::Identifier {
            name: "greeting".to_string(),
            java_type: JavaType::string(),
            span: dummy_span(),
        }],
        argument_style: CallArgumentStyle::Comma,
        java_type: JavaType::void(),
        span: dummy_span(),
    };

    let program = IrProgram {
        package: None,
        imports: vec![],
        type_declarations: vec![
            greeting_decl,
            IrStatement::Expression {
                expr: println_call,
                span: dummy_span(),
            },
        ],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span: dummy_span(),
    };

    let unit = generate_java_code(&program).expect("script lowering");
    let source = unit.to_source(&JavaCodeGenConfig::default());

    assert!(source.contains("public final class GeneratedMain"));
    assert!(source.contains("public static void main(String[] args) throws Exception"));
    assert!(source.contains("final String greeting = \"Hello, jv!\";"));
    assert!(source.contains("System.out.println(greeting);"));
}

#[test]
fn script_regex_val_is_hoisted_to_static_field() {
    let regex_initializer = IrExpression::RegexPattern {
        pattern: "^[a-z]+$".to_string(),
        java_type: JavaType::pattern(),
        span: dummy_span(),
    };

    let regex_decl = IrStatement::VariableDeclaration {
        name: "usernamePattern".to_string(),
        java_type: JavaType::pattern(),
        initializer: Some(regex_initializer),
        is_final: true,
        modifiers: IrModifiers::default(),
        span: dummy_span(),
    };

    let program = IrProgram {
        package: None,
        imports: vec![],
        type_declarations: vec![regex_decl],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span: dummy_span(),
    };

    let unit = generate_java_code(&program).expect("script regex lowering");
    let source = unit.to_source(&JavaCodeGenConfig::default());

    assert!(
        source.contains("static final java.util.regex.Pattern usernamePattern"),
        "regex pattern should be hoisted to a static field: {source}"
    );
    assert!(
        source.contains("Pattern.compile(\"^[a-z]+$\")"),
        "field initializer should compile pattern"
    );
    let field_index = source
        .find("static final java.util.regex.Pattern usernamePattern")
        .expect("hoisted field present");
    let main_index = source
        .find("void main")
        .expect("generated main should exist for scripts without entry method");
    assert!(
        field_index < main_index,
        "hoisted field must appear before main method"
    );
    assert!(
        source.contains("import java.util.regex.Pattern;"),
        "Pattern import should be added"
    );
}

#[test]
fn class_regex_field_is_emitted_as_static_final() {
    let field = IrStatement::FieldDeclaration {
        name: "EMAIL_PATTERN".to_string(),
        java_type: JavaType::pattern(),
        initializer: Some(IrExpression::RegexPattern {
            pattern: "^.+@.+$".to_string(),
            java_type: JavaType::pattern(),
            span: dummy_span(),
        }),
        modifiers: IrModifiers {
            visibility: IrVisibility::Private,
            is_final: true,
            ..IrModifiers::default()
        },
        span: dummy_span(),
    };

    let class = IrStatement::ClassDeclaration {
        name: "EmailValidator".to_string(),
        type_parameters: vec![],
        superclass: None,
        interfaces: vec![],
        fields: vec![field],
        methods: vec![],
        nested_classes: vec![],
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        span: dummy_span(),
    };

    let program = IrProgram {
        package: Some("example.validation".to_string()),
        imports: vec![],
        type_declarations: vec![class],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span: dummy_span(),
    };

    let unit = generate_java_code(&program).expect("class regex lowering");
    let source = unit.to_source(&JavaCodeGenConfig::default());

    assert!(
        source.contains("static final java.util.regex.Pattern EMAIL_PATTERN"),
        "class field should become static final pattern"
    );
    assert!(
        source.contains("Pattern.compile(\"^.+@.+$\")"),
        "class field initializer should compile pattern"
    );
}

#[test]
fn method_overload_generation_wraps_body() {
    let mut generator = JavaCodeGenerator::new();
    let overload = MethodOverload {
        name: "sum".to_string(),
        parameters: vec![IrParameter {
            name: "a".to_string(),
            java_type: int_type(),
            modifiers: IrModifiers::default(),
            span: dummy_span(),
        }],
        return_type: int_type(),
        body: IrExpression::Identifier {
            name: "a".to_string(),
            java_type: int_type(),
            span: dummy_span(),
        },
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        span: dummy_span(),
    };

    let overloads = generator
        .generate_method_overloads(&[overload])
        .expect("overload generation should succeed");

    assert_eq!(overloads.len(), 1);
    assert!(overloads[0].contains("public int sum"));
    assert!(overloads[0].contains("return a;"));
}

#[test]
fn snapshot_ir_program() {
    let program = snapshot_program();
    let json = to_string_pretty(&program).expect("serialise IR program");
    assert_snapshot!(
        json.as_str(),
        @r###"{
  "package": "example.greeter",
  "imports": [],
  "type_declarations": [
    {
      "ClassDeclaration": {
        "name": "Greeter",
        "type_parameters": [],
        "superclass": null,
        "interfaces": [],
        "fields": [],
        "methods": [
          {
            "MethodDeclaration": {
              "name": "greet",
              "java_name": null,
              "type_parameters": [],
              "parameters": [
                {
                  "name": "input",
                  "java_type": {
                    "Reference": {
                      "name": "String",
                      "generic_args": []
                    }
                  },
                  "modifiers": {
                    "visibility": "Package",
                    "is_static": false,
                    "is_final": false,
                    "is_abstract": false,
                    "is_synchronized": false,
                    "is_native": false,
                    "is_strictfp": false,
                    "annotations": [],
                    "is_sealed": false,
                    "permitted_types": []
                  },
                  "span": {
                    "start_line": 0,
                    "start_column": 0,
                    "end_line": 0,
                    "end_column": 0
                  }
                }
              ],
              "primitive_return": null,
              "return_type": {
                "Reference": {
                  "name": "String",
                  "generic_args": []
                }
              },
              "body": {
                "Block": {
                  "statements": [
                    {
                      "VariableDeclaration": {
                        "name": "message",
                        "java_type": {
                          "Reference": {
                            "name": "String",
                            "generic_args": []
                          }
                        },
                        "initializer": {
                          "Literal": [
                            {
                              "String": "Hello"
                            },
                            null,
                            {
                              "start_line": 0,
                              "start_column": 0,
                              "end_line": 0,
                              "end_column": 0
                            }
                          ]
                        },
                        "is_final": true,
                        "modifiers": {
                          "visibility": "Package",
                          "is_static": false,
                          "is_final": false,
                          "is_abstract": false,
                          "is_synchronized": false,
                          "is_native": false,
                          "is_strictfp": false,
                          "annotations": [],
                          "is_sealed": false,
                          "permitted_types": []
                        },
                        "span": {
                          "start_line": 0,
                          "start_column": 0,
                          "end_line": 0,
                          "end_column": 0
                        }
                      }
                    },
                    {
                      "If": {
                        "condition": {
                          "Binary": {
                            "left": {
                              "Identifier": {
                                "name": "input",
                                "java_type": {
                                  "Reference": {
                                    "name": "String",
                                    "generic_args": []
                                  }
                                },
                                "span": {
                                  "start_line": 0,
                                  "start_column": 0,
                                  "end_line": 0,
                                  "end_column": 0
                                }
                              }
                            },
                            "op": "NotEqual",
                            "right": {
                              "Literal": [
                                {
                                  "String": ""
                                },
                                null,
                                {
                                  "start_line": 0,
                                  "start_column": 0,
                                  "end_line": 0,
                                  "end_column": 0
                                }
                              ]
                            },
                            "java_type": {
                              "Primitive": "boolean"
                            },
                            "span": {
                              "start_line": 0,
                              "start_column": 0,
                              "end_line": 0,
                              "end_column": 0
                            }
                          }
                        },
                        "then_stmt": {
                          "Block": {
                            "statements": [
                              {
                                "Return": {
                                  "value": {
                                    "Identifier": {
                                      "name": "input",
                                      "java_type": {
                                        "Reference": {
                                          "name": "String",
                                          "generic_args": []
                                        }
                                      },
                                      "span": {
                                        "start_line": 0,
                                        "start_column": 0,
                                        "end_line": 0,
                                        "end_column": 0
                                      }
                                    }
                                  },
                                  "span": {
                                    "start_line": 0,
                                    "start_column": 0,
                                    "end_line": 0,
                                    "end_column": 0
                                  }
                                }
                              }
                            ],
                            "span": {
                              "start_line": 0,
                              "start_column": 0,
                              "end_line": 0,
                              "end_column": 0
                            }
                          }
                        },
                        "else_stmt": {
                          "Block": {
                            "statements": [
                              {
                                "Return": {
                                  "value": {
                                    "Identifier": {
                                      "name": "message",
                                      "java_type": {
                                        "Reference": {
                                          "name": "String",
                                          "generic_args": []
                                        }
                                      },
                                      "span": {
                                        "start_line": 0,
                                        "start_column": 0,
                                        "end_line": 0,
                                        "end_column": 0
                                      }
                                    }
                                  },
                                  "span": {
                                    "start_line": 0,
                                    "start_column": 0,
                                    "end_line": 0,
                                    "end_column": 0
                                  }
                                }
                              }
                            ],
                            "span": {
                              "start_line": 0,
                              "start_column": 0,
                              "end_line": 0,
                              "end_column": 0
                            }
                          }
                        },
                        "span": {
                          "start_line": 0,
                          "start_column": 0,
                          "end_line": 0,
                          "end_column": 0
                        }
                      }
                    },
                    {
                      "Return": {
                        "value": {
                          "Identifier": {
                            "name": "message",
                            "java_type": {
                              "Reference": {
                                "name": "String",
                                "generic_args": []
                              }
                            },
                            "span": {
                              "start_line": 0,
                              "start_column": 0,
                              "end_line": 0,
                              "end_column": 0
                            }
                          }
                        },
                        "span": {
                          "start_line": 0,
                          "start_column": 0,
                          "end_line": 0,
                          "end_column": 0
                        }
                      }
                    }
                  ],
                  "java_type": {
                    "Reference": {
                      "name": "String",
                      "generic_args": []
                    }
                  },
                  "span": {
                    "start_line": 0,
                    "start_column": 0,
                    "end_line": 0,
                    "end_column": 0
                  }
                }
              },
              "modifiers": {
                "visibility": "Public",
                "is_static": true,
                "is_final": false,
                "is_abstract": false,
                "is_synchronized": false,
                "is_native": false,
                "is_strictfp": false,
                "annotations": [],
                "is_sealed": false,
                "permitted_types": []
              },
              "throws": [],
              "span": {
                "start_line": 0,
                "start_column": 0,
                "end_line": 0,
                "end_column": 0
              }
            }
          }
        ],
        "nested_classes": [],
        "modifiers": {
          "visibility": "Public",
          "is_static": false,
          "is_final": false,
          "is_abstract": false,
          "is_synchronized": false,
          "is_native": false,
          "is_strictfp": false,
          "annotations": [],
          "is_sealed": false,
          "permitted_types": []
        },
        "span": {
          "start_line": 0,
          "start_column": 0,
          "end_line": 0,
          "end_column": 0
        }
      }
    }
  ],
  "generic_metadata": {},
  "conversion_metadata": [],
  "span": {
    "start_line": 0,
    "start_column": 0,
    "end_line": 0,
    "end_column": 0
  }
}

"###
    );
}

#[test]
fn snapshot_java_output() {
    let program = snapshot_program();
    let source = generate_java_source(&program).expect("java generation");
    assert_snapshot!(
        source.as_str(),
        @r###"package example.greeter;

public class Greeter {
    public static String greet(String input) {
        final String message = "Hello";
        if (input != "") {
            {
                return input;
            }
        } else {
            {
                return message;
            }
        }
        return message;
    }
    
}

"###
    );
}

#[test]
fn sample_record_generation_emits_expected_record() {
    let program = IrProgram {
        package: Some("sample.users".to_string()),
        imports: vec![],
        type_declarations: vec![user_sample_record()],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span: dummy_span(),
    };

    let unit = generate_java_code(&program).expect("generate record for @Sample schema");
    let source = unit.to_source(&JavaCodeGenConfig::default());

    assert!(
        source.contains("public record UserSample(int id, String name, String email)"),
        "record declaration should include inferred fields: {source}"
    );
}

#[test]
fn sample_declaration_generates_records_from_descriptors() {
    let declaration = sample_declaration(vec![sample_record_descriptor()]);
    let program = IrProgram {
        package: Some("sample.generated".to_string()),
        imports: vec![],
        type_declarations: vec![IrStatement::SampleDeclaration(declaration)],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span: dummy_span(),
    };

    let unit = generate_java_code(&program).expect("generate records from sample declaration");
    assert_eq!(
        unit.type_declarations.len(),
        2,
        "record and helper class should be emitted"
    );

    let source = unit.to_source(&JavaCodeGenConfig::default());
    assert!(
        source.contains("public record UserSample(int id, String name, String email)"),
        "generated source should contain inferred record: {source}"
    );
}

#[test]
fn inline_json_sample_generates_payload_records() {
    let source = r#"
        val payload = {
            "user": { "name": "Alice", "age": 30 },
            "tags": ["admin", "core"]
        }
    "#;

    let ir = parse_program(source);

    let unit = generate_java_code(&ir).expect("generate Java for inline JSON payload");
    let source = unit.to_source(&JavaCodeGenConfig::default());

    assert!(
        source.contains("record PayloadSample("),
        "root record should be generated for inline JSON payload: {source}"
    );
    assert!(
        source.contains("record PayloadUserSample("),
        "nested user record should be generated for inline JSON payload: {source}"
    );
}

#[test]
fn embed_mode_sample_declaration_handles_csv() {
    let declaration = sample_declaration_csv();
    let program = IrProgram {
        package: Some("sample.embed".to_string()),
        imports: vec![],
        type_declarations: vec![IrStatement::SampleDeclaration(declaration)],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span: dummy_span(),
    };

    let unit =
        generate_java_code(&program).expect("generate embed helper from CSV sample declaration");
    let source = unit.to_source(&JavaCodeGenConfig::default());

    assert!(
        source.contains("private static final String RAW_CSV"),
        "embedded CSV constant should be generated: {source}"
    );
    assert!(
        source.contains("public static final java.util.List<UserSample> USERS = java.util.List.of"),
        "CSV data should initialise the record list via List.of: {source}"
    );
    assert!(
        source.contains("new UserSample(1, \"Alice\", \"alice@example.com\")"),
        "CSV rows should be materialised as UserSample records: {source}"
    );
}

#[test]
fn embed_mode_sample_declaration_handles_tsv() {
    let declaration = sample_declaration_tsv();
    let program = IrProgram {
        package: Some("sample.embed".to_string()),
        imports: vec![],
        type_declarations: vec![IrStatement::SampleDeclaration(declaration)],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span: dummy_span(),
    };

    let unit =
        generate_java_code(&program).expect("generate embed helper from TSV sample declaration");
    let source = unit.to_source(&JavaCodeGenConfig::default());

    assert!(
        source.contains("private static final String RAW_TSV"),
        "embedded TSV constant should be generated: {source}"
    );
    assert!(
        source.contains("public static final java.util.List<UserSample> USERS = java.util.List.of"),
        "TSV data should initialise the record list via List.of: {source}"
    );
    assert!(
        source.contains("new UserSample(2, \"Bob\", \"bob@example.com\")"),
        "TSV rows should be materialised as UserSample records: {source}"
    );
}

#[test]
fn embed_mode_sample_declaration_produces_helper_class() {
    let declaration = sample_declaration(vec![sample_record_descriptor()]);
    let program = IrProgram {
        package: Some("sample.embed".to_string()),
        imports: vec![],
        type_declarations: vec![IrStatement::SampleDeclaration(declaration)],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span: dummy_span(),
    };

    let unit = generate_java_code(&program).expect("generate embed helper from sample declaration");
    assert!(
        unit.type_declarations
            .iter()
            .any(|decl| decl.contains("class UserSampleData")),
        "helper class should be generated alongside records"
    );

    let source = unit.to_source(&JavaCodeGenConfig::default());
    assert!(
        source.contains("private static final String RAW_JSON"),
        "embedded raw JSON constant should exist: {source}"
    );
    assert!(
        source.contains("public static final java.util.List<UserSample> USERS = java.util.List.of"),
        "embedded data list should be initialised using List.of: {source}"
    );
    assert!(
        source.contains("new UserSample(1, \"Alice\", \"alice@example.com\")"),
        "embedded list should contain deserialised record instances: {source}"
    );
}

#[test]
fn embed_mode_generates_inline_data_class() {
    let program = sample_embed_program();
    let unit = generate_java_code(&program).expect("generate embed-mode helpers");
    let source = unit.to_source(&JavaCodeGenConfig::default());

    assert_eq!(
        unit.type_declarations.len(),
        2,
        "record + data helper expected"
    );
    assert!(
        unit.type_declarations
            .iter()
            .any(|decl| decl.contains("class UserSampleData")),
        "embed helper class should be present"
    );
    assert!(
        source.contains("private static final String RAW_JSON = \"{\\\"users\\\": 2}\";"),
        "embedded JSON should be inlined: {source}"
    );
    assert!(
        source.contains(
            "public static final java.util.List<UserSample> USERS = java.util.List.of(new UserSample(\"Alice\", 28, \"alice@example.com\"), new UserSample(\"Bob\", 31, \"bob@example.com\"));"
        ),
        "embedded record instances should be initialised: {source}"
    );
}

#[test]
fn embed_mode_java_source_snapshot() {
    let program = sample_embed_program();
    let source = generate_java_source(&program).expect("render embed-mode source");

    assert_snapshot!(
        source.as_str(),
        @r###"package sample.embed;

public record UserSample(int id, String name, String email) {}

public class UserSampleData {
    private static final String RAW_JSON = "{\"users\": 2}";
    public static final java.util.List<UserSample> USERS = java.util.List.of(new UserSample("Alice", 28, "alice@example.com"), new UserSample("Bob", 31, "bob@example.com"));
}
"###
    );
}

#[test]
fn load_mode_generates_filesystem_loader() {
    let program = sample_load_program();
    let unit = generate_java_code(&program).expect("generate load-mode helpers");
    let source = unit.to_source(&JavaCodeGenConfig::default());

    assert_eq!(
        unit.type_declarations.len(),
        1,
        "only loader helper expected"
    );
    assert!(
        source.contains("public static String loadUsers() throws java.io.IOException {"),
        "load mode method signature should include IOException: {source}"
    );
    assert!(
        source.contains(
            "return java.nio.file.Files.readString(java.nio.file.Path.of(\"/data/users.json\"));"
        ),
        "load mode should delegate to Files.readString: {source}"
    );
}

#[test]
fn load_mode_java_source_snapshot() {
    let program = sample_load_program();
    let source = generate_java_source(&program).expect("render load-mode source");

    assert_snapshot!(
        source.as_str(),
        @r###"package sample.load;

public class UserSampleLoader {
    public static String loadUsers() throws java.io.IOException {
        return java.nio.file.Files.readString(java.nio.file.Path.of("/data/users.json"));
    }
    
}
"###
    );
}

#[test]
fn numeric_variable_declarations_preserve_literal_suffixes() {
    let mut generator = JavaCodeGenerator::new();
    let span = dummy_span();

    let cases = [
        (
            "totalLong",
            JavaType::Primitive("long".to_string()),
            "0L",
            "long totalLong = 0L;",
        ),
        (
            "totalFloat",
            JavaType::Primitive("float".to_string()),
            "0.0f",
            "float totalFloat = 0.0f;",
        ),
        (
            "totalDouble",
            JavaType::Primitive("double".to_string()),
            "0.0",
            "double totalDouble = 0.0;",
        ),
    ];

    for (name, java_type, literal, expected) in cases {
        let statement = IrStatement::VariableDeclaration {
            name: name.to_string(),
            java_type: java_type.clone(),
            initializer: Some(IrExpression::Literal(
                Literal::Number(literal.to_string()),
                None,
                span.clone(),
            )),
            is_final: false,
            modifiers: IrModifiers::default(),
            span: span.clone(),
        };

        let rendered = generator
            .generate_statement(&statement)
            .expect("generate variable declaration with numeric suffix");
        assert_eq!(rendered, expected);
    }
}

#[test]
fn load_mode_sample_declaration_generates_http_fetch_pipeline() {
    let declaration = sample_load_declaration_with(
        DataFormat::Json,
        "https://example.com/users.json",
        SampleSourceKind::Http,
        Some("/tmp/sample-cache/users.json"),
        Some(4096),
    );
    let program = IrProgram {
        package: Some("sample.load.http".to_string()),
        imports: vec![],
        type_declarations: vec![IrStatement::SampleDeclaration(declaration)],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span: dummy_span(),
    };

    let unit = generate_java_code(&program).expect("generate load helper from sample declaration");

    for expected_import in [
        "java.net.http.HttpClient",
        "java.net.http.HttpRequest",
        "java.net.http.HttpResponse",
    ] {
        assert!(
            unit.imports.iter().any(|import| import == expected_import),
            "expected import {expected_import} to be generated: {:?}",
            unit.imports
        );
    }

    let source = unit.to_source(&JavaCodeGenConfig::default());
    assert!(
        source.contains("private static final String SOURCE = \"https://example.com/users.json\";"),
        "source constant should reference remote URI: {source}"
    );
    assert!(
        source
            .contains("private static final String CACHE_PATH = \"/tmp/sample-cache/users.json\";"),
        "cache constant should be set when cache path is supplied: {source}"
    );
    assert!(
        source.contains("private static final long LIMIT_BYTES = 4096L;"),
        "limit constant should preserve configured byte ceiling: {source}"
    );
    assert!(
        source.contains("private static final java.net.http.HttpClient HTTP_CLIENT"),
        "HTTP client helper should be initialised: {source}"
    );
    assert!(
        source.contains(
            "public static java.util.List<UserSample> loadUsers() throws java.io.IOException {"
        ),
        "load helper should expose typed list loader: {source}"
    );
    assert!(
        source.contains("return fetchHttp(SOURCE);"),
        "primary fetch should invoke HTTP path: {source}"
    );
    assert!(
        source.contains("return decodeJson(bytes);"),
        "JSON loader should decode via decodeJson: {source}"
    );
    assert!(
        source.contains("verifySha(data);"),
        "sha256 verification should be enforced before decode: {source}"
    );
    assert!(
        source.contains("enforceLimit(data.length);"),
        "byte limit enforcement should be generated: {source}"
    );
    assert!(
        source.contains("public record UserSample"),
        "record descriptor should be emitted alongside loader: {source}"
    );
}

#[test]
fn load_mode_sample_declaration_supports_s3_and_tabular_decoding() {
    let declaration = sample_load_declaration_with(
        DataFormat::Csv,
        "s3://bucket/data/users.csv",
        SampleSourceKind::S3,
        None,
        None,
    );
    let program = IrProgram {
        package: Some("sample.load.s3".to_string()),
        imports: vec![],
        type_declarations: vec![IrStatement::SampleDeclaration(declaration)],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span: dummy_span(),
    };

    let unit =
        generate_java_code(&program).expect("generate S3 load helper from sample declaration");
    let source = unit.to_source(&JavaCodeGenConfig::default());

    assert!(
        source.contains("private static final String CACHE_PATH = null;"),
        "cache constant should be null when cache path omitted: {source}"
    );
    assert!(
        source.contains("private static final long LIMIT_BYTES = -1L;"),
        "limit constant should disable enforcement when unspecified: {source}"
    );
    assert!(
        source.contains("return fetchS3(SOURCE);"),
        "S3 fetch branch should be emitted for s3:// sources: {source}"
    );
    assert!(
        source.contains("return decodeTabular(bytes, ',');"),
        "CSV load helpers should decode via decodeTabular: {source}"
    );
    assert!(
        source.contains(
            "ProcessBuilder builder = new ProcessBuilder(\"aws\", \"s3\", \"cp\", uri, \"-\", \"--no-progress\");"
        ),
        "aws CLI invocation should be generated for S3 fetch: {source}"
    );
    assert!(
        source.contains("throw new java.io.IOException(\"aws s3 cp が失敗しました"),
        "S3 helper should surface CLI failure diagnostics: {source}"
    );
    assert!(
        source.contains(
            "public static java.util.List<UserSample> loadUsers() throws java.io.IOException {"
        ),
        "loader should expose typed CSV results: {source}"
    );
    assert!(
        source.contains("public record UserSample"),
        "record definition should accompany S3 loader: {source}"
    );
}

fn ir_identifier(name: &str, ty: &JavaType) -> IrExpression {
    IrExpression::Identifier {
        name: name.to_string(),
        java_type: ty.clone(),
        span: dummy_span(),
    }
}

fn switch_case(
    labels: Vec<IrCaseLabel>,
    guard: Option<IrExpression>,
    body: IrExpression,
) -> IrSwitchCase {
    IrSwitchCase {
        labels,
        guard,
        body,
        span: dummy_span(),
    }
}

fn string_literal(value: &str) -> IrExpression {
    IrExpression::Literal(Literal::String(value.to_string()), None, dummy_span())
}

fn raw_multiline_literal(value: &str) -> IrExpression {
    IrExpression::Literal(
        Literal::String(value.to_string()),
        Some(RawStringFlavor::MultiLine),
        dummy_span(),
    )
}

#[test]
fn char_to_string_conversion_emits_string_value_of() {
    let conversion = CharToStringConversion {
        value: Box::new(IrExpression::Literal(
            Literal::Character('a'),
            None,
            dummy_span(),
        )),
        span: dummy_span(),
    };
    let expression = IrExpression::CharToString(conversion);

    let mut generator = JavaCodeGenerator::new();
    let rendered = generator
        .generate_expression(&expression)
        .expect("char-to-string conversion should render");

    assert_eq!(rendered, "String.valueOf('a')");
}

#[test]
fn raw_triple_string_literal_renders_text_block_without_trailing_newline() {
    let expression = raw_multiline_literal("line1\nline2");

    let mut generator = JavaCodeGenerator::new();
    let rendered = generator
        .generate_expression(&expression)
        .expect("raw triple string should render");

    let expected = "\"\"\"\nline1\nline2\\\n\"\"\"";
    assert_eq!(rendered, expected);
}

#[test]
fn raw_triple_string_literal_preserves_terminal_newline() {
    let expression = raw_multiline_literal("行1\n行2\n");

    let mut generator = JavaCodeGenerator::new();
    let rendered = generator
        .generate_expression(&expression)
        .expect("raw triple string with trailing newline should render");

    let expected = "\"\"\"\n行1\n行2\n\"\"\"";
    assert_eq!(rendered, expected);
}

#[test]
fn switch_expression_renders_guards_and_default_branch() {
    let int_type = JavaType::Primitive("int".to_string());

    let expression = IrExpression::Switch {
        discriminant: Box::new(ir_identifier("x", &int_type)),
        cases: vec![
            switch_case(
                vec![IrCaseLabel::Literal(Literal::Number("1".to_string()))],
                Some(IrExpression::Binary {
                    left: Box::new(ir_identifier("x", &int_type)),
                    op: BinaryOp::Greater,
                    right: Box::new(IrExpression::Literal(
                        Literal::Number("0".to_string()),
                        None,
                        dummy_span(),
                    )),
                    java_type: JavaType::boolean(),
                    span: dummy_span(),
                }),
                string_literal("positive"),
            ),
            switch_case(vec![IrCaseLabel::Default], None, string_literal("fallback")),
        ],
        java_type: JavaType::string(),
        implicit_end: None,
        strategy_description: None,
        span: dummy_span(),
    };

    let mut generator = JavaCodeGenerator::new();
    let rendered = generator
        .generate_expression(&expression)
        .expect("switch expression generation should succeed");

    let expected =
        "switch (x) {\n    case 1 when (x > 0) -> \"positive\";\n    default -> \"fallback\";\n}\n";
    assert_eq!(
        rendered, expected,
        "switch with guard/default should render"
    );
}

#[test]
fn switch_expression_appends_implicit_unit_branch_when_missing_default() {
    let int_type = JavaType::Primitive("int".to_string());

    let expression = IrExpression::Switch {
        discriminant: Box::new(ir_identifier("x", &int_type)),
        cases: vec![switch_case(
            vec![IrCaseLabel::Literal(Literal::Number("1".to_string()))],
            None,
            string_literal("one"),
        )],
        java_type: JavaType::string(),
        implicit_end: Some(IrImplicitWhenEnd::Unit { span: dummy_span() }),
        strategy_description: None,
        span: dummy_span(),
    };

    let mut generator = JavaCodeGenerator::new();
    let rendered = generator
        .generate_expression(&expression)
        .expect("implicit end should render");

    let expected = "switch (x) {\n    case 1 -> \"one\";\n    default -> { }\n}\n";
    assert_eq!(
        rendered, expected,
        "implicit Unit branch should emit default"
    );
}

#[test]
fn switch_expression_respects_existing_default_over_implicit_unit() {
    let int_type = JavaType::Primitive("int".to_string());

    let expression = IrExpression::Switch {
        discriminant: Box::new(ir_identifier("x", &int_type)),
        cases: vec![
            switch_case(
                vec![IrCaseLabel::Literal(Literal::Number("1".to_string()))],
                None,
                string_literal("one"),
            ),
            switch_case(vec![IrCaseLabel::Default], None, string_literal("other")),
        ],
        java_type: JavaType::string(),
        implicit_end: Some(IrImplicitWhenEnd::Unit { span: dummy_span() }),
        strategy_description: None,
        span: dummy_span(),
    };

    let mut generator = JavaCodeGenerator::new();
    let rendered = generator
        .generate_expression(&expression)
        .expect("explicit default should win");

    let expected = "switch (x) {\n    case 1 -> \"one\";\n    default -> \"other\";\n}\n";
    assert_eq!(
        rendered, expected,
        "implicit end must not duplicate default"
    );
}

#[test]
fn switch_expression_includes_strategy_comment_when_present() {
    let int_type = JavaType::Primitive("int".to_string());

    let expression = IrExpression::Switch {
        discriminant: Box::new(ir_identifier("x", &int_type)),
        cases: vec![switch_case(
            vec![IrCaseLabel::Literal(Literal::Number("1".to_string()))],
            None,
            string_literal("one"),
        )],
        java_type: JavaType::string(),
        implicit_end: Some(IrImplicitWhenEnd::Unit { span: dummy_span() }),
        strategy_description: Some(
            "strategy=Switch arms=1 guards=0 default=false exhaustive=unknown".to_string(),
        ),
        span: dummy_span(),
    };

    let mut generator = JavaCodeGenerator::new();
    let rendered = generator
        .generate_expression(&expression)
        .expect("strategy comment should render");

    let expected = "// strategy=Switch arms=1 guards=0 default=false exhaustive=unknown\nswitch (x) {\n    case 1 -> \"one\";\n    default -> { }\n}\n";
    assert_eq!(rendered, expected, "strategy comment missing");
}

#[test]
fn switch_expression_renders_range_case_with_comment() {
    let int_type = JavaType::Primitive("int".to_string());
    let bool_type = JavaType::Primitive("boolean".to_string());

    let binding_identifier = |name: &str| IrExpression::Identifier {
        name: name.to_string(),
        java_type: int_type.clone(),
        span: dummy_span(),
    };

    let range_lower = IrExpression::Literal(Literal::Number("0".to_string()), None, dummy_span());
    let range_upper = IrExpression::Literal(Literal::Number("10".to_string()), None, dummy_span());

    let lower_guard = IrExpression::Binary {
        left: Box::new(binding_identifier("it")),
        op: BinaryOp::GreaterEqual,
        right: Box::new(range_lower.clone()),
        java_type: bool_type.clone(),
        span: dummy_span(),
    };

    let upper_guard = IrExpression::Binary {
        left: Box::new(binding_identifier("it")),
        op: BinaryOp::LessEqual,
        right: Box::new(range_upper.clone()),
        java_type: bool_type.clone(),
        span: dummy_span(),
    };

    let guard = IrExpression::Binary {
        left: Box::new(lower_guard),
        op: BinaryOp::And,
        right: Box::new(upper_guard),
        java_type: bool_type,
        span: dummy_span(),
    };

    let expression = IrExpression::Switch {
        discriminant: Box::new(ir_identifier("x", &int_type)),
        cases: vec![
            switch_case(
                vec![IrCaseLabel::Range {
                    type_name: "int".to_string(),
                    variable: "it".to_string(),
                    lower: Box::new(range_lower),
                    upper: Box::new(range_upper),
                    inclusive_end: true,
                }],
                Some(guard),
                string_literal("small"),
            ),
            switch_case(vec![IrCaseLabel::Default], None, string_literal("fallback")),
        ],
        java_type: JavaType::string(),
        implicit_end: None,
        strategy_description: None,
        span: dummy_span(),
    };

    let mut generator = JavaCodeGenerator::new();
    let rendered = generator
        .generate_expression(&expression)
        .expect("range case should render");

    let expected = "new Object() {\n    String matchExpr() {\n        final var __subject = x;\n        String __matchResult = null;\n        boolean __matched = false;\n        if (!__matched) {\n            // range: 0..=10\n            final var it = __subject;\n            if (it >= 0 && it <= 10) {\n                __matchResult = \"small\";\n                __matched = true;\n            }\n        }\n        if (!__matched) {\n            __matchResult = \"fallback\";\n            __matched = true;\n        }\n        return __matchResult;\n    }\n}.matchExpr()\n";
    assert_eq!(rendered, expected, "range case output mismatch");
}

#[test]
fn switch_expression_implicit_unit_renders_for_java21_target() {
    let int_type = JavaType::Primitive("int".to_string());

    let expression = IrExpression::Switch {
        discriminant: Box::new(ir_identifier("x", &int_type)),
        cases: vec![switch_case(
            vec![IrCaseLabel::Literal(Literal::Number("1".to_string()))],
            None,
            string_literal("one"),
        )],
        java_type: JavaType::string(),
        implicit_end: Some(IrImplicitWhenEnd::Unit { span: dummy_span() }),
        strategy_description: None,
        span: dummy_span(),
    };

    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java21));
    let rendered = generator
        .generate_expression(&expression)
        .expect("java21 switch generation should succeed");

    let expected = "new Object() {\n    String matchExpr() {\n        final var __subject = x;\n        String __matchResult = null;\n        boolean __matched = false;\n        if (!__matched) {\n            if (java.util.Objects.equals(__subject, 1)) {\n                __matchResult = \"one\";\n                __matched = true;\n            }\n        }\n        if (!__matched) {\n            throw new IllegalStateException(\"non-exhaustive when expression\");\n        }\n        return __matchResult;\n    }\n}.matchExpr()\n";
    assert_eq!(
        rendered, expected,
        "java21 fallback should render lambda IIFE"
    );
}

#[test]
fn switch_expression_java21_type_pattern_fallback() {
    let object_type = JavaType::Reference {
        name: "Object".to_string(),
        generic_args: vec![],
    };

    let expression = IrExpression::Switch {
        discriminant: Box::new(ir_identifier("subject", &object_type)),
        cases: vec![
            switch_case(
                vec![IrCaseLabel::TypePattern {
                    type_name: "String".to_string(),
                    variable: "value".to_string(),
                    deconstruction: None,
                }],
                None,
                string_literal("string"),
            ),
            switch_case(vec![IrCaseLabel::Default], None, string_literal("other")),
        ],
        java_type: JavaType::string(),
        implicit_end: None,
        strategy_description: Some(
            "strategy=Switch arms=2 guards=0 default=true exhaustive=true".to_string(),
        ),
        span: dummy_span(),
    };

    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java21));
    let rendered = generator
        .generate_expression(&expression)
        .expect("java21 type pattern fallback should render");

    let expected = "// strategy=Switch arms=2 guards=0 default=true exhaustive=true\nnew Object() {\n    String matchExpr() {\n        final var __subject = subject;\n        String __matchResult = null;\n        boolean __matched = false;\n        if (!__matched) {\n            if (__subject instanceof String value) {\n                __matchResult = \"string\";\n                __matched = true;\n            }\n        }\n        if (!__matched) {\n            __matchResult = \"other\";\n            __matched = true;\n        }\n        return __matchResult;\n    }\n}.matchExpr()\n";
    assert_eq!(
        rendered, expected,
        "java21 fallback should use instanceof pattern"
    );
}

#[test]
fn switch_expression_java21_range_pattern_fallback() {
    let int_type = JavaType::Primitive("int".to_string());
    let bool_type = JavaType::Primitive("boolean".to_string());

    let lower_check = IrExpression::Binary {
        left: Box::new(IrExpression::Identifier {
            name: "it0".to_string(),
            java_type: int_type.clone(),
            span: dummy_span(),
        }),
        op: BinaryOp::GreaterEqual,
        right: Box::new(IrExpression::Literal(
            Literal::Number("0".to_string()),
            None,
            dummy_span(),
        )),
        java_type: bool_type.clone(),
        span: dummy_span(),
    };

    let upper_check = IrExpression::Binary {
        left: Box::new(IrExpression::Identifier {
            name: "it0".to_string(),
            java_type: int_type.clone(),
            span: dummy_span(),
        }),
        op: BinaryOp::LessEqual,
        right: Box::new(IrExpression::Literal(
            Literal::Number("10".to_string()),
            None,
            dummy_span(),
        )),
        java_type: bool_type.clone(),
        span: dummy_span(),
    };

    let guard = IrExpression::Binary {
        left: Box::new(lower_check),
        op: BinaryOp::And,
        right: Box::new(upper_check),
        java_type: bool_type,
        span: dummy_span(),
    };

    let expression = IrExpression::Switch {
        discriminant: Box::new(ir_identifier("score", &int_type)),
        cases: vec![
            switch_case(
                vec![IrCaseLabel::Range {
                    type_name: "int".to_string(),
                    variable: "it0".to_string(),
                    lower: Box::new(IrExpression::Literal(
                        Literal::Number("0".to_string()),
                        None,
                        dummy_span(),
                    )),
                    upper: Box::new(IrExpression::Literal(
                        Literal::Number("10".to_string()),
                        None,
                        dummy_span(),
                    )),
                    inclusive_end: true,
                }],
                Some(guard),
                string_literal("small"),
            ),
            switch_case(vec![IrCaseLabel::Default], None, string_literal("other")),
        ],
        java_type: JavaType::string(),
        implicit_end: None,
        strategy_description: None,
        span: dummy_span(),
    };

    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java21));
    let rendered = generator
        .generate_expression(&expression)
        .expect("java21 range pattern fallback should render");

    let expected = "new Object() {\n    String matchExpr() {\n        final var __subject = score;\n        String __matchResult = null;\n        boolean __matched = false;\n        if (!__matched) {\n            // range: 0..=10\n            final var it0 = __subject;\n            if (it0 >= 0 && it0 <= 10) {\n                __matchResult = \"small\";\n                __matched = true;\n            }\n        }\n        if (!__matched) {\n            __matchResult = \"other\";\n            __matched = true;\n        }\n        return __matchResult;\n    }\n}.matchExpr()\n";
    assert_eq!(
        rendered, expected,
        "java21 fallback should expand range patterns"
    );
}

#[test]
fn switch_expression_with_boolean_literals_does_not_require_preview() {
    let bool_type = JavaType::Primitive("boolean".to_string());

    let expression = IrExpression::Switch {
        discriminant: Box::new(ir_identifier("flag", &bool_type)),
        cases: vec![
            switch_case(
                vec![IrCaseLabel::Literal(Literal::Boolean(true))],
                None,
                string_literal("set"),
            ),
            switch_case(
                vec![IrCaseLabel::Literal(Literal::Boolean(false))],
                None,
                string_literal("unset"),
            ),
        ],
        java_type: JavaType::string(),
        implicit_end: None,
        strategy_description: Some(
            "strategy=Switch arms=2 guards=0 default=false exhaustive=unknown".to_string(),
        ),
        span: dummy_span(),
    };

    let mut generator = JavaCodeGenerator::new();
    let rendered = generator
        .generate_expression(&expression)
        .expect("boolean literal switch should render");

    let expected = "// strategy=Switch arms=2 guards=0 default=false exhaustive=unknown\nnew Object() {\n    String matchExpr() {\n        final var __subject = flag;\n        String __matchResult = null;\n        boolean __matched = false;\n        if (!__matched) {\n            if (java.util.Objects.equals(__subject, true)) {\n                __matchResult = \"set\";\n                __matched = true;\n            }\n        }\n        if (!__matched) {\n            if (java.util.Objects.equals(__subject, false)) {\n                __matchResult = \"unset\";\n                __matched = true;\n            }\n        }\n        if (!__matched) {\n            throw new IllegalStateException(\"non-exhaustive when expression\");\n        }\n        return __matchResult;\n    }\n}.matchExpr()\n";
    assert_eq!(
        rendered, expected,
        "boolean cases should fall back to safe lowering"
    );
}

#[test]
fn switch_expression_nested_destructuring_java25_and_java21() {
    let outer_type = JavaType::Reference {
        name: "Outer".to_string(),
        generic_args: vec![],
    };
    let int_type = JavaType::int();

    let nested_pattern = IrDeconstructionPattern {
        components: vec![
            IrDeconstructionComponent::Type {
                type_name: "Inner".to_string(),
                pattern: Some(Box::new(IrDeconstructionPattern {
                    components: vec![
                        IrDeconstructionComponent::Binding {
                            name: "x".to_string(),
                        },
                        IrDeconstructionComponent::Binding {
                            name: "y".to_string(),
                        },
                    ],
                })),
            },
            IrDeconstructionComponent::Binding {
                name: "count".to_string(),
            },
        ],
    };

    let sum_xy = IrExpression::Binary {
        left: Box::new(ir_identifier("x", &int_type)),
        op: BinaryOp::Add,
        right: Box::new(ir_identifier("y", &int_type)),
        java_type: int_type.clone(),
        span: dummy_span(),
    };

    let body_expr = IrExpression::Binary {
        left: Box::new(sum_xy),
        op: BinaryOp::Add,
        right: Box::new(ir_identifier("count", &int_type)),
        java_type: int_type.clone(),
        span: dummy_span(),
    };

    let expression = IrExpression::Switch {
        discriminant: Box::new(ir_identifier("value", &outer_type)),
        cases: vec![
            switch_case(
                vec![IrCaseLabel::TypePattern {
                    type_name: "Outer".to_string(),
                    variable: "outer".to_string(),
                    deconstruction: Some(nested_pattern.clone()),
                }],
                None,
                body_expr.clone(),
            ),
            switch_case(
                vec![IrCaseLabel::Default],
                None,
                IrExpression::Literal(Literal::Number("0".to_string()), None, dummy_span()),
            ),
        ],
        java_type: int_type.clone(),
        implicit_end: None,
        strategy_description: Some(
            "strategy=Switch arms=2 guards=0 default=true exhaustive=true".to_string(),
        ),
        span: dummy_span(),
    };

    let mut java25 =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let rendered_java25 = java25
        .generate_expression(&expression)
        .expect("java25 nested pattern generation");

    let expected_java25 = "// strategy=Switch arms=2 guards=0 default=true exhaustive=true\nswitch (value) {\n    case Outer(Inner(var x, var y), var count) -> x + y + count;\n    default -> 0;\n}\n";
    assert_eq!(rendered_java25, expected_java25);

    let mut java21 =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java21));
    let rendered_java21 = java21
        .generate_expression(&expression)
        .expect("java21 nested pattern fallback generation");

    let expected_java21 = "// strategy=Switch arms=2 guards=0 default=true exhaustive=true\nnew Object() {\n    int matchExpr() {\n        final var __subject = value;\n        int __matchResult = 0;\n        boolean __matched = false;\n        if (!__matched) {\n            do {\n                if (!(__subject instanceof Outer(Inner(var x, var y), var count) outer)) {\n                    break;\n                }\n                __matched = true;\n                __matchResult = x + y + count;\n                break;\n            } while (false);\n        }\n        if (!__matched) {\n            __matchResult = 0;\n            __matched = true;\n        }\n        return __matchResult;\n    }\n}.matchExpr()\n";

    assert_eq!(rendered_java21, expected_java21);
}

#[test]
fn switch_expression_java21_mixed_labels_emits_jv3105() {
    let int_type = JavaType::Primitive("int".to_string());

    let expression = IrExpression::Switch {
        discriminant: Box::new(ir_identifier("x", &int_type)),
        cases: vec![switch_case(
            vec![
                IrCaseLabel::Literal(Literal::Number("1".to_string())),
                IrCaseLabel::TypePattern {
                    type_name: "String".to_string(),
                    variable: "value".to_string(),
                    deconstruction: None,
                },
            ],
            None,
            string_literal("mixed"),
        )],
        java_type: JavaType::string(),
        implicit_end: None,
        strategy_description: None,
        span: dummy_span(),
    };

    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java21));
    let error = generator
        .generate_expression(&expression)
        .expect_err("mixed labels must error");

    match error {
        CodeGenError::PatternMatchingError { message, .. } => {
            assert!(
                message.contains("JV3105"),
                "expected JV3105 diagnostic message, got {message}"
            );
            assert!(
                message.contains("--explain JV3105"),
                "message should include explain hint: {message}"
            );
        }
        other => panic!("expected pattern matching error, got {other:?}"),
    }
}

mod pattern_switch;
mod regex_command;
mod regex_is;
mod target_matrix;

#[test]
fn passthrough_comments_emit_in_java() {
    let mut generator = JavaCodeGenerator::new();

    let line_comment = IrStatement::Comment {
        kind: IrCommentKind::Line,
        text: "// keep outer".to_string(),
        span: dummy_span(),
    };
    let block_comment = IrStatement::Comment {
        kind: IrCommentKind::Block,
        text: "/* block keep */".to_string(),
        span: dummy_span(),
    };

    assert_eq!(
        "// keep outer",
        generator.generate_statement(&line_comment).unwrap()
    );
    assert_eq!(
        "/* block keep */",
        generator.generate_statement(&block_comment).unwrap()
    );
}

#[test]
fn trailing_comments_remain_inline() {
    let base = IrStatement::VariableDeclaration {
        name: "age".to_string(),
        java_type: JavaType::Primitive("int".to_string()),
        initializer: Some(IrExpression::Literal(
            Literal::Number("25".to_string()),
            None,
            dummy_span(),
        )),
        is_final: false,
        modifiers: IrModifiers::default(),
        span: dummy_span(),
    };

    let commented = IrStatement::Commented {
        statement: Box::new(base),
        comment: "// mutable".to_string(),
        kind: IrCommentKind::Line,
        comment_span: dummy_span(),
    };

    let mut generator = JavaCodeGenerator::new();
    let rendered = generator
        .generate_statement(&commented)
        .expect("commented statement generation");

    assert_eq!(rendered, "int age = 25; // mutable");
}
