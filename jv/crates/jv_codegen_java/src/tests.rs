use super::*;
use insta::assert_snapshot;
use jv_ast::{CallArgumentStyle, Literal, SequenceDelimiter, Span};
use jv_ir::{
    DataFormat, IrExpression, IrModifiers, IrParameter, IrProgram, IrRecordComponent,
    IrSampleDeclaration, IrStatement, IrVisibility, JavaType, MethodOverload, PrimitiveType,
    SampleMode, SampleRecordDescriptor, SampleRecordField, SampleSourceKind, Schema,
};
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
            IrExpression::Literal(Literal::String(name.to_string()), dummy_span()),
            IrExpression::Literal(Literal::Number(age.to_string()), dummy_span()),
            IrExpression::Literal(Literal::String(email.to_string()), dummy_span()),
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
        args: vec![IrExpression::Literal(
            Literal::String("/data/users.json".to_string()),
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
        args: vec![load_path_of_call()],
        argument_style: CallArgumentStyle::Comma,
        java_type: string_type(),
        span: dummy_span(),
    }
}

fn load_users_method() -> IrStatement {
    IrStatement::MethodDeclaration {
        name: "loadUsers".to_string(),
        parameters: vec![],
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
        parameters: vec![],
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
        parameters: vec![IrParameter {
            name: "input".to_string(),
            java_type: string_type(),
            modifiers: IrModifiers::default(),
            span: dummy_span(),
        }],
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
fn whitespace_array_uses_list_of_for_java25_target() {
    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));

    let expression = IrExpression::ArrayCreation {
        element_type: int_type(),
        dimensions: vec![],
        initializer: Some(vec![
            IrExpression::Literal(Literal::Number("1".to_string()), dummy_span()),
            IrExpression::Literal(Literal::Number("2".to_string()), dummy_span()),
            IrExpression::Literal(Literal::Number("3".to_string()), dummy_span()),
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
            IrExpression::Literal(Literal::Number("1".to_string()), dummy_span()),
            IrExpression::Literal(Literal::Number("2".to_string()), dummy_span()),
            IrExpression::Literal(Literal::Number("3".to_string()), dummy_span()),
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
        args: vec![
            IrExpression::Literal(Literal::Number("1".to_string()), dummy_span()),
            IrExpression::Literal(Literal::Number("2".to_string()), dummy_span()),
            IrExpression::Literal(Literal::Number("3".to_string()), dummy_span()),
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
        span: dummy_span(),
    };

    let unit = generate_java_code(&program).expect("compilation unit generation");
    let source = unit.to_source(&JavaCodeGenConfig::default());

    assert!(source.contains("package com.example;"));
    assert!(source.contains("public class Sample"));
    assert!(source.contains("sayHello"));
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
fn embed_mode_sample_declaration_handles_csv() {
    let declaration = sample_declaration_csv();
    let program = IrProgram {
        package: Some("sample.embed".to_string()),
        imports: vec![],
        type_declarations: vec![IrStatement::SampleDeclaration(declaration)],
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

public record UserSample(int id, String name, String email);

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

mod target_matrix;
