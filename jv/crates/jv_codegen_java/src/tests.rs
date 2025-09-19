use super::*;
use insta::assert_snapshot;
use jv_ast::{Literal, Span};
use jv_ir::{
    IrExpression, IrModifiers, IrParameter, IrProgram, IrStatement, IrVisibility, JavaType,
    MethodOverload,
};
use serde_json::to_string_pretty;

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
                    "annotations": []
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
                          "annotations": []
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
                "annotations": []
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
          "annotations": []
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
