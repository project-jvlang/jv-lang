// Statement tests for jv_ast
use jv_ast::*;

// Test helper functions
fn dummy_span() -> Span {
    Span::dummy()
}

// Statement tests
#[test]
fn test_statement_val_declaration() {
    let stmt = Statement::ValDeclaration {
        name: "x".to_string(),
        type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
        initializer: Expression::Literal(Literal::Number("42".to_string()), dummy_span()),
        modifiers: Modifiers::default(),
        origin: ValBindingOrigin::ExplicitKeyword,
        span: dummy_span(),
    };
    match stmt {
        Statement::ValDeclaration { name, .. } => assert_eq!(name, "x"),
        _ => panic!("Expected val declaration"),
    }
}

#[test]
fn test_statement_var_declaration() {
    let stmt = Statement::VarDeclaration {
        name: "y".to_string(),
        type_annotation: Some(TypeAnnotation::Simple("String".to_string())),
        initializer: Some(Expression::Literal(
            Literal::String("hello".to_string()),
            dummy_span(),
        )),
        modifiers: Modifiers::default(),
        span: dummy_span(),
    };
    match stmt {
        Statement::VarDeclaration { name, .. } => assert_eq!(name, "y"),
        _ => panic!("Expected var declaration"),
    }
}

#[test]
fn test_statement_function_declaration() {
    let param = Parameter {
        name: "x".to_string(),
        type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
        default_value: None,
        span: dummy_span(),
    };
    let stmt = Statement::FunctionDeclaration {
        name: "add".to_string(),
        type_parameters: Vec::new(),
        where_clause: None,
        parameters: vec![param],
        return_type: Some(TypeAnnotation::Simple("Int".to_string())),
        body: Box::new(Expression::Identifier("x".to_string(), dummy_span())),
        modifiers: Modifiers::default(),
        span: dummy_span(),
    };
    match stmt {
        Statement::FunctionDeclaration {
            name, parameters, ..
        } => {
            assert_eq!(name, "add");
            assert_eq!(parameters.len(), 1);
        }
        _ => panic!("Expected function declaration"),
    }
}

#[test]
fn test_where_clause_trait_bound_predicate() {
    let predicate_span = Span::new(1, 20, 1, 40);
    let clause_span = Span::new(1, 10, 1, 40);
    let clause = WhereClause {
        predicates: vec![WherePredicate::TraitBound {
            type_param: "T".to_string(),
            trait_name: QualifiedName::new(vec!["Comparable".to_string()], predicate_span.clone()),
            type_args: vec![TypeAnnotation::Simple("T".to_string())],
            span: predicate_span.clone(),
        }],
        span: clause_span.clone(),
    };

    assert_eq!(clause.predicates.len(), 1);
    let predicate = clause.predicates.first().unwrap();
    match predicate {
        WherePredicate::TraitBound {
            type_param,
            trait_name,
            type_args,
            span,
        } => {
            assert_eq!(type_param, "T");
            assert_eq!(trait_name.qualified(), "Comparable");
            assert_eq!(type_args.len(), 1);
            assert_eq!(span, &predicate_span);
        }
        _ => panic!("Expected trait bound predicate"),
    }
    assert_eq!(clause.span, clause_span);
}

#[test]
fn test_statement_class_declaration() {
    let property = Property {
        name: "value".to_string(),
        type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
        initializer: None,
        is_mutable: false,
        modifiers: Modifiers::default(),
        getter: None,
        setter: None,
        span: dummy_span(),
    };
    let stmt = Statement::ClassDeclaration {
        name: "MyClass".to_string(),
        type_parameters: vec![],
        superclass: None,
        interfaces: vec![],
        properties: vec![property],
        methods: vec![],
        modifiers: Modifiers::default(),
        span: dummy_span(),
    };
    match stmt {
        Statement::ClassDeclaration {
            name, properties, ..
        } => {
            assert_eq!(name, "MyClass");
            assert_eq!(properties.len(), 1);
        }
        _ => panic!("Expected class declaration"),
    }
}

#[test]
fn test_statement_data_class_declaration() {
    let param = Parameter {
        name: "value".to_string(),
        type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
        default_value: None,
        span: dummy_span(),
    };
    let stmt = Statement::DataClassDeclaration {
        name: "Point".to_string(),
        parameters: vec![param],
        type_parameters: vec![],
        is_mutable: false,
        modifiers: Modifiers::default(),
        span: dummy_span(),
    };
    match stmt {
        Statement::DataClassDeclaration {
            name, parameters, ..
        } => {
            assert_eq!(name, "Point");
            assert_eq!(parameters.len(), 1);
        }
        _ => panic!("Expected data class declaration"),
    }
}

#[test]
fn test_statement_interface_declaration() {
    let method = Box::new(Statement::FunctionDeclaration {
        name: "method".to_string(),
        type_parameters: Vec::new(),
        where_clause: None,
        parameters: vec![],
        return_type: None,
        body: Box::new(Expression::Literal(Literal::Null, dummy_span())),
        modifiers: Modifiers::default(),
        span: dummy_span(),
    });
    let stmt = Statement::InterfaceDeclaration {
        name: "MyInterface".to_string(),
        type_parameters: vec![],
        superinterfaces: vec![],
        methods: vec![method],
        properties: vec![],
        modifiers: Modifiers::default(),
        span: dummy_span(),
    };
    match stmt {
        Statement::InterfaceDeclaration { name, methods, .. } => {
            assert_eq!(name, "MyInterface");
            assert_eq!(methods.len(), 1);
        }
        _ => panic!("Expected interface declaration"),
    }
}

#[test]
fn test_statement_expression() {
    let expr = Expression::Literal(Literal::Number("42".to_string()), dummy_span());
    let stmt = Statement::Expression {
        expr,
        span: dummy_span(),
    };
    match stmt {
        Statement::Expression { .. } => assert!(true),
        _ => panic!("Expected expression statement"),
    }
}

#[test]
fn test_statement_return() {
    let stmt = Statement::Return {
        value: Some(Expression::Literal(
            Literal::String("result".to_string()),
            dummy_span(),
        )),
        span: dummy_span(),
    };
    match stmt {
        Statement::Return { value, .. } => assert!(value.is_some()),
        _ => panic!("Expected return statement"),
    }
}

#[test]
fn test_statement_assignment() {
    let target = Expression::Identifier("x".to_string(), dummy_span());
    let value = Expression::Literal(Literal::Number("10".to_string()), dummy_span());
    let stmt = Statement::Assignment {
        target,
        value,
        span: dummy_span(),
    };
    match stmt {
        Statement::Assignment { .. } => assert!(true),
        _ => panic!("Expected assignment statement"),
    }
}

#[test]
fn test_statement_for_in_iterable_strategy() {
    let binding = LoopBinding {
        name: "item".to_string(),
        type_annotation: None,
        span: dummy_span(),
    };
    let iterable = Expression::Identifier("items".to_string(), dummy_span());
    let body = Expression::Block {
        statements: vec![],
        span: dummy_span(),
    };
    let stmt = Statement::ForIn(ForInStatement {
        binding,
        iterable: iterable.clone(),
        strategy: LoopStrategy::Iterable,
        body: Box::new(body),
        span: dummy_span(),
    });

    match stmt {
        Statement::ForIn(for_in) => {
            assert_eq!(for_in.binding.name, "item");
            assert!(matches!(for_in.strategy, LoopStrategy::Iterable));
            if let Expression::Identifier(name, _) = for_in.iterable {
                assert_eq!(name, "items");
            } else {
                panic!("Expected iterable expression to be identifier");
            }
        }
        _ => panic!("Expected for-in statement"),
    }
}

#[test]
fn test_statement_for_in_numeric_range_strategy() {
    let binding = LoopBinding {
        name: "index".to_string(),
        type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
        span: dummy_span(),
    };
    let range_span = dummy_span();
    let strategy = LoopStrategy::NumericRange(NumericRangeLoop {
        start: Expression::Literal(Literal::Number("0".to_string()), dummy_span()),
        end: Expression::Literal(Literal::Number("10".to_string()), dummy_span()),
        inclusive: false,
        span: range_span.clone(),
    });
    let iterable = Expression::Identifier("range".to_string(), dummy_span());
    let body = Expression::Block {
        statements: vec![],
        span: dummy_span(),
    };
    let stmt = Statement::ForIn(ForInStatement {
        binding,
        iterable,
        strategy,
        body: Box::new(body),
        span: dummy_span(),
    });

    match stmt {
        Statement::ForIn(for_in) => match for_in.strategy {
            LoopStrategy::NumericRange(ref meta) => {
                assert_eq!(meta.inclusive, false);
                assert_eq!(meta.span, range_span);
            }
            _ => panic!("Expected numeric range strategy"),
        },
        _ => panic!("Expected for-in statement"),
    }
}

#[test]
fn test_statement_break() {
    let stmt = Statement::Break(dummy_span());
    match stmt {
        Statement::Break(_) => assert!(true),
        _ => panic!("Expected break statement"),
    }
}

#[test]
fn test_statement_continue() {
    let stmt = Statement::Continue(dummy_span());
    match stmt {
        Statement::Continue(_) => assert!(true),
        _ => panic!("Expected continue statement"),
    }
}

#[test]
fn test_statement_import() {
    let stmt = Statement::Import {
        path: "std.collections".to_string(),
        alias: Some("collections".to_string()),
        is_wildcard: false,
        span: dummy_span(),
    };
    match stmt {
        Statement::Import { path, alias, .. } => {
            assert_eq!(path, "std.collections");
            assert!(alias.is_some());
        }
        _ => panic!("Expected import statement"),
    }
}

#[test]
fn test_statement_package() {
    let stmt = Statement::Package {
        name: "com.example".to_string(),
        span: dummy_span(),
    };
    match stmt {
        Statement::Package { name, .. } => assert_eq!(name, "com.example"),
        _ => panic!("Expected package statement"),
    }
}

// Program tests
#[test]
fn test_program() {
    let import_stmt = Statement::Import {
        path: "std.io".to_string(),
        alias: None,
        is_wildcard: false,
        span: dummy_span(),
    };
    let main_stmt = Statement::FunctionDeclaration {
        name: "main".to_string(),
        type_parameters: Vec::new(),
        where_clause: None,
        parameters: vec![],
        return_type: None,
        body: Box::new(Expression::Block {
            statements: vec![],
            span: dummy_span(),
        }),
        modifiers: Modifiers::default(),
        span: dummy_span(),
    };
    let program = Program {
        package: Some("com.example".to_string()),
        imports: vec![import_stmt],
        statements: vec![main_stmt],
        span: dummy_span(),
    };
    assert_eq!(program.package, Some("com.example".to_string()));
    assert_eq!(program.imports.len(), 1);
    assert_eq!(program.statements.len(), 1);
}

// Property tests
#[test]
fn test_property() {
    let property = Property {
        name: "name".to_string(),
        type_annotation: Some(TypeAnnotation::Simple("String".to_string())),
        initializer: Some(Expression::Literal(
            Literal::String("default".to_string()),
            dummy_span(),
        )),
        is_mutable: true,
        modifiers: Modifiers::default(),
        getter: None,
        setter: None,
        span: dummy_span(),
    };
    assert_eq!(property.name, "name");
    assert!(property.type_annotation.is_some());
    assert!(property.initializer.is_some());
    assert!(property.is_mutable);
}

// ExtensionFunction tests
#[test]
fn test_extension_function() {
    let function = Box::new(Statement::FunctionDeclaration {
        name: "toString".to_string(),
        type_parameters: Vec::new(),
        where_clause: None,
        parameters: vec![],
        return_type: Some(TypeAnnotation::Simple("String".to_string())),
        body: Box::new(Expression::Literal(
            Literal::String("".to_string()),
            dummy_span(),
        )),
        modifiers: Modifiers::default(),
        span: dummy_span(),
    });
    let ext_func = ExtensionFunction {
        receiver_type: TypeAnnotation::Simple("Int".to_string()),
        function,
        span: dummy_span(),
    };
    match ext_func.receiver_type {
        TypeAnnotation::Simple(name) => assert_eq!(name, "Int"),
        _ => panic!("Expected simple type annotation"),
    }
}

// ConcurrencyConstruct tests
#[test]
fn test_concurrency_spawn() {
    let body = Expression::Block {
        statements: vec![],
        span: dummy_span(),
    };
    let construct = ConcurrencyConstruct::Spawn {
        body: Box::new(body),
        span: dummy_span(),
    };
    match construct {
        ConcurrencyConstruct::Spawn { .. } => assert!(true),
        _ => panic!("Expected spawn construct"),
    }
}

#[test]
fn test_concurrency_async() {
    let body = Expression::Block {
        statements: vec![],
        span: dummy_span(),
    };
    let construct = ConcurrencyConstruct::Async {
        body: Box::new(body),
        span: dummy_span(),
    };
    match construct {
        ConcurrencyConstruct::Async { .. } => assert!(true),
        _ => panic!("Expected async construct"),
    }
}

#[test]
fn test_concurrency_await() {
    let expr = Expression::Call {
        function: Box::new(Expression::Identifier(
            "async_func".to_string(),
            dummy_span(),
        )),
        args: vec![],
        argument_metadata: CallArgumentMetadata::with_style(CallArgumentStyle::Comma),
        span: dummy_span(),
    };
    let construct = ConcurrencyConstruct::Await {
        expr: Box::new(expr),
        span: dummy_span(),
    };
    match construct {
        ConcurrencyConstruct::Await { .. } => assert!(true),
        _ => panic!("Expected await construct"),
    }
}

// ResourceManagement tests
#[test]
fn test_resource_use() {
    let resource = Expression::Call {
        function: Box::new(Expression::Identifier("open".to_string(), dummy_span())),
        args: vec![],
        argument_metadata: CallArgumentMetadata::with_style(CallArgumentStyle::Comma),
        span: dummy_span(),
    };
    let body = Expression::Block {
        statements: vec![],
        span: dummy_span(),
    };
    let construct = ResourceManagement::Use {
        resource: Box::new(resource),
        body: Box::new(body),
        span: dummy_span(),
    };
    match construct {
        ResourceManagement::Use { .. } => assert!(true),
        _ => panic!("Expected use construct"),
    }
}

#[test]
fn test_resource_defer() {
    let body = Expression::Call {
        function: Box::new(Expression::Identifier("cleanup".to_string(), dummy_span())),
        args: vec![],
        argument_metadata: CallArgumentMetadata::with_style(CallArgumentStyle::Comma),
        span: dummy_span(),
    };
    let construct = ResourceManagement::Defer {
        body: Box::new(body),
        span: dummy_span(),
    };
    match construct {
        ResourceManagement::Defer { .. } => assert!(true),
        _ => panic!("Expected defer construct"),
    }
}
