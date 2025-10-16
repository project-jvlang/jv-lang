// Expression tests for jv_ast
use jv_ast::*;

// Test helper functions
fn dummy_span() -> Span {
    Span::dummy()
}

// Expression tests
#[test]
fn test_expression_literal() {
    let expr = Expression::Literal(Literal::String("hello".to_string()), dummy_span());
    match expr {
        Expression::Literal(Literal::String(s), _) => assert_eq!(s, "hello"),
        _ => panic!("Expected literal expression"),
    }
}

#[test]
fn test_expression_identifier() {
    let expr = Expression::Identifier("variable".to_string(), dummy_span());
    match expr {
        Expression::Identifier(name, _) => assert_eq!(name, "variable"),
        _ => panic!("Expected identifier expression"),
    }
}

#[test]
fn test_expression_binary() {
    let left = Expression::Literal(Literal::Number("1".to_string()), dummy_span());
    let right = Expression::Literal(Literal::Number("2".to_string()), dummy_span());
    let expr = Expression::Binary {
        left: Box::new(left),
        op: BinaryOp::Add,
        right: Box::new(right),
        span: dummy_span(),
    };
    match expr {
        Expression::Binary { op, .. } => assert_eq!(op, BinaryOp::Add),
        _ => panic!("Expected binary expression"),
    }
}

#[test]
fn test_expression_unary() {
    let operand = Expression::Literal(Literal::Number("42".to_string()), dummy_span());
    let expr = Expression::Unary {
        op: UnaryOp::Minus,
        operand: Box::new(operand),
        span: dummy_span(),
    };
    match expr {
        Expression::Unary { op, .. } => assert_eq!(op, UnaryOp::Minus),
        _ => panic!("Expected unary expression"),
    }
}

#[test]
fn test_expression_call() {
    let function = Expression::Identifier("func".to_string(), dummy_span());
    let arg = Argument::Positional(Expression::Literal(
        Literal::String("arg".to_string()),
        dummy_span(),
    ));
    let expr = Expression::Call {
        function: Box::new(function),
        args: vec![arg],
        type_arguments: Vec::new(),
        argument_metadata: CallArgumentMetadata::with_style(CallArgumentStyle::Comma),
        span: dummy_span(),
    };
    match expr {
        Expression::Call { args, .. } => assert_eq!(args.len(), 1),
        _ => panic!("Expected call expression"),
    }
}

#[test]
fn test_expression_member_access() {
    let object = Expression::Identifier("obj".to_string(), dummy_span());
    let expr = Expression::MemberAccess {
        object: Box::new(object),
        property: "prop".to_string(),
        span: dummy_span(),
    };
    match expr {
        Expression::MemberAccess { property, .. } => assert_eq!(property, "prop"),
        _ => panic!("Expected member access expression"),
    }
}

#[test]
fn test_expression_null_safe_member_access() {
    let object = Expression::Identifier("obj".to_string(), dummy_span());
    let expr = Expression::NullSafeMemberAccess {
        object: Box::new(object),
        property: "prop".to_string(),
        span: dummy_span(),
    };
    match expr {
        Expression::NullSafeMemberAccess { property, .. } => assert_eq!(property, "prop"),
        _ => panic!("Expected null-safe member access expression"),
    }
}

#[test]
fn test_expression_index_access() {
    let object = Expression::Identifier("arr".to_string(), dummy_span());
    let index = Expression::Literal(Literal::Number("0".to_string()), dummy_span());
    let expr = Expression::IndexAccess {
        object: Box::new(object),
        index: Box::new(index),
        span: dummy_span(),
    };
    match expr {
        Expression::IndexAccess { .. } => assert!(true),
        _ => panic!("Expected index access expression"),
    }
}

#[test]
fn test_expression_null_safe_index_access() {
    let object = Expression::Identifier("arr".to_string(), dummy_span());
    let index = Expression::Literal(Literal::Number("0".to_string()), dummy_span());
    let expr = Expression::NullSafeIndexAccess {
        object: Box::new(object),
        index: Box::new(index),
        span: dummy_span(),
    };
    match expr {
        Expression::NullSafeIndexAccess { .. } => assert!(true),
        _ => panic!("Expected null-safe index access expression"),
    }
}

#[test]
fn test_expression_string_interpolation() {
    let parts = vec![
        StringPart::Text("Hello ".to_string()),
        StringPart::Expression(Expression::Identifier("name".to_string(), dummy_span())),
        StringPart::Text("!".to_string()),
    ];
    let expr = Expression::StringInterpolation {
        parts,
        span: dummy_span(),
    };
    match expr {
        Expression::StringInterpolation { parts, .. } => assert_eq!(parts.len(), 3),
        _ => panic!("Expected string interpolation expression"),
    }
}

#[test]
fn test_expression_when_with_subject() {
    let subject = Expression::Identifier("x".to_string(), dummy_span());
    let pattern = Pattern::Literal(Literal::Number("1".to_string()), dummy_span());
    let arm = WhenArm {
        pattern,
        guard: None,
        body: Expression::Literal(Literal::String("one".to_string()), dummy_span()),
        span: dummy_span(),
    };
    let expr = Expression::When {
        expr: Some(Box::new(subject)),
        arms: vec![arm],
        else_arm: None,
        implicit_end: None,
        span: dummy_span(),
    };
    match expr {
        Expression::When { expr, arms, .. } => {
            assert!(expr.is_some());
            assert_eq!(arms.len(), 1);
        }
        _ => panic!("Expected when expression"),
    }
}

#[test]
fn test_expression_when_without_subject() {
    let pattern = Pattern::Literal(Literal::Boolean(true), dummy_span());
    let arm = WhenArm {
        pattern,
        guard: None,
        body: Expression::Literal(Literal::String("true case".to_string()), dummy_span()),
        span: dummy_span(),
    };
    let expr = Expression::When {
        expr: None,
        arms: vec![arm],
        else_arm: Some(Box::new(Expression::Literal(
            Literal::String("default".to_string()),
            dummy_span(),
        ))),
        implicit_end: None,
        span: dummy_span(),
    };
    match expr {
        Expression::When {
            expr,
            arms,
            else_arm,
            ..
        } => {
            assert!(expr.is_none());
            assert_eq!(arms.len(), 1);
            assert!(else_arm.is_some());
        }
        _ => panic!("Expected when expression"),
    }
}

#[test]
fn test_expression_if() {
    let condition = Expression::Literal(Literal::Boolean(true), dummy_span());
    let then_branch = Expression::Literal(Literal::String("true".to_string()), dummy_span());
    let else_branch = Expression::Literal(Literal::String("false".to_string()), dummy_span());
    let expr = Expression::If {
        condition: Box::new(condition),
        then_branch: Box::new(then_branch),
        else_branch: Some(Box::new(else_branch)),
        span: dummy_span(),
    };
    match expr {
        Expression::If { else_branch, .. } => assert!(else_branch.is_some()),
        _ => panic!("Expected if expression"),
    }
}

#[test]
fn test_expression_block() {
    let stmt = Statement::Expression {
        expr: Expression::Literal(Literal::Number("42".to_string()), dummy_span()),
        span: dummy_span(),
    };
    let expr = Expression::Block {
        statements: vec![stmt],
        span: dummy_span(),
    };
    match expr {
        Expression::Block { statements, .. } => assert_eq!(statements.len(), 1),
        _ => panic!("Expected block expression"),
    }
}

#[test]
fn test_expression_array() {
    let elements = vec![
        Expression::Literal(Literal::Number("1".to_string()), dummy_span()),
        Expression::Literal(Literal::Number("2".to_string()), dummy_span()),
        Expression::Literal(Literal::Number("3".to_string()), dummy_span()),
    ];
    let expr = Expression::Array {
        elements,
        delimiter: SequenceDelimiter::Comma,
        span: dummy_span(),
    };
    match expr {
        Expression::Array { elements, .. } => assert_eq!(elements.len(), 3),
        _ => panic!("Expected array expression"),
    }
}

#[test]
fn test_expression_lambda() {
    let param = Parameter {
        name: "x".to_string(),
        type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
        default_value: None,
        span: dummy_span(),
    };
    let body = Expression::Binary {
        left: Box::new(Expression::Identifier("x".to_string(), dummy_span())),
        op: BinaryOp::Add,
        right: Box::new(Expression::Literal(
            Literal::Number("1".to_string()),
            dummy_span(),
        )),
        span: dummy_span(),
    };
    let expr = Expression::Lambda {
        parameters: vec![param],
        body: Box::new(body),
        span: dummy_span(),
    };
    match expr {
        Expression::Lambda { parameters, .. } => assert_eq!(parameters.len(), 1),
        _ => panic!("Expected lambda expression"),
    }
}

#[test]
fn test_expression_try() {
    let inner_expr = Expression::Call {
        function: Box::new(Expression::Identifier(
            "risky_func".to_string(),
            dummy_span(),
        )),
        args: vec![],
        type_arguments: Vec::new(),
        argument_metadata: CallArgumentMetadata::with_style(CallArgumentStyle::Comma),
        span: dummy_span(),
    };
    let catch_clause = TryCatchClause {
        parameter: Some(Parameter {
            name: "error".to_string(),
            type_annotation: None,
            default_value: None,
            span: dummy_span(),
        }),
        body: Box::new(Expression::Identifier("handled".to_string(), dummy_span())),
        span: dummy_span(),
    };
    let expr = Expression::Try {
        body: Box::new(inner_expr),
        catch_clauses: vec![catch_clause],
        finally_block: Some(Box::new(Expression::Literal(
            Literal::Number("0".to_string()),
            dummy_span(),
        ))),
        span: dummy_span(),
    };
    match expr {
        Expression::Try {
            catch_clauses,
            finally_block,
            ..
        } => {
            assert_eq!(catch_clauses.len(), 1);
            assert!(finally_block.is_some());
        }
        _ => panic!("Expected try expression"),
    }
}

#[test]
fn test_expression_this() {
    let expr = Expression::This(dummy_span());
    match expr {
        Expression::This(_) => assert!(true),
        _ => panic!("Expected this expression"),
    }
}

#[test]
fn test_expression_super() {
    let expr = Expression::Super(dummy_span());
    match expr {
        Expression::Super(_) => assert!(true),
        _ => panic!("Expected super expression"),
    }
}

// Argument tests
#[test]
fn test_argument_positional() {
    let expr = Expression::Literal(Literal::String("value".to_string()), dummy_span());
    let arg = Argument::Positional(expr);
    match arg {
        Argument::Positional(_) => assert!(true),
        _ => panic!("Expected positional argument"),
    }
}

#[test]
fn test_argument_named() {
    let expr = Expression::Literal(Literal::String("value".to_string()), dummy_span());
    let arg = Argument::Named {
        name: "param".to_string(),
        value: expr,
        span: dummy_span(),
    };
    match arg {
        Argument::Named { name, .. } => assert_eq!(name, "param"),
        _ => panic!("Expected named argument"),
    }
}

// StringPart tests
#[test]
fn test_string_part_text() {
    let part = StringPart::Text("Hello".to_string());
    match part {
        StringPart::Text(text) => assert_eq!(text, "Hello"),
        _ => panic!("Expected text string part"),
    }
}

#[test]
fn test_string_part_expression() {
    let expr = Expression::Identifier("name".to_string(), dummy_span());
    let part = StringPart::Expression(expr);
    match part {
        StringPart::Expression(_) => assert!(true),
        _ => panic!("Expected expression string part"),
    }
}

// WhenArm tests
#[test]
fn test_when_arm() {
    let pattern = Pattern::Identifier("x".to_string(), dummy_span());
    let body = Expression::Literal(Literal::String("matched".to_string()), dummy_span());
    let arm = WhenArm {
        pattern,
        guard: None,
        body,
        span: dummy_span(),
    };
    match arm.pattern {
        Pattern::Identifier(name, _) => assert_eq!(name, "x"),
        _ => panic!("Expected identifier pattern"),
    }
}

#[test]
fn test_pattern_range_inclusive_flag() {
    let pattern = Pattern::Range {
        start: Box::new(Expression::Literal(
            Literal::Number("0".to_string()),
            dummy_span(),
        )),
        end: Box::new(Expression::Literal(
            Literal::Number("10".to_string()),
            dummy_span(),
        )),
        inclusive_end: true,
        span: dummy_span(),
    };

    match pattern {
        Pattern::Range { inclusive_end, .. } => assert!(inclusive_end),
        _ => panic!("Expected range pattern"),
    }
}

// Parameter tests
#[test]
fn test_parameter_simple() {
    let param = Parameter {
        name: "x".to_string(),
        type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
        default_value: None,
        span: dummy_span(),
    };
    assert_eq!(param.name, "x");
    assert!(param.type_annotation.is_some());
    assert!(param.default_value.is_none());
}

#[test]
fn test_parameter_with_default() {
    let default_expr = Expression::Literal(Literal::Number("0".to_string()), dummy_span());
    let param = Parameter {
        name: "y".to_string(),
        type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
        default_value: Some(default_expr),
        span: dummy_span(),
    };
    assert_eq!(param.name, "y");
    assert!(param.type_annotation.is_some());
    assert!(param.default_value.is_some());
}
