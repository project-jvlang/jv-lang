// Integration tests for jv_ast
use jv_ast::*;

// Test helper functions
fn dummy_span() -> Span {
    Span::dummy()
}

#[test]
fn test_span_creation() {
    let span = Span::new(1, 0, 1, 5);
    assert_eq!(span.start_line, 1);
    assert_eq!(span.start_column, 0);
    assert_eq!(span.end_line, 1);
    assert_eq!(span.end_column, 5);
}

#[test]
fn test_span_dummy() {
    let span = Span::dummy();
    assert_eq!(span, Span::default());
}

// Literal tests
#[test]
fn test_literal_string() {
    let lit = Literal::String("hello".to_string());
    assert_eq!(lit, Literal::String("hello".to_string()));
}

#[test]
fn test_literal_number() {
    let lit = Literal::Number("42".to_string());
    assert_eq!(lit, Literal::Number("42".to_string()));
}

#[test]
fn test_literal_boolean() {
    let lit = Literal::Boolean(true);
    assert_eq!(lit, Literal::Boolean(true));
}

#[test]
fn test_literal_null() {
    let lit = Literal::Null;
    assert_eq!(lit, Literal::Null);
}

#[test]
fn test_literal_character() {
    let lit = Literal::Character('a');
    assert_eq!(lit, Literal::Character('a'));
}

// Binary operator tests
#[test]
fn test_binary_op_arithmetic() {
    assert_eq!(BinaryOp::Add, BinaryOp::Add);
    assert_eq!(BinaryOp::Subtract, BinaryOp::Subtract);
    assert_eq!(BinaryOp::Multiply, BinaryOp::Multiply);
    assert_eq!(BinaryOp::Divide, BinaryOp::Divide);
    assert_eq!(BinaryOp::Modulo, BinaryOp::Modulo);
}

#[test]
fn test_binary_op_comparison() {
    assert_eq!(BinaryOp::Equal, BinaryOp::Equal);
    assert_eq!(BinaryOp::NotEqual, BinaryOp::NotEqual);
    assert_eq!(BinaryOp::Less, BinaryOp::Less);
    assert_eq!(BinaryOp::LessEqual, BinaryOp::LessEqual);
    assert_eq!(BinaryOp::Greater, BinaryOp::Greater);
    assert_eq!(BinaryOp::GreaterEqual, BinaryOp::GreaterEqual);
}

#[test]
fn test_binary_op_logical() {
    assert_eq!(BinaryOp::And, BinaryOp::And);
    assert_eq!(BinaryOp::Or, BinaryOp::Or);
}

// Unary operator tests
#[test]
fn test_unary_op() {
    assert_eq!(UnaryOp::Not, UnaryOp::Not);
    assert_eq!(UnaryOp::Minus, UnaryOp::Minus);
    assert_eq!(UnaryOp::Plus, UnaryOp::Plus);
    assert_eq!(UnaryOp::BitNot, UnaryOp::BitNot);
}

// Type annotation tests
#[test]
fn test_type_annotation_simple() {
    let type_ann = TypeAnnotation::Simple("String".to_string());
    match type_ann {
        TypeAnnotation::Simple(name) => assert_eq!(name, "String"),
        _ => panic!("Expected simple type annotation"),
    }
}

#[test]
fn test_type_annotation_nullable() {
    let inner = TypeAnnotation::Simple("String".to_string());
    let type_ann = TypeAnnotation::Nullable(Box::new(inner));
    match type_ann {
        TypeAnnotation::Nullable(inner) => match *inner {
            TypeAnnotation::Simple(name) => assert_eq!(name, "String"),
            _ => panic!("Expected simple inner type"),
        },
        _ => panic!("Expected nullable type annotation"),
    }
}

#[test]
fn test_type_annotation_generic() {
    let type_ann = TypeAnnotation::Generic {
        name: "List".to_string(),
        type_args: vec![TypeAnnotation::Simple("String".to_string())],
    };
    match type_ann {
        TypeAnnotation::Generic { name, type_args } => {
            assert_eq!(name, "List");
            assert_eq!(type_args.len(), 1);
        }
        _ => panic!("Expected generic type annotation"),
    }
}

#[test]
fn test_type_annotation_function() {
    let params = vec![TypeAnnotation::Simple("Int".to_string())];
    let return_type = TypeAnnotation::Simple("String".to_string());
    let type_ann = TypeAnnotation::Function {
        params,
        return_type: Box::new(return_type),
    };
    match type_ann {
        TypeAnnotation::Function { params, .. } => {
            assert_eq!(params.len(), 1);
        }
        _ => panic!("Expected function type annotation"),
    }
}

#[test]
fn test_type_annotation_array() {
    let inner = TypeAnnotation::Simple("Int".to_string());
    let type_ann = TypeAnnotation::Array(Box::new(inner));
    match type_ann {
        TypeAnnotation::Array(inner) => match *inner {
            TypeAnnotation::Simple(name) => assert_eq!(name, "Int"),
            _ => panic!("Expected simple inner type"),
        },
        _ => panic!("Expected array type annotation"),
    }
}

// Pattern tests
#[test]
fn test_pattern_literal() {
    let pattern = Pattern::Literal(Literal::String("test".to_string()), dummy_span());
    match pattern {
        Pattern::Literal(Literal::String(s), _) => assert_eq!(s, "test"),
        _ => panic!("Expected literal pattern"),
    }
}

#[test]
fn test_pattern_identifier() {
    let pattern = Pattern::Identifier("x".to_string(), dummy_span());
    match pattern {
        Pattern::Identifier(name, _) => assert_eq!(name, "x"),
        _ => panic!("Expected identifier pattern"),
    }
}

#[test]
fn test_pattern_wildcard() {
    let pattern = Pattern::Wildcard(dummy_span());
    match pattern {
        Pattern::Wildcard(_) => assert!(true),
        _ => panic!("Expected wildcard pattern"),
    }
}

// Visibility tests
#[test]
fn test_visibility() {
    assert_eq!(Visibility::Public, Visibility::Public);
    assert_eq!(Visibility::Private, Visibility::Private);
    assert_eq!(Visibility::Internal, Visibility::Internal);
    assert_eq!(Visibility::Protected, Visibility::Protected);
}

// Modifiers tests
#[test]
fn test_modifiers_default() {
    let modifiers = Modifiers::default();
    assert_eq!(modifiers.visibility, Visibility::Private);
    assert!(!modifiers.is_abstract);
    assert!(!modifiers.is_final);
    assert!(!modifiers.is_static);
    assert!(!modifiers.is_override);
    assert!(!modifiers.is_open);
}

#[test]
fn test_modifiers_custom() {
    let modifiers = Modifiers {
        visibility: Visibility::Public,
        is_abstract: true,
        is_final: false,
        is_static: true,
        is_override: false,
        is_open: true,
        annotations: Vec::new(),
    };
    assert_eq!(modifiers.visibility, Visibility::Public);
    assert!(modifiers.is_abstract);
    assert!(!modifiers.is_final);
    assert!(modifiers.is_static);
    assert!(!modifiers.is_override);
    assert!(modifiers.is_open);
}
