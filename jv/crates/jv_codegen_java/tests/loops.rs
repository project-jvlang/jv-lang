use jv_ast::{BinaryOp, Literal, Span};
use jv_codegen_java::{JavaCodeGenConfig, JavaCodeGenerator};
use jv_ir::{
    IrExpression, IrForEachKind, IrForLoopMetadata, IrModifiers, IrNumericRangeLoop, IrStatement,
    JavaType,
};
use jv_pm::JavaTarget;

fn dummy_span() -> Span {
    Span::dummy()
}

fn identifier(name: &str, ty: &JavaType, span: Span) -> IrExpression {
    IrExpression::Identifier {
        name: name.to_string(),
        java_type: ty.clone(),
        span,
    }
}

fn number_literal(value: &str, span: Span) -> IrExpression {
    IrExpression::Literal(Literal::Number(value.to_string()), None, span)
}

fn empty_block(span: Span) -> IrStatement {
    IrStatement::Block {
        statements: Vec::new(),
        span,
    }
}

fn numeric_range_for(inclusive: bool) -> IrStatement {
    let span = dummy_span();
    let binding_type = JavaType::int();
    let end_type = JavaType::int();

    let init = IrStatement::VariableDeclaration {
        name: "idx".to_string(),
        java_type: binding_type.clone(),
        initializer: Some(number_literal("0", span.clone())),
        is_final: false,
        modifiers: IrModifiers::default(),
        span: span.clone(),
    };

    let condition_op = if inclusive {
        BinaryOp::LessEqual
    } else {
        BinaryOp::Less
    };

    let condition = IrExpression::Binary {
        left: Box::new(identifier("idx", &binding_type, span.clone())),
        op: condition_op,
        right: Box::new(identifier("__jv_range_end_0", &end_type, span.clone())),
        java_type: JavaType::boolean(),
        span: span.clone(),
    };

    let update = IrExpression::Assignment {
        target: Box::new(identifier("idx", &binding_type, span.clone())),
        value: Box::new(IrExpression::Binary {
            left: Box::new(identifier("idx", &binding_type, span.clone())),
            op: BinaryOp::Add,
            right: Box::new(number_literal("1", span.clone())),
            java_type: binding_type.clone(),
            span: span.clone(),
        }),
        java_type: binding_type.clone(),
        span: span.clone(),
    };

    IrStatement::For {
        init: Some(Box::new(init)),
        condition: Some(condition),
        update: Some(update),
        body: Box::new(empty_block(span.clone())),
        metadata: Some(IrForLoopMetadata::NumericRange(IrNumericRangeLoop {
            binding: "idx".to_string(),
            binding_type: binding_type.clone(),
            end_variable: "__jv_range_end_0".to_string(),
            end_type,
            inclusive,
            span,
        })),
        span: dummy_span(),
    }
}

fn foreach_statement(kind: IrForEachKind) -> IrStatement {
    let span = dummy_span();
    let item_type = JavaType::string();
    let iterable_type = JavaType::object();
    IrStatement::ForEach {
        variable: "item".to_string(),
        variable_type: item_type.clone(),
        iterable: identifier("items", &iterable_type, span.clone()),
        body: Box::new(empty_block(span.clone())),
        iterable_kind: kind,
        span,
    }
}

#[test]
fn numeric_range_loop_uses_increment_operator_java25() {
    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let rendered = generator
        .generate_statement(&numeric_range_for(false))
        .expect("code generation should succeed");
    let header = rendered.lines().next().expect("loop should render header");
    assert_eq!(
        header, "for (int idx = 0; idx < __jv_range_end_0; idx++) {",
        "exclusive range should use < and idx++"
    );
}

#[test]
fn inclusive_range_loop_uses_less_equal() {
    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let rendered = generator
        .generate_statement(&numeric_range_for(true))
        .expect("code generation should succeed");
    let header = rendered.lines().next().expect("loop should render header");
    assert!(
        header.contains("<="),
        "inclusive range should render <= comparison"
    );
    assert!(
        header.ends_with("idx++) {"),
        "inclusive range should still use increment operator"
    );
}

#[test]
fn numeric_range_loop_preserves_legacy_java21_output() {
    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java21));
    let rendered = generator
        .generate_statement(&numeric_range_for(false))
        .expect("code generation should succeed");
    let header = rendered.lines().next().expect("loop should render header");
    assert!(
        header.contains("idx < __jv_range_end_0"),
        "legacy target should continue using < comparison"
    );
    assert!(
        header.contains("idx++)"),
        "legacy target should render compact increment"
    );
}

#[test]
fn iterable_foreach_uses_enhanced_for() {
    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let rendered = generator
        .generate_statement(&foreach_statement(IrForEachKind::Iterable))
        .expect("foreach generation");
    let header = rendered
        .lines()
        .next()
        .expect("foreach should render header");
    assert_eq!(
        header, "for (String item : items) {",
        "iterable loops should render enhanced for"
    );
}

#[test]
fn lazy_sequence_foreach_wraps_with_cleanup() {
    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let rendered = generator
        .generate_statement(&foreach_statement(IrForEachKind::LazySequence {
            needs_cleanup: true,
        }))
        .expect("lazy sequence generation");
    assert!(
        rendered.contains("final var __jvSequence = items;"),
        "lazy sequence should cache iterable expression"
    );
    assert!(
        rendered.contains("if (__jvSequence instanceof AutoCloseable closeable)"),
        "lazy sequence should guard cleanup with AutoCloseable"
    );
    assert!(
        rendered.contains("closeable.close();"),
        "lazy sequence cleanup should invoke close()"
    );
}
