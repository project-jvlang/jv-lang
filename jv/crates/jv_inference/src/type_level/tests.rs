use super::*;
use crate::constraint::AstId;
use crate::service::TypeLevelValue;
use jv_ast::Span;
use jv_ast::types::{TypeLevelExpr, TypeLevelIdentifier, TypeLevelOp};

fn node_id(id: u32) -> AstId {
    id
}

fn int(value: i64) -> TypeLevelExpr {
    TypeLevelExpr::LiteralInt(value)
}

fn ident(name: &str) -> TypeLevelExpr {
    TypeLevelExpr::Identifier(TypeLevelIdentifier {
        name: name.to_string(),
        span: Span::dummy(),
    })
}

fn binary(op: TypeLevelOp, lhs: TypeLevelExpr, rhs: TypeLevelExpr) -> TypeLevelExpr {
    TypeLevelExpr::BinaryOp {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: Span::dummy(),
    }
}

#[test]
fn evaluator_computes_arithmetic_expression() {
    let expr_id = node_id(1);
    let expr = binary(TypeLevelOp::Add, int(1), int(2));
    let mut evaluator = TypeLevelEvaluator::new();
    let context = EvaluationContext::new();

    let outcome = evaluator.evaluate(expr_id, &expr, &context);
    assert!(outcome.is_success());
    assert_eq!(outcome.value, Some(TypeLevelValue::Int(3)));
    assert!(!outcome.cache_hit);

    let (evaluations, cache_hits) = evaluator.metrics();
    assert_eq!(evaluations, 1);
    assert_eq!(cache_hits, 0);
}

#[test]
fn evaluator_hits_cache_on_repeated_evaluation() {
    let expr_id = node_id(2);
    let expr = binary(TypeLevelOp::Multiply, int(4), int(5));
    let mut evaluator = TypeLevelEvaluator::new();
    let context = EvaluationContext::new();

    let first = evaluator.evaluate(expr_id, &expr, &context);
    assert!(first.is_success());
    assert!(!first.cache_hit);

    let second = evaluator.evaluate(expr_id, &expr, &context);
    assert!(second.is_success());
    assert!(second.cache_hit);

    let (evaluations, cache_hits) = evaluator.metrics();
    assert_eq!(
        evaluations, 1,
        "cache reuse should not increment evaluation count twice"
    );
    assert_eq!(cache_hits, 1);
    assert_eq!(evaluator.cache_size(), 1);
}

#[test]
fn evaluator_reports_unknown_identifier() {
    let expr_id = node_id(3);
    let expr = ident("SIZE");
    let mut evaluator = TypeLevelEvaluator::new();
    let context = EvaluationContext::new();

    let outcome = evaluator.evaluate(expr_id, &expr, &context);
    assert!(!outcome.is_success());
    assert_eq!(outcome.value, None);
    assert_eq!(outcome.diagnostics.len(), 1);
    let diagnostic = &outcome.diagnostics[0];
    assert_eq!(diagnostic.code, "JV3101");
    assert!(diagnostic.message.contains("SIZE"));
}

#[test]
fn evaluator_enforces_termination_depth() {
    let expr_id = node_id(4);
    let expr = binary(TypeLevelOp::Add, int(10), int(20));
    let mut evaluator = TypeLevelEvaluator::with_termination_config(TerminationConfig::new(1));
    let context = EvaluationContext::new();

    let outcome = evaluator.evaluate(expr_id, &expr, &context);
    assert!(!outcome.is_success());
    assert_eq!(outcome.value, None);
    assert_eq!(outcome.diagnostics.len(), 1);
    assert_eq!(outcome.diagnostics[0].code, "JV3102");
}

#[test]
fn evaluator_detects_cyclic_binding() {
    let expr_id = node_id(5);
    let expr = ident("DIM");

    let mut context = EvaluationContext::new();
    context.insert_binding(
        "DIM",
        TypeLevelBinding::with_origin(TypeLevelValue::Int(42), expr_id),
    );

    let mut evaluator = TypeLevelEvaluator::new();
    let outcome = evaluator.evaluate(expr_id, &expr, &context);

    assert!(!outcome.is_success());
    assert_eq!(outcome.diagnostics.len(), 1);
    assert_eq!(outcome.diagnostics[0].code, "JV3102");
    assert!(outcome.diagnostics[0].message.contains("循環"));
}

#[test]
fn evaluator_collects_dependencies_from_context_bindings() {
    let expr_node = node_id(6);
    let expr = ident("LENGTH");

    let mut context = EvaluationContext::new();
    let origin = node_id(99);
    context.insert_binding(
        "LENGTH",
        TypeLevelBinding::with_origin(TypeLevelValue::Int(64), origin),
    );

    let mut evaluator = TypeLevelEvaluator::new();
    let outcome = evaluator.evaluate(expr_node, &expr, &context);

    assert!(outcome.is_success());
    assert_eq!(outcome.value, Some(TypeLevelValue::Int(64)));

    let dependents = evaluator.dependency_tracker().dependents_of(origin);
    assert!(dependents.contains(&expr_node));
}
