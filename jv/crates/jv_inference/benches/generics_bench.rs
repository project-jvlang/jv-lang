use std::time::Instant;

use jv_ast::Span;
use jv_ast::types::{TypeLevelExpr, TypeLevelOp};
use jv_inference::constraint::AstId;
use jv_inference::type_level::{EvaluationContext, TypeLevelEvaluator};

fn literal(value: i64) -> TypeLevelExpr {
    TypeLevelExpr::LiteralInt(value)
}

fn nested_sum(count: i64) -> TypeLevelExpr {
    let mut expr = literal(0);
    for idx in 0..count {
        expr = TypeLevelExpr::BinaryOp {
            op: TypeLevelOp::Add,
            lhs: Box::new(expr),
            rhs: Box::new(literal(idx)),
            span: Span::dummy(),
        };
    }
    expr
}

fn run_scenario(label: &str, depth: i64) {
    let mut evaluator = TypeLevelEvaluator::new();
    let context = EvaluationContext::new();
    let expr = nested_sum(depth);
    let node: AstId = depth as AstId;

    let start = Instant::now();
    let outcome = evaluator.evaluate(node, &expr, &context);
    let elapsed = start.elapsed();

    match outcome.value {
        Some(value) => println!(
            "{label}: depth={} result={} evals={} cache_hits={} elapsed_ms={:.2}",
            depth,
            value,
            evaluator.metrics().0,
            evaluator.metrics().1,
            elapsed.as_secs_f64() * 1000.0
        ),
        None => println!(
            "{label}: depth={} failed diagnostics={:?}",
            depth, outcome.diagnostics
        ),
    }
}

fn main() {
    let scenarios = [8, 64, 256, 1024];
    for depth in scenarios {
        run_scenario("type-level-sum", depth);
    }
}
