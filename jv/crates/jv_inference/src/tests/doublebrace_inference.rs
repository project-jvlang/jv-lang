use crate::doublebrace::DoublebraceHeuristics;

use jv_ast::expression::{CallArgumentMetadata, CallArgumentStyle};
use jv_ast::{Argument, Expression, Literal, Span, Statement};

fn call_statement(name: &str) -> Statement {
    let span = Span::dummy();
    let expr_span = span.clone();
    Statement::Expression {
        expr: Expression::Call {
            function: Box::new(Expression::Identifier(name.into(), expr_span.clone())),
            args: vec![Argument::Positional(Expression::Literal(
                Literal::Number("1".into()),
                expr_span.clone(),
            ))],
            type_arguments: Vec::new(),
            argument_metadata: CallArgumentMetadata::with_style(CallArgumentStyle::Whitespace),
            span: expr_span,
        },
        span: span.clone(),
    }
}

#[test]
fn heuristics_prefers_deque_over_list_when_push_exists() {
    // push と add が混在する場合、優先順位に従って Deque が選ばれること。
    let statements = vec![call_statement("add"), call_statement("push")];
    let inferred = DoublebraceHeuristics::infer_interface(&statements);

    assert_eq!(
        inferred.as_deref(),
        Some("java.util.Deque"),
        "push を含む場合は Deque が最優先になること"
    );
}

#[test]
fn heuristics_returns_none_for_empty_block() {
    // ステートメントが無い場合はインターフェース推測を行わないこと。
    let inferred = DoublebraceHeuristics::infer_interface(&[]);
    assert!(
        inferred.is_none(),
        "空ブロックでは推測結果が無いことが望ましい"
    );
}

#[test]
fn registry_resolves_default_queue_implementation() {
    // ヒューリスティクスで Queue が検出された場合、デフォルト実装が ArrayDeque になること。
    let resolved = DoublebraceHeuristics::resolve_default_implementation("java.util.Queue");
    assert_eq!(
        resolved.as_deref(),
        Some("java.util.ArrayDeque"),
        "Queue のデフォルト実装は java.util.ArrayDeque のはず"
    );
}
