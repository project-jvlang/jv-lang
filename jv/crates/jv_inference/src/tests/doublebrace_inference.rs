use crate::doublebrace::{
    ControlFlowViolation, DoublebraceBindingKind, DoublebraceContext, DoublebraceHeuristics,
    ReceiverResolution, detect_control_flow_violation, evaluate_member_usage, infer_doublebrace,
};
use crate::registry::default_impl::ImplementationVariant;
use crate::session::InferenceSession;
use jv_ast::expression::{CallArgumentMetadata, CallArgumentStyle, CallKind, DoublebraceInit};
use jv_ast::{Argument, Expression, Literal, Span, Statement};
use jv_build::metadata::{JavaMethodSignature, SymbolIndex, TypeEntry};
use jv_ir::types::JavaType;

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
            call_kind: CallKind::Function,
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
    let resolved = DoublebraceHeuristics::resolve_default_implementation(
        "java.util.Queue",
        ImplementationVariant::Mutable,
    );
    assert_eq!(
        resolved.as_deref(),
        Some("java.util.ArrayDeque"),
        "Queue のデフォルト実装は java.util.ArrayDeque のはず"
    );
}

fn sample_init(statements: Vec<Statement>) -> DoublebraceInit {
    DoublebraceInit {
        base: None,
        receiver_hint: None,
        statements,
        span: Span::dummy(),
    }
}

#[test]
fn infer_uses_base_type_when_expected_missing() {
    let session = InferenceSession::new();
    let init = sample_init(vec![call_statement("touch")]);
    let ctx = DoublebraceContext {
        base_type: Some("com.example.MutableBean"),
        expected_type: None,
        receiver_hint: None,
        binding_kind: DoublebraceBindingKind::Anonymous,
    };

    let result = infer_doublebrace(&init, ctx, &session);

    assert_eq!(
        result.resolved_type.as_deref(),
        Some("com.example.MutableBean")
    );
    assert_eq!(result.resolution, ReceiverResolution::BaseExpression);
    assert!(result.control_flow.is_none());
}

#[test]
fn infer_prefers_expected_type_when_available() {
    let session = InferenceSession::new();
    let init = sample_init(vec![call_statement("add")]);
    let ctx = DoublebraceContext {
        base_type: None,
        expected_type: Some("java.util.List"),
        receiver_hint: None,
        binding_kind: DoublebraceBindingKind::Anonymous,
    };

    let result = infer_doublebrace(&init, ctx, &session);

    assert_eq!(result.resolved_type.as_deref(), Some("java.util.ArrayList"));
    assert_eq!(result.resolution, ReceiverResolution::ExpectedType);
}

#[test]
fn infer_preserves_interface_for_val_binding() {
    let session = InferenceSession::new();
    let init = sample_init(vec![call_statement("add")]);
    let ctx = DoublebraceContext {
        base_type: None,
        expected_type: Some("java.util.List"),
        receiver_hint: None,
        binding_kind: DoublebraceBindingKind::Val,
    };

    let result = infer_doublebrace(&init, ctx, &session);

    assert_eq!(result.resolved_type.as_deref(), Some("java.util.List"));
    assert_eq!(result.resolution, ReceiverResolution::ExpectedType);
}

#[test]
fn infer_prefers_receiver_hint_over_other_sources() {
    let session = InferenceSession::new();
    let init = sample_init(Vec::new());
    let ctx = DoublebraceContext {
        base_type: Some("java.util.List"),
        expected_type: Some("java.util.Collection"),
        receiver_hint: Some("java.util.LinkedList"),
        binding_kind: DoublebraceBindingKind::Anonymous,
    };

    let result = infer_doublebrace(&init, ctx, &session);

    assert_eq!(
        result.resolved_type.as_deref(),
        Some("java.util.LinkedList")
    );
    assert_eq!(result.resolution, ReceiverResolution::ReceiverHint);
}

#[test]
fn infer_applies_registry_to_generic_interfaces() {
    let session = InferenceSession::new();
    let init = sample_init(vec![call_statement("add")]);
    let ctx = DoublebraceContext {
        base_type: Some("java.util.List<String>"),
        expected_type: None,
        receiver_hint: None,
        binding_kind: DoublebraceBindingKind::Anonymous,
    };

    let result = infer_doublebrace(&init, ctx, &session);

    assert_eq!(
        result.resolved_type.as_deref(),
        Some("java.util.ArrayList<String>"),
        "default implementation should preserve type arguments"
    );
    assert_eq!(result.resolution, ReceiverResolution::BaseExpression);
}

#[test]
fn infer_accepts_registry_equivalent_mismatch_between_base_and_expected() {
    let session = InferenceSession::new();
    let init = sample_init(vec![call_statement("add")]);
    let ctx = DoublebraceContext {
        base_type: Some("java.util.List<java.lang.String>"),
        expected_type: Some("java.util.Collection<java.lang.String>"),
        receiver_hint: None,
        binding_kind: DoublebraceBindingKind::Var,
    };

    let result = infer_doublebrace(&init, ctx, &session);

    assert_eq!(
        result.resolved_type.as_deref(),
        Some("java.util.ArrayList<java.lang.String>"),
        "registry-resolved concrete types should align even when base and expected differ"
    );
    assert_eq!(result.resolution, ReceiverResolution::ExpectedType);
}

#[test]
fn detect_control_flow_reports_return() {
    let span = Span::dummy();
    let statements = vec![Statement::Return {
        value: None,
        span: span.clone(),
    }];

    let violation = detect_control_flow_violation(&statements);
    assert_eq!(violation, Some(ControlFlowViolation::Return));
}

#[test]
fn member_usage_reports_missing_and_candidates() {
    let mut index = SymbolIndex::new(Some(16));
    let mut entry = TypeEntry::new("com.example.Widget".into(), "com.example".into(), None);
    entry.add_instance_field("status".into());
    entry.add_instance_method(
        "initialize".into(),
        JavaMethodSignature {
            parameters: Vec::new(),
            return_type: JavaType::Void,
        },
    );
    entry.add_instance_method(
        "reset".into(),
        JavaMethodSignature {
            parameters: Vec::new(),
            return_type: JavaType::Void,
        },
    );
    index.add_type(entry);

    let statements = vec![call_statement("configure")];
    let check = evaluate_member_usage(Some(&index), "com.example.Widget", &statements, 8);

    assert_eq!(check.missing, vec!["configure".to_string()]);
    assert!(
        check.candidates.contains(&"initialize".to_string())
            || check.candidates.contains(&"reset".to_string()),
        "候補一覧は既存メンバーを含むはず"
    );
}
