use crate::registry::default_impl::DefaultImplementationRegistry;
use crate::session::InferenceSession;
use jv_ast::expression::DoublebraceInit;
use jv_ast::{Expression, Statement};
use jv_build::metadata::{SymbolIndex, TypeEntry};
use std::collections::HashSet;

/// ヒューリスティクスベースで Doublebrace 初期化ブロックのレシーバー型を推測するユーティリティ。
pub struct DoublebraceHeuristics;

impl DoublebraceHeuristics {
    /// ブロック内のステートメントからインターフェース FQCN を推測する。
    pub fn infer_interface(statements: &[Statement]) -> Option<String> {
        let mut suggestions: HashSet<&'static str> = HashSet::new();
        let method_names = Self::collect_method_names(statements);

        if method_names.is_empty() {
            return None;
        }

        for name in method_names {
            if let Some(interface) = Self::interface_for_method(&name) {
                suggestions.insert(interface);
            }
        }

        if suggestions.is_empty() {
            return None;
        }

        // 優先度順に解決する。
        for preferred in [
            "java.util.NavigableMap",
            "java.util.SortedMap",
            "java.util.Map",
            "java.util.concurrent.ConcurrentMap",
            "java.util.NavigableSet",
            "java.util.SortedSet",
            "java.util.Deque",
            "java.util.Queue",
            "java.util.List",
            "java.util.Set",
            "java.util.Collection",
            "java.lang.Iterable",
        ] {
            if suggestions.contains(preferred) {
                return Some(preferred.to_string());
            }
        }

        suggestions
            .iter()
            .copied()
            .next()
            .map(|iface| iface.to_string())
    }

    /// レジストリからインターフェースに対応する具象クラスを取得する。
    pub fn resolve_default_implementation(interface: &str) -> Option<String> {
        let registry = DefaultImplementationRegistry::shared();
        registry
            .resolve_interface(interface)
            .map(|source| source.target().to_string())
    }

    fn collect_method_names(statements: &[Statement]) -> Vec<String> {
        let mut names = Vec::new();
        for statement in statements {
            match statement {
                Statement::Expression { expr, .. } => {
                    Self::collect_method_names_from_expression(expr, &mut names);
                }
                Statement::Assignment { value, .. } => {
                    Self::collect_method_names_from_expression(value, &mut names);
                }
                Statement::Return { value, .. } => {
                    if let Some(expr) = value {
                        Self::collect_method_names_from_expression(expr, &mut names);
                    }
                }
                _ => {}
            }
        }
        names
    }

    fn collect_method_names_from_expression(expr: &Expression, names: &mut Vec<String>) {
        match expr {
            Expression::Call { function, .. } => match function.as_ref() {
                Expression::Identifier(name, _) => names.push(name.clone()),
                Expression::MemberAccess { property, .. } => names.push(property.clone()),
                Expression::NullSafeMemberAccess { property, .. } => names.push(property.clone()),
                _ => {}
            },
            Expression::Block { statements, .. } => {
                for statement in statements {
                    match statement {
                        Statement::Expression { expr, .. } => {
                            Self::collect_method_names_from_expression(expr, names)
                        }
                        Statement::Assignment { value, .. } => {
                            Self::collect_method_names_from_expression(value, names)
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }

    fn interface_for_method(name: &str) -> Option<&'static str> {
        let lower = name.to_ascii_lowercase();
        let key = lower.as_str();
        if NAVIGABLE_MAP_METHODS.contains(&key) {
            Some("java.util.NavigableMap")
        } else if SORTED_MAP_METHODS.contains(&key) {
            Some("java.util.SortedMap")
        } else if MAP_METHODS.contains(&key) {
            Some("java.util.Map")
        } else if CONCURRENT_MAP_METHODS.contains(&key) {
            Some("java.util.concurrent.ConcurrentMap")
        } else if NAVIGABLE_SET_METHODS.contains(&key) {
            Some("java.util.NavigableSet")
        } else if SORTED_SET_METHODS.contains(&key) {
            Some("java.util.SortedSet")
        } else if DEQUE_METHODS.contains(&key) {
            Some("java.util.Deque")
        } else if QUEUE_METHODS.contains(&key) {
            Some("java.util.Queue")
        } else if SET_METHODS.contains(&key) {
            Some("java.util.Set")
        } else if LIST_METHODS.contains(&key) {
            Some("java.util.List")
        } else if COLLECTION_METHODS.contains(&key) {
            Some("java.util.Collection")
        } else if ITERABLE_METHODS.contains(&key) {
            Some("java.lang.Iterable")
        } else {
            None
        }
    }
}

// 代表的なメソッド名テーブル。
const MAP_METHODS: &[&str] = &[
    "put",
    "putall",
    "putifabsent",
    "remove",
    "removevalue",
    "compute",
    "computeifabsent",
    "computeifpresent",
    "merge",
    "get",
    "getordefault",
    "containskey",
    "containsvalue",
    "replace",
    "replaceall",
    "entryset",
    "keyset",
    "values",
];

const SORTED_MAP_METHODS: &[&str] = &[
    "firstkey",
    "lastkey",
    "headmap",
    "tailmap",
    "submap",
    "comparator",
];

const NAVIGABLE_MAP_METHODS: &[&str] = &[
    "descendingkeyset",
    "descendingmap",
    "navigablekeyset",
    "floorentry",
    "ceilingentry",
    "higherentry",
    "lowerentry",
    "floorkey",
    "ceilingkey",
    "higherkey",
    "lowerkey",
    "pollfirstentry",
    "polllastentry",
];

const CONCURRENT_MAP_METHODS: &[&str] = &[
    "putifabsent",
    "compute",
    "computeifabsent",
    "computeifpresent",
    "merge",
];

const DEQUE_METHODS: &[&str] = &[
    "push",
    "pop",
    "peekfirst",
    "peeklast",
    "addfirst",
    "addlast",
    "offerfirst",
    "offerlast",
    "removefirst",
    "removelast",
];

const QUEUE_METHODS: &[&str] = &["offer", "poll", "peek", "element"];

const SET_METHODS: &[&str] = &["addall", "remove", "contains", "retainall", "clear"];

const SORTED_SET_METHODS: &[&str] = &[
    "first",
    "last",
    "headset",
    "tailset",
    "subset",
    "comparator",
];

const NAVIGABLE_SET_METHODS: &[&str] = &[
    "descendingiterator",
    "descendingset",
    "floor",
    "ceiling",
    "higher",
    "lower",
    "pollfirst",
    "polllast",
];

const LIST_METHODS: &[&str] = &[
    "add",
    "addall",
    "get",
    "set",
    "remove",
    "removeat",
    "insert",
    "clear",
    "sort",
    "replaceall",
];

const COLLECTION_METHODS: &[&str] = &["foreach", "stream", "isempty", "size", "toarray"];

const ITERABLE_METHODS: &[&str] = &["iterator", "spliterator", "foreach"];

/// Doublebrace 初期化ブロック内で検出される制御フロー違反の種別。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ControlFlowViolation {
    Return,
    Break,
    Continue,
}

/// レシーバー型の決定に利用した情報を示す。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReceiverResolution {
    ReceiverHint,
    ExpectedType,
    BaseExpression,
    Heuristic,
    ObjectFallback,
    Unknown,
}

/// Doublebrace 推論で扱う文脈情報。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct DoublebraceContext<'a> {
    pub base_type: Option<&'a str>,
    pub expected_type: Option<&'a str>,
    pub receiver_hint: Option<&'a str>,
}

/// Doublebrace 推論の結果を保持する。
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DoublebraceInferenceResult {
    pub resolved_type: Option<String>,
    pub resolution: ReceiverResolution,
    pub control_flow: Option<ControlFlowViolation>,
}

impl DoublebraceInferenceResult {
    pub fn is_error(&self) -> bool {
        matches!(self.resolution, ReceiverResolution::Unknown) && self.resolved_type.is_none()
    }
}

/// Doublebrace ステートメントから抽出したメンバー検証結果。
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DoublebraceMemberCheck {
    pub missing: Vec<String>,
    pub candidates: Vec<String>,
}

/// Doublebrace 初期化ブロックのレシーバー型を推論し、制御フロー違反を検出する。
pub fn infer_doublebrace(
    expr: &DoublebraceInit,
    ctx: DoublebraceContext<'_>,
    session: &InferenceSession,
) -> DoublebraceInferenceResult {
    let control_flow = detect_control_flow_violation(&expr.statements);
    let symbol_index = session.symbol_index();
    let receiver = resolve_receiver(ctx, &expr.statements, symbol_index);

    DoublebraceInferenceResult {
        resolved_type: receiver.resolved_type,
        resolution: receiver.resolution,
        control_flow,
    }
}

/// Doublebrace ブロックで利用されたメンバーに対し、存在確認と候補提示を行う。
pub fn evaluate_member_usage(
    symbol_index: Option<&SymbolIndex>,
    receiver: &str,
    statements: &[Statement],
    limit: usize,
) -> DoublebraceMemberCheck {
    let mut missing = Vec::new();
    let mut candidates = Vec::new();

    if let Some(index) = symbol_index {
        if let Some(entry) = index.lookup_type(receiver) {
            let mut seen = HashSet::new();
            for name in DoublebraceHeuristics::collect_method_names(statements) {
                if !seen.insert(name.clone()) {
                    continue;
                }

                if !entry.has_field(&name) && !entry.has_instance_method(&name) {
                    missing.push(name);
                }
            }

            candidates = collect_candidate_members(entry, limit);
        }
    }

    DoublebraceMemberCheck {
        missing,
        candidates,
    }
}

/// Doublebrace ブロック内の制御フロー違反を検出する。
pub fn detect_control_flow_violation(statements: &[Statement]) -> Option<ControlFlowViolation> {
    for statement in statements {
        match statement {
            Statement::Return { .. } => return Some(ControlFlowViolation::Return),
            Statement::Break(_) => return Some(ControlFlowViolation::Break),
            Statement::Continue(_) => return Some(ControlFlowViolation::Continue),
            Statement::Expression { expr, .. } => {
                if let Some(code) = detect_control_flow_in_expression(expr) {
                    return Some(code);
                }
            }
            Statement::Assignment { value, .. } => {
                if let Some(code) = detect_control_flow_in_expression(value) {
                    return Some(code);
                }
            }
            Statement::ForIn(for_in) => {
                if let Some(code) = detect_control_flow_in_expression(&for_in.body) {
                    return Some(code);
                }
            }
            Statement::FunctionDeclaration { body, .. } => {
                if let Some(code) = detect_control_flow_in_expression(body) {
                    return Some(code);
                }
            }
            _ => {}
        }
    }
    None
}

fn detect_control_flow_in_expression(expr: &Expression) -> Option<ControlFlowViolation> {
    match expr {
        Expression::Block { statements, .. } => detect_control_flow_violation(statements),
        Expression::If {
            then_branch,
            else_branch,
            ..
        } => detect_control_flow_in_expression(then_branch).or_else(|| {
            else_branch
                .as_deref()
                .and_then(detect_control_flow_in_expression)
        }),
        Expression::When { arms, else_arm, .. } => {
            for arm in arms {
                if let Some(code) = detect_control_flow_in_expression(&arm.body) {
                    return Some(code);
                }
            }
            if let Some(else_expr) = else_arm.as_deref() {
                return detect_control_flow_in_expression(else_expr);
            }
            None
        }
        Expression::DoublebraceInit(inner) => detect_control_flow_violation(&inner.statements),
        _ => None,
    }
}

struct ReceiverSelection {
    resolved_type: Option<String>,
    resolution: ReceiverResolution,
}

fn resolve_receiver(
    ctx: DoublebraceContext<'_>,
    statements: &[Statement],
    symbol_index: Option<&SymbolIndex>,
) -> ReceiverSelection {
    if let Some(hint) = normalize_candidate(ctx.receiver_hint) {
        let resolved = apply_registry(&hint, symbol_index);
        return ReceiverSelection {
            resolved_type: Some(resolved),
            resolution: ReceiverResolution::ReceiverHint,
        };
    }

    let base = normalize_candidate(ctx.base_type);
    let expected = normalize_candidate(ctx.expected_type);

    if let (Some(base_ty), Some(expected_ty)) = (&base, &expected) {
        let base_resolved = apply_registry(base_ty, symbol_index);
        let expected_resolved = apply_registry(expected_ty, symbol_index);

        if base_resolved == expected_resolved {
            return ReceiverSelection {
                resolved_type: Some(base_resolved),
                resolution: ReceiverResolution::ExpectedType,
            };
        }

        if expected_resolved != "java.lang.Object" {
            return ReceiverSelection {
                resolved_type: Some(expected_resolved),
                resolution: ReceiverResolution::ExpectedType,
            };
        }

        return ReceiverSelection {
            resolved_type: Some(base_resolved),
            resolution: ReceiverResolution::BaseExpression,
        };
    }

    if let Some(expected_ty) = expected {
        let resolved = apply_registry(&expected_ty, symbol_index);
        return ReceiverSelection {
            resolved_type: Some(resolved),
            resolution: ReceiverResolution::ExpectedType,
        };
    }

    if let Some(base_ty) = base {
        let resolved = apply_registry(&base_ty, symbol_index);
        return ReceiverSelection {
            resolved_type: Some(resolved),
            resolution: ReceiverResolution::BaseExpression,
        };
    }

    if let Some(interface) = DoublebraceHeuristics::infer_interface(statements) {
        let resolved = apply_registry(&interface, symbol_index);
        return ReceiverSelection {
            resolved_type: Some(resolved),
            resolution: ReceiverResolution::Heuristic,
        };
    }

    if statements.is_empty() {
        return ReceiverSelection {
            resolved_type: Some("java.lang.Object".to_string()),
            resolution: ReceiverResolution::ObjectFallback,
        };
    }

    ReceiverSelection {
        resolved_type: None,
        resolution: ReceiverResolution::Unknown,
    }
}

fn normalize_candidate(candidate: Option<&str>) -> Option<String> {
    candidate
        .map(str::trim)
        .filter(|value| !value.is_empty())
        .and_then(|value| {
            if value.eq_ignore_ascii_case("unknown") {
                None
            } else {
                Some(value.to_string())
            }
        })
}

fn split_type_name(candidate: &str) -> (&str, Option<&str>) {
    if let Some(start) = candidate.find('<') {
        let (base, generics) = candidate.split_at(start);
        (base.trim(), Some(generics.trim()))
    } else {
        (candidate.trim(), None)
    }
}

fn resolve_base_type(candidate: &str, symbol_index: Option<&SymbolIndex>) -> String {
    if let Some(default_impl) = DoublebraceHeuristics::resolve_default_implementation(candidate) {
        return default_impl;
    }

    let registry = DefaultImplementationRegistry::shared();
    if let Some(abstract_impl) = registry.resolve_abstract(candidate, symbol_index) {
        return abstract_impl.target().to_string();
    }
    if let Some(interface_impl) = registry.resolve_interface(candidate) {
        return interface_impl.target().to_string();
    }

    candidate.to_string()
}

fn apply_registry(candidate: &str, symbol_index: Option<&SymbolIndex>) -> String {
    let (base, generics) = split_type_name(candidate);
    let resolved = resolve_base_type(base, symbol_index);
    if let Some(args) = generics {
        if args.is_empty() {
            resolved
        } else if resolved.contains('<') {
            resolved
        } else {
            format!("{resolved}{args}")
        }
    } else {
        resolved
    }
}

fn collect_candidate_members(entry: &TypeEntry, limit: usize) -> Vec<String> {
    let mut names: Vec<String> = entry.instance_fields.iter().cloned().collect();
    names.extend(entry.instance_methods.keys().cloned());
    if names.len() > 1 {
        names.sort();
        names.dedup();
    }
    if names.len() > limit {
        names.truncate(limit);
    }
    names
}

#[cfg(test)]
mod tests {
    use super::*;
    use jv_ast::CallKind;
    use jv_ast::expression::{CallArgumentMetadata, CallArgumentStyle};
    use jv_ast::{Argument, Literal, Span};

    fn call_expr(name: &str) -> Expression {
        let span = Span::dummy();
        Expression::Call {
            function: Box::new(Expression::Identifier(name.into(), span.clone())),
            args: vec![Argument::Positional(Expression::Literal(
                Literal::Number("1".into()),
                span.clone(),
            ))],
            type_arguments: Vec::new(),
            argument_metadata: CallArgumentMetadata::with_style(CallArgumentStyle::Whitespace),
            call_kind: CallKind::Function,
            span,
        }
    }

    #[test]
    fn infer_list_interface_from_add() {
        let stmt = Statement::Expression {
            expr: call_expr("add"),
            span: Span::dummy(),
        };
        let inferred = DoublebraceHeuristics::infer_interface(&[stmt]);
        assert_eq!(inferred.as_deref(), Some("java.util.List"));
    }

    #[test]
    fn infer_map_interface_from_put() {
        let stmt = Statement::Expression {
            expr: call_expr("put"),
            span: Span::dummy(),
        };
        let inferred = DoublebraceHeuristics::infer_interface(&[stmt]);
        assert_eq!(inferred.as_deref(), Some("java.util.Map"));
    }

    #[test]
    fn infer_queue_interface_from_offer() {
        let stmt = Statement::Expression {
            expr: call_expr("offer"),
            span: Span::dummy(),
        };
        let inferred = DoublebraceHeuristics::infer_interface(&[stmt]);
        assert_eq!(inferred.as_deref(), Some("java.util.Queue"));
    }

    #[test]
    fn infer_sorted_set_interface_from_first() {
        let stmt = Statement::Expression {
            expr: call_expr("first"),
            span: Span::dummy(),
        };
        let inferred = DoublebraceHeuristics::infer_interface(&[stmt]);
        assert_eq!(inferred.as_deref(), Some("java.util.SortedSet"));
    }

    #[test]
    fn infer_navigable_map_interface_from_floor_entry() {
        let stmt = Statement::Expression {
            expr: call_expr("floorEntry"),
            span: Span::dummy(),
        };
        let inferred = DoublebraceHeuristics::infer_interface(&[stmt]);
        assert_eq!(inferred.as_deref(), Some("java.util.NavigableMap"));
    }

    #[test]
    fn infer_iterable_from_iterator_method() {
        let stmt = Statement::Expression {
            expr: call_expr("iterator"),
            span: Span::dummy(),
        };
        let inferred = DoublebraceHeuristics::infer_interface(&[stmt]);
        assert_eq!(inferred.as_deref(), Some("java.lang.Iterable"));
    }
}
