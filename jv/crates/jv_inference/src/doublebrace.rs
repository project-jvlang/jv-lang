use crate::registry::default_impl::DefaultImplementationRegistry;
use jv_ast::{Expression, Statement};
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
            "java.util.Map",
            "java.util.concurrent.ConcurrentMap",
            "java.util.Deque",
            "java.util.Queue",
            "java.util.List",
            "java.util.Set",
            "java.util.Collection",
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
        if MAP_METHODS.contains(&key) {
            Some("java.util.Map")
        } else if CONCURRENT_MAP_METHODS.contains(&key) {
            Some("java.util.concurrent.ConcurrentMap")
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

const COLLECTION_METHODS: &[&str] = &["forEach", "stream", "isEmpty", "size", "toArray"];

#[cfg(test)]
mod tests {
    use super::*;
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
}
