use super::NullabilityKind;
use jv_ast::types::Span;

/// Details about the operand supplied to a null safety operator.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OperatorOperand {
    pub state: NullabilityKind,
    pub symbol: Option<String>,
}

impl OperatorOperand {
    pub fn new(state: NullabilityKind, symbol: Option<String>) -> Self {
        Self { state, symbol }
    }

    pub fn state(&self) -> NullabilityKind {
        self.state
    }
}

/// Enumerates the null safety operators that require Java lowering guidance.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JavaLoweringStrategy {
    /// Safe member access `?.` operations.
    NullSafeMemberAccess,
    /// Safe index access `?[ ]` operations.
    NullSafeIndexAccess,
    /// Elvis operator `?:` expressions.
    ElvisOperator,
    /// Non-null assertion `!!` expressions.
    NotNullAssertion,
}

/// Hint that describes how a null safety operator should be lowered to Java code.
#[derive(Debug, Clone)]
pub struct JavaLoweringHint {
    pub span: Span,
    pub strategy: JavaLoweringStrategy,
    pub description: String,
    pub operand: Option<OperatorOperand>,
}

impl JavaLoweringHint {
    pub fn new(
        span: Span,
        strategy: JavaLoweringStrategy,
        description: impl Into<String>,
        operand: Option<OperatorOperand>,
    ) -> Self {
        Self {
            span,
            strategy,
            description: description.into(),
            operand,
        }
    }
}

/// Result of applying an operator semantic rule.
#[derive(Debug, Clone)]
pub struct OperatorOutcome {
    pub nullability: NullabilityKind,
    pub hint: Option<JavaLoweringHint>,
}

impl OperatorOutcome {
    fn with_hint(nullability: NullabilityKind, hint: JavaLoweringHint) -> Self {
        Self {
            nullability,
            hint: Some(hint),
        }
    }
}

/// Null safety operator semantics including Java lowering guidance.
pub struct OperatorSemantics;

impl OperatorSemantics {
    /// Applies semantics for a null-safe member access (`obj?.property`).
    pub fn null_safe_member_access(object: OperatorOperand, span: Span) -> OperatorOutcome {
        let nullability = match object.state {
            NullabilityKind::NonNull => NullabilityKind::Unknown,
            NullabilityKind::Nullable | NullabilityKind::Platform | NullabilityKind::Unknown => {
                NullabilityKind::Nullable
            }
        };

        let hint = JavaLoweringHint::new(
            span,
            JavaLoweringStrategy::NullSafeMemberAccess,
            "Javaローワリング: ?. 演算子を (receiver != null ? receiver.member : default) 形式へ変換する",
            Some(object),
        );

        OperatorOutcome::with_hint(nullability, hint)
    }

    /// Applies semantics for a null-safe index access (`array?[index]`).
    pub fn null_safe_index_access(object: OperatorOperand, span: Span) -> OperatorOutcome {
        let nullability = match object.state {
            NullabilityKind::NonNull => NullabilityKind::Unknown,
            NullabilityKind::Nullable | NullabilityKind::Platform | NullabilityKind::Unknown => {
                NullabilityKind::Nullable
            }
        };

        let hint = JavaLoweringHint::new(
            span,
            JavaLoweringStrategy::NullSafeIndexAccess,
            "Javaローワリング: ?[ ] 演算子を (receiver != null ? receiver[index] : default) 形式へ変換する",
            Some(object),
        );

        OperatorOutcome::with_hint(nullability, hint)
    }

    /// Applies semantics for the Elvis operator (`left ?: right`).
    pub fn elvis(
        left_state: NullabilityKind,
        right_state: NullabilityKind,
        span: Span,
    ) -> OperatorOutcome {
        use NullabilityKind::*;

        let nullability = match left_state {
            NonNull => NonNull,
            Nullable | Platform => match right_state {
                NonNull => NonNull,
                Nullable => Nullable,
                Platform => Platform,
                Unknown => Unknown,
            },
            Unknown => match right_state {
                Platform => Platform,
                other => other,
            },
        };

        let hint = JavaLoweringHint::new(
            span,
            JavaLoweringStrategy::ElvisOperator,
            "Javaローワリング: ?: 演算子を (lhs != null ? lhs : rhs) 三項演算子へ変換する",
            None,
        );

        OperatorOutcome::with_hint(nullability, hint)
    }

    /// Applies semantics for the non-null assertion operator (`expr!!`).
    ///
    /// Currently we treat the result as non-null irrespective of the operand's
    /// state while preserving lowering guidance for downstream stages.
    #[allow(dead_code)]
    pub fn not_null_assertion(operand: OperatorOperand, span: Span) -> OperatorOutcome {
        let hint = JavaLoweringHint::new(
            span,
            JavaLoweringStrategy::NotNullAssertion,
            "Javaローワリング: !! 演算子を Objects.requireNonNull もしくは適切な静的アサートへ変換する",
            Some(operand),
        );

        OperatorOutcome::with_hint(NullabilityKind::NonNull, hint)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn dummy_span() -> Span {
        Span::dummy()
    }

    #[test]
    fn safe_member_access_marks_nullable_and_emits_hint() {
        let operand = OperatorOperand::new(NullabilityKind::Nullable, None);
        let outcome = OperatorSemantics::null_safe_member_access(operand, dummy_span());
        assert_eq!(outcome.nullability, NullabilityKind::Nullable);
        assert!(matches!(
            outcome.hint,
            Some(JavaLoweringHint {
                strategy: JavaLoweringStrategy::NullSafeMemberAccess,
                ..
            })
        ));
    }

    #[test]
    fn platform_member_access_is_treated_as_nullable() {
        let operand = OperatorOperand::new(NullabilityKind::Platform, None);
        let outcome = OperatorSemantics::null_safe_member_access(operand, dummy_span());
        assert_eq!(outcome.nullability, NullabilityKind::Nullable);
    }

    #[test]
    fn elvis_operator_prefers_non_null_and_emits_hint() {
        let outcome = OperatorSemantics::elvis(
            NullabilityKind::Nullable,
            NullabilityKind::NonNull,
            dummy_span(),
        );
        assert_eq!(outcome.nullability, NullabilityKind::NonNull);
        assert!(matches!(
            outcome.hint,
            Some(JavaLoweringHint {
                strategy: JavaLoweringStrategy::ElvisOperator,
                ..
            })
        ));
    }

    #[test]
    fn elvis_platform_operand_propagates_platform_when_right_uncertain() {
        let outcome = OperatorSemantics::elvis(
            NullabilityKind::Platform,
            NullabilityKind::Unknown,
            dummy_span(),
        );
        assert_eq!(outcome.nullability, NullabilityKind::Unknown);

        let platform_outcome = OperatorSemantics::elvis(
            NullabilityKind::Platform,
            NullabilityKind::Platform,
            dummy_span(),
        );
        assert_eq!(platform_outcome.nullability, NullabilityKind::Platform);
    }

    #[test]
    fn not_null_assertion_force_non_null() {
        let operand = OperatorOperand::new(NullabilityKind::Unknown, None);
        let outcome = OperatorSemantics::not_null_assertion(operand, dummy_span());
        assert_eq!(outcome.nullability, NullabilityKind::NonNull);
        assert!(matches!(
            outcome.hint,
            Some(JavaLoweringHint {
                strategy: JavaLoweringStrategy::NotNullAssertion,
                ..
            })
        ));
    }
}
