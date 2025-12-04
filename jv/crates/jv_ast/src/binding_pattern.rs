use crate::Span;
use serde::{Deserialize, Serialize};

/// Binding patterns describe the structure on the left-hand side of declarations and assignments.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum BindingPatternKind {
    /// A single identifier binding.
    Identifier {
        /// Identifier text.
        name: String,
        /// Source span covering the identifier.
        span: Span,
    },
    /// A tuple-style binding pattern surrounded by parentheses.
    Tuple {
        /// Elements contained in the tuple binding.
        elements: Vec<BindingPatternKind>,
        /// Source span covering the entire tuple pattern.
        span: Span,
    },
    /// A list-style binding pattern surrounded by square brackets.
    List {
        /// Elements contained in the list binding.
        elements: Vec<BindingPatternKind>,
        /// Source span covering the entire list pattern.
        span: Span,
    },
    /// Placeholder binding that intentionally discards the matched value.
    Wildcard {
        /// Source span covering the wildcard token.
        span: Span,
    },
    /// Literal pattern (number/string/bool/null).
    Literal {
        literal: crate::types::Literal,
        span: Span,
    },
}

impl BindingPatternKind {
    /// Construct an identifier binding pattern.
    pub fn identifier(name: impl Into<String>, span: Span) -> Self {
        BindingPatternKind::Identifier {
            name: name.into(),
            span,
        }
    }

    /// Construct a wildcard binding pattern.
    pub fn wildcard(span: Span) -> Self {
        BindingPatternKind::Wildcard { span }
    }

    /// Returns the span associated with this pattern.
    pub fn span(&self) -> Span {
        match self {
            BindingPatternKind::Identifier { span, .. }
            | BindingPatternKind::Tuple { span, .. }
            | BindingPatternKind::List { span, .. }
            | BindingPatternKind::Wildcard { span }
            | BindingPatternKind::Literal { span, .. } => span.clone(),
        }
    }

    /// Returns nested elements when the pattern is composite.
    pub fn elements(&self) -> &[BindingPatternKind] {
        match self {
            BindingPatternKind::Tuple { elements, .. }
            | BindingPatternKind::List { elements, .. } => elements.as_slice(),
            _ => &[],
        }
    }

    /// Indicates whether the pattern is a simple identifier.
    pub fn is_identifier(&self) -> bool {
        matches!(self, BindingPatternKind::Identifier { .. })
    }

    /// Returns the first identifier encountered in this pattern, if any.
    /// For composite patterns, this walks depth-first.
    pub fn first_identifier(&self) -> Option<&str> {
        match self {
            BindingPatternKind::Identifier { name, .. } => Some(name.as_str()),
            BindingPatternKind::Tuple { elements, .. }
            | BindingPatternKind::List { elements, .. } => elements
                .iter()
                .find_map(BindingPatternKind::first_identifier),
            BindingPatternKind::Wildcard { .. } | BindingPatternKind::Literal { .. } => None,
        }
    }
}
