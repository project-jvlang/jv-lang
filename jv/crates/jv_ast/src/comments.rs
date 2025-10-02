use crate::types::Span;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum CommentVisibility {
    Passthrough,
    JvOnly,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum CommentKind {
    Line,
    Block,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CommentStatement {
    pub kind: CommentKind,
    pub visibility: CommentVisibility,
    pub text: String,
    pub span: Span,
}

impl CommentStatement {
    pub fn is_passthrough(&self) -> bool {
        matches!(self.visibility, CommentVisibility::Passthrough)
    }
}
