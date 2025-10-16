use super::{ConversionKind, HelperSpec, NullableGuard};
use crate::inference::types::TypeKind;
use jv_ast::Span;
use serde::{Deserialize, Serialize};

/// ソルバが適用した変換をTelemetryへ報告するための詳細情報。
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AppliedConversion {
    pub from_type: TypeKind,
    pub to_type: TypeKind,
    pub kind: ConversionKind,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub helper_method: Option<HelperSpec>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub nullable_guard: Option<NullableGuard>,
    pub warned: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source_span: Option<Span>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub java_span_hint: Option<String>,
}

impl AppliedConversion {
    /// 変換イベントを構築する。
    pub fn new(
        from_type: TypeKind,
        to_type: TypeKind,
        kind: ConversionKind,
        helper_method: Option<HelperSpec>,
        nullable_guard: Option<NullableGuard>,
        warned: bool,
    ) -> Self {
        Self {
            from_type,
            to_type,
            kind,
            helper_method,
            nullable_guard,
            warned,
            source_span: None,
            java_span_hint: None,
        }
    }

    /// ソース上のSpan情報を付与した新しいインスタンスを返す。
    pub fn with_source_span(mut self, span: Span) -> Self {
        self.source_span = Some(span);
        self
    }

    /// Java側でのSpanヒントを付与した新しいインスタンスを返す。
    pub fn with_java_span_hint(mut self, hint: impl Into<String>) -> Self {
        self.java_span_hint = Some(hint.into());
        self
    }
}
