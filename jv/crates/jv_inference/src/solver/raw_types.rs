//! Raw type analysis helpers used by the inference solver.
//!
//! Phase 2 の時点では Raw 型の深刻度分類は未実装であり、すべてのイベントで
//! `severity_todo = true` を設定するプレースホルダプランを返す。

use super::SolverTelemetry;
use crate::types::{GenericSignature, RawTypeContinuation, RawTypeDirective};
use jv_ast::Span;

/// Telemetry で利用するイベントキー。
pub const RAW_TYPE_TELEMETRY_KEY: &str = "raw_type_detected";

/// Raw 型検出時に適用する防御策。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RawTypeMitigation {
    DefensiveCast,
    NullCheck,
    CommentOnly,
}

impl RawTypeMitigation {
    fn default_for(continuation: &RawTypeContinuation) -> Self {
        match continuation {
            RawTypeContinuation::AllowWithComment => RawTypeMitigation::CommentOnly,
            RawTypeContinuation::DefaultPolicy => RawTypeMitigation::NullCheck,
        }
    }
}

/// Raw 型検出イベント。診断およびテレメトリで共有するペイロード。
#[derive(Debug, Clone, PartialEq)]
pub struct RawTypeEvent {
    pub symbol: String,
    pub span: Span,
    pub mitigation: RawTypeMitigation,
    pub continuation: RawTypeContinuation,
    pub telemetry_key: &'static str,
}

impl RawTypeEvent {
    /// 新しい Raw 型イベントを生成する。
    pub fn new(
        symbol: impl Into<String>,
        span: Span,
        continuation: RawTypeContinuation,
        mitigation: RawTypeMitigation,
    ) -> Self {
        Self {
            symbol: symbol.into(),
            span,
            mitigation,
            continuation,
            telemetry_key: RAW_TYPE_TELEMETRY_KEY,
        }
    }

    /// AST/IR で収集したディレクティブからイベントを組み立てる。
    pub fn from_directive(directive: &RawTypeDirective) -> Self {
        let mitigation = RawTypeMitigation::default_for(&directive.mode);
        Self::new(
            directive.owner.qualified(),
            directive.span.clone(),
            directive.mode.clone(),
            mitigation,
        )
    }
}

/// Raw 型ポリシーのプレースホルダプラン。
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RawTypePolicyPlan {
    pub severity_todo: bool,
}

impl RawTypePolicyPlan {
    /// Phase 2 では常に TODO として記録する。
    pub fn phase2_placeholder() -> Self {
        Self { severity_todo: true }
    }
}

/// Raw 型ディレクティブの解釈を行うユーティリティ。
#[derive(Debug, Default)]
pub struct RawTypeAnalyzer;

impl RawTypeAnalyzer {
    /// ジェネリックシグネチャからディレクティブを読み取り、テレメトリへ記録する。
    pub fn analyze_signature(
        signature: &GenericSignature,
        telemetry: &mut SolverTelemetry,
    ) -> Vec<RawTypeEvent> {
        Self::record_events(
            signature
                .raw_directives
                .iter()
                .map(RawTypeEvent::from_directive),
            telemetry,
        )
    }

    /// 任意の Raw 型イベント列をテレメトリへ記録し、収集結果を返す。
    pub fn record_events<I>(events: I, telemetry: &mut SolverTelemetry) -> Vec<RawTypeEvent>
    where
        I: IntoIterator<Item = RawTypeEvent>,
    {
        let events: Vec<RawTypeEvent> = events.into_iter().collect();
        if events.is_empty() {
            return events;
        }

        telemetry.set_raw_type_policy(RawTypePolicyPlan::phase2_placeholder());
        for event in &events {
            telemetry.record_raw_type_event(event.clone());
        }

        events
    }

    /// ディレクティブ集合を直接処理するショートカット。
    pub fn record_directives(
        directives: &[RawTypeDirective],
        telemetry: &mut SolverTelemetry,
    ) -> Vec<RawTypeEvent> {
        Self::record_events(
            directives.iter().map(RawTypeEvent::from_directive),
            telemetry,
        )
    }
}
