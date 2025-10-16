//! 型変換ルールとメタデータ表現。
//!
//! 将来的に SymbolIndex 由来のヘルパー検出や Telemetry 連携を拡張するため、
//! ここでは変換判定に必要な基礎的な型・構造体のみを定義する。

mod catalog;
mod rules;

pub use catalog::ConversionHelperCatalog;
pub use rules::ConversionRulesEngine;

use crate::inference::types::{TypeError, TypeKind};

/// 暗黙・半暗黙変換の種別（JLS Chapter 5をベースに簡略化）。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConversionKind {
    /// 型が完全一致し変換不要な場合。
    Identity,
    /// プリミティブの widening conversion。
    WideningPrimitive,
    /// ボクシング変換。
    Boxing,
    /// アンボクシング変換。
    Unboxing,
    /// 文字列変換（`toString` 相当）。
    StringConversion,
    /// メソッド呼び出しによる変換。
    MethodInvocation,
}

impl ConversionKind {
    /// Telemetry 計測時にヒューマンリーダブルな名称を提供する。
    pub fn label(self) -> &'static str {
        match self {
            ConversionKind::Identity => "identity",
            ConversionKind::WideningPrimitive => "widening",
            ConversionKind::Boxing => "boxing",
            ConversionKind::Unboxing => "unboxing",
            ConversionKind::StringConversion => "string",
            ConversionKind::MethodInvocation => "method",
        }
    }
}

/// 変換時に利用される補助メソッド情報。
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HelperSpec {
    /// メソッドを所有する型（FQCN）。
    pub owner: String,
    /// 呼び出すメソッド名。
    pub method: String,
    /// 静的メソッドかどうか。
    pub is_static: bool,
}

impl HelperSpec {
    /// インスタンスメソッド向けのヘルパー定義を生成する。
    pub fn instance(owner: impl Into<String>, method: impl Into<String>) -> Self {
        Self {
            owner: owner.into(),
            method: method.into(),
            is_static: false,
        }
    }

    /// 静的メソッド向けのヘルパー定義を生成する。
    pub fn static_method(owner: impl Into<String>, method: impl Into<String>) -> Self {
        Self {
            owner: owner.into(),
            method: method.into(),
            is_static: true,
        }
    }
}

/// null ガード挿入が必要な理由を表す。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NullableGuardReason {
    /// Optional<T> から T へ抜ける際の非 null 保証。
    OptionalLift,
    /// ボックス化型をプリミティブへアンボクシングする際の非 null 保証。
    Unboxing,
}

/// null ガード挿入メタデータ。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NullableGuard {
    pub reason: NullableGuardReason,
}

impl NullableGuard {
    pub const fn new(reason: NullableGuardReason) -> Self {
        Self { reason }
    }
}

/// 変換時のメタデータ。Helper/ガード情報を含む。
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConversionMetadata {
    pub kind: ConversionKind,
    pub helper: Option<HelperSpec>,
    pub nullable_guard: Option<NullableGuard>,
}

impl ConversionMetadata {
    pub fn new(kind: ConversionKind) -> Self {
        Self {
            kind,
            helper: None,
            nullable_guard: None,
        }
    }

    pub fn with_helper(mut self, helper: HelperSpec) -> Self {
        self.helper = Some(helper);
        self
    }

    pub fn with_nullable_guard(mut self, guard: NullableGuard) -> Self {
        self.nullable_guard = Some(guard);
        self
    }
}

/// ソルバが適用した変換結果。
#[derive(Debug, Clone)]
pub struct AppliedConversion {
    pub from: TypeKind,
    pub to: TypeKind,
    pub metadata: ConversionMetadata,
    pub warned: bool,
}

/// 変換判定の結果。
#[derive(Debug, Clone)]
pub enum ConversionOutcome {
    Identity,
    Allowed(ConversionMetadata),
    Rejected(TypeError),
}
