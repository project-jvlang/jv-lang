use crate::diagnostics::{
    DiagnosticDescriptor, DiagnosticSeverity, DiagnosticStrategy, EnhancedDiagnostic,
};

pub const ENTRIES: &[DiagnosticDescriptor] = &[
    DiagnosticDescriptor {
        code: "JV2001",
        title: "ターゲットが不足しています / Target version too low",
        help: "jv.toml の [build].java_version または CLI --target を必要なリリースへ引き上げるか、互換性のある依存関係へ切り替えてください。/ Update the Java target or swap dependencies for compatible ones.",
        severity: DiagnosticSeverity::Warning,
    },
    DiagnosticDescriptor {
        code: "JV2002",
        title: "互換性フォールバックを適用しました / Compatibility fallback applied",
        help: "フォールバックを避けるには --target で高いリリースを指定するか、依存関係を更新してください。/ Raise the Java target or update dependencies to avoid fallbacks.",
        severity: DiagnosticSeverity::Information,
    },
];

pub fn descriptor(code: &str) -> Option<&'static DiagnosticDescriptor> {
    ENTRIES.iter().find(|desc| desc.code == code)
}

pub fn requires_higher_target(
    artifact: impl AsRef<str>,
    required_label: impl AsRef<str>,
    target_label: impl AsRef<str>,
) -> EnhancedDiagnostic {
    let descriptor = descriptor("JV2001").expect("JV2001 descriptor must be registered");
    let artifact = artifact.as_ref();
    let required_label = required_label.as_ref();
    let target_label = target_label.as_ref();

    EnhancedDiagnostic::new(
        descriptor,
        format!(
            "{artifact} は {required_label} 以上を必要とするため、ターゲット {target_label} ではビルドできません。\n{artifact} requires {required_label} or newer; target {target_label} cannot satisfy this requirement.",
        ),
        None,
    )
    .with_strategy(DiagnosticStrategy::Deferred)
}

pub fn fallback_applied(
    target_label: impl AsRef<str>,
    detail: impl AsRef<str>,
) -> EnhancedDiagnostic {
    let descriptor = descriptor("JV2002").expect("JV2002 descriptor must be registered");
    let target_label = target_label.as_ref();
    let detail = detail.as_ref();

    EnhancedDiagnostic::new(
        descriptor,
        format!(
            "ターゲット {target_label} では {detail} をフォールバックで生成します。\nTarget {target_label} emits fallback: {detail}.",
        ),
        None,
    )
    .with_strategy(DiagnosticStrategy::Deferred)
}
