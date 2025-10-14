use super::ImportError;
use crate::diagnostics::{DiagnosticDescriptor, DiagnosticSeverity, EnhancedDiagnostic};
use jv_ast::Span;

const MAX_SUGGESTIONS: usize = 4;

pub const ENTRIES: &[DiagnosticDescriptor] = &[
    DiagnosticDescriptor {
        code: "JV4100",
        title: "未解決の import / Unresolved import",
        help: "import 先の完全修飾名、静的メンバー、またはパッケージが見つかりません。候補から選ぶかタイプミスを修正してください。/ The import path does not match a known type, static member, or package. Pick a candidate or correct the typo.",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "JV4101",
        title: "曖昧な import / Ambiguous import",
        help: "`as` で別名を付けるか、完全修飾名を指定して競合を解消してください。/ Use `as` to alias the import or reference the fully qualified name to remove the ambiguity.",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "JV4102",
        title: "モジュールが解決できません / Required module missing",
        help: "`--add-modules` または jv.toml の build.modules でモジュールを読み込み、必要なエクスポートを有効にしてください。/ Add the module with `--add-modules` or list it under build.modules in jv.toml so that its exports are available.",
        severity: DiagnosticSeverity::Error,
    },
];

pub fn descriptor(code: &str) -> Option<&'static DiagnosticDescriptor> {
    ENTRIES.iter().find(|descriptor| descriptor.code == code)
}

pub fn from_error(error: &ImportError) -> Option<EnhancedDiagnostic> {
    match error {
        ImportError::UnknownSymbol {
            path,
            span,
            candidates,
        } => Some(unknown_import(path, span, candidates)),
        ImportError::AmbiguousSymbol {
            path,
            span,
            candidates,
        } => Some(ambiguous_import(path, span, candidates)),
        _ => None,
    }
}

pub fn unknown_import(path: &str, span: &Span, candidates: &[String]) -> EnhancedDiagnostic {
    let descriptor = descriptor("JV4100").expect("JV4100 descriptor must be registered");
    let message = if candidates.is_empty() {
        format!(
            "import `{path}` は既知のシンボルに一致しません。\nImport `{path}` could not be resolved."
        )
    } else {
        let rendered = render_candidates(candidates);
        format!(
            "import `{path}` は既知のシンボルに一致しません。近い候補:\n{rendered}\nImport `{path}` could not be resolved. Close matches:\n{rendered}"
        )
    };

    let mut diagnostic = EnhancedDiagnostic::new(descriptor, message, Some(span.clone()));
    if !candidates.is_empty() {
        let suggestions = candidates
            .iter()
            .take(MAX_SUGGESTIONS)
            .map(|candidate| format!("`import {candidate}` を使用 / Use `import {candidate}`"));
        diagnostic = diagnostic.with_suggestions(suggestions);
    }
    diagnostic
}

pub fn ambiguous_import(path: &str, span: &Span, candidates: &[String]) -> EnhancedDiagnostic {
    let descriptor = descriptor("JV4101").expect("JV4101 descriptor must be registered");
    let rendered = render_candidates(candidates);
    let message = if candidates.is_empty() {
        format!(
            "import `{path}` は複数候補に一致します。\nImport `{path}` matches multiple symbols."
        )
    } else {
        format!(
            "import `{path}` は複数のシンボルに一致します。明示的に選択してください:\n{rendered}\nImport `{path}` matches multiple symbols. Pick an explicit target:\n{rendered}"
        )
    };

    let mut diagnostic = EnhancedDiagnostic::new(descriptor, message, Some(span.clone()));
    if !candidates.is_empty() {
        let suggestions = candidates.iter().take(MAX_SUGGESTIONS).map(|candidate| {
            format!(
                "`import {candidate}` または `import {candidate} as ...` を使用 / Use `import {candidate}` or add `as` to alias it"
            )
        });
        diagnostic = diagnostic.with_suggestions(suggestions);
    } else {
        diagnostic = diagnostic.with_suggestions([String::from(
            "`as` で別名を付けて曖昧さを解消 / Use `as` to provide an explicit alias",
        )]);
    }
    diagnostic
}

pub fn missing_module(module: &str, required_by: &str, span: &Span) -> EnhancedDiagnostic {
    let descriptor = descriptor("JV4102").expect("JV4102 descriptor must be registered");
    let message = format!(
        "モジュール `{module}` がモジュールグラフに存在しないため `{required_by}` を解決できません。\nModule `{module}` is missing from the module graph, preventing `{required_by}` from resolving."
    );

    EnhancedDiagnostic::new(descriptor, message, Some(span.clone())).with_suggestions([
        format!(
            "`--add-modules {module}` を指定するか jv.toml の build.modules に追加 / Add `--add-modules {module}` or list it under build.modules in jv.toml"
        ),
    ])
}

fn render_candidates(candidates: &[String]) -> String {
    candidates
        .iter()
        .take(MAX_SUGGESTIONS)
        .map(|candidate| format!("  - {candidate}"))
        .collect::<Vec<_>>()
        .join("\n")
}
