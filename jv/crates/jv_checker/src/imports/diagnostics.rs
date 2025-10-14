use super::ImportError;
use crate::diagnostics::{DiagnosticDescriptor, DiagnosticSeverity, EnhancedDiagnostic};
use jv_ast::Span;
use jv_support::i18n::{catalog, LocaleCode};
use std::collections::HashMap;

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
    let mut args = HashMap::new();
    args.insert("path", path.to_string());
    let message = if candidates.is_empty() {
        bilingual_block("imports.diagnostic.unknown.message", &args).unwrap_or_else(|| {
            format!(
                "import `{path}` は既知のシンボルに一致しません。\nImport `{path}` could not be resolved."
            )
        })
    } else {
        let rendered = render_candidates(candidates);
        args.insert("candidates", rendered.clone());
        bilingual_block("imports.diagnostic.unknown.message_with_candidates", &args)
            .unwrap_or_else(|| {
                format!(
                    "import `{path}` は既知のシンボルに一致しません。近い候補:\n{rendered}\nImport `{path}` could not be resolved. Close matches:\n{rendered}"
                )
            })
    };

    let mut diagnostic = EnhancedDiagnostic::new(descriptor, message, Some(span.clone()));
    if !candidates.is_empty() {
        let suggestions = candidates.iter().take(MAX_SUGGESTIONS).map(|candidate| {
            let mut args = HashMap::new();
            args.insert("candidate", candidate.clone());
            bilingual_inline("imports.diagnostic.unknown.suggestion", &args).unwrap_or_else(|| {
                format!("`import {candidate}` を使用 / Use `import {candidate}`")
            })
        });
        diagnostic = diagnostic.with_suggestions(suggestions);
    }
    diagnostic
}

pub fn ambiguous_import(path: &str, span: &Span, candidates: &[String]) -> EnhancedDiagnostic {
    let descriptor = descriptor("JV4101").expect("JV4101 descriptor must be registered");
    let rendered = render_candidates(candidates);
    let mut args = HashMap::new();
    args.insert("path", path.to_string());
    let message = if candidates.is_empty() {
        bilingual_block("imports.diagnostic.ambiguous.message", &args).unwrap_or_else(|| {
            format!(
                "import `{path}` は複数候補に一致します。\nImport `{path}` matches multiple symbols."
            )
        })
    } else {
        args.insert("candidates", rendered.clone());
        bilingual_block("imports.diagnostic.ambiguous.message_with_candidates", &args)
            .unwrap_or_else(|| {
                format!(
                    "import `{path}` は複数のシンボルに一致します。明示的に選択してください:\n{rendered}\nImport `{path}` matches multiple symbols. Pick an explicit target:\n{rendered}"
                )
            })
    };

    let mut diagnostic = EnhancedDiagnostic::new(descriptor, message, Some(span.clone()));
    if !candidates.is_empty() {
        let suggestions = candidates.iter().take(MAX_SUGGESTIONS).map(|candidate| {
            let mut args = HashMap::new();
            args.insert("candidate", candidate.clone());
            bilingual_inline("imports.diagnostic.ambiguous.suggestion", &args).unwrap_or_else(|| {
                format!(
                    "`import {candidate}` または `import {candidate} as ...` を使用 / Use `import {candidate}` or add `as` to alias it"
                )
            })
        });
        diagnostic = diagnostic.with_suggestions(suggestions);
    } else {
        let fallback =
            String::from("`as` で別名を付けて曖昧さを解消 / Use `as` to provide an explicit alias");
        let args = HashMap::new();
        let suggestion = bilingual_inline("imports.diagnostic.ambiguous.generic_suggestion", &args)
            .unwrap_or(fallback);
        diagnostic = diagnostic.with_suggestions([suggestion]);
    }
    diagnostic
}

pub fn missing_module(module: &str, required_by: &str, span: &Span) -> EnhancedDiagnostic {
    let descriptor = descriptor("JV4102").expect("JV4102 descriptor must be registered");
    let mut args = HashMap::new();
    args.insert("module", module.to_string());
    args.insert("required_by", required_by.to_string());
    let message = bilingual_block("imports.diagnostic.missing_module.message", &args).unwrap_or_else(|| {
        format!(
            "モジュール `{module}` がモジュールグラフに存在しないため `{required_by}` を解決できません。\nModule `{module}` is missing from the module graph, preventing `{required_by}` from resolving."
        )
    });
    let suggestion = bilingual_inline("imports.diagnostic.missing_module.suggestion", &args).unwrap_or_else(|| {
        format!(
            "`--add-modules {module}` を指定するか jv.toml の build.modules に追加 / Add `--add-modules {module}` or list it under build.modules in jv.toml"
        )
    });

    EnhancedDiagnostic::new(descriptor, message, Some(span.clone())).with_suggestions([suggestion])
}

fn render_candidates(candidates: &[String]) -> String {
    candidates
        .iter()
        .take(MAX_SUGGESTIONS)
        .map(|candidate| format!("  - {candidate}"))
        .collect::<Vec<_>>()
        .join("\n")
}

fn bilingual_block(key: &str, args: &HashMap<&str, String>) -> Option<String> {
    render_bilingual(key, args).map(|(ja, en)| format!("{ja}\n{en}"))
}

fn bilingual_inline(key: &str, args: &HashMap<&str, String>) -> Option<String> {
    render_bilingual(key, args).map(|(ja, en)| format!("{ja} / {en}"))
}

fn render_bilingual(key: &str, args: &HashMap<&str, String>) -> Option<(String, String)> {
    let ja = catalog(LocaleCode::Ja).render(key, args)?;
    let en = catalog(LocaleCode::En).render(key, args)?;
    Some((ja, en))
}
