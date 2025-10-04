use crate::cache::CachedDiagnostic;
use crate::constraint::CapabilityResolutionError;
use crate::solver::GenericSolverDiagnostic;
use crate::types::{BoundPredicate, CapabilityBound, SymbolId, TypeId, TypeKind};
use jv_ast::Span;
use jv_support::{load_catalog, LocaleCode};
use std::collections::HashMap;

/// Human-readable diagnostic entry describing issues discovered while resolving
/// generic constraints.
#[derive(Debug, Clone, PartialEq)]
pub struct GenericDiagnostic {
    pub code: &'static str,
    pub message: String,
    pub span: Option<Span>,
    pub notes: Vec<String>,
}

impl GenericDiagnostic {
    pub fn new(code: &'static str, message: impl Into<String>) -> Self {
        Self {
            code,
            message: message.into(),
            span: None,
            notes: Vec::new(),
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    pub fn into_cached(self) -> CachedDiagnostic {
        let mut cached = CachedDiagnostic::new(self.code, self.message);
        if let Some(span) = self.span {
            cached = cached.with_span(span);
        }
        if !self.notes.is_empty() {
            cached = cached.with_notes(self.notes);
        }
        cached
    }
}

/// Collection of generic diagnostics emitted by the solver.
#[derive(Debug, Clone, PartialEq)]
pub struct GenericDiagnostics {
    entries: Vec<GenericDiagnostic>,
    locale: LocaleCode,
}

impl GenericDiagnostics {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
            locale: LocaleCode::default(),
        }
    }

    pub fn with_locale(locale: LocaleCode) -> Self {
        Self {
            entries: Vec::new(),
            locale,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn entries(&self) -> &[GenericDiagnostic] {
        &self.entries
    }

    pub fn push(&mut self, diagnostic: GenericDiagnostic) {
        self.entries.push(diagnostic);
    }

    pub fn extend_from_solver(&mut self, diagnostics: &[GenericSolverDiagnostic]) {
        for diagnostic in diagnostics {
            self.entries
                .push(translate_solver_diagnostic(self.locale, diagnostic));
        }
    }

    pub fn into_cached(self) -> Vec<CachedDiagnostic> {
        self.entries
            .into_iter()
            .map(GenericDiagnostic::into_cached)
            .collect()
    }
}

impl Default for GenericDiagnostics {
    fn default() -> Self {
        Self::new()
    }
}

/// Maps a [`GenericSolverDiagnostic`] to its user-facing representation.
pub fn translate_solver_diagnostic(
    locale: LocaleCode,
    diagnostic: &GenericSolverDiagnostic,
) -> GenericDiagnostic {
    match diagnostic {
        GenericSolverDiagnostic::ConflictingArgument {
            symbol,
            parameter,
            previous,
            candidate,
            previous_span,
            candidate_span,
        } => conflicting_argument(
            locale,
            symbol,
            parameter,
            previous,
            candidate,
            previous_span,
            candidate_span,
        ),
        GenericSolverDiagnostic::BoundViolation {
            symbol,
            parameter,
            predicate,
            span,
        } => bound_violation(locale, symbol, parameter, predicate, span),
        GenericSolverDiagnostic::UnresolvedParameter {
            symbol,
            parameter,
            predicate,
            span,
        } => unresolved_parameter(locale, symbol, parameter, predicate.as_ref(), span),
        GenericSolverDiagnostic::CapabilityResolutionFailed {
            symbol,
            parameter,
            bound,
            span,
            error,
        } => capability_resolution_failed(locale, symbol, parameter, bound, error, span),
    }
}

fn conflicting_argument(
    locale: LocaleCode,
    symbol: &SymbolId,
    parameter: &TypeId,
    previous: &TypeKind,
    candidate: &TypeKind,
    previous_span: &Span,
    candidate_span: &Span,
) -> GenericDiagnostic {
    let mut args = HashMap::new();
    args.insert("symbol", symbol.as_str().to_string());
    args.insert("type_param", parameter.to_raw().to_string());
    args.insert("previous", describe_type(previous));
    args.insert("candidate", describe_type(candidate));
    args.insert("existing_span", format_span(previous_span));

    let fallback_message = format!(
        "{symbol} の型パラメータ T{} に複数の候補が推論されました: 既存 {} と新規 {} が競合しています。型注釈を追加して曖昧さを解消してください。",
        parameter.to_raw(),
        describe_type(previous),
        describe_type(candidate),
    );
    let catalog = load_catalog(locale);
    let message = catalog
        .render("generic.conflicting_argument.message", &args)
        .unwrap_or(fallback_message);
    let note_existing = catalog
        .render("generic.conflicting_argument.note_existing", &args)
        .unwrap_or_else(|| {
            format!(
                "既存の割り当ては {} に記録されています。",
                format_span(previous_span)
            )
        });
    let note_explain = catalog
        .render("generic.conflicting_argument.note_explain", &args)
        .unwrap_or_else(|| "--explain JV2001 で詳細な制約内訳を表示できます。".to_string());

    GenericDiagnostic::new("JV2001", message)
        .with_span(candidate_span.clone())
        .with_note(note_existing)
        .with_note(note_explain)
}

fn bound_violation(
    locale: LocaleCode,
    symbol: &SymbolId,
    parameter: &TypeId,
    predicate: &BoundPredicate,
    span: &Span,
) -> GenericDiagnostic {
    let mut args = HashMap::new();
    args.insert("symbol", symbol.as_str().to_string());
    args.insert("type_param", parameter.to_raw().to_string());
    args.insert("predicate", describe_predicate(predicate));

    let fallback_message = format!(
        "{symbol} の型パラメータ T{} は {} を満たす必要がありますが、推論された型では条件を満たせません。",
        parameter.to_raw(),
        describe_predicate(predicate)
    );
    let catalog = load_catalog(locale);
    let message = catalog
        .render("generic.bound_violation.message", &args)
        .unwrap_or(fallback_message);
    let note_hint = catalog
        .render("generic.bound_violation.note_hint", &args)
        .unwrap_or_else(|| {
            "必要なインターフェース/トレイトを実装するか、型引数を変更してください。".to_string()
        });
    let note_explain = catalog
        .render("generic.bound_violation.note_explain", &args)
        .unwrap_or_else(|| {
            "--explain JV2002 で不足している限定条件の確認方法を参照できます。".to_string()
        });

    GenericDiagnostic::new("JV2002", message)
        .with_span(span.clone())
        .with_note(note_hint)
        .with_note(note_explain)
}

fn unresolved_parameter(
    locale: LocaleCode,
    symbol: &SymbolId,
    parameter: &TypeId,
    predicate: Option<&BoundPredicate>,
    span: &Span,
) -> GenericDiagnostic {
    let requirement_text = predicate
        .map(|bound| format!("必要条件: {}", describe_predicate(bound)))
        .unwrap_or_default();

    let mut args = HashMap::new();
    args.insert("symbol", symbol.as_str().to_string());
    args.insert("type_param", parameter.to_raw().to_string());
    args.insert("requirement", requirement_text.clone());

    let mut fallback_message = format!(
        "{symbol} の型パラメータ T{} を解決できませんでした。",
        parameter.to_raw()
    );
    if let Some(bound) = predicate {
        fallback_message.push_str(&format!(" 必要条件: {}.", describe_predicate(bound)));
    }

    let catalog = load_catalog(locale);
    let message = catalog
        .render("generic.unresolved_parameter.message", &args)
        .unwrap_or(fallback_message);
    let note_hint = catalog
        .render("generic.unresolved_parameter.note_hint", &args)
        .unwrap_or_else(|| {
            "型推論を補助するために明示的な型引数または境界を追加してください。".to_string()
        });
    let note_explain = catalog
        .render("generic.unresolved_parameter.note_explain", &args)
        .unwrap_or_else(|| "--explain JV2001 で詳細ガイドを確認できます。".to_string());

    GenericDiagnostic::new("JV2001", message)
        .with_span(span.clone())
        .with_note(note_hint)
        .with_note(note_explain)
}

fn capability_resolution_failed(
    locale: LocaleCode,
    symbol: &SymbolId,
    parameter: &TypeId,
    bound: &CapabilityBound,
    error: &CapabilityResolutionError,
    span: &Span,
) -> GenericDiagnostic {
    let capability_label = format!("{}<{}>", bound.name, bound.target.describe());
    match error {
        CapabilityResolutionError::NotFound {
            preferred_impl,
            inline_only,
            ..
        } => {
            let mut args = HashMap::new();
            args.insert("symbol", symbol.as_str().to_string());
            args.insert("type_param", parameter.to_raw().to_string());
            args.insert("capability", capability_label.clone());
            args.insert("target", bound.target.describe());
            if let Some(preferred) = preferred_impl {
                args.insert("preferred_impl", preferred.clone());
            }

            let fallback_message = format!(
                "{symbol} の型パラメータ T{} に要求された能力 {capability_label} の実装が見つかりません。",
                parameter.to_raw()
            );
            let catalog = load_catalog(locale);
            let message = catalog
                .render("generic.capability_not_found.message", &args)
                .unwrap_or(fallback_message);
            let note_hint = catalog
                .render("generic.capability_not_found.note_hint", &args)
                .unwrap_or_else(|| {
                    format!(
                        "impl {}<{}> {{ ... }} を追加するか、依存パッケージを確認してください。",
                        bound.name,
                        bound.target.describe()
                    )
                });

            let mut diagnostic = GenericDiagnostic::new("JV2003", message)
                .with_span(span.clone())
                .with_note(note_hint);

            if let Some(preferred) = preferred_impl {
                let preferred_note = catalog
                    .render("generic.capability_not_found.note_preferred", &args)
                    .unwrap_or_else(|| {
                        format!("指定された優先実装 {} も見つかりませんでした。", preferred)
                    });
                diagnostic = diagnostic.with_note(preferred_note);
            }
            if *inline_only {
                diagnostic = diagnostic.with_note(
                    "inline-only ヒントが指定されているため、インライン実装のみが候補になります。",
                );
            }
            diagnostic
        }
        CapabilityResolutionError::Ambiguous {
            candidates,
            preferred_impl,
            ..
        } => {
            let candidate_list = candidates
                .iter()
                .map(SymbolId::as_str)
                .collect::<Vec<_>>()
                .join(", ");
            let mut args = HashMap::new();
            args.insert("symbol", symbol.as_str().to_string());
            args.insert("type_param", parameter.to_raw().to_string());
            args.insert("capability", capability_label.clone());
            args.insert("candidates", candidate_list.clone());
            if let Some(preferred) = preferred_impl {
                args.insert("preferred_impl", preferred.clone());
            }

            let fallback_message = format!(
                "{symbol} の型パラメータ T{} に対する能力 {capability_label} の実装候補が複数存在し、決定できません。",
                parameter.to_raw()
            );
            let catalog = load_catalog(locale);
            let message = catalog
                .render("generic.capability_ambiguous.message", &args)
                .unwrap_or(fallback_message);
            let note_hint = catalog
                .render("generic.capability_ambiguous.note_hint", &args)
                .unwrap_or_else(|| {
                    "preferredImpl または明示的な型注釈で使用する実装を指定してください。"
                        .to_string()
                });

            GenericDiagnostic::new("JV2004", message)
                .with_span(span.clone())
                .with_note(note_hint)
        }
    }
}

fn describe_type(ty: &TypeKind) -> String {
    format!("{:?}", ty)
}

fn describe_predicate(predicate: &BoundPredicate) -> String {
    predicate.describe()
}

fn format_span(span: &Span) -> String {
    format!(
        "L{}C{}-L{}C{}",
        span.start_line, span.start_column, span.end_line, span.end_column
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{TraitBound, TypeVariant};

    fn dummy_span() -> Span {
        Span {
            start_line: 1,
            start_column: 1,
            end_line: 1,
            end_column: 1,
        }
    }

    #[test]
    fn translate_conflicting_argument_creates_jv2001() {
        let previous_span = dummy_span();
        let mut candidate_span = dummy_span();
        candidate_span.start_column = 5;
        candidate_span.end_column = 10;

        let diagnostic = GenericSolverDiagnostic::ConflictingArgument {
            symbol: SymbolId::from("pkg::Foo"),
            parameter: TypeId::new(1),
            previous: TypeKind::new(TypeVariant::Primitive("Int")),
            candidate: TypeKind::new(TypeVariant::Primitive("String")),
            previous_span: previous_span.clone(),
            candidate_span: candidate_span.clone(),
        };

        let translated = translate_solver_diagnostic(LocaleCode::Ja, &diagnostic);
        assert_eq!(translated.code, "JV2001");
        assert!(translated.message.contains("pkg::Foo"));
        assert!(translated.message.contains("Int"));
        assert!(translated.message.contains("String"));
        assert_eq!(translated.span, Some(candidate_span));
        assert!(translated
            .notes
            .iter()
            .any(|note| note.contains("--explain JV2001")));
    }

    #[test]
    fn translate_conflicting_argument_english_locale() {
        let diagnostic = GenericSolverDiagnostic::ConflictingArgument {
            symbol: SymbolId::from("pkg::Foo"),
            parameter: TypeId::new(1),
            previous: TypeKind::new(TypeVariant::Primitive("Int")),
            candidate: TypeKind::new(TypeVariant::Primitive("String")),
            previous_span: dummy_span(),
            candidate_span: dummy_span(),
        };

        let translated = translate_solver_diagnostic(LocaleCode::En, &diagnostic);
        assert!(translated
            .message
            .contains("Multiple candidates were inferred"));
    }

    #[test]
    fn translate_bound_violation_creates_jv2002() {
        let predicate = BoundPredicate::Trait(TraitBound::simple("Display"));
        let diagnostic = GenericSolverDiagnostic::BoundViolation {
            symbol: SymbolId::from("pkg::Bar"),
            parameter: TypeId::new(2),
            predicate: predicate.clone(),
            span: dummy_span(),
        };

        let translated = translate_solver_diagnostic(LocaleCode::Ja, &diagnostic);
        assert_eq!(translated.code, "JV2002");
        assert!(translated.message.contains("Display"));
    }

    #[test]
    fn unresolved_parameter_defaults_to_jv2001() {
        let diagnostic = GenericSolverDiagnostic::UnresolvedParameter {
            symbol: SymbolId::from("pkg::Baz"),
            parameter: TypeId::new(3),
            predicate: Some(BoundPredicate::Interface("Comparable".into())),
            span: dummy_span(),
        };

        let translated = translate_solver_diagnostic(LocaleCode::Ja, &diagnostic);
        assert_eq!(translated.code, "JV2001");
        assert!(translated.message.contains("Comparable"));
    }

    #[test]
    fn into_cached_preserves_notes_and_span() {
        let diagnostic = GenericDiagnostic::new("JV2002", "message")
            .with_span(dummy_span())
            .with_note("note1")
            .with_note("note2");

        let cached = diagnostic.into_cached();
        assert_eq!(cached.code, "JV2002");
        assert_eq!(cached.message, "message");
        assert!(cached.span.is_some());
        assert_eq!(cached.notes.len(), 2);
    }
}
