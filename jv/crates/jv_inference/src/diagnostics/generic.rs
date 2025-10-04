use crate::cache::CachedDiagnostic;
use crate::solver::GenericSolverDiagnostic;
use crate::types::{BoundPredicate, SymbolId, TypeId, TypeKind};
use jv_ast::Span;

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
#[derive(Debug, Default, Clone, PartialEq)]
pub struct GenericDiagnostics {
    entries: Vec<GenericDiagnostic>,
}

impl GenericDiagnostics {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
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
            self.entries.push(translate_solver_diagnostic(diagnostic));
        }
    }

    pub fn into_cached(self) -> Vec<CachedDiagnostic> {
        self.entries
            .into_iter()
            .map(GenericDiagnostic::into_cached)
            .collect()
    }
}

/// Maps a [`GenericSolverDiagnostic`] to its user-facing representation.
pub fn translate_solver_diagnostic(diagnostic: &GenericSolverDiagnostic) -> GenericDiagnostic {
    match diagnostic {
        GenericSolverDiagnostic::ConflictingArgument {
            symbol,
            parameter,
            previous,
            candidate,
            previous_span,
            candidate_span,
        } => conflicting_argument(
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
        } => bound_violation(symbol, parameter, predicate, span),
        GenericSolverDiagnostic::UnresolvedParameter {
            symbol,
            parameter,
            predicate,
            span,
        } => unresolved_parameter(symbol, parameter, predicate.as_ref(), span),
    }
}

fn conflicting_argument(
    symbol: &SymbolId,
    parameter: &TypeId,
    previous: &TypeKind,
    candidate: &TypeKind,
    previous_span: &Span,
    candidate_span: &Span,
) -> GenericDiagnostic {
    let message = format!(
        "{symbol} の型パラメータ T{} に複数の候補が推論されました: 既存 {} と新規 {} が競合しています。型注釈を追加して曖昧さを解消してください。",
        parameter.to_raw(),
        describe_type(previous),
        describe_type(candidate),
    );

    GenericDiagnostic::new("JV2001", message)
        .with_span(candidate_span.clone())
        .with_note(format!(
            "既存の割り当ては {} に記録されています。",
            format_span(previous_span)
        ))
        .with_note("--explain JV2001 で詳細な制約内訳を表示できます。")
}

fn bound_violation(
    symbol: &SymbolId,
    parameter: &TypeId,
    predicate: &BoundPredicate,
    span: &Span,
) -> GenericDiagnostic {
    let message = format!(
        "{symbol} の型パラメータ T{} は {} を満たす必要がありますが、推論された型では条件を満たせません。",
        parameter.to_raw(),
        describe_predicate(predicate)
    );

    GenericDiagnostic::new("JV2002", message)
        .with_span(span.clone())
        .with_note("必要なインターフェース/トレイトを実装するか、型引数を変更してください。")
        .with_note("--explain JV2002 で不足している限定条件の確認方法を参照できます。")
}

fn unresolved_parameter(
    symbol: &SymbolId,
    parameter: &TypeId,
    predicate: Option<&BoundPredicate>,
    span: &Span,
) -> GenericDiagnostic {
    let mut message = format!(
        "{symbol} の型パラメータ T{} を解決できませんでした。",
        parameter.to_raw()
    );
    if let Some(bound) = predicate {
        message.push_str(&format!(" 必要条件: {}.", describe_predicate(bound)));
    }

    GenericDiagnostic::new("JV2001", message)
        .with_span(span.clone())
        .with_note("型推論を補助するために明示的な型引数または境界を追加してください。")
        .with_note("--explain JV2001 で詳細ガイドを確認できます。")
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

        let translated = translate_solver_diagnostic(&diagnostic);
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
    fn translate_bound_violation_creates_jv2002() {
        let predicate = BoundPredicate::Trait(TraitBound::simple("Display"));
        let diagnostic = GenericSolverDiagnostic::BoundViolation {
            symbol: SymbolId::from("pkg::Bar"),
            parameter: TypeId::new(2),
            predicate: predicate.clone(),
            span: dummy_span(),
        };

        let translated = translate_solver_diagnostic(&diagnostic);
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

        let translated = translate_solver_diagnostic(&diagnostic);
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
