use crate::CheckError;
use jv_ast::Span;
use jv_ir::error::TransformError;
use jv_parser::ParseError;

/// Severity level used when surfacing diagnostics to users.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Information,
    Hint,
}

/// Presentation strategy that downstream consumers (CLI/LSP) apply.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticStrategy {
    /// Surface the diagnostic immediately and halt processing.
    Immediate,
    /// Queue for later presentation after the current action completes.
    Deferred,
    /// Present interactively, allowing quick fixes or user acknowledgement.
    Interactive,
}

/// Static descriptor providing localized titles and remediation guidance.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DiagnosticDescriptor {
    pub code: &'static str,
    pub title: &'static str,
    pub help: &'static str,
    pub severity: DiagnosticSeverity,
}

/// Rich diagnostic payload shared between CLI, LSP, and other tooling surfaces.
#[derive(Debug, Clone)]
pub struct EnhancedDiagnostic {
    pub code: &'static str,
    pub title: &'static str,
    pub message: String,
    pub help: &'static str,
    pub severity: DiagnosticSeverity,
    pub strategy: DiagnosticStrategy,
    pub span: Option<Span>,
    pub related_locations: Vec<Span>,
    pub suggestions: Vec<String>,
    pub learning_hints: Option<String>,
}

impl EnhancedDiagnostic {
    pub fn new(
        descriptor: &'static DiagnosticDescriptor,
        message: impl Into<String>,
        span: Option<Span>,
    ) -> Self {
        Self {
            code: descriptor.code,
            title: descriptor.title,
            message: message.into(),
            help: descriptor.help,
            severity: descriptor.severity,
            strategy: DiagnosticStrategy::Immediate,
            span,
            related_locations: Vec::new(),
            suggestions: Vec::new(),
            learning_hints: None,
        }
    }

    pub fn with_strategy(mut self, strategy: DiagnosticStrategy) -> Self {
        self.strategy = strategy;
        self
    }

    pub fn with_suggestions<I, S>(mut self, suggestions: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.suggestions = suggestions.into_iter().map(Into::into).collect();
        self
    }

    pub fn add_related_location(mut self, span: Span) -> Self {
        self.related_locations.push(span);
        self
    }

    pub fn with_learning_hint(mut self, hint: impl Into<String>) -> Self {
        self.learning_hints = Some(hint.into());
        self
    }
}

/// Backwards compatible alias – legacy call sites still refer to ToolingDiagnostic.
pub type ToolingDiagnostic = EnhancedDiagnostic;

const DIAGNOSTICS: &[DiagnosticDescriptor] = &[
    DiagnosticDescriptor {
        code: "E_LOOP_001",
        title: "`while`/`do-while` loops have been removed from the language",
        help: "Replace legacy loops with `for (item in ...)` and consult docs/language/loops.md for migration examples.",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "JV3100",
        title: "`when` expression is not exhaustive / `when` 式が未網羅です",
        help: "欠落している sealed / enum / boolean ケースを網羅する分岐を追加してください。/ Add branches covering the missing sealed, enum, or boolean cases. (--explain JV3100)",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "JV3101",
        title: "`when` branch is unreachable / 到達不能な `when` 分岐です",
        help: "前の分岐が同じケースを先に処理しているため、この分岐には到達しません。分岐を削除するか条件を調整してください。/ A preceding arm already matches this case. Remove the branch or refine its guard. (--explain JV3101)",
        severity: DiagnosticSeverity::Warning,
    },
    DiagnosticDescriptor {
        code: "JV3102",
        title: "Range pattern boundary type mismatch / 範囲パターンの境界型が不一致です",
        help: "範囲の上下限は同じ比較可能な型でなければなりません。境界を揃えるか guard へ書き換えてください。/ Range bounds must share a comparable type. Align the endpoints or rewrite the check as a guard. (--explain JV3102)",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "JV3103",
        title: "`if` expressions are not supported / `if` 式はサポートされていません",
        help: "条件分岐は `when` 式を使用してください。Quick Fix: when.convert.if. / Use a `when` expression for branching. Quick Fix: when.convert.if. (--explain JV3103)",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "JV3104",
        title: "Unused pattern binding / 未使用のパターン束縛です",
        help: "未使用の変数は削除するか `_` へ置き換えてください。/ Remove the binding or rename it to `_`. (--explain JV3104)",
        severity: DiagnosticSeverity::Warning,
    },
    DiagnosticDescriptor {
        code: "JV3105",
        title: "Pattern requires Java 25 target / パターンが Java 25 を要求しています",
        help: "Java 21 フォールバックではこのパターンを表現できません。`jv.toml` で target を 25 に上げるか、分岐を単純化してください。/ Java 21 fallback cannot represent this pattern. Raise the target to 25 in jv.toml or simplify the branch. (--explain JV3105)",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "JV3106",
        title: "`when` expression missing `else` branch / `when` 式に `else` 分岐が必要です",
        help: "値コンテキストでは `else` 分岐を追加して網羅性を確保してください。/ Add an `else` branch to cover the default case in value positions. (--explain JV3106)",
        severity: DiagnosticSeverity::Information,
    },
    DiagnosticDescriptor {
        code: "JV3107",
        title: "Overlapping range patterns detected / 範囲パターンが重複しています",
        help: "重複する範囲を統合するか順序を調整してください。/ Merge the overlapping ranges or adjust their order. (--explain JV3107)",
        severity: DiagnosticSeverity::Warning,
    },
    DiagnosticDescriptor {
        code: "JV3108",
        title: "`when` null branch contradicts non-null type / non-null 型と null 分岐が矛盾しています",
        help: "変数が non-null と推論されている場合、`null` 分岐は到達不能です。分岐を削除するか型を nullable へ変更してください。/ The value is inferred as non-null, so a `null` branch is unreachable. Remove the branch or update the type to be nullable. (--explain JV3108)",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "JV3109",
        title: "Pattern binding type inference failed / パターン束縛の型推論に失敗しました",
        help: "束縛に型注釈を追加するか、データクラスの宣言を更新してください。/ Annotate the bindings or update the data class declaration. (--explain JV3109)",
        severity: DiagnosticSeverity::Warning,
    },
    DiagnosticDescriptor {
        code: "JV3110",
        title: "Pattern analysis exceeded performance budget / パターン解析が性能予算を超過しました",
        help: "when 式を簡素化するか `pattern_cache` 設定を調整して再試行してください。/ Simplify the `when` expression or adjust pattern cache settings, then retry. (--explain JV3110)",
        severity: DiagnosticSeverity::Warning,
    },
    DiagnosticDescriptor {
        code: "E_WHEN_002",
        title: "`when` in value position requires `else` / 値コンテキストの`when`には`else`が必要",
        help: "Add an `else` branch or keep the `when` in a Unit context. See docs/language-guide.md#when-expression and docs/language-guide-en.md#when-expression.",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "E_LOOP_002",
        title: "Numeric range bounds must resolve to the same type",
        help: "Ensure both range endpoints share a compatible numeric type and that the loop binding matches that element type.",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "E_LOOP_003",
        title: "Loop target does not expose iterable semantics",
        help: "Provide a value implementing the iterable protocol (e.g., Sequence/Iterable) or convert the expression using `into_iter`.",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "JV1007",
        title: "配列リテラルの区切り記号が混在しています",
        help: "配列全体をカンマ区切りにするか、空白区切りに統一してください。コメントを挟む場合はカンマ区切りに戻すと安全です。",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "JV1008",
        title: "空白区切りの要素が同種ではありません",
        help: "空白区切りを使う場合は同じ型の要素だけを並べます。型が混在する場合はカンマ区切りに切り替えてください。",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "JV1009",
        title: "空白区切りの引数に不正な形式があります",
        help: "空白区切りの呼び出しでは位置引数のみを並べ、名前付き引数やカンマとの混在を避けてください。",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "JV2000",
        title: "jv.toml の Java target が不正です",
        help: "jv.toml の [build].java_version には 21 または 25 を指定してください。",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "JV3001",
        title: "null安全演算子の使用が不正です / Invalid null-safety operator usage",
        help: "`?.` や `!!` の適用対象を見直し、null チェックの順序を修正してください。/ Review how `?.` and `!!` are applied and add explicit null checks before dereferencing.",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "JV3002",
        title: "null 参照の可能性があります / Potential null dereference detected",
        help: "対象値を `?.` でガードするか、null を排除する条件分岐を追加してください。/ Guard the value with `?.` or insert a branch that excludes null before use.",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "JV3003",
        title: "null ケースが分岐で未処理です / Null case is not covered in control flow",
        help: "`when` や `if` の分岐に null ケースを追加し、全経路で値を初期化してください。/ Add a null branch to `when`/`if` so every execution path initialises the value.",
        severity: DiagnosticSeverity::Warning,
    },
    DiagnosticDescriptor {
        code: "JV3004",
        title: "null メタデータが不整合です / Nullability metadata is inconsistent",
        help: "生成対象の POJO 定義を再確認し、フィールドの型と null 属性を揃えてください。/ Align generated POJO field types with their declared nullability annotations before emitting code.",
        severity: DiagnosticSeverity::Warning,
    },
    DiagnosticDescriptor {
        code: "JV3005",
        title: "型推論情報が不足しています / Inference snapshot unavailable",
        help: "`cargo check` などで最新の型推論を実行し、DSL 境界に注釈を追加してください。/ Re-run inference (e.g., `cargo check`) and annotate DSL boundaries to restore full null-safety precision.",
        severity: DiagnosticSeverity::Information,
    },
    DiagnosticDescriptor {
        code: "JV3199",
        title: "Advanced pattern matching not yet supported / 高度なパターンマッチングは未対応",
        help: "深度3以上の分解パターンや複合ガードは段階的に提供されます。サポート済みの構文へ書き換えるか今後のアップデートをお待ちください。/ Deep destructuring (depth ≥ 3) and complex guards are still rolling out. Rewrite the pattern using supported constructs or wait for a forthcoming update. (--explain JV3199)",
        severity: DiagnosticSeverity::Error,
    },
];

/// 診断コードに対応するディスクリプタを取得します。
pub fn lookup(code: &str) -> Option<&'static DiagnosticDescriptor> {
    DIAGNOSTICS
        .iter()
        .chain(crate::compat::diagnostics::ENTRIES.iter())
        .find(|desc| desc.code == code)
}

/// パーサーのエラーからホワイトスペース関連診断を抽出します。
pub fn from_parse_error(error: &ParseError) -> Option<EnhancedDiagnostic> {
    match error {
        ParseError::Syntax { message, span } => detect_in_message(message, Some(span.clone())),
        _ => None,
    }
}

/// トランスフォーマのエラーからホワイトスペース関連診断を抽出します。
pub fn from_transform_error(error: &TransformError) -> Option<EnhancedDiagnostic> {
    match error {
        TransformError::WhitespaceSequenceTypeMismatch { span, .. } => {
            let message = error.to_string();
            detect_in_message(&message, Some(span.clone()))
        }
        _ => None,
    }
}

fn detect_in_message(message: &str, span: Option<Span>) -> Option<EnhancedDiagnostic> {
    let descriptor = DIAGNOSTICS
        .iter()
        .chain(crate::compat::diagnostics::ENTRIES.iter())
        .find(|descriptor| message.contains(descriptor.code))?;

    let (clean_message, suggestions, learning_hint) = extract_tooling_metadata(message);

    let mut diagnostic = EnhancedDiagnostic::new(descriptor, clean_message, span);
    if !suggestions.is_empty() {
        diagnostic = diagnostic.with_suggestions(suggestions);
    }
    if let Some(hint) = learning_hint {
        diagnostic = diagnostic.with_learning_hint(hint);
    }

    Some(diagnostic)
}

fn extract_tooling_metadata(message: &str) -> (String, Vec<String>, Option<String>) {
    let mut suggestions = Vec::new();
    let mut learning = Vec::new();
    let mut cleaned_lines = Vec::new();

    for line in message.lines() {
        if let Some(index) = line.find("Quick Fix:") {
            let (prefix, suffix) = line.split_at(index);
            let quick_fix = suffix.trim_start_matches("Quick Fix:").trim();
            if !quick_fix.is_empty() {
                suggestions.push(format!("Quick Fix: {}", quick_fix));
            }
            let trimmed_prefix = prefix.trim_end();
            if !trimmed_prefix.is_empty() {
                cleaned_lines.push(trimmed_prefix.to_string());
            }
            continue;
        }

        if let Some(index) = line.find("--explain ") {
            let explain = line[index..].trim();
            if !explain.is_empty() {
                learning.push(explain.to_string());
            }
            let trimmed_prefix = line[..index].trim_end();
            if !trimmed_prefix.is_empty() {
                cleaned_lines.push(trimmed_prefix.to_string());
            }
            continue;
        }

        cleaned_lines.push(line.to_string());
    }

    let cleaned_message = cleaned_lines
        .into_iter()
        .map(|line| line.trim_end().to_string())
        .filter(|line| !line.is_empty())
        .collect::<Vec<_>>()
        .join("\n");

    let learning_hint = if learning.is_empty() {
        None
    } else {
        Some(learning.join("\n"))
    };

    (cleaned_message, suggestions, learning_hint)
}

#[cfg(test)]
mod tests {
    use super::extract_tooling_metadata;

    #[test]
    fn extract_metadata_captures_quick_fix_and_learning_hint() {
        let message = "JV3103: sample message\nQuick Fix: when.convert.if -> template\n--explain JV3103: Additional guidance";
        let (clean, suggestions, hint) = extract_tooling_metadata(message);
        assert_eq!(clean, "JV3103: sample message");
        assert_eq!(
            suggestions,
            vec!["Quick Fix: when.convert.if -> template".to_string()]
        );
        assert_eq!(
            hint.as_deref(),
            Some("--explain JV3103: Additional guidance")
        );
    }

    #[test]
    fn extract_metadata_preserves_non_metadata_lines() {
        let message = "JV3100: line a\nSecondary line\nQuick Fix: when.add -> foo";
        let (clean, suggestions, hint) = extract_tooling_metadata(message);
        assert_eq!(clean, "JV3100: line a\nSecondary line");
        assert_eq!(suggestions, vec!["Quick Fix: when.add -> foo".to_string()]);
        assert!(hint.is_none());
    }
}

pub fn from_check_error(error: &CheckError) -> Option<EnhancedDiagnostic> {
    match error {
        CheckError::TypeError(message)
        | CheckError::NullSafetyError(message)
        | CheckError::UndefinedVariable(message)
        | CheckError::SyntaxError(message) => detect_in_message(message, None),
        CheckError::ValidationError { message, span } => detect_in_message(message, span.clone()),
    }
}
