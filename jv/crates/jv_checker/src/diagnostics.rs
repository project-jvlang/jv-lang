pub mod messages;

use crate::CheckError;
use jv_ast::{Span, types::RawTypeContinuation};
use jv_ir::{
    error::TransformError,
    types::{IrProgram, IrStatement},
};
use jv_lexer::LexError;
use jv_parser_frontend::{
    Diagnostic as FrontendDiagnostic, DiagnosticSeverity as FrontendSeverity, ParseError,
};

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

pub fn descriptor(code: &str) -> Option<&'static DiagnosticDescriptor> {
    DIAGNOSTICS
        .iter()
        .chain(crate::inference::diagnostics::CONVERSION_DIAGNOSTICS.iter())
        .chain(crate::compat::diagnostics::ENTRIES.iter())
        .chain(crate::imports::diagnostics::ENTRIES.iter())
        .find(|descriptor| descriptor.code == code)
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
        code: "JV2101",
        title: "配列リテラルでカンマ区切りは使用できません / Array literals cannot use comma separators",
        help: "要素は空白または改行で区切ってください。examples: docs/whitespace-arrays.md。/ Separate elements with whitespace or newlines. See docs/whitespace-arrays.md for examples. (--explain JV2101)",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "JV2102",
        title: "関数呼び出しでカンマ区切りは使用できません / Function calls cannot use comma separators",
        help: "位置引数は空白区切りで並べ、必要ならカンマ区切りに戻してください。/ Use whitespace between positional arguments or revert to comma-separated form. See docs/whitespace-arrays.md#function-calls. (--explain JV2102)",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "JV4201",
        title: "不変変数に再代入できません / Cannot reassign immutable binding",
        help: "`var` で宣言されていない変数には再代入できません。再代入が必要な場合は `var` へ変更するか、新しい名前の変数を導入してください。/ Reassignment is not allowed for immutable bindings. Use `var` when mutation is required or introduce a new binding name.",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "JV4202",
        title: "暗黙宣言には初期化子が必要です / Implicit binding requires an initializer",
        help: "`identifier = value` の形式で初期化子を指定してください。同じ名前を右辺で参照することはできません。/ Provide an initializer when introducing an implicit binding (use `identifier = value`). The right-hand side cannot reference the binding being declared.",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "JV4300",
        title: "生文字列リテラルが未終端です / Raw string literal is unterminated",
        help: "対応する閉じクォート（' または '''）を追加して生文字列を終了してください。/ Add the matching closing quote sequence (' or ''') to finish the raw string literal.",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "JV4301",
        title: "1文字リテラルの型が曖昧です / Single-character literal has ambiguous type",
        help: "Char か String のどちらとして扱うか型注釈や補助メソッドで明示してください。/ Disambiguate whether the literal should be treated as Char or String via annotations or helper calls.",
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
        title: "空白区切りの引数に不正な形式があります / Invalid whitespace-delimited call arguments",
        help: "空白区切りの呼び出しでは名前付き引数を使用せず、必要ならカンマ区切りへ戻してください。/ Avoid named arguments in whitespace-delimited calls or revert to comma-separated syntax. (--explain JV1009)",
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
        code: "JV3201",
        title: "型トークンを生成できません / Unable to synthesise type token",
        help: "対象のジェネリクス型に対する実行時トークンを生成できません。型注釈を追加するか API を調整してください。/ The generic type requires an explicit runtime token. Provide an explicit annotation or refactor the API to supply a token. (--explain JV3201)",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "JV3202",
        title: "Raw型の使用を検出しました / Raw type usage detected",
        help: "`// jv:raw-allow <QualifiedName>` で意図を明示するか、欠落している型引数を補ってください。/ Either acknowledge the intent with `// jv:raw-allow <QualifiedName>` or supply the missing generic arguments to restore type safety.",
        severity: DiagnosticSeverity::Warning,
    },
    DiagnosticDescriptor {
        code: "JV3203",
        title: "Raw型継続コメント / Raw type continuation comment",
        help: "このコメントは防御策を最小限に抑えます。継続が妥当か再確認してください。/ This directive minimises defensive handling; review to ensure the raw continuation is acceptable.",
        severity: DiagnosticSeverity::Information,
    },
    DiagnosticDescriptor {
        code: "JV5101",
        title: "正規表現リテラルの構造が不正です / Regex literal structure is invalid",
        help: "終端スラッシュと括弧の対応を確認し、リテラル全体が閉じていることを確認してください。/ Ensure the literal closes with `/` and that all grouping brackets are balanced.",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "JV5102",
        title: "サポートされない正規表現エスケープ / Unsupported regex escape sequence",
        help: "Java互換のエスケープシーケンス (例: \\n, \\t, \\\\) へ置き換えてください。/ Replace the sequence with a Java-compatible escape such as \\n, \\t, or \\\\.",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "JV5103",
        title: "Java互換性が不確かな正規表現です / Regex may not behave identically on Java",
        help: "パターンを Java の `Pattern.compile` で検証し、互換性の高い構文へ書き換えてください。/ Validate the pattern with Java `Pattern.compile` or rewrite it using portable constructs.",
        severity: DiagnosticSeverity::Warning,
    },
    DiagnosticDescriptor {
        code: "JV5104",
        title: "正規表現検証が遅延しています / Regex validation is slow",
        help: "パターンを簡素化するか、部分的に分割して検証コストを抑えてください。/ Simplify the pattern or split it into smaller parts to reduce validation time.",
        severity: DiagnosticSeverity::Information,
    },
    DiagnosticDescriptor {
        code: "JV3199",
        title: "Advanced pattern matching not yet supported / 高度なパターンマッチングは未対応",
        help: "深度11以上の分解パターンや複合ガードは段階的に提供されます。サポート済みの構文へ書き換えるか今後のアップデートをお待ちください。/ Deep destructuring (depth ≥ 11) and complex guards are still rolling out. Rewrite the pattern using supported constructs or wait for a forthcoming update. (--explain JV3199)",
        severity: DiagnosticSeverity::Error,
    },
];

const FRONTEND_GENERIC_DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    code: "JVF000",
    title: "Frontend diagnostic / フロントエンド診断",
    help: "パーサーフロントエンドから報告された診断です。メッセージの詳細を確認してください。",
    severity: DiagnosticSeverity::Warning,
};

pub fn from_frontend_diagnostics(diagnostics: &[FrontendDiagnostic]) -> Vec<EnhancedDiagnostic> {
    diagnostics
        .iter()
        .map(frontend_diagnostic_to_tooling)
        .collect()
}

fn frontend_diagnostic_to_tooling(diagnostic: &FrontendDiagnostic) -> EnhancedDiagnostic {
    let message = diagnostic.message().to_string();
    let span = diagnostic.span().cloned();
    let severity = map_frontend_severity(diagnostic.severity());

    if let Some(code) = diagnostic.code() {
        if let Some(descriptor) = descriptor(code) {
            let mut tooling = EnhancedDiagnostic::new(descriptor, message, span);
            tooling.severity = severity;
            return tooling;
        }
    }

    let fallback_message = if let Some(code) = diagnostic.code() {
        format!("{code}: {message}")
    } else {
        message
    };
    let mut tooling = EnhancedDiagnostic::new(&FRONTEND_GENERIC_DESCRIPTOR, fallback_message, span);
    tooling.severity = severity;
    tooling
}

fn map_frontend_severity(severity: FrontendSeverity) -> DiagnosticSeverity {
    match severity {
        FrontendSeverity::Error => DiagnosticSeverity::Error,
        FrontendSeverity::Warning => DiagnosticSeverity::Warning,
        FrontendSeverity::Information => DiagnosticSeverity::Information,
    }
}

/// 診断コードに対応するディスクリプタを取得します。
pub fn lookup(code: &str) -> Option<&'static DiagnosticDescriptor> {
    DIAGNOSTICS
        .iter()
        .chain(crate::inference::diagnostics::CONVERSION_DIAGNOSTICS.iter())
        .chain(crate::compat::diagnostics::ENTRIES.iter())
        .chain(crate::imports::diagnostics::ENTRIES.iter())
        .find(|desc| desc.code == code)
}

/// パーサーのエラーからホワイトスペース関連診断を抽出します。
pub fn from_parse_error(error: &ParseError) -> Option<EnhancedDiagnostic> {
    match error {
        ParseError::Syntax { message, span } => detect_in_message(message, Some(span.clone())),
        ParseError::LexError(lex) => lex_error_to_diagnostic(lex),
        ParseError::UnexpectedEof { .. } => None,
    }
}

fn lex_error_to_diagnostic(error: &LexError) -> Option<EnhancedDiagnostic> {
    match error {
        LexError::UnterminatedRawString { line, column } => {
            let descriptor = lookup("JV4300")?;
            let message = format!(
                "JV4300: 生文字列リテラルが閉じられていません（{}行{}列）。' または ''' で閉じてください。\nJV4300: Raw string literal is unterminated at line {}, column {}. Add the matching closing quote sequence.",
                line, column, line, column
            );
            let span = Span::new(*line, *column, *line, column.saturating_add(1));
            Some(EnhancedDiagnostic::new(descriptor, message, Some(span)))
        }
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
        TransformError::UnsupportedConstruct { construct, span } => {
            detect_in_message(construct, Some(span.clone()))
                .or_else(|| detect_in_message(&error.to_string(), Some(span.clone())))
        }
        TransformError::InvalidPattern { message, span }
        | TransformError::NullSafetyError { message, span }
        | TransformError::ScopeError { message, span }
        | TransformError::DefaultParameterError { message, span }
        | TransformError::ExtensionFunctionError { message, span }
        | TransformError::ConcurrencyError { message, span }
        | TransformError::ResourceManagementError { message, span }
        | TransformError::SampleAnnotationError { message, span }
        | TransformError::SampleProcessingError { message, span }
        | TransformError::TypeInferenceError { message, span } => {
            detect_in_message(message, Some(span.clone()))
                .or_else(|| detect_in_message(&error.to_string(), Some(span.clone())))
        }
    }
}

fn detect_in_message(message: &str, span: Option<Span>) -> Option<EnhancedDiagnostic> {
    let descriptor = DIAGNOSTICS
        .iter()
        .chain(crate::inference::diagnostics::CONVERSION_DIAGNOSTICS.iter())
        .chain(crate::compat::diagnostics::ENTRIES.iter())
        .chain(crate::imports::diagnostics::ENTRIES.iter())
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

pub fn collect_raw_type_diagnostics(program: &IrProgram) -> Vec<EnhancedDiagnostic> {
    let mut diagnostics = Vec::new();

    for import in &program.imports {
        collect_statement_raw_diagnostics(import, &mut diagnostics);
    }

    for declaration in &program.type_declarations {
        collect_statement_raw_diagnostics(declaration, &mut diagnostics);
    }

    diagnostics
}

fn collect_statement_raw_diagnostics(
    statement: &IrStatement,
    diagnostics: &mut Vec<EnhancedDiagnostic>,
) {
    match statement {
        IrStatement::Commented {
            statement: inner,
            comment,
            comment_span,
            ..
        } => {
            if let Some((mode, owner)) = parse_raw_type_comment(comment) {
                if let Some(diagnostic) =
                    build_raw_type_diagnostic(mode, owner, comment_span.clone())
                {
                    diagnostics.push(diagnostic);
                }
            }
            collect_statement_raw_diagnostics(inner, diagnostics);
        }
        IrStatement::ClassDeclaration {
            fields,
            methods,
            nested_classes,
            ..
        } => {
            for field in fields {
                collect_statement_raw_diagnostics(field, diagnostics);
            }
            for method in methods {
                collect_statement_raw_diagnostics(method, diagnostics);
            }
            for class in nested_classes {
                collect_statement_raw_diagnostics(class, diagnostics);
            }
        }
        IrStatement::InterfaceDeclaration {
            methods,
            default_methods,
            fields,
            nested_types,
            ..
        } => {
            for method in methods {
                collect_statement_raw_diagnostics(method, diagnostics);
            }
            for method in default_methods {
                collect_statement_raw_diagnostics(method, diagnostics);
            }
            for field in fields {
                collect_statement_raw_diagnostics(field, diagnostics);
            }
            for ty in nested_types {
                collect_statement_raw_diagnostics(ty, diagnostics);
            }
        }
        IrStatement::RecordDeclaration { methods, .. } => {
            for method in methods {
                collect_statement_raw_diagnostics(method, diagnostics);
            }
        }
        IrStatement::Block { statements, .. } => {
            for stmt in statements {
                collect_statement_raw_diagnostics(stmt, diagnostics);
            }
        }
        IrStatement::If {
            then_stmt,
            else_stmt,
            ..
        } => {
            collect_statement_raw_diagnostics(then_stmt, diagnostics);
            if let Some(else_stmt) = else_stmt {
                collect_statement_raw_diagnostics(else_stmt, diagnostics);
            }
        }
        IrStatement::While { body, .. } | IrStatement::ForEach { body, .. } => {
            collect_statement_raw_diagnostics(body, diagnostics);
        }
        IrStatement::For { init, body, .. } => {
            if let Some(initializer) = init.as_deref() {
                collect_statement_raw_diagnostics(initializer, diagnostics);
            }
            collect_statement_raw_diagnostics(body, diagnostics);
        }
        IrStatement::Try {
            body,
            catch_clauses,
            finally_block,
            ..
        }
        | IrStatement::TryWithResources {
            body,
            catch_clauses,
            finally_block,
            ..
        } => {
            collect_statement_raw_diagnostics(body, diagnostics);
            for clause in catch_clauses {
                collect_statement_raw_diagnostics(&clause.body, diagnostics);
            }
            if let Some(finally_stmt) = finally_block {
                collect_statement_raw_diagnostics(finally_stmt, diagnostics);
            }
        }
        IrStatement::Comment { .. }
        | IrStatement::VariableDeclaration { .. }
        | IrStatement::SampleDeclaration(_)
        | IrStatement::MethodDeclaration { .. }
        | IrStatement::FieldDeclaration { .. }
        | IrStatement::Expression { .. }
        | IrStatement::Return { .. }
        | IrStatement::Switch { .. }
        | IrStatement::Throw { .. }
        | IrStatement::Break { .. }
        | IrStatement::Continue { .. }
        | IrStatement::Import(..)
        | IrStatement::Package { .. } => {}
    }
}

fn parse_raw_type_comment(comment: &str) -> Option<(RawTypeContinuation, String)> {
    let trimmed = comment.trim();
    let content = if let Some(rest) = trimmed.strip_prefix("//") {
        rest.trim_start_matches('*').trim()
    } else if let Some(rest) = trimmed.strip_prefix("/*") {
        rest.trim_end_matches("*/").trim()
    } else {
        trimmed
    };

    let (mode, payload) = if let Some(rest) = content.strip_prefix("jv:raw-allow") {
        (RawTypeContinuation::AllowWithComment, rest.trim())
    } else if let Some(rest) = content.strip_prefix("jv:raw-default") {
        (RawTypeContinuation::DefaultPolicy, rest.trim())
    } else {
        return None;
    };

    let owner_token = payload.split_whitespace().next().unwrap_or("");
    let normalized_owner = owner_token
        .split('.')
        .map(|segment| segment.trim())
        .filter(|segment| !segment.is_empty())
        .collect::<Vec<_>>()
        .join(".");

    if normalized_owner.is_empty() {
        return None;
    }

    Some((mode, normalized_owner))
}

fn build_raw_type_diagnostic(
    mode: RawTypeContinuation,
    owner: String,
    span: Span,
) -> Option<EnhancedDiagnostic> {
    let code = match mode {
        RawTypeContinuation::DefaultPolicy => "JV3202",
        RawTypeContinuation::AllowWithComment => "JV3203",
    };

    let descriptor = lookup(code)?;
    let message = match mode {
        RawTypeContinuation::DefaultPolicy => format!(
            "Raw型 `{owner}` を検出し、防御コードを挿入しました。ジェネリクス型を明示して警告を解消してください。/ Raw type `{owner}` detected; defensive guards were emitted. Provide explicit generics to address the warning."
        ),
        RawTypeContinuation::AllowWithComment => format!(
            "Raw型 `{owner}` はコメントによって継続されています。影響範囲を再確認してください。/ Raw type `{owner}` is continued via comment; verify that the trade-offs are acceptable."
        ),
    };

    let mut diagnostic = EnhancedDiagnostic::new(descriptor, message, Some(span));

    if matches!(mode, RawTypeContinuation::AllowWithComment) {
        diagnostic.severity = DiagnosticSeverity::Information;
    }

    Some(diagnostic)
}

#[cfg(test)]
mod tests {
    use super::{DiagnosticSeverity, collect_raw_type_diagnostics, extract_tooling_metadata};
    use jv_ast::{Literal, Span};
    use jv_ir::types::{
        IrCommentKind, IrExpression, IrModifiers, IrProgram, IrStatement, JavaType,
    };

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

    fn program_with_comment(comment: &str) -> IrProgram {
        let span = Span::new(1, 0, 1, comment.len().max(1));
        let statement = IrStatement::Commented {
            statement: Box::new(IrStatement::VariableDeclaration {
                name: "value".to_string(),
                java_type: JavaType::Primitive("int".to_string()),
                initializer: Some(IrExpression::Literal(
                    Literal::Number("1".to_string()),
                    None,
                    span.clone(),
                )),
                is_final: false,
                modifiers: IrModifiers::default(),
                span: span.clone(),
            }),
            comment: comment.to_string(),
            kind: IrCommentKind::Line,
            comment_span: span.clone(),
        };

        IrProgram {
            package: None,
            imports: Vec::new(),
            type_declarations: vec![statement],
            generic_metadata: Default::default(),
            conversion_metadata: Vec::new(),
            span,
        }
    }

    #[test]
    fn collect_raw_type_diagnostics_detects_default_policy() {
        let program = program_with_comment("// jv:raw-default demo.Value");
        let diagnostics = collect_raw_type_diagnostics(&program);
        assert_eq!(diagnostics.len(), 1);
        let diagnostic = &diagnostics[0];
        assert_eq!(diagnostic.code, "JV3202");
        assert_eq!(diagnostic.severity, DiagnosticSeverity::Warning);
        assert!(
            diagnostic.message.contains("demo.Value"),
            "diagnostic message should mention owner"
        );
    }

    #[test]
    fn collect_raw_type_diagnostics_detects_allow_comment() {
        let program = program_with_comment("// jv:raw-allow demo.Value");
        let diagnostics = collect_raw_type_diagnostics(&program);
        assert_eq!(diagnostics.len(), 1);
        let diagnostic = &diagnostics[0];
        assert_eq!(diagnostic.code, "JV3203");
        assert_eq!(diagnostic.severity, DiagnosticSeverity::Information);
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
