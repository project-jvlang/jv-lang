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
        code: "E_COND_001",
        title: "`if` expressions are not available / `if` 式は廃止されました",
        help: "Use `when` for branching. See docs/language-guide.md#when-expression and docs/language-guide-en.md#when-expression.",
        severity: DiagnosticSeverity::Error,
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

    Some(EnhancedDiagnostic::new(
        descriptor,
        message.to_string(),
        span,
    ))
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
