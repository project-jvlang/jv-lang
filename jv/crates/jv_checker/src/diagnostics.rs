use jv_ast::Span;
use jv_ir::error::TransformError;
use jv_parser::ParseError;

/// 診断コードと補修ガイダンスをまとめたディスクリプタ。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DiagnosticDescriptor {
    pub code: &'static str,
    pub title: &'static str,
    pub help: &'static str,
}

/// CLI/LSP/フォーマッタが共有する診断情報。
#[derive(Debug, Clone)]
pub struct ToolingDiagnostic {
    pub code: &'static str,
    pub title: &'static str,
    pub message: String,
    pub help: &'static str,
    pub span: Option<Span>,
}

const DIAGNOSTICS: &[DiagnosticDescriptor] = &[
    DiagnosticDescriptor {
        code: "JV1007",
        title: "配列リテラルの区切り記号が混在しています",
        help: "配列全体をカンマ区切りにするか、空白区切りに統一してください。コメントを挟む場合はカンマ区切りに戻すと安全です。",
    },
    DiagnosticDescriptor {
        code: "JV1008",
        title: "空白区切りの要素が同種ではありません",
        help: "空白区切りを使う場合は同じ型の要素だけを並べます。型が混在する場合はカンマ区切りに切り替えてください。",
    },
    DiagnosticDescriptor {
        code: "JV1009",
        title: "空白区切りの引数に不正な形式があります",
        help: "空白区切りの呼び出しでは位置引数のみを並べ、名前付き引数やカンマとの混在を避けてください。",
    },
];

/// 診断コードに対応するディスクリプタを取得します。
pub fn lookup(code: &str) -> Option<&'static DiagnosticDescriptor> {
    DIAGNOSTICS.iter().find(|desc| desc.code == code)
}

/// パーサーのエラーからホワイトスペース関連診断を抽出します。
pub fn from_parse_error(error: &ParseError) -> Option<ToolingDiagnostic> {
    match error {
        ParseError::Syntax { message, span } => detect_in_message(message, Some(span.clone())),
        _ => None,
    }
}

/// トランスフォーマのエラーからホワイトスペース関連診断を抽出します。
pub fn from_transform_error(error: &TransformError) -> Option<ToolingDiagnostic> {
    match error {
        TransformError::WhitespaceSequenceTypeMismatch { span, .. } => {
            let message = error.to_string();
            detect_in_message(&message, Some(span.clone()))
        }
        _ => None,
    }
}

fn detect_in_message(message: &str, span: Option<Span>) -> Option<ToolingDiagnostic> {
    let code = DIAGNOSTICS
        .iter()
        .find(|descriptor| message.contains(descriptor.code))?;

    Some(ToolingDiagnostic {
        code: code.code,
        title: code.title,
        message: message.to_string(),
        help: code.help,
        span,
    })
}
