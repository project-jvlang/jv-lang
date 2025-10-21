use super::helpers::{
    collect_annotation_texts, first_identifier_text, JvSyntaxNode, LoweringContext,
};
use crate::syntax::SyntaxKind;
use jv_ast::Statement;
use jv_lexer::Token;

/// ローワリング結果。
#[derive(Debug)]
pub struct LoweringResult {
    /// 正常にローワリングされたステートメント列。
    pub statements: Vec<Statement>,
    /// ローワリング中に発生した診断。
    pub diagnostics: Vec<LoweringDiagnostic>,
}

impl LoweringResult {
    /// 診断が存在しないか判定する。
    pub fn is_success(&self) -> bool {
        self.diagnostics.is_empty()
    }
}

/// ローワリング診断の深刻度。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LoweringDiagnosticSeverity {
    /// エラー。
    Error,
    /// 警告。
    Warning,
}

/// ローワリング診断情報。
#[derive(Debug, Clone)]
pub struct LoweringDiagnostic {
    /// メッセージ本文。
    pub message: String,
    /// 深刻度。
    pub severity: LoweringDiagnosticSeverity,
    /// 対象スパン。
    pub span: Option<jv_ast::Span>,
    /// 対象ノード種別。
    pub node_kind: SyntaxKind,
    /// 同定に利用できる識別子（存在する場合）。
    pub identifier: Option<String>,
    /// 関連するアノテーションテキスト。
    pub annotations: Vec<String>,
}

impl LoweringDiagnostic {
    fn new(
        severity: LoweringDiagnosticSeverity,
        message: impl Into<String>,
        span: Option<jv_ast::Span>,
        node_kind: SyntaxKind,
        identifier: Option<String>,
        annotations: Vec<String>,
    ) -> Self {
        Self {
            message: message.into(),
            severity,
            span,
            node_kind,
            identifier,
            annotations,
        }
    }
}

/// Rowan構文木からASTステートメントを生成する。
pub fn lower_program(root: &JvSyntaxNode, tokens: &[Token]) -> LoweringResult {
    let context = LoweringContext::new(tokens);
    let mut statements = Vec::new();
    let mut diagnostics = Vec::new();

    for child in root.children() {
        if child.kind().is_token() {
            continue;
        }

        if matches!(child.kind(), SyntaxKind::Error | SyntaxKind::BlockError) {
            let diagnostic = LoweringDiagnostic::new(
                LoweringDiagnosticSeverity::Error,
                "Rowanパーサでエラーノードが生成されました",
                context.span_for(&child),
                child.kind(),
                first_identifier_text(&child),
                collect_annotation_texts(&child),
            );
            diagnostics.push(diagnostic);
            continue;
        }

        if !is_top_level_statement(child.kind()) {
            continue;
        }

        match lower_single_statement(&context, &child) {
            Ok(statement) => statements.push(statement),
            Err(diagnostic) => diagnostics.push(diagnostic),
        }
    }

    LoweringResult {
        statements,
        diagnostics,
    }
}

fn is_top_level_statement(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::PackageDeclaration
            | SyntaxKind::ImportDeclaration
            | SyntaxKind::ValDeclaration
            | SyntaxKind::VarDeclaration
            | SyntaxKind::FunctionDeclaration
            | SyntaxKind::ClassDeclaration
            | SyntaxKind::IfStatement
            | SyntaxKind::WhenStatement
            | SyntaxKind::ForStatement
            | SyntaxKind::WhileStatement
            | SyntaxKind::DoWhileStatement
            | SyntaxKind::ReturnStatement
            | SyntaxKind::ThrowStatement
            | SyntaxKind::BreakStatement
            | SyntaxKind::ContinueStatement
            | SyntaxKind::Expression
    )
}

fn lower_single_statement(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
) -> Result<Statement, LoweringDiagnostic> {
    if context.tokens_for(node).is_empty() {
        return Err(LoweringDiagnostic::new(
            LoweringDiagnosticSeverity::Error,
            "Rowanノードに対応するトークン列が空です",
            None,
            node.kind(),
            first_identifier_text(node),
            collect_annotation_texts(node),
        ));
    }

    Err(LoweringDiagnostic::new(
        LoweringDiagnosticSeverity::Error,
        format!(
            "Rowanノード {:?} のローワリングはまだ実装されていません",
            node.kind()
        ),
        context.span_for(node),
        node.kind(),
        first_identifier_text(node),
        collect_annotation_texts(node),
    ))
}
