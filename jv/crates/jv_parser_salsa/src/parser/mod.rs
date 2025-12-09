//! 再帰下降パーサーのエントリポイント。
//!
//! Phase 3 のタスクに従い、salsa パーサーの構造を段階的に組み立てる。

use crate::lexer::{Span, TokenDiagnostic, TokenKind, TokenMetadata, TokenTrivia};

mod context;
pub mod cst;
mod expression;
mod recovery;
pub mod strategies;
#[cfg(test)]
mod tests;

pub use context::ParserContext;
pub use recovery::SYNC_TOKENS;

/// 解析結果で使用する構文種別。
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SyntaxKind {
    Root,
    StatementList,
    PackageDeclaration,
    ImportDeclaration,
    ImportPath,
    ImportClause,
    ImportAlias,
    ValDeclaration,
    VarDeclaration,
    DestructuringPattern,
    PatternElement,
    FunctionDeclaration,
    ClassDeclaration,
    DataClassDeclaration,
    ClassBody,
    ClassParameterList,
    ClassParameter,
    Modifier,
    IfStatement,
    WhenStatement,
    WhenBranch,
    ForStatement,
    WhileStatement,
    ReturnStatement,
    BreakStatement,
    ContinueStatement,
    AssignmentStatement,
    UseStatement,
    DeferStatement,
    SpawnStatement,
    AsyncStatement,
    TestDeclaration,
    Block,
    UnitTypeDefinition,
    UnitCategory,
    Expression,
    Identifier,
    Error,
    Token(TokenKind),
}

impl From<TokenKind> for SyntaxKind {
    fn from(kind: TokenKind) -> Self {
        SyntaxKind::Token(kind)
    }
}

/// 構文解析イベント。
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ParseEvent {
    /// 新しいノードを開始する。
    StartNode { kind: SyntaxKind },
    /// 現在のノードを終了する。
    FinishNode,
    /// トークンを追加する。
    Token {
        /// トークン種別。
        kind: SyntaxKind,
        /// トークン配列内のインデックス。
        token_index: usize,
    },
    /// パーサ内部で発生したエラー。
    Error {
        /// 診断メッセージ。
        message: String,
        /// 対象範囲。
        span: TokenSpan,
    },
}

/// 診断の深刻度。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DiagnosticSeverity {
    /// エラー。
    Error,
    /// 警告。
    Warning,
}

/// パーサ診断情報。
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParserDiagnostic {
    /// メッセージ本文。
    pub message: String,
    /// 深刻度。
    pub severity: DiagnosticSeverity,
    /// 対象トークン範囲。
    pub span: TokenSpan,
}

impl ParserDiagnostic {
    /// 新しい診断を生成する。
    pub fn new(message: impl Into<String>, severity: DiagnosticSeverity, span: TokenSpan) -> Self {
        Self {
            message: message.into(),
            severity,
            span,
        }
    }
}

/// トークン範囲（開始≦idx＜終了）。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TokenSpan {
    /// 開始トークンインデックス。
    pub start: usize,
    /// 終了トークンインデックス。
    pub end: usize,
}

impl TokenSpan {
    /// 範囲を作成する。
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OwnedToken {
    pub kind: TokenKind,
    pub span: Span,
    pub lexeme: String,
    pub leading_trivia: TokenTrivia,
    pub metadata: Vec<TokenMetadata>,
    pub diagnostic: Option<TokenDiagnostic>,
}

/// 構文解析結果。
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParseOutput {
    /// 発行されたイベント列。
    pub events: Vec<ParseEvent>,
    /// 解析中に発生した診断。
    pub diagnostics: Vec<ParserDiagnostic>,
    /// エラー回復が発生したか。
    pub recovered: bool,
}

impl ParseOutput {
    fn new(events: Vec<ParseEvent>, diagnostics: Vec<ParserDiagnostic>, recovered: bool) -> Self {
        Self {
            events,
            diagnostics,
            recovered,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParseResult {
    /// 解析対象となったトークン列。
    pub tokens: Vec<OwnedToken>,
    /// 構文解析イベント出力。
    pub output: ParseOutput,
    /// レキサー段階でのエラー（存在する場合）。
    pub errors: Vec<String>,
}

/// トークン列からイベント列を生成する。
pub fn parse(tokens: Vec<OwnedToken>) -> ParseResult {
    let mut ctx = ParserContext::new(tokens.clone());
    let output = ctx.parse();
    ParseResult {
        tokens,
        output,
        errors: Vec::new(),
    }
}
