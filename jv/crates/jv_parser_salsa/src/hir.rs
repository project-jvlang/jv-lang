use crate::lower::LoweringDiagnostic;
use jv_ast::{Expression, Span, Statement};

/// HIR ファイル全体を表すコンテナ。
#[derive(Debug, Clone, PartialEq)]
pub struct HirFile {
    pub statements: Vec<Statement>,
    #[allow(dead_code)]
    pub expressions: Vec<Expression>,
    pub diagnostics: Vec<LoweringDiagnostic>,
    pub source_map: HirSourceMap,
}

impl HirFile {
    pub fn new(
        statements: Vec<Statement>,
        diagnostics: Vec<LoweringDiagnostic>,
        token_spans: Vec<Span>,
    ) -> Self {
        Self {
            statements,
            expressions: Vec::new(),
            diagnostics,
            source_map: HirSourceMap { token_spans },
        }
    }
}

impl Eq for HirFile {}

/// トークンと AST スパンの対応情報。
#[derive(Debug, Clone, PartialEq)]
pub struct HirSourceMap {
    pub token_spans: Vec<Span>,
}
