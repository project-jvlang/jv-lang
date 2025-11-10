use jv_ast::{Expression, Span, Statement};
use serde::Serialize;

use super::fixtures::{
    BlockExpectation, ExpectationSpec, SpanExpectation, StatementExpectation, StatementKindKey,
};
use crate::lowering::LoweringDiagnostic;
use crate::parser::{DiagnosticSeverity, ParserDiagnostic};
use crate::support::spans::statement_span;

/// 検証違反。
#[derive(Debug, Clone, Serialize)]
pub struct RuleViolation {
    /// 違反が発生した規則名。
    pub rule: String,
    /// 詳細メッセージ。
    pub message: String,
}

/// AST ステートメントのスナップショット。
#[derive(Debug, Clone, Serialize)]
pub struct StatementSummary {
    /// ステートメント種別。
    pub kind: StatementKindKey,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// ステートメント名（存在する場合）。
    pub name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// ステートメントがカバーするスパン。
    pub span: Option<Span>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    /// ブロック本文に含まれるステートメント種別の列。
    pub block_statement_kinds: Vec<StatementKindKey>,
}

impl StatementSummary {
    /// `Statement` からサマリ情報を生成する。
    pub fn from_statement(stmt: &Statement) -> Self {
        let kind = StatementKindKey::from_statement(stmt);
        let span = statement_span(stmt);
        let name = statement_name(stmt);
        let block_statement_kinds = match stmt {
            Statement::FunctionDeclaration { body, .. } => block_statement_kinds(body),
            Statement::ForIn(for_in) => block_statement_kinds(&for_in.body),
            _ => Vec::new(),
        };

        Self {
            kind,
            name,
            span: Some(span),
            block_statement_kinds,
        }
    }
}

fn block_statement_kinds(body: &Expression) -> Vec<StatementKindKey> {
    match body {
        Expression::Block { statements, .. } => statements
            .iter()
            .map(StatementKindKey::from_statement)
            .collect(),
        _ => Vec::new(),
    }
}

/// 診断情報のサマリ。
#[derive(Debug, Clone, Serialize)]
pub struct ParserDiagnosticSummary {
    /// 診断メッセージ。
    pub message: String,
    /// 深刻度（`error`/`warning`）。
    pub severity: String,
    /// 開始トークンインデックス。
    pub span_start: usize,
    /// 終了トークンインデックス。
    pub span_end: usize,
}

impl From<&ParserDiagnostic> for ParserDiagnosticSummary {
    fn from(diag: &ParserDiagnostic) -> Self {
        Self {
            message: diag.message.clone(),
            severity: match diag.severity {
                DiagnosticSeverity::Error => "error".to_string(),
                DiagnosticSeverity::Warning => "warning".to_string(),
            },
            span_start: diag.span.start,
            span_end: diag.span.end,
        }
    }
}

/// ローワリング診断のサマリ。
#[derive(Debug, Clone, Serialize)]
pub struct LoweringDiagnosticSummary {
    /// 診断メッセージ。
    pub message: String,
    /// 深刻度。
    pub severity: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// 対象スパン。
    pub span: Option<Span>,
    /// 対象ノード種別。
    pub node_kind: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// 関連する識別子（存在する場合）。
    pub identifier: Option<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    /// 付随するアノテーションテキスト。
    pub annotations: Vec<String>,
}

impl From<&LoweringDiagnostic> for LoweringDiagnosticSummary {
    fn from(diag: &LoweringDiagnostic) -> Self {
        Self {
            message: diag.message.clone(),
            severity: format!("{:?}", diag.severity).to_lowercase(),
            span: diag.span.clone(),
            node_kind: format!("{:?}", diag.node_kind),
            identifier: diag.identifier.clone(),
            annotations: diag.annotations.clone(),
        }
    }
}

/// 期待値を適用し違反を収集する。
pub fn apply_expectations(
    expect: &ExpectationSpec,
    statements: &[Statement],
) -> Vec<RuleViolation> {
    let mut violations = Vec::new();

    if let Some(expected) = expect.statement_count {
        if statements.len() != expected {
            violations.push(RuleViolation {
                rule: "statement_count".to_string(),
                message: format!(
                    "expected {} statement(s) but found {}",
                    expected,
                    statements.len()
                ),
            });
        }
    }

    for stmt_expect in &expect.statements {
        violations.extend(apply_statement_expectation(stmt_expect, statements));
    }

    violations
}

fn apply_statement_expectation(
    expect: &StatementExpectation,
    statements: &[Statement],
) -> Vec<RuleViolation> {
    let mut violations = Vec::new();
    let Some(statement) = statements.get(expect.index) else {
        violations.push(RuleViolation {
            rule: format!("statement[{}]", expect.index),
            message: "statement not found".to_string(),
        });
        return violations;
    };

    let actual_kind = StatementKindKey::from_statement(statement);
    if actual_kind != expect.kind {
        violations.push(RuleViolation {
            rule: format!("statement[{}].kind", expect.index),
            message: format!("expected {:?} but found {:?}", expect.kind, actual_kind),
        });
        return violations;
    }

    if let Some(expected_name) = &expect.name {
        match statement_name(statement) {
            Some(actual_name) if actual_name == *expected_name => {}
            Some(actual_name) => violations.push(RuleViolation {
                rule: format!("statement[{}].name", expect.index),
                message: format!(
                    "expected name '{}' but found '{}'",
                    expected_name, actual_name
                ),
            }),
            None => violations.push(RuleViolation {
                rule: format!("statement[{}].name", expect.index),
                message: format!(
                    "expected name '{}' but statement has no name",
                    expected_name
                ),
            }),
        }
    }

    if let Some(span_expectation) = &expect.span {
        violations.extend(check_span(expect.index, span_expectation, statement));
    }

    if let Some(block_expectation) = &expect.block {
        violations.extend(check_block(expect.index, block_expectation, statement));
    }

    violations
}

fn check_span(index: usize, expect: &SpanExpectation, statement: &Statement) -> Vec<RuleViolation> {
    let mut violations = Vec::new();
    let span = statement_span(statement);

    if let Some(expected) = expect.start_line {
        if span.start_line != expected {
            violations.push(RuleViolation {
                rule: format!("statement[{}].span.start_line", index),
                message: format!("expected {} but found {}", expected, span.start_line),
            });
        }
    }

    if let Some(expected) = expect.start_column {
        if span.start_column != expected {
            violations.push(RuleViolation {
                rule: format!("statement[{}].span.start_column", index),
                message: format!("expected {} but found {}", expected, span.start_column),
            });
        }
    }

    if let Some(expected) = expect.end_line {
        if span.end_line != expected {
            violations.push(RuleViolation {
                rule: format!("statement[{}].span.end_line", index),
                message: format!("expected {} but found {}", expected, span.end_line),
            });
        }
    }

    if let Some(expected) = expect.end_column {
        if span.end_column != expected {
            violations.push(RuleViolation {
                rule: format!("statement[{}].span.end_column", index),
                message: format!("expected {} but found {}", expected, span.end_column),
            });
        }
    }

    violations
}

fn check_block(
    index: usize,
    expect: &BlockExpectation,
    statement: &Statement,
) -> Vec<RuleViolation> {
    let mut violations = Vec::new();

    let body_expr = match statement {
        Statement::FunctionDeclaration { body, .. } => body.as_ref(),
        Statement::ForIn(for_in) => for_in.body.as_ref(),
        _ => {
            violations.push(RuleViolation {
                rule: format!("statement[{}].block", index),
                message:
                    "block expectation currently applies to function declarations or for-in loops"
                        .to_string(),
            });
            return violations;
        }
    };

    match body_expr {
        Expression::Block { statements, .. } => {
            let kinds: Vec<StatementKindKey> = statements
                .iter()
                .map(StatementKindKey::from_statement)
                .collect();
            if kinds != expect.statement_kinds {
                violations.push(RuleViolation {
                    rule: format!("statement[{}].block.statement_kinds", index),
                    message: format!(
                        "expected {:?} but found {:?}",
                        expect.statement_kinds, kinds
                    ),
                });
            }
        }
        _ => {
            violations.push(RuleViolation {
                rule: format!("statement[{}].block", index),
                message: "expected block body but found non-block expression".to_string(),
            });
        }
    }

    violations
}

fn statement_name(statement: &Statement) -> Option<String> {
    match statement {
        Statement::ValDeclaration { name, .. } => Some(name.clone()),
        Statement::VarDeclaration { name, .. } => Some(name.clone()),
        Statement::FunctionDeclaration { name, .. } => Some(name.clone()),
        Statement::ClassDeclaration { name, .. } => Some(name.clone()),
        Statement::DataClassDeclaration { name, .. } => Some(name.clone()),
        Statement::InterfaceDeclaration { name, .. } => Some(name.clone()),
        Statement::ExtensionFunction(ext) => match ext.function.as_ref() {
            Statement::FunctionDeclaration { name, .. } => Some(name.clone()),
            other => statement_name(other),
        },
        Statement::Package { name, .. } => Some(name.clone()),
        Statement::Import { path, .. } => Some(path.clone()),
        Statement::ForIn(for_in) => Some(for_in.binding.name.clone()),
        Statement::Assignment { target, .. } => Some(format!("{target:?}")),
        Statement::UnitTypeDefinition(definition) => Some(definition.name.name.clone()),
        _ => None,
    }
}

impl StatementKindKey {
    /// `jv_ast::Statement` からキーを特定する。
    pub fn from_statement(statement: &Statement) -> Self {
        match statement {
            Statement::Assignment { .. } => StatementKindKey::Assignment,
            Statement::Break(_) => StatementKindKey::BreakStatement,
            Statement::ClassDeclaration { .. } => StatementKindKey::ClassDeclaration,
            Statement::Comment(_) => StatementKindKey::Comment,
            Statement::Concurrency(_) => StatementKindKey::Concurrency,
            Statement::Continue(_) => StatementKindKey::ContinueStatement,
            Statement::DataClassDeclaration { .. } => StatementKindKey::DataClassDeclaration,
            Statement::ExtensionFunction(_) => StatementKindKey::ExtensionFunction,
            Statement::Expression { .. } => StatementKindKey::Expression,
            Statement::ForIn(_) => StatementKindKey::ForIn,
            Statement::FunctionDeclaration { .. } => StatementKindKey::FunctionDeclaration,
            Statement::Import { .. } => StatementKindKey::Import,
            Statement::InterfaceDeclaration { .. } => StatementKindKey::InterfaceDeclaration,
            Statement::Package { .. } => StatementKindKey::Package,
            Statement::ResourceManagement(_) => StatementKindKey::ResourceManagement,
            Statement::Return { .. } => StatementKindKey::ReturnStatement,
            Statement::Throw { .. } => StatementKindKey::ThrowStatement,
            Statement::ValDeclaration { .. } => StatementKindKey::ValDeclaration,
            Statement::VarDeclaration { .. } => StatementKindKey::VarDeclaration,
            Statement::UnitTypeDefinition(_) => StatementKindKey::UnitTypeDefinition,
        }
    }
}
