//! 正規表現に起因する `Pattern` 型推論を管理する補助モジュール。
//!
//! `TypeExpectation` による期待型の伝搬と、実際に `Pattern` 型として扱うべき
//! 正規表現式（リテラル／コマンド）を照合することで、宣言・戻り値・プロパティ
//! などのシグネチャに一貫した型情報を届ける。

use crate::inference::environment::TypeEnvironment;
use crate::inference::types::TypeKind;
use crate::pattern::expression_span;
use jv_ast::{Expression, PatternOrigin, RegexCommand, RegexLiteral, Span, Statement};

const JAVA_PATTERN_FQCN: &str = "java.util.regex.Pattern";

/// `Pattern` 型として解釈された式のメタデータ。
#[derive(Debug, Clone, PartialEq)]
pub struct PatternTypeBinding {
    pub span: Span,
    pub origin: PatternOrigin,
    pub ty: TypeKind,
}

impl PatternTypeBinding {
    pub fn new(span: Span, origin: PatternOrigin, ty: TypeKind) -> Self {
        Self { span, origin, ty }
    }
}

/// 期待される型情報。
#[derive(Debug, Clone, PartialEq)]
pub struct TypeExpectation {
    pub span: Span,
    pub expected: TypeKind,
}

impl TypeExpectation {
    pub fn new(span: Span, expected: TypeKind) -> Self {
        Self { span, expected }
    }
}

/// `Pattern` 型推論を補助するバインダ。
pub struct PatternTypeBinder<'env> {
    env: &'env mut TypeEnvironment,
}

impl<'env> PatternTypeBinder<'env> {
    /// 環境への参照を受け取って初期化する。
    pub fn new(env: &'env mut TypeEnvironment) -> Self {
        Self { env }
    }

    /// 正規表現リテラルを `Pattern` 型として束縛する。
    pub fn bind_literal(&mut self, literal: &RegexLiteral) -> TypeKind {
        let ty = Self::pattern_type();
        let origin = literal
            .origin
            .clone()
            .unwrap_or_else(|| PatternOrigin::literal(literal.span.clone()));
        let binding = PatternTypeBinding::new(literal.span.clone(), origin, ty.clone());
        self.env.push_pattern_type_binding(binding);
        ty
    }

    /// 推論済みの `RegexCommand` 戻り値に、型期待値に応じた調整を行う。
    pub fn resolve_command_type(&mut self, command: &RegexCommand, inferred: TypeKind) -> TypeKind {
        if self
            .env
            .has_type_expectation(&command.span, &Self::pattern_type())
        {
            let ty = Self::pattern_type();
            let origin = command
                .pattern
                .origin
                .clone()
                .unwrap_or_else(|| PatternOrigin::command(command.span.clone()));
            let binding = PatternTypeBinding::new(command.span.clone(), origin, ty.clone());
            self.env.push_pattern_type_binding(binding);
            ty
        } else {
            inferred
        }
    }

    /// 期待される型から、対象式にパターン型期待を仕込む。
    pub fn seed_expectation(env: &mut TypeEnvironment, expected: &TypeKind, expr: &Expression) {
        if let Expression::Block { statements, .. } = expr {
            if let Some(last_stmt) = statements.last() {
                match last_stmt {
                    Statement::Expression { expr: inner, .. } => {
                        Self::seed_expectation(env, expected, inner);
                    }
                    Statement::Return {
                        value: Some(inner), ..
                    } => {
                        Self::seed_expectation(env, expected, inner);
                    }
                    _ => {}
                }
            }
            return;
        }

        if Self::is_pattern_type(expected) {
            if let Some(span) = expression_span(expr) {
                let expectation = TypeExpectation::new(span.clone(), Self::pattern_type());
                env.push_type_expectation(expectation);
            }
            return;
        }

        if let (Expression::Lambda { body, .. }, TypeKind::Function(_, ret)) = (expr, expected) {
            Self::seed_expectation(env, ret, body);
        }
    }

    /// パターン型に一致するか判定する。
    pub fn is_pattern_type(ty: &TypeKind) -> bool {
        matches!(ty, TypeKind::Reference(name) if name == JAVA_PATTERN_FQCN)
    }

    /// パターン型を生成する。
    pub fn pattern_type() -> TypeKind {
        TypeKind::reference(JAVA_PATTERN_FQCN)
    }

    /// 対象式へ明示的にパターン期待を与える。
    pub fn register_pattern_expectation(env: &mut TypeEnvironment, span: Span) {
        let expectation = TypeExpectation::new(span, Self::pattern_type());
        env.push_type_expectation(expectation);
    }
}
