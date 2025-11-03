//! Doublebrace 初期化式に対する Copy/Mutate プランナー。
//!
//! 型推論で求めたレシーバー型を基に、可変オブジェクトか否かを判定し、
//! IR 以降へ渡すための計画（プラン）を構築する。data class / record など
//! 不変オブジェクトはコピー戦略を優先し、コピー API を利用できない場合は
//! エラーとして報告する。

use crate::inference::types::TypeKind;
use crate::java::primitive::JavaPrimitive;
use crate::pattern::expression_span;
use jv_ast::expression::{CallArgumentMetadata, CallArgumentStyle, CallKind, DoublebraceInit};
use jv_ast::types::{BinaryOp, UnaryOp};
use jv_ast::{Argument, ConcurrencyConstruct, Expression, ResourceManagement, Span, Statement};
use jv_build::metadata::SymbolIndex;
use jv_inference::{DefaultImplementationRegistry, DoublebraceHeuristics, ImplementationVariant};
use std::fmt;
use thiserror::Error;

/// Doublebrace 初期化に対する計画。
#[derive(Debug, Clone, PartialEq)]
pub enum DoublebracePlan {
    Mutate(MutatePlan),
    Copy(CopyPlan),
}

/// ミューテーション戦略。
#[derive(Debug, Clone, PartialEq)]
pub struct MutatePlan {
    pub receiver: TypeKind,
    pub base: PlanBase,
    pub steps: Vec<MutationStep>,
}

/// プラン生成時に利用するベースオブジェクトの種別。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PlanBase {
    /// 既存インスタンスをそのまま利用する。
    ExistingInstance,
    /// 暗黙に新規インスタンスを生成して利用する。
    SynthesizedInstance,
}

/// Doublebrace ブロック内で実行される操作の分類。
#[derive(Debug, Clone, PartialEq)]
pub enum MutationStep {
    /// プロパティ代入。
    FieldAssignment(FieldUpdate),
    /// 暗黙レシーバーへのメソッド呼び出し。
    MethodCall(MethodInvocation),
    /// それ以外（IR 段階で個別処理が必要）。
    Other(Statement),
}

/// メソッド呼び出し情報。
#[derive(Debug, Clone, PartialEq)]
pub struct MethodInvocation {
    pub name: String,
    pub arguments: Vec<Argument>,
    pub metadata: CallArgumentMetadata,
    pub span: Span,
}

/// コピー戦略。
#[derive(Debug, Clone, PartialEq)]
pub struct CopyPlan {
    pub receiver: TypeKind,
    pub source: CopySource,
    pub updates: Vec<FieldUpdate>,
}

/// コピー元の種別。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CopySource {
    /// 既存インスタンスをコピー元とする。
    ExistingInstance,
    /// 暗黙に生成したインスタンスをコピー元とする。
    SynthesizedInstance,
}

/// フィールド更新の記録。
#[derive(Debug, Clone, PartialEq)]
pub struct FieldUpdate {
    pub name: String,
    pub value: Expression,
    pub span: Span,
}

/// プラン構築時のエラー。
#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum DoublebracePlanError {
    #[error("Doublebrace のレシーバー型 `{ty}` を処理できません。")]
    UnsupportedReceiverType { ty: String },
    #[error("Doublebrace のコピー計画を構築できません（型: `{receiver}`）：{reason}")]
    CopyUnavailable { receiver: String, reason: String },
}

/// Doublebrace 初期化ブロックを Copy/Mutate どちらで処理すべきか判定する。
pub fn plan_doublebrace_application(
    base_ty: Option<&TypeKind>,
    target_ty: &TypeKind,
    init: &DoublebraceInit,
    symbol_index: Option<&SymbolIndex>,
) -> Result<DoublebracePlan, DoublebracePlanError> {
    let receiver_ty = target_ty.clone();
    let receiver_name = receiver_fqcn(&receiver_ty).ok_or_else(|| {
        DoublebracePlanError::UnsupportedReceiverType {
            ty: receiver_ty.describe(),
        }
    })?;

    let base_requires_synthesized = init
        .base
        .as_ref()
        .map(|expr| {
            matches!(
                expr.as_ref(),
                Expression::Call {
                    call_kind: CallKind::Constructor { .. },
                    ..
                }
            )
        })
        .unwrap_or(false);
    let has_existing_base = base_ty.is_some() && !base_requires_synthesized;

    let category = classify_receiver(symbol_index, &receiver_name);
    match category {
        ReceiverCategory::Mutable | ReceiverCategory::Unknown => {
            let steps = collect_mutation_steps(&init.statements);
            let base = determine_base_strategy(
                base_ty,
                init,
                &receiver_name,
                base_requires_synthesized,
                symbol_index,
            );
            Ok(DoublebracePlan::Mutate(MutatePlan {
                receiver: receiver_ty,
                base,
                steps,
            }))
        }
        ReceiverCategory::Immutable(profile) => {
            if !profile.supports_copy() {
                return Err(DoublebracePlanError::CopyUnavailable {
                    receiver: receiver_name,
                    reason: "コピー用の公開 API を判定できませんでした。".into(),
                });
            }

            if !has_existing_base {
                return Err(DoublebracePlanError::CopyUnavailable {
                    receiver: receiver_name,
                    reason: "コピー元となる既存インスタンスが存在しません。".into(),
                });
            }

            let updates = collect_field_updates(&init.statements).map_err(|reason| {
                DoublebracePlanError::CopyUnavailable {
                    receiver: receiver_name.clone(),
                    reason,
                }
            })?;

            Ok(DoublebracePlan::Copy(CopyPlan {
                receiver: receiver_ty,
                source: CopySource::ExistingInstance,
                updates,
            }))
        }
    }
}

#[derive(Debug, Clone)]
enum ReceiverCategory {
    Mutable,
    Immutable(ImmutableProfile),
    Unknown,
}

#[derive(Debug, Clone)]
struct ImmutableProfile {
    has_copy_method: bool,
    with_method_count: usize,
    component_count: usize,
}

impl ImmutableProfile {
    fn supports_copy(&self) -> bool {
        self.has_copy_method || self.with_method_count > 0 || self.component_count > 0
    }
}

fn classify_receiver(symbol_index: Option<&SymbolIndex>, fqcn: &str) -> ReceiverCategory {
    if let Some(index) = symbol_index {
        if let Some(entry) = index.lookup_type(fqcn) {
            let has_public_fields = !entry.instance_fields.is_empty();
            let has_copy_method = entry.instance_methods.contains_key("copy");
            let with_method_count = entry
                .instance_methods
                .keys()
                .filter(|name| name.starts_with("with"))
                .count();
            let component_count = entry
                .instance_methods
                .keys()
                .filter(|name| name.starts_with("component"))
                .count();

            if !has_public_fields
                && (has_copy_method || with_method_count > 0 || component_count > 0)
            {
                return ReceiverCategory::Immutable(ImmutableProfile {
                    has_copy_method,
                    with_method_count,
                    component_count,
                });
            } else {
                return ReceiverCategory::Mutable;
            }
        }
    }

    if fqcn.ends_with("Record") {
        return ReceiverCategory::Immutable(ImmutableProfile {
            has_copy_method: false,
            with_method_count: 0,
            component_count: 1,
        });
    }

    ReceiverCategory::Unknown
}

fn determine_base_strategy(
    base_ty: Option<&TypeKind>,
    init: &DoublebraceInit,
    receiver_name: &str,
    base_requires_synthesized: bool,
    symbol_index: Option<&SymbolIndex>,
) -> PlanBase {
    if base_requires_synthesized {
        return PlanBase::SynthesizedInstance;
    }

    let Some(base_expr) = init.base.as_deref() else {
        return PlanBase::SynthesizedInstance;
    };

    let Some(base_ty) = base_ty else {
        return PlanBase::SynthesizedInstance;
    };

    let Some(base_fqcn) = receiver_fqcn(base_ty) else {
        return PlanBase::ExistingInstance;
    };

    let normalized_receiver = receiver_name.trim();
    let normalized_base = base_fqcn.trim();

    if normalized_base == normalized_receiver {
        return PlanBase::ExistingInstance;
    }

    if is_factory_like_expression(base_expr) {
        let (base_raw, generics) = split_type_name(normalized_base);
        let resolved = resolve_mutable_reference_with_generics(base_raw, generics, symbol_index);
        if resolved.trim() == normalized_receiver {
            return PlanBase::SynthesizedInstance;
        }
    }

    PlanBase::ExistingInstance
}

fn resolve_mutable_reference_with_generics(
    candidate: &str,
    generics: Option<&str>,
    symbol_index: Option<&SymbolIndex>,
) -> String {
    if let Some(default) = DoublebraceHeuristics::resolve_default_implementation(
        candidate,
        ImplementationVariant::Mutable,
    ) {
        return append_generics(&default, generics);
    }

    let registry = DefaultImplementationRegistry::shared();
    if let Some(interface_impl) =
        registry.resolve_interface_variant(candidate, ImplementationVariant::Mutable)
    {
        return append_generics(interface_impl.target(), generics);
    }
    if let Some(abstract_impl) = registry.resolve_abstract(candidate, symbol_index) {
        return append_generics(abstract_impl.target(), generics);
    }

    append_generics(candidate, generics)
}

fn append_generics(base: &str, generics: Option<&str>) -> String {
    if let Some(args) = generics {
        if args.is_empty() || base.contains('<') {
            base.to_string()
        } else {
            format!("{base}{args}")
        }
    } else {
        base.to_string()
    }
}

fn split_type_name(candidate: &str) -> (&str, Option<&str>) {
    if let Some(start) = candidate.find('<') {
        let (base, generics) = candidate.split_at(start);
        (base.trim(), Some(generics.trim()))
    } else {
        (candidate.trim(), None)
    }
}

fn is_factory_like_expression(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::Call { .. }
            | Expression::Array { .. }
            | Expression::JsonLiteral(_)
            | Expression::DoublebraceInit(_)
    )
}

fn collect_mutation_steps(statements: &[Statement]) -> Vec<MutationStep> {
    statements
        .iter()
        .map(|statement| match statement {
            Statement::Assignment {
                target,
                value,
                span,
                ..
            } => {
                if let Some(name) = assignment_target_name(target) {
                    MutationStep::FieldAssignment(FieldUpdate {
                        name,
                        value: value.clone(),
                        span: span.clone(),
                    })
                } else {
                    MutationStep::Other(statement.clone())
                }
            }
            Statement::Expression { expr, span } => match expr {
                Expression::Call {
                    function,
                    args,
                    argument_metadata,
                    ..
                } => {
                    if let Expression::Identifier(name, _) = function.as_ref() {
                        let (normalized_args, normalized_metadata) =
                            normalize_method_call_arguments(
                                args.clone(),
                                argument_metadata.clone(),
                            );
                        MutationStep::MethodCall(MethodInvocation {
                            name: name.clone(),
                            arguments: normalized_args,
                            metadata: normalized_metadata,
                            span: span.clone(),
                        })
                    } else {
                        MutationStep::Other(statement.clone())
                    }
                }
                _ => MutationStep::Other(statement.clone()),
            },
            _ => MutationStep::Other(statement.clone()),
        })
        .collect()
}

fn normalize_method_call_arguments(
    args: Vec<Argument>,
    metadata: CallArgumentMetadata,
) -> (Vec<Argument>, CallArgumentMetadata) {
    if metadata.style != CallArgumentStyle::Whitespace || args.len() != 2 {
        return (args, metadata);
    }

    let left = match &args[0] {
        Argument::Positional(expr) => expr.clone(),
        _ => return (args, metadata),
    };

    let (operand, operand_span) = match &args[1] {
        Argument::Positional(Expression::Unary { op, operand, span })
            if matches!(op, UnaryOp::Minus) =>
        {
            (operand.as_ref().clone(), span.clone())
        }
        _ => return (args, metadata),
    };

    let left_span = expression_span(&left).cloned();
    let combined_span = left_span
        .map(|span| {
            Span::new(
                span.start_line,
                span.start_column,
                operand_span.end_line,
                operand_span.end_column,
            )
        })
        .unwrap_or_else(|| operand_span.clone());

    let binary_expr = Expression::Binary {
        left: Box::new(left),
        op: BinaryOp::Subtract,
        right: Box::new(operand),
        span: combined_span,
    };

    let new_args = vec![Argument::Positional(binary_expr)];
    let mut new_metadata = metadata;
    new_metadata.style = CallArgumentStyle::Comma;
    (new_args, new_metadata)
}

fn collect_field_updates(statements: &[Statement]) -> Result<Vec<FieldUpdate>, String> {
    let mut updates = Vec::new();
    for statement in statements {
        match statement {
            Statement::Assignment {
                target,
                value,
                span,
                ..
            } => {
                if let Some(name) = assignment_target_name(target) {
                    updates.push(FieldUpdate {
                        name,
                        value: value.clone(),
                        span: span.clone(),
                    });
                } else {
                    return Err(format!(
                        "暗黙レシーバーのプロパティ名を特定できない代入が含まれています（位置: {}）。",
                        format_span(span)
                    ));
                }
            }
            Statement::Expression { expr, span } => match expr {
                Expression::Call { .. } => {
                    return Err(format!(
                        "コピー計画ではメソッド呼び出しを処理できません（位置: {}）。",
                        format_span(span)
                    ));
                }
                _ => {
                    return Err(format!(
                        "コピー計画が対応していないステートメントが含まれています（位置: {}）。",
                        format_span(span)
                    ));
                }
            },
            other => {
                return Err(format!(
                    "コピー計画が対応していないステートメントが含まれています（位置: {}）。",
                    format_span(statement_span(other))
                ));
            }
        }
    }

    if updates.is_empty() {
        return Err("コピー計画に反映すべき更新が見つかりません。".into());
    }

    Ok(updates)
}

fn assignment_target_name(expr: &Expression) -> Option<String> {
    match expr {
        Expression::Identifier(name, _) => Some(name.clone()),
        Expression::MemberAccess {
            object, property, ..
        } if is_implicit_this(object.as_ref()) => Some(property.clone()),
        Expression::NullSafeMemberAccess {
            object, property, ..
        } if is_implicit_this(object.as_ref()) => Some(property.clone()),
        _ => None,
    }
}

fn is_implicit_this(expr: &Expression) -> bool {
    match expr {
        Expression::This(_) => true,
        Expression::Identifier(name, _) => name == "this",
        _ => false,
    }
}

fn receiver_fqcn(ty: &TypeKind) -> Option<String> {
    match ty {
        TypeKind::Reference(name) => Some(name.clone()),
        TypeKind::Optional(inner) => receiver_fqcn(inner),
        TypeKind::Boxed(primitive) => Some(primitive.boxed_fqcn().to_string()),
        TypeKind::Primitive(primitive) => Some(JavaPrimitive::boxed_fqcn(*primitive).to_string()),
        _ => None,
    }
}

fn format_span(span: &Span) -> SpanDisplay {
    SpanDisplay(span.clone())
}

struct SpanDisplay(Span);

impl fmt::Display for SpanDisplay {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let span = &self.0;
        write!(
            f,
            "{}:{}",
            span.start_line.saturating_add(1),
            span.start_column.saturating_add(1)
        )
    }
}

fn statement_span(statement: &Statement) -> &Span {
    match statement {
        Statement::Comment(comment) => &comment.span,
        Statement::ValDeclaration { span, .. }
        | Statement::VarDeclaration { span, .. }
        | Statement::FunctionDeclaration { span, .. }
        | Statement::ClassDeclaration { span, .. }
        | Statement::DataClassDeclaration { span, .. }
        | Statement::InterfaceDeclaration { span, .. }
        | Statement::Expression { span, .. }
        | Statement::Return { span, .. }
        | Statement::Throw { span, .. }
        | Statement::Assignment { span, .. }
        | Statement::Import { span, .. }
        | Statement::Package { span, .. } => span,
        Statement::ExtensionFunction(extension) => &extension.span,
        Statement::ForIn(for_in) => &for_in.span,
        Statement::Concurrency(construct) => match construct {
            ConcurrencyConstruct::Spawn { span, .. }
            | ConcurrencyConstruct::Async { span, .. }
            | ConcurrencyConstruct::Await { span, .. } => span,
        },
        Statement::ResourceManagement(resource) => match resource {
            ResourceManagement::Use { span, .. } | ResourceManagement::Defer { span, .. } => span,
        },
        Statement::Break(span) | Statement::Continue(span) => span,
    }
}
