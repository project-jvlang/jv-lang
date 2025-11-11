use super::graph::format_symbol;
use super::registry::{
    ConversionLambdaIr, ReverseMode, UnitCategoryEntry, UnitConversionAstRef, UnitConversionBody,
    UnitConversionRef, UnitEdge, UnitEdgeId, UnitEntry,
};
use super::{UnitConversionKind, UnitConversionRaw, UnitRateRaw};
use crate::diagnostics::{unit_semantics, DiagnosticSeverity};
use crate::inference::type_factory::TypeFactory;
use crate::inference::types::TypeKind;
use crate::CheckError;
use jv_ast::{BinaryOp, Expression, Literal, Statement, TypeAnnotation, UnaryOp};
use rust_decimal::Decimal;
use std::collections::HashMap;
use std::str::FromStr;
use std::sync::Arc;

/// `UnitRelation::ConversionArrow` に対応する補助メタデータ。
#[derive(Debug, Default, Clone)]
pub(super) struct EdgeConversionPlan {
    pub forward: Option<UnitConversionRaw>,
    pub reverse: Option<UnitConversionRaw>,
    pub rate: Option<UnitRateRaw>,
}

/// 変換ブロックとレートを解析し、`UnitEdge` を更新する。
pub(super) fn apply_edge_conversions(
    plans: &HashMap<UnitEdgeId, EdgeConversionPlan>,
    edges: &mut [UnitEdge],
    units: &[UnitEntry],
    categories: &[UnitCategoryEntry],
    conversions: &mut Vec<UnitConversionBody>,
    diagnostics: &mut Vec<CheckError>,
) -> bool {
    let mut had_error = false;

    for (edge_id, plan) in plans {
        let Some(edge) = edges.get_mut(*edge_id as usize) else {
            continue;
        };

        if let Some(rate) = &plan.rate {
            let label = edge_label(edge, units);
            if let Some(value) = evaluate_decimal(&rate.expression) {
                edge.rate = Some(value);
            } else if emit_unit_diagnostic(
                "JV_UNIT_SEM_041",
                format!("`{label}` の `@ConversionRate` は数値リテラルで表現してください。"),
                &rate.span,
                diagnostics,
            ) {
                had_error = true;
            }
        }

        let Some(base_type) = base_type_for_edge(edge, units, categories) else {
            continue;
        };

        if let Some(block) = &plan.forward {
            let label = edge_label(edge, units);
            let lambda_result = build_lambda_ir(block, base_type, &label, diagnostics);
            let Ok(ir) = lambda_result else {
                if lambda_result.err().unwrap_or(false) {
                    had_error = true;
                }
                continue;
            };

            let ref_index = conversions.len() as u32;
            conversions.push(UnitConversionBody {
                edge: *edge_id,
                ir: Arc::new(ir.clone()),
                original_ast: Some(UnitConversionAstRef {
                    kind: block.kind,
                    span: block.span.clone(),
                }),
            });
            edge.conversion_ref = Some(UnitConversionRef(ref_index));

            if let Some(reverse_block) = &plan.reverse {
                let reverse_result = build_lambda_ir(reverse_block, base_type, &label, diagnostics);
                if reverse_result.is_ok() {
                    edge.reverse_mode = ReverseMode::Provided;
                } else if reverse_result.err().unwrap_or(false) {
                    had_error = true;
                }
            } else {
                match analyze_linear_form(&ir) {
                    Some(form) if !form.scale.is_zero() => {
                        let reverse_scale = Decimal::ONE / form.scale;
                        let reverse_offset = -form.offset / form.scale;
                        edge.reverse_mode = ReverseMode::Auto {
                            scale: reverse_scale,
                            offset: reverse_offset,
                        };
                    }
                    _ => {
                        let detail = format!(
                            "`{}` の {} は線形式 `value * k + b` で表現できないため自動逆変換を生成できません。",
                            label,
                            kind_label(UnitConversionKind::Conversion)
                        );
                        emit_unit_diagnostic("JV_UNIT_SEM_050", detail, &block.span, diagnostics);
                    }
                }
            }
        } else if let Some(reverse_block) = &plan.reverse {
            let label = edge_label(edge, units);
            if emit_unit_diagnostic(
                "JV_UNIT_SEM_040",
                format!(
                    "`{}` には `@ReverseConversion` が存在しますが、対応する `@Conversion` が不足しています。",
                    label
                ),
                &reverse_block.span,
                diagnostics,
            ) {
                had_error = true;
            }
        }
    }

    had_error
}

fn build_lambda_ir(
    block: &UnitConversionRaw,
    expected_type: &TypeKind,
    edge_label: &str,
    diagnostics: &mut Vec<CheckError>,
) -> Result<ConversionLambdaIr, bool> {
    let Some(lambda_expr) = extract_lambda_expression(block) else {
        let detail = format!(
            "`{}` の {} には 1 つのラムダ式を記述してください。",
            edge_label,
            kind_label(block.kind)
        );
        let fatal = emit_unit_diagnostic("JV_UNIT_SEM_040", detail, &block.span, diagnostics);
        return Err(fatal);
    };

    let (parameters, lambda_span) = match &lambda_expr {
        Expression::Lambda {
            parameters, span, ..
        } => (parameters, span.clone()),
        _ => unreachable!("extract_lambda_expression always returns a lambda"),
    };

    if parameters.len() != 1 {
        let detail = format!(
            "{} では単一引数のラムダのみサポートされています。",
            kind_label(block.kind)
        );
        let fatal = emit_unit_diagnostic("JV_UNIT_SEM_040", detail, &lambda_span, diagnostics);
        return Err(fatal);
    }

    let parameter = &parameters[0];
    let parameter_type = if let Some(annotation) = &parameter.type_annotation {
        match annotation_to_type(annotation) {
            Some(kind) => kind,
            None => {
                let detail = format!(
                    "{} の引数型を解析できません: `{}`",
                    kind_label(block.kind),
                    annotation_label(annotation)
                );
                let fatal =
                    emit_unit_diagnostic("JV_UNIT_SEM_040", detail, &parameter.span, diagnostics);
                return Err(fatal);
            }
        }
    } else {
        expected_type.clone()
    };

    if parameter_type != *expected_type {
        let detail = format!(
            "{} の引数型 `{}` がカテゴリの基底型 `{}` と一致しません。",
            kind_label(block.kind),
            parameter_type.describe(),
            expected_type.describe()
        );
        let fatal = emit_unit_diagnostic("JV_UNIT_SEM_040", detail, &parameter.span, diagnostics);
        return Err(fatal);
    }

    Ok(ConversionLambdaIr {
        parameter_name: parameter.name.clone(),
        parameter_type,
        return_type: expected_type.clone(),
        lambda: lambda_expr,
        span: lambda_span,
    })
}

fn annotation_to_type(annotation: &TypeAnnotation) -> Option<TypeKind> {
    match annotation {
        TypeAnnotation::Simple(name) => TypeFactory::from_annotation(name).ok(),
        TypeAnnotation::Nullable(inner) => annotation_to_type(inner).map(TypeKind::optional),
        TypeAnnotation::Unit { base, .. } => annotation_to_type(base),
        _ => None,
    }
}

fn annotation_label(annotation: &TypeAnnotation) -> String {
    match annotation {
        TypeAnnotation::Simple(name) => name.clone(),
        TypeAnnotation::Nullable(inner) => format!("{}?", annotation_label(inner)),
        TypeAnnotation::Unit { base, unit, .. } => {
            format!("{}@[{}]", annotation_label(base), unit.name)
        }
        _ => "<unsupported>".to_string(),
    }
}

fn extract_lambda_expression(block: &UnitConversionRaw) -> Option<Expression> {
    for statement in &block.body {
        match statement {
            Statement::Expression { expr, .. } => {
                if matches!(expr, Expression::Lambda { .. }) {
                    return Some(expr.clone());
                }
            }
            Statement::Return {
                value: Some(expr), ..
            } => {
                if matches!(expr, Expression::Lambda { .. }) {
                    return Some(expr.clone());
                }
            }
            Statement::ValDeclaration { initializer, .. } => {
                if matches!(initializer, Expression::Lambda { .. }) {
                    return Some(initializer.clone());
                }
            }
            _ => {}
        }
    }
    None
}

fn base_type_for_edge<'a>(
    edge: &UnitEdge,
    units: &[UnitEntry],
    categories: &'a [UnitCategoryEntry],
) -> Option<&'a TypeKind> {
    let unit = units.get(edge.from as usize)?;
    categories
        .get(unit.category_id as usize)
        .map(|entry| &entry.base_type)
}

fn edge_label(edge: &UnitEdge, units: &[UnitEntry]) -> String {
    let from = units
        .get(edge.from as usize)
        .map(|entry| format_symbol(&entry.symbol))
        .unwrap_or_else(|| edge.from.to_string());
    let to = units
        .get(edge.to as usize)
        .map(|entry| format_symbol(&entry.symbol))
        .unwrap_or_else(|| edge.to.to_string());
    format!("{from} -> {to}")
}

fn kind_label(kind: UnitConversionKind) -> &'static str {
    match kind {
        UnitConversionKind::Conversion => "@Conversion",
        UnitConversionKind::ReverseConversion => "@ReverseConversion",
    }
}

fn emit_unit_diagnostic(
    code: &str,
    detail: impl Into<String>,
    span: &jv_ast::Span,
    diagnostics: &mut Vec<CheckError>,
) -> bool {
    let descriptor = unit_semantics::descriptor(code).expect("diagnostic registered");
    let mut lines = vec![format!("{}: {}", descriptor.code, descriptor.title)];
    let detail = detail.into();
    if !detail.is_empty() {
        lines.push(detail);
    }
    if !descriptor.help.is_empty() {
        lines.push(descriptor.help.to_string());
    }

    diagnostics.push(CheckError::ValidationError {
        message: lines.join("\n"),
        span: Some(span.clone()),
    });

    matches!(descriptor.severity, DiagnosticSeverity::Error)
}

fn evaluate_decimal(expr: &Expression) -> Option<Decimal> {
    match expr {
        Expression::Literal(Literal::Number(value), _) => parse_decimal(value),
        Expression::Unary {
            op: UnaryOp::Minus,
            operand,
            ..
        } => evaluate_decimal(operand).map(|value| -value),
        Expression::Unary {
            op: UnaryOp::Plus,
            operand,
            ..
        } => evaluate_decimal(operand),
        Expression::Binary {
            op: BinaryOp::Add,
            left,
            right,
            ..
        } => match (evaluate_decimal(left), evaluate_decimal(right)) {
            (Some(a), Some(b)) => Some(a + b),
            _ => None,
        },
        Expression::Binary {
            op: BinaryOp::Subtract,
            left,
            right,
            ..
        } => match (evaluate_decimal(left), evaluate_decimal(right)) {
            (Some(a), Some(b)) => Some(a - b),
            _ => None,
        },
        Expression::Binary {
            op: BinaryOp::Multiply,
            left,
            right,
            ..
        } => match (evaluate_decimal(left), evaluate_decimal(right)) {
            (Some(a), Some(b)) => Some(a * b),
            _ => None,
        },
        Expression::Binary {
            op: BinaryOp::Divide,
            left,
            right,
            ..
        } => match (evaluate_decimal(left), evaluate_decimal(right)) {
            (Some(a), Some(b)) if !b.is_zero() => Some(a / b),
            _ => None,
        },
        _ => None,
    }
}

fn parse_decimal(value: &str) -> Option<Decimal> {
    let normalized: String = value.chars().filter(|ch| *ch != '_').collect();
    Decimal::from_str(&normalized).ok()
}

#[derive(Debug, Clone, Copy)]
struct LinearForm {
    scale: Decimal,
    offset: Decimal,
}

impl LinearForm {
    fn identity() -> Self {
        Self {
            scale: Decimal::ONE,
            offset: Decimal::ZERO,
        }
    }

    fn constant(value: Decimal) -> Self {
        Self {
            scale: Decimal::ZERO,
            offset: value,
        }
    }

    fn add(self, other: Self) -> Self {
        Self {
            scale: self.scale + other.scale,
            offset: self.offset + other.offset,
        }
    }

    fn sub(self, other: Self) -> Self {
        Self {
            scale: self.scale - other.scale,
            offset: self.offset - other.offset,
        }
    }

    fn scale_by(self, factor: Decimal) -> Self {
        Self {
            scale: self.scale * factor,
            offset: self.offset * factor,
        }
    }

    fn divide_by(self, divisor: Decimal) -> Option<Self> {
        if divisor.is_zero() {
            None
        } else {
            Some(Self {
                scale: self.scale / divisor,
                offset: self.offset / divisor,
            })
        }
    }

    fn negate(self) -> Self {
        Self {
            scale: -self.scale,
            offset: -self.offset,
        }
    }

    fn is_constant(&self) -> bool {
        self.scale.is_zero()
    }
}

fn analyze_linear_form(ir: &ConversionLambdaIr) -> Option<LinearForm> {
    let Expression::Lambda { body, .. } = &ir.lambda else {
        return None;
    };
    let expr = extract_effective_expression(body)?;
    linear_form(expr, &ir.parameter_name)
}

fn extract_effective_expression(expr: &Expression) -> Option<&Expression> {
    match expr {
        Expression::Block { statements, .. } => {
            for statement in statements.iter().rev() {
                match statement {
                    Statement::Expression { expr, .. } => return Some(expr),
                    Statement::Return {
                        value: Some(expr), ..
                    } => return Some(expr),
                    _ => continue,
                }
            }
            None
        }
        other => Some(other),
    }
}

fn linear_form(expr: &Expression, parameter: &str) -> Option<LinearForm> {
    match expr {
        Expression::Identifier(name, _) if name == parameter => Some(LinearForm::identity()),
        Expression::Literal(Literal::Number(value), _) => {
            parse_decimal(value).map(LinearForm::constant)
        }
        Expression::Unary {
            op: UnaryOp::Minus,
            operand,
            ..
        } => linear_form(operand, parameter).map(LinearForm::negate),
        Expression::Unary {
            op: UnaryOp::Plus,
            operand,
            ..
        } => linear_form(operand, parameter),
        Expression::Binary {
            op: BinaryOp::Add,
            left,
            right,
            ..
        } => Some(linear_form(left, parameter)?.add(linear_form(right, parameter)?)),
        Expression::Binary {
            op: BinaryOp::Subtract,
            left,
            right,
            ..
        } => Some(linear_form(left, parameter)?.sub(linear_form(right, parameter)?)),
        Expression::Binary {
            op: BinaryOp::Multiply,
            left,
            right,
            ..
        } => combine_multiplication(
            linear_form(left, parameter)?,
            linear_form(right, parameter)?,
        ),
        Expression::Binary {
            op: BinaryOp::Divide,
            left,
            right,
            ..
        } => {
            let divisor = linear_form(right, parameter)?;
            if !divisor.is_constant() {
                return None;
            }
            linear_form(left, parameter)?.divide_by(divisor.offset)
        }
        _ => None,
    }
}

fn combine_multiplication(left: LinearForm, right: LinearForm) -> Option<LinearForm> {
    match (left.is_constant(), right.is_constant()) {
        (true, true) => Some(LinearForm::constant(left.offset * right.offset)),
        (true, false) => Some(right.scale_by(left.offset)),
        (false, true) => Some(left.scale_by(right.offset)),
        (false, false) => None,
    }
}
