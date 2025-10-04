use super::transform_expression;
use super::utils::extract_java_type;
use crate::context::TransformContext;
use crate::error::TransformError;
use crate::types::{
    IrCaseLabel, IrDeconstructionComponent, IrDeconstructionPattern, IrExpression,
    IrImplicitWhenEnd, IrSwitchCase, JavaType, JavaWildcardKind,
};
use jv_ast::{BinaryOp, Expression, ImplicitWhenEnd, Literal, Pattern, Span, WhenArm};

/// Maximum supported destructuring depth inside a single `when` arm. The outer
/// constructor counts as depth = 1; each nested constructor increments the
/// depth. Values above this constant are surfaced as JV3199 until later phases
/// extend support.
const MAX_DECONSTRUCTION_DEPTH: usize = 10;

#[derive(Debug, Default, Clone)]
pub struct PatternAnalysisSummary {
    pub is_exhaustive: Option<bool>,
}

#[derive(Debug)]
pub struct WhenLoweringPlan {
    pub description: String,
    pub ir: IrExpression,
}

#[derive(Debug)]
pub struct WhenLoweringPlanner {
    subject: Option<Box<Expression>>,
    arms: Vec<WhenArm>,
    else_arm: Option<Box<Expression>>,
    implicit_end: Option<ImplicitWhenEnd>,
    span: Span,
    analysis: PatternAnalysisSummary,
}

impl WhenLoweringPlanner {
    pub fn new(
        subject: Option<Box<Expression>>,
        arms: Vec<WhenArm>,
        else_arm: Option<Box<Expression>>,
        implicit_end: Option<ImplicitWhenEnd>,
        span: Span,
    ) -> Self {
        Self {
            subject,
            arms,
            else_arm,
            implicit_end,
            span,
            analysis: PatternAnalysisSummary::default(),
        }
    }

    pub fn plan(self, context: &mut TransformContext) -> Result<WhenLoweringPlan, TransformError> {
        let WhenLoweringPlanner {
            subject,
            arms,
            else_arm,
            implicit_end,
            span,
            analysis,
        } = self;

        match subject {
            Some(expr) => Self::plan_with_subject(
                *expr,
                arms,
                else_arm,
                implicit_end,
                span,
                analysis,
                context,
            ),
            None => {
                Self::plan_without_subject(arms, else_arm, implicit_end, span, analysis, context)
            }
        }
    }

    fn plan_with_subject(
        subject: Expression,
        arms: Vec<WhenArm>,
        else_arm: Option<Box<Expression>>,
        implicit_end: Option<ImplicitWhenEnd>,
        span: Span,
        analysis: PatternAnalysisSummary,
        context: &mut TransformContext,
    ) -> Result<WhenLoweringPlan, TransformError> {
        let mut implicit_ir_end = implicit_end.map(convert_implicit_end);
        let discriminant = transform_expression(subject, context)?;
        let discriminant_type = extract_java_type(&discriminant).unwrap_or_else(JavaType::object);

        let mut cases = Vec::new();
        let mut result_type: Option<JavaType> = None;
        let mut has_default_case = false;
        let mut guard_count = 0usize;
        let total_arms = arms.len();

        for (index, arm) in arms.into_iter().enumerate() {
            let PatternLowering {
                labels,
                guard,
                is_default,
            } = lower_pattern_case(arm.pattern, &discriminant_type, context, arm.span.clone())?;

            if is_default {
                if has_default_case {
                    return Err(TransformError::UnsupportedConstruct {
                        construct: "Multiple default patterns in when expression".to_string(),
                        span: arm.span,
                    });
                }
                has_default_case = true;
            }

            let mut combined_guard = guard;
            if let Some(guard_expr) = arm.guard {
                let transformed_guard = transform_expression(guard_expr, context)?;
                combined_guard = combine_guards(combined_guard, Some(transformed_guard), &arm.span);
            }

            if combined_guard.is_some() {
                guard_count += 1;
            }

            let body = transform_expression(arm.body, context)?;
            if result_type.is_none() {
                result_type = extract_java_type(&body);
            }

            cases.push(IrSwitchCase {
                labels: if labels.is_empty() {
                    vec![IrCaseLabel::Default]
                } else {
                    labels
                },
                guard: combined_guard,
                body,
                span: arm.span.clone(),
            });

            if is_default && index != total_arms.saturating_sub(1) {
                // continue allowing later arms to collect errors.
            }
        }

        if let Some(else_arm) = else_arm {
            if has_default_case {
                return Err(TransformError::UnsupportedConstruct {
                    construct: "when expression cannot have both default pattern and else arm"
                        .to_string(),
                    span,
                });
            }

            let body = transform_expression(*else_arm, context)?;
            if result_type.is_none() {
                result_type = extract_java_type(&body);
            }

            cases.push(IrSwitchCase {
                labels: vec![IrCaseLabel::Default],
                guard: None,
                body,
                span: span.clone(),
            });
            implicit_ir_end = None;
            has_default_case = true;
        }

        if cases.is_empty() {
            return Err(TransformError::UnsupportedConstruct {
                construct: "when expression must have at least one arm".to_string(),
                span,
            });
        }

        let java_type = result_type.unwrap_or_else(JavaType::object);
        let strategy_label = if guard_count > 0 { "Hybrid" } else { "Switch" };
        let exhaustive = describe_exhaustiveness(&analysis);
        let description = format!(
            "strategy={} arms={} guards={} default={} exhaustive={}",
            strategy_label,
            cases.len(),
            guard_count,
            has_default_case,
            exhaustive
        );

        let ir = IrExpression::Switch {
            discriminant: Box::new(discriminant),
            cases,
            java_type,
            implicit_end: implicit_ir_end,
            strategy_description: Some(description.clone()),
            span,
        };

        Ok(WhenLoweringPlan { description, ir })
    }

    fn plan_without_subject(
        arms: Vec<WhenArm>,
        else_arm: Option<Box<Expression>>,
        implicit_end: Option<ImplicitWhenEnd>,
        span: Span,
        analysis: PatternAnalysisSummary,
        context: &mut TransformContext,
    ) -> Result<WhenLoweringPlan, TransformError> {
        let mut arms = arms;
        if arms.is_empty() {
            return Err(TransformError::UnsupportedConstruct {
                construct: "when expression must have at least one arm".to_string(),
                span,
            });
        }

        let mut fallback = match (else_arm, implicit_end) {
            (Some(else_arm), _) => transform_expression(*else_arm, context)?,
            (None, Some(end)) => implicit_end_expression(end),
            (None, None) => {
                return Err(TransformError::UnsupportedConstruct {
                    construct: "subjectless when requires else branch".to_string(),
                    span,
                });
            }
        };

        let mut result_type = extract_java_type(&fallback);
        let total_arms = arms.len();

        while let Some(arm) = arms.pop() {
            let condition = lower_subjectless_pattern(arm.pattern, context, &arm.span)?;
            let mut combined_guard = None;
            if let Some(guard_expr) = arm.guard {
                let transformed_guard = transform_expression(guard_expr, context)?;
                combined_guard = Some(transformed_guard);
            }
            let condition =
                combine_guards(Some(condition), combined_guard, &arm.span).ok_or_else(|| {
                    TransformError::InvalidPattern {
                        message: "subjectless when arm requires a condition".to_string(),
                        span: arm.span.clone(),
                    }
                })?;

            let body = transform_expression(arm.body, context)?;
            if result_type.is_none() {
                result_type = extract_java_type(&body);
            }
            let java_type = result_type.clone().unwrap_or_else(JavaType::object);

            fallback = IrExpression::Conditional {
                condition: Box::new(condition),
                then_expr: Box::new(body),
                else_expr: Box::new(fallback),
                java_type,
                span: arm.span,
            };
        }

        let exhaustive = describe_exhaustiveness(&analysis);
        let description = format!(
            "strategy=IfChain arms={} exhaustive={}",
            total_arms, exhaustive
        );

        Ok(WhenLoweringPlan {
            description,
            ir: fallback,
        })
    }
}

#[derive(Debug)]
struct PatternLowering {
    labels: Vec<IrCaseLabel>,
    guard: Option<IrExpression>,
    is_default: bool,
}

#[derive(Debug, Clone)]
struct CaseBinding {
    name: String,
    java_type: JavaType,
}

impl CaseBinding {
    fn new(name: String, java_type: JavaType) -> Self {
        Self { name, java_type }
    }

    fn identifier(&self, span: &Span) -> IrExpression {
        IrExpression::Identifier {
            name: self.name.clone(),
            java_type: self.java_type.clone(),
            span: span.clone(),
        }
    }
}

fn lower_pattern_case(
    pattern: Pattern,
    subject_type: &JavaType,
    context: &mut TransformContext,
    arm_span: Span,
) -> Result<PatternLowering, TransformError> {
    match pattern {
        Pattern::Wildcard(_) => Ok(PatternLowering {
            labels: vec![IrCaseLabel::Default],
            guard: None,
            is_default: true,
        }),
        Pattern::Literal(literal, _) => Ok(PatternLowering {
            labels: vec![IrCaseLabel::Literal(literal)],
            guard: None,
            is_default: false,
        }),
        Pattern::Constructor {
            name,
            patterns,
            span,
        } => {
            let binding = CaseBinding::new(context.fresh_identifier("it"), subject_type.clone());
            let (deconstruction, nested_guard) =
                lower_constructor_deconstruction(patterns, context, &span, 1)?;
            Ok(PatternLowering {
                labels: vec![IrCaseLabel::TypePattern {
                    type_name: name,
                    variable: binding.name.clone(),
                    deconstruction,
                }],
                guard: nested_guard,
                is_default: false,
            })
        }
        Pattern::Range {
            start,
            end,
            inclusive_end,
            span,
        } => {
            let binding = CaseBinding::new(context.fresh_identifier("it"), subject_type.clone());
            let start_ir = transform_expression(*start, context)?;
            let end_ir = transform_expression(*end, context)?;

            let lower_guard = comparison_expression(
                binding.identifier(&span),
                BinaryOp::GreaterEqual,
                start_ir.clone(),
                span.clone(),
            );

            let upper_op = if inclusive_end {
                BinaryOp::LessEqual
            } else {
                BinaryOp::Less
            };
            let upper_guard = comparison_expression(
                binding.identifier(&span),
                upper_op,
                end_ir.clone(),
                span.clone(),
            );

            let guard = Some(and_expression(lower_guard, upper_guard, span.clone()));
            Ok(PatternLowering {
                labels: vec![IrCaseLabel::Range {
                    type_name: type_name_for_case(subject_type),
                    variable: binding.name.clone(),
                    lower: Box::new(start_ir),
                    upper: Box::new(end_ir),
                    inclusive_end,
                }],
                guard,
                is_default: false,
            })
        }
        Pattern::Guard {
            pattern,
            condition,
            span,
        } => {
            let mut lowered =
                lower_pattern_case(*pattern, subject_type, context, arm_span.clone())?;
            let guard_expr = transform_expression(condition, context)?;
            lowered.guard = combine_guards(lowered.guard, Some(guard_expr), &span);
            Ok(lowered)
        }
        Pattern::Identifier(name, span) => Err(TransformError::UnsupportedConstruct {
            construct: unsupported_pattern_message(
                &format!("Identifier pattern '{name}' is not supported"),
                &format!("識別子パターン '{name}' はまだ利用できません"),
            ),
            span,
        }),
    }
}

fn lower_subjectless_pattern(
    pattern: Pattern,
    context: &mut TransformContext,
    span: &Span,
) -> Result<IrExpression, TransformError> {
    match pattern {
        Pattern::Wildcard(wild_span) => {
            Ok(IrExpression::Literal(Literal::Boolean(true), wild_span))
        }
        Pattern::Literal(Literal::Boolean(value), literal_span) => {
            Ok(IrExpression::Literal(Literal::Boolean(value), literal_span))
        }
        Pattern::Guard {
            pattern,
            condition,
            span: guard_span,
        } => {
            if !matches!(*pattern, Pattern::Wildcard(_)) {
                return Err(TransformError::UnsupportedConstruct {
                    construct: unsupported_pattern_message(
                        "Subjectless when requires guards to wrap `_`",
                        "主題なし when では guard は常に `_` を包む必要があります",
                    ),
                    span: guard_span,
                });
            }
            transform_expression(condition, context)
        }
        other => Err(TransformError::UnsupportedConstruct {
            construct: unsupported_pattern_message(
                &format!("Unsupported subjectless when pattern: {:?}", other),
                "主題なし when でサポートされていないパターンです",
            ),
            span: span.clone(),
        }),
    }
}

fn combine_guards(
    first: Option<IrExpression>,
    second: Option<IrExpression>,
    span: &Span,
) -> Option<IrExpression> {
    match (first, second) {
        (Some(left), Some(right)) => Some(and_expression(left, right, span.clone())),
        (Some(left), None) => Some(left),
        (None, Some(right)) => Some(right),
        (None, None) => None,
    }
}

fn and_expression(left: IrExpression, right: IrExpression, span: Span) -> IrExpression {
    IrExpression::Binary {
        left: Box::new(left),
        op: BinaryOp::And,
        right: Box::new(right),
        java_type: JavaType::boolean(),
        span,
    }
}

fn comparison_expression(
    subject: IrExpression,
    op: BinaryOp,
    other: IrExpression,
    span: Span,
) -> IrExpression {
    IrExpression::Binary {
        left: Box::new(subject),
        op,
        right: Box::new(other),
        java_type: JavaType::boolean(),
        span,
    }
}

fn implicit_end_expression(end: ImplicitWhenEnd) -> IrExpression {
    match end {
        ImplicitWhenEnd::Unit { span } => IrExpression::Block {
            statements: Vec::new(),
            java_type: JavaType::void(),
            span,
        },
    }
}

fn convert_implicit_end(end: ImplicitWhenEnd) -> IrImplicitWhenEnd {
    match end {
        ImplicitWhenEnd::Unit { span } => IrImplicitWhenEnd::Unit { span },
    }
}

fn lower_constructor_deconstruction(
    patterns: Vec<Pattern>,
    context: &mut TransformContext,
    span: &Span,
    current_depth: usize,
) -> Result<(Option<IrDeconstructionPattern>, Option<IrExpression>), TransformError> {
    if patterns.is_empty() {
        return Ok((None, None));
    }

    if current_depth > MAX_DECONSTRUCTION_DEPTH {
        return Err(TransformError::UnsupportedConstruct {
            construct: unsupported_pattern_message(
                "Destructuring depth ≥ 11 is not supported yet",
                "分解パターンの深さ（深度11以上）はまだサポートされていません",
            ),
            span: span.clone(),
        });
    }

    let mut components = Vec::new();
    for nested in patterns {
        components.push(lower_deconstruction_component(
            nested,
            context,
            current_depth + 1,
        )?);
    }

    Ok((Some(IrDeconstructionPattern { components }), None))
}

fn lower_deconstruction_component(
    pattern: Pattern,
    context: &mut TransformContext,
    current_depth: usize,
) -> Result<IrDeconstructionComponent, TransformError> {
    match pattern {
        Pattern::Wildcard(_) => Ok(IrDeconstructionComponent::Wildcard),
        Pattern::Identifier(name, _) => Ok(IrDeconstructionComponent::Binding { name }),
        Pattern::Literal(literal, _) => Ok(IrDeconstructionComponent::Literal(literal)),
        Pattern::Constructor {
            name,
            patterns,
            span,
        } => {
            let (nested, nested_guard) =
                lower_constructor_deconstruction(patterns, context, &span, current_depth)?;
            if nested_guard.is_some() {
                return Err(TransformError::UnsupportedConstruct {
                    construct: unsupported_nested_feature_message(
                        "Nested guards inside destructuring patterns are not supported yet",
                        "分解パターン内の guard はまだサポートされていません",
                    ),
                    span,
                });
            }
            Ok(IrDeconstructionComponent::Type {
                type_name: name,
                pattern: nested.map(Box::new),
            })
        }
        Pattern::Guard { span, .. } => Err(TransformError::UnsupportedConstruct {
            construct: unsupported_nested_feature_message(
                "Guard patterns inside destructuring require Phase 4 diagnostics",
                "分解パターン内の guard は現行フェーズでは未対応です",
            ),
            span,
        }),
        Pattern::Range { span, .. } => Err(TransformError::UnsupportedConstruct {
            construct: unsupported_nested_feature_message(
                "Range patterns inside destructuring are not supported",
                "分解パターン内の範囲指定はサポートされていません",
            ),
            span,
        }),
    }
}

fn describe_exhaustiveness(summary: &PatternAnalysisSummary) -> String {
    match summary.is_exhaustive {
        Some(true) => "true".to_string(),
        Some(false) => "false".to_string(),
        None => "unknown".to_string(),
    }
}

fn type_name_for_case(java_type: &JavaType) -> String {
    match java_type {
        JavaType::Primitive(name) => name.clone(),
        JavaType::Reference { name, .. } => name.clone(),
        JavaType::Array { element_type, .. } => {
            format!("{}[]", type_name_for_case(element_type.as_ref()))
        }
        JavaType::Functional { interface_name, .. } => interface_name.clone(),
        JavaType::Wildcard { kind, bound } => match kind {
            JavaWildcardKind::Unbounded => "Object".to_string(),
            JavaWildcardKind::Extends | JavaWildcardKind::Super => bound
                .as_ref()
                .map(|inner| type_name_for_case(inner))
                .unwrap_or_else(|| "Object".to_string()),
        },
        JavaType::Void => "Object".to_string(),
    }
}

fn unsupported_pattern_message(reason_en: &str, reason_ja: &str) -> String {
    format!(
        "JV3199: {reason_ja}。今後のアップデートで順次対応予定です。\nJV3199: {reason_en}. This construct will be supported in a forthcoming update.\n--explain JV3199: Advanced pattern matching features such as deep guards or range checks inside destructuring are partially limited in this release. Rewrite the pattern using supported constructs or target a newer compiler build."
    )
}

fn unsupported_nested_feature_message(reason_en: &str, reason_ja: &str) -> String {
    format!(
        "JV3199: {reason_ja}。後続タスクで対応予定です。\nJV3199: {reason_en}. This construct will be available in a subsequent update.\n--explain JV3199: Advanced nested pattern features (guards, ranges) are limited in the current release."
    )
}
