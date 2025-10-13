use super::*;
use jv_ast::Span;
use jv_ir::PipelineShape;
use jv_ir::{
    SequencePipeline, SequenceSource, SequenceStage, SequenceTerminal, SequenceTerminalKind,
};

impl JavaCodeGenerator {
    pub fn generate_expression(&mut self, expr: &IrExpression) -> Result<String, CodeGenError> {
        match expr {
            IrExpression::Literal(literal, _) => Ok(Self::literal_to_string(literal)),
            IrExpression::RegexPattern { pattern, .. } => {
                self.add_import("java.util.regex.Pattern");
                Ok(format!(
                    "Pattern.compile(\"{}\")",
                    Self::escape_string(pattern)
                ))
            }
            IrExpression::Identifier { name, .. } => Ok(name.clone()),
            IrExpression::MethodCall {
                receiver,
                method_name,
                args,
                argument_style,
                ..
            } => {
                let mut invocation = String::new();
                if let Some(target) = receiver {
                    invocation.push_str(&self.generate_expression(target)?);
                    invocation.push('.');
                }
                invocation.push_str(method_name);
                invocation.push('(');
                invocation.push_str(&self.render_arguments_with_style(args, *argument_style)?);
                invocation.push(')');
                Ok(invocation)
            }
            IrExpression::FieldAccess {
                receiver,
                field_name,
                ..
            } => Ok(format!(
                "{}.{}",
                self.generate_expression(receiver)?,
                field_name
            )),
            IrExpression::ArrayAccess { array, index, .. } => Ok(format!(
                "{}[{}]",
                self.generate_expression(array)?,
                self.generate_expression(index)?
            )),
            IrExpression::Binary {
                left, op, right, ..
            } => {
                if matches!(op, BinaryOp::Elvis) {
                    let left_expr = self.generate_expression(left)?;
                    let right_expr = self.generate_expression(right)?;
                    Ok(format!("({0} != null ? {0} : {1})", left_expr, right_expr))
                } else {
                    Ok(format!(
                        "{} {} {}",
                        self.generate_expression(left)?,
                        self.generate_binary_op(op)?,
                        self.generate_expression(right)?
                    ))
                }
            }
            IrExpression::Unary { op, operand, .. } => {
                let operand_code = self.generate_expression(operand)?;
                Ok(match op {
                    UnaryOp::Not => format!("!{}", operand_code),
                    UnaryOp::Minus => format!("-{}", operand_code),
                    UnaryOp::Plus => format!("+{}", operand_code),
                    UnaryOp::BitNot => format!("~{}", operand_code),
                })
            }
            IrExpression::Assignment { target, value, .. } => Ok(format!(
                "{} = {}",
                self.generate_expression(target)?,
                self.generate_expression(value)?
            )),
            IrExpression::Conditional {
                condition,
                then_expr,
                else_expr,
                ..
            } => Ok(format!(
                "{} ? {} : {}",
                self.generate_expression(condition)?,
                self.generate_expression(then_expr)?,
                self.generate_expression(else_expr)?
            )),
            IrExpression::Block { statements, .. } => {
                let mut builder = self.builder();
                builder.push_line("{");
                builder.indent();
                for statement in statements {
                    let stmt_code = self.generate_statement(statement)?;
                    Self::push_lines(&mut builder, &stmt_code);
                }
                builder.dedent();
                builder.push_line("}");
                Ok(builder.build())
            }
            IrExpression::ArrayCreation {
                element_type,
                dimensions,
                initializer,
                delimiter,
                ..
            } => {
                if *delimiter == SequenceDelimiter::Whitespace {
                    if let Some(values) = initializer {
                        return self.render_whitespace_array(values);
                    }
                }

                let mut expr_str = format!("new {}", self.generate_type(element_type)?);
                if let Some(values) = initializer {
                    expr_str.push('{');
                    expr_str.push_str(&self.render_arguments(values)?);
                    expr_str.push('}');
                } else {
                    for dim in dimensions {
                        expr_str.push('[');
                        if let Some(size_expr) = dim {
                            expr_str.push_str(&self.generate_expression(size_expr)?);
                        }
                        expr_str.push(']');
                    }
                }
                Ok(expr_str)
            }
            IrExpression::ObjectCreation {
                class_name,
                generic_args,
                args,
                ..
            } => {
                let mut expr_str = String::from("new ");
                expr_str.push_str(class_name);
                if !generic_args.is_empty() {
                    let mut rendered = Vec::new();
                    for arg in generic_args {
                        rendered.push(self.generate_type(arg)?);
                    }
                    expr_str.push('<');
                    expr_str.push_str(&rendered.join(", "));
                    expr_str.push('>');
                }
                expr_str.push('(');
                expr_str.push_str(&self.render_arguments(args)?);
                expr_str.push(')');
                Ok(expr_str)
            }
            IrExpression::Lambda {
                param_names, body, ..
            } => {
                let params = param_names.join(", ");
                let body_str = self.generate_expression(body)?;
                Ok(format!("({}) -> {}", params, body_str))
            }
            IrExpression::SequencePipeline {
                pipeline,
                java_type,
                span,
            } => self.generate_sequence_pipeline_expression(pipeline, java_type, span),
            IrExpression::Switch { .. } => self.generate_switch_expression(expr),
            IrExpression::Cast {
                expr, target_type, ..
            } => Ok(format!(
                "({}) {}",
                self.generate_type(target_type)?,
                self.generate_expression(expr)?
            )),
            IrExpression::InstanceOf {
                expr, target_type, ..
            } => Ok(format!(
                "{} instanceof {}",
                self.generate_expression(expr)?,
                self.generate_type(target_type)?
            )),
            IrExpression::This { .. } => Ok("this".to_string()),
            IrExpression::Super { .. } => Ok("super".to_string()),
            IrExpression::NullSafeOperation {
                expr,
                operation,
                default_value,
                ..
            } => self.generate_null_safe_operation(expr, operation, default_value.as_deref()),
            IrExpression::StringFormat {
                format_string,
                args,
                ..
            } => self.generate_string_format(format_string, args),
            IrExpression::CompletableFuture {
                operation, args, ..
            } => self.generate_completable_future(operation.clone(), args),
            IrExpression::VirtualThread {
                operation, args, ..
            } => self.generate_virtual_thread(operation.clone(), args),
            IrExpression::TryWithResources {
                resources, body, ..
            } => self.generate_try_with_resources_expression(resources, body),
        }
    }

    pub fn generate_switch_expression(
        &mut self,
        switch: &IrExpression,
    ) -> Result<String, CodeGenError> {
        if let IrExpression::Switch {
            discriminant,
            cases,
            java_type,
            implicit_end,
            strategy_description,
            span,
        } = switch
        {
            if self.targeting.supports_pattern_switch() {
                self.render_switch_expression_java25(
                    discriminant,
                    cases,
                    implicit_end.as_ref(),
                    strategy_description.as_ref(),
                )
            } else {
                self.render_switch_expression_java21(
                    discriminant,
                    cases,
                    java_type,
                    implicit_end.as_ref(),
                    strategy_description.as_ref(),
                    span,
                )
            }
        } else {
            Err(CodeGenError::UnsupportedConstruct {
                construct: "Expected switch expression".to_string(),
                span: None,
            })
        }
    }

    fn render_switch_expression_java25(
        &mut self,
        discriminant: &IrExpression,
        cases: &[IrSwitchCase],
        implicit_end: Option<&IrImplicitWhenEnd>,
        strategy_description: Option<&String>,
    ) -> Result<String, CodeGenError> {
        let mut builder = self.builder();
        if let Some(description) = strategy_description {
            builder.push_line(&format!("// {}", description));
        }
        builder.push_line(&format!(
            "switch ({}) {{",
            self.generate_expression(discriminant)?
        ));
        builder.indent();
        for case in cases {
            if let Some(comment) = self.render_case_leading_comment(case)? {
                builder.push_line(&comment);
            }
            let guard = match &case.guard {
                Some(guard_expr) => {
                    format!(" when ({})", self.generate_expression(guard_expr)?)
                }
                None => String::new(),
            };
            let body_expr = self.generate_expression(&case.body)?;
            if Self::is_default_only_case(case) {
                builder.push_line(&format!("default{} -> {}", guard, body_expr));
            } else {
                let labels = self.render_case_labels(&case.labels)?;
                builder.push_line(&format!("case {}{} -> {}", labels, guard, body_expr));
            }
        }
        if let Some(implicit) = implicit_end {
            if let Some(rendered) = self.render_implicit_when_end_case(implicit, cases)? {
                builder.push_line(&rendered);
            }
        }
        builder.dedent();
        builder.push_line("}");
        Ok(builder.build())
    }

    fn render_case_leading_comment(
        &mut self,
        case: &IrSwitchCase,
    ) -> Result<Option<String>, CodeGenError> {
        for label in &case.labels {
            if let IrCaseLabel::Range {
                lower,
                upper,
                inclusive_end,
                ..
            } = label
            {
                let lower_rendered = self.generate_expression(lower.as_ref())?;
                let upper_rendered = self.generate_expression(upper.as_ref())?;
                let operator = if *inclusive_end { "..=" } else { ".." };
                return Ok(Some(format!(
                    "// range: {}{}{}",
                    lower_rendered, operator, upper_rendered
                )));
            }
        }

        Ok(None)
    }

    fn java21_incompatibility_error(
        &self,
        span: &Span,
        detail_ja: &str,
        detail_en: &str,
    ) -> CodeGenError {
        let target = self.targeting.target().as_str();
        let message = format!(
            "JV3105: Java {target} では {detail_ja}。ターゲットを Java 25 に上げるか、パターンを単純化してください。\nJV3105: Target Java {target} cannot lower this when arm because {detail_en}. Raise the target to Java 25 or simplify the branch.\n--explain JV3105: Java 21 fallback supports only literal labels, simple type patterns, and range guards. Advanced pattern forms require Java 25 pattern switches."
        );

        CodeGenError::PatternMatchingError {
            message,
            span: Some(span.clone()),
        }
    }

    fn render_switch_expression_java21(
        &mut self,
        discriminant: &IrExpression,
        cases: &[IrSwitchCase],
        java_type: &JavaType,
        _implicit_end: Option<&IrImplicitWhenEnd>,
        strategy_description: Option<&String>,
        span: &Span,
    ) -> Result<String, CodeGenError> {
        if cases.is_empty() {
            return Err(CodeGenError::UnsupportedConstruct {
                construct: "when expression must contain at least one arm".to_string(),
                span: Some(span.clone()),
            });
        }

        let mut non_default_cases = Vec::new();
        let mut default_case: Option<&IrSwitchCase> = None;

        for case in cases {
            if Self::is_default_only_case(case) {
                if default_case.is_some() {
                    return Err(CodeGenError::UnsupportedConstruct {
                        construct:
                            "Multiple default branches are not supported in Java 21 fallback"
                                .to_string(),
                        span: Some(case.span.clone()),
                    });
                }
                default_case = Some(case);
            } else {
                non_default_cases.push(case);
            }
        }

        let result_is_void = Self::is_void_like(java_type);
        let subject_binding = "__subject";
        let result_binding = "__matchResult";

        let subject_expr = self.generate_expression(discriminant)?;
        let result_type = if result_is_void {
            String::new()
        } else {
            self.generate_type(java_type)?
        };

        let mut builder = self.builder();
        if let Some(description) = strategy_description {
            builder.push_line(&format!("// {}", description));
        }
        builder.push_line("new Object() {");
        builder.indent();
        builder.push_line(&format!(
            "{} matchExpr() {{",
            if result_is_void {
                "void".to_string()
            } else {
                result_type.clone()
            }
        ));
        builder.indent();

        builder.push_line(&format!(
            "final var {} = {};",
            subject_binding, subject_expr
        ));
        if !result_is_void {
            builder.push_line(&format!("final {} {};", result_type, result_binding));
        }

        builder.push_line("boolean __matched = false;");
        for case in non_default_cases {
            if let Some(block) = self.render_case_condition_block_java21(
                case,
                subject_binding,
                result_is_void,
                result_binding,
            )? {
                builder.push_line("if (!__matched) {");
                builder.indent();
                Self::push_lines(&mut builder, &block);
                builder.dedent();
                builder.push_line("}");
            } else {
                return Err(self.java21_incompatibility_error(
                    &case.span,
                    "Java 21 フォールバックがこの分岐の条件を構築できません",
                    "Java 21 fallback could not derive a condition for this branch",
                ));
            }
        }

        if let Some(case) = default_case {
            builder.push_line("if (!__matched) {");
            builder.indent();
            self.write_switch_case_body_java21(&mut builder, case, result_is_void, result_binding)?;
            builder.push_line("__matched = true;");
            builder.dedent();
            builder.push_line("}");
        } else if !result_is_void {
            builder.push_line("if (!__matched) {");
            builder.indent();
            self.add_import("java.lang.IllegalStateException");
            builder
                .push_line("throw new IllegalStateException(\"non-exhaustive when expression\");");
            builder.dedent();
            builder.push_line("}");
        }

        if result_is_void {
            builder.push_line("return;");
        } else {
            builder.push_line(&format!("return {};", result_binding));
        }

        builder.dedent();
        builder.push_line("}");
        builder.dedent();
        builder.push_line("}.matchExpr()");

        Ok(builder.build())
    }

    fn render_switch_case_condition_java21(
        &mut self,
        case: &IrSwitchCase,
        subject_binding: &str,
    ) -> Result<Option<String>, CodeGenError> {
        let mut literal_conditions = Vec::new();
        let mut type_pattern_condition: Option<String> = None;

        for label in &case.labels {
            match label {
                IrCaseLabel::Default => {}
                IrCaseLabel::Literal(literal) => {
                    literal_conditions
                        .push(self.literal_condition_java21(subject_binding, literal)?);
                }
                IrCaseLabel::TypePattern {
                    type_name,
                    variable,
                    deconstruction: None,
                } => {
                    if !literal_conditions.is_empty() || type_pattern_condition.is_some() {
                        return Err(self.java21_incompatibility_error(
                            &case.span,
                            "同じ when 分岐で複数の型パターンやリテラルと組み合わせることはできません",
                            "this branch mixes literals or multiple type patterns",
                        ));
                    }
                    type_pattern_condition = Some(format!(
                        "{} instanceof {} {}",
                        subject_binding, type_name, variable
                    ));
                }
                IrCaseLabel::TypePattern {
                    deconstruction: Some(_),
                    ..
                } => {
                    // Nested destructuring is handled by the higher level block builder.
                    return Ok(None);
                }
                IrCaseLabel::Range {
                    type_name,
                    variable,
                    ..
                } => {
                    if !literal_conditions.is_empty() || type_pattern_condition.is_some() {
                        return Err(self.java21_incompatibility_error(
                            &case.span,
                            "範囲パターンを同じ分岐の他のラベルと混在させることはできません",
                            "range patterns cannot be combined with other labels in the same branch",
                        ));
                    }
                    type_pattern_condition = Some(format!(
                        "{} instanceof {} {}",
                        subject_binding, type_name, variable
                    ));
                }
            }
        }

        let mut condition = if let Some(pattern_condition) = type_pattern_condition {
            pattern_condition
        } else if literal_conditions.is_empty() {
            return Ok(None);
        } else if literal_conditions.len() == 1 {
            literal_conditions.remove(0)
        } else {
            format!("({})", literal_conditions.join(" || "))
        };

        if let Some(guard) = case.guard.as_ref() {
            let guard_expr = self.generate_expression(guard)?;
            let guard_trim = guard_expr.trim();
            if !guard_trim.is_empty() && guard_trim != "true" {
                if condition.is_empty() {
                    condition = format!("({})", guard_trim);
                } else {
                    condition = format!("({}) && ({})", condition, guard_trim);
                }
            }
        }

        Ok(Some(condition))
    }

    fn render_case_condition_block_java21(
        &mut self,
        case: &IrSwitchCase,
        subject_binding: &str,
        result_is_void: bool,
        result_binding: &str,
    ) -> Result<Option<String>, CodeGenError> {
        if Self::case_has_deconstruction(case) {
            return self
                .render_deconstruction_case_block_java21(
                    case,
                    subject_binding,
                    result_is_void,
                    result_binding,
                )
                .map(Some);
        }

        if let Some(condition) = self.render_switch_case_condition_java21(case, subject_binding)? {
            let mut builder = self.builder();
            builder.push_line(&format!("if ({}) {{", condition));
            builder.indent();
            self.write_switch_case_body_java21(&mut builder, case, result_is_void, result_binding)?;
            builder.push_line("__matched = true;");
            builder.dedent();
            builder.push_line("}");
            Ok(Some(builder.build()))
        } else {
            Ok(None)
        }
    }

    fn case_has_deconstruction(case: &IrSwitchCase) -> bool {
        case.labels.iter().any(|label| {
            matches!(
                label,
                IrCaseLabel::TypePattern {
                    deconstruction: Some(_),
                    ..
                }
            )
        })
    }

    fn render_deconstruction_case_block_java21(
        &mut self,
        case: &IrSwitchCase,
        subject_binding: &str,
        result_is_void: bool,
        result_binding: &str,
    ) -> Result<String, CodeGenError> {
        let (type_name, variable, pattern) = case
            .labels
            .iter()
            .find_map(|label| match label {
                IrCaseLabel::TypePattern {
                    type_name,
                    variable,
                    deconstruction: Some(pattern),
                } => Some((type_name.clone(), variable.clone(), pattern.clone())),
                _ => None,
            })
            .ok_or_else(|| {
                self.java21_incompatibility_error(
                    &case.span,
                    "分解パターンを含む case ラベルが見つかりません",
                    "no destructuring pattern label available for this branch",
                )
            })?;

        let mut builder = self.builder();
        let pattern_label = self.render_type_deconstruction_label(&type_name, &pattern)?;

        builder.push_line("do {");
        builder.indent();
        builder.push_line(&format!(
            "if (!({} instanceof {} {})) {{",
            subject_binding, pattern_label, variable
        ));
        builder.indent();
        builder.push_line("break;");
        builder.dedent();
        builder.push_line("}");

        if let Some(guard) = case.guard.as_ref() {
            let guard_expr = self.generate_expression(guard)?;
            builder.push_line(&format!("if (!({})) {{", guard_expr));
            builder.indent();
            builder.push_line("break;");
            builder.dedent();
            builder.push_line("}");
        }

        builder.push_line("__matched = true;");
        self.write_switch_case_body_java21(&mut builder, case, result_is_void, result_binding)?;
        builder.push_line("break;");

        builder.dedent();
        builder.push_line("} while (false);");

        Ok(builder.build())
    }

    fn literal_condition_java21(
        &mut self,
        subject_binding: &str,
        literal: &Literal,
    ) -> Result<String, CodeGenError> {
        let literal_value = Self::literal_to_string(literal);
        self.add_import("java.util.Objects");
        Ok(format!(
            "java.util.Objects.equals({}, {})",
            subject_binding, literal_value
        ))
    }

    fn write_switch_case_body_java21(
        &mut self,
        builder: &mut JavaSourceBuilder,
        case: &IrSwitchCase,
        result_is_void: bool,
        result_binding: &str,
    ) -> Result<(), CodeGenError> {
        let mut body_expr = self.generate_expression(&case.body)?;
        let body_is_block = body_expr.trim_start().starts_with('{');
        let body_has_semicolon = body_expr.trim_end().ends_with(';');

        if result_is_void {
            if body_is_block {
                Self::push_lines(builder, &body_expr);
            } else {
                if !body_has_semicolon {
                    body_expr.push(';');
                }
                builder.push_line(&body_expr);
            }
        } else {
            if body_is_block {
                return Err(self.java21_incompatibility_error(
                    &case.span,
                    "Java 21 フォールバックでは値分岐にブロック式を使用できません",
                    "block expressions as branch results are not supported when targeting Java 21",
                ));
            }
            let mut assignment = format!("{} = {}", result_binding, body_expr);
            if !assignment.trim_end().ends_with(';') {
                assignment.push(';');
            }
            builder.push_line(&assignment);
        }

        Ok(())
    }

    pub fn generate_null_safe_operation(
        &mut self,
        expr: &IrExpression,
        operation: &IrExpression,
        default_value: Option<&IrExpression>,
    ) -> Result<String, CodeGenError> {
        let expr_code = self.generate_expression(expr)?;
        let op_code = self.generate_expression(operation)?;
        let default_code = match default_value {
            Some(value) => self.generate_expression(value)?,
            None => "null".to_string(),
        };
        Ok(format!(
            "({expr} != null ? {op} : {default})",
            expr = expr_code,
            op = op_code,
            default = default_code
        ))
    }

    pub fn generate_completable_future(
        &mut self,
        op: CompletableFutureOp,
        args: &[IrExpression],
    ) -> Result<String, CodeGenError> {
        self.add_import("java.util.concurrent.CompletableFuture");
        let rendered_args = self.render_argument_vec(args)?;
        Ok(match op {
            CompletableFutureOp::SupplyAsync => match rendered_args.as_slice() {
                [supplier] => format!("CompletableFuture.supplyAsync({})", supplier),
                [supplier, executor] => {
                    format!("CompletableFuture.supplyAsync({}, {})", supplier, executor)
                }
                _ => {
                    return Err(CodeGenError::InvalidMethodSignature {
                        message: "supplyAsync expects supplier (and optional executor)".to_string(),
                        span: None,
                    })
                }
            },
            CompletableFutureOp::ThenApply => match rendered_args.as_slice() {
                [future, func] => format!("{}.thenApply({})", future, func),
                _ => {
                    return Err(CodeGenError::InvalidMethodSignature {
                        message: "thenApply expects future and function".to_string(),
                        span: None,
                    })
                }
            },
            CompletableFutureOp::ThenCompose => match rendered_args.as_slice() {
                [future, func] => format!("{}.thenCompose({})", future, func),
                _ => {
                    return Err(CodeGenError::InvalidMethodSignature {
                        message: "thenCompose expects future and function".to_string(),
                        span: None,
                    })
                }
            },
            CompletableFutureOp::Get => match rendered_args.as_slice() {
                [future] => format!("{}.get()", future),
                _ => {
                    return Err(CodeGenError::InvalidMethodSignature {
                        message: "get expects future".to_string(),
                        span: None,
                    })
                }
            },
            CompletableFutureOp::CompletedFuture => match rendered_args.as_slice() {
                [value] => format!("CompletableFuture.completedFuture({})", value),
                _ => {
                    return Err(CodeGenError::InvalidMethodSignature {
                        message: "completedFuture expects a single value".to_string(),
                        span: None,
                    })
                }
            },
        })
    }

    pub fn generate_virtual_thread(
        &mut self,
        op: VirtualThreadOp,
        args: &[IrExpression],
    ) -> Result<String, CodeGenError> {
        self.add_import("java.lang.Thread");
        match op {
            VirtualThreadOp::Start => match args {
                [runnable] => Ok(format!(
                    "Thread.ofVirtual().start({})",
                    self.generate_expression(runnable)?
                )),
                _ => Err(CodeGenError::InvalidMethodSignature {
                    message: "Thread.ofVirtual().start expects runnable".to_string(),
                    span: None,
                }),
            },
            VirtualThreadOp::Factory => Ok("Thread.ofVirtual().factory()".to_string()),
        }
    }

    pub fn generate_try_with_resources_expression(
        &mut self,
        resources: &[IrResource],
        body: &IrExpression,
    ) -> Result<String, CodeGenError> {
        let mut builder = self.builder();
        builder.push_line(&self.render_try_with_resources_header(resources)?);
        builder.indent();
        let body_code = self.generate_expression(body)?;
        if body_code.trim_start().starts_with('{') {
            builder.push_line(&body_code);
        } else {
            builder.push_line(&format!("return {};", body_code));
        }
        builder.dedent();
        builder.push_line("}");
        Ok(builder.build())
    }

    fn generate_sequence_pipeline_expression(
        &mut self,
        pipeline: &SequencePipeline,
        result_type: &JavaType,
        span: &Span,
    ) -> Result<String, CodeGenError> {
        let (source_expr, needs_resource_guard) = self.render_sequence_source(&pipeline.source)?;

        if pipeline.terminal.is_none() {
            let chained = self.render_sequence_stage_chain(&source_expr, pipeline)?;
            return self.render_lazy_sequence_pipeline(
                chained,
                needs_resource_guard,
                result_type,
                span,
            );
        }

        let needs_helper = self.sequence_pipeline_requires_helper(pipeline);

        if needs_helper {
            return self.render_sequence_pipeline_with_helper(
                &source_expr,
                pipeline,
                result_type,
                needs_resource_guard,
                span,
            );
        }

        if needs_resource_guard {
            let chained = self.render_sequence_chain("__jvStream", pipeline)?;
            let normalized_result_type = Self::normalize_void_like(result_type);
            let return_type = self.generate_type(normalized_result_type.as_ref())?;
            let is_void = Self::is_void_like(normalized_result_type.as_ref());

            let mut builder = self.builder();
            builder.push_line("new Object() {");
            builder.indent();
            builder.push_line(&format!("{} run() {{", return_type));
            builder.indent();
            builder.push_line(&format!("try (var __jvStream = {}) {{", source_expr));
            builder.indent();
            if is_void {
                builder.push_line(&format!("{};", chained));
            } else {
                builder.push_line(&format!("return {};", chained));
            }
            builder.dedent();
            builder.push_line("}");
            builder.dedent();
            builder.push_line("}");
            builder.dedent();
            builder.push_line("}.run()");
            Ok(builder.build())
        } else {
            self.render_sequence_chain(&source_expr, pipeline)
        }
    }

    fn sequence_pipeline_requires_helper(&self, pipeline: &SequencePipeline) -> bool {
        match &pipeline.shape {
            PipelineShape::MultiStage {
                stages,
                repeated_transforms,
                ..
            } => *stages >= 2 || *repeated_transforms,
            PipelineShape::ExplicitSequenceSource => true,
            _ => false,
        }
    }

    fn render_lazy_sequence_pipeline(
        &mut self,
        stream_expr: String,
        _needs_resource_guard: bool,
        result_type: &JavaType,
        span: &Span,
    ) -> Result<String, CodeGenError> {
        match result_type {
            JavaType::Reference { name, .. } if name == "jv.collections.SequenceCore" => {}
            _ => {
                return Err(CodeGenError::TypeGenerationError {
                    message: "lazy sequence pipeline must resolve to jv.collections.SequenceCore"
                        .to_string(),
                    span: Some(span.clone()),
                })
            }
        }

        self.add_import("jv.collections.SequenceFactory");
        Ok(format!("SequenceFactory.fromStream({})", stream_expr))
    }

    fn render_sequence_source(
        &mut self,
        source: &SequenceSource,
    ) -> Result<(String, bool), CodeGenError> {
        match source {
            SequenceSource::Collection { expr, .. } => {
                let rendered = self.generate_expression(expr)?;
                Ok((format!("({}).stream()", rendered), false))
            }
            SequenceSource::Array { expr, .. } => {
                let rendered = self.generate_expression(expr)?;
                self.add_import("java.util.Arrays");
                Ok((format!("Arrays.stream({})", rendered), false))
            }
            SequenceSource::ListLiteral { elements, .. } => {
                let list_expr = self.render_whitespace_array(elements)?;
                Ok((format!("({}).stream()", list_expr), false))
            }
            SequenceSource::JavaStream {
                expr, auto_close, ..
            } => {
                let rendered = self.generate_expression(expr)?;
                Ok((rendered, *auto_close))
            }
        }
    }

    fn render_sequence_stage_chain(
        &mut self,
        start_expr: &str,
        pipeline: &SequencePipeline,
    ) -> Result<String, CodeGenError> {
        let mut chain = start_expr.to_string();
        for stage in &pipeline.stages {
            chain.push_str(&self.render_sequence_stage(stage)?);
        }
        Ok(chain)
    }

    fn render_sequence_chain(
        &mut self,
        start_expr: &str,
        pipeline: &SequencePipeline,
    ) -> Result<String, CodeGenError> {
        let mut chain = start_expr.to_string();
        for stage in &pipeline.stages {
            chain.push_str(&self.render_sequence_stage(stage)?);
        }

        if let Some(terminal) = &pipeline.terminal {
            self.render_sequence_terminal(chain, terminal)
        } else {
            Ok(chain)
        }
    }

    fn render_sequence_stage(&mut self, stage: &SequenceStage) -> Result<String, CodeGenError> {
        match stage {
            SequenceStage::Map { lambda, .. } => {
                let rendered = self.generate_expression(lambda)?;
                Ok(format!(".map({})", rendered))
            }
            SequenceStage::Filter { predicate, .. } => {
                let rendered = self.generate_expression(predicate)?;
                Ok(format!(".filter({})", rendered))
            }
            SequenceStage::FlatMap { lambda, .. } => {
                let rendered = self.generate_expression(lambda)?;
                Ok(format!(".flatMap({})", rendered))
            }
            SequenceStage::Take { count, .. } => {
                let rendered = self.generate_expression(count)?;
                Ok(format!(".limit({})", rendered))
            }
            SequenceStage::Drop { count, .. } => {
                let rendered = self.generate_expression(count)?;
                Ok(format!(".skip({})", rendered))
            }
            SequenceStage::Sorted { comparator, .. } => match comparator {
                Some(comp) => {
                    let rendered = self.generate_expression(comp)?;
                    Ok(format!(".sorted({})", rendered))
                }
                None => Ok(".sorted()".to_string()),
            },
        }
    }

    fn render_sequence_pipeline_with_helper(
        &mut self,
        source_expr: &str,
        pipeline: &SequencePipeline,
        result_type: &JavaType,
        needs_resource_guard: bool,
        span: &Span,
    ) -> Result<String, CodeGenError> {
        let terminal =
            pipeline
                .terminal
                .as_ref()
                .ok_or_else(|| CodeGenError::TypeGenerationError {
                    message: "helper-assisted sequence pipeline requires a terminal operation"
                        .to_string(),
                    span: Some(span.clone()),
                })?;

        self.ensure_sequence_helper();

        let normalized_result_type = Self::normalize_void_like(result_type);
        let return_type = self.generate_type(normalized_result_type.as_ref())?;
        let is_void = Self::is_void_like(normalized_result_type.as_ref());
        let helper_var = "__jvSequence";

        let helper_stream = format!("{}.toStream()", helper_var);
        let terminal_expr = self.render_sequence_terminal(helper_stream, terminal)?;

        let mut builder = self.builder();
        builder.push_line("new Object() {");
        builder.indent();
        builder.push_line(&format!("{} run() {{", return_type));
        builder.indent();

        if needs_resource_guard {
            builder.push_line(&format!("try (var __jvStream = {}) {{", source_expr));
            builder.indent();
            let stage_chain = self.render_sequence_stage_chain("__jvStream", pipeline)?;
            builder.push_line(&format!(
                "try (var {} = new JvSequence<>({})) {{",
                helper_var, stage_chain
            ));
            builder.indent();
            if is_void {
                builder.push_line(&format!("{};", terminal_expr));
            } else {
                builder.push_line(&format!("return {};", terminal_expr));
            }
            builder.dedent();
            builder.push_line("}");
            builder.dedent();
            builder.push_line("}");
        } else {
            let stage_chain = self.render_sequence_stage_chain(source_expr, pipeline)?;
            builder.push_line(&format!(
                "try (var {} = new JvSequence<>({})) {{",
                helper_var, stage_chain
            ));
            builder.indent();
            if is_void {
                builder.push_line(&format!("{};", terminal_expr));
            } else {
                builder.push_line(&format!("return {};", terminal_expr));
            }
            builder.dedent();
            builder.push_line("}");
        }

        builder.dedent();
        builder.push_line("}");
        builder.dedent();
        builder.push_line("}.run()");

        Ok(builder.build())
    }

    fn render_sequence_terminal(
        &mut self,
        chain: String,
        terminal: &SequenceTerminal,
    ) -> Result<String, CodeGenError> {
        match &terminal.kind {
            SequenceTerminalKind::ToList => {
                if self.targeting.supports_collection_factories() {
                    Ok(format!("{}{}", chain, ".toList()"))
                } else {
                    self.add_import("java.util.stream.Collectors");
                    Ok(format!("{}.collect(Collectors.toList())", chain))
                }
            }
            SequenceTerminalKind::ToSet => {
                self.add_import("java.util.stream.Collectors");
                Ok(format!("{}.collect(Collectors.toSet())", chain))
            }
            SequenceTerminalKind::Fold {
                initial,
                accumulator,
            } => {
                let initial_expr = self.generate_expression(initial)?;
                let accumulator_expr = self.generate_expression(accumulator)?;
                Ok(format!(
                    "{}.reduce({}, {})",
                    chain, initial_expr, accumulator_expr
                ))
            }
            SequenceTerminalKind::Reduce { accumulator } => {
                let accumulator_expr = self.generate_expression(accumulator)?;
                let mut rendered = format!("{}.reduce({})", chain, accumulator_expr);
                if terminal.requires_non_empty_source {
                    self.add_import("java.lang.IllegalArgumentException");
                    rendered.push_str(
                        ".orElseThrow(() -> new IllegalArgumentException(\"Sequence reduce() on empty source\"))",
                    );
                }
                Ok(rendered)
            }
            SequenceTerminalKind::GroupBy { key_selector } => {
                let selector_expr = self.generate_expression(key_selector)?;
                self.add_import("java.util.stream.Collectors");
                Ok(format!(
                    "{}.collect(Collectors.groupingBy({}))",
                    chain, selector_expr
                ))
            }
            SequenceTerminalKind::Associate { pair_selector } => {
                let selector_expr = self.generate_expression(pair_selector)?;
                self.add_import("java.util.stream.Collectors");
                self.add_import("java.util.Map");
                let mapped = format!("{}.map({})", chain, selector_expr);
                Ok(format!(
                    "{}.collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue))",
                    mapped
                ))
            }
            SequenceTerminalKind::Count => Ok(format!("{}.count()", chain)),
            SequenceTerminalKind::Sum => Ok(format!(
                "{}.mapToLong(value -> ((Number) value).longValue()).sum()",
                chain
            )),
            SequenceTerminalKind::ForEach { action } => {
                let action_expr = self.generate_expression(action)?;
                Ok(format!("{}.forEach({})", chain, action_expr))
            }
        }
    }

    fn render_arguments(&mut self, args: &[IrExpression]) -> Result<String, CodeGenError> {
        let rendered = self.render_argument_vec(args)?;
        Ok(rendered.join(", "))
    }

    fn render_arguments_with_style(
        &mut self,
        args: &[IrExpression],
        style: CallArgumentStyle,
    ) -> Result<String, CodeGenError> {
        let rendered = self.render_argument_vec(args)?;

        match style {
            CallArgumentStyle::Comma => Ok(rendered.join(", ")),
            CallArgumentStyle::Whitespace => {
                if rendered.is_empty() {
                    return Ok(String::new());
                }

                if rendered.len() == 1 {
                    return Ok(rendered.into_iter().next().unwrap());
                }

                Ok(self.render_multiline_arguments(&rendered))
            }
        }
    }

    fn render_multiline_arguments(&self, rendered: &[String]) -> String {
        if rendered.is_empty() {
            return String::new();
        }

        let mut result = String::new();
        result.push('\n');
        for (idx, arg) in rendered.iter().enumerate() {
            result.push_str(&self.config.indent);
            result.push_str(arg);
            if idx + 1 != rendered.len() {
                result.push_str(",\n");
            } else {
                result.push('\n');
            }
        }
        result
    }

    fn render_argument_vec(&mut self, args: &[IrExpression]) -> Result<Vec<String>, CodeGenError> {
        let mut rendered = Vec::new();
        for arg in args {
            rendered.push(self.generate_expression(arg)?);
        }
        Ok(rendered)
    }

    fn render_whitespace_array(&mut self, values: &[IrExpression]) -> Result<String, CodeGenError> {
        let rendered = self.render_argument_vec(values)?;
        let joined = rendered.join(", ");

        self.add_import("java.util.List");

        if self.targeting.supports_collection_factories() {
            Ok(format!("List.of({})", joined))
        } else {
            self.add_import("java.util.Arrays");
            if rendered.is_empty() {
                Ok("Arrays.asList().stream().toList()".to_string())
            } else {
                Ok(format!("Arrays.asList({}).stream().toList()", joined))
            }
        }
    }

    pub(super) fn render_case_labels(
        &mut self,
        labels: &[IrCaseLabel],
    ) -> Result<String, CodeGenError> {
        let mut rendered = Vec::new();
        for label in labels.iter().cloned() {
            match label {
                IrCaseLabel::Literal(literal) => rendered.push(Self::literal_to_string(&literal)),
                IrCaseLabel::TypePattern {
                    type_name,
                    variable: _,
                    deconstruction: Some(pattern),
                } => {
                    rendered.push(self.render_type_deconstruction_label(&type_name, &pattern)?);
                }
                IrCaseLabel::TypePattern {
                    type_name,
                    variable,
                    deconstruction: None,
                } => rendered.push(format!("{} {}", type_name, variable)),
                IrCaseLabel::Range {
                    type_name,
                    variable,
                    ..
                } => rendered.push(format!("{} {}", type_name, variable)),
                IrCaseLabel::Default => rendered.push("default".to_string()),
            }
        }
        Ok(rendered.join(", "))
    }

    fn render_type_deconstruction_label(
        &mut self,
        type_name: &str,
        pattern: &IrDeconstructionPattern,
    ) -> Result<String, CodeGenError> {
        let rendered = self.render_deconstruction_pattern(pattern)?;
        Ok(format!("{}{}", type_name, rendered))
    }

    fn render_deconstruction_pattern(
        &mut self,
        pattern: &IrDeconstructionPattern,
    ) -> Result<String, CodeGenError> {
        let mut parts = Vec::new();
        for component in &pattern.components {
            parts.push(self.render_deconstruction_component(component)?);
        }
        Ok(format!("({})", parts.join(", ")))
    }

    fn render_deconstruction_component(
        &mut self,
        component: &IrDeconstructionComponent,
    ) -> Result<String, CodeGenError> {
        match component {
            IrDeconstructionComponent::Wildcard => Ok("_".to_string()),
            IrDeconstructionComponent::Binding { name } => Ok(format!("var {}", name)),
            IrDeconstructionComponent::Literal(literal) => Ok(Self::literal_to_string(literal)),
            IrDeconstructionComponent::Type { type_name, pattern } => {
                if let Some(nested) = pattern {
                    let nested_rendered = self.render_deconstruction_pattern(nested)?;
                    Ok(format!("{}{}", type_name, nested_rendered))
                } else {
                    Ok(type_name.clone())
                }
            }
        }
    }

    fn render_implicit_when_end_case(
        &self,
        end: &IrImplicitWhenEnd,
        cases: &[IrSwitchCase],
    ) -> Result<Option<String>, CodeGenError> {
        if Self::has_default_case(cases) {
            return Ok(None);
        }

        let rendered = match end {
            IrImplicitWhenEnd::Unit { .. } => "default -> { }".to_string(),
        };

        Ok(Some(rendered))
    }

    fn has_default_case(cases: &[IrSwitchCase]) -> bool {
        cases.iter().any(|case| {
            case.labels
                .iter()
                .any(|label| matches!(label, IrCaseLabel::Default))
        })
    }

    fn generate_string_format(
        &mut self,
        format_string: &str,
        args: &[IrExpression],
    ) -> Result<String, CodeGenError> {
        self.add_import("java.lang.String");
        let mut result = String::from("String.format(");
        result.push_str(&format!("\"{}\"", Self::escape_string(format_string)));
        if !args.is_empty() {
            result.push_str(", ");
            result.push_str(&self.render_arguments(args)?);
        }
        result.push(')');
        Ok(result)
    }

    // === Expression Helpers (moved from helpers.rs) ===

    /// Generate Java binary operator from IR BinaryOp.
    pub fn generate_binary_op(&self, op: &BinaryOp) -> Result<String, CodeGenError> {
        Ok(match op {
            BinaryOp::Add => "+".to_string(),
            BinaryOp::Subtract => "-".to_string(),
            BinaryOp::Multiply => "*".to_string(),
            BinaryOp::Divide => "/".to_string(),
            BinaryOp::Modulo => "%".to_string(),
            BinaryOp::Equal => "==".to_string(),
            BinaryOp::NotEqual => "!=".to_string(),
            BinaryOp::Less => "<".to_string(),
            BinaryOp::LessEqual => "<=".to_string(),
            BinaryOp::Greater => ">".to_string(),
            BinaryOp::GreaterEqual => ">=".to_string(),
            BinaryOp::Is => "instanceof".to_string(),
            BinaryOp::And => "&&".to_string(),
            BinaryOp::Or => "||".to_string(),
            BinaryOp::BitAnd => "&".to_string(),
            BinaryOp::BitOr => "|".to_string(),
            BinaryOp::BitXor => "^".to_string(),
            BinaryOp::PlusAssign => "+=".to_string(),
            BinaryOp::MinusAssign => "-=".to_string(),
            BinaryOp::MultiplyAssign => "*=".to_string(),
            BinaryOp::DivideAssign => "/=".to_string(),
            BinaryOp::RangeExclusive | BinaryOp::RangeInclusive => {
                return Err(CodeGenError::UnsupportedConstruct {
                    construct: "Range operators must be lowered before Java emission".to_string(),
                    span: None,
                })
            }
            BinaryOp::Elvis => {
                return Err(CodeGenError::UnsupportedConstruct {
                    construct: "Elvis operator requires specialised lowering".to_string(),
                    span: None,
                })
            }
        })
    }

    /// Convert literal to Java string representation.
    pub(super) fn literal_to_string(literal: &Literal) -> String {
        match literal {
            Literal::String(value) => format!("\"{}\"", Self::escape_string(value)),
            Literal::Number(value) => value.clone(),
            Literal::Boolean(value) => value.to_string(),
            Literal::Null => "null".to_string(),
            Literal::Character(value) => {
                let escaped = match value {
                    '\\' => "\\".to_string(),
                    '\n' => "\\n".to_string(),
                    '\r' => "\\r".to_string(),
                    '\t' => "\\t".to_string(),
                    '"' => "\"".to_string(),
                    '\'' => "\\'".to_string(),
                    ch => ch.to_string(),
                };
                format!("'{}'", escaped)
            }
            Literal::Regex(regex) => format!(
                "java.util.regex.Pattern.compile(\"{}\")",
                Self::escape_string(&regex.pattern)
            ),
        }
    }
}
