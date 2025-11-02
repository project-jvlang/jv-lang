use super::*;
use jv_ast::{RegexCommandMode, RegexFlag, RegexGuardStrategy, Span, types::PrimitiveTypeName};
use jv_ir::PipelineShape;
use jv_ir::{
    IrRegexCommand, IrRegexLambdaReplacement, IrRegexLiteralReplacement, IrRegexReplacement,
    IrRegexTemplateSegment, JavaType, RawStringFlavor, SequencePipeline, SequenceSource,
    SequenceStage, SequenceTerminal, SequenceTerminalKind,
};
use std::fmt::Write;

impl JavaCodeGenerator {
    pub fn generate_expression(&mut self, expr: &IrExpression) -> Result<String, CodeGenError> {
        let span = expr.span();
        let base = self.generate_expression_inner(expr)?;
        if let Some(metadata) = self.take_conversion_metadata_for_span(&span) {
            self.wrap_with_conversions(base, metadata, span)
        } else {
            Ok(base)
        }
    }

    fn generate_expression_inner(&mut self, expr: &IrExpression) -> Result<String, CodeGenError> {
        match expr {
            IrExpression::Literal(literal, raw_flavor, _) => {
                Ok(Self::literal_to_string(literal, *raw_flavor))
            }
            IrExpression::RegexPattern { pattern, .. } => {
                self.add_import("java.util.regex.Pattern");
                Ok(format!(
                    "Pattern.compile(\"{}\")",
                    Self::escape_string(pattern)
                ))
            }
            IrExpression::Identifier { name, .. } => {
                if self.mutable_captures.contains(name) {
                    Ok(format!("{}.get()", name))
                } else {
                    Ok(name.clone())
                }
            }
            IrExpression::MethodCall {
                receiver,
                method_name,
                java_name,
                resolved_target,
                args,
                argument_style,
                ..
            } => {
                let effective_name = java_name
                    .as_deref()
                    .or_else(|| {
                        resolved_target
                            .as_ref()
                            .and_then(|target| target.java_name.as_deref())
                    })
                    .unwrap_or(method_name);
                if let Some(shortcut) = self.try_render_collectors_to_list(
                    receiver.as_deref(),
                    effective_name,
                    args.as_slice(),
                )? {
                    return Ok(shortcut);
                }
                let mut invocation = String::new();
                if let Some(target) = receiver {
                    if let Some(java_type) = Self::expression_java_type(target) {
                        self.ensure_stdlib_import(java_type);
                    }
                    let mut receiver_expr = self.generate_expression(target)?;
                    if matches!(target.as_ref(), IrExpression::Cast { .. }) {
                        receiver_expr = format!("({receiver_expr})");
                    }
                    invocation.push_str(&receiver_expr);
                    invocation.push('.');
                } else if let Some(owner) = resolved_target
                    .as_ref()
                    .and_then(|target| target.owner.as_deref())
                {
                    let owner_name = self.helper_owner_simple_name(owner);
                    invocation.push_str(&owner_name);
                    invocation.push('.');
                } else if let Some(owner) = self.owner_for_unqualified_call(method_name) {
                    invocation.push_str(owner);
                    invocation.push('.');
                }
                invocation.push_str(effective_name);
                invocation.push('(');
                let rendered_args = if effective_name == "println"
                    && Self::is_system_out(receiver.as_deref())
                    && args.len() == 1
                    && matches!(args[0], IrExpression::Literal(Literal::Null, _, _))
                {
                    "(Object) null".to_string()
                } else {
                    self.render_arguments_with_style(args, *argument_style)?
                };
                invocation.push_str(&rendered_args);
                invocation.push(')');
                Ok(invocation)
            }
            IrExpression::FieldAccess {
                receiver,
                field_name,
                is_record_component,
                ..
            } => {
                let receiver_code = self.generate_expression(receiver)?;
                if *is_record_component
                    || self.is_known_record_component(receiver.as_ref(), field_name)
                    || self.should_call_method_for_field(receiver.as_ref(), field_name.as_str())
                {
                    return Ok(format!("{}.{field_name}()", receiver_code));
                }
                Ok(format!("{receiver_code}.{field_name}"))
            }
            IrExpression::ArrayAccess { array, index, .. } => Ok(format!(
                "{}[{}]",
                self.generate_expression(array)?,
                self.generate_expression(index)?
            )),
            IrExpression::Binary {
                left,
                op,
                right,
                java_type,
                ..
            } => {
                if matches!(op, BinaryOp::Elvis) {
                    let left_expr = self.generate_expression(left)?;
                    let right_expr = self.generate_expression(right)?;
                    Ok(format!("({0} != null ? {0} : {1})", left_expr, right_expr))
                } else {
                    let operator = self.generate_binary_op(op)?;
                    let left_expr = self.generate_expression(left)?;
                    let right_expr = self.generate_expression(right)?;
                    let (left_expr, right_expr) = self.coerce_numeric_operands(
                        op, left, right, java_type, left_expr, right_expr,
                    )?;
                    Ok(format!("{} {} {}", left_expr, operator, right_expr))
                }
            }
            IrExpression::RegexMatch {
                subject,
                pattern,
                guard_strategy,
                ..
            } => {
                let subject_code = self.generate_expression(subject)?;
                let pattern_code = self.generate_expression(pattern)?;
                let pattern_expr = format!("({})", pattern_code);

                match guard_strategy {
                    RegexGuardStrategy::None => Ok(format!(
                        "{}.matcher({}).matches()",
                        pattern_expr, subject_code
                    )),
                    RegexGuardStrategy::CaptureAndGuard { temp_name } => {
                        let temp = temp_name.as_deref().unwrap_or("__jvRegexSubject_guard");
                        Ok(format!(
                            "(({}) instanceof java.lang.CharSequence {} && {}.matcher({}).matches())",
                            subject_code, temp, pattern_expr, temp
                        ))
                    }
                }
            }
            IrExpression::RegexCommand(command) => self.render_regex_command(command),
            IrExpression::Unary { op, operand, .. } => {
                let operand_code = self.generate_expression(operand)?;
                Ok(match op {
                    UnaryOp::Not => format!("!{}", operand_code),
                    UnaryOp::Minus => format!("-{}", operand_code),
                    UnaryOp::Plus => format!("+{}", operand_code),
                    UnaryOp::BitNot => format!("~{}", operand_code),
                })
            }
            IrExpression::Assignment { target, value, .. } => {
                if let IrExpression::Identifier { name, .. } = target.as_ref() {
                    if self.mutable_captures.contains(name) {
                        let value_expr = self.generate_expression(value)?;
                        return Ok(format!("{}.set({})", name, value_expr));
                    }
                }
                Ok(format!(
                    "{} = {}",
                    self.generate_expression(target)?,
                    self.generate_expression(value)?
                ))
            }
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
            IrExpression::CharToString(conversion) => {
                let value = self.generate_expression(&conversion.value)?;
                Ok(format!("String.valueOf({value})"))
            }
        }
    }

    fn try_render_collectors_to_list(
        &mut self,
        receiver: Option<&IrExpression>,
        method_name: &str,
        args: &[IrExpression],
    ) -> Result<Option<String>, CodeGenError> {
        if !self.targeting.supports_collection_factories() {
            return Ok(None);
        }
        if method_name != "collect" || args.len() != 1 {
            return Ok(None);
        }
        if !Self::is_collectors_to_list_invocation(&args[0]) {
            return Ok(None);
        }
        if let Some(target) = receiver {
            let receiver_code = self.generate_expression(target)?;
            return Ok(Some(format!("{receiver_code}.toList()")));
        }
        Ok(None)
    }

    fn is_collectors_to_list_invocation(expr: &IrExpression) -> bool {
        if let IrExpression::MethodCall {
            method_name,
            java_name,
            resolved_target,
            receiver,
            ..
        } = expr
        {
            let is_to_list = java_name
                .as_deref()
                .or_else(|| {
                    resolved_target
                        .as_ref()
                        .and_then(|target| target.java_name.as_deref())
                })
                .unwrap_or(method_name)
                == "toList";

            if !is_to_list {
                return false;
            }

            if let Some(target) = resolved_target {
                if target
                    .owner
                    .as_deref()
                    .is_some_and(|owner| owner == "java.util.stream.Collectors")
                {
                    return true;
                }
            }

            if let Some(rcv) = receiver {
                if let IrExpression::Identifier { name, .. } = &**rcv {
                    return name == "Collectors";
                }
            }
        }
        false
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
                    java_type,
                    implicit_end.as_ref(),
                    strategy_description.as_ref(),
                    span,
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
        java_type: &JavaType,
        implicit_end: Option<&IrImplicitWhenEnd>,
        strategy_description: Option<&String>,
        span: &Span,
    ) -> Result<String, CodeGenError> {
        if cases.iter().any(|case| {
            case.labels
                .iter()
                .any(|label| matches!(label, IrCaseLabel::Range { .. }))
        }) {
            return self.render_switch_expression_java21(
                discriminant,
                cases,
                java_type,
                implicit_end,
                strategy_description,
                span,
            );
        }

        if Self::switch_requires_boolean_fallback(discriminant, cases) {
            return self.render_switch_expression_java21(
                discriminant,
                cases,
                java_type,
                implicit_end,
                strategy_description,
                span,
            );
        }

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
            let body_expr = Self::finalize_switch_arm_body(body_expr);
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

    fn switch_requires_boolean_fallback(
        discriminant: &IrExpression,
        cases: &[IrSwitchCase],
    ) -> bool {
        if !Self::expression_java_type(discriminant)
            .map(Self::is_boolean_switch_type)
            .unwrap_or(false)
        {
            return false;
        }

        cases.iter().any(|case| {
            case.labels
                .iter()
                .any(|label| matches!(label, IrCaseLabel::Literal(Literal::Boolean(_))))
        })
    }

    fn is_boolean_switch_type(java_type: &JavaType) -> bool {
        match java_type {
            JavaType::Primitive(name) => name == "boolean",
            JavaType::Reference { name, .. } => name == "Boolean" || name == "java.lang.Boolean",
            _ => false,
        }
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

    fn default_value_literal(java_type: &JavaType) -> String {
        match java_type {
            JavaType::Primitive(name) => match name.as_str() {
                "boolean" => "false".to_string(),
                "byte" => "(byte) 0".to_string(),
                "short" => "(short) 0".to_string(),
                "int" => "0".to_string(),
                "long" => "0L".to_string(),
                "float" => "0.0f".to_string(),
                "double" => "0.0d".to_string(),
                "char" => "'\\0'".to_string(),
                _ => "0".to_string(),
            },
            _ => "null".to_string(),
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
            let default_value = Self::default_value_literal(java_type);
            builder.push_line(&format!(
                "{} {} = {};",
                result_type, result_binding, default_value
            ));
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
                IrCaseLabel::Range { .. } => continue,
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
        if Self::case_has_range_label(case) {
            return self
                .render_range_case_block_java21(
                    case,
                    subject_binding,
                    result_is_void,
                    result_binding,
                )
                .map(Some);
        }

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

    fn case_has_range_label(case: &IrSwitchCase) -> bool {
        case.labels
            .iter()
            .any(|label| matches!(label, IrCaseLabel::Range { .. }))
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

    fn render_range_case_block_java21(
        &mut self,
        case: &IrSwitchCase,
        subject_binding: &str,
        result_is_void: bool,
        result_binding: &str,
    ) -> Result<String, CodeGenError> {
        let label_count = case
            .labels
            .iter()
            .filter(|label| !matches!(label, IrCaseLabel::Default))
            .count();
        if label_count != 1 {
            return Err(self.java21_incompatibility_error(
                &case.span,
                "範囲パターンを同じ分岐の他のラベルと混在させることはできません",
                "range patterns cannot be combined with other labels in the same branch",
            ));
        }

        let variable = case
            .labels
            .iter()
            .find_map(|label| match label {
                IrCaseLabel::Range { variable, .. } => Some(variable.clone()),
                _ => None,
            })
            .ok_or_else(|| {
                self.java21_incompatibility_error(
                    &case.span,
                    "範囲パターンのバインディング名が見つかりません",
                    "range pattern binding name missing",
                )
            })?;

        let guard = case.guard.as_ref().ok_or_else(|| {
            self.java21_incompatibility_error(
                &case.span,
                "範囲パターンにはガード条件が必要です",
                "range patterns require a guard condition",
            )
        })?;
        let guard_expr = self.generate_expression(guard)?;

        let mut builder = self.builder();
        if let Some(comment) = self.render_case_leading_comment(case)? {
            builder.push_line(&comment);
        }
        builder.push_line(&format!("final var {} = {};", variable, subject_binding));
        builder.push_line(&format!("if ({}) {{", guard_expr));
        builder.indent();
        self.write_switch_case_body_java21(&mut builder, case, result_is_void, result_binding)?;
        builder.push_line("__matched = true;");
        builder.dedent();
        builder.push_line("}");

        Ok(builder.build())
    }

    fn literal_condition_java21(
        &mut self,
        subject_binding: &str,
        literal: &Literal,
    ) -> Result<String, CodeGenError> {
        let literal_value = Self::literal_to_string(literal, None);
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
                    });
                }
            },
            CompletableFutureOp::ThenApply => match rendered_args.as_slice() {
                [future, func] => format!("{}.thenApply({})", future, func),
                _ => {
                    return Err(CodeGenError::InvalidMethodSignature {
                        message: "thenApply expects future and function".to_string(),
                        span: None,
                    });
                }
            },
            CompletableFutureOp::ThenCompose => match rendered_args.as_slice() {
                [future, func] => format!("{}.thenCompose({})", future, func),
                _ => {
                    return Err(CodeGenError::InvalidMethodSignature {
                        message: "thenCompose expects future and function".to_string(),
                        span: None,
                    });
                }
            },
            CompletableFutureOp::Get => match rendered_args.as_slice() {
                [future] => format!("{}.get()", future),
                _ => {
                    return Err(CodeGenError::InvalidMethodSignature {
                        message: "get expects future".to_string(),
                        span: None,
                    });
                }
            },
            CompletableFutureOp::CompletedFuture => match rendered_args.as_slice() {
                [value] => format!("CompletableFuture.completedFuture({})", value),
                _ => {
                    return Err(CodeGenError::InvalidMethodSignature {
                        message: "completedFuture expects a single value".to_string(),
                        span: None,
                    });
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

        let element_type = Self::infer_sequence_element_type(pipeline);
        let element_type_ref = element_type.as_ref();

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
            let chained = self.render_sequence_chain(
                "__jvStream",
                pipeline,
                Some(result_type),
                element_type_ref,
            )?;
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
            self.render_sequence_chain(&source_expr, pipeline, Some(result_type), element_type_ref)
        }
    }

    fn sequence_pipeline_requires_helper(&self, pipeline: &SequencePipeline) -> bool {
        if matches!(pipeline.source, SequenceSource::JavaStream { .. }) {
            return false;
        }

        if pipeline.terminal.is_some() {
            return false;
        }

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
        if Self::is_java_stream_type(result_type) {
            return Ok(stream_expr);
        }

        if Self::is_java_list_type(result_type) {
            if self.targeting.supports_collection_factories() {
                return Ok(format!("{stream_expr}.toList()"));
            }
            self.add_import("java.util.stream.Collectors");
            return Ok(format!("{}.collect(Collectors.toList())", stream_expr));
        }

        Err(CodeGenError::TypeGenerationError {
            message:
                "lazy sequence pipeline must resolve to java.util.stream.Stream or a supported materialization"
                    .to_string(),
            span: Some(span.clone()),
        })
    }

    fn take_conversion_metadata_for_span(
        &mut self,
        span: &Span,
    ) -> Option<Vec<ConversionMetadata>> {
        self.conversion_metadata.remove(&super::SpanKey::from(span))
    }

    fn wrap_with_conversions(
        &mut self,
        mut value: String,
        metadata: Vec<ConversionMetadata>,
        span: Span,
    ) -> Result<String, CodeGenError> {
        for metadata_entry in &metadata {
            value = self.apply_conversion(value, metadata_entry)?;
        }

        let ir_node = self.metadata_lookup_key();
        self.conversion_map_records.push(ConversionSourceMapRecord {
            span,
            java_snippet: value.clone(),
            metadata,
            ir_node,
        });

        Ok(value)
    }

    fn apply_conversion(
        &mut self,
        input: String,
        metadata: &ConversionMetadata,
    ) -> Result<String, CodeGenError> {
        let guarded = if let Some(guard) = metadata.nullable_guard {
            self.wrap_with_nullable_guard(input, guard)
        } else {
            input
        };

        let result = match metadata.kind {
            ConversionKind::Identity => guarded,
            ConversionKind::WideningPrimitive => guarded,
            ConversionKind::Boxing => self.apply_boxing(guarded, metadata)?,
            ConversionKind::Unboxing => self.apply_unboxing(guarded, metadata)?,
            ConversionKind::StringConversion | ConversionKind::MethodInvocation => {
                self.apply_helper_conversion(guarded, metadata)
            }
        };

        Ok(result)
    }

    fn apply_helper_conversion(&mut self, input: String, metadata: &ConversionMetadata) -> String {
        if let Some(helper) = metadata.helper.as_ref() {
            self.invoke_helper(helper, input)
        } else {
            input
        }
    }

    fn invoke_helper(&mut self, helper: &ConversionHelper, input: String) -> String {
        if helper.is_static {
            let owner = self.helper_owner_simple_name(&helper.owner);
            format!("{owner}.{}({input})", helper.method)
        } else {
            format!("({input}).{}()", helper.method)
        }
    }

    fn apply_boxing(
        &mut self,
        input: String,
        metadata: &ConversionMetadata,
    ) -> Result<String, CodeGenError> {
        if let Some(info) = primitive_info_by_primitive(&metadata.from_type) {
            let owner = self.helper_owner_simple_name(info.wrapper);
            Ok(format!("{owner}.valueOf({input})"))
        } else if let Some(helper) = metadata.helper.as_ref() {
            Ok(self.invoke_helper(helper, input))
        } else {
            Ok(input)
        }
    }

    fn apply_unboxing(
        &mut self,
        input: String,
        metadata: &ConversionMetadata,
    ) -> Result<String, CodeGenError> {
        if let Some(info) = primitive_info_by_wrapper(&metadata.from_type) {
            Ok(format!("({input}).{}()", info.unboxing_method))
        } else if let Some(helper) = metadata.helper.as_ref() {
            Ok(self.invoke_helper(helper, input))
        } else {
            Ok(input)
        }
    }

    fn helper_owner_simple_name(&mut self, owner: &str) -> String {
        if owner.starts_with("java.lang.") {
            Self::simple_name(owner)
        } else {
            self.add_import(owner);
            Self::simple_name(owner)
        }
    }

    fn wrap_with_nullable_guard(&mut self, input: String, guard: NullableGuard) -> String {
        self.add_import("java.util.Objects");
        match guard.reason {
            NullableGuardReason::OptionalLift | NullableGuardReason::Unboxing => {
                format!("Objects.requireNonNull({input})")
            }
        }
    }

    fn render_sequence_source(
        &mut self,
        source: &SequenceSource,
    ) -> Result<(String, bool), CodeGenError> {
        match source {
            SequenceSource::Collection { expr, .. } => {
                let java_stream = Self::is_java_stream_expression(expr);
                let rendered = self.generate_expression(expr)?;

                if java_stream {
                    Ok((rendered, false))
                } else {
                    Ok((format!("({}).stream()", rendered), false))
                }
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

    fn is_java_stream_expression(expr: &IrExpression) -> bool {
        Self::expression_java_type(expr)
            .map(Self::is_java_stream_type)
            .unwrap_or(false)
    }

    fn adapt_flat_map_lambda(
        &self,
        lambda: &IrExpression,
        element_hint: Option<&JavaType>,
    ) -> IrExpression {
        match lambda {
            IrExpression::Lambda {
                functional_interface,
                param_names,
                param_types,
                body,
                java_type,
                span,
            } => {
                let body_expr = body.as_ref();
                if Self::is_java_stream_expression(body_expr) {
                    return lambda.clone();
                }

                let inferred_hint = element_hint
                    .cloned()
                    .or_else(|| Self::infer_iterable_hint_from_expression(body_expr));

                if self.lambda_body_requires_sequence_stream(body_expr, inferred_hint.as_ref()) {
                    let body_span = body_expr.span();
                    let sequence_identifier = IrExpression::Identifier {
                        name: "Sequence".to_string(),
                        java_type: JavaType::Reference {
                            name: "jv.collections.Sequence".to_string(),
                            generic_args: Vec::new(),
                        },
                        span: body_span.clone(),
                    };

                    let to_stream_call = IrExpression::MethodCall {
                        receiver: Some(Box::new(sequence_identifier)),
                        method_name: "toStream".to_string(),
                        java_name: None,
                        resolved_target: None,
                        args: vec![*body.clone()],
                        argument_style: CallArgumentStyle::Whitespace,
                        java_type: JavaType::Reference {
                            name: "java.util.stream.Stream".to_string(),
                            generic_args: vec![JavaType::object()],
                        },
                        span: body_span.clone(),
                    };

                    return IrExpression::Lambda {
                        functional_interface: functional_interface.clone(),
                        param_names: param_names.clone(),
                        param_types: param_types.clone(),
                        body: Box::new(to_stream_call),
                        java_type: java_type.clone(),
                        span: span.clone(),
                    };
                }

                lambda.clone()
            }
            other => other.clone(),
        }
    }

    fn lambda_body_requires_sequence_stream(
        &self,
        body: &IrExpression,
        element_hint: Option<&JavaType>,
    ) -> bool {
        if Self::is_java_stream_expression(body) {
            return false;
        }

        if element_hint
            .map(Self::is_java_iterable_type_from_type)
            .unwrap_or(false)
        {
            return true;
        }

        if Self::expression_java_type(body)
            .map(Self::is_java_iterable_type_from_type)
            .unwrap_or(false)
        {
            return true;
        }

        match body {
            IrExpression::MethodCall {
                resolved_target,
                receiver,
                ..
            } => {
                if resolved_target
                    .as_ref()
                    .and_then(|target| target.owner.as_deref())
                    .map_or(false, Self::is_java_iterable_type)
                {
                    return true;
                }

                receiver
                    .as_deref()
                    .and_then(Self::expression_java_type)
                    .map(Self::is_java_iterable_type_from_type)
                    .unwrap_or(false)
            }
            IrExpression::ArrayCreation { .. } => true,
            IrExpression::ObjectCreation { class_name, .. } => {
                Self::is_java_iterable_type(class_name)
            }
            IrExpression::Identifier { java_type, .. }
            | IrExpression::FieldAccess { java_type, .. }
            | IrExpression::ArrayAccess { java_type, .. }
            | IrExpression::Assignment { java_type, .. }
            | IrExpression::Block { java_type, .. }
            | IrExpression::Conditional { java_type, .. }
            | IrExpression::SequencePipeline { java_type, .. }
            | IrExpression::Switch { java_type, .. }
            | IrExpression::NullSafeOperation { java_type, .. }
            | IrExpression::TryWithResources { java_type, .. }
            | IrExpression::CompletableFuture { java_type, .. }
            | IrExpression::VirtualThread { java_type, .. } => {
                Self::is_java_iterable_type_from_type(java_type)
            }
            _ => false,
        }
    }

    fn infer_iterable_hint_from_expression(expr: &IrExpression) -> Option<JavaType> {
        if let Some(java_type) = Self::expression_java_type(expr) {
            if Self::is_java_iterable_type_from_type(java_type) {
                return Some(java_type.clone());
            }
        }

        match expr {
            IrExpression::MethodCall {
                method_name,
                resolved_target,
                receiver,
                java_type,
                ..
            } => {
                if !Self::returns_object_like(java_type) {
                    return None;
                }

                if let Some(target) = resolved_target {
                    if let Some(owner) = target.owner.as_deref() {
                        if Self::is_java_iterable_type(owner)
                            && Self::is_iterable_factory_method(owner, method_name)
                        {
                            return Some(Self::iterable_hint_from_owner(owner));
                        }
                    }
                }

                if let Some(recv_expr) = receiver {
                    if let Some(recv_type) = Self::expression_java_type(recv_expr) {
                        if Self::is_java_iterable_type_from_type(recv_type) {
                            if let Some(owner) = Self::iterable_type_name(recv_type) {
                                if Self::is_iterable_factory_method(owner, method_name) {
                                    return Some(Self::iterable_hint_from_owner(owner));
                                }
                            }
                        }
                    }
                }

                None
            }
            _ => None,
        }
    }

    fn iterable_hint_from_owner(owner: &str) -> JavaType {
        JavaType::Reference {
            name: owner.to_string(),
            generic_args: Vec::new(),
        }
    }

    fn iterable_type_name(java_type: &JavaType) -> Option<&str> {
        match java_type {
            JavaType::Reference { name, .. } => Some(name.as_str()),
            _ => None,
        }
    }

    fn is_iterable_factory_method(owner: &str, method_name: &str) -> bool {
        match method_name {
            "of" | "copyOf" => true,
            "from" | "fromIterable" | "fromSequence" | "fromStream" => {
                Self::is_sequence_owner(owner)
            }
            _ => false,
        }
    }

    fn is_sequence_owner(name: &str) -> bool {
        let simple = name.rsplit('.').next().unwrap_or(name);
        simple == "Sequence"
    }

    fn returns_object_like(java_type: &JavaType) -> bool {
        matches!(
            java_type,
            JavaType::Reference { name, .. }
                if name == "java.lang.Object" || name == "Object"
        )
    }

    fn is_java_iterable_type_from_type(java_type: &JavaType) -> bool {
        match java_type {
            JavaType::Reference { name, .. } => Self::is_java_iterable_type(name),
            _ => false,
        }
    }

    fn is_java_iterable_type(name: &str) -> bool {
        let simple = name.rsplit('.').next().unwrap_or(name);
        matches!(
            simple,
            "Iterable"
                | "Collection"
                | "List"
                | "Set"
                | "Queue"
                | "Deque"
                | "ArrayList"
                | "LinkedList"
                | "LinkedHashSet"
                | "HashSet"
                | "Sequence"
        ) || matches!(
            name,
            "java.lang.Iterable"
                | "java.util.Collection"
                | "java.util.List"
                | "java.util.Set"
                | "java.util.Queue"
                | "java.util.Deque"
                | "java.util.ArrayList"
                | "java.util.LinkedList"
                | "java.util.LinkedHashSet"
                | "java.util.HashSet"
                | "jv.collections.Sequence"
        )
    }

    pub(super) fn expression_java_type(expr: &IrExpression) -> Option<&JavaType> {
        match expr {
            IrExpression::Identifier { java_type, .. }
            | IrExpression::MethodCall { java_type, .. }
            | IrExpression::FieldAccess { java_type, .. }
            | IrExpression::ArrayAccess { java_type, .. }
            | IrExpression::Binary { java_type, .. }
            | IrExpression::Unary { java_type, .. }
            | IrExpression::Assignment { java_type, .. }
            | IrExpression::Conditional { java_type, .. }
            | IrExpression::Block { java_type, .. }
            | IrExpression::ObjectCreation { java_type, .. }
            | IrExpression::Lambda { java_type, .. }
            | IrExpression::SequencePipeline { java_type, .. }
            | IrExpression::Switch { java_type, .. }
            | IrExpression::RegexMatch { java_type, .. }
            | IrExpression::NullSafeOperation { java_type, .. }
            | IrExpression::CompletableFuture { java_type, .. }
            | IrExpression::VirtualThread { java_type, .. }
            | IrExpression::TryWithResources { java_type, .. }
            | IrExpression::This { java_type, .. }
            | IrExpression::Super { java_type, .. } => Some(java_type),
            IrExpression::Cast { target_type, .. } => Some(target_type),
            _ => None,
        }
    }

    fn owner_for_unqualified_call(&self, method_name: &str) -> Option<&str> {
        if let Some(owner) = self.script_class_simple_name.as_deref() {
            if self.script_method_names.contains(method_name) {
                return Some(owner);
            }
        }
        None
    }

    fn should_call_method_for_field(&self, receiver: &IrExpression, field_name: &str) -> bool {
        if Self::is_java_stream_expression(receiver) {
            return true;
        }

        if let Some(index) = self.symbol_index() {
            if let Some(JavaType::Reference { name, .. }) = Self::expression_java_type(receiver) {
                if let Some(entry) = index.lookup_type(name) {
                    if entry.has_instance_method(field_name) {
                        return true;
                    }
                    if entry.has_field(field_name) {
                        return false;
                    }
                }
            }
        }

        match field_name {
            "size" => Self::is_java_stream_expression(receiver),
            "length" => Self::expression_java_type(receiver)
                .map(Self::is_string_like_type)
                .unwrap_or(false),
            _ => false,
        }
    }

    fn is_string_like_type(java_type: &JavaType) -> bool {
        match java_type {
            JavaType::Reference { name, .. } => matches!(
                name.as_str(),
                "String"
                    | "java.lang.String"
                    | "CharSequence"
                    | "java.lang.CharSequence"
                    | "StringBuilder"
                    | "java.lang.StringBuilder"
                    | "StringBuffer"
                    | "java.lang.StringBuffer"
            ),
            _ => false,
        }
    }

    fn is_java_stream_type(java_type: &JavaType) -> bool {
        match java_type {
            JavaType::Reference { name, .. } => {
                name == "Stream" || name == "java.util.stream.Stream"
            }
            _ => false,
        }
    }

    fn is_java_list_type(java_type: &JavaType) -> bool {
        match java_type {
            JavaType::Reference { name, .. } => name == "java.util.List" || name == "List",
            _ => false,
        }
    }

    fn ensure_stdlib_import(&mut self, java_type: &JavaType) {
        if let JavaType::Reference { name, .. } = java_type {
            self.ensure_stdlib_import_name(name);
        }
    }

    fn ensure_stdlib_import_name(&mut self, fqcn: &str) {
        if fqcn.starts_with("jv.") {
            self.add_import(fqcn);
        }
    }

    fn is_system_out(receiver: Option<&IrExpression>) -> bool {
        match receiver {
            Some(IrExpression::FieldAccess {
                receiver,
                field_name,
                ..
            }) if field_name == "out" => {
                matches!(
                    receiver.as_ref(),
                    IrExpression::Identifier { name, .. } if name == "System"
                )
            }
            _ => false,
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
        result_type: Option<&JavaType>,
        element_type: Option<&JavaType>,
    ) -> Result<String, CodeGenError> {
        let mut chain = start_expr.to_string();
        for stage in &pipeline.stages {
            chain.push_str(&self.render_sequence_stage(stage)?);
        }

        if let Some(terminal) = &pipeline.terminal {
            self.render_sequence_terminal(chain, terminal, result_type, element_type)
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
            SequenceStage::FlatMap {
                lambda,
                element_hint,
                ..
            } => {
                let adapted_lambda = self.adapt_flat_map_lambda(lambda, element_hint.as_ref());
                let rendered = self.generate_expression(&adapted_lambda)?;
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
        let element_type = Self::infer_sequence_element_type(pipeline);
        let terminal_expr = self.render_sequence_terminal(
            helper_stream,
            terminal,
            Some(result_type),
            element_type.as_ref(),
        )?;

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
        result_type: Option<&JavaType>,
        element_type: Option<&JavaType>,
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
                Ok(format!("{}.reduce({})", chain, accumulator_expr))
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
            SequenceTerminalKind::Sum => {
                if let Some(hint) = terminal.specialization_hint.as_ref() {
                    match hint.canonical {
                        PrimitiveTypeName::Int | PrimitiveTypeName::Char => {
                            let mapper = self.render_int_stream_mapper(terminal)?;
                            Ok(format!("{chain}.mapToInt({mapper}).sum()"))
                        }
                        PrimitiveTypeName::Long => {
                            let mapper = self.render_long_stream_mapper();
                            Ok(format!("{chain}.mapToLong({mapper}).sum()"))
                        }
                        PrimitiveTypeName::Double | PrimitiveTypeName::Float => {
                            let mapper = self.render_double_stream_mapper();
                            Ok(format!("{chain}.mapToDouble({mapper}).sum()"))
                        }
                        _ => {
                            let mapper = self.render_int_stream_mapper(terminal)?;
                            Ok(format!("{chain}.mapToInt({mapper}).sum()"))
                        }
                    }
                } else if let Some(category) =
                    element_type.and_then(Self::numeric_category_from_type)
                {
                    Self::render_sum_for_category(self, chain, terminal, category)
                } else if let Some(primitive_name) = result_type.and_then(Self::primitive_name) {
                    match primitive_name {
                        "long" => Self::render_sum_for_category(self, chain, terminal, "long"),
                        "double" | "float" => {
                            Self::render_sum_for_category(self, chain, terminal, "double")
                        }
                        _ => {
                            let mapper = self.render_int_stream_mapper(terminal)?;
                            Ok(format!("{chain}.mapToInt({mapper}).sum()"))
                        }
                    }
                } else {
                    let mapper = self.render_int_stream_mapper(terminal)?;
                    Ok(format!("{chain}.mapToInt({mapper}).sum()"))
                }
            }
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

    fn coerce_numeric_operands(
        &self,
        op: &BinaryOp,
        left: &IrExpression,
        right: &IrExpression,
        result_type: &JavaType,
        mut rendered_left: String,
        mut rendered_right: String,
    ) -> Result<(String, String), CodeGenError> {
        let arithmetic = matches!(
            op,
            BinaryOp::Add
                | BinaryOp::Subtract
                | BinaryOp::Multiply
                | BinaryOp::Divide
                | BinaryOp::Modulo
        );

        if !arithmetic {
            return Ok((rendered_left, rendered_right));
        }

        let target_primitive = match result_type {
            JavaType::Primitive(name) => name.as_str(),
            _ => return Ok((rendered_left, rendered_right)),
        };

        let Some(info) = primitive_info_by_primitive(target_primitive) else {
            return Ok((rendered_left, rendered_right));
        };

        rendered_left =
            self.ensure_numeric_operand(rendered_left, Self::expression_java_type(left), info)?;
        rendered_right =
            self.ensure_numeric_operand(rendered_right, Self::expression_java_type(right), info)?;

        Ok((rendered_left, rendered_right))
    }

    fn ensure_numeric_operand(
        &self,
        rendered: String,
        operand_type: Option<&JavaType>,
        target_info: &PrimitiveConversionInfo,
    ) -> Result<String, CodeGenError> {
        match operand_type {
            Some(JavaType::Primitive(_)) => Ok(rendered),
            Some(JavaType::Reference { .. }) => Ok(format!(
                "((Number) {}).{}()",
                rendered, target_info.unboxing_method
            )),
            Some(JavaType::Wildcard { bound, .. }) => {
                if let Some(inner) = bound.as_ref() {
                    self.ensure_numeric_operand(rendered, Some(inner), target_info)
                } else {
                    Ok(format!(
                        "((Number) {}).{}()",
                        rendered, target_info.unboxing_method
                    ))
                }
            }
            _ => Ok(rendered),
        }
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
                IrCaseLabel::Literal(literal) => {
                    rendered.push(Self::literal_to_string(&literal, None))
                }
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
            IrDeconstructionComponent::Literal(literal) => {
                Ok(Self::literal_to_string(literal, None))
            }
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
            IrImplicitWhenEnd::Unit { .. } => {
                let body = Self::finalize_switch_arm_body("{ }".to_string());
                format!("default -> {}", body)
            }
        };

        Ok(Some(rendered))
    }

    fn finalize_switch_arm_body(body_expr: String) -> String {
        let trimmed_start = body_expr.trim_start();
        if trimmed_start.starts_with('{') {
            return body_expr;
        }

        let trimmed_end = body_expr.trim_end();
        if trimmed_end.ends_with(';') || trimmed_end.is_empty() {
            return body_expr;
        }

        let trailing_whitespace = &body_expr[trimmed_end.len()..];
        let mut result = String::with_capacity(body_expr.len() + 1);
        result.push_str(trimmed_end);
        result.push(';');
        result.push_str(trailing_whitespace);
        result
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

    fn render_regex_command(&mut self, command: &IrRegexCommand) -> Result<String, CodeGenError> {
        match command.mode {
            RegexCommandMode::All => self.render_regex_replace(command, "replaceAll"),
            RegexCommandMode::First => self.render_regex_replace(command, "replaceFirst"),
            RegexCommandMode::Match => self.render_regex_match_command(command),
            RegexCommandMode::Split => self.render_regex_split_command(command),
            RegexCommandMode::Iterate => self.render_regex_iterate_command(command),
        }
    }

    fn render_regex_replace(
        &mut self,
        command: &IrRegexCommand,
        method: &str,
    ) -> Result<String, CodeGenError> {
        let pattern_code =
            self.render_regex_pattern_expr(command.pattern.as_ref(), &command.flags)?;
        let pattern_expr = format!("({pattern_code})", pattern_code = pattern_code);
        let replacement_code = self.render_regex_replacement(&command.replacement)?;
        self.guard_regex_subject(
            &command.subject,
            &command.guard_strategy,
            "\"\"",
            &command.java_type,
            move |subject_ref| {
                Ok(format!(
                    "{pattern}.matcher({subject}).{method}({replacement})",
                    pattern = pattern_expr,
                    subject = subject_ref,
                    method = method,
                    replacement = replacement_code,
                ))
            },
        )
    }

    fn render_regex_match_command(
        &mut self,
        command: &IrRegexCommand,
    ) -> Result<String, CodeGenError> {
        let pattern_code =
            self.render_regex_pattern_expr(command.pattern.as_ref(), &command.flags)?;
        let pattern_expr = format!("({pattern_code})", pattern_code = pattern_code);
        self.guard_regex_subject(
            &command.subject,
            &command.guard_strategy,
            "false",
            &command.java_type,
            move |subject_ref| {
                Ok(format!(
                    "{pattern}.matcher({subject}).matches()",
                    pattern = pattern_expr,
                    subject = subject_ref,
                ))
            },
        )
    }

    fn render_regex_split_command(
        &mut self,
        command: &IrRegexCommand,
    ) -> Result<String, CodeGenError> {
        let pattern_code =
            self.render_regex_pattern_expr(command.pattern.as_ref(), &command.flags)?;
        let pattern_expr = format!("({pattern_code})", pattern_code = pattern_code);
        self.guard_regex_subject(
            &command.subject,
            &command.guard_strategy,
            "new String[0]",
            &command.java_type,
            move |subject_ref| {
                Ok(format!(
                    "{pattern}.split({subject}, -1)",
                    pattern = pattern_expr,
                    subject = subject_ref,
                ))
            },
        )
    }

    fn render_regex_iterate_command(
        &mut self,
        command: &IrRegexCommand,
    ) -> Result<String, CodeGenError> {
        if matches!(command.replacement, IrRegexReplacement::None) {
            self.add_import("java.util.stream.Stream");
            let pattern_code =
                self.render_regex_pattern_expr(command.pattern.as_ref(), &command.flags)?;
            let pattern_expr = format!("({pattern_code})", pattern_code = pattern_code);
            return self.guard_regex_subject(
                &command.subject,
                &command.guard_strategy,
                "Stream.empty()",
                &command.java_type,
                move |subject_ref| {
                    Ok(format!(
                        "{pattern}.matcher({subject}).results()",
                        pattern = pattern_expr,
                        subject = subject_ref,
                    ))
                },
            );
        }

        self.render_regex_replace(command, "replaceAll")
    }

    fn render_regex_replacement(
        &mut self,
        replacement: &IrRegexReplacement,
    ) -> Result<String, CodeGenError> {
        match replacement {
            IrRegexReplacement::None => Ok("\"\"".to_string()),
            IrRegexReplacement::Literal(literal) => self.render_regex_literal_replacement(literal),
            IrRegexReplacement::Lambda(lambda) => self.render_regex_lambda_replacement(lambda),
            IrRegexReplacement::Expression(expr) => self.generate_expression(expr),
        }
    }

    fn render_regex_literal_replacement(
        &mut self,
        literal: &IrRegexLiteralReplacement,
    ) -> Result<String, CodeGenError> {
        if literal.segments.is_empty() {
            return Ok("\"\"".to_string());
        }

        self.add_import("java.util.regex.Matcher");
        let mut builder = String::from("new StringBuilder()");

        for segment in &literal.segments {
            match segment {
                IrRegexTemplateSegment::Text(text) => {
                    builder.push_str(&format!(
                        ".append(Matcher.quoteReplacement(\"{}\"))",
                        Self::escape_string(text)
                    ));
                }
                IrRegexTemplateSegment::BackReference(index) => {
                    builder.push_str(&format!(".append(\"${}\")", index));
                }
                IrRegexTemplateSegment::Expression(expr) => {
                    let expr_code = self.generate_expression(expr)?;
                    builder.push_str(&format!(
                        ".append(Matcher.quoteReplacement(String.valueOf({})))",
                        expr_code
                    ));
                }
            }
        }

        builder.push_str(".toString()");
        Ok(builder)
    }

    fn render_regex_lambda_replacement(
        &mut self,
        lambda: &IrRegexLambdaReplacement,
    ) -> Result<String, CodeGenError> {
        if !lambda.param_types.is_empty() {
            self.add_import("java.util.regex.MatchResult");
        }

        let mut params = Vec::with_capacity(lambda.param_names.len());
        for (index, name) in lambda.param_names.iter().enumerate() {
            if let Some(java_type) = lambda.param_types.get(index) {
                let ty = self.generate_type(java_type)?;
                params.push(format!("{ty} {name}", ty = ty, name = name));
            } else {
                params.push(name.clone());
            }
        }

        let params_code = if params.len() == 1 {
            params[0].clone()
        } else {
            params.join(", ")
        };

        let body_expr = self.generate_expression(&lambda.body)?;
        Ok(format!(
            "({params}) -> {body}",
            params = params_code,
            body = body_expr
        ))
    }

    fn render_regex_pattern_expr(
        &mut self,
        pattern: &IrExpression,
        flags: &[RegexFlag],
    ) -> Result<String, CodeGenError> {
        if let IrExpression::RegexPattern { pattern: value, .. } = pattern {
            self.add_import("java.util.regex.Pattern");
            let escaped = Self::escape_string(value);
            if flags.is_empty() {
                Ok(format!("Pattern.compile(\"{escaped}\")"))
            } else {
                let flag_mask = self.render_regex_flag_mask(flags);
                Ok(format!("Pattern.compile(\"{escaped}\", {flag_mask})"))
            }
        } else {
            if flags.is_empty() {
                self.generate_expression(pattern)
            } else {
                let pattern_expr = self.generate_expression(pattern)?;
                let flag_mask = self.render_regex_flag_mask(flags);
                Ok(format!(
                    "Pattern.compile({pattern_expr}, {flag_mask})",
                    pattern_expr = pattern_expr
                ))
            }
        }
    }

    fn render_regex_flag_mask(&mut self, flags: &[RegexFlag]) -> String {
        self.add_import("java.util.regex.Pattern");
        let mut parts = Vec::with_capacity(flags.len());
        for flag in flags {
            let constant = match flag {
                RegexFlag::CaseInsensitive => "CASE_INSENSITIVE",
                RegexFlag::Multiline => "MULTILINE",
                RegexFlag::DotAll => "DOTALL",
                RegexFlag::UnicodeCase => "UNICODE_CASE",
                RegexFlag::UnixLines => "UNIX_LINES",
                RegexFlag::Comments => "COMMENTS",
                RegexFlag::Literal => "LITERAL",
                RegexFlag::CanonEq => "CANON_EQ",
            };
            parts.push(format!("Pattern.{constant}", constant = constant));
        }

        if parts.is_empty() {
            "0".to_string()
        } else {
            parts.join(" | ")
        }
    }

    fn guard_regex_subject<F>(
        &mut self,
        subject: &IrExpression,
        strategy: &RegexGuardStrategy,
        fallback: &str,
        result_type: &JavaType,
        build: F,
    ) -> Result<String, CodeGenError>
    where
        F: FnOnce(&str) -> Result<String, CodeGenError>,
    {
        let subject_code = self.generate_expression(subject)?;
        match strategy {
            RegexGuardStrategy::None => build(&subject_code),
            RegexGuardStrategy::CaptureAndGuard { temp_name } => {
                self.add_import("java.util.function.Supplier");
                let temp = temp_name
                    .clone()
                    .unwrap_or_else(|| "__jvRegexSubject_guard".to_string());
                let supplier_type = self.guard_supplier_type(result_type)?;
                let body = build(&temp)?;
                let source_var = format!("{temp}_source");
                let subject_expr = format!("({})", subject_code);
                Ok(format!(
                    "((java.util.function.Supplier<{supplier_type}>) () -> {{\n    final Object {source_var} = {subject_expr};\n    if ({source_var} instanceof java.lang.CharSequence {temp}) {{\n        return {body};\n    }} else {{\n        return {fallback};\n    }}\n}}).get()",
                    supplier_type = supplier_type,
                    source_var = source_var,
                    subject_expr = subject_expr,
                    temp = temp,
                    body = body,
                    fallback = fallback,
                ))
            }
        }
    }

    fn guard_supplier_type(&self, result_type: &JavaType) -> Result<String, CodeGenError> {
        Ok(match result_type {
            JavaType::Primitive(name) => primitive_info_by_primitive(name)
                .map(|info| info.wrapper.to_string())
                .unwrap_or_else(|| "java.lang.Object".to_string()),
            JavaType::Void => "java.lang.Void".to_string(),
            _ => self.generate_type(result_type)?,
        })
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
                });
            }
            BinaryOp::Elvis => {
                return Err(CodeGenError::UnsupportedConstruct {
                    construct: "Elvis operator requires specialised lowering".to_string(),
                    span: None,
                });
            }
        })
    }

    /// Convert literal to Java string representation.
    pub(super) fn literal_to_string(
        literal: &Literal,
        raw_flavor: Option<RawStringFlavor>,
    ) -> String {
        match literal {
            Literal::String(value) => Self::render_string_literal(value, raw_flavor),
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

    fn render_string_literal(value: &str, raw_flavor: Option<RawStringFlavor>) -> String {
        match raw_flavor {
            Some(RawStringFlavor::MultiLine) if value.contains('\n') && !value.is_empty() => {
                Self::render_text_block(value)
            }
            _ => format!("\"{}\"", Self::escape_string(value)),
        }
    }

    fn render_text_block(value: &str) -> String {
        let mut builder = String::from("\"\"\"\n");
        let escaped = Self::escape_text_block_content(value);
        builder.push_str(&escaped);
        if !value.ends_with('\n') {
            builder.push('\\');
            builder.push('\n');
        }
        builder.push_str("\"\"\"");
        builder
    }

    fn escape_text_block_content(value: &str) -> String {
        let mut result = String::new();
        let mut chars = value.chars().peekable();
        while let Some(ch) = chars.next() {
            match ch {
                '\\' => result.push_str("\\\\"),
                '\n' => result.push('\n'),
                '\r' => result.push_str("\\r"),
                '\t' => result.push_str("\\t"),
                '\u{0008}' => result.push_str("\\b"),
                '\u{000C}' => result.push_str("\\f"),
                '"' => {
                    let mut count = 1;
                    while matches!(chars.peek(), Some('"')) {
                        chars.next();
                        count += 1;
                    }
                    if count >= 3 {
                        for _ in 0..count {
                            result.push_str("\\\"");
                        }
                    } else {
                        for _ in 0..count {
                            result.push('"');
                        }
                    }
                }
                other if other.is_control() => {
                    let _ = write!(result, "\\u{:04X}", other as u32);
                }
                other => result.push(other),
            }
        }
        result
    }
}

impl JavaCodeGenerator {
    fn primitive_name(java_type: &JavaType) -> Option<&str> {
        match java_type {
            JavaType::Primitive(name) => Some(name),
            _ => None,
        }
    }

    fn render_sum_for_category(
        &mut self,
        chain: String,
        terminal: &SequenceTerminal,
        category: &str,
    ) -> Result<String, CodeGenError> {
        match category {
            "long" => {
                let mapper = self.render_long_stream_mapper();
                Ok(format!("{chain}.mapToLong({mapper}).sum()"))
            }
            "double" => {
                let mapper = self.render_double_stream_mapper();
                Ok(format!("{chain}.mapToDouble({mapper}).sum()"))
            }
            _ => {
                let mapper = self.render_int_stream_mapper(terminal)?;
                Ok(format!("{chain}.mapToInt({mapper}).sum()"))
            }
        }
    }

    fn numeric_category_from_type(java_type: &JavaType) -> Option<&'static str> {
        match java_type {
            JavaType::Primitive(name) => Self::primitive_numeric_category(name),
            JavaType::Reference { name, .. } => {
                let simple = name.rsplit('.').next().unwrap_or(name);
                match simple {
                    "Byte" | "Short" | "Integer" | "Character" => Some("int"),
                    "Long" => Some("long"),
                    "Float" | "Double" => Some("double"),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn primitive_numeric_category(name: &str) -> Option<&'static str> {
        match name {
            "byte" | "short" | "int" | "char" => Some("int"),
            "long" => Some("long"),
            "float" | "double" => Some("double"),
            _ => None,
        }
    }

    fn infer_sequence_element_type(pipeline: &SequencePipeline) -> Option<JavaType> {
        pipeline.element_type().cloned()
    }

    fn render_int_stream_mapper(
        &mut self,
        terminal: &SequenceTerminal,
    ) -> Result<String, CodeGenError> {
        if let Some(adapter) = terminal.canonical_adapter.as_ref() {
            self.generate_expression(adapter)
        } else {
            Ok("(value) -> { if (value instanceof Character) { return ((Character) value).charValue(); } else { return ((Number) value).intValue(); } }".to_string())
        }
    }

    fn render_long_stream_mapper(&self) -> String {
        "value -> ((Number) value).longValue()".to_string()
    }

    fn render_double_stream_mapper(&self) -> String {
        "value -> ((Number) value).doubleValue()".to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jv_build::metadata::{JavaMethodSignature, SymbolIndex, TypeEntry};
    use jv_ir::IrExpression;
    use std::sync::Arc;

    fn list_identifier(name: &str) -> IrExpression {
        IrExpression::Identifier {
            name: name.to_string(),
            java_type: JavaType::Reference {
                name: "java.util.List".to_string(),
                generic_args: Vec::new(),
            },
            span: Span::dummy(),
        }
    }

    #[test]
    fn converts_size_field_to_method_when_instance_method_exists() {
        let mut generator = JavaCodeGenerator::new();

        let mut index = SymbolIndex::new(None);
        let mut entry = TypeEntry::new("java.util.List".to_string(), "java.util".to_string(), None);
        entry.add_instance_method(
            "size".to_string(),
            JavaMethodSignature {
                parameters: Vec::new(),
                return_type: JavaType::Primitive("int".to_string()),
            },
        );
        index.types.insert("java.util.List".to_string(), entry);

        generator.set_symbol_index(Some(Arc::new(index)));

        let expr = IrExpression::FieldAccess {
            receiver: Box::new(list_identifier("items")),
            field_name: "size".to_string(),
            java_type: JavaType::Primitive("int".to_string()),
            span: Span::dummy(),
            is_record_component: false,
        };

        let rendered = generator
            .generate_expression(&expr)
            .expect("codegen succeeds");
        assert_eq!(rendered, "items.size()");
    }

    #[test]
    fn converts_string_length_field_to_method() {
        let mut generator = JavaCodeGenerator::new();

        let expr = IrExpression::FieldAccess {
            receiver: Box::new(IrExpression::Identifier {
                name: "text".to_string(),
                java_type: JavaType::string(),
                span: Span::dummy(),
            }),
            field_name: "length".to_string(),
            java_type: JavaType::Primitive("int".to_string()),
            span: Span::dummy(),
            is_record_component: false,
        };

        let rendered = generator
            .generate_expression(&expr)
            .expect("codegen succeeds");
        assert_eq!(rendered, "text.length()");
    }

    #[test]
    fn preserves_field_access_when_field_exists() {
        let mut generator = JavaCodeGenerator::new();

        let mut index = SymbolIndex::new(None);
        let mut entry = TypeEntry::new(
            "com.example.Counter".to_string(),
            "com.example".to_string(),
            None,
        );
        entry.add_instance_field("size".to_string());
        index.types.insert("com.example.Counter".to_string(), entry);

        generator.set_symbol_index(Some(Arc::new(index)));

        let expr = IrExpression::FieldAccess {
            receiver: Box::new(IrExpression::Identifier {
                name: "counter".to_string(),
                java_type: JavaType::Reference {
                    name: "com.example.Counter".to_string(),
                    generic_args: Vec::new(),
                },
                span: Span::dummy(),
            }),
            field_name: "size".to_string(),
            java_type: JavaType::Primitive("int".to_string()),
            span: Span::dummy(),
            is_record_component: false,
        };

        let rendered = generator
            .generate_expression(&expr)
            .expect("codegen succeeds");
        assert_eq!(rendered, "counter.size");
    }

    #[test]
    fn renders_regex_match_without_guard() {
        let mut generator = JavaCodeGenerator::new();

        let expr = IrExpression::RegexMatch {
            subject: Box::new(IrExpression::Identifier {
                name: "text".to_string(),
                java_type: JavaType::string(),
                span: Span::dummy(),
            }),
            pattern: Box::new(IrExpression::RegexPattern {
                pattern: "\\d+".to_string(),
                java_type: JavaType::Reference {
                    name: "java.util.regex.Pattern".to_string(),
                    generic_args: Vec::new(),
                },
                span: Span::dummy(),
            }),
            guard_strategy: RegexGuardStrategy::None,
            java_type: JavaType::boolean(),
            span: Span::dummy(),
        };

        let rendered = generator
            .generate_expression(&expr)
            .expect("codegen succeeds");

        assert_eq!(
            rendered,
            "(Pattern.compile(\"\\\\d+\")).matcher(text).matches()"
        );
    }

    #[test]
    fn renders_regex_match_with_capture_guard() {
        let mut generator = JavaCodeGenerator::new();

        let expr = IrExpression::RegexMatch {
            subject: Box::new(IrExpression::Identifier {
                name: "userInput".to_string(),
                java_type: JavaType::string(),
                span: Span::dummy(),
            }),
            pattern: Box::new(IrExpression::RegexPattern {
                pattern: "[a-zA-Z]+".to_string(),
                java_type: JavaType::Reference {
                    name: "java.util.regex.Pattern".to_string(),
                    generic_args: Vec::new(),
                },
                span: Span::dummy(),
            }),
            guard_strategy: RegexGuardStrategy::CaptureAndGuard {
                temp_name: Some("__tmp".to_string()),
            },
            java_type: JavaType::boolean(),
            span: Span::dummy(),
        };

        let rendered = generator
            .generate_expression(&expr)
            .expect("codegen succeeds");

        assert_eq!(
            rendered,
            "((userInput) instanceof java.lang.CharSequence __tmp && (Pattern.compile(\"[a-zA-Z]+\")).matcher(__tmp).matches())"
        );
    }
}

#[derive(Debug, Clone, Copy)]
struct PrimitiveConversionInfo {
    primitive: &'static str,
    wrapper: &'static str,
    unboxing_method: &'static str,
}

const PRIMITIVE_CONVERSIONS: &[PrimitiveConversionInfo] = &[
    PrimitiveConversionInfo {
        primitive: "boolean",
        wrapper: "java.lang.Boolean",
        unboxing_method: "booleanValue",
    },
    PrimitiveConversionInfo {
        primitive: "byte",
        wrapper: "java.lang.Byte",
        unboxing_method: "byteValue",
    },
    PrimitiveConversionInfo {
        primitive: "short",
        wrapper: "java.lang.Short",
        unboxing_method: "shortValue",
    },
    PrimitiveConversionInfo {
        primitive: "int",
        wrapper: "java.lang.Integer",
        unboxing_method: "intValue",
    },
    PrimitiveConversionInfo {
        primitive: "long",
        wrapper: "java.lang.Long",
        unboxing_method: "longValue",
    },
    PrimitiveConversionInfo {
        primitive: "float",
        wrapper: "java.lang.Float",
        unboxing_method: "floatValue",
    },
    PrimitiveConversionInfo {
        primitive: "double",
        wrapper: "java.lang.Double",
        unboxing_method: "doubleValue",
    },
    PrimitiveConversionInfo {
        primitive: "char",
        wrapper: "java.lang.Character",
        unboxing_method: "charValue",
    },
];

fn primitive_info_by_primitive(name: &str) -> Option<&'static PrimitiveConversionInfo> {
    PRIMITIVE_CONVERSIONS
        .iter()
        .find(|info| info.primitive.eq_ignore_ascii_case(name))
}

fn primitive_info_by_wrapper(name: &str) -> Option<&'static PrimitiveConversionInfo> {
    PRIMITIVE_CONVERSIONS.iter().find(|info| {
        info.wrapper.eq_ignore_ascii_case(name)
            || info
                .wrapper
                .rsplit('.')
                .next()
                .map(|simple| simple.eq_ignore_ascii_case(name))
                .unwrap_or(false)
    })
}
