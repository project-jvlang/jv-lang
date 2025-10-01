use super::*;

impl JavaCodeGenerator {
    pub fn generate_expression(&mut self, expr: &IrExpression) -> Result<String, CodeGenError> {
        match expr {
            IrExpression::Literal(literal, _) => Ok(Self::literal_to_string(literal)),
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
            implicit_end,
            strategy_description,
            ..
        } = switch
        {
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
            if let Some(implicit) = implicit_end.as_ref() {
                if let Some(rendered) = self.render_implicit_when_end_case(implicit, cases)? {
                    builder.push_line(&rendered);
                }
            }
            builder.dedent();
            builder.push_line("}");
            Ok(builder.build())
        } else {
            Err(CodeGenError::UnsupportedConstruct {
                construct: "Expected switch expression".to_string(),
                span: None,
            })
        }
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
        for label in labels {
            match label {
                IrCaseLabel::Literal(literal) => rendered.push(Self::literal_to_string(literal)),
                IrCaseLabel::TypePattern {
                    type_name,
                    variable,
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
}
