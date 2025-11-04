use super::*;
use jv_build::metadata::JavaMethodSignature;
use jv_ir::{
    IrDoublebraceCopyPlan, IrDoublebraceMutatePlan, IrDoublebraceMutation, IrDoublebracePlan,
};

impl JavaCodeGenerator {
    pub(super) fn generate_doublebrace_expression(
        &mut self,
        base: &Option<Box<IrExpression>>,
        receiver_type: &JavaType,
        plan: &IrDoublebracePlan,
        span: &Span,
    ) -> Result<String, CodeGenError> {
        let base_expr = base
            .as_ref()
            .map(|expr| self.render_doublebrace_base(expr, receiver_type))
            .transpose()?;

        let base_expr = base_expr.ok_or_else(|| CodeGenError::UnsupportedConstruct {
            construct: "Doublebrace 初期化式に必要なベース式が不足しています".to_string(),
            span: Some(span.clone()),
        })?;

        let type_str = self.generate_type(receiver_type)?;
        self.ensure_stdlib_import(receiver_type);
        self.add_import("java.util.function.Supplier");

        let supplier_type = format!("java.util.function.Supplier<{}>", type_str);
        let mut builder = self.builder();
        builder.push_line(&format!("(({supplier_type}) () -> {{"));
        builder.indent();

        let base_var = self.fresh_identifier("__db_base");
        let mut base_assignment = format!("{} {} = {}", type_str, base_var, base_expr);
        if !base_assignment.trim_end().ends_with(';') {
            base_assignment.push(';');
        }
        Self::push_lines(&mut builder, base_assignment.trim_end_matches('\n'));

        let mut emitted = false;

        let return_var = match plan {
            IrDoublebracePlan::Mutate(mutate) => {
                emitted |= self.render_doublebrace_mutate_plan(&mut builder, mutate, &base_var)?;
                base_var.clone()
            }
            IrDoublebracePlan::Copy(copy) => {
                let (result_var, wrote) = self.render_doublebrace_copy_plan(
                    &mut builder,
                    copy,
                    receiver_type,
                    &type_str,
                    &base_var,
                    span,
                )?;
                emitted |= wrote;
                result_var
            }
        };

        if emitted {
            builder.push_line("");
        }
        builder.push_line(&format!("return {};", return_var));
        builder.dedent();
        builder.push_line("}).get()");

        let rendered = builder.build();
        Ok(rendered.trim_end().to_string())
    }

    fn render_doublebrace_mutate_plan(
        &mut self,
        builder: &mut JavaSourceBuilder,
        plan: &IrDoublebraceMutatePlan,
        target_var: &str,
    ) -> Result<bool, CodeGenError> {
        let mut wrote = false;
        for step in &plan.steps {
            match step {
                IrDoublebraceMutation::FieldAssignment(update) => {
                    let value_expr = self.generate_expression(&update.value)?;
                    let mut statement = format!("{}.{} = {}", target_var, update.name, value_expr);
                    if !statement.trim_end().ends_with(';') {
                        statement.push(';');
                    }
                    Self::push_lines(builder, statement.trim_end_matches('\n'));
                    wrote = true;
                }
                IrDoublebraceMutation::MethodCall(call) => {
                    let args =
                        self.render_arguments_with_style(&call.arguments, call.argument_style)?;
                    let mut statement = format!(
                        "{target}.{method}({args})",
                        target = target_var,
                        method = call.name,
                        args = args
                    );
                    if !statement.trim_end().ends_with(';') {
                        statement.push(';');
                    }
                    Self::push_lines(builder, statement.trim_end_matches('\n'));
                    wrote = true;
                }
                IrDoublebraceMutation::Statement(statements) => {
                    for stmt in statements {
                        let stmt_code = self.generate_statement(stmt)?;
                        Self::push_lines(builder, &stmt_code);
                        wrote = true;
                    }
                }
            }
        }

        Ok(wrote)
    }

    fn render_doublebrace_copy_plan(
        &mut self,
        builder: &mut JavaSourceBuilder,
        plan: &IrDoublebraceCopyPlan,
        receiver_type: &JavaType,
        rendered_type: &str,
        source_var: &str,
        span: &Span,
    ) -> Result<(String, bool), CodeGenError> {
        let result_var = self.fresh_identifier("__db_copy");
        let mut init_line = format!("{} {} = {};", rendered_type, result_var, source_var);
        if !init_line.trim_end().ends_with(';') {
            init_line.push(';');
        }
        Self::push_lines(builder, init_line.trim_end_matches('\n'));

        let mut wrote = false;
        for update in &plan.updates {
            let method_name = self
                .resolve_with_method(receiver_type, &update.name)
                .ok_or_else(|| CodeGenError::UnsupportedConstruct {
                    construct: format!(
                        "型 `{}` にフィールド `{}` を更新する `with*` メソッドが見つかりません",
                        rendered_type, update.name
                    ),
                    span: Some(span.clone()),
                })?;
            let value_expr = self.generate_expression(&update.value)?;
            let mut statement = format!(
                "{var} = {var}.{method}({value})",
                var = result_var,
                method = method_name,
                value = value_expr
            );
            if !statement.trim_end().ends_with(';') {
                statement.push(';');
            }
            Self::push_lines(builder, statement.trim_end_matches('\n'));
            wrote = true;
        }

        Ok((result_var, wrote))
    }

    fn resolve_with_method(&self, receiver_type: &JavaType, field_name: &str) -> Option<String> {
        let method_name = format!("with{}", to_pascal_case(field_name));
        match receiver_type {
            JavaType::Reference { name, .. } => {
                if let Some(index) = self.symbol_index() {
                    let base_name = strip_type_arguments(name);
                    if let Some(entry) = index.lookup_type(base_name) {
                        if let Some(signature) = entry.instance_methods.get(&method_name) {
                            if returns_same_type(signature, base_name) {
                                return Some(method_name);
                            } else {
                                return None;
                            }
                        }
                        return None;
                    }
                }
                Some(method_name)
            }
            _ => None,
        }
    }
}

fn to_pascal_case(value: &str) -> String {
    let mut result = String::new();
    for segment in value.split(|c: char| !c.is_ascii_alphanumeric()) {
        if segment.is_empty() {
            continue;
        }
        let mut chars = segment.chars();
        if let Some(first) = chars.next() {
            result.push(first.to_ascii_uppercase());
            for ch in chars {
                result.push(ch.to_ascii_lowercase());
            }
        }
    }
    if result.is_empty() {
        value.to_string()
    } else {
        result
    }
}

fn strip_type_arguments(name: &str) -> &str {
    name.split('<').next().unwrap_or(name).trim()
}

fn returns_same_type(signature: &JavaMethodSignature, expected: &str) -> bool {
    match &signature.return_type {
        JavaType::Reference { name, .. } => type_name_matches(name, expected),
        _ => false,
    }
}

fn type_name_matches(actual: &str, expected: &str) -> bool {
    let actual_base = strip_type_arguments(actual);
    let expected_base = strip_type_arguments(expected);
    if actual_base == expected_base {
        return true;
    }

    let actual_simple = actual_base.rsplit('.').next().unwrap_or(actual_base);
    let expected_simple = expected_base.rsplit('.').next().unwrap_or(expected_base);
    actual_simple == expected_simple
}
