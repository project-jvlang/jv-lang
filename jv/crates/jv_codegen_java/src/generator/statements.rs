use super::*;
use jv_ast::types::RawTypeContinuation;

impl JavaCodeGenerator {
    pub fn generate_statement(&mut self, stmt: &IrStatement) -> Result<String, CodeGenError> {
        Ok(match stmt {
            IrStatement::Comment { text, .. } => text.clone(),
            IrStatement::Commented {
                statement, comment, ..
            } => {
                if let Some((mode, owner)) = Self::parse_raw_type_comment(comment) {
                    let rendered = match mode {
                        RawTypeContinuation::AllowWithComment => {
                            self.generate_statement(statement)?
                        }
                        RawTypeContinuation::DefaultPolicy => {
                            self.generate_statement_with_null_guard(statement, &owner)?
                        }
                    };
                    Self::append_inline_comment(rendered, comment)
                } else {
                    let rendered = self.generate_statement(statement)?;
                    Self::append_inline_comment(rendered, comment)
                }
            }
            IrStatement::VariableDeclaration {
                name,
                java_type,
                initializer,
                is_final,
                modifiers,
                ..
            } => {
                if self.mutable_captures.contains(name) {
                    self.add_import("java.util.concurrent.atomic.AtomicReference");
                    let boxed_type = JavaCodeGenerator::boxed_type(java_type);
                    let mut parts = Vec::new();
                    let modifier = self.generate_local_modifiers(true, modifiers);
                    if !modifier.is_empty() {
                        parts.push(modifier);
                    }
                    let type_str = format!("AtomicReference<{}>", self.generate_type(&boxed_type)?);
                    parts.push(type_str);
                    parts.push(name.clone());
                    let mut line = parts.join(" ");
                    line.push_str(" = new AtomicReference<>(");
                    if let Some(expr) = initializer {
                        line.push_str(&self.generate_expression(expr)?);
                    }
                    line.push_str(");");
                    line
                } else {
                    let mut parts = Vec::new();
                    let modifier = self.generate_local_modifiers(*is_final, modifiers);
                    if !modifier.is_empty() {
                        parts.push(modifier);
                    }
                    parts.push(self.generate_type(java_type)?);
                    parts.push(name.clone());
                    let mut line = parts.join(" ");
                    if let Some(expr) = initializer {
                        line.push_str(" = ");
                        line.push_str(&self.generate_expression(expr)?);
                    }
                    line.push(';');
                    line
                }
            }
            IrStatement::FieldDeclaration {
                name,
                java_type,
                initializer,
                modifiers,
                ..
            } => {
                let mut parts = Vec::new();
                let promote_static = initializer
                    .as_ref()
                    .map(|expr| matches!(expr, IrExpression::RegexPattern { .. }))
                    .unwrap_or(false)
                    && modifiers.is_final;
                let promoted_modifiers = if promote_static {
                    let mut clone = modifiers.clone();
                    clone.is_static = true;
                    clone.is_final = true;
                    Some(clone)
                } else {
                    None
                };
                let modifiers_to_render = promoted_modifiers.as_ref().unwrap_or(modifiers);
                let modifiers_str = self.generate_modifiers(modifiers_to_render);
                if !modifiers_str.is_empty() {
                    parts.push(modifiers_str);
                }
                parts.push(self.generate_type(java_type)?);
                parts.push(name.clone());
                let mut line = parts.join(" ");
                if let Some(expr) = initializer {
                    line.push_str(" = ");
                    line.push_str(&self.generate_expression(expr)?);
                }
                line.push(';');
                line
            }
            IrStatement::MethodDeclaration { .. } => self.generate_method(stmt)?,
            IrStatement::ClassDeclaration { .. } => self.generate_class(stmt)?,
            IrStatement::InterfaceDeclaration { .. } => self.generate_interface(stmt)?,
            IrStatement::RecordDeclaration { .. } => self.generate_record(stmt)?,
            IrStatement::SampleDeclaration(declaration) => {
                self.generate_sample_declaration_binding(declaration)?
            }
            IrStatement::Expression { expr, .. } => {
                if matches!(expr, IrExpression::Block { .. }) {
                    self.generate_expression(expr)?
                } else {
                    let mut line = self.generate_expression(expr)?;
                    if !line.ends_with(';') {
                        line.push(';');
                    }
                    line
                }
            }
            IrStatement::Return { value, .. } => match value {
                Some(expr) => {
                    let expr_code = self.generate_expression(expr)?;
                    let coerced = if let Some(target) = self.current_return_type.as_ref() {
                        let target_clone = target.clone();
                        self.coerce_return_expression(expr, expr_code, &target_clone)?
                    } else {
                        expr_code
                    };
                    format!("return {};", coerced)
                }
                None => "return;".to_string(),
            },
            IrStatement::Block {
                label, statements, ..
            } => {
                let mut builder = self.builder();
                if let Some(raw_label) = label.as_deref() {
                    builder.push_line(&format!("{}: {{", Self::normalize_label(raw_label)));
                } else {
                    builder.push_line("{");
                }
                builder.indent();
                for statement in statements {
                    let inner = self.generate_statement(statement)?;
                    Self::push_lines(&mut builder, &inner);
                }
                builder.dedent();
                builder.push_line("}");
                builder.build()
            }
            IrStatement::If {
                condition,
                then_stmt,
                else_stmt,
                ..
            } => {
                let mut builder = self.builder();
                builder.push_line(&format!("if ({}) {{", self.generate_expression(condition)?));
                builder.indent();
                let then_block = self.generate_statement(then_stmt)?;
                Self::push_lines(&mut builder, &then_block);
                builder.dedent();
                if let Some(else_branch) = else_stmt {
                    builder.push_line("} else {");
                    builder.indent();
                    let else_block = self.generate_statement(else_branch)?;
                    Self::push_lines(&mut builder, &else_block);
                    builder.dedent();
                }
                builder.push_line("}");
                builder.build()
            }
            IrStatement::While {
                condition, body, ..
            } => {
                let mut builder = self.builder();
                builder.push_line(&format!(
                    "while ({}) {{",
                    self.generate_expression(condition)?
                ));
                builder.indent();
                let body_code = self.generate_statement(body)?;
                Self::push_lines(&mut builder, &body_code);
                builder.dedent();
                builder.push_line("}");
                builder.build()
            }
            IrStatement::ForEach {
                label,
                variable,
                variable_type,
                iterable,
                body,
                iterable_kind,
                ..
            } => self.generate_for_each_loop(
                label.as_deref(),
                variable,
                variable_type,
                iterable,
                body,
                iterable_kind,
            )?,
            IrStatement::For {
                label,
                init,
                condition,
                update,
                body,
                metadata,
                ..
            } => self.generate_for_loop(
                label.as_deref(),
                init.as_deref(),
                condition.as_ref(),
                update.as_ref(),
                body,
                metadata,
            )?,
            IrStatement::Switch {
                discriminant,
                cases,
                ..
            } => self.generate_switch_statement(discriminant, cases)?,
            IrStatement::Try {
                body,
                catch_clauses,
                finally_block,
                ..
            } => self.generate_try_statement(body, catch_clauses, finally_block.as_deref())?,
            IrStatement::TryWithResources {
                resources,
                body,
                catch_clauses,
                finally_block,
                ..
            } => self.generate_try_with_resources_statement(
                resources,
                body,
                catch_clauses,
                finally_block.as_deref(),
            )?,
            IrStatement::Throw { expr, .. } => {
                format!("throw {};", self.generate_expression(expr)?)
            }
            IrStatement::Break { label, .. } => match label {
                Some(name) => format!("break {};", Self::normalize_label(name)),
                None => "break;".to_string(),
            },
            IrStatement::Continue { label, .. } => match label {
                Some(name) => format!("continue {};", Self::normalize_label(name)),
                None => "continue;".to_string(),
            },
            IrStatement::Package { name, .. } => format!("package {};", name),
            IrStatement::Import(import) => {
                let entry = Self::render_import_entry(import);
                format!("import {};", entry)
            }
        })
    }

    fn generate_for_each_loop(
        &mut self,
        label: Option<&str>,
        variable: &str,
        variable_type: &JavaType,
        iterable: &IrExpression,
        body: &IrStatement,
        iterable_kind: &IrForEachKind,
    ) -> Result<String, CodeGenError> {
        match iterable_kind {
            IrForEachKind::Iterable => {
                self.render_enhanced_for_loop(label, variable, variable_type, iterable, body)
            }
            IrForEachKind::LazySequence { needs_cleanup } => {
                if *needs_cleanup {
                    self.render_lazy_sequence_loop(label, variable, variable_type, iterable, body)
                } else {
                    self.render_enhanced_for_loop(label, variable, variable_type, iterable, body)
                }
            }
        }
    }

    fn render_enhanced_for_loop(
        &mut self,
        label: Option<&str>,
        variable: &str,
        variable_type: &JavaType,
        iterable: &IrExpression,
        body: &IrStatement,
    ) -> Result<String, CodeGenError> {
        let item_type = self.resolve_for_each_item_type(variable_type, iterable);
        let item_type_str = self.generate_type(&item_type)?;
        let iterable_expr = self.generate_expression(iterable)?;
        let mut builder = self.builder();
        let header = if let Some(raw_label) = label {
            format!(
                "{}: for ({} {} : {}) {{",
                Self::normalize_label(raw_label),
                item_type_str,
                variable,
                iterable_expr
            )
        } else {
            format!(
                "for ({} {} : {}) {{",
                item_type_str, variable, iterable_expr
            )
        };
        builder.push_line(&header);
        builder.indent();
        let body_code = self.generate_statement(body)?;
        Self::push_lines(&mut builder, &body_code);
        builder.dedent();
        builder.push_line("}");
        Ok(builder.build())
    }

    fn render_lazy_sequence_loop(
        &mut self,
        label: Option<&str>,
        variable: &str,
        variable_type: &JavaType,
        iterable: &IrExpression,
        body: &IrStatement,
    ) -> Result<String, CodeGenError> {
        let iterable_source = self.generate_expression(iterable)?;
        let resolved_type = self.resolve_for_each_item_type(variable_type, iterable);
        let item_type = self.generate_type(&resolved_type)?;
        let mut builder = self.builder();
        builder.push_line("{");
        builder.indent();
        builder.push_line(&format!("final var __jvSequence = {};", iterable_source));
        builder.push_line("try {");
        builder.indent();
        let header = if let Some(raw_label) = label {
            format!(
                "{}: for ({} {} : __jvSequence) {{",
                Self::normalize_label(raw_label),
                item_type,
                variable
            )
        } else {
            format!("for ({} {} : __jvSequence) {{", item_type, variable)
        };
        builder.push_line(&header);
        builder.indent();
        let body_code = self.generate_statement(body)?;
        Self::push_lines(&mut builder, &body_code);
        builder.dedent();
        builder.push_line("}");
        builder.dedent();
        builder.push_line("} finally {");
        builder.indent();
        builder.push_line("if (__jvSequence instanceof AutoCloseable closeable) {");
        builder.indent();
        builder.push_line("try {");
        builder.indent();
        builder.push_line("closeable.close();");
        builder.dedent();
        builder.push_line("} catch (Exception closeError) {");
        builder.indent();
        builder.push_line("throw new RuntimeException(closeError);");
        builder.dedent();
        builder.push_line("}");
        builder.dedent();
        builder.push_line("}");
        builder.dedent();
        builder.push_line("}");
        builder.dedent();
        builder.push_line("}");
        Ok(builder.build())
    }

    fn resolve_for_each_item_type(
        &self,
        declared_type: &JavaType,
        iterable: &IrExpression,
    ) -> JavaType {
        if !Self::is_java_lang_object_type(declared_type) {
            return declared_type.clone();
        }

        if let Some(iterable_type) = Self::expression_java_type(iterable) {
            if let Some(element) = Self::iterable_element_type(iterable_type) {
                return element;
            }
        }

        declared_type.clone()
    }

    fn iterable_element_type(java_type: &JavaType) -> Option<JavaType> {
        match java_type {
            JavaType::Reference { generic_args, .. } if !generic_args.is_empty() => {
                Some(Self::normalize_iterable_element(generic_args[0].clone()))
            }
            JavaType::Array { element_type, .. } => Some(*element_type.clone()),
            _ => None,
        }
    }

    fn normalize_iterable_element(element: JavaType) -> JavaType {
        match element {
            JavaType::Wildcard {
                bound: Some(bound), ..
            } => (*bound).clone(),
            JavaType::Wildcard { .. } => JavaType::Reference {
                name: "java.lang.Object".to_string(),
                generic_args: Vec::new(),
            },
            other => other,
        }
    }

    fn is_java_lang_object_type(java_type: &JavaType) -> bool {
        matches!(
            java_type,
            JavaType::Reference { name, generic_args }
                if generic_args.is_empty()
                    && (name == "java.lang.Object" || name == "Object")
        )
    }

    fn generate_for_loop(
        &mut self,
        label: Option<&str>,
        init: Option<&IrStatement>,
        condition: Option<&IrExpression>,
        update: Option<&IrExpression>,
        body: &IrStatement,
        metadata: &Option<IrForLoopMetadata>,
    ) -> Result<String, CodeGenError> {
        let init_str = init.map(|stmt| self.generate_statement(stmt)).transpose()?;
        let init_str = init_str
            .as_deref()
            .map(|rendered| rendered.trim_end_matches(';').to_string())
            .unwrap_or_default();

        let condition_str = match condition {
            Some(expr) => self.generate_expression(expr)?,
            None => String::new(),
        };

        let update_str = match update {
            Some(expr) => self.render_for_update(expr, metadata)?,
            None => String::new(),
        };

        let mut builder = self.builder();
        let header = if let Some(raw_label) = label {
            format!(
                "{}: for ({}; {}; {}) {{",
                Self::normalize_label(raw_label),
                init_str,
                condition_str,
                update_str
            )
        } else {
            format!("for ({}; {}; {}) {{", init_str, condition_str, update_str)
        };
        builder.push_line(&header);
        builder.indent();
        let body_code = self.generate_statement(body)?;
        Self::push_lines(&mut builder, &body_code);
        builder.dedent();
        builder.push_line("}");
        Ok(builder.build())
    }

    fn render_for_update(
        &mut self,
        update: &IrExpression,
        metadata: &Option<IrForLoopMetadata>,
    ) -> Result<String, CodeGenError> {
        if let Some(IrForLoopMetadata::NumericRange(meta)) = metadata {
            if let Some(compact) = self.try_render_numeric_update(update, meta) {
                return Ok(compact);
            }
        }
        self.generate_expression(update)
    }

    fn try_render_numeric_update(
        &self,
        update: &IrExpression,
        metadata: &IrNumericRangeLoop,
    ) -> Option<String> {
        if let IrExpression::Assignment { target, value, .. } = update {
            if let IrExpression::Identifier {
                name: target_name, ..
            } = target.as_ref()
            {
                if target_name == &metadata.binding {
                    if let IrExpression::Binary {
                        left, op, right, ..
                    } = value.as_ref()
                    {
                        if matches!(op, BinaryOp::Add) {
                            if let IrExpression::Identifier {
                                name: left_name, ..
                            } = left.as_ref()
                            {
                                if left_name == target_name {
                                    if let IrExpression::Literal(literal, _) = right.as_ref() {
                                        if let Literal::Number(number) = literal {
                                            if number == "1" {
                                                return Some(format!("{}++", target_name));
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        None
    }

    pub(super) fn generate_switch_statement(
        &mut self,
        discriminant: &IrExpression,
        cases: &[IrSwitchCase],
    ) -> Result<String, CodeGenError> {
        let mut builder = self.builder();
        builder.push_line(&format!(
            "switch ({}) {{",
            self.generate_expression(discriminant)?
        ));
        builder.indent();
        for case in cases {
            let guard = match &case.guard {
                Some(cond) => format!(" when ({})", self.generate_expression(cond)?),
                None => String::new(),
            };
            if Self::is_default_only_case(case) {
                builder.push_line(&format!("default{}:", guard));
            } else {
                let labels = self.render_case_labels(&case.labels)?;
                builder.push_line(&format!("case {}{}:", labels, guard));
            }
            builder.indent();
            let body_stmt = Self::expression_to_statement(&case.body);
            let body_code = self.generate_statement(&body_stmt)?;
            Self::push_lines(&mut builder, &body_code);
            if Self::case_body_needs_break(&body_stmt) {
                builder.push_line("break;");
            }
            builder.dedent();
        }
        builder.dedent();
        builder.push_line("}");
        Ok(builder.build())
    }

    fn expression_to_statement(expr: &IrExpression) -> IrStatement {
        match expr {
            IrExpression::Block {
                statements, span, ..
            } => IrStatement::Block {
                label: None,
                statements: statements.clone(),
                span: span.clone(),
            },
            IrExpression::Lambda { body, .. } => Self::expression_to_statement(body),
            other => IrStatement::Expression {
                expr: other.clone(),
                span: other.span(),
            },
        }
    }

    fn case_body_needs_break(stmt: &IrStatement) -> bool {
        match stmt {
            IrStatement::Return { .. }
            | IrStatement::Throw { .. }
            | IrStatement::Break { .. }
            | IrStatement::Continue { .. } => false,
            IrStatement::Block { statements, .. } => statements
                .last()
                .map(|inner| Self::case_body_needs_break(inner))
                .unwrap_or(true),
            _ => true,
        }
    }

    fn generate_try_statement(
        &mut self,
        body: &IrStatement,
        catches: &[IrCatchClause],
        finally_block: Option<&IrStatement>,
    ) -> Result<String, CodeGenError> {
        let mut builder = self.builder();
        builder.push_line("try {");
        builder.indent();
        let body_code = self.generate_statement(body)?;
        Self::push_lines(&mut builder, &body_code);
        builder.dedent();
        builder.push_line("}");
        for catch in catches {
            builder.push_line(&format!(
                "catch ({} {}) {{",
                self.generate_type(&catch.exception_type)?,
                catch.variable_name
            ));
            builder.indent();
            let catch_body = self.generate_statement(&catch.body)?;
            Self::push_lines(&mut builder, &catch_body);
            builder.dedent();
            builder.push_line("}");
        }
        if let Some(finally_stmt) = finally_block {
            builder.push_line("finally {");
            builder.indent();
            let finally_code = self.generate_statement(finally_stmt)?;
            Self::push_lines(&mut builder, &finally_code);
            builder.dedent();
            builder.push_line("}");
        }
        Ok(builder.build())
    }

    fn generate_try_with_resources_statement(
        &mut self,
        resources: &[IrResource],
        body: &IrStatement,
        catches: &[IrCatchClause],
        finally_block: Option<&IrStatement>,
    ) -> Result<String, CodeGenError> {
        let mut builder = self.builder();
        builder.push_line(&self.render_try_with_resources_header(resources)?);
        builder.indent();
        let body_code = self.generate_statement(body)?;
        Self::push_lines(&mut builder, &body_code);
        builder.dedent();
        builder.push_line("}");
        for catch in catches {
            builder.push_line(&format!(
                "catch ({} {}) {{",
                self.generate_type(&catch.exception_type)?,
                catch.variable_name
            ));
            builder.indent();
            let catch_body = self.generate_statement(&catch.body)?;
            Self::push_lines(&mut builder, &catch_body);
            builder.dedent();
            builder.push_line("}");
        }
        if let Some(finally_stmt) = finally_block {
            builder.push_line("finally {");
            builder.indent();
            let finally_code = self.generate_statement(finally_stmt)?;
            Self::push_lines(&mut builder, &finally_code);
            builder.dedent();
            builder.push_line("}");
        }
        Ok(builder.build())
    }

    pub(super) fn render_try_with_resources_header(
        &mut self,
        resources: &[IrResource],
    ) -> Result<String, CodeGenError> {
        let mut header = String::from("try (");
        let mut rendered = Vec::new();
        for resource in resources {
            let mut entry = String::new();
            entry.push_str(&self.generate_type(&resource.java_type)?);
            entry.push(' ');
            entry.push_str(&resource.name);
            entry.push_str(" = ");
            entry.push_str(&self.generate_expression(&resource.initializer)?);
            rendered.push(entry);
        }
        header.push_str(&rendered.join("; "));
        header.push_str(") {");
        Ok(header)
    }

    fn generate_statement_with_null_guard(
        &mut self,
        statement: &IrStatement,
        owner: &str,
    ) -> Result<String, CodeGenError> {
        match statement {
            IrStatement::VariableDeclaration {
                name,
                java_type,
                initializer,
                is_final,
                modifiers,
                ..
            } => {
                if self.mutable_captures.contains(name) {
                    self.add_import("java.util.concurrent.atomic.AtomicReference");
                    let boxed_type = JavaCodeGenerator::boxed_type(java_type);
                    let mut parts = Vec::new();
                    let modifier = self.generate_local_modifiers(true, modifiers);
                    if !modifier.is_empty() {
                        parts.push(modifier);
                    }
                    let type_str = format!("AtomicReference<{}>", self.generate_type(&boxed_type)?);
                    parts.push(type_str);
                    parts.push(name.clone());
                    let mut line = parts.join(" ");
                    line.push_str(" = new AtomicReference<>(");
                    if let Some(expr) = initializer {
                        let expr_code = self.generate_expression(expr)?;
                        let guarded = self.wrap_with_null_guard(expr_code, owner);
                        line.push_str(&guarded);
                    }
                    line.push_str(");");
                    Ok(line)
                } else {
                    let mut parts = Vec::new();
                    let modifier = self.generate_local_modifiers(*is_final, modifiers);
                    if !modifier.is_empty() {
                        parts.push(modifier);
                    }
                    parts.push(self.generate_type(java_type)?);
                    parts.push(name.clone());
                    let mut line = parts.join(" ");
                    if let Some(expr) = initializer {
                        let expr_code = self.generate_expression(expr)?;
                        let guarded = self.wrap_with_null_guard(expr_code, owner);
                        line.push_str(" = ");
                        line.push_str(&guarded);
                    }
                    line.push(';');
                    Ok(line)
                }
            }
            IrStatement::FieldDeclaration {
                name,
                java_type,
                initializer,
                modifiers,
                ..
            } => {
                let mut parts = Vec::new();
                let modifiers_str = self.generate_modifiers(modifiers);
                if !modifiers_str.is_empty() {
                    parts.push(modifiers_str);
                }
                parts.push(self.generate_type(java_type)?);
                parts.push(name.clone());
                let mut line = parts.join(" ");
                if let Some(expr) = initializer {
                    let expr_code = self.generate_expression(expr)?;
                    let guarded = self.wrap_with_null_guard(expr_code, owner);
                    line.push_str(" = ");
                    line.push_str(&guarded);
                }
                line.push(';');
                Ok(line)
            }
            IrStatement::Return {
                value: Some(expr), ..
            } => {
                let expr_code = self.generate_expression(expr)?;
                let coerced = if let Some(target) = self.current_return_type.as_ref() {
                    let target_clone = target.clone();
                    self.coerce_return_expression(expr, expr_code, &target_clone)?
                } else {
                    expr_code
                };
                let guarded = self.wrap_with_null_guard(coerced, owner);
                Ok(format!("return {};", guarded))
            }
            IrStatement::Expression { expr, .. } => {
                if let IrExpression::Assignment { target, value, .. } = expr {
                    if let IrExpression::Identifier { name, .. } = target.as_ref() {
                        if self.mutable_captures.contains(name) {
                            let rhs = self.generate_expression(value)?;
                            let guarded_rhs = self.wrap_with_null_guard(rhs, owner);
                            return Ok(format!("{}.set({});", name, guarded_rhs));
                        }
                    }
                    let lhs = self.generate_expression(target)?;
                    let rhs = self.generate_expression(value)?;
                    let guarded_rhs = self.wrap_with_null_guard(rhs, owner);
                    Ok(format!("{} = {};", lhs, guarded_rhs))
                } else {
                    let mut rendered = self.generate_expression(expr)?;
                    if !rendered.ends_with(';') {
                        rendered.push(';');
                    }
                    Ok(rendered)
                }
            }
            _ => self.generate_statement(statement),
        }
    }

    fn wrap_with_null_guard(&mut self, expr: String, owner: &str) -> String {
        self.add_import("java.util.Objects");
        format!(
            "Objects.requireNonNull({}, \"JV: raw type guard for {}\")",
            expr, owner
        )
    }

    fn generate_local_modifiers(&self, is_final: bool, modifiers: &IrModifiers) -> String {
        if is_final || modifiers.is_final {
            "final".to_string()
        } else {
            String::new()
        }
    }

    // === Statement Helpers (moved from helpers.rs) ===

    /// Check if switch case contains only a default label.
    pub(super) fn is_default_only_case(case: &IrSwitchCase) -> bool {
        case.labels.len() == 1 && matches!(case.labels[0], IrCaseLabel::Default)
    }

    pub(super) fn normalize_label(label: &str) -> String {
        let trimmed = label.trim();
        let without_hash = trimmed.strip_prefix('#').unwrap_or(trimmed);
        if without_hash.is_empty() {
            trimmed.to_string()
        } else {
            without_hash.to_string()
        }
    }
}
