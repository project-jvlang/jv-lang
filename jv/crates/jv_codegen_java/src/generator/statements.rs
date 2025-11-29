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
                let mut effective_type = java_type.clone();
                if let Some(init_expr) = initializer.as_ref() {
                    if let IrExpression::TupleLiteral { span, .. } = init_expr {
                        if let Some(usage) = self.tuple_usages.get(&SpanKey::from(span)) {
                            effective_type = JavaType::Reference {
                                name: usage.record_name.clone(),
                                generic_args: vec![],
                            };
                        }
                    }
                    if let IrExpression::ObjectCreation { class_name, .. } = init_expr {
                        if self
                            .tuple_record_component_types
                            .contains_key(class_name.as_str())
                        {
                            effective_type = JavaType::Reference {
                                name: class_name.clone(),
                                generic_args: vec![],
                            };
                        }
                    }
                    if let IrExpression::FieldAccess {
                        receiver,
                        field_name,
                        is_record_component,
                        ..
                    } = init_expr
                    {
                        if *is_record_component {
                            let receiver_type = if let IrExpression::Identifier { name, .. } =
                                receiver.as_ref()
                            {
                                self.local_tuple_types
                                    .get(name)
                                    .cloned()
                                    .or_else(|| Self::expression_java_type(receiver).cloned())
                            } else {
                                Self::expression_java_type(receiver).cloned()
                            };
                            if let Some(JavaType::Reference { name, .. }) = receiver_type {
                                if let Some(component_type) =
                                    self.tuple_component_type(&name, field_name)
                                {
                                    effective_type = component_type;
                                }
                            }
                        }
                    }

                    let mut tuple_type = if let IrExpression::MethodCall {
                        method_name,
                        java_name,
                        resolved_target,
                        ..
                    } = init_expr
                    {
                        let mut candidates: Vec<String> = Vec::new();
                        if let Some(name) = java_name.as_deref() {
                            candidates.push(name.to_string());
                        }
                        if let Some(name) = resolved_target
                            .as_ref()
                            .and_then(|target| target.java_name.as_deref())
                        {
                            candidates.push(name.to_string());
                        }
                        if let Some(name) = resolved_target
                            .as_ref()
                            .and_then(|target| target.original_name.as_deref())
                        {
                            candidates.push(name.to_string());
                        }
                        candidates.push(method_name.to_string());

                        let tuple_type = candidates.iter().find_map(|name| {
                            self.tuple_return_records
                                .get(name.as_str())
                                .map(|record| JavaType::Reference {
                                    name: record.clone(),
                                    generic_args: vec![],
                                })
                        });

                        let tuple_type = tuple_type.or_else(|| {
                            candidates.iter().find_map(|name| {
                                let fallback = format!("{}_Result", pascalize(name));
                                let known_record = self
                                    .tuple_return_records
                                    .values()
                                    .any(|record| record == &fallback);
                                if known_record
                                    || self.has_tuple_record_definition(&fallback)
                                {
                                    Some(JavaType::Reference {
                                        name: fallback,
                                        generic_args: vec![],
                                    })
                                } else {
                                    None
                                }
                            })
                        });

                        tuple_type
                    } else {
                        None
                    };

                    if tuple_type.is_none() {
                        if let IrExpression::Identifier { name: init_name, .. } = init_expr {
                            if let Some(record_type) = self.local_tuple_types.get(init_name) {
                                tuple_type = Some(record_type.clone());
                            }
                        }
                    }

                    if let Some(record_type) = tuple_type {
                        effective_type = record_type;
                    } else if effective_type == JavaType::object() {
                        if let Some(JavaType::Reference { name, .. }) =
                            Self::expression_java_type(init_expr)
                        {
                            if self.tuple_record_component_types.contains_key(name) {
                                effective_type = JavaType::Reference {
                                    name: name.clone(),
                                    generic_args: vec![],
                                };
                            }
                        }
                    }
                }
                if effective_type != JavaType::object() {
                    self.local_tuple_types
                        .insert(name.clone(), effective_type.clone());
                }

                if self.mutable_captures.contains(name) {
                    self.add_import("java.util.concurrent.atomic.AtomicReference");
                    let boxed_type = JavaCodeGenerator::boxed_type(&effective_type);
                    let mut parts = Vec::new();
                    let modifier = self.generate_local_modifiers(true, modifiers);
                    if !modifier.is_empty() {
                        parts.push(modifier);
                    }
                    let type_str = format!("AtomicReference<{}>", self.generate_type(&boxed_type)?);
                    parts.push(type_str);
                    parts.push(name.clone());
                    let mut line = parts.join(" ");
                    let rendered_initializer = if let Some(expr) = initializer {
                        Some(
                            self.try_render_destructure_component(expr)?
                                .unwrap_or(self.generate_expression(expr)?),
                        )
                    } else {
                        None
                    };
                    line.push_str(" = new AtomicReference<>(");
                    if let Some(expr) = rendered_initializer {
                        line.push_str(&expr);
                    }
                    line.push_str(");");
                    line
                } else {
                    let mut parts = Vec::new();
                    let modifier = self.generate_local_modifiers(*is_final, modifiers);
                    if !modifier.is_empty() {
                        parts.push(modifier);
                    }
                    parts.push(self.generate_type(&effective_type)?);
                    parts.push(name.clone());
                    let mut line = parts.join(" ");
                    if let Some(expr) = initializer {
                        let rendered = self
                            .try_render_destructure_component(expr)?
                            .unwrap_or(self.generate_expression(expr)?);
                        line.push_str(" = ");
                        line.push_str(&rendered);
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
                if !modifiers.annotations.is_empty() {
                    let mut builder = self.builder();
                    for annotation in &modifiers.annotations {
                        // Ensure imports are registered for qualified annotations
                        self.register_annotation_imports(annotation);
                        builder.push_line(&self.format_annotation(annotation));
                    }
                    builder.push_line(&line);
                    builder.build()
                } else {
                    line
                }
            }
            IrStatement::MethodDeclaration { .. } => self.generate_method(stmt)?,
            IrStatement::ClassDeclaration { .. } => self.generate_class(stmt)?,
            IrStatement::InterfaceDeclaration { .. } => self.generate_interface(stmt)?,
            IrStatement::RecordDeclaration { .. } => self.generate_record(stmt)?,
            IrStatement::SampleDeclaration(declaration) => {
                let artifacts = self.generate_sample_declaration_artifacts(declaration)?;
                let mut builder = self.builder();
                for (index, code) in artifacts.iter().enumerate() {
                    if index > 0 {
                        builder.push_line("");
                    }
                    Self::push_lines(&mut builder, code);
                }
                builder.build()
            }
            IrStatement::Expression { expr, .. } => {
                if let IrExpression::LogInvocation { plan, .. } = expr {
                    self.generate_log_invocation(plan)?
                } else if let Some(rendered) = self.try_render_destructure_expression(expr)? {
                    let mut line = rendered;
                    if !line.ends_with(';') {
                        line.push(';');
                    }
                    line
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
            IrStatement::Block { statements, .. } => {
                let mut builder = self.builder();
                builder.push_line("{");
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
                variable,
                variable_type,
                iterable,
                body,
                iterable_kind,
                ..
            } => {
                self.generate_for_each_loop(variable, variable_type, iterable, body, iterable_kind)?
            }
            IrStatement::For {
                init,
                condition,
                update,
                body,
                metadata,
                ..
            } => self.generate_for_loop(
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
                Some(name) => format!("break {};", name),
                None => "break;".to_string(),
            },
            IrStatement::Continue { label, .. } => match label {
                Some(name) => format!("continue {};", name),
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
        variable: &str,
        variable_type: &JavaType,
        iterable: &IrExpression,
        body: &IrStatement,
        iterable_kind: &IrForEachKind,
    ) -> Result<String, CodeGenError> {
        match iterable_kind {
            IrForEachKind::Iterable => {
                self.render_enhanced_for_loop(variable, variable_type, iterable, body)
            }
            IrForEachKind::LazySequence { needs_cleanup } => {
                if *needs_cleanup {
                    self.render_lazy_sequence_loop(variable, variable_type, iterable, body)
                } else {
                    self.render_enhanced_for_loop(variable, variable_type, iterable, body)
                }
            }
        }
    }

    fn render_enhanced_for_loop(
        &mut self,
        variable: &str,
        variable_type: &JavaType,
        iterable: &IrExpression,
        body: &IrStatement,
    ) -> Result<String, CodeGenError> {
        let item_type = self.resolve_for_each_item_type(variable_type, iterable);
        let mut builder = self.builder();
        builder.push_line(&format!(
            "for ({} {} : {}) {{",
            self.generate_type(&item_type)?,
            variable,
            self.generate_expression(iterable)?
        ));
        builder.indent();
        let body_code = self.generate_statement(body)?;
        Self::push_lines(&mut builder, &body_code);
        builder.dedent();
        builder.push_line("}");
        Ok(builder.build())
    }

    fn render_lazy_sequence_loop(
        &mut self,
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
        builder.push_line(&format!(
            "for ({} {} : __jvSequence) {{",
            item_type, variable
        ));
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
        builder.push_line(&format!(
            "for ({}; {}; {}) {{",
            init_str, condition_str, update_str
        ));
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

    fn generate_switch_statement(
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
            let body_expr = self.generate_expression(&case.body)?;
            builder.push_line(&format!("{};", body_expr));
            builder.push_line("break;");
            builder.dedent();
        }
        builder.dedent();
        builder.push_line("}");
        Ok(builder.build())
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

    fn tuple_component_type(&self, record_name: &str, field_name: &str) -> Option<JavaType> {
        let components = self.tuple_record_component_types.get(record_name)?;
        let index = if let Some(stripped) = field_name.strip_prefix('_') {
            stripped
                .parse::<usize>()
                .ok()
                .and_then(|pos| pos.checked_sub(1))
        } else {
            components
                .iter()
                .position(|(name, _)| name == field_name)
        }?;
        components.get(index).map(|(_, ty)| ty.clone())
    }
}

fn pascalize(name: &str) -> String {
    let mut chars = name.chars();
    match chars.next() {
        Some(first) => {
            let mut result = String::new();
            result.extend(first.to_uppercase());
            result.push_str(chars.as_str());
            result
        }
        None => String::new(),
    }
}
