use super::*;

impl JavaCodeGenerator {
    pub fn generate_statement(&mut self, stmt: &IrStatement) -> Result<String, CodeGenError> {
        Ok(match stmt {
            IrStatement::VariableDeclaration {
                name,
                java_type,
                initializer,
                is_final,
                modifiers,
                ..
            } => {
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
                let mut line = self.generate_expression(expr)?;
                if !line.ends_with(';') {
                    line.push(';');
                }
                line
            }
            IrStatement::Return { value, .. } => match value {
                Some(expr) => format!("return {};", self.generate_expression(expr)?),
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
                ..
            } => {
                let mut builder = self.builder();
                builder.push_line(&format!(
                    "for ({} {} : {}) {{",
                    self.generate_type(variable_type)?,
                    variable,
                    self.generate_expression(iterable)?
                ));
                builder.indent();
                let body_code = self.generate_statement(body)?;
                Self::push_lines(&mut builder, &body_code);
                builder.dedent();
                builder.push_line("}");
                builder.build()
            }
            IrStatement::For {
                init,
                condition,
                update,
                body,
                ..
            } => {
                let init_str = match init {
                    Some(stmt) => {
                        let rendered = self.generate_statement(stmt)?;
                        rendered.trim_end_matches(';').to_string()
                    }
                    None => String::new(),
                };
                let condition_str = match condition {
                    Some(expr) => self.generate_expression(expr)?,
                    None => String::new(),
                };
                let update_str = match update {
                    Some(expr) => self.generate_expression(expr)?,
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
                builder.build()
            }
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
            IrStatement::Import {
                path,
                is_static,
                is_wildcard,
                ..
            } => {
                let mut stmt = String::from("import ");
                if *is_static {
                    stmt.push_str("static ");
                }
                stmt.push_str(path);
                if *is_wildcard && !path.ends_with(".*") {
                    stmt.push_str(".*");
                }
                stmt.push(';');
                stmt
            }
        })
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
            let labels = self.render_case_labels(&case.labels)?;
            let guard = match &case.guard {
                Some(cond) => format!(" when ({})", self.generate_expression(cond)?),
                None => String::new(),
            };
            builder.push_line(&format!("case {}{}:", labels, guard));
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

    fn generate_local_modifiers(&self, is_final: bool, modifiers: &IrModifiers) -> String {
        if is_final || modifiers.is_final {
            "final".to_string()
        } else {
            String::new()
        }
    }
}
