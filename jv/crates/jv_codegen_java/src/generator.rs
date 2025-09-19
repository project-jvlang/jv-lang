use crate::builder::{JavaCompilationUnit, JavaSourceBuilder};
use crate::config::JavaCodeGenConfig;
use crate::error::CodeGenError;
use jv_ast::{BinaryOp, Literal, UnaryOp};
use jv_ir::*;
use std::collections::HashMap;

/// Main Java code generator
pub struct JavaCodeGenerator {
    /// Import statements the current compilation unit requires.
    imports: HashMap<String, String>,
    /// Configuration options controlling formatting and feature usage.
    config: JavaCodeGenConfig,
}

impl JavaCodeGenerator {
    pub fn new() -> Self {
        Self::with_config(JavaCodeGenConfig::default())
    }

    pub fn with_config(config: JavaCodeGenConfig) -> Self {
        Self {
            imports: HashMap::new(),
            config,
        }
    }

    /// Generate a Java compilation unit for the provided IR program.
    pub fn generate_compilation_unit(
        &mut self,
        program: &IrProgram,
    ) -> Result<JavaCompilationUnit, CodeGenError> {
        self.reset();

        let mut unit = JavaCompilationUnit::new();
        unit.package_declaration = program.package.clone();

        // Propagate explicit import statements that were preserved in IR.
        for import in &program.imports {
            if let IrStatement::Import {
                path,
                is_static,
                is_wildcard,
                ..
            } = import
            {
                let mut stmt = String::from("import ");
                if *is_static {
                    stmt.push_str("static ");
                }
                stmt.push_str(path);
                if *is_wildcard && !path.ends_with(".*") {
                    stmt.push_str(".*");
                }
                stmt.push(';');
                unit.imports.push(stmt);
            }
        }

        for declaration in &program.type_declarations {
            let code = match declaration {
                IrStatement::ClassDeclaration { .. } => self.generate_class(declaration)?,
                IrStatement::InterfaceDeclaration { .. } => self.generate_interface(declaration)?,
                IrStatement::RecordDeclaration { .. } => self.generate_record(declaration)?,
                IrStatement::MethodDeclaration { .. }
                | IrStatement::VariableDeclaration { .. }
                | IrStatement::Block { .. }
                | IrStatement::If { .. }
                | IrStatement::While { .. }
                | IrStatement::ForEach { .. }
                | IrStatement::For { .. }
                | IrStatement::Switch { .. }
                | IrStatement::Try { .. }
                | IrStatement::TryWithResources { .. }
                | IrStatement::Expression { .. }
                | IrStatement::Return { .. }
                | IrStatement::Throw { .. }
                | IrStatement::Break { .. }
                | IrStatement::Continue { .. }
                | IrStatement::FieldDeclaration { .. }
                | IrStatement::Package { .. }
                | IrStatement::Import { .. } => self.generate_statement(declaration)?,
            };
            unit.type_declarations.push(code);
        }

        // Merge inferred imports from generator helpers.
        let mut inferred = self.imports.values().cloned().collect::<Vec<_>>();
        inferred.sort();
        inferred.dedup();
        unit.imports.extend(inferred);
        unit.imports.sort();
        unit.imports.dedup();

        Ok(unit)
    }

    /// Generate Java class declaration.
    pub fn generate_class(&mut self, class: &IrStatement) -> Result<String, CodeGenError> {
        if let IrStatement::ClassDeclaration {
            name,
            type_parameters,
            superclass,
            interfaces,
            fields,
            methods,
            nested_classes,
            modifiers,
            ..
        } = class
        {
            let mut builder = self.builder();
            self.render_annotations(&mut builder, modifiers);

            let mut header = String::new();
            let modifiers_str = self.generate_modifiers(modifiers);
            if !modifiers_str.is_empty() {
                header.push_str(&modifiers_str);
                header.push(' ');
            }
            header.push_str("class ");
            header.push_str(name);

            let generics = self.render_type_parameters(type_parameters)?;
            header.push_str(&generics);

            if let Some(super_ty) = superclass {
                header.push_str(" extends ");
                header.push_str(&self.generate_type(super_ty)?);
            }
            let implements = self.render_interface_clause("implements", interfaces)?;
            header.push_str(&implements);

            builder.push_line(&format!("{} {{", header.trim()));
            builder.indent();

            for field in fields {
                let field_code = self.generate_statement(field)?;
                Self::push_lines(&mut builder, &field_code);
            }

            if !fields.is_empty() && (!methods.is_empty() || !nested_classes.is_empty()) {
                builder.push_line("");
            }

            for method in methods {
                let method_code = self.generate_method(method)?;
                Self::push_lines(&mut builder, &method_code);
                builder.push_line("");
            }

            for nested in nested_classes {
                let nested_code = self.generate_class(nested)?;
                Self::push_lines(&mut builder, &nested_code);
                builder.push_line("");
            }

            builder.dedent();
            builder.push_line("}");

            Ok(builder.build())
        } else {
            Err(CodeGenError::UnsupportedConstruct {
                construct: "Expected class declaration".to_string(),
                span: None,
            })
        }
    }

    /// Generate Java record declaration (from jv data class).
    pub fn generate_record(&mut self, record: &IrStatement) -> Result<String, CodeGenError> {
        if let IrStatement::RecordDeclaration {
            name,
            type_parameters,
            components,
            interfaces,
            methods,
            modifiers,
            ..
        } = record
        {
            let mut builder = self.builder();
            self.render_annotations(&mut builder, modifiers);

            let mut header = String::new();
            let modifiers_str = self.generate_modifiers(modifiers);
            if !modifiers_str.is_empty() {
                header.push_str(&modifiers_str);
                header.push(' ');
            }
            header.push_str("record ");
            header.push_str(name);

            let generics = self.render_type_parameters(type_parameters)?;
            header.push_str(&generics);

            header.push('(');
            header.push_str(&self.render_record_components(components)?);
            header.push(')');

            let implements = self.render_interface_clause("implements", interfaces)?;
            header.push_str(&implements);

            if methods.is_empty() {
                header.push(';');
                builder.push_line(&header);
                return Ok(builder.build());
            }

            builder.push_line(&format!("{} {{", header));
            builder.indent();
            for method in methods {
                let method_code = self.generate_method(method)?;
                Self::push_lines(&mut builder, &method_code);
                builder.push_line("");
            }
            builder.dedent();
            builder.push_line("}");

            Ok(builder.build())
        } else {
            Err(CodeGenError::UnsupportedConstruct {
                construct: "Expected record declaration".to_string(),
                span: None,
            })
        }
    }

    /// Generate Java interface declaration.
    pub fn generate_interface(&mut self, interface: &IrStatement) -> Result<String, CodeGenError> {
        if let IrStatement::InterfaceDeclaration {
            name,
            type_parameters,
            superinterfaces,
            methods,
            default_methods,
            fields,
            nested_types,
            modifiers,
            ..
        } = interface
        {
            let mut builder = self.builder();
            self.render_annotations(&mut builder, modifiers);

            let mut header = String::new();
            let modifiers_str = self.generate_modifiers(modifiers);
            if !modifiers_str.is_empty() {
                header.push_str(&modifiers_str);
                header.push(' ');
            }
            header.push_str("interface ");
            header.push_str(name);

            let generics = self.render_type_parameters(type_parameters)?;
            header.push_str(&generics);

            let extends = self.render_interface_clause("extends", superinterfaces)?;
            header.push_str(&extends);

            builder.push_line(&format!("{} {{", header.trim()));
            builder.indent();

            for field in fields {
                let field_code = self.generate_statement(field)?;
                Self::push_lines(&mut builder, &field_code);
            }
            if !fields.is_empty() && (!methods.is_empty() || !default_methods.is_empty()) {
                builder.push_line("");
            }

            for method in methods {
                let method_code = self.generate_method(method)?;
                Self::push_lines(&mut builder, &method_code);
            }

            for method in default_methods {
                let method_code = self.generate_method(method)?;
                Self::push_lines(&mut builder, &method_code);
            }

            for nested in nested_types {
                let nested_code = match nested {
                    IrStatement::ClassDeclaration { .. } => self.generate_class(nested)?,
                    IrStatement::InterfaceDeclaration { .. } => self.generate_interface(nested)?,
                    IrStatement::RecordDeclaration { .. } => self.generate_record(nested)?,
                    _ => self.generate_statement(nested)?,
                };
                Self::push_lines(&mut builder, &nested_code);
                builder.push_line("");
            }

            builder.dedent();
            builder.push_line("}");

            Ok(builder.build())
        } else {
            Err(CodeGenError::UnsupportedConstruct {
                construct: "Expected interface declaration".to_string(),
                span: None,
            })
        }
    }

    /// Generate Java method declaration.
    pub fn generate_method(&mut self, method: &IrStatement) -> Result<String, CodeGenError> {
        if let IrStatement::MethodDeclaration {
            name,
            parameters,
            return_type,
            body,
            modifiers,
            throws,
            ..
        } = method
        {
            let mut builder = self.builder();
            self.render_annotations(&mut builder, modifiers);

            let mut signature = String::new();
            let modifiers_str = self.generate_modifiers(modifiers);
            if !modifiers_str.is_empty() {
                signature.push_str(&modifiers_str);
                signature.push(' ');
            }

            signature.push_str(&self.generate_type(return_type)?);
            signature.push(' ');
            signature.push_str(name);
            signature.push('(');
            signature.push_str(&self.render_parameters(parameters)?);
            signature.push(')');

            if !throws.is_empty() {
                signature.push_str(" throws ");
                signature.push_str(&throws.join(", "));
            }

            match body {
                Some(expr) => {
                    builder.push_line(&format!("{} {{", signature));
                    builder.indent();
                    self.write_method_body(&mut builder, expr, return_type)?;
                    builder.dedent();
                    builder.push_line("}");
                }
                None => builder.push_line(&format!("{};", signature)),
            }

            Ok(builder.build())
        } else {
            Err(CodeGenError::UnsupportedConstruct {
                construct: "Expected method declaration".to_string(),
                span: None,
            })
        }
    }

    /// Generate method overloads for default parameters.
    pub fn generate_method_overloads(
        &mut self,
        overloads: &[MethodOverload],
    ) -> Result<Vec<String>, CodeGenError> {
        let mut results = Vec::new();
        for overload in overloads {
            let mut builder = self.builder();
            let modifiers_str = self.generate_modifiers(&overload.modifiers);
            let mut signature = String::new();
            if !modifiers_str.is_empty() {
                signature.push_str(&modifiers_str);
                signature.push(' ');
            }
            signature.push_str(&self.generate_type(&overload.return_type)?);
            signature.push(' ');
            signature.push_str(&overload.name);
            signature.push('(');
            signature.push_str(&self.render_parameters(&overload.parameters)?);
            signature.push(')');
            builder.push_line(&format!("{} {{", signature));
            builder.indent();
            let body_expr = self.generate_expression(&overload.body)?;
            if matches!(overload.return_type, JavaType::Void) {
                builder.push_line(&format!("{};", body_expr));
            } else {
                builder.push_line(&format!("return {};", body_expr));
            }
            builder.dedent();
            builder.push_line("}");
            results.push(builder.build());
        }
        Ok(results)
    }

    /// Generate utility class for top-level functions.
    pub fn generate_utility_class(
        &mut self,
        utility_class: &UtilityClass,
    ) -> Result<String, CodeGenError> {
        let mut builder = self.builder();
        let mut header = String::new();
        let modifiers_str = self.generate_modifiers(&utility_class.modifiers);
        if !modifiers_str.is_empty() {
            header.push_str(&modifiers_str);
            header.push(' ');
        }
        header.push_str(&format!("class {}", utility_class.name));
        header.push_str(" {");
        builder.push_line(&header);
        builder.indent();
        for method in &utility_class.methods {
            let method_code = self.generate_method(method)?;
            Self::push_lines(&mut builder, &method_code);
            builder.push_line("");
        }
        builder.dedent();
        builder.push_line("}");
        Ok(builder.build())
    }

    /// Generate Java statement.
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

    /// Generate Java expression.
    pub fn generate_expression(&mut self, expr: &IrExpression) -> Result<String, CodeGenError> {
        match expr {
            IrExpression::Literal(literal, _) => Ok(Self::literal_to_string(literal)),
            IrExpression::Identifier { name, .. } => Ok(name.clone()),
            IrExpression::MethodCall {
                receiver,
                method_name,
                args,
                ..
            } => {
                let mut invocation = String::new();
                if let Some(target) = receiver {
                    invocation.push_str(&self.generate_expression(target)?);
                    invocation.push('.');
                }
                invocation.push_str(method_name);
                invocation.push('(');
                invocation.push_str(&self.render_arguments(args)?);
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
                ..
            } => {
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
                if body_str.trim_start().starts_with('{') {
                    Ok(format!("({}) -> {}", params, body_str))
                } else {
                    Ok(format!("({}) -> {}", params, body_str))
                }
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

    /// Generate Java type representation.
    pub fn generate_type(&self, java_type: &JavaType) -> Result<String, CodeGenError> {
        Ok(match java_type {
            JavaType::Primitive(name) => name.clone(),
            JavaType::Reference { name, generic_args } => {
                if generic_args.is_empty() {
                    name.clone()
                } else {
                    let mut rendered = Vec::new();
                    for arg in generic_args {
                        rendered.push(self.generate_type(arg)?);
                    }
                    format!("{}<{}>", name, rendered.join(", "))
                }
            }
            JavaType::Array {
                element_type,
                dimensions,
            } => {
                let base = self.generate_type(element_type)?;
                let suffix = "[]".repeat(*dimensions);
                format!("{}{}", base, suffix)
            }
            JavaType::Void => "void".to_string(),
            JavaType::Functional { interface_name, .. } => interface_name.clone(),
        })
    }

    /// Generate binary operator representation.
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
            BinaryOp::And => "&&".to_string(),
            BinaryOp::Or => "||".to_string(),
            BinaryOp::BitAnd => "&".to_string(),
            BinaryOp::BitOr => "|".to_string(),
            BinaryOp::BitXor => "^".to_string(),
            BinaryOp::PlusAssign => "+=".to_string(),
            BinaryOp::MinusAssign => "-=".to_string(),
            BinaryOp::MultiplyAssign => "*=".to_string(),
            BinaryOp::DivideAssign => "/=".to_string(),
            BinaryOp::Elvis => {
                return Err(CodeGenError::UnsupportedConstruct {
                    construct: "Elvis operator requires specialised lowering".to_string(),
                    span: None,
                })
            }
        })
    }

    /// Generate modifiers (public, static, final, etc.).
    pub fn generate_modifiers(&self, modifiers: &IrModifiers) -> String {
        let mut parts = Vec::new();
        match modifiers.visibility {
            IrVisibility::Public => parts.push("public"),
            IrVisibility::Protected => parts.push("protected"),
            IrVisibility::Private => parts.push("private"),
            IrVisibility::Package => {}
        }
        if modifiers.is_abstract {
            parts.push("abstract");
        }
        if modifiers.is_static {
            parts.push("static");
        }
        if modifiers.is_final {
            parts.push("final");
        }
        if modifiers.is_synchronized {
            parts.push("synchronized");
        }
        if modifiers.is_native {
            parts.push("native");
        }
        if modifiers.is_strictfp {
            parts.push("strictfp");
        }
        parts.join(" ")
    }

    /// Generate switch expression (Java 14+ pattern matching).
    pub fn generate_switch_expression(
        &mut self,
        switch: &IrExpression,
    ) -> Result<String, CodeGenError> {
        if let IrExpression::Switch {
            discriminant,
            cases,
            ..
        } = switch
        {
            let mut builder = self.builder();
            builder.push_line(&format!(
                "switch ({}) {{",
                self.generate_expression(discriminant)?
            ));
            builder.indent();
            for case in cases {
                let labels = self.render_case_labels(&case.labels)?;
                let guard = match &case.guard {
                    Some(guard_expr) => {
                        format!(" when ({})", self.generate_expression(guard_expr)?)
                    }
                    None => String::new(),
                };
                let body_expr = self.generate_expression(&case.body)?;
                if body_expr.trim_start().starts_with('{') {
                    builder.push_line(&format!("case {}{} -> {}", labels, guard, body_expr));
                } else {
                    builder.push_line(&format!("case {}{} -> {}", labels, guard, body_expr));
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

    /// Generate null-safe operations with proper null checks.
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

    /// Generate CompletableFuture operations (from async/await).
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

    /// Generate Virtual Thread operations (from spawn).
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

    /// Generate try-with-resources (from use blocks) as expression.
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

    fn render_try_with_resources_header(
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

    fn render_type_parameters(
        &self,
        type_parameters: &[IrTypeParameter],
    ) -> Result<String, CodeGenError> {
        if type_parameters.is_empty() {
            return Ok(String::new());
        }
        let mut parts = Vec::new();
        for param in type_parameters {
            let mut fragment = param.name.clone();
            if !param.bounds.is_empty() {
                let mut bounds_rendered = Vec::new();
                for bound in &param.bounds {
                    bounds_rendered.push(self.generate_type(bound)?);
                }
                fragment.push_str(" extends ");
                fragment.push_str(&bounds_rendered.join(" & "));
            }
            parts.push(fragment);
        }
        Ok(format!("<{}>", parts.join(", ")))
    }

    fn render_interface_clause(
        &self,
        prefix: &str,
        interfaces: &[JavaType],
    ) -> Result<String, CodeGenError> {
        if interfaces.is_empty() {
            return Ok(String::new());
        }
        let mut rendered = Vec::new();
        for interface in interfaces {
            rendered.push(self.generate_type(interface)?);
        }
        Ok(format!(" {} {}", prefix, rendered.join(", ")))
    }

    fn render_record_components(
        &self,
        components: &[IrRecordComponent],
    ) -> Result<String, CodeGenError> {
        let mut rendered = Vec::new();
        for component in components {
            rendered.push(format!(
                "{} {}",
                self.generate_type(&component.java_type)?,
                component.name
            ));
        }
        Ok(rendered.join(", "))
    }

    fn render_parameters(&self, params: &[IrParameter]) -> Result<String, CodeGenError> {
        let mut rendered = Vec::new();
        for param in params {
            let mut entry = String::new();
            let modifiers = self.generate_parameter_modifiers(&param.modifiers);
            if !modifiers.is_empty() {
                entry.push_str(&modifiers);
                entry.push(' ');
            }
            entry.push_str(&self.generate_type(&param.java_type)?);
            entry.push(' ');
            entry.push_str(&param.name);
            rendered.push(entry);
        }
        Ok(rendered.join(", "))
    }

    fn render_arguments(&mut self, args: &[IrExpression]) -> Result<String, CodeGenError> {
        let mut rendered = Vec::new();
        for arg in args {
            rendered.push(self.generate_expression(arg)?);
        }
        Ok(rendered.join(", "))
    }

    fn render_argument_vec(&mut self, args: &[IrExpression]) -> Result<Vec<String>, CodeGenError> {
        let mut rendered = Vec::new();
        for arg in args {
            rendered.push(self.generate_expression(arg)?);
        }
        Ok(rendered)
    }

    fn render_case_labels(&mut self, labels: &[IrCaseLabel]) -> Result<String, CodeGenError> {
        let mut rendered = Vec::new();
        for label in labels {
            match label {
                IrCaseLabel::Literal(literal) => rendered.push(Self::literal_to_string(literal)),
                IrCaseLabel::TypePattern {
                    type_name,
                    variable,
                } => rendered.push(format!("{} {}", type_name, variable)),
                IrCaseLabel::Range { .. } => {
                    return Err(CodeGenError::UnsupportedConstruct {
                        construct: "Range switch labels".to_string(),
                        span: None,
                    })
                }
                IrCaseLabel::Default => rendered.push("default".to_string()),
            }
        }
        Ok(rendered.join(", "))
    }

    fn write_method_body(
        &mut self,
        builder: &mut JavaSourceBuilder,
        body: &IrExpression,
        return_type: &JavaType,
    ) -> Result<(), CodeGenError> {
        if let IrExpression::Block { statements, .. } = body {
            for statement in statements {
                let stmt_code = self.generate_statement(statement)?;
                Self::push_lines(builder, &stmt_code);
            }
        } else {
            let expr_code = self.generate_expression(body)?;
            if matches!(return_type, JavaType::Void) {
                builder.push_line(&format!("{};", expr_code));
            } else {
                builder.push_line(&format!("return {};", expr_code));
            }
        }
        Ok(())
    }

    fn generate_parameter_modifiers(&self, modifiers: &IrModifiers) -> String {
        if modifiers.is_final {
            "final".to_string()
        } else {
            String::new()
        }
    }

    fn generate_local_modifiers(&self, is_final: bool, modifiers: &IrModifiers) -> String {
        if is_final || modifiers.is_final {
            "final".to_string()
        } else {
            String::new()
        }
    }

    fn render_annotations(&self, builder: &mut JavaSourceBuilder, modifiers: &IrModifiers) {
        for annotation in &modifiers.annotations {
            builder.push_line(&format!("@{}", annotation));
        }
    }

    fn builder(&self) -> JavaSourceBuilder {
        JavaSourceBuilder::new(self.config.indent.clone())
    }

    fn add_import(&mut self, import_path: &str) {
        self.imports
            .insert(import_path.to_string(), import_path.to_string());
    }

    fn reset(&mut self) {
        self.imports.clear();
    }

    fn literal_to_string(literal: &Literal) -> String {
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
        }
    }

    fn escape_string(value: &str) -> String {
        value
            .replace('\\', "\\\\")
            .replace('"', "\\\"")
            .replace('\n', "\\n")
            .replace('\r', "\\r")
            .replace('\t', "\\t")
    }

    fn push_lines(builder: &mut JavaSourceBuilder, text: &str) {
        for line in text.lines() {
            builder.push_line(line);
        }
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
