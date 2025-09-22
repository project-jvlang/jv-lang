use super::*;

impl JavaCodeGenerator {
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

    fn render_annotations(&self, builder: &mut JavaSourceBuilder, modifiers: &IrModifiers) {
        for annotation in &modifiers.annotations {
            builder.push_line(&format!("@{}", annotation));
        }
    }
}
