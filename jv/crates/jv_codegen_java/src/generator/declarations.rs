use super::*;
use jv_ast::types::{Kind, PrimitiveTypeName};
use jv_ir::{
    IrAnnotation, IrAnnotationArgument, IrAnnotationValue, IrGenericMetadata, IrTypeLevelValue,
    PrimitiveReturnMetadata,
};
use std::borrow::Cow;

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
        } = JavaCodeGenerator::base_statement(class)
        {
            let is_nested = !self.metadata_path.is_empty();
            self.metadata_path.push(name.clone());
            let metadata_entry = self.current_generic_metadata().cloned();

            let scope_len = self.variance_scope_len();
            self.push_variance_scope(type_parameters);

            let result = (|| -> Result<String, CodeGenError> {
                let mut builder = self.builder();
                let modifiers_view = Self::normalize_type_modifiers(modifiers, is_nested);
                let modifiers = modifiers_view.as_ref();

                self.render_annotations(&mut builder, modifiers);
                self.emit_generic_metadata_comment(
                    &mut builder,
                    type_parameters,
                    metadata_entry.as_ref(),
                );

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

                let mut fallback_comment = None;
                if modifiers.is_sealed {
                    if let Some(clause) = self.targeting.permits_clause(&modifiers.permitted_types)
                    {
                        header.push_str(&clause);
                    } else {
                        fallback_comment = self
                            .targeting
                            .sealed_fallback_comment(&modifiers.permitted_types);
                    }
                }

                builder.push_line(&format!("{} {{", header.trim()));
                builder.indent();

                if let Some(comment) = fallback_comment {
                    builder.push_line(&comment);
                }

                let extension_methods = self.take_instance_extension_methods(name);

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

                for method in extension_methods {
                    let method_code = self.generate_instance_method_from_extension(&method)?;
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
            })();

            self.truncate_variance_scopes(scope_len);
            self.metadata_path.pop();
            result
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
        } = JavaCodeGenerator::base_statement(record)
        {
            let is_nested = !self.metadata_path.is_empty();
            self.metadata_path.push(name.clone());
            let metadata_entry = self.current_generic_metadata().cloned();

            let scope_len = self.variance_scope_len();
            self.push_variance_scope(type_parameters);

            let result = (|| -> Result<String, CodeGenError> {
                let mut builder = self.builder();
                let modifiers_view = Self::normalize_type_modifiers(modifiers, is_nested);
                let modifiers = modifiers_view.as_ref();

                self.render_annotations(&mut builder, modifiers);
                self.emit_generic_metadata_comment(
                    &mut builder,
                    type_parameters,
                    metadata_entry.as_ref(),
                );

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

                let extension_methods = self.take_instance_extension_methods(name);

                if methods.is_empty() && extension_methods.is_empty() {
                    builder.push_line(&format!("{} {{}}", header));
                    return Ok(builder.build());
                }

                builder.push_line(&format!("{} {{", header));
                builder.indent();
                for method in methods {
                    let method_code = self.generate_method(method)?;
                    Self::push_lines(&mut builder, &method_code);
                    builder.push_line("");
                }
                for method in extension_methods {
                    let method_code = self.generate_instance_method_from_extension(&method)?;
                    Self::push_lines(&mut builder, &method_code);
                    builder.push_line("");
                }
                builder.dedent();
                builder.push_line("}");

                Ok(builder.build())
            })();

            self.truncate_variance_scopes(scope_len);
            self.metadata_path.pop();
            result
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
        } = JavaCodeGenerator::base_statement(interface)
        {
            let is_nested = !self.metadata_path.is_empty();
            self.metadata_path.push(name.clone());
            let metadata_entry = self.current_generic_metadata().cloned();

            let scope_len = self.variance_scope_len();
            self.push_variance_scope(type_parameters);

            let result = (|| -> Result<String, CodeGenError> {
                let mut builder = self.builder();
                let modifiers_view = Self::normalize_type_modifiers(modifiers, is_nested);
                let modifiers = modifiers_view.as_ref();

                self.render_annotations(&mut builder, modifiers);
                self.emit_generic_metadata_comment(
                    &mut builder,
                    type_parameters,
                    metadata_entry.as_ref(),
                );

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

                let mut fallback_comment = None;
                if modifiers.is_sealed {
                    if let Some(clause) = self.targeting.permits_clause(&modifiers.permitted_types)
                    {
                        header.push_str(&clause);
                    } else {
                        fallback_comment = self
                            .targeting
                            .sealed_fallback_comment(&modifiers.permitted_types);
                    }
                }

                builder.push_line(&format!("{} {{", header.trim()));
                builder.indent();

                if let Some(comment) = fallback_comment {
                    builder.push_line(&comment);
                }

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
                    let nested_code = match JavaCodeGenerator::base_statement(nested) {
                        IrStatement::ClassDeclaration { .. } => self.generate_class(nested)?,
                        IrStatement::InterfaceDeclaration { .. } => {
                            self.generate_interface(nested)?
                        }
                        IrStatement::RecordDeclaration { .. } => self.generate_record(nested)?,
                        _ => self.generate_statement(nested)?,
                    };
                    Self::push_lines(&mut builder, &nested_code);
                    builder.push_line("");
                }

                builder.dedent();
                builder.push_line("}");

                Ok(builder.build())
            })();

            self.truncate_variance_scopes(scope_len);
            self.metadata_path.pop();
            result
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
            java_name,
            type_parameters,
            parameters,
            primitive_return,
            return_type,
            body,
            modifiers,
            throws,
            ..
        } = JavaCodeGenerator::base_statement(method)
        {
            let emitted_name = if let Some(explicit) = java_name.clone() {
                explicit
            } else if let Some(metadata) = primitive_return.as_ref() {
                Self::specialized_method_name(name, metadata)
            } else {
                name.clone()
            };
            let metadata_depth = self.metadata_path.len();
            self.metadata_path.push(emitted_name.clone());
            let scope_len = self.variance_scope_len();
            let effective_type_parameters = if modifiers.is_static {
                type_parameters.clone()
            } else {
                self.filter_shadowed_type_parameters(type_parameters)
            };
            self.push_variance_scope(&effective_type_parameters);

            let result = (|| -> Result<String, CodeGenError> {
                let normalized_return_type = Self::normalize_void_like(return_type);
                let mut builder = self.builder();
                self.render_annotations(&mut builder, modifiers);

                let mut signature = String::new();
                let modifiers_str = self.generate_modifiers(modifiers);
                if !modifiers_str.is_empty() {
                    signature.push_str(&modifiers_str);
                    signature.push(' ');
                }

                if !effective_type_parameters.is_empty() {
                    let generics = self.render_type_parameters(&effective_type_parameters)?;
                    signature.push_str(&generics);
                    signature.push(' ');
                }

                signature.push_str(&self.generate_type(normalized_return_type.as_ref())?);
                signature.push(' ');
                signature.push_str(&emitted_name);
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
                        self.write_method_body(
                            &mut builder,
                            expr,
                            normalized_return_type.as_ref(),
                        )?;
                        builder.dedent();
                        builder.push_line("}");
                    }
                    None => builder.push_line(&format!("{};", signature)),
                }

                Ok(builder.build())
            })();

            self.truncate_variance_scopes(scope_len);
            self.metadata_path.truncate(metadata_depth);

            result
        } else {
            Err(CodeGenError::UnsupportedConstruct {
                construct: "Expected method declaration".to_string(),
                span: None,
            })
        }
    }

    pub fn generate_instance_method_from_extension(
        &mut self,
        method: &IrStatement,
    ) -> Result<String, CodeGenError> {
        match method {
            IrStatement::Commented {
                statement, comment, ..
            } => {
                let rendered = self.generate_instance_method_from_extension(statement)?;
                Ok(Self::append_inline_comment(rendered, comment))
            }
            IrStatement::MethodDeclaration {
                name,
                java_name,
                type_parameters,
                parameters,
                primitive_return,
                return_type,
                body,
                modifiers,
                throws,
                span,
                assertion_patterns,
            } => {
                if parameters.is_empty() {
                    return Err(CodeGenError::UnsupportedConstruct {
                        construct: format!(
                            "Extension method {} must have a receiver parameter",
                            name
                        ),
                        span: Some(span.clone()),
                    });
                }

                let receiver_type = parameters[0].java_type.clone();
                let mut instance_modifiers = modifiers.clone();
                instance_modifiers.is_static = false;

                let instance_parameters = parameters[1..].to_vec();
                let instance_body = body
                    .as_ref()
                    .map(|expr| replace_receiver_expression(expr.clone(), &receiver_type));

                let instance_method = IrStatement::MethodDeclaration {
                    name: name.clone(),
                    java_name: java_name.clone(),
                    type_parameters: type_parameters.clone(),
                    parameters: instance_parameters,
                    primitive_return: primitive_return.clone(),
                    return_type: return_type.clone(),
                    body: instance_body,
                    modifiers: instance_modifiers,
                    throws: throws.clone(),
                    span: span.clone(),
                    assertion_patterns: assertion_patterns.clone(),
                };

                self.generate_method(&instance_method)
            }
            _ => Err(CodeGenError::UnsupportedConstruct {
                construct: "Expected method declaration for extension".to_string(),
                span: None,
            }),
        }
    }

    pub fn generate_method_overloads(
        &mut self,
        overloads: &[MethodOverload],
    ) -> Result<Vec<String>, CodeGenError> {
        let mut results = Vec::new();
        for overload in overloads {
            let normalized_return_type = Self::normalize_void_like(&overload.return_type);
            let mut builder = self.builder();
            let modifiers_str = self.generate_modifiers(&overload.modifiers);
            let mut signature = String::new();
            if !modifiers_str.is_empty() {
                signature.push_str(&modifiers_str);
                signature.push(' ');
            }
            signature.push_str(&self.generate_type(normalized_return_type.as_ref())?);
            signature.push(' ');
            signature.push_str(&overload.name);
            signature.push('(');
            signature.push_str(&self.render_parameters(&overload.parameters)?);
            signature.push(')');
            builder.push_line(&format!("{} {{", signature));
            builder.indent();
            let body_expr = self.generate_expression(&overload.body)?;
            if Self::is_void_like(normalized_return_type.as_ref()) {
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
            let kind_hint = param.kind.clone().or_else(|| {
                self.current_generic_metadata()
                    .and_then(|entry| entry.type_parameter_kinds.get(&param.name).cloned())
            });
            if let Some(kind) = kind_hint {
                fragment.push_str(" /* kind: ");
                fragment.push_str(&self.render_kind(&kind));
                fragment.push_str(" */");
            }
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

    fn emit_generic_metadata_comment(
        &self,
        builder: &mut JavaSourceBuilder,
        type_parameters: &[IrTypeParameter],
        metadata: Option<&IrGenericMetadata>,
    ) {
        let mut lines: Vec<String> = Vec::new();

        for param in type_parameters {
            let kind_hint = param.kind.clone().or_else(|| {
                metadata.and_then(|entry| entry.type_parameter_kinds.get(&param.name).cloned())
            });
            if let Some(kind) = kind_hint {
                lines.push(format!(
                    "type parameter {} kind = {}",
                    param.name,
                    self.render_kind(&kind)
                ));
            }
        }

        if let Some(entry) = metadata {
            for (name, value) in &entry.const_parameter_values {
                lines.push(format!(
                    "const {} = {}",
                    name,
                    self.render_type_level_value(value)
                ));
            }
            for (slot, value) in &entry.type_level_bindings {
                lines.push(format!(
                    "type-level {} = {}",
                    slot,
                    self.render_type_level_value(value)
                ));
            }
        }

        if lines.is_empty() {
            return;
        }

        builder.push_line("/**");
        builder.push_line(" * JV Generic Metadata");
        for line in lines {
            builder.push_line(&format!(" * - {}", line));
        }
        builder.push_line(" */");
    }

    fn render_kind(&self, kind: &Kind) -> String {
        match kind {
            Kind::Star => "*".to_string(),
            Kind::Arrow { parameter, result } => format!(
                "{} -> {}",
                self.render_kind(parameter),
                self.render_kind(result)
            ),
            Kind::Higher { parameters, result } => {
                let rendered_params: Vec<String> =
                    parameters.iter().map(|k| self.render_kind(k)).collect();
                format!(
                    "({}) -> {}",
                    rendered_params.join(", "),
                    self.render_kind(result)
                )
            }
            Kind::Constraint { base, constraints } => {
                let mut rendered = self.render_kind(base);
                if !constraints.is_empty() {
                    let constraint_names: Vec<String> = constraints
                        .iter()
                        .map(|constraint| constraint.trait_name.qualified())
                        .collect();
                    rendered.push_str(" with ");
                    rendered.push_str(&constraint_names.join(", "));
                }
                rendered
            }
        }
    }

    fn render_type_level_value(&self, value: &IrTypeLevelValue) -> String {
        match value {
            IrTypeLevelValue::Int(v) => v.to_string(),
            IrTypeLevelValue::Bool(v) => v.to_string(),
            IrTypeLevelValue::String(v) => format!("\"{}\"", v),
        }
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

    fn specialized_method_name(base_name: &str, metadata: &PrimitiveReturnMetadata) -> String {
        let suffix = match metadata.reference.primitive {
            PrimitiveTypeName::Int => "IntVersion",
            PrimitiveTypeName::Long => "LongVersion",
            PrimitiveTypeName::Short => "ShortVersion",
            PrimitiveTypeName::Byte => "ByteVersion",
            PrimitiveTypeName::Float => "FloatVersion",
            PrimitiveTypeName::Double => "DoubleVersion",
            PrimitiveTypeName::Boolean => "BooleanVersion",
            PrimitiveTypeName::Char => "CharVersion",
        };
        format!("{base_name}${suffix}")
    }

    fn write_method_body(
        &mut self,
        builder: &mut JavaSourceBuilder,
        body: &IrExpression,
        return_type: &JavaType,
    ) -> Result<(), CodeGenError> {
        let previous_return = self.current_return_type.clone();
        let previous_captures = std::mem::take(&mut self.mutable_captures);
        self.current_return_type = Some(return_type.clone());
        self.mutable_captures = self.analyze_mutable_captures(body);
        if let IrExpression::Block { statements, .. } = body {
            for statement in statements {
                let stmt_code = self.generate_statement(statement)?;
                Self::push_lines(builder, &stmt_code);
            }
        } else {
            let expr_code = self.generate_expression(body)?;
            if Self::is_void_like(return_type) {
                builder.push_line(&format!("{};", expr_code));
            } else {
                let coerced = self.coerce_return_expression(body, expr_code, return_type)?;
                builder.push_line(&format!("return {};", coerced));
            }
        }

        self.current_return_type = previous_return;
        self.mutable_captures = previous_captures;
        Ok(())
    }

    fn render_annotations(&mut self, builder: &mut JavaSourceBuilder, modifiers: &IrModifiers) {
        for annotation in &modifiers.annotations {
            self.register_annotation_imports(annotation);
            builder.push_line(&self.format_annotation(annotation));
        }
    }

    fn format_annotation(&self, annotation: &IrAnnotation) -> String {
        let mut rendered = String::new();
        rendered.push('@');
        let qualified = annotation.name.qualified_name();
        let name_to_use = if annotation.name.segments.len() > 1 {
            annotation.name.simple_name().to_string()
        } else {
            qualified.clone()
        };
        rendered.push_str(&name_to_use);
        if !annotation.arguments.is_empty() {
            rendered.push('(');
            let args = annotation
                .arguments
                .iter()
                .map(|arg| self.format_annotation_argument(arg))
                .collect::<Vec<_>>()
                .join(", ");
            rendered.push_str(&args);
            rendered.push(')');
        }
        rendered
    }

    fn format_annotation_argument(&self, argument: &IrAnnotationArgument) -> String {
        match argument {
            IrAnnotationArgument::Positional(value) => self.format_annotation_value(value),
            IrAnnotationArgument::Named { name, value } => {
                format!("{} = {}", name, self.format_annotation_value(value))
            }
        }
    }

    fn format_annotation_value(&self, value: &IrAnnotationValue) -> String {
        match value {
            IrAnnotationValue::Literal(literal) => Self::literal_to_string(literal),
            IrAnnotationValue::EnumConstant {
                type_name,
                constant,
            } => {
                if type_name.is_empty() {
                    constant.clone()
                } else {
                    format!("{}.{constant}", type_name)
                }
            }
            IrAnnotationValue::Array(values) => {
                let joined = values
                    .iter()
                    .map(|value| self.format_annotation_value(value))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{{{joined}}}")
            }
            IrAnnotationValue::ClassLiteral(type_name) => format!("{}.class", type_name),
            IrAnnotationValue::Nested(annotation) => self.format_annotation(annotation),
        }
    }

    fn register_annotation_imports(&mut self, annotation: &IrAnnotation) {
        let (package, _) = annotation.name.split_package();
        if package.is_some() {
            let qualified_name = annotation.name.qualified_name();
            self.add_import(&qualified_name);
        }

        for argument in &annotation.arguments {
            match argument {
                IrAnnotationArgument::Positional(value) => {
                    self.register_annotation_value_imports(value)
                }
                IrAnnotationArgument::Named { value, .. } => {
                    self.register_annotation_value_imports(value)
                }
            }
        }
    }

    fn register_annotation_value_imports(&mut self, value: &IrAnnotationValue) {
        match value {
            IrAnnotationValue::Array(values) => {
                for value in values {
                    self.register_annotation_value_imports(value);
                }
            }
            IrAnnotationValue::Nested(annotation) => {
                self.register_annotation_imports(annotation);
            }
            _ => {}
        }
    }

    fn normalize_type_modifiers<'a>(
        modifiers: &'a IrModifiers,
        is_nested: bool,
    ) -> Cow<'a, IrModifiers> {
        if is_nested {
            return Cow::Borrowed(modifiers);
        }

        match modifiers.visibility {
            IrVisibility::Private | IrVisibility::Protected => {
                let mut adjusted = modifiers.clone();
                adjusted.visibility = IrVisibility::Package;
                Cow::Owned(adjusted)
            }
            _ => Cow::Borrowed(modifiers),
        }
    }

    // === Declaration Helpers (moved from helpers.rs) ===

    /// Generate Java modifiers string from IR modifiers.
    ///
    /// Handles visibility (public/protected/private), abstract, sealed, static,
    /// final, synchronized, native, strictfp.
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
        if modifiers.is_sealed {
            if self.targeting.supports_sealed_types() {
                parts.push("sealed");
            } else if !modifiers.is_final {
                parts.push("final");
            }
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

    /// Generate Java parameter modifiers (currently only 'final').
    pub fn generate_parameter_modifiers(&self, modifiers: &IrModifiers) -> String {
        if modifiers.is_final {
            "final".to_string()
        } else {
            String::new()
        }
    }
}

fn replace_receiver_expression(expr: IrExpression, receiver_type: &JavaType) -> IrExpression {
    match expr {
        IrExpression::Identifier { name, span, .. } if name == "receiver" => IrExpression::This {
            java_type: receiver_type.clone(),
            span,
        },
        IrExpression::MethodCall {
            receiver,
            method_name,
            java_name,
            resolved_target,
            args,
            argument_style,
            java_type,
            span,
        } => IrExpression::MethodCall {
            receiver: receiver
                .map(|inner| Box::new(replace_receiver_expression(*inner, receiver_type))),
            method_name,
            java_name,
            resolved_target,
            args: args
                .into_iter()
                .map(|arg| replace_receiver_expression(arg, receiver_type))
                .collect(),
            argument_style,
            java_type,
            span,
        },
        IrExpression::FieldAccess {
            receiver,
            field_name,
            java_type,
            span,
            is_record_component,
        } => IrExpression::FieldAccess {
            receiver: Box::new(replace_receiver_expression(*receiver, receiver_type)),
            field_name,
            java_type,
            span,
            is_record_component,
        },
        IrExpression::ArrayAccess {
            array,
            index,
            java_type,
            span,
        } => IrExpression::ArrayAccess {
            array: Box::new(replace_receiver_expression(*array, receiver_type)),
            index: Box::new(replace_receiver_expression(*index, receiver_type)),
            java_type,
            span,
        },
        IrExpression::Binary {
            left,
            op,
            right,
            java_type,
            span,
        } => IrExpression::Binary {
            left: Box::new(replace_receiver_expression(*left, receiver_type)),
            op,
            right: Box::new(replace_receiver_expression(*right, receiver_type)),
            java_type,
            span,
        },
        IrExpression::Unary {
            op,
            operand,
            java_type,
            span,
        } => IrExpression::Unary {
            op,
            operand: Box::new(replace_receiver_expression(*operand, receiver_type)),
            java_type,
            span,
        },
        IrExpression::Assignment {
            target,
            value,
            java_type,
            span,
        } => IrExpression::Assignment {
            target: Box::new(replace_receiver_expression(*target, receiver_type)),
            value: Box::new(replace_receiver_expression(*value, receiver_type)),
            java_type,
            span,
        },
        IrExpression::Conditional {
            condition,
            then_expr,
            else_expr,
            java_type,
            span,
        } => IrExpression::Conditional {
            condition: Box::new(replace_receiver_expression(*condition, receiver_type)),
            then_expr: Box::new(replace_receiver_expression(*then_expr, receiver_type)),
            else_expr: Box::new(replace_receiver_expression(*else_expr, receiver_type)),
            java_type,
            span,
        },
        IrExpression::Block {
            statements,
            java_type,
            span,
        } => IrExpression::Block {
            statements: statements
                .into_iter()
                .map(|stmt| replace_receiver_in_statement(stmt, receiver_type))
                .collect(),
            java_type,
            span,
        },
        IrExpression::ArrayCreation {
            element_type,
            dimensions,
            initializer,
            delimiter,
            span,
        } => IrExpression::ArrayCreation {
            element_type,
            dimensions: dimensions
                .into_iter()
                .map(|dim| dim.map(|expr| replace_receiver_expression(expr, receiver_type)))
                .collect(),
            initializer: initializer.map(|values| {
                values
                    .into_iter()
                    .map(|expr| replace_receiver_expression(expr, receiver_type))
                    .collect()
            }),
            delimiter,
            span,
        },
        IrExpression::ObjectCreation {
            class_name,
            generic_args,
            args,
            java_type,
            span,
        } => IrExpression::ObjectCreation {
            class_name,
            generic_args,
            args: args
                .into_iter()
                .map(|expr| replace_receiver_expression(expr, receiver_type))
                .collect(),
            java_type,
            span,
        },
        IrExpression::Lambda {
            functional_interface,
            param_names,
            param_types,
            body,
            java_type,
            span,
        } => IrExpression::Lambda {
            functional_interface,
            param_names,
            param_types,
            body: Box::new(replace_receiver_expression(*body, receiver_type)),
            java_type,
            span,
        },
        IrExpression::Switch {
            discriminant,
            cases,
            java_type,
            implicit_end,
            span,
            strategy_description,
        } => IrExpression::Switch {
            discriminant: Box::new(replace_receiver_expression(*discriminant, receiver_type)),
            cases: cases
                .into_iter()
                .map(|case| IrSwitchCase {
                    labels: case.labels,
                    guard: case
                        .guard
                        .map(|expr| replace_receiver_expression(expr, receiver_type)),
                    body: replace_receiver_expression(case.body, receiver_type),
                    span: case.span,
                })
                .collect(),
            java_type,
            implicit_end,
            strategy_description,
            span,
        },
        IrExpression::Cast {
            expr,
            target_type,
            span,
        } => IrExpression::Cast {
            expr: Box::new(replace_receiver_expression(*expr, receiver_type)),
            target_type,
            span,
        },
        IrExpression::InstanceOf {
            expr,
            target_type,
            span,
        } => IrExpression::InstanceOf {
            expr: Box::new(replace_receiver_expression(*expr, receiver_type)),
            target_type,
            span,
        },
        IrExpression::NullSafeOperation {
            expr,
            operation,
            default_value,
            java_type,
            span,
        } => IrExpression::NullSafeOperation {
            expr: Box::new(replace_receiver_expression(*expr, receiver_type)),
            operation: Box::new(replace_receiver_expression(*operation, receiver_type)),
            default_value: default_value
                .map(|expr| Box::new(replace_receiver_expression(*expr, receiver_type))),
            java_type,
            span,
        },
        IrExpression::StringFormat {
            format_string,
            args,
            span,
        } => IrExpression::StringFormat {
            format_string,
            args: args
                .into_iter()
                .map(|expr| replace_receiver_expression(expr, receiver_type))
                .collect(),
            span,
        },
        IrExpression::CompletableFuture {
            operation,
            args,
            java_type,
            span,
        } => IrExpression::CompletableFuture {
            operation,
            args: args
                .into_iter()
                .map(|expr| replace_receiver_expression(expr, receiver_type))
                .collect(),
            java_type,
            span,
        },
        IrExpression::VirtualThread {
            operation,
            args,
            java_type,
            span,
        } => IrExpression::VirtualThread {
            operation,
            args: args
                .into_iter()
                .map(|expr| replace_receiver_expression(expr, receiver_type))
                .collect(),
            java_type,
            span,
        },
        IrExpression::TryWithResources {
            resources,
            body,
            java_type,
            span,
        } => IrExpression::TryWithResources {
            resources: resources
                .into_iter()
                .map(|res| IrResource {
                    name: res.name,
                    initializer: replace_receiver_expression(res.initializer, receiver_type),
                    java_type: res.java_type,
                    span: res.span,
                })
                .collect(),
            body: Box::new(replace_receiver_expression(*body, receiver_type)),
            java_type,
            span,
        },
        other => other,
    }
}

fn replace_receiver_in_statement(stmt: IrStatement, receiver_type: &JavaType) -> IrStatement {
    match stmt {
        IrStatement::Expression { expr, span } => IrStatement::Expression {
            expr: replace_receiver_expression(expr, receiver_type),
            span,
        },
        IrStatement::Return { value, span } => IrStatement::Return {
            value: value.map(|expr| replace_receiver_expression(expr, receiver_type)),
            span,
        },
        IrStatement::VariableDeclaration {
            name,
            java_type,
            initializer,
            is_final,
            modifiers,
            span,
        } => IrStatement::VariableDeclaration {
            name,
            java_type,
            initializer: initializer.map(|expr| replace_receiver_expression(expr, receiver_type)),
            is_final,
            modifiers,
            span,
        },
        IrStatement::FieldDeclaration {
            name,
            java_type,
            initializer,
            modifiers,
            span,
        } => IrStatement::FieldDeclaration {
            name,
            java_type,
            initializer: initializer.map(|expr| replace_receiver_expression(expr, receiver_type)),
            modifiers,
            span,
        },
        IrStatement::Block { statements, span } => IrStatement::Block {
            statements: statements
                .into_iter()
                .map(|inner| replace_receiver_in_statement(inner, receiver_type))
                .collect(),
            span,
        },
        IrStatement::If {
            condition,
            then_stmt,
            else_stmt,
            span,
        } => IrStatement::If {
            condition: replace_receiver_expression(condition, receiver_type),
            then_stmt: Box::new(replace_receiver_in_statement(*then_stmt, receiver_type)),
            else_stmt: else_stmt
                .map(|stmt| Box::new(replace_receiver_in_statement(*stmt, receiver_type))),
            span,
        },
        IrStatement::While {
            condition,
            body,
            span,
        } => IrStatement::While {
            condition: replace_receiver_expression(condition, receiver_type),
            body: Box::new(replace_receiver_in_statement(*body, receiver_type)),
            span,
        },
        IrStatement::ForEach {
            variable,
            variable_type,
            iterable,
            body,
            iterable_kind,
            span,
        } => IrStatement::ForEach {
            variable,
            variable_type,
            iterable: replace_receiver_expression(iterable, receiver_type),
            body: Box::new(replace_receiver_in_statement(*body, receiver_type)),
            iterable_kind,
            span,
        },
        IrStatement::For {
            init,
            condition,
            update,
            body,
            metadata,
            span,
        } => IrStatement::For {
            init: init.map(|stmt| Box::new(replace_receiver_in_statement(*stmt, receiver_type))),
            condition: condition.map(|expr| replace_receiver_expression(expr, receiver_type)),
            update: update.map(|expr| replace_receiver_expression(expr, receiver_type)),
            body: Box::new(replace_receiver_in_statement(*body, receiver_type)),
            metadata,
            span,
        },
        IrStatement::Switch {
            discriminant,
            cases,
            span,
        } => IrStatement::Switch {
            discriminant: replace_receiver_expression(discriminant, receiver_type),
            cases: cases
                .into_iter()
                .map(|case| IrSwitchCase {
                    labels: case.labels,
                    guard: case
                        .guard
                        .map(|expr| replace_receiver_expression(expr, receiver_type)),
                    body: replace_receiver_expression(case.body, receiver_type),
                    span: case.span,
                })
                .collect(),
            span,
        },
        IrStatement::Try {
            body,
            catch_clauses,
            finally_block,
            span,
        } => IrStatement::Try {
            body: Box::new(replace_receiver_in_statement(*body, receiver_type)),
            catch_clauses: catch_clauses
                .into_iter()
                .map(|clause| IrCatchClause {
                    exception_type: clause.exception_type,
                    variable_name: clause.variable_name,
                    body: replace_receiver_in_statement(clause.body, receiver_type),
                    span: clause.span,
                })
                .collect(),
            finally_block: finally_block
                .map(|stmt| Box::new(replace_receiver_in_statement(*stmt, receiver_type))),
            span,
        },
        IrStatement::TryWithResources {
            resources,
            body,
            catch_clauses,
            finally_block,
            span,
        } => IrStatement::TryWithResources {
            resources: resources
                .into_iter()
                .map(|res| IrResource {
                    name: res.name,
                    initializer: replace_receiver_expression(res.initializer, receiver_type),
                    java_type: res.java_type,
                    span: res.span,
                })
                .collect(),
            body: Box::new(replace_receiver_in_statement(*body, receiver_type)),
            catch_clauses: catch_clauses
                .into_iter()
                .map(|clause| IrCatchClause {
                    exception_type: clause.exception_type,
                    variable_name: clause.variable_name,
                    body: replace_receiver_in_statement(clause.body, receiver_type),
                    span: clause.span,
                })
                .collect(),
            finally_block: finally_block
                .map(|stmt| Box::new(replace_receiver_in_statement(*stmt, receiver_type))),
            span,
        },
        IrStatement::Throw { expr, span } => IrStatement::Throw {
            expr: replace_receiver_expression(expr, receiver_type),
            span,
        },
        IrStatement::Commented {
            statement,
            comment,
            kind,
            comment_span,
        } => IrStatement::Commented {
            statement: Box::new(replace_receiver_in_statement(*statement, receiver_type)),
            comment,
            kind,
            comment_span,
        },
        _ => stmt,
    }
}
