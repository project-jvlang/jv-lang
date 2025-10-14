use crate::builder::{JavaCompilationUnit, JavaSourceBuilder};
use crate::config::JavaCodeGenConfig;
use crate::error::CodeGenError;
use crate::target_version::TargetedJavaEmitter;
use jv_ast::{BinaryOp, CallArgumentStyle, Literal, SequenceDelimiter, UnaryOp};
use jv_ir::{
    CompletableFutureOp, IrCaseLabel, IrCatchClause, IrDeconstructionComponent,
    IrDeconstructionPattern, IrExpression, IrForEachKind, IrForLoopMetadata, IrGenericMetadata,
    IrImplicitWhenEnd, IrModifiers, IrNumericRangeLoop, IrParameter, IrProgram, IrRecordComponent,
    IrResource, IrSampleDeclaration, IrStatement, IrSwitchCase, IrTypeParameter, IrVariance,
    IrVisibility, JavaType, MethodOverload, UtilityClass, VirtualThreadOp,
};
use std::collections::{BTreeMap, HashMap};

mod declarations;
mod expressions;
mod formatting;
mod sample;
mod statements;
mod types;

pub use types::ErasurePlan;

pub struct JavaCodeGenerator {
    imports: HashMap<String, String>,
    config: JavaCodeGenConfig,
    targeting: TargetedJavaEmitter,
    variance_stack: Vec<HashMap<String, IrVariance>>,
    sequence_helper: Option<String>,
    generic_metadata: BTreeMap<String, IrGenericMetadata>,
    metadata_path: Vec<String>,
    package: Option<String>,
}

impl JavaCodeGenerator {
    pub fn new() -> Self {
        Self::with_config(JavaCodeGenConfig::default())
    }

    pub fn with_config(config: JavaCodeGenConfig) -> Self {
        let target = config.target;
        Self {
            imports: HashMap::new(),
            config,
            targeting: TargetedJavaEmitter::new(target),
            variance_stack: Vec::new(),
            sequence_helper: None,
            generic_metadata: BTreeMap::new(),
            metadata_path: Vec::new(),
            package: None,
        }
    }

    pub fn generate_compilation_unit(
        &mut self,
        program: &IrProgram,
    ) -> Result<JavaCompilationUnit, CodeGenError> {
        self.reset();
        self.package = program.package.clone();
        self.generic_metadata = program.generic_metadata.clone();
        self.metadata_path.clear();

        let mut unit = JavaCompilationUnit::new();
        unit.package_declaration = program.package.clone();

        for import in &program.imports {
            if let IrStatement::Import {
                path,
                is_static,
                is_wildcard,
                ..
            } = Self::base_statement(import)
            {
                let mut entry = String::new();
                if *is_static {
                    entry.push_str("static ");
                }
                entry.push_str(path);
                if *is_wildcard && !path.ends_with(".*") {
                    entry.push_str(".*");
                }
                unit.imports.push(entry);
            }
        }

        let mut script_statements = Vec::new();
        let mut script_methods = Vec::new();
        let mut remaining_declarations = Vec::new();

        for declaration in &program.type_declarations {
            match Self::base_statement(declaration) {
                IrStatement::ClassDeclaration { .. }
                | IrStatement::InterfaceDeclaration { .. }
                | IrStatement::RecordDeclaration { .. }
                | IrStatement::SampleDeclaration(_) => {
                    remaining_declarations.push(declaration.clone());
                }
                IrStatement::MethodDeclaration { .. } => {
                    script_methods.push(declaration.clone());
                }
                _ => {
                    script_statements.push(declaration.clone());
                }
            }
        }

        let mut hoisted_regex_fields = Vec::new();
        let mut retained_statements = Vec::new();
        for statement in script_statements.drain(..) {
            if let Some(field) = Self::hoist_regex_pattern_field(&statement) {
                hoisted_regex_fields.push(field);
            } else {
                retained_statements.push(statement);
            }
        }
        script_statements = retained_statements;

        let has_entry_method = script_methods.iter().any(Self::is_entry_point_method);
        let needs_wrapper = !script_statements.is_empty() || !has_entry_method;

        if !script_statements.is_empty()
            || !script_methods.is_empty()
            || !hoisted_regex_fields.is_empty()
        {
            let script_class = &self.config.script_main_class;
            let mut builder = self.builder();
            builder.push_line(&format!("public final class {} {{", script_class));
            builder.indent();

            if !hoisted_regex_fields.is_empty() {
                for field in &hoisted_regex_fields {
                    let code = self.generate_statement(field)?;
                    Self::push_lines(&mut builder, &code);
                }

                if needs_wrapper || !script_methods.is_empty() {
                    builder.push_line("");
                }
            }

            if needs_wrapper {
                builder.push_line("public static void main(String[] args) throws Exception {");
                builder.indent();
                for statement in &script_statements {
                    let code = self.generate_statement(statement)?;
                    Self::push_lines(&mut builder, &code);
                }
                if let Some(entry_call) = Self::script_entry_invocation(&script_methods) {
                    builder.push_line(&entry_call);
                }
                builder.dedent();
                builder.push_line("}");
            } else {
                for statement in &script_statements {
                    let code = self.generate_statement(statement)?;
                    Self::push_lines(&mut builder, &code);
                }
            }

            for method in &script_methods {
                builder.push_line("");
                let method_code = self.generate_method(method)?;
                Self::push_lines(&mut builder, &method_code);
            }

            builder.dedent();
            builder.push_line("}");

            unit.type_declarations.push(builder.build());
        }

        for declaration in remaining_declarations {
            if let IrStatement::SampleDeclaration(sample) = Self::base_statement(&declaration) {
                let artifacts = self.generate_sample_declaration_artifacts(sample)?;
                unit.type_declarations.extend(artifacts);
                continue;
            }

            let code = match Self::base_statement(&declaration) {
                IrStatement::ClassDeclaration { .. } => self.generate_class(&declaration)?,
                IrStatement::InterfaceDeclaration { .. } => {
                    self.generate_interface(&declaration)?
                }
                IrStatement::RecordDeclaration { .. } => self.generate_record(&declaration)?,
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
                | IrStatement::Import { .. }
                | IrStatement::Comment { .. } => self.generate_statement(&declaration)?,
                IrStatement::Commented { .. } => {
                    unreachable!("base_statement must unwrap commented statements")
                }
                IrStatement::SampleDeclaration { .. } => unreachable!(),
            };
            unit.type_declarations.push(code);
        }

        if let Some(helper) = self.sequence_helper.take() {
            unit.type_declarations.push(helper);
        }

        let mut inferred = self.imports.values().cloned().collect::<Vec<_>>();
        inferred.sort();
        inferred.dedup();
        unit.imports.extend(inferred);
        unit.imports.sort();
        unit.imports.dedup();

        Ok(unit)
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
        self.variance_stack.clear();
        self.sequence_helper = None;
        self.metadata_path.clear();
    }

    fn metadata_lookup_key(&self) -> Option<String> {
        if self.metadata_path.is_empty() {
            return None;
        }

        let mut segments: Vec<String> = Vec::new();
        if let Some(pkg) = &self.package {
            if !pkg.is_empty() {
                segments.extend(pkg.split('.').map(|segment| segment.to_string()));
            }
        }
        segments.extend(self.metadata_path.iter().cloned());
        Some(segments.join("::"))
    }

    fn current_generic_metadata(&self) -> Option<&IrGenericMetadata> {
        self.metadata_lookup_key()
            .and_then(|key| self.generic_metadata.get(&key))
    }

    pub(super) fn push_variance_scope(&mut self, params: &[IrTypeParameter]) {
        if params.is_empty() {
            return;
        }

        let mut scope = HashMap::with_capacity(params.len());
        for param in params {
            scope.insert(param.name.clone(), param.variance);
        }
        self.variance_stack.push(scope);
    }

    pub(super) fn truncate_variance_scopes(&mut self, len: usize) {
        self.variance_stack.truncate(len);
    }

    pub(super) fn ensure_sequence_helper(&mut self) {
        if self.sequence_helper.is_some() {
            return;
        }

        self.add_import("java.util.Iterator");
        self.add_import("java.util.stream.Stream");

        let mut builder = self.builder();
        builder.push_line("final class JvSequence<T> implements Iterable<T>, AutoCloseable {");
        builder.indent();
        builder.push_line("private final Stream<T> delegate;");
        builder.push_line("");
        builder.push_line("JvSequence(Stream<T> delegate) { this.delegate = delegate; }");
        builder.push_line("");
        builder.push_line("public Iterator<T> iterator() { return delegate.iterator(); }");
        builder.push_line("public void close() { delegate.close(); }");
        builder.push_line("public Stream<T> toStream() { return delegate; }");
        builder.dedent();
        builder.push_line("}");

        self.sequence_helper = Some(builder.build());
    }

    pub(super) fn variance_scope_len(&self) -> usize {
        self.variance_stack.len()
    }

    pub(super) fn lookup_variance(&self, name: &str) -> Option<IrVariance> {
        for scope in self.variance_stack.iter().rev() {
            if let Some(variance) = scope.get(name) {
                return Some(*variance);
            }
        }
        None
    }

    fn base_statement<'a>(statement: &'a IrStatement) -> &'a IrStatement {
        match statement {
            IrStatement::Commented { statement, .. } => Self::base_statement(statement),
            other => other,
        }
    }

    fn script_entry_invocation(methods: &[IrStatement]) -> Option<String> {
        for method in methods {
            if let IrStatement::MethodDeclaration {
                name, parameters, ..
            } = Self::base_statement(method)
            {
                if name != "main" {
                    continue;
                }

                if parameters.len() == 1 && Self::is_string_array(&parameters[0].java_type) {
                    return None;
                }

                return match parameters.len() {
                    0 => Some("main();".to_string()),
                    1 if Self::is_string_array(&parameters[0].java_type) => {
                        Some("main(args);".to_string())
                    }
                    _ => None,
                };
            }
        }

        None
    }

    fn is_entry_point_method(method: &IrStatement) -> bool {
        match Self::base_statement(method) {
            IrStatement::MethodDeclaration {
                name,
                parameters,
                modifiers,
                ..
            } if name == "main" && modifiers.visibility == IrVisibility::Public => {
                parameters.len() == 1 && Self::is_string_array(&parameters[0].java_type)
            }
            _ => false,
        }
    }

    fn is_string_array(java_type: &JavaType) -> bool {
        match java_type {
            JavaType::Array { element_type, .. } => match element_type.as_ref() {
                JavaType::Reference { name, .. } => name == "String",
                _ => false,
            },
            _ => false,
        }
    }

    fn hoist_regex_pattern_field(statement: &IrStatement) -> Option<IrStatement> {
        match statement {
            IrStatement::Commented {
                statement,
                comment,
                kind,
                comment_span,
            } => Self::hoist_regex_pattern_field(statement).map(|inner| IrStatement::Commented {
                statement: Box::new(inner),
                comment: comment.clone(),
                kind: kind.clone(),
                comment_span: comment_span.clone(),
            }),
            IrStatement::VariableDeclaration {
                name,
                java_type,
                initializer,
                is_final,
                modifiers,
                span,
            } if *is_final => {
                if let Some(expr) = initializer {
                    if matches!(expr, IrExpression::RegexPattern { .. }) {
                        let mut field_modifiers = modifiers.clone();
                        field_modifiers.is_static = true;
                        field_modifiers.is_final = true;
                        return Some(IrStatement::FieldDeclaration {
                            name: name.clone(),
                            java_type: java_type.clone(),
                            initializer: Some(expr.clone()),
                            modifiers: field_modifiers,
                            span: span.clone(),
                        });
                    }
                }
                None
            }
            _ => None,
        }
    }
}
