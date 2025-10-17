use crate::builder::{JavaCompilationUnit, JavaSourceBuilder};
use crate::config::JavaCodeGenConfig;
use crate::error::CodeGenError;
use crate::target_version::TargetedJavaEmitter;
use jv_ast::{BinaryOp, CallArgumentStyle, Literal, SequenceDelimiter, Span, UnaryOp};
use jv_build::metadata::SymbolIndex;
use jv_ir::{
    CompletableFutureOp, ConversionHelper, ConversionKind, ConversionMetadata, IrCaseLabel,
    IrCatchClause, IrDeconstructionComponent, IrDeconstructionPattern, IrExpression, IrForEachKind,
    IrForLoopMetadata, IrGenericMetadata, IrImplicitWhenEnd, IrImport, IrImportDetail, IrModifiers,
    IrNumericRangeLoop, IrParameter, IrProgram, IrRecordComponent, IrResource, IrSampleDeclaration,
    IrStatement, IrSwitchCase, IrTypeParameter, IrVariance, IrVisibility, JavaType, MethodOverload,
    NullableGuard, NullableGuardReason, UtilityClass, VirtualThreadOp,
};
use jv_mapper::{
    JavaPosition, JavaSpan, MappingCategory, MappingError, SourceMap, SourceMapBuilder,
};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::sync::Arc;

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
    type_parameter_stack: Vec<HashMap<String, IrTypeParameter>>,
    sequence_helper: Option<String>,
    generic_metadata: BTreeMap<String, IrGenericMetadata>,
    metadata_path: Vec<String>,
    package: Option<String>,
    symbol_index: Option<Arc<SymbolIndex>>,
    instance_extension_methods: HashMap<String, Vec<IrStatement>>,
    script_method_names: HashSet<String>,
    script_class_simple_name: Option<String>,
    conversion_metadata: HashMap<SpanKey, Vec<ConversionMetadata>>,
    conversion_map_records: Vec<ConversionSourceMapRecord>,
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
            type_parameter_stack: Vec::new(),
            sequence_helper: None,
            generic_metadata: BTreeMap::new(),
            metadata_path: Vec::new(),
            package: None,
            symbol_index: None,
            instance_extension_methods: HashMap::new(),
            script_method_names: HashSet::new(),
            script_class_simple_name: None,
            conversion_metadata: HashMap::new(),
            conversion_map_records: Vec::new(),
        }
    }

    pub fn set_symbol_index(&mut self, index: Option<Arc<SymbolIndex>>) {
        self.symbol_index = index;
    }

    pub fn generate_compilation_unit(
        &mut self,
        program: &IrProgram,
    ) -> Result<JavaCompilationUnit, CodeGenError> {
        self.reset();
        self.package = program.package.clone();
        self.generic_metadata = program.generic_metadata.clone();
        self.metadata_path.clear();
        self.conversion_metadata.clear();
        for entry in &program.conversion_metadata {
            self.conversion_metadata
                .entry(SpanKey::from(&entry.span))
                .or_default()
                .push(entry.metadata.clone());
        }

        let mut unit = JavaCompilationUnit::new();
        unit.package_declaration = program.package.clone();

        for import in &program.imports {
            if let IrStatement::Import(spec) = Self::base_statement(import) {
                unit.imports.push(Self::render_import_entry(spec));
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
                    if self.try_register_instance_extension_method(declaration, program) {
                        continue;
                    }
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
            let script_class = self.config.script_main_class.clone();

            self.script_method_names = script_methods
                .iter()
                .filter_map(|method| match Self::base_statement(method) {
                    IrStatement::MethodDeclaration { name, .. } => Some(name.clone()),
                    _ => None,
                })
                .collect();
            if !self.script_method_names.is_empty() {
                self.script_class_simple_name = Some(script_class.clone());
            }

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

            let pushed_script_class = !script_methods.is_empty();
            if pushed_script_class {
                self.metadata_path.push(script_class.clone());
            }

            let methods_result = (|| -> Result<(), CodeGenError> {
                for method in &script_methods {
                    builder.push_line("");
                    let method_code = self.generate_method(method)?;
                    Self::push_lines(&mut builder, &method_code);
                }
                Ok(())
            })();

            if pushed_script_class {
                self.metadata_path.pop();
            }

            methods_result?;

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
                | IrStatement::Import(..)
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
        self.instance_extension_methods.clear();
        self.script_method_names.clear();
        self.script_class_simple_name = None;
        self.conversion_metadata.clear();
        self.conversion_map_records.clear();
    }

    pub fn build_conversion_source_map(
        &mut self,
        java_source: &str,
        source_file: impl Into<String>,
        generated_file: impl Into<String>,
    ) -> Result<SourceMap, MappingError> {
        let records = std::mem::take(&mut self.conversion_map_records);
        let mut builder = SourceMapBuilder::new(source_file, generated_file);
        let mut search_from = 0;

        for record in records {
            if record.java_snippet.trim().is_empty() {
                continue;
            }

            if let Some((start, end)) =
                find_snippet_span(java_source, &record.java_snippet, search_from)
            {
                let (start_line, start_column) = offset_to_line_column(java_source, start);
                let (end_line, end_column) = offset_to_line_column(java_source, end);
                let java_span = JavaSpan::new(
                    JavaPosition::new(start_line, start_column),
                    JavaPosition::new(end_line, end_column),
                )?;

                builder.record_mapping(
                    record.span.clone(),
                    java_span,
                    MappingCategory::Expression,
                    record.ir_node.clone(),
                    Some(record.metadata.clone()),
                )?;

                search_from = end;
            } else {
                return Err(MappingError::GenerationError(format!(
                    "Unable to locate Java snippet for conversion at span {}:{}-{}:{}",
                    record.span.start_line,
                    record.span.start_column,
                    record.span.end_line,
                    record.span.end_column
                )));
            }
        }

        Ok(builder.build())
    }

    fn symbol_index(&self) -> Option<&SymbolIndex> {
        self.symbol_index.as_deref()
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
        let mut type_scope = HashMap::with_capacity(params.len());
        for param in params {
            scope.insert(param.name.clone(), param.variance);
            type_scope.insert(param.name.clone(), param.clone());
        }
        self.variance_stack.push(scope);
        self.type_parameter_stack.push(type_scope);
    }

    pub(super) fn truncate_variance_scopes(&mut self, len: usize) {
        self.variance_stack.truncate(len);
        self.type_parameter_stack.truncate(len);
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

    pub(super) fn filter_shadowed_type_parameters(
        &self,
        params: &[IrTypeParameter],
    ) -> Vec<IrTypeParameter> {
        params
            .iter()
            .filter(|param| !self.is_shadowed_type_parameter(param))
            .cloned()
            .collect()
    }

    fn is_shadowed_type_parameter(&self, param: &IrTypeParameter) -> bool {
        for scope in self.type_parameter_stack.iter().rev() {
            if let Some(existing) = scope.get(&param.name) {
                if Self::equivalent_type_parameters(existing, param) {
                    return true;
                }
            }
        }
        false
    }

    fn equivalent_type_parameters(a: &IrTypeParameter, b: &IrTypeParameter) -> bool {
        a.bounds == b.bounds
            && a.variance == b.variance
            && a.permits == b.permits
            && a.kind == b.kind
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

    fn render_import_entry(import: &IrImport) -> String {
        match &import.detail {
            IrImportDetail::Type { fqcn } => fqcn.clone(),
            IrImportDetail::Package { name } => format!("{name}.*"),
            IrImportDetail::Static { owner, member } => {
                if member == "*" {
                    format!("static {owner}.*")
                } else {
                    format!("static {owner}.{member}")
                }
            }
            IrImportDetail::Module { name } => format!("module {name}"),
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

    fn try_register_instance_extension_method(
        &mut self,
        declaration: &IrStatement,
        program: &IrProgram,
    ) -> bool {
        match Self::base_statement(declaration) {
            IrStatement::MethodDeclaration {
                parameters,
                modifiers,
                ..
            } => {
                if !modifiers.is_static || parameters.is_empty() {
                    return false;
                }
                let receiver_param = &parameters[0];
                if receiver_param.name != "receiver" {
                    return false;
                }
                if !self.should_emit_as_instance_method(&receiver_param.java_type, program) {
                    return false;
                }
                if let Some(type_name) = Self::extract_type_name(&receiver_param.java_type) {
                    self.instance_extension_methods
                        .entry(type_name)
                        .or_default()
                        .push(declaration.clone());
                    return true;
                }
                false
            }
            _ => false,
        }
    }

    fn should_emit_as_instance_method(
        &self,
        receiver_type: &JavaType,
        program: &IrProgram,
    ) -> bool {
        if let Some(type_name) = Self::extract_type_name(receiver_type) {
            self.is_defined_in_current_program(&type_name, program)
        } else {
            false
        }
    }

    fn is_defined_in_current_program(&self, type_name: &str, program: &IrProgram) -> bool {
        program
            .type_declarations
            .iter()
            .any(|decl| match Self::base_statement(decl) {
                IrStatement::ClassDeclaration { name, .. }
                | IrStatement::RecordDeclaration { name, .. }
                | IrStatement::InterfaceDeclaration { name, .. } => name == type_name,
                _ => false,
            })
    }

    fn extract_type_name(java_type: &JavaType) -> Option<String> {
        match java_type {
            JavaType::Reference { name, .. } => Some(Self::simple_name(name)),
            JavaType::Array { element_type, .. } => Self::extract_type_name(element_type),
            _ => None,
        }
    }

    fn simple_name(name: &str) -> String {
        name.rsplit('.').next().unwrap_or(name).to_string()
    }

    fn take_instance_extension_methods(&mut self, type_name: &str) -> Vec<IrStatement> {
        self.instance_extension_methods
            .remove(type_name)
            .unwrap_or_default()
    }
}

#[derive(Debug, Clone)]
struct ConversionSourceMapRecord {
    span: Span,
    java_snippet: String,
    metadata: Vec<ConversionMetadata>,
    ir_node: Option<String>,
}

fn find_snippet_span(haystack: &str, needle: &str, start_index: usize) -> Option<(usize, usize)> {
    if needle.is_empty() || start_index >= haystack.len() {
        return None;
    }

    haystack[start_index..].find(needle).map(|relative| {
        let absolute_start = start_index + relative;
        let absolute_end = absolute_start + needle.len();
        (absolute_start, absolute_end)
    })
}

fn offset_to_line_column(text: &str, offset: usize) -> (usize, usize) {
    let bytes = text.as_bytes();
    let mut line = 1;
    let mut column = 0;
    let mut index = 0;

    while index < offset && index < bytes.len() {
        match bytes[index] {
            b'\r' => {
                line += 1;
                column = 0;
                if index + 1 < bytes.len() && bytes[index + 1] == b'\n' && index + 1 < offset {
                    index += 2;
                } else {
                    index += 1;
                }
            }
            b'\n' => {
                line += 1;
                column = 0;
                index += 1;
            }
            _ => {
                column += 1;
                index += 1;
            }
        }
    }

    (line, column)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct SpanKey {
    start_line: usize,
    start_column: usize,
    end_line: usize,
    end_column: usize,
}

impl From<&Span> for SpanKey {
    fn from(span: &Span) -> Self {
        Self {
            start_line: span.start_line,
            start_column: span.start_column,
            end_line: span.end_line,
            end_column: span.end_column,
        }
    }
}
