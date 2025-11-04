use crate::builder::{JavaCompilationUnit, JavaSourceBuilder};
use crate::config::JavaCodeGenConfig;
use crate::error::CodeGenError;
use crate::target_version::TargetedJavaEmitter;
use jv_ast::{BinaryOp, CallArgumentStyle, Literal, SequenceDelimiter, Span, UnaryOp};
use jv_build::metadata::SymbolIndex;
use jv_ir::{
    CompletableFutureOp, ConversionHelper, ConversionKind, ConversionMetadata, IrCaseLabel,
    IrCatchClause, IrDeconstructionComponent, IrDeconstructionPattern, IrDoublebraceMutation,
    IrDoublebracePlan, IrExpression, IrForEachKind, IrForLoopMetadata, IrGenericMetadata,
    IrImplicitWhenEnd, IrImport, IrImportDetail, IrModifiers, IrNumericRangeLoop, IrParameter,
    IrProgram, IrRecordComponent, IrResource, IrSampleDeclaration, IrStatement, IrSwitchCase,
    IrTypeParameter, IrVariance, IrVisibility, JavaType, MethodOverload, NullableGuard,
    NullableGuardReason, SampleMode, SequencePipeline, SequenceSource, SequenceStage,
    SequenceTerminalKind, UtilityClass, VirtualThreadOp,
};
use jv_mapper::{
    JavaPosition, JavaSpan, MappingCategory, MappingError, SourceMap, SourceMapBuilder,
};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::sync::Arc;

const MAP_BRIDGE_METHOD_NAME: &str = "toMutableMap";
const LIST_BRIDGE_METHOD_NAME: &str = "toMutableList";
const SET_BRIDGE_METHOD_NAME: &str = "toMutableSet";
const SORTED_SET_BRIDGE_METHOD_NAME: &str = "toMutableSortedSet";
const QUEUE_BRIDGE_METHOD_NAME: &str = "toMutableQueue";
const DEQUE_BRIDGE_METHOD_NAME: &str = "toMutableDeque";

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
    current_return_type: Option<JavaType>,
    mutable_captures: HashSet<String>,
    record_components: HashMap<String, HashSet<String>>,
    temp_counter: usize,
    extra_type_declarations: Vec<String>,
    collection_literal_target: Vec<Option<JavaType>>,
    sample_bindings: HashMap<String, SampleBindingInfo>,
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
            current_return_type: None,
            mutable_captures: HashSet::new(),
            record_components: HashMap::new(),
            temp_counter: 0,
            extra_type_declarations: Vec::new(),
            collection_literal_target: Vec::new(),
            sample_bindings: HashMap::new(),
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
        self.register_record_components_from_declarations(&program.type_declarations, &[]);
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
                self.extra_type_declarations.extend(artifacts);
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

        if !self.extra_type_declarations.is_empty() {
            unit.type_declarations
                .extend(self.extra_type_declarations.drain(..));
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
        self.current_return_type = None;
        self.mutable_captures.clear();
        self.record_components.clear();
        self.temp_counter = 0;
        self.extra_type_declarations.clear();
        self.collection_literal_target.clear();
        self.sample_bindings.clear();
    }

    fn register_record_components_from_declarations(
        &mut self,
        declarations: &[IrStatement],
        enclosing: &[String],
    ) {
        for declaration in declarations {
            self.register_record_components_from_statement(declaration, enclosing);
        }
    }

    fn register_record_components_from_statement(
        &mut self,
        statement: &IrStatement,
        enclosing: &[String],
    ) {
        match statement {
            IrStatement::RecordDeclaration {
                name, components, ..
            } => {
                self.add_record_components(name, components, enclosing);
            }
            IrStatement::ClassDeclaration {
                name,
                nested_classes,
                ..
            }
            | IrStatement::InterfaceDeclaration {
                name,
                nested_types: nested_classes,
                ..
            } => {
                let mut next_enclosing = enclosing.to_vec();
                next_enclosing.push(name.clone());
                self.register_record_components_from_declarations(nested_classes, &next_enclosing);
            }
            _ => {}
        }
    }

    fn add_record_components(
        &mut self,
        name: &str,
        components: &[IrRecordComponent],
        enclosing: &[String],
    ) {
        let mut keys = Vec::new();
        let simple_name = name.to_string();
        keys.push(simple_name);

        if !enclosing.is_empty() {
            let dotted = format!("{}.{name}", enclosing.join("."));
            let nested = format!("{}${name}", enclosing.join("$"));
            keys.push(dotted);
            keys.push(nested);
        }

        if let Some(pkg) = &self.package {
            if !pkg.is_empty() {
                keys.push(format!("{pkg}.{name}"));
                if !enclosing.is_empty() {
                    let dotted = format!("{pkg}.{}.{name}", enclosing.join("."));
                    let nested = format!("{pkg}.{}${name}", enclosing.join("$"));
                    keys.push(dotted);
                    keys.push(nested);
                }
            }
        }

        let component_names: HashSet<String> = components
            .iter()
            .map(|component| component.name.clone())
            .collect();

        for key in keys {
            if key.is_empty() {
                continue;
            }
            self.record_components
                .entry(key)
                .or_insert_with(HashSet::new)
                .extend(component_names.iter().cloned());
        }
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

    fn coerce_return_expression(
        &mut self,
        expr: &IrExpression,
        rendered: String,
        target_type: &JavaType,
    ) -> Result<String, CodeGenError> {
        if let Some(source_type) = Self::expression_java_type(expr) {
            if source_type == target_type {
                return Ok(rendered);
            }

            match (source_type, target_type) {
                (JavaType::Primitive(source_name), JavaType::Primitive(target_name))
                    if source_name == "double" && target_name == "float" =>
                {
                    let target_rendered = self.generate_type(target_type)?;
                    return Ok(format!("({}) {}", target_rendered, rendered));
                }
                _ => {}
            }
        }

        Ok(rendered)
    }

    pub(crate) fn analyze_mutable_captures(&self, body: &IrExpression) -> HashSet<String> {
        let mut locals = HashSet::new();
        self.collect_method_locals_from_expression(body, &mut locals);
        let mut captures = HashSet::new();
        self.collect_mutable_captures_in_expression(body, &locals, &mut captures, &HashSet::new());
        captures
    }

    fn collect_method_locals_from_expression(
        &self,
        expr: &IrExpression,
        locals: &mut HashSet<String>,
    ) {
        match expr {
            IrExpression::Block { statements, .. } => {
                for statement in statements {
                    self.collect_method_locals_from_statement(statement, locals);
                }
            }
            IrExpression::DoublebraceInit { base, plan, .. } => {
                if let Some(inner) = base.as_deref() {
                    self.collect_method_locals_from_expression(inner, locals);
                }
                self.collect_method_locals_from_doublebrace(plan, locals);
            }
            _ => {}
        }
    }

    fn collect_method_locals_from_statement(
        &self,
        statement: &IrStatement,
        locals: &mut HashSet<String>,
    ) {
        match statement {
            IrStatement::Commented { statement, .. } => {
                self.collect_method_locals_from_statement(statement, locals);
            }
            IrStatement::VariableDeclaration {
                name, initializer, ..
            } => {
                locals.insert(name.clone());
                if let Some(init) = initializer {
                    self.collect_method_locals_from_expression(init, locals);
                }
            }
            IrStatement::Block { statements, .. } => {
                for stmt in statements {
                    self.collect_method_locals_from_statement(stmt, locals);
                }
            }
            IrStatement::If {
                condition,
                then_stmt,
                else_stmt,
                ..
            } => {
                self.collect_method_locals_from_expression(condition, locals);
                self.collect_method_locals_from_statement(then_stmt, locals);
                if let Some(else_branch) = else_stmt {
                    self.collect_method_locals_from_statement(else_branch, locals);
                }
            }
            IrStatement::While {
                condition, body, ..
            } => {
                self.collect_method_locals_from_expression(condition, locals);
                self.collect_method_locals_from_statement(body, locals);
            }
            IrStatement::ForEach { iterable, body, .. } => {
                self.collect_method_locals_from_expression(iterable, locals);
                self.collect_method_locals_from_statement(body, locals);
            }
            IrStatement::For {
                init,
                condition,
                update,
                body,
                ..
            } => {
                if let Some(init_stmt) = init.as_deref() {
                    self.collect_method_locals_from_statement(init_stmt, locals);
                }
                if let Some(cond) = condition {
                    self.collect_method_locals_from_expression(cond, locals);
                }
                if let Some(update_expr) = update {
                    self.collect_method_locals_from_expression(update_expr, locals);
                }
                self.collect_method_locals_from_statement(body, locals);
            }
            IrStatement::Expression { expr, .. } => {
                self.collect_method_locals_from_expression(expr, locals);
            }
            IrStatement::Return { value, .. } => {
                if let Some(expr) = value {
                    self.collect_method_locals_from_expression(expr, locals);
                }
            }
            IrStatement::Switch {
                discriminant,
                cases,
                ..
            } => {
                self.collect_method_locals_from_expression(discriminant, locals);
                for case in cases {
                    if let Some(guard) = &case.guard {
                        self.collect_method_locals_from_expression(guard, locals);
                    }
                    self.collect_method_locals_from_expression(&case.body, locals);
                }
            }
            IrStatement::Try {
                body,
                catch_clauses,
                finally_block,
                ..
            } => {
                self.collect_method_locals_from_statement(body, locals);
                for clause in catch_clauses {
                    self.collect_method_locals_from_statement(&clause.body, locals);
                }
                if let Some(finally_stmt) = finally_block {
                    self.collect_method_locals_from_statement(finally_stmt, locals);
                }
            }
            IrStatement::TryWithResources {
                resources,
                body,
                catch_clauses,
                finally_block,
                ..
            } => {
                for resource in resources {
                    locals.insert(resource.name.clone());
                    self.collect_method_locals_from_expression(&resource.initializer, locals);
                }
                self.collect_method_locals_from_statement(body, locals);
                for clause in catch_clauses {
                    self.collect_method_locals_from_statement(&clause.body, locals);
                }
                if let Some(finally_stmt) = finally_block {
                    self.collect_method_locals_from_statement(finally_stmt, locals);
                }
            }
            _ => {}
        }
    }

    fn collect_method_locals_from_doublebrace(
        &self,
        plan: &IrDoublebracePlan,
        locals: &mut HashSet<String>,
    ) {
        match plan {
            IrDoublebracePlan::Mutate(mutate) => {
                for step in &mutate.steps {
                    match step {
                        IrDoublebraceMutation::FieldAssignment(update) => {
                            self.collect_method_locals_from_expression(&update.value, locals);
                        }
                        IrDoublebraceMutation::MethodCall(call) => {
                            for arg in &call.arguments {
                                self.collect_method_locals_from_expression(arg, locals);
                            }
                        }
                        IrDoublebraceMutation::Statement(statements) => {
                            for statement in statements {
                                self.collect_method_locals_from_statement(statement, locals);
                            }
                        }
                    }
                }
            }
            IrDoublebracePlan::Copy(copy) => {
                for update in &copy.updates {
                    self.collect_method_locals_from_expression(&update.value, locals);
                }
            }
        }
    }

    fn collect_mutable_captures_in_expression(
        &self,
        expr: &IrExpression,
        method_locals: &HashSet<String>,
        captures: &mut HashSet<String>,
        scope_locals: &HashSet<String>,
    ) {
        match expr {
            IrExpression::Assignment { target, value, .. } => {
                if let IrExpression::Identifier { name, .. } = target.as_ref() {
                    if method_locals.contains(name) && !scope_locals.contains(name) {
                        captures.insert(name.clone());
                    }
                }
                self.collect_mutable_captures_in_expression(
                    value,
                    method_locals,
                    captures,
                    scope_locals,
                );
            }
            IrExpression::Block { statements, .. } => {
                let mut block_scope = scope_locals.clone();
                for statement in statements {
                    block_scope = self.collect_mutable_captures_in_statement(
                        statement,
                        method_locals,
                        captures,
                        &block_scope,
                    );
                }
            }
            IrExpression::Lambda {
                param_names, body, ..
            } => {
                let lambda_scope: HashSet<String> = param_names.iter().cloned().collect();
                self.collect_mutable_captures_in_expression(
                    body,
                    method_locals,
                    captures,
                    &lambda_scope,
                );
            }
            IrExpression::MethodCall {
                receiver,
                method_name: _,
                args,
                ..
            } => {
                if let Some(recv) = receiver {
                    self.collect_mutable_captures_in_expression(
                        recv,
                        method_locals,
                        captures,
                        scope_locals,
                    );
                }
                for arg in args {
                    self.collect_mutable_captures_in_expression(
                        arg,
                        method_locals,
                        captures,
                        scope_locals,
                    );
                }
            }
            IrExpression::DoublebraceInit { base, plan, .. } => {
                if let Some(inner) = base.as_deref() {
                    self.collect_mutable_captures_in_expression(
                        inner,
                        method_locals,
                        captures,
                        scope_locals,
                    );
                }
                self.collect_mutable_captures_in_doublebrace(
                    plan,
                    method_locals,
                    captures,
                    scope_locals,
                );
            }
            IrExpression::Binary { left, right, .. } => {
                self.collect_mutable_captures_in_expression(
                    left,
                    method_locals,
                    captures,
                    scope_locals,
                );
                self.collect_mutable_captures_in_expression(
                    right,
                    method_locals,
                    captures,
                    scope_locals,
                );
            }
            IrExpression::Unary { operand, .. } => {
                self.collect_mutable_captures_in_expression(
                    operand,
                    method_locals,
                    captures,
                    scope_locals,
                );
            }
            IrExpression::Conditional {
                condition,
                then_expr,
                else_expr,
                ..
            } => {
                self.collect_mutable_captures_in_expression(
                    condition,
                    method_locals,
                    captures,
                    scope_locals,
                );
                self.collect_mutable_captures_in_expression(
                    then_expr,
                    method_locals,
                    captures,
                    scope_locals,
                );
                self.collect_mutable_captures_in_expression(
                    else_expr,
                    method_locals,
                    captures,
                    scope_locals,
                );
            }
            IrExpression::ObjectCreation { args, .. } => {
                for arg in args {
                    self.collect_mutable_captures_in_expression(
                        arg,
                        method_locals,
                        captures,
                        scope_locals,
                    );
                }
            }
            IrExpression::ArrayCreation { initializer, .. } => {
                if let Some(values) = initializer {
                    for value in values {
                        self.collect_mutable_captures_in_expression(
                            value,
                            method_locals,
                            captures,
                            scope_locals,
                        );
                    }
                }
            }
            IrExpression::SequencePipeline { pipeline, .. } => {
                self.collect_mutable_captures_in_sequence_pipeline(
                    pipeline,
                    method_locals,
                    captures,
                    scope_locals,
                );
            }
            IrExpression::Literal(_, _)
            | IrExpression::RegexPattern { .. }
            | IrExpression::Identifier { .. }
            | IrExpression::Switch { .. }
            | IrExpression::TryWithResources { .. }
            | IrExpression::CompletableFuture { .. }
            | IrExpression::VirtualThread { .. }
            | IrExpression::NullSafeOperation { .. }
            | IrExpression::FieldAccess { .. }
            | IrExpression::ArrayAccess { .. }
            | IrExpression::This { .. }
            | IrExpression::Super { .. }
            | IrExpression::StringFormat { .. }
            | IrExpression::InstanceOf { .. }
            | IrExpression::Cast { .. } => {}
        }
    }

    fn render_doublebrace_base(
        &mut self,
        expr: &IrExpression,
        receiver_type: &JavaType,
    ) -> Result<String, CodeGenError> {
        let rendered = self.generate_expression(expr)?;
        if let Some(name) = Self::sample_identifier_name(expr) {
            if let Some(binding) = self.sample_bindings.get(name) {
                if let Some(call) = binding.bridge_call(receiver_type, &rendered) {
                    return Ok(call);
                }
            }
        }
        if let Some(converted) =
            self.maybe_convert_collection_base(expr, receiver_type, &rendered)?
        {
            return Ok(converted);
        }
        Ok(rendered)
    }

    fn sample_identifier_name(expr: &IrExpression) -> Option<&str> {
        if let IrExpression::Identifier { name, .. } = expr {
            Some(name.as_str())
        } else {
            None
        }
    }

    fn generate_expression_with_target(
        &mut self,
        expr: &IrExpression,
        target: Option<&JavaType>,
    ) -> Result<String, CodeGenError> {
        if let Some(java_type) = target {
            self.collection_literal_target.push(Some(java_type.clone()));
            let result = self.generate_expression(expr);
            self.collection_literal_target.pop();
            result
        } else {
            self.generate_expression(expr)
        }
    }

    fn current_collection_literal_target(&self) -> Option<&JavaType> {
        self.collection_literal_target
            .last()
            .and_then(|ty| ty.as_ref())
    }

    fn wrap_literal_for_target(target: &JavaType, literal: &str) -> Option<String> {
        let base = reference_base_name(target)?;
        let wrapped = match base {
            "java.util.List" | "java.util.ArrayList" => {
                "new java.util.ArrayList<>(".to_string() + literal + ")"
            }
            "java.util.Set" | "java.util.LinkedHashSet" => {
                "new java.util.LinkedHashSet<>(".to_string() + literal + ")"
            }
            "java.util.SortedSet" | "java.util.NavigableSet" | "java.util.TreeSet" => {
                "new java.util.TreeSet<>(".to_string() + literal + ")"
            }
            "java.util.Queue" | "java.util.ArrayDeque" => {
                "new java.util.ArrayDeque<>(".to_string() + literal + ")"
            }
            "java.util.Deque" => "new java.util.ArrayDeque<>(".to_string() + literal + ")",
            _ => return None,
        };
        Some(wrapped)
    }

    fn maybe_convert_collection_base(
        &mut self,
        expr: &IrExpression,
        receiver_type: &JavaType,
        base_expr: &str,
    ) -> Result<Option<String>, CodeGenError> {
        let Some(instantiation) = collection_copy_instantiation(receiver_type) else {
            return Ok(None);
        };

        let Some(expr_type) = Self::expression_java_type(expr) else {
            return Ok(None);
        };

        if let Some(expr_base) = reference_base_name(expr_type) {
            if expr_base == instantiation {
                return Ok(None);
            }

            return Ok(Some(format!("new {}<>({})", instantiation, base_expr)));
        }

        Ok(None)
    }

    fn collect_mutable_captures_in_doublebrace(
        &self,
        plan: &IrDoublebracePlan,
        method_locals: &HashSet<String>,
        captures: &mut HashSet<String>,
        scope_locals: &HashSet<String>,
    ) {
        match plan {
            IrDoublebracePlan::Mutate(mutate) => {
                for step in &mutate.steps {
                    match step {
                        IrDoublebraceMutation::FieldAssignment(update) => {
                            self.collect_mutable_captures_in_expression(
                                &update.value,
                                method_locals,
                                captures,
                                scope_locals,
                            );
                        }
                        IrDoublebraceMutation::MethodCall(call) => {
                            for arg in &call.arguments {
                                self.collect_mutable_captures_in_expression(
                                    arg,
                                    method_locals,
                                    captures,
                                    scope_locals,
                                );
                            }
                        }
                        IrDoublebraceMutation::Statement(statements) => {
                            let mut nested_scope = scope_locals.clone();
                            for statement in statements {
                                nested_scope = self.collect_mutable_captures_in_statement(
                                    statement,
                                    method_locals,
                                    captures,
                                    &nested_scope,
                                );
                            }
                        }
                    }
                }
            }
            IrDoublebracePlan::Copy(copy) => {
                for update in &copy.updates {
                    self.collect_mutable_captures_in_expression(
                        &update.value,
                        method_locals,
                        captures,
                        scope_locals,
                    );
                }
            }
        }
    }

    fn collect_mutable_captures_in_sequence_pipeline(
        &self,
        pipeline: &SequencePipeline,
        method_locals: &HashSet<String>,
        captures: &mut HashSet<String>,
        scope_locals: &HashSet<String>,
    ) {
        self.collect_mutable_captures_in_sequence_source(
            &pipeline.source,
            method_locals,
            captures,
            scope_locals,
        );

        for stage in &pipeline.stages {
            match stage {
                SequenceStage::Map { lambda, .. } | SequenceStage::FlatMap { lambda, .. } => {
                    self.collect_mutable_captures_in_expression(
                        lambda,
                        method_locals,
                        captures,
                        scope_locals,
                    );
                }
                SequenceStage::Filter { predicate, .. } => {
                    self.collect_mutable_captures_in_expression(
                        predicate,
                        method_locals,
                        captures,
                        scope_locals,
                    );
                }
                SequenceStage::Take { count, .. } | SequenceStage::Drop { count, .. } => {
                    self.collect_mutable_captures_in_expression(
                        count,
                        method_locals,
                        captures,
                        scope_locals,
                    );
                }
                SequenceStage::Sorted { comparator, .. } => {
                    if let Some(expr) = comparator {
                        self.collect_mutable_captures_in_expression(
                            expr,
                            method_locals,
                            captures,
                            scope_locals,
                        );
                    }
                }
            }
        }

        if let Some(terminal) = &pipeline.terminal {
            match &terminal.kind {
                SequenceTerminalKind::Fold {
                    initial,
                    accumulator,
                } => {
                    self.collect_mutable_captures_in_expression(
                        initial,
                        method_locals,
                        captures,
                        scope_locals,
                    );
                    self.collect_mutable_captures_in_expression(
                        accumulator,
                        method_locals,
                        captures,
                        scope_locals,
                    );
                }
                SequenceTerminalKind::Reduce { accumulator } => {
                    self.collect_mutable_captures_in_expression(
                        accumulator,
                        method_locals,
                        captures,
                        scope_locals,
                    );
                }
                SequenceTerminalKind::GroupBy { key_selector } => {
                    self.collect_mutable_captures_in_expression(
                        key_selector,
                        method_locals,
                        captures,
                        scope_locals,
                    );
                }
                SequenceTerminalKind::Associate { pair_selector } => {
                    self.collect_mutable_captures_in_expression(
                        pair_selector,
                        method_locals,
                        captures,
                        scope_locals,
                    );
                }
                SequenceTerminalKind::ForEach { action } => {
                    self.collect_mutable_captures_in_expression(
                        action,
                        method_locals,
                        captures,
                        scope_locals,
                    );
                }
                SequenceTerminalKind::ToList
                | SequenceTerminalKind::ToSet
                | SequenceTerminalKind::Count
                | SequenceTerminalKind::Sum => {}
            }

            if let Some(adapter) = &terminal.canonical_adapter {
                self.collect_mutable_captures_in_expression(
                    adapter,
                    method_locals,
                    captures,
                    scope_locals,
                );
            }
        }
    }

    fn collect_mutable_captures_in_sequence_source(
        &self,
        source: &SequenceSource,
        method_locals: &HashSet<String>,
        captures: &mut HashSet<String>,
        scope_locals: &HashSet<String>,
    ) {
        match source {
            SequenceSource::Collection { expr, .. }
            | SequenceSource::Array { expr, .. }
            | SequenceSource::JavaStream { expr, .. } => {
                self.collect_mutable_captures_in_expression(
                    expr,
                    method_locals,
                    captures,
                    scope_locals,
                );
            }
            SequenceSource::ListLiteral { elements, .. } => {
                for element in elements {
                    self.collect_mutable_captures_in_expression(
                        element,
                        method_locals,
                        captures,
                        scope_locals,
                    );
                }
            }
        }
    }

    fn is_known_record_component(&self, receiver: &IrExpression, field_name: &str) -> bool {
        let Some(java_type) = Self::expression_java_type(receiver) else {
            return false;
        };

        let JavaType::Reference { name, .. } = java_type else {
            return false;
        };

        let mut candidates = Vec::new();
        candidates.push(name.as_str());
        if let Some(simple) = name.rsplit('.').next() {
            candidates.push(simple);
        }
        if let Some(simple) = name.rsplit('$').next() {
            candidates.push(simple);
        }

        for candidate in candidates {
            if let Some(components) = self.record_components.get(candidate) {
                if components.contains(field_name) {
                    return true;
                }
            }
        }

        false
    }

    fn collect_mutable_captures_in_statement(
        &self,
        statement: &IrStatement,
        method_locals: &HashSet<String>,
        captures: &mut HashSet<String>,
        scope_locals: &HashSet<String>,
    ) -> HashSet<String> {
        match statement {
            IrStatement::Commented { statement, .. } => self.collect_mutable_captures_in_statement(
                statement,
                method_locals,
                captures,
                scope_locals,
            ),
            IrStatement::VariableDeclaration {
                name, initializer, ..
            } => {
                if let Some(init) = initializer {
                    self.collect_mutable_captures_in_expression(
                        init,
                        method_locals,
                        captures,
                        scope_locals,
                    );
                }
                let mut updated = scope_locals.clone();
                updated.insert(name.clone());
                updated
            }
            IrStatement::Expression { expr, .. } => {
                self.collect_mutable_captures_in_expression(
                    expr,
                    method_locals,
                    captures,
                    scope_locals,
                );
                scope_locals.clone()
            }
            IrStatement::Return { value, .. } => {
                if let Some(expr) = value {
                    self.collect_mutable_captures_in_expression(
                        expr,
                        method_locals,
                        captures,
                        scope_locals,
                    );
                }
                scope_locals.clone()
            }
            IrStatement::Block { statements, .. } => {
                let mut block_scope = scope_locals.clone();
                for stmt in statements {
                    block_scope = self.collect_mutable_captures_in_statement(
                        stmt,
                        method_locals,
                        captures,
                        &block_scope,
                    );
                }
                scope_locals.clone()
            }
            IrStatement::If {
                condition,
                then_stmt,
                else_stmt,
                ..
            } => {
                self.collect_mutable_captures_in_expression(
                    condition,
                    method_locals,
                    captures,
                    scope_locals,
                );
                let mut then_scope = scope_locals.clone();
                then_scope = self.collect_mutable_captures_in_statement(
                    then_stmt,
                    method_locals,
                    captures,
                    &then_scope,
                );
                if let Some(else_branch) = else_stmt {
                    let else_scope = scope_locals.clone();
                    self.collect_mutable_captures_in_statement(
                        else_branch,
                        method_locals,
                        captures,
                        &else_scope,
                    );
                }
                scope_locals.clone()
            }
            IrStatement::While {
                condition, body, ..
            } => {
                self.collect_mutable_captures_in_expression(
                    condition,
                    method_locals,
                    captures,
                    scope_locals,
                );
                self.collect_mutable_captures_in_statement(
                    body,
                    method_locals,
                    captures,
                    scope_locals,
                );
                scope_locals.clone()
            }
            IrStatement::ForEach {
                variable,
                iterable,
                body,
                ..
            } => {
                self.collect_mutable_captures_in_expression(
                    iterable,
                    method_locals,
                    captures,
                    scope_locals,
                );
                let mut loop_scope = scope_locals.clone();
                loop_scope.insert(variable.clone());
                self.collect_mutable_captures_in_statement(
                    body,
                    method_locals,
                    captures,
                    &loop_scope,
                );
                scope_locals.clone()
            }
            IrStatement::For {
                init,
                condition,
                update,
                body,
                ..
            } => {
                let mut loop_scope = scope_locals.clone();
                if let Some(init_stmt) = init.as_deref() {
                    loop_scope = self.collect_mutable_captures_in_statement(
                        init_stmt,
                        method_locals,
                        captures,
                        &loop_scope,
                    );
                }
                if let Some(cond) = condition {
                    self.collect_mutable_captures_in_expression(
                        cond,
                        method_locals,
                        captures,
                        &loop_scope,
                    );
                }
                if let Some(update_expr) = update {
                    self.collect_mutable_captures_in_expression(
                        update_expr,
                        method_locals,
                        captures,
                        &loop_scope,
                    );
                }
                self.collect_mutable_captures_in_statement(
                    body,
                    method_locals,
                    captures,
                    &loop_scope,
                );
                scope_locals.clone()
            }
            IrStatement::Switch {
                discriminant,
                cases,
                ..
            } => {
                self.collect_mutable_captures_in_expression(
                    discriminant,
                    method_locals,
                    captures,
                    scope_locals,
                );
                for case in cases {
                    if let Some(guard) = &case.guard {
                        self.collect_mutable_captures_in_expression(
                            guard,
                            method_locals,
                            captures,
                            scope_locals,
                        );
                    }
                    self.collect_mutable_captures_in_expression(
                        &case.body,
                        method_locals,
                        captures,
                        scope_locals,
                    );
                }
                scope_locals.clone()
            }
            IrStatement::Try {
                body,
                catch_clauses,
                finally_block,
                ..
            } => {
                self.collect_mutable_captures_in_statement(
                    body,
                    method_locals,
                    captures,
                    scope_locals,
                );
                for clause in catch_clauses {
                    self.collect_mutable_captures_in_statement(
                        &clause.body,
                        method_locals,
                        captures,
                        scope_locals,
                    );
                }
                if let Some(finally_stmt) = finally_block {
                    self.collect_mutable_captures_in_statement(
                        finally_stmt,
                        method_locals,
                        captures,
                        scope_locals,
                    );
                }
                scope_locals.clone()
            }
            IrStatement::TryWithResources {
                resources,
                body,
                catch_clauses,
                finally_block,
                ..
            } => {
                let mut resource_scope = scope_locals.clone();
                for resource in resources {
                    resource_scope.insert(resource.name.clone());
                    self.collect_mutable_captures_in_expression(
                        &resource.initializer,
                        method_locals,
                        captures,
                        &resource_scope,
                    );
                }
                self.collect_mutable_captures_in_statement(
                    body,
                    method_locals,
                    captures,
                    &resource_scope,
                );
                for clause in catch_clauses {
                    self.collect_mutable_captures_in_statement(
                        &clause.body,
                        method_locals,
                        captures,
                        scope_locals,
                    );
                }
                if let Some(finally_stmt) = finally_block {
                    self.collect_mutable_captures_in_statement(
                        finally_stmt,
                        method_locals,
                        captures,
                        scope_locals,
                    );
                }
                scope_locals.clone()
            }
            _ => scope_locals.clone(),
        }
    }

    fn boxed_type(java_type: &JavaType) -> JavaType {
        match java_type {
            JavaType::Primitive(name) => match name.as_str() {
                "int" => JavaType::Reference {
                    name: "Integer".to_string(),
                    generic_args: vec![],
                },
                "boolean" => JavaType::Reference {
                    name: "Boolean".to_string(),
                    generic_args: vec![],
                },
                "double" => JavaType::Reference {
                    name: "Double".to_string(),
                    generic_args: vec![],
                },
                "float" => JavaType::Reference {
                    name: "Float".to_string(),
                    generic_args: vec![],
                },
                "long" => JavaType::Reference {
                    name: "Long".to_string(),
                    generic_args: vec![],
                },
                "byte" => JavaType::Reference {
                    name: "Byte".to_string(),
                    generic_args: vec![],
                },
                "short" => JavaType::Reference {
                    name: "Short".to_string(),
                    generic_args: vec![],
                },
                "char" => JavaType::Reference {
                    name: "Character".to_string(),
                    generic_args: vec![],
                },
                _ => JavaType::Reference {
                    name: "Object".to_string(),
                    generic_args: vec![],
                },
            },
            _ => java_type.clone(),
        }
    }

    fn symbol_index(&self) -> Option<&SymbolIndex> {
        self.symbol_index.as_deref()
    }

    fn fresh_identifier(&mut self, prefix: &str) -> String {
        let name = format!("{}{}", prefix, self.temp_counter);
        self.temp_counter += 1;
        name
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
struct SampleBindingInfo {
    helper_class: String,
    bridges: Vec<SampleBridgeKind>,
}

impl SampleBindingInfo {
    fn bridge_call(&self, receiver_type: &JavaType, base_expr: &str) -> Option<String> {
        self.bridges
            .iter()
            .find(|kind| kind.matches_receiver(receiver_type))
            .map(|kind| {
                format!(
                    "{}.{}({})",
                    self.helper_class,
                    kind.method_name(),
                    base_expr
                )
            })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SampleBridgeKind {
    MapFromRecord,
    ListLike,
    SetLike,
    SortedSetLike,
    QueueLike,
    DequeLike,
}

impl SampleBridgeKind {
    fn method_name(self) -> &'static str {
        match self {
            SampleBridgeKind::MapFromRecord => MAP_BRIDGE_METHOD_NAME,
            SampleBridgeKind::ListLike => LIST_BRIDGE_METHOD_NAME,
            SampleBridgeKind::SetLike => SET_BRIDGE_METHOD_NAME,
            SampleBridgeKind::SortedSetLike => SORTED_SET_BRIDGE_METHOD_NAME,
            SampleBridgeKind::QueueLike => QUEUE_BRIDGE_METHOD_NAME,
            SampleBridgeKind::DequeLike => DEQUE_BRIDGE_METHOD_NAME,
        }
    }

    fn target_class_name(self) -> Option<&'static str> {
        match self {
            SampleBridgeKind::MapFromRecord => None,
            SampleBridgeKind::ListLike => Some("java.util.ArrayList"),
            SampleBridgeKind::SetLike => Some("java.util.LinkedHashSet"),
            SampleBridgeKind::SortedSetLike => Some("java.util.TreeSet"),
            SampleBridgeKind::QueueLike | SampleBridgeKind::DequeLike => {
                Some("java.util.ArrayDeque")
            }
        }
    }

    fn matches_receiver(self, receiver_type: &JavaType) -> bool {
        if let Some(base) = reference_base_name(receiver_type) {
            match self {
                SampleBridgeKind::MapFromRecord => is_map_like_name(base),
                SampleBridgeKind::ListLike => is_list_like_name(base),
                SampleBridgeKind::SetLike => {
                    base == "java.util.Set" || base == "java.util.LinkedHashSet"
                }
                SampleBridgeKind::SortedSetLike => {
                    base == "java.util.SortedSet"
                        || base == "java.util.NavigableSet"
                        || base == "java.util.TreeSet"
                }
                SampleBridgeKind::QueueLike => {
                    base == "java.util.Queue" || base == "java.util.ArrayDeque"
                }
                SampleBridgeKind::DequeLike => {
                    base == "java.util.Deque" || base == "java.util.ArrayDeque"
                }
            }
        } else {
            false
        }
    }
}

pub(super) fn reference_base_name(java_type: &JavaType) -> Option<&str> {
    if let JavaType::Reference { name, .. } = java_type {
        Some(name.split('<').next().unwrap_or(name).trim())
    } else {
        None
    }
}

pub(super) fn is_map_like_name(base: &str) -> bool {
    matches!(
        base,
        "java.util.Map"
            | "java.util.LinkedHashMap"
            | "java.util.HashMap"
            | "java.util.TreeMap"
            | "java.util.SortedMap"
            | "java.util.NavigableMap"
            | "java.util.concurrent.ConcurrentMap"
    )
}

pub(super) fn is_list_like_name(base: &str) -> bool {
    matches!(
        base,
        "java.util.List"
            | "java.util.ArrayList"
            | "java.util.LinkedList"
            | "java.util.Collection"
            | "java.lang.Iterable"
    )
}

fn collection_copy_instantiation(receiver_type: &JavaType) -> Option<&'static str> {
    let base = reference_base_name(receiver_type)?;
    match base {
        "java.util.List"
        | "java.util.ArrayList"
        | "java.util.Collection"
        | "java.lang.Iterable" => Some("java.util.ArrayList"),
        "java.util.Set" | "java.util.LinkedHashSet" => Some("java.util.LinkedHashSet"),
        "java.util.SortedSet" | "java.util.NavigableSet" | "java.util.TreeSet" => {
            Some("java.util.TreeSet")
        }
        "java.util.Map" | "java.util.LinkedHashMap" => Some("java.util.LinkedHashMap"),
        "java.util.Queue" | "java.util.Deque" | "java.util.ArrayDeque" => {
            Some("java.util.ArrayDeque")
        }
        _ => None,
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
