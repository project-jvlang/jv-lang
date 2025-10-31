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
    NullableGuard, NullableGuardReason, SequencePipeline, SequenceSource, SequenceStage,
    SequenceTerminal, SequenceTerminalKind, UtilityClass, VirtualThreadOp,
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
    current_return_type: Option<JavaType>,
    mutable_captures: HashSet<String>,
    record_components: HashMap<String, HashSet<String>>,
    record_component_types: HashMap<String, HashMap<String, JavaType>>,
    local_type_overrides: Vec<HashMap<String, JavaType>>,
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
            record_component_types: HashMap::new(),
            local_type_overrides: Vec::new(),
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
        let mut script_sample_nested_types: Vec<String> = Vec::new();
        let mut script_sample_bindings: Vec<String> = Vec::new();
        let mut script_sample_declarations: Vec<IrSampleDeclaration> = Vec::new();
        let mut standalone_sample_types: Vec<IrStatement> = Vec::new();

        for declaration in &program.type_declarations {
            match Self::base_statement(declaration) {
                IrStatement::ClassDeclaration { name, .. } if Self::is_sample_type_name(name) => {
                    let code = self.generate_nested_type(declaration)?;
                    script_sample_nested_types.push(code);
                    standalone_sample_types.push(declaration.clone());
                }
                IrStatement::RecordDeclaration { name, .. } if Self::is_sample_type_name(name) => {
                    let code = self.generate_nested_type(declaration)?;
                    script_sample_nested_types.push(code);
                    standalone_sample_types.push(declaration.clone());
                }
                IrStatement::ClassDeclaration { .. }
                | IrStatement::InterfaceDeclaration { .. }
                | IrStatement::RecordDeclaration { .. } => {
                    remaining_declarations.push(declaration.clone());
                }
                IrStatement::SampleDeclaration(sample) => {
                    let artifacts = self.generate_sample_declaration_artifacts(sample, true)?;
                    script_sample_nested_types.extend(artifacts);
                    let binding = self.generate_sample_declaration_binding(sample)?;
                    script_sample_bindings.push(binding);
                    script_sample_declarations.push(sample.clone());
                }
                IrStatement::MethodDeclaration { body, .. } => {
                    if let Some(expr) = body.as_ref() {
                        self.collect_sample_declarations_from_expression(
                            expr,
                            &mut script_sample_nested_types,
                            &mut script_sample_declarations,
                        )?;
                    }
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

        let mut processed_statements = Vec::new();
        for statement in script_statements.drain(..) {
            self.collect_sample_declarations_from_statement(
                &statement,
                &mut script_sample_nested_types,
                &mut script_sample_declarations,
            )?;
            processed_statements.push(statement);
        }
        script_statements = processed_statements;

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

        let mut refined_statements = Vec::new();
        for statement in script_statements.drain(..) {
            match Self::base_statement(&statement) {
                IrStatement::RecordDeclaration { name, .. } if Self::is_sample_type_name(name) => {
                    let code = self.generate_record(&statement)?;
                    script_sample_nested_types.push(code);
                }
                IrStatement::ClassDeclaration { name, .. } if Self::is_sample_type_name(name) => {
                    let code = self.generate_class(&statement)?;
                    script_sample_nested_types.push(code);
                }
                _ => refined_statements.push(statement),
            }
        }
        script_statements = refined_statements;

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

            if !script_sample_nested_types.is_empty() {
                for nested in &script_sample_nested_types {
                    Self::push_lines(&mut builder, nested);
                    builder.push_line("");
                }
            }

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
                for binding in &script_sample_bindings {
                    builder.push_line(binding);
                }
                if !script_sample_bindings.is_empty() && !script_statements.is_empty() {
                    builder.push_line("");
                }
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
                let artifacts = self.generate_sample_declaration_artifacts(sample, false)?;
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

        if unit.type_declarations.is_empty() && !standalone_sample_types.is_empty() {
            for declaration in standalone_sample_types.drain(..) {
                let code = match Self::base_statement(&declaration) {
                    IrStatement::ClassDeclaration { .. } => self.generate_class(&declaration)?,
                    IrStatement::RecordDeclaration { .. } => self.generate_record(&declaration)?,
                    _ => continue,
                };
                unit.type_declarations.push(code);
            }
        }

        if unit.type_declarations.is_empty() && !script_sample_declarations.is_empty() {
            for sample in &script_sample_declarations {
                let artifacts = self.generate_sample_declaration_artifacts(sample, false)?;
                unit.type_declarations.extend(artifacts);
            }
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

    fn is_sample_type_name(name: &str) -> bool {
        name.contains("Sample")
    }

    fn generate_nested_type(&mut self, statement: &IrStatement) -> Result<String, CodeGenError> {
        match Self::base_statement(statement) {
            IrStatement::ClassDeclaration { .. } => {
                let mut cloned = statement.clone();
                if let IrStatement::ClassDeclaration { modifiers, .. } = &mut cloned {
                    modifiers.is_static = true;
                }
                self.generate_class(&cloned)
            }
            IrStatement::RecordDeclaration { .. } => {
                let mut cloned = statement.clone();
                if let IrStatement::RecordDeclaration { modifiers, .. } = &mut cloned {
                    modifiers.is_static = true;
                }
                self.generate_record(&cloned)
            }
            _ => Err(CodeGenError::UnsupportedConstruct {
                construct: "Nested type generation対象ではありません".to_string(),
                span: None,
            }),
        }
    }

    fn collect_sample_declarations_from_expression(
        &mut self,
        expression: &IrExpression,
        nested_types: &mut Vec<String>,
        declarations: &mut Vec<IrSampleDeclaration>,
    ) -> Result<(), CodeGenError> {
        match expression {
            IrExpression::Block { statements, .. } => {
                for statement in statements {
                    self.collect_sample_declarations_from_statement(
                        statement,
                        nested_types,
                        declarations,
                    )?;
                }
            }
            IrExpression::MethodCall { receiver, args, .. } => {
                if let Some(receiver) = receiver.as_deref() {
                    self.collect_sample_declarations_from_expression(
                        receiver,
                        nested_types,
                        declarations,
                    )?;
                }
                for arg in args {
                    self.collect_sample_declarations_from_expression(
                        arg,
                        nested_types,
                        declarations,
                    )?;
                }
            }
            IrExpression::FieldAccess { receiver, .. } => {
                self.collect_sample_declarations_from_expression(
                    receiver,
                    nested_types,
                    declarations,
                )?;
            }
            IrExpression::ArrayAccess { array, index, .. } => {
                self.collect_sample_declarations_from_expression(
                    array,
                    nested_types,
                    declarations,
                )?;
                self.collect_sample_declarations_from_expression(
                    index,
                    nested_types,
                    declarations,
                )?;
            }
            IrExpression::Binary { left, right, .. } => {
                self.collect_sample_declarations_from_expression(left, nested_types, declarations)?;
                self.collect_sample_declarations_from_expression(
                    right,
                    nested_types,
                    declarations,
                )?;
            }
            IrExpression::Unary { operand, .. } => {
                self.collect_sample_declarations_from_expression(
                    operand,
                    nested_types,
                    declarations,
                )?;
            }
            IrExpression::Assignment { target, value, .. } => {
                self.collect_sample_declarations_from_expression(
                    target,
                    nested_types,
                    declarations,
                )?;
                self.collect_sample_declarations_from_expression(
                    value,
                    nested_types,
                    declarations,
                )?;
            }
            IrExpression::Conditional {
                condition,
                then_expr,
                else_expr,
                ..
            } => {
                self.collect_sample_declarations_from_expression(
                    condition,
                    nested_types,
                    declarations,
                )?;
                self.collect_sample_declarations_from_expression(
                    then_expr,
                    nested_types,
                    declarations,
                )?;
                self.collect_sample_declarations_from_expression(
                    else_expr,
                    nested_types,
                    declarations,
                )?;
            }
            IrExpression::ArrayCreation {
                dimensions,
                initializer,
                ..
            } => {
                for dimension in dimensions {
                    if let Some(expr) = dimension {
                        self.collect_sample_declarations_from_expression(
                            expr,
                            nested_types,
                            declarations,
                        )?;
                    }
                }
                if let Some(values) = initializer {
                    for expr in values {
                        self.collect_sample_declarations_from_expression(
                            expr,
                            nested_types,
                            declarations,
                        )?;
                    }
                }
            }
            IrExpression::ObjectCreation { args, .. } => {
                for arg in args {
                    self.collect_sample_declarations_from_expression(
                        arg,
                        nested_types,
                        declarations,
                    )?;
                }
            }
            IrExpression::Lambda { body, .. } => {
                self.collect_sample_declarations_from_expression(
                    body.as_ref(),
                    nested_types,
                    declarations,
                )?;
            }
            IrExpression::SequencePipeline { pipeline, .. } => {
                self.collect_sample_declarations_from_sequence_pipeline(
                    pipeline,
                    nested_types,
                    declarations,
                )?;
            }
            IrExpression::Switch {
                discriminant,
                cases,
                ..
            } => {
                self.collect_sample_declarations_from_expression(
                    discriminant,
                    nested_types,
                    declarations,
                )?;
                for case in cases {
                    self.collect_sample_declarations_from_switch_case(
                        case,
                        nested_types,
                        declarations,
                    )?;
                }
            }
            IrExpression::Cast { expr, .. } | IrExpression::InstanceOf { expr, .. } => {
                self.collect_sample_declarations_from_expression(
                    expr.as_ref(),
                    nested_types,
                    declarations,
                )?;
            }
            IrExpression::NullSafeOperation {
                expr,
                operation,
                default_value,
                ..
            } => {
                self.collect_sample_declarations_from_expression(
                    expr.as_ref(),
                    nested_types,
                    declarations,
                )?;
                self.collect_sample_declarations_from_expression(
                    operation.as_ref(),
                    nested_types,
                    declarations,
                )?;
                if let Some(value) = default_value.as_deref() {
                    self.collect_sample_declarations_from_expression(
                        value,
                        nested_types,
                        declarations,
                    )?;
                }
            }
            IrExpression::StringFormat { args, .. }
            | IrExpression::CompletableFuture { args, .. }
            | IrExpression::VirtualThread { args, .. } => {
                for arg in args {
                    self.collect_sample_declarations_from_expression(
                        arg,
                        nested_types,
                        declarations,
                    )?;
                }
            }
            IrExpression::TryWithResources {
                resources, body, ..
            } => {
                for resource in resources {
                    self.collect_sample_declarations_from_expression(
                        &resource.initializer,
                        nested_types,
                        declarations,
                    )?;
                }
                self.collect_sample_declarations_from_expression(
                    body.as_ref(),
                    nested_types,
                    declarations,
                )?;
            }
            _ => {}
        }

        Ok(())
    }

    fn collect_sample_declarations_from_statement(
        &mut self,
        statement: &IrStatement,
        nested_types: &mut Vec<String>,
        declarations: &mut Vec<IrSampleDeclaration>,
    ) -> Result<(), CodeGenError> {
        let base = Self::base_statement(statement);
        match base {
            IrStatement::SampleDeclaration(sample) => {
                let artifacts = self.generate_sample_declaration_artifacts(sample, true)?;
                nested_types.extend(artifacts);
                declarations.push(sample.clone());
            }
            IrStatement::Block { statements, .. } => {
                for stmt in statements {
                    self.collect_sample_declarations_from_statement(
                        stmt,
                        nested_types,
                        declarations,
                    )?;
                }
            }
            IrStatement::If {
                condition,
                then_stmt,
                else_stmt,
                ..
            } => {
                self.collect_sample_declarations_from_expression(
                    condition,
                    nested_types,
                    declarations,
                )?;
                self.collect_sample_declarations_from_statement(
                    then_stmt.as_ref(),
                    nested_types,
                    declarations,
                )?;
                if let Some(else_branch) = else_stmt.as_deref() {
                    self.collect_sample_declarations_from_statement(
                        else_branch,
                        nested_types,
                        declarations,
                    )?;
                }
            }
            IrStatement::While {
                condition, body, ..
            } => {
                self.collect_sample_declarations_from_expression(
                    condition,
                    nested_types,
                    declarations,
                )?;
                self.collect_sample_declarations_from_statement(
                    body.as_ref(),
                    nested_types,
                    declarations,
                )?;
            }
            IrStatement::ForEach { iterable, body, .. } => {
                self.collect_sample_declarations_from_expression(
                    iterable,
                    nested_types,
                    declarations,
                )?;
                self.collect_sample_declarations_from_statement(
                    body.as_ref(),
                    nested_types,
                    declarations,
                )?;
            }
            IrStatement::For {
                init,
                condition,
                update,
                body,
                ..
            } => {
                if let Some(init_stmt) = init.as_deref() {
                    self.collect_sample_declarations_from_statement(
                        init_stmt,
                        nested_types,
                        declarations,
                    )?;
                }
                if let Some(cond) = condition {
                    self.collect_sample_declarations_from_expression(
                        cond,
                        nested_types,
                        declarations,
                    )?;
                }
                if let Some(update_expr) = update {
                    self.collect_sample_declarations_from_expression(
                        update_expr,
                        nested_types,
                        declarations,
                    )?;
                }
                self.collect_sample_declarations_from_statement(
                    body.as_ref(),
                    nested_types,
                    declarations,
                )?;
            }
            IrStatement::Switch {
                discriminant,
                cases,
                ..
            } => {
                self.collect_sample_declarations_from_expression(
                    discriminant,
                    nested_types,
                    declarations,
                )?;
                for case in cases {
                    self.collect_sample_declarations_from_switch_case(
                        case,
                        nested_types,
                        declarations,
                    )?;
                }
            }
            IrStatement::Try {
                body,
                catch_clauses,
                finally_block,
                ..
            } => {
                self.collect_sample_declarations_from_statement(
                    body.as_ref(),
                    nested_types,
                    declarations,
                )?;
                for clause in catch_clauses {
                    self.collect_sample_declarations_from_statement(
                        &clause.body,
                        nested_types,
                        declarations,
                    )?;
                }
                if let Some(finally_stmt) = finally_block.as_deref() {
                    self.collect_sample_declarations_from_statement(
                        finally_stmt,
                        nested_types,
                        declarations,
                    )?;
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
                    self.collect_sample_declarations_from_expression(
                        &resource.initializer,
                        nested_types,
                        declarations,
                    )?;
                }
                self.collect_sample_declarations_from_statement(
                    body.as_ref(),
                    nested_types,
                    declarations,
                )?;
                for clause in catch_clauses {
                    self.collect_sample_declarations_from_statement(
                        &clause.body,
                        nested_types,
                        declarations,
                    )?;
                }
                if let Some(finally_stmt) = finally_block.as_deref() {
                    self.collect_sample_declarations_from_statement(
                        finally_stmt,
                        nested_types,
                        declarations,
                    )?;
                }
            }
            IrStatement::VariableDeclaration { initializer, .. } => {
                if let Some(init) = initializer {
                    self.collect_sample_declarations_from_expression(
                        init,
                        nested_types,
                        declarations,
                    )?;
                }
            }
            IrStatement::Expression { expr, .. } => {
                self.collect_sample_declarations_from_expression(expr, nested_types, declarations)?;
            }
            IrStatement::Return { value, .. } => {
                if let Some(expr) = value {
                    self.collect_sample_declarations_from_expression(
                        expr,
                        nested_types,
                        declarations,
                    )?;
                }
            }
            IrStatement::Throw { expr, .. } => {
                self.collect_sample_declarations_from_expression(expr, nested_types, declarations)?;
            }
            _ => {}
        }

        Ok(())
    }

    fn collect_sample_declarations_from_sequence_pipeline(
        &mut self,
        pipeline: &SequencePipeline,
        nested_types: &mut Vec<String>,
        declarations: &mut Vec<IrSampleDeclaration>,
    ) -> Result<(), CodeGenError> {
        self.collect_sample_declarations_from_sequence_source(
            &pipeline.source,
            nested_types,
            declarations,
        )?;

        for stage in &pipeline.stages {
            self.collect_sample_declarations_from_sequence_stage(
                stage,
                nested_types,
                declarations,
            )?;
        }

        if let Some(terminal) = pipeline.terminal.as_ref() {
            self.collect_sample_declarations_from_sequence_terminal(
                terminal,
                nested_types,
                declarations,
            )?;
        }

        Ok(())
    }

    fn collect_sample_declarations_from_sequence_source(
        &mut self,
        source: &SequenceSource,
        nested_types: &mut Vec<String>,
        declarations: &mut Vec<IrSampleDeclaration>,
    ) -> Result<(), CodeGenError> {
        match source {
            SequenceSource::Collection { expr, .. }
            | SequenceSource::Array { expr, .. }
            | SequenceSource::JavaStream { expr, .. } => {
                self.collect_sample_declarations_from_expression(
                    expr.as_ref(),
                    nested_types,
                    declarations,
                )?;
            }
            SequenceSource::ListLiteral { elements, .. } => {
                for element in elements {
                    self.collect_sample_declarations_from_expression(
                        element,
                        nested_types,
                        declarations,
                    )?;
                }
            }
        }
        Ok(())
    }

    fn collect_sample_declarations_from_sequence_stage(
        &mut self,
        stage: &SequenceStage,
        nested_types: &mut Vec<String>,
        declarations: &mut Vec<IrSampleDeclaration>,
    ) -> Result<(), CodeGenError> {
        match stage {
            SequenceStage::Map { lambda, .. } | SequenceStage::FlatMap { lambda, .. } => {
                self.collect_sample_declarations_from_expression(
                    lambda.as_ref(),
                    nested_types,
                    declarations,
                )?;
            }
            SequenceStage::Filter { predicate, .. } => {
                self.collect_sample_declarations_from_expression(
                    predicate.as_ref(),
                    nested_types,
                    declarations,
                )?;
            }
            SequenceStage::Take { count, .. } | SequenceStage::Drop { count, .. } => {
                self.collect_sample_declarations_from_expression(
                    count.as_ref(),
                    nested_types,
                    declarations,
                )?;
            }
            SequenceStage::Sorted { comparator, .. } => {
                if let Some(comparator) = comparator.as_deref() {
                    self.collect_sample_declarations_from_expression(
                        comparator,
                        nested_types,
                        declarations,
                    )?;
                }
            }
        }
        Ok(())
    }

    fn collect_sample_declarations_from_sequence_terminal(
        &mut self,
        terminal: &SequenceTerminal,
        nested_types: &mut Vec<String>,
        declarations: &mut Vec<IrSampleDeclaration>,
    ) -> Result<(), CodeGenError> {
        if let Some(adapter) = terminal.canonical_adapter.as_deref() {
            self.collect_sample_declarations_from_expression(adapter, nested_types, declarations)?;
        }

        match &terminal.kind {
            SequenceTerminalKind::Fold {
                initial,
                accumulator,
            } => {
                self.collect_sample_declarations_from_expression(
                    initial.as_ref(),
                    nested_types,
                    declarations,
                )?;
                self.collect_sample_declarations_from_expression(
                    accumulator.as_ref(),
                    nested_types,
                    declarations,
                )?;
            }
            SequenceTerminalKind::Reduce { accumulator } => {
                self.collect_sample_declarations_from_expression(
                    accumulator.as_ref(),
                    nested_types,
                    declarations,
                )?;
            }
            SequenceTerminalKind::GroupBy { key_selector } => {
                self.collect_sample_declarations_from_expression(
                    key_selector.as_ref(),
                    nested_types,
                    declarations,
                )?;
            }
            SequenceTerminalKind::Associate { pair_selector } => {
                self.collect_sample_declarations_from_expression(
                    pair_selector.as_ref(),
                    nested_types,
                    declarations,
                )?;
            }
            SequenceTerminalKind::ForEach { action } => {
                self.collect_sample_declarations_from_expression(
                    action.as_ref(),
                    nested_types,
                    declarations,
                )?;
            }
            SequenceTerminalKind::ToList
            | SequenceTerminalKind::ToSet
            | SequenceTerminalKind::Count
            | SequenceTerminalKind::Sum => {}
        }

        Ok(())
    }

    fn collect_sample_declarations_from_switch_case(
        &mut self,
        case: &IrSwitchCase,
        nested_types: &mut Vec<String>,
        declarations: &mut Vec<IrSampleDeclaration>,
    ) -> Result<(), CodeGenError> {
        for label in &case.labels {
            self.collect_sample_declarations_from_case_label(label, nested_types, declarations)?;
        }

        if let Some(guard) = case.guard.as_ref() {
            self.collect_sample_declarations_from_expression(guard, nested_types, declarations)?;
        }

        self.collect_sample_declarations_from_expression(&case.body, nested_types, declarations)?;

        Ok(())
    }

    fn collect_sample_declarations_from_case_label(
        &mut self,
        label: &IrCaseLabel,
        nested_types: &mut Vec<String>,
        declarations: &mut Vec<IrSampleDeclaration>,
    ) -> Result<(), CodeGenError> {
        if let IrCaseLabel::Range { lower, upper, .. } = label {
            self.collect_sample_declarations_from_expression(
                lower.as_ref(),
                nested_types,
                declarations,
            )?;
            self.collect_sample_declarations_from_expression(
                upper.as_ref(),
                nested_types,
                declarations,
            )?;
        }
        Ok(())
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
        self.record_component_types.clear();
        self.local_type_overrides.clear();
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
                .entry(key.clone())
                .or_insert_with(HashSet::new)
                .extend(component_names.iter().cloned());
            let type_entry = self
                .record_component_types
                .entry(key.clone())
                .or_insert_with(HashMap::new);
            for component in components {
                type_entry
                    .entry(component.name.clone())
                    .or_insert_with(|| component.java_type.clone());
            }
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
        if let IrExpression::Block { statements, .. } = expr {
            for statement in statements {
                self.collect_method_locals_from_statement(statement, locals);
            }
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
        if let Some(overridden) = self.overridden_type(receiver) {
            if self.record_component_type_matches(overridden, field_name) {
                return true;
            }
        }

        let Some(java_type) = Self::expression_java_type(receiver) else {
            return false;
        };

        self.record_component_type_matches(java_type, field_name)
    }

    fn overridden_type(&self, expr: &IrExpression) -> Option<&JavaType> {
        if let IrExpression::Identifier { name, .. } = expr {
            for scope in self.local_type_overrides.iter().rev() {
                if let Some(java_type) = scope.get(name) {
                    return Some(java_type);
                }
            }
        }
        None
    }

    fn record_component_type_matches(&self, java_type: &JavaType, field_name: &str) -> bool {
        let JavaType::Reference { name, .. } = java_type else {
            return false;
        };
        self.record_component_type_lookup(name, field_name)
            .is_some()
    }

    fn record_component_type_lookup(&self, type_name: &str, field_name: &str) -> Option<JavaType> {
        let mut candidates = Vec::new();
        candidates.push(type_name);
        if let Some(simple) = type_name.rsplit('.').next() {
            candidates.push(simple);
        }
        if let Some(simple) = type_name.rsplit('$').next() {
            candidates.push(simple);
        }

        for candidate in candidates {
            if let Some(components) = self.record_component_types.get(candidate) {
                if let Some(java_type) = components.get(field_name) {
                    return Some(java_type.clone());
                }
            }
        }

        None
    }

    fn with_local_type_override<F, R>(
        &mut self,
        variable: &str,
        ty: JavaType,
        f: F,
    ) -> Result<R, CodeGenError>
    where
        F: FnOnce(&mut Self) -> Result<R, CodeGenError>,
    {
        let mut scope = HashMap::new();
        scope.insert(variable.to_string(), ty);
        self.local_type_overrides.push(scope);
        let result = f(self);
        self.local_type_overrides.pop();
        result
    }

    fn field_access_component_type(
        &self,
        receiver: &IrExpression,
        field_name: &str,
    ) -> Option<JavaType> {
        let receiver_type = self.expression_java_type_with_overrides(receiver)?;
        let JavaType::Reference { name, .. } = receiver_type else {
            return None;
        };
        self.record_component_type_lookup(&name, field_name)
    }

    fn expression_java_type_with_overrides(&self, expr: &IrExpression) -> Option<JavaType> {
        let base_type = Self::expression_java_type(expr).cloned();

        if let Some(overridden) = self.overridden_type(expr) {
            if base_type
                .as_ref()
                .map_or(true, |ty| Self::is_object_reference(ty))
            {
                return Some(overridden.clone());
            }
        }

        if let Some(java_type) = base_type {
            if let IrExpression::FieldAccess {
                receiver,
                field_name,
                ..
            } = expr
            {
                if Self::is_object_reference(&java_type) {
                    if let Some(component) = self.field_access_component_type(receiver, field_name)
                    {
                        return Some(component);
                    }
                }
            }
            return Some(java_type);
        }

        if let IrExpression::FieldAccess {
            receiver,
            field_name,
            ..
        } = expr
        {
            return self.field_access_component_type(receiver, field_name);
        }

        None
    }

    fn is_object_reference(java_type: &JavaType) -> bool {
        matches!(
            java_type,
            JavaType::Reference { name, .. }
                if name == "java.lang.Object" || name == "Object"
        )
    }

    fn is_boolean_like_type(java_type: &JavaType) -> bool {
        match java_type {
            JavaType::Primitive(name) => name == "boolean",
            JavaType::Reference { name, .. } => matches!(
                name.as_str(),
                "Boolean" | "java.lang.Boolean" | "Object" | "java.lang.Object"
            ),
            _ => false,
        }
    }

    fn should_fallback_boolean_switch_expression(
        &self,
        discriminant: &IrExpression,
        cases: &[IrSwitchCase],
    ) -> bool {
        if self.extract_boolean_cases(cases).is_none() {
            return false;
        }

        match self.expression_java_type_with_overrides(discriminant) {
            Some(java_type) => Self::is_boolean_like_type(&java_type),
            None => true,
        }
    }

    fn extract_boolean_cases(
        &self,
        cases: &[IrSwitchCase],
    ) -> Option<(
        Option<IrStatement>,
        Option<IrStatement>,
        Option<IrStatement>,
    )> {
        let mut true_case = None;
        let mut false_case = None;
        let mut default_case = None;

        for case in cases {
            if case.guard.is_some() {
                return None;
            }
            if Self::is_default_only_case(case) {
                if default_case.is_some() {
                    return None;
                }
                default_case = Some(Self::expression_to_statement(&case.body));
                continue;
            }

            let mut handled = false;
            for label in &case.labels {
                match label {
                    IrCaseLabel::Literal(Literal::Boolean(value)) => {
                        let stmt = Self::expression_to_statement(&case.body);
                        if *value {
                            if true_case.is_some() {
                                return None;
                            }
                            true_case = Some(stmt);
                        } else {
                            if false_case.is_some() {
                                return None;
                            }
                            false_case = Some(stmt);
                        }
                        handled = true;
                    }
                    _ => return None,
                }
            }

            if !handled {
                return None;
            }
        }

        if true_case.is_none() && false_case.is_none() && default_case.is_none() {
            return None;
        }

        Some((true_case, false_case, default_case))
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
