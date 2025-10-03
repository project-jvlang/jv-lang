use crate::builder::{JavaCompilationUnit, JavaSourceBuilder};
use crate::config::JavaCodeGenConfig;
use crate::error::CodeGenError;
use jv_ast::{BinaryOp, CallArgumentStyle, Literal, SequenceDelimiter, UnaryOp};
use jv_ir::{
    CompletableFutureOp, IrCaseLabel, IrCatchClause, IrDeconstructionComponent,
    IrDeconstructionPattern, IrExpression, IrForEachKind, IrForLoopMetadata, IrImplicitWhenEnd,
    IrModifiers, IrNumericRangeLoop, IrParameter, IrProgram, IrRecordComponent, IrResource,
    IrSampleDeclaration, IrStatement, IrSwitchCase, IrTypeParameter, IrVisibility, JavaType,
    MethodOverload, UtilityClass, VirtualThreadOp,
};
use std::collections::HashMap;

mod declarations;
mod expressions;
mod helpers;
mod sample;
mod statements;
mod targeting;

use targeting::TargetedJavaEmitter;

pub struct JavaCodeGenerator {
    imports: HashMap<String, String>,
    config: JavaCodeGenConfig,
    targeting: TargetedJavaEmitter,
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
        }
    }

    pub fn generate_compilation_unit(
        &mut self,
        program: &IrProgram,
    ) -> Result<JavaCompilationUnit, CodeGenError> {
        self.reset();

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

        let has_entry_method = script_methods.iter().any(Self::is_entry_point_method);
        let needs_wrapper = !script_statements.is_empty() || !has_entry_method;

        if !script_statements.is_empty() || !script_methods.is_empty() {
            let script_class = &self.config.script_main_class;
            let mut builder = self.builder();
            builder.push_line(&format!("public final class {} {{", script_class));
            builder.indent();

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
}
