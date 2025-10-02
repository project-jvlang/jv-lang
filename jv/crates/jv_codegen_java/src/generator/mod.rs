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

        let mut script_statements = Vec::new();
        let mut script_methods = Vec::new();
        let mut remaining_declarations = Vec::new();

        for declaration in &program.type_declarations {
            match declaration {
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

        if !script_statements.is_empty() || !script_methods.is_empty() {
            let script_class = &self.config.script_main_class;
            let mut builder = self.builder();
            builder.push_line(&format!("public final class {} {{", script_class));
            builder.indent();

            builder.push_line("public static void main(String[] args) throws Exception {");
            builder.indent();
            for statement in &script_statements {
                let code = self.generate_statement(statement)?;
                Self::push_lines(&mut builder, &code);
            }
            builder.dedent();
            builder.push_line("}");

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
            if let IrStatement::SampleDeclaration(sample) = &declaration {
                let artifacts = self.generate_sample_declaration_artifacts(sample)?;
                unit.type_declarations.extend(artifacts);
                continue;
            }

            let code = match &declaration {
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
}
