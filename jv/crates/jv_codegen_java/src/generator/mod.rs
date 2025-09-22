use crate::builder::{JavaCompilationUnit, JavaSourceBuilder};
use crate::config::JavaCodeGenConfig;
use crate::error::CodeGenError;
use jv_ast::{BinaryOp, Literal, UnaryOp};
use jv_ir::{
    CompletableFutureOp, IrCaseLabel, IrCatchClause, IrExpression, IrModifiers, IrParameter,
    IrProgram, IrRecordComponent, IrResource, IrSampleDeclaration, IrStatement, IrSwitchCase,
    IrTypeParameter, IrVisibility, JavaType, MethodOverload, UtilityClass, VirtualThreadOp,
};
use std::collections::HashMap;

mod declarations;
mod expressions;
mod helpers;
mod sample;
mod statements;

pub struct JavaCodeGenerator {
    imports: HashMap<String, String>,
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

        for declaration in &program.type_declarations {
            if let IrStatement::SampleDeclaration(sample) = declaration {
                let artifacts = self.generate_sample_declaration_artifacts(sample)?;
                unit.type_declarations.extend(artifacts);
                continue;
            }

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
