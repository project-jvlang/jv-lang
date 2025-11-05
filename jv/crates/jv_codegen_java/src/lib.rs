pub mod builder;
pub mod config;
pub mod error;
pub mod generator;
pub mod java21;
pub mod record;
pub mod target_version;
pub mod types;

pub use builder::{JavaCompilationUnit, JavaSourceBuilder};
pub use config::JavaCodeGenConfig;
pub use error::CodeGenError;
pub use generator::JavaCodeGenerator;
pub use jv_pm::JavaTarget;
pub use target_version::TargetedJavaEmitter;
pub use types::{
    ImportManager, Java25FeatureGenerator, JavaTypeMapper, NullSafetyGenerator, StandardImport,
};

use jv_ir::IrProgram;

/// Generate a Java compilation unit from the supplied IR using default configuration.
pub fn generate_java_code(program: &IrProgram) -> Result<JavaCompilationUnit, CodeGenError> {
    generate_java_code_with_config(program, JavaCodeGenConfig::default())
}

/// Generate a Java compilation unit from the supplied IR and configuration.
pub fn generate_java_code_with_config(
    program: &IrProgram,
    config: JavaCodeGenConfig,
) -> Result<JavaCompilationUnit, CodeGenError> {
    let mut generator = JavaCodeGenerator::with_config(config);
    generator.generate_compilation_unit(program)
}

/// Render Java source string from IR using the default configuration.
pub fn generate_java_source(program: &IrProgram) -> Result<String, CodeGenError> {
    generate_java_source_with_config(program, &JavaCodeGenConfig::default())
}

/// Render Java source string from IR using the provided configuration.
pub fn generate_java_source_with_config(
    program: &IrProgram,
    config: &JavaCodeGenConfig,
) -> Result<String, CodeGenError> {
    let unit = generate_java_code_with_config(program, config.clone())?;
    Ok(unit.to_source(config))
}

#[cfg(test)]
mod tests;
