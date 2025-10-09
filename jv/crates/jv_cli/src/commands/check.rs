use anyhow::{Context, Result};
use std::fs;
use std::path::Path;

use jv_checker::diagnostics::{
    collect_raw_type_diagnostics, from_check_error, from_parse_error, from_transform_error,
    DiagnosticStrategy,
};
use jv_checker::TypeChecker;
use jv_ir::transform_program;
use jv_parser::Parser as JvParser;

use crate::{format_tooling_diagnostic, tooling_failure};

/// Execute the `jv check` workflow against a single input file.
pub fn run(input: &str) -> Result<()> {
    println!("Checking: {}", input);

    if !Path::new(input).exists() {
        anyhow::bail!("Input file '{}' not found", input);
    }

    let source =
        fs::read_to_string(input).with_context(|| format!("Failed to read file: {}", input))?;

    let program = match JvParser::parse(&source) {
        Ok(program) => program,
        Err(error) => {
            if let Some(diagnostic) = from_parse_error(&error) {
                return Err(tooling_failure(
                    Path::new(input),
                    diagnostic.with_strategy(DiagnosticStrategy::Deferred),
                ));
            }
            return Err(anyhow::anyhow!("Parser error: {:?}", error));
        }
    };

    let mut type_checker = TypeChecker::new();
    match type_checker.check_program(&program) {
        Ok(_) => {
            println!("✓ No errors found");
        }
        Err(errors) => {
            if let Some(diagnostic) = errors.iter().find_map(from_check_error) {
                return Err(tooling_failure(
                    Path::new(input),
                    diagnostic.with_strategy(DiagnosticStrategy::Deferred),
                ));
            }
            println!("Found {} error(s):", errors.len());
            for error in &errors {
                println!("  Error: {}", error);
            }
            return Err(anyhow::anyhow!("Type checking failed"));
        }
    }

    let warnings = if let Some(normalized) = type_checker.normalized_program() {
        let cloned = normalized.clone();
        type_checker.check_null_safety(&cloned, None)
    } else {
        type_checker.check_null_safety(&program, None)
    };
    if !warnings.is_empty() {
        println!("Null safety warnings:");
        for warning in &warnings {
            println!("  Warning: {}", warning);
        }
    }

    let normalized_program = type_checker.take_normalized_program().unwrap_or(program);
    let ir_program = match transform_program(normalized_program) {
        Ok(ir) => ir,
        Err(error) => {
            if let Some(diagnostic) = from_transform_error(&error) {
                return Err(tooling_failure(
                    Path::new(input),
                    diagnostic.with_strategy(DiagnosticStrategy::Deferred),
                ));
            }
            return Err(anyhow::anyhow!("IR transformation error: {:?}", error));
        }
    };

    let raw_type_diagnostics = collect_raw_type_diagnostics(&ir_program);
    if !raw_type_diagnostics.is_empty() {
        println!("Raw type diagnostics:");
        for diagnostic in raw_type_diagnostics {
            let diagnostic = diagnostic.with_strategy(DiagnosticStrategy::Deferred);
            println!(
                "{}",
                format_tooling_diagnostic(Path::new(input), &diagnostic)
            );
        }
    }

    println!("Check completed successfully!");
    Ok(())
}
