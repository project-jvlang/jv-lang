use anyhow::{Context, Result};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;

use jv_checker::diagnostics::messages::{helper_label, helper_recommendation};
use jv_checker::diagnostics::{
    collect_raw_type_diagnostics, from_check_error, from_parse_error, from_transform_error,
    DiagnosticStrategy,
};
use jv_checker::inference::conversions::HelperSpec;
use jv_checker::inference::diagnostics::conversion_diagnostic;
use jv_checker::InferenceTelemetry;
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
    let telemetry_snapshot = type_checker.telemetry().clone();
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

    let summary_lines = conversion_summary_lines(&telemetry_snapshot);
    if !summary_lines.is_empty() {
        println!("Conversion telemetry:");
        for line in summary_lines {
            println!("  {}", line);
        }
    }

    let helper_messages = helper_recommendation_lines(&telemetry_snapshot);
    if !helper_messages.is_empty() {
        println!("Helper recommendations:");
        for message in helper_messages {
            println!("  {}", message);
        }
    }

    let conversion_reports = conversion_diagnostic_blocks(Path::new(input), &telemetry_snapshot);
    if !conversion_reports.is_empty() {
        println!("Implicit conversion diagnostics:");
        for report in conversion_reports {
            println!("{report}");
        }
    }

    println!("Check completed successfully!");
    Ok(())
}

fn conversion_summary_lines(telemetry: &InferenceTelemetry) -> Vec<String> {
    let conversion_total = telemetry.widening_conversions
        + telemetry.boxing_conversions
        + telemetry.unboxing_conversions
        + telemetry.string_conversions
        + telemetry.method_invocation_conversions;
    let helper_activity = telemetry.conversion_catalog_hits + telemetry.conversion_catalog_misses;

    if conversion_total == 0 && helper_activity == 0 && telemetry.nullable_guards_generated == 0 {
        return Vec::new();
    }

    let mut lines = Vec::new();
    lines.push(format!(
        "conversions – widening: {widening}, boxing: {boxing}, unboxing: {unboxing}, string: {string}, method: {method}",
        widening = telemetry.widening_conversions,
        boxing = telemetry.boxing_conversions,
        unboxing = telemetry.unboxing_conversions,
        string = telemetry.string_conversions,
        method = telemetry.method_invocation_conversions,
    ));
    lines.push(format!(
        "events recorded: {}",
        telemetry.conversion_events.len()
    ));
    if telemetry.nullable_guards_generated > 0 {
        lines.push(format!(
            "nullable guards inserted: {}",
            telemetry.nullable_guards_generated
        ));
    }
    lines.push(format!(
        "helper catalog – hits: {}, misses: {}",
        telemetry.conversion_catalog_hits, telemetry.conversion_catalog_misses
    ));
    lines
}

fn helper_recommendation_lines(telemetry: &InferenceTelemetry) -> Vec<String> {
    if telemetry.catalog_hits.is_empty() {
        return Vec::new();
    }

    let mut counts: HashMap<String, (HelperSpec, usize)> = HashMap::new();
    for helper in &telemetry.catalog_hits {
        let label = helper_label(helper);
        counts
            .entry(label)
            .and_modify(|entry| entry.1 += 1)
            .or_insert((helper.clone(), 1));
    }

    let mut entries: Vec<_> = counts.into_iter().collect();
    entries.sort_by(|a, b| a.0.cmp(&b.0));
    entries
        .into_iter()
        .map(|(_, (helper, count))| helper_recommendation(&helper, count))
        .collect()
}

fn conversion_diagnostic_blocks(input_path: &Path, telemetry: &InferenceTelemetry) -> Vec<String> {
    if telemetry.conversion_events.is_empty() {
        return Vec::new();
    }

    let mut seen = HashSet::new();
    let mut reports = Vec::new();

    for event in &telemetry.conversion_events {
        let helper_key = event
            .helper_method
            .as_ref()
            .map(|helper| helper_label(helper))
            .unwrap_or_else(|| "none".to_string());
        let key = format!(
            "{}|{}|{}|{}",
            event.kind.label(),
            event.from_type.describe(),
            event.to_type.describe(),
            helper_key
        );
        if !seen.insert(key) {
            continue;
        }

        if let Some(mut diagnostic) = conversion_diagnostic(event) {
            diagnostic = diagnostic.with_strategy(DiagnosticStrategy::Deferred);
            if diagnostic.learning_hints.is_none() {
                if let Some(helper) = event.helper_method.as_ref() {
                    let hint = format!(
                        "Implicit helper: {}. Call it explicitly if you prefer explicit conversions.",
                        helper_label(helper)
                    );
                    diagnostic = diagnostic.with_learning_hint(hint);
                }
            }
            let rendered = format_tooling_diagnostic(input_path, &diagnostic);
            reports.push(rendered);
        }
    }

    reports
}
