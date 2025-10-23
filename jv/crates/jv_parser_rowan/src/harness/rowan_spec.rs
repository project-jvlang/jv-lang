#![cfg_attr(not(feature = "rowan-parser"), allow(dead_code))]

use std::env;
use std::path::PathBuf;
use std::process::ExitCode;

#[cfg(not(feature = "rowan-parser"))]
fn main() -> ExitCode {
    eprintln!("The rowan_spec_harness binary requires the 'rowan-parser' feature to be enabled.");
    ExitCode::FAILURE
}

#[cfg(feature = "rowan-parser")]
fn main() -> ExitCode {
    match execute() {
        Ok(exit_code) => exit_code,
        Err(exit_code) => exit_code,
    }
}

#[cfg(feature = "rowan-parser")]
fn execute() -> Result<ExitCode, ExitCode> {
    use jv_parser_rowan::verification::{self, HarnessReport};

    let mut args = env::args().skip(1);
    let mut spec_root: Option<PathBuf> = None;
    let mut report_path: Option<PathBuf> = None;
    let mut show_help = false;

    while let Some(arg) = args.next() {
        match arg.as_str() {
            flag if flag.starts_with("--spec-root=") => {
                let value = &flag["--spec-root=".len()..];
                if value.is_empty() {
                    eprintln!("error: '--spec-root=' requires a non-empty path\n{USAGE}");
                    return Err(ExitCode::FAILURE);
                }
                spec_root = Some(PathBuf::from(value));
            }
            flag if flag.starts_with("--spec=") => {
                let value = &flag["--spec=".len()..];
                if value.is_empty() {
                    eprintln!("error: '--spec=' requires a non-empty path\n{USAGE}");
                    return Err(ExitCode::FAILURE);
                }
                spec_root = Some(PathBuf::from(value));
            }
            flag if flag.starts_with("--report=") => {
                let value = &flag["--report=".len()..];
                if value.is_empty() {
                    eprintln!("error: '--report=' requires a non-empty path\n{USAGE}");
                    return Err(ExitCode::FAILURE);
                }
                report_path = Some(PathBuf::from(value));
            }
            "--spec-root" | "--spec" => {
                let Some(value) = args.next() else {
                    eprintln!("error: {arg} requires a path argument\n{USAGE}");
                    return Err(ExitCode::FAILURE);
                };
                spec_root = Some(PathBuf::from(value));
            }
            "--report" | "-o" => {
                let Some(value) = args.next() else {
                    eprintln!("error: {arg} requires a path argument\n{USAGE}");
                    return Err(ExitCode::FAILURE);
                };
                report_path = Some(PathBuf::from(value));
            }
            "--help" | "-h" => {
                show_help = true;
            }
            unknown => {
                eprintln!("error: unrecognized argument '{unknown}'\n{USAGE}");
                return Err(ExitCode::FAILURE);
            }
        }
    }

    if show_help {
        println!("{USAGE}");
        return Ok(ExitCode::SUCCESS);
    }

    let spec_root = spec_root.unwrap_or_else(default_spec_root);
    let report = match verification::run_spec_directory(&spec_root) {
        Ok(report) => report,
        Err(error) => {
            eprintln!("error: {error}");
            return Err(ExitCode::FAILURE);
        }
    };

    let workspace_root = workspace_root();
    let default_report_path = HarnessReport::default_report_path(&workspace_root);
    let mut report_path = report_path.unwrap_or(default_report_path);
    if !report_path.is_absolute() {
        report_path = workspace_root.join(report_path);
    }

    if let Err(error) = report.write_report(&report_path) {
        eprintln!("error: {error}");
        return Err(ExitCode::FAILURE);
    }

    let failures = report.failures();
    let total = report.fixtures.len();
    let spec_root_display = report.spec_root.clone();
    let report_display = display_relative(&report_path, &workspace_root);

    println!("Rowan specification verification");
    println!("  Spec root : {}", spec_root_display);
    println!("  Report    : {}", report_display);
    println!("  Fixtures  : {total}");
    println!("  Failures  : {failures}");

    if failures > 0 {
        println!("\nViolation details:");
        for fixture in &report.fixtures {
            if fixture.violations.is_empty() {
                continue;
            }
            println!(" - {}", fixture.spec);
            if let Some(description) = &fixture.description {
                println!("     {}", description);
            }
            for violation in &fixture.violations {
                println!("     {}: {}", violation.rule, violation.message);
            }
        }
        Err(ExitCode::FAILURE)
    } else {
        Ok(ExitCode::SUCCESS)
    }
}

#[cfg(feature = "rowan-parser")]
fn default_spec_root() -> PathBuf {
    PathBuf::from("tests/parser_rowan_specs")
}

#[cfg(feature = "rowan-parser")]
fn workspace_root() -> PathBuf {
    let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    manifest
        .ancestors()
        .nth(3)
        .map(PathBuf::from)
        .expect("workspace root to be discoverable from manifest path")
}

#[cfg(feature = "rowan-parser")]
fn display_relative(path: &std::path::Path, workspace_root: &PathBuf) -> String {
    path.strip_prefix(workspace_root)
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/")
}

const USAGE: &str = "Usage: rowan_spec_harness [--spec-root <path>] [--report <path>] [--help]";
