use anyhow::Result;
use clap::Parser;
use std::fs;
use std::io::{self, Read};
use std::path::Path;
use std::time::{SystemTime, UNIX_EPOCH};

use jv_cli::pipeline::run_program;

#[derive(Parser)]
#[command(name = "jvx", about = "Quickly execute a jv file or snippet")]
struct JvxCli {
    /// Path to .jv file, '-' for stdin, or inline snippet
    target: String,
    /// Arguments passed to the program
    #[arg(trailing_var_arg = true)]
    args: Vec<String>,
}

fn main() -> Result<()> {
    let cli = JvxCli::parse();
    exec_quick(&cli.target, cli.args)
}

fn exec_quick(target: &str, args: Vec<String>) -> Result<()> {
    if target == "-" {
        let mut buf = String::new();
        io::stdin().read_to_string(&mut buf)?;
        return run_snippet(&buf, &args);
    }

    if Path::new(target).exists() {
        return run_jv_file(target, &args);
    }

    run_snippet(target, &args)
}

fn run_snippet(source: &str, args: &[String]) -> Result<()> {
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis();
    let temp_file = std::env::temp_dir().join(format!("jvx-snippet-{}.jv", timestamp));
    fs::write(&temp_file, source)?;

    let result = run_jv_file(&temp_file, args);
    let _ = fs::remove_file(temp_file);
    result
}

fn run_jv_file<P: AsRef<Path>>(input: P, args: &[String]) -> Result<()> {
    run_program(input, args)
}
