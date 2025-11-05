use std::fmt::Write as _;

use anyhow::{Context, Result, anyhow};
use clap::{Parser, Subcommand};
use jv_pm::{ResolverAlgorithmKind, ResolverDispatcher, ResolverStrategyInfo, StrategyStability};

#[derive(Parser, Debug)]
#[command(name = "jvpm")]
#[command(about = "jv package manager helper", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Inspect resolver strategies
    #[command(subcommand)]
    Resolver(ResolverCommand),
}

#[derive(Subcommand, Debug)]
enum ResolverCommand {
    /// List every registered resolver strategy
    List {
        /// Emit machine-readable JSON instead of a human table
        #[arg(long)]
        json: bool,
    },
    /// Show extended metadata for a single strategy
    Info {
        /// Strategy name or alias (see `jv resolver list`)
        name: String,
        /// Emit machine-readable JSON instead of a textual block
        #[arg(long)]
        json: bool,
    },
}

fn main() {
    if let Err(error) = real_main() {
        eprintln!("{error}");
        std::process::exit(1);
    }
}

fn real_main() -> Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Commands::Resolver(command) => handle_resolver_command(command),
    }
}

fn handle_resolver_command(command: ResolverCommand) -> Result<()> {
    let dispatcher = ResolverDispatcher::with_default_strategies();
    match command {
        ResolverCommand::List { json } => {
            let strategies = dispatcher.list_strategies();
            if json {
                let payload = serde_json::to_string_pretty(&strategies)
                    .context("failed to serialize resolver metadata")?;
                println!("{payload}");
                return Ok(());
            }

            let table = render_strategy_table(&strategies);
            println!("{table}");
            Ok(())
        }
        ResolverCommand::Info { name, json } => {
            let info = dispatcher
                .strategy_info(&name)
                .ok_or_else(|| unknown_strategy_error(&name))?;
            if json {
                let payload = serde_json::to_string_pretty(&info)
                    .context("failed to serialize resolver metadata")?;
                println!("{payload}");
                return Ok(());
            }

            let block = render_strategy_details(&info);
            println!("{block}");
            Ok(())
        }
    }
}

fn unknown_strategy_error(requested: &str) -> anyhow::Error {
    let dispatcher = ResolverDispatcher::with_default_strategies();
    let available = dispatcher
        .list_strategies()
        .into_iter()
        .map(|info| info.name)
        .collect::<Vec<_>>()
        .join(", ");
    anyhow!(
        "Unknown resolver strategy '{requested}'. Available: {available}. Run `jv resolver list` for details."
    )
}

fn render_strategy_table(strategies: &[ResolverStrategyInfo]) -> String {
    let headers = [
        "strategy",
        "display",
        "algorithm",
        "stability",
        "default",
        "deterministic",
        "offline",
        "conflicts",
        "policy",
    ];
    let mut widths = headers.iter().map(|h| h.len()).collect::<Vec<_>>();
    let rows = strategies
        .iter()
        .map(|info| {
            vec![
                info.name.clone(),
                info.display_name.clone(),
                format_algorithm(info.algorithm).to_string(),
                format_stability(info.stability).to_string(),
                yes_no(info.is_default),
                yes_no(info.deterministic),
                yes_no(info.supports_offline),
                yes_no(info.emits_conflict_reasons),
                info.conflict_policy.clone(),
            ]
        })
        .collect::<Vec<_>>();

    for row in &rows {
        for (idx, cell) in row.iter().enumerate() {
            widths[idx] = widths[idx].max(cell.len());
        }
    }

    let mut buffer = String::new();
    append_row(&mut buffer, &headers, &widths);
    append_separator(&mut buffer, &widths);
    for row in rows {
        let refs = row.iter().map(|cell| cell.as_str()).collect::<Vec<_>>();
        append_row(&mut buffer, &refs, &widths);
    }

    buffer
}

fn render_strategy_details(info: &ResolverStrategyInfo) -> String {
    let mut buffer = String::new();
    writeln!(
        &mut buffer,
        "Strategy     : {} ({})",
        info.name, info.display_name
    )
    .unwrap();
    writeln!(
        &mut buffer,
        "Algorithm    : {}",
        format_algorithm(info.algorithm)
    )
    .unwrap();
    writeln!(
        &mut buffer,
        "Stability    : {}",
        format_stability(info.stability)
    )
    .unwrap();
    writeln!(&mut buffer, "Default      : {}", yes_no(info.is_default)).unwrap();
    writeln!(&mut buffer, "Deterministic: {}", yes_no(info.deterministic)).unwrap();
    writeln!(
        &mut buffer,
        "Offline      : {}",
        yes_no(info.supports_offline)
    )
    .unwrap();
    writeln!(
        &mut buffer,
        "Conflicts    : {}",
        yes_no(info.emits_conflict_reasons)
    )
    .unwrap();
    writeln!(&mut buffer, "Policy       : {}", info.conflict_policy).unwrap();

    if info.aliases.is_empty() {
        writeln!(&mut buffer, "Aliases      : -").unwrap();
    } else {
        writeln!(&mut buffer, "Aliases      : {}", info.aliases.join(", ")).unwrap();
    }

    buffer.push_str("Description:\n");
    for line in info.description.lines() {
        writeln!(&mut buffer, "  {line}").unwrap();
    }

    buffer
}

fn append_row(buffer: &mut String, cells: &[&str], widths: &[usize]) {
    for (idx, cell) in cells.iter().enumerate() {
        if idx > 0 {
            buffer.push_str("  ");
        }
        let width = widths[idx];
        let formatted = format!("{cell:<width$}");
        buffer.push_str(&formatted);
    }
    buffer.push('\n');
}

fn append_separator(buffer: &mut String, widths: &[usize]) {
    for (idx, width) in widths.iter().enumerate() {
        if idx > 0 {
            buffer.push_str("  ");
        }
        buffer.push_str(&"-".repeat(*width));
    }
    buffer.push('\n');
}

fn format_algorithm(kind: ResolverAlgorithmKind) -> &'static str {
    match kind {
        ResolverAlgorithmKind::PubGrub => "pubgrub",
        ResolverAlgorithmKind::BreadthFirst => "breadth-first",
        ResolverAlgorithmKind::MavenCompat => "maven-compat",
    }
}

fn format_stability(stability: StrategyStability) -> &'static str {
    match stability {
        StrategyStability::Experimental => "experimental",
        StrategyStability::Stable => "stable",
    }
}

fn yes_no(value: bool) -> String {
    if value {
        "yes".to_string()
    } else {
        "no".to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn table_renders_all_rows() {
        let dispatcher = ResolverDispatcher::with_default_strategies();
        let strategies = dispatcher.list_strategies();
        let table = render_strategy_table(&strategies);
        for info in strategies {
            assert!(table.contains(&info.name));
            assert!(table.contains(&info.display_name));
        }
    }

    #[test]
    fn details_include_aliases_and_description() {
        let dispatcher = ResolverDispatcher::with_default_strategies();
        let info = dispatcher
            .strategy_info(dispatcher.default_strategy())
            .expect("default strategy should exist");
        let block = render_strategy_details(&info);
        assert!(block.contains("Strategy     :"));
        assert!(block.contains("Description:"));
    }
}
