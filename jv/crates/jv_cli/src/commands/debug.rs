use std::fs::{self, File};
use std::io::{self, BufWriter, Write};
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, bail};
use clap::{ArgAction, Args, ValueEnum};
use jv_ir::debug::{
    AstArtifactWriter, DiagnosticsSummary, IrArtifactSource, IrAstRebuilder, ReconstructedAst,
    ReconstructionOptions, load_ir_program,
};

#[derive(Debug, Clone, Copy, ValueEnum, PartialEq, Eq)]
pub enum DebugStage {
    #[value(name = "ir", help = "Inspect lowered IR artifacts")]
    Ir,
}

#[derive(Debug, Clone, Copy, ValueEnum, PartialEq, Eq)]
pub enum DebugEmit {
    #[value(name = "ast", help = "Reconstruct an AST artifact")]
    Ast,
}

#[derive(Debug, Clone, Copy, ValueEnum, PartialEq, Eq)]
pub enum OutputFormat {
    #[value(name = "json", help = "Emit JSON with summary sections")]
    Json,
    #[value(
        name = "pretty",
        help = "Emit human-readable text with warnings and stats"
    )]
    Pretty,
}

#[derive(Args, Debug, Clone)]
pub struct DebugArgs {
    /// Pipeline stage to inspect (currently only `ir`)
    #[arg(long, value_enum, default_value_t = DebugStage::Ir)]
    pub stage: DebugStage,
    /// Artifact kind to emit (currently only `ast`)
    #[arg(long, value_enum, default_value_t = DebugEmit::Ast)]
    pub emit: DebugEmit,
    /// IR artifact input file (`-` or未指定で標準入力)
    #[arg(long, value_name = "IR_PATH")]
    pub input: Option<PathBuf>,
    /// 再構築結果の出力先 (`-` で標準出力)
    #[arg(long, value_name = "OUTPUT")]
    pub output: Option<PathBuf>,
    /// 出力形式
    #[arg(long, value_enum, default_value_t = OutputFormat::Json)]
    pub format: OutputFormat,
    /// サマリーフッター出力を無効化
    #[arg(long = "no-stats", action = ArgAction::SetFalse, default_value_t = true)]
    pub stats: bool,
    /// 再構築時間が閾値 (ms) を超えた場合に失敗させる
    #[arg(long, value_name = "MILLIS")]
    pub fail_on_timeout: Option<u64>,
}

pub fn run(args: DebugArgs) -> Result<()> {
    if args.stage != DebugStage::Ir {
        bail!("未対応のステージ: {:?}", args.stage);
    }
    if args.emit != DebugEmit::Ast {
        bail!("未対応のemit種別: {:?}", args.emit);
    }

    let source = match &args.input {
        Some(path) if path != Path::new("-") => IrArtifactSource::from_path(path),
        _ => IrArtifactSource::stdin(),
    };

    let program = load_ir_program(source).context("IRアーティファクトの読み込みに失敗しました")?;

    let rebuilder = IrAstRebuilder::new(ReconstructionOptions::default());
    let artifact = rebuilder
        .reconstruct_program(&program)
        .context("IR→AST 再構築に失敗しました")?;

    if let Some(limit) = args.fail_on_timeout {
        let elapsed_ms = artifact.stats.elapsed.as_millis();
        if elapsed_ms > u128::from(limit) {
            bail!(
                "再構築がタイムアウト閾値({}ms)を超過しました: 実測 {}ms",
                limit,
                elapsed_ms
            );
        }
    }

    write_artifact(&args, &artifact)?;

    if args.stats {
        print_summary(&artifact);
    }

    Ok(())
}

fn write_artifact(args: &DebugArgs, artifact: &ReconstructedAst) -> Result<()> {
    let writer = AstArtifactWriter::new();

    match &args.output {
        Some(path) if path != Path::new("-") => {
            if let Some(parent) = path.parent() {
                if !parent.as_os_str().is_empty() {
                    fs::create_dir_all(parent).with_context(|| {
                        format!("出力ディレクトリの作成に失敗しました: {}", parent.display())
                    })?;
                }
            }
            let file = File::create(path).with_context(|| {
                format!("出力ファイルを作成できませんでした: {}", path.display())
            })?;
            let mut buf = BufWriter::new(file);
            stream_artifact(&writer, artifact, args.format, &mut buf)?;
            buf.flush()
                .context("再構築アーティファクトの書き出しに失敗しました")?;
        }
        _ => {
            let stdout = io::stdout();
            let mut handle = stdout.lock();
            stream_artifact(&writer, artifact, args.format, &mut handle)?;
            handle
                .flush()
                .context("標準出力への書き出しに失敗しました")?;
        }
    }

    Ok(())
}

fn stream_artifact<W: Write>(
    writer: &AstArtifactWriter,
    artifact: &ReconstructedAst,
    format: OutputFormat,
    sink: &mut W,
) -> Result<()> {
    match format {
        OutputFormat::Json => writer.write_json(sink, artifact),
        OutputFormat::Pretty => writer.write_pretty(sink, artifact),
    }
}

fn print_summary(artifact: &ReconstructedAst) {
    let summary = DiagnosticsSummary::from_artifact(artifact);
    eprintln!("=== 再構築サマリー ===");
    eprintln!("合計ノード数: {}", summary.stats.total_nodes);
    eprintln!("復元ノード数: {}", summary.stats.reconstructed_nodes);
    eprintln!("プレースホルダー数: {}", summary.stats.placeholder_nodes);
    eprintln!("警告件数: {}", summary.warning_summary.total());
    if summary.warning_summary.is_empty() {
        eprintln!("警告内訳: (none)");
    } else {
        eprintln!("警告内訳:");
        for (kind, count) in summary.warning_summary.iter() {
            eprintln!("  - {}: {}", kind, count);
        }
    }
    eprintln!("経過時間: {}ms", summary.stats.elapsed.as_millis());
}
