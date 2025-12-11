use jv_parser_frontend::ParserPipeline;
use jv_parser_rowan::frontend::RowanPipeline;
use jv_parser_salsa::pipeline::{CacheMode, ParseOptions, SalsaPipeline};
use std::env;
use std::fs;
use std::path::PathBuf;

fn current_rss_kb() -> Option<u64> {
    let status = fs::read_to_string("/proc/self/status").ok()?;
    status.lines().find_map(|line| {
        if let Some(rest) = line.strip_prefix("VmRSS:") {
            rest.split_whitespace()
                .next()
                .and_then(|num| num.parse::<u64>().ok())
        } else {
            None
        }
    })
}

#[derive(Clone, Copy, Debug)]
enum Pipeline {
    SalsaFast,
    SalsaFull,
    Rowan,
}

impl Pipeline {
    fn all() -> [Self; 3] {
        [Self::SalsaFast, Self::SalsaFull, Self::Rowan]
    }

    fn as_str(self) -> &'static str {
        match self {
            Pipeline::SalsaFast => "salsa_fast",
            Pipeline::SalsaFull => "salsa_full",
            Pipeline::Rowan => "rowan",
        }
    }
}

#[derive(Default, Debug)]
struct Config {
    pipeline: Option<Pipeline>,
    corpus: Option<PathBuf>,
    generate_functions: Option<usize>,
    cache_mode: CacheMode,
}

fn parse_args() -> Config {
    let mut args = env::args().skip(1);
    let mut config = Config::default();

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--pipeline" => {
                if let Some(value) = args.next() {
                    let pipeline = match value.as_str() {
                        "salsa_fast" => Some(Pipeline::SalsaFast),
                        "salsa_full" => Some(Pipeline::SalsaFull),
                        "rowan" => Some(Pipeline::Rowan),
                        other => {
                            eprintln!(
                                "Unknown pipeline '{other}'. Use salsa_fast|salsa_full|rowan."
                            );
                            std::process::exit(1);
                        }
                    };
                    config.pipeline = pipeline;
                } else {
                    eprintln!("--pipeline requires a value");
                    std::process::exit(1);
                }
            }
            "--corpus" => {
                if let Some(path) = args.next() {
                    config.corpus = Some(PathBuf::from(path));
                } else {
                    eprintln!("--corpus requires a path");
                    std::process::exit(1);
                }
            }
            "--generate-functions" => {
                if let Some(count) = args.next() {
                    let parsed = count.parse::<usize>().unwrap_or_else(|_| {
                        eprintln!("--generate-functions requires a positive integer");
                        std::process::exit(1);
                    });
                    config.generate_functions = Some(parsed);
                } else {
                    eprintln!("--generate-functions requires a value");
                    std::process::exit(1);
                }
            }
            "--cache-mode" => {
                if let Some(value) = args.next() {
                    let cache_mode = match value.as_str() {
                        "shared" => CacheMode::Shared,
                        "cacheless" => CacheMode::Ephemeral,
                        other => {
                            eprintln!("Unknown cache mode '{other}'. Use shared|cacheless.");
                            std::process::exit(1);
                        }
                    };
                    config.cache_mode = cache_mode;
                } else {
                    eprintln!("--cache-mode requires a value");
                    std::process::exit(1);
                }
            }
            "--help" => {
                print_help_and_exit();
            }
            other => {
                eprintln!("Unknown argument '{other}'");
                print_help_and_exit();
            }
        }
    }

    config
}

fn print_help_and_exit() -> ! {
    eprintln!(
        "\
Usage: rss_probe [--pipeline salsa_fast|salsa_full|rowan] [--corpus PATH] [--generate-functions N]
                 [--cache-mode shared|cacheless]

Without arguments this runs all pipelines on benches/corpus/synthetic/synthetic-2000.jv.
When --pipeline is set, only that pipeline is executed. --generate-functions builds an
in-memory synthetic corpus with N functions (~6 lines per function). --cache-mode
controls whether Salsa keeps query cache across runs (default: shared).
"
    );
    std::process::exit(1);
}

fn generate_synthetic(functions: usize) -> String {
    let mut source = String::from("package bench.synthetic\n\n");
    for i in 0..functions {
        let base = i;
        let add = (i % 3) + 1;
        source.push_str(&format!(
            "fun synthetic_{i}(): Int {{\n    val base = {base}\n    val next = base + {add}\n    return next\n}}\n\n"
        ));
    }
    source
}

fn load_source(config: &Config) -> Result<(String, PathBuf), Box<dyn std::error::Error>> {
    if let Some(funcs) = config.generate_functions {
        let generated = generate_synthetic(funcs);
        return Ok((
            generated,
            PathBuf::from(format!("synthetic-generated-{funcs}.jv")),
        ));
    }

    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let default_path = manifest_dir
        .join("benches")
        .join("corpus")
        .join("synthetic")
        .join("synthetic-2000.jv");

    let path = config.corpus.clone().unwrap_or(default_path);
    let source = fs::read_to_string(&path)?;
    Ok((source, path))
}

fn measure_pipeline(
    pipeline: Pipeline,
    source: &str,
    cache_mode: CacheMode,
) -> Result<u64, Box<dyn std::error::Error>> {
    let before = current_rss_kb().unwrap_or(0);
    match pipeline {
        Pipeline::SalsaFast => {
            let salsa = SalsaPipeline::with_cache_mode(cache_mode);
            salsa
                .execute_with_options(
                    source,
                    ParseOptions {
                        generate_cst: false,
                        generate_trivia_map: false,
                        trim_trivia_and_metadata: true,
                    },
                )
                .map_err(|e| format!("salsa_fast parse failed: {e:?}"))?;
        }
        Pipeline::SalsaFull => {
            let salsa = SalsaPipeline::with_cache_mode(cache_mode);
            salsa
                .execute_with_options(
                    source,
                    ParseOptions {
                        generate_cst: true,
                        generate_trivia_map: true,
                        trim_trivia_and_metadata: false,
                    },
                )
                .map_err(|e| format!("salsa_full parse failed: {e:?}"))?;
        }
        Pipeline::Rowan => {
            let rowan = RowanPipeline::new();
            rowan
                .execute(source)
                .map_err(|e| format!("rowan parse failed: {e:?}"))?;
        }
    };
    let after = current_rss_kb().unwrap_or(before);
    Ok(after.saturating_sub(before))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let config = parse_args();
    let (source, label) = load_source(&config)?;
    let lines = source.lines().count();

    let pipelines: Vec<Pipeline> = match config.pipeline {
        Some(p) => vec![p],
        None => Pipeline::all().to_vec(),
    };

    for pipeline in pipelines {
        let delta = measure_pipeline(pipeline, &source, config.cache_mode)?;
        println!(
            "pipeline={} rss_delta_kib={} lines={} corpus={} cache_mode={:?}",
            pipeline.as_str(),
            delta,
            lines,
            label.display(),
            config.cache_mode
        );
    }

    Ok(())
}
