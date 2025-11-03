use std::fs;
use std::net::{Shutdown, SocketAddr, TcpStream, ToSocketAddrs};
use std::path::{Path, PathBuf};
use std::thread;
use std::time::{Duration, Instant};

use anyhow::{Context, Result, anyhow, bail};
use clap::{ArgAction, Args, Subcommand};
use jv_build::manifest::parse_logging_config;
use jv_pm::{LoggingConfig, LoggingConfigLayer, OpenTelemetryConfig, OtelProtocol};
use toml::Value;
use url::{ParseError, Url};

use crate::logging_overrides::{build_cli_logging_layer, read_env_logging_layer};
use crate::pipeline::project::locator::{ProjectLocator, ProjectRoot};
use crate::pipeline::project::manifest::ManifestLoader;
use crate::tooling_failure;

#[derive(Args, Debug, Clone)]
pub struct OtelCommand {
    /// jv.toml を探索する開始パス
    #[arg(long = "manifest", value_name = "PATH")]
    pub manifest: Option<PathBuf>,
    #[command(flatten)]
    pub overrides: OtelOverrideArgs,
    #[command(subcommand)]
    pub action: OtelSubcommand,
}

#[derive(Args, Debug, Clone, Default)]
pub struct OtelOverrideArgs {
    /// OpenTelemetry の有効・無効を上書きする
    #[arg(long = "otel-enabled", value_name = "bool")]
    pub otel_enabled: Option<String>,
    /// Collector のエンドポイントを上書きする
    #[arg(long = "otel-endpoint", value_name = "URL")]
    pub otel_endpoint: Option<String>,
    /// Collector との通信プロトコルを上書きする
    #[arg(long = "otel-protocol", value_name = "protocol")]
    pub otel_protocol: Option<String>,
    /// TraceContext ヘッダ注入の有効・無効を上書きする
    #[arg(long = "otel-trace-context", value_name = "bool")]
    pub otel_trace_context: Option<String>,
}

#[derive(Subcommand, Debug, Clone)]
pub enum OtelSubcommand {
    /// Collector への疎通テストを実行する
    Test(OtelTestArgs),
    /// OpenTelemetry 設定を検証する
    Validate(OtelValidateArgs),
}

#[derive(Args, Debug, Clone)]
pub struct OtelTestArgs {
    /// 1回の試行で待機する最大時間 (ミリ秒)
    #[arg(long = "timeout-ms", value_name = "MILLIS", default_value_t = 3000)]
    pub timeout_ms: u64,
    /// 接続試行回数
    #[arg(long = "retries", value_name = "COUNT", default_value_t = 3)]
    pub retries: u8,
    /// 再試行の前に待機する時間 (ミリ秒)
    #[arg(long = "retry-sleep-ms", value_name = "MILLIS", default_value_t = 300)]
    pub retry_sleep_ms: u64,
    /// 詳細ログの出力を抑制する
    #[arg(long = "quiet", short = 'q', action = ArgAction::SetTrue)]
    pub quiet: bool,
}

#[derive(Args, Debug, Clone, Default)]
pub struct OtelValidateArgs {
    /// 警告をエラーとして扱う
    #[arg(long = "strict", action = ArgAction::SetTrue)]
    pub strict: bool,
}

pub fn run(command: OtelCommand) -> Result<()> {
    let project_root = locate_project(command.manifest.as_deref())?;
    let manifest_path = project_root.manifest_path().to_path_buf();

    // マニフェスト全体の検証（一般的なエラーを早期発見する）
    let _settings = ManifestLoader::load(&manifest_path)
        .map_err(|diagnostic| tooling_failure(&manifest_path, diagnostic))?;

    let manifest_logging = load_manifest_logging(&manifest_path)?;
    let env_layer = read_env_logging_layer()?;
    let cli_layer = build_cli_logging_layer(
        None,
        None,
        None,
        command.overrides.otel_enabled.as_deref(),
        command.overrides.otel_endpoint.as_deref(),
        command.overrides.otel_protocol.as_deref(),
        command.overrides.otel_trace_context.as_deref(),
    )?;

    let mut layers = Vec::new();
    if !env_layer.is_empty() {
        layers.push(env_layer.clone());
    }
    if !cli_layer.is_empty() {
        layers.push(cli_layer.clone());
    }
    let effective_logging = manifest_logging.clone().with_layers(&layers);

    let context = OtelContext {
        manifest_path,
        effective_logging,
        env_layer,
        cli_layer,
    };

    match command.action {
        OtelSubcommand::Validate(args) => run_validate(&context, &args),
        OtelSubcommand::Test(args) => run_test(&context, &args),
    }
}

struct OtelContext {
    manifest_path: PathBuf,
    effective_logging: LoggingConfig,
    env_layer: LoggingConfigLayer,
    cli_layer: LoggingConfigLayer,
}

struct EndpointInfo {
    url: Url,
    original: String,
    inferred_scheme: bool,
    explicit_port: Option<u16>,
}

enum IssueSeverity {
    Error,
    Warning,
}

struct ValidationIssue {
    severity: IssueSeverity,
    message: String,
}

struct ValidationSummary {
    issues: Vec<ValidationIssue>,
    endpoint: Option<EndpointInfo>,
}

impl ValidationSummary {
    fn has_errors(&self) -> bool {
        self.issues
            .iter()
            .any(|issue| matches!(issue.severity, IssueSeverity::Error))
    }

    fn warnings(&self) -> impl Iterator<Item = &ValidationIssue> {
        self.issues
            .iter()
            .filter(|issue| matches!(issue.severity, IssueSeverity::Warning))
    }
}

fn run_validate(context: &OtelContext, args: &OtelValidateArgs) -> Result<()> {
    println!("=== OpenTelemetry 設定検証 ===");
    println!("マニフェスト: {}", context.manifest_path.display());
    print_effective_config(&context.effective_logging);

    if !context.cli_layer.is_empty() {
        println!("CLI 上書き: 適用されています");
    } else {
        println!("CLI 上書き: ありません");
    }
    if !context.env_layer.is_empty() {
        println!("環境変数上書き: 適用されています");
    } else {
        println!("環境変数上書き: ありません");
    }

    let summary = validate_effective_config(&context.effective_logging);
    if let Some(endpoint) = summary.endpoint.as_ref() {
        println!("解決されたエンドポイント: {}", endpoint.url);
        if endpoint.inferred_scheme {
            println!(
                "補足: スキームが指定されていなかったため '{}://' を補完しました。",
                endpoint.url.scheme()
            );
        }
    }

    if summary.issues.is_empty() {
        println!("✓ OpenTelemetry 設定に問題は見つかりませんでした。");
        return Ok(());
    }

    for issue in &summary.issues {
        match issue.severity {
            IssueSeverity::Error => println!("[エラー] {}", issue.message),
            IssueSeverity::Warning => println!("[警告] {}", issue.message),
        }
    }

    if summary.has_errors() || (args.strict && summary.warnings().next().is_some()) {
        bail!("OpenTelemetry 設定の検証に失敗しました。");
    }

    println!("⚠ 警告がありますが致命的な問題はありません。");
    println!("   必要に応じて設定を見直してください。");
    Ok(())
}

fn run_test(context: &OtelContext, args: &OtelTestArgs) -> Result<()> {
    println!("=== OpenTelemetry Collector 疎通テスト ===");
    println!("マニフェスト: {}", context.manifest_path.display());
    print_effective_config(&context.effective_logging);

    let summary = validate_effective_config(&context.effective_logging);
    if summary.has_errors() {
        for issue in &summary.issues {
            if matches!(issue.severity, IssueSeverity::Error) {
                println!("[エラー] {}", issue.message);
            }
        }
        bail!(
            "設定にエラーがあるため疎通テストを実行できません。`jv otel validate` で修正してください。"
        );
    }

    for warning in summary
        .issues
        .iter()
        .filter(|issue| matches!(issue.severity, IssueSeverity::Warning))
    {
        println!("[警告] {}", warning.message);
    }

    let endpoint = summary.endpoint.ok_or_else(|| {
        anyhow!(
            "有効なエンドポイントを解決できませんでした。`jv otel validate` を実行して設定を確認してください。"
        )
    })?;

    let protocol = context.effective_logging.opentelemetry.protocol;
    let host = endpoint.url.host_str().ok_or_else(|| {
        anyhow!(
            "エンドポイント '{}' にホストが含まれていません。",
            endpoint.original
        )
    })?;
    let port = determine_port(&endpoint, protocol);
    println!("接続先: {}:{} ({})", host, port, protocol);

    let addresses = resolve_addresses(host, port)?;
    if !args.quiet {
        println!(
            "解決されたアドレス: {}",
            addresses
                .iter()
                .map(|addr| addr.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        );
    }

    let attempts = args.retries.max(1) as usize;
    let timeout = Duration::from_millis(args.timeout_ms);
    let mut last_error = None;

    for attempt in 1..=attempts {
        if !args.quiet {
            println!("- 試行 {}/{} ...", attempt, attempts);
        }
        match connect_once(&addresses, timeout) {
            Ok(elapsed) => {
                println!(
                    "✓ 接続に成功しました ({:.2} ms)。Collector は到達可能です。",
                    elapsed.as_secs_f64() * 1000.0
                );
                return Ok(());
            }
            Err(error) => {
                if !args.quiet {
                    println!("  -> {}", error);
                }
                last_error = Some(error);
                if attempt < attempts {
                    thread::sleep(Duration::from_millis(args.retry_sleep_ms));
                }
            }
        }
    }

    Err(last_error.unwrap_or_else(|| anyhow!("Collector への接続に失敗しました。")))
}

fn locate_project(manifest_hint: Option<&Path>) -> Result<ProjectRoot> {
    let locator = ProjectLocator::new();
    match manifest_hint {
        Some(path) => locator
            .locate(path)
            .map_err(|diagnostic| tooling_failure(path, diagnostic)),
        None => {
            let cwd = std::env::current_dir()?;
            locator
                .locate(&cwd)
                .map_err(|diagnostic| tooling_failure(&cwd, diagnostic))
        }
    }
}

fn load_manifest_logging(manifest_path: &Path) -> Result<LoggingConfig> {
    let contents = fs::read_to_string(manifest_path)
        .with_context(|| format!("{} の読み込みに失敗しました。", manifest_path.display()))?;
    let value: Value = contents.parse().with_context(|| {
        format!(
            "{} をTOMLとして解析できませんでした。",
            manifest_path.display()
        )
    })?;
    parse_logging_config(&value).map_err(|error| anyhow!(error.to_string()))
}

fn validate_effective_config(config: &LoggingConfig) -> ValidationSummary {
    let mut issues = Vec::new();
    let otel = &config.opentelemetry;

    if !otel.enabled {
        issues.push(ValidationIssue {
            severity: IssueSeverity::Warning,
            message: "OpenTelemetry は無効化されています。Collector との疎通は実行されません。"
                .to_string(),
        });
        return ValidationSummary {
            issues,
            endpoint: None,
        };
    }

    let endpoint = match resolve_endpoint(otel) {
        Ok(info) => info,
        Err(error) => {
            issues.push(ValidationIssue {
                severity: IssueSeverity::Error,
                message: error.to_string(),
            });
            return ValidationSummary {
                issues,
                endpoint: None,
            };
        }
    };

    issues.extend(collect_endpoint_warnings(&endpoint, otel.protocol));

    ValidationSummary {
        issues,
        endpoint: Some(endpoint),
    }
}

fn resolve_endpoint(config: &OpenTelemetryConfig) -> Result<EndpointInfo> {
    let raw = config
        .endpoint
        .as_ref()
        .map(|value| value.trim())
        .filter(|value| !value.is_empty())
        .ok_or_else(|| anyhow!("OpenTelemetry を有効にした場合は endpoint を指定してください。"))?;
    normalize_endpoint(raw, config.protocol)
}

fn normalize_endpoint(raw: &str, protocol: OtelProtocol) -> Result<EndpointInfo> {
    let default_scheme = match protocol {
        OtelProtocol::Grpc => "http",
        OtelProtocol::Http => "http",
    };

    let (url, inferred_scheme, explicit_port) = match Url::parse(raw) {
        Ok(url) if !url.cannot_be_a_base() && url.host_str().is_some() => {
            let port = url.port();
            (url, false, port)
        }
        Ok(_) | Err(ParseError::RelativeUrlWithoutBase) => {
            let candidate = format!("{default_scheme}://{raw}");
            let mut url = Url::parse(&candidate).with_context(|| {
                format!("endpoint '{}' を URL として解釈できませんでした。", raw)
            })?;
            let explicit_port = url.port();
            if explicit_port.is_some() {
                let _ = url.set_port(None);
            }
            (url, true, explicit_port)
        }
        Err(err) => {
            return Err(anyhow!(
                "endpoint '{}' を URL として解釈できませんでした: {}",
                raw,
                err
            ))
        }
    };

    if url.host_str().is_none() {
        bail!(
            "endpoint '{}' にはホスト名を含めてください (例: collector.example.com)。",
            raw
        );
    }

    Ok(EndpointInfo {
        url,
        original: raw.to_string(),
        inferred_scheme,
        explicit_port,
    })
}

fn collect_endpoint_warnings(
    endpoint: &EndpointInfo,
    protocol: OtelProtocol,
) -> Vec<ValidationIssue> {
    let mut issues = Vec::new();
    let scheme = endpoint.url.scheme();
    match protocol {
        OtelProtocol::Http => {
            if scheme != "http" && scheme != "https" {
                issues.push(ValidationIssue {
                    severity: IssueSeverity::Error,
                    message: format!(
                        "HTTP プロトコルを指定した場合、エンドポイントのスキームは http(s) を使用してください (現在: '{}').",
                        scheme
                    ),
                });
            }
        }
        OtelProtocol::Grpc => {
            if scheme != "http" && scheme != "https" && scheme != "grpc" && scheme != "grpcs" {
                issues.push(ValidationIssue {
                    severity: IssueSeverity::Warning,
                    message: format!(
                        "gRPC プロトコルで想定外のスキーム '{}' が指定されています。Collector の設定を確認してください。",
                        scheme
                    ),
                });
            }
        }
    }

    let port = endpoint
        .explicit_port
        .or_else(|| endpoint.url.port());
    if port.is_none() {
        let recommendation = recommended_port(protocol);
        issues.push(ValidationIssue {
            severity: IssueSeverity::Warning,
            message: format!(
                "エンドポイントにポートが指定されていません。推奨ポート {} を明示することをおすすめします。",
                recommendation
            ),
        });
    } else {
        let port_value = port.unwrap();
        match protocol {
            OtelProtocol::Grpc if port_value == 4318 => {
                issues.push(ValidationIssue {
                    severity: IssueSeverity::Warning,
                    message:
                        "gRPC プロトコルで HTTP 用推奨ポート 4318 が設定されています。Collector のプロトコルを再確認してください。"
                            .to_string(),
                });
            }
            OtelProtocol::Http if port_value == 4317 => {
                issues.push(ValidationIssue {
                    severity: IssueSeverity::Warning,
                    message:
                        "HTTP プロトコルで gRPC 用推奨ポート 4317 が設定されています。Collector のプロトコルを再確認してください。"
                            .to_string(),
                });
            }
            _ => {}
        }
    }

    if protocol == OtelProtocol::Http {
        let path = endpoint.url.path();
        if path == "/" || path.is_empty() {
            issues.push(ValidationIssue {
                severity: IssueSeverity::Warning,
                message:
                    "HTTP プロトコルでは `/v1/traces` などの OTLP パスを含めることを推奨します。"
                        .to_string(),
            });
        }
    }

    issues
}

fn determine_port(endpoint: &EndpointInfo, protocol: OtelProtocol) -> u16 {
    endpoint
        .explicit_port
        .or_else(|| endpoint.url.port())
        .or_else(|| endpoint.url.port_or_known_default())
        .unwrap_or_else(|| recommended_port(protocol))
}

fn recommended_port(protocol: OtelProtocol) -> u16 {
    match protocol {
        OtelProtocol::Grpc => 4317,
        OtelProtocol::Http => 4318,
    }
}

fn resolve_addresses(host: &str, port: u16) -> Result<Vec<SocketAddr>> {
    let target = format!("{host}:{port}");
    let addrs: Vec<_> = target
        .to_socket_addrs()
        .with_context(|| format!("エンドポイント {} の名前解決に失敗しました。", target))?
        .collect();
    if addrs.is_empty() {
        bail!(
            "エンドポイント {} に対応するアドレスが見つかりません。",
            target
        );
    }
    Ok(addrs)
}

fn connect_once(addresses: &[SocketAddr], timeout: Duration) -> Result<Duration> {
    let mut last_error = None;
    for addr in addresses {
        let start = Instant::now();
        match TcpStream::connect_timeout(addr, timeout) {
            Ok(stream) => {
                let elapsed = start.elapsed();
                let _ = stream.shutdown(Shutdown::Both);
                return Ok(elapsed);
            }
            Err(error) => {
                last_error = Some(error);
            }
        }
    }
    let detail = last_error
        .map(|err| err.to_string())
        .unwrap_or_else(|| "原因不明のエラーが発生しました。".to_string());
    Err(anyhow!("接続に失敗しました: {}", detail))
}

fn print_effective_config(config: &LoggingConfig) {
    let otel = &config.opentelemetry;
    println!("有効なロギング設定:");
    println!("  フレームワーク        : {}", config.framework);
    println!("  ログレベル            : {}", config.log_level);
    println!("  既定ログレベル        : {}", config.default_level);
    println!("  OpenTelemetry Enabled : {}", bool_label(otel.enabled));
    println!(
        "  OpenTelemetry Endpoint: {}",
        otel.endpoint
            .as_deref()
            .filter(|value| !value.is_empty())
            .unwrap_or("(未設定)")
    );
    println!("  OpenTelemetry Protocol: {}", otel.protocol);
    println!(
        "  TraceContext 注入     : {}",
        bool_label(otel.trace_context)
    );
    println!("  Resource 属性数       : {}", otel.resource.len());
    println!("  カスタム属性数        : {}", otel.attributes.len());
}

fn bool_label(value: bool) -> &'static str {
    if value { "有効" } else { "無効" }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jv_pm::{LogLevel, LoggingFramework, OpenTelemetryLayer};

    fn base_logging_config() -> LoggingConfig {
        LoggingConfig {
            framework: LoggingFramework::Slf4j,
            log_level: LogLevel::Info,
            default_level: LogLevel::Info,
            opentelemetry: OpenTelemetryConfig::default(),
        }
    }

    #[test]
    fn normalize_endpoint_inferrs_scheme() {
        let info = normalize_endpoint("localhost:4317", OtelProtocol::Grpc).expect("normalize");
        assert_eq!(info.url.scheme(), "http");
        assert!(info.inferred_scheme);
        assert_eq!(info.url.host_str(), Some("localhost"));
        assert_eq!(info.url.port_or_known_default(), Some(80));
    }

    #[test]
    fn validate_detects_missing_endpoint() {
        let mut config = base_logging_config();
        config.opentelemetry.enabled = true;
        let summary = validate_effective_config(&config);
        assert!(summary.has_errors());
        assert!(summary.endpoint.is_none());
    }

    #[test]
    fn validate_accepts_valid_endpoint() {
        let mut config = base_logging_config();
        config.opentelemetry = OpenTelemetryConfig {
            enabled: true,
            endpoint: Some("http://localhost:4317".to_string()),
            protocol: OtelProtocol::Grpc,
            trace_context: false,
            resource: Default::default(),
            attributes: Default::default(),
        };
        let summary = validate_effective_config(&config);
        assert!(!summary.has_errors());
        assert!(summary.endpoint.is_some());
    }

    #[test]
    fn determine_port_uses_override_layer() {
        let mut config = base_logging_config();
        config.opentelemetry.enabled = true;
        let mut layer = LoggingConfigLayer::default();
        layer.opentelemetry = Some(OpenTelemetryLayer {
            enabled: Some(true),
            endpoint: Some(Some("collector.local:5522".to_string())),
            protocol: Some(OtelProtocol::Grpc),
            trace_context: None,
            resource: None,
            attributes: None,
        });
        let effective = config.with_layers(&[layer]);
        let summary = validate_effective_config(&effective);
        assert!(summary.endpoint.is_some());
        let info = summary.endpoint.unwrap();
        assert_eq!(determine_port(&info, OtelProtocol::Grpc), 5522);
    }
}
