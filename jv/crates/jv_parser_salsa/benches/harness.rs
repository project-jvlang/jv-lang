use criterion::Criterion;
use jv_parser_frontend::{ParseError, ParserPipeline};
use jv_parser_rowan::frontend::RowanPipeline;
use jv_parser_salsa::pipeline::{CacheMode, ParseOptions, SalsaPipeline};
use std::env;
use std::fs;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{Duration, SystemTime};

#[derive(Clone, Copy, Debug)]
pub enum PipelineKind {
    /// Salsa パイプライン（CST/Trivia なし）
    SalsaFast,
    /// Salsa パイプライン（CST/Trivia あり）
    SalsaFull,
    /// Rowan ベースのリファレンスパイプライン
    Rowan,
}

pub struct PipelineSwitcher {
    salsa: SalsaPipeline,
    rowan: RowanPipeline,
}

impl PipelineSwitcher {
    pub fn new() -> Self {
        Self::with_cache_mode(CacheMode::default())
    }

    pub fn with_cache_mode(cache_mode: CacheMode) -> Self {
        Self {
            salsa: SalsaPipeline::with_cache_mode(cache_mode),
            rowan: RowanPipeline::new(),
        }
    }

    /// パイプラインを切り替えて実行する。診断件数を返す。
    pub fn run(&self, kind: PipelineKind, source: &str) -> Result<usize, ParseError> {
        match kind {
            PipelineKind::SalsaFast => self
                .salsa
                .execute_with_options(
                    source,
                    ParseOptions {
                        trim_trivia_and_metadata: true,
                        ..ParseOptions::default()
                    },
                )
                .map(|out| out.artifacts.diagnostics.final_diagnostics().len()),
            PipelineKind::SalsaFull => self
                .salsa
                .execute_with_options(
                    source,
                    ParseOptions {
                        generate_cst: true,
                        generate_trivia_map: true,
                        trim_trivia_and_metadata: false,
                    },
                )
                .map(|out| out.artifacts.diagnostics.final_diagnostics().len()),
            PipelineKind::Rowan => self
                .rowan
                .execute(source)
                .map(|artifacts| artifacts.diagnostics.final_diagnostics().len()),
        }
    }
}

#[derive(Clone)]
pub struct CorpusEntry {
    pub name: String,
    pub source: String,
}

pub struct BenchCorpus {
    stdlib: Vec<CorpusEntry>,
    synthetic: Vec<CorpusEntry>,
}

#[derive(Clone, Debug)]
pub struct JdkModulesCorpus {
    pub modules_path: PathBuf,
}

impl BenchCorpus {
    pub fn load() -> Result<Self, String> {
        let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let workspace_root = workspace_root(&manifest_dir);
        let corpus_root = manifest_dir.join("benches").join("corpus");

        let stdlib = load_stdlib(&workspace_root, &corpus_root)?;
        let synthetic = load_synthetic(&corpus_root)?;

        Ok(Self { stdlib, synthetic })
    }

    pub fn stdlib(&self) -> &[CorpusEntry] {
        &self.stdlib
    }

    pub fn synthetic(&self) -> &[CorpusEntry] {
        &self.synthetic
    }

    pub fn synthetic_by_lines(&self, expected_lines: usize) -> Option<&CorpusEntry> {
        self.synthetic
            .iter()
            .find(|entry| entry.source.lines().count() == expected_lines)
    }
}

impl JdkModulesCorpus {
    pub fn load() -> Result<Self, String> {
        let modules_path = resolve_jdk_modules_path()?;
        Ok(Self { modules_path })
    }

    pub fn read_entries(&self) -> Result<Vec<CorpusEntry>, String> {
        read_jimage_modules(&self.modules_path)
    }
}

fn load_stdlib(workspace_root: &Path, corpus_root: &Path) -> Result<Vec<CorpusEntry>, String> {
    let list_path = corpus_root.join("stdlib.txt");
    let content = fs::read_to_string(&list_path)
        .map_err(|err| format!("cannot read {list_path:?}: {err}"))?;

    let mut entries = Vec::new();
    for line in content.lines().map(str::trim).filter(|l| !l.is_empty()) {
        let abs_path = workspace_root.join(line);
        let source = fs::read_to_string(&abs_path)
            .map_err(|err| format!("cannot read stdlib file {abs_path:?}: {err}"))?;
        entries.push(CorpusEntry {
            name: line.to_string(),
            source,
        });
    }
    Ok(entries)
}

fn load_synthetic(corpus_root: &Path) -> Result<Vec<CorpusEntry>, String> {
    let synthetic_dir = corpus_root.join("synthetic");
    let mut entries = Vec::new();
    let files = ["synthetic-100.jv", "synthetic-500.jv", "synthetic-2000.jv"];
    for file in files {
        let path = synthetic_dir.join(file);
        let source =
            fs::read_to_string(&path).map_err(|err| format!("cannot read {path:?}: {err}"))?;
        entries.push(CorpusEntry {
            name: file.to_string(),
            source,
        });
    }
    Ok(entries)
}

fn workspace_root(manifest_dir: &Path) -> PathBuf {
    manifest_dir
        .parent()
        .and_then(|p| p.parent())
        .map(PathBuf::from)
        .unwrap_or_else(|| manifest_dir.to_path_buf())
}

/// デフォルトの JDK モジュールイメージパスを解決する。
fn resolve_jdk_modules_path() -> Result<PathBuf, String> {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let workspace_root = workspace_root(&manifest_dir);

    let env_override = env::var("JV_BENCH_JDK_MODULES").ok();
    let candidate = env_override
        .map(PathBuf::from)
        .unwrap_or_else(|| workspace_root.join("toolchains/jdk25/lib/modules"));
    let modules_path = if candidate.is_absolute() {
        candidate
    } else {
        workspace_root.join(candidate)
    };

    if !modules_path.exists() {
        return Err(format!(
            "JDK modules image not found at {modules_path:?}. \
             Set JV_BENCH_JDK_MODULES=/path/to/lib/modules or provision toolchains/jdk25/lib/modules."
        ));
    }

    if !modules_path.is_file() {
        return Err(format!(
            "JDK modules path {modules_path:?} is not a file; expected a lib/modules jimage."
        ));
    }

    Ok(modules_path)
}

/// JRT イメージを走査し、各エントリのサイズ・ハッシュ・サンプルをベースに簡易ソースを生成する。
fn read_jimage_modules(path: &Path) -> Result<Vec<CorpusEntry>, String> {
    let (java_bin, jdk_home) = java_command_for_modules(path);

    let temp_java = TempJavaSource::new().map_err(|e| format!("cannot write temp java: {e}"))?;
    fs::write(&temp_java.path, JIMAGE_DUMP_SOURCE)
        .map_err(|e| format!("cannot write java helper to {:?}: {e}", temp_java.path))?;

    let mut cmd = Command::new(java_bin);
    cmd.arg(&temp_java.path)
        .arg(path)
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit());

    if let Some(home) = jdk_home {
        cmd.env("JAVA_HOME", home);
    }

    let mut child = cmd
        .spawn()
        .map_err(|e| format!("failed to spawn java helper: {e}"))?;
    let stdout = child
        .stdout
        .take()
        .ok_or_else(|| "failed to capture java helper stdout".to_string())?;
    let reader = BufReader::new(stdout);

    let mut entries = Vec::new();
    for line in reader.lines() {
        let line = line.map_err(|e| format!("failed to read java helper output: {e}"))?;
        if line.starts_with("ERROR\t") {
            return Err(line);
        }
        let mut parts = line.splitn(4, '\t');
        let path_part = parts
            .next()
            .ok_or_else(|| "missing path in java helper output".to_string())?;
        let size_part = parts
            .next()
            .ok_or_else(|| "missing size in java helper output".to_string())?;
        let hash_part = parts
            .next()
            .ok_or_else(|| "missing hash in java helper output".to_string())?;
        let sample_part = parts
            .next()
            .ok_or_else(|| "missing sample in java helper output".to_string())?;

        let size: u64 = size_part
            .parse()
            .map_err(|e| format!("invalid size from java helper: {e}"))?;

        let source = format!(
            "// jimage entry: {path}\n// size_bytes: {size}\n// sha256_b64: {hash}\nval sample = \"{sample}\";\n",
            path = path_part,
            hash = hash_part,
            sample = sample_part
        );

        entries.push(CorpusEntry {
            name: path_part.to_string(),
            source,
        });
    }

    let status = child
        .wait()
        .map_err(|e| format!("failed to wait java helper: {e}"))?;
    if !status.success() {
        return Err(format!("java helper exited with {status}"));
    }

    Ok(entries)
}

fn java_command_for_modules(path: &Path) -> (PathBuf, Option<PathBuf>) {
    let jdk_home = path.parent().and_then(|p| p.parent()).map(PathBuf::from);
    if let Some(home) = &jdk_home {
        let candidate = home.join("bin").join("java");
        if candidate.exists() {
            return (candidate, Some(home.clone()));
        }
    }
    (PathBuf::from("java"), jdk_home)
}

/// ベンチ用 Criterion 設定。
pub fn criterion_config() -> Criterion {
    Criterion::default()
        .warm_up_time(Duration::from_secs(3))
        .measurement_time(Duration::from_secs(10))
        .sample_size(20)
}

/// /proc/self/status から現在の RSS(KiB) を取得する。
pub fn current_rss_kb() -> Option<u64> {
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

/// 変更行数に基づいてキャッシュヒット率を概算する。
pub fn estimate_hit_rate(before: &str, after: &str) -> f64 {
    let total_lines = after.lines().count().max(1);
    let changed = before
        .lines()
        .zip(after.lines())
        .filter(|(l, r)| l != r)
        .count();
    let delta = changed as f64 / total_lines as f64;
    (1.0_f64 - delta).max(0.0)
}

/// ベンチで共有するハーネスとコーパスを取得する。
pub fn bench_state() -> (PipelineSwitcher, BenchCorpus) {
    let harness = PipelineSwitcher::new();
    let corpus = BenchCorpus::load().expect("corpus should load");
    (harness, corpus)
}

/// JDK モジュールイメージを解決し、読み込みを試行する。
pub fn load_jdk_modules_entries() -> Result<(PathBuf, Vec<CorpusEntry>), String> {
    let corpus = JdkModulesCorpus::load()?;
    let entries = corpus.read_entries()?;
    Ok((corpus.modules_path, entries))
}

struct TempJavaSource {
    path: PathBuf,
}

impl TempJavaSource {
    fn new() -> Result<Self, std::io::Error> {
        let mut path = env::temp_dir();
        let nanos = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos();
        path.push(format!("jrt_dump_{nanos}.java"));
        Ok(Self { path })
    }
}

impl Drop for TempJavaSource {
    fn drop(&mut self) {
        let _ = fs::remove_file(&self.path);
    }
}

const JIMAGE_DUMP_SOURCE: &str = r#"
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.MessageDigest;
import java.util.Base64;
import java.util.Map;
import java.util.stream.Stream;

class JrtDump {
    private static final int SAMPLE_LIMIT = 64;

    public static void main(String[] args) throws Exception {
        if (args.length != 1) {
            System.err.println("ERROR\tmissing modules path");
            System.exit(2);
        }

        Path modulesPath = Paths.get(args[0]).toAbsolutePath();
        if (modulesPath.getParent() == null || modulesPath.getParent().getParent() == null) {
            System.err.println("ERROR\tinvalid modules path: " + modulesPath);
            System.exit(2);
        }
        Path jdkHome = modulesPath.getParent().getParent();
        Map<String, String> env = Map.of("java.home", jdkHome.toString());

        try (JrtHandle handle = openJrtFileSystem(env)) {
            FileSystem fs = handle.fs();
            Path modulesRoot = fs.getPath("/modules");
            Base64.Encoder encoder = Base64.getEncoder();
            MessageDigest digest = MessageDigest.getInstance("SHA-256");

            try (Stream<Path> moduleDirs = Files.list(modulesRoot)) {
                moduleDirs.filter(Files::isDirectory).forEach(module -> {
                    try (Stream<Path> files = Files.walk(module)) {
                        files.filter(Files::isRegularFile).forEach(file -> {
                            try {
                                digest.reset();
                                long size = 0;
                                byte[] sample = new byte[SAMPLE_LIMIT];
                                int sampled = 0;

                                try (InputStream in = Files.newInputStream(file)) {
                                    byte[] buf = new byte[8192];
                                    int read;
                                    while ((read = in.read(buf)) != -1) {
                                        digest.update(buf, 0, read);
                                        size += read;

                                        if (sampled < SAMPLE_LIMIT) {
                                            int copy = Math.min(read, SAMPLE_LIMIT - sampled);
                                            System.arraycopy(buf, 0, sample, sampled, copy);
                                            sampled += copy;
                                        }
                                    }
                                }

                                String hash = encoder.encodeToString(digest.digest());
                                String sampleHex = toHex(sample, sampled);
                                String rel = file.toString().substring(1); // drop leading /
                                System.out.println(rel + "\t" + size + "\t" + hash + "\t" + sampleHex);
                            } catch (Exception ex) {
                                System.err.println("ERROR\t" + file.toString() + "\t" + ex.getMessage());
                            }
                        });
                    } catch (IOException io) {
                        System.err.println("ERROR\t" + module.toString() + "\t" + io.getMessage());
                    }
                });
            }
        }
    }

    private static String toHex(byte[] data, int len) {
        StringBuilder sb = new StringBuilder(len * 2);
        for (int i = 0; i < len; i++) {
            sb.append(String.format("%02x", data[i]));
        }
        return sb.toString();
    }

    private static JrtHandle openJrtFileSystem(Map<String, String> env) throws IOException {
        try {
            FileSystem fs = FileSystems.newFileSystem(URI.create("jrt:/"), env);
            return new JrtHandle(fs, true);
        } catch (Exception alreadyOpen) {
            FileSystem fs = FileSystems.getFileSystem(URI.create("jrt:/"));
            return new JrtHandle(fs, false);
        }
    }
}

record JrtHandle(FileSystem fs, boolean closeable) implements AutoCloseable {
    @Override
    public void close() throws IOException {
        if (closeable) {
            fs.close();
        }
    }
}
"#;
