use std::{
    env,
    fs,
    io::{Read, Write},
    net::{SocketAddr, TcpListener, TcpStream},
    path::{Path, PathBuf},
    process::{Command, Output},
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
    thread,
    time::{Duration, SystemTime},
};

use anyhow::{bail, Context, Result};
use sha2::{Digest, Sha256};
use tempfile::{tempdir, TempDir};
use zip::{write::FileOptions, CompressionMethod};

#[derive(Clone)]
struct DependencySpec {
    group: &'static str,
    artifact: &'static str,
    version: &'static str,
    dependencies: Vec<DependencyCoordinate>,
}

#[derive(Clone)]
struct DependencyCoordinate {
    group: &'static str,
    artifact: &'static str,
    version: &'static str,
}

impl DependencySpec {
    fn new(group: &'static str, artifact: &'static str, version: &'static str) -> Self {
        Self {
            group,
            artifact,
            version,
            dependencies: Vec::new(),
        }
    }

    fn with_dependencies(mut self, dependencies: Vec<DependencyCoordinate>) -> Self {
        self.dependencies = dependencies;
        self
    }

    fn coordinate(&self) -> String {
        format!("{}:{}", self.group, self.artifact)
    }

    fn coordinate_with_version(&self) -> String {
        format!("{}:{}@{}", self.group, self.artifact, self.version)
    }

    fn group_path(&self) -> PathBuf {
        let mut path = PathBuf::new();
        for segment in self.group.split('.') {
            path.push(segment);
        }
        path
    }

    fn jar_name(&self) -> String {
        format!("{}-{}.jar", self.artifact, self.version)
    }

    fn pom_name(&self) -> String {
        format!("{}-{}.pom", self.artifact, self.version)
    }
}

impl DependencyCoordinate {
    fn new(group: &'static str, artifact: &'static str, version: &'static str) -> Self {
        Self {
            group,
            artifact,
            version,
        }
    }
}

struct FakeMavenRepo;

impl FakeMavenRepo {
    fn create(base_dir: &Path, dependencies: &[DependencySpec]) -> Result<()> {
        if base_dir.exists() {
            fs::remove_dir_all(base_dir)
                .with_context(|| format!("{} の削除に失敗しました", base_dir.display()))?;
        }
        fs::create_dir_all(base_dir)
            .with_context(|| format!("{} の作成に失敗しました", base_dir.display()))?;

        for dep in dependencies {
            Self::write_metadata(base_dir, dep)?;
            Self::write_artifacts(base_dir, dep)?;
        }

        Ok(())
    }

    fn write_metadata(base_dir: &Path, dep: &DependencySpec) -> Result<()> {
        let mut path = base_dir.join(dep.group_path());
        fs::create_dir_all(&path)
            .with_context(|| format!("{} の作成に失敗しました", path.display()))?;
        path.push(dep.artifact);
        fs::create_dir_all(&path)
            .with_context(|| format!("{} の作成に失敗しました", path.display()))?;
        let metadata_path = path.join("maven-metadata.xml");
        let metadata = format!(
            "<metadata>\n  <groupId>{}</groupId>\n  <artifactId>{}</artifactId>\n  <versioning>\n    <latest>{}</latest>\n    <release>{}</release>\n    <versions>\n      <version>{}</version>\n    </versions>\n    <lastUpdated>{}</lastUpdated>\n  </versioning>\n</metadata>\n",
            dep.group,
            dep.artifact,
            dep.version,
            dep.version,
            dep.version,
            Self::last_updated_stamp()
        );
        fs::write(&metadata_path, metadata)
            .with_context(|| format!("{} への書き込みに失敗しました", metadata_path.display()))?;
        Ok(())
    }

    fn write_artifacts(base_dir: &Path, dep: &DependencySpec) -> Result<()> {
        let mut version_dir = base_dir.join(dep.group_path());
        version_dir.push(dep.artifact);
        version_dir.push(dep.version);
        fs::create_dir_all(&version_dir)
            .with_context(|| format!("{} の作成に失敗しました", version_dir.display()))?;

        let jar_path = version_dir.join(dep.jar_name());
        let jar_bytes = Self::build_sample_jar(dep)?;
        fs::write(&jar_path, &jar_bytes)
            .with_context(|| format!("{} への書き込みに失敗しました", jar_path.display()))?;

        let checksum = format!("{:x}", Sha256::digest(&jar_bytes));
        let checksum_path = jar_path.with_extension("jar.sha256");
        let checksum_text = format!("{}  {}\n", checksum, dep.jar_name());
        fs::write(&checksum_path, checksum_text)
            .with_context(|| format!("{} への書き込みに失敗しました", checksum_path.display()))?;

        let pom_path = version_dir.join(dep.pom_name());
        let pom = format!(
            "<project>\n  <modelVersion>4.0.0</modelVersion>\n  <groupId>{}</groupId>\n  <artifactId>{}</artifactId>\n  <version>{}</version>\n  <packaging>jar</packaging>\n  <name>{}</name>\n{}\n</project>\n",
            dep.group,
            dep.artifact,
            dep.version,
            dep.artifact,
            emit_dependency_section(&dep.dependencies)
        );
        fs::write(&pom_path, pom)
            .with_context(|| format!("{} への書き込みに失敗しました", pom_path.display()))?;

        Ok(())
    }

    fn build_sample_jar(dep: &DependencySpec) -> Result<Vec<u8>> {
        let cursor = std::io::Cursor::new(Vec::new());
        let mut writer = zip::ZipWriter::new(cursor);
        let options = FileOptions::default().compression_method(CompressionMethod::Stored);

        writer
            .start_file("META-INF/MANIFEST.MF", options)
            .context("MANIFEST作成に失敗しました")?;
        writer
            .write_all(b"Manifest-Version: 1.0\nCreated-By: jv_pm wrapper test\n")
            .context("MANIFEST書き込みに失敗しました")?;

        let entry_name = format!("{}/README.txt", dep.artifact.replace('-', "/"));
        writer
            .start_file(entry_name, options)
            .context("JARエントリ作成に失敗しました")?;
        writer
            .write_all(b"wrapper mode artifact")
            .context("JARエントリ書き込みに失敗しました")?;

        let cursor = writer.finish().context("JAR末尾処理に失敗しました")?;
        Ok(cursor.into_inner())
    }

    fn last_updated_stamp() -> String {
        let now = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs();
        format!("2025{:0>10}", now % 10_000_000_000)
    }
}

fn emit_dependency_section(dependencies: &[DependencyCoordinate]) -> String {
    if dependencies.is_empty() {
        return String::new();
    }

    let mut buffer = String::new();
    buffer.push_str("  <dependencies>\n");
    for dep in dependencies {
        buffer.push_str(&format!(
            "    <dependency>\n      <groupId>{}</groupId>\n      <artifactId>{}</artifactId>\n      <version>{}</version>\n    </dependency>\n",
            dep.group, dep.artifact, dep.version
        ));
    }
    buffer.push_str("  </dependencies>");
    buffer
}

struct FakeMavenServer {
    address: SocketAddr,
    shutdown: Arc<AtomicBool>,
    handle: Option<thread::JoinHandle<()>>,
}

impl FakeMavenServer {
    fn start(base_dir: PathBuf) -> Result<Self> {
        let listener = TcpListener::bind(("127.0.0.1", 0))
            .context("テスト用HTTPサーバーの起動に失敗しました")?;
        listener
            .set_nonblocking(true)
            .context("HTTPリスナーの非同期設定に失敗しました")?;
        let address = listener
            .local_addr()
            .context("リスナーアドレスの取得に失敗しました")?;
        let shutdown = Arc::new(AtomicBool::new(false));
        let shutdown_clone = Arc::clone(&shutdown);
        let base_clone = base_dir.clone();
        let handle = thread::spawn(move || {
            Self::serve(listener, base_clone, shutdown_clone);
        });

        Ok(Self {
            address,
            shutdown,
            handle: Some(handle),
        })
    }

    fn base_url(&self) -> String {
        format!("http://{}", self.address)
    }

    fn serve(listener: TcpListener, base_dir: PathBuf, shutdown: Arc<AtomicBool>) {
        while !shutdown.load(Ordering::Relaxed) {
            match listener.accept() {
                Ok((stream, _)) => {
                    if let Err(error) = Self::handle_connection(stream, &base_dir) {
                        eprintln!("[fake-maven] connection error: {error:?}");
                    }
                }
                Err(err) if err.kind() == std::io::ErrorKind::WouldBlock => {
                    thread::sleep(Duration::from_millis(5));
                }
                Err(error) => {
                    eprintln!("[fake-maven] listener error: {error:?}");
                    break;
                }
            }
        }
    }

    fn handle_connection(mut stream: TcpStream, base_dir: &Path) -> Result<()> {
        let mut buffer = [0u8; 4096];
        let read = stream
            .read(&mut buffer)
            .context("HTTPリクエストの読み取りに失敗しました")?;
        if read == 0 {
            return Ok(());
        }

        let request = String::from_utf8_lossy(&buffer[..read]);
        let mut lines = request.lines();
        let request_line = lines.next().unwrap_or("");
        let mut segments = request_line.split_whitespace();
        let method = segments.next().unwrap_or("GET").to_uppercase();
        let raw_path = segments.next().unwrap_or("/");
        let path = raw_path.split('?').next().unwrap_or(raw_path);

        if method != "GET" && method != "HEAD" {
            let response = "HTTP/1.1 405 Method Not Allowed\r\nContent-Length: 0\r\nConnection: close\r\n\r\n";
            stream.write_all(response.as_bytes()).ok();
            return Ok(());
        }

        let resolved = base_dir.join(path.trim_start_matches('/'));
        if resolved.is_file() {
            let body = fs::read(&resolved)
                .with_context(|| format!("{} の読み込みに失敗しました", resolved.display()))?;
            let content_type = Self::detect_content_type(&resolved);
            let header = format!(
                "HTTP/1.1 200 OK\r\nContent-Length: {}\r\nContent-Type: {}\r\nConnection: close\r\n\r\n",
                body.len(),
                content_type
            );
            stream.write_all(header.as_bytes()).ok();
            if method != "HEAD" {
                stream.write_all(&body).ok();
            }
        } else {
            let response = "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\nConnection: close\r\n\r\n";
            stream.write_all(response.as_bytes()).ok();
        }

        Ok(())
    }

    fn detect_content_type(path: &Path) -> &'static str {
        match path.extension().and_then(|ext| ext.to_str()) {
            Some("xml") | Some("pom") => "application/xml",
            Some("sha256") => "text/plain",
            Some("jar") => "application/java-archive",
            _ => "application/octet-stream",
        }
    }
}

impl Drop for FakeMavenServer {
    fn drop(&mut self) {
        self.shutdown.store(true, Ordering::Relaxed);
        let _ = TcpStream::connect(self.address);
        if let Some(handle) = self.handle.take() {
            let _ = handle.join();
        }
    }
}

fn configure_command(command: &mut Command, project: &Path, home: &Path) -> Result<()> {
    command.current_dir(project);
    command.env("HOME", home);

    let root = repo_root();
    let java25 = root.join("toolchains/jdk25");
    let java21 = root.join("toolchains/jdk21");
    command.env("JAVA_HOME", &java25);
    command.env("JAVA25_HOME", &java25);
    command.env("JAVA21_HOME", &java21);

    let mut path_entries = Vec::new();
    path_entries.push(java25.join("bin"));
    path_entries.push(java21.join("bin"));
    if let Some(maven_dir) = maven_bin_dir() {
        path_entries.push(maven_dir);
    }
    if let Some(existing) = env::var_os("PATH") {
        path_entries.extend(env::split_paths(&existing));
    }
    let joined =
        env::join_paths(path_entries).context("PATH の結合に失敗しました")?;
    command.env("PATH", joined);

    Ok(())
}

fn write_global_config(home: &TempDir, repo_url: &str) -> Result<()> {
    let config_dir = home.path().join(".jv");
    fs::create_dir_all(&config_dir)
        .with_context(|| format!("{} の作成に失敗しました", config_dir.display()))?;
    let config_path = config_dir.join("config.toml");
    let contents = format!(
        "repositories = [{{ name = \"integration\", url = \"{}\", priority = 5 }}]\n",
        repo_url
    );
    fs::write(&config_path, contents)
        .with_context(|| format!("{} への書き込みに失敗しました", config_path.display()))?;
    Ok(())
}

fn run_jvpm_command(
    binary: &Path,
    project: &Path,
    home: &Path,
    args: &[&str],
) -> Result<Output> {
    let mut command = Command::new(binary);
    configure_command(&mut command, project, home)?;
    for arg in args {
        command.arg(arg);
    }
    command
        .output()
        .context("jvpm コマンドの実行に失敗しました")
}

fn cargo_bin_path(name: &str) -> Option<PathBuf> {
    let var_name = format!("CARGO_BIN_EXE_{}", name.replace('-', "_"));
    env::var_os(var_name).map(PathBuf::from)
}

fn maven_binary() -> Option<PathBuf> {
    if let Some(home) = env::var_os("MVN_HOME") {
        let candidate = PathBuf::from(home).join("bin").join("mvn");
        if candidate.exists() {
            return Some(candidate);
        }
    }
    let candidate = repo_root().join("toolchains/maven/bin/mvn");
    if candidate.exists() {
        return Some(candidate);
    }
    None
}

fn maven_bin_dir() -> Option<PathBuf> {
    maven_binary().and_then(|path| path.parent().map(|dir| dir.to_path_buf()))
}

fn repo_root() -> PathBuf {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    manifest_dir
        .ancestors()
        .find(|candidate| candidate.join("toolchains").is_dir())
        .expect("リポジトリルートを検出できませんでした")
        .to_path_buf()
}

fn assert_dependency_jar(repo_root: &Path, spec: &DependencySpec) -> Result<()> {
    let jar_path = repo_root
        .join(spec.group_path())
        .join(spec.artifact)
        .join(spec.version)
        .join(spec.jar_name());
    if !jar_path.exists() {
        bail!("Jar が見つかりません: {}", jar_path.display());
    }
    Ok(())
}

#[test]
fn e2e_wrapper_add_synchronizes_maven_files() -> Result<()> {
    let binary = match cargo_bin_path("jvpm") {
        Some(path) => path,
        None => {
            eprintln!("skipping e2e_wrapper_add_synchronizes_maven_files: jvpm binary unavailable");
            return Ok(());
        }
    };

    let dependencies = vec![DependencySpec::new("org.example", "demo", "1.0.0")];
    let home = tempdir().context("HOMEディレクトリの作成に失敗しました")?;
    let repo_dir = home.path().join("fake-maven");
    FakeMavenRepo::create(&repo_dir, &dependencies)?;
    let server = FakeMavenServer::start(repo_dir)?;
    write_global_config(&home, &server.base_url())?;

    let project = tempdir().context("プロジェクト用ディレクトリの作成に失敗しました")?;
    let args = ["add", "--non-interactive", "org.example:demo@1.0.0"];
    let output = run_jvpm_command(&binary, project.path(), home.path(), &args)?;
    assert!(
        output.status.success(),
        "jvpm add failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("更新: pom.xml / settings.xml / jv.lock"),
        "expected file summary in stdout"
    );

    let pom = fs::read_to_string(project.path().join("pom.xml"))
        .context("pom.xml の読み込みに失敗しました")?;
    assert!(pom.contains("<artifactId>demo</artifactId>"));
    let settings = fs::read_to_string(project.path().join("settings.xml"))
        .context("settings.xml の読み込みに失敗しました")?;
    assert!(settings.contains("<localRepository>"));
    let lockfile = fs::read_to_string(project.path().join("jv.lock"))
        .context("jv.lock の読み込みに失敗しました")?;
    assert!(lockfile.contains("wrapper-"));
    Ok(())
}

#[test]
fn e2e_wrapper_downloads_transitive_dependencies() -> Result<()> {
    let binary = match cargo_bin_path("jvpm") {
        Some(path) => path,
        None => {
            eprintln!(
                "skipping e2e_wrapper_downloads_transitive_dependencies: jvpm binary unavailable"
            );
            return Ok(());
        }
    };

    let root = DependencySpec::new("org.example", "demo-app", "1.0.0").with_dependencies(vec![
        DependencyCoordinate::new("org.libs", "support-lib", "2.0.0"),
    ]);
    let transitive = DependencySpec::new("org.libs", "support-lib", "2.0.0");
    let dependencies = vec![root.clone(), transitive.clone()];

    let home = tempdir().context("HOMEディレクトリの作成に失敗しました")?;
    let repo_dir = home.path().join("fake-maven");
    FakeMavenRepo::create(&repo_dir, &dependencies)?;
    let server = FakeMavenServer::start(repo_dir)?;
    write_global_config(&home, &server.base_url())?;

    let project = tempdir().context("プロジェクト用ディレクトリの作成に失敗しました")?;
    let args = ["add", "--non-interactive", "org.example:demo-app@1.0.0"];
    let output = run_jvpm_command(&binary, project.path(), home.path(), &args)?;
    assert!(
        output.status.success(),
        "jvpm add failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("[wrapper-mode] Maven ラッパーモードで起動しました"),
        "wrapper mode log missing: {stdout}"
    );

    let repo_root = project.path().join(".jv/repository");
    assert_dependency_jar(&repo_root, &root)?;
    assert_dependency_jar(&repo_root, &transitive)?;
    Ok(())
}

#[test]
fn e2e_wrapper_detects_native_project() -> Result<()> {
    let binary = match cargo_bin_path("jvpm") {
        Some(path) => path,
        None => {
            eprintln!("skipping e2e_wrapper_detects_native_project: jvpm binary unavailable");
            return Ok(());
        }
    };

    let dependencies = vec![DependencySpec::new("org.example", "demo", "1.0.0")];
    let home = tempdir().context("HOMEディレクトリの作成に失敗しました")?;
    let repo_dir = home.path().join("fake-maven");
    FakeMavenRepo::create(&repo_dir, &dependencies)?;
    let server = FakeMavenServer::start(repo_dir)?;
    write_global_config(&home, &server.base_url())?;

    let project = tempdir().context("プロジェクト用ディレクトリの作成に失敗しました")?;
    fs::write(
        project.path().join("jv.toml"),
        "[package]\nname = \"demo\"\nversion = \"0.1.0\"\n",
    )?;

    let args = ["add", "--non-interactive", "org.example:demo@1.0.0"];
    let output = run_jvpm_command(&binary, project.path(), home.path(), &args)?;
    assert!(
        !output.status.success(),
        "expected failure when jv.toml exists"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("JV ネイティブプロジェクト"),
        "unexpected error message: {stderr}"
    );
    Ok(())
}

#[test]
fn e2e_wrapper_detects_mixed_configuration() -> Result<()> {
    let binary = match cargo_bin_path("jvpm") {
        Some(path) => path,
        None => {
            eprintln!("skipping e2e_wrapper_detects_mixed_configuration: jvpm binary unavailable");
            return Ok(());
        }
    };

    let dependencies = vec![DependencySpec::new("org.example", "demo", "1.0.0")];
    let home = tempdir().context("HOMEディレクトリの作成に失敗しました")?;
    let repo_dir = home.path().join("fake-maven");
    FakeMavenRepo::create(&repo_dir, &dependencies)?;
    let server = FakeMavenServer::start(repo_dir)?;
    write_global_config(&home, &server.base_url())?;

    let project = tempdir().context("プロジェクト用ディレクトリの作成に失敗しました")?;
    fs::write(
        project.path().join("jv.toml"),
        "[package]\nname = \"demo\"\nversion = \"0.1.0\"\n",
    )?;
    fs::write(project.path().join("pom.xml"), "<project></project>")?;

    let args = ["add", "--non-interactive", "org.example:demo@1.0.0"];
    let output = run_jvpm_command(&binary, project.path(), home.path(), &args)?;
    assert!(
        !output.status.success(),
        "expected failure when both manifests exist"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("構成の混在"),
        "unexpected error message: {stderr}"
    );
    Ok(())
}

#[test]
fn e2e_wrapper_blocks_jv_commands() -> Result<()> {
    let binary = match cargo_bin_path("jvpm") {
        Some(path) => path,
        None => {
            eprintln!("skipping e2e_wrapper_blocks_jv_commands: jvpm binary unavailable");
            return Ok(());
        }
    };

    let dependencies = vec![DependencySpec::new("org.example", "demo", "1.0.0")];
    let home = tempdir().context("HOMEディレクトリの作成に失敗しました")?;
    let repo_dir = home.path().join("fake-maven");
    FakeMavenRepo::create(&repo_dir, &dependencies)?;
    let server = FakeMavenServer::start(repo_dir)?;
    write_global_config(&home, &server.base_url())?;

    let project = tempdir().context("プロジェクト用ディレクトリの作成に失敗しました")?;
    let args = ["repo", "list"];
    let output = run_jvpm_command(&binary, project.path(), home.path(), &args)?;
    assert!(
        !output.status.success(),
        "repo list should be rejected in wrapper mode"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("JV専用コマンド `repo`"),
        "unexpected error message: {stderr}"
    );
    Ok(())
}

#[test]
fn e2e_wrapper_strategy_flag_records_strategy() -> Result<()> {
    let binary = match cargo_bin_path("jvpm") {
        Some(path) => path,
        None => {
            eprintln!("skipping e2e_wrapper_strategy_flag_records_strategy: jvpm binary unavailable");
            return Ok(());
        }
    };

    let dependencies = vec![DependencySpec::new("org.example", "demo", "1.0.0")];
    let home = tempdir().context("HOMEディレクトリの作成に失敗しました")?;
    let repo_dir = home.path().join("fake-maven");
    FakeMavenRepo::create(&repo_dir, &dependencies)?;
    let server = FakeMavenServer::start(repo_dir)?;
    write_global_config(&home, &server.base_url())?;

    let workspace = tempdir().context("workspace ディレクトリの作成に失敗しました")?;
    let strategies = ["pubgrub", "breadth-first", "maven-compat"];
    for strategy in &strategies {
        let project = workspace.path().join(strategy.replace('-', "_"));
        fs::create_dir_all(&project)
            .with_context(|| format!("{} の作成に失敗しました", project.display()))?;
        let args = [
            "add",
            "--non-interactive",
            "--strategy",
            strategy,
            "org.example:demo@1.0.0",
        ];
        let output = run_jvpm_command(&binary, &project, home.path(), &args)?;
        assert!(
            output.status.success(),
            "jvpm add failed for strategy {}: {}",
            strategy,
            String::from_utf8_lossy(&output.stderr)
        );
        let lockfile = fs::read_to_string(project.join("jv.lock"))
            .context("jv.lock の読み込みに失敗しました")?;
        let manifest_marker = format!("\"wrapper-{}\"", strategy);
        assert!(
            lockfile.contains(&manifest_marker),
            "jv.lock に {} が含まれていません",
            manifest_marker
        );
    }
    drop(server);
    Ok(())
}
