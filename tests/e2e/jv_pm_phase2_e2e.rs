use std::env;
use std::ffi::{OsStr, OsString};
use std::fs;
use std::io::{Read, Write};
use std::net::{SocketAddr, TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::{
    atomic::{AtomicBool, Ordering},
    Arc, Mutex,
};
use std::thread;
use std::time::{Duration, SystemTime};

use anyhow::{Context, Result};
use serde::Deserialize;
use serde_json::Value;
use sha2::{Digest, Sha256};
use tempfile::{tempdir, TempDir};
use toml::{Value as TomlValue, map::Map as TomlMap};
use zip::write::FileOptions;
use zip::CompressionMethod;

#[cfg(unix)]
use std::os::unix::fs::{MetadataExt, PermissionsExt};

#[derive(Clone)]
struct DependencySpec {
    group: &'static str,
    artifact: &'static str,
    version: &'static str,
}

impl DependencySpec {
    const fn new(group: &'static str, artifact: &'static str, version: &'static str) -> Self {
        Self {
            group,
            artifact,
            version,
        }
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
        fs::create_dir_all(&path).with_context(|| format!("{} の作成に失敗しました", path.display()))?;
        path.push(&dep.artifact);
        fs::create_dir_all(&path).with_context(|| format!("{} の作成に失敗しました", path.display()))?;
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
        version_dir.push(&dep.artifact);
        version_dir.push(&dep.version);
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
            "<project>\n  <modelVersion>4.0.0</modelVersion>\n  <groupId>{}</groupId>\n  <artifactId>{}</artifactId>\n  <version>{}</version>\n  <packaging>jar</packaging>\n  <name>{}</name>\n</project>\n",
            dep.group, dep.artifact, dep.version, dep.artifact
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
            .write_all(b"Manifest-Version: 1.0\nCreated-By: jv_pm integration test\n")
            .context("MANIFEST書き込みに失敗しました")?;

        let entry_name = format!("{}/README.txt", dep.artifact.replace('-', "/"));
        writer
            .start_file(entry_name, options)
            .context("JARエントリ作成に失敗しました")?;
        writer
            .write_all(b"sample artifact for jv_pm tests")
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

#[derive(Clone, Debug)]
struct RequestLog {
    method: String,
    path: String,
}

struct FakeMavenServer {
    _base_dir: PathBuf,
    address: SocketAddr,
    shutdown: Arc<AtomicBool>,
    logs: Arc<Mutex<Vec<RequestLog>>>,
    handle: Option<thread::JoinHandle<()>>,
}

impl FakeMavenServer {
    fn start(base_dir: PathBuf) -> Result<Self> {
        let listener = TcpListener::bind(("127.0.0.1", 0)).context("テスト用HTTPサーバーの起動に失敗しました")?;
        listener
            .set_nonblocking(true)
            .context("HTTPリスナーの非同期設定に失敗しました")?;
        let address = listener.local_addr().context("リスナーアドレスの取得に失敗しました")?;
        let shutdown = Arc::new(AtomicBool::new(false));
        let logs = Arc::new(Mutex::new(Vec::new()));
        let base_clone = base_dir.clone();
        let shutdown_clone = Arc::clone(&shutdown);
        let logs_clone = Arc::clone(&logs);
        let handle = thread::spawn(move || Self::serve(listener, base_clone, shutdown_clone, logs_clone));

        Ok(Self {
            _base_dir: base_dir,
            address,
            shutdown,
            logs,
            handle: Some(handle),
        })
    }

    fn base_url(&self) -> String {
        format!("http://{}/", self.address)
    }

    fn jar_request_count(&self) -> usize {
        self.logs
            .lock()
            .expect("request log poisoned")
            .iter()
            .filter(|entry| entry.path.ends_with(".jar"))
            .count()
    }

    fn serve(listener: TcpListener, base_dir: PathBuf, shutdown: Arc<AtomicBool>, logs: Arc<Mutex<Vec<RequestLog>>>) {
        while !shutdown.load(Ordering::Relaxed) {
            match listener.accept() {
                Ok((stream, _)) => {
                    if let Err(error) = Self::handle_connection(stream, &base_dir, &logs) {
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

    fn handle_connection(mut stream: TcpStream, base_dir: &Path, logs: &Arc<Mutex<Vec<RequestLog>>>) -> Result<()> {
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

        logs.lock()
            .expect("request log poisoned")
            .push(RequestLog {
                method: method.clone(),
                path: path.to_string(),
            });

        let resolved = base_dir.join(path.trim_start_matches('/'));
        if method != "GET" && method != "HEAD" {
            let response = "HTTP/1.1 405 Method Not Allowed\r\nContent-Length: 0\r\nConnection: close\r\n\r\n";
            stream.write_all(response.as_bytes()).ok();
            return Ok(());
        }

        if resolved.is_file() {
            let body = fs::read(&resolved)
                .with_context(|| format!("{} の読み込みに失敗しました", resolved.display()))?;
            let content_type = Self::detect_content_type(&resolved);
            let header = format!(
                "HTTP/1.1 200 OK\r\nContent-Length: {}\r\nContent-Type: {}\r\nConnection: close\r\n\r\n",
                body.len(), content_type
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

struct TestEnvironment {
    home: TempDir,
    project: PathBuf,
    secondary_project: PathBuf,
    server: FakeMavenServer,
    dependencies: Vec<DependencySpec>,
}

impl TestEnvironment {
    fn new(dependencies: Vec<DependencySpec>) -> Result<Self> {
        let home = tempdir().context("HOME用テンポラリディレクトリの作成に失敗しました")?;
        let repo_dir = home.path().join("fake-maven");
        FakeMavenRepo::create(&repo_dir, &dependencies)?;
        let server = FakeMavenServer::start(repo_dir)?;

        let workspace = home.path().join("projects");
        fs::create_dir_all(&workspace)
            .with_context(|| format!("{} の作成に失敗しました", workspace.display()))?;
        let project = workspace.join("primary");
        fs::create_dir_all(&project)
            .with_context(|| format!("{} の作成に失敗しました", project.display()))?;
        let secondary_project = workspace.join("secondary");
        fs::create_dir_all(&secondary_project)
            .with_context(|| format!("{} の作成に失敗しました", secondary_project.display()))?;

        Ok(Self {
            home,
            project,
            secondary_project,
            server,
            dependencies,
        })
    }

    fn repo_url(&self) -> String {
        self.server.base_url()
    }

    fn run_jv<I, S>(&self, project: &Path, args: I) -> Result<Output>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        let binary = match cargo_bin_path("jv") {
            Some(path) => path,
            None => {
                anyhow::bail!("jv バイナリが見つからないためテストを実行できません");
            }
        };

        let mut command = Command::new(binary);
        self.configure_command(&mut command, project)?;
        for arg in args {
            command.arg(arg);
        }
        command.output().context("jv コマンドの実行に失敗しました")
    }

    fn run_jvpm<I, S>(&self, project: &Path, args: I) -> Result<Output>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        let binary = match cargo_bin_path("jvpm") {
            Some(path) => path,
            None => anyhow::bail!("jvpm バイナリが見つからないためテストを実行できません"),
        };

        let mut command = Command::new(binary);
        self.configure_command(&mut command, project)?;
        for arg in args {
            command.arg(arg);
        }
        command.output().context("jvpm コマンドの実行に失敗しました")
    }

    fn run_jvpm_with_env<I, S>(
        &self,
        project: &Path,
        args: I,
        extras: &[(&OsStr, &OsStr)],
    ) -> Result<Output>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        let binary = match cargo_bin_path("jvpm") {
            Some(path) => path,
            None => anyhow::bail!("jvpm バイナリが見つからないためテストを実行できません"),
        };

        let mut command = Command::new(binary);
        self.configure_command(&mut command, project)?;
        for (key, value) in extras {
            command.env(key, value);
        }
        for arg in args {
            command.arg(arg);
        }
        command.output().context("jvpm コマンドの実行に失敗しました")
    }

    fn configure_command(&self, command: &mut Command, project: &Path) -> Result<()> {
        command.current_dir(project);
        command.env("HOME", self.home.path());

        let root = repo_root();
        let java25 = root.join("toolchains/jdk25");
        let java21 = root.join("toolchains/jdk21");
        command.env("JAVA_HOME", &java25);
        command.env("JAVA25_HOME", &java25);
        command.env("JAVA21_HOME", &java21);

        if let Some(jvpm_bin) = cargo_bin_path("jvpm") {
            command.env("JVPM_BIN", jvpm_bin);
        }

        let mut path_entries = Vec::new();
        path_entries.push(java25.join("bin"));
        path_entries.push(java21.join("bin"));
        if let Some(maven_dir) = maven_bin_dir() {
            path_entries.push(maven_dir);
        }
        if let Some(existing) = env::var_os("PATH") {
            path_entries.extend(env::split_paths(&existing));
        }
        let joined = env::join_paths(path_entries).context("PATH の結合に失敗しました")?;
        command.env("PATH", joined);

        Ok(())
    }

    fn configure_manifest(&self, project: &Path) -> Result<()> {
        let manifest_path = project.join("jv.toml");
        let contents = fs::read_to_string(&manifest_path)
            .with_context(|| format!("{} の読み込みに失敗しました", manifest_path.display()))?;
        let mut manifest: TomlValue = toml::from_str(&contents)
            .with_context(|| format!("{} の解析に失敗しました", manifest_path.display()))?;

        let table = manifest
            .as_table_mut()
            .ok_or_else(|| anyhow::anyhow!("manifest root is not a table"))?;
        let repositories = table
            .entry("repositories".to_string())
            .or_insert_with(|| TomlValue::Table(TomlMap::new()));

        match repositories {
            TomlValue::Table(map) => {
                map.insert("use-global".to_string(), TomlValue::Boolean(false));
            }
            TomlValue::Array(values) => {
                if values.is_empty() {
                    let mut new_table = TomlMap::new();
                    new_table.insert("use-global".to_string(), TomlValue::Boolean(false));
                    *repositories = TomlValue::Table(new_table);
                }
            }
            _ => {
                let mut new_table = TomlMap::new();
                new_table.insert("use-global".to_string(), TomlValue::Boolean(false));
                *repositories = TomlValue::Table(new_table);
            }
        }

        let updated = toml::to_string_pretty(&manifest)
            .with_context(|| format!("{} のシリアライズに失敗しました", manifest_path.display()))?;
        fs::write(&manifest_path, updated)
            .with_context(|| format!("{} への書き込みに失敗しました", manifest_path.display()))?;
        Ok(())
    }

    fn cache_jar_path(&self, dep: &DependencySpec) -> PathBuf {
        let mut path = self.home.path().join(".jv/cache/jars");
        path.push(dep.group_path());
        path.push(&dep.artifact);
        path.push(&dep.version);
        path.join(dep.jar_name())
    }

    fn local_repo_jar_path(&self, project: &Path, dep: &DependencySpec) -> PathBuf {
        let mut path = project.join(".jv/repository");
        path.push(dep.group_path());
        path.push(&dep.artifact);
        path.push(&dep.version);
        path.join(dep.jar_name())
    }

    fn verify_cache_and_repository(&self, project: &Path, dep: &DependencySpec) -> Result<()> {
        let cache_path = self.cache_jar_path(dep);
        assert!(cache_path.exists(), "{} が存在しません", cache_path.display());
        let cache_checksum = cache_path.with_extension("jar.sha256");
        assert!(
            cache_checksum.exists(),
            "{} が存在しません",
            cache_checksum.display()
        );

        let repo_path = self.local_repo_jar_path(project, dep);
        assert!(repo_path.exists(), "{} が存在しません", repo_path.display());
        let repo_checksum = repo_path.with_extension("jar.sha256");
        assert!(
            repo_checksum.exists(),
            "{} が存在しません",
            repo_checksum.display()
        );

        #[cfg(unix)]
        {
            let cache_meta = fs::metadata(&cache_path)
                .with_context(|| format!("{} のメタデータ取得に失敗しました", cache_path.display()))?;
            let repo_meta = fs::metadata(&repo_path)
                .with_context(|| format!("{} のメタデータ取得に失敗しました", repo_path.display()))?;
            assert_eq!(
                cache_meta.ino(),
                repo_meta.ino(),
                "{} と {} はハードリンクではありません",
                cache_path.display(),
                repo_path.display()
            );
            assert!(
                cache_meta.nlink() >= 2,
                "{} のリンク数が期待値に達していません",
                cache_path.display()
            );
            assert_eq!(cache_meta.len(), repo_meta.len());

            let cache_checksum_meta = fs::metadata(&cache_checksum)
                .with_context(|| format!("{} のメタデータ取得に失敗しました", cache_checksum.display()))?;
            let repo_checksum_meta = fs::metadata(&repo_checksum)
                .with_context(|| format!("{} のメタデータ取得に失敗しました", repo_checksum.display()))?;
            assert_eq!(
                cache_checksum_meta.ino(),
                repo_checksum_meta.ino(),
                "checksum ファイルがハードリンクではありません"
            );
        }

        Ok(())
    }

    fn verify_export_outputs(&self, project: &Path, dependencies: &[DependencySpec]) -> Result<()> {
        let output_dir = project.join("target").join("java-project");
        let pom_path = output_dir.join("pom.xml");
        let settings_path = output_dir.join("settings.xml");
        let classpath_path = output_dir.join(".jv/classpath.txt");

        let pom = fs::read_to_string(&pom_path)
            .with_context(|| format!("{} の読み込みに失敗しました", pom_path.display()))?;
        for dep in dependencies {
            assert!(
                pom.contains(&format!("<artifactId>{}</artifactId>", dep.artifact)),
                "pom.xml に {} が含まれていません",
                dep.artifact
            );
            assert!(
                pom.contains(&format!("<version>{}</version>", dep.version)),
                "pom.xml に {} のバージョン {} が含まれていません",
                dep.artifact,
                dep.version
            );
        }

        let settings = fs::read_to_string(&settings_path)
            .with_context(|| format!("{} の読み込みに失敗しました", settings_path.display()))?;
        let local_repo_path = normalise_path(&project.join(".jv/repository"));
        assert!(
            settings.contains(&local_repo_path),
            "settings.xml にローカルリポジトリ {} が含まれていません",
            local_repo_path
        );

        let classpath = fs::read_to_string(&classpath_path)
            .with_context(|| format!("{} の読み込みに失敗しました", classpath_path.display()))?;
        for dep in dependencies {
            assert!(
                classpath.contains(&dep.jar_name()),
                "classpath.txt に {} が含まれていません",
                dep.jar_name()
            );
        }

        Ok(())
    }

    fn run_maven_install(&self, project: &Path) -> Result<Output> {
        let maven = maven_binary()
            .ok_or_else(|| anyhow::anyhow!("Mavenツールチェーンが見つかりません"))?;

        let output_dir = project.join("target/java-project");
        let settings = output_dir.join("settings.xml");

        let mut command = Command::new(maven);
        command.current_dir(&output_dir);
        command.env("HOME", self.home.path());
        command.env("MAVEN_USER_HOME", self.home.path().join(".m2"));

        let root = repo_root();
        let java_home = root.join("toolchains/jdk25");
        command.env("JAVA_HOME", &java_home);
        let mut path_entries = vec![java_home.join("bin")];
        if let Some(maven_dir) = maven_bin_dir() {
            path_entries.push(maven_dir);
        }
        if let Some(existing) = env::var_os("PATH") {
            path_entries.extend(env::split_paths(&existing));
        }
        let joined = env::join_paths(path_entries).context("PATH の結合に失敗しました")?;
        command.env("PATH", joined);

        command.arg("-s");
        command.arg(&settings);
        command.arg("install");
        command.arg("-DskipTests");

        command.output().context("mvn install の実行に失敗しました")
    }

    fn verify_maven_repository_unused(&self, dependencies: &[DependencySpec]) -> Result<()> {
        let m2_repo = self.home.path().join(".m2/repository");
        if !m2_repo.exists() {
            return Ok(());
        }

        for dep in dependencies {
            let mut path = m2_repo.clone();
            path.push(dep.group_path());
            path.push(&dep.artifact);
            if path.exists() {
                anyhow::bail!(
                    "Mavenローカルリポジトリに {} が生成されています ({})",
                    dep.coordinate(),
                    path.display()
                );
            }
        }

        Ok(())
    }
}

#[derive(Deserialize)]
struct RepoListEntry {
    name: String,
}

#[test]
fn e2e_maven_flow_with_local_cache_and_export() -> Result<()> {
    if cargo_bin_path("jv").is_none() {
        eprintln!("skipping e2e_maven_flow_with_local_cache_and_export: jv binary unavailable");
        return Ok(());
    }
    if cargo_bin_path("jvpm").is_none() {
        eprintln!("skipping e2e_maven_flow_with_local_cache_and_export: jvpm binary unavailable");
        return Ok(());
    }
    if maven_binary().is_none() {
        eprintln!("skipping e2e_maven_flow_with_local_cache_and_export: Maven toolchain unavailable");
        return Ok(());
    }

    let dependencies = vec![
        DependencySpec::new("org.example", "demo", "1.0.0"),
        DependencySpec::new("org.example", "util", "1.0.0"),
        DependencySpec::new("org.example", "data", "1.0.0"),
    ];

    let env = TestEnvironment::new(dependencies.clone())?;

    let init_output = env.run_jv(&env.project, ["init", "."])?;
    ensure_success(&init_output, "jv init");
    env.configure_manifest(&env.project)?;

    let repo_url = env.repo_url();
    ensure_success(
        &env.run_jv(
            &env.project,
            [
                "repo",
                "add",
                "integration",
                repo_url.as_str(),
                "--priority",
                "5",
            ],
        )?,
        "jv repo add integration",
    );

    ensure_success(
        &env.run_jv(
            &env.project,
            [
                "repo",
                "add",
                "temporary",
                repo_url.as_str(),
                "--priority",
                "50",
            ],
        )?,
        "jv repo add temporary",
    );

    let repo_list = env
        .run_jv(&env.project, ["repo", "list", "--json"])
        .context("repo list 実行失敗")?;
    ensure_success(&repo_list, "jv repo list");
    let entries: Vec<RepoListEntry> = serde_json::from_slice(&repo_list.stdout)
        .context("repo list JSON パースに失敗しました")?;
    assert!(entries.iter().any(|entry| entry.name == "integration"));
    assert!(entries.iter().any(|entry| entry.name == "temporary"));

    ensure_success(
        &env.run_jv(&env.project, ["repo", "remove", "temporary"])? ,
        "jv repo remove temporary",
    );
    let repo_list_after = env
        .run_jv(&env.project, ["repo", "list", "--json"])
        .context("repo list 実行失敗")?;
    ensure_success(&repo_list_after, "jv repo list after remove");
    let entries_after: Vec<RepoListEntry> = serde_json::from_slice(&repo_list_after.stdout)
        .context("repo list JSON パースに失敗しました")?;
    assert!(!entries_after.iter().any(|entry| entry.name == "temporary"));

    for dep in &dependencies {
        let coord = dep.coordinate_with_version();
        let output = env.run_jv(&env.project, ["add", "--non-interactive", coord.as_str()])?;
        ensure_success(&output, &format!("jv add {}", dep.coordinate()));
    }

    let lock_before = fs::read_to_string(env.project.join("jv.lock"))
        .context("jv.lock の読み込みに失敗しました")?;

    let build_output = env.run_jv(&env.project, ["build", "--java-only", "src/main.jv"])?;
    ensure_success(&build_output, "jv build");

    for dep in &dependencies {
        env.verify_cache_and_repository(&env.project, dep)?;
    }
    env.verify_export_outputs(&env.project, &dependencies)?;

    let maven_output = env.run_maven_install(&env.project)?;
    ensure_success(&maven_output, "mvn install");
    env.verify_maven_repository_unused(&dependencies)?;

    let resolver_list = env.run_jv(&env.project, ["resolver", "list", "--json"])?;
    ensure_success(&resolver_list, "jv resolver list");
    let strategies: Value = serde_json::from_slice(&resolver_list.stdout)
        .context("resolver list JSON パースに失敗しました")?;
    let names = strategies
        .as_array()
        .expect("resolver list should be array")
        .iter()
        .filter_map(|item| item.get("name"))
        .filter_map(Value::as_str)
        .collect::<Vec<_>>();
    assert!(names.contains(&"pubgrub"));
    assert!(names.contains(&"maven"));

    let resolver_info = env.run_jv(&env.project, ["resolver", "info", "maven", "--json"])?;
    ensure_success(&resolver_info, "jv resolver info maven");
    let info: Value = serde_json::from_slice(&resolver_info.stdout)
        .context("resolver info JSON パースに失敗しました")?;
    assert_eq!(info.get("name").and_then(Value::as_str), Some("maven"));
    assert_eq!(
        info.get("algorithm").and_then(Value::as_str),
        Some("maven_compat")
    );

    let jar_requests_before = env.server.jar_request_count();

    let init_secondary = env.run_jv(&env.secondary_project, ["init", "."])?;
    ensure_success(&init_secondary, "jv init secondary");
    env.configure_manifest(&env.secondary_project)?;
    ensure_success(
        &env.run_jv(
            &env.secondary_project,
            [
                "repo",
                "add",
                "integration",
                repo_url.as_str(),
                "--priority",
                "5",
            ],
        )?,
        "jv repo add secondary integration",
    );
    for dep in &dependencies {
        let coord = dep.coordinate_with_version();
        let output = env.run_jv(
            &env.secondary_project,
            ["add", "--non-interactive", coord.as_str()],
        )?;
        ensure_success(&output, &format!("secondary jv add {}", dep.coordinate()));
    }
    let build_secondary = env.run_jv(&env.secondary_project, ["build", "--java-only", "src/main.jv"])?;
    ensure_success(&build_secondary, "jv build secondary");

    let jar_requests_after = env.server.jar_request_count();
    assert_eq!(
        jar_requests_before,
        jar_requests_after,
        "二つ目のプロジェクトで再ダウンロードが発生しました"
    );

    ensure_success(
        &env.run_jv(
            &env.project,
            ["remove", "--non-interactive", dependencies[2].coordinate().as_str()],
        )?,
        "jv remove",
    );
    let lock_after = fs::read_to_string(env.project.join("jv.lock"))
        .context("更新後のjv.lock の読み込みに失敗しました")?;
    assert_ne!(lock_before, lock_after, "lockfile が更新されていません");

    Ok(())
}

#[cfg(unix)]
#[test]
fn e2e_jvpm_wrapper_passthrough() -> Result<()> {
    if cargo_bin_path("jv").is_none() {
        eprintln!("skipping e2e_jvpm_wrapper_passthrough: jv binary unavailable");
        return Ok(());
    }
    if cargo_bin_path("jvpm").is_none() {
        eprintln!("skipping e2e_jvpm_wrapper_passthrough: jvpm binary unavailable");
        return Ok(());
    }

    let dependencies = vec![
        DependencySpec::new("org.example", "demo", "1.0.0"),
        DependencySpec::new("org.example", "util", "1.0.0"),
    ];

    let env = TestEnvironment::new(dependencies.clone())?;

    let init_output = env.run_jv(&env.project, ["init", "."])?;
    ensure_success(&init_output, "jv init");
    env.configure_manifest(&env.project)?;

    let repo_url = env.repo_url();
    ensure_success(
        &env.run_jvpm(
            &env.project,
            [
                "repo",
                "add",
                "integration",
                repo_url.as_str(),
                "--priority",
                "5",
            ],
        )?,
        "jvpm repo add integration",
    );

    for dep in &dependencies {
        let coord = dep.coordinate_with_version();
        let add_output = env.run_jvpm(
            &env.project,
            ["add", "--non-interactive", coord.as_str()],
        )?;
        ensure_success(&add_output, &format!("jvpm add {}", dep.coordinate()));
    }

    let manifest_path = env.project.join("jv.toml");
    let manifest_text = fs::read_to_string(&manifest_path)
        .with_context(|| format!("{} の読み込みに失敗しました", manifest_path.display()))?;
    for dep in &dependencies {
        assert!(
            manifest_text.contains(&dep.coordinate()),
            "jv.toml に {} が含まれていません",
            dep.coordinate()
        );
    }

    let lock_path = env.project.join("jv.lock");
    assert!(lock_path.exists(), "jvpm add 実行後に jv.lock が生成されていません");

    let build_output = env.run_jv(&env.project, ["build", "--java-only", "src/main.jv"])?;
    ensure_success(&build_output, "jv build after jvpm add");

    for dep in &dependencies {
        env.verify_cache_and_repository(&env.project, dep)?;
    }
    env.verify_export_outputs(&env.project, &dependencies)?;

    let script_dir = env.home.path().join("maven-stub");
    fs::create_dir_all(&script_dir)
        .with_context(|| format!("{} の作成に失敗しました", script_dir.display()))?;
    let script_path = script_dir.join("mvn");
    let script_contents = r#"#!/usr/bin/env bash
set -e
: "${JVPM_MAVEN_LOG:?JVPM_MAVEN_LOG not set}"
printf "%s\n" "$@" >> "$JVPM_MAVEN_LOG"
if [[ "$1" == "unknown-command" ]]; then
  echo "Unknown goal $1" >&2
  exit 1
fi
exit 0
"#;
    fs::write(&script_path, script_contents)
        .with_context(|| format!("{} の書き込みに失敗しました", script_path.display()))?;
    let mut perms = fs::metadata(&script_path)
        .with_context(|| format!("{} のメタデータ取得に失敗しました", script_path.display()))?
        .permissions();
    perms.set_mode(0o755);
    fs::set_permissions(&script_path, perms)
        .with_context(|| format!("{} の権限設定に失敗しました", script_path.display()))?;

    let log_path = script_dir.join("maven.log");
    fs::write(&log_path, "")
        .with_context(|| format!("{} の初期化に失敗しました", log_path.display()))?;

    let env_pairs = vec![
        (
            OsString::from("JVPM_MAVEN_BIN"),
            script_path.as_os_str().to_os_string(),
        ),
        (
            OsString::from("JVPM_MAVEN_LOG"),
            log_path.as_os_str().to_os_string(),
        ),
    ];
    let env_refs: Vec<(&OsStr, &OsStr)> = env_pairs
        .iter()
        .map(|(key, value)| (key.as_os_str(), value.as_os_str()))
        .collect();

    let install_output = env.run_jvpm_with_env(&env.project, ["install"], &env_refs)?;
    ensure_success(&install_output, "jvpm install proxy");

    let clean_package_output =
        env.run_jvpm_with_env(&env.project, ["clean", "package"], &env_refs)?;
    ensure_success(&clean_package_output, "jvpm clean package proxy");

    let unknown_output =
        env.run_jvpm_with_env(&env.project, ["unknown-command"], &env_refs)?;
    assert!(
        !unknown_output.status.success(),
        "jvpm unknown-command が期待通り失敗していません"
    );
    let unknown_stderr = String::from_utf8_lossy(&unknown_output.stderr);
    assert!(
        unknown_stderr.contains("Unknown goal unknown-command"),
        "未知コマンドのstderrが期待値を含んでいません: {}",
        unknown_stderr
    );

    let maven_project = env.secondary_project.clone();
    fs::write(
        maven_project.join("pom.xml"),
        r#"<project>
  <modelVersion>4.0.0</modelVersion>
  <groupId>org.example</groupId>
  <artifactId>standalone</artifactId>
  <version>0.1.0</version>
</project>
"#,
    )
    .with_context(|| format!("{} の書き込みに失敗しました", maven_project.join("pom.xml").display()))?;
    fs::create_dir_all(maven_project.join(".jv").join("repository"))
        .with_context(|| format!("{} の作成に失敗しました", maven_project.join(".jv/repository").display()))?;

    let wrapper_output = env.run_jvpm_with_env(&maven_project, ["install"], &env_refs)?;
    ensure_success(&wrapper_output, "jvpm wrapper install");

    let m2_repo = env.home.path().join(".m2").join("repository");
    assert!(
        !m2_repo.exists(),
        "Mavenローカルリポジトリ {} が作成されています",
        m2_repo.display()
    );

    let log_contents = fs::read_to_string(&log_path)
        .with_context(|| format!("{} の読み込みに失敗しました", log_path.display()))?;
    let entries: Vec<&str> = log_contents
        .lines()
        .filter(|line| !line.trim().is_empty())
        .collect();
    assert_eq!(
        entries,
        ["install", "clean package", "unknown-command", "install"],
        "Mavenラッパーモードで実行されたコマンド順が一致しません: {:?}",
        entries
    );

    Ok(())
}

fn ensure_success(output: &Output, context: &str) {
    if !output.status.success() {
        panic!(
            "{} failed:\nstatus: {:?}\nstdout:\n{}\nstderr:\n{}",
            context,
            output.status,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }
}

fn normalise_path(path: &Path) -> String {
    let mut text = path.to_string_lossy().to_string();
    if std::path::MAIN_SEPARATOR != '/' {
        text = text.replace(std::path::MAIN_SEPARATOR, "/");
    }
    text
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
