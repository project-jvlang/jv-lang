use anyhow::{bail, Context, Result};
use assert_cmd::prelude::*;
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::env;
use std::fs;
use std::io::{Read, Write};
use std::net::{SocketAddr, TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::{
    atomic::{AtomicBool, Ordering},
    Arc, Mutex,
};
use std::thread;
use tempfile::tempdir;

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
    optional: bool,
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

    fn jar_name(&self) -> String {
        format!("{}-{}.jar", self.artifact, self.version)
    }

    fn pom_name(&self) -> String {
        format!("{}-{}.pom", self.artifact, self.version)
    }

    fn group_path(&self) -> PathBuf {
        let mut path = PathBuf::new();
        for segment in self.group.split('.') {
            path.push(segment);
        }
        path
    }
}

impl DependencyCoordinate {
    fn new(group: &'static str, artifact: &'static str, version: &'static str) -> Self {
        Self {
            group,
            artifact,
            version,
            optional: false,
        }
    }

    fn optional(mut self) -> Self {
        self.optional = true;
        self
    }
}

struct StubMavenRepo;

impl StubMavenRepo {
    fn create(base_dir: &Path, specs: &[DependencySpec]) -> Result<()> {
        if base_dir.exists() {
            fs::remove_dir_all(base_dir)
                .with_context(|| format!("failed to clear {}", base_dir.display()))?;
        }
        fs::create_dir_all(base_dir)
            .with_context(|| format!("failed to create {}", base_dir.display()))?;

        for spec in specs {
            Self::write_metadata(base_dir, spec)?;
            Self::write_artifacts(base_dir, spec)?;
        }

        Ok(())
    }

    fn write_metadata(base_dir: &Path, spec: &DependencySpec) -> Result<()> {
        let mut path = base_dir.join(spec.group_path());
        fs::create_dir_all(&path)
            .with_context(|| format!("failed to create {}", path.display()))?;
        path.push(spec.artifact);
        fs::create_dir_all(&path)
            .with_context(|| format!("failed to create {}", path.display()))?;
        let metadata_path = path.join("maven-metadata.xml");
        let metadata = format!(
            "<metadata>\n  <groupId>{}</groupId>\n  <artifactId>{}</artifactId>\n  <versioning>\n    <latest>{}</latest>\n    <release>{}</release>\n    <versions>\n      <version>{}</version>\n    </versions>\n    <lastUpdated>{}</lastUpdated>\n  </versioning>\n</metadata>\n",
            spec.group,
            spec.artifact,
            spec.version,
            spec.version,
            spec.version,
            Self::last_updated_stamp()
        );
        fs::write(&metadata_path, metadata)
            .with_context(|| format!("failed to write {}", metadata_path.display()))?;
        Ok(())
    }

    fn write_artifacts(base_dir: &Path, spec: &DependencySpec) -> Result<()> {
        let mut version_dir = base_dir.join(spec.group_path());
        version_dir.push(spec.artifact);
        version_dir.push(spec.version);
        fs::create_dir_all(&version_dir)
            .with_context(|| format!("failed to create {}", version_dir.display()))?;

        let jar_path = version_dir.join(spec.jar_name());
        let jar_bytes = format!("test-bytes-{}-{}", spec.artifact, spec.version).into_bytes();
        fs::write(&jar_path, &jar_bytes)
            .with_context(|| format!("failed to write {}", jar_path.display()))?;

        let checksum = Sha256::digest(&jar_bytes);
        let checksum_path = jar_path.with_extension("jar.sha256");
        let checksum_text = format!("{:x}  {}\n", checksum, spec.jar_name());
        fs::write(&checksum_path, checksum_text)
            .with_context(|| format!("failed to write {}", checksum_path.display()))?;

        let pom_path = version_dir.join(spec.pom_name());
        let pom = Self::render_pom(spec);
        fs::write(&pom_path, pom).with_context(|| format!("failed to write {}", pom_path.display()))?;

        Ok(())
    }

    fn last_updated_stamp() -> String {
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs();
        format!("2025{:0>10}", now % 10_000_000_000)
    }

    fn render_pom(spec: &DependencySpec) -> String {
        let mut buffer = String::new();
        buffer.push_str("<project>\n");
        buffer.push_str("  <modelVersion>4.0.0</modelVersion>\n");
        buffer.push_str(&format!("  <groupId>{}</groupId>\n", spec.group));
        buffer.push_str(&format!("  <artifactId>{}</artifactId>\n", spec.artifact));
        buffer.push_str(&format!("  <version>{}</version>\n", spec.version));
        buffer.push_str("  <packaging>jar</packaging>\n");
        if !spec.dependencies.is_empty() {
            buffer.push_str("  <dependencies>\n");
            for dep in &spec.dependencies {
                buffer.push_str("    <dependency>\n");
                buffer.push_str(&format!("      <groupId>{}</groupId>\n", dep.group));
                buffer.push_str(&format!("      <artifactId>{}</artifactId>\n", dep.artifact));
                buffer.push_str(&format!("      <version>{}</version>\n", dep.version));
                if dep.optional {
                    buffer.push_str("      <optional>true</optional>\n");
                }
                buffer.push_str("    </dependency>\n");
            }
            buffer.push_str("  </dependencies>\n");
        }
        buffer.push_str("</project>\n");
        buffer
    }
}

struct StubMavenServer {
    address: SocketAddr,
    shutdown: Arc<AtomicBool>,
    requests: Arc<Mutex<HashMap<String, usize>>>,
    handle: Option<thread::JoinHandle<()>>,
}

impl StubMavenServer {
    fn start(base_dir: PathBuf) -> Result<Self> {
        let listener = TcpListener::bind(("127.0.0.1", 0)).context("failed to bind stub server")?;
        listener
            .set_nonblocking(true)
            .context("failed to configure listener")?;
        let address = listener.local_addr().context("failed to read address")?;
        let shutdown = Arc::new(AtomicBool::new(false));
        let shutdown_clone = Arc::clone(&shutdown);
        let requests = Arc::new(Mutex::new(HashMap::new()));
        let request_clone = Arc::clone(&requests);
        let handle = thread::spawn(move || Self::serve(listener, base_dir, shutdown_clone, request_clone));

        Ok(Self {
            address,
            shutdown,
            requests,
            handle: Some(handle),
        })
    }

    fn base_url(&self) -> String {
        format!("http://{}", self.address)
    }

    fn request_count(&self, path_fragment: &str) -> usize {
        self.requests
            .lock()
            .ok()
            .and_then(|map| map.get(path_fragment).copied())
            .unwrap_or(0)
    }

    fn serve(
        listener: TcpListener,
        base_dir: PathBuf,
        shutdown: Arc<AtomicBool>,
        requests: Arc<Mutex<HashMap<String, usize>>>,
    ) {
        while !shutdown.load(Ordering::Relaxed) {
            match listener.accept() {
                Ok((stream, _)) => {
                    let _ = Self::handle_client(stream, &base_dir, &requests);
                }
                Err(err) if err.kind() == std::io::ErrorKind::WouldBlock => {
                    std::thread::sleep(std::time::Duration::from_millis(5));
                }
                Err(error) => {
                    eprintln!("stub server accept error: {error:?}");
                    break;
                }
            }
        }
    }

    fn handle_client(
        mut stream: TcpStream,
        base_dir: &Path,
        requests: &Arc<Mutex<HashMap<String, usize>>>,
    ) -> Result<()> {
        let mut buffer = [0u8; 2048];
        let read = stream.read(&mut buffer)?;
        if read == 0 {
            return Ok(());
        }

        let request = String::from_utf8_lossy(&buffer[..read]);
        let mut lines = request.lines();
        let Some(request_line) = lines.next() else {
            return Ok(());
        };

        let mut parts = request_line.split_whitespace();
        let method = parts.next().unwrap_or_default();
        let path = parts.next().unwrap_or("/");

        {
            let mut guard = requests.lock().expect("request map poisoned");
            *guard.entry(path.to_string()).or_default() += 1;
        }

        if !method.eq_ignore_ascii_case("GET") && !method.eq_ignore_ascii_case("HEAD") {
            return Ok(());
        }

        let resolved = base_dir.join(path.trim_start_matches('/'));
        if resolved.is_file() {
            let body = fs::read(&resolved)
                .with_context(|| format!("failed to read {}", resolved.display()))?;
            let content_type = Self::detect_content_type(&resolved);
            let header = format!(
                "HTTP/1.1 200 OK\r\nContent-Length: {}\r\nContent-Type: {}\r\nConnection: close\r\n\r\n",
                body.len(),
                content_type
            );
            stream.write_all(header.as_bytes()).ok();
            if !method.eq_ignore_ascii_case("HEAD") {
                stream.write_all(&body).ok();
            }
        } else {
            let response =
                "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\nConnection: close\r\n\r\n";
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

impl Drop for StubMavenServer {
    fn drop(&mut self) {
        self.shutdown.store(true, Ordering::Relaxed);
        let _ = TcpStream::connect(self.address);
        if let Some(handle) = self.handle.take() {
            let _ = handle.join();
        }
    }
}

fn repo_root() -> PathBuf {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    manifest_dir
        .ancestors()
        .find(|candidate| candidate.join("toolchains").is_dir())
        .expect("failed to locate repository root")
        .to_path_buf()
}

fn cargo_bin_path(name: &str) -> Option<PathBuf> {
    let var_name = format!("CARGO_BIN_EXE_{}", name.replace('-', "_"));
    env::var_os(var_name).map(PathBuf::from)
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

    let mut path_entries = vec![java25.join("bin"), java21.join("bin")];
    if let Some(maven_dir) = maven_bin_dir() {
        path_entries.push(maven_dir);
    }
    if let Some(existing) = env::var_os("PATH") {
        path_entries.extend(env::split_paths(&existing));
    }
    let joined = env::join_paths(path_entries).context("failed to join PATH entries")?;
    command.env("PATH", joined);

    Ok(())
}

fn maven_bin_dir() -> Option<PathBuf> {
    maven_binary().and_then(|path| path.parent().map(|dir| dir.to_path_buf()))
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

fn write_global_config(home: &Path, repo_url: &str) -> Result<()> {
    let config_dir = home.join(".jv");
    fs::create_dir_all(&config_dir)
        .with_context(|| format!("failed to create {}", config_dir.display()))?;
    let config_path = config_dir.join("config.toml");
    let contents = format!(
        "repositories = [{{ name = \"integration\", url = \"{}\", priority = 5 }}]\n",
        repo_url
    );
    fs::write(&config_path, contents)
        .with_context(|| format!("failed to write {}", config_path.display()))?;
    Ok(())
}

fn write_wrapper_pom(target: &Path) -> Result<()> {
    let pom = r#"<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
                             http://maven.apache.org/maven-v4_0_0.xsd">
  <modelVersion>4.0.0</modelVersion>
  <groupId>com.example</groupId>
  <artifactId>wrapper-project</artifactId>
  <version>0.1.0</version>
  <packaging>jar</packaging>
</project>
"#;
    fs::write(target.join("pom.xml"), pom)
        .with_context(|| format!("failed to write {}", target.join("pom.xml").display()))
}

fn write_jv_manifest(target: &Path, repo_url: &str) -> Result<()> {
    fs::create_dir_all(target.join("src"))
        .with_context(|| format!("failed to create {}", target.join("src").display()))?;
    fs::write(
        target.join("src/main.jv"),
        "fun main() { println(\"hello\") }\n",
    )
    .with_context(|| "failed to write src/main.jv")?;
    let manifest = format!(
        "[package]\nname = \"jv-project\"\nversion = \"0.1.0\"\n\n[package.dependencies]\n\n[project]\nentrypoint = \"src/main.jv\"\n\n[project.sources]\ninclude = [\"src/**/*.jv\"]\n\n[project.output]\ndirectory = \"target\"\n\n[[repositories]]\nname = \"integration\"\nurl = \"{}\"\npriority = 5\n",
        repo_url
    );
    fs::write(target.join("jv.toml"), manifest)
        .with_context(|| format!("failed to write {}", target.join("jv.toml").display()))
}

fn jar_path(root: &Path, spec: &DependencySpec) -> PathBuf {
    root.join(".jv")
        .join("repository")
        .join(spec.group_path())
        .join(spec.artifact)
        .join(spec.version)
        .join(spec.jar_name())
}

fn lockfile_contains(path: &Path, needle: &str) -> Result<bool> {
    let content =
        fs::read_to_string(path).with_context(|| format!("failed to read {}", path.display()))?;
    Ok(content.contains(needle))
}

fn sample_specs() -> Vec<DependencySpec> {
    let commons_transitive = DependencySpec::new("org.apache.commons", "commons-collections", "4.5");
    let commons = DependencySpec::new("org.apache.commons", "commons-lang3", "3.14.0")
        .with_dependencies(vec![DependencyCoordinate::new(
            "org.apache.commons",
            "commons-collections",
            "4.5",
        )]);

    let junit_api =
        DependencySpec::new("org.junit.jupiter", "junit-jupiter-api", "5.9.2").with_dependencies(
            vec![DependencyCoordinate::new(
                "org.opentest4j",
                "opentest4j",
                "1.3.0",
            )
            .optional()],
        );
    let junit = DependencySpec::new("org.junit.jupiter", "junit-jupiter", "5.9.2")
        .with_dependencies(vec![DependencyCoordinate::new(
            "org.junit.jupiter",
            "junit-jupiter-api",
            "5.9.2",
        )]);
    let opentest = DependencySpec::new("org.opentest4j", "opentest4j", "1.3.0");

    vec![commons, commons_transitive, junit, junit_api, opentest]
}

#[test]
fn wrapper_default_add_and_remove_manage_jars() -> Result<()> {
    let Some(jvpm_bin) = cargo_bin_path("jvpm") else {
        eprintln!("skipping wrapper_default_add_and_remove_manage_jars: jvpm binary unavailable");
        return Ok(());
    };

    let home = tempdir().context("failed to create home tempdir")?;
    let repo_dir = home.path().join("fake-maven");
    let specs = sample_specs();
    StubMavenRepo::create(&repo_dir, &specs)?;
    let server = StubMavenServer::start(repo_dir)?;
    write_global_config(home.path(), &server.base_url())?;

    let project = tempdir().context("failed to create project tempdir")?;
    write_wrapper_pom(project.path())?;

    let commons = specs
        .iter()
        .find(|spec| spec.artifact == "commons-lang3")
        .cloned()
        .expect("commons-lang3 spec missing");
    let collections = specs
        .iter()
        .find(|spec| spec.artifact == "commons-collections")
        .cloned()
        .expect("commons-collections spec missing");

    let mut add = Command::new(&jvpm_bin);
    configure_command(&mut add, project.path(), home.path())?;
    add.arg("add")
        .arg("--non-interactive")
        .arg("org.apache.commons:commons-lang3:3.14.0");
    let output = add.output().context("failed to run jvpm add")?;
    if !output.status.success() {
        bail!(
            "jvpm add failed: {}\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let pom = fs::read_to_string(project.path().join("pom.xml"))
        .context("failed to read pom.xml after add")?;
    assert!(
        pom.contains("commons-lang3"),
        "pom.xml did not contain commons-lang3 entry"
    );

    let lockfile = project.path().join("jv.lock");
    assert!(lockfile.exists(), "jv.lock was not generated");

    let commons_path = jar_path(project.path(), &commons);
    let collections_path = jar_path(project.path(), &collections);
    assert!(
        commons_path.exists(),
        "commons-lang3 jar missing: {}",
        commons_path.display()
    );
    assert!(
        collections_path.exists(),
        "transitive jar missing: {}",
        collections_path.display()
    );

    let mut remove = Command::new(&jvpm_bin);
    configure_command(&mut remove, project.path(), home.path())?;
    remove
        .arg("remove")
        .arg("--non-interactive")
        .arg("org.apache.commons:commons-lang3:3.14.0");
    let output = remove.output().context("failed to run jvpm remove")?;
    if !output.status.success() {
        bail!(
            "jvpm remove failed: {}\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    assert!(
        !commons_path.exists(),
        "commons-lang3 jar should be removed"
    );
    assert!(
        !collections_path.exists(),
        "transitive jar should be removed"
    );

    let pom_after = fs::read_to_string(project.path().join("pom.xml"))
        .context("failed to read pom.xml after remove")?;
    assert!(
        !pom_after.contains("commons-lang3"),
        "pom.xml still contains commons-lang3 after removal"
    );

    Ok(())
}

#[test]
fn wrapper_pubgrub_strategy_downloads_jars() -> Result<()> {
    let Some(jvpm_bin) = cargo_bin_path("jvpm") else {
        eprintln!("skipping wrapper_pubgrub_strategy_downloads_jars: jvpm binary unavailable");
        return Ok(());
    };

    let home = tempdir().context("failed to create home tempdir")?;
    let repo_dir = home.path().join("fake-maven");
    let specs = sample_specs();
    StubMavenRepo::create(&repo_dir, &specs)?;
    let server = StubMavenServer::start(repo_dir)?;
    write_global_config(home.path(), &server.base_url())?;

    let project = tempdir().context("failed to create project tempdir")?;
    write_wrapper_pom(project.path())?;

    let junit = specs
        .iter()
        .find(|spec| spec.artifact == "junit-jupiter")
        .cloned()
        .expect("junit-jupiter spec missing");
    let junit_api = specs
        .iter()
        .find(|spec| spec.artifact == "junit-jupiter-api")
        .cloned()
        .expect("junit-jupiter-api spec missing");

    let mut add = Command::new(&jvpm_bin);
    configure_command(&mut add, project.path(), home.path())?;
    add.args([
        "add",
        "--non-interactive",
        "--strategy",
        "pubgrub",
        "org.junit.jupiter:junit-jupiter:5.9.2",
    ]);
    let output = add.output().context("failed to run jvpm add with pubgrub")?;
    if !output.status.success() {
        bail!(
            "jvpm add (pubgrub) failed: {}\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let pom = fs::read_to_string(project.path().join("pom.xml"))
        .context("failed to read pom.xml after add")?;
    assert!(
        pom.contains("junit-jupiter"),
        "pom.xml did not contain junit-jupiter entry"
    );

    let junit_path = jar_path(project.path(), &junit);
    let api_path = jar_path(project.path(), &junit_api);
    assert!(junit_path.exists(), "junit jar missing: {}", junit_path.display());
    assert!(
        api_path.exists(),
        "junit api jar missing: {}",
        api_path.display()
    );

    Ok(())
}

#[test]
fn jv_native_default_downloads_and_records_lockfile() -> Result<()> {
    let Some(jv_bin) = cargo_bin_path("jv") else {
        eprintln!("skipping jv_native_default_downloads_and_records_lockfile: jv binary unavailable");
        return Ok(());
    };
    let Some(jvpm_bin) = cargo_bin_path("jvpm") else {
        eprintln!("skipping jv_native_default_downloads_and_records_lockfile: jvpm binary unavailable");
        return Ok(());
    };

    let home = tempdir().context("failed to create home tempdir")?;
    let repo_dir = home.path().join("fake-maven");
    let specs = sample_specs();
    StubMavenRepo::create(&repo_dir, &specs)?;
    let server = StubMavenServer::start(repo_dir)?;
    write_global_config(home.path(), &server.base_url())?;

    let project = tempdir().context("failed to create project tempdir")?;
    write_jv_manifest(project.path(), &server.base_url())?;

    let commons = specs
        .iter()
        .find(|spec| spec.artifact == "commons-lang3")
        .cloned()
        .expect("commons-lang3 spec missing");

    let mut add = Command::new(&jv_bin);
    configure_command(&mut add, project.path(), home.path())?;
    add.env("JVPM_BIN", &jvpm_bin);
    add.args([
        "add",
        "--non-interactive",
        "org.apache.commons:commons-lang3:3.14.0",
    ]);
    let output = add.output().context("failed to run jv add")?;
    if !output.status.success() {
        bail!(
            "jv add failed: {}\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let manifest = fs::read_to_string(project.path().join("jv.toml"))
        .context("failed to read jv.toml after add")?;
    assert!(
        manifest.contains("commons-lang3"),
        "manifest missing commons-lang3 entry"
    );

    let lockfile = project.path().join("jv.lock");
    assert!(lockfile.exists(), "jv.lock was not generated in native mode");
    assert!(
        lockfile_contains(&lockfile, "commons-lang3")?,
        "jv.lock missing commons-lang3 entry"
    );

    let commons_path = jar_path(project.path(), &commons);
    assert!(
        commons_path.exists(),
        "commons-lang3 jar missing: {}",
        commons_path.display()
    );

    Ok(())
}

#[test]
fn jv_native_maven_compat_strategy_downloads_via_wrapper_flow() -> Result<()> {
    let Some(jv_bin) = cargo_bin_path("jv") else {
        eprintln!("skipping jv_native_maven_compat_strategy_downloads_via_wrapper_flow: jv binary unavailable");
        return Ok(());
    };
    let Some(jvpm_bin) = cargo_bin_path("jvpm") else {
        eprintln!("skipping jv_native_maven_compat_strategy_downloads_via_wrapper_flow: jvpm binary unavailable");
        return Ok(());
    };

    let home = tempdir().context("failed to create home tempdir")?;
    let repo_dir = home.path().join("fake-maven");
    let specs = sample_specs();
    StubMavenRepo::create(&repo_dir, &specs)?;
    let server = StubMavenServer::start(repo_dir)?;
    write_global_config(home.path(), &server.base_url())?;

    let project = tempdir().context("failed to create project tempdir")?;
    write_jv_manifest(project.path(), &server.base_url())?;

    let junit = specs
        .iter()
        .find(|spec| spec.artifact == "junit-jupiter")
        .cloned()
        .expect("junit-jupiter spec missing");

    let mut add = Command::new(&jv_bin);
    configure_command(&mut add, project.path(), home.path())?;
    add.env("JVPM_BIN", &jvpm_bin);
    add.args([
        "add",
        "--non-interactive",
        "--strategy",
        "maven-compat",
        "org.junit.jupiter:junit-jupiter:5.9.2",
    ]);
    let output = add.output().context("failed to run jv add with maven-compat")?;
    if !output.status.success() {
        bail!(
            "jv add (maven-compat) failed: {}\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let lockfile = project.path().join("jv.lock");
    assert!(lockfile.exists(), "jv.lock was not generated");
    assert!(
        lockfile_contains(&lockfile, "junit-jupiter")?,
        "jv.lock missing junit-jupiter entry"
    );

    let junit_path = jar_path(project.path(), &junit);
    assert!(junit_path.exists(), "junit jar missing: {}", junit_path.display());

    Ok(())
}
