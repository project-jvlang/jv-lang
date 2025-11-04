use crate::BuildError;
use std::env;
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

const SUPPORTED_MAJOR_VERSIONS: &[u32] = &[21, 25];

#[derive(Debug, Clone)]
pub struct JdkInfo {
    pub javac_path: PathBuf,
    pub java_home: PathBuf,
    pub major_version: u32,
}

pub fn discover_jdk() -> Result<JdkInfo, BuildError> {
    let javac_path = find_javac().ok_or_else(|| {
        BuildError::JdkNotFound(
            "Unable to locate 'javac'. Ensure JDK 21 or 25 is installed.".into(),
        )
    })?;

    let java_home = find_jdk_home_from_javac(&javac_path).ok_or_else(|| {
        BuildError::JdkNotFound(format!(
            "Failed to derive JAVA_HOME from '{}'",
            javac_path.display()
        ))
    })?;

    let major_version = get_jdk_version(&java_home)?;

    if !SUPPORTED_MAJOR_VERSIONS.contains(&major_version) {
        return Err(BuildError::UnsupportedJdkVersion {
            detected: major_version,
        });
    }

    Ok(JdkInfo {
        javac_path,
        java_home,
        major_version,
    })
}

fn find_javac() -> Option<PathBuf> {
    find_javac_from_env()
        .or_else(find_javac_in_path)
        .or_else(search_known_installations)
}

fn find_javac_from_env() -> Option<PathBuf> {
    for var in ["JAVA_HOME", "JDK_HOME"] {
        if let Ok(value) = env::var(var) {
            let path = PathBuf::from(value);
            if let Some(javac) = javac_from_home(&path) {
                return Some(javac);
            }
        }
    }
    None
}

fn find_javac_in_path() -> Option<PathBuf> {
    which::which("javac").ok()
}

fn search_known_installations() -> Option<PathBuf> {
    for root in known_jdk_roots() {
        if let Some(found) = search_root_for_jdk(&root) {
            return Some(found);
        }
    }
    None
}

fn known_jdk_roots() -> Vec<PathBuf> {
    let mut roots = Vec::new();

    #[cfg(target_os = "windows")]
    {
        let program_files = env::var_os("ProgramFiles").map(PathBuf::from);
        let program_files_x86 = env::var_os("ProgramFiles(x86)").map(PathBuf::from);
        let common_roots = [
            PathBuf::from(r"C:\Java"),
            PathBuf::from(r"C:\Program Files\Java"),
            PathBuf::from(r"C:\Program Files\AdoptOpenJDK"),
            PathBuf::from(r"C:\Program Files\Eclipse Adoptium"),
            PathBuf::from(r"C:\Program Files\Microsoft"),
        ];

        roots.extend(common_roots);
        if let Some(dir) = program_files {
            roots.push(dir.join("Java"));
            roots.push(dir.join("Zulu"));
            roots.push(dir.join("ZuluJDK"));
        }
        if let Some(dir) = program_files_x86 {
            roots.push(dir.join("Java"));
            roots.push(dir.join("Zulu"));
        }
    }

    #[cfg(target_os = "macos")]
    {
        roots.push(PathBuf::from("/Library/Java/JavaVirtualMachines"));
        roots.push(PathBuf::from("/System/Library/Java/JavaVirtualMachines"));
        roots.push(PathBuf::from("/usr/local/Cellar/openjdk"));
        roots.push(PathBuf::from("/opt/homebrew/Cellar/openjdk"));
    }

    #[cfg(all(unix, not(target_os = "macos")))]
    {
        roots.push(PathBuf::from("/usr/lib/jvm"));
        roots.push(PathBuf::from("/usr/java"));
        roots.push(PathBuf::from("/opt/java"));
        roots.push(PathBuf::from("/opt/jdk"));
    }

    roots
}

fn search_root_for_jdk(root: &Path) -> Option<PathBuf> {
    if !root.exists() {
        return None;
    }

    if root.is_file() {
        return None;
    }

    if let Some(javac) = javac_from_home(root) {
        return Some(javac);
    }

    if let Ok(entries) = fs::read_dir(root) {
        for entry in entries.flatten() {
            let path = entry.path();
            if !path.is_dir() {
                continue;
            }

            #[cfg(target_os = "macos")]
            let candidate = {
                let contents_home = path.join("Contents").join("Home");
                if contents_home.exists() {
                    contents_home
                } else {
                    path.clone()
                }
            };

            #[cfg(not(target_os = "macos"))]
            let candidate = path.clone();

            if let Some(javac) = javac_from_home(&candidate) {
                return Some(javac);
            }
        }
    }

    None
}

fn javac_from_home(home: &Path) -> Option<PathBuf> {
    let exe_name = javac_executable();

    let candidate = home.join("bin").join(exe_name);
    if candidate.exists() {
        return Some(candidate);
    }

    if home.ends_with("bin") {
        let candidate = home.join(exe_name);
        if candidate.exists() {
            return Some(candidate);
        }
    }

    if home.file_name() == Some(OsStr::new(exe_name)) && home.is_file() {
        return Some(home.to_path_buf());
    }

    None
}

fn find_jdk_home_from_javac(javac_path: &Path) -> Option<PathBuf> {
    let bin_dir = javac_path.parent()?;
    let home = bin_dir.parent()?;
    let java_exe = bin_dir.join(java_executable());
    if java_exe.exists() {
        Some(home.to_path_buf())
    } else {
        None
    }
}

fn get_jdk_version(jdk_home: &Path) -> Result<u32, BuildError> {
    let java_exe = jdk_home.join("bin").join(java_executable());
    if !java_exe.exists() {
        return Err(BuildError::JdkNotFound(format!(
            "java executable not found at '{}'",
            java_exe.display()
        )));
    }

    let output = Command::new(&java_exe)
        .arg("-version")
        .output()
        .map_err(|error| {
            BuildError::JdkNotFound(format!(
                "Failed to run '{} -version': {}",
                java_exe.display(),
                error
            ))
        })?;

    let mut version_output = String::from_utf8_lossy(&output.stderr).to_string();
    if version_output.trim().is_empty() {
        version_output = String::from_utf8_lossy(&output.stdout).to_string();
    }

    parse_major_version(&version_output)
        .map_err(|error| BuildError::JdkVersionParse(error.to_string()))
}

fn parse_major_version(output: &str) -> Result<u32, &'static str> {
    if let Some(token) = extract_version_token(output) {
        return interpret_version_token(token).ok_or("Unsupported version token format");
    }
    Err("Failed to locate version token in output")
}

fn extract_version_token(output: &str) -> Option<&str> {
    for line in output.lines() {
        if let Some(start) = line.find('"') {
            let rest = &line[start + 1..];
            if let Some(end) = rest.find('"') {
                return Some(&rest[..end]);
            }
        }
    }
    None
}

fn interpret_version_token(token: &str) -> Option<u32> {
    if let Some(stripped) = token.strip_prefix("1.") {
        let mut parts = stripped.split(|ch| ch == '.' || ch == '_' || ch == '-');
        let minor = parts.next()?;
        return minor
            .chars()
            .take_while(|ch| ch.is_ascii_digit())
            .collect::<String>()
            .parse::<u32>()
            .ok();
    }

    let digits: String = token.chars().take_while(|ch| ch.is_ascii_digit()).collect();
    if digits.is_empty() {
        None
    } else {
        digits.parse::<u32>().ok()
    }
}

fn javac_executable() -> &'static str {
    if cfg!(windows) { "javac.exe" } else { "javac" }
}

fn java_executable() -> &'static str {
    if cfg!(windows) { "java.exe" } else { "java" }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{BuildConfig, BuildSystem};
    use std::ffi::OsString;
    use std::path::{Path, PathBuf};
    use std::sync::Mutex;
    use std::time::{SystemTime, UNIX_EPOCH};
    use std::{env, fs};

    #[cfg(unix)]
    use std::os::unix::fs::PermissionsExt;

    static ENV_MUTEX: Mutex<()> = Mutex::new(());

    #[test]
    fn interpret_version_token_supports_legacy_format() {
        assert_eq!(interpret_version_token("1.8.0_362"), Some(8));
    }

    #[test]
    fn interpret_version_token_supports_modern_format() {
        assert_eq!(interpret_version_token("21.0.2"), Some(21));
        assert_eq!(interpret_version_token("25-ea"), Some(25));
    }

    #[test]
    fn extract_version_token_finds_first_line() {
        let sample = r#"openjdk version "21.0.2" 2024-01-16
OpenJDK Runtime Environment (build 21.0.2)
OpenJDK 64-Bit Server VM (build 21.0.2)"#;
        assert_eq!(extract_version_token(sample), Some("21.0.2"));
    }

    #[test]
    fn parse_major_version_handles_multiple_formats() {
        let modern = r#"openjdk version "21.0.2"
OpenJDK Runtime Environment"#;
        let legacy = r#"java version "1.8.0_321"
Java(TM) SE Runtime Environment"#;
        assert_eq!(parse_major_version(modern).unwrap(), 21);
        assert_eq!(parse_major_version(legacy).unwrap(), 8);
    }

    #[cfg(unix)]
    #[test]
    fn discover_jdk_accepts_supported_java_home() {
        let _lock = ENV_MUTEX.lock().unwrap();
        let temp_jdk = TempJdk::new("openjdk version \"25.0.1\"", "javac 25.0.1");
        let _java_home = EnvVarGuard::set_path("JAVA_HOME", temp_jdk.home());

        let info = discover_jdk().expect("discover jdk");
        assert_eq!(info.major_version, 25);
        assert_eq!(info.java_home, temp_jdk.home().to_path_buf());
        assert_eq!(info.javac_path, temp_jdk.bin_path(javac_executable()));
    }

    #[cfg(unix)]
    #[test]
    fn discover_jdk_rejects_unsupported_version() {
        let _lock = ENV_MUTEX.lock().unwrap();
        let temp_jdk = TempJdk::new("openjdk version \"19.0.2\"", "javac 19.0.2");
        let _java_home = EnvVarGuard::set_path("JAVA_HOME", temp_jdk.home());

        match discover_jdk() {
            Err(BuildError::UnsupportedJdkVersion { detected }) => assert_eq!(detected, 19),
            other => panic!("expected UnsupportedJdkVersion error, got {:?}", other),
        }
    }

    #[cfg(unix)]
    #[test]
    fn build_system_check_javac_uses_discovered_jdk() {
        let _lock = ENV_MUTEX.lock().unwrap();
        let temp_jdk = TempJdk::new("openjdk version \"25.0.1\"", "javac 25.0.1");
        let _java_home = EnvVarGuard::set_path("JAVA_HOME", temp_jdk.home());

        let build_system = BuildSystem::new(BuildConfig::default());
        let version = build_system
            .check_javac_availability()
            .expect("javac availability");
        assert_eq!(version, "javac 25.0.1");
    }

    struct EnvVarGuard {
        key: &'static str,
        original: Option<OsString>,
    }

    impl EnvVarGuard {
        fn set_path(key: &'static str, value: &Path) -> Self {
            Self::set_os(key, value.as_os_str())
        }

        fn set_os(key: &'static str, value: &std::ffi::OsStr) -> Self {
            let original = env::var_os(key);
            env::set_var(key, value);
            Self { key, original }
        }
    }

    impl Drop for EnvVarGuard {
        fn drop(&mut self) {
            if let Some(value) = self.original.take() {
                env::set_var(self.key, value);
            } else {
                env::remove_var(self.key);
            }
        }
    }

    #[cfg(unix)]
    struct TempJdk {
        home: PathBuf,
    }

    #[cfg(unix)]
    impl TempJdk {
        fn new(java_version_line: &str, javac_version_line: &str) -> Self {
            let unique = format!(
                "jv-test-jdk-{}-{}",
                std::process::id(),
                SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .unwrap_or_default()
                    .as_nanos()
            );
            let home = env::temp_dir().join(unique);
            let bin_dir = home.join("bin");
            fs::create_dir_all(&bin_dir).expect("create fake jdk bin dir");

            let java_script = format!(
                "#!/bin/sh\ncat <<'__JV_END__' 1>&2\n{}\n__JV_END__\nexit 0\n",
                java_version_line
            );
            write_exec_script(&bin_dir.join(java_executable()), &java_script);

            let javac_script = format!(
                "#!/bin/sh\nif [ \"$1\" = \"--version\" ] || [ \"$1\" = \"-version\" ]; then\n  cat <<'__JV_END__'\n{}\n__JV_END__\nfi\nexit 0\n",
                javac_version_line
            );
            write_exec_script(&bin_dir.join(javac_executable()), &javac_script);

            Self { home }
        }

        fn home(&self) -> &Path {
            &self.home
        }

        fn bin_path(&self, name: &str) -> PathBuf {
            self.home.join("bin").join(name)
        }
    }

    #[cfg(unix)]
    impl Drop for TempJdk {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.home);
        }
    }

    #[cfg(unix)]
    fn write_exec_script(path: &Path, body: &str) {
        fs::write(path, body).expect("write script");
        let mut permissions = fs::metadata(path).expect("metadata").permissions();
        permissions.set_mode(0o755);
        fs::set_permissions(path, permissions).expect("set permissions");
    }
}
