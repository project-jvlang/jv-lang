use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::Duration;

use reqwest::blocking::Client;
use sha2::{Digest, Sha256};
use tempfile::TempDir;
use thiserror::Error;
use url::Url;

/// Source classification for fetched sample data.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SampleSourceKind {
    LocalFile,
    Http,
    S3,
    GitSsh,
    CachedFile,
}

/// Request parameters controlling sample data fetching.
#[derive(Debug, Clone)]
pub struct SampleFetchRequest {
    pub source: String,
    pub base_dir: Option<PathBuf>,
    pub allow_network: bool,
    pub expected_sha256: Option<String>,
    pub max_bytes: Option<u64>,
    pub cache_dir: Option<PathBuf>,
    pub aws_cli_path: Option<PathBuf>,
    pub git_cli_path: Option<PathBuf>,
    pub timeout: Duration,
}

impl SampleFetchRequest {
    pub fn new(source: impl Into<String>) -> Self {
        Self {
            source: source.into(),
            base_dir: None,
            allow_network: false,
            expected_sha256: None,
            max_bytes: None,
            cache_dir: None,
            aws_cli_path: None,
            git_cli_path: None,
            timeout: Duration::from_secs(30),
        }
    }
}

/// Result payload containing fetched bytes and metadata.
#[derive(Debug, Clone)]
pub struct SampleFetchResult {
    pub bytes: Vec<u8>,
    pub sha256: String,
    pub source_kind: SampleSourceKind,
    pub origin: String,
    pub cache_path: Option<PathBuf>,
}

#[derive(Debug, Error)]
pub enum SampleFetchError {
    #[error("無効なURIです: {uri} ({message})")]
    InvalidUri { uri: String, message: String },

    #[error("ネットワークアクセスが許可されていません: {uri}")]
    NetworkDisabled { uri: String },

    #[error("HTTPリクエストに失敗しました: {uri} ({message})")]
    HttpRequest { uri: String, message: String },

    #[error("HTTPレスポンスエラーです: {uri} (status={status})")]
    HttpResponse { uri: String, status: u16 },

    #[error("ファイルアクセスに失敗しました: {path} ({error})")]
    Io { path: PathBuf, #[source] error: io::Error },

    #[error("CLIが見つかりません: {command}")]
    CommandMissing { command: String },

    #[error("CLI実行に失敗しました: {command} (status={status:?}) {stderr}")]
    CommandFailed {
        command: String,
        status: Option<i32>,
        stderr: String,
    },

    #[error("サイズ上限を超えています (limit={limit} bytes, actual={actual} bytes)")]
    SizeLimitExceeded { limit: u64, actual: u64 },

    #[error("SHA256ハッシュが一致しません (expected={expected}, actual={actual})")]
    Sha256Mismatch { expected: String, actual: String },

    #[error("git+ssh URIにpathクエリがありません: {uri}")]
    GitPathMissing { uri: String },

    #[error("サポートされていないスキームです: {scheme}")]
    UnsupportedScheme { scheme: String },
}

/// フェッチと検証を一括で実行する。
pub fn fetch_sample_data(
    request: &SampleFetchRequest,
) -> Result<SampleFetchResult, SampleFetchError> {
    if let Some(cached) = try_read_cache(request)? {
        return Ok(cached);
    }

    let (bytes, kind) = fetch_uncached(request)?;
    enforce_limit(request, bytes.len() as u64)?;

    let sha256 = compute_sha256(&bytes);
    if let Some(expected) = &request.expected_sha256 {
        let expected_norm = expected.to_ascii_lowercase();
        if expected_norm != sha256 {
            return Err(SampleFetchError::Sha256Mismatch {
                expected: expected_norm,
                actual: sha256,
            });
        }
    }

    let cache_path = if let Some(dir) = request.cache_dir.as_ref() {
        Some(store_cache(dir, &sha256, &bytes)?)
    } else {
        None
    };

    Ok(SampleFetchResult {
        bytes,
        sha256,
        source_kind: kind,
        origin: request.source.clone(),
        cache_path,
    })
}

fn try_read_cache(request: &SampleFetchRequest) -> Result<Option<SampleFetchResult>, SampleFetchError> {
    let (cache_dir, expected_hash) = match (&request.cache_dir, &request.expected_sha256) {
        (Some(dir), Some(hash)) => (dir, hash.to_ascii_lowercase()),
        _ => return Ok(None),
    };

    let cache_path = cache_dir.join(&expected_hash);
    if !cache_path.exists() {
        return Ok(None);
    }

    let bytes = fs::read(&cache_path)
        .map_err(|error| SampleFetchError::Io { path: cache_path.clone(), error })?;
    enforce_limit(request, bytes.len() as u64)?;

    let actual_hash = compute_sha256(&bytes);
    if actual_hash != expected_hash {
        return Err(SampleFetchError::Sha256Mismatch {
            expected: expected_hash,
            actual: actual_hash,
        });
    }

    Ok(Some(SampleFetchResult {
        bytes,
        sha256: expected_hash,
        source_kind: SampleSourceKind::CachedFile,
        origin: request.source.clone(),
        cache_path: Some(cache_path),
    }))
}

fn store_cache(
    cache_dir: &Path,
    sha256: &str,
    bytes: &[u8],
) -> Result<PathBuf, SampleFetchError> {
    fs::create_dir_all(cache_dir)
        .map_err(|error| SampleFetchError::Io { path: cache_dir.to_path_buf(), error })?;
    let cache_path = cache_dir.join(sha256);
    fs::write(&cache_path, bytes)
        .map_err(|error| SampleFetchError::Io { path: cache_path.clone(), error })?;
    Ok(cache_path)
}

fn fetch_uncached(
    request: &SampleFetchRequest,
) -> Result<(Vec<u8>, SampleSourceKind), SampleFetchError> {
    match Url::parse(&request.source) {
        Ok(url) => match url.scheme() {
            "file" => fetch_local_file_from_url(&url).map(|data| (data, SampleSourceKind::LocalFile)),
            "http" | "https" => fetch_http(&url, request).map(|data| (data, SampleSourceKind::Http)),
            "s3" => fetch_s3(&request.source, request).map(|data| (data, SampleSourceKind::S3)),
            "git+ssh" => fetch_git_ssh(&url, request).map(|data| (data, SampleSourceKind::GitSsh)),
            other => Err(SampleFetchError::UnsupportedScheme {
                scheme: other.to_string(),
            }),
        },
        Err(_) => fetch_local_path(&request.source, request)
            .map(|data| (data, SampleSourceKind::LocalFile)),
    }
}

fn fetch_local_file_from_url(url: &Url) -> Result<Vec<u8>, SampleFetchError> {
    let path = url
        .to_file_path()
        .map_err(|_| SampleFetchError::InvalidUri {
            uri: url.to_string(),
            message: "file:// URI をパスに変換できませんでした".to_string(),
        })?;
    read_file(&path)
}

fn fetch_local_path(
    source: &str,
    request: &SampleFetchRequest,
) -> Result<Vec<u8>, SampleFetchError> {
    let path = Path::new(source);
    let resolved = if path.is_absolute() {
        path.to_path_buf()
    } else if let Some(base) = &request.base_dir {
        base.join(path)
    } else {
        std::env::current_dir()
            .map_err(|error| SampleFetchError::Io {
                path: PathBuf::from("."),
                error,
            })?
            .join(path)
    };

    read_file(&resolved)
}

fn read_file(path: &Path) -> Result<Vec<u8>, SampleFetchError> {
    fs::read(path).map_err(|error| SampleFetchError::Io {
        path: path.to_path_buf(),
        error,
    })
}

fn fetch_http(url: &Url, request: &SampleFetchRequest) -> Result<Vec<u8>, SampleFetchError> {
    ensure_network_allowed(url.as_str(), request)?;

    let client = Client::builder()
        .timeout(request.timeout)
        .build()
        .map_err(|err| SampleFetchError::HttpRequest {
            uri: url.to_string(),
            message: err.to_string(),
        })?;

    let response = client
        .get(url.clone())
        .send()
        .map_err(|err| SampleFetchError::HttpRequest {
            uri: url.to_string(),
            message: err.to_string(),
        })?;

    let status = response.status();
    if !status.is_success() {
        return Err(SampleFetchError::HttpResponse {
            uri: url.to_string(),
            status: status.as_u16(),
        });
    }

    response
        .bytes()
        .map(|bytes| bytes.to_vec())
        .map_err(|err| SampleFetchError::HttpRequest {
            uri: url.to_string(),
            message: err.to_string(),
        })
}

fn fetch_s3(source: &str, request: &SampleFetchRequest) -> Result<Vec<u8>, SampleFetchError> {
    ensure_network_allowed(source, request)?;
    let aws = resolve_command("aws", &request.aws_cli_path)?;

    let output = Command::new(aws)
        .args(["s3", "cp", source, "-", "--no-progress"])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .map_err(|err| SampleFetchError::CommandFailed {
            command: "aws".to_string(),
            status: None,
            stderr: err.to_string(),
        })?;

    if !output.status.success() {
        return Err(SampleFetchError::CommandFailed {
            command: "aws".to_string(),
            status: output.status.code(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
        });
    }

    Ok(output.stdout)
}

fn fetch_git_ssh(url: &Url, request: &SampleFetchRequest) -> Result<Vec<u8>, SampleFetchError> {
    ensure_network_allowed(url.as_str(), request)?;
    let git = resolve_command("git", &request.git_cli_path)?;

    let mut repo_url = url.clone();
    repo_url.set_scheme("ssh").map_err(|_| SampleFetchError::InvalidUri {
        uri: url.to_string(),
        message: "git+ssh URI を ssh スキームへ変換できません".to_string(),
    })?;
    repo_url.set_query(None);

    let mut file_path = None;
    let mut reference = None;
    if let Some(query) = url.query() {
        for (key, value) in query.split('&').filter_map(|pair| pair.split_once('=')) {
            let decoded = urlencoding::decode(value)
                .map_err(|err| SampleFetchError::InvalidUri {
                    uri: url.to_string(),
                    message: format!("query decode error: {err}"),
                })?
                .into_owned();
            match key {
                "path" => file_path = Some(PathBuf::from(decoded)),
                "ref" => reference = Some(decoded),
                _ => {}
            }
        }
    }

    let file_path = file_path.ok_or_else(|| SampleFetchError::GitPathMissing {
        uri: url.to_string(),
    })?;

    let temp_dir = TempDir::new().map_err(|err| SampleFetchError::CommandFailed {
        command: "git".to_string(),
        status: None,
        stderr: err.to_string(),
    })?;
    let repo_dir = temp_dir.path().join("repo");

    let mut command = Command::new(&git);
    command.arg("clone");
    command.arg("--depth");
    command.arg("1");
    if let Some(reference) = &reference {
        command.arg("--branch");
        command.arg(reference);
    }
    command.arg(repo_url.as_str());
    command.arg(&repo_dir);

    let output = command
        .stderr(Stdio::piped())
        .stdout(Stdio::null())
        .output()
        .map_err(|err| SampleFetchError::CommandFailed {
            command: "git".to_string(),
            status: None,
            stderr: err.to_string(),
        })?;

    if !output.status.success() {
        return Err(SampleFetchError::CommandFailed {
            command: "git".to_string(),
            status: output.status.code(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
        });
    }

    let full_path = repo_dir.join(&file_path);
    read_file(&full_path)
}

fn ensure_network_allowed(source: &str, request: &SampleFetchRequest) -> Result<(), SampleFetchError> {
    if request.allow_network {
        return Ok(());
    }
    Err(SampleFetchError::NetworkDisabled {
        uri: source.to_string(),
    })
}

fn resolve_command(command: &str, override_path: &Option<PathBuf>) -> Result<PathBuf, SampleFetchError> {
    if let Some(path) = override_path {
        if path.exists() {
            return Ok(path.clone());
        }
        return Err(SampleFetchError::CommandMissing {
            command: path.display().to_string(),
        });
    }

    which::which(command).map_err(|_| SampleFetchError::CommandMissing {
        command: command.to_string(),
    })
}

fn compute_sha256(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    let digest = hasher.finalize();
    hex::encode(digest)
}

fn enforce_limit(request: &SampleFetchRequest, actual: u64) -> Result<(), SampleFetchError> {
    if let Some(limit) = request.max_bytes {
        if actual > limit {
            return Err(SampleFetchError::SizeLimitExceeded { limit, actual });
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use std::net::TcpListener;
    use std::thread;

    fn write_temp_file(content: &str) -> (TempDir, PathBuf) {
        let dir = TempDir::new().expect("temp dir");
        let file_path = dir.path().join("data.json");
        let mut file = fs::File::create(&file_path).expect("create file");
        file.write_all(content.as_bytes()).expect("write file");
        (dir, file_path)
    }

    #[test]
    fn fetches_local_file_by_relative_path() {
        let (dir, file_path) = write_temp_file("{\"name\": \"Alice\"}");
        let relative = file_path
            .strip_prefix(dir.path())
            .unwrap()
            .to_string_lossy()
            .to_string();

        let mut request = SampleFetchRequest::new(relative);
        request.base_dir = Some(dir.path().to_path_buf());
        request.expected_sha256 = Some(
            "6d4a333838d0ef96756cccc680af2531075c512502fb68c5503c63d93de859b3".to_string(),
        );
        request.cache_dir = Some(dir.path().join("cache"));

        let result1 = fetch_sample_data(&request).expect("fetch local");
        assert_eq!(result1.source_kind, SampleSourceKind::LocalFile);
        assert!(result1.cache_path.is_some());
        assert_eq!(result1.bytes, fs::read(&file_path).unwrap());

        let result2 = fetch_sample_data(&request).expect("fetch cached");
        assert_eq!(result2.source_kind, SampleSourceKind::CachedFile);
        assert_eq!(result2.bytes, fs::read(file_path).unwrap());
    }

    #[test]
    fn honors_size_limit() {
        let (dir, file_path) = write_temp_file("123456");
        let relative = file_path
            .strip_prefix(dir.path())
            .unwrap()
            .to_string_lossy()
            .to_string();

        let mut request = SampleFetchRequest::new(relative);
        request.base_dir = Some(dir.path().to_path_buf());
        request.max_bytes = Some(2);

        let error = fetch_sample_data(&request).expect_err("size limit error");
        assert!(matches!(error, SampleFetchError::SizeLimitExceeded { .. }));
    }

    #[test]
    fn blocks_network_when_not_allowed() {
        let request = SampleFetchRequest::new("https://example.com/data.json");
        let error = fetch_sample_data(&request).expect_err("network disabled");
        assert!(matches!(error, SampleFetchError::NetworkDisabled { .. }));
    }

    #[test]
    fn fetches_http_when_allowed() {
        let listener = match TcpListener::bind("127.0.0.1:0") {
            Ok(listener) => listener,
            Err(err) => {
                eprintln!("Skipping HTTP fetch test: {err}");
                return;
            }
        };
        let addr = listener.local_addr().unwrap();
        let handle = thread::spawn(move || {
            if let Ok((mut stream, _)) = listener.accept() {
                let response = "HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\ntest";
                let _ = stream.write_all(response.as_bytes());
            }
        });

        let mut request = SampleFetchRequest::new(format!("http://{addr}/data"));
        request.allow_network = true;

        let result = fetch_sample_data(&request).expect("fetch http");
        assert_eq!(result.source_kind, SampleSourceKind::Http);
        assert_eq!(result.bytes, b"test");

        handle.join().unwrap();
    }

    #[test]
    fn reports_missing_cli_for_s3() {
        let mut request = SampleFetchRequest::new("s3://bucket/object.json");
        request.allow_network = true;
        request.aws_cli_path = Some(PathBuf::from("/does/not/exist/aws"));

        let error = fetch_sample_data(&request).expect_err("missing aws");
        assert!(matches!(error, SampleFetchError::CommandMissing { .. }));
    }

    #[test]
    fn reports_missing_cli_for_git() {
        let url = "git+ssh://git@example.com/repo.git?path=data.json";
        let mut request = SampleFetchRequest::new(url);
        request.allow_network = true;
        request.git_cli_path = Some(PathBuf::from("/does/not/exist/git"));

        let error = fetch_sample_data(&request).expect_err("missing git");
        assert!(matches!(error, SampleFetchError::CommandMissing { .. }));
    }
}
