use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::fs;
use std::io::{self, Cursor};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::Duration;

use reqwest::blocking::Client;
use serde_json::{Map, Value};
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
    Io {
        path: PathBuf,
        #[source]
        error: io::Error,
    },

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

/// サポートされているデータフォーマット。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DataFormat {
    Json,
    Csv,
    Tsv,
}

impl fmt::Display for DataFormat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DataFormat::Json => write!(f, "JSON"),
            DataFormat::Csv => write!(f, "CSV"),
            DataFormat::Tsv => write!(f, "TSV"),
        }
    }
}

/// 推論されたスキーマ表現。
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Schema {
    Primitive(PrimitiveType),
    Object {
        fields: BTreeMap<String, Schema>,
        required: BTreeSet<String>,
    },
    Array {
        element_type: Box<Schema>,
    },
    Optional(Box<Schema>),
    Union(Vec<Schema>),
}

/// プリミティブ型の分類。
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum PrimitiveType {
    String,
    Boolean,
    Integer,
    Long,
    BigInteger,
    Double,
    BigDecimal,
    Null,
}

impl PrimitiveType {
    fn is_integral(self) -> bool {
        matches!(
            self,
            PrimitiveType::Integer | PrimitiveType::Long | PrimitiveType::BigInteger
        )
    }

    fn is_decimal(self) -> bool {
        matches!(self, PrimitiveType::Double | PrimitiveType::BigDecimal)
    }

    fn is_numeric(self) -> bool {
        self.is_integral() || self.is_decimal()
    }
}

#[derive(Debug, Error)]
pub enum SchemaError {
    #[error("JSONデータの解析に失敗しました: {message}")]
    InvalidJson { message: String },

    #[error("{format}データの解析に失敗しました: {message}")]
    InvalidTabular { format: DataFormat, message: String },

    #[error("カラムヘッダーが見つかりません")]
    MissingHeaders,

    #[error("データセットが空です")]
    EmptyDataset,
}

/// サンプルデータのフォーマットに応じてスキーマを推論する。
pub fn infer_schema(data: &[u8], format: DataFormat) -> Result<Schema, SchemaError> {
    match format {
        DataFormat::Json => infer_json_schema(data),
        DataFormat::Csv => infer_tabular_schema(data, b',', DataFormat::Csv),
        DataFormat::Tsv => infer_tabular_schema(data, b'\t', DataFormat::Tsv),
    }
}

fn infer_json_schema(data: &[u8]) -> Result<Schema, SchemaError> {
    let value: Value = serde_json::from_slice(data).map_err(|err| SchemaError::InvalidJson {
        message: err.to_string(),
    })?;

    if let Some(array) = value.as_array() {
        if array.is_empty() {
            return Err(SchemaError::EmptyDataset);
        }
    }

    Ok(infer_json_value_schema(&value))
}

fn infer_json_value_schema(value: &Value) -> Schema {
    match value {
        Value::Null => Schema::Primitive(PrimitiveType::Null),
        Value::Bool(_) => Schema::Primitive(PrimitiveType::Boolean),
        Value::Number(number) => Schema::Primitive(classify_json_number(number)),
        Value::String(_) => Schema::Primitive(PrimitiveType::String),
        Value::Array(values) => infer_json_array_schema(values),
        Value::Object(map) => infer_json_object_schema(map),
    }
}

fn infer_json_object_schema(map: &Map<String, Value>) -> Schema {
    let mut fields = BTreeMap::new();
    let mut required = BTreeSet::new();

    for (key, value) in map {
        fields.insert(key.clone(), infer_json_value_schema(value));
        required.insert(key.clone());
    }

    Schema::Object { fields, required }
}

fn infer_json_array_schema(values: &[Value]) -> Schema {
    let mut element_schema: Option<Schema> = None;

    for value in values {
        let schema = infer_json_value_schema(value);
        element_schema = Some(match element_schema {
            Some(existing) => merge_schema(existing, schema),
            None => schema,
        });
    }

    Schema::Array {
        element_type: Box::new(
            element_schema.unwrap_or_else(|| Schema::Primitive(PrimitiveType::Null)),
        ),
    }
}

fn classify_json_number(number: &serde_json::Number) -> PrimitiveType {
    let literal = number.to_string();
    classify_numeric_literal(&literal).unwrap_or(PrimitiveType::Double)
}

#[derive(Clone, Default)]
struct ColumnAccumulator {
    schema: Option<Schema>,
    present_count: usize,
}

fn infer_tabular_schema(
    data: &[u8],
    delimiter: u8,
    format: DataFormat,
) -> Result<Schema, SchemaError> {
    let mut reader = csv::ReaderBuilder::new()
        .delimiter(delimiter)
        .has_headers(true)
        .from_reader(Cursor::new(data));

    let headers = reader
        .headers()
        .map_err(|err| SchemaError::InvalidTabular {
            format,
            message: err.to_string(),
        })?
        .clone();

    if headers.is_empty() {
        return Err(SchemaError::MissingHeaders);
    }

    let header_names: Vec<String> = headers.iter().map(|h| h.trim().to_string()).collect();
    let mut accumulators = vec![ColumnAccumulator::default(); header_names.len()];
    let mut record_count = 0usize;

    for record in reader.records() {
        let record = record.map_err(|err| SchemaError::InvalidTabular {
            format,
            message: err.to_string(),
        })?;

        record_count += 1;

        for (idx, value) in record.iter().enumerate().take(header_names.len()) {
            let trimmed = value.trim();
            if trimmed.is_empty() || trimmed.eq_ignore_ascii_case("null") {
                continue;
            }

            let value_schema = infer_tabular_value_schema(trimmed);
            let accumulator = &mut accumulators[idx];
            let merged = match accumulator.schema.clone() {
                Some(existing) => merge_schema(existing, value_schema),
                None => value_schema,
            };
            accumulator.schema = Some(merged);
            accumulator.present_count += 1;
        }
    }

    if record_count == 0 {
        return Err(SchemaError::EmptyDataset);
    }

    let mut fields = BTreeMap::new();
    let mut required = BTreeSet::new();

    for (idx, header) in header_names.iter().enumerate() {
        let accumulator = &accumulators[idx];
        let schema = accumulator
            .schema
            .clone()
            .unwrap_or_else(|| Schema::Primitive(PrimitiveType::Null));
        fields.insert(header.clone(), schema);
        if accumulator.present_count == record_count {
            required.insert(header.clone());
        }
    }

    Ok(Schema::Array {
        element_type: Box::new(Schema::Object { fields, required }),
    })
}

fn infer_tabular_value_schema(value: &str) -> Schema {
    let lowered = value.to_ascii_lowercase();
    if lowered == "true" || lowered == "false" {
        return Schema::Primitive(PrimitiveType::Boolean);
    }

    if let Some(numeric) = classify_numeric_literal(value) {
        return Schema::Primitive(numeric);
    }

    Schema::Primitive(PrimitiveType::String)
}

fn classify_numeric_literal(literal: &str) -> Option<PrimitiveType> {
    let trimmed = literal.trim();
    if trimmed.is_empty() {
        return None;
    }

    let without_plus = trimmed.strip_prefix('+').unwrap_or(trimmed);

    if without_plus.contains('.') || without_plus.contains('e') || without_plus.contains('E') {
        let has_digit = without_plus.chars().any(|c| c.is_ascii_digit());
        if !has_digit {
            return None;
        }

        if without_plus.parse::<f64>().is_err() {
            return None;
        }

        if needs_big_decimal(without_plus) {
            return Some(PrimitiveType::BigDecimal);
        }

        return Some(PrimitiveType::Double);
    }

    let digits = without_plus.trim_start_matches('-');
    if !digits.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }

    if let Ok(value) = without_plus.parse::<i64>() {
        if value >= i64::from(i32::MIN) && value <= i64::from(i32::MAX) {
            Some(PrimitiveType::Integer)
        } else {
            Some(PrimitiveType::Long)
        }
    } else {
        Some(PrimitiveType::BigInteger)
    }
}

fn needs_big_decimal(literal: &str) -> bool {
    let digit_count = literal.chars().filter(|c| c.is_ascii_digit()).count();
    digit_count > 15 || literal.contains('e') || literal.contains('E')
}

fn merge_schema(left: Schema, right: Schema) -> Schema {
    if left == right {
        return left;
    }

    match (left, right) {
        (Schema::Optional(left_inner), Schema::Optional(right_inner)) => {
            Schema::Optional(Box::new(merge_schema(*left_inner, *right_inner)))
        }
        (Schema::Optional(left_inner), right) => {
            Schema::Optional(Box::new(merge_schema(*left_inner, right)))
        }
        (left, Schema::Optional(right_inner)) => {
            Schema::Optional(Box::new(merge_schema(left, *right_inner)))
        }
        (Schema::Primitive(PrimitiveType::Null), right) => optionalize(right),
        (left, Schema::Primitive(PrimitiveType::Null)) => optionalize(left),
        (Schema::Primitive(left_prim), Schema::Primitive(right_prim)) => {
            if let Some(merged) = merge_primitive_types(left_prim, right_prim) {
                Schema::Primitive(merged)
            } else {
                make_union(vec![
                    Schema::Primitive(left_prim),
                    Schema::Primitive(right_prim),
                ])
            }
        }
        (
            Schema::Array {
                element_type: left_element,
            },
            Schema::Array {
                element_type: right_element,
            },
        ) => Schema::Array {
            element_type: Box::new(merge_schema(*left_element, *right_element)),
        },
        (
            Schema::Object {
                fields: left_fields,
                required: left_required,
            },
            Schema::Object {
                fields: right_fields,
                required: right_required,
            },
        ) => Schema::Object {
            fields: merge_object_fields(left_fields, right_fields),
            required: merge_required_fields(left_required, right_required),
        },
        (Schema::Union(left_variants), Schema::Union(right_variants)) => {
            make_union(left_variants.into_iter().chain(right_variants))
        }
        (Schema::Union(left_variants), right) => {
            make_union(left_variants.into_iter().chain(std::iter::once(right)))
        }
        (left, Schema::Union(right_variants)) => {
            make_union(std::iter::once(left).chain(right_variants.into_iter()))
        }
        (left, right) => make_union(vec![left, right]),
    }
}

fn merge_object_fields(
    mut left_fields: BTreeMap<String, Schema>,
    mut right_fields: BTreeMap<String, Schema>,
) -> BTreeMap<String, Schema> {
    let mut keys = BTreeSet::new();
    keys.extend(left_fields.keys().cloned());
    keys.extend(right_fields.keys().cloned());

    let mut merged = BTreeMap::new();

    for key in keys {
        match (left_fields.remove(&key), right_fields.remove(&key)) {
            (Some(left), Some(right)) => {
                merged.insert(key.clone(), merge_schema(left, right));
            }
            (Some(left), None) => {
                merged.insert(key.clone(), left);
            }
            (None, Some(right)) => {
                merged.insert(key.clone(), right);
            }
            (None, None) => {}
        }
    }

    merged
}

fn merge_required_fields(
    mut left_required: BTreeSet<String>,
    right_required: BTreeSet<String>,
) -> BTreeSet<String> {
    left_required.retain(|key| right_required.contains(key));
    left_required
}

fn merge_primitive_types(a: PrimitiveType, b: PrimitiveType) -> Option<PrimitiveType> {
    if a == b {
        return Some(a);
    }

    if a.is_numeric() && b.is_numeric() {
        return Some(widen_numeric(a, b));
    }

    None
}

fn widen_numeric(a: PrimitiveType, b: PrimitiveType) -> PrimitiveType {
    use PrimitiveType::*;

    if matches!(a, BigDecimal) || matches!(b, BigDecimal) {
        return BigDecimal;
    }

    if matches!(a, Double) || matches!(b, Double) {
        if matches!(a, BigInteger) || matches!(b, BigInteger) {
            return BigDecimal;
        }
        return Double;
    }

    if matches!(a, BigInteger) || matches!(b, BigInteger) {
        return BigInteger;
    }

    if matches!(a, Long) || matches!(b, Long) {
        return Long;
    }

    Integer
}

fn optionalize(schema: Schema) -> Schema {
    match schema {
        Schema::Optional(inner) => Schema::Optional(inner),
        other => Schema::Optional(Box::new(other)),
    }
}

fn make_union<I>(items: I) -> Schema
where
    I: IntoIterator<Item = Schema>,
{
    let mut unique: BTreeMap<String, Schema> = BTreeMap::new();

    for item in items {
        match item {
            Schema::Union(inner) => {
                for variant in inner {
                    insert_union_variant(&mut unique, variant);
                }
            }
            other => insert_union_variant(&mut unique, other),
        }
    }

    let mut variants: Vec<Schema> = unique.into_values().collect();
    if variants.len() == 1 {
        variants.remove(0)
    } else {
        Schema::Union(variants)
    }
}

fn insert_union_variant(map: &mut BTreeMap<String, Schema>, schema: Schema) {
    let key = schema_sort_key(&schema);
    map.entry(key).or_insert(schema);
}

fn schema_sort_key(schema: &Schema) -> String {
    match schema {
        Schema::Primitive(primitive) => format!("primitive:{primitive:?}"),
        Schema::Array { element_type } => format!("array:{}", schema_sort_key(element_type)),
        Schema::Optional(inner) => format!("optional:{}", schema_sort_key(inner)),
        Schema::Union(variants) => {
            let mut keys: Vec<String> = variants.iter().map(schema_sort_key).collect();
            keys.sort();
            format!("union:[{}]", keys.join(","))
        }
        Schema::Object { fields, required } => {
            let mut field_parts = Vec::new();
            for (name, schema) in fields {
                field_parts.push(format!("{name}={}", schema_sort_key(schema)));
            }
            let required_key = required.iter().cloned().collect::<Vec<_>>().join("|");
            format!("object:{{{}}}|req:{required_key}", field_parts.join(","))
        }
    }
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

fn try_read_cache(
    request: &SampleFetchRequest,
) -> Result<Option<SampleFetchResult>, SampleFetchError> {
    let (cache_dir, expected_hash) = match (&request.cache_dir, &request.expected_sha256) {
        (Some(dir), Some(hash)) => (dir, hash.to_ascii_lowercase()),
        _ => return Ok(None),
    };

    let cache_path = cache_dir.join(&expected_hash);
    if !cache_path.exists() {
        return Ok(None);
    }

    let bytes = fs::read(&cache_path).map_err(|error| SampleFetchError::Io {
        path: cache_path.clone(),
        error,
    })?;
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

fn store_cache(cache_dir: &Path, sha256: &str, bytes: &[u8]) -> Result<PathBuf, SampleFetchError> {
    fs::create_dir_all(cache_dir).map_err(|error| SampleFetchError::Io {
        path: cache_dir.to_path_buf(),
        error,
    })?;
    let cache_path = cache_dir.join(sha256);
    fs::write(&cache_path, bytes).map_err(|error| SampleFetchError::Io {
        path: cache_path.clone(),
        error,
    })?;
    Ok(cache_path)
}

fn fetch_uncached(
    request: &SampleFetchRequest,
) -> Result<(Vec<u8>, SampleSourceKind), SampleFetchError> {
    match Url::parse(&request.source) {
        Ok(url) => match url.scheme() {
            "file" => {
                fetch_local_file_from_url(&url).map(|data| (data, SampleSourceKind::LocalFile))
            }
            "http" | "https" => {
                fetch_http(&url, request).map(|data| (data, SampleSourceKind::Http))
            }
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
    repo_url
        .set_scheme("ssh")
        .map_err(|_| SampleFetchError::InvalidUri {
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

fn ensure_network_allowed(
    source: &str,
    request: &SampleFetchRequest,
) -> Result<(), SampleFetchError> {
    if request.allow_network {
        return Ok(());
    }
    Err(SampleFetchError::NetworkDisabled {
        uri: source.to_string(),
    })
}

fn resolve_command(
    command: &str,
    override_path: &Option<PathBuf>,
) -> Result<PathBuf, SampleFetchError> {
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
        request.expected_sha256 =
            Some("6d4a333838d0ef96756cccc680af2531075c512502fb68c5503c63d93de859b3".to_string());
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

    #[test]
    fn infers_json_object_schema() {
        let data = br#"{"name": "Alice", "age": 29}"#;
        let schema = infer_schema(data, DataFormat::Json).expect("schema");

        let Schema::Object { fields, required } = schema else {
            panic!("unexpected schema");
        };

        assert!(required.contains("name"));
        assert!(required.contains("age"));
        assert_eq!(
            fields.get("name"),
            Some(&Schema::Primitive(PrimitiveType::String))
        );
        assert_eq!(
            fields.get("age"),
            Some(&Schema::Primitive(PrimitiveType::Integer))
        );
    }

    #[test]
    fn infers_json_array_with_optional_field() {
        let data = br#"[{"age": 30}, {"age": null}]"#;
        let schema = infer_schema(data, DataFormat::Json).expect("schema");

        let Schema::Array { element_type } = schema else {
            panic!("expected array schema");
        };

        let Schema::Object { fields, required } = *element_type else {
            panic!("expected object schema");
        };

        assert!(required.contains("age"));
        let age_schema = fields.get("age").expect("age field");
        assert!(matches!(
            age_schema,
            Schema::Optional(inner) if matches!(**inner, Schema::Primitive(PrimitiveType::Integer))
        ));
    }

    #[test]
    fn infers_csv_schema_with_optional_columns() {
        let data = b"name,age\nAlice,30\nBob,\n";
        let schema = infer_schema(data, DataFormat::Csv).expect("schema");

        let Schema::Array { element_type } = schema else {
            panic!("expected array schema");
        };

        let Schema::Object { fields, required } = *element_type else {
            panic!("expected object schema");
        };

        assert!(required.contains("name"));
        assert!(!required.contains("age"));
        assert_eq!(
            fields.get("name"),
            Some(&Schema::Primitive(PrimitiveType::String))
        );
        assert_eq!(
            fields.get("age"),
            Some(&Schema::Primitive(PrimitiveType::Integer))
        );
    }

    #[test]
    fn classifies_large_integers_as_long() {
        let data = br#"{"value": 2147483648}"#;
        let schema = infer_schema(data, DataFormat::Json).expect("schema");

        let Schema::Object { fields, .. } = schema else {
            panic!("expected object schema");
        };

        assert_eq!(
            fields.get("value"),
            Some(&Schema::Primitive(PrimitiveType::Long))
        );
    }
}
