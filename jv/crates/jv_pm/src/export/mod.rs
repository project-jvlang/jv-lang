use std::borrow::Cow;
use std::collections::HashSet;
use std::fs;
use std::fs::File;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

use thiserror::Error;

use crate::Manifest;
use crate::lockfile::{LockedPackage, Lockfile};
use crate::maven::pom_generator::PomGenerationError;
use crate::maven::{MavenIntegrationConfig, MavenIntegrationDispatcher, MavenIntegrationError};
use crate::maven::{MavenMirrorConfig, MavenRepositoryConfig};
use crate::resolver::{
    DependencyScope, ResolutionSource, ResolutionStats, ResolvedDependencies, ResolvedDependency,
    ResolverAlgorithmKind, VersionDecision,
};

const BUFFER_SIZE: usize = 64 * 1024;

/// エクスポート処理の要求内容。
pub struct ExportRequest<'a> {
    /// プロジェクトルート（`jv.toml`配置ディレクトリ）。
    pub project_root: &'a Path,
    /// マニフェスト。
    pub manifest: &'a Manifest,
    /// ロックファイル。
    pub lockfile: &'a Lockfile,
    /// 生成済みJavaソースの配置ディレクトリ。
    pub sources_dir: &'a Path,
    /// エクスポート先のルートディレクトリ（OUTPUT_DIR）。
    pub output_dir: &'a Path,
    /// 既存ローカルリポジトリ（`.jv/repository`）のパス。
    pub local_repository: &'a Path,
    /// Mavenリポジトリ設定。
    pub repositories: &'a [MavenRepositoryConfig],
    /// Mavenミラー設定。
    pub mirrors: &'a [MavenMirrorConfig],
    /// 既に解決済みの依存情報（任意）。指定が無ければ`lockfile`から再構成する。
    pub resolved: Option<&'a ResolvedDependencies>,
}

/// エクスポート結果のサマリ。
#[derive(Debug, Clone)]
pub struct ExportSummary {
    /// エクスポート先のルートディレクトリ。
    pub output_dir: PathBuf,
    /// 作成・更新したファイル数。
    pub updated_files: usize,
    /// コピーしたリポジトリアーティファクト数。
    pub repository_artifacts: usize,
    /// コピーしたJavaソースファイル数。
    pub source_files: usize,
    /// classpath.txtに出力したJARエントリ数。
    pub classpath_entries: usize,
}

/// エクスポート処理で発生し得るエラー。
#[derive(Debug, Error)]
pub enum ExportError {
    #[error("ロックファイルから依存情報を再構成できませんでした: {0}")]
    LockfileReconstruction(String),
    #[error("Maven統合ファイルの生成に失敗しました: {0}")]
    MavenIntegration(#[from] MavenIntegrationError),
    #[error("pom.xml生成に失敗しました: {0}")]
    PomGeneration(#[from] PomGenerationError),
    #[error("Javaソースディレクトリが見つかりません: {0}")]
    MissingSources(PathBuf),
    #[error("IOエラーが発生しました: {0}")]
    Io(#[from] std::io::Error),
    #[error("UTF-8へ変換できません: {0}")]
    Utf8(#[from] std::string::FromUtf8Error),
}

/// Javaプロジェクトのエクスポートを担当するユーティリティ。
pub struct JavaProjectExporter;

impl JavaProjectExporter {
    /// エクスポート処理を実行する。
    pub fn export(request: &ExportRequest<'_>) -> Result<ExportSummary, ExportError> {
        if !request.sources_dir.exists() {
            return Err(ExportError::MissingSources(
                request.sources_dir.to_path_buf(),
            ));
        }

        fs::create_dir_all(request.output_dir)?;

        let output_repo = request.output_dir.join(".jv").join("repository");
        let resolved_owned;
        let resolved = if let Some(resolved) = request.resolved {
            Cow::Borrowed(resolved)
        } else {
            resolved_owned = reconstruct_resolved_dependencies(request.manifest, request.lockfile)?;
            Cow::Owned(resolved_owned)
        };

        let integration_summary = generate_maven_files(request, &output_repo, resolved.as_ref())?;

        let repo_stats = if request.local_repository.exists() {
            sync_directory(request.local_repository, &output_repo)?
        } else {
            fs::create_dir_all(&output_repo)?;
            CopyStats::default()
        };
        let sources_stats = copy_sources(request.sources_dir, &request.output_dir.join("src"))?;

        let classpath_entries = write_classpath_file(request.output_dir, &output_repo)?;

        Ok(ExportSummary {
            output_dir: request.output_dir.to_path_buf(),
            updated_files: integration_summary,
            repository_artifacts: repo_stats.files_copied,
            source_files: sources_stats.files_copied,
            classpath_entries,
        })
    }
}

/// 再帰的コピーの統計値。
#[derive(Default)]
struct CopyStats {
    files_copied: usize,
    files_skipped: usize,
    files_removed: usize,
    dirs_created: usize,
    dirs_removed: usize,
}

fn generate_maven_files(
    request: &ExportRequest<'_>,
    output_repo: &Path,
    resolved: &ResolvedDependencies,
) -> Result<usize, ExportError> {
    let dispatcher = MavenIntegrationDispatcher::new();
    let integration = dispatcher.generate_default(&MavenIntegrationConfig {
        manifest: Some(request.manifest),
        resolved,
        lockfile: Some(request.lockfile),
        repositories: request.repositories,
        mirrors: request.mirrors,
        project_root: request.project_root,
        local_repository: output_repo,
    })?;

    let mut updated = 0usize;
    for (relative, contents) in integration.files {
        let target = request.output_dir.join(relative);
        if write_if_different(&target, contents.as_bytes())? {
            updated += 1;
        }
    }
    Ok(updated)
}

fn reconstruct_resolved_dependencies(
    manifest: &Manifest,
    lockfile: &Lockfile,
) -> Result<ResolvedDependencies, ExportError> {
    let mut scope_index = std::collections::HashMap::new();
    if let Some(root) = lockfile
        .packages
        .iter()
        .find(|package| package.name == manifest.package.name)
    {
        for dependency in &root.dependencies {
            scope_index.insert(dependency.name.clone(), dependency.scope);
        }
    }

    let mut dependencies = Vec::new();
    for package in lockfile
        .packages
        .iter()
        .filter(|package| package.name != manifest.package.name)
    {
        dependencies.push(lockfile_package_to_dependency(
            package,
            scope_index
                .get(&package.name)
                .copied()
                .unwrap_or(DependencyScope::Main),
        )?);
    }

    dependencies.sort_by(|left, right| {
        let order_left = version_order(&left.decision);
        let order_right = version_order(&right.decision);
        left.name
            .cmp(&right.name)
            .then(order_left.cmp(&order_right))
            .then(left.scope.cmp(&right.scope))
    });

    let count = dependencies.len();
    Ok(ResolvedDependencies {
        strategy: "lockfile".to_string(),
        algorithm: ResolverAlgorithmKind::MavenCompat,
        dependencies,
        diagnostics: Vec::new(),
        stats: ResolutionStats {
            elapsed_ms: 0,
            total_dependencies: count,
            decided_dependencies: count,
        },
    })
}

fn lockfile_package_to_dependency(
    package: &LockedPackage,
    scope: DependencyScope,
) -> Result<ResolvedDependency, ExportError> {
    if package.version.trim().is_empty() {
        return Err(ExportError::LockfileReconstruction(format!(
            "依存 '{}' のバージョンが空です",
            package.name
        )));
    }
    Ok(ResolvedDependency {
        name: package.name.clone(),
        requested: package.version.clone(),
        decision: VersionDecision::Exact(package.version.clone()),
        scope,
        source: ResolutionSource::Lockfile,
        local_artifact: None,
    })
}

fn version_order(decision: &VersionDecision) -> (u8, &str) {
    match decision {
        VersionDecision::Exact(value) => (0, value.as_str()),
        VersionDecision::Range(value) => (1, value.as_str()),
        VersionDecision::Unspecified => (2, ""),
    }
}

fn sync_directory(source: &Path, dest: &Path) -> Result<CopyStats, ExportError> {
    fs::create_dir_all(dest)?;
    sync_directory_inner(source, dest)
}

fn sync_directory_inner(source: &Path, dest: &Path) -> Result<CopyStats, ExportError> {
    let mut stats = CopyStats::default();
    let mut seen = HashSet::new();

    for entry in fs::read_dir(source)? {
        let entry = entry?;
        let file_name = entry.file_name();
        seen.insert(file_name.clone());
        let source_path = entry.path();
        let dest_path = dest.join(&file_name);
        let file_type = entry.file_type()?;

        if file_type.is_dir() {
            if !dest_path.exists() {
                fs::create_dir_all(&dest_path)?;
                stats.dirs_created += 1;
            }
            let inner = sync_directory_inner(&source_path, &dest_path)?;
            stats.files_copied += inner.files_copied;
            stats.files_skipped += inner.files_skipped;
            stats.files_removed += inner.files_removed;
            stats.dirs_created += inner.dirs_created;
            stats.dirs_removed += inner.dirs_removed;
            continue;
        }

        if file_type.is_file() {
            if copy_file_if_needed(&source_path, &dest_path)? {
                stats.files_copied += 1;
            } else {
                stats.files_skipped += 1;
            }
        }
    }

    // 存在しなくなったファイル/ディレクトリを削除
    if dest.exists() {
        for dest_entry in fs::read_dir(dest)? {
            let dest_entry = dest_entry?;
            let name = dest_entry.file_name();
            if !seen.contains(&name) {
                let path = dest_entry.path();
                if dest_entry.file_type()?.is_dir() {
                    fs::remove_dir_all(&path)?;
                    stats.dirs_removed += 1;
                } else {
                    fs::remove_file(&path)?;
                    stats.files_removed += 1;
                }
            }
        }
    }

    Ok(stats)
}

fn copy_sources(source: &Path, dest: &Path) -> Result<CopyStats, ExportError> {
    if dest.exists() {
        fs::remove_dir_all(dest)?;
    }
    fs::create_dir_all(dest)?;
    copy_tree(source, dest)
}

fn copy_tree(source: &Path, dest: &Path) -> Result<CopyStats, ExportError> {
    let mut stats = CopyStats::default();

    for entry in fs::read_dir(source)? {
        let entry = entry?;
        let file_type = entry.file_type()?;
        let source_path = entry.path();
        let dest_path = dest.join(entry.file_name());
        if file_type.is_dir() {
            fs::create_dir_all(&dest_path)?;
            stats.dirs_created += 1;
            let inner = copy_tree(&source_path, &dest_path)?;
            stats.files_copied += inner.files_copied;
            stats.files_skipped += inner.files_skipped;
            stats.files_removed += inner.files_removed;
            stats.dirs_created += inner.dirs_created;
            stats.dirs_removed += inner.dirs_removed;
        } else if file_type.is_file() {
            if copy_file_if_needed(&source_path, &dest_path)? {
                stats.files_copied += 1;
            } else {
                stats.files_skipped += 1;
            }
        }
    }

    Ok(stats)
}

fn copy_file_if_needed(source: &Path, dest: &Path) -> Result<bool, ExportError> {
    if dest.exists() && files_equal(source, dest)? {
        return Ok(false);
    }

    if let Some(parent) = dest.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::copy(source, dest)?;
    Ok(true)
}

fn files_equal(lhs: &Path, rhs: &Path) -> Result<bool, ExportError> {
    let lhs_meta = fs::metadata(lhs)?;
    let rhs_meta = fs::metadata(rhs)?;
    if lhs_meta.len() != rhs_meta.len() {
        return Ok(false);
    }

    let mut left = File::open(lhs)?;
    let mut right = File::open(rhs)?;
    let mut left_buf = [0u8; BUFFER_SIZE];
    let mut right_buf = [0u8; BUFFER_SIZE];

    loop {
        let left_read = left.read(&mut left_buf)?;
        let right_read = right.read(&mut right_buf)?;

        if left_read != right_read {
            return Ok(false);
        }
        if left_read == 0 {
            break;
        }
        if left_buf[..left_read] != right_buf[..right_read] {
            return Ok(false);
        }
    }

    Ok(true)
}

fn write_if_different(path: &Path, contents: &[u8]) -> Result<bool, ExportError> {
    if path.exists() {
        let existing = fs::read(path)?;
        if existing.as_slice() == contents {
            return Ok(false);
        }
    }

    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    let mut file = File::create(path)?;
    file.write_all(contents)?;
    Ok(true)
}

fn write_classpath_file(output_dir: &Path, repo_dir: &Path) -> Result<usize, ExportError> {
    let classpath_path = output_dir.join(".jv").join("classpath.txt");
    if let Some(parent) = classpath_path.parent() {
        fs::create_dir_all(parent)?;
    }

    let mut jar_paths = Vec::<PathBuf>::new();
    collect_jars(repo_dir, &mut jar_paths)?;
    jar_paths.sort();
    let jar_count = jar_paths.len();

    let mut buffer = Vec::new();
    for path in &jar_paths {
        let relative = path
            .strip_prefix(output_dir)
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|_| path.to_path_buf());
        let normalised = normalise_path(&relative);
        buffer.extend_from_slice(normalised.as_bytes());
        buffer.push(b'\n');
    }

    write_if_different(&classpath_path, &buffer)?;
    Ok(jar_count)
}

fn collect_jars(current: &Path, jars: &mut Vec<PathBuf>) -> Result<(), ExportError> {
    if !current.exists() {
        return Ok(());
    }

    for entry in fs::read_dir(current)? {
        let entry = entry?;
        let path = entry.path();
        let file_type = entry.file_type()?;
        if file_type.is_dir() {
            collect_jars(&path, jars)?;
        } else if file_type.is_file() {
            if let Some(ext) = path.extension() {
                if ext.eq_ignore_ascii_case("jar") {
                    jars.push(path);
                }
            }
        }
    }

    Ok(())
}

fn normalise_path(path: &Path) -> String {
    let mut text = path.to_string_lossy().to_string();
    if std::path::MAIN_SEPARATOR != '/' {
        text = text.replace(std::path::MAIN_SEPARATOR, "/");
    }
    text
}
