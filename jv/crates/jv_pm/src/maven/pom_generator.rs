use std::collections::HashMap;
use std::io::Cursor;

use quick_xml::Writer;
use quick_xml::events::{BytesDecl, BytesEnd, BytesStart, BytesText, Event};
use thiserror::Error;

use crate::Manifest;
use crate::lockfile::Lockfile;
use crate::resolver::{DependencyScope, ResolvedDependencies, ResolvedDependency, VersionDecision};

use super::MavenRepositoryConfig;

/// pom.xml を生成するためのヘルパー。
pub struct PomGenerator<'a> {
    manifest: &'a Manifest,
    resolved: &'a ResolvedDependencies,
    lockfile: Option<&'a Lockfile>,
    repositories: &'a [MavenRepositoryConfig],
}

impl<'a> PomGenerator<'a> {
    pub fn new(manifest: &'a Manifest, resolved: &'a ResolvedDependencies) -> Self {
        Self {
            manifest,
            resolved,
            lockfile: None,
            repositories: &[],
        }
    }

    pub fn with_lockfile(mut self, lockfile: Option<&'a Lockfile>) -> Self {
        self.lockfile = lockfile;
        self
    }

    pub fn with_repositories(mut self, repositories: &'a [MavenRepositoryConfig]) -> Self {
        self.repositories = repositories;
        self
    }

    pub fn generate(&self) -> Result<String, PomGenerationError> {
        let group_id = self
            .manifest
            .maven_group_id()
            .ok_or(PomGenerationError::MissingGroupId)?
            .to_string();

        let lock_versions = self.build_lock_index();
        let dependencies = self.collect_dependencies(&group_id, &lock_versions)?;

        let mut writer = Writer::new_with_indent(Cursor::new(Vec::new()), b' ', 4);
        writer.write_event(Event::Decl(BytesDecl::new("1.0", Some("UTF-8"), None)))?;

        let mut project = BytesStart::new("project");
        project.push_attribute(("xmlns", "http://maven.apache.org/POM/4.0.0"));
        project.push_attribute(("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance"));
        project.push_attribute((
            "xsi:schemaLocation",
            "http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd",
        ));
        writer.write_event(Event::Start(project))?;

        self.write_simple(&mut writer, "modelVersion", "4.0.0")?;
        self.write_simple(&mut writer, "groupId", &group_id)?;
        self.write_simple(&mut writer, "artifactId", self.manifest.maven_artifact_id())?;
        self.write_simple(&mut writer, "version", &self.manifest.package.version)?;
        self.write_simple(&mut writer, "packaging", self.manifest.maven_packaging())?;

        self.write_simple(&mut writer, "name", &self.manifest.package.name)?;

        if let Some(description) = self.manifest.maven_description() {
            self.write_simple(&mut writer, "description", description)?;
        }

        if let Some(url) = self.manifest.maven.url.as_deref() {
            if !url.trim().is_empty() {
                self.write_simple(&mut writer, "url", url)?;
            }
        }

        if !dependencies.is_empty() {
            writer.write_event(Event::Start(BytesStart::new("dependencies")))?;
            for dep in dependencies {
                writer.write_event(Event::Start(BytesStart::new("dependency")))?;
                self.write_simple(&mut writer, "groupId", &dep.group_id)?;
                self.write_simple(&mut writer, "artifactId", &dep.artifact_id)?;
                self.write_simple(&mut writer, "version", &dep.version)?;
                if let Some(scope) = dep.scope.as_deref() {
                    self.write_simple(&mut writer, "scope", scope)?;
                }
                writer.write_event(Event::End(BytesEnd::new("dependency")))?;
            }
            writer.write_event(Event::End(BytesEnd::new("dependencies")))?;
        }

        if !self.repositories.is_empty() {
            writer.write_event(Event::Start(BytesStart::new("repositories")))?;
            for repo in self.repositories {
                writer.write_event(Event::Start(BytesStart::new("repository")))?;
                self.write_simple(&mut writer, "id", &repo.id)?;
                if let Some(name) = repo.name.as_deref() {
                    if !name.trim().is_empty() {
                        self.write_simple(&mut writer, "name", name)?;
                    }
                }
                self.write_simple(&mut writer, "url", &repo.url)?;

                writer.write_event(Event::Start(BytesStart::new("releases")))?;
                self.write_simple(
                    &mut writer,
                    "enabled",
                    if repo.releases_enabled {
                        "true"
                    } else {
                        "false"
                    },
                )?;
                writer.write_event(Event::End(BytesEnd::new("releases")))?;

                writer.write_event(Event::Start(BytesStart::new("snapshots")))?;
                self.write_simple(
                    &mut writer,
                    "enabled",
                    if repo.snapshots_enabled {
                        "true"
                    } else {
                        "false"
                    },
                )?;
                writer.write_event(Event::End(BytesEnd::new("snapshots")))?;

                writer.write_event(Event::End(BytesEnd::new("repository")))?;
            }
            writer.write_event(Event::End(BytesEnd::new("repositories")))?;
        }

        writer.write_event(Event::End(BytesEnd::new("project")))?;

        let bytes = writer.into_inner().into_inner();
        let mut xml = String::from_utf8(bytes)?;
        if !xml.ends_with('\n') {
            xml.push('\n');
        }
        Ok(xml)
    }

    fn build_lock_index(&self) -> Option<HashMap<&str, &str>> {
        self.lockfile.map(|lockfile| {
            lockfile
                .packages
                .iter()
                .filter(|package| package.name != self.manifest.package.name)
                .map(|package| (package.name.as_str(), package.version.as_str()))
                .collect()
        })
    }

    fn collect_dependencies(
        &self,
        default_group: &str,
        lock_versions: &Option<HashMap<&str, &str>>,
    ) -> Result<Vec<DependencyEntry>, PomGenerationError> {
        let mut map: HashMap<(String, String), DependencyEntry> = HashMap::new();

        for (name, requirement) in &self.manifest.package.dependencies {
            let resolved_entry = self
                .resolved
                .dependencies
                .iter()
                .find(|dep| dep.name == *name);
            let (group_id, artifact_id) = self.parse_coordinates(name, default_group)?;
            let version =
                self.resolve_manifest_version(name, requirement, resolved_entry, lock_versions)?;
            let scope = resolved_entry.and_then(|dep| Self::map_scope(dep.scope));
            let key = (group_id.clone(), artifact_id.clone());
            map.entry(key)
                .and_modify(|existing| {
                    existing.version = version.clone();
                    existing.scope = scope.clone();
                })
                .or_insert_with(|| DependencyEntry {
                    group_id,
                    artifact_id,
                    version,
                    scope,
                });
        }

        let mut deps: Vec<_> = map.into_values().collect();
        deps.sort_by(|left, right| {
            left.group_id
                .cmp(&right.group_id)
                .then(left.artifact_id.cmp(&right.artifact_id))
        });
        Ok(deps)
    }

    fn parse_coordinates(
        &self,
        raw: &str,
        default_group: &str,
    ) -> Result<(String, String), PomGenerationError> {
        let trimmed = raw.trim();
        if trimmed.is_empty() {
            return Err(PomGenerationError::InvalidCoordinate {
                name: raw.to_string(),
                reason: "依存名が空文字です".to_string(),
            });
        }

        let mut parts = trimmed.split(':');
        let first = parts.next().unwrap();
        let second = parts.next();
        let third = parts.next();

        if third.is_some() {
            return Err(PomGenerationError::InvalidCoordinate {
                name: raw.to_string(),
                reason: "フォーマットは 'groupId:artifactId' を想定しています".to_string(),
            });
        }

        if let Some(artifact) = second {
            if first.is_empty() || artifact.is_empty() {
                return Err(PomGenerationError::InvalidCoordinate {
                    name: raw.to_string(),
                    reason: "groupId または artifactId が空です".to_string(),
                });
            }
            Ok((first.to_string(), artifact.to_string()))
        } else {
            if default_group.is_empty() {
                return Err(PomGenerationError::InvalidCoordinate {
                    name: raw.to_string(),
                    reason: "groupId を推測できません".to_string(),
                });
            }
            Ok((default_group.to_string(), first.to_string()))
        }
    }

    fn resolve_manifest_version(
        &self,
        dependency_name: &str,
        requirement: &str,
        resolved: Option<&ResolvedDependency>,
        lock_versions: &Option<HashMap<&str, &str>>,
    ) -> Result<String, PomGenerationError> {
        if let Some(map) = lock_versions.as_ref() {
            if let Some(version) = map.get(dependency_name) {
                return Ok((*version).to_string());
            }
        }

        if let Some(resolved) = resolved {
            if let VersionDecision::Exact(value) = &resolved.decision {
                return Ok(value.clone());
            }
        }

        let trimmed = requirement.trim();
        if trimmed.is_empty() {
            return Err(PomGenerationError::UnresolvedVersion {
                name: dependency_name.to_string(),
            });
        }

        Ok(trimmed.to_string())
    }

    fn map_scope(scope: DependencyScope) -> Option<String> {
        match scope {
            DependencyScope::Main => None,
            DependencyScope::Dev => Some("test".to_string()),
            DependencyScope::Build => Some("provided".to_string()),
        }
    }

    fn write_simple(
        &self,
        writer: &mut Writer<Cursor<Vec<u8>>>,
        tag: &str,
        value: &str,
    ) -> Result<(), PomGenerationError> {
        writer.write_event(Event::Start(BytesStart::new(tag)))?;
        writer.write_event(Event::Text(BytesText::new(value)))?;
        writer.write_event(Event::End(BytesEnd::new(tag)))?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct DependencyEntry {
    group_id: String,
    artifact_id: String,
    version: String,
    scope: Option<String>,
}

#[derive(Debug, Error)]
pub enum PomGenerationError {
    #[error("マニフェストに Maven グループID が設定されていません")]
    MissingGroupId,
    #[error("依存 '{name}' の座標が不正です: {reason}")]
    InvalidCoordinate { name: String, reason: String },
    #[error("依存 '{name}' のバージョンを特定できません")]
    UnresolvedVersion { name: String },
    #[error("pom.xml の書き込み中にIOエラーが発生しました: {0}")]
    Io(#[from] std::io::Error),
    #[error("pom.xml のXML生成に失敗しました: {0}")]
    Writer(#[from] quick_xml::Error),
    #[error("pom.xml をUTF-8文字列へ変換できません: {0}")]
    Utf8(#[from] std::string::FromUtf8Error),
}
