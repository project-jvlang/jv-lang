use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use indexmap::IndexSet;
use roxmltree::{Document, Node};

use crate::cache::DependencyCache;
use crate::registry::{ArtifactCoordinates, MavenRegistry, RegistryError};
use crate::wrapper::error::WrapperError;

/// Resolves the full Maven dependency graph (including transitives) for a set of
/// root artifacts by reading and interpreting their POM files.
pub struct MavenDependencyResolver<'a> {
    runtime: &'a tokio::runtime::Runtime,
    cache: Arc<DependencyCache>,
    registries: Vec<Arc<MavenRegistry>>,
}

pub struct PomMetadata {
    pub properties: HashMap<String, String>,
    pub plugin_management: HashMap<(String, String), String>,
}

impl<'a> MavenDependencyResolver<'a> {
    pub fn new(
        runtime: &'a tokio::runtime::Runtime,
        cache: Arc<DependencyCache>,
        registries: Vec<Arc<MavenRegistry>>,
    ) -> Self {
        Self {
            runtime,
            cache,
            registries,
        }
    }

    /// Returns the closure of every dependency reachable from the provided root
    /// artifacts. The returned list preserves insertion order and de-duplicates
    /// coordinates.
    pub fn resolve_closure(
        &self,
        roots: &[ArtifactCoordinates],
    ) -> Result<Vec<ArtifactCoordinates>, WrapperError> {
        self.resolve_closure_with_optional(roots, false)
    }

    /// Resolves the dependency closure, optionally keeping dependencies marked as optional.
    pub fn resolve_closure_with_optional(
        &self,
        roots: &[ArtifactCoordinates],
        include_optional: bool,
    ) -> Result<Vec<ArtifactCoordinates>, WrapperError> {
        let mut ordered = IndexSet::new();
        let mut memo = HashMap::new();
        // Maven の nearest-wins 調停に合わせ、最初に現れた groupId/artifactId(+classifier) のバージョンを固定する。
        let mut seen_versions: HashMap<(String, String, Option<String>), String> = HashMap::new();
        for root in roots {
            self.expand(
                root.clone(),
                &mut ordered,
                &mut memo,
                &[],
                include_optional,
                &mut HashSet::new(),
                &mut seen_versions,
            )?;
        }
        Ok(ordered.into_iter().collect())
    }

    pub fn metadata_for(&self, coords: &ArtifactCoordinates) -> Result<PomMetadata, WrapperError> {
        let mut memo = HashMap::new();
        let mut stack = HashSet::new();
        let effective = self.load_effective_pom(coords.clone(), &mut memo, &mut stack)?;
        let plugin_management = effective
            .plugin_management
            .iter()
            .map(|((group, artifact), managed)| {
                ((group.clone(), artifact.clone()), managed.version.clone())
            })
            .collect();
        Ok(PomMetadata {
            properties: effective.properties.clone(),
            plugin_management,
        })
    }

    fn expand(
        &self,
        coords: ArtifactCoordinates,
        ordered: &mut IndexSet<ArtifactCoordinates>,
        memo: &mut HashMap<ArtifactCoordinates, Arc<EffectivePom>>,
        inherited_exclusions: &[(String, String)],
        include_optional: bool,
        stack: &mut HashSet<ArtifactCoordinates>,
        seen_versions: &mut HashMap<(String, String, Option<String>), String>,
    ) -> Result<(), WrapperError> {
        if ordered.contains(&coords) {
            return Ok(());
        }

        let key = (
            coords.group_id.clone(),
            coords.artifact_id.clone(),
            coords.classifier.clone(),
        );

        if let Some(existing) = seen_versions.get(&key) {
            if existing != &coords.version {
                // nearest-wins: 先に確定したバージョンを優先し、後続の異なるバージョンは展開しない。
                return Ok(());
            }
        } else {
            seen_versions.insert(key, coords.version.clone());
        }

        if !stack.insert(coords.clone()) {
            return Err(WrapperError::OperationFailed(format!(
                "依存グラフに循環が検出されました: {}",
                coords
            )));
        }

        ordered.insert(coords.clone());
        let effective = self.load_effective_pom(coords.clone(), memo, &mut HashSet::new())?;

        for dependency in &effective.dependencies {
            if dependency.optional && !include_optional {
                continue;
            }
            if !matches!(
                dependency.scope.as_deref(),
                None | Some("compile") | Some("runtime") | Some("provided")
            ) {
                continue;
            }
            if is_excluded(&dependency.coordinates, inherited_exclusions) {
                continue;
            }

            let mut next_exclusions = inherited_exclusions.to_vec();
            next_exclusions.extend(
                dependency
                    .exclusions
                    .iter()
                    .map(|ex| (ex.group_id.clone(), ex.artifact_id.clone())),
            );
            self.expand(
                dependency.coordinates.clone(),
                ordered,
                memo,
                &next_exclusions,
                include_optional,
                stack,
                seen_versions,
            )?;
        }

        stack.remove(&coords);
        Ok(())
    }

    fn load_effective_pom(
        &self,
        coords: ArtifactCoordinates,
        memo: &mut HashMap<ArtifactCoordinates, Arc<EffectivePom>>,
        parent_stack: &mut HashSet<ArtifactCoordinates>,
    ) -> Result<Arc<EffectivePom>, WrapperError> {
        if let Some(existing) = memo.get(&coords) {
            return Ok(existing.clone());
        }

        if !parent_stack.insert(coords.clone()) {
            return Err(WrapperError::OperationFailed(format!(
                "親POMの解決中に循環が検出されました: {}",
                coords
            )));
        }

        let pom_text = self.fetch_pom(&coords)?;
        let mut model = PomModel::parse(&pom_text)?;

        let parent_effective = if let Some(parent) = &model.parent {
            let parent_coords = ArtifactCoordinates::new(
                parent.group_id.clone(),
                parent.artifact_id.clone(),
                parent.version.clone(),
            );
            Some(self.load_effective_pom(parent_coords, memo, parent_stack)?)
        } else {
            None
        };

        let mut property_context = parent_effective
            .as_ref()
            .map(|parent| parent.properties.clone())
            .unwrap_or_default();
        property_context.extend(model.properties.clone());
        property_context.insert("project.groupId".to_string(), coords.group_id.clone());
        property_context.insert("project.artifactId".to_string(), coords.artifact_id.clone());
        property_context.insert("project.version".to_string(), coords.version.clone());
        if let Some(parent_pom) = parent_effective.as_deref() {
            property_context.insert(
                "project.parent.groupId".to_string(),
                parent_pom.coordinates.group_id.clone(),
            );
            property_context.insert(
                "project.parent.artifactId".to_string(),
                parent_pom.coordinates.artifact_id.clone(),
            );
            property_context.insert(
                "project.parent.version".to_string(),
                parent_pom.coordinates.version.clone(),
            );
        }

        self.expand_dependency_management_imports(
            &mut model,
            &property_context,
            memo,
            parent_stack,
        )?;

        parent_stack.remove(&coords);

        let effective =
            EffectivePom::from_model(coords.clone(), model, parent_effective.as_deref())?;
        let shared = Arc::new(effective);
        memo.insert(coords, shared.clone());
        Ok(shared)
    }

    fn expand_dependency_management_imports(
        &self,
        model: &mut PomModel,
        properties: &HashMap<String, String>,
        memo: &mut HashMap<ArtifactCoordinates, Arc<EffectivePom>>,
        parent_stack: &mut HashSet<ArtifactCoordinates>,
    ) -> Result<(), WrapperError> {
        let mut retained = Vec::new();
        for entry in model.dependency_management.drain(..) {
            let is_import = matches!(
                entry.dep_type.as_deref(),
                Some(dep_type) if dep_type.eq_ignore_ascii_case("pom")
            ) && matches!(
                entry.scope.as_deref(),
                Some(scope) if scope.eq_ignore_ascii_case("import")
            );

            if !is_import {
                retained.push(entry);
                continue;
            }

            let group =
                resolve_property(entry.group_id.as_deref(), properties).ok_or_else(|| {
                    WrapperError::OperationFailed(
                        "dependencyManagement import の groupId が未設定です".to_string(),
                    )
                })?;
            let artifact =
                resolve_property(entry.artifact_id.as_deref(), properties).ok_or_else(|| {
                    WrapperError::OperationFailed(
                        "dependencyManagement import の artifactId が未設定です".to_string(),
                    )
                })?;
            let version =
                resolve_property(entry.version.as_deref(), properties).ok_or_else(|| {
                    WrapperError::OperationFailed(format!(
                        "{group}:{artifact} の dependencyManagement import にバージョンがありません"
                    ))
                })?;

            let import_coords = ArtifactCoordinates::new(group, artifact, version);
            let imported = self.load_effective_pom(import_coords, memo, parent_stack)?;
            for ((managed_group, managed_artifact), managed) in
                imported.dependency_management.iter()
            {
                retained.push(PomDependency {
                    group_id: Some(managed_group.clone()),
                    artifact_id: Some(managed_artifact.clone()),
                    version: Some(managed.version.clone()),
                    scope: None,
                    optional: false,
                    exclusions: Vec::new(),
                    classifier: None,
                    dep_type: None,
                });
            }
        }

        model.dependency_management = retained;
        Ok(())
    }

    fn fetch_pom(&self, coords: &ArtifactCoordinates) -> Result<String, WrapperError> {
        if let Some(cached) = self.cache.get_pom(coords).map_err(|error| {
            WrapperError::OperationFailed(format!("POMキャッシュの読み込みに失敗しました: {error}"))
        })? {
            return Ok(cached.content);
        }

        let mut last_error: Option<RegistryError> = None;
        for registry in &self.registries {
            match self.runtime.block_on(registry.fetch_pom(coords)) {
                Ok(text) => {
                    if let Err(error) = self.cache.store_pom(coords, &text) {
                        tracing::warn!(
                            artifact = %coords,
                            error = %error,
                            "POMをキャッシュへ保存できませんでした"
                        );
                    }
                    return Ok(text);
                }
                Err(error) => {
                    last_error = Some(error);
                }
            }
        }

        Err(WrapperError::OperationFailed(match last_error {
            Some(error) => format!("{} の POM 取得に失敗しました: {error}", coords),
            None => format!("{} の POM を取得できるレジストリがありません", coords),
        }))
    }
}

fn is_excluded(coords: &ArtifactCoordinates, exclusions: &[(String, String)]) -> bool {
    exclusions
        .iter()
        .any(|(group, artifact)| group == &coords.group_id && artifact == &coords.artifact_id)
}

#[derive(Debug, Clone)]
struct PomModel {
    _group_id: Option<String>,
    _artifact_id: Option<String>,
    _version: Option<String>,
    _packaging: Option<String>,
    parent: Option<PomParent>,
    properties: HashMap<String, String>,
    dependency_management: Vec<PomDependency>,
    dependencies: Vec<PomDependency>,
    plugin_management: Vec<PomPlugin>,
}

#[derive(Debug, Clone)]
struct PomParent {
    group_id: String,
    artifact_id: String,
    version: String,
}

#[derive(Debug, Clone)]
struct PomDependency {
    group_id: Option<String>,
    artifact_id: Option<String>,
    version: Option<String>,
    scope: Option<String>,
    optional: bool,
    exclusions: Vec<DependencyExclusion>,
    classifier: Option<String>,
    dep_type: Option<String>,
}

#[derive(Debug, Clone)]
struct PomPlugin {
    group_id: Option<String>,
    artifact_id: Option<String>,
    version: Option<String>,
}

#[derive(Debug, Clone)]
struct DependencyExclusion {
    group_id: String,
    artifact_id: String,
}

impl PomModel {
    fn parse(xml: &str) -> Result<Self, WrapperError> {
        let normalized = normalize_xml_entities(xml);
        let document = Document::parse(normalized.as_ref()).map_err(|error| {
            WrapperError::OperationFailed(format!("pom.xml の解析に失敗しました: {error}"))
        })?;
        let project = document
            .descendants()
            .find(|node| node.has_tag_name("project"))
            .ok_or_else(|| {
                WrapperError::OperationFailed("pom.xml に <project> タグが存在しません".to_string())
            })?;

        let group_id = node_text(&project, "groupId");
        let artifact_id = node_text(&project, "artifactId");
        let version = node_text(&project, "version");
        let packaging = node_text(&project, "packaging");
        let parent = project
            .children()
            .find(|node| node.is_element() && node.tag_name().name() == "parent")
            .map(parse_parent)
            .transpose()?;
        let properties = parse_properties(&project);
        let dependency_management = parse_dependency_group(&project, "dependencyManagement")?;
        let dependencies = parse_dependency_group(&project, "dependencies")?;
        let (plugin_management, _) = parse_plugin_sections(&project)?;

        Ok(Self {
            _group_id: group_id,
            _artifact_id: artifact_id,
            _version: version,
            _packaging: packaging,
            parent,
            properties,
            dependency_management,
            dependencies,
            plugin_management,
        })
    }
}

fn parse_parent(node: Node<'_, '_>) -> Result<PomParent, WrapperError> {
    let group_id = node_text(&node, "groupId")
        .ok_or_else(|| WrapperError::OperationFailed("parent.groupId が未指定です".to_string()))?;
    let artifact_id = node_text(&node, "artifactId").ok_or_else(|| {
        WrapperError::OperationFailed("parent.artifactId が未指定です".to_string())
    })?;
    let version = node_text(&node, "version")
        .ok_or_else(|| WrapperError::OperationFailed("parent.version が未指定です".to_string()))?;

    Ok(PomParent {
        group_id,
        artifact_id,
        version,
    })
}

fn parse_properties(node: &Node<'_, '_>) -> HashMap<String, String> {
    node.children()
        .find(|child| child.is_element() && child.tag_name().name() == "properties")
        .map(|props| {
            props
                .children()
                .filter(|child| child.is_element())
                .filter_map(|prop| {
                    let key = prop.tag_name().name().to_string();
                    let value = prop.text().map(|text| text.trim().to_string())?;
                    Some((key, value))
                })
                .collect()
        })
        .unwrap_or_default()
}

fn parse_dependency_group(
    node: &Node<'_, '_>,
    group_tag: &str,
) -> Result<Vec<PomDependency>, WrapperError> {
    let group_node = node
        .children()
        .find(|child| child.is_element() && child.tag_name().name() == group_tag);

    if let Some(group) = group_node {
        let deps_parent = if group.tag_name().name() == "dependencies" {
            group
        } else {
            group
                .children()
                .find(|child| child.is_element() && child.tag_name().name() == "dependencies")
                .ok_or_else(|| {
                    WrapperError::OperationFailed(format!(
                        "<{group_tag}> 内に <dependencies> タグが存在しません"
                    ))
                })?
        };

        let mut deps = Vec::new();
        for dependency in deps_parent
            .children()
            .filter(|child| child.is_element() && child.tag_name().name() == "dependency")
        {
            deps.push(parse_dependency(dependency)?);
        }
        Ok(deps)
    } else {
        Ok(Vec::new())
    }
}

fn parse_dependency(node: Node<'_, '_>) -> Result<PomDependency, WrapperError> {
    let exclusions = node
        .children()
        .find(|child| child.is_element() && child.tag_name().name() == "exclusions")
        .map(|exclusions_node| {
            exclusions_node
                .children()
                .filter(|child| child.is_element() && child.tag_name().name() == "exclusion")
                .filter_map(|ex_node| {
                    let group = node_text(&ex_node, "groupId")?;
                    let artifact = node_text(&ex_node, "artifactId")?;
                    Some(DependencyExclusion {
                        group_id: group,
                        artifact_id: artifact,
                    })
                })
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();

    Ok(PomDependency {
        group_id: node_text(&node, "groupId"),
        artifact_id: node_text(&node, "artifactId"),
        version: node_text(&node, "version"),
        scope: node_text(&node, "scope"),
        optional: node_text(&node, "optional")
            .map(|value| value.eq_ignore_ascii_case("true"))
            .unwrap_or(false),
        exclusions,
        classifier: node_text(&node, "classifier"),
        dep_type: node_text(&node, "type"),
    })
}

fn parse_plugin_sections(
    node: &Node<'_, '_>,
) -> Result<(Vec<PomPlugin>, Vec<PomPlugin>), WrapperError> {
    let mut management = Vec::new();
    let mut plugins = Vec::new();

    if let Some(build) = node
        .children()
        .find(|child| child.is_element() && child.tag_name().name() == "build")
    {
        management.extend(parse_plugin_group(&build, true)?);
        plugins.extend(parse_plugin_group(&build, false)?);
    }

    if let Some(reporting) = node
        .children()
        .find(|child| child.is_element() && child.tag_name().name() == "reporting")
    {
        plugins.extend(parse_plugin_group(&reporting, false)?);
    }

    Ok((management, plugins))
}

fn parse_plugin_group(
    node: &Node<'_, '_>,
    management: bool,
) -> Result<Vec<PomPlugin>, WrapperError> {
    let container = if management {
        node.children()
            .find(|child| child.is_element() && child.tag_name().name() == "pluginManagement")
            .and_then(|pm| {
                pm.children()
                    .find(|child| child.is_element() && child.tag_name().name() == "plugins")
            })
    } else {
        node.children()
            .find(|child| child.is_element() && child.tag_name().name() == "plugins")
    };

    if let Some(plugins_node) = container {
        let mut plugins = Vec::new();
        for plugin in plugins_node
            .children()
            .filter(|child| child.is_element() && child.tag_name().name() == "plugin")
        {
            plugins.push(parse_plugin(plugin)?);
        }
        Ok(plugins)
    } else {
        Ok(Vec::new())
    }
}

fn parse_plugin(node: Node<'_, '_>) -> Result<PomPlugin, WrapperError> {
    Ok(PomPlugin {
        group_id: node_text(&node, "groupId"),
        artifact_id: node_text(&node, "artifactId"),
        version: node_text(&node, "version"),
    })
}

fn node_text(node: &Node<'_, '_>, tag: &str) -> Option<String> {
    node.children()
        .find(|child| child.is_element() && child.tag_name().name() == tag)
        .and_then(|child| child.text())
        .map(|text| text.trim().to_string())
        .filter(|text| !text.is_empty())
}

fn normalize_xml_entities(input: &str) -> Cow<'_, str> {
    if !input.contains('&') {
        return Cow::Borrowed(input);
    }

    let mut output = String::with_capacity(input.len());
    let mut chars = input.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '&' {
            let mut entity = String::new();
            while let Some(&next) = chars.peek() {
                entity.push(next);
                chars.next();
                if next == ';' || entity.len() > 32 {
                    break;
                }
            }

            if entity.ends_with(';') {
                let name = &entity[..entity.len() - 1];
                if name.eq_ignore_ascii_case("lt")
                    || name.eq_ignore_ascii_case("gt")
                    || name.eq_ignore_ascii_case("amp")
                    || name.eq_ignore_ascii_case("quot")
                    || name.eq_ignore_ascii_case("apos")
                    || name.starts_with('#')
                {
                    output.push('&');
                    output.push_str(&entity);
                } else {
                    output.push(' ');
                }
            } else {
                output.push('&');
                output.push_str(&entity);
            }
        } else {
            output.push(ch);
        }
    }

    Cow::Owned(output)
}

#[derive(Debug, Clone)]
struct EffectivePom {
    coordinates: ArtifactCoordinates,
    properties: HashMap<String, String>,
    dependency_management: HashMap<(String, String), ManagedDependency>,
    dependencies: Vec<ResolvedDependency>,
    plugin_management: HashMap<(String, String), ManagedPlugin>,
}

#[derive(Debug, Clone)]
struct ManagedDependency {
    version: String,
    _scope: Option<String>,
    _optional: bool,
    _classifier: Option<String>,
    _dep_type: Option<String>,
}

#[derive(Debug, Clone)]
struct ManagedPlugin {
    version: String,
}

#[derive(Debug, Clone)]
struct ResolvedDependency {
    coordinates: ArtifactCoordinates,
    scope: Option<String>,
    optional: bool,
    exclusions: Vec<DependencyExclusion>,
}

impl EffectivePom {
    fn from_model(
        coords: ArtifactCoordinates,
        model: PomModel,
        parent: Option<&EffectivePom>,
    ) -> Result<Self, WrapperError> {
        let mut properties = parent
            .map(|parent| parent.properties.clone())
            .unwrap_or_default();
        properties.extend(model.properties.into_iter());

        properties.insert("project.groupId".to_string(), coords.group_id.clone());
        properties.insert("project.artifactId".to_string(), coords.artifact_id.clone());
        properties.insert("project.version".to_string(), coords.version.clone());
        if let Some(parent_pom) = parent {
            properties.insert(
                "project.parent.groupId".to_string(),
                parent_pom.coordinates.group_id.clone(),
            );
            properties.insert(
                "project.parent.artifactId".to_string(),
                parent_pom.coordinates.artifact_id.clone(),
            );
            properties.insert(
                "project.parent.version".to_string(),
                parent_pom.coordinates.version.clone(),
            );
        }

        let mut dependency_management = parent
            .map(|parent| parent.dependency_management.clone())
            .unwrap_or_default();
        for entry in model.dependency_management {
            let group =
                resolve_property(entry.group_id.as_deref(), &properties).ok_or_else(|| {
                    WrapperError::OperationFailed(
                        "dependencyManagement の groupId が未設定です".to_string(),
                    )
                })?;
            let artifact =
                resolve_property(entry.artifact_id.as_deref(), &properties).ok_or_else(|| {
                    WrapperError::OperationFailed(
                        "dependencyManagement の artifactId が未設定です".to_string(),
                    )
                })?;
            let version =
                resolve_property(entry.version.as_deref(), &properties).ok_or_else(|| {
                    WrapperError::OperationFailed(format!(
                        "{group}:{artifact} の dependencyManagement にバージョンがありません"
                    ))
                })?;
            dependency_management.insert(
                (group.clone(), artifact.clone()),
                ManagedDependency {
                    version,
                    _scope: entry.scope,
                    _optional: entry.optional,
                    _classifier: entry.classifier,
                    _dep_type: entry.dep_type,
                },
            );
        }

        let mut resolved: Vec<ResolvedDependency> = Vec::new();
        for dependency in model.dependencies {
            let group = match resolve_property(dependency.group_id.as_deref(), &properties) {
                Some(value) => value,
                None => continue,
            };
            let artifact = match resolve_property(dependency.artifact_id.as_deref(), &properties) {
                Some(value) => value,
                None => continue,
            };
            let version = match resolve_property(dependency.version.as_deref(), &properties) {
                Some(value) => value,
                None => {
                    if let Some(managed) =
                        dependency_management.get(&(group.clone(), artifact.clone()))
                    {
                        managed.version.clone()
                    } else if matches!(
                        dependency.scope.as_deref(),
                        Some("compile") | Some("runtime") | None
                    ) {
                        return Err(WrapperError::OperationFailed(format!(
                            "{}:{} のバージョンを特定できません",
                            group, artifact
                        )));
                    } else {
                        continue;
                    }
                }
            };

            let mut coords = ArtifactCoordinates::new(group, artifact, version);
            if let Some(classifier) = dependency.classifier {
                coords = coords.with_classifier(classifier);
            }

            resolved.push(ResolvedDependency {
                coordinates: coords,
                scope: dependency.scope.clone(),
                optional: dependency.optional,
                exclusions: dependency.exclusions.clone(),
            });
        }

        let mut plugin_management = parent
            .map(|parent| parent.plugin_management.clone())
            .unwrap_or_default();
        for plugin in model.plugin_management {
            let group = resolve_plugin_group(plugin.group_id.as_deref(), &properties);
            let artifact = resolve_property(plugin.artifact_id.as_deref(), &properties)
                .ok_or_else(|| {
                    WrapperError::OperationFailed(
                        "pluginManagement の artifactId が未設定です".to_string(),
                    )
                })?;
            let Some(version) = resolve_property(plugin.version.as_deref(), &properties) else {
                continue;
            };
            plugin_management.insert((group.clone(), artifact.clone()), ManagedPlugin { version });
        }

        Ok(Self {
            coordinates: coords,
            properties,
            dependency_management,
            dependencies: resolved,
            plugin_management,
        })
    }
}

fn resolve_property(value: Option<&str>, properties: &HashMap<String, String>) -> Option<String> {
    let mut current = value?.trim().to_string();
    if current.is_empty() {
        return None;
    }

    if !current.contains("${") {
        return Some(current);
    }

    let mut attempts = 0;
    while current.contains("${") {
        attempts += 1;
        if attempts > 8 {
            return None;
        }
        current = resolve_placeholders(&current, properties).ok()?;
    }

    Some(current)
}

fn resolve_plugin_group(value: Option<&str>, properties: &HashMap<String, String>) -> String {
    resolve_property(value, properties)
        .filter(|group| !group.is_empty())
        .unwrap_or_else(|| "org.apache.maven.plugins".to_string())
}

fn resolve_placeholders(
    raw: &str,
    properties: &HashMap<String, String>,
) -> Result<String, WrapperError> {
    let mut result = String::new();
    let mut rest = raw;
    while let Some(start) = rest.find("${") {
        result.push_str(&rest[..start]);
        let suffix = &rest[start + 2..];
        let end = suffix.find('}').ok_or_else(|| {
            WrapperError::OperationFailed(format!("プロパティ参照が閉じていません: {raw}"))
        })?;
        let key = &suffix[..end];
        let replacement = properties.get(key).ok_or_else(|| {
            WrapperError::OperationFailed(format!("プロパティ '{key}' が未定義です"))
        })?;
        result.push_str(replacement);
        rest = &suffix[end + 1..];
    }
    result.push_str(rest);
    Ok(result)
}
