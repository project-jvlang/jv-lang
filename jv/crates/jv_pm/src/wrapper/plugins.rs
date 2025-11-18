use std::collections::HashMap;

use indexmap::IndexSet;
use once_cell::sync::OnceCell;
use roxmltree::{Document, Node};

use crate::maven::dependency_graph::{MavenDependencyResolver, PomMetadata};
use crate::registry::ArtifactCoordinates;
use crate::repository::defaults;
use crate::wrapper::error::WrapperError;

/// Represents a Maven plugin artifact that needs to be hydrated in wrapper mode.
#[derive(Debug, Clone)]
pub struct MavenPluginCoordinate {
    pub group_id: String,
    pub artifact_id: String,
    pub version: String,
}

impl MavenPluginCoordinate {
    pub fn new(
        group_id: impl Into<String>,
        artifact_id: impl Into<String>,
        version: impl Into<String>,
    ) -> Self {
        Self {
            group_id: group_id.into(),
            artifact_id: artifact_id.into(),
            version: version.into(),
        }
    }

    pub fn to_artifact(&self) -> ArtifactCoordinates {
        ArtifactCoordinates::new(
            self.group_id.clone(),
            self.artifact_id.clone(),
            self.version.clone(),
        )
    }
}

impl From<&defaults::MavenPluginDefinition> for MavenPluginCoordinate {
    fn from(value: &defaults::MavenPluginDefinition) -> Self {
        Self::new(
            value.group_id.clone(),
            value.artifact_id.clone(),
            value.version.clone(),
        )
    }
}

pub fn default_plugin_version(group: &str, artifact: &str) -> Option<&'static str> {
    standard_plugins()
        .iter()
        .find(|plugin| plugin.group_id == group && plugin.artifact_id == artifact)
        .map(|plugin| plugin.version.as_str())
}

pub fn standard_plugins() -> &'static [MavenPluginCoordinate] {
    static CACHE: OnceCell<Vec<MavenPluginCoordinate>> = OnceCell::new();
    CACHE.get_or_init(|| {
        defaults::maven_standard_plugins()
            .iter()
            .map(MavenPluginCoordinate::from)
            .collect()
    })
}

pub fn managed_artifacts() -> &'static [ArtifactCoordinates] {
    static CACHE: OnceCell<Vec<ArtifactCoordinates>> = OnceCell::new();
    CACHE.get_or_init(|| {
        defaults::maven_managed_artifacts()
            .iter()
            .map(|artifact| {
                ArtifactCoordinates::new(
                    artifact.group_id.clone(),
                    artifact.artifact_id.clone(),
                    artifact.version.clone(),
                )
            })
            .collect()
    })
}

pub fn discover_declared_plugins(
    pom_contents: &str,
    resolver: Option<&MavenDependencyResolver>,
) -> Result<Vec<ArtifactCoordinates>, WrapperError> {
    let document = Document::parse(pom_contents).map_err(|error| {
        WrapperError::OperationFailed(format!("pom.xml の解析に失敗しました: {error}"))
    })?;

    let project = document
        .descendants()
        .find(|node| node.has_tag_name("project"))
        .ok_or_else(|| {
            WrapperError::OperationFailed("pom.xml に <project> タグが存在しません".to_string())
        })?;

    let mut properties = HashMap::new();
    let mut plugin_versions: HashMap<(String, String), String> = HashMap::new();

    if let Some(parent) = project
        .children()
        .find(|node| node.is_element() && node.tag_name().name() == "parent")
    {
        if let Some(resolver) = resolver {
            if let Ok(parent_coords) = parse_parent_coordinates(parent) {
                if let Ok(metadata) = resolver.metadata_for(&parent_coords) {
                    import_parent_metadata(&mut properties, &mut plugin_versions, metadata);
                }
            }
        }
    }

    if let Some(group) = node_text(&project, "groupId") {
        properties.insert("project.groupId".to_string(), group);
    }
    if let Some(artifact) = node_text(&project, "artifactId") {
        properties.insert("project.artifactId".to_string(), artifact);
    }
    if let Some(version) = node_text(&project, "version") {
        properties.insert("project.version".to_string(), version);
    }

    properties.extend(parse_properties(&project));

    let mut declared = IndexSet::new();
    let mut management_entries = HashMap::new();
    for build in collect_build_nodes(&project) {
        let management = parse_plugin_group(&build, true)?;
        for plugin in management {
            if let Some((group, artifact, version)) =
                resolve_management_plugin(&plugin, &properties)
            {
                management_entries.insert((group.clone(), artifact.clone()), version.clone());
                plugin_versions.insert((group, artifact), version);
            }
        }

        let plugins = parse_plugin_group(&build, false)?;
        for plugin in plugins {
            if let Some(coords) = build_coordinates(&plugin, &properties, &plugin_versions)? {
                declared.insert(coords);
            }
        }
    }

    if let Some(reporting) = project
        .children()
        .find(|child| child.is_element() && child.tag_name().name() == "reporting")
    {
        for plugin in parse_plugins_only(&reporting)? {
            if let Some(coords) = build_coordinates(&plugin, &properties, &plugin_versions)? {
                declared.insert(coords);
            }
        }
    }

    Ok(declared.into_iter().collect())
}

fn import_parent_metadata(
    properties: &mut HashMap<String, String>,
    plugin_versions: &mut HashMap<(String, String), String>,
    metadata: PomMetadata,
) {
    for (key, value) in metadata.properties {
        properties.entry(key).or_insert(value);
    }

    for ((group, artifact), version) in metadata.plugin_management {
        plugin_versions.entry((group, artifact)).or_insert(version);
    }
}

fn parse_parent_coordinates(node: Node<'_, '_>) -> Result<ArtifactCoordinates, WrapperError> {
    let group = node_text(&node, "groupId")
        .ok_or_else(|| WrapperError::OperationFailed("parent.groupId が未指定です".to_string()))?;
    let artifact = node_text(&node, "artifactId").ok_or_else(|| {
        WrapperError::OperationFailed("parent.artifactId が未指定です".to_string())
    })?;
    let version = node_text(&node, "version")
        .ok_or_else(|| WrapperError::OperationFailed("parent.version が未指定です".to_string()))?;
    Ok(ArtifactCoordinates::new(group, artifact, version))
}

fn collect_build_nodes<'a>(project: &Node<'a, 'a>) -> Vec<Node<'a, 'a>> {
    let mut nodes = Vec::new();
    if let Some(build) = project
        .children()
        .find(|node| node.is_element() && node.tag_name().name() == "build")
    {
        nodes.push(build);
    }

    if let Some(profiles) = project
        .children()
        .find(|node| node.is_element() && node.tag_name().name() == "profiles")
    {
        for profile in profiles
            .children()
            .filter(|node| node.is_element() && node.tag_name().name() == "profile")
        {
            if let Some(build) = profile
                .children()
                .find(|node| node.is_element() && node.tag_name().name() == "build")
            {
                nodes.push(build);
            }
        }
    }

    nodes
}

fn parse_plugin_group(
    node: &Node<'_, '_>,
    management: bool,
) -> Result<Vec<PomPluginDecl>, WrapperError> {
    if management {
        let pm = node
            .children()
            .find(|child| child.is_element() && child.tag_name().name() == "pluginManagement")
            .and_then(|pm| {
                pm.children()
                    .find(|child| child.is_element() && child.tag_name().name() == "plugins")
            });
        if let Some(container) = pm {
            return parse_plugins_from_container(&container);
        }
        Ok(Vec::new())
    } else {
        parse_plugins_only(node)
    }
}

fn parse_plugins_only(node: &Node<'_, '_>) -> Result<Vec<PomPluginDecl>, WrapperError> {
    let container = node
        .children()
        .find(|child| child.is_element() && child.tag_name().name() == "plugins");
    if let Some(container) = container {
        parse_plugins_from_container(&container)
    } else {
        Ok(Vec::new())
    }
}

fn parse_plugins_from_container(node: &Node<'_, '_>) -> Result<Vec<PomPluginDecl>, WrapperError> {
    let mut result = Vec::new();
    for plugin in node
        .children()
        .filter(|child| child.is_element() && child.tag_name().name() == "plugin")
    {
        result.push(PomPluginDecl {
            group_id: node_text(&plugin, "groupId"),
            artifact_id: node_text(&plugin, "artifactId"),
            version: node_text(&plugin, "version"),
        });
    }
    Ok(result)
}

fn build_coordinates(
    plugin: &PomPluginDecl,
    properties: &HashMap<String, String>,
    plugin_versions: &HashMap<(String, String), String>,
) -> Result<Option<ArtifactCoordinates>, WrapperError> {
    let artifact = match resolve_property(plugin.artifact_id.as_deref(), properties) {
        Some(value) => value,
        None => return Ok(None),
    };
    let group = resolve_plugin_group(plugin.group_id.as_deref(), properties);
    let version = match resolve_property(plugin.version.as_deref(), properties) {
        Some(value) => value,
        None => plugin_versions
            .get(&(group.clone(), artifact.clone()))
            .cloned()
            .or_else(|| default_plugin_version(&group, &artifact).map(|v| v.to_string()))
            .ok_or_else(|| {
                WrapperError::OperationFailed(format!(
                    "{}:{} のプラグインバージョンを特定できません",
                    group, artifact
                ))
            })?,
    };

    Ok(Some(ArtifactCoordinates::new(group, artifact, version)))
}

fn resolve_management_plugin(
    plugin: &PomPluginDecl,
    properties: &HashMap<String, String>,
) -> Option<(String, String, String)> {
    let artifact = resolve_property(plugin.artifact_id.as_deref(), properties)?;
    let group = resolve_plugin_group(plugin.group_id.as_deref(), properties);
    let version = resolve_property(plugin.version.as_deref(), properties)?;
    Some((group, artifact, version))
}

fn node_text(node: &Node<'_, '_>, tag: &str) -> Option<String> {
    node.children()
        .find(|child| child.is_element() && child.tag_name().name() == tag)
        .and_then(|child| child.text())
        .map(|text| text.trim().to_string())
        .filter(|text| !text.is_empty())
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

#[derive(Clone)]
struct PomPluginDecl {
    group_id: Option<String>,
    artifact_id: Option<String>,
    version: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_declared_plugins_resolves_versions() {
        let pom = r#"
<project>
  <modelVersion>4.0.0</modelVersion>
  <groupId>com.example</groupId>
  <artifactId>demo</artifactId>
  <version>0.1.0</version>
  <properties>
    <custom.plugin.version>2.1.0</custom.plugin.version>
  </properties>
  <build>
    <pluginManagement>
      <plugins>
        <plugin>
          <groupId>com.tools</groupId>
          <artifactId>custom-plugin</artifactId>
          <version>${custom.plugin.version}</version>
        </plugin>
      </plugins>
    </pluginManagement>
    <plugins>
      <plugin>
        <groupId>com.tools</groupId>
        <artifactId>custom-plugin</artifactId>
      </plugin>
      <plugin>
        <artifactId>maven-jar-plugin</artifactId>
      </plugin>
    </plugins>
  </build>
</project>
"#;

        let plugins = discover_declared_plugins(pom, None).expect("parse");
        assert!(plugins.iter().any(|coords| {
            coords.group_id == "com.tools"
                && coords.artifact_id == "custom-plugin"
                && coords.version == "2.1.0"
        }));
        assert!(plugins.iter().any(|coords| {
            coords.group_id == "org.apache.maven.plugins"
                && coords.artifact_id == "maven-jar-plugin"
                && coords.version == "3.4.1"
        }));
    }
}
