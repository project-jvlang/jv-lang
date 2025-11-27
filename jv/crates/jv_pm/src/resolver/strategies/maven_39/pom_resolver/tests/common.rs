use super::*;

pub(crate) fn store_pom(
    cache: &DependencyCache,
    coords: &ArtifactCoordinates,
    deps: &[(&str, &str, &str, Option<&str>, bool)],
) {
    let mut deps_xml = String::new();
    if !deps.is_empty() {
        deps_xml.push_str("<dependencies>");
        for (group, artifact, version, scope, optional) in deps {
            deps_xml.push_str("<dependency>");
            deps_xml.push_str(&format!("<groupId>{group}</groupId>"));
            deps_xml.push_str(&format!("<artifactId>{artifact}</artifactId>"));
            deps_xml.push_str(&format!("<version>{version}</version>"));
            if let Some(scope) = scope {
                deps_xml.push_str(&format!("<scope>{scope}</scope>"));
            }
            if *optional {
                deps_xml.push_str("<optional>true</optional>");
            }
            deps_xml.push_str("</dependency>");
        }
        deps_xml.push_str("</dependencies>");
    }

    let pom = format!(
        r#"
                <project>
                  <modelVersion>4.0.0</modelVersion>
                  <groupId>{}</groupId>
                  <artifactId>{}</artifactId>
                  <version>{}</version>
                  {deps}
                </project>
            "#,
        coords.group_id,
        coords.artifact_id,
        coords.version,
        deps = deps_xml
    );
    cache.store_pom(coords, &pom).expect("store pom");
}
