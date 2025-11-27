use super::*;
use roxmltree::Document;

fn sample_pom() -> String {
    r#"
            <project>
              <modelVersion>4.0.0</modelVersion>
              <groupId>org.example</groupId>
              <artifactId>demo</artifactId>
              <version>1.0.0</version>
              <properties>
                <compiler.version>3.13.0</compiler.version>
              </properties>
              <dependencyManagement>
                <dependencies>
                  <dependency>
                    <groupId>commons-codec</groupId>
                    <artifactId>commons-codec</artifactId>
                    <version>1.16.1</version>
                  </dependency>
                </dependencies>
              </dependencyManagement>
              <dependencies>
                <dependency>
                  <groupId>org.apache.commons</groupId>
                  <artifactId>commons-lang3</artifactId>
                  <version>3.14.0</version>
                </dependency>
                <dependency>
                  <groupId>org.apache.commons</groupId>
                  <artifactId>commons-compress</artifactId>
                  <version>1.26.1</version>
                  <scope>provided</scope>
                  <optional>true</optional>
                  <classifier>tests</classifier>
                  <type>jar</type>
                  <exclusions>
                    <exclusion>
                      <groupId>org.unwanted</groupId>
                      <artifactId>skip-me</artifactId>
                    </exclusion>
                  </exclusions>
                </dependency>
              </dependencies>
              <build>
                <pluginManagement>
                  <plugins>
                    <plugin>
                      <groupId>org.apache.maven.plugins</groupId>
                      <artifactId>maven-compiler-plugin</artifactId>
                      <version>${compiler.version}</version>
                    </plugin>
                  </plugins>
                </pluginManagement>
                <plugins>
                  <plugin>
                    <groupId>org.apache.maven.plugins</groupId>
                    <artifactId>maven-surefire-plugin</artifactId>
                    <version>3.2.5</version>
                  </plugin>
                </plugins>
              </build>
              <reporting>
                <plugins>
                  <plugin>
                    <groupId>org.apache.maven.plugins</groupId>
                    <artifactId>maven-site-plugin</artifactId>
                    <version>3.12.1</version>
                  </plugin>
                </plugins>
              </reporting>
            </project>
            "#
    .to_string()
}

#[test]
fn parse_dependencies_respects_scope_optional_and_exclusions() {
    let model = PomModel::parse(&sample_pom()).expect("parse pom");
    assert_eq!(model.dependencies.len(), 2, "should keep two dependencies");

    let main = &model.dependencies[0];
    assert_eq!(main.group_id.as_deref(), Some("org.apache.commons"));
    assert_eq!(main.artifact_id.as_deref(), Some("commons-lang3"));
    assert_eq!(main.version.as_deref(), Some("3.14.0"));
    assert_eq!(main.scope.as_deref(), None, "default scope is compile");
    assert!(!main.optional);

    let provided = &model.dependencies[1];
    assert_eq!(provided.scope.as_deref(), Some("provided"));
    assert!(
        provided.optional,
        "optional should be true when explicitly set"
    );
    assert_eq!(provided.classifier.as_deref(), Some("tests"));
    assert_eq!(provided.dep_type.as_deref(), Some("jar"));
    assert_eq!(provided.exclusions.len(), 1, "exclusions should be parsed");
    assert_eq!(provided.exclusions[0].group_id, "org.unwanted");
    assert_eq!(provided.exclusions[0].artifact_id, "skip-me");

    assert_eq!(
        model
            .dependency_management
            .iter()
            .map(|dep| dep.version.clone())
            .collect::<Vec<_>>(),
        vec![Some("1.16.1".to_string())],
        "dependencyManagement version should be captured"
    );
}

#[test]
fn parse_plugins_includes_management_build_and_reporting() {
    let xml = sample_pom();
    let model = PomModel::parse(&xml).expect("parse pom");

    assert_eq!(
        model.plugin_management.len(),
        1,
        "pluginManagement should include compiler plugin"
    );
    let pm = &model.plugin_management[0];
    assert_eq!(pm.artifact_id.as_deref(), Some("maven-compiler-plugin"));
    assert_eq!(
        pm.version.as_deref(),
        Some("${compiler.version}"),
        "properties should be preserved before resolution"
    );

    // build.plugins + reporting.plugins should both be captured
    let doc = Document::parse(&xml).unwrap();
    let project = doc
        .descendants()
        .find(|n| n.has_tag_name("project"))
        .unwrap();
    let (_, plugins) = parse_plugin_sections(&project).expect("parse plugins");
    assert_eq!(
        plugins.len(),
        2,
        "plugins from build and reporting should be collected"
    );
    let artifact_ids: Vec<String> = plugins
        .iter()
        .filter_map(|p| p.artifact_id.clone())
        .collect();
    assert!(
        artifact_ids.contains(&"maven-surefire-plugin".to_string()),
        "should contain build plugin"
    );
    assert!(
        artifact_ids.contains(&"maven-site-plugin".to_string()),
        "should contain reporting plugin"
    );
}
