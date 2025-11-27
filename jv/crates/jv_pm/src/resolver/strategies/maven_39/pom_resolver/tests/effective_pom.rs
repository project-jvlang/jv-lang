use super::*;

#[test]
fn effective_pom_applies_dependency_management_version() {
    let xml = r#"
            <project>
              <modelVersion>4.0.0</modelVersion>
              <groupId>org.example</groupId>
              <artifactId>demo</artifactId>
              <version>1.0.0</version>
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
                  <groupId>commons-codec</groupId>
                  <artifactId>commons-codec</artifactId>
                </dependency>
              </dependencies>
            </project>
            "#;

    let model = PomModel::parse(xml).expect("parse pom");
    let effective = EffectivePom::from_model(
        ArtifactCoordinates::new(
            "org.example".to_string(),
            "demo".to_string(),
            "1.0.0".to_string(),
        ),
        model,
        None,
    )
    .expect("build effective pom");

    assert_eq!(
        effective.dependencies.len(),
        1,
        "one dependency should remain"
    );
    let dep = &effective.dependencies[0];
    assert_eq!(
        dep.coordinates.version, "1.16.1",
        "dependencyManagement version should be applied to missing version"
    );
}

#[test]
fn effective_pom_preserves_optional_and_scope_and_classifier() {
    let xml = r#"
            <project>
              <modelVersion>4.0.0</modelVersion>
              <groupId>org.example</groupId>
              <artifactId>demo</artifactId>
              <version>1.0.0</version>
              <dependencies>
                <dependency>
                  <groupId>org.apache.commons</groupId>
                  <artifactId>commons-compress</artifactId>
                  <version>1.26.1</version>
                  <scope>test</scope>
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
            </project>
            "#;

    let model = PomModel::parse(xml).expect("parse pom");
    let effective = EffectivePom::from_model(
        ArtifactCoordinates::new(
            "org.example".to_string(),
            "demo".to_string(),
            "1.0.0".to_string(),
        ),
        model,
        None,
    )
    .expect("build effective pom");

    assert_eq!(effective.dependencies.len(), 1);
    let dep = &effective.dependencies[0];
    assert_eq!(dep.scope.as_deref(), Some("test"));
    assert!(dep.optional);
    assert_eq!(dep.coordinates.classifier.as_deref(), Some("tests"));
    assert_eq!(dep.coordinates.version, "1.26.1");
    assert_eq!(dep.coordinates.group_id, "org.apache.commons");
    assert_eq!(dep.coordinates.artifact_id, "commons-compress");
    assert_eq!(
        dep.exclusions.len(),
        1,
        "exclusions should be preserved into effective pom"
    );
}
