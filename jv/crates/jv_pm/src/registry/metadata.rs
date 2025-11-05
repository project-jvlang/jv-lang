use quick_xml::de::from_reader;
use serde::Deserialize;
use thiserror::Error;

/// Mavenリポジトリが提供する `maven-metadata.xml` の表現。
#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct MavenMetadata {
    #[serde(rename = "groupId")]
    pub group_id: String,
    #[serde(rename = "artifactId")]
    pub artifact_id: String,
    #[serde(default)]
    pub versioning: Option<Versioning>,
}

impl MavenMetadata {
    /// 利用可能なバージョン一覧を返す（空の場合は空ベクタ）。
    pub fn versions(&self) -> &[String] {
        self.versioning
            .as_ref()
            .map(|versioning| versioning.versions.items.as_slice())
            .unwrap_or_default()
    }

    /// 最も新しいリリースバージョンを返す。
    pub fn latest_release(&self) -> Option<&str> {
        self.versioning
            .as_ref()
            .and_then(|versioning| versioning.release.as_deref())
    }

    /// リポジトリが公開している最新版（スナップショット含む）を返す。
    pub fn latest(&self) -> Option<&str> {
        self.versioning
            .as_ref()
            .and_then(|versioning| versioning.latest.as_deref())
    }
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct Versioning {
    pub latest: Option<String>,
    pub release: Option<String>,
    #[serde(default)]
    pub versions: Versions,
    #[serde(rename = "lastUpdated")]
    pub last_updated: Option<String>,
}

#[derive(Debug, Clone, Default, Deserialize, PartialEq, Eq)]
pub struct Versions {
    #[serde(default)]
    #[serde(rename = "version")]
    pub items: Vec<String>,
}

/// `maven-metadata.xml` のパースエラー。
#[derive(Debug, Error)]
pub enum MetadataParseError {
    #[error("maven-metadata.xmlの解析に失敗しました: {0}")]
    Xml(#[from] quick_xml::DeError),
}

/// `maven-metadata.xml` を構造体へ変換する。
pub fn parse_metadata(bytes: &[u8]) -> Result<MavenMetadata, MetadataParseError> {
    from_reader(bytes).map_err(MetadataParseError::from)
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = r#"
        <metadata>
          <groupId>org.example</groupId>
          <artifactId>demo</artifactId>
          <versioning>
            <latest>1.2.3</latest>
            <release>1.2.2</release>
            <versions>
              <version>1.0.0</version>
              <version>1.1.0</version>
              <version>1.2.2</version>
              <version>1.2.3</version>
            </versions>
            <lastUpdated>20250101010101</lastUpdated>
          </versioning>
        </metadata>
    "#;

    #[test]
    fn parse_sample_metadata() {
        let metadata = parse_metadata(SAMPLE.as_bytes()).expect("metadata parse");
        assert_eq!(metadata.group_id, "org.example");
        assert_eq!(metadata.artifact_id, "demo");
        let versions = metadata.versions();
        assert_eq!(versions.len(), 4);
        assert_eq!(metadata.latest().unwrap(), "1.2.3");
        assert_eq!(metadata.latest_release().unwrap(), "1.2.2");
    }
}
