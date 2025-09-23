use crate::BuildConfig;
use jv_pm::JavaTarget;
use std::fs::{self, File};
use std::io::{self, BufReader, Read};
use std::path::{Path, PathBuf};
use thiserror::Error;
use zip::read::ZipFile;
use zip::ZipArchive;

#[derive(Debug, Error)]
pub enum CompatibilityError {
    #[error("IO error while inspecting '{path}': {source}")]
    Io {
        path: PathBuf,
        #[source]
        source: io::Error,
    },
    #[error("Failed to read ZIP archive '{path}': {source}")]
    Zip {
        path: PathBuf,
        #[source]
        source: zip::result::ZipError,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompatibilityEvidence {
    Manifest,
    Bytecode,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DetectedVersion {
    /// Java release number, e.g. 21 or 25, usually sourced from manifests.
    Release(u16),
    /// Bytecode major version, e.g. 65 or 69.
    Major(u16),
}

impl DetectedVersion {
    pub fn as_major(self) -> Option<u16> {
        match self {
            DetectedVersion::Major(value) => Some(value),
            DetectedVersion::Release(release) => release_to_major(release),
        }
    }

    pub fn as_release(self) -> Option<u16> {
        match self {
            DetectedVersion::Release(value) => Some(value),
            DetectedVersion::Major(major) => major_to_release(major),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CompatibilityFinding {
    pub artifact: String,
    pub version: DetectedVersion,
    pub evidence: CompatibilityEvidence,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompatibilityStatus {
    Compatible,
    RequiresHigherTarget { required_major: u16 },
}

impl CompatibilityStatus {
    pub fn requires_upgrade(self) -> bool {
        matches!(self, CompatibilityStatus::RequiresHigherTarget { .. })
    }
}

#[derive(Debug, Clone)]
pub struct CompatibilityReport {
    pub target: JavaTarget,
    pub status: CompatibilityStatus,
    pub findings: Vec<CompatibilityFinding>,
    pub warnings: Vec<String>,
    pub highest_required_major: Option<u16>,
}

impl CompatibilityReport {
    pub fn is_compatible(&self) -> bool {
        matches!(self.status, CompatibilityStatus::Compatible)
    }

    pub fn required_release(&self) -> Option<u16> {
        match self.status {
            CompatibilityStatus::Compatible => None,
            CompatibilityStatus::RequiresHigherTarget { required_major } => {
                major_to_release(required_major)
            }
        }
    }
}

pub struct CompatibilityAnalyzer {
    target: JavaTarget,
}

impl CompatibilityAnalyzer {
    pub fn new(target: JavaTarget) -> Self {
        Self { target }
    }

    pub fn analyze_config(&self, config: &BuildConfig) -> Result<CompatibilityReport, CompatibilityError> {
        let classpath = config.classpath.iter().map(PathBuf::from);
        self.analyze_classpath(classpath)
    }

    pub fn analyze_classpath<I, P>(
        &self,
        classpath: I,
    ) -> Result<CompatibilityReport, CompatibilityError>
    where
        I: IntoIterator<Item = P>,
        P: Into<PathBuf>,
    {
        let mut findings = Vec::new();
        let mut warnings = Vec::new();
        let mut highest_major: Option<u16> = None;

        for raw_path in classpath.into_iter() {
            let path = raw_path.into();
            if path.as_os_str().is_empty() {
                continue;
            }
            self.inspect_path(&path, &mut findings, &mut warnings, &mut highest_major)?;
        }

        let target_major = target_to_major(self.target);
        let status = match highest_major {
            Some(required) if required > target_major => {
                CompatibilityStatus::RequiresHigherTarget {
                    required_major: required,
                }
            }
            _ => CompatibilityStatus::Compatible,
        };

        Ok(CompatibilityReport {
            target: self.target,
            status,
            findings,
            warnings,
            highest_required_major: highest_major,
        })
    }

    fn inspect_path(
        &self,
        path: &Path,
        findings: &mut Vec<CompatibilityFinding>,
        warnings: &mut Vec<String>,
        highest_major: &mut Option<u16>,
    ) -> Result<(), CompatibilityError> {
        if !path.exists() {
            warnings.push(format!("クラスパス項目が見つかりません: {}", path.display()));
            return Ok(());
        }

        if path.is_dir() {
            for entry in fs::read_dir(path).map_err(|source| CompatibilityError::Io {
                path: path.to_path_buf(),
                source,
            })? {
                let entry = entry.map_err(|source| CompatibilityError::Io {
                    path: path.to_path_buf(),
                    source,
                })?;
                self.inspect_path(&entry.path(), findings, warnings, highest_major)?;
            }
            return Ok(());
        }

        match path.extension().and_then(|ext| ext.to_str()).map(|s| s.to_ascii_lowercase()) {
            Some(ref ext) if ext == "jar" || ext == "zip" => {
                self.inspect_archive(path, findings, highest_major)?;
            }
            Some(ref ext) if ext == "class" => {
                self.inspect_class_file(path, findings, highest_major)?;
            }
            _ => {}
        }

        Ok(())
    }

    fn inspect_archive(
        &self,
        path: &Path,
        findings: &mut Vec<CompatibilityFinding>,
        highest_major: &mut Option<u16>,
    ) -> Result<(), CompatibilityError> {
        let file = File::open(path).map_err(|source| CompatibilityError::Io {
            path: path.to_path_buf(),
            source,
        })?;
        let reader = BufReader::new(file);
        let mut archive = ZipArchive::new(reader).map_err(|source| CompatibilityError::Zip {
            path: path.to_path_buf(),
            source,
        })?;

        for index in 0..archive.len() {
            let mut entry = archive.by_index(index).map_err(|source| CompatibilityError::Zip {
                path: path.to_path_buf(),
                source,
            })?;

            if entry.name().eq_ignore_ascii_case("META-INF/MANIFEST.MF") {
                if let Some(version) = extract_manifest_version(&mut entry) {
                    self.record_finding(
                        findings,
                        highest_major,
                        CompatibilityFinding {
                            artifact: format!("{}!{}", path.display(), entry.name()),
                            version,
                            evidence: CompatibilityEvidence::Manifest,
                        },
                    );
                }
                continue;
            }

            if entry.name().ends_with(".class") {
                if let Some(major) = extract_class_major(&mut entry) {
                    self.record_finding(
                        findings,
                        highest_major,
                        CompatibilityFinding {
                            artifact: format!("{}!{}", path.display(), entry.name()),
                            version: DetectedVersion::Major(major),
                            evidence: CompatibilityEvidence::Bytecode,
                        },
                    );
                }
            }
        }

        Ok(())
    }

    fn inspect_class_file(
        &self,
        path: &Path,
        findings: &mut Vec<CompatibilityFinding>,
        highest_major: &mut Option<u16>,
    ) -> Result<(), CompatibilityError> {
        let mut file = File::open(path).map_err(|source| CompatibilityError::Io {
            path: path.to_path_buf(),
            source,
        })?;
        if let Some(major) = extract_class_major(&mut file) {
            self.record_finding(
                findings,
                highest_major,
                CompatibilityFinding {
                    artifact: path.display().to_string(),
                    version: DetectedVersion::Major(major),
                    evidence: CompatibilityEvidence::Bytecode,
                },
            );
        }
        Ok(())
    }

    fn record_finding(
        &self,
        findings: &mut Vec<CompatibilityFinding>,
        highest_major: &mut Option<u16>,
        finding: CompatibilityFinding,
    ) {
        if let Some(major) = finding.version.as_major() {
            if highest_major.map(|current| major > current).unwrap_or(true) {
                *highest_major = Some(major);
            }
        }
        findings.push(finding);
    }
}

fn extract_manifest_version(entry: &mut ZipFile<'_>) -> Option<DetectedVersion> {
    let mut content = String::new();
    if entry.read_to_string(&mut content).is_err() {
        return None;
    }

    manifest_keys()
        .filter_map(|key| extract_key_version(&content, key))
        .next()
}

fn manifest_keys() -> impl Iterator<Item = &'static str> {
    [
        "Build-Jdk",
        "Build-Jdk-Spec",
        "Target-Jdk",
        "Source-Compatibility",
        "Target-Compatibility",
    ]
    .into_iter()
}

fn extract_key_version(manifest: &str, key: &str) -> Option<DetectedVersion> {
    for line in manifest.lines() {
        let Some((found_key, value)) = line.split_once(':') else {
            continue;
        };
        if !found_key.trim().eq_ignore_ascii_case(key) {
            continue;
        }

        let token = value.trim();
        if let Some(release) = parse_release_number(token) {
            return Some(DetectedVersion::Release(release));
        }
    }
    None
}

fn parse_release_number(token: &str) -> Option<u16> {
    let mut segments = Vec::new();
    let mut buffer = String::new();

    for ch in token.chars() {
        if ch.is_ascii_digit() {
            buffer.push(ch);
        } else if !buffer.is_empty() {
            segments.push(buffer.clone());
            buffer.clear();
        }
    }

    if !buffer.is_empty() {
        segments.push(buffer);
    }

    if segments.is_empty() {
        return None;
    }

    let candidate = if segments.len() == 1 {
        &segments[0]
    } else if segments[0] == "1" {
        // Java 8 and earlier were often reported as 1.x
        &segments[1]
    } else {
        &segments[0]
    };

    candidate.parse::<u16>().ok()
}

fn extract_class_major(reader: &mut dyn Read) -> Option<u16> {
    let mut header = [0u8; 8];
    if reader.read_exact(&mut header).is_err() {
        return None;
    }

    if header[0..4] != [0xCA, 0xFE, 0xBA, 0xBE] {
        return None;
    }

    Some(u16::from_be_bytes([header[6], header[7]]))
}

const fn target_to_major(target: JavaTarget) -> u16 {
    match target {
        JavaTarget::Java21 => 65,
        JavaTarget::Java25 => 69,
    }
}

const fn release_to_major(release: u16) -> Option<u16> {
    if release < 1 {
        return None;
    }
    Some(release + 44)
}

const fn major_to_release(major: u16) -> Option<u16> {
    if major < 45 {
        return None;
    }
    Some(major - 44)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::io::Write;
    use std::time::{SystemTime, UNIX_EPOCH};
    use zip::write::FileOptions;

    fn temp_dir(label: &str) -> PathBuf {
        let mut base = std::env::temp_dir();
        let unique = format!(
            "jv-build-compat-{}-{}-{}",
            label,
            std::process::id(),
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        );
        base.push(unique);
        fs::create_dir_all(&base).unwrap();
        base
    }

    #[test]
    fn release_round_trip() {
        for release in [8u16, 11, 17, 21, 25] {
            let major = release_to_major(release).unwrap();
            assert_eq!(major_to_release(major).unwrap(), release);
        }
    }

    #[test]
    fn manifest_requires_newer_target() {
        let dir = temp_dir("manifest");
        let jar_path = dir.join("requires25.jar");

        {
            let file = File::create(&jar_path).unwrap();
            let mut writer = zip::ZipWriter::new(file);
            let options = FileOptions::default();
            writer
                .start_file("META-INF/MANIFEST.MF", options)
                .unwrap();
            writer
                .write_all(b"Manifest-Version: 1.0\nBuild-Jdk: 25.0.1\n")
                .unwrap();
            writer.finish().unwrap();
        }

        let analyzer = CompatibilityAnalyzer::new(JavaTarget::Java21);
        let report = analyzer
            .analyze_classpath([jar_path.clone()])
            .unwrap();

        assert!(matches!(
            report.status,
            CompatibilityStatus::RequiresHigherTarget {
                required_major: 69
            }
        ));
        assert_eq!(report.required_release(), Some(25));
        assert!(report
            .findings
            .iter()
            .any(|finding| finding.artifact.contains("requires25.jar")));

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn class_file_major_triggers_requirement() {
        let dir = temp_dir("class-major");
        let class_path = dir.join("ModernFeature.class");
        let mut bytes = vec![0xCA, 0xFE, 0xBA, 0xBE, 0x00, 0x00];
        bytes.extend_from_slice(&69u16.to_be_bytes());
        fs::write(&class_path, bytes).unwrap();

        let analyzer = CompatibilityAnalyzer::new(JavaTarget::Java21);
        let report = analyzer
            .analyze_classpath([class_path.clone()])
            .unwrap();

        assert!(matches!(
            report.status,
            CompatibilityStatus::RequiresHigherTarget {
                required_major: 69
            }
        ));
        assert_eq!(report.required_release(), Some(25));

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn legacy_class_is_compatible() {
        let dir = temp_dir("legacy-class");
        let class_path = dir.join("Legacy.class");
        let mut bytes = vec![0xCA, 0xFE, 0xBA, 0xBE, 0x00, 0x00];
        bytes.extend_from_slice(&52u16.to_be_bytes());
        fs::write(&class_path, bytes).unwrap();

        let analyzer = CompatibilityAnalyzer::new(JavaTarget::Java21);
        let report = analyzer
            .analyze_classpath([class_path.clone()])
            .unwrap();

        assert!(report.is_compatible());
        assert!(report
            .highest_required_major
            .map(|major| major <= 65)
            .unwrap_or(true));

        let _ = fs::remove_dir_all(&dir);
    }
}
