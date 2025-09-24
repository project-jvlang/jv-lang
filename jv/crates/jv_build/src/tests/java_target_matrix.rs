use super::*;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};
use zip::write::FileOptions;

struct TempFixture {
    root: PathBuf,
}

impl TempFixture {
    fn new(label: &str) -> Self {
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis();
        let root = std::env::temp_dir().join(format!("jv-build-matrix-{}-{}", label, timestamp));
        fs::create_dir_all(&root).expect("create fixture root");
        Self { root }
    }

    fn write_manifest_jar(&self, name: &str, build_jdk: &str) -> PathBuf {
        let jar_path = self.root.join(name);
        let file = File::create(&jar_path).expect("create jar");
        let mut writer = zip::ZipWriter::new(file);
        let options = FileOptions::default();
        writer
            .start_file("META-INF/MANIFEST.MF", options)
            .expect("start manifest");
        writer
            .write_all(
                format!(
                    "Manifest-Version: 1.0\nBuild-Jdk: {}\n",
                    build_jdk
                )
                .as_bytes(),
            )
            .expect("write manifest");
        writer.finish().expect("finish jar");
        jar_path
    }

    fn write_class_file(&self, name: &str, major: u16) -> PathBuf {
        let path = self.root.join(name);
        let mut bytes = vec![0xCA, 0xFE, 0xBA, 0xBE, 0x00, 0x00];
        bytes.extend_from_slice(&major.to_be_bytes());
        fs::write(&path, bytes).expect("write class file");
        path
    }
}

impl Drop for TempFixture {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.root);
    }
}

#[test]
fn set_target_refreshes_release_flags() {
    let mut config = BuildConfig::default();
    assert_eq!(config.target, JavaTarget::Java25);
    assert_eq!(config.compiler_options, vec!["--release".to_string(), "25".to_string()]);

    config.set_target(JavaTarget::Java21);
    assert_eq!(config.target, JavaTarget::Java21);
    assert_eq!(config.compiler_options, vec!["--release".to_string(), "21".to_string()]);

    config.set_target(JavaTarget::Java25);
    assert_eq!(config.compiler_options[1], "25");
}

#[test]
fn analyze_config_flags_manifest_requiring_java25() {
    let fixture = TempFixture::new("manifest-upgrade");
    let jar_path = fixture.write_manifest_jar("needs-25.jar", "25.0.2");

    let mut config = BuildConfig::with_target(JavaTarget::Java21);
    config.classpath = vec![jar_path.to_string_lossy().into_owned()];

    let report = BuildSystem::new(config.clone())
        .analyze_compatibility()
        .expect("compatibility analysis succeeds");

    match report.status {
        CompatibilityStatus::RequiresHigherTarget { required_major } => {
            assert_eq!(required_major, 69);
        }
        other => panic!("expected RequiresHigherTarget, got {other:?}"),
    }
    assert_eq!(report.required_release(), Some(25));
    assert!(report
        .findings
        .iter()
        .any(|finding| finding.artifact.contains("needs-25.jar")));

    config.set_target(JavaTarget::Java25);
    let report_for_25 = BuildSystem::new(config)
        .analyze_compatibility()
        .expect("analysis for Java 25 target");
    assert!(report_for_25.is_compatible());
    assert_eq!(report_for_25.highest_required_major, Some(69));
}

#[test]
fn analyze_config_tracks_bytecode_major_versions() {
    let fixture = TempFixture::new("class-major");
    let class_path = fixture.write_class_file("PatternMatching.class", 69);

    let mut config = BuildConfig::with_target(JavaTarget::Java21);
    config.classpath = vec![class_path.to_string_lossy().into_owned()];

    let report = BuildSystem::new(config)
        .analyze_compatibility()
        .expect("compatibility analysis succeeds");

    assert!(report.status.requires_upgrade());
    assert_eq!(report.required_release(), Some(25));
    assert_eq!(report.highest_required_major, Some(69));
}
