use crate::pipeline::report;
use jv_build::{
    CompatibilityEvidence, CompatibilityFinding, CompatibilityReport, CompatibilityStatus,
    DetectedVersion, JavaTarget,
};
use serde_json::Value;
use tempfile::tempdir;

fn sample_report() -> CompatibilityReport {
    CompatibilityReport {
        target: JavaTarget::Java21,
        status: CompatibilityStatus::Compatible,
        findings: vec![CompatibilityFinding {
            artifact: "libs/sealed.jar".to_string(),
            version: DetectedVersion::Release(25),
            evidence: CompatibilityEvidence::Manifest,
        }],
        warnings: vec!["sealed クラスを final クラスへフォールバックします".to_string()],
        highest_required_major: Some(69),
    }
}

#[test]
fn render_emits_summary_table_and_json() {
    let report = sample_report();
    let temp = tempdir().unwrap();

    let rendered =
        report::render(&report, temp.path()).expect("render should succeed for sample report");

    assert!(rendered.summary.contains("Compatibility Summary"));
    assert!(rendered.table.contains("Artifact"));
    assert_eq!(rendered.status, CompatibilityStatus::Compatible);
    assert_eq!(rendered.target, JavaTarget::Java21);

    let json = std::fs::read_to_string(&rendered.json_path).expect("json file exists");
    let value: Value = serde_json::from_str(&json).expect("json parses");
    assert_eq!(value["target"], "21");
    assert_eq!(value["status"]["code"], "compatible");
    assert_eq!(value["findings"].as_array().unwrap().len(), 1);
    assert_eq!(value["warnings"].as_array().unwrap().len(), 1);
}

#[test]
fn render_creates_output_directory() {
    let mut report = sample_report();
    report.status = CompatibilityStatus::RequiresHigherTarget { required_major: 70 };
    report.highest_required_major = Some(70);

    let temp = tempdir().unwrap();
    let nested = temp.path().join("reports/compat");

    let rendered = report::render(&report, &nested).expect("render ensures directories");
    assert!(rendered.json_path.starts_with(&nested));

    let json = std::fs::read_to_string(&rendered.json_path).expect("json file exists");
    let value: Value = serde_json::from_str(&json).expect("json parses");
    assert_eq!(value["status"]["code"], "requires_higher_target");
    assert_eq!(value["highest_required_major"], 70);
}
