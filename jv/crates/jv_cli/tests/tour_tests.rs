use std::fs;
use std::io::Cursor;
use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

use anyhow::Result;
use jv_build::BuildError;
use jv_cli::tour::{
    EnvironmentManager, JdkProbe, JdkStatus, OperatingSystem, PortfolioGenerator, ProgressTracker,
    SectionId, SectionStatus, TourCli,
};

#[derive(Clone, Copy)]
struct ReadyProbe;

impl JdkProbe for ReadyProbe {
    fn probe(&self) -> std::result::Result<String, BuildError> {
        Ok("javac 25.0.1".to_string())
    }
}

#[derive(Clone, Copy)]
struct OutdatedProbe;

impl JdkProbe for OutdatedProbe {
    fn probe(&self) -> std::result::Result<String, BuildError> {
        Ok("javac 21.0.7".to_string())
    }
}

fn unique_temp_path(label: &str) -> PathBuf {
    let mut path = std::env::temp_dir();
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    path.push(format!("jv_tour_test_{}_{}", label, nanos));
    path
}

#[test]
fn tour_cli_displays_progress_and_exit() -> Result<()> {
    let manager = EnvironmentManager::with_probe(ReadyProbe);
    let cli = TourCli::with_environment_manager(manager);

    let mut input = Cursor::new(b"P\n\n0\n".to_vec());
    let mut output = Vec::new();

    cli.run_with_io(&mut input, &mut output)?;

    let rendered = String::from_utf8(output)?;

    assert!(rendered.contains("jv言語ツアーへようこそ"));
    assert!(rendered.contains("進捗確認"));
    assert!(rendered.contains("ツアーを終了します"));

    Ok(())
}

#[test]
fn tour_cli_completes_basic_section_and_awards_achievement() -> Result<()> {
    let manager = EnvironmentManager::with_probe(ReadyProbe);
    let cli = TourCli::with_environment_manager(manager);

    let mut input = Cursor::new(b"1\n3\n\n0\n".to_vec());
    let mut output = Vec::new();

    cli.run_with_io(&mut input, &mut output)?;

    let rendered = String::from_utf8(output)?;

    assert!(rendered.contains("✅ 基本構文 セクションを完了として記録しました。"));
    assert!(rendered.contains("🏅 新しい実績を獲得しました"));

    Ok(())
}

#[test]
fn environment_manager_flags_outdated_versions() {
    let manager = EnvironmentManager::with_probe_and_requirement(OutdatedProbe, 25);
    let report = manager.build_report();

    match report.status {
        JdkStatus::Outdated {
            detected_major,
            required_major,
            ..
        } => {
            assert_eq!(detected_major, Some(21));
            assert_eq!(required_major, 25);
        }
        other => panic!("expected outdated status, got {:?}", other),
    }
}

#[test]
fn environment_manager_provides_guides_for_all_platforms() {
    let manager = EnvironmentManager::with_probe(ReadyProbe);

    for os in [
        OperatingSystem::Windows,
        OperatingSystem::MacOs,
        OperatingSystem::Linux,
        OperatingSystem::Unknown,
    ] {
        let guide = manager.setup_guide(os);
        assert_eq!(guide.os, os);
        assert!(
            !guide.steps.is_empty(),
            "steps should not be empty for {:?}",
            os
        );
        assert!(
            !guide.post_install_checks.is_empty(),
            "post install checks should not be empty for {:?}",
            os
        );
        assert!(
            !guide.distributions.is_empty(),
            "distributions should not be empty for {:?}",
            os
        );
    }
}

#[test]
fn portfolio_generation_creates_artifacts_from_summary() -> Result<()> {
    let mut tracker = ProgressTracker::ephemeral();

    for section in SectionId::all() {
        tracker.mark_section_started(*section)?;
        let answer = quiz_answer(*section);
        let mut reader = Cursor::new(format!("{}\n", answer).into_bytes());
        let mut sink = Vec::new();
        let outcome = tracker.run_quiz(*section, &mut reader, &mut sink)?;
        assert!(outcome.passed, "quiz should pass for {:?}", section);
        tracker.complete_section(*section)?;
    }

    let summary = tracker.summary();
    assert!(summary
        .sections
        .iter()
        .all(|display| display.status == SectionStatus::Completed));
    assert!(!summary.certificates.is_empty());
    assert!(!summary.achievements.is_empty());
    assert!(summary.achievements.len() >= 4);

    let output_root = unique_temp_path("portfolio");
    let generator = PortfolioGenerator::with_output_root(&output_root);
    let artifact = generator.generate(&summary)?;

    assert!(artifact.root_dir.exists());
    assert!(artifact.zip_path.exists());
    assert_eq!(
        artifact.metadata.documented_sections,
        SectionId::all().len()
    );
    assert!(artifact.metadata.certificate_included);
    assert_eq!(
        artifact.metadata.achievements_recorded,
        summary.achievements.len()
    );
    assert!(fs::metadata(&artifact.zip_path)?.len() > 0);

    fs::remove_dir_all(&output_root).ok();

    Ok(())
}

fn quiz_answer(section: SectionId) -> &'static str {
    match section {
        SectionId::BasicSyntax => "3",
        SectionId::ControlFlow => "2",
        SectionId::DataClasses => "2",
        SectionId::Functions => "3",
        SectionId::Concurrency => "2",
        SectionId::AsyncProgramming => "2",
        SectionId::BuildTools => "2",
        SectionId::InteractiveEditor => "2",
        SectionId::MiniProjectBuilder => "2",
    }
}
