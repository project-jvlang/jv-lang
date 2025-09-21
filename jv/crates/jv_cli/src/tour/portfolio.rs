use std::fs::{self, File};
use std::io::{self, Cursor};
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use chrono::{DateTime, Utc};
use serde_json::json;
use zip::write::FileOptions;
use zip::{CompressionMethod, ZipWriter};

use super::cli::SectionId;
use super::progress::{
    AchievementId, AchievementRecord, Certificate, ProgressSummary, SectionDisplay,
};
use super::projects::{self, JarArtifact, Project, ProjectType};
use super::sections;

const DEFAULT_ROOT: &str = "target/jv/portfolio";

#[derive(Debug, Clone)]
pub struct PortfolioConfig {
    pub output_root: PathBuf,
    pub include_execution_examples: bool,
}

impl Default for PortfolioConfig {
    fn default() -> Self {
        Self {
            output_root: PathBuf::from(DEFAULT_ROOT),
            include_execution_examples: true,
        }
    }
}

#[derive(Debug, Clone)]
pub struct PortfolioMetadata {
    pub generated_at: DateTime<Utc>,
    pub documented_sections: usize,
    pub documented_projects: usize,
    pub achievements_recorded: usize,
    pub certificate_included: bool,
}

#[derive(Debug, Clone)]
pub struct PortfolioArtifact {
    pub root_dir: PathBuf,
    pub zip_path: PathBuf,
    pub folder_name: String,
    pub metadata: PortfolioMetadata,
}

#[derive(Debug, Clone)]
pub struct PortfolioGenerator {
    config: PortfolioConfig,
}

impl PortfolioGenerator {
    pub fn new(config: PortfolioConfig) -> Self {
        Self { config }
    }

    pub fn with_output_root<P: Into<PathBuf>>(root: P) -> Self {
        let mut config = PortfolioConfig::default();
        config.output_root = root.into();
        Self { config }
    }

    pub fn generate(&self, summary: &ProgressSummary) -> Result<PortfolioArtifact> {
        fs::create_dir_all(&self.config.output_root).with_context(|| {
            format!(
                "ポートフォリオ出力ディレクトリ ({}) の作成に失敗しました",
                self.config.output_root.display()
            )
        })?;

        let timestamp = Utc::now();
        let folder_name = format!(
            "jv-language-tour-portfolio-{}",
            timestamp.format("%Y%m%d-%H%M%S")
        );
        let root_dir = self.config.output_root.join(&folder_name);
        fs::create_dir_all(&root_dir).with_context(|| {
            format!(
                "ポートフォリオルート ({}) の作成に失敗しました",
                root_dir.display()
            )
        })?;

        write_root_readme(&root_dir, summary, timestamp)?;
        write_section_documents(&root_dir, summary)?;
        write_project_documents(&root_dir, self.config.include_execution_examples)?;
        write_achievement_documents(&root_dir, summary, timestamp)?;

        let zip_path = self.config.output_root.join(format!("{}.zip", folder_name));
        package_directory(&root_dir, &zip_path, &folder_name)?;

        let metadata = PortfolioMetadata {
            generated_at: timestamp,
            documented_sections: summary.sections.len(),
            documented_projects: ProjectType::ALL.len(),
            achievements_recorded: summary.achievements.len(),
            certificate_included: !summary.certificates.is_empty(),
        };

        Ok(PortfolioArtifact {
            root_dir,
            zip_path,
            folder_name,
            metadata,
        })
    }
}

fn write_root_readme(
    root: &Path,
    summary: &ProgressSummary,
    generated_at: DateTime<Utc>,
) -> Result<()> {
    let mut content = String::new();
    content.push_str("# jv言語ツアー ポートフォリオ\n\n");
    content.push_str(
        "学習ツアーで作成したコード、ドキュメント、成果物をGitHub風に整理したパッケージです。\n\n",
    );
    content.push_str(&format!("- 生成日時: {}\n", generated_at.to_rfc3339()));
    content.push_str(&format!("- セクション数: {}\n", summary.sections.len()));
    content.push_str(&format!(
        "- ミニプロジェクト数: {}\n",
        ProjectType::ALL.len()
    ));
    content.push_str(&format!(
        "- 実績バッジ数: {}\n\n",
        summary.achievements.len()
    ));

    content.push_str("## セクション別ステータス\n");
    content.push_str("| # | セクション | ステータス | クイズ | 最終更新 |\n");
    content.push_str("| --- | --- | --- | --- | --- |\n");
    for display in &summary.sections {
        let quiz = if display.quiz_passed {
            "✅ 合格"
        } else {
            "⏳ 未実施"
        };
        let completed = display
            .completed_at
            .map(|ts| ts.to_rfc3339())
            .unwrap_or_else(|| "未完了".to_string());
        content.push_str(&format!(
            "| {:02} | {} | {} {} | {} | {} |\n",
            display.section.order(),
            display.title,
            display.icon,
            display.status.label(),
            quiz,
            completed,
        ));
    }

    if !summary.achievements.is_empty() {
        content.push_str("\n## 獲得実績\n");
        for achievement in &summary.achievements {
            content.push_str(&format!(
                "- {}: {}\n",
                achievement.title, achievement.description
            ));
        }
    }

    if let Some(certificate) = summary.certificates.last() {
        content.push_str("\n## 認定証\n");
        content.push_str(&format!(
            "- {}\n- {}\n",
            certificate.title, certificate.message
        ));
    }

    fs::write(root.join("README.md"), content).with_context(|| {
        format!(
            "ポートフォリオREADME ({}) の書き込みに失敗しました",
            root.display()
        )
    })
}

fn write_section_documents(root: &Path, summary: &ProgressSummary) -> Result<()> {
    let sections_dir = root.join("sections");
    fs::create_dir_all(&sections_dir).with_context(|| {
        format!(
            "セクションディレクトリ ({}) の作成に失敗しました",
            sections_dir.display()
        )
    })?;

    for display in &summary.sections {
        let folder = sections_dir.join(format!(
            "{:02}-{}",
            display.section.order(),
            display.section.slug()
        ));
        fs::create_dir_all(&folder).with_context(|| {
            format!(
                "セクション {} ({}) のディレクトリ作成に失敗しました",
                display.section.title(),
                folder.display()
            )
        })?;

        let mut readme = String::new();
        readme.push_str(&format!("# {}\n\n", display.title));
        readme.push_str(&format!(
            "- ステータス: {} {}\n",
            display.icon,
            display.status.label()
        ));
        if let Some(started) = display.started_at {
            readme.push_str(&format!("- 学習開始: {}\n", started.to_rfc3339()));
        }
        if let Some(completed) = display.completed_at {
            readme.push_str(&format!("- 学習完了: {}\n", completed.to_rfc3339()));
        }
        readme.push_str("\n## レッスン概要\n");

        let lesson_content = render_section_content(display)?;
        readme.push_str(&lesson_content);

        fs::write(folder.join("README.md"), readme).with_context(|| {
            format!(
                "セクションREADME ({}) の書き込みに失敗しました",
                folder.display()
            )
        })?;
    }

    Ok(())
}

fn render_section_content(display: &SectionDisplay) -> Result<String> {
    let mut buffer = Vec::new();
    match display.section {
        SectionId::BasicSyntax => sections::basic_syntax::render(&mut buffer)?,
        SectionId::ControlFlow => sections::control_flow::render(&mut buffer)?,
        SectionId::DataClasses => sections::data_classes::render(&mut buffer)?,
        SectionId::Functions => sections::functions::render(&mut buffer)?,
        SectionId::Concurrency => sections::concurrency::render(&mut buffer)?,
        SectionId::AsyncProgramming => sections::async_prog::render(&mut buffer)?,
        SectionId::BuildTools => sections::build_tools::render(&mut buffer)?,
        SectionId::InteractiveEditor => {
            let mut reader = Cursor::new(Vec::new());
            sections::interactive::render(&mut reader, &mut buffer)?;
        }
        SectionId::MiniProjectBuilder => projects::render(&mut buffer)?,
    }

    String::from_utf8(buffer).context("セクション出力のUTF-8変換に失敗しました")
}

fn write_project_documents(root: &Path, include_examples: bool) -> Result<()> {
    let projects_dir = root.join("projects");
    fs::create_dir_all(&projects_dir).with_context(|| {
        format!(
            "プロジェクトディレクトリ ({}) の作成に失敗しました",
            projects_dir.display()
        )
    })?;

    for project_type in ProjectType::ALL {
        let project = projects::start_project(project_type);
        let artifact = projects::build_executable(&project)?;
        let folder = projects_dir.join(project.slug);
        fs::create_dir_all(&folder).with_context(|| {
            format!(
                "プロジェクト {} ({}) のディレクトリ作成に失敗しました",
                project.name,
                folder.display()
            )
        })?;

        let readme = build_project_readme(&project, &artifact);
        fs::write(folder.join("README.md"), readme).with_context(|| {
            format!(
                "プロジェクトREADME ({}) の書き込みに失敗しました",
                folder.display()
            )
        })?;

        if include_examples {
            let example = build_execution_example(&project, &artifact);
            fs::write(folder.join("execution-example.txt"), example).with_context(|| {
                format!(
                    "プロジェクト実行例 ({}) の書き込みに失敗しました",
                    folder.display()
                )
            })?;
        }
    }

    Ok(())
}

fn build_project_readme(project: &Project, artifact: &JarArtifact) -> String {
    let mut content = String::new();
    content.push_str(&format!("# {}\n\n", project.name));
    content.push_str(&format!("スラッグ: `{}`\n\n", project.slug));
    content.push_str(&format!("{}\n\n", project.description));

    if !project.required_sections.is_empty() {
        let sections = project
            .required_sections
            .iter()
            .map(|section| format!("{} ({})", section.title(), section.slug()))
            .collect::<Vec<_>>()
            .join(", ");
        content.push_str(&format!("**参照セクション:** {}\n\n", sections));
    }

    if !project.features.is_empty() {
        content.push_str("## 主要フィーチャー\n");
        for (index, feature) in project.features.iter().enumerate() {
            content.push_str(&format!("{}. {}\n", index + 1, feature.name));
            content.push_str(&format!("   - {}\n", feature.description));
            if !feature.requirements.is_empty() {
                content.push_str(&format!(
                    "   - 対応要件: {}\n",
                    feature.requirements.join(", ")
                ));
            }
            if !feature.section_refs.is_empty() {
                let refs = feature
                    .section_refs
                    .iter()
                    .map(|section| section.title())
                    .collect::<Vec<_>>()
                    .join(", ");
                content.push_str(&format!("   - 関連セクション: {}\n", refs));
            }
            if !feature.code_highlight.trim().is_empty() {
                content.push_str("\n```jv\n");
                content.push_str(feature.code_highlight.trim());
                content.push_str("\n```\n\n");
            }
        }
    }

    if !project.steps.is_empty() {
        content.push_str("## ステップバイステップガイド\n");
        for (index, step) in project.steps.iter().enumerate() {
            content.push_str(&format!("\n### STEP {}: {}\n", index + 1, step.title));
            content.push_str(&format!("目的: {}\n", step.goal));
            for item in step.walkthrough {
                content.push_str(&format!("- {}\n", item));
            }
            if let Some(code) = step.code {
                if !code.trim().is_empty() {
                    content.push_str("\n```jv\n");
                    content.push_str(code.trim());
                    content.push_str("\n```\n");
                }
            }
            if !step.verification.is_empty() {
                content.push_str("\n検証コマンド:\n");
                for command in step.verification {
                    content.push_str(&format!("- $ {}\n", command));
                }
            }
        }
    }

    content.push_str("\n## 実行と配布\n");
    content.push_str(&format!("- 出力Jar: `{}`\n", artifact.output_path));
    content.push_str(&format!("- メインクラス: `{}`\n", artifact.main_class));
    if !artifact.commands.is_empty() {
        content.push_str("- 推奨コマンド:\n");
        for command in &artifact.commands {
            content.push_str(&format!("  - $ {}\n", command));
        }
    }

    content
}

fn build_execution_example(project: &Project, artifact: &JarArtifact) -> String {
    format!(
        "=== {name} 実行例 ===\n$ {run}\n> ... サンプル出力をここに追加してください (セクションで学んだ内容を示すログ)。\n",
        name = project.name,
        run = artifact
            .commands
            .last()
            .cloned()
            .unwrap_or_else(|| format!("java -jar target/jv/{}/dist/{}.jar", project.slug, project.slug))
    )
}

fn write_achievement_documents(
    root: &Path,
    summary: &ProgressSummary,
    generated_at: DateTime<Utc>,
) -> Result<()> {
    let achievements_dir = root.join("achievements");
    fs::create_dir_all(&achievements_dir).with_context(|| {
        format!(
            "実績ディレクトリ ({}) の作成に失敗しました",
            achievements_dir.display()
        )
    })?;

    let mut readme = String::new();
    readme.push_str("# 学習実績と認定証\n\n");
    if summary.achievements.is_empty() {
        readme
            .push_str("現時点で記録された実績はありません。学習を進めてバッジを獲得しましょう。\n");
    } else {
        readme.push_str("## 実績バッジ\n");
        for achievement in &summary.achievements {
            readme.push_str(&format!(
                "- {title} ({id:?})\n  - {desc}\n",
                title = achievement.title,
                id = achievement.id,
                desc = achievement.description
            ));
        }
    }

    if let Some(certificate) = summary.certificates.last() {
        readme.push_str("\n## 最新の認定証\n");
        readme.push_str(&format!(
            "- {}\n- {}\n",
            certificate.title, certificate.message
        ));
        let certificate_path = achievements_dir.join("certificate.txt");
        let doc = format!(
            "{title}\n発行日時: {issued}\n\n{message}\n",
            title = certificate.title,
            issued = certificate.awarded_at.to_rfc3339(),
            message = certificate.message
        );
        fs::write(&certificate_path, doc).with_context(|| {
            format!(
                "認定証ドキュメント ({}) の書き込みに失敗しました",
                certificate_path.display()
            )
        })?;
    }

    fs::write(achievements_dir.join("README.md"), readme).with_context(|| {
        format!(
            "実績README ({}) の書き込みに失敗しました",
            achievements_dir.display()
        )
    })?;

    let snapshot = json!({
        "generated_at": generated_at.to_rfc3339(),
        "achievement_count": summary.achievements.len(),
        "achievements": summary
            .achievements
            .iter()
            .map(|achievement| json!({
                "id": format!("{:?}", achievement.id),
                "title": achievement.title,
                "description": achievement.description,
                "awarded_at": achievement.awarded_at.to_rfc3339(),
            }))
            .collect::<Vec<_>>(),
        "certificate_included": !summary.certificates.is_empty(),
    });

    fs::write(
        achievements_dir.join("summary.json"),
        serde_json::to_string_pretty(&snapshot).expect("json serialization"),
    )
    .with_context(|| {
        format!(
            "実績サマリー ({}) の書き込みに失敗しました",
            achievements_dir.display()
        )
    })
}

fn package_directory(root: &Path, destination: &Path, folder_name: &str) -> Result<()> {
    if let Some(parent) = destination.parent() {
        fs::create_dir_all(parent).with_context(|| {
            format!(
                "Zip出力ディレクトリ ({}) の作成に失敗しました",
                parent.display()
            )
        })?;
    }

    let file = File::create(destination).with_context(|| {
        format!(
            "Zipファイル ({}) の作成に失敗しました",
            destination.display()
        )
    })?;
    let mut writer = ZipWriter::new(file);
    let options = FileOptions::default().compression_method(CompressionMethod::Deflated);

    writer
        .add_directory(format!("{}/", folder_name), options)
        .context("Zipにルートフォルダーを追加できませんでした")?;

    add_entries(&mut writer, root, root, folder_name, options)?;
    writer
        .finish()
        .context("Zipファイルのクローズに失敗しました")?;
    Ok(())
}

fn add_entries(
    writer: &mut ZipWriter<File>,
    root: &Path,
    current: &Path,
    prefix: &str,
    options: FileOptions,
) -> Result<()> {
    let mut entries = Vec::new();
    for entry in fs::read_dir(current)
        .with_context(|| format!("ディレクトリ ({}) の走査に失敗しました", current.display()))?
    {
        let entry = entry.with_context(|| {
            format!("エントリ ({}) の読み取りに失敗しました", current.display())
        })?;
        entries.push(entry);
    }

    entries.sort_by(|a, b| a.path().cmp(&b.path()));

    for entry in entries {
        let path = entry.path();
        let relative = path.strip_prefix(root).with_context(|| {
            format!(
                "ディレクトリ ({}) の相対パス計算に失敗しました",
                path.display()
            )
        })?;

        if relative.as_os_str().is_empty() {
            continue;
        }

        let entry_path = format!(
            "{}/{}",
            prefix,
            relative.to_string_lossy().replace('\\', "/")
        );

        if path.is_dir() {
            writer
                .add_directory(format!("{}/", entry_path), options)
                .with_context(|| {
                    format!("Zipにディレクトリ ({}) を追加できませんでした", entry_path)
                })?;
            add_entries(writer, root, &path, prefix, options)?;
        } else {
            writer.start_file(&entry_path, options).with_context(|| {
                format!("Zipにファイル ({}) を追加できませんでした", entry_path)
            })?;
            let mut input = File::open(&path).with_context(|| {
                format!("ファイル ({}) のオープンに失敗しました", path.display())
            })?;
            io::copy(&mut input, writer).with_context(|| {
                format!("Zipへのコピー中にエラーが発生しました: {}", path.display())
            })?;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::Result;

    fn sample_summary() -> ProgressSummary {
        let now = Utc::now();
        let sections = SectionId::ALL
            .iter()
            .map(|section| {
                let completed = section.order() % 2 == 0;
                SectionDisplay {
                    section: *section,
                    title: section.title().to_string(),
                    description: section.description().to_string(),
                    status: if completed {
                        super::super::progress::SectionStatus::Completed
                    } else {
                        super::super::progress::SectionStatus::InProgress
                    },
                    icon: if completed { "✅" } else { "🔄" },
                    started_at: Some(now),
                    completed_at: if completed { Some(now) } else { None },
                    quiz_passed: completed,
                }
            })
            .collect();

        let achievements = vec![AchievementRecord {
            id: AchievementId::FirstSection,
            title: "最初のセクションを完了".to_string(),
            description: "学習の最初のマイルストーンを達成しました".to_string(),
            awarded_at: now,
        }];

        let certificates = vec![Certificate {
            title: "jv言語ツアー達成証".to_string(),
            awarded_at: now,
            message: "全ての学習モジュールを修了しました".to_string(),
        }];

        ProgressSummary {
            sections,
            achievements,
            certificates,
            last_active_section: Some(SectionId::BuildTools),
        }
    }

    #[test]
    fn generate_portfolio_creates_artifacts() -> Result<()> {
        let temp_root = std::env::temp_dir().join(format!(
            "jv-portfolio-test-{}",
            Utc::now().format("%Y%m%d%H%M%S%f")
        ));

        let config = PortfolioConfig {
            output_root: temp_root.clone(),
            include_execution_examples: true,
        };

        let generator = PortfolioGenerator::new(config);
        let summary = sample_summary();
        let artifact = generator.generate(&summary)?;

        assert!(artifact.root_dir.exists());
        assert!(artifact.root_dir.join("README.md").exists());
        assert!(artifact.zip_path.exists());

        let archive = File::open(&artifact.zip_path)?;
        let mut zip = zip::ZipArchive::new(archive)?;
        let expected_path = format!("{}/README.md", artifact.folder_name);
        zip.by_name(&expected_path)
            .with_context(|| format!("Zip内に {} が存在しません", expected_path))?;

        fs::remove_dir_all(temp_root).ok();
        Ok(())
    }
}
