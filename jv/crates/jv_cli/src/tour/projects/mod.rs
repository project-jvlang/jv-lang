use std::io::Write;

use anyhow::{Result, ensure};
use jv_build::BuildConfig;

use super::cli::SectionId;

pub mod calculator;
pub mod game;
pub mod todo_app;

/// Miniプロジェクトの種類。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ProjectType {
    TodoApp,
    Calculator,
    RockPaperScissors,
}

impl ProjectType {
    pub const ALL: [ProjectType; 3] = [
        ProjectType::TodoApp,
        ProjectType::Calculator,
        ProjectType::RockPaperScissors,
    ];

    pub fn title(self) -> &'static str {
        match self {
            ProjectType::TodoApp => "ToDoアプリ",
            ProjectType::Calculator => "電卓",
            ProjectType::RockPaperScissors => "じゃんけんゲーム",
        }
    }

    pub fn slug(self) -> &'static str {
        match self {
            ProjectType::TodoApp => "todo-app",
            ProjectType::Calculator => "calculator",
            ProjectType::RockPaperScissors => "rps-game",
        }
    }

    pub fn order(self) -> usize {
        match self {
            ProjectType::TodoApp => 1,
            ProjectType::Calculator => 2,
            ProjectType::RockPaperScissors => 3,
        }
    }
}

/// ミニプロジェクトの全体像。
#[derive(Debug, Clone)]
pub struct MiniProject {
    pub name: &'static str,
    pub slug: &'static str,
    pub description: &'static str,
    pub main_class: &'static str,
    pub entry_file: &'static str,
    pub required_sections: Vec<SectionId>,
    pub features: Vec<Feature>,
    pub steps: Vec<ProjectStep>,
}

impl MiniProject {
    pub fn new(
        name: &'static str,
        slug: &'static str,
        description: &'static str,
        main_class: &'static str,
        entry_file: &'static str,
        required_sections: &[SectionId],
    ) -> Self {
        Self {
            name,
            slug,
            description,
            main_class,
            entry_file,
            required_sections: required_sections.to_vec(),
            features: Vec::new(),
            steps: Vec::new(),
        }
    }
}

/// 学習機能を組み込んだフィーチャー粒度の説明。
#[derive(Debug, Clone, Copy)]
pub struct Feature {
    pub name: &'static str,
    pub description: &'static str,
    pub requirements: &'static [&'static str],
    pub section_refs: &'static [SectionId],
    pub code_highlight: &'static str,
}

impl Feature {
    fn render<W: Write>(&self, writer: &mut W, index: usize) -> Result<()> {
        writeln!(writer, "  [{}] {}", index, self.name)?;
        writeln!(writer, "    {}", self.description)?;

        if !self.requirements.is_empty() {
            writeln!(writer, "    対応要件: {}", self.requirements.join(", "))?;
        }

        if !self.section_refs.is_empty() {
            let titles = self
                .section_refs
                .iter()
                .map(|section| section.title())
                .collect::<Vec<_>>()
                .join(", ");
            writeln!(writer, "    参照セクション: {}", titles)?;
        }

        let snippet = self.code_highlight.trim();
        if !snippet.is_empty() {
            writeln!(writer, "    ハイライト例:")?;
            writeln!(writer, "```jv")?;
            writeln!(writer, "{}", snippet)?;
            writeln!(writer, "```")?;
        }

        Ok(())
    }
}

/// ステップバイステップの作業内容。
#[derive(Debug, Clone, Copy)]
pub struct ProjectStep {
    pub title: &'static str,
    pub goal: &'static str,
    pub walkthrough: &'static [&'static str],
    pub code: Option<&'static str>,
    pub verification: &'static [&'static str],
}

impl ProjectStep {
    fn render<W: Write>(&self, writer: &mut W, index: usize) -> Result<()> {
        writeln!(writer, "\n[STEP {}] {}", index, self.title)?;
        writeln!(writer, "目的: {}", self.goal)?;
        for bullet in self.walkthrough {
            writeln!(writer, "  - {}", bullet)?;
        }

        if let Some(code) = self.code {
            let snippet = code.trim();
            if !snippet.is_empty() {
                writeln!(writer, "```jv")?;
                writeln!(writer, "{}", snippet)?;
                writeln!(writer, "```")?;
            }
        }

        if !self.verification.is_empty() {
            writeln!(writer, "検証コマンド:")?;
            for command in self.verification {
                writeln!(writer, "  $ {}", command)?;
            }
        }

        Ok(())
    }
}

/// Jar成果物の出力情報。
#[derive(Debug, Clone)]
pub struct JarArtifact {
    pub name: String,
    pub output_path: String,
    pub main_class: &'static str,
    pub build_config: BuildConfig,
    pub commands: Vec<String>,
    pub description: String,
}

impl JarArtifact {
    fn render<W: Write>(&self, writer: &mut W) -> Result<()> {
        writeln!(writer, "\n📦 Jar出力ガイド")?;
        writeln!(writer, "  - 成果物: {}", self.name)?;
        writeln!(writer, "  - 出力先: {}", self.output_path)?;
        writeln!(writer, "  - メインクラス: {}", self.main_class)?;
        if !self.build_config.classpath.is_empty() {
            writeln!(
                writer,
                "  - クラスパス: {}",
                self.build_config.classpath.join(":")
            )?;
        }
        writeln!(
            writer,
            "  - javac出力ディレクトリ: {}",
            self.build_config.output_dir
        )?;
        if !self.build_config.compiler_options.is_empty() {
            writeln!(
                writer,
                "  - コンパイラオプション: {}",
                self.build_config.compiler_options.join(" ")
            )?;
        }
        writeln!(writer, "  - 推奨コマンド:")?;
        for command in &self.commands {
            writeln!(writer, "    $ {}", command)?;
        }
        writeln!(writer, "  - {}", self.description)?;
        Ok(())
    }
}

pub type Project = MiniProject;

/// プロジェクトを初期化する。
pub fn start_project(project_type: ProjectType) -> MiniProject {
    match project_type {
        ProjectType::TodoApp => todo_app::create_project(),
        ProjectType::Calculator => calculator::create_project(),
        ProjectType::RockPaperScissors => game::create_project(),
    }
}

/// プロジェクトにフィーチャーを追加する。
pub fn add_feature(project: &mut MiniProject, feature: Feature) {
    project.features.push(feature);
}

/// jar成果物のためのビルド情報を組み立てる。
pub fn build_executable(project: &MiniProject) -> Result<JarArtifact> {
    ensure!(
        !project.entry_file.is_empty(),
        "{} にエントリーファイルが設定されていません",
        project.name
    );
    ensure!(
        !project.main_class.is_empty(),
        "{} にメインクラスが設定されていません",
        project.name
    );

    let output_dir = format!("target/jv/{}/classes", project.slug);
    let jar_path = format!("target/jv/{}/dist/{}.jar", project.slug, project.slug);

    let build_config = BuildConfig {
        output_dir: output_dir.clone(),
        classpath: vec![format!("target/jv/{}/deps/*", project.slug)],
        ..BuildConfig::default()
    };

    let commands = vec![
        format!(
            "jv build --input {} --output target/jv/{}/java --binary jar --bin-name {}",
            project.entry_file, project.slug, project.slug
        ),
        format!("java -jar {}", jar_path),
    ];

    Ok(JarArtifact {
        name: format!("{}.jar", project.slug),
        output_path: jar_path,
        main_class: project.main_class,
        build_config,
        commands,
        description: format!(
            "{} をJava 25互換の実行可能Jarとして配布可能な形で出力します。",
            project.name
        ),
    })
}

/// CLI出力をレンダリングする。
pub fn render<W: Write>(writer: &mut W) -> Result<()> {
    writeln!(writer, "--- ミニプロジェクトビルダー ---")?;
    writeln!(
        writer,
        "学習したセクションを横断して3つの実用アプリを構築し、段階的に成果物を完成させます。"
    )?;
    writeln!(
        writer,
        "各プロジェクトはステップごとに動作確認コマンドを提示し、最終的には実行可能Jarの作成手順を提供します。"
    )?;

    for project_type in ProjectType::ALL {
        let project = start_project(project_type);
        writeln!(
            writer,
            "\n=== {}. {} ({}) ===",
            project_type.order(),
            project.name,
            project.slug
        )?;
        writeln!(writer, "{}", project.description)?;

        if !project.required_sections.is_empty() {
            let titles = project
                .required_sections
                .iter()
                .map(|section| section.title())
                .collect::<Vec<_>>()
                .join(", ");
            writeln!(writer, "参照する学習セクション: {}", titles)?;
        }

        if !project.features.is_empty() {
            writeln!(writer, "\n主要フィーチャー:")?;
            for (index, feature) in project.features.iter().enumerate() {
                feature.render(writer, index + 1)?;
            }
        }

        if !project.steps.is_empty() {
            writeln!(writer, "\nステップバイステップガイド:")?;
            for (index, step) in project.steps.iter().enumerate() {
                step.render(writer, index + 1)?;
            }
        }

        let artifact = build_executable(&project)?;
        artifact.render(writer)?;
    }

    writeln!(
        writer,
        "\n✅ ミニプロジェクトビルダーを完了したら、ポートフォリオ生成セクションで成果をまとめましょう。"
    )?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn start_project_populates_features_and_steps() {
        let project = start_project(ProjectType::TodoApp);
        assert_eq!(project.name, "ToDoアプリ");
        assert!(!project.features.is_empty());
        assert!(!project.steps.is_empty());
        assert!(project.required_sections.contains(&SectionId::DataClasses));
    }

    #[test]
    fn build_executable_provides_commands() {
        let project = start_project(ProjectType::Calculator);
        let artifact = build_executable(&project).expect("artifact");
        assert!(
            artifact
                .commands
                .iter()
                .any(|command| command.contains("jv build"))
        );
        assert!(artifact.output_path.ends_with(".jar"));
    }

    #[test]
    fn render_outputs_project_titles() {
        let mut buffer = Vec::new();
        render(&mut buffer).expect("render");
        let text = String::from_utf8(buffer).expect("utf8");
        assert!(text.contains("ミニプロジェクトビルダー"));
        assert!(text.contains("ToDoアプリ"));
        assert!(text.contains("電卓"));
    }
}
