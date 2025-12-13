use std::cell::RefCell;
use std::io::{self, BufRead, Write};

use anyhow::{Context, Result};
use chrono::Local;
use serde::{Deserialize, Serialize};

use super::environment::{EnvironmentManager, JdkProbe, JdkStatus};
use super::progress::{CompletionOutcome, ProgressSummary, ProgressTracker};
use super::projects;
use super::sections;

/// Identifier for tour sections that are accessible from the main menu.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub enum SectionId {
    BasicSyntax,
    ControlFlow,
    DataClasses,
    Functions,
    Concurrency,
    AsyncProgramming,
    BuildTools,
    InteractiveEditor,
    MiniProjectBuilder,
}

impl SectionId {
    pub const ALL: [SectionId; 9] = [
        SectionId::BasicSyntax,
        SectionId::ControlFlow,
        SectionId::DataClasses,
        SectionId::Functions,
        SectionId::Concurrency,
        SectionId::AsyncProgramming,
        SectionId::BuildTools,
        SectionId::InteractiveEditor,
        SectionId::MiniProjectBuilder,
    ];

    pub fn all() -> &'static [SectionId] {
        &Self::ALL
    }

    pub fn title(self) -> &'static str {
        match self {
            SectionId::BasicSyntax => "基本構文",
            SectionId::ControlFlow => "制御フロー",
            SectionId::DataClasses => "データクラス",
            SectionId::Functions => "関数",
            SectionId::Concurrency => "並行性",
            SectionId::AsyncProgramming => "非同期プログラミング",
            SectionId::BuildTools => "ビルドツール体験",
            SectionId::InteractiveEditor => "インタラクティブ編集",
            SectionId::MiniProjectBuilder => "ミニプロジェクトビルダー",
        }
    }

    pub fn description(self) -> &'static str {
        match self {
            SectionId::BasicSyntax => {
                "Hello Worldやval/var宣言、型推論、null安全性を学ぶセクション"
            }
            SectionId::ControlFlow => "when式やパターンマッチングなどの制御フローパターンを扱う",
            SectionId::DataClasses => "record生成やmutableクラスを含むデータクラスの使い方",
            SectionId::Functions => "拡張関数、デフォルト引数、トップレベル関数にフォーカス",
            SectionId::Concurrency => "spawn{}と仮想スレッドによる並行処理の実例",
            SectionId::AsyncProgramming => "CompletableFutureを用いたasync/awaitパターン",
            SectionId::BuildTools => "jv.toml設定からjv buildまでのワークフロー",
            SectionId::InteractiveEditor => {
                "リアルタイムにコードを編集して検証するインタラクティブ体験"
            }
            SectionId::MiniProjectBuilder => "ToDoアプリなどのミニプロジェクトを段階的に構築",
        }
    }
}

impl SectionId {
    pub fn slug(self) -> &'static str {
        match self {
            SectionId::BasicSyntax => "basic-syntax",
            SectionId::ControlFlow => "control-flow",
            SectionId::DataClasses => "data-classes",
            SectionId::Functions => "functions",
            SectionId::Concurrency => "concurrency",
            SectionId::AsyncProgramming => "async-programming",
            SectionId::BuildTools => "build-tools",
            SectionId::InteractiveEditor => "interactive-editor",
            SectionId::MiniProjectBuilder => "mini-project-builder",
        }
    }

    pub fn order(self) -> usize {
        match self {
            SectionId::BasicSyntax => 1,
            SectionId::ControlFlow => 2,
            SectionId::DataClasses => 3,
            SectionId::Functions => 4,
            SectionId::Concurrency => 5,
            SectionId::AsyncProgramming => 6,
            SectionId::BuildTools => 7,
            SectionId::InteractiveEditor => 8,
            SectionId::MiniProjectBuilder => 9,
        }
    }
}

/// A menu entry presented to the learner.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MenuEntry {
    /// Display key used in the menu (e.g., "1" or "P").
    pub key: &'static str,
    /// Title presented alongside the key.
    pub title: &'static str,
    /// Optional description giving additional context.
    pub description: &'static str,
    /// Action invoked when the entry is selected.
    pub action: MenuAction,
}

/// Available actions the menu can trigger.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MenuAction {
    Section(SectionId),
    Progress,
    Exit,
}

/// Public facade that coordinates environment validation and menu navigation.
#[derive(Debug, Clone)]
pub struct TourCli<P: JdkProbe = super::environment::BuildSystemProbe> {
    environment: EnvironmentManager<P>,
    progress: RefCell<ProgressTracker>,
}

impl Default for TourCli {
    fn default() -> Self {
        Self::new()
    }
}

impl TourCli {
    /// Create a CLI experience using the default environment probe.
    pub fn new() -> Self {
        Self {
            environment: EnvironmentManager::new(),
            progress: RefCell::new(ProgressTracker::load_default()),
        }
    }
}

impl<P: JdkProbe> TourCli<P> {
    /// Create a CLI instance with a custom environment manager (mainly for testing).
    pub fn with_environment_manager(environment: EnvironmentManager<P>) -> Self {
        Self {
            environment,
            progress: RefCell::new(ProgressTracker::ephemeral()),
        }
    }

    /// Entry point that locks stdin/stdout and runs the interactive menu loop.
    pub fn run(&self) -> Result<()> {
        let mut stdin = io::stdin().lock();
        let mut stdout = io::stdout();
        self.run_with_io(&mut stdin, &mut stdout)
    }

    /// Run the tour experience using the provided IO handles.
    pub fn run_with_io<R, W>(&self, reader: &mut R, writer: &mut W) -> Result<()>
    where
        R: BufRead,
        W: Write,
    {
        display_welcome(writer)?;
        self.ensure_environment_ready(reader, writer)?;

        let entries = menu_entries();
        loop {
            {
                let progress = self.progress.borrow();
                render_menu(writer, &entries, &progress)?;
            }
            writer.flush().context("Failed to flush menu output")?;

            let mut buffer = String::new();
            reader
                .read_line(&mut buffer)
                .context("Failed to read menu selection")?;

            let trimmed = buffer.trim();
            match parse_selection(trimmed) {
                Some(MenuAction::Section(section)) => {
                    {
                        let mut progress = self.progress.borrow_mut();
                        progress.mark_section_started(section)?;
                    }
                    self.run_section(reader, writer, section)?;
                    prompt_return_to_menu(reader, writer)?;
                }
                Some(MenuAction::Progress) => {
                    self.render_progress(writer)?;
                    prompt_return_to_menu(reader, writer)?;
                }
                Some(MenuAction::Exit) => {
                    writeln!(writer, "ツアーを終了します。ご利用ありがとうございました！")?;
                    writer.flush().ok();
                    break;
                }
                None => {
                    writeln!(
                        writer,
                        "入力を認識できませんでした。対応する番号または記号を入力してください。"
                    )?;
                }
            }
        }

        Ok(())
    }

    fn ensure_environment_ready<R, W>(&self, reader: &mut R, writer: &mut W) -> Result<()>
    where
        R: BufRead,
        W: Write,
    {
        loop {
            let report = self.environment.build_report();
            match &report.status {
                JdkStatus::Ready { version, .. } => {
                    writeln!(writer, "✓ JDK環境を検出しました: {}", version)?;
                    writeln!(writer)?;
                    return Ok(());
                }
                JdkStatus::Outdated {
                    version,
                    detected_major,
                    required_major,
                } => {
                    writeln!(writer, "⚠️ JDKのバージョンが古いようです: {}", version)?;
                    if let Some(actual) = detected_major {
                        writeln!(writer, "検出されたメジャーバージョン: {}", actual)?;
                    }
                    writeln!(writer, "必要なメジャーバージョン: {}", required_major)?;
                    writeln!(writer)?;
                    render_setup_guide(writer, &report.guide)?;
                }
                JdkStatus::NotInstalled { message } => {
                    writeln!(writer, "❌ JDKが見つかりませんでした: {}", message)?;
                    writeln!(writer)?;
                    render_setup_guide(writer, &report.guide)?;
                }
                JdkStatus::Error { message } => {
                    writeln!(writer, "予期しないエラーが発生しました: {}", message)?;
                    writeln!(writer)?;
                    render_setup_guide(writer, &report.guide)?;
                }
            }

            prompt_for_setup_retry(reader, writer, report.completion_message)?;
        }
    }

    fn run_section<R, W>(&self, reader: &mut R, writer: &mut W, section: SectionId) -> Result<()>
    where
        R: BufRead,
        W: Write,
    {
        render_section_content(reader, writer, section)?;

        let quiz_outcome = {
            let mut progress = self.progress.borrow_mut();
            progress.run_quiz(section, reader, writer)?
        };

        for line in &quiz_outcome.feedback {
            writeln!(writer, "{}", line)?;
        }

        if quiz_outcome.passed {
            let completion = {
                let mut progress = self.progress.borrow_mut();
                progress.complete_section(section)?
            };
            self.render_completion_feedback(writer, section, completion)?;
        } else {
            writeln!(writer, "🔄 クイズに正解すると完了マークが付きます。")?;
        }

        Ok(())
    }

    fn render_progress<W: Write>(&self, writer: &mut W) -> Result<()> {
        let summary = self.progress.borrow().summary();
        render_progress_view(writer, &summary)
    }

    fn render_completion_feedback<W: Write>(
        &self,
        writer: &mut W,
        section: SectionId,
        outcome: CompletionOutcome,
    ) -> Result<()> {
        writeln!(
            writer,
            "\n✅ {} セクションを完了として記録しました。",
            section.title()
        )?;

        if let Some(path) = outcome.note_path {
            writeln!(
                writer,
                "💾 学習メモを {} に保存しました。必要に応じて追記してください。",
                path.display()
            )?;
        }

        if !outcome.achievements.is_empty() {
            writeln!(writer, "🏅 新しい実績を獲得しました:")?;
            for achievement in outcome.achievements {
                writeln!(
                    writer,
                    "  - {} ({})",
                    achievement.title, achievement.description
                )?;
            }
        }

        if let Some(certificate) = outcome.certificate {
            writeln!(
                writer,
                "\n📜 {}\n{}",
                certificate.title, certificate.message
            )?;
        }

        Ok(())
    }
}

fn display_welcome<W: Write>(writer: &mut W) -> Result<()> {
    writeln!(writer, "🎉 jv言語ツアーへようこそ！")?;
    writeln!(writer, "このツアーではjv言語の主要機能を段階的に学習し、")?;
    writeln!(
        writer,
        "Rustで実装されたコンパイラを通じてJava 25コード生成を体験します。"
    )?;
    writeln!(
        writer,
        "環境チェックの後、学習したいセクションを選択してください。\n"
    )?;
    Ok(())
}

const SECTION_MENU_KEYS: [(SectionId, &str); 9] = [
    (SectionId::BasicSyntax, "1"),
    (SectionId::ControlFlow, "2"),
    (SectionId::DataClasses, "3"),
    (SectionId::Functions, "4"),
    (SectionId::Concurrency, "5"),
    (SectionId::AsyncProgramming, "6"),
    (SectionId::BuildTools, "7"),
    (SectionId::InteractiveEditor, "8"),
    (SectionId::MiniProjectBuilder, "9"),
];

fn render_menu<W: Write>(
    writer: &mut W,
    entries: &[MenuEntry],
    progress: &ProgressTracker,
) -> Result<()> {
    writeln!(writer, "📚 学習メニュー")?;
    for entry in entries {
        match entry.action {
            MenuAction::Section(section) => {
                let status = progress.status_of(section);
                writeln!(
                    writer,
                    "  {}. {} {} — {} [{}]",
                    entry.key,
                    status.icon(),
                    entry.title,
                    entry.description,
                    status.label()
                )?;
            }
            MenuAction::Progress => {
                writeln!(
                    writer,
                    "  {}. {} — {}",
                    entry.key, entry.title, entry.description
                )?;
            }
            MenuAction::Exit => {
                writeln!(writer, "  {}. {}", entry.key, entry.title)?;
            }
        }
    }
    write!(writer, "> ")?;
    Ok(())
}

fn render_section_content<R, W>(reader: &mut R, writer: &mut W, section: SectionId) -> Result<()>
where
    R: BufRead,
    W: Write,
{
    match section {
        SectionId::BasicSyntax => sections::basic_syntax::render(writer),
        SectionId::ControlFlow => sections::control_flow::render(writer),
        SectionId::DataClasses => sections::data_classes::render(writer),
        SectionId::BuildTools => sections::build_tools::render(writer),
        SectionId::InteractiveEditor => sections::interactive::render(reader, writer),
        SectionId::MiniProjectBuilder => projects::render(writer),
        _ => render_placeholder_for_section(writer, section),
    }
}

fn render_placeholder_for_section<W: Write>(writer: &mut W, section: SectionId) -> Result<()> {
    writeln!(writer, "--- {} セクション ---", section.title())?;
    writeln!(writer, "このセクションの詳細コンテンツは現在準備中です。")?;
    writeln!(
        writer,
        "今後のタスクでコード例とJava出力が追加される予定です。"
    )?;
    Ok(())
}

fn render_progress_view<W: Write>(writer: &mut W, summary: &ProgressSummary) -> Result<()> {
    writeln!(writer, "--- 学習進捗ダッシュボード ---")?;
    if let Some(section) = summary.last_active_section {
        writeln!(
            writer,
            "直近の学習: {} ({})",
            section.title(),
            section.slug()
        )?;
    }

    writeln!(writer, "\n📊 セクション状況")?;
    for info in &summary.sections {
        writeln!(
            writer,
            "{} {} [{}]",
            info.icon,
            info.title,
            info.status.label()
        )?;
        writeln!(writer, "    {}", info.description)?;
        writeln!(
            writer,
            "    開始: {} / 完了: {} / クイズ: {}",
            format_timestamp(info.started_at),
            format_timestamp(info.completed_at),
            if info.quiz_passed {
                "合格"
            } else {
                "未合格"
            }
        )?;
    }

    writeln!(writer, "\n🏅 実績")?;
    if summary.achievements.is_empty() {
        writeln!(writer, "  (まだ獲得していません)")?;
    } else {
        for achievement in &summary.achievements {
            writeln!(
                writer,
                "  - {} [{}]",
                achievement.title,
                format_timestamp(Some(achievement.awarded_at))
            )?;
            writeln!(writer, "    {}", achievement.description)?;
        }
    }

    writeln!(writer, "\n📜 達成証明")?;
    if summary.certificates.is_empty() {
        writeln!(writer, "  (証明書はまだ発行されていません)")?;
    } else {
        for certificate in &summary.certificates {
            writeln!(
                writer,
                "  - {} [{}]",
                certificate.title,
                format_timestamp(Some(certificate.awarded_at))
            )?;
            writeln!(writer, "    {}", certificate.message)?;
        }
    }

    Ok(())
}

fn format_timestamp(value: Option<chrono::DateTime<chrono::Utc>>) -> String {
    value
        .map(|ts| {
            ts.with_timezone(&Local)
                .format("%Y-%m-%d %H:%M")
                .to_string()
        })
        .unwrap_or_else(|| "-".to_string())
}

fn prompt_return_to_menu<R, W>(reader: &mut R, writer: &mut W) -> Result<()>
where
    R: BufRead,
    W: Write,
{
    writeln!(writer, "\n続行するにはEnterキーを押してください...")?;
    writer.flush().ok();
    let mut buffer = String::new();
    reader.read_line(&mut buffer).ok();
    Ok(())
}

fn prompt_for_setup_retry<R, W>(
    reader: &mut R,
    writer: &mut W,
    completion_message: &str,
) -> Result<()>
where
    R: BufRead,
    W: Write,
{
    writeln!(
        writer,
        "セットアップ手順を完了したらEnterキーを押して再チェックします。"
    )?;
    writeln!(writer, "{}", completion_message)?;
    writer.flush().ok();
    let mut buffer = String::new();
    reader.read_line(&mut buffer).ok();
    Ok(())
}

fn render_setup_guide<W: Write>(
    writer: &mut W,
    guide: &super::environment::SetupGuide<'_>,
) -> Result<()> {
    writeln!(writer, "JDKセットアップガイド")?;
    writeln!(writer, "--- {} ---", guide.headline)?;
    for step in &guide.steps {
        writeln!(writer, "{}", step)?;
    }
    writeln!(writer, "\nインストール後の確認:")?;
    for check in &guide.post_install_checks {
        writeln!(writer, "{}", check)?;
    }
    writeln!(writer, "\n推奨ディストリビューション:")?;
    for distribution in &guide.distributions {
        writeln!(
            writer,
            "- {}: {} ({})",
            distribution.name, distribution.notes, distribution.url
        )?;
    }
    writeln!(writer)?;
    Ok(())
}

fn menu_entries() -> Vec<MenuEntry> {
    let mut entries: Vec<MenuEntry> = SECTION_MENU_KEYS
        .iter()
        .map(|(section, key)| MenuEntry {
            key,
            title: section.title(),
            description: section.description(),
            action: MenuAction::Section(*section),
        })
        .collect();

    entries.push(MenuEntry {
        key: "P",
        title: "進捗確認",
        description: "完了ステータス、実績、証明書を表示",
        action: MenuAction::Progress,
    });

    entries.push(MenuEntry {
        key: "0",
        title: "終了",
        description: "",
        action: MenuAction::Exit,
    });

    entries
}

fn parse_selection(input: &str) -> Option<MenuAction> {
    let normalized = input.trim().to_ascii_uppercase();
    if normalized.is_empty() {
        return None;
    }

    let entries = menu_entries();
    entries
        .into_iter()
        .find(|entry| entry.key.eq_ignore_ascii_case(normalized.as_str()))
        .map(|entry| entry.action)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::VecDeque;

    use jv_build::BuildError;

    #[test]
    fn section_metadata_contains_expected_titles() {
        assert_eq!(SectionId::BasicSyntax.title(), "基本構文");
        assert!(
            SectionId::AsyncProgramming
                .description()
                .contains("CompletableFuture")
        );
    }

    #[test]
    fn menu_entries_include_all_sections_and_controls() {
        let entries = menu_entries();
        assert_eq!(entries.len(), 11);
        let section_count = entries
            .iter()
            .filter(|entry| matches!(entry.action, MenuAction::Section(_)))
            .count();
        assert_eq!(section_count, 9);
        assert!(
            entries
                .iter()
                .any(|entry| entry.action == MenuAction::Progress)
        );
        assert!(entries.iter().any(|entry| entry.action == MenuAction::Exit));
    }

    #[test]
    fn parse_selection_matches_keys_case_insensitively() {
        assert_eq!(
            parse_selection("1"),
            Some(MenuAction::Section(SectionId::BasicSyntax))
        );
        assert_eq!(parse_selection("p"), Some(MenuAction::Progress));
        assert_eq!(parse_selection("0"), Some(MenuAction::Exit));
        assert_eq!(parse_selection(""), None);
        assert_eq!(parse_selection("x"), None);
    }

    #[test]
    fn environment_ready_short_circuits() {
        let manager =
            EnvironmentManager::with_probe(MockSequenceProbe::new(vec![
                Ok("javac 25".to_string()),
            ]));
        let cli = TourCli::with_environment_manager(manager);
        let mut input = io::Cursor::new(Vec::<u8>::new());
        let mut output = Vec::new();
        cli.ensure_environment_ready(&mut input, &mut output)
            .expect("environment ready");
        let rendered = String::from_utf8(output).unwrap();
        assert!(rendered.contains("JDK環境を検出"));
    }

    #[test]
    fn environment_retry_renders_guide_and_rechecks() {
        let manager = EnvironmentManager::with_probe(MockSequenceProbe::new(vec![
            Err(BuildError::JdkNotFound("missing".into())),
            Ok("javac 25".into()),
        ]));
        let cli = TourCli::with_environment_manager(manager);
        let mut input = io::Cursor::new(b"\n".to_vec());
        let mut output = Vec::new();
        cli.ensure_environment_ready(&mut input, &mut output)
            .expect("environment eventually ready");
        let rendered = String::from_utf8(output).unwrap();
        assert!(rendered.contains("JDKセットアップガイド"));
        assert!(rendered.contains("JDK環境を検出"));
    }

    #[derive(Debug, Clone)]
    struct MockSequenceProbe {
        results: std::sync::Arc<std::sync::Mutex<VecDeque<Result<String, BuildError>>>>,
    }

    impl MockSequenceProbe {
        fn new(results: Vec<Result<String, BuildError>>) -> Self {
            Self {
                results: std::sync::Arc::new(std::sync::Mutex::new(results.into())),
            }
        }
    }

    impl JdkProbe for MockSequenceProbe {
        fn probe(&self) -> Result<String, BuildError> {
            let mut results = self.results.lock().unwrap();
            results
                .pop_front()
                .unwrap_or_else(|| Err(BuildError::JdkNotFound("no more results".into())))
        }
    }
}
