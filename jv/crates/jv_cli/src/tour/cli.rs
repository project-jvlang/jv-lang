use std::io::{self, BufRead, Write};

use anyhow::{Context, Result};

use super::environment::{EnvironmentManager, JdkProbe, JdkStatus};
use super::sections;

/// Identifier for tour sections that are accessible from the main menu.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
    fn title(self) -> &'static str {
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

    fn description(self) -> &'static str {
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
}

impl TourCli {
    /// Create a CLI experience using the default environment probe.
    pub fn new() -> Self {
        Self {
            environment: EnvironmentManager::new(),
        }
    }
}

impl<P: JdkProbe> TourCli<P> {
    /// Create a CLI instance with a custom environment manager (mainly for testing).
    pub fn with_environment_manager(environment: EnvironmentManager<P>) -> Self {
        Self { environment }
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
            render_menu(writer, &entries)?;
            writer.flush().context("Failed to flush menu output")?;

            let mut buffer = String::new();
            reader
                .read_line(&mut buffer)
                .context("Failed to read menu selection")?;

            let trimmed = buffer.trim();
            match parse_selection(trimmed) {
                Some(MenuAction::Section(section)) => {
                    render_section(writer, section)?;
                    prompt_return_to_menu(reader, writer)?;
                }
                Some(MenuAction::Progress) => {
                    render_progress_placeholder(writer)?;
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

fn render_menu<W: Write>(writer: &mut W, entries: &[MenuEntry]) -> Result<()> {
    writeln!(writer, "📚 学習メニュー")?;
    for entry in entries
        .iter()
        .filter(|e| matches!(e.action, MenuAction::Section(_)))
    {
        writeln!(
            writer,
            "  {}. {} — {}",
            entry.key, entry.title, entry.description
        )?;
    }
    if let Some(progress) = entries.iter().find(|e| e.action == MenuAction::Progress) {
        writeln!(
            writer,
            "  {}. {} — {}",
            progress.key, progress.title, progress.description
        )?;
    }
    if let Some(exit) = entries.iter().find(|e| e.action == MenuAction::Exit) {
        writeln!(writer, "  {}. {}", exit.key, exit.title)?;
    }
    write!(writer, "> ")?;
    Ok(())
}

fn render_section<W: Write>(writer: &mut W, section: SectionId) -> Result<()> {
    match section {
        SectionId::BasicSyntax => sections::basic_syntax::render(writer),
        SectionId::ControlFlow => sections::control_flow::render(writer),
        SectionId::DataClasses => sections::data_classes::render(writer),
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

fn render_progress_placeholder<W: Write>(writer: &mut W) -> Result<()> {
    writeln!(writer, "--- 進捗確認 ---")?;
    writeln!(
        writer,
        "学習進捗の保存と可視化機能は別タスクで実装されます。"
    )?;
    writeln!(
        writer,
        "完了済みのセクションはメニュー上で✅等のマークで表示予定です。"
    )?;
    Ok(())
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
    vec![
        MenuEntry {
            key: "1",
            title: "基本構文",
            description: SectionId::BasicSyntax.description(),
            action: MenuAction::Section(SectionId::BasicSyntax),
        },
        MenuEntry {
            key: "2",
            title: "制御フロー",
            description: SectionId::ControlFlow.description(),
            action: MenuAction::Section(SectionId::ControlFlow),
        },
        MenuEntry {
            key: "3",
            title: "データクラス",
            description: SectionId::DataClasses.description(),
            action: MenuAction::Section(SectionId::DataClasses),
        },
        MenuEntry {
            key: "4",
            title: "関数",
            description: SectionId::Functions.description(),
            action: MenuAction::Section(SectionId::Functions),
        },
        MenuEntry {
            key: "5",
            title: "並行性",
            description: SectionId::Concurrency.description(),
            action: MenuAction::Section(SectionId::Concurrency),
        },
        MenuEntry {
            key: "6",
            title: "非同期プログラミング",
            description: SectionId::AsyncProgramming.description(),
            action: MenuAction::Section(SectionId::AsyncProgramming),
        },
        MenuEntry {
            key: "7",
            title: "ビルドツール体験",
            description: SectionId::BuildTools.description(),
            action: MenuAction::Section(SectionId::BuildTools),
        },
        MenuEntry {
            key: "8",
            title: "インタラクティブ編集",
            description: SectionId::InteractiveEditor.description(),
            action: MenuAction::Section(SectionId::InteractiveEditor),
        },
        MenuEntry {
            key: "9",
            title: "ミニプロジェクト",
            description: SectionId::MiniProjectBuilder.description(),
            action: MenuAction::Section(SectionId::MiniProjectBuilder),
        },
        MenuEntry {
            key: "P",
            title: "進捗確認",
            description: "完了したセクションのステータスと達成状況を表示",
            action: MenuAction::Progress,
        },
        MenuEntry {
            key: "0",
            title: "終了",
            description: "",
            action: MenuAction::Exit,
        },
    ]
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
        assert!(SectionId::AsyncProgramming
            .description()
            .contains("CompletableFuture"));
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
        assert!(entries
            .iter()
            .any(|entry| entry.action == MenuAction::Progress));
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
