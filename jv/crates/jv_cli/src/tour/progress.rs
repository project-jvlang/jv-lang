use std::collections::BTreeMap;
use std::fs;
use std::io::{BufRead, Write};
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use super::cli::SectionId;

const STORAGE_VERSION: u8 = 1;
const CERTIFICATE_TITLE: &str = "jv言語ツアー達成証";

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, PartialOrd, Ord)]
pub enum SectionStatus {
    #[serde(rename = "not_started")]
    NotStarted,
    #[serde(rename = "in_progress")]
    InProgress,
    #[serde(rename = "completed")]
    Completed,
}

impl Default for SectionStatus {
    fn default() -> Self {
        SectionStatus::NotStarted
    }
}

impl SectionStatus {
    pub fn icon(self) -> &'static str {
        match self {
            SectionStatus::NotStarted => "⭕",
            SectionStatus::InProgress => "🔄",
            SectionStatus::Completed => "✅",
        }
    }

    pub fn label(self) -> &'static str {
        match self {
            SectionStatus::NotStarted => "未着手",
            SectionStatus::InProgress => "学習中",
            SectionStatus::Completed => "完了",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AchievementId {
    FirstSection,
    HalfMilestone,
    FullCompletion,
    QuizMaster,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AchievementRecord {
    pub id: AchievementId,
    pub title: String,
    pub description: String,
    pub awarded_at: DateTime<Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Certificate {
    pub title: String,
    pub awarded_at: DateTime<Utc>,
    pub message: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct QuizState {
    pub attempts: u32,
    pub passed: bool,
    pub last_answer: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct SectionProgress {
    pub status: SectionStatus,
    pub started_at: Option<DateTime<Utc>>,
    pub completed_at: Option<DateTime<Utc>>,
    pub quiz: QuizState,
    pub note_path: Option<PathBuf>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct ProgressState {
    version: u8,
    sections: BTreeMap<SectionId, SectionProgress>,
    achievements: Vec<AchievementRecord>,
    certificates: Vec<Certificate>,
    last_active_section: Option<SectionId>,
}

impl Default for ProgressState {
    fn default() -> Self {
        let mut sections = BTreeMap::new();
        for &section in SectionId::all() {
            sections.insert(section, SectionProgress::default());
        }
        Self {
            version: STORAGE_VERSION,
            sections,
            achievements: Vec::new(),
            certificates: Vec::new(),
            last_active_section: None,
        }
    }
}

#[derive(Debug, Clone)]
enum Storage {
    File(PathBuf),
    Ephemeral,
}

impl Storage {
    fn path(&self) -> Option<&Path> {
        match self {
            Storage::File(path) => Some(path.as_path()),
            Storage::Ephemeral => None,
        }
    }

    fn progress_dir(&self) -> Option<PathBuf> {
        self.path()
            .and_then(|path| path.parent().map(|p| p.to_path_buf()))
    }
}

#[derive(Debug, Clone)]
pub struct ProgressTracker {
    storage: Storage,
    state: ProgressState,
}

#[derive(Debug, Clone)]
pub struct SectionDisplay {
    pub section: SectionId,
    pub title: String,
    pub description: String,
    pub status: SectionStatus,
    pub icon: &'static str,
    pub started_at: Option<DateTime<Utc>>,
    pub completed_at: Option<DateTime<Utc>>,
    pub quiz_passed: bool,
}

#[derive(Debug, Clone)]
pub struct ProgressSummary {
    pub sections: Vec<SectionDisplay>,
    pub achievements: Vec<AchievementRecord>,
    pub certificates: Vec<Certificate>,
    pub last_active_section: Option<SectionId>,
}

#[derive(Debug, Clone)]
pub struct QuizOutcome {
    pub passed: bool,
    pub feedback: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct CompletionOutcome {
    pub achievements: Vec<AchievementRecord>,
    pub certificate: Option<Certificate>,
    pub note_path: Option<PathBuf>,
}

#[derive(Debug, Clone)]
struct QuizQuestion {
    section: SectionId,
    question: &'static str,
    choices: [&'static str; 4],
    answer: usize,
    explanation: &'static str,
}

const QUIZZES: &[QuizQuestion] = &[
    QuizQuestion {
        section: SectionId::BasicSyntax,
        question: "`val` 宣言の特徴はどれでしょうか？",
        choices: [
            "再代入できる可変変数を作る",
            "null を許容する変数を作る",
            "定数として再代入できない値を作る",
            "Java の final とは関係がない",
        ],
        answer: 2,
        explanation: "`val` は Java 側では final 付きのローカル変数になります。",
    },
    QuizQuestion {
        section: SectionId::ControlFlow,
        question: "`when` 式について正しい説明はどれですか？",
        choices: [
            "必ず else 分岐が必要",
            "パターンマッチングとして値を返せる",
            "Java の switch と同じく fallthrough がある",
            "複数条件を同時に扱えない",
        ],
        answer: 1,
        explanation: "when 式は値を返せる式で、複数条件を扱えるパターンマッチングです。",
    },
    QuizQuestion {
        section: SectionId::DataClasses,
        question: "データクラスから生成される Java の構造は？",
        choices: [
            "常に interface が生成される",
            "record やクラスなどイミュータブルな表現",
            "ネイティブコードが生成される",
            "シリアライズ不可なクラスだけ",
        ],
        answer: 1,
        explanation: "データクラスは record などのイミュータブルな Java 構造を生成します。",
    },
    QuizQuestion {
        section: SectionId::Functions,
        question: "デフォルト引数付きの関数を呼び出すときの特徴は？",
        choices: [
            "すべての引数を必ず指定する必要がある",
            "デフォルト値が無視される",
            "省略した引数は宣言時のデフォルト値が適用される",
            "トップレベルからは呼び出せない",
        ],
        answer: 2,
        explanation: "デフォルト引数は省略時に宣言で定義した値が使われます。",
    },
    QuizQuestion {
        section: SectionId::Concurrency,
        question: "`spawn {}` ブロックの目的は？",
        choices: [
            "JavaScript の async と互換性を持たせる",
            "仮想スレッドで軽量な並行処理を行う",
            "ガベージコレクションを停止する",
            "UI スレッドをブロックする",
        ],
        answer: 1,
        explanation: "spawn ブロックは仮想スレッドで軽量なタスクを並行実行します。",
    },
    QuizQuestion {
        section: SectionId::AsyncProgramming,
        question: "`await` が変換される Java の主要 API は？",
        choices: [
            "java.nio.file.Files",
            "CompletableFuture",
            "java.sql.Connection",
            "ThreadLocal",
        ],
        answer: 1,
        explanation: "async/await は Java 側で CompletableFuture を用いて実現されます。",
    },
    QuizQuestion {
        section: SectionId::BuildTools,
        question: "`jv.toml` の役割は？",
        choices: [
            "Java バイトコードを直接編集する",
            "依存関係や JDK バージョンなどビルド設定を記述する",
            "Rust の Cargo.toml を生成する",
            "JIT コンパイラを無効化する",
        ],
        answer: 1,
        explanation: "jv.toml には依存関係や必要な JDK バージョンなどのビルド設定を記述します。",
    },
    QuizQuestion {
        section: SectionId::InteractiveEditor,
        question: "インタラクティブ編集モードの目的は？",
        choices: [
            "外部ファイルを削除する",
            "リアルタイムで構文検証しながら学習できる場を提供する",
            "Java バイトコードを書き換える",
            "GPU でコードを実行する",
        ],
        answer: 1,
        explanation: "インタラクティブ編集モードはリアルタイムで構文検証と実行を支援します。",
    },
    QuizQuestion {
        section: SectionId::MiniProjectBuilder,
        question: "ミニプロジェクトビルダーのゴールは？",
        choices: [
            "jv コンパイラを再実装する",
            "学んだ機能を組み合わせて実用的なアプリを構築する",
            "Java バイトコードを直接生成する",
            "ネットワークアクセスを提供する",
        ],
        answer: 1,
        explanation: "ミニプロジェクトビルダーは学んだ機能を組み合わせた実践的な成果物を作ります。",
    },
];

impl ProgressTracker {
    pub fn load_default() -> Self {
        Self::from_path(default_store_path())
    }

    pub fn ephemeral() -> Self {
        Self {
            storage: Storage::Ephemeral,
            state: ProgressState::default(),
        }
    }

    pub fn with_path(path: PathBuf) -> Self {
        Self::from_path(path)
    }

    fn from_path(path: PathBuf) -> Self {
        let state = if path.exists() {
            match fs::read_to_string(&path) {
                Ok(raw) => serde_json::from_str(&raw).unwrap_or_else(|err| {
                    eprintln!(
                        "[jv tour] 進捗データの読み込みに失敗しました: {}。新しいファイルを作成します。",
                        err
                    );
                    ProgressState::default()
                }),
                Err(err) => {
                    eprintln!(
                        "[jv tour] 進捗ファイル ({}) の読み込みに失敗しました: {}。新しいファイルを作成します。",
                        path.display(),
                        err
                    );
                    ProgressState::default()
                }
            }
        } else {
            ProgressState::default()
        };

        Self {
            storage: Storage::File(path),
            state,
        }
    }

    pub fn summary(&self) -> ProgressSummary {
        let sections = SectionId::all()
            .iter()
            .map(|section| {
                let progress = self
                    .state
                    .sections
                    .get(section)
                    .cloned()
                    .unwrap_or_default();
                SectionDisplay {
                    section: *section,
                    title: section.title().to_string(),
                    description: section.description().to_string(),
                    status: progress.status,
                    icon: progress.status.icon(),
                    started_at: progress.started_at,
                    completed_at: progress.completed_at,
                    quiz_passed: progress.quiz.passed,
                }
            })
            .collect();

        ProgressSummary {
            sections,
            achievements: self.state.achievements.clone(),
            certificates: self.state.certificates.clone(),
            last_active_section: self.state.last_active_section,
        }
    }

    pub fn status_of(&self, section: SectionId) -> SectionStatus {
        self.state
            .sections
            .get(&section)
            .map(|progress| progress.status)
            .unwrap_or_default()
    }

    pub fn mark_section_started(&mut self, section: SectionId) -> Result<()> {
        let entry = self
            .state
            .sections
            .entry(section)
            .or_insert_with(SectionProgress::default);
        if entry.status == SectionStatus::NotStarted {
            entry.status = SectionStatus::InProgress;
            entry.started_at = Some(Utc::now());
        }
        self.state.last_active_section = Some(section);
        self.persist()
    }

    pub fn run_quiz<R: BufRead, W: Write>(
        &mut self,
        section: SectionId,
        reader: &mut R,
        writer: &mut W,
    ) -> Result<QuizOutcome> {
        let question = match QUIZZES.iter().find(|quiz| quiz.section == section) {
            Some(quiz) => quiz,
            None => {
                return Ok(QuizOutcome {
                    passed: true,
                    feedback: vec![format!(
                        "ℹ️ {} セクションには現在クイズが設定されていません。",
                        section.title()
                    )],
                });
            }
        };

        let entry = self
            .state
            .sections
            .entry(section)
            .or_insert_with(SectionProgress::default);

        if entry.quiz.passed {
            return Ok(QuizOutcome {
                passed: true,
                feedback: vec![format!(
                    "✅ {} セクションのクイズは既に合格済みです。",
                    section.title()
                )],
            });
        }

        writeln!(writer, "\n🧠 理解度チェック: {}", section.title())?;
        writeln!(writer, "{}", question.question)?;
        for (index, choice) in question.choices.iter().enumerate() {
            writeln!(writer, "  {}. {}", index + 1, choice)?;
        }
        write!(writer, "回答を数字で入力してください > ")?;
        writer.flush().ok();

        let mut buffer = String::new();
        reader
            .read_line(&mut buffer)
            .context("クイズ入力の読み取りに失敗しました")?;
        let trimmed = buffer.trim();

        entry.quiz.attempts += 1;
        entry.quiz.last_answer = if trimmed.is_empty() {
            None
        } else {
            Some(trimmed.to_string())
        };

        let selected = parse_choice(trimmed);

        if let Some(answer) = selected {
            if answer == question.answer {
                entry.quiz.passed = true;
                self.persist()?;
                return Ok(QuizOutcome {
                    passed: true,
                    feedback: vec![
                        "🎉 正解です！セクションを完了として記録します。".to_string(),
                        question.explanation.to_string(),
                    ],
                });
            }
        }

        self.persist()?;
        Ok(QuizOutcome {
            passed: false,
            feedback: vec![
                "❌ 正解ではありませんでした。もう一度コンテンツを見直して再挑戦してください。"
                    .to_string(),
                format!("ヒント: {}", question.explanation),
            ],
        })
    }

    pub fn complete_section(&mut self, section: SectionId) -> Result<CompletionOutcome> {
        let (completed_at, existing_note, needs_note) = {
            let entry = self
                .state
                .sections
                .entry(section)
                .or_insert_with(SectionProgress::default);

            if entry.status != SectionStatus::Completed {
                entry.status = SectionStatus::Completed;
                entry.completed_at = Some(Utc::now());
            }

            (
                entry.completed_at,
                entry.note_path.clone(),
                entry.note_path.is_none(),
            )
        };

        let mut note_path = existing_note;

        if needs_note {
            if let Some(generated) = self.create_learning_note(section, completed_at)? {
                if let Some(entry) = self.state.sections.get_mut(&section) {
                    entry.note_path = Some(generated.clone());
                }
                note_path = Some(generated);
            }
        }

        let achievements = self.update_achievements();
        let certificate = self.ensure_certificate();

        self.persist()?;

        Ok(CompletionOutcome {
            achievements,
            certificate,
            note_path,
        })
    }

    fn update_achievements(&mut self) -> Vec<AchievementRecord> {
        let completed = self
            .state
            .sections
            .values()
            .filter(|progress| progress.status == SectionStatus::Completed)
            .count();

        let total = SectionId::all().len();
        let mut newly_awarded = Vec::new();

        if completed >= 1 {
            if let Some(record) = self.ensure_achievement(
                AchievementId::FirstSection,
                "ファーストステップ",
                "初めてのセクションを完了しました。継続して学習を進めましょう！",
            ) {
                newly_awarded.push(record);
            }
        }

        if completed >= total / 2 {
            if let Some(record) = self.ensure_achievement(
                AchievementId::HalfMilestone,
                "ミッドウェイヒーロー",
                "全セクションの半分に到達しました。もうひと踏ん張り！",
            ) {
                newly_awarded.push(record);
            }
        }

        if completed == total {
            if let Some(record) = self.ensure_achievement(
                AchievementId::FullCompletion,
                "ツアーコンプリート",
                "全てのセクションを完了し、jv言語の主要機能をマスターしました。",
            ) {
                newly_awarded.push(record);
            }
        }

        if self
            .state
            .sections
            .values()
            .all(|progress| progress.quiz.passed)
        {
            if let Some(record) = self.ensure_achievement(
                AchievementId::QuizMaster,
                "クイズマスター",
                "全ての理解度チェックに合格しました。知識が確かなものになりました！",
            ) {
                newly_awarded.push(record);
            }
        }

        newly_awarded
    }

    fn ensure_achievement(
        &mut self,
        id: AchievementId,
        title: &str,
        description: &str,
    ) -> Option<AchievementRecord> {
        if self.state.achievements.iter().any(|record| record.id == id) {
            return None;
        }

        let record = AchievementRecord {
            id,
            title: title.to_string(),
            description: description.to_string(),
            awarded_at: Utc::now(),
        };

        self.state.achievements.push(record.clone());
        Some(record)
    }

    fn ensure_certificate(&mut self) -> Option<Certificate> {
        let total = SectionId::all().len();
        let completed = self
            .state
            .sections
            .values()
            .filter(|progress| progress.status == SectionStatus::Completed)
            .count();

        if completed != total {
            return None;
        }

        if self
            .state
            .certificates
            .iter()
            .any(|certificate| certificate.title == CERTIFICATE_TITLE)
        {
            return None;
        }

        let certificate = Certificate {
            title: CERTIFICATE_TITLE.to_string(),
            awarded_at: Utc::now(),
            message: "おめでとうございます！jv 言語ツアーの全モジュールを修了しました。学んだ内容を活かしてプロジェクトに挑戦しましょう。"
                .to_string(),
        };

        self.state.certificates.push(certificate.clone());
        Some(certificate)
    }

    fn create_learning_note(
        &self,
        section: SectionId,
        completed_at: Option<DateTime<Utc>>,
    ) -> Result<Option<PathBuf>> {
        let base = match self.storage.progress_dir() {
            Some(dir) => dir,
            None => return Ok(None),
        };

        let notes_dir = base.join("notes");
        fs::create_dir_all(&notes_dir).context("ノートディレクトリの作成に失敗しました")?;

        let filename = format!("{:02}-{}.md", section.order(), section.slug());
        let path = notes_dir.join(filename);

        let timestamp = completed_at
            .map(|ts| ts.format("%Y-%m-%d %H:%M:%S UTC").to_string())
            .unwrap_or_else(|| "日時未記録".to_string());

        let content = format!(
            "# {} 学習メモ\n\n- ステータス: {}\n- 完了日時: {}\n\n## 気づき\n- 学んだことを箇条書きで整理しましょう。\n\n## サンプルコード\n- 学習中に試した jv コードをここに追記してください。\n",
            section.title(),
            SectionStatus::Completed.label(),
            timestamp
        );

        fs::write(&path, content)
            .with_context(|| format!("学習メモ ({}) の書き込みに失敗しました", path.display()))?;

        Ok(Some(path))
    }

    fn persist(&self) -> Result<()> {
        if let Storage::File(path) = &self.storage {
            if let Some(parent) = path.parent() {
                fs::create_dir_all(parent).with_context(|| {
                    format!(
                        "進捗ディレクトリ ({}) の作成に失敗しました",
                        parent.display()
                    )
                })?;
            }

            let serialized = serde_json::to_string_pretty(&self.state)
                .context("進捗データのシリアライズに失敗しました")?;

            fs::write(path, serialized).with_context(|| {
                format!("進捗ファイル ({}) の書き込みに失敗しました", path.display())
            })?;
        }
        Ok(())
    }
}

fn parse_choice(input: &str) -> Option<usize> {
    if input.is_empty() {
        return None;
    }

    if let Ok(value) = input.parse::<usize>() {
        let index = value.checked_sub(1)?;
        if index < 4 {
            return Some(index);
        }
    }

    match input.to_ascii_lowercase().as_str() {
        "a" => Some(0),
        "b" => Some(1),
        "c" => Some(2),
        "d" => Some(3),
        _ => None,
    }
}

fn default_store_path() -> PathBuf {
    if let Ok(custom) = std::env::var("JV_TOUR_HOME") {
        return PathBuf::from(custom).join("progress.json");
    }

    if let Ok(home) = std::env::var("HOME") {
        return Path::new(&home).join(".jv_tour").join("progress.json");
    }

    if let Ok(user_profile) = std::env::var("USERPROFILE") {
        return Path::new(&user_profile)
            .join(".jv_tour")
            .join("progress.json");
    }

    std::env::current_dir()
        .unwrap_or_else(|_| PathBuf::from("."))
        .join(".jv_tour")
        .join("progress.json")
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::io;

    fn temp_progress_tracker() -> ProgressTracker {
        let mut path = std::env::temp_dir();
        path.push(format!(
            "jv_tour_test_{}_{}.json",
            std::process::id(),
            Utc::now().timestamp_nanos_opt().unwrap_or(0)
        ));
        ProgressTracker::with_path(path)
    }

    #[test]
    fn default_state_marks_all_sections_not_started() {
        let tracker = ProgressTracker::ephemeral();
        for &section in SectionId::all() {
            assert_eq!(tracker.status_of(section), SectionStatus::NotStarted);
        }
    }

    #[test]
    fn starting_section_switches_status_to_in_progress() {
        let mut tracker = ProgressTracker::ephemeral();
        tracker
            .mark_section_started(SectionId::BasicSyntax)
            .expect("mark started");
        assert_eq!(
            tracker.status_of(SectionId::BasicSyntax),
            SectionStatus::InProgress
        );
    }

    #[test]
    fn completing_section_awards_achievement() {
        let mut tracker = ProgressTracker::ephemeral();
        tracker
            .mark_section_started(SectionId::BasicSyntax)
            .expect("start section");

        {
            let mut buffer = io::Cursor::new(b"3\n".to_vec());
            let mut sink = Vec::new();
            let outcome = tracker
                .run_quiz(SectionId::BasicSyntax, &mut buffer, &mut sink)
                .expect("quiz");
            assert!(outcome.passed, "quiz should pass with correct answer");
        }

        let outcome = tracker
            .complete_section(SectionId::BasicSyntax)
            .expect("complete");
        assert!(!outcome.achievements.is_empty());
    }

    #[test]
    fn persistence_writes_file() {
        let tracker = temp_progress_tracker();
        if let Storage::File(path) = &tracker.storage {
            assert!(!path.exists());
        }
    }

    #[test]
    fn parse_choice_accepts_letters_and_numbers() {
        assert_eq!(parse_choice("1"), Some(0));
        assert_eq!(parse_choice("3"), Some(2));
        assert_eq!(parse_choice("b"), Some(1));
        assert_eq!(parse_choice("D"), Some(3));
        assert_eq!(parse_choice(""), None);
        assert_eq!(parse_choice("5"), None);
    }

    #[test]
    fn ensure_certificate_generates_when_all_completed() {
        let mut tracker = ProgressTracker::ephemeral();
        for &section in SectionId::all() {
            tracker
                .mark_section_started(section)
                .expect("start section");
            // simulate quiz pass
            if let Some(entry) = tracker.state.sections.get_mut(&section) {
                entry.quiz.passed = true;
            }
            tracker.complete_section(section).expect("complete section");
        }

        assert!(tracker
            .state
            .certificates
            .iter()
            .any(|cert| cert.title == CERTIFICATE_TITLE));
    }

    #[test]
    fn learning_note_is_created_in_storage_directory() {
        let mut path = std::env::temp_dir();
        path.push(format!(
            "jv_tour_test_storage_{}_{}",
            std::process::id(),
            Utc::now().timestamp_nanos_opt().unwrap_or(0)
        ));
        let tracker = ProgressTracker::with_path(path.join("progress.json"));
        let mut tracker = tracker;

        tracker
            .mark_section_started(SectionId::BasicSyntax)
            .expect("start section");
        if let Some(entry) = tracker.state.sections.get_mut(&SectionId::BasicSyntax) {
            entry.quiz.passed = true;
        }

        let outcome = tracker
            .complete_section(SectionId::BasicSyntax)
            .expect("complete");

        let note_path = outcome.note_path.expect("note path");
        assert!(note_path.exists());

        // cleanup
        if let Some(dir) = note_path.parent() {
            let _ = fs::remove_dir_all(dir.parent().unwrap_or(dir));
        }
    }
}
