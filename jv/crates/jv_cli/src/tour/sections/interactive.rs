use std::fs;
use std::io::{BufRead, Write};
use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

use anyhow::{Context, Result, anyhow};
use jv_ast::Span;
use jv_checker::TypeChecker;
use jv_codegen_java::generate_java_source;
use jv_ir::transform_program;
use jv_parser_frontend::{ParseError, Parser2Pipeline, ParserPipeline};

const DEFAULT_SNIPPET: &str = r#"fun main() {
    println("こんにちは、インタラクティブエディタ！")
}
"#;

/// Entry point for the Interactive Editor experience.
pub fn render<R, W>(reader: &mut R, writer: &mut W) -> Result<()>
where
    R: BufRead,
    W: Write,
{
    let mut session = SessionState::default();
    session.print_intro(writer)?;

    loop {
        write!(writer, "editor> ")?;
        writer.flush().ok();

        let mut input = String::new();
        if reader.read_line(&mut input)? == 0 {
            writeln!(writer, "入力が終了しました。メニューへ戻ります。")?;
            break;
        }

        let trimmed = input.trim_end();
        if trimmed.is_empty() {
            continue;
        }

        if trimmed.starts_with(':') {
            if session.handle_command(trimmed, reader, writer)? {
                break;
            }
        } else {
            session.append_line(trimmed, writer)?;
        }
    }

    Ok(())
}

struct SessionState {
    lines: Vec<String>,
}

impl Default for SessionState {
    fn default() -> Self {
        Self {
            lines: DEFAULT_SNIPPET
                .lines()
                .map(|line| line.to_string())
                .collect(),
        }
    }
}

impl SessionState {
    fn print_intro<W: Write>(&self, writer: &mut W) -> Result<()> {
        writeln!(writer, "--- インタラクティブエディタ ---")?;
        writeln!(
            writer,
            "リアルタイムでjvコードを編集し、構文チェックとJava出力、サンドボックス実行を体験できます。"
        )?;
        writeln!(
            writer,
            "主なコマンド: :help, :show, :edit, :append, :reset, :run, :quit"
        )?;
        writeln!(writer, "そのままコード行を入力すると末尾に追加されます。")?;
        writeln!(
            writer,
            "初期コードを表示するには :show を入力してください。\n"
        )?;
        Ok(())
    }

    fn handle_command<R, W>(
        &mut self,
        command: &str,
        reader: &mut R,
        writer: &mut W,
    ) -> Result<bool>
    where
        R: BufRead,
        W: Write,
    {
        match command {
            ":help" => self.print_help(writer)?,
            ":show" => self.print_buffer(writer)?,
            ":edit" => self.replace_buffer(reader, writer)?,
            ":append" => self.append_block(reader, writer)?,
            ":reset" => self.reset(writer)?,
            ":run" => self.run_pipeline(writer)?,
            ":quit" | ":exit" => {
                writeln!(writer, "インタラクティブエディタを終了します。")?;
                return Ok(true);
            }
            other => {
                writeln!(
                    writer,
                    "未知のコマンドです: {} ( :help で一覧を表示 )",
                    other
                )?;
            }
        }
        Ok(false)
    }

    fn append_line<W: Write>(&mut self, line: &str, writer: &mut W) -> Result<()>
    where
        W: Write,
    {
        self.lines.push(line.to_string());
        writeln!(
            writer,
            "追加しました (現在 {} 行)。:run で検証を実行できます。",
            self.lines.len()
        )?;
        Ok(())
    }

    fn print_help<W: Write>(&self, writer: &mut W) -> Result<()> {
        writeln!(writer, "利用可能なコマンド:")?;
        writeln!(writer, "  :help   このヘルプを表示")?;
        writeln!(writer, "  :show   現在のコードを行番号付きで表示")?;
        writeln!(writer, "  :edit   コード全体を入力し直し ( :end で終了 )")?;
        writeln!(
            writer,
            "  :append 既存コードの末尾に複数行を追加 ( :end で終了 )"
        )?;
        writeln!(writer, "  :reset  初期サンプルに戻す")?;
        writeln!(
            writer,
            "  :run    構文/型チェック後にJava生成とサンドボックス実行を試行"
        )?;
        writeln!(writer, "  :quit   エディタを終了してメニューへ戻る")?;
        writeln!(writer, "空行以外を直接入力すると1行として追加されます。")?;
        Ok(())
    }

    fn print_buffer<W: Write>(&self, writer: &mut W) -> Result<()> {
        if self.lines.is_empty() {
            writeln!(writer, "コードは空です。:edit で入力してください。")?;
            return Ok(());
        }

        writeln!(writer, "現在のコード:")?;
        for (index, line) in self.lines.iter().enumerate() {
            writeln!(writer, "{:>3}: {}", index + 1, line)?;
        }
        Ok(())
    }

    fn replace_buffer<R, W>(&mut self, reader: &mut R, writer: &mut W) -> Result<()>
    where
        R: BufRead,
        W: Write,
    {
        self.lines.clear();
        writeln!(writer, "新しいコードを入力してください ( :end で完了 )。")?;
        self.collect_block(reader, writer)?;
        writeln!(writer, "コードを更新しました。:run で検証できます。")?;
        Ok(())
    }

    fn append_block<R, W>(&mut self, reader: &mut R, writer: &mut W) -> Result<()>
    where
        R: BufRead,
        W: Write,
    {
        writeln!(writer, "追記するコードを入力してください ( :end で完了 )。")?;
        self.collect_block(reader, writer)?;
        writeln!(writer, "追記しました (現在 {} 行)。", self.lines.len())?;
        Ok(())
    }

    fn collect_block<R, W>(&mut self, reader: &mut R, writer: &mut W) -> Result<()>
    where
        R: BufRead,
        W: Write,
    {
        loop {
            write!(writer, "code> ")?;
            writer.flush().ok();

            let mut buffer = String::new();
            if reader.read_line(&mut buffer)? == 0 {
                break;
            }

            let trimmed = buffer.trim_end();
            if trimmed == ":end" {
                break;
            }

            self.lines.push(trimmed.to_string());
        }
        Ok(())
    }

    fn reset<W: Write>(&mut self, writer: &mut W) -> Result<()> {
        self.lines = DEFAULT_SNIPPET
            .lines()
            .map(|line| line.to_string())
            .collect();
        writeln!(writer, "初期サンプルにリセットしました。")?;
        Ok(())
    }

    fn run_pipeline<W: Write>(&self, writer: &mut W) -> Result<()> {
        let source = self.current_source();
        if source.trim().is_empty() {
            writeln!(
                writer,
                "コードが空のため検証できません。:edit で入力してください。"
            )?;
            return Ok(());
        }

        writeln!(writer, "\n🧪 構文チェックを実行中...")?;
        let pipeline = Parser2Pipeline::default();
        let frontend_output = match pipeline.parse(&source) {
            Ok(output) => {
                writeln!(writer, "✅ 構文チェックOK")?;
                output
            }
            Err(err) => {
                self.print_parse_error(writer, err, &source)?;
                return Ok(());
            }
        };
        let program = frontend_output.into_program();

        let mut checker = TypeChecker::new();
        if let Err(errors) = checker.check_program(&program) {
            writeln!(writer, "❌ 型チェックで問題が見つかりました:")?;
            for issue in errors {
                writeln!(writer, "  - {}", issue)?;
            }
            return Ok(());
        }
        writeln!(writer, "✅ 型チェックOK")?;

        let ir_program = match transform_program(program) {
            Ok(ir) => ir,
            Err(err) => {
                writeln!(writer, "❌ IR変換に失敗しました: {}", err)?;
                return Ok(());
            }
        };

        let java_source = match generate_java_source(&ir_program) {
            Ok(code) => code,
            Err(err) => {
                writeln!(writer, "❌ Javaコード生成に失敗しました: {}", err)?;
                return Ok(());
            }
        };

        writeln!(writer, "\n📝 生成されたJavaコード:")?;
        writeln!(writer, "```java")?;
        for line in java_source.lines() {
            writeln!(writer, "{}", line)?;
        }
        writeln!(writer, "```")?;

        writeln!(writer, "\n🚀 サンドボックス実行を試みます...")?;
        match run_in_sandbox(&java_source) {
            Ok(report) => {
                writeln!(writer, "✅ 実行完了 (exit code {:?})", report.exit_code)?;
                if !report.stdout.trim().is_empty() {
                    writeln!(writer, "stdout:\n{}", report.stdout.trim_end())?;
                }
                if !report.stderr.trim().is_empty() {
                    writeln!(writer, "stderr:\n{}", report.stderr.trim_end())?;
                }
            }
            Err(err) => {
                writeln!(writer, "⚠️ サンドボックス実行をスキップしました: {}", err)?;
            }
        }

        writeln!(
            writer,
            "\n完了しました。引き続き編集するか :quit で終了してください。"
        )?;
        Ok(())
    }

    fn print_parse_error<W: Write>(
        &self,
        writer: &mut W,
        error: ParseError,
        source: &str,
    ) -> Result<()> {
        writeln!(writer, "❌ 構文エラーが発生しました: {}", error)?;
        let span = error.span();
        if span != Span::dummy() {
            let (line_idx, column) = (span.start_line, span.start_column);
            if let Some(line) = source.lines().nth(line_idx.saturating_sub(1)) {
                writeln!(
                    writer,
                    "  → {}行目 {}列付近: {}",
                    line_idx,
                    column,
                    line.trim_end()
                )?;
            }
        }
        writeln!(writer, "コードを修正して再度 :run を実行してください。")?;
        Ok(())
    }

    fn current_source(&self) -> String {
        let mut joined = self.lines.join("\n");
        if !joined.ends_with('\n') {
            joined.push('\n');
        }
        joined
    }
}

#[derive(Debug)]
struct ExecutionReport {
    stdout: String,
    stderr: String,
    exit_code: Option<i32>,
}

fn run_in_sandbox(java_source: &str) -> Result<ExecutionReport> {
    if !is_java_source_safe(java_source) {
        return Err(anyhow!(
            "安全ではないAPIが含まれている可能性があるため実行を拒否しました"
        ));
    }

    let session_dir = create_sandbox_dir()?;
    let class_name = extract_entry_class_name(java_source).unwrap_or_else(|| "Main".to_string());
    let java_path = session_dir.join(format!("{}.java", class_name));
    fs::write(&java_path, java_source)?;

    let javac = std::process::Command::new("javac")
        .arg("-d")
        .arg(&session_dir)
        .arg(&java_path)
        .output();

    match javac {
        Ok(output) => {
            if !output.status.success() {
                let stderr = String::from_utf8_lossy(&output.stderr);
                fs::remove_dir_all(&session_dir).ok();
                return Err(anyhow!("javacの実行に失敗しました: {}", stderr.trim_end()));
            }
        }
        Err(err) => {
            fs::remove_dir_all(&session_dir).ok();
            return Err(anyhow!("javacが見つかりません: {}", err));
        }
    }

    let java = std::process::Command::new("java")
        .current_dir(&session_dir)
        .arg(&class_name)
        .output();

    let result = match java {
        Ok(output) => ExecutionReport {
            stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
            stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
            exit_code: output.status.code(),
        },
        Err(err) => {
            fs::remove_dir_all(&session_dir).ok();
            return Err(anyhow!("javaコマンドの起動に失敗しました: {}", err));
        }
    };

    fs::remove_dir_all(&session_dir).ok();
    Ok(result)
}

fn create_sandbox_dir() -> Result<PathBuf> {
    let base = std::env::temp_dir().join("jv-tour-sandbox");
    fs::create_dir_all(&base).with_context(|| {
        format!(
            "サンドボックスディレクトリの作成に失敗しました: {}",
            base.display()
        )
    })?;

    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    let dir = base.join(format!("session-{}", nanos));
    fs::create_dir_all(&dir).with_context(|| {
        format!(
            "セッションディレクトリの作成に失敗しました: {}",
            dir.display()
        )
    })?;
    Ok(dir)
}

fn extract_entry_class_name(java_source: &str) -> Option<String> {
    for line in java_source.lines() {
        let trimmed = line.trim();
        if let Some(rest) = trimmed.strip_prefix("public class ") {
            return rest
                .split_whitespace()
                .next()
                .map(|token| token.trim_end_matches('{').to_string());
        }
    }

    for line in java_source.lines() {
        let trimmed = line.trim();
        if let Some(index) = trimmed.find("class ") {
            let rest = &trimmed[index + 6..];
            return rest
                .split_whitespace()
                .next()
                .map(|token| token.trim_end_matches('{').to_string());
        }
    }

    None
}

fn is_java_source_safe(java_source: &str) -> bool {
    const FORBIDDEN_PATTERNS: &[&str] = &[
        "Runtime.getRuntime",
        "ProcessBuilder",
        "java.io.",
        "java.nio.file",
        "System.getenv",
        "System.setProperty",
        "Class.forName",
        "Files.",
    ];

    !FORBIDDEN_PATTERNS
        .iter()
        .any(|pattern| java_source.contains(pattern))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn detects_public_class_name() {
        let java = "public class Sample {\n    public static void main(String[] args) {}\n}";
        assert_eq!(extract_entry_class_name(java), Some("Sample".to_string()));
    }

    #[test]
    fn falls_back_when_no_class_found() {
        let java = "package demo;\nrecord Data(int x) {}";
        assert_eq!(extract_entry_class_name(java), None);
    }

    #[test]
    fn forbids_dangerous_constructs() {
        let java = "public class Bad { void run(){ Runtime.getRuntime().exec(\"rm\"); }}";
        assert!(!is_java_source_safe(java));
    }

    #[test]
    fn allows_simple_programs() {
        let java = "public class Main { public static void main(String[] args){ System.out.println(\"Hi\"); }}";
        assert!(is_java_source_safe(java));
    }
}
