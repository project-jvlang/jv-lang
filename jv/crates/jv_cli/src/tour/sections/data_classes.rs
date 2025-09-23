use std::io::Write;

use anyhow::{anyhow, Result};
use jv_parser::Parser;

/// Render the Data Classes learning module.
pub fn render<W: Write>(writer: &mut W) -> Result<()> {
    writeln!(writer, "--- データクラスセクション ---")?;
    writeln!(
        writer,
        "jv の data class を活用してイミュータブルなモデルや mutable レコードを表現する方法を学びます。"
    )?;
    writeln!(
        writer,
        "Java 25 では record やクラスに変換され、equals/hashCode/toString が自動的に生成されます。"
    )?;

    for (index, lesson) in LESSONS.iter().enumerate() {
        lesson.render(writer, index + 1)?;
    }

    Ok(())
}

struct LessonExample {
    title: &'static str,
    description: &'static str,
    takeaways: &'static [&'static str],
    jv_code: &'static str,
    java_output: &'static str,
    validate: bool,
}

impl LessonExample {
    fn render<W: Write>(&self, writer: &mut W, index: usize) -> Result<()> {
        writeln!(writer, "\n[{}] {}", index, self.title)?;
        writeln!(writer, "{}", self.description)?;
        for takeaway in self.takeaways {
            writeln!(writer, "  - {}", takeaway)?;
        }

        writeln!(writer, "\njvコード:")?;
        writeln!(writer, "```jv")?;
        writeln!(writer, "{}", self.jv_code.trim())?;
        writeln!(writer, "```")?;

        if self.validate {
            validate_jv(self.jv_code)
                .map_err(|err| anyhow!("{} のパースに失敗しました: {}", self.title, err))?;
        }

        writeln!(writer, "Java出力:")?;
        writeln!(writer, "```java")?;
        writeln!(writer, "{}", self.java_output.trim())?;
        writeln!(writer, "```")?;

        Ok(())
    }
}

fn validate_jv(source: &str) -> Result<()> {
    Parser::parse(source)
        .map(|_| ())
        .map_err(|err| anyhow!("{:?}", err))
}

const LESSONS: [LessonExample; 3] = [
    LessonExample {
        title: "イミュータブルなユーザーモデル",
        description: "val プロパティのみを持つ data class は Java の record に変換されます。",
        takeaways: &[
            "`data class` はプロパティを宣言するだけで自動的にアクセサを生成",
            "デフォルトの `copy` 関数で部分的な差分を作成できる",
            "Java 側では `record` になるため equals/hashCode/toString が提供される",
        ],
        jv_code: r#"data class UserProfile(val name: String, val level: Int = 1)

fun promote(user: UserProfile): UserProfile {
    return user.copy(user.name, user.level + 1)
}"#,
        java_output: r#"public record UserProfile(String name, int level) {
    public UserProfile {
        if (level < 1) {
            throw new IllegalArgumentException("level must be >= 1");
        }
    }

    public UserProfile promote() {
        return new UserProfile(this.name, this.level + 1);
    }
}
"#,
        validate: false,
    },
    LessonExample {
        title: "可変プロパティを持つ設定クラス",
        description: "var プロパティを含む data class は通常の Java クラスとして生成され、ミューテーションを許可します。",
        takeaways: &[
            "var プロパティは Java のフィールドとして生成され setter/getter が用意される",
            "CLI の設定など変更が必要なモデルに適する",
        ],
        jv_code: r#"data class CliSettings(var theme: String, var retries: Int)

fun enableDarkMode(settings: CliSettings) {
    settings.theme = "dark"
    settings.retries = settings.retries + 1
}"#,
        java_output: r#"public final class CliSettings {
    private String theme;
    private int retries;

    public CliSettings(String theme, int retries) {
        this.theme = theme;
        this.retries = retries;
    }

    public String getTheme() {
        return theme;
    }

    public void setTheme(String theme) {
        this.theme = theme;
    }

    public int getRetries() {
        return retries;
    }

    public void setRetries(int retries) {
        this.retries = retries;
    }

    @Override
    public String toString() {
        return "CliSettings(theme=" + theme + ", retries=" + retries + ")";
    }
}
"#,
        validate: true,
    },
    LessonExample {
        title: "ネストしたデータクラスとシリアライズ",
        description: "複合的なモデルも data class で表現でき、後続タスクで serde と連携して永続化します。",
        takeaways: &[
            "ネストした data class でツリー構造を安全に表現",
            "Java では入れ子の record/クラスとして出力される",
            "今後のタスクで JSON シリアライズに利用予定",
        ],
        jv_code: r#"data class Lesson(val id: Int, val title: String)
data class SectionProgress(val section: String, val completedLessons: List<Lesson>)

type ProgressSnapshot = List<SectionProgress>
"#,
        java_output: r#"public record Lesson(int id, String title) {}

public record SectionProgress(String section, java.util.List<Lesson> completedLessons) {}

public interface ProgressSnapshot extends java.util.List<SectionProgress> {}
"#,
        validate: false,
    },
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn validates_data_class_samples() {
        for lesson in &LESSONS {
            if lesson.validate {
                assert!(validate_jv(lesson.jv_code).is_ok());
            }
        }
    }

    #[test]
    fn java_output_mentions_record_generation() {
        assert!(LESSONS[0].java_output.contains("record UserProfile"));
        assert!(LESSONS[1].java_output.contains("final class CliSettings"));
    }
}
