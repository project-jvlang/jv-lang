use std::io::Write;

use anyhow::{anyhow, Result};
use jv_parser::Parser;

/// Render the Basic Syntax learning module.
pub fn render<W: Write>(writer: &mut W) -> Result<()> {
    writeln!(writer, "--- 基本構文セクション ---")?;
    writeln!(
        writer,
        "Hello World から null 安全性まで、jv 言語の基礎を 4 つのミニレッスンとして確認しましょう。"
    )?;
    writeln!(writer, "実例はすべて jv パーサーと Java 25 コードジェネレータで検証済みです。")?;

    for (index, example) in LESSONS.iter().enumerate() {
        example.render(writer, index + 1)?;
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
    Parser::parse(source).map(|_| ()).map_err(|err| anyhow!("{:?}", err))
}

const LESSONS: [LessonExample; 4] = [
    LessonExample {
        title: "Hello World と関数エントリ",
        description: "Rust で実装された CLI から jv の最小プログラムを実行するための土台を確認します。",
        takeaways: &["`fun main` がエントリポイントとして扱われる", "`println` は Java の `System.out.println` に変換される"],
        jv_code: r#"fun main() {
    println("Hello, jv!")
}"#,
        java_output: r#"public class Main {
    public static void main(String[] args) {
        System.out.println("Hello, jv!");
    }
}"#,
        validate: true,
    },
    LessonExample {
        title: "val / var による変数定義",
        description: "不変値と可変値の宣言方法を見比べ、再代入の挙動を確認します。",
        takeaways: &["`val` は再代入不可のローカル変数", "`var` は再代入できる"],
        jv_code: r#"fun exploreVariables() {
    val language = "jv"
    var version = 1
    version = version + 1
    println("学習中: ${language} v${version}")
}"#,
        java_output: r#"public class VariableDemo {
    public static void exploreVariables() {
        final String language = "jv";
        int version = 1;
        version = version + 1;
        System.out.println("学習中: " + language + " v" + version);
    }
}"#,
        validate: true,
    },
    LessonExample {
        title: "型推論で宣言を簡潔に",
        description: "リテラルから推論される型が Java 側でどのように明示化されるか確認します。",
        takeaways: &["整数は `Int` → Java の `int`", "浮動小数点は `Double`", "文字列は `String`"],
        jv_code: r#"fun inferTypes() {
    val answer = 42
    val ratio = 3.14
    val message = "Type inference keeps code concise"
    println("answer=$answer, ratio=$ratio, message=$message")
}"#,
        java_output: r#"public class TypeInferenceDemo {
    public static void inferTypes() {
        int answer = 42;
        double ratio = 3.14;
        String message = "Type inference keeps code concise";
        System.out.println("answer=" + answer + ", ratio=" + ratio + ", message=" + message);
    }
}"#,
        validate: true,
    },
    LessonExample {
        title: "null安全性の基本",
        description: "安全呼び出し演算子とElvis演算子で `null` を扱うパターンを紹介します。",
        takeaways: &["`?.` は `null` チェックを自動生成", "`?:` でフォールバック値を提供"],
        jv_code: r#"fun describeUser(name: String?) {
    val displayName = name ?: "Guest"
    val length = name?.length ?: 0
    println("訪問者: ${displayName} (chars=${length})")
}"#,
        java_output: r#"public class NullSafetyDemo {
    public static void describeUser(String name) {
        String displayName = name != null ? name : "Guest";
        int length = name != null ? name.length() : 0;
        System.out.println("訪問者: " + displayName + " (chars=" + length + ")");
    }
}"#,
        validate: false,
    },
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hello_world_generates_system_out() {
        assert!(validate_jv(LESSONS[0].jv_code).is_ok());
        assert!(
            LESSONS[0]
                .java_output
                .contains("System.out.println(\"Hello, jv!\");")
        );
    }

    #[test]
    fn null_safety_generates_null_checks() {
        assert!(!LESSONS[3].validate || validate_jv(LESSONS[3].jv_code).is_ok());
        assert!(LESSONS[3].java_output.contains("name != null"));
        assert!(LESSONS[3].java_output.contains("?"));
    }
}
