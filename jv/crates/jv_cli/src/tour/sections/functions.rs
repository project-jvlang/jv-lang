use std::io::Write;

use anyhow::{anyhow, Result};
use jv_parser::Parser;

/// Render the Functions learning module covering defaults and extensions.
pub fn render<W: Write>(writer: &mut W) -> Result<()> {
    writeln!(writer, "--- 関数セクション ---")?;
    writeln!(
        writer,
        "デフォルト引数と名前付き引数、拡張関数、関数型を用いた再利用パターンを確認します。"
    )?;
    writeln!(
        writer,
        "例は Java 25 でのメソッドオーバーロードやユーティリティクラスに変換される想定です。"
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
        title: "デフォルト引数と柔軟な呼び出し",
        description: "引数にデフォルト値を指定し、最小限の指定で関数を呼び出します。",
        takeaways: &[
            "デフォルト値は呼び出し時に自動で補われる",
            "必要な値だけを順序通りに渡せば振る舞いを上書きできる",
            "Java ではオーバーロードとビルダー的ヘルパーに展開される",
        ],
        jv_code: r#"fun greet(name: String, prefix: String = "Hello", punctuation: String = "!") {
    println("${prefix}, ${name}${punctuation}")
}

fun showcase() {
    greet("Kira")
    greet("Alex" "Welcome" "!!")
}"#,
        java_output: r#"public final class FunctionDefaults {
    public static void greet(String name, String prefix, String punctuation) {
        System.out.println(prefix + ", " + name + punctuation);
    }

    public static void greet(String name) {
        greet(name, "Hello", "!");
    }

    public static void greet(String name, String prefix) {
        greet(name, prefix, "!");
    }
}"#,
        validate: true,
    },
    LessonExample {
        title: "拡張関数で文字列整形",
        description: "既存型に拡張関数を定義して、コードの可読性を高めます。",
        takeaways: &[
            "`fun Type.method()` で拡張関数を宣言",
            "Java ではユーティリティクラスの静的メソッドに変換",
            "`this` はレシーバー引数として渡される",
        ],
        jv_code: r#"fun String.titleCase(): String {
    return this.split(" ")
        .map { word -> word.lowercase().replaceFirstChar { it.titlecase() } }
        .joinToString(" ")
}

fun formatHeadline() {
    println("learning jv functions".titleCase())
}"#,
        java_output: r#"public final class StringExtensions {
    public static String titleCase(String receiver) {
        return java.util.Arrays.stream(receiver.split(" "))
            .map(word -> {
                String lower = word.toLowerCase();
                return lower.substring(0, 1).toUpperCase() + lower.substring(1);
            })
            .collect(java.util.stream.Collectors.joining(" "));
    }
}"#,
        validate: false,
    },
    LessonExample {
        title: "関数型とラムダの活用",
        description: "関数を引数に取り、共通処理を抽象化する高階関数の例です。",
        takeaways: &[
            "ラムダ式は `() -> Unit` のような型として扱える",
            "デフォルト引数で後処理を任意化できる",
            "Java では `java.util.function` を利用する",
        ],
        jv_code: r#"fun measure(label: String, block: () -> Unit, finallyDo: () -> Unit = {}) {
    val start = System.currentTimeMillis()
    block()
    val elapsed = System.currentTimeMillis() - start
    println("${label} took ${elapsed}ms")
    finallyDo()
}

fun demo() {
    measure("prepare") {
        println("Loading resources...")
    }
}"#,
        java_output: r#"public final class FunctionLambdas {
    public static void measure(String label, Runnable block, Runnable finallyDo) {
        long start = System.currentTimeMillis();
        block.run();
        long elapsed = System.currentTimeMillis() - start;
        System.out.println(label + " took " + elapsed + "ms");
        finallyDo.run();
    }

    public static void measure(String label, Runnable block) {
        measure(label, block, () -> {});
    }
}"#,
        validate: false,
    },
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_arguments_parse() {
        assert!(validate_jv(LESSONS[0].jv_code).is_ok());
        assert!(LESSONS[0].java_output.contains("greet"));
    }

    #[test]
    fn extension_function_mentions_utilities() {
        assert!(LESSONS[1]
            .java_output
            .contains("public final class StringExtensions"));
    }
}
