use std::io::Write;

use anyhow::{anyhow, Result};
use jv_parser::Parser;

/// Render the Control Flow learning module with `when` expression examples.
pub fn render<W: Write>(writer: &mut W) -> Result<()> {
    writeln!(writer, "--- 制御フローセクション ---")?;
    writeln!(
        writer,
        "when式とパターンマッチングを活用して、条件分岐を安全かつ表現力豊かに記述する方法を学びます。"
    )?;
    writeln!(
        writer,
        "すべての例は Java 25 の switch 式やパターンに対応するコードへ変換される想定です。"
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
        title: "when式で整数スコアを分類",
        description: "switch 分岐よりも宣言的に評価値を返す when 式の基本パターンです。",
        takeaways: &[
            "when 式は値を返すためローカル変数初期化にも使える",
            "複数の case にマッチしない場合は else を必ず用意する",
        ],
        jv_code: r#"fun describeScore(score: Int): String {
    return when (score) {
        100 -> "満点!"
        90 -> "とても良い"
        80 -> "あと少し"
        else -> "継続して学習しましょう"
    }
}"#,
        java_output: r#"public final class ControlFlowExamples {
    public static String describeScore(int score) {
        return switch (score) {
            case 100 -> "満点!";
            case 90 -> "とても良い";
            case 80 -> "あと少し";
            default -> "継続して学習しましょう";
        };
    }
}"#,
        validate: true,
    },
    LessonExample {
        title: "パターンマッチでレスポンスを解析",
        description:
            "データクラスと is パターンを組み合わせて成功・失敗レスポンスを安全に分類します。",
        takeaways: &[
            "`is Type` パターンで型ごとの分岐が書ける",
            "ガード条件 `if` を併用して詳細な条件を表現できる",
            "Java 側ではパターンマッチ付き switch へ変換される",
        ],
        jv_code: r#"data class Success(val data: String)
data class Error(val code: Int, val message: String)

type Response = Success | Error

fun render(response: Response): String {
    return when (response) {
        is Success -> "成功: ${response.data}"
        Error(code, message) if code >= 500 -> "致命的エラー(${code}): ${message}"
        Error(_, message) -> "エラー: ${message}"
    }
}"#,
        java_output: r#"sealed interface Response {}

record Success(String data) implements Response {}
record Error(int code, String message) implements Response {}

public final class ControlFlowExamples {
    public static String render(Response response) {
        return switch (response) {
            case Success success -> "成功: " + success.data();
            case Error error && error.code() >= 500 ->
                "致命的エラー(" + error.code() + "): " + error.message();
            case Error error -> "エラー: " + error.message();
        };
    }
}"#,
        validate: false,
    },
    LessonExample {
        title: "トークン抽出とガード条件",
        description: "コンストラクタパターンとガードで抽象構文木ノードを安全に展開します。",
        takeaways: &[
            "コンストラクタパターン `Number(value)` でフィールドを直接束縛できる",
            "`_` ワイルドカードで不要な値を無視可能",
            "Java 25 では record パターンと when ガードとして表現される",
        ],
        jv_code: r#"data class NumberToken(val value: Int)
data class IdentifierToken(val name: String)
data class KeywordToken(val lexeme: String)

type Token = NumberToken | IdentifierToken | KeywordToken

fun explain(token: Token): String {
    return when (token) {
        NumberToken(value) if value >= 0 -> "正の数 ${value}"
        NumberToken(value) -> "負の数 ${value}"
        IdentifierToken(name) -> "識別子 ${name}"
        _ -> "予約語 ${token}"
    }
}"#,
        java_output: r#"sealed interface Token {}

record NumberToken(int value) implements Token {}
record IdentifierToken(String name) implements Token {}
record KeywordToken(String lexeme) implements Token {}

public final class ControlFlowExamples {
    public static String explain(Token token) {
        return switch (token) {
            case NumberToken(var value) when value >= 0 -> "正の数 " + value;
            case NumberToken(var value) -> "負の数 " + value;
            case IdentifierToken(var name) -> "識別子 " + name;
            default -> "予約語 " + token;
        };
    }
}"#,
        validate: false,
    },
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn validates_simple_when_example() {
        assert!(LESSONS[0].validate);
        assert!(validate_jv(LESSONS[0].jv_code).is_ok());
    }

    #[test]
    fn java_output_mentions_switch_patterns() {
        assert!(LESSONS[1].java_output.contains("switch (response)"));
        assert!(LESSONS[2].java_output.contains("case NumberToken"));
    }
}
