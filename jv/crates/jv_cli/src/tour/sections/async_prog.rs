use std::io::Write;

use anyhow::{Result, anyhow};
use jv_parser_frontend::{Parser2Pipeline, ParserPipeline};

/// Render the Async programming module bridging async/await to Java futures.
pub fn render<W: Write>(writer: &mut W) -> Result<()> {
    writeln!(writer, "--- 非同期処理セクション ---")?;
    writeln!(
        writer,
        "async ブロックと await で IO をノンブロッキング化し、CompletableFuture との連携を学びます。"
    )?;
    writeln!(
        writer,
        "生成される Java コードは `CompletableFuture` と `StructuredTaskScope` を活用します。"
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
    Parser2Pipeline::default()
        .parse(source)
        .map(|_| ())
        .map_err(|err| anyhow!("{:?}", err))
}

const LESSONS: [LessonExample; 3] = [
    LessonExample {
        title: "async 関数の基本",
        description: "async 関数を宣言し、await で結果を受け取る最小構成です。",
        takeaways: &[
            "`async fun` は暗黙的に Future を返す",
            "`await()` で非同期結果を取得",
            "Java では CompletableFuture チェーンに展開",
        ],
        jv_code: r#"async fun fetchProfile(): Profile {
    val raw = httpGet("/api/profile")
    return Profile.fromJson(raw)
}

suspend fun showProfile() {
    val profile = fetchProfile().await()
    println("Hello ${profile.name}")
}"#,
        java_output: r#"public final class AsyncSamples {
    public static java.util.concurrent.CompletableFuture<Profile> fetchProfile() {
        return java.util.concurrent.CompletableFuture.supplyAsync(() ->
            Profile.fromJson(httpGet("/api/profile"))
        );
    }

    public static void showProfile() {
        fetchProfile()
            .thenAccept(profile -> System.out.println("Hello " + profile.name()));
    }
}"#,
        validate: false,
    },
    LessonExample {
        title: "awaitAll で同時実行",
        description: "複数の async タスクを同時に起動し、すべて完了してから結果を統合します。",
        takeaways: &[
            "`async {}` ブロックは遅延評価の Future を生成",
            "`awaitAll` で複数結果をまとめて取得",
            "Java では `CompletableFuture.allOf` を使用",
        ],
        jv_code: r#"suspend fun loadDashboard(): Dashboard {
    val summary = async { fetchSummary() }
    val chart = async { fetchChart() }
    val alerts = async { fetchAlerts() }
    awaitAll(summary, chart, alerts)
    return Dashboard(summary.await(), chart.await(), alerts.await())
}"#,
        java_output: r#"public final class AsyncSamples {
    public static java.util.concurrent.CompletableFuture<Dashboard> loadDashboard() {
        var summary = java.util.concurrent.CompletableFuture.supplyAsync(AsyncSamples::fetchSummary);
        var chart = java.util.concurrent.CompletableFuture.supplyAsync(AsyncSamples::fetchChart);
        var alerts = java.util.concurrent.CompletableFuture.supplyAsync(AsyncSamples::fetchAlerts);
        return java.util.concurrent.CompletableFuture.allOf(summary, chart, alerts)
            .thenApply(ignored -> new Dashboard(summary.join(), chart.join(), alerts.join()));
    }
}"#,
        validate: false,
    },
    LessonExample {
        title: "Java の Future と橋渡し",
        description: "既存の CompletableFuture を jv の async/await と相互運用します。",
        takeaways: &[
            "`future.await()` で Java 側の Future から値を取得",
            "`future {}` で Java コードへ Future を公開",
            "キャンセルは相互に伝播する",
        ],
        jv_code: r#"suspend fun syncLegacy(): LegacyData {
    val future = legacyApiCall()
    return future.await()
}

fun exportFuture(block: suspend () -> String): CompletableFuture<String> {
    return future { block() }
}"#,
        java_output: r#"public final class AsyncSamples {
    public static LegacyData syncLegacy() throws Exception {
        java.util.concurrent.CompletableFuture<LegacyData> future = legacyApiCall();
        return future.get();
    }

    public static java.util.concurrent.CompletableFuture<String> exportFuture(
            java.util.concurrent.Callable<String> block) {
        return java.util.concurrent.CompletableFuture.supplyAsync(() -> {
            try {
                return block.call();
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        });
    }
}"#,
        validate: false,
    },
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn async_examples_reference_completable_future() {
        for lesson in LESSONS.iter() {
            assert!(lesson.java_output.contains("CompletableFuture"));
        }
    }
}
