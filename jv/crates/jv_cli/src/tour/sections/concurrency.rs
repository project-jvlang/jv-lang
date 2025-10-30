use std::io::Write;

use anyhow::{Result, anyhow};
use jv_parser_frontend::ParserPipeline;
use jv_parser_rowan::frontend::RowanPipeline;

/// Render the Concurrency learning module showcasing virtual threads.
pub fn render<W: Write>(writer: &mut W) -> Result<()> {
    writeln!(writer, "--- 並行処理セクション ---")?;
    writeln!(
        writer,
        "spawn ブロックと軽量スレッドを使って IO 待ちを並列化し、構造化された合流を学びます。"
    )?;
    writeln!(
        writer,
        "Java 25 では virtual thread API を用いて `Thread.ofVirtual().start` へ展開されます。"
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
    RowanPipeline::default()
        .parse(source)
        .map(|_| ())
        .map_err(|err| anyhow!("{:?}", err))
}

const LESSONS: [LessonExample; 3] = [
    LessonExample {
        title: "spawn で IO を並列化",
        description: "軽量スレッドで API 呼び出しをバックグラウンド実行し、結果を待機します。",
        takeaways: &[
            "`spawn {}` は JoinHandle を返し、`.join()` で結果を取得",
            "virtual thread によりスレッド数を気にせず並列化できる",
            "例外は join 時に伝播する",
        ],
        jv_code: r#"fun loadUser(): User {
    val profile = spawn { fetchProfile() }
    val history = spawn { fetchHistory() }
    return User(profile = profile.join(), history = history.join())
}"#,
        java_output: r#"public final class ConcurrencySamples {
    public static User loadUser() {
        var profile = java.lang.Thread.ofVirtual().start(ConcurrencySamples::fetchProfile);
        var history = java.lang.Thread.ofVirtual().start(ConcurrencySamples::fetchHistory);
        try {
            return new User(profile.join(), history.join());
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new RuntimeException(e);
        }
    }
}"#,
        validate: false,
    },
    LessonExample {
        title: "構造化並行性で複数タスクを管理",
        description: "spawnScope を用いてスコープ終了時にすべてのタスクが合流するよう保証します。",
        takeaways: &[
            "`spawnScope` 内で起動したタスクは暗黙的に join",
            "例外が発生すると残りのタスクはキャンセルされる",
            "Java では StructuredTaskScope で表現される",
        ],
        jv_code: r#"fun refreshDashboard(): Dashboard {
    return spawnScope { scope ->
        val news = scope.spawn { loadNews() }
        val metrics = scope.spawn { loadMetrics() }
        Dashboard(news = news.join(), metrics = metrics.join())
    }
}"#,
        java_output: r#"public final class ConcurrencySamples {
    public static Dashboard refreshDashboard() {
        try (var scope = new java.util.concurrent.StructuredTaskScope.ShutdownOnFailure()) {
            var news = scope.fork(ConcurrencySamples::loadNews);
            var metrics = scope.fork(ConcurrencySamples::loadMetrics);
            scope.join();
            scope.throwIfFailed();
            return new Dashboard(news.get(), metrics.get());
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new RuntimeException(e);
        }
    }
}"#,
        validate: false,
    },
    LessonExample {
        title: "スレッド安全な集計",
        description: "軽量スレッドとミューテックスで共有状態を安全に更新します。",
        takeaways: &[
            "`Mutex` で共有データを守る",
            "spawn したタスクから lock/unlock を自動処理",
            "Java では `ReentrantLock` や同期コレクションに展開",
        ],
        jv_code: r#"val counter = Mutex<Int>(0)

fun incrementMany(times: Int) {
    val workers = (0 until times).map {
        spawn {
            counter.withLock { value -> value + 1 }
        }
    }
    workers.forEach { it.join() }
}"#,
        java_output: r#"public final class ConcurrencySamples {
    private static final java.util.concurrent.locks.ReentrantLock COUNTER_LOCK =
        new java.util.concurrent.locks.ReentrantLock();
    private static int counter = 0;

    public static void incrementMany(int times) {
        java.util.List<java.lang.Thread> workers = new java.util.ArrayList<>();
        for (int i = 0; i < times; i++) {
            workers.add(java.lang.Thread.ofVirtual().start(() -> {
                COUNTER_LOCK.lock();
                try {
                    counter = counter + 1;
                } finally {
                    COUNTER_LOCK.unlock();
                }
            }));
        }
        for (var worker : workers) {
            try {
                worker.join();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                throw new RuntimeException(e);
            }
        }
    }
}"#,
        validate: false,
    },
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn virtual_thread_is_highlighted() {
        assert!(LESSONS[0].java_output.contains("Thread.ofVirtual().start"));
    }

    #[test]
    fn structured_scope_example_mentions_scope() {
        assert!(LESSONS[1].java_output.contains("StructuredTaskScope"));
    }
}
