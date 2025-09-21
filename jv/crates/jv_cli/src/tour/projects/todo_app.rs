use super::{add_feature, Feature, MiniProject, ProjectStep};
use crate::tour::cli::SectionId;

pub fn create_project() -> MiniProject {
    let mut project = MiniProject::new(
        "ToDoアプリ",
        "todo-app",
        "日々のタスクを整理し、完了状況を永続化できるCLIアプリケーションです。",
        "mini.todo.Main",
        "tour/projects/todo-app/src/main.jv",
        &[
            SectionId::BasicSyntax,
            SectionId::DataClasses,
            SectionId::ControlFlow,
            SectionId::Functions,
            SectionId::BuildTools,
        ],
    );

    for feature in TODO_FEATURES {
        add_feature(&mut project, feature);
    }

    project.steps.extend_from_slice(&TODO_STEPS);
    project
}

const TODO_FEATURES: [Feature; 5] = [
    Feature {
        name: "データクラスによるタスクモデル",
        description:
            "学んだデータクラス構文で immutability を保ちながらrecord/Java classを自動生成します。",
        requirements: &["8.1"],
        section_refs: &[SectionId::DataClasses, SectionId::BasicSyntax],
        code_highlight: r#"data class Task(
    val id: Int,
    val title: String,
    val completed: Boolean = false,
)
"#,
    },
    Feature {
        name: "関数型ヘルパーによるステート操作",
        description: "Section Functions の学習内容を活かし、純粋関数でタスク更新を扱います。",
        requirements: &["8.2"],
        section_refs: &[SectionId::Functions, SectionId::ControlFlow],
        code_highlight: r#"fun toggle(task: Task): Task =
    task.copy(completed = !task.completed)
"#,
    },
    Feature {
        name: "when式メニューでのコマンド分岐",
        description:
            "制御フローセクションの when 式をメニュー選択に適用し、読みやすい分岐を実現します。",
        requirements: &["8.2"],
        section_refs: &[SectionId::ControlFlow],
        code_highlight: r#"when (command) {
    "add" -> addTask(tasks)
    "done" -> markDone(tasks)
    else -> println("Unknown command: ${command}")
}
"#,
    },
    Feature {
        name: "学習進捗セクションと連携した永続化",
        description: "Progress Tracker の仕組みを流用し、タスク一覧をJSONファイルに保存します。",
        requirements: &["8.3", "8.4"],
        section_refs: &[SectionId::BuildTools],
        code_highlight: r#"import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path

fun save(tasks: List<Task>) {
    val lines = tasks.map { task ->
        "${task.id},${task.title},${task.completed}"
    }
    Files.createDirectories(Path.of("data"))
    Files.write(Path.of("data/todo.csv"), lines, StandardCharsets.UTF_8)
}
"#,
    },
    Feature {
        name: "jv build と連携したJar出力",
        description: "ビルドツールセクションの設定を踏襲し、完成後は実行可能Jarを生成します。",
        requirements: &["8.5"],
        section_refs: &[SectionId::BuildTools],
        code_highlight: r#"# jv build --binary jar --bin-name todo-app
"#,
    },
];

const TODO_STEPS: [ProjectStep; 4] = [
    ProjectStep {
        title: "プロジェクトの初期化",
        goal: "ひな形と jv.toml を準備し、タスクデータ用のディレクトリを作成する",
        walkthrough: &["jv init todo-app を実行し、src/main.jv をベースに構造化する", "data/ ディレクトリを作成し永続化ファイルを配置"],
        code: Some(r#"fun main() {
    println("=== ToDo CLI ===")
    println("1) タスク追加, 2) 完了, 3) 一覧")
}
"#),
        verification: &["jv run tour/projects/todo-app/src/main.jv"],
    },
    ProjectStep {
        title: "タスクデータモデルの実装",
        goal: "Task データクラスと一覧操作関数を追加する",
        walkthrough: &["Task data class を定義し、ID自動採番ロジックを追加", "List<Task> に対する add/remove/toggle 関数を作成"],
        code: Some(r#"data class Task(
    val id: Int,
    val title: String,
    val completed: Boolean = false,
)

fun add(tasks: List<Task>, title: String): List<Task> {
    val nextId = tasks.maxOfOrNull { it.id }?.plus(1) ?: 1
    return tasks + Task(nextId, title)
}
"#),
        verification: &["jv run tour/projects/todo-app/src/main.jv -- add 'Write spec'"],
    },
    ProjectStep {
        title: "メニュー駆動のCLI実装",
        goal: "when式でコマンドを分岐し、状態を更新する",
        walkthrough: &["標準入力からコマンドを読み取り when 式で処理する", "完了済みタスクには ✅ を付けて表示"],
        code: Some(r#"fun handleCommand(tasks: List<Task>, input: String): List<Task> = when {
    input.startsWith("add ") -> add(tasks, input.removePrefix("add "))
    input.startsWith("done ") -> markDone(tasks, input.removePrefix("done ").toInt())
    input == "list" -> {
        tasks.forEach { task ->
            val icon = if (task.completed) "✅" else "⭕"
            println("${task.id}. ${icon} ${task.title}")
        }
        tasks
    }
    else -> {
        println("Unknown command: ${input}")
        tasks
    }
}
"#),
        verification: &["printf 'list\n' | jv run tour/projects/todo-app/src/main.jv"],
    },
    ProjectStep {
        title: "永続化とJar出力の準備",
        goal: "ファイル保存とロードを実装し、ビルド設定を確認する",
        walkthrough: &["Files.write と Files.readAllLines を利用してCSVファイルを永続化", "jv build で classes ディレクトリを検証"],
        code: Some(r#"import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path

fun load(): List<Task> = Files.readAllLines(Path.of("data/todo.csv"), StandardCharsets.UTF_8)
    .filter { it.isNotBlank() }
    .map { line ->
        val parts = line.split(",")
        Task(parts[0].toInt(), parts[1], parts[2].toBoolean())
    }
"#),
        verification: &["jv build --input tour/projects/todo-app/src/main.jv --output target/jv/todo-app/java --binary jar --bin-name todo-app"],
    },
];
