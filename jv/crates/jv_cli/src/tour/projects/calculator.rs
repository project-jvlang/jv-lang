use super::{Feature, MiniProject, ProjectStep, add_feature};
use crate::tour::cli::SectionId;

pub fn create_project() -> MiniProject {
    let mut project = MiniProject::new(
        "電卓",
        "calculator",
        "基本演算から関数合成、入力検証までを体験する対話型電卓です。",
        "mini.calc.Main",
        "tour/projects/calculator/src/main.jv",
        &[
            SectionId::BasicSyntax,
            SectionId::Functions,
            SectionId::ControlFlow,
        ],
    );

    for feature in CALC_FEATURES {
        add_feature(&mut project, feature);
    }

    project.steps.extend_from_slice(&CALC_STEPS);
    project
}

const CALC_FEATURES: [Feature; 4] = [
    Feature {
        name: "関数型の演算テーブル",
        description: "学んだ高階関数パターンを用いて演算子文字列から関数を解決します。",
        requirements: &["8.1", "8.2"],
        section_refs: &[SectionId::Functions],
        code_highlight: r#"val operations = mapOf(
    "+" to { lhs: Double, rhs: Double -> lhs + rhs },
    "-" to { lhs: Double, rhs: Double -> lhs - rhs },
    "*" to { lhs: Double, rhs: Double -> lhs * rhs },
    "/" to { lhs: Double, rhs: Double -> lhs / rhs },
)
"#,
    },
    Feature {
        name: "when式による入力ディスパッチ",
        description: "制御フローセクションで扱った when 式を活用し、コマンドを柔軟に処理します。",
        requirements: &["8.2"],
        section_refs: &[SectionId::ControlFlow],
        code_highlight: r#"when (command) {
    "quit" -> exitProcess(0)
    "history" -> showHistory()
    else -> evaluate(command)
}
"#,
    },
    Feature {
        name: "バリデーションとエラーメッセージ",
        description: "入力チェックと例外ハンドリングでユーザーに明確なフィードバックを提供します。",
        requirements: &["8.3"],
        section_refs: &[SectionId::BasicSyntax],
        code_highlight: r#"if (rhs == 0.0 && op == "/") {
    println("⚠️ 0 で割ることはできません")
    return null
}
"#,
    },
    Feature {
        name: "バッチ評価レポート",
        description: "複数計算をまとめて実行し、結果を要約表示するための関数合成を実装します。",
        requirements: &["8.4", "8.5"],
        section_refs: &[SectionId::Functions],
        code_highlight: r#"fun evaluateBatch(lines: List<String>): List<Double> =
    lines.mapNotNull { parse(it)?.let { cmd -> cmd.operation(cmd.lhs, cmd.rhs) } }
"#,
    },
];

const CALC_STEPS: [ProjectStep; 3] = [
    ProjectStep {
        title: "演算テーブルと入力パース",
        goal: "演算子マップとコマンド分解ロジックを定義する",
        walkthrough: &[
            "operations マップを作成し、未知の演算子には警告を出す",
            "split を用いて '3 + 4' 形式の入力を分解する",
        ],
        code: Some(
            r#"data class ParsedCommand(
    val lhs: Double,
    val rhs: Double,
    val operation: (Double, Double) -> Double,
)

fun parse(command: String): ParsedCommand? {
    val parts = command.trim().split(" ")
    if (parts.size != 3) {
        println("形式: <左辺> <演算子> <右辺>")
        return null
    }
    val op = parts[1]
    val operation = operations[op]
    if (operation == null) {
        println("未対応の演算子です: ${op}")
        return null
    }
    return ParsedCommand(parts[0].toDouble(), parts[2].toDouble(), operation)
}
"#,
        ),
        verification: &["printf '3 + 4\n' | jv run tour/projects/calculator/src/main.jv"],
    },
    ProjectStep {
        title: "対話ループと履歴管理",
        goal: "while ループでCLIを構築し、過去の計算を保存する",
        walkthrough: &[
            "mutableList に計算履歴を追加し、history コマンドで表示",
            "quit コマンドでアプリを終了",
        ],
        code: Some(
            r#"fun repl() {
    val history = mutableListOf<String>()
    while (true) {
        print("> ")
        val line = readLine()
        if (line == null || line == "quit") return
        if (line == "history") {
            history.forEach(::println)
            continue
        }
        val parsed = parse(line)
        val result = parsed?.let { it.operation(it.lhs, it.rhs) }
        if (result != null) {
            val summary = "${line} = ${result}"
            println(summary)
            history.add(summary)
        }
    }
}
"#,
        ),
        verification: &["jv run tour/projects/calculator/src/main.jv"],
    },
    ProjectStep {
        title: "バッチ処理とJar化",
        goal: "複数計算をファイルから読み込み、結果をまとめて出力する",
        walkthrough: &[
            "Files.readAllLines を使用して入力バッチを読み込む",
            "jv build --binary jar で成果物を生成",
        ],
        code: Some(
            r#"fun batch(path: String) {
    val lines = java.nio.file.Files.readAllLines(java.nio.file.Path.of(path))
    val results = evaluateBatch(lines) // List<Double>
    results.forEachIndexed { index, value ->
        println("${index + 1}: ${value}")
    }
}
"#,
        ),
        verification: &[
            "jv build --input tour/projects/calculator/src/main.jv --output target/jv/calculator/java --binary jar --bin-name calculator",
        ],
    },
];
