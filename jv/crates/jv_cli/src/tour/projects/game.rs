use super::{Feature, MiniProject, ProjectStep, add_feature};
use crate::tour::cli::SectionId;

pub fn create_project() -> MiniProject {
    let mut project = MiniProject::new(
        "じゃんけんゲーム",
        "rps-game",
        "乱数で手を選ぶコンピューターと対戦し、スコアを記録する学習用ゲームです。",
        "mini.game.Main",
        "tour/projects/rps-game/src/main.jv",
        &[
            SectionId::BasicSyntax,
            SectionId::ControlFlow,
            SectionId::DataClasses,
            SectionId::InteractiveEditor,
        ],
    );

    for feature in GAME_FEATURES {
        add_feature(&mut project, feature);
    }

    project.steps.extend_from_slice(&GAME_STEPS);
    project
}

const GAME_FEATURES: [Feature; 4] = [
    Feature {
        name: "列挙型による手の表現",
        description: "基本構文セクションを応用し、Rock/Paper/Scissors を安全に列挙します。",
        requirements: &["8.1"],
        section_refs: &[SectionId::BasicSyntax, SectionId::DataClasses],
        code_highlight: r#"enum class Move { ROCK, PAPER, SCISSORS }
"#,
    },
    Feature {
        name: "when式とタプルパターンで勝敗判定",
        description: "制御フローのパターンマッチを使い、読みやすい勝敗ロジックを実装します。",
        requirements: &["8.2"],
        section_refs: &[SectionId::ControlFlow],
        code_highlight: r#"fun judge(player: Move, cpu: Move): Outcome = when (player to cpu) {
    Move.ROCK to Move.SCISSORS -> Outcome.Player
    Move.PAPER to Move.ROCK -> Outcome.Player
    Move.SCISSORS to Move.PAPER -> Outcome.Player
    cpu to player -> Outcome.Cpu
    else -> Outcome.Draw
}
"#,
    },
    Feature {
        name: "ランダム選択とUI演出",
        description: "Java標準ライブラリを利用して CPU の手を生成し、インタラクティブ表示を整えます。",
        requirements: &["8.3"],
        section_refs: &[SectionId::InteractiveEditor],
        code_highlight: r#"val random = java.util.Random()
fun cpuMove(): Move = Move.values()[random.nextInt(3)]
"#,
    },
    Feature {
        name: "スコアボードとJar出力",
        description: "勝敗結果をデータクラスで集計し、最終的にJar化して配布します。",
        requirements: &["8.4", "8.5"],
        section_refs: &[SectionId::DataClasses, SectionId::BuildTools],
        code_highlight: r#"data class ScoreBoard(var player: Int = 0, var cpu: Int = 0) {
    fun record(outcome: Outcome) {
        when (outcome) {
            Outcome.Player -> player++
            Outcome.Cpu -> cpu++
            Outcome.Draw -> {}
        }
    }
}
"#,
    },
];

const GAME_STEPS: [ProjectStep; 3] = [
    ProjectStep {
        title: "列挙と勝敗判定の定義",
        goal: "Move enum と Outcome 判定関数を実装する",
        walkthrough: &[
            "Move/Outcome を enum class で定義",
            "judge 関数で when 式による勝敗ロジックを実装",
        ],
        code: Some(
            r#"enum class Outcome { Player, Cpu, Draw }

enum class Move { ROCK, PAPER, SCISSORS }

fun judge(player: Move, cpu: Move): Outcome = when (player to cpu) {
    Move.ROCK to Move.SCISSORS -> Outcome.Player
    Move.PAPER to Move.ROCK -> Outcome.Player
    Move.SCISSORS to Move.PAPER -> Outcome.Player
    cpu to player -> Outcome.Cpu
    else -> Outcome.Draw
}
"#,
        ),
        verification: &["jv fmt tour/projects/rps-game/src/main.jv"],
    },
    ProjectStep {
        title: "ゲームループと演出",
        goal: "プレイヤー入力を受け取り、CPU の手と演出を表示する",
        walkthrough: &[
            "readLine で入力を受け取り、toUpperCase で Move に変換",
            "Thread.sleep を使ってカウントダウン演出を追加",
        ],
        code: Some(
            r#"fun promptMove(): Move? {
    print("rock / paper / scissors > ")
    val choice = readLine()?.trim()?.uppercase()
    return Move.values().find { it.name == choice }
}

fun playRound(board: ScoreBoard) {
    val player = promptMove()
    if (player == null) {
        println("⛔ 入力が正しくありません")
        return
    }
    println("CPU が手を選択中...")
    Thread.sleep(400)
    val cpu = cpuMove()
    val outcome = judge(player, cpu)
    println("あなた: ${player} / CPU: ${cpu} => ${outcome}")
    board.record(outcome)
}
"#,
        ),
        verification: &["printf \"rock\\n\" | jv run tour/projects/rps-game/src/main.jv"],
    },
    ProjectStep {
        title: "スコア保存とJar化",
        goal: "スコアボードをファイルに書き出し、ビルドを自動化する",
        walkthrough: &[
            "Files.write を使って結果を results/summary.txt に保存",
            "jv build --binary jar で配布可能な成果物を生成",
        ],
        code: Some(
            r#"fun saveScore(board: ScoreBoard) {
    val content = "Player=${board.player}, Cpu=${board.cpu}"
    java.nio.file.Files.createDirectories(java.nio.file.Path.of("results"))
    java.nio.file.Files.writeString(
        java.nio.file.Path.of("results/summary.txt"),
        content,
        java.nio.charset.StandardCharsets.UTF_8,
    )
}
"#,
        ),
        verification: &[
            "jv build --input tour/projects/rps-game/src/main.jv --output target/jv/rps-game/java --binary jar --bin-name rps-game",
        ],
    },
];
