use anyhow::{Result, bail};
use std::fmt::Write as _;

#[derive(Debug, Clone)]
struct Explanation {
    code: &'static str,
    title: &'static str,
    overview: &'static str,
    remediation: &'static [&'static str],
    details: &'static [&'static str],
}

const EXPLANATIONS: &[Explanation] = &[
    Explanation {
        code: "JV2001",
        title: "型引数を推論できません / Type argument inference failed",
        overview: "関数またはコンストラクタ呼び出しで複数の型候補が競合したか、情報が不足しています。",
        remediation: &[
            "呼び出し側に明示的な型引数を追加する",
            "型パラメータに境界を追加して候補を絞り込む",
            "デフォルト引数を見直し、制約を満たす型へ調整する",
        ],
        details: &[
            "候補が複数存在する場合、推論器は最初に解決した型を保持し、新しい候補と競合すると JV2001 を報告します。",
            "`TypeFacts` には各候補の制約グラフが保持されるため、IDE では競合した位置へジャンプ可能です。",
        ],
    },
    Explanation {
        code: "JV2002",
        title: "境界条件を満たしていません / Generic bound not satisfied",
        overview: "型パラメータに指定されたトレイトやインターフェースの境界が満たされません。",
        remediation: &[
            "対象の型に必要なトレイト/インターフェースを実装する",
            "境界を緩和する、または別の型パラメータへ切り分ける",
            "呼び出し側でワイルドカード (`out`/`in`) など variance 指定を見直す",
        ],
        details: &[
            "`GenericSolver` は型引数に付与された `GenericBounds` を評価し、欠落している predicate を列挙します。",
            "`--telemetry` で `bound_checks` カウンタを確認すると境界評価の回数を把握できます。",
        ],
    },
    Explanation {
        code: "JV2003",
        title: "変位指定が矛盾しています / Variance conflict detected",
        overview: "`out`/`in` 指定または推論結果が宣言側の変位条件と矛盾しています。",
        remediation: &[
            "宣言側の variance (`out`/`in`) を再確認する",
            "呼び出し側でワイルドカードを用いて共変・反変位置を合わせる",
            "安全でなければ型パラメータを不変 (`invariant`) として扱う",
        ],
        details: &[
            "`VarianceAnalyzer` は使用位置から `VarianceTable` を構築し、矛盾する場合に JV2003 を生成します。",
            "`--telemetry` の `variance_conflicts`・`variance_analysis_ms` で解析コストを確認できます。",
        ],
    },
    Explanation {
        code: "JV2004",
        title: "sealed クラスの permits が不足しています / Sealed hierarchy incomplete",
        overview: "推論された型が sealed クラスの `permits` 句や when 式の網羅性と一致していません。",
        remediation: &[
            "sealed クラスに欠落している派生型を `permits` へ追加する",
            "Java 21 互換モードではコメントとして permits 情報を残す",
            "when 式に missing ケースを追加する (IDE の quick fix を利用)",
        ],
        details: &[
            "`TypeFacts` に記録された `sealed_permits` と推論結果を突合し、欠落一覧を JV2004 として提示します。",
            "Codegen では Java 21 フォールバック時にコメントへ降格されるため、--explain で完全な一覧を確認できます。",
        ],
    },
];

/// Render the explanation text for the provided diagnostic code.
pub fn render_explanation(code: &str) -> Option<String> {
    let needle = code.trim().to_ascii_uppercase();
    let entry = EXPLANATIONS.iter().find(|item| item.code == needle)?;

    let mut buffer = String::new();
    let _ = writeln!(buffer, "{} — {}", entry.code, entry.title);
    let _ = writeln!(buffer, "");
    let _ = writeln!(buffer, "概要 / Overview:");
    let _ = writeln!(buffer, "  {}", entry.overview);
    if !entry.details.is_empty() {
        let _ = writeln!(buffer, "");
        let _ = writeln!(buffer, "背景 / Details:");
        for line in entry.details {
            let _ = writeln!(buffer, "  - {}", line);
        }
    }
    if !entry.remediation.is_empty() {
        let _ = writeln!(buffer, "");
        let _ = writeln!(buffer, "対処方法 / Remediation steps:");
        for step in entry.remediation {
            let _ = writeln!(buffer, "  1. {}", step);
        }
    }
    let _ = writeln!(buffer, "");
    let _ = writeln!(
        buffer,
        "ヒント / Hints: `jv check --telemetry` を併用すると推論コストを解析できます。"
    );

    Some(buffer)
}

/// Entry point invoked by the CLI `explain` subcommand.
pub fn run(code: &str) -> Result<()> {
    match render_explanation(code) {
        Some(text) => {
            println!("{}", text.trim_end());
            Ok(())
        }
        None => bail!("Unknown diagnostic code: {}", code),
    }
}
