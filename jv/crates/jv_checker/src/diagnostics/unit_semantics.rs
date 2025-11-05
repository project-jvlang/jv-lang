use super::{DiagnosticDescriptor, DiagnosticSeverity};

pub const ENTRIES: &[DiagnosticDescriptor] = &[
    DiagnosticDescriptor {
        code: "JV_UNIT_SEM_001",
        title: "未登録の単位カテゴリです",
        help: "カテゴリは Currency / Encoding / Calendar / Tax / Custom のいずれかを指定してください。",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "JV_UNIT_SEM_002",
        title: "単位の基底型がカテゴリ要件を満たしていません",
        help: "カテゴリごとに許可された基底型のみ利用できます。要件に適合する Java 型へ書き換えてください。",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "JV_UNIT_SEM_005",
        title: "Custom カテゴリは追加レビューが必要です",
        help: "Custom は設計段階での安全性確認が前提です。可能であれば標準カテゴリへ移行してください。",
        severity: DiagnosticSeverity::Warning,
    },
    DiagnosticDescriptor {
        code: "JV_UNIT_SEM_010",
        title: "デフォルト単位 `!` が複数存在します",
        help: "カテゴリごとに `!` マーカーは 1 つだけ許可されています。どちらか一方を削除してください。",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "JV_UNIT_SEM_011",
        title: "デフォルト単位が未定義です",
        help: "`!` マーカー付きの単位を 1 つ追加して暗黙変換の基準を示してください。",
        severity: DiagnosticSeverity::Warning,
    },
    DiagnosticDescriptor {
        code: "JV_UNIT_SEM_020",
        title: "依存先の単位が未定義か自己参照になっています",
        help: "同じカテゴリ内で参照先の単位を定義し、自己参照を解消してください。",
        severity: DiagnosticSeverity::Error,
    },
    DiagnosticDescriptor {
        code: "JV_UNIT_SEM_030",
        title: "単位依存グラフに循環があります",
        help: "依存がDAGになるように `:=` / `->` の関係を見直してください。",
        severity: DiagnosticSeverity::Error,
    },
];

pub fn descriptor(code: &str) -> Option<&'static DiagnosticDescriptor> {
    ENTRIES.iter().find(|descriptor| descriptor.code == code)
}
