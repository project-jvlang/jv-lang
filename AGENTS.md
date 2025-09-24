# 開発ガイドライン

## プロジェクト概要
**jv**は、Java 25 LTSにトランスパイルするJava Sugar Languageです。
- **メイン言語**: Rust（実装）、jv（開発対象言語）
- **ファイル拡張子**: `.jv`
- **プロジェクト構造**: Rustワークスペース（複数のcrate）
- **開発手法**: Spec-driven development（仕様駆動開発）

## プロジェクト構造とモジュール構成
```
project-jv/
├── jv/                       # Rust workspace（メイン実装）
│   ├── crates/               # コア機能のcrate群
│   │   ├── jv_lexer/         # 字句解析
│   │   ├── jv_parser/        # 構文解析
│   │   ├── jv_ast/           # 抽象構文木
│   │   ├── jv_ir/            # 中間表現
│   │   ├── jv_codegen_java/  # Java 25コード生成
│   │   ├── jv_cli/           # CLIエントリーポイント
│   │   └── ...
│   └── target/               # Rustビルド成果物
├── .spec-workflow/           # 仕様駆動開発ワークフロー
│   ├── specs/                # 機能仕様書
│   ├── templates/            # 仕様書テンプレート
│   └── steering/             # プロジェクト指針書（オプション）
├── .project-todolist/        # プロジェクト進捗管理
│   ├── phase*-checklist.md   # フェーズ別チェックリスト
│   └── checklists/           # 詳細作業リスト
└── .serena/                  # コードベース解析・記憶
```

## 開発ワークフロー

### Spec-driven Development（仕様駆動開発）
機能開発は以下の4段階で進めます：

1. **Requirements（要求定義）**
   - ユーザーストーリーとEARS基準による要件定義
   - ファイル: `.spec-workflow/specs/{feature-name}/requirements.md`

2. **Design（設計）**
   - 技術設計とアーキテクチャ決定
   - ファイル: `.spec-workflow/specs/{feature-name}/design.md`

3. **Tasks（タスク分解）**
   - 実装タスクへの原子的分解
   - ファイル: `.spec-workflow/specs/{feature-name}/tasks.md`

4. **Implementation（実装）**
   - タスクの段階的実装と進捗管理

### 開発コマンド

**Rust開発コマンド**:
```bash
# プロジェクト全体のビルド
cargo build

# 特定のバイナリ実行（CLI）
cargo run --bin jv_cli

# テスト実行
cargo test

# コードチェック（ビルドなし）
cargo check

# コードフォーマット
cargo fmt

# リント
cargo clippy
```

**Spec-workflow MCP使用**:
- `mcp__spec-workflow__spec-workflow-guide`: ワークフロー手順の確認
- `mcp__spec-workflow__approvals`: 仕様書承認管理
- `mcp__spec-workflow__spec-status`: 進捗状況確認

## コーディングスタイルと命名規則

### Rust（実装）
- **命名**: snake_case（変数・関数）、PascalCase（型・構造体）、SCREAMING_SNAKE_CASE（定数）
- **インデント**: 4スペース
- **リント**: `cargo clippy`で品質チェック
- **フォーマット**: `cargo fmt`で自動整形

### jv言語（仕様）
- **機能名**: kebab-case（例: `user-authentication`）
- **ファイル**: `.jv`拡張子
- **Java出力**: 純粋なJava 25コード（ランタイム依存なし）

## テストガイドライン
- **テストフレームワーク**: Rustの標準テスト + `test-case`クレート
- **実行**: `cargo test`でユニットテスト
- **カバレッジ**: 可能な限り高いテストカバレッジを目指す
- **統合テスト**: jv→Java変換の妥当性確認

## コミットとプルリクエストのガイドライン
- **コミット**: Conventional Commits形式
  - 例: `feat(lexer): add string interpolation tokens`
  - 例: `fix(codegen): handle null safety operators`
- **スコープ**: crateまたは機能単位
- **原子的**: 1つの論理的変更per commit

## 仕様開発の進め方

### 新機能の追加
1. **仕様作成**: spec-workflowを使用してRequirements→Design→Tasks
2. **承認プロセス**: 各段階でapprovalを取得
3. **実装**: tasks.mdに従って段階的に実装
4. **進捗管理**: `[ ]`→`[-]`→`[x]`でタスク状態を更新

### 既存仕様の確認
```bash
# 仕様状況確認
mcp__spec-workflow__spec-status

# 特定仕様の詳細確認
ls .spec-workflow/specs/{feature-name}/
```

## プロジェクト進捗管理
- **全体進捗**: `.project-todolist/PROJECT_STATUS_REPORT.md`
- **フェーズ管理**: `.project-todolist/phase*-checklist.md`
- **詳細作業**: `.project-todolist/checklists/`配下

## 品質管理
- **コード品質**: `cargo clippy`でlintチェック
- **フォーマット**: `cargo fmt`で統一
- **テスト**: `cargo test`で回帰防止
- **仕様品質**: spec-workflowのapprovalプロセス

