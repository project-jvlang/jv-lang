# jvpm TUI仕様書

## 概要

jvpm（jv Package Manager）を対話型のTUI（Terminal User Interface）として実装し、Mavenエコシステムとの統合を強化しながら、直感的なパッケージ管理体験を提供する。

## CLIアーキテクチャとTUIの位置付け

### 実行モードの関係性

jvpmとjv CLIの関係を正確に理解することは、TUI設計において重要です。

```
┌─────────────────────────────────────────────────────────────┐
│  ユーザーコマンド                                               │
└─────────────────────────────────────────────────────────────┘
                    │
         ┌──────────┴──────────┐
         │                     │
    ┌────▼────┐          ┌────▼────┐
    │ jv CLI  │          │  jvpm   │
    └────┬────┘          └────┬────┘
         │                     │
         │ jv pm add ...       │ jvpm add ...
         │ jv pm remove ...    │ jvpm clean ...
         │ jv pm tui           │ jvpm tui
         └──────────┬──────────┘
                    │
              ┌─────▼─────┐
              │   jvpm    │
              │  (実体)    │
              └─────┬─────┘
                    │
         ┌──────────┴──────────┐
         │                     │
    ┌────▼────────┐      ┌────▼────────────┐
    │ jv PM Mode  │      │ Maven Wrapper   │
    │  (TUI含む)  │      │ (デフォルト)     │
    └─────────────┘      └────┬────────────┘
                              │
                         ┌────▼────┐
                         │   mvn   │
                         └─────────┘
```

### 動作モード詳細

#### 1. **Maven Wrapper モード（デフォルト）**

jvpmは**デフォルトでMavenのラッパー**として動作します。

**特徴**:
- 未定義コマンドは自動的に`mvn`へパススルー
- `#[command(external_subcommand)]`による実装
- Maven検出ロジック（4段階フォールバック）:
  1. `JVPM_MAVEN_BIN` 環境変数
  2. プロジェクトローカル `toolchains/maven/bin/mvn`
  3. `MVN_HOME` 環境変数
  4. システムPATH

**コード例** (`jv/crates/jv_pm/src/bin/jvpm.rs:394-413`):
```rust
#[derive(Subcommand, Debug)]
enum Commands {
    Add(AddArgs),
    Remove(RemoveArgs),
    Resolver(ResolverCommand),
    Repo(RepoCommand),
    /// 未定義コマンドはMavenへフォワード
    #[command(external_subcommand)]
    Maven(Vec<OsString>),
}

fn handle_maven_passthrough(args: Vec<OsString>) -> Result<()> {
    let maven_cmd = resolve_maven_binary()?;
    let status = Command::new(&maven_cmd)
        .args(&args)
        .status()
        .with_context(|| format!("{} の実行に失敗しました", maven_cmd.display()))?;
    // ...
}
```

#### 2. **jv Package Manager モード**

jv言語のパッケージ管理は、**常にjv CLIがjvpmのラッパーとして動作**します。

**特徴**:
- `jv pm add`, `jv pm remove`, `jv pm tui` などのコマンド
- jv CLIが`jvpm`バイナリを内部的に呼び出し
- jv.toml/jv.lock とpom.xmlの双方向管理
- TUIはこのモードで起動

**コード例** (`jv/crates/jv_cli/src/commands/jvpm_bridge.rs:1-50`):
```rust
pub fn spawn_jvpm(args: &[OsString]) -> Result<ExitStatus> {
    if let Some(explicit) = env::var_os("JVPM_BIN") {
        let explicit_path = PathBuf::from(explicit);
        return Command::new(&explicit_path)
            .args(args)
            .status()
            .with_context(|| format!("{} の実行に失敗しました", explicit_path.display()));
    }

    // フォールバックロジック
    match Command::new(binary_name()).args(args).status() {
        Ok(status) => return Ok(status),
        Err(error) if error.kind() == io::ErrorKind::NotFound => {}
        Err(error) => return Err(error.into()),
    }
    // ...
}
```

### TUIの起動コンテキスト

**デフォルト動作**: jvpmは**対話型TUI**をデフォルトとします。

| 起動方法 | コンテキスト | 想定動作 |
|---------|------------|---------|
| `jvpm` (引数なし) | **デフォルト** | TUI自動起動（対話型モード） |
| `jvpm --non-interactive <subcommand>` | 非対話モード | 従来のCLI動作（TUI抑止） |
| `jv pm` | jv CLI経由 | TUI起動、jv Package Managerモード |
| `jv pm --non-interactive <subcommand>` | jv CLI経由非対話 | CLI動作、双方向管理有効 |

**重要な設計原則**:
- **TUIファースト**: デフォルトで対話型UIを提供し、ユーザー体験を向上
- **CI/CD対応**: `--non-interactive` フラグで自動化スクリプトをサポート
- TUIは起動コンテキストを検出し、適切なモードで動作
- `jv pm`経由の場合、jv.toml/pom.xml双方向管理を強調
- jv.tomlが存在する場合、双方向管理を提案

**フラグ制御**:
```bash
# TUI起動（デフォルト）
jvpm
jv pm

# 非対話モード（CI/CDや自動化スクリプト用）
jvpm --non-interactive add "org.springframework.boot:spring-boot-starter:3.4.0"
jv pm --non-interactive remove junit

# 明示的なサブコマンドは従来通りCLI動作
jvpm add <package>        # 対話的な候補選択あり
jvpm resolver list        # CLI出力
jvpm clean                # Maven passthroughでmvn cleanを実行
```

**ターミナル検出**:
- 標準入力が端末（TTY）の場合: TUI起動
- 標準入力がパイプ/リダイレクトの場合: 自動的に非対話モード
- 環境変数 `NO_COLOR` または `CI=true` の場合: 自動的に非対話モード

### 起動コンテキスト検出の実装例

```rust
/// TUIの起動判定と抑止制御
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InteractiveMode {
    /// TUI起動（デフォルト）
    Interactive,
    /// CLI動作（--non-interactiveまたは自動検出）
    NonInteractive,
}

impl InteractiveMode {
    /// 環境とフラグから対話モードを判定
    pub fn detect(force_non_interactive: bool) -> Self {
        // 1. 明示的な--non-interactiveフラグ
        if force_non_interactive {
            return Self::NonInteractive;
        }

        // 2. CI環境の検出
        if env::var("CI").is_ok() || env::var("CONTINUOUS_INTEGRATION").is_ok() {
            return Self::NonInteractive;
        }

        // 3. NO_COLOR環境変数（非対話的環境の慣例）
        if env::var("NO_COLOR").is_ok() {
            return Self::NonInteractive;
        }

        // 4. 標準入力がTTYかチェック
        if !io::stdin().is_terminal() {
            return Self::NonInteractive;
        }

        // 5. 標準出力がTTYかチェック（パイプやリダイレクト検出）
        if !io::stdout().is_terminal() {
            return Self::NonInteractive;
        }

        // デフォルトは対話型
        Self::Interactive
    }

    /// TUIを起動すべきか
    pub fn should_launch_tui(&self) -> bool {
        matches!(self, Self::Interactive)
    }
}

/// TUIの起動コンテキストを検出
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LaunchContext {
    /// jv CLI経由で起動（jv pm）
    JvCli,
    /// jvpm直接起動（jvpm）
    Standalone,
}

impl LaunchContext {
    /// 環境変数と実行状態から起動コンテキストを判定
    pub fn detect() -> Self {
        // 方法1: 環境変数による検出
        if env::var("JVPM_LAUNCHED_BY_JV_CLI").is_ok() {
            return Self::JvCli;
        }

        // 方法2: 親プロセス名の確認（実装依存）
        // プロセスツリーを調べてjv-cliが親かチェック
        if is_launched_by_jv_cli() {
            return Self::JvCli;
        }

        // デフォルトはスタンドアロン
        Self::Standalone
    }

    /// このコンテキストで双方向管理を推奨するか
    pub fn should_emphasize_dual_management(&self) -> bool {
        matches!(self, Self::JvCli)
    }

    /// 起動モードに応じたウェルカムメッセージを取得
    pub fn welcome_message(&self) -> &'static str {
        match self {
            Self::JvCli => {
                "jvpm TUI - jv Package Manager Mode\n\
                 jv.toml と pom.xml の両方を自動管理します"
            }
            Self::Standalone => {
                "jvpm TUI - Maven Wrapper Mode\n\
                 Maven互換のパッケージ管理を提供します"
            }
        }
    }
}

/// jv CLI経由で起動されたかをプロセス情報から判定
fn is_launched_by_jv_cli() -> bool {
    // 実装例: /proc/self/statまたはsysinfo crateを使用
    #[cfg(target_os = "linux")]
    {
        use std::fs;
        if let Ok(cmdline) = fs::read_to_string("/proc/self/cmdline") {
            // 親プロセスのコマンドラインに"jv-cli"が含まれるかチェック
            // （実際の実装はより堅牢な方法を使用）
            return cmdline.contains("jv-cli") || cmdline.contains("jv pm");
        }
    }
    false
}

/// AppStateに起動コンテキストを追加
pub struct AppState {
    pub launch_context: LaunchContext,
    pub current_screen: Screen,
    // ... その他のフィールド
}

impl AppState {
    pub fn new() -> Self {
        let launch_context = LaunchContext::detect();
        Self {
            launch_context,
            current_screen: Screen::MainMenu,
            // ... 初期化
        }
    }

    /// 現在のコンテキストに応じたヘルプメッセージ
    pub fn context_aware_help(&self) -> String {
        match self.launch_context {
            LaunchContext::JvCli => {
                "jv言語プロジェクトのパッケージ管理\n\
                 - パッケージ追加/削除時に jv.toml と pom.xml を自動更新\n\
                 - jv.lock による依存関係の固定\n\
                 - Maven ツールチェーンとの完全互換性".to_string()
            }
            LaunchContext::Standalone => {
                "Maven互換のパッケージマネージャー\n\
                 - 既存のMavenプロジェクトで使用可能\n\
                 - jv言語プロジェクトの場合は 'jv pm' を推奨".to_string()
            }
        }
    }
}

/// main関数での統合例
fn main() -> Result<()> {
    let cli = Cli::parse();

    // 対話モードの判定
    let interactive_mode = InteractiveMode::detect(cli.non_interactive);

    match (interactive_mode, cli.command) {
        // 引数なしでTUI起動（デフォルト）
        (InteractiveMode::Interactive, None) => {
            launch_tui()?;
        }
        // 非対話モードまたは明示的なサブコマンド
        (InteractiveMode::NonInteractive, Some(cmd)) | (_, Some(cmd)) => {
            execute_cli_command(cmd)?;
        }
        // 非対話モードで引数なし → エラーまたはヘルプ表示
        (InteractiveMode::NonInteractive, None) => {
            eprintln!("Non-interactive mode requires a command.");
            eprintln!("Use --help for usage information.");
            std::process::exit(1);
        }
    }

    Ok(())
}

/// TUIモードの起動
fn launch_tui() -> Result<()> {
    let launch_context = LaunchContext::detect();
    let app_state = AppState::new_with_context(launch_context)?;

    // Ratatui初期化と実行
    tui::run(app_state)?;

    Ok(())
}

/// CLI構造体の更新（グローバルフラグとしての--non-interactive）
#[derive(Parser, Debug)]
#[command(name = "jvpm")]
#[command(about = "jv package manager helper", long_about = None)]
struct Cli {
    /// 非対話モード（TUI抑止、CI/CD用）
    #[arg(long, global = true)]
    non_interactive: bool,

    #[command(subcommand)]
    command: Option<Commands>,
}
```

### プロジェクトファイル検出とモード提案

```rust
/// プロジェクトタイプの検出
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProjectType {
    /// jv言語プロジェクト（jv.toml存在）
    JvProject,
    /// Mavenプロジェクト（pom.xml存在）
    MavenProject,
    /// 両方存在（推奨状態）
    Hybrid,
    /// いずれも存在しない
    Uninitialized,
}

impl ProjectType {
    pub fn detect(project_root: &Path) -> Self {
        let has_jv_toml = project_root.join("jv.toml").exists();
        let has_pom_xml = project_root.join("pom.xml").exists();

        match (has_jv_toml, has_pom_xml) {
            (true, true) => Self::Hybrid,
            (true, false) => Self::JvProject,
            (false, true) => Self::MavenProject,
            (false, false) => Self::Uninitialized,
        }
    }

    /// プロジェクトタイプに応じた推奨アクション
    pub fn recommended_action(&self, launch_context: LaunchContext) -> Option<String> {
        match (self, launch_context) {
            // jv.tomlのみ存在、スタンドアロン起動 → jv CLI経由を推奨
            (Self::JvProject, LaunchContext::Standalone) => Some(
                "ℹ️  このプロジェクトはjv言語プロジェクトです。\n\
                 'jv pm tui' での起動を推奨します（双方向ファイル管理が有効化されます）"
                    .to_string(),
            ),
            // pom.xmlのみ存在、jv CLI経由 → jv.toml作成を提案
            (Self::MavenProject, LaunchContext::JvCli) => Some(
                "ℹ️  このプロジェクトはまだjv言語プロジェクトではありません。\n\
                 'jv init' でjv.tomlを作成しますか？"
                    .to_string(),
            ),
            _ => None,
        }
    }
}
```

### アーキテクチャの設計上の利点

1. **後方互換性**: 既存のMaven環境で`jvpm`をドロップイン置換として使用可能
2. **段階的移行**: プロジェクトをjv言語に移行する際も、Mavenツールチェーンを引き続き利用
3. **透過的統合**: ユーザーは`mvn`コマンドをそのまま使える（jvpm経由で自動フォワード）
4. **明確な責務分離**:
   - jvpm: Maven互換性とパッケージ管理の実体
   - jv CLI: jv言語固有の機能とワークフロー統合

## 目標

- Maven Centralライクなパッケージブラウジング体験の提供
- 複数パッケージの選択・管理機能
- Javaフレームワーク/ライブラリのカテゴリベース探索
- 機能カテゴリによるパッケージグループ選択
- 直感的なキーボード操作による効率的なワークフロー

## 技術スタック

### 必須依存クレート

```toml
[dependencies]
# TUIフレームワーク
ratatui = "0.28"
crossterm = "0.28"

# 非同期ランタイム（既存）
tokio = { version = "1.0", features = ["full"] }

# HTTPクライアント（既存）
reqwest = { version = "0.12", features = ["json", "rustls-tls"] }

# データ構造
indexmap = "2.0"
fuzzy-matcher = "0.3"  # あいまい検索

# バージョン管理
semver = "1.0"  # セマンティックバージョニング
chrono = "0.4"  # 日時処理

# 既存の依存関係
anyhow = "1.0"
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

### アーキテクチャパターン

- **Elm Architecture**: 単方向データフロー
- **状態管理**: イミュータブルステート + メッセージパッシング
- **レンダリング**: 宣言的UI構築

## コア機能仕様

### 1. メインメニュー画面

```
╭──────────────────────────────────────╮
│  jvpm - Java Package Manager        │
├──────────────────────────────────────┤
│  ↑ ↓ Select  Enter Confirm  q Quit  │
├──────────────────────────────────────┤
│                                      │
│  → Browse Maven Central              │
│    Add Package by Name               │
│    Manage Dependencies               │
│    Search by Category                │
│    Feature-based Selection           │
│    View Project Info                 │
│    Settings                          │
│                                      │
╰──────────────────────────────────────╯
```

**操作**:
- `↑`/`↓` or `j`/`k`: メニュー項目の選択
- `Enter`: 選択項目の実行
- `q`: 終了

### 2. Maven Centralブラウザ

#### 2.1 検索インターフェース

```
╭────────────────────────────────────────────────────────╮
│  Search Maven Central                                  │
├────────────────────────────────────────────────────────┤
│  Query: [spring-boot________________]  🔍 Search       │
│  Filter: [All] [Framework] [Library] [Plugin] [Tool]  │
│  Sort: [Relevance] [Popularity] [Latest]               │
├────────────────────────────────────────────────────────┤
│  Results (15/150)                                      │
├────────────────────────────────────────────────────────┤
│  [x] org.springframework.boot:spring-boot-starter     │
│      └─ 3.4.0 (Latest) | Installed: 3.3.5 | 🔄 Update │
│  [✓] org.springframework.boot:spring-boot-starter-web │
│      └─ 3.4.0 (Latest) | Installed: 3.4.0 | ✅ Up-to-date
│  [x] org.springframework.boot:spring-boot-devtools    │
│      └─ 3.4.0 (Latest) | ⭐ 15.8K | 🛠 Tool           │
│  [ ] org.springframework.boot:spring-boot-starter-test│
│      └─ 3.4.0 (Latest) | Previously selected: 2.7.0   │
├────────────────────────────────────────────────────────┤
│  Selected: 2 new | Installed: 1 | Updates: 1          │
│  Enter Add | Tab Filter | Esc Back | / Search         │
╰────────────────────────────────────────────────────────╯
```

**状態表示**:
- `[ ]`: 未選択
- `[x]`: 選択中（新規追加予定）
- `[✓]`: インストール済み（jv.tomlに存在）
- `🔄`: アップデート利用可能
- `✅`: 最新版使用中

**操作**:
- `/`: 検索モードに入る
- `Tab`: フィルタ切り替え
- `Space`: パッケージ選択/解除のトグル
- `↑`/`↓`: リスト移動
- `Enter`: 選択したパッケージを依存関係に追加
- `→`: パッケージ詳細を表示
- `Esc`: 前の画面に戻る

#### 2.2 パッケージ詳細ビュー

```
╭────────────────────────────────────────────────────────╮
│  Package Details                                       │
├────────────────────────────────────────────────────────┤
│  Name: spring-boot-starter-web                         │
│  Group: org.springframework.boot                       │
│  Latest: 3.4.0 (Released: 2024-11-21)                 │
│  License: Apache-2.0                                   │
│  ⭐ GitHub Stars: 22.1K | 📥 Downloads: 50M/month     │
├────────────────────────────────────────────────────────┤
│  Description:                                          │
│    Starter for building web applications using        │
│    Spring MVC. Uses Tomcat as the default embedded    │
│    container.                                          │
├────────────────────────────────────────────────────────┤
│  Dependencies (5 direct):                              │
│    ├─ spring-boot-starter (3.4.0)                     │
│    ├─ spring-web (6.2.0)                              │
│    ├─ spring-webmvc (6.2.0)                           │
│    └─ tomcat-embed-core (10.1.30)                     │
├────────────────────────────────────────────────────────┤
│  Available Versions:                                   │
│    [x] 3.4.0 (Latest)  [ ] 3.3.5  [ ] 3.3.4  [ ] ...  │
├────────────────────────────────────────────────────────┤
│  a Add | Space Select Version | Esc Back               │
╰────────────────────────────────────────────────────────╯
```

**操作**:
- `a`: 選択したバージョンを依存関係に追加
- `Space`: バージョン選択
- `↑`/`↓`: 項目移動
- `Esc`: 検索画面に戻る

### 3. カテゴリベース探索

```
╭────────────────────────────────────────────────────────╮
│  Browse by Category                                    │
├────────────────────────────────────────────────────────┤
│  Java Frameworks & Libraries                           │
├────────────────────────────────────────────────────────┤
│  📦 Web Frameworks                                     │
│    ├─ 🍃 Spring Framework                             │
│    │   ├─ Spring Boot                      [Add All]  │
│    │   ├─ Spring MVC                                  │
│    │   ├─ Spring Data                                 │
│    │   └─ Spring Security                             │
│    ├─ ☕ Jakarta EE                                    │
│    ├─ 🎯 Micronaut                                    │
│    └─ ⚡ Quarkus                                       │
│  🗄️ Data & Persistence                                │
│    ├─ Hibernate ORM                                   │
│    ├─ MyBatis                                         │
│    └─ jOOQ                                            │
│  🧪 Testing                                            │
│    ├─ JUnit 5                           [Selected 3]  │
│    ├─ Mockito                                         │
│    └─ TestContainers                                  │
│  📊 Logging                                            │
│  🔧 Utilities                                          │
├────────────────────────────────────────────────────────┤
│  Enter Expand | Space Select | a Add Selected | Esc   │
╰────────────────────────────────────────────────────────╯
```

**カテゴリ構造**:

```yaml
categories:
  - name: "Web Frameworks"
    icon: "📦"
    subcategories:
      - name: "Spring Framework"
        icon: "🍃"
        packages:
          - group: "org.springframework.boot"
            artifacts:
              - "spring-boot-starter"
              - "spring-boot-starter-web"
              - "spring-boot-starter-data-jpa"
          - group: "org.springframework"
            artifacts: ["spring-webmvc"]
      - name: "Jakarta EE"
        icon: "☕"
        # ...

  - name: "Data & Persistence"
    icon: "🗄️"
    subcategories:
      - name: "Hibernate ORM"
        packages: [...]

  - name: "Testing"
    icon: "🧪"
    # ...
```

**操作**:
- `Enter`: カテゴリ/サブカテゴリの展開・折りたたみ
- `Space`: パッケージ/グループの選択トグル
- `a`: 選択したパッケージを追加
- `Ctrl+a`: カテゴリ内すべてを選択
- `Esc`: メインメニューに戻る

### 4. 機能ベース選択

```
╭────────────────────────────────────────────────────────╮
│  Feature-based Package Selection                       │
├────────────────────────────────────────────────────────┤
│  Select the features you need:                         │
├────────────────────────────────────────────────────────┤
│  🌐 Web Development                                    │
│    [x] RESTful API                                     │
│    [x] WebSocket                                       │
│    [ ] GraphQL                                         │
│    [x] Template Engine                                 │
│  🗄️ Database                                           │
│    [x] Relational DB (JPA)                            │
│    [ ] NoSQL (MongoDB)                                 │
│    [ ] Redis Cache                                     │
│  🔐 Security                                           │
│    [x] Authentication & Authorization                  │
│    [ ] OAuth2/OIDC                                     │
│  📊 Observability                                      │
│    [x] Logging                                         │
│    [x] Metrics                                         │
│    [ ] Distributed Tracing                             │
│  🧪 Testing                                            │
│    [x] Unit Testing                                    │
│    [x] Integration Testing                             │
├────────────────────────────────────────────────────────┤
│  → Recommended Packages (8)                            │
│    ✓ spring-boot-starter-web (RESTful API)            │
│    ✓ spring-boot-starter-websocket (WebSocket)        │
│    ✓ spring-boot-starter-thymeleaf (Template)         │
│    ✓ spring-boot-starter-data-jpa (JPA)               │
│    ✓ spring-boot-starter-security (Auth)              │
│    ✓ logback-classic (Logging)                        │
│    ✓ micrometer-core (Metrics)                        │
│    ✓ junit-jupiter (Testing)                          │
├────────────────────────────────────────────────────────┤
│  Space Select | Enter Confirm Selection | Esc Cancel   │
╰────────────────────────────────────────────────────────╯
```

**機能マッピングテーブル**:

```rust
struct FeaturePackageMapping {
    feature: Feature,
    packages: Vec<PackageRecommendation>,
    alternatives: Vec<Vec<PackageRecommendation>>,
}

#[derive(Debug, Clone)]
enum Feature {
    RestfulApi,
    WebSocket,
    GraphQL,
    TemplateEngine,
    RelationalDB,
    NoSQL,
    Redis,
    Authentication,
    OAuth2,
    Logging,
    Metrics,
    Tracing,
    UnitTest,
    IntegrationTest,
}

impl Feature {
    fn recommend_packages(&self) -> Vec<PackageRecommendation> {
        match self {
            Feature::RestfulApi => vec![
                PackageRecommendation {
                    group: "org.springframework.boot".into(),
                    artifact: "spring-boot-starter-web".into(),
                    version: RecommendedVersion::Latest,
                    priority: Priority::Primary,
                }
            ],
            Feature::RelationalDB => vec![
                PackageRecommendation {
                    group: "org.springframework.boot".into(),
                    artifact: "spring-boot-starter-data-jpa".into(),
                    version: RecommendedVersion::Latest,
                    priority: Priority::Primary,
                }
            ],
            // ...
        }
    }
}
```

**操作**:
- `Space`: 機能の選択/解除
- `Enter`: 選択した機能に基づくパッケージ群を依存関係に追加
- `→`: 推奨パッケージ詳細を表示
- `Esc`: メインメニューに戻る

### 5. 統合依存関係管理画面

```
╭────────────────────────────────────────────────────────╮
│  Dependency Management                                 │
├────────────────────────────────────────────────────────┤
│  Project: my-app (jv.toml)                            │
│  Total: 8 direct | 24 transitive | 🔄 3 updates       │
├────────────────────────────────────────────────────────┤
│  Filter: [All] [Updates Available] [Outdated] [Latest]│
├────────────────────────────────────────────────────────┤
│  🔄 [x] spring-boot-starter-web     3.3.5 → 3.4.0     │
│      └─ Released: 2 days ago | Security: None         │
│      ├─ [T] spring-boot-starter    3.3.5 → 3.4.0      │
│      ├─ [T] spring-web             6.1.0 → 6.2.0      │
│      └─ [T] tomcat-embed-core      10.1.28 → 10.1.30  │
│  ✅ [ ] spring-boot-starter-data-jpa  3.4.0 (Latest)  │
│  ⚠️  [x] lombok                      1.18.30 → 1.18.34 │
│      └─ 🔐 CVE-2023-XXXX (Medium) | Update required  │
│  🔄 [ ] junit-jupiter               5.10.1 → 5.11.3   │
│      └─ Released: 1 week ago                          │
├────────────────────────────────────────────────────────┤
│  Selected: 2 packages for update                      │
│  Actions: Space Select | u Update | U Update All      │
│           r Remove | v Tree | i Info | Esc Back       │
├────────────────────────────────────────────────────────┤
│  Status: ⚠ 3 updates | 🔐 1 security | ✅ 5 up-to-date│
╰────────────────────────────────────────────────────────╯
```

**状態アイコン**:
- `✅`: 最新版使用中
- `🔄`: マイナー/パッチアップデート利用可能
- `⚠️`: メジャーアップデートまたはセキュリティ警告
- `🔐`: セキュリティ脆弱性あり（即時更新推奨）
- `🚫`: 非推奨バージョン

**操作**:
- `Space`: 依存関係の選択
- `u`: 選択した依存関係を更新
- `U`: すべての更新可能なパッケージを一括更新
- `r`: 選択した依存関係を削除
- `v`: 依存関係ツリーの詳細表示
- `i`: パッケージ詳細情報とアップデート内容
- `Tab`: フィルタ切り替え
- `Enter`: パッケージ詳細を表示
- `Esc`: メインメニューに戻る

### 5.1 アップデート詳細ビュー

```
╭────────────────────────────────────────────────────────╮
│  Update Details: spring-boot-starter-web               │
├────────────────────────────────────────────────────────┤
│  Current Version: 3.3.5                                │
│  Latest Version:  3.4.0                                │
│  Update Type: Minor (Breaking changes unlikely)        │
│  Released: 2024-11-21 (2 days ago)                    │
├────────────────────────────────────────────────────────┤
│  What's New:                                           │
│    • Performance improvements in auto-configuration    │
│    • New observability features                        │
│    • Bug fixes: #12345, #12346, #12347                │
│                                                         │
│  Breaking Changes: None                                 │
│                                                         │
│  Security Fixes:                                        │
│    ✓ CVE-2024-XXXX (Low severity)                     │
├────────────────────────────────────────────────────────┤
│  Dependencies (will also update):                      │
│    • spring-boot-starter    3.3.5 → 3.4.0             │
│    • spring-web             6.1.0 → 6.2.0             │
│    • tomcat-embed-core      10.1.28 → 10.1.30         │
├────────────────────────────────────────────────────────┤
│  u Update Now | s Skip | v View Changelog | Esc Back  │
╰────────────────────────────────────────────────────────╯
```

### 5.2 一括更新確認画面

```
╭────────────────────────────────────────────────────────╮
│  Confirm Bulk Update                                   │
├────────────────────────────────────────────────────────┤
│  The following packages will be updated:               │
│                                                         │
│  🔄 spring-boot-starter-web     3.3.5 → 3.4.0         │
│  ⚠️ lombok                       1.18.30 → 1.18.34     │
│  🔄 junit-jupiter               5.10.1 → 5.11.3       │
│                                                         │
│  Total: 3 packages + 8 transitive dependencies         │
│                                                         │
│  Security Updates: 1                                    │
│  Breaking Changes: None detected                        │
├────────────────────────────────────────────────────────┤
│  ⚠ Recommendation:                                     │
│    Review changelog for major version updates          │
│    Test thoroughly after security updates              │
├────────────────────────────────────────────────────────┤
│  Enter Proceed | Esc Cancel                            │
╰────────────────────────────────────────────────────────╯
```

## データモデル

### パッケージ情報

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageInfo {
    pub group_id: String,
    pub artifact_id: String,
    pub latest_version: String,
    pub versions: Vec<String>,
    pub description: Option<String>,
    pub license: Option<String>,
    pub repository_url: Option<String>,
    pub homepage_url: Option<String>,
    pub stars: Option<u32>,
    pub downloads: Option<u64>,
    pub published_at: Option<String>,
    pub category: Option<PackageCategory>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PackageCategory {
    Framework,
    Library,
    Plugin,
    Tool,
    Testing,
    Logging,
    Database,
    Security,
    Utility,
    Other,
}
```

### 統合状態管理

```rust
#[derive(Debug, Clone, Default)]
pub struct DependencyState {
    /// 現在選択中のパッケージ（新規追加予定）
    selected_packages: IndexMap<String, SelectedPackage>,
    /// インストール済みパッケージ（jv.tomlから読み込み）
    installed_packages: IndexMap<String, InstalledPackage>,
    /// 利用可能な更新情報
    available_updates: IndexMap<String, UpdateInfo>,
}

#[derive(Debug, Clone)]
pub struct SelectedPackage {
    pub group_id: String,
    pub artifact_id: String,
    pub version: String,
    pub selected_via: SelectionSource,
    pub install_status: InstallStatus,
}

#[derive(Debug, Clone)]
pub struct InstalledPackage {
    pub group_id: String,
    pub artifact_id: String,
    pub current_version: String,
    pub installed_at: Option<chrono::DateTime<chrono::Utc>>,
    pub is_direct: bool, // 直接依存か推移的依存か
}

#[derive(Debug, Clone)]
pub struct UpdateInfo {
    pub group_id: String,
    pub artifact_id: String,
    pub current_version: String,
    pub latest_version: String,
    pub update_type: UpdateType,
    pub released_at: Option<chrono::DateTime<chrono::Utc>>,
    pub security_advisory: Option<SecurityAdvisory>,
    pub breaking_changes: Vec<String>,
    pub changelog_url: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UpdateType {
    Major,      // 1.0.0 -> 2.0.0 (破壊的変更の可能性)
    Minor,      // 1.0.0 -> 1.1.0 (機能追加)
    Patch,      // 1.0.0 -> 1.0.1 (バグ修正)
    Security,   // セキュリティ修正
}

#[derive(Debug, Clone)]
pub struct SecurityAdvisory {
    pub cve_id: String,
    pub severity: Severity,
    pub description: String,
    pub fixed_in: String,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
    Low,
    Medium,
    High,
    Critical,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InstallStatus {
    NotInstalled,
    Installed,
    UpdateAvailable,
    SecurityUpdate,
    Outdated,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SelectionSource {
    ManualSearch,
    CategoryBrowse,
    FeatureBased,
    UpdateSelection,
}

impl DependencyState {
    pub fn toggle_selection(
        &mut self,
        package: PackageInfo,
        version: String,
        source: SelectionSource,
    ) {
        let key = format!("{}:{}", package.group_id, package.artifact_id);
        let install_status = self.get_install_status(&package.group_id, &package.artifact_id);

        if self.selected_packages.contains_key(&key) {
            self.selected_packages.remove(&key);
        } else {
            self.selected_packages.insert(key, SelectedPackage {
                group_id: package.group_id,
                artifact_id: package.artifact_id,
                version,
                selected_via: source,
                install_status,
            });
        }
    }

    pub fn get_install_status(&self, group_id: &str, artifact_id: &str) -> InstallStatus {
        let key = format!("{}:{}", group_id, artifact_id);

        if let Some(installed) = self.installed_packages.get(&key) {
            if let Some(update) = self.available_updates.get(&key) {
                match update.update_type {
                    UpdateType::Security => InstallStatus::SecurityUpdate,
                    UpdateType::Major if self.is_outdated(&installed.current_version, &update.latest_version) => {
                        InstallStatus::Outdated
                    }
                    _ => InstallStatus::UpdateAvailable,
                }
            } else {
                InstallStatus::Installed
            }
        } else {
            InstallStatus::NotInstalled
        }
    }

    pub fn is_selected(&self, group_id: &str, artifact_id: &str) -> bool {
        let key = format!("{}:{}", group_id, artifact_id);
        self.selected_packages.contains_key(&key)
    }

    pub fn count_by_status(&self) -> StatusCounts {
        StatusCounts {
            new_selections: self.selected_packages.values()
                .filter(|p| p.install_status == InstallStatus::NotInstalled)
                .count(),
            updates_available: self.available_updates.len(),
            security_updates: self.available_updates.values()
                .filter(|u| u.update_type == UpdateType::Security)
                .count(),
            up_to_date: self.installed_packages.len() - self.available_updates.len(),
        }
    }

    fn is_outdated(&self, current: &str, latest: &str) -> bool {
        // セマンティックバージョニングでメジャーバージョンの差が2以上
        if let (Ok(curr), Ok(lat)) = (semver::Version::parse(current), semver::Version::parse(latest)) {
            lat.major - curr.major >= 2
        } else {
            false
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct StatusCounts {
    pub new_selections: usize,
    pub updates_available: usize,
    pub security_updates: usize,
    pub up_to_date: usize,
}
```

### カテゴリツリー

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CategoryTree {
    pub categories: Vec<Category>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Category {
    pub name: String,
    pub icon: String,
    pub subcategories: Vec<Subcategory>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Subcategory {
    pub name: String,
    pub icon: Option<String>,
    pub packages: Vec<PackageGroup>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageGroup {
    pub group_id: String,
    pub artifacts: Vec<String>,
    pub description: Option<String>,
}
```

## UIコンポーネント設計

### ウィジェット階層

```
App
├─ MainMenu
├─ MavenBrowser
│   ├─ SearchBar
│   ├─ FilterBar
│   ├─ PackageList
│   │   └─ PackageItem (repeated)
│   └─ PackageDetailPanel
├─ CategoryBrowser
│   ├─ CategoryTree
│   │   ├─ CategoryNode (recursive)
│   │   └─ PackageNode
│   └─ SelectionSummary
├─ FeatureSelector
│   ├─ FeatureList
│   └─ RecommendationPanel
└─ DependencyManager
    ├─ DependencyList
    └─ ActionBar
```

### 再利用可能なコンポーネント

```rust
/// 汎用的な選択可能リスト
pub struct SelectableList<T> {
    items: Vec<T>,
    selected_index: usize,
    selected_items: HashSet<usize>,
    multi_select: bool,
}

/// ツリービュー（カテゴリ表示用）
pub struct TreeView<T> {
    root: TreeNode<T>,
    expanded_paths: HashSet<String>,
    selected_path: Option<String>,
}

/// 検索バー
pub struct SearchBar {
    query: String,
    cursor_position: usize,
    focused: bool,
}

/// ステータスバー
pub struct StatusBar {
    left_text: String,
    center_text: String,
    right_text: String,
}
```

## 状態管理アーキテクチャ

### アプリケーション状態

```rust
#[derive(Debug)]
pub struct AppState {
    pub current_screen: Screen,
    pub selection_state: SelectionState,
    pub search_state: SearchState,
    pub category_state: CategoryState,
    pub dependency_state: DependencyState,
    pub error_message: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Screen {
    MainMenu,
    MavenBrowser,
    CategoryBrowser,
    FeatureSelector,
    DependencyManager,
    Settings,
}
```

### メッセージ駆動更新

```rust
#[derive(Debug)]
pub enum Message {
    // ナビゲーション
    NavigateTo(Screen),
    NavigateBack,

    // 検索
    SearchQueryChanged(String),
    SearchSubmit,
    SearchResultsReceived(Vec<PackageInfo>),

    // 選択
    TogglePackage { group: String, artifact: String, version: String },
    SelectAll,
    DeselectAll,

    // 依存関係操作
    AddSelectedPackages,
    RemovePackage(String),
    UpdatePackage { coordinate: String, version: String },

    // カテゴリブラウジング
    ExpandCategory(String),
    CollapseCategory(String),

    // エラー
    Error(String),
    ClearError,
}

pub async fn update(state: &mut AppState, message: Message) -> Result<()> {
    match message {
        Message::NavigateTo(screen) => {
            state.current_screen = screen;
        }
        Message::TogglePackage { group, artifact, version } => {
            state.selection_state.toggle(
                PackageInfo { group_id: group, artifact_id: artifact, .. },
                version,
                SelectionSource::ManualSearch,
            );
        }
        Message::AddSelectedPackages => {
            // jv.tomlへの書き込み処理
            add_dependencies_to_manifest(&state.selection_state).await?;
            state.selection_state.clear();
        }
        // ...
    }
    Ok(())
}
```

## Maven Central統合

### Maven Search API

```rust
pub struct MavenSearchClient {
    client: reqwest::Client,
    base_url: String,
}

impl MavenSearchClient {
    pub async fn search(&self, query: &str, limit: usize) -> Result<Vec<PackageInfo>> {
        let url = format!(
            "https://search.maven.org/solrsearch/select?q={}&rows={}&wt=json",
            urlencoding::encode(query),
            limit
        );

        let response: MavenSearchResponse = self.client
            .get(&url)
            .send()
            .await?
            .json()
            .await?;

        Ok(response.response.docs.into_iter().map(|doc| PackageInfo {
            group_id: doc.g,
            artifact_id: doc.a,
            latest_version: doc.latest_version.unwrap_or_default(),
            // ...
        }).collect())
    }

    pub async fn fetch_versions(&self, group: &str, artifact: &str) -> Result<Vec<String>> {
        let url = format!(
            "https://search.maven.org/solrsearch/select?q=g:{}+AND+a:{}&core=gav&rows=50&wt=json",
            urlencoding::encode(group),
            urlencoding::encode(artifact)
        );

        let response: MavenSearchResponse = self.client
            .get(&url)
            .send()
            .await?
            .json()
            .await?;

        let versions: Vec<String> = response.response.docs
            .into_iter()
            .filter_map(|doc| doc.v)
            .collect();

        Ok(versions)
    }

    pub async fn check_for_updates(
        &self,
        installed: &InstalledPackage,
    ) -> Result<Option<UpdateInfo>> {
        let versions = self.fetch_versions(&installed.group_id, &installed.artifact_id).await?;

        if versions.is_empty() {
            return Ok(None);
        }

        let current = semver::Version::parse(&installed.current_version)?;
        let latest_str = versions.first().unwrap();
        let latest = semver::Version::parse(latest_str)?;

        if latest > current {
            let update_type = determine_update_type(&current, &latest);
            Ok(Some(UpdateInfo {
                group_id: installed.group_id.clone(),
                artifact_id: installed.artifact_id.clone(),
                current_version: installed.current_version.clone(),
                latest_version: latest_str.clone(),
                update_type,
                released_at: None, // Maven APIでは提供されない
                security_advisory: None,
                breaking_changes: vec![],
                changelog_url: None,
            }))
        } else {
            Ok(None)
        }
    }
}

fn determine_update_type(current: &semver::Version, latest: &semver::Version) -> UpdateType {
    if latest.major > current.major {
        UpdateType::Major
    } else if latest.minor > current.minor {
        UpdateType::Minor
    } else {
        UpdateType::Patch
    }
}
```

## セキュリティ脆弱性チェック

### GitHub Advisory Database API

```rust
pub struct SecurityAdvisoryClient {
    client: reqwest::Client,
    github_token: Option<String>,
}

impl SecurityAdvisoryClient {
    pub async fn check_vulnerabilities(
        &self,
        group_id: &str,
        artifact_id: &str,
        version: &str,
    ) -> Result<Vec<SecurityAdvisory>> {
        // GitHub Advisory Database GraphQL API
        let query = format!(
            r#"
            query {{
              securityVulnerabilities(
                first: 10,
                ecosystem: MAVEN,
                package: "{}:{}"
              ) {{
                nodes {{
                  advisory {{
                    identifiers {{
                      type
                      value
                    }}
                    severity
                    summary
                    withdrawnAt
                  }}
                  vulnerableVersionRange
                }}
              }}
            }}
            "#,
            group_id, artifact_id
        );

        let response = self.graphql_request(&query).await?;
        let vulnerabilities = parse_advisory_response(&response)?;

        // 現在のバージョンが脆弱性の影響を受けるかチェック
        let current_version = semver::Version::parse(version)?;
        let applicable: Vec<SecurityAdvisory> = vulnerabilities
            .into_iter()
            .filter(|vuln| is_version_affected(&current_version, &vuln.affected_range))
            .collect();

        Ok(applicable)
    }

    async fn graphql_request(&self, query: &str) -> Result<serde_json::Value> {
        let mut request = self.client
            .post("https://api.github.com/graphql")
            .json(&serde_json::json!({ "query": query }));

        if let Some(token) = &self.github_token {
            request = request.header("Authorization", format!("Bearer {}", token));
        }

        let response = request.send().await?.json().await?;
        Ok(response)
    }
}

fn is_version_affected(version: &semver::Version, range: &str) -> bool {
    // バージョン範囲のパース（例: ">= 1.0.0, < 1.5.2"）
    // 簡易実装 - 実際にはより複雑な範囲表記に対応が必要
    let parts: Vec<&str> = range.split(',').collect();

    for part in parts {
        let trimmed = part.trim();
        if trimmed.starts_with(">=") {
            // 最小バージョンチェック
        } else if trimmed.starts_with("<") {
            // 最大バージョンチェック
        }
        // ... その他の演算子
    }

    true // 簡易実装
}
```

### OSV (Open Source Vulnerabilities) API

代替として、OSV APIを使用することも可能：

```rust
pub struct OsvClient {
    client: reqwest::Client,
}

impl OsvClient {
    pub async fn query_vulnerabilities(
        &self,
        group_id: &str,
        artifact_id: &str,
        version: &str,
    ) -> Result<Vec<SecurityAdvisory>> {
        let package = format!("{}:{}", group_id, artifact_id);

        let query = serde_json::json!({
            "version": version,
            "package": {
                "name": package,
                "ecosystem": "Maven"
            }
        });

        let response: OsvResponse = self.client
            .post("https://api.osv.dev/v1/query")
            .json(&query)
            .send()
            .await?
            .json()
            .await?;

        let advisories = response.vulns
            .into_iter()
            .map(|vuln| SecurityAdvisory {
                cve_id: vuln.id,
                severity: parse_severity(&vuln.severity),
                description: vuln.summary,
                fixed_in: vuln.fixed_version.unwrap_or_default(),
            })
            .collect();

        Ok(advisories)
    }
}

#[derive(Debug, Deserialize)]
struct OsvResponse {
    vulns: Vec<OsvVulnerability>,
}

#[derive(Debug, Deserialize)]
struct OsvVulnerability {
    id: String,
    summary: String,
    #[serde(default)]
    severity: String,
    fixed_version: Option<String>,
}
```

### メタデータキャッシング

```rust
pub struct PackageCache {
    cache_dir: PathBuf,
    ttl: Duration,
}

impl PackageCache {
    pub fn get_cached(&self, key: &str) -> Result<Option<PackageInfo>> {
        let cache_file = self.cache_dir.join(format!("{}.json", key));
        if !cache_file.exists() {
            return Ok(None);
        }

        let metadata = fs::metadata(&cache_file)?;
        let modified = metadata.modified()?;
        let elapsed = modified.elapsed()?;

        if elapsed > self.ttl {
            return Ok(None);
        }

        let content = fs::read_to_string(cache_file)?;
        let info: PackageInfo = serde_json::from_str(&content)?;
        Ok(Some(info))
    }

    pub fn store(&self, key: &str, info: &PackageInfo) -> Result<()> {
        let cache_file = self.cache_dir.join(format!("{}.json", key));
        let content = serde_json::to_string_pretty(info)?;
        fs::write(cache_file, content)?;
        Ok(())
    }
}
```

## キーバインディング

### グローバル

| キー | 動作 |
|------|------|
| `q` / `Ctrl+C` | 終了 |
| `Esc` | 前の画面に戻る |
| `?` | ヘルプ表示 |
| `↑` / `k` | 上に移動 |
| `↓` / `j` | 下に移動 |
| `←` / `h` | 左に移動 |
| `→` / `l` | 右に移動 |

### コンテキスト固有

| 画面 | キー | 動作 |
|------|------|------|
| 検索 | `/` | 検索モードに入る |
| 検索 | `Tab` | フィルタ切り替え |
| 検索 | `Space` | パッケージ選択トグル |
| 検索 | `Enter` | 選択パッケージを追加 |
| カテゴリ | `Enter` | カテゴリ展開/折りたたみ |
| カテゴリ | `Space` | パッケージ選択トグル |
| カテゴリ | `Ctrl+A` | カテゴリ内全選択 |
| カテゴリ | `a` | 選択したパッケージを追加 |
| 依存関係 | `r` | 削除 |
| 依存関係 | `u` | 更新 |
| 依存関係 | `v` | ツリー表示 |

## 設定ファイル

### ~/.jv/tui-config.toml

```toml
[search]
default_limit = 50
cache_ttl_hours = 24

[ui]
theme = "dark"  # "dark" | "light"
show_icons = true
compact_mode = false

[categories]
# カテゴリ定義ファイルのパス
definitions = "~/.jv/categories.yaml"

[features]
# 機能マッピングファイルのパス
mappings = "~/.jv/feature-mappings.yaml"
```

### ~/.jv/categories.yaml

```yaml
categories:
  - name: "Web Frameworks"
    icon: "📦"
    subcategories:
      - name: "Spring Framework"
        icon: "🍃"
        packages:
          - group: "org.springframework.boot"
            artifacts:
              - "spring-boot-starter"
              - "spring-boot-starter-web"
              - "spring-boot-starter-webflux"
              - "spring-boot-starter-data-jpa"
              - "spring-boot-starter-security"
          - group: "org.springframework"
            artifacts:
              - "spring-webmvc"
              - "spring-web"
      - name: "Jakarta EE"
        icon: "☕"
        packages:
          - group: "jakarta.platform"
            artifacts: ["jakarta.jakartaee-api"]

  - name: "Data & Persistence"
    icon: "🗄️"
    subcategories:
      - name: "Hibernate ORM"
        packages:
          - group: "org.hibernate.orm"
            artifacts: ["hibernate-core", "hibernate-envers"]
      - name: "MyBatis"
        packages:
          - group: "org.mybatis"
            artifacts: ["mybatis", "mybatis-spring"]
```

## エラーハンドリング

### エラー表示

```rust
pub fn render_error_popup(f: &mut Frame, error_msg: &str) {
    let block = Block::default()
        .title("Error")
        .borders(Borders::ALL)
        .style(Style::default().fg(Color::Red));

    let paragraph = Paragraph::new(error_msg)
        .block(block)
        .wrap(Wrap { trim: true });

    let area = centered_rect(60, 20, f.size());
    f.render_widget(Clear, area);
    f.render_widget(paragraph, area);
}
```

### エラーカテゴリ

```rust
#[derive(Debug, thiserror::Error)]
pub enum TuiError {
    #[error("Network error: {0}")]
    Network(#[from] reqwest::Error),

    #[error("Manifest error: {0}")]
    Manifest(String),

    #[error("Cache error: {0}")]
    Cache(#[from] std::io::Error),

    #[error("Parse error: {0}")]
    Parse(#[from] serde_json::Error),

    #[error("No packages selected")]
    NoSelection,
}
```

## パフォーマンス最適化

### 仮想スクロール

```rust
/// 大量のリスト項目を効率的にレンダリング
pub struct VirtualList<T> {
    items: Vec<T>,
    viewport_height: usize,
    scroll_offset: usize,
}

impl<T> VirtualList<T> {
    pub fn visible_items(&self) -> &[T] {
        let start = self.scroll_offset;
        let end = (start + self.viewport_height).min(self.items.len());
        &self.items[start..end]
    }
}
```

### 非同期データフェッチ

```rust
pub async fn fetch_packages_async(query: String) -> Result<Vec<PackageInfo>> {
    tokio::spawn(async move {
        let client = MavenSearchClient::new();
        client.search(&query, 50).await
    }).await?
}
```

## テスト戦略

### ユニットテスト

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_selection_toggle() {
        let mut state = SelectionState::default();
        let package = PackageInfo {
            group_id: "org.junit.jupiter".into(),
            artifact_id: "junit-jupiter".into(),
            latest_version: "5.10.1".into(),
            // ...
        };

        state.toggle(package.clone(), "5.10.1".into(), SelectionSource::ManualSearch);
        assert!(state.is_selected("org.junit.jupiter", "junit-jupiter"));

        state.toggle(package, "5.10.1".into(), SelectionSource::ManualSearch);
        assert!(!state.is_selected("org.junit.jupiter", "junit-jupiter"));
    }
}
```

### 統合テスト

```rust
#[tokio::test]
async fn test_maven_search_integration() {
    let client = MavenSearchClient::new();
    let results = client.search("junit", 10).await.unwrap();

    assert!(!results.is_empty());
    assert!(results.iter().any(|p| p.artifact_id.contains("junit")));
}
```

## 双方向ファイル管理 (Dual-File Management)

jvpmのTUIは、jvネイティブ形式（`jv.toml`/`jv.lock`）とMaven互換形式（`pom.xml`）の両方を自動的に管理します。この双方向管理により、jvプロジェクトをMavenツールチェーンとシームレスに統合できます。

### 管理対象ファイル

| ファイル | 形式 | 役割 | 生成タイミング |
|---------|------|------|--------------|
| `jv.toml` | TOML | プロジェクトマニフェスト（直接依存関係） | ユーザー編集 + TUI更新 |
| `jv.lock` | TOML | ロックファイル（解決済み依存関係グラフ） | 依存関係解決時 |
| `pom.xml` | XML | Maven互換プロジェクト記述 | Export時に自動生成 |
| `classpath.txt` | Text | Javaコンパイル用クラスパス | Export時に自動生成 |

### アーキテクチャ概要

```
┌─────────────────────────────────────────────────────────┐
│                      TUI操作                            │
│  (Add/Remove/Update Package)                            │
└───────────────────┬─────────────────────────────────────┘
                    │
                    ↓
┌─────────────────────────────────────────────────────────┐
│              Manifest更新 (jv.toml)                     │
│  - dependencies セクションの追加/削除                    │
│  - バージョン指定の更新                                  │
└───────────────────┬─────────────────────────────────────┘
                    │
                    ↓
┌─────────────────────────────────────────────────────────┐
│           依存関係解決 (Resolver)                        │
│  - PubGrubアルゴリズムによるバージョン解決               │
│  - 推移的依存関係の展開                                  │
│  - jv.lockの更新                                         │
└───────────────────┬─────────────────────────────────────┘
                    │
                    ↓
┌─────────────────────────────────────────────────────────┐
│        ExportRequest生成と実行                           │
│  - JavaProjectExporter::export() 呼び出し               │
│  - MavenIntegrationDispatcher による pom.xml 生成       │
│  - ローカルリポジトリ同期                                │
└───────────────────┬─────────────────────────────────────┘
                    │
                    ↓
┌─────────────────────────────────────────────────────────┐
│          Maven互換ファイル生成                           │
│  - pom.xml                                              │
│  - classpath.txt                                        │
│  - output_dir へのファイル配置                           │
└─────────────────────────────────────────────────────────┘
```

### ExportRequest統合

TUIでのパッケージ操作（追加/削除/更新）後、以下のフローで双方向ファイルを生成します：

```rust
// TUI操作後の処理フロー
pub async fn handle_package_operation(
    app_state: &mut AppState,
    operation: PackageOperation,
) -> Result<()> {
    // 1. jv.tomlを更新
    update_manifest(&app_state.project_root, &operation)?;

    // 2. 依存関係を再解決
    let lockfile = resolve_dependencies(&app_state.project_root).await?;

    // 3. ExportRequestを構築して実行
    let request = ExportRequest {
        project_root: &app_state.project_root,
        manifest: &app_state.manifest,
        lockfile: &lockfile,
        sources_dir: &app_state.project_root.join("src"),
        output_dir: &app_state.project_root.join("target"),
        local_repository: &app_state.local_repository,
        repositories: &app_state.repositories,
        mirrors: &app_state.mirrors,
        resolved: Some(&app_state.resolved_dependencies),
    };

    // 4. Java/Mavenファイル生成
    let exporter = JavaProjectExporter::new();
    exporter.export(&request).await?;

    Ok(())
}
```

### MavenIntegrationDispatcher連携

`ExportRequest`の実行時、`MavenIntegrationDispatcher`が以下を生成します：

```rust
// export/mod.rs の内部処理
fn generate_maven_files(
    request: &ExportRequest<'_>,
    output_repo: &Path,
    resolved: &ResolvedDependencies,
) -> Result<usize, ExportError> {
    let dispatcher = MavenIntegrationDispatcher::new();

    // Maven統合ファイル生成
    let integration = dispatcher.generate_default(&MavenIntegrationConfig {
        manifest: request.manifest,
        resolved,
        lockfile: Some(request.lockfile),
        repositories: request.repositories,
        mirrors: request.mirrors,
        project_root: request.project_root,
        local_repository: output_repo,
    })?;

    // ファイル書き出し
    let mut updated = 0usize;
    for (relative, contents) in integration.files {
        let target = request.output_dir.join(relative);
        if write_if_different(&target, contents.as_bytes())? {
            updated += 1;
        }
    }

    Ok(updated)
}
```

### UI統合とフィードバック

TUIでのパッケージ操作時、以下のUIフィードバックを提供します：

#### 操作中のステータス表示

```
╭────────────────────────────────────────────────────────╮
│  Adding Packages...                                    │
├────────────────────────────────────────────────────────┤
│  ✓ Updating jv.toml                                   │
│  🔄 Resolving dependencies...                          │
│  ⏳ Generating pom.xml...                              │
│  ⏳ Syncing local repository...                        │
├────────────────────────────────────────────────────────┤
│  Progress: [████████░░░░░░░░] 60%                     │
╰────────────────────────────────────────────────────────╯
```

#### 完了後のサマリー

```
╭────────────────────────────────────────────────────────╮
│  ✅ Packages Added Successfully                        │
├────────────────────────────────────────────────────────┤
│  Updated Files:                                        │
│    ✓ jv.toml (2 dependencies added)                   │
│    ✓ jv.lock (8 resolved dependencies)                │
│    ✓ pom.xml (regenerated)                            │
│    ✓ classpath.txt (updated)                          │
│                                                         │
│  Downloaded:                                           │
│    • 5 artifacts (2.3 MB)                             │
│    • 3 POM files                                       │
│                                                         │
│  Build Status:                                         │
│    ✓ Ready for `jv build`                             │
│    ✓ Maven-compatible                                  │
├────────────────────────────────────────────────────────┤
│  Enter Continue                                        │
╰────────────────────────────────────────────────────────╯
```

### エラーハンドリング

双方向ファイル管理で発生しうるエラーと対処方法：

```rust
#[derive(Debug, thiserror::Error)]
pub enum ExportError {
    #[error("Failed to update manifest: {0}")]
    ManifestUpdate(String),

    #[error("Dependency resolution failed: {0}")]
    DependencyResolution(String),

    #[error("Maven file generation failed: {0}")]
    MavenGeneration(String),

    #[error("Repository sync failed: {0}")]
    RepositorySync(#[from] std::io::Error),

    #[error("Lockfile is inconsistent with manifest")]
    InconsistentLockfile,
}
```

#### エラー表示UI

```
╭────────────────────────────────────────────────────────╮
│  ❌ Export Failed                                       │
├────────────────────────────────────────────────────────┤
│  Error: Dependency resolution failed                   │
│                                                         │
│  Details:                                              │
│    Cannot resolve version conflict:                    │
│    • spring-boot-starter:3.4.0 requires               │
│      spring-core:6.2.0                                 │
│    • hibernate-core:6.3.0 requires                    │
│      spring-core:6.1.x                                 │
│                                                         │
│  Rollback Status:                                      │
│    ✓ jv.toml restored                                 │
│    ✓ jv.lock unchanged                                │
│    ✓ pom.xml unchanged                                │
├────────────────────────────────────────────────────────┤
│  r Retry | e Edit Versions | Esc Cancel               │
╰────────────────────────────────────────────────────────╯
```

### 同期戦略

#### 自動同期トリガー

以下の操作で自動的にExportが実行されます：

1. **パッケージ追加**: `Space` → `Enter` で選択パッケージを追加
2. **パッケージ削除**: `r` キーで依存関係を削除
3. **パッケージ更新**: `u` / `U` キーでアップデート
4. **一括操作**: 複数パッケージの同時追加/更新

#### 手動同期コマンド

```bash
# TUI内での強制同期
jvpm tui --sync

# CLI経由での同期
jvpm export
```

### ファイル整合性チェック

TUI起動時に以下の整合性チェックを実行します：

```rust
pub async fn verify_project_integrity(
    project_root: &Path,
) -> Result<IntegrityStatus> {
    let manifest = load_manifest(project_root)?;
    let lockfile = load_lockfile(project_root)?;
    let pom_exists = project_root.join("pom.xml").exists();

    // jv.toml と jv.lock の整合性
    if !lockfile.is_consistent_with(&manifest) {
        return Ok(IntegrityStatus::LockfileOutdated);
    }

    // pom.xml の存在チェック
    if !pom_exists {
        return Ok(IntegrityStatus::MavenFileMissing);
    }

    // pom.xml と jv.lock の整合性
    let pom = parse_pom(project_root)?;
    if !pom.matches_lockfile(&lockfile) {
        return Ok(IntegrityStatus::MavenFileOutdated);
    }

    Ok(IntegrityStatus::Consistent)
}
```

#### 整合性警告UI

```
╭────────────────────────────────────────────────────────╮
│  ⚠️ Project Files Inconsistent                         │
├────────────────────────────────────────────────────────┤
│  Status:                                               │
│    ✓ jv.toml: OK                                      │
│    ⚠️ jv.lock: Outdated (run resolver)                │
│    ⚠️ pom.xml: Missing (needs regeneration)           │
│                                                         │
│  Recommended Action:                                   │
│    Run synchronization to update all files            │
├────────────────────────────────────────────────────────┤
│  s Sync Now | c Continue Anyway | q Quit              │
╰────────────────────────────────────────────────────────╯
```

### パフォーマンス最適化

#### 増分更新

完全再生成を避け、変更部分のみを更新します：

```rust
pub async fn incremental_export(
    request: &ExportRequest<'_>,
    changes: &[PackageChange],
) -> Result<()> {
    // 変更の影響範囲を分析
    let affected = analyze_change_impact(changes)?;

    if affected.requires_full_resolve {
        // 完全な依存関係解決が必要
        full_export(request).await?;
    } else {
        // 増分更新で対応可能
        update_manifest_incrementally(request, changes)?;
        update_pom_incrementally(request, &affected)?;
        update_classpath_incrementally(request, &affected)?;
    }

    Ok(())
}
```

#### バックグラウンド処理

重い操作をバックグラウンドで実行し、UIをブロックしません：

```rust
pub async fn async_export_with_progress(
    request: ExportRequest<'static>,
    progress_tx: mpsc::Sender<ExportProgress>,
) -> Result<()> {
    tokio::spawn(async move {
        // Phase 1: Manifest update
        progress_tx.send(ExportProgress::ManifestUpdate).await?;
        update_manifest(&request)?;

        // Phase 2: Dependency resolution
        progress_tx.send(ExportProgress::Resolving).await?;
        let resolved = resolve_dependencies(&request).await?;

        // Phase 3: Maven generation
        progress_tx.send(ExportProgress::GeneratingMaven).await?;
        generate_maven_files(&request, &resolved)?;

        // Phase 4: Repository sync
        progress_tx.send(ExportProgress::SyncingRepo).await?;
        sync_local_repository(&request)?;

        progress_tx.send(ExportProgress::Complete).await?;
        Ok(())
    });
}
```

### TUIデータ構造への統合

`AppState`に双方向ファイル管理の状態を追加します：

```rust
#[derive(Debug)]
pub struct AppState {
    pub current_screen: Screen,
    pub selection_state: SelectionState,
    pub search_state: SearchState,
    pub category_state: CategoryState,
    pub dependency_state: DependencyState,

    // 双方向ファイル管理
    pub export_state: ExportState,
    pub integrity_status: IntegrityStatus,

    pub error_message: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExportState {
    Idle,
    Exporting {
        phase: ExportPhase,
        progress: u8, // 0-100
    },
    Complete {
        updated_files: Vec<PathBuf>,
        duration: Duration,
    },
    Failed {
        error: String,
        rollback_successful: bool,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExportPhase {
    ManifestUpdate,
    DependencyResolution,
    MavenGeneration,
    RepositorySync,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IntegrityStatus {
    Consistent,
    LockfileOutdated,
    MavenFileMissing,
    MavenFileOutdated,
    ManifestCorrupted,
}
```

## 実装ロードマップ

### Phase 1: 基礎実装
- [ ] Ratatuiのセットアップ
- [ ] 基本的なナビゲーション（メインメニュー）
- [ ] 状態管理アーキテクチャ
- [ ] キーボードイベント処理

### Phase 2: Maven統合（Week 2-3）
- [ ] Maven Search API クライアント
- [ ] パッケージ検索UI
- [ ] 検索結果リスト表示
- [ ] パッケージ詳細ビュー

### Phase 3: 選択機能（Week 3-4）
- [ ] 複数選択ロジック
- [ ] 選択状態の永続化
- [ ] 選択サマリー表示
- [ ] jv.toml への書き込み

### Phase 4: カテゴリブラウザ（Week 4-5）
- [ ] カテゴリツリーデータ構造
- [ ] ツリービューウィジェット
- [ ] カテゴリ定義ファイルの読み込み
- [ ] カテゴリ展開/折りたたみ

### Phase 5: 機能ベース選択（Week 5-6）
- [ ] 機能マッピングシステム
- [ ] 機能選択UI
- [ ] パッケージ推奨エンジン
- [ ] 推奨パッケージリスト表示

### Phase 6: 依存関係管理（Week 6-7）
- [ ] 現在の依存関係読み込み
- [ ] 依存関係ツリー表示
- [ ] 削除/更新機能
- [ ] 依存関係解決の統合

### Phase 7: ポリッシュ（Week 7-8）
- [ ] エラーハンドリング改善
- [ ] パフォーマンス最適化
- [ ] ヘルプ画面
- [ ] 設定画面

## 参考リソース

### Ratatui Examples
- https://github.com/ratatui-org/ratatui/tree/main/examples
- https://ratatui.rs/concepts/
- https://ratatui.rs/how-to/

### Maven API
- https://search.maven.org/classic/#api
- https://central.sonatype.org/search/rest-api-guide/

### 類似ツール
- `cargo add` (Cargo TUI: https://github.com/cargo-bins/cargo-binstall)
- `npm` / `yarn` interactive mode
- `lazygit` (TUI architecture reference)

## 付録: Maven Search API仕様

### エンドポイント

```
GET https://search.maven.org/solrsearch/select
```

### クエリパラメータ

| パラメータ | 説明 | 例 |
|-----------|------|-----|
| `q` | 検索クエリ | `q=spring-boot` |
| `rows` | 結果数 | `rows=20` |
| `start` | オフセット | `start=0` |
| `wt` | レスポンス形式 | `wt=json` |

### クエリ構文

```
# Artifact IDで検索
q=a:junit

# Group ID + Artifact ID
q=g:org.springframework.boot AND a:spring-boot-starter

# 全文検索
q=spring boot web

# バージョンも含む
q=g:junit AND a:junit AND v:4.13.2
```

### レスポンス例

```json
{
  "responseHeader": {
    "status": 0,
    "QTime": 5
  },
  "response": {
    "numFound": 150,
    "start": 0,
    "docs": [
      {
        "id": "org.springframework.boot:spring-boot-starter",
        "g": "org.springframework.boot",
        "a": "spring-boot-starter",
        "latestVersion": "3.4.0",
        "repositoryId": "central",
        "p": "jar",
        "timestamp": 1700000000000,
        "versionCount": 85,
        "text": ["org.springframework.boot", "spring-boot-starter"],
        "ec": ["-javadoc.jar", "-sources.jar", ".jar", ".pom"]
      }
    ]
  }
}
```
