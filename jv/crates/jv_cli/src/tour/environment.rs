use std::fmt;

use jv_build::{BuildConfig, BuildError, BuildSystem};

/// Java major version required for the language tour.
const REQUIRED_JAVA_MAJOR: u8 = 25;

/// Summary of the user's JDK environment state.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JdkStatus {
    /// javac is available and meets the required major version.
    Ready { version: String, major: u8 },
    /// javac is present but the detected version is below the requirement.
    Outdated {
        version: String,
        detected_major: Option<u8>,
        required_major: u8,
    },
    /// javac could not be located on PATH.
    NotInstalled { message: String },
    /// An unexpected error occurred while probing the toolchain.
    Error { message: String },
}

impl JdkStatus {
    pub fn is_ready(&self) -> bool {
        matches!(self, JdkStatus::Ready { .. })
    }
}

/// Supported operating systems for the setup guide.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperatingSystem {
    Windows,
    MacOs,
    Linux,
    Unknown,
}

impl OperatingSystem {
    pub fn current() -> Self {
        match std::env::consts::OS {
            "windows" => OperatingSystem::Windows,
            "macos" => OperatingSystem::MacOs,
            "linux" => OperatingSystem::Linux,
            _ => OperatingSystem::Unknown,
        }
    }
}

impl fmt::Display for OperatingSystem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OperatingSystem::Windows => write!(f, "Windows"),
            OperatingSystem::MacOs => write!(f, "macOS"),
            OperatingSystem::Linux => write!(f, "Linux"),
            OperatingSystem::Unknown => write!(f, "Unknown"),
        }
    }
}

/// Recommended JDK distributions communicated to the learner.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Distribution<'a> {
    pub name: &'a str,
    pub url: &'a str,
    pub notes: &'a str,
}

/// Step-by-step instructions for installing a JDK.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SetupGuide<'a> {
    pub os: OperatingSystem,
    pub headline: &'a str,
    pub steps: Vec<&'a str>,
    pub post_install_checks: Vec<&'a str>,
    pub distributions: Vec<Distribution<'a>>,
}

/// Execute the flow that ensures the JDK toolchain is available for the tour.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnvironmentReport<'a> {
    pub status: JdkStatus,
    pub os: OperatingSystem,
    pub guide: SetupGuide<'a>,
    pub completion_message: &'a str,
}

/// Abstraction over how javac availability is probed so we can test the logic deterministically.
pub trait JdkProbe {
    fn probe(&self) -> Result<String, BuildError>;
}

#[derive(Debug, Default, Clone, Copy)]
pub struct BuildSystemProbe;

impl JdkProbe for BuildSystemProbe {
    fn probe(&self) -> Result<String, BuildError> {
        let build_system = BuildSystem::new(BuildConfig::default());
        build_system.check_javac_availability()
    }
}

/// Central coordinator for environment detection and guidance.
#[derive(Debug, Clone)]
pub struct EnvironmentManager<P: JdkProbe = BuildSystemProbe> {
    probe: P,
    required_major: u8,
}

impl EnvironmentManager<BuildSystemProbe> {
    pub fn new() -> Self {
        Self {
            probe: BuildSystemProbe,
            required_major: REQUIRED_JAVA_MAJOR,
        }
    }
}

impl<P: JdkProbe> EnvironmentManager<P> {
    pub fn with_probe(probe: P) -> Self {
        Self {
            probe,
            required_major: REQUIRED_JAVA_MAJOR,
        }
    }

    pub fn with_probe_and_requirement(probe: P, required_major: u8) -> Self {
        Self {
            probe,
            required_major,
        }
    }

    /// Check the current JDK environment.
    pub fn check_jdk(&self) -> JdkStatus {
        interpret_probe_result(self.probe.probe(), self.required_major)
    }

    /// Re-run the probe to confirm installation after the learner completes setup steps.
    pub fn verify_installation(&self) -> JdkStatus {
        self.check_jdk()
    }

    /// Build an instruction set tailored to the detected operating system.
    pub fn setup_guide(&self, os: OperatingSystem) -> SetupGuide<'static> {
        guide_for(os)
    }

    /// Produce a summary that downstream CLI code can render.
    pub fn build_report(&self) -> EnvironmentReport<'static> {
        let os = OperatingSystem::current();
        let status = self.check_jdk();
        let guide = self.setup_guide(os);
        EnvironmentReport {
            status,
            os,
            guide,
            completion_message: "JDKセットアップが完了しました。メインメニューに戻ります...",
        }
    }
}

fn interpret_probe_result(result: Result<String, BuildError>, required_major: u8) -> JdkStatus {
    match result {
        Ok(version_line) => match parse_java_major(&version_line) {
            Some(major) if major >= required_major => JdkStatus::Ready {
                version: version_line,
                major,
            },
            detected_major => JdkStatus::Outdated {
                version: version_line,
                detected_major,
                required_major,
            },
        },
        Err(BuildError::JdkNotFound(message)) => JdkStatus::NotInstalled { message },
        Err(err) => JdkStatus::Error {
            message: err.to_string(),
        },
    }
}

fn parse_java_major(version_line: &str) -> Option<u8> {
    let token = version_line.split_whitespace().nth(1).unwrap_or_default();

    let digits: String = token.chars().take_while(|ch| ch.is_ascii_digit()).collect();

    if digits.is_empty() {
        None
    } else {
        digits.parse().ok()
    }
}

fn guide_for(os: OperatingSystem) -> SetupGuide<'static> {
    match os {
        OperatingSystem::Windows => SetupGuide {
            os,
            headline: "Windows向けJDKセットアップ",
            steps: vec![
                "1. Microsoft Store版は使用せず、公式配布からフルJDKを取得します",
                "2. TemurinまたはOracleのJDK 25 (x64) インストーラをダウンロードします",
                "3. インストーラを管理者権限で実行し、\"Set JAVA_HOME\" オプションを有効にします",
                "4. インストール後にコントロールパネル > システム > 詳細設定でJAVA_HOMEとPATHを確認します",
            ],
            post_install_checks: default_post_install_checks(),
            distributions: recommended_distributions(),
        },
        OperatingSystem::MacOs => SetupGuide {
            os,
            headline: "macOS向けJDKセットアップ",
            steps: vec![
                "1. Homebrewを利用できる場合は `brew install --cask temurin@25` を実行します",
                "2. もしくはOracle公式サイトからmacOS (x64/ARM) 向けのDMGをダウンロードします",
                "3. インストーラに従いJDKをインストール後、`/usr/libexec/java_home -V` で候補を確認します",
                "4. シェルのプロファイル(~/.zshrcなど)に `export JAVA_HOME=\"$(/usr/libexec/java_home -v 25)\"` を追加します",
            ],
            post_install_checks: default_post_install_checks(),
            distributions: recommended_distributions(),
        },
        OperatingSystem::Linux => SetupGuide {
            os,
            headline: "Linux向けJDKセットアップ",
            steps: vec![
                "1. ディストリビューションに合わせてパッケージマネージャーを利用します",
                "   - Debian/Ubuntu: `sudo apt install temurin-25-jdk`",
                "   - Fedora/RHEL: `sudo dnf install temurin-25-jdk`",
                "   - Arch: `sudo pacman -S temurin-25-jdk`",
                "2. 公式バイナリを使用する場合はtar.gzを展開し、`/opt/java`などに配置します",
                "3. `/etc/profile.d/java.sh`等にJAVA_HOMEとPATHの設定を追加します",
                "4. シェルを再読み込みし設定を反映させます",
            ],
            post_install_checks: default_post_install_checks(),
            distributions: recommended_distributions(),
        },
        OperatingSystem::Unknown => SetupGuide {
            os,
            headline: "JDKセットアップガイド",
            steps: vec![
                "1. OSに対応したTemurinまたはOracle JDK 25をダウンロードします",
                "2. インストール手順に従い、JAVA_HOMEとPATHを設定します",
                "3. ターミナルを再起動して環境変数が適用されたことを確認します",
            ],
            post_install_checks: default_post_install_checks(),
            distributions: recommended_distributions(),
        },
    }
}

fn default_post_install_checks() -> Vec<&'static str> {
    vec![
        "1. 新しいターミナルを開き、環境変数が反映されているか確認します",
        "2. `javac --version` を実行してバージョンを確認します",
        "3. 出力が `javac 25` 以上であることを確認します",
        "4. 必要に応じて `echo %JAVA_HOME%` / `echo $JAVA_HOME` でパスを確認します",
        "5. JDKが検出されたらツアーのメインメニューに自動復帰します",
    ]
}

fn recommended_distributions() -> Vec<Distribution<'static>> {
    vec![
        Distribution {
            name: "Eclipse Temurin",
            url: "https://adoptium.net/temurin/releases/?version=25",
            notes: "無償・長期サポート。企業利用に向いた安定版。",
        },
        Distribution {
            name: "Oracle JDK",
            url: "https://www.oracle.com/java/technologies/downloads/",
            notes: "商用サポートが必要な場合に利用。インストール時のライセンス条項に注意。",
        },
        Distribution {
            name: "GraalVM",
            url: "https://www.graalvm.org/downloads/",
            notes: "ネイティブイメージを検証したい場合に適した高度なディストリビューション。",
        },
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    struct MockProbe<F>
    where
        F: Fn() -> Result<String, BuildError>,
    {
        inner: F,
    }

    impl<F> MockProbe<F>
    where
        F: Fn() -> Result<String, BuildError>,
    {
        fn new(inner: F) -> Self {
            Self { inner }
        }
    }

    impl<F> JdkProbe for MockProbe<F>
    where
        F: Fn() -> Result<String, BuildError>,
    {
        fn probe(&self) -> Result<String, BuildError> {
            (self.inner)()
        }
    }

    #[test]
    fn parse_major_version_handles_ea_suffix() {
        assert_eq!(parse_java_major("javac 25-ea"), Some(25));
        assert_eq!(parse_java_major("javac 17.0.8"), Some(17));
        assert_eq!(parse_java_major("javac"), None);
    }

    #[test]
    fn ready_status_for_supported_version() {
        let manager =
            EnvironmentManager::with_probe(MockProbe::new(|| Ok("javac 25.0.1".to_string())));

        let status = manager.check_jdk();
        assert!(matches!(status, JdkStatus::Ready { major: 25, .. }));
    }

    #[test]
    fn outdated_status_for_older_version() {
        let manager =
            EnvironmentManager::with_probe(MockProbe::new(|| Ok("javac 17.0.8".to_string())));

        let status = manager.check_jdk();
        match status {
            JdkStatus::Outdated {
                detected_major,
                required_major,
                ..
            } => {
                assert_eq!(detected_major, Some(17));
                assert_eq!(required_major, REQUIRED_JAVA_MAJOR);
            }
            other => panic!("unexpected status: {other:?}"),
        }
    }

    #[test]
    fn missing_status_when_not_found() {
        let manager = EnvironmentManager::with_probe(MockProbe::new(|| {
            Err(BuildError::JdkNotFound("javac command not found".into()))
        }));

        let status = manager.check_jdk();
        assert!(matches!(status, JdkStatus::NotInstalled { .. }));
    }

    #[test]
    fn guide_contains_post_install_steps() {
        let manager = EnvironmentManager::with_probe(MockProbe::new(|| {
            Err(BuildError::JdkNotFound("test".into()))
        }));

        let guide = manager.setup_guide(OperatingSystem::Windows);
        assert_eq!(guide.os, OperatingSystem::Windows);
        assert!(guide
            .post_install_checks
            .iter()
            .any(|step| step.contains("javac --version")));
        assert_eq!(guide.distributions.len(), 3);
    }

    #[test]
    fn environment_report_includes_completion_message() {
        let manager = EnvironmentManager::with_probe(MockProbe::new(|| Ok("javac 25".to_string())));
        let report = manager.build_report();
        assert!(report.completion_message.contains("メインメニュー"));
        assert!(report.guide.steps.len() >= 3);
    }
}
