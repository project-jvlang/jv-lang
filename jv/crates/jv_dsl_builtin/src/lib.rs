//! 組み込みDSLプラグインのスタブ実装。
//!
//! Phase1ではコンパイルを通すための最小骨格のみ。

#[cfg(any(
    feature = "dsl-log",
    feature = "dsl-io",
    feature = "dsl-lock",
    feature = "dsl-cron",
    feature = "dsl-assert"
))]
use jv_dsl_api::{BlockPlugin, DslPlugin, GlobalOperatorPlugin, TokenPlugin};

#[cfg(feature = "dsl-log")]
pub mod log {
    use super::*;

    pub struct LogPlugin;

    impl DslPlugin for LogPlugin {
        fn name(&self) -> &'static str {
            "log"
        }

        fn registered_keywords(&self) -> &'static [&'static str] {
            &["LOG"]
        }
    }

    impl BlockPlugin for LogPlugin {}
}

#[cfg(feature = "dsl-io")]
pub mod io {
    use super::*;

    pub struct IoPlugin;

    impl DslPlugin for IoPlugin {
        fn name(&self) -> &'static str {
            "io"
        }

        fn registered_keywords(&self) -> &'static [&'static str] {
            &["IO"]
        }
    }

    impl BlockPlugin for IoPlugin {}
}

#[cfg(feature = "dsl-lock")]
pub mod lock {
    use super::*;

    pub struct LockPlugin;

    impl DslPlugin for LockPlugin {
        fn name(&self) -> &'static str {
            "lock"
        }

        fn registered_keywords(&self) -> &'static [&'static str] {
            &["LOCK"]
        }
    }

    impl BlockPlugin for LockPlugin {}
}

#[cfg(feature = "dsl-cron")]
pub mod cron {
    use super::*;

    pub struct CronPlugin;

    impl DslPlugin for CronPlugin {
        fn name(&self) -> &'static str {
            "cron"
        }

        fn registered_keywords(&self) -> &'static [&'static str] {
            &["CRON"]
        }
    }

    impl GlobalOperatorPlugin for CronPlugin {}
}

#[cfg(feature = "dsl-assert")]
pub mod assert_dsl {
    use super::*;

    pub struct AssertPlugin;

    impl DslPlugin for AssertPlugin {
        fn name(&self) -> &'static str {
            "assert"
        }

        fn registered_keywords(&self) -> &'static [&'static str] {
            &["ASSERT"]
        }
    }

    impl TokenPlugin for AssertPlugin {}
}
