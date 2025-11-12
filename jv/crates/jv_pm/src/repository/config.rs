use serde::{Deserialize, Deserializer, Serialize, Serializer, de, ser::Error as SerError};
use std::borrow::Cow;

/// リポジトリ定義。
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RepositoryConfig {
    /// リポジトリ識別子。
    pub name: String,
    /// ベースURL（file:// を含む完全URL）。
    pub url: String,
    /// 優先度（小さいほど高優先度）。
    #[serde(default = "default_priority")]
    pub priority: u32,
    /// 認証設定。
    #[serde(skip_serializing_if = "Option::is_none")]
    pub auth: Option<AuthConfig>,
    /// グループフィルタ設定。
    #[serde(skip_serializing_if = "Option::is_none")]
    pub filter: Option<FilterConfig>,
}

fn default_priority() -> u32 {
    100
}

/// 認証設定。
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct AuthConfig {
    /// 認証種別。
    #[serde(rename = "type")]
    #[serde(default)]
    pub auth_type: AuthType,
    /// ユーザー名を格納した環境変数名。
    #[serde(default, rename = "username-env")]
    pub username_env: Option<String>,
    /// パスワードを格納した環境変数名。
    #[serde(default, rename = "password-env")]
    pub password_env: Option<String>,
    /// トークンを格納した環境変数名。
    #[serde(default, rename = "token-env")]
    pub token_env: Option<String>,
}

impl AuthConfig {
    pub fn is_none(&self) -> bool {
        matches!(self.auth_type, AuthType::None)
            && self.username_env.is_none()
            && self.password_env.is_none()
            && self.token_env.is_none()
    }
}

/// 認証方式。
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum AuthType {
    None,
    Basic,
    Token,
}

impl Default for AuthType {
    fn default() -> Self {
        AuthType::None
    }
}

/// グループフィルタ。
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct FilterConfig {
    /// 許可するグループIDパターン。
    #[serde(default, rename = "include-groups")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub include_groups: Option<Vec<String>>,
    /// 除外するグループIDパターン。
    #[serde(default, rename = "exclude-groups")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub exclude_groups: Option<Vec<String>>,
}

/// ミラー設定。
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct MirrorConfig {
    /// ミラーの表示名。
    #[serde(default)]
    pub name: Option<String>,
    /// 対象となるリポジトリ識別子（ワイルドカード対応）。
    #[serde(rename = "mirror-of")]
    pub mirror_of: String,
    /// ミラーURL。
    pub url: String,
}

impl MirrorConfig {
    pub fn resolved_name(&self) -> Cow<'_, str> {
        self.name
            .as_ref()
            .map(|s| Cow::Borrowed(s.as_str()))
            .unwrap_or_else(|| Cow::Owned(self.mirror_of.clone()))
    }
}

/// マニフェストに定義されるリポジトリセクション。
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RepositorySection {
    pub use_global: bool,
    pub entries: Vec<RepositoryConfig>,
}

impl Default for RepositorySection {
    fn default() -> Self {
        Self {
            use_global: true,
            entries: Vec::new(),
        }
    }
}

impl RepositorySection {
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty() && self.use_global
    }
}

impl Serialize for RepositorySection {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut table = toml::value::Table::new();
        if self.use_global {
            table.insert("use-global".to_string(), toml::Value::Boolean(true));
        }
        if !self.entries.is_empty() {
            let entries_value = toml::Value::try_from(&self.entries)
                .map_err(|err| SerError::custom(err.to_string()))?;
            table.insert("entries".to_string(), entries_value);
        }

        toml::Value::Table(table).serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for RepositorySection {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value = toml::Value::deserialize(deserializer)?;

        match value {
            toml::Value::Array(items) => {
                let entries = items
                    .into_iter()
                    .map(RepositoryConfig::try_from)
                    .collect::<Result<Vec<_>, _>>()
                    .map_err(de::Error::custom)?;
                Ok(Self {
                    use_global: false,
                    entries,
                })
            }
            toml::Value::Table(mut table) => {
                let use_global = table
                    .remove("use-global")
                    .or_else(|| table.remove("use_global"))
                    .map(|v| v.try_into::<bool>().map_err(de::Error::custom))
                    .transpose()?
                    .unwrap_or(false);

                let entries_value = table
                    .remove("entries")
                    .or_else(|| table.remove("repositories"))
                    .unwrap_or_else(|| toml::Value::Array(Vec::new()));

                let entries = match entries_value {
                    toml::Value::Array(items) => items
                        .into_iter()
                        .map(RepositoryConfig::try_from)
                        .collect::<Result<Vec<_>, _>>()
                        .map_err(de::Error::custom)?,
                    other => {
                        return Err(de::Error::custom(format!(
                            "entries フィールドは配列である必要があります (種類: {})",
                            describe_kind(&other)
                        )));
                    }
                };

                Ok(Self {
                    use_global,
                    entries,
                })
            }
            other => Err(de::Error::custom(format!(
                "repositories セクションの形式が不正です (種類: {})",
                describe_kind(&other)
            ))),
        }
    }
}

impl TryFrom<toml::Value> for RepositoryConfig {
    type Error = toml::de::Error;

    fn try_from(value: toml::Value) -> Result<Self, Self::Error> {
        RepositoryConfig::deserialize(value)
    }
}

fn describe_kind(value: &toml::Value) -> &'static str {
    match value {
        toml::Value::Boolean(_) => "bool",
        toml::Value::Integer(_) => "integer",
        toml::Value::Float(_) => "float",
        toml::Value::String(_) => "string",
        toml::Value::Datetime(_) => "datetime",
        toml::Value::Array(_) => "array",
        toml::Value::Table(_) => "table",
    }
}
