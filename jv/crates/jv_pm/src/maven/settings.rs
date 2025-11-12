use std::io::Cursor;
use std::path::Path;

use quick_xml::Writer;
use quick_xml::events::{BytesDecl, BytesEnd, BytesStart, BytesText, Event};
use thiserror::Error;

use super::{MavenMirrorConfig, MavenRepositoryConfig};

#[derive(Debug)]
pub struct SettingsGenerationRequest<'a> {
    pub local_repository: &'a Path,
    pub repositories: &'a [MavenRepositoryConfig],
    pub mirrors: &'a [MavenMirrorConfig],
}

pub fn generate_settings_xml(
    request: &SettingsGenerationRequest<'_>,
) -> Result<String, SettingsGenerationError> {
    let mut writer = Writer::new_with_indent(Cursor::new(Vec::new()), b' ', 2);
    writer.write_event(Event::Decl(BytesDecl::new("1.0", Some("UTF-8"), None)))?;

    let mut settings = BytesStart::new("settings");
    settings.push_attribute(("xmlns", "http://maven.apache.org/SETTINGS/1.0.0"));
    settings.push_attribute(("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance"));
    settings.push_attribute((
        "xsi:schemaLocation",
        "http://maven.apache.org/SETTINGS/1.0.0 http://maven.apache.org/xsd/settings-1.0.0.xsd",
    ));
    writer.write_event(Event::Start(settings))?;

    write_simple(
        &mut writer,
        "localRepository",
        &request.local_repository.to_string_lossy(),
    )?;

    if !request.mirrors.is_empty() {
        writer.write_event(Event::Start(BytesStart::new("mirrors")))?;
        for mirror in request.mirrors {
            writer.write_event(Event::Start(BytesStart::new("mirror")))?;
            write_simple(&mut writer, "id", &mirror.id)?;
            write_simple(&mut writer, "mirrorOf", &mirror.mirror_of)?;
            write_simple(&mut writer, "url", &mirror.url)?;
            writer.write_event(Event::End(BytesEnd::new("mirror")))?;
        }
        writer.write_event(Event::End(BytesEnd::new("mirrors")))?;
    }

    writer.write_event(Event::Start(BytesStart::new("profiles")))?;
    writer.write_event(Event::Start(BytesStart::new("profile")))?;
    write_simple(&mut writer, "id", "jv-repositories")?;

    writer.write_event(Event::Start(BytesStart::new("repositories")))?;
    for repo in request.repositories {
        writer.write_event(Event::Start(BytesStart::new("repository")))?;
        write_simple(&mut writer, "id", &repo.id)?;
        if let Some(name) = repo.name.as_deref() {
            if !name.trim().is_empty() {
                write_simple(&mut writer, "name", name)?;
            }
        }
        write_simple(&mut writer, "url", &repo.url)?;

        writer.write_event(Event::Start(BytesStart::new("releases")))?;
        write_simple(
            &mut writer,
            "enabled",
            if repo.releases_enabled {
                "true"
            } else {
                "false"
            },
        )?;
        writer.write_event(Event::End(BytesEnd::new("releases")))?;

        writer.write_event(Event::Start(BytesStart::new("snapshots")))?;
        write_simple(
            &mut writer,
            "enabled",
            if repo.snapshots_enabled {
                "true"
            } else {
                "false"
            },
        )?;
        writer.write_event(Event::End(BytesEnd::new("snapshots")))?;

        writer.write_event(Event::End(BytesEnd::new("repository")))?;
    }
    writer.write_event(Event::End(BytesEnd::new("repositories")))?;
    writer.write_event(Event::End(BytesEnd::new("profile")))?;
    writer.write_event(Event::End(BytesEnd::new("profiles")))?;

    writer.write_event(Event::Start(BytesStart::new("activeProfiles")))?;
    write_simple(&mut writer, "activeProfile", "jv-repositories")?;
    writer.write_event(Event::End(BytesEnd::new("activeProfiles")))?;

    writer.write_event(Event::End(BytesEnd::new("settings")))?;

    let bytes = writer.into_inner().into_inner();
    let mut xml = String::from_utf8(bytes)?;
    if !xml.ends_with('\n') {
        xml.push('\n');
    }
    Ok(xml)
}

fn write_simple(
    writer: &mut Writer<Cursor<Vec<u8>>>,
    tag: &str,
    value: impl AsRef<str>,
) -> Result<(), SettingsGenerationError> {
    writer.write_event(Event::Start(BytesStart::new(tag)))?;
    writer.write_event(Event::Text(BytesText::new(value.as_ref())))?;
    writer.write_event(Event::End(BytesEnd::new(tag)))?;
    Ok(())
}

#[derive(Debug, Error)]
pub enum SettingsGenerationError {
    #[error("settings.xml の書き込み中にIOエラーが発生しました: {0}")]
    Io(#[from] std::io::Error),
    #[error("settings.xml のXML生成に失敗しました: {0}")]
    Writer(#[from] quick_xml::Error),
    #[error("settings.xml をUTF-8文字列へ変換できません: {0}")]
    Utf8(#[from] std::string::FromUtf8Error),
}
