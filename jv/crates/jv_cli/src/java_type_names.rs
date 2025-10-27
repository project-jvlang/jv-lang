pub fn derive_type_name(type_decl: &str) -> Option<String> {
    let mut tokens = type_decl.split_whitespace().peekable();

    while let Some(token) = tokens.next() {
        match token {
            "class" | "record" | "interface" | "enum" => {
                let name_token = tokens.next()?;
                return Some(clean_type_token(name_token));
            }
            "sealed" | "non-sealed" => continue,
            _ => continue,
        }
    }

    None
}

fn clean_type_token(raw: &str) -> String {
    let trimmed = raw.trim_matches(|c: char| c == '{' || c == ';');
    let split = trimmed
        .split(|c| matches!(c, '<' | '('))
        .next()
        .unwrap_or(trimmed);
    split.trim().to_string()
}

#[cfg(test)]
mod tests {
    use super::derive_type_name;

    #[test]
    fn extracts_class_name_with_modifiers() {
        let decl = "public final class Sample { }";
        assert_eq!(
            derive_type_name(decl),
            Some("Sample".to_string()),
            "expected to find class name"
        );
    }

    #[test]
    fn extracts_record_name_with_parameters() {
        let decl = "private record User(String name, int age) {}";
        assert_eq!(
            derive_type_name(decl),
            Some("User".to_string()),
            "expected to find record name"
        );
    }

    #[test]
    fn extracts_interface_name_with_annotations() {
        let decl = "@FunctionalInterface\npublic interface Mapper<T> { T map(); }";
        assert_eq!(
            derive_type_name(decl),
            Some("Mapper".to_string()),
            "expected to find interface name"
        );
    }

    #[test]
    fn extracts_enum_name_with_body() {
        let decl = "enum Direction { NORTH, SOUTH }";
        assert_eq!(
            derive_type_name(decl),
            Some("Direction".to_string()),
            "expected to find enum name"
        );
    }

    #[test]
    fn returns_none_when_no_type_keyword_present() {
        assert_eq!(derive_type_name("System.out.println(\"hi\");"), None);
    }
}
