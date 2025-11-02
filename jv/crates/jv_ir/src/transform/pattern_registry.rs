use crate::types::{
    IrExpression, IrModifiers, IrStatement, IrVisibility, JavaType, PatternStaticFieldHandle,
};
use jv_ast::{PatternConstKey, RegexFlag, Span};
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct RegistryEntry {
    pattern: String,
    flags: Vec<RegexFlag>,
    handle: PatternStaticFieldHandle,
}

/// 正規表現定数を静的フィールドへ割り当てるためのレジストリ。
#[derive(Debug, Default, Clone)]
pub struct PatternConstRegistry {
    entries: HashMap<[u8; 16], RegistryEntry>,
    counter: usize,
    class_name: String,
}

impl PatternConstRegistry {
    const DEFAULT_CLASS_NAME: &'static str = "__JVPatternCache";

    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
            counter: 0,
            class_name: Self::DEFAULT_CLASS_NAME.to_string(),
        }
    }

    pub fn clear(&mut self) {
        self.entries.clear();
        self.counter = 0;
    }

    pub fn register(
        &mut self,
        pattern: &str,
        flags: &[RegexFlag],
        const_key: &PatternConstKey,
    ) -> PatternStaticFieldHandle {
        let key = const_key.hash;
        if let Some(entry) = self.entries.get(&key) {
            return entry.handle.clone();
        }

        self.counter += 1;
        let field_name = format!("PATTERN_{}", self.counter);
        let mut sorted_flags = flags.to_vec();
        sorted_flags.sort_by_key(|flag| flag_mask_value(flag));
        let entry = RegistryEntry {
            pattern: pattern.to_string(),
            flags: sorted_flags,
            handle: PatternStaticFieldHandle {
                class_name: self.class_name.clone(),
                field_name: field_name.clone(),
            },
        };
        let handle = entry.handle.clone();
        self.entries.insert(key, entry);
        handle
    }

    pub fn take_registry_class(&mut self) -> Option<IrStatement> {
        if self.entries.is_empty() {
            return None;
        }

        let mut fields: Vec<IrStatement> = self
            .entries
            .values()
            .map(|entry| make_field(entry))
            .collect();
        fields.sort_by(|a, b| field_name(a).cmp(field_name(b)));

        let mut class_modifiers = IrModifiers::default();
        class_modifiers.visibility = IrVisibility::Public;
        class_modifiers.is_final = true;

        let class = IrStatement::ClassDeclaration {
            name: self.class_name.clone(),
            type_parameters: Vec::new(),
            superclass: None,
            interfaces: Vec::new(),
            fields,
            methods: Vec::new(),
            nested_classes: Vec::new(),
            modifiers: class_modifiers,
            span: Span::dummy(),
        };

        self.clear();
        Some(class)
    }

    pub fn class_name(&self) -> &str {
        &self.class_name
    }
}

fn make_field(entry: &RegistryEntry) -> IrStatement {
    let mut modifiers = IrModifiers::default();
    modifiers.visibility = IrVisibility::Public;
    modifiers.is_static = true;
    modifiers.is_final = true;

    let initializer = IrExpression::RegexPattern {
        pattern: entry.pattern.clone(),
        flags: entry.flags.clone(),
        java_type: JavaType::pattern(),
        span: Span::dummy(),
        const_key: None,
        static_handle: None,
    };

    IrStatement::FieldDeclaration {
        name: entry.handle.field_name.clone(),
        java_type: JavaType::pattern(),
        initializer: Some(initializer),
        modifiers,
        span: Span::dummy(),
    }
}

fn field_name(statement: &IrStatement) -> &str {
    match statement {
        IrStatement::FieldDeclaration { name, .. } => name,
        _ => "",
    }
}

fn flag_mask_value(flag: &RegexFlag) -> u8 {
    match flag {
        RegexFlag::CaseInsensitive => 0,
        RegexFlag::Multiline => 1,
        RegexFlag::DotAll => 2,
        RegexFlag::UnicodeCase => 3,
        RegexFlag::UnixLines => 4,
        RegexFlag::Comments => 5,
        RegexFlag::Literal => 6,
        RegexFlag::CanonEq => 7,
    }
}
