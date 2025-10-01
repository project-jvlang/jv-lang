use std::collections::{BTreeMap, BTreeSet, HashMap};

/// Catalog describing the relationship between constructors, enum constants,
/// and their enclosing types. The analyzer uses this to provide
/// exhaustiveness-related suggestions without hardcoding type knowledge.
#[derive(Debug, Default, Clone)]
pub struct PatternTypeCatalog {
    sealed_types: BTreeMap<String, BTreeSet<String>>,
    variant_to_sealed: HashMap<String, String>,
    enums: BTreeMap<String, BTreeSet<String>>,
    constant_to_enum: HashMap<String, String>,
}

impl PatternTypeCatalog {
    pub fn register_sealed_type(
        &mut self,
        type_name: String,
        variants: impl Iterator<Item = String>,
    ) {
        let mut set = BTreeSet::new();
        for variant in variants {
            self.variant_to_sealed
                .insert(variant.clone(), type_name.clone());
            set.insert(variant);
        }
        self.sealed_types.insert(type_name, set);
    }

    pub fn register_enum(&mut self, enum_name: String, constants: impl Iterator<Item = String>) {
        let mut set = BTreeSet::new();
        for constant in constants {
            self.constant_to_enum
                .insert(constant.clone(), enum_name.clone());
            set.insert(constant);
        }
        self.enums.insert(enum_name, set);
    }

    pub fn sealed_variants(&self, type_name: &str) -> Option<&BTreeSet<String>> {
        self.sealed_types.get(type_name)
    }

    pub fn sealed_base_for_variant(&self, variant: &str) -> Option<&str> {
        self.variant_to_sealed.get(variant).map(String::as_str)
    }

    pub fn enum_constants(&self, enum_name: &str) -> Option<&BTreeSet<String>> {
        self.enums.get(enum_name)
    }

    pub fn enum_for_constant(&self, constant: &str) -> Option<&str> {
        self.constant_to_enum.get(constant).map(String::as_str)
    }
}
