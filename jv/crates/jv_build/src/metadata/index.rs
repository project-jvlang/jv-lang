use crate::metadata::classfile::{JavaMethodSignature, StaticMember, StaticMemberKind};
use jv_ir::types::JavaType;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

/// Aggregated symbol metadata harvested from the JDK and project classpath.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct SymbolIndex {
    pub jdk_release: Option<u16>,
    pub modules: HashMap<String, ModuleEntry>,
    pub packages: HashMap<String, PackageEntry>,
    pub types: HashMap<String, TypeEntry>,
}

impl SymbolIndex {
    pub fn new(jdk_release: Option<u16>) -> Self {
        Self {
            jdk_release,
            modules: HashMap::new(),
            packages: HashMap::new(),
            types: HashMap::new(),
        }
    }

    pub fn lookup_type(&self, fqcn: &str) -> Option<&TypeEntry> {
        self.types.get(fqcn)
    }

    pub fn lookup_static(&self, owner: &str, member: &str) -> Option<StaticMember> {
        self.types
            .get(owner)
            .and_then(|ty| ty.lookup_static(member))
    }

    pub fn module_of(&self, fqcn: &str) -> Option<&ModuleEntry> {
        self.types
            .get(fqcn)
            .and_then(|ty| ty.module.as_ref())
            .and_then(|module| self.modules.get(module))
    }

    pub fn add_type(&mut self, entry: TypeEntry) {
        self.insert_type(entry);
    }

    pub(crate) fn insert_module(&mut self, module: ModuleEntry) {
        self.modules
            .entry(module.name.clone())
            .and_modify(|entry| entry.merge(&module))
            .or_insert(module);
    }

    pub(crate) fn insert_type(&mut self, entry: TypeEntry) {
        if let Some(pkg) = self.packages.get_mut(&entry.package) {
            pkg.types.insert(entry.fqcn.clone());
        } else {
            let mut pkg = PackageEntry {
                name: entry.package.clone(),
                module: entry.module.clone(),
                types: HashSet::new(),
            };
            pkg.types.insert(entry.fqcn.clone());
            self.packages.insert(entry.package.clone(), pkg);
        }
        self.types.insert(entry.fqcn.clone(), entry);
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModuleEntry {
    pub name: String,
    pub exports: HashSet<String>,
}

impl ModuleEntry {
    pub fn new(name: String) -> Self {
        Self {
            name,
            exports: HashSet::new(),
        }
    }

    pub(crate) fn merge(&mut self, other: &ModuleEntry) {
        self.exports.extend(other.exports.iter().cloned());
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageEntry {
    pub name: String,
    pub module: Option<String>,
    pub types: HashSet<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeEntry {
    pub fqcn: String,
    pub package: String,
    pub module: Option<String>,
    pub static_fields: HashMap<String, JavaType>,
    pub static_methods: HashMap<String, JavaMethodSignature>,
    #[serde(default)]
    pub instance_fields: HashSet<String>,
    #[serde(default)]
    pub instance_methods: HashMap<String, JavaMethodSignature>,
}

impl TypeEntry {
    pub fn new(fqcn: String, package: String, module: Option<String>) -> Self {
        Self {
            fqcn,
            package,
            module,
            static_fields: HashMap::new(),
            static_methods: HashMap::new(),
            instance_fields: HashSet::new(),
            instance_methods: HashMap::new(),
        }
    }

    pub fn add_static_field(&mut self, name: String, ty: JavaType) {
        self.static_fields.insert(name, ty);
    }

    pub fn add_static_method(&mut self, name: String, signature: JavaMethodSignature) {
        self.static_methods.insert(name, signature);
    }

    pub fn add_instance_field(&mut self, name: String) {
        self.instance_fields.insert(name);
    }

    pub fn add_instance_method(&mut self, name: String, signature: JavaMethodSignature) {
        self.instance_methods.insert(name, signature);
    }

    pub fn has_field(&self, name: &str) -> bool {
        self.static_fields.contains_key(name) || self.instance_fields.contains(name)
    }

    pub fn has_instance_method(&self, name: &str) -> bool {
        self.instance_methods.contains_key(name)
    }

    pub fn lookup_static(&self, member: &str) -> Option<StaticMember> {
        if let Some(ty) = self.static_fields.get(member) {
            return Some(StaticMember {
                owner: self.fqcn.clone(),
                member: member.to_string(),
                kind: StaticMemberKind::Field { ty: ty.clone() },
            });
        }

        self.static_methods.get(member).map(|sig| StaticMember {
            owner: self.fqcn.clone(),
            member: member.to_string(),
            kind: StaticMemberKind::Method {
                signature: sig.clone(),
            },
        })
    }
}
