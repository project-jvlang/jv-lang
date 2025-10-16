//! 型推論におけるスコープと型スキームを管理する環境。
//!
//! `TypeEnvironment` は val/var などのシンボルに対応する型スキームを保持し、
//! スコープ境界の push/pop、型変数の払い出し、一般化（generalization）と
//! インスタンス化（instantiation）を提供する。これにより制約ジェネレータや
//! 単一化ソルバが決定的で借用しやすい API を通じて推論状態へアクセスできる。

use crate::inference::types::{TypeId, TypeKind};
use crate::inference::utils::TypeIdGenerator;
use std::collections::{HashMap, HashSet};

/// 汎用型を表現する型スキーム。
#[derive(Debug, Clone, PartialEq)]
pub struct TypeScheme {
    /// スキームが束縛する型変数の集合。
    pub quantifiers: Vec<TypeId>,
    /// 本体となる型表現。
    pub ty: TypeKind,
}

impl TypeScheme {
    /// 任意の量化子と型本体からスキームを構築する。
    pub fn new(mut quantifiers: Vec<TypeId>, ty: TypeKind) -> Self {
        quantifiers.sort_by_key(|id| id.to_raw());
        quantifiers.dedup();
        Self { quantifiers, ty }
    }

    /// モノタイプ（量化子なし）としてスキームを作成する。
    pub fn monotype(ty: TypeKind) -> Self {
        Self {
            quantifiers: Vec::new(),
            ty,
        }
    }

    /// スキームが多相かどうかを判定する。
    pub fn is_polymorphic(&self) -> bool {
        !self.quantifiers.is_empty()
    }
}

/// スコープごとの型スキームと型変数IDを管理する。
#[derive(Debug, Default, Clone)]
pub struct TypeEnvironment {
    scopes: Vec<HashMap<String, TypeScheme>>,
    generator: TypeIdGenerator,
    instantiation_origins: HashMap<TypeId, TypeId>,
}

impl TypeEnvironment {
    /// 新しい環境を構築する。グローバルスコープを1つ持った状態で初期化する。
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            generator: TypeIdGenerator::new(),
            instantiation_origins: HashMap::new(),
        }
    }

    /// 現在のスコープ深度を取得する。
    pub fn depth(&self) -> usize {
        self.scopes.len()
    }

    /// スコープを1段深くする。
    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    /// スコープを1段戻る。グローバルスコープは破棄しない。
    pub fn leave_scope(&mut self) -> bool {
        if self.scopes.len() > 1 {
            self.scopes.pop();
            true
        } else {
            false
        }
    }

    /// 現在のスコープへ型スキームを登録する。
    pub fn define_scheme(&mut self, name: impl Into<String>, scheme: TypeScheme) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.into(), scheme);
        }
    }

    /// モノタイプを渡してシンボルを登録するユーティリティ。
    pub fn define_monotype(&mut self, name: impl Into<String>, ty: TypeKind) {
        self.define_scheme(name, TypeScheme::monotype(ty));
    }

    /// 既存のシンボルを新しいスキームで再定義する。見つからない場合はグローバルスコープへ登録する。
    pub fn redefine_scheme(&mut self, name: impl AsRef<str>, scheme: TypeScheme) {
        let name_ref = name.as_ref();
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name_ref) {
                scope.insert(name_ref.to_string(), scheme);
                return;
            }
        }
        self.define_scheme(name_ref.to_string(), scheme);
    }

    /// もっとも内側のスコープから順にシンボルを探索する。
    pub fn lookup(&self, name: &str) -> Option<&TypeScheme> {
        self.scopes.iter().rev().find_map(|scope| scope.get(name))
    }

    /// 新しい型変数IDを払い出す。
    pub fn fresh_type_id(&mut self) -> TypeId {
        self.generator.allocate()
    }

    /// 型変数を表す `TypeKind` を生成する。
    pub fn fresh_type_variable(&mut self) -> TypeKind {
        TypeKind::Variable(self.fresh_type_id())
    }

    /// 型を一般化し、環境に閉じた型スキームへ変換する。
    pub fn generalize(&self, ty: TypeKind) -> TypeScheme {
        let mut free_vars = HashSet::new();
        ty.collect_free_type_vars_into(&mut free_vars);

        let env_free_vars = self.environment_free_type_vars();
        let mut quantifiers: Vec<TypeId> = free_vars
            .into_iter()
            .filter(|id| !env_free_vars.contains(id))
            .collect();
        quantifiers.sort_by_key(|id| id.to_raw());

        TypeScheme::new(quantifiers, ty)
    }

    /// 型スキームをインスタンス化し、新しい型を生成する。
    pub fn instantiate(&mut self, scheme: &TypeScheme) -> TypeKind {
        let substitutions: HashMap<TypeId, TypeKind> = scheme
            .quantifiers
            .iter()
            .map(|id| {
                let fresh = self.fresh_type_id();
                self.instantiation_origins.insert(fresh, *id);
                (*id, TypeKind::Variable(fresh))
            })
            .collect();

        substitute_type(&scheme.ty, &substitutions)
    }

    /// すべてのスコープを走査し、可視なシンボルとその型スキームを収集する。
    pub fn flattened_bindings(&self) -> HashMap<String, TypeScheme> {
        let mut merged = HashMap::new();
        for scope in &self.scopes {
            for (name, scheme) in scope {
                merged.insert(name.clone(), scheme.clone());
            }
        }
        merged
    }

    /// 環境全体で自由な型変数を収集する。
    fn environment_free_type_vars(&self) -> HashSet<TypeId> {
        let mut acc = HashSet::new();
        for scope in &self.scopes {
            for scheme in scope.values() {
                let mut scheme_free = HashSet::new();
                scheme.ty.collect_free_type_vars_into(&mut scheme_free);
                for quantifier in &scheme.quantifiers {
                    scheme_free.remove(quantifier);
                }
                acc.extend(scheme_free);
            }
        }
        acc
    }

    /// Follows instantiation mappings to locate the origin quantifier for a type variable, if any.
    pub fn type_origin(&self, id: TypeId) -> Option<TypeId> {
        let mut current = id;
        let mut visited = HashSet::new();
        while let Some(origin) = self.instantiation_origins.get(&current) {
            if !visited.insert(current) {
                break;
            }
            current = *origin;
        }
        if current != id {
            Some(current)
        } else {
            self.instantiation_origins.get(&id).copied()
        }
    }
}

fn substitute_type(ty: &TypeKind, subs: &HashMap<TypeId, TypeKind>) -> TypeKind {
    match ty {
        TypeKind::Primitive(primitive) => TypeKind::Primitive(*primitive),
        TypeKind::Boxed(primitive) => TypeKind::Boxed(*primitive),
        TypeKind::Reference(name) => TypeKind::Reference(name.clone()),
        TypeKind::Optional(inner) => TypeKind::Optional(Box::new(substitute_type(inner, subs))),
        TypeKind::Variable(id) => subs
            .get(id)
            .cloned()
            .unwrap_or_else(|| TypeKind::Variable(*id)),
        TypeKind::Function(params, ret) => TypeKind::Function(
            params
                .iter()
                .map(|param| substitute_type(param, subs))
                .collect(),
            Box::new(substitute_type(ret, subs)),
        ),
        TypeKind::Unknown => TypeKind::Unknown,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::inference::types::PrimitiveType;

    #[test]
    fn scope_push_and_pop_respects_shadowing() {
        let mut env = TypeEnvironment::new();
        env.define_monotype("x", TypeKind::primitive(PrimitiveType::Int));
        assert!(env.lookup("x").is_some());

        env.enter_scope();
        env.define_monotype("y", TypeKind::primitive(PrimitiveType::Boolean));
        assert!(env.lookup("y").is_some());

        env.leave_scope();
        assert!(env.lookup("y").is_none());
        assert!(env.lookup("x").is_some());
    }

    #[test]
    fn generalize_respects_environment_free_variables() {
        let mut env = TypeEnvironment::new();
        let shared_id = env.fresh_type_id();
        env.define_scheme(
            "existing",
            TypeScheme::monotype(TypeKind::Variable(shared_id)),
        );

        let scheme = env.generalize(TypeKind::Variable(shared_id));
        assert!(scheme.quantifiers.is_empty());
    }

    #[test]
    fn instantiate_produces_fresh_type_variables() {
        let mut env = TypeEnvironment::new();
        let alpha = env.fresh_type_id();
        let scheme = env.generalize(TypeKind::Variable(alpha));

        let first = env.instantiate(&scheme);
        let second = env.instantiate(&scheme);

        match (first, second) {
            (TypeKind::Variable(a), TypeKind::Variable(b)) => {
                assert_ne!(a.to_raw(), b.to_raw());
            }
            _ => panic!("expected type variables"),
        }
    }
}
