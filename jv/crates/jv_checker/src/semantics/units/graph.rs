use super::conversion::{self, EdgeConversionPlan};
use super::registry::{
    ReverseMode, UnitCategoryEntry, UnitCategoryId, UnitConversionBody, UnitEdge, UnitEdgeId,
    UnitEntry, UnitId, UnitLookupKey, UnitRegistry,
};
use super::{
    DefaultUnit, UnitConversionKind, UnitDefinitionValidated, UnitMemberRaw, UnitSymbolRaw,
    ValidatedCatalog,
};
use crate::CheckError;
use crate::diagnostics::unit_semantics;
use jv_ast::{Argument, Expression, Span, Statement, StringPart, UnitRelation};
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Clone, Copy, PartialEq, Eq)]
enum VisitState {
    Unvisited,
    Visiting,
    Done,
}

/// `RawUnitCatalog` 検証済みデータから `UnitRegistry` を構築するビルダー。
#[derive(Debug, Default)]
pub struct UnitDependencyGraphBuilder;

impl UnitDependencyGraphBuilder {
    pub fn build(
        catalog: ValidatedCatalog,
        diagnostics: &mut Vec<CheckError>,
    ) -> Option<Arc<UnitRegistry>> {
        let mut builder = RegistryBuilder::new();
        builder.register_categories(&catalog.definitions);
        builder.register_units(&catalog.definitions);
        builder.assign_defaults(&catalog.defaults);
        builder.register_edges(&catalog.definitions, diagnostics);
        if conversion::apply_edge_conversions(
            &builder.edge_conversions,
            &mut builder.edges,
            &builder.units,
            &builder.categories,
            &mut builder.conversions,
            diagnostics,
        ) {
            builder.had_error = true;
        }
        builder.detect_cycles(diagnostics);

        if builder.had_error() {
            return None;
        }

        Some(Arc::new(builder.finish()))
    }
}

#[derive(Debug)]
struct RegistryBuilder {
    categories: Vec<UnitCategoryEntry>,
    category_lookup: HashMap<String, UnitCategoryId>,
    units: Vec<UnitEntry>,
    unit_lookup: HashMap<UnitLookupKey, UnitId>,
    edges: Vec<UnitEdge>,
    adjacency: Vec<Vec<(UnitId, UnitEdgeId)>>,
    conversions: Vec<UnitConversionBody>,
    edge_conversions: HashMap<UnitEdgeId, EdgeConversionPlan>,
    had_error: bool,
}

impl RegistryBuilder {
    fn new() -> Self {
        Self {
            categories: Vec::new(),
            category_lookup: HashMap::new(),
            units: Vec::new(),
            unit_lookup: HashMap::new(),
            edges: Vec::new(),
            adjacency: Vec::new(),
            conversions: Vec::new(),
            edge_conversions: HashMap::new(),
            had_error: false,
        }
    }

    fn had_error(&self) -> bool {
        self.had_error
    }

    fn register_categories(&mut self, definitions: &[UnitDefinitionValidated]) {
        for definition in definitions {
            let name = &definition.definition.category;
            if self.category_lookup.contains_key(name) {
                continue;
            }

            let id = self.categories.len() as UnitCategoryId;
            self.category_lookup.insert(name.clone(), id);
            self.categories.push(UnitCategoryEntry {
                id,
                name: name.clone(),
                spec: definition.category,
                base_type: definition.base_type.clone(),
                base_capability: definition.base_capability,
                default_unit: None,
            });
        }
    }

    fn register_units(&mut self, definitions: &[UnitDefinitionValidated]) {
        for definition in definitions {
            let Some(&category_id) = self.category_lookup.get(&definition.definition.category)
            else {
                continue;
            };

            self.register_symbol(category_id, &definition.definition.symbol);

            for member in &definition.definition.members {
                match member {
                    UnitMemberRaw::SymbolDecl(symbol) => {
                        self.register_symbol(category_id, symbol);
                    }
                    UnitMemberRaw::Dependency(dependency) => {
                        self.register_symbol(category_id, &dependency.symbol);
                    }
                    _ => {}
                }
            }
        }
    }

    fn register_symbol(&mut self, category_id: UnitCategoryId, symbol: &UnitSymbolRaw) -> UnitId {
        let key = UnitLookupKey::new(category_id, symbol.name.clone(), symbol.is_bracketed);
        if let Some(existing) = self.unit_lookup.get(&key) {
            return *existing;
        }

        let id = self.units.len() as UnitId;
        self.unit_lookup.insert(key, id);
        self.units.push(UnitEntry {
            id,
            category_id,
            symbol: symbol.clone(),
            is_default: false,
        });
        self.adjacency.push(Vec::new());
        id
    }

    fn assign_defaults(&mut self, defaults: &HashMap<String, DefaultUnit>) {
        for (category, default_unit) in defaults {
            let Some(&category_id) = self.category_lookup.get(category) else {
                continue;
            };

            if let Some(unit_id) = self.lookup_by_name(category_id, &default_unit.symbol, None) {
                if let Some(entry) = self.units.get_mut(unit_id as usize) {
                    entry.is_default = true;
                }
                if let Some(category_entry) = self.categories.get_mut(category_id as usize) {
                    category_entry.default_unit = Some(unit_id);
                }
            }
        }
    }

    fn register_edges(
        &mut self,
        definitions: &[UnitDefinitionValidated],
        diagnostics: &mut Vec<CheckError>,
    ) {
        for definition in definitions {
            let Some(&category_id) = self.category_lookup.get(&definition.definition.category)
            else {
                continue;
            };

            let mut pending_conversion_edge: Option<UnitEdgeId> = None;

            for member in &definition.definition.members {
                match member {
                    UnitMemberRaw::Dependency(dependency) => {
                        pending_conversion_edge = None;
                        let Some(from_id) = self.lookup_by_symbol(category_id, &dependency.symbol)
                        else {
                            continue;
                        };

                        match dependency.relation {
                            UnitRelation::DefinitionAssign => {
                                if let Some(value) = &dependency.value {
                                    let mut references = Vec::new();
                                    collect_unit_references(value, &mut references);

                                    for reference in references {
                                        self.connect_edge(
                                            category_id,
                                            from_id,
                                            &reference,
                                            dependency.relation,
                                            &dependency.span,
                                            diagnostics,
                                        );
                                    }
                                }
                            }
                            UnitRelation::ConversionArrow => {
                                if let Some(target) = &dependency.target {
                                    let reference =
                                        UnitSymbolRaw::from_identifier(target, &dependency.span);
                                    pending_conversion_edge = self.connect_edge(
                                        category_id,
                                        from_id,
                                        &reference,
                                        dependency.relation,
                                        &dependency.span,
                                        diagnostics,
                                    );
                                }
                            }
                        }
                    }
                    UnitMemberRaw::Conversion(block) => {
                        if let Some(edge_id) = pending_conversion_edge {
                            let entry = self.edge_conversions.entry(edge_id).or_default();
                            match block.kind {
                                UnitConversionKind::Conversion => {
                                    if entry.forward.is_none() {
                                        entry.forward = Some(block.clone());
                                    }
                                }
                                UnitConversionKind::ReverseConversion => {
                                    if entry.reverse.is_none() {
                                        entry.reverse = Some(block.clone());
                                    }
                                }
                            }
                        }
                    }
                    UnitMemberRaw::ConversionRate(rate) => {
                        if let Some(edge_id) = pending_conversion_edge {
                            self.edge_conversions
                                .entry(edge_id)
                                .or_default()
                                .rate
                                .get_or_insert(rate.clone());
                        }
                    }
                    _ => {
                        pending_conversion_edge = None;
                    }
                }
            }
        }
    }

    fn connect_edge(
        &mut self,
        category_id: UnitCategoryId,
        from_id: UnitId,
        target: &UnitSymbolRaw,
        relation: UnitRelation,
        span: &Span,
        diagnostics: &mut Vec<CheckError>,
    ) -> Option<UnitEdgeId> {
        let Some(target_id) = self.lookup_by_symbol(category_id, target) else {
            self.had_error = true;
            emit_dependency_error(
                "JV_UNIT_SEM_020",
                format!(
                    "単位 `{}` から参照された `{}` はカテゴリ `{}` で定義されていません。",
                    self.describe_unit(from_id),
                    format_symbol(target),
                    self.categories[category_id as usize].name
                ),
                span,
                diagnostics,
            );
            return None;
        };

        if target_id == from_id {
            self.had_error = true;
            emit_dependency_error(
                "JV_UNIT_SEM_020",
                format!(
                    "単位 `{}` が自分自身を参照しています。異なる単位を指定してください。",
                    self.describe_unit(from_id)
                ),
                span,
                diagnostics,
            );
            return None;
        }

        Some(self.push_edge(from_id, target_id, relation, span.clone()))
    }

    fn push_edge(
        &mut self,
        from: UnitId,
        to: UnitId,
        relation: UnitRelation,
        span: Span,
    ) -> UnitEdgeId {
        let id = self.edges.len() as UnitEdgeId;
        self.edges.push(UnitEdge {
            id,
            from,
            to,
            relation,
            rate: None,
            reverse_mode: ReverseMode::Unavailable,
            conversion_ref: None,
            span: span.clone(),
        });

        if let Some(bucket) = self.adjacency.get_mut(from as usize) {
            bucket.push((to, id));
        }

        id
    }

    fn detect_cycles(&mut self, diagnostics: &mut Vec<CheckError>) {
        let mut state = vec![VisitState::Unvisited; self.units.len()];
        let mut stack = Vec::new();

        for index in 0..self.units.len() {
            if matches!(state[index], VisitState::Unvisited) {
                if self.visit(index as UnitId, &mut state, &mut stack, diagnostics) {
                    self.had_error = true;
                    break;
                }
            }
        }
    }

    fn visit(
        &self,
        unit_id: UnitId,
        state: &mut [VisitState],
        stack: &mut Vec<UnitId>,
        diagnostics: &mut Vec<CheckError>,
    ) -> bool {
        state[unit_id as usize] = VisitState::Visiting;
        stack.push(unit_id);

        for (next, edge_id) in &self.adjacency[unit_id as usize] {
            match state[*next as usize] {
                VisitState::Unvisited => {
                    if self.visit(*next, state, stack, diagnostics) {
                        return true;
                    }
                }
                VisitState::Visiting => {
                    self.emit_cycle(*edge_id, *next, stack, diagnostics);
                    return true;
                }
                VisitState::Done => {}
            }
        }

        stack.pop();
        state[unit_id as usize] = VisitState::Done;
        false
    }

    fn emit_cycle(
        &self,
        edge_id: UnitEdgeId,
        start: UnitId,
        stack: &[UnitId],
        diagnostics: &mut Vec<CheckError>,
    ) {
        let descriptor =
            unit_semantics::descriptor("JV_UNIT_SEM_030").expect("JV_UNIT_SEM_030 registered");
        let Some(position) = stack.iter().position(|candidate| *candidate == start) else {
            emit_dependency_error(
                descriptor.code,
                "単位依存グラフで循環が検出されました。".to_string(),
                &self.edges[edge_id as usize].span,
                diagnostics,
            );
            return;
        };

        let mut cycle: Vec<String> = stack[position..]
            .iter()
            .map(|unit_id| self.describe_unit(*unit_id))
            .collect();
        cycle.push(self.describe_unit(start));
        let category_name = &self.categories[self.units[start as usize].category_id as usize].name;
        let detail = format!(
            "カテゴリ `{}` で循環依存 `{}` が検出されました。",
            category_name,
            cycle.join(" -> ")
        );

        emit_dependency_error(
            descriptor.code,
            detail,
            &self.edges[edge_id as usize].span,
            diagnostics,
        );
    }

    fn describe_unit(&self, unit_id: UnitId) -> String {
        format_symbol(&self.units[unit_id as usize].symbol)
    }

    fn lookup_by_symbol(
        &self,
        category_id: UnitCategoryId,
        symbol: &UnitSymbolRaw,
    ) -> Option<UnitId> {
        self.lookup_by_name(category_id, &symbol.name, Some(symbol.is_bracketed))
    }

    fn lookup_by_name(
        &self,
        category_id: UnitCategoryId,
        name: &str,
        is_bracketed: Option<bool>,
    ) -> Option<UnitId> {
        match is_bracketed {
            Some(flag) => self
                .unit_lookup
                .get(&UnitLookupKey::new(category_id, name, flag))
                .copied(),
            None => self
                .unit_lookup
                .get(&UnitLookupKey::new(category_id, name, false))
                .copied()
                .or_else(|| {
                    self.unit_lookup
                        .get(&UnitLookupKey::new(category_id, name, true))
                        .copied()
                }),
        }
    }

    fn finish(self) -> UnitRegistry {
        UnitRegistry::new(
            self.categories,
            self.units,
            self.edges,
            self.conversions,
            self.unit_lookup,
            self.category_lookup,
        )
    }
}

fn emit_dependency_error(
    code: &str,
    detail: String,
    span: &Span,
    diagnostics: &mut Vec<CheckError>,
) {
    let descriptor = unit_semantics::descriptor(code).expect("diagnostic must be registered");
    let mut lines = vec![format!("{}: {}", descriptor.code, descriptor.title)];
    if !detail.is_empty() {
        lines.push(detail);
    }
    if !descriptor.help.is_empty() {
        lines.push(descriptor.help.to_string());
    }

    diagnostics.push(CheckError::ValidationError {
        message: lines.join("\n"),
        span: Some(span.clone()),
    });
}

fn collect_unit_references(expr: &Expression, output: &mut Vec<UnitSymbolRaw>) {
    match expr {
        Expression::UnitLiteral { value, unit, .. } => {
            collect_unit_references(value, output);
            output.push(UnitSymbolRaw::from_symbol(unit));
        }
        Expression::Identifier(name, span) => {
            output.push(UnitSymbolRaw::from_identifier(name, span));
        }
        Expression::Binary { left, right, .. } => {
            collect_unit_references(left, output);
            collect_unit_references(right, output);
        }
        Expression::Unary { operand, .. } => collect_unit_references(operand, output),
        Expression::Call { function, args, .. } => {
            collect_unit_references(function, output);
            for arg in args {
                match arg {
                    Argument::Positional(expr) => collect_unit_references(expr, output),
                    Argument::Named { value, .. } => collect_unit_references(value, output),
                }
            }
        }
        Expression::MemberAccess { object, .. }
        | Expression::NullSafeMemberAccess { object, .. } => {
            collect_unit_references(object, output);
        }
        Expression::IndexAccess { object, index, .. }
        | Expression::NullSafeIndexAccess { object, index, .. } => {
            collect_unit_references(object, output);
            collect_unit_references(index, output);
        }
        Expression::TypeCast { expr, .. } => collect_unit_references(expr, output),
        Expression::StringInterpolation { parts, .. } => {
            for part in parts {
                if let StringPart::Expression(inner) = part {
                    collect_unit_references(inner, output);
                }
            }
        }
        Expression::When {
            expr: subject,
            arms,
            else_arm,
            ..
        } => {
            if let Some(subject) = subject {
                collect_unit_references(subject, output);
            }
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_unit_references(guard, output);
                }
                collect_unit_references(&arm.body, output);
            }
            if let Some(fallback) = else_arm {
                collect_unit_references(fallback, output);
            }
        }
        Expression::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            collect_unit_references(condition, output);
            collect_unit_references(then_branch, output);
            if let Some(else_branch) = else_branch {
                collect_unit_references(else_branch, output);
            }
        }
        Expression::Block { statements, .. } => {
            for statement in statements {
                collect_from_statement(statement, output);
            }
        }
        Expression::Literal(_, _)
        | Expression::RegexLiteral(_)
        | Expression::JsonLiteral(_)
        | Expression::MultilineString(_) => {}
        _ => {}
    }
}

fn collect_from_statement(statement: &Statement, output: &mut Vec<UnitSymbolRaw>) {
    match statement {
        Statement::Expression { expr, .. } => collect_unit_references(expr, output),
        Statement::Return { value, .. } => {
            if let Some(expr) = value {
                collect_unit_references(expr, output);
            }
        }
        Statement::ValDeclaration { initializer, .. } => {
            collect_unit_references(initializer, output);
        }
        Statement::VarDeclaration { initializer, .. } => {
            if let Some(expr) = initializer {
                collect_unit_references(expr, output);
            }
        }
        Statement::Assignment { target, value, .. } => {
            collect_unit_references(target, output);
            collect_unit_references(value, output);
        }
        _ => {}
    }
}

pub(super) fn format_symbol(symbol: &UnitSymbolRaw) -> String {
    let mut label = if symbol.is_bracketed {
        format!("[{}]", symbol.name)
    } else {
        symbol.name.clone()
    };

    if symbol.has_default_marker {
        label.push('!');
    }

    label
}
