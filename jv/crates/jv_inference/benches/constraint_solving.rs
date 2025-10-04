use std::time::Instant;

use jv_ast::Span;
use jv_inference::cache::ConstraintCache;
use jv_inference::constraint::{
    ConstraintGraph, ConstraintKind, GenericConstraint, GenericConstraintKind, NullabilitySummary,
    WhereConstraintSummary,
};
use jv_inference::solver::{Constraint, ConstraintSet, ConstraintSolver};
use jv_inference::types::{BoundPredicate, SymbolId, TraitBound, TypeId};

fn main() {
    let scenarios = [
        ("small", 10usize, 5usize),
        ("medium", 100usize, 20usize),
        ("large", 1000usize, 50usize),
    ];

    for (name, constraint_count, type_params) in scenarios {
        run_scenario(name, constraint_count, type_params);
    }
}

fn run_scenario(label: &str, constraint_count: usize, type_params: usize) {
    let symbol = SymbolId::from(format!("bench::Scenario::{label}"));
    let type_ids: Vec<TypeId> = (0..type_params)
        .map(|idx| TypeId::new(idx as u32))
        .collect();
    let constraints = build_constraints(&symbol, &type_ids, constraint_count);

    let mut graph = ConstraintGraph::new();
    let summary = graph.add_where_constraints(&constraints);
    let nullability = clone_summary(&summary);

    let mut cache = ConstraintCache::new();
    let (first_solution, hit_first) =
        jv_inference::constraint::ConstraintSolution::from_generic_constraints_cached(
            symbol.clone(),
            &constraints,
            nullability.clone(),
            Vec::new(),
            Some(&mut cache),
        );
    assert!(!hit_first, "first evaluation should compute solution");

    let (_, hit_second) =
        jv_inference::constraint::ConstraintSolution::from_generic_constraints_cached(
            symbol.clone(),
            &constraints,
            nullability,
            Vec::new(),
            Some(&mut cache),
        );
    assert!(hit_second, "second evaluation should hit the cache");

    let mut constraint_set = ConstraintSet::new();
    populate_constraint_set(&mut constraint_set, &type_ids);

    let start = Instant::now();
    let outcome = ConstraintSolver::new()
        .solve(constraint_set, Some(&graph))
        .expect("solver to succeed");
    let elapsed = start.elapsed();

    let cache_metrics = cache.metrics();
    println!(
        "{label}: constraints={} params={} solve_ms={:.2} cache_entries={} pruned={} preserved={} capability_bindings={}",
        constraint_count,
        type_params,
        elapsed.as_secs_f64() * 1000.0,
        cache_metrics.lookups.saturating_sub(cache_metrics.misses),
        outcome.telemetry.pruned_constraints,
        outcome.telemetry.preserved_constraints,
        first_solution.capability_bindings.len()
    );
}

fn build_constraints(
    symbol: &SymbolId,
    type_ids: &[TypeId],
    constraint_count: usize,
) -> Vec<GenericConstraint> {
    let mut result = Vec::with_capacity(constraint_count);
    for idx in 0..constraint_count {
        let type_param = type_ids[idx % type_ids.len()];
        let trait_name = format!("Trait{}", idx % 13);
        result.push(GenericConstraint::new(
            GenericConstraintKind::BoundRequirement {
                owner: symbol.clone(),
                parameter: type_param,
                predicate: BoundPredicate::Trait(TraitBound::simple(trait_name)),
            },
            Span::dummy(),
        ));
    }
    result
}

fn populate_constraint_set(set: &mut ConstraintSet, type_ids: &[TypeId]) {
    for window in type_ids.windows(2) {
        if let [left, right] = window {
            set.push(Constraint::new(ConstraintKind::Equality {
                left: *left,
                right: *right,
            }));
        }
    }

    if let Some(first) = type_ids.first() {
        for id in type_ids.iter().skip(1).take(3) {
            set.push(Constraint::new(ConstraintKind::Assignment {
                from: *first,
                to: *id,
            }));
        }
    }
}

fn clone_summary(summary: &WhereConstraintSummary) -> NullabilitySummary {
    let mut nullability = NullabilitySummary::default();
    for (param, _) in summary.nullable_parameters() {
        nullability.mark_nullable(param);
    }
    nullability
}
