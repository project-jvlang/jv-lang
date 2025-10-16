//! Type inference prelude registrations for standard library symbols.
//!
//! The prelude seeds the type environment with well-known symbols so that end-user
//! programs can rely on stdlib facilities without explicitly compiling the stdlib
//! sources as part of the same unit. At the moment we focus on the rewritten
//! Sequence API (`SequenceCore`, the factory helpers, and their extension functions).

use crate::inference::environment::{TypeEnvironment, TypeScheme};
use crate::inference::extensions::ExtensionRegistry;
use crate::inference::type_factory::TypeFactory;
use crate::inference::types::{TypeId, TypeKind};

const SEQUENCE_CORE: &str = "jv.collections.SequenceCore";
const ITERABLE: &str = "java.lang.Iterable";
const ITERATOR: &str = "java.util.Iterator";
const STREAM: &str = "java.util.stream.Stream";
const LIST: &str = "java.util.List";
const MAP: &str = "java.util.Map";
const MAP_ENTRY: &str = "java.util.Map.Entry";
const BOOLEAN: &str = "Boolean";
const INT: &str = "Int";
const LONG: &str = "Long";
const UNIT: &str = "Unit";

fn primitive(name: &'static str) -> TypeKind {
    TypeFactory::from_annotation(name).unwrap_or_else(|_| TypeKind::reference(name))
}

fn function(params: Vec<TypeKind>, return_ty: TypeKind) -> TypeKind {
    TypeKind::function(params, return_ty)
}

fn type_var(id: TypeId) -> TypeKind {
    TypeKind::Variable(id)
}

fn scheme(vars: Vec<TypeId>, ty: TypeKind) -> TypeScheme {
    TypeScheme::new(vars, ty)
}

/// Install built-in symbols and extension functions into the provided environment.
pub fn install_prelude(env: &mut TypeEnvironment) -> ExtensionRegistry {
    let mut registry = ExtensionRegistry::new();
    register_sequence_symbols(env, &mut registry);
    registry
}

fn register_sequence_symbols(env: &mut TypeEnvironment, registry: &mut ExtensionRegistry) {
    register_sequence_factory_functions(env);
    register_sequence_core_extensions(env, registry);
    register_iterable_extensions(env, registry);
}

fn register_sequence_factory_functions(env: &mut TypeEnvironment) {
    let from_iterable = TypeScheme::monotype(function(
        vec![primitive(ITERABLE)],
        primitive(SEQUENCE_CORE),
    ));
    env.define_scheme("sequenceFromIterable", from_iterable);

    let from_stream =
        TypeScheme::monotype(function(vec![primitive(STREAM)], primitive(SEQUENCE_CORE)));
    env.define_scheme("sequenceFromStream", from_stream);
}

fn register_sequence_core_extensions(env: &mut TypeEnvironment, registry: &mut ExtensionRegistry) {
    registry.register(
        SEQUENCE_CORE,
        "iterator",
        TypeScheme::monotype(function(Vec::new(), primitive(ITERATOR))),
    );

    registry.register(
        SEQUENCE_CORE,
        "close",
        TypeScheme::monotype(function(Vec::new(), primitive(UNIT))),
    );

    registry.register(
        SEQUENCE_CORE,
        "toStream",
        TypeScheme::monotype(function(Vec::new(), primitive(STREAM))),
    );

    // map: (T -> R) -> SequenceCore
    let map_t = env.fresh_type_id();
    let map_r = env.fresh_type_id();
    let map_transform = function(vec![type_var(map_t)], type_var(map_r));
    registry.register(
        SEQUENCE_CORE,
        "map",
        scheme(
            vec![map_t, map_r],
            function(vec![map_transform], primitive(SEQUENCE_CORE)),
        ),
    );

    // filter: (T -> Boolean) -> SequenceCore
    let filter_t = env.fresh_type_id();
    let filter_predicate = function(vec![type_var(filter_t)], primitive(BOOLEAN));
    registry.register(
        SEQUENCE_CORE,
        "filter",
        scheme(
            vec![filter_t],
            function(vec![filter_predicate], primitive(SEQUENCE_CORE)),
        ),
    );

    // take/drop: Int -> SequenceCore
    let take_sig = TypeScheme::monotype(function(vec![primitive(INT)], primitive(SEQUENCE_CORE)));
    registry.register(SEQUENCE_CORE, "take", take_sig.clone());
    registry.register(SEQUENCE_CORE, "drop", take_sig);

    // flatMap: (T -> ?) -> SequenceCore
    let flat_map_t = env.fresh_type_id();
    let flat_map_r = env.fresh_type_id();
    let flat_map_transform = function(vec![type_var(flat_map_t)], TypeKind::Unknown);
    registry.register(
        SEQUENCE_CORE,
        "flatMap",
        scheme(
            vec![flat_map_t, flat_map_r],
            function(vec![flat_map_transform], primitive(SEQUENCE_CORE)),
        ),
    );

    // sorted / sortedBy
    registry.register(
        SEQUENCE_CORE,
        "sorted",
        TypeScheme::monotype(function(Vec::new(), primitive(SEQUENCE_CORE))),
    );
    let sorted_by_t = env.fresh_type_id();
    let sorted_by_key = env.fresh_type_id();
    let sorted_selector = function(vec![type_var(sorted_by_t)], type_var(sorted_by_key));
    registry.register(
        SEQUENCE_CORE,
        "sortedBy",
        scheme(
            vec![sorted_by_t, sorted_by_key],
            function(vec![sorted_selector], primitive(SEQUENCE_CORE)),
        ),
    );

    // toList
    registry.register(
        SEQUENCE_CORE,
        "toList",
        TypeScheme::monotype(function(Vec::new(), primitive(LIST))),
    );

    // fold: R, (R, T) -> R
    let fold_t = env.fresh_type_id();
    let fold_r = env.fresh_type_id();
    let fold_op = function(vec![type_var(fold_r), type_var(fold_t)], type_var(fold_r));
    registry.register(
        SEQUENCE_CORE,
        "fold",
        scheme(
            vec![fold_t, fold_r],
            function(vec![type_var(fold_r), fold_op], type_var(fold_r)),
        ),
    );

    // reduce: (T, T) -> T
    let reduce_t = env.fresh_type_id();
    let reduce_op = function(
        vec![type_var(reduce_t), type_var(reduce_t)],
        type_var(reduce_t),
    );
    registry.register(
        SEQUENCE_CORE,
        "reduce",
        scheme(
            vec![reduce_t],
            function(vec![reduce_op], type_var(reduce_t)),
        ),
    );

    registry.register(
        SEQUENCE_CORE,
        "count",
        TypeScheme::monotype(function(Vec::new(), primitive(LONG))),
    );

    registry.register(
        SEQUENCE_CORE,
        "sum",
        TypeScheme::monotype(function(Vec::new(), primitive(LONG))),
    );

    let for_each_t = env.fresh_type_id();
    let for_each_action = function(vec![type_var(for_each_t)], primitive(UNIT));
    registry.register(
        SEQUENCE_CORE,
        "forEach",
        scheme(
            vec![for_each_t],
            function(vec![for_each_action], primitive(UNIT)),
        ),
    );

    let group_by_t = env.fresh_type_id();
    let group_by_key = env.fresh_type_id();
    let group_by_selector = function(vec![type_var(group_by_t)], type_var(group_by_key));
    registry.register(
        SEQUENCE_CORE,
        "groupBy",
        scheme(
            vec![group_by_t, group_by_key],
            function(vec![group_by_selector], primitive(MAP)),
        ),
    );

    let associate_t = env.fresh_type_id();
    let associate_k = env.fresh_type_id();
    let associate_v = env.fresh_type_id();
    let associate_transform = function(vec![type_var(associate_t)], primitive(MAP_ENTRY));
    registry.register(
        SEQUENCE_CORE,
        "associate",
        scheme(
            vec![associate_t, associate_k, associate_v],
            function(vec![associate_transform], primitive(MAP)),
        ),
    );
}

fn register_iterable_extensions(env: &mut TypeEnvironment, registry: &mut ExtensionRegistry) {
    let iterable_map_t = env.fresh_type_id();
    let iterable_map_r = env.fresh_type_id();
    let iterable_map_transform = function(vec![type_var(iterable_map_t)], type_var(iterable_map_r));
    registry.register(
        ITERABLE,
        "map",
        scheme(
            vec![iterable_map_t, iterable_map_r],
            function(vec![iterable_map_transform], primitive(SEQUENCE_CORE)),
        ),
    );

    let iterable_filter_t = env.fresh_type_id();
    let iterable_filter_predicate = function(vec![type_var(iterable_filter_t)], primitive(BOOLEAN));
    registry.register(
        ITERABLE,
        "filter",
        scheme(
            vec![iterable_filter_t],
            function(vec![iterable_filter_predicate], primitive(SEQUENCE_CORE)),
        ),
    );

    let iterable_take_sig =
        TypeScheme::monotype(function(vec![primitive(INT)], primitive(SEQUENCE_CORE)));
    registry.register(ITERABLE, "take", iterable_take_sig.clone());
    registry.register(ITERABLE, "drop", iterable_take_sig);

    let iterable_flat_map_t = env.fresh_type_id();
    let iterable_flat_map_r = env.fresh_type_id();
    let iterable_flat_map_transform =
        function(vec![type_var(iterable_flat_map_t)], TypeKind::Unknown);
    registry.register(
        ITERABLE,
        "flatMap",
        scheme(
            vec![iterable_flat_map_t, iterable_flat_map_r],
            function(vec![iterable_flat_map_transform], primitive(SEQUENCE_CORE)),
        ),
    );

    registry.register(
        ITERABLE,
        "sorted",
        TypeScheme::monotype(function(Vec::new(), primitive(SEQUENCE_CORE))),
    );

    let iterable_sorted_by_t = env.fresh_type_id();
    let iterable_sorted_by_key = env.fresh_type_id();
    let iterable_selector = function(
        vec![type_var(iterable_sorted_by_t)],
        type_var(iterable_sorted_by_key),
    );
    registry.register(
        ITERABLE,
        "sortedBy",
        scheme(
            vec![iterable_sorted_by_t, iterable_sorted_by_key],
            function(vec![iterable_selector], primitive(SEQUENCE_CORE)),
        ),
    );

    let iterable_fold_t = env.fresh_type_id();
    let iterable_fold_r = env.fresh_type_id();
    let iterable_fold_op = function(
        vec![type_var(iterable_fold_r), type_var(iterable_fold_t)],
        type_var(iterable_fold_r),
    );
    registry.register(
        ITERABLE,
        "fold",
        scheme(
            vec![iterable_fold_t, iterable_fold_r],
            function(
                vec![type_var(iterable_fold_r), iterable_fold_op],
                type_var(iterable_fold_r),
            ),
        ),
    );

    let iterable_reduce_t = env.fresh_type_id();
    let iterable_reduce_op = function(
        vec![type_var(iterable_reduce_t), type_var(iterable_reduce_t)],
        type_var(iterable_reduce_t),
    );
    registry.register(
        ITERABLE,
        "reduce",
        scheme(
            vec![iterable_reduce_t],
            function(vec![iterable_reduce_op], type_var(iterable_reduce_t)),
        ),
    );

    registry.register(
        ITERABLE,
        "count",
        TypeScheme::monotype(function(Vec::new(), primitive(LONG))),
    );

    registry.register(
        ITERABLE,
        "sum",
        TypeScheme::monotype(function(Vec::new(), primitive(LONG))),
    );

    let iterable_for_each_t = env.fresh_type_id();
    let iterable_for_each_action = function(vec![type_var(iterable_for_each_t)], primitive(UNIT));
    registry.register(
        ITERABLE,
        "forEach",
        scheme(
            vec![iterable_for_each_t],
            function(vec![iterable_for_each_action], primitive(UNIT)),
        ),
    );

    let iterable_group_by_t = env.fresh_type_id();
    let iterable_group_by_key = env.fresh_type_id();
    let iterable_group_selector = function(
        vec![type_var(iterable_group_by_t)],
        type_var(iterable_group_by_key),
    );
    registry.register(
        ITERABLE,
        "groupBy",
        scheme(
            vec![iterable_group_by_t, iterable_group_by_key],
            function(vec![iterable_group_selector], primitive(MAP)),
        ),
    );

    let iterable_associate_t = env.fresh_type_id();
    let iterable_associate_k = env.fresh_type_id();
    let iterable_associate_v = env.fresh_type_id();
    let iterable_associate_transform =
        function(vec![type_var(iterable_associate_t)], primitive(MAP_ENTRY));
    registry.register(
        ITERABLE,
        "associate",
        scheme(
            vec![
                iterable_associate_t,
                iterable_associate_k,
                iterable_associate_v,
            ],
            function(vec![iterable_associate_transform], primitive(MAP)),
        ),
    );
}
