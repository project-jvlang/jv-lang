use crate::constraint::incremental::{AstId, IncrementalConstraintBuilder};
use crate::constraint::{
    ConstraintGraph, ConstraintKind, ConstraintNode, Edge, EdgeKind, GenericConstraint,
    GenericConstraintKind, NodeId,
};
use crate::diagnostics::{ConstructorDiagnostics, ConstructorOrigin};
use crate::types::{SymbolId, TypeId, TypeKind};
use jv_ast::Span;

/// Lightweight metadata describing the collection literal associated with a constructor.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CollectionShape {
    pub element_count: usize,
}

impl CollectionShape {
    pub fn new(element_count: usize) -> Self {
        Self { element_count }
    }

    pub fn is_empty(&self) -> bool {
        self.element_count == 0
    }
}

/// Metadata captured for JSON-derived literals.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct JsonDiagnosticMetadata {
    pub object_keys: usize,
    pub array_entries: usize,
}

impl JsonDiagnosticMetadata {
    pub fn new(object_keys: usize, array_entries: usize) -> Self {
        Self {
            object_keys,
            array_entries,
        }
    }
}

/// Constructor argument description processed by [`ConstructorConstraintBuilder`].
#[derive(Debug, Clone)]
pub struct ConstructorArgument {
    pub type_id: TypeId,
    pub type_hint: TypeKind,
    pub parameter: Option<TypeId>,
    pub parameter_hint: Option<TypeKind>,
    pub label: Option<String>,
    pub span: Span,
}

impl ConstructorArgument {
    pub fn new(type_id: TypeId, type_hint: TypeKind, span: Span) -> Self {
        Self {
            type_id,
            type_hint,
            parameter: None,
            parameter_hint: None,
            label: None,
            span,
        }
    }

    pub fn with_parameter(mut self, parameter: TypeId, hint: Option<TypeKind>) -> Self {
        self.parameter = Some(parameter);
        self.parameter_hint = hint;
        self
    }

    pub fn with_label(mut self, label: impl Into<String>) -> Self {
        self.label = Some(label.into());
        self
    }
}

/// Complete constructor-site description consumed by the builder.
#[derive(Debug, Clone)]
pub struct ConstructorConstraintInput {
    pub ast_id: Option<AstId>,
    pub constructor: SymbolId,
    pub span: Span,
    pub origin: ConstructorOrigin,
    pub arguments: Vec<ConstructorArgument>,
    pub collection_shape: Option<CollectionShape>,
    pub json_metadata: Option<JsonDiagnosticMetadata>,
}

impl ConstructorConstraintInput {
    pub fn new(
        ast_id: Option<AstId>,
        constructor: SymbolId,
        span: Span,
        origin: ConstructorOrigin,
        arguments: Vec<ConstructorArgument>,
    ) -> Self {
        Self {
            ast_id,
            constructor,
            span,
            origin,
            arguments,
            collection_shape: None,
            json_metadata: None,
        }
    }

    pub fn with_collection_shape(mut self, shape: CollectionShape) -> Self {
        self.collection_shape = Some(shape);
        self
    }

    pub fn with_json_metadata(mut self, metadata: JsonDiagnosticMetadata) -> Self {
        self.json_metadata = Some(metadata);
        self
    }
}

/// Result describing emitted constraints, graph updates, and diagnostic seeds.
#[derive(Debug, Clone)]
pub struct ConstructorConstraintResult {
    pub constructor: SymbolId,
    pub span: Span,
    pub constraints: Vec<GenericConstraint>,
    pub recorded_edges: usize,
    pub diagnostics: ConstructorDiagnostics,
}

impl ConstructorConstraintResult {
    pub fn is_empty(&self) -> bool {
        self.constraints.is_empty() && self.recorded_edges == 0
    }
}

/// Builds generic constraints and graph edges for constructor-style initializations.
pub struct ConstructorConstraintBuilder<'graph> {
    graph: &'graph mut ConstraintGraph,
}

impl<'graph> ConstructorConstraintBuilder<'graph> {
    pub fn new(graph: &'graph mut ConstraintGraph) -> Self {
        Self { graph }
    }

    pub fn build(
        &mut self,
        input: ConstructorConstraintInput,
        incremental: Option<&mut IncrementalConstraintBuilder>,
    ) -> ConstructorConstraintResult {
        let mut constraints = Vec::new();
        let mut diagnostics = ConstructorDiagnostics::new();
        let mut recorded_edges = 0usize;
        let mut incremental = incremental;

        if let Some(ast_id) = input.ast_id {
            if let Some(builder) = incremental.as_mut() {
                builder.mark_dirty(ast_id);
            }
        }

        for (index, argument) in input.arguments.iter().enumerate() {
            self.graph
                .ensure_type_node(argument.type_id, Some(argument.type_hint.clone()));

            if let Some(parameter) = argument.parameter {
                let constraint = GenericConstraint::new(
                    GenericConstraintKind::ConstructorArgument {
                        ctor: input.constructor.clone(),
                        parameter,
                        argument: argument.type_hint.clone(),
                        argument_index: index,
                        field: argument.label.clone(),
                    },
                    argument.span.clone(),
                );
                constraints.push(constraint);

                recorded_edges += self.connect_assignment(
                    argument.type_id,
                    Some(argument.type_hint.clone()),
                    parameter,
                    argument.parameter_hint.clone(),
                    &argument.span,
                    &mut incremental,
                );
            }
        }

        if should_record_empty(&input) {
            diagnostics.record_empty_initialization(
                input.constructor.clone(),
                input.span.clone(),
                input.origin,
            );
        }

        if let Some(json) = input.json_metadata {
            diagnostics.record_json_literal_shape(
                input.span.clone(),
                json.object_keys,
                json.array_entries,
            );
        }

        ConstructorConstraintResult {
            constructor: input.constructor,
            span: input.span,
            constraints,
            recorded_edges,
            diagnostics,
        }
    }

    fn connect_assignment(
        &mut self,
        from: TypeId,
        from_hint: Option<TypeKind>,
        to: TypeId,
        to_hint: Option<TypeKind>,
        span: &Span,
        incremental: &mut Option<&mut IncrementalConstraintBuilder>,
    ) -> usize {
        let from_node = self.graph.ensure_type_node(from, from_hint);
        let to_node = self.graph.ensure_type_node(to, to_hint);

        let constraint_id = self
            .graph
            .add_constraint_node(ConstraintNode::new(ConstraintKind::Assignment { from, to }));

        let constraint_node = NodeId::Constraint(constraint_id);
        if self.graph.metadata().get(&constraint_node).is_none() {
            self.graph
                .metadata_mut()
                .insert(constraint_node, span.clone());
        }

        let first_edge = Edge::new(
            NodeId::Type(from_node),
            constraint_node,
            EdgeKind::Assignment,
        );
        let second_edge = Edge::new(constraint_node, NodeId::Type(to_node), EdgeKind::Assignment);

        self.graph
            .add_edge(first_edge.from, first_edge.to, first_edge.kind);
        self.graph
            .add_edge(second_edge.from, second_edge.to, second_edge.kind);

        if let Some(builder) = incremental.as_mut() {
            builder.add_edge(first_edge);
            builder.add_edge(second_edge);
            builder.update_span(constraint_node, span.clone());
        }

        2
    }
}

fn should_record_empty(input: &ConstructorConstraintInput) -> bool {
    let explicit_shape = input.collection_shape.map(|shape| shape.is_empty());
    match explicit_shape {
        Some(true) => true,
        Some(false) => false,
        None => {
            matches!(
                input.origin,
                ConstructorOrigin::CollectionLiteral | ConstructorOrigin::BuilderDsl
            ) && input.arguments.is_empty()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::constraint::incremental::IncrementalConstraintBuilder as IncrBuilder;
    use crate::constraint::ConstraintGraph;
    use crate::diagnostics::ConstructorDiagnosticKind;
    use crate::types::{TypeId, TypeKind, TypeVariant};

    fn dummy_span(start: usize) -> Span {
        Span::new(start, 0, start, 5)
    }

    #[test]
    fn produces_constructor_constraints_and_edges() {
        let mut graph = ConstraintGraph::new();
        let mut incremental = IncrBuilder::new();
        let mut builder = ConstructorConstraintBuilder::new(&mut graph);

        let argument = ConstructorArgument::new(
            TypeId::new(100),
            TypeKind::new(TypeVariant::Primitive("String")),
            dummy_span(2),
        )
        .with_parameter(TypeId::new(5), None)
        .with_label("value");

        let input = ConstructorConstraintInput::new(
            Some(9),
            SymbolId::from("demo::Builder"),
            dummy_span(1),
            ConstructorOrigin::NewExpression,
            vec![argument],
        );

        let result = builder.build(input, Some(&mut incremental));
        assert_eq!(result.constraints.len(), 1);
        assert_eq!(result.recorded_edges, 2);
        assert!(result.diagnostics.is_empty());

        match &result.constraints[0].kind {
            GenericConstraintKind::ConstructorArgument {
                ctor,
                parameter,
                argument_index,
                field,
                ..
            } => {
                assert_eq!(ctor.as_str(), "demo::Builder");
                assert_eq!(*parameter, TypeId::new(5));
                assert_eq!(*argument_index, 0);
                assert_eq!(field.as_deref(), Some("value"));
            }
            GenericConstraintKind::TypeArgument { .. } => {
                panic!("constructor builder must not emit call constraints")
            }
            GenericConstraintKind::BoundRequirement { .. }
            | GenericConstraintKind::VarianceUsage { .. } => {
                panic!("constructor builder must not emit bound or variance constraints")
            }
        }

        let diff = incremental.diff();
        assert_eq!(diff.added_edges.len(), 2);
        assert!(diff.removed_edges.is_empty());
        assert_eq!(graph.constraint_node_count(), 1);
    }

    #[test]
    fn records_empty_collection_initialization() {
        let mut graph = ConstraintGraph::new();
        let mut builder = ConstructorConstraintBuilder::new(&mut graph);

        let input = ConstructorConstraintInput::new(
            None,
            SymbolId::from("demo::List"),
            dummy_span(5),
            ConstructorOrigin::CollectionLiteral,
            Vec::new(),
        )
        .with_collection_shape(CollectionShape::new(0));

        let result = builder.build(input, None);
        assert!(result.constraints.is_empty());
        assert_eq!(result.recorded_edges, 0);
        assert_eq!(result.diagnostics.entries().len(), 1);

        match &result.diagnostics.entries()[0] {
            ConstructorDiagnosticKind::EmptyInitialization { symbol, origin, .. } => {
                assert_eq!(symbol.as_str(), "demo::List");
                assert_eq!(*origin, ConstructorOrigin::CollectionLiteral);
            }
            ConstructorDiagnosticKind::JsonLiteralShape { .. } => {
                panic!("expected empty initialization diagnostic")
            }
        }
    }

    #[test]
    fn records_json_metadata() {
        let mut graph = ConstraintGraph::new();
        let mut builder = ConstructorConstraintBuilder::new(&mut graph);

        let input = ConstructorConstraintInput::new(
            None,
            SymbolId::from("demo::JsonModel"),
            dummy_span(7),
            ConstructorOrigin::JsonLiteral,
            Vec::new(),
        )
        .with_json_metadata(JsonDiagnosticMetadata::new(3, 2));

        let result = builder.build(input, None);
        assert_eq!(result.diagnostics.entries().len(), 1);

        match &result.diagnostics.entries()[0] {
            ConstructorDiagnosticKind::JsonLiteralShape {
                object_keys,
                array_entries,
                ..
            } => {
                assert_eq!(*object_keys, 3);
                assert_eq!(*array_entries, 2);
            }
            ConstructorDiagnosticKind::EmptyInitialization { .. } => {
                panic!("expected json literal diagnostic")
            }
        }
    }
}
