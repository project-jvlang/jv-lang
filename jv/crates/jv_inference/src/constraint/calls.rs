use crate::constraint::incremental::{AstId, IncrementalConstraintBuilder};
use crate::constraint::{
    ConstraintGraph, ConstraintKind, ConstraintNode, Edge, EdgeKind, GenericConstraint,
    GenericConstraintKind, NodeId,
};
use crate::types::{SymbolId, TypeId, TypeKind, TypeVariant};
use jv_ast::Expression;
use jv_ast::Span;

/// Function call argument description used by [`CallConstraintBuilder`].
#[derive(Debug, Clone)]
pub struct CallArgument {
    pub expression: Expression,
    pub type_id: TypeId,
    pub type_hint: TypeKind,
    pub type_parameter: Option<TypeId>,
    pub span: Span,
}

impl CallArgument {
    pub fn new(
        expression: Expression,
        type_id: TypeId,
        type_hint: TypeKind,
        type_parameter: Option<TypeId>,
        span: Span,
    ) -> Self {
        Self {
            expression,
            type_id,
            type_hint,
            type_parameter,
            span,
        }
    }
}

/// Binding describing how the call result feeds into its surrounding context.
#[derive(Debug, Clone)]
pub struct CallResultBinding {
    pub type_id: TypeId,
    pub type_hint: Option<TypeKind>,
    pub usage: Option<TypeId>,
    pub usage_hint: Option<TypeKind>,
    pub span: Span,
}

impl CallResultBinding {
    pub fn new(
        type_id: TypeId,
        type_hint: Option<TypeKind>,
        usage: Option<TypeId>,
        usage_hint: Option<TypeKind>,
        span: Span,
    ) -> Self {
        Self {
            type_id,
            type_hint,
            usage,
            usage_hint,
            span,
        }
    }
}

/// Complete call-site description processed by the builder.
#[derive(Debug, Clone)]
pub struct CallConstraintInput {
    pub ast_id: Option<AstId>,
    pub callee: SymbolId,
    pub call_span: Span,
    pub arguments: Vec<CallArgument>,
    pub result: Option<CallResultBinding>,
}

impl CallConstraintInput {
    pub fn new(
        ast_id: Option<AstId>,
        callee: SymbolId,
        call_span: Span,
        arguments: Vec<CallArgument>,
        result: Option<CallResultBinding>,
    ) -> Self {
        Self {
            ast_id,
            callee,
            call_span,
            arguments,
            result,
        }
    }
}

/// Result describing the emitted constraints and graph updates.
#[derive(Debug, Clone)]
pub struct CallConstraintResult {
    pub symbol: SymbolId,
    pub call_span: Span,
    pub constraints: Vec<GenericConstraint>,
    pub higher_order_arguments: Vec<usize>,
    pub recorded_edges: usize,
}

impl CallConstraintResult {
    pub fn is_empty(&self) -> bool {
        self.constraints.is_empty() && self.recorded_edges == 0
    }
}

/// Builds generic constraints and graph edges for call expressions.
pub struct CallConstraintBuilder<'graph> {
    graph: &'graph mut ConstraintGraph,
}

impl<'graph> CallConstraintBuilder<'graph> {
    pub fn new(graph: &'graph mut ConstraintGraph) -> Self {
        Self { graph }
    }

    pub fn build(
        &mut self,
        input: CallConstraintInput,
        incremental: Option<&mut IncrementalConstraintBuilder>,
    ) -> CallConstraintResult {
        let mut constraints = Vec::new();
        let mut higher_order_arguments = Vec::new();
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

            if let Some(parameter) = argument.type_parameter {
                let constraint = GenericConstraint::new(
                    GenericConstraintKind::TypeArgument {
                        callee: input.callee.clone(),
                        parameter,
                        argument: argument.type_hint.clone(),
                        argument_index: index,
                    },
                    argument.span.clone(),
                );
                constraints.push(constraint);

                recorded_edges += self.connect_assignment(
                    argument.type_id,
                    Some(argument.type_hint.clone()),
                    parameter,
                    None,
                    &argument.span,
                    &mut incremental,
                );
            }

            if matches!(argument.expression, Expression::Lambda { .. })
                || matches!(argument.type_hint.variant(), TypeVariant::Function(_, _))
            {
                higher_order_arguments.push(index);
            }
        }

        if let Some(result) = &input.result {
            self.graph
                .ensure_type_node(result.type_id, result.type_hint.clone());
            if let Some(usage) = result.usage {
                recorded_edges += self.connect_assignment(
                    result.type_id,
                    result.type_hint.clone(),
                    usage,
                    result.usage_hint.clone(),
                    &result.span,
                    &mut incremental,
                );
            }
        }

        CallConstraintResult {
            symbol: input.callee,
            call_span: input.call_span,
            constraints,
            higher_order_arguments,
            recorded_edges,
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
        {
            let needs_span = { self.graph.metadata().get(&constraint_node).is_none() };
            if needs_span {
                self.graph
                    .metadata_mut()
                    .insert(constraint_node, span.clone());
            }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::constraint::IncrementalConstraintBuilder;
    use crate::constraint::{ConstraintGraph, GenericConstraintKind};
    use crate::types::{TypeId, TypeKind, TypeVariant};
    use jv_ast::ParameterModifiers;
    use jv_ast::expression::Parameter;

    fn dummy_span(start: usize) -> Span {
        Span::new(start, 0, start, 5)
    }

    #[test]
    fn produces_type_argument_constraints_and_edges() {
        let mut graph = ConstraintGraph::new();
        let mut incremental = IncrementalConstraintBuilder::new();
        let mut builder = CallConstraintBuilder::new(&mut graph);

        let arg1_span = dummy_span(1);
        let arg1_expr = Expression::Identifier("value".into(), arg1_span.clone());
        let arg1 = CallArgument::new(
            arg1_expr,
            TypeId::new(10),
            TypeKind::new(TypeVariant::Primitive("Int")),
            Some(TypeId::new(1)),
            arg1_span.clone(),
        );

        let lambda_span = dummy_span(2);
        let lambda_expr = Expression::Lambda {
            parameters: vec![Parameter {
                name: "x".into(),
                type_annotation: None,
                default_value: None,
                modifiers: ParameterModifiers::default(),
                span: lambda_span.clone(),
            }],
            body: Box::new(Expression::Identifier("x".into(), lambda_span.clone())),
            span: lambda_span.clone(),
        };
        let lambda_type = TypeKind::function(
            vec![TypeKind::new(TypeVariant::Primitive("Int"))],
            TypeKind::new(TypeVariant::Primitive("Int")),
        );
        let arg2 = CallArgument::new(
            lambda_expr,
            TypeId::new(11),
            lambda_type.clone(),
            Some(TypeId::new(2)),
            lambda_span.clone(),
        );

        let result_binding = CallResultBinding::new(
            TypeId::new(20),
            None,
            Some(TypeId::new(30)),
            None,
            dummy_span(3),
        );

        let input = CallConstraintInput::new(
            Some(7),
            SymbolId::from("demo::map"),
            dummy_span(0),
            vec![arg1, arg2],
            Some(result_binding),
        );

        let outcome = builder.build(input, Some(&mut incremental));

        assert_eq!(outcome.constraints.len(), 2);
        assert_eq!(outcome.recorded_edges, 6);
        assert_eq!(outcome.higher_order_arguments, vec![1]);

        match &outcome.constraints[0].kind {
            GenericConstraintKind::TypeArgument {
                callee,
                parameter,
                argument_index,
                ..
            } => {
                assert_eq!(callee.as_str(), "demo::map");
                assert_eq!(*parameter, TypeId::new(1));
                assert_eq!(*argument_index, 0);
            }
            GenericConstraintKind::ConstructorArgument { .. } => {
                panic!("call builder must not emit constructor constraints")
            }
            GenericConstraintKind::BoundRequirement { .. }
            | GenericConstraintKind::VarianceUsage { .. } => {
                panic!("call builder must not emit bound or variance constraints")
            }
        }

        let diff = incremental.diff();
        assert_eq!(diff.added_edges.len(), 6);
        assert!(diff.removed_edges.is_empty());
        assert_eq!(graph.constraint_node_count(), 3);
    }

    #[test]
    fn handles_calls_without_incremental_builder() {
        let mut graph = ConstraintGraph::new();
        let mut builder = CallConstraintBuilder::new(&mut graph);

        let arg_span = dummy_span(1);
        let arg_expr = Expression::Identifier("value".into(), arg_span.clone());
        let argument = CallArgument::new(
            arg_expr,
            TypeId::new(40),
            TypeKind::default(),
            None,
            arg_span,
        );

        let input = CallConstraintInput::new(
            None,
            SymbolId::from("demo::noop"),
            dummy_span(0),
            vec![argument],
            None,
        );

        let outcome = builder.build(input, None);
        assert!(outcome.constraints.is_empty());
        assert_eq!(outcome.recorded_edges, 0);
        assert!(outcome.higher_order_arguments.is_empty());
        assert_eq!(graph.constraint_node_count(), 0);
    }
}
