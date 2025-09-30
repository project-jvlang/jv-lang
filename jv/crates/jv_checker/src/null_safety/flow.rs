use std::collections::{HashMap, VecDeque};

use jv_ast::{
    expression::Argument,
    types::{Literal, Span},
    BinaryOp, Expression, Program, Statement,
};

use super::{NullSafetyContext, NullabilityKind, NullabilityLattice};
use crate::CheckError;

use crate::null_safety::graph::{
    BranchAssumption, FlowConstraint, FlowEdgeKind, FlowGraph, FlowNodeId, FlowNodeKind,
    FlowStateSnapshot,
};

/// Outcome of running the flow solver. Diagnostics are placeholders for upcoming tasks.
#[derive(Default)]
pub struct FlowAnalysisOutcome {
    pub states: HashMap<FlowNodeId, FlowStateSnapshot>,
    pub diagnostics: Vec<CheckError>,
}

pub fn build_graph(program: &Program) -> FlowGraph {
    let mut graph = FlowGraph::new(program.span.clone());
    let entry = graph.entry();
    let exit = graph.exit();

    {
        let mut builder = FlowGraphBuilder::new(&mut graph);
        let mut current = entry;
        for statement in &program.statements {
            current = builder.handle_statement(current, statement);
        }

        builder.connect(current, exit, FlowEdgeKind::Normal);
        builder.finish();
    }

    graph
}

pub struct FlowSolver<'ctx> {
    graph: &'ctx FlowGraph,
    context: &'ctx NullSafetyContext<'ctx>,
}

impl<'ctx> FlowSolver<'ctx> {
    pub fn new(graph: &'ctx FlowGraph, context: &'ctx NullSafetyContext<'ctx>) -> Self {
        Self { graph, context }
    }

    pub fn solve(&self) -> FlowAnalysisOutcome {
        let mut outcome = FlowAnalysisOutcome::default();
        let mut worklist = VecDeque::new();
        let mut in_states: HashMap<FlowNodeId, FlowStateSnapshot> = HashMap::new();
        let mut out_states: HashMap<FlowNodeId, FlowStateSnapshot> = HashMap::new();

        in_states.insert(
            self.graph.entry(),
            FlowStateSnapshot::from_lattice(self.context.lattice()),
        );
        worklist.push_back(self.graph.entry());

        while let Some(node_id) = worklist.pop_front() {
            let mut state_in = in_states
                .get(&node_id)
                .cloned()
                .unwrap_or_else(FlowStateSnapshot::new);

            for edge in self.graph.predecessors(node_id) {
                if let Some(pred_state) = out_states.get(&edge.from) {
                    state_in.merge_with(pred_state);
                }
            }

            let mut state_out = state_in.clone();
            self.apply_constraints(node_id, &mut state_out);

            let mut changed = false;
            match out_states.get_mut(&node_id) {
                Some(existing) => {
                    if existing.merge_with(&state_out) {
                        changed = true;
                    }
                }
                None => {
                    out_states.insert(node_id, state_out.clone());
                    changed = true;
                }
            }

            if let Some(updated) = out_states.get(&node_id) {
                outcome.states.insert(node_id, updated.clone());
            }

            for edge in self.graph.adjacency(node_id) {
                let mut next_state = out_states
                    .get(&node_id)
                    .cloned()
                    .unwrap_or_else(FlowStateSnapshot::new);
                apply_edge_kind(&edge.kind, &mut next_state);
                let entry = in_states
                    .entry(edge.to)
                    .or_insert_with(FlowStateSnapshot::new);
                let merged = entry.merge_with(&next_state);
                if merged || changed {
                    worklist.push_back(edge.to);
                }
            }
        }

        outcome
    }

    fn apply_constraints(&self, node_id: FlowNodeId, state: &mut FlowStateSnapshot) {
        let node = self.graph.node(node_id);

        if let Some(span) = node.span() {
            debug_assert!(span.end_line >= span.start_line);
        }

        if matches!(node.kind(), FlowNodeKind::Entry) {
            return;
        }

        for constraint in node.constraints() {
            state.apply_constraint(constraint);
        }
    }
}

struct FlowGraphBuilder<'g> {
    graph: &'g mut FlowGraph,
}

impl<'g> FlowGraphBuilder<'g> {
    fn new(graph: &'g mut FlowGraph) -> Self {
        Self { graph }
    }

    fn finish(&mut self) {
        // currently no-op placeholder for future instrumentation hooks
    }

    fn connect(&mut self, from: FlowNodeId, to: FlowNodeId, kind: FlowEdgeKind) {
        self.graph.add_edge(from, to, kind);
    }

    fn handle_statement(&mut self, current: FlowNodeId, statement: &Statement) -> FlowNodeId {
        match statement {
            Statement::ValDeclaration {
                name,
                initializer,
                span,
                ..
            } => self.emit_assignment(current, name, initializer, span),
            Statement::VarDeclaration {
                name,
                initializer,
                span,
                ..
            } => {
                let state = initializer
                    .as_ref()
                    .map(|expr| classify_expression(expr))
                    .unwrap_or(NullabilityKind::Unknown);
                self.emit_constraint(
                    current,
                    FlowNodeKind::Statement,
                    FlowConstraint::Assign {
                        variable: name.clone(),
                        state,
                    },
                    Some(span.clone()),
                )
            }
            Statement::Assignment {
                target,
                value,
                span,
            } => {
                if let Some(identifier) = extract_assignment_target(target) {
                    self.emit_constraint(
                        current,
                        FlowNodeKind::Statement,
                        FlowConstraint::Assign {
                            variable: identifier,
                            state: classify_expression(value),
                        },
                        Some(span.clone()),
                    )
                } else {
                    self.emit_passthrough(current, FlowNodeKind::Statement, Some(span.clone()))
                }
            }
            Statement::Expression { expr, span } => {
                self.handle_expression(current, expr, Some(span.clone()))
            }
            Statement::Return { span, .. } => {
                self.emit_passthrough(current, FlowNodeKind::Statement, Some(span.clone()))
            }
            Statement::ForIn(for_in) => self.build_for_in(current, for_in),
            _ => self.emit_passthrough(current, FlowNodeKind::Statement, None),
        }
    }

    fn handle_expression(
        &mut self,
        current: FlowNodeId,
        expression: &Expression,
        span: Option<Span>,
    ) -> FlowNodeId {
        match expression {
            Expression::If {
                condition,
                then_branch,
                else_branch,
                span: expr_span,
            } => self.build_if_expression(
                current,
                condition,
                then_branch,
                else_branch.as_deref(),
                expr_span.clone(),
            ),
            Expression::Block {
                statements,
                span: block_span,
            } => {
                let mut cursor = current;
                for statement in statements {
                    cursor = self.handle_statement(cursor, statement);
                }
                self.emit_passthrough(cursor, FlowNodeKind::Expression, Some(block_span.clone()))
            }
            Expression::When { .. } => {
                // Placeholder: currently treat when as expression node without branches
                self.emit_passthrough(current, FlowNodeKind::Expression, span)
            }
            _ => self.emit_passthrough(current, FlowNodeKind::Expression, span),
        }
    }

    fn build_if_expression(
        &mut self,
        current: FlowNodeId,
        condition: &Expression,
        then_branch: &Expression,
        else_branch: Option<&Expression>,
        span: Span,
    ) -> FlowNodeId {
        let condition_node = self
            .graph
            .add_node(FlowNodeKind::Expression, Some(span.clone()));
        self.connect(current, condition_node, FlowEdgeKind::Normal);

        let assumptions = detect_null_comparison(condition);

        let then_node = self.graph.add_node(FlowNodeKind::Merge, None);
        let true_edge = FlowEdgeKind::TrueBranch {
            assumption: assumptions.true_assumption,
        };
        self.connect(condition_node, then_node, true_edge);
        let then_end = self.handle_expression(then_node, then_branch, None);

        let merge_node = self.graph.add_node(FlowNodeKind::Merge, Some(span.clone()));

        let else_target = if let Some(else_branch) = else_branch {
            let else_node = self.graph.add_node(FlowNodeKind::Merge, None);
            let false_edge = FlowEdgeKind::FalseBranch {
                assumption: assumptions.false_assumption,
            };
            self.connect(condition_node, else_node, false_edge);
            let else_end = self.handle_expression(else_node, else_branch, None);
            self.connect(else_end, merge_node, FlowEdgeKind::Normal);
            else_end
        } else {
            let false_edge = FlowEdgeKind::FalseBranch {
                assumption: assumptions.false_assumption,
            };
            self.connect(condition_node, merge_node, false_edge);
            merge_node
        };

        self.connect(then_end, merge_node, FlowEdgeKind::Normal);
        let _ = else_target;

        merge_node
    }

    fn build_for_in(&mut self, current: FlowNodeId, for_in: &jv_ast::ForInStatement) -> FlowNodeId {
        let loop_entry = self
            .graph
            .add_node(FlowNodeKind::Loop, Some(for_in.span.clone()));
        self.connect(current, loop_entry, FlowEdgeKind::Normal);

        if let Some(annotation) = &for_in.binding.type_annotation {
            let state = match annotation {
                jv_ast::TypeAnnotation::Nullable(_) => NullabilityKind::Nullable,
                _ => NullabilityKind::NonNull,
            };
            self.graph
                .node_mut(loop_entry)
                .push_constraint(FlowConstraint::Assign {
                    variable: for_in.binding.name.clone(),
                    state,
                });
        }

        let body_start = self
            .graph
            .add_node(FlowNodeKind::Expression, Some(for_in.span.clone()));
        self.connect(
            loop_entry,
            body_start,
            FlowEdgeKind::TrueBranch { assumption: None },
        );
        let body_end = self.handle_expression(body_start, &for_in.body, None);
        self.connect(body_end, loop_entry, FlowEdgeKind::LoopBack);

        let loop_exit = self
            .graph
            .add_node(FlowNodeKind::Merge, Some(for_in.span.clone()));
        self.connect(
            loop_entry,
            loop_exit,
            FlowEdgeKind::FalseBranch { assumption: None },
        );
        loop_exit
    }

    fn emit_assignment(
        &mut self,
        current: FlowNodeId,
        name: &str,
        initializer: &Expression,
        span: &Span,
    ) -> FlowNodeId {
        self.emit_constraint(
            current,
            FlowNodeKind::Statement,
            FlowConstraint::Assign {
                variable: name.to_string(),
                state: classify_expression(initializer),
            },
            Some(span.clone()),
        )
    }

    fn emit_constraint(
        &mut self,
        current: FlowNodeId,
        kind: FlowNodeKind,
        constraint: FlowConstraint,
        span: Option<Span>,
    ) -> FlowNodeId {
        let node_id = self.graph.add_node(kind, span);
        self.connect(current, node_id, FlowEdgeKind::Normal);
        self.graph.node_mut(node_id).push_constraint(constraint);
        node_id
    }

    fn emit_passthrough(
        &mut self,
        current: FlowNodeId,
        kind: FlowNodeKind,
        span: Option<Span>,
    ) -> FlowNodeId {
        let node_id = self.graph.add_node(kind, span);
        self.connect(current, node_id, FlowEdgeKind::Normal);
        node_id
    }
}

struct ConditionAssumptions {
    true_assumption: Option<BranchAssumption>,
    false_assumption: Option<BranchAssumption>,
}

impl Default for ConditionAssumptions {
    fn default() -> Self {
        Self {
            true_assumption: None,
            false_assumption: None,
        }
    }
}

fn detect_null_comparison(condition: &Expression) -> ConditionAssumptions {
    let mut assumptions = ConditionAssumptions::default();

    if let Expression::Binary {
        left, op, right, ..
    } = condition
    {
        match op {
            BinaryOp::Equal | BinaryOp::NotEqual => {
                if let Some(identifier) = extract_identifier(left) {
                    if is_null_literal(right) {
                        match op {
                            BinaryOp::Equal => {
                                assumptions.true_assumption = Some(BranchAssumption::Equals {
                                    variable: identifier.clone(),
                                    state: NullabilityKind::Nullable,
                                });
                                assumptions.false_assumption = Some(BranchAssumption::NotEquals {
                                    variable: identifier,
                                    state: NullabilityKind::NonNull,
                                });
                            }
                            BinaryOp::NotEqual => {
                                assumptions.true_assumption = Some(BranchAssumption::NotEquals {
                                    variable: identifier.clone(),
                                    state: NullabilityKind::NonNull,
                                });
                                assumptions.false_assumption = Some(BranchAssumption::Equals {
                                    variable: identifier,
                                    state: NullabilityKind::Nullable,
                                });
                            }
                            _ => {}
                        }
                    }
                } else if let Some(identifier) = extract_identifier(right) {
                    if is_null_literal(left) {
                        match op {
                            BinaryOp::Equal => {
                                assumptions.true_assumption = Some(BranchAssumption::Equals {
                                    variable: identifier.clone(),
                                    state: NullabilityKind::Nullable,
                                });
                                assumptions.false_assumption = Some(BranchAssumption::NotEquals {
                                    variable: identifier,
                                    state: NullabilityKind::NonNull,
                                });
                            }
                            BinaryOp::NotEqual => {
                                assumptions.true_assumption = Some(BranchAssumption::NotEquals {
                                    variable: identifier.clone(),
                                    state: NullabilityKind::NonNull,
                                });
                                assumptions.false_assumption = Some(BranchAssumption::Equals {
                                    variable: identifier,
                                    state: NullabilityKind::Nullable,
                                });
                            }
                            _ => {}
                        }
                    }
                }
            }
            _ => {}
        }
    }

    assumptions
}

fn extract_identifier(expr: &Expression) -> Option<String> {
    match expr {
        Expression::Identifier(name, _) => Some(name.clone()),
        _ => None,
    }
}

fn is_null_literal(expr: &Expression) -> bool {
    match expr {
        Expression::Literal(Literal::Null, _) => true,
        _ => false,
    }
}

fn extract_assignment_target(expr: &Expression) -> Option<String> {
    match expr {
        Expression::Identifier(name, _) => Some(name.clone()),
        _ => None,
    }
}

fn classify_expression(expr: &Expression) -> NullabilityKind {
    match expr {
        Expression::Literal(literal, _) => match literal {
            Literal::Null => NullabilityKind::Nullable,
            Literal::Boolean(_)
            | Literal::Character(_)
            | Literal::Number(_)
            | Literal::String(_) => NullabilityKind::NonNull,
        },
        Expression::Identifier(_, _) => NullabilityKind::Unknown,
        Expression::NullSafeMemberAccess { .. } | Expression::NullSafeIndexAccess { .. } => {
            NullabilityKind::Nullable
        }
        Expression::Binary { left, right, .. } => {
            classify_expression(left).join(classify_expression(right))
        }
        Expression::Unary { operand, .. } => classify_expression(operand),
        Expression::Call { function, args, .. } => {
            let mut state = classify_expression(function);
            for arg in args {
                match arg {
                    Argument::Positional(expr) => {
                        state = state.join(classify_expression(expr));
                    }
                    Argument::Named { value, .. } => {
                        state = state.join(classify_expression(value));
                    }
                }
            }
            state
        }
        Expression::If {
            then_branch,
            else_branch,
            ..
        } => {
            let mut state = classify_expression(then_branch);
            if let Some(else_branch) = else_branch {
                state = state.join(classify_expression(else_branch));
            }
            state
        }
        Expression::Block { statements, .. } => {
            let mut state = NullabilityKind::Unknown;
            for statement in statements {
                state = state.join(classify_statement_expression(statement));
            }
            state
        }
        Expression::When { arms, else_arm, .. } => {
            let mut state = NullabilityKind::Unknown;
            for arm in arms {
                state = state.join(classify_expression(&arm.body));
            }
            if let Some(else_expr) = else_arm {
                state = state.join(classify_expression(else_expr));
            }
            state
        }
        Expression::Lambda { .. } => NullabilityKind::NonNull,
        Expression::Array { .. } => NullabilityKind::NonNull,
        Expression::Try { expr, .. } => classify_expression(expr),
        Expression::MemberAccess { .. } | Expression::IndexAccess { .. } => {
            NullabilityKind::Unknown
        }
        Expression::StringInterpolation { .. } => NullabilityKind::NonNull,
        Expression::This(_) | Expression::Super(_) => NullabilityKind::NonNull,
    }
}

fn classify_statement_expression(statement: &Statement) -> NullabilityKind {
    match statement {
        Statement::Expression { expr, .. } => classify_expression(expr),
        Statement::Return { value, .. } => value
            .as_ref()
            .map(|expr| classify_expression(expr))
            .unwrap_or(NullabilityKind::Unknown),
        _ => NullabilityKind::Unknown,
    }
}

fn apply_edge_kind(kind: &FlowEdgeKind, state: &mut FlowStateSnapshot) {
    match kind {
        FlowEdgeKind::TrueBranch { assumption } | FlowEdgeKind::FalseBranch { assumption } => {
            if let Some(assumption) = assumption {
                assumption.apply(state);
            }
        }
        FlowEdgeKind::LoopBack | FlowEdgeKind::Normal => {}
    }
}

impl FlowStateSnapshot {
    pub fn from_lattice(lattice: &NullabilityLattice) -> Self {
        let mut snapshot = FlowStateSnapshot::new();
        for (name, state) in lattice.iter() {
            snapshot.assign(name.clone(), *state);
        }
        snapshot
    }

    pub fn apply_constraint(&mut self, constraint: &FlowConstraint) {
        match constraint {
            FlowConstraint::Assign { variable, state } => {
                self.assign(variable.clone(), *state);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jv_ast::types::Span;

    #[test]
    fn build_graph_with_linear_statements() {
        let program = Program {
            package: None,
            imports: vec![],
            statements: vec![Statement::ValDeclaration {
                name: "x".into(),
                type_annotation: None,
                initializer: Expression::Literal(Literal::Null, Span::dummy()),
                modifiers: Default::default(),
                span: Span::dummy(),
            }],
            span: Span::dummy(),
        };

        let graph = build_graph(&program);
        assert!(graph.adjacency(graph.entry()).len() > 0);
    }
}
