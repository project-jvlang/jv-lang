use std::collections::{HashMap, VecDeque};

use jv_ast::{
    expression::Argument,
    types::{Literal, Span},
    BinaryOp, Expression, Program, Statement, WhenArm,
};

use super::{patterns, NullSafetyContext, NullabilityKind, NullabilityLattice};
use crate::CheckError;

use crate::null_safety::graph::{
    BranchAssumption, FlowConstraint, FlowEdgeKind, FlowGraph, FlowNodeId, FlowNodeKind,
    FlowStateSnapshot,
};
use crate::null_safety::operators::{JavaLoweringHint, OperatorOutcome, OperatorSemantics};

/// Outcome of running the flow solver. Diagnostics are placeholders for upcoming tasks.
#[derive(Default)]
pub struct FlowAnalysisOutcome {
    pub states: HashMap<FlowNodeId, FlowStateSnapshot>,
    pub diagnostics: Vec<CheckError>,
    pub java_hints: Vec<JavaLoweringHint>,
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
        outcome
            .java_hints
            .extend(self.graph.hints().iter().cloned());
        let mut worklist = VecDeque::new();
        let mut in_states: HashMap<FlowNodeId, FlowStateSnapshot> = HashMap::new();
        let mut out_states: HashMap<FlowNodeId, FlowStateSnapshot> = HashMap::new();

        in_states.insert(
            self.graph.entry(),
            FlowStateSnapshot::from_lattice(self.context.lattice()),
        );
        worklist.push_back(self.graph.entry());

        while let Some(node_id) = worklist.pop_front() {
            let state_in = in_states
                .remove(&node_id)
                .unwrap_or_else(FlowStateSnapshot::new);

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
                    .map(|expr| classify_expression(self, expr))
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
                    let value_state = classify_expression(self, value);
                    self.emit_constraint(
                        current,
                        FlowNodeKind::Statement,
                        FlowConstraint::Assign {
                            variable: identifier,
                            state: value_state,
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
            Expression::When {
                expr,
                arms,
                else_arm,
                span: expr_span,
                ..
            } => self.build_when_expression(
                current,
                expr.as_deref(),
                arms,
                else_arm.as_deref(),
                expr_span.clone(),
            ),
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

    fn build_when_expression(
        &mut self,
        current: FlowNodeId,
        subject: Option<&Expression>,
        arms: &[WhenArm],
        else_branch: Option<&Expression>,
        span: Span,
    ) -> FlowNodeId {
        let mut cursor = if let Some(subject_expr) = subject {
            self.handle_expression(current, subject_expr, None)
        } else {
            current
        };

        let merge_node = self.graph.add_node(FlowNodeKind::Merge, Some(span.clone()));

        if arms.is_empty() {
            if let Some(else_expr) = else_branch {
                let else_end = self.handle_expression(cursor, else_expr, None);
                self.connect(else_end, merge_node, FlowEdgeKind::Normal);
            } else {
                self.connect(cursor, merge_node, FlowEdgeKind::Normal);
            }
            return merge_node;
        }

        for (index, arm) in arms.iter().enumerate() {
            let test_node = self
                .graph
                .add_node(FlowNodeKind::Expression, Some(arm.span.clone()));
            self.connect(cursor, test_node, FlowEdgeKind::Normal);

            let assumptions = patterns::analyze_pattern(subject, &arm.pattern);

            let branch_entry = self
                .graph
                .add_node(FlowNodeKind::Merge, Some(arm.span.clone()));
            self.connect(
                test_node,
                branch_entry,
                FlowEdgeKind::TrueBranch {
                    assumption: assumptions.on_match.clone(),
                },
            );

            let mut branch_cursor = branch_entry;
            if let Some(guard) = &arm.guard {
                // Guards are evaluated for side-effect tracking; boolean gating is handled in a later task.
                branch_cursor = self.handle_expression(branch_cursor, guard, None);
            }

            let branch_end = self.handle_expression(branch_cursor, &arm.body, None);
            self.connect(branch_end, merge_node, FlowEdgeKind::Normal);

            let is_last_arm = index == arms.len() - 1;
            let fallback_target = if !is_last_arm {
                Some(self.graph.add_node(FlowNodeKind::Merge, None))
            } else if else_branch.is_some() {
                Some(self.graph.add_node(FlowNodeKind::Merge, Some(span.clone())))
            } else {
                None
            };

            match fallback_target {
                Some(target) => {
                    self.connect(
                        test_node,
                        target,
                        FlowEdgeKind::FalseBranch {
                            assumption: assumptions.on_mismatch.clone(),
                        },
                    );
                    cursor = target;
                }
                None => {
                    self.connect(
                        test_node,
                        merge_node,
                        FlowEdgeKind::FalseBranch {
                            assumption: assumptions.on_mismatch.clone(),
                        },
                    );
                    cursor = merge_node;
                }
            }
        }

        if let Some(else_expr) = else_branch {
            let else_end = self.handle_expression(cursor, else_expr, None);
            self.connect(else_end, merge_node, FlowEdgeKind::Normal);
        } else if cursor != merge_node {
            self.connect(cursor, merge_node, FlowEdgeKind::Normal);
        }

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
        let assignment_state = classify_expression(self, initializer);
        self.emit_constraint(
            current,
            FlowNodeKind::Statement,
            FlowConstraint::Assign {
                variable: name.to_string(),
                state: assignment_state,
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

fn apply_operator_outcome(
    builder: &mut FlowGraphBuilder<'_>,
    outcome: OperatorOutcome,
) -> NullabilityKind {
    if let Some(hint) = outcome.hint {
        builder.graph.add_hint(hint);
    }
    outcome.nullability
}

fn classify_expression(builder: &mut FlowGraphBuilder<'_>, expr: &Expression) -> NullabilityKind {
    match expr {
        Expression::Literal(literal, _) => match literal {
            Literal::Null => NullabilityKind::Nullable,
            Literal::Boolean(_)
            | Literal::Character(_)
            | Literal::Number(_)
            | Literal::String(_) => NullabilityKind::NonNull,
        },
        Expression::Identifier(_, _) => NullabilityKind::Unknown,
        Expression::NullSafeMemberAccess { object, span, .. } => {
            let object_state = classify_expression(builder, object);
            apply_operator_outcome(
                builder,
                OperatorSemantics::null_safe_member_access(object_state, span.clone()),
            )
        }
        Expression::NullSafeIndexAccess {
            object,
            index,
            span,
        } => {
            let object_state = classify_expression(builder, object);
            // Evaluate index for completeness even though it does not impact nullability directly.
            let _ = classify_expression(builder, index);
            apply_operator_outcome(
                builder,
                OperatorSemantics::null_safe_index_access(object_state, span.clone()),
            )
        }
        Expression::Binary {
            left,
            op,
            right,
            span,
        } => {
            let left_state = classify_expression(builder, left);
            let right_state = classify_expression(builder, right);

            if matches!(op, BinaryOp::Elvis) {
                apply_operator_outcome(
                    builder,
                    OperatorSemantics::elvis(left_state, right_state, span.clone()),
                )
            } else {
                left_state.join(right_state)
            }
        }
        Expression::Unary { operand, .. } => classify_expression(builder, operand),
        Expression::Call { function, args, .. } => {
            let mut state = classify_expression(builder, function);
            for arg in args {
                match arg {
                    Argument::Positional(expr) => {
                        state = state.join(classify_expression(builder, expr));
                    }
                    Argument::Named { value, .. } => {
                        state = state.join(classify_expression(builder, value));
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
            let mut state = classify_expression(builder, then_branch);
            if let Some(else_branch) = else_branch {
                state = state.join(classify_expression(builder, else_branch));
            }
            state
        }
        Expression::Block { statements, .. } => {
            let mut state = NullabilityKind::Unknown;
            for statement in statements {
                state = state.join(classify_statement_expression(builder, statement));
            }
            state
        }
        Expression::When { arms, else_arm, .. } => {
            let mut state = NullabilityKind::Unknown;
            for arm in arms {
                state = state.join(classify_expression(builder, &arm.body));
            }
            if let Some(else_expr) = else_arm {
                state = state.join(classify_expression(builder, else_expr));
            }
            state
        }
        Expression::Lambda { .. } => NullabilityKind::NonNull,
        Expression::Array { .. } => NullabilityKind::NonNull,
        Expression::Try { expr, .. } => classify_expression(builder, expr),
        Expression::MemberAccess { .. } | Expression::IndexAccess { .. } => {
            NullabilityKind::Unknown
        }
        Expression::StringInterpolation { .. } => NullabilityKind::NonNull,
        Expression::This(_) | Expression::Super(_) => NullabilityKind::NonNull,
    }
}

fn classify_statement_expression(
    builder: &mut FlowGraphBuilder<'_>,
    statement: &Statement,
) -> NullabilityKind {
    match statement {
        Statement::Expression { expr, .. } => classify_expression(builder, expr),
        Statement::Return { value, .. } => value
            .as_ref()
            .map(|expr| classify_expression(builder, expr))
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
    use crate::null_safety::graph::BranchAssumption;
    use crate::null_safety::JavaLoweringStrategy;
    use jv_ast::{types::Span, Expression, Pattern, Program, Statement, WhenArm};

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

    #[test]
    fn null_safe_member_access_records_nullable_state_and_hint() {
        let span = Span::dummy();
        let program = Program {
            package: None,
            imports: vec![],
            statements: vec![Statement::VarDeclaration {
                name: "result".into(),
                type_annotation: None,
                initializer: Some(Expression::NullSafeMemberAccess {
                    object: Box::new(Expression::Identifier("user".into(), span.clone())),
                    property: "name".into(),
                    span: span.clone(),
                }),
                modifiers: Default::default(),
                span: span.clone(),
            }],
            span,
        };

        let graph = build_graph(&program);
        let mut assigned_states = Vec::new();
        for node in graph.nodes() {
            for constraint in node.constraints() {
                let FlowConstraint::Assign { state, .. } = constraint;
                assigned_states.push(*state);
            }
        }

        assert_eq!(assigned_states.len(), 1);
        assert_eq!(assigned_states[0], NullabilityKind::Nullable);
        assert!(matches!(
            graph
                .hints()
                .iter()
                .find(|hint| matches!(hint.strategy, JavaLoweringStrategy::NullSafeMemberAccess)),
            Some(_)
        ));
    }

    #[test]
    fn elvis_operator_prefers_non_null_and_emits_hint() {
        let span = Span::dummy();
        let elvis_expr = Expression::Binary {
            left: Box::new(Expression::NullSafeMemberAccess {
                object: Box::new(Expression::Identifier("user".into(), span.clone())),
                property: "nickname".into(),
                span: span.clone(),
            }),
            op: BinaryOp::Elvis,
            right: Box::new(Expression::Literal(
                Literal::String("guest".into()),
                span.clone(),
            )),
            span: span.clone(),
        };

        let program = Program {
            package: None,
            imports: vec![],
            statements: vec![Statement::VarDeclaration {
                name: "display".into(),
                type_annotation: None,
                initializer: Some(elvis_expr),
                modifiers: Default::default(),
                span: span.clone(),
            }],
            span,
        };

        let graph = build_graph(&program);
        let mut assigned_states = Vec::new();
        for node in graph.nodes() {
            for constraint in node.constraints() {
                let FlowConstraint::Assign { state, .. } = constraint;
                assigned_states.push(*state);
            }
        }

        assert_eq!(assigned_states.len(), 1);
        assert_eq!(assigned_states[0], NullabilityKind::NonNull);
        assert!(matches!(
            graph
                .hints()
                .iter()
                .find(|hint| matches!(hint.strategy, JavaLoweringStrategy::ElvisOperator)),
            Some(_)
        ));
    }

    #[test]
    fn when_null_pattern_narrows_branch_states() {
        let decl_span = Span::new(1, 1, 1, 5);
        let when_span = Span::new(2, 1, 6, 1);
        let null_arm_span = Span::new(3, 1, 3, 10);
        let else_span = Span::new(5, 1, 5, 10);
        let id_span = Span::new(2, 5, 2, 9);

        let when_expression = Expression::When {
            expr: Some(Box::new(Expression::Identifier(
                "x".into(),
                id_span.clone(),
            ))),
            arms: vec![WhenArm {
                pattern: Pattern::Literal(Literal::Null, null_arm_span.clone()),
                guard: None,
                body: Expression::Block {
                    statements: vec![Statement::Expression {
                        expr: Expression::Identifier("x".into(), id_span.clone()),
                        span: null_arm_span.clone(),
                    }],
                    span: null_arm_span.clone(),
                },
                span: null_arm_span.clone(),
            }],
            else_arm: Some(Box::new(Expression::Block {
                statements: vec![Statement::Expression {
                    expr: Expression::Identifier("x".into(), id_span.clone()),
                    span: else_span.clone(),
                }],
                span: else_span.clone(),
            })),
            implicit_end: None,
            span: when_span.clone(),
        };

        let program = Program {
            package: None,
            imports: vec![],
            statements: vec![
                Statement::VarDeclaration {
                    name: "x".into(),
                    type_annotation: None,
                    initializer: Some(Expression::Literal(Literal::Null, decl_span.clone())),
                    modifiers: Default::default(),
                    span: decl_span.clone(),
                },
                Statement::Expression {
                    expr: when_expression,
                    span: when_span.clone(),
                },
            ],
            span: Span::new(1, 1, 6, 1),
        };

        let graph = build_graph(&program);
        let context = NullSafetyContext::hydrate(None);
        let outcome = FlowSolver::new(&graph, &context).solve();

        let mut null_branch_node = None;
        let mut not_null_node = None;

        for node_id in 0..graph.nodes().len() {
            for edge in graph.adjacency(node_id) {
                match &edge.kind {
                    FlowEdgeKind::TrueBranch {
                        assumption: Some(BranchAssumption::Equals { variable, state }),
                    } if variable == "x" && *state == NullabilityKind::Nullable => {
                        null_branch_node = Some(edge.to);
                    }
                    FlowEdgeKind::FalseBranch {
                        assumption: Some(BranchAssumption::NotEquals { variable, state }),
                    } if variable == "x" && *state == NullabilityKind::NonNull => {
                        not_null_node = Some(edge.to);
                    }
                    _ => {}
                }
            }
        }

        let null_state = outcome
            .states
            .get(&null_branch_node.expect("null branch target"))
            .expect("state for null arm");
        assert_eq!(null_state.states.get("x"), Some(&NullabilityKind::Nullable));

        let non_null_state = outcome
            .states
            .get(&not_null_node.expect("non-null branch target"))
            .expect("state for else arm");
        assert_eq!(
            non_null_state.states.get("x"),
            Some(&NullabilityKind::NonNull)
        );
    }
}
