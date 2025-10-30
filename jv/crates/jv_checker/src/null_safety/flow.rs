use std::collections::{HashMap, VecDeque};

use jv_ast::{
    BinaryOp, Expression, Program, Statement, TryCatchClause, WhenArm,
    expression::Argument,
    types::{Literal, Modifiers, Span},
};

use super::{NullSafetyContext, NullabilityKind, NullabilityLattice, patterns};
use crate::CheckError;
use crate::pattern::{self, ArmId, NarrowedBinding, NarrowedNullability, NarrowingSnapshot};

use crate::null_safety::graph::{
    BranchAssumption, FlowConstraint, FlowEdgeKind, FlowGraph, FlowNodeId, FlowNodeKind,
    FlowStateSnapshot,
};
use crate::null_safety::operators::{
    JavaLoweringHint, OperatorOperand, OperatorOutcome, OperatorSemantics,
};

/// Outcome of running the flow solver. Diagnostics are placeholders for upcoming tasks.
#[derive(Default)]
pub struct FlowAnalysisOutcome {
    pub states: HashMap<FlowNodeId, FlowStateSnapshot>,
    pub diagnostics: Vec<CheckError>,
    pub java_hints: Vec<JavaLoweringHint>,
    pub exit_state: Option<FlowStateSnapshot>,
}

pub fn build_graph(program: &Program, context: &mut NullSafetyContext) -> FlowGraph {
    let mut graph = FlowGraph::new(program.span.clone());
    let entry = graph.entry();
    let exit = graph.exit();

    {
        let mut builder = FlowGraphBuilder::new(&mut graph, context);
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

        outcome.exit_state = out_states.get(&self.graph.exit()).cloned();
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

struct FlowGraphBuilder<'g, 'ctx, 'facts> {
    graph: &'g mut FlowGraph,
    context: &'ctx mut NullSafetyContext<'facts>,
}

impl<'g, 'ctx, 'facts> FlowGraphBuilder<'g, 'ctx, 'facts> {
    fn new(graph: &'g mut FlowGraph, context: &'ctx mut NullSafetyContext<'facts>) -> Self {
        Self { graph, context }
    }

    fn finish(&mut self) {
        // currently no-op placeholder for future instrumentation hooks
    }

    fn connect(&mut self, from: FlowNodeId, to: FlowNodeId, kind: FlowEdgeKind) {
        self.graph.add_edge(from, to, kind);
    }

    fn register_late_init_annotation(&mut self, name: &str, modifiers: &Modifiers) {
        if modifiers.annotations.iter().any(|annotation| {
            annotation
                .name
                .simple_name()
                .eq_ignore_ascii_case("LateInit")
        }) {
            self.context
                .late_init_mut()
                .allow_late_init(name.to_string());
        }
    }

    fn handle_statement(&mut self, current: FlowNodeId, statement: &Statement) -> FlowNodeId {
        match statement {
            Statement::ValDeclaration {
                name,
                initializer,
                span,
                ..
            } => {
                self.context.late_init_mut().allow_late_init(name.clone());
                self.emit_assignment(current, name, initializer, span)
            }
            Statement::VarDeclaration {
                name,
                initializer,
                span,
                modifiers,
                ..
            } => {
                self.register_late_init_annotation(&name, modifiers);
                let state = initializer
                    .as_ref()
                    .map(|expr| classify_expression(self, expr).state)
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
                binding_pattern: _,
            } => {
                if let Some(identifier) = extract_assignment_target(target) {
                    let value_state = classify_expression(self, value).state;
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
            Expression::Try {
                body,
                catch_clauses,
                finally_block,
                span: expr_span,
            } => self.build_try_expression(
                current,
                body,
                catch_clauses,
                finally_block.as_deref(),
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
        let condition_node = self.add_graph_node(FlowNodeKind::Expression, Some(span.clone()));
        self.connect(current, condition_node, FlowEdgeKind::Normal);

        let assumptions = detect_null_comparison(condition);

        let then_node = self.add_graph_node(FlowNodeKind::Merge, None);
        let true_edge = FlowEdgeKind::TrueBranch {
            assumption: assumptions.true_assumption,
        };
        self.connect(condition_node, then_node, true_edge);
        let then_end = self.handle_expression(then_node, then_branch, None);

        let merge_node = self.add_graph_node(FlowNodeKind::Merge, Some(span.clone()));

        let else_target = if let Some(else_branch) = else_branch {
            let else_node = self.add_graph_node(FlowNodeKind::Merge, None);
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
        let node_key = pattern::node_identifier(&span);

        let mut cursor = if let Some(subject_expr) = subject {
            self.handle_expression(current, subject_expr, None)
        } else {
            current
        };

        let merge_node = self.add_graph_node(FlowNodeKind::Merge, Some(span.clone()));

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
            let test_node = self.add_graph_node(FlowNodeKind::Expression, Some(arm.span.clone()));
            self.connect(cursor, test_node, FlowEdgeKind::Normal);

            let assumptions = patterns::analyze_pattern(subject, &arm.pattern);

            let branch_entry = self.add_graph_node(FlowNodeKind::Merge, Some(arm.span.clone()));
            self.connect(
                test_node,
                branch_entry,
                FlowEdgeKind::TrueBranch {
                    assumption: select_true_assumption(
                        assumptions.on_match.clone(),
                        self.context
                            .pattern_facts(node_key)
                            .and_then(|facts| facts.arm_narrowing(index as ArmId)),
                    ),
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
                Some(self.add_graph_node(FlowNodeKind::Merge, None))
            } else if else_branch.is_some() {
                Some(self.add_graph_node(FlowNodeKind::Merge, Some(span.clone())))
            } else {
                None
            };

            match fallback_target {
                Some(target) => {
                    self.connect(
                        test_node,
                        target,
                        FlowEdgeKind::FalseBranch {
                            assumption: select_false_assumption(
                                assumptions.on_mismatch.clone(),
                                self.context
                                    .pattern_facts(node_key)
                                    .and_then(|facts| facts.arm_narrowing(index as ArmId)),
                            ),
                        },
                    );
                    cursor = target;
                }
                None => {
                    self.connect(
                        test_node,
                        merge_node,
                        FlowEdgeKind::FalseBranch {
                            assumption: select_false_assumption(
                                assumptions.on_mismatch.clone(),
                                self.context
                                    .pattern_facts(node_key)
                                    .and_then(|facts| facts.arm_narrowing(index as ArmId)),
                            ),
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

    fn build_try_expression(
        &mut self,
        current: FlowNodeId,
        body: &Expression,
        catch_clauses: &[TryCatchClause],
        finally_block: Option<&Expression>,
        span: Span,
    ) -> FlowNodeId {
        let base_count = self.graph.node_count();
        let try_entry = self.add_graph_node(FlowNodeKind::Expression, Some(span.clone()));
        self.connect(current, try_entry, FlowEdgeKind::Normal);

        let body_end = self.handle_expression(try_entry, body, None);
        let after_body_count = self.graph.node_count();

        let mut exceptional_sources: Vec<FlowNodeId> = (base_count..after_body_count)
            .map(|id| id as FlowNodeId)
            .filter(|&id| id != body_end)
            .collect();

        exceptional_sources.dedup();
        if exceptional_sources.is_empty() {
            exceptional_sources.push(try_entry);
        }

        let mut normal_targets = vec![body_end];

        if !catch_clauses.is_empty() {
            for clause in catch_clauses {
                let catch_entry =
                    self.add_graph_node(FlowNodeKind::Expression, Some(clause.span.clone()));

                for &source in &exceptional_sources {
                    self.connect(source, catch_entry, FlowEdgeKind::Exceptional);
                }

                if let Some(param) = &clause.parameter {
                    self.graph
                        .node_mut(catch_entry)
                        .push_constraint(FlowConstraint::Assign {
                            variable: param.name.clone(),
                            state: NullabilityKind::NonNull,
                        });
                }

                let catch_end = self.handle_expression(catch_entry, &clause.body, None);
                normal_targets.push(catch_end);
            }
        }

        if let Some(finally_expr) = finally_block {
            let finally_entry = self.add_graph_node(FlowNodeKind::Expression, Some(span.clone()));

            for target in &normal_targets {
                self.connect(*target, finally_entry, FlowEdgeKind::Normal);
            }

            if catch_clauses.is_empty() {
                for &source in &exceptional_sources {
                    self.connect(source, finally_entry, FlowEdgeKind::Exceptional);
                }
            }

            let finally_end = self.handle_expression(finally_entry, finally_expr, None);
            let merge_after_finally = self.add_graph_node(FlowNodeKind::Merge, Some(span.clone()));
            self.connect(finally_end, merge_after_finally, FlowEdgeKind::Normal);

            if catch_clauses.is_empty() {
                self.connect(finally_end, self.graph.exit(), FlowEdgeKind::Exceptional);
            }

            merge_after_finally
        } else {
            let merge_node = self.add_graph_node(FlowNodeKind::Merge, Some(span.clone()));

            for target in &normal_targets {
                self.connect(*target, merge_node, FlowEdgeKind::Normal);
            }

            if catch_clauses.is_empty() {
                for &source in &exceptional_sources {
                    self.connect(source, self.graph.exit(), FlowEdgeKind::Exceptional);
                }
            }

            merge_node
        }
    }

    fn build_for_in(&mut self, current: FlowNodeId, for_in: &jv_ast::ForInStatement) -> FlowNodeId {
        let loop_entry = self.add_graph_node(FlowNodeKind::Loop, Some(for_in.span.clone()));
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

        let body_start = self.add_graph_node(FlowNodeKind::Expression, Some(for_in.span.clone()));
        self.connect(
            loop_entry,
            body_start,
            FlowEdgeKind::TrueBranch { assumption: None },
        );
        let body_end = self.handle_expression(body_start, &for_in.body, None);
        self.connect(body_end, loop_entry, FlowEdgeKind::LoopBack);

        let loop_exit = self.add_graph_node(FlowNodeKind::Merge, Some(for_in.span.clone()));
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
        let assignment_state = classify_expression(self, initializer).state;
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
        let node_id = self.add_graph_node(kind, span);
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
        let node_id = self.add_graph_node(kind, span);
        self.connect(current, node_id, FlowEdgeKind::Normal);
        node_id
    }

    fn add_graph_node(&mut self, kind: FlowNodeKind, span: Option<Span>) -> FlowNodeId {
        let sanitized = Self::normalize_span(span);
        self.graph.add_node(kind, sanitized)
    }

    fn normalize_span(span: Option<Span>) -> Option<Span> {
        span.map(|mut span| {
            if span.start_line == 0 {
                span.start_line = 1;
            }
            if span.start_column == 0 {
                span.start_column = 1;
            }

            if span.end_line == 0 {
                span.end_line = span.start_line;
            }
            if span.end_line < span.start_line {
                span.end_line = span.start_line;
            }

            if span.end_line == span.start_line {
                if span.end_column <= span.start_column {
                    span.end_column = span.start_column + 1;
                }
            } else if span.end_column == 0 {
                span.end_column = span.start_column + 1;
            }

            span
        })
    }
}

fn select_true_assumption(
    existing: Option<BranchAssumption>,
    snapshot: Option<&NarrowingSnapshot>,
) -> Option<BranchAssumption> {
    if existing.is_some() {
        return existing;
    }

    snapshot
        .and_then(|snap| first_meaningful_binding(snap.on_match()))
        .and_then(|binding| binding_to_assumption(binding, true))
}

fn select_false_assumption(
    existing: Option<BranchAssumption>,
    snapshot: Option<&NarrowingSnapshot>,
) -> Option<BranchAssumption> {
    if existing.is_some() {
        return existing;
    }

    snapshot
        .and_then(|snap| first_meaningful_binding(snap.on_mismatch()))
        .and_then(|binding| binding_to_assumption(binding, false))
}

fn first_meaningful_binding(bindings: &[NarrowedBinding]) -> Option<&NarrowedBinding> {
    bindings
        .iter()
        .find(|binding| binding.nullability != NarrowedNullability::Unknown)
}

fn binding_to_assumption(
    binding: &NarrowedBinding,
    is_true_branch: bool,
) -> Option<BranchAssumption> {
    let state = match binding.nullability {
        NarrowedNullability::NonNull => NullabilityKind::NonNull,
        NarrowedNullability::Nullable => NullabilityKind::Nullable,
        NarrowedNullability::Unknown => return None,
    };

    let variable = binding.variable.clone();
    let assumption = if is_true_branch {
        BranchAssumption::Equals { variable, state }
    } else {
        BranchAssumption::NotEquals { variable, state }
    };
    Some(assumption)
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
            BinaryOp::Is => {
                if let Some(identifier) = extract_identifier(left) {
                    assumptions.true_assumption = Some(BranchAssumption::Equals {
                        variable: identifier,
                        state: NullabilityKind::NonNull,
                    });
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

#[derive(Clone, Debug)]
struct ExpressionInfo {
    state: NullabilityKind,
    symbol: Option<String>,
}

impl ExpressionInfo {
    fn new(state: NullabilityKind) -> Self {
        Self {
            state,
            symbol: None,
        }
    }

    fn with_symbol(state: NullabilityKind, symbol: Option<String>) -> Self {
        Self { state, symbol }
    }

    fn join(&self, other: &ExpressionInfo) -> Self {
        ExpressionInfo::new(self.state.join(other.state))
    }

    fn into_operand(&self) -> OperatorOperand {
        OperatorOperand::new(self.state, self.symbol.clone())
    }
}

fn apply_operator_outcome(
    builder: &mut FlowGraphBuilder<'_, '_, '_>,
    outcome: OperatorOutcome,
) -> NullabilityKind {
    if let Some(hint) = outcome.hint {
        builder.graph.add_hint(hint);
    }
    outcome.nullability
}

fn classify_expression(
    builder: &mut FlowGraphBuilder<'_, '_, '_>,
    expr: &Expression,
) -> ExpressionInfo {
    match expr {
        Expression::RegexLiteral(_) => ExpressionInfo::new(NullabilityKind::NonNull),
        Expression::Literal(literal, _) => match literal {
            Literal::Null => ExpressionInfo::new(NullabilityKind::Nullable),
            Literal::Boolean(_)
            | Literal::Character(_)
            | Literal::Number(_)
            | Literal::String(_)
            | Literal::Regex(_) => ExpressionInfo::new(NullabilityKind::NonNull),
        },
        Expression::Identifier(name, _) => {
            ExpressionInfo::with_symbol(NullabilityKind::Unknown, Some(name.clone()))
        }
        Expression::NullSafeMemberAccess { object, span, .. } => {
            let object_info = classify_expression(builder, object);
            let outcome = OperatorSemantics::null_safe_member_access(
                object_info.into_operand(),
                span.clone(),
            );
            ExpressionInfo::new(apply_operator_outcome(builder, outcome))
        }
        Expression::NullSafeIndexAccess {
            object,
            index,
            span,
        } => {
            let object_info = classify_expression(builder, object);
            let _ = classify_expression(builder, index);
            let outcome =
                OperatorSemantics::null_safe_index_access(object_info.into_operand(), span.clone());
            ExpressionInfo::new(apply_operator_outcome(builder, outcome))
        }
        Expression::TypeCast { expr, .. } => classify_expression(builder, expr),
        Expression::Binary {
            left,
            op,
            right,
            span,
        } => {
            let left_info = classify_expression(builder, left);
            let right_info = classify_expression(builder, right);

            if matches!(op, BinaryOp::Elvis) {
                let outcome =
                    OperatorSemantics::elvis(left_info.state, right_info.state, span.clone());
                ExpressionInfo::new(apply_operator_outcome(builder, outcome))
            } else {
                left_info.join(&right_info)
            }
        }
        Expression::Unary { operand, .. } => classify_expression(builder, operand),
        Expression::Call { function, args, .. } => {
            let mut info = classify_expression(builder, function);
            for arg in args {
                match arg {
                    Argument::Positional(expr) => {
                        info = info.join(&classify_expression(builder, expr));
                    }
                    Argument::Named { value, .. } => {
                        info = info.join(&classify_expression(builder, value));
                    }
                }
            }
            info
        }
        Expression::If {
            then_branch,
            else_branch,
            ..
        } => {
            let mut info = classify_expression(builder, then_branch);
            if let Some(else_branch) = else_branch {
                info = info.join(&classify_expression(builder, else_branch));
            }
            info
        }
        Expression::Block { statements, .. } => {
            let mut info = ExpressionInfo::new(NullabilityKind::Unknown);
            for statement in statements {
                info = info.join(&classify_statement_expression(builder, statement));
            }
            info
        }
        Expression::When { arms, else_arm, .. } => {
            let mut info = ExpressionInfo::new(NullabilityKind::Unknown);
            for arm in arms {
                info = info.join(&classify_expression(builder, &arm.body));
            }
            if let Some(else_expr) = else_arm {
                info = info.join(&classify_expression(builder, else_expr));
            }
            info
        }
        Expression::Lambda { .. }
        | Expression::Array { .. }
        | Expression::StringInterpolation { .. }
        | Expression::MultilineString { .. }
        | Expression::JsonLiteral { .. }
        | Expression::This(_)
        | Expression::Super(_) => ExpressionInfo::new(NullabilityKind::NonNull),
        Expression::Try {
            body,
            catch_clauses,
            finally_block,
            ..
        } => {
            let mut info = classify_expression(builder, body);
            for clause in catch_clauses {
                info = info.join(&classify_expression(builder, &clause.body));
            }
            if let Some(finally_expr) = finally_block.as_deref() {
                info = info.join(&classify_expression(builder, finally_expr));
            }
            info
        }
        Expression::MemberAccess { .. } | Expression::IndexAccess { .. } => {
            ExpressionInfo::new(NullabilityKind::Unknown)
        }
    }
}

fn classify_statement_expression(
    builder: &mut FlowGraphBuilder<'_, '_, '_>,
    statement: &Statement,
) -> ExpressionInfo {
    match statement {
        Statement::Expression { expr, .. } => classify_expression(builder, expr),
        Statement::Return { value, .. } => value
            .as_ref()
            .map(|expr| classify_expression(builder, expr))
            .unwrap_or_else(|| ExpressionInfo::new(NullabilityKind::Unknown)),
        _ => ExpressionInfo::new(NullabilityKind::Unknown),
    }
}

fn apply_edge_kind(kind: &FlowEdgeKind, state: &mut FlowStateSnapshot) {
    match kind {
        FlowEdgeKind::TrueBranch { assumption } | FlowEdgeKind::FalseBranch { assumption } => {
            if let Some(assumption) = assumption {
                assumption.apply(state);
            }
        }
        FlowEdgeKind::LoopBack | FlowEdgeKind::Normal | FlowEdgeKind::Exceptional => {}
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
    use crate::null_safety::JavaLoweringStrategy;
    use crate::null_safety::graph::BranchAssumption;
    use jv_ast::ValBindingOrigin;
    use jv_ast::{Expression, Pattern, Program, Statement, TryCatchClause, WhenArm, types::Span};

    #[test]
    fn build_graph_with_linear_statements() {
        let program = Program {
            package: None,
            imports: vec![],
            statements: vec![Statement::ValDeclaration {
                name: "x".into(),
                binding: None,

                type_annotation: None,
                initializer: Expression::Literal(Literal::Null, Span::dummy()),
                modifiers: Default::default(),
                origin: ValBindingOrigin::ExplicitKeyword,
                span: Span::dummy(),
            }],
            span: Span::dummy(),
        };

        let mut context = NullSafetyContext::hydrate(None);
        let graph = build_graph(&program, &mut context);
        assert!(graph.adjacency(graph.entry()).len() > 0);
    }

    #[test]
    fn flow_nodes_sanitize_zero_width_spans() {
        let program = Program {
            package: None,
            imports: vec![],
            statements: vec![Statement::Expression {
                expr: Expression::Block {
                    statements: vec![Statement::Return {
                        value: Some(Expression::Literal(
                            Literal::Number("0".into()),
                            Span::dummy(),
                        )),
                        span: Span::dummy(),
                    }],
                    span: Span::dummy(),
                },
                span: Span::dummy(),
            }],
            span: Span::new(1, 1, 1, 2),
        };

        let mut context = NullSafetyContext::hydrate(None);
        let graph = build_graph(&program, &mut context);

        for node in graph.nodes() {
            if let Some(span) = node.span() {
                assert!(
                    span.end_line >= span.start_line,
                    "expected end_line >= start_line for span {:?}",
                    span
                );

                if span.end_line == span.start_line {
                    assert!(
                        span.end_column > span.start_column,
                        "expected span to cover at least one column, got {:?}",
                        span
                    );
                }
            }
        }
    }

    #[test]
    fn flow_graph_normalizes_when_block_spans() {
        let block_span = Span::new(3, 4, 3, 4);
        let else_block_span = Span::new(4, 8, 4, 8);

        let block_statements = vec![
            Statement::ValDeclaration {
                name: "trimmed".into(),
                binding: None,

                type_annotation: None,
                initializer: Expression::Identifier("token".into(), Span::new(3, 18, 3, 23)),
                modifiers: Default::default(),
                origin: ValBindingOrigin::ExplicitKeyword,
                span: Span::new(3, 8, 3, 24),
            },
            Statement::Expression {
                expr: Expression::Identifier("trimmed".into(), Span::new(3, 26, 3, 33)),
                span: Span::new(3, 26, 3, 33),
            },
        ];

        let when_arm = WhenArm {
            pattern: Pattern::Identifier("value".into(), Span::new(3, 4, 3, 9)),
            guard: None,
            body: Expression::Block {
                statements: block_statements,
                span: block_span.clone(),
            },
            span: Span::new(3, 4, 3, 34),
        };

        let else_block = Expression::Block {
            statements: vec![Statement::Expression {
                expr: Expression::Literal(Literal::String("".into()), Span::new(4, 10, 4, 12)),
                span: Span::new(4, 10, 4, 12),
            }],
            span: else_block_span.clone(),
        };

        let when_expression = Expression::When {
            expr: Some(Box::new(Expression::Identifier(
                "token".into(),
                Span::new(2, 15, 2, 20),
            ))),
            arms: vec![when_arm],
            else_arm: Some(Box::new(else_block)),
            implicit_end: None,
            span: Span::new(2, 5, 4, 15),
        };

        let program = Program {
            package: None,
            imports: vec![],
            statements: vec![Statement::Expression {
                expr: when_expression,
                span: Span::new(2, 1, 4, 16),
            }],
            span: Span::new(1, 1, 4, 16),
        };

        let mut context = NullSafetyContext::hydrate(None);
        let graph = build_graph(&program, &mut context);

        let normalized_spans: Vec<Span> = graph
            .nodes()
            .iter()
            .filter_map(|node| match node.kind() {
                FlowNodeKind::Expression => node.span().cloned(),
                _ => None,
            })
            .collect();

        assert!(
            !normalized_spans.is_empty(),
            "expected expression nodes with spans, got {:?}",
            graph
                .nodes()
                .iter()
                .map(|node| (node.kind().clone(), node.span().cloned()))
                .collect::<Vec<_>>()
        );

        assert!(
            normalized_spans.iter().any(|span| {
                span.start_line == 3
                    && span.start_column == 4
                    && span.end_column > span.start_column
            }),
            "expected normalized span for when arm block, got: {:?}",
            normalized_spans
        );

        assert!(
            normalized_spans.iter().any(|span| {
                span.start_line == 4
                    && span.start_column == 8
                    && span.end_column > span.start_column
            }),
            "expected normalized span for else block, got: {:?}",
            normalized_spans
        );
    }

    #[test]
    fn try_with_catch_creates_exceptional_edges() {
        let span = Span::dummy();
        let try_body = Expression::Block {
            statements: vec![Statement::VarDeclaration {
                name: "value".into(),
                binding: None,
                type_annotation: None,
                initializer: Some(Expression::Literal(Literal::Null, span.clone())),
                modifiers: Default::default(),
                span: span.clone(),
            }],
            span: span.clone(),
        };

        let catch_clause = TryCatchClause {
            parameter: Some(jv_ast::Parameter {
                name: "error".into(),
                type_annotation: None,
                default_value: None,
                modifiers: jv_ast::ParameterModifiers::default(),
                span: span.clone(),
            }),
            body: Box::new(Expression::Block {
                statements: vec![Statement::Expression {
                    expr: Expression::Identifier("value".into(), span.clone()),
                    span: span.clone(),
                }],
                span: span.clone(),
            }),
            span: span.clone(),
        };

        let program = Program {
            package: None,
            imports: vec![],
            statements: vec![Statement::Expression {
                expr: Expression::Try {
                    body: Box::new(try_body),
                    catch_clauses: vec![catch_clause],
                    finally_block: None,
                    span: span.clone(),
                },
                span: span.clone(),
            }],
            span: span.clone(),
        };

        let mut context = NullSafetyContext::hydrate(None);
        let graph = build_graph(&program, &mut context);
        let catch_node = graph
            .nodes()
            .iter()
            .enumerate()
            .find(|(_, node)| {
                node.constraints()
                    .iter()
                    .any(|constraint| match constraint {
                        FlowConstraint::Assign { variable, .. } => variable == "error",
                    })
            })
            .map(|(id, _)| id)
            .expect("catch entry node present");

        let has_exception_edge = (0..graph.node_count()).any(|id| {
            graph
                .adjacency(id)
                .iter()
                .any(|edge| edge.to == catch_node && matches!(edge.kind, FlowEdgeKind::Exceptional))
        });

        assert!(
            has_exception_edge,
            "expected exceptional edge into catch block"
        );
    }

    #[test]
    fn try_without_catch_routes_exception_to_exit() {
        let span = Span::dummy();
        let program = Program {
            package: None,
            imports: vec![],
            statements: vec![Statement::Expression {
                expr: Expression::Try {
                    body: Box::new(Expression::Block {
                        statements: vec![Statement::VarDeclaration {
                            name: "flag".into(),
                            binding: None,
                            type_annotation: None,
                            initializer: Some(Expression::Literal(Literal::Null, span.clone())),
                            modifiers: Default::default(),
                            span: span.clone(),
                        }],
                        span: span.clone(),
                    }),
                    catch_clauses: vec![],
                    finally_block: Some(Box::new(Expression::Block {
                        statements: vec![Statement::Expression {
                            expr: Expression::Identifier("flag".into(), span.clone()),
                            span: span.clone(),
                        }],
                        span: span.clone(),
                    })),
                    span: span.clone(),
                },
                span: span.clone(),
            }],
            span: span.clone(),
        };

        let mut context = NullSafetyContext::hydrate(None);
        let graph = build_graph(&program, &mut context);
        let exit_id = graph.exit();
        let exceptional_to_exit = (0..graph.node_count()).any(|id| {
            graph
                .adjacency(id)
                .iter()
                .any(|edge| edge.to == exit_id && matches!(edge.kind, FlowEdgeKind::Exceptional))
        });
        assert!(
            exceptional_to_exit,
            "expected exceptional edge to exit from try without catch"
        );
    }

    #[test]
    fn null_safe_member_access_records_nullable_state_and_hint() {
        let span = Span::dummy();
        let program = Program {
            package: None,
            imports: vec![],
            statements: vec![Statement::VarDeclaration {
                name: "result".into(),
                binding: None,
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

        let mut context = NullSafetyContext::hydrate(None);
        let graph = build_graph(&program, &mut context);
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
                binding: None,
                type_annotation: None,
                initializer: Some(elvis_expr),
                modifiers: Default::default(),
                span: span.clone(),
            }],
            span,
        };

        let mut context = NullSafetyContext::hydrate(None);
        let graph = build_graph(&program, &mut context);
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
                    binding: None,
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

        let mut context = NullSafetyContext::hydrate(None);
        let graph = build_graph(&program, &mut context);
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
