use jv_ast::types::Span;

use super::{JavaLoweringHint, NullabilityKind};

pub type FlowNodeId = usize;

#[derive(Clone, Debug)]
pub struct FlowGraph {
    nodes: Vec<FlowNode>,
    adjacency: Vec<Vec<FlowEdge>>,    // outgoing edges
    predecessors: Vec<Vec<FlowEdge>>, // incoming edges
    entry: FlowNodeId,
    exit: FlowNodeId,
    hints: Vec<JavaLoweringHint>,
}

impl FlowGraph {
    pub fn new(entry_span: Span) -> Self {
        let entry_node = FlowNode::new(FlowNodeKind::Entry, Some(entry_span.clone()));
        let exit_node = FlowNode::new(FlowNodeKind::Exit, Some(entry_span));

        Self {
            nodes: vec![entry_node, exit_node],
            adjacency: vec![Vec::new(), Vec::new()],
            predecessors: vec![Vec::new(), Vec::new()],
            entry: 0,
            exit: 1,
            hints: Vec::new(),
        }
    }

    pub fn entry(&self) -> FlowNodeId {
        self.entry
    }

    pub fn exit(&self) -> FlowNodeId {
        self.exit
    }

    pub fn node(&self, id: FlowNodeId) -> &FlowNode {
        &self.nodes[id]
    }

    pub fn node_mut(&mut self, id: FlowNodeId) -> &mut FlowNode {
        &mut self.nodes[id]
    }

    pub fn adjacency(&self, id: FlowNodeId) -> &[FlowEdge] {
        &self.adjacency[id]
    }

    #[allow(dead_code)]
    pub fn predecessors(&self, id: FlowNodeId) -> &[FlowEdge] {
        &self.predecessors[id]
    }

    pub fn add_node(&mut self, kind: FlowNodeKind, span: Option<Span>) -> FlowNodeId {
        let id = self.nodes.len();
        self.nodes.push(FlowNode::new(kind, span));
        self.adjacency.push(Vec::new());
        self.predecessors.push(Vec::new());
        id
    }

    pub fn add_edge(&mut self, from: FlowNodeId, to: FlowNodeId, kind: FlowEdgeKind) {
        let edge = FlowEdge { from, to, kind };
        self.adjacency[from].push(edge.clone());
        self.predecessors[to].push(edge);
    }

    pub fn hints(&self) -> &[JavaLoweringHint] {
        &self.hints
    }

    pub fn add_hint(&mut self, hint: JavaLoweringHint) {
        self.hints.push(hint);
    }

    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }

    #[cfg(test)]
    pub(crate) fn nodes(&self) -> &[FlowNode] {
        &self.nodes
    }
}

#[derive(Clone, Debug)]
pub struct FlowNode {
    kind: FlowNodeKind,
    span: Option<Span>,
    constraints: Vec<FlowConstraint>,
}

impl FlowNode {
    pub fn new(kind: FlowNodeKind, span: Option<Span>) -> Self {
        Self {
            kind,
            span,
            constraints: Vec::new(),
        }
    }

    pub fn kind(&self) -> &FlowNodeKind {
        &self.kind
    }

    pub fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }

    pub fn constraints(&self) -> &[FlowConstraint] {
        &self.constraints
    }

    pub fn push_constraint(&mut self, constraint: FlowConstraint) {
        self.constraints.push(constraint);
    }
}

#[derive(Clone, Debug)]
pub enum FlowNodeKind {
    Entry,
    Exit,
    Statement,
    Expression,
    Merge,
    Loop,
}

#[derive(Clone, Debug)]
pub struct FlowEdge {
    #[allow(dead_code)]
    pub from: FlowNodeId,
    pub to: FlowNodeId,
    pub kind: FlowEdgeKind,
}

#[derive(Clone, Debug)]
pub enum FlowEdgeKind {
    Normal,
    TrueBranch {
        assumption: Option<BranchAssumption>,
    },
    FalseBranch {
        assumption: Option<BranchAssumption>,
    },
    LoopBack,
    Exceptional,
}

#[derive(Clone, Debug)]
pub enum BranchAssumption {
    Equals {
        variable: String,
        state: NullabilityKind,
    },
    NotEquals {
        variable: String,
        state: NullabilityKind,
    },
}

impl BranchAssumption {
    pub fn apply(&self, snapshot: &mut FlowStateSnapshot) {
        match self {
            BranchAssumption::Equals { variable, state }
            | BranchAssumption::NotEquals { variable, state } => {
                snapshot.assign(variable.clone(), *state);
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum FlowConstraint {
    Assign {
        variable: String,
        state: NullabilityKind,
    },
}

#[derive(Clone, Debug, Default)]
pub struct FlowStateSnapshot {
    pub states: std::collections::HashMap<String, NullabilityKind>,
}

impl FlowStateSnapshot {
    pub fn new() -> Self {
        Self {
            states: std::collections::HashMap::new(),
        }
    }

    pub fn assign(&mut self, variable: String, state: NullabilityKind) {
        self.states.insert(variable, state);
    }

    pub fn merge_with(&mut self, other: &FlowStateSnapshot) -> bool {
        let mut changed = false;
        for (name, other_state) in other.states.iter() {
            match self.states.get(name) {
                Some(current) => {
                    let joined = current.join(*other_state);
                    if joined != *current {
                        self.states.insert(name.clone(), joined);
                        changed = true;
                    }
                }
                None => {
                    self.states.insert(name.clone(), *other_state);
                    changed = true;
                }
            }
        }
        changed
    }
}
