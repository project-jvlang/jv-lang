//! Java 型システムに依存するヘルパ API 群。

pub mod doublebrace_plan;
pub mod member_resolver;
pub mod primitive;

pub use doublebrace_plan::{
    CopyPlan, CopySource, DoublebracePlan, DoublebracePlanError, FieldUpdate, MethodInvocation,
    MutatePlan, MutationStep, PlanBase, plan_doublebrace_application,
};
pub use member_resolver::MemberResolver;
pub use primitive::{JavaBoxingTable, JavaNullabilityPolicy, JavaPrimitive};
