//! Java 型システムに依存するヘルパ API 群。

pub mod primitive;

pub use primitive::{JavaBoxingTable, JavaNullabilityPolicy, JavaPrimitive};
