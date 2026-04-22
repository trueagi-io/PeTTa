//! Core trie types: nodes, references, and the PathMap struct.
//!
//! This module contains the fundamental data structures for representing
//! byte-path-indexed tries. Consumers should use [`PathMap`] from the
//! parent module; the internal types are exposed for pathmap-internal use only.

pub(crate) mod node;
pub(crate) mod dense_byte;
pub(crate) mod empty;
pub(crate) mod line_list;
pub(crate) mod tiny;
#[cfg(feature = "bridge_nodes")]
pub(crate) mod bridge;

pub(crate) mod map;
pub(crate) mod r#ref;

// Re-export the main PathMap type
pub use map::PathMap;
