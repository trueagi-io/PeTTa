//! Shared test utilities for PeTTa integration tests.
//!
//! This module provides common helper functions to eliminate code duplication
//! across test files.

use petta::PeTTaEngine;
use std::path::{Path, PathBuf};

/// Get the project root directory.
///
/// CARGO_MANIFEST_DIR points to the crate root (where Cargo.toml is).
pub fn project_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).to_path_buf()
}

/// Create a new PeTTaEngine instance for testing.
///
/// Panics if the engine cannot be created.
pub fn make_engine() -> PeTTaEngine {
    PeTTaEngine::new(&project_root(), false).expect("Failed to create engine")
}
