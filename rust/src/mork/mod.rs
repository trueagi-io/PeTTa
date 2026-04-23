//! MORK (MeTTa Optimal Reduction Kernel) - Zipper-based execution backend.
//!
//! This module contains the inlined MORK crates: expr, frontend, interning, and kernel.
//! Originally sourced from <https://github.com/trueagi-io/MORK>.
//!
//! **Note:** This module requires nightly Rust and is only compiled when the
//! `mork` feature is enabled.

#[cfg(feature = "mork")]
pub mod expr;
#[cfg(feature = "mork")]
pub mod frontend;
#[cfg(feature = "mork")]
pub mod interning;

// Kernel modules - only compiled with mork feature
#[cfg(feature = "mork")]
pub mod execution;
#[cfg(feature = "mork")]
pub mod space;

// Empty stub module when mork feature is not enabled.
// Cargo requires that declared modules exist, even if empty.
#[cfg(not(feature = "mork"))]
mod stub;
