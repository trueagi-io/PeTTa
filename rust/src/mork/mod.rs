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
pub mod space;
#[cfg(feature = "mork")]
mod sources;
#[cfg(feature = "mork")]
mod sinks;
#[cfg(feature = "mork")]
mod pure;

// Stub module when mork feature is not enabled (to keep the parent module valid)
#[cfg(not(feature = "mork"))]
mod stub {}
