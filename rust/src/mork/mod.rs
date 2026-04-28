//! MORK (MeTTa Optimal Reduction Kernel) - Zipper-based execution backend.
//!
//! This module contains the inlined MORK crates: expr, frontend, interning, and kernel.
//! MORK implementation inlined in PeTTa.
//!
//! **Note:** This module requires nightly Rust and is only compiled when the
//! `mork` feature is enabled.

#[cfg(feature = "mork")]
pub mod execution;
#[cfg(feature = "mork")]
pub mod expr;
#[cfg(feature = "mork")]
pub mod frontend;
#[cfg(feature = "mork")]
pub mod interning;
#[cfg(feature = "mork")]
pub mod interpreter;
#[cfg(feature = "mork")]
pub mod space;
