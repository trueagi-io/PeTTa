#![cfg_attr(all(test, feature = "mork"), allow(implicit_autoref))]
#![cfg_attr(feature = "mork", allow(internal_features))]
#![cfg_attr(feature = "mork", feature(core_intrinsics))]
#![cfg_attr(feature = "mork", feature(portable_simd))]
#![cfg_attr(feature = "mork", feature(allocator_api))]
#![cfg_attr(feature = "mork", feature(coroutine_trait))]
#![cfg_attr(feature = "mork", feature(coroutines))]
#![cfg_attr(feature = "mork", feature(stmt_expr_attributes))]
#![cfg_attr(feature = "mork", feature(gen_blocks))]
#![cfg_attr(feature = "mork", feature(yield_expr))]

pub mod parser;

#[cfg(feature = "profiling")]
pub mod profiler;

#[cfg(feature = "mork")]
pub mod mork;
#[cfg(not(feature = "mork"))]
mod mork;

pub mod gxhash;
pub mod pathmap;
pub mod utils;
pub mod viz;

pub mod engine;
pub use engine::{
Backend, BackendCapabilities, BackendConfig, BackendErrorKind, DiagLocation, DiagSeverity, Diagnostic,
EngineConfig, MettaResult, MettaValue, PeTTaEngine, PeTTaError, MIN_SWIPL_VERSION, create_formatter,
CompactFormatter, JsonFormatter, OutputFormatter, PrettyFormatter, SExprFormatter, swipl_available,
};

#[cfg(feature = "bench")]
pub mod benchmark;
pub mod differential;

pub mod observability;
pub mod reliability;

pub use observability::{HealthStatus, Metrics, ObservabilityConfig, ServiceStatus};
pub use reliability::{CircuitBreaker, CircuitState, ReliabilityConfig};

mod lib_tests;
