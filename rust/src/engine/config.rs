//! Streamlined engine configuration

use std::path::{Path, PathBuf};
use std::time::Duration;

use super::version::MIN_SWIPL_VERSION;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Backend {
#[default]
Swipl,
Mork,
}

impl std::fmt::Display for Backend {
fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
match self {
Backend::Mork => write!(f, "Mork"),
Backend::Swipl => write!(f, "Swipl"),
}
}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BackendCapabilities {
pub supports_parallel: bool,
pub supports_persistence: bool,
pub supports_incremental: bool,
pub max_atoms: Option<usize>,
pub features: Vec<&'static str>,
}

impl BackendCapabilities {
pub fn mork() -> Self {
Self { supports_parallel: true, supports_persistence: true, supports_incremental: true, max_atoms: None,
features: vec!["native-rust", "zipper-execution", "pathmap-storage"] }
}

pub fn swipl() -> Self {
Self { supports_parallel: false, supports_persistence: true, supports_incremental: true, max_atoms: None,
features: vec!["mature", "prolog-based", "subprocess"] }
}

pub fn for_backend(backend: Backend) -> Self {
match backend {
Backend::Mork => Self::mork(),
Backend::Swipl => Self::swipl(),
}
}

pub fn has_feature(&self, feature: &str) -> bool { self.features.contains(&feature) }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BackendConfig {
Mork { arena_size: usize, enable_gc: bool },
Swipl { path: PathBuf },
}

impl Default for BackendConfig {
fn default() -> Self { Self::Mork { arena_size: 1024 * 1024, enable_gc: true } }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EngineConfig {
pub backend: Backend,
pub backend_config: BackendConfig,
pub swipl_path: PathBuf,
pub src_dir: Option<PathBuf>,
pub verbose: bool,
pub profile: bool,
pub query_timeout: Option<Duration>,
pub max_restarts: u32,
pub min_swipl_version: (u32, u32),
pub parallel: bool,
pub auto_detect: bool,
}

impl Default for EngineConfig {
fn default() -> Self {
Self {
backend: Backend::Swipl,
backend_config: BackendConfig::default(),
swipl_path: PathBuf::from("swipl"),
src_dir: None,
verbose: false,
profile: false,
query_timeout: None,
max_restarts: 0,
min_swipl_version: MIN_SWIPL_VERSION,
parallel: false,
auto_detect: true,
}
}
}

impl EngineConfig {
pub fn new(project_root: &Path) -> Self {
Self { src_dir: Some(project_root.join("prolog")), ..Default::default() }
}

pub fn swipl_path(mut self, path: impl Into<PathBuf>) -> Self { self.swipl_path = path.into(); self }
pub fn src_dir(mut self, dir: impl Into<PathBuf>) -> Self { self.src_dir = Some(dir.into()); self }
pub fn profile(mut self, p: bool) -> Self { self.profile = p; self }
pub fn verbose(mut self, v: bool) -> Self { self.verbose = v; self }
pub fn query_timeout(mut self, timeout: Duration) -> Self { self.query_timeout = Some(timeout); self }
pub fn max_restarts(mut self, n: u32) -> Self { self.max_restarts = n; self }
pub fn backend(mut self, backend: Backend) -> Self { self.backend = backend; self }
pub fn parallel(mut self, p: bool) -> Self { self.parallel = p; self }
pub fn auto_detect(mut self, auto: bool) -> Self { self.auto_detect = auto; self }

pub fn mork_opts(mut self, arena_size: usize, enable_gc: bool) -> Self {
self.backend_config = BackendConfig::Mork { arena_size, enable_gc };
self
}

pub fn detect_backend() -> Backend {
if Self::is_mork_available() { Backend::Mork } else { Backend::Swipl }
}

#[cfg(feature = "mork")]
fn is_mork_available() -> bool { true }
#[cfg(not(feature = "mork"))]
fn is_mork_available() -> bool { false }

pub fn capabilities(&self) -> BackendCapabilities { BackendCapabilities::for_backend(self.backend) }
}
