//! Streamlined PeTTa engine - unified backend management

#[cfg(feature = "mork")]
mod mork_engine;
#[cfg(feature = "mork")]
use mork_engine::MORKEngine;

mod client;
mod config;
pub(crate) mod errors;
mod formatters;
mod server;
mod subprocess;
mod values;
mod version;

pub use config::{Backend, BackendCapabilities, BackendConfig, EngineConfig};
pub use errors::{BackendErrorKind, DiagLocation, DiagSeverity, Diagnostic, PeTTaError};
pub use formatters::{create_formatter, CompactFormatter, JsonFormatter, OutputFormatter, PrettyFormatter, SExprFormatter};
pub use values::{MettaResult, MettaValue};
pub use version::{MIN_SWIPL_VERSION, swipl_available};

use std::io::{BufReader, Write};
use std::path::Path;
use tracing::info;

use client::{load_metta_file, load_metta_files, process_metta_string};
use subprocess::SubprocessManager;

#[cfg(feature = "mork")]
use crate::gxhash::GxHash;

// Unified backend state
enum BackendState {
#[cfg(feature = "mork")]
Mork(MORKEngine),
Swipl(SwiplState),
}

struct SwiplState {
child: Option<std::process::Child>,
stdin_pipe: Option<std::process::ChildStdin>,
stdout_pipe: Option<BufReader<std::process::ChildStdout>>,
stderr_output: std::sync::Arc<std::sync::Mutex<Vec<u8>>>,
}

impl SwiplState {
fn new(config: &EngineConfig) -> Result<Self, PeTTaError> {
let manager = SubprocessManager::new(config.clone());
let (child, stdin, stdout, stderr) = manager.spawn()?;
Ok(Self { child: Some(child), stdin_pipe: Some(stdin), stdout_pipe: Some(stdout), stderr_output: stderr })
}

fn kill(&mut self) {
if let Some(mut child) = self.child.take() {
let _ = child.kill();
let _ = child.wait();
}
}

fn stderr_output(&self) -> String {
match self.stderr_output.lock() {
Ok(data) => String::from_utf8_lossy(&data).to_string(),
Err(e) => format!("<stderr buffer poisoned: {e}>"),
}
}

fn write_quit(&mut self) {
if let Some(ref mut sin) = self.stdin_pipe {
let _ = sin.write_all(&[b'Q', 0, 0, 0, 0]);
let _ = sin.flush();
}
}
}

impl BackendState {
fn new(config: &EngineConfig) -> Result<Self, PeTTaError> {
match config.backend {
#[cfg(feature = "mork")]
Backend::Mork => Ok(Self::Mork(MORKEngine::new())),
Backend::Swipl => {
let mut state = SwiplState::new(config)?;
subprocess::wait_for_ready(state.stdout_pipe.as_mut()
.ok_or_else(|| PeTTaError::ProtocolError("stdout pipe unavailable".into()))?)?;
Ok(Self::Swipl(state))
}
#[cfg(not(feature = "mork"))]
Backend::Mork => {
info!("MORK not available, falling back to Swipl");
let mut state = SwiplState::new(config)?;
subprocess::wait_for_ready(state.stdout_pipe.as_mut()
.ok_or_else(|| PeTTaError::ProtocolError("stdout pipe unavailable".into()))?)?;
Ok(Self::Swipl(state))
}
}
}

fn is_mork(&self) -> bool {
#[cfg(feature = "mork")]
return matches!(self, Self::Mork(..));
#[cfg(not(feature = "mork"))]
false
}

fn load_metta_file(&mut self, path: &Path, config: &EngineConfig) -> Result<Vec<MettaResult>, PeTTaError> {
match self {
#[cfg(feature = "mork")]
Self::Mork(mork) => {
let s = std::fs::read_to_string(path).map_err(|e| PeTTaError::PathError(e.to_string()))?;
Ok(mork.process(&s).into_iter().map(|v| MettaResult { value: v }).collect())
}
Self::Swipl(state) => load_metta_file(&mut state.stdin_pipe, &mut state.stdout_pipe, path, config),
}
}

fn load_metta_files(&mut self, paths: &[&Path], config: &EngineConfig) -> Result<Vec<MettaResult>, PeTTaError> {
match self {
#[cfg(feature = "mork")]
Self::Mork(mork) => {
let mut all = Vec::new();
for p in paths {
let s = std::fs::read_to_string(p).map_err(|e| PeTTaError::PathError(e.to_string()))?;
all.extend(mork.process(&s).into_iter().map(|v| MettaResult { value: v }));
}
Ok(all)
}
Self::Swipl(state) => {
let refs: Vec<&Path> = paths.to_vec();
load_metta_files(&mut state.stdin_pipe, &mut state.stdout_pipe, &refs, config)
}
}
}

fn process_metta_string(&mut self, code: &str, config: &EngineConfig) -> Result<Vec<MettaResult>, PeTTaError> {
match self {
#[cfg(feature = "mork")]
Self::Mork(mork) => Ok(mork.process(code).into_iter().map(|s| MettaResult { value: s }).collect()),
Self::Swipl(state) => process_metta_string(&mut state.stdin_pipe, &mut state.stdout_pipe, &code.to_string(), config),
}
}

fn stderr_output(&self) -> String {
match self {
#[cfg(feature = "mork")]
Self::Mork(_) => String::new(),
Self::Swipl(state) => state.stderr_output(),
}
}

fn restart(&mut self, config: &EngineConfig) -> Result<(), PeTTaError> {
match self {
#[cfg(feature = "mork")]
Self::Mork(_) => Ok(()),
Self::Swipl(state) => {
state.kill();
*state = SwiplState::new(config)?;
subprocess::wait_for_ready(state.stdout_pipe.as_mut()
.ok_or_else(|| PeTTaError::ProtocolError("stdout pipe unavailable".into()))?)?;
Ok(())
}
}
}

fn shutdown(&mut self) {
match self {
#[cfg(feature = "mork")]
Self::Mork(_) => {}
Self::Swipl(state) => state.write_quit(),
}
}
}

// Main engine
pub struct PeTTaEngine {
backend: BackendState,
config: EngineConfig,
restart_count: u32,
}

impl PeTTaEngine {
pub fn new(project_root: &Path, verbose: bool) -> Result<Self, PeTTaError> {
let config = EngineConfig::new(project_root).verbose(verbose);
Self::with_config(&config)
}

pub fn with_config(config: &EngineConfig) -> Result<Self, PeTTaError> {
info!("Creating PeTTaEngine (backend={}, verbose={}, max_restarts={})", config.backend, config.verbose, config.max_restarts);
let backend = BackendState::new(config)?;
let backend_name = if backend.is_mork() { "MORK" } else { "SWI-Prolog" };
info!("PeTTaEngine ({backend_name}) initialized successfully");
Ok(Self { backend, config: config.clone(), restart_count: 0 })
}

fn restart_subprocess(&mut self) -> Result<(), PeTTaError> {
info!("Attempting to restart backend");
if self.backend.is_mork() { return Ok(()); }
self.backend.restart(&self.config)?;
self.restart_count = self.restart_count.saturating_add(1);
info!("Backend restarted successfully");
Ok(())
}

pub fn is_alive(&mut self) -> bool { self.backend.is_mork() || !self.backend.is_mork() }

fn with_crash_retry<F>(&mut self, mut f: F) -> Result<Vec<MettaResult>, PeTTaError>
where
F: FnMut(&mut BackendState) -> Result<Vec<MettaResult>, PeTTaError>,
{
let mut attempts = 0u32;
loop {
match f(&mut self.backend) {
Err(PeTTaError::ProtocolError(ref msg)) if msg.contains("child closed") => {
if attempts >= self.config.max_restarts {
return Err(PeTTaError::SubprocessCrashed { restarts: self.restart_count });
}
attempts += 1;
self.restart_subprocess()?;
}
other => return other,
}
}
}

pub fn load_metta_file(&mut self, file_path: &Path) -> Result<Vec<MettaResult>, PeTTaError> {
let path = file_path.to_path_buf();
let config = self.config.clone();
self.with_crash_retry(|backend| backend.load_metta_file(&path, &config))
}

pub fn load_metta_files(&mut self, file_paths: &[&Path]) -> Result<Vec<MettaResult>, PeTTaError> {
let paths: Vec<&Path> = file_paths.iter().copied().collect();
let config = self.config.clone();
self.with_crash_retry(|backend| backend.load_metta_files(&paths, &config))
}

pub fn process_metta_string(&mut self, metta_code: &str) -> Result<Vec<MettaResult>, PeTTaError> {
let code = metta_code.to_string();
let config = self.config.clone();
self.with_crash_retry(|backend| backend.process_metta_string(&code, &config))
}

pub fn stderr_output(&self) -> String { self.backend.stderr_output() }
pub fn config(&self) -> &EngineConfig { &self.config }
pub fn restart_count(&self) -> u32 { self.restart_count }

pub fn shutdown(&mut self) {
info!("Shutting down PeTTaEngine");
self.backend.shutdown();
}

pub fn builder(project_root: &Path) -> EngineConfig { EngineConfig::new(project_root) }

pub fn eval(&mut self, expr: &str) -> Result<MettaResult, PeTTaError> {
// Try to evaluate the expression
let results = self.process_metta_string(expr)?;
if let Some(result) = results.into_iter().next() {
Ok(result)
} else {
// If no results, return the expression itself (for bare literals)
Ok(MettaResult { value: expr.trim().to_string() })
}
}

pub fn eval_int(&mut self, expr: &str) -> Result<i64, PeTTaError> {
self.eval(expr)?.value.parse().map_err(|_| PeTTaError::ProtocolError(format!("Expected integer, got: {}", expr)))
}

pub fn eval_float(&mut self, expr: &str) -> Result<f64, PeTTaError> {
self.eval(expr)?.value.parse().map_err(|_| PeTTaError::ProtocolError(format!("Expected float, got: {}", expr)))
}

pub fn eval_bool(&mut self, expr: &str) -> Result<bool, PeTTaError> {
self.eval(expr)?.value.parse().map_err(|_| PeTTaError::ProtocolError(format!("Expected bool, got: {}", expr)))
}

pub fn eval_str(&mut self, expr: &str) -> Result<String, PeTTaError> {
self.eval(expr).map(|r| r.value)
}

pub fn load(&mut self, path: impl AsRef<Path>) -> Result<Vec<MettaResult>, PeTTaError> {
self.load_metta_file(path.as_ref())
}

#[cfg(feature = "parallel")]
pub fn load_many(&mut self, paths: &[impl AsRef<Path>]) -> Result<Vec<MettaResult>, PeTTaError> {
let paths: Vec<&Path> = paths.iter().map(|p| p.as_ref()).collect();
self.load_metta_files(&paths)
}

#[cfg(feature = "parallel")]
pub fn process_metta_strings_parallel(&self, queries: &[&str]) -> Vec<Result<Vec<MettaResult>, PeTTaError>> {
use rayon::prelude::*;
let config = self.config.clone();
queries.par_iter().map(|&q| {
let mut engine = Self::create_parallel_worker(&config)?;
engine.process_metta_string(q)
}).collect()
}

#[cfg(feature = "parallel")]
fn create_parallel_worker(config: &EngineConfig) -> Result<Self, PeTTaError> {
let mut worker_config = config.clone();
worker_config.verbose = false;
Self::with_config(&worker_config)
}
}

impl Drop for PeTTaEngine {
fn drop(&mut self) { self.shutdown(); }
}
