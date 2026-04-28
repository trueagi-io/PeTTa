//! PeTTa engine: persistent SWI-Prolog subprocess management
//!
//! The engine communicates with SWI-Prolog over a binary length-prefixed
//! protocol via stdin/stdout pipes. A single Prolog process lives for the
//! entire session, eliminating startup overhead.

#[cfg(feature = "mork")]
mod mork_engine;
#[cfg(feature = "mork")]
use crate::engine::mork_engine::MORKEngine;

mod client;
mod config;
pub(crate) mod errors;
mod server;
mod subprocess;
mod values;
mod version;

pub use config::Backend;
pub use config::EngineConfig;
pub use errors::{BackendErrorKind, PeTTaError};
pub use values::{MettaResult, MettaValue};
pub use version::{MIN_SWIPL_VERSION, swipl_available};

use std::io::{BufReader, Write};
use std::path::Path;
#[cfg(feature = "profiling")]
use std::time::Instant;

use tracing::info;

use self::client::{load_metta_file, load_metta_files, process_metta_string};
use self::subprocess::SubprocessManager;
#[cfg(feature = "profiling")]
use crate::profiler;

// ---------------------------------------------------------------------------
// Persistent engine
// ---------------------------------------------------------------------------

/// Main engine struct managing the Prolog subprocess
pub struct PeTTaEngine {
    child: Option<std::process::Child>,
    stdin_pipe: Option<std::process::ChildStdin>,
    stdout_pipe: Option<BufReader<std::process::ChildStdout>>,
    stderr_output: std::sync::Arc<std::sync::Mutex<Vec<u8>>>,
    config: EngineConfig,
    restart_count: u32,
    #[cfg(feature = "mork")]
    mork: Option<MORKEngine>,
}

impl PeTTaEngine {
    /// Create a new engine with default config for the given project root
    pub fn new(project_root: &Path, verbose: bool) -> Result<Self, PeTTaError> {
        let config = EngineConfig::new(project_root).verbose(verbose);
        Self::with_config(&config)
    }

    /// Create a new engine with a custom configuration
    pub fn with_config(config: &EngineConfig) -> Result<Self, PeTTaError> {
        info!(
            "Creating PeTTaEngine (swipl={}, verbose={}, max_restarts={})",
            config.swipl_path.display(),
            config.verbose,
            config.max_restarts
        );

        // If configured to use the native MORK backend, create and return that
        #[cfg(feature = "mork")]
        if config.backend == Backend::Mork {
            let mork = MORKEngine::new();
            let mut engine = Self {
                child: None,
                stdin_pipe: None,
                stdout_pipe: None,
                stderr_output: std::sync::Arc::new(std::sync::Mutex::new(Vec::new())),
                config: config.clone(),
                restart_count: 0,
                mork: Some(mork),
            };
            info!("PeTTaEngine (MORK) initialized successfully");
            return Ok(engine);
        }

        let manager = SubprocessManager::new(config.clone());
        let (child, stdin, stdout, stderr_output) = manager.spawn()?;

        let mut engine = Self {
            child: Some(child),
            stdin_pipe: Some(stdin),
            stdout_pipe: Some(stdout),
            stderr_output,
            config: config.clone(),
            restart_count: 0,
            #[cfg(feature = "mork")]
            mork: None,
        };

        // Wait for the ready signal (0xFF)
        let reader = engine
            .stdout_pipe
            .as_mut()
            .ok_or_else(|| PeTTaError::ProtocolError("stdout pipe unavailable".into()))?;
        subprocess::wait_for_ready(reader)?;

        info!("PeTTaEngine initialized successfully");
        Ok(engine)
    }

    /// Attempt to restart the subprocess
    fn restart_subprocess(&mut self) -> Result<(), PeTTaError> {
        info!("Attempting to restart SWI-Prolog subprocess");
        #[cfg(feature = "mork")]
        if self.config.backend == Backend::Mork {
            // nothing to restart for the in-process MORK backend
            return Ok(());
        }
        let manager = SubprocessManager::new(self.config.clone());
        let (child, stdin, stdout, stderr_output) = manager.spawn()?;

        // Replace old child and pipes
        if let Some(mut old_child) = self.child.take() {
            let _ = old_child.kill();
            let _ = old_child.wait();
        }
        self.child = Some(child);
        self.stdin_pipe = Some(stdin);
        self.stdout_pipe = Some(stdout);
        self.stderr_output = stderr_output;
        self.restart_count = self.restart_count.saturating_add(1);

        // Wait for ready signal
        let reader = self
            .stdout_pipe
            .as_mut()
            .ok_or_else(|| PeTTaError::ProtocolError("stdout pipe unavailable".into()))?;
        subprocess::wait_for_ready(reader)?;

        info!("SWI-Prolog subprocess restarted successfully");
        Ok(())
    }

    /// Check if the Prolog subprocess is alive and responsive
    pub fn is_alive(&mut self) -> bool {
        #[cfg(feature = "mork")]
        {
            if self.config.backend == Backend::Mork {
                return self.mork.is_some();
            }
        }
        self.stdin_pipe.is_some() && self.stdout_pipe.is_some()
    }

    fn with_crash_retry<F>(&mut self, f: F) -> Result<Vec<MettaResult>, PeTTaError>
    where
        F: Fn(
            &mut Option<std::process::ChildStdin>,
            &mut Option<BufReader<std::process::ChildStdout>>,
            &EngineConfig,
        ) -> Result<Vec<MettaResult>, PeTTaError>,
    {
        let mut attempts = 0u32;
        loop {
            match f(&mut self.stdin_pipe, &mut self.stdout_pipe, &self.config) {
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
#[cfg(feature = "mork")]
if self.config.backend == Backend::Mork {
// For MORK, read the file and process it (parse and evaluate)
let s = std::fs::read_to_string(file_path).map_err(|e| PeTTaError::PathError(e.to_string()))?;
if let Some(m) = self.mork.as_mut() {
let results = m.process(&s);
return Ok(results.into_iter().map(|v| MettaResult { value: v }).collect());
}
}
        let path = file_path.to_path_buf();
        self.with_crash_retry(move |stdin, stdout, config| {
            load_metta_file(stdin, stdout, &path, config)
        })
    }

pub fn load_metta_files(
&mut self,
file_paths: &[&Path],
) -> Result<Vec<MettaResult>, PeTTaError> {
#[cfg(feature = "mork")]
if self.config.backend == Backend::Mork {
let mut all_results = Vec::new();
for p in file_paths {
let s = std::fs::read_to_string(p).map_err(|e| PeTTaError::PathError(e.to_string()))?;
if let Some(m) = self.mork.as_mut() {
let results = m.process(&s);
all_results.extend(results.into_iter().map(|v| MettaResult { value: v }));
}
}
return Ok(all_results);
}

        let paths: Vec<std::path::PathBuf> = file_paths.iter().map(|p| p.to_path_buf()).collect();
        self.with_crash_retry(move |stdin, stdout, config| {
            let refs: Vec<&Path> = paths.iter().map(|p| p.as_path()).collect();
            load_metta_files(stdin, stdout, &refs, config)
        })
    }

    pub fn process_metta_string(
        &mut self,
        metta_code: &str,
    ) -> Result<Vec<MettaResult>, PeTTaError> {
        #[cfg(feature = "mork")]
        if self.config.backend == Backend::Mork {
            if let Some(m) = self.mork.as_mut() {
                let out = m.process(metta_code);
                let results = out.into_iter().map(|s| MettaResult { value: s }).collect();
                return Ok(results);
            }
        }

        let code = metta_code.to_string();
        self.with_crash_retry(move |stdin, stdout, config| {
            process_metta_string(stdin, stdout, &code, config)
        })
    }

    /// Get the stderr output from the Prolog subprocess
    pub fn stderr_output(&self) -> String {
        match self.stderr_output.lock() {
            Ok(data) => String::from_utf8_lossy(&data).to_string(),
            Err(e) => format!("<stderr buffer poisoned: {}>", e),
        }
    }

    /// Returns the current configuration
    pub fn config(&self) -> &EngineConfig {
        &self.config
    }

    /// Returns the number of times the subprocess has been restarted
    pub fn restart_count(&self) -> u32 {
        self.restart_count
    }

    /// Shutdown the engine and terminate the Prolog subprocess
    pub fn shutdown(&mut self) {
        info!("Shutting down PeTTaEngine");
        if let Some(sin) = self.stdin_pipe.as_mut() {
            let _ = sin.write_all(&[b'Q', 0, 0, 0, 0]);
            let _ = sin.flush();
        }
        if let Some(mut child) = self.child.take() {
            let _ = child.wait();
        }
    }

    pub fn builder(project_root: &Path) -> EngineConfig {
        EngineConfig::new(project_root)
    }

    pub fn eval(&mut self, expr: &str) -> Result<MettaResult, PeTTaError> {
        let results = self.process_metta_string(expr)?;
        Ok(results.into_iter().next().unwrap_or(MettaResult { value: String::new() }))
    }

    pub fn eval_int(&mut self, expr: &str) -> Result<i64, PeTTaError> {
        let result = self.eval(expr)?;
        result.value.parse().map_err(|_| PeTTaError::ProtocolError(format!("Expected integer, got: {}", result.value)))
    }

    pub fn eval_float(&mut self, expr: &str) -> Result<f64, PeTTaError> {
        let result = self.eval(expr)?;
        result.value.parse().map_err(|_| PeTTaError::ProtocolError(format!("Expected float, got: {}", result.value)))
    }

    pub fn eval_bool(&mut self, expr: &str) -> Result<bool, PeTTaError> {
        let result = self.eval(expr)?;
        result.value.parse().map_err(|_| PeTTaError::ProtocolError(format!("Expected bool, got: {}", result.value)))
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

    // Profiling methods
    // -----------------------------------------------------------------------

    /// Build a profile for a query operation
    #[cfg(feature = "profiling")]
    fn build_query_profile(
        op_name: &str,
        input_size: usize,
        send_time: std::time::Duration,
        parse_time: std::time::Duration,
        total_time: std::time::Duration,
        result_count: usize,
    ) -> profiler::QueryProfile {
        let mut profile = profiler::QueryProfile::new(op_name, input_size);
        profile.serialization_time = send_time;
        profile.parse_time = parse_time;
        profile.round_trip_time = total_time;
        profile.total_time = total_time;
        profile.result_count = result_count;
        profile
    }

    /// Execute an operation with profiling enabled
    #[cfg(feature = "profiling")]
    fn execute_profiled<F>(
        &mut self,
        op_name: &str,
        input_size: usize,
        operation: F,
    ) -> Result<(Vec<MettaResult>, profiler::QueryProfile), PeTTaError>
    where
        F: FnOnce(&mut Self) -> Result<Vec<MettaResult>, PeTTaError>,
    {
        let total_start = Instant::now();
        let send_start = Instant::now();
        let results = operation(self);
        let send_time = send_start.elapsed();

        let parse_start = Instant::now();
        let results = results?;
        let parse_time = parse_start.elapsed();

        let total_time = total_start.elapsed();
        let profile = Self::build_query_profile(
            op_name,
            input_size,
            send_time,
            parse_time,
            total_time,
            results.len(),
        );

        if self.config.profile {
            info!("{}", profile.summary());
        }

        Ok((results, profile))
    }

    /// Execute a query with profiling enabled
    #[cfg(feature = "profiling")]
    pub fn process_metta_string_profiled(
        &mut self,
        metta_code: &str,
    ) -> Result<(Vec<MettaResult>, profiler::QueryProfile), PeTTaError> {
        let input_size = metta_code.len();
        self.execute_profiled("process_metta_string", input_size, |engine| {
            process_metta_string(
                &mut engine.stdin_pipe,
                &mut engine.stdout_pipe,
                metta_code,
                &engine.config,
            )
        })
    }

    /// Execute a file query with profiling enabled
    #[cfg(feature = "profiling")]
    pub fn load_metta_file_profiled(
        &mut self,
        file_path: &Path,
    ) -> Result<(Vec<MettaResult>, profiler::QueryProfile), PeTTaError> {
        let abs = file_path.canonicalize().map_err(|e| PeTTaError::PathError(e.to_string()))?;
        if !abs.exists() {
            return Err(PeTTaError::FileNotFound(abs));
        }
        let input_size = abs.to_string_lossy().len();
        self.execute_profiled("load_metta_file", input_size, |engine| {
            load_metta_file(&mut engine.stdin_pipe, &mut engine.stdout_pipe, &abs, &engine.config)
        })
    }

    // -----------------------------------------------------------------------
    // Parallel batch execution (requires 'parallel' feature)
    // -----------------------------------------------------------------------

    /// Create a non-verbose engine instance for parallel execution
    #[cfg(feature = "parallel")]
    fn create_parallel_worker(config: &EngineConfig) -> Result<Self, PeTTaError> {
        let mut worker_config = config.clone();
        worker_config.verbose = false;
        PeTTaEngine::with_config(&worker_config)
    }

    /// Execute multiple independent MeTTa strings in parallel using rayon
    #[cfg(feature = "parallel")]
    pub fn process_metta_strings_parallel(
        &self,
        queries: &[&str],
    ) -> Vec<Result<Vec<MettaResult>, PeTTaError>> {
        use rayon::prelude::*;
        let config = self.config.clone();

        queries
            .par_iter()
            .map(|&q| {
                let mut engine = Self::create_parallel_worker(&config)?;
                process_metta_string(
                    &mut engine.stdin_pipe,
                    &mut engine.stdout_pipe,
                    q,
                    &engine.config,
                )
            })
            .collect()
    }

    /// Execute multiple independent MeTTa files in parallel using rayon
    #[cfg(feature = "parallel")]
    pub fn load_metta_files_parallel(
        &self,
        file_paths: &[PathBuf],
    ) -> Vec<Result<Vec<MettaResult>, PeTTaError>> {
        use rayon::prelude::*;
        let config = self.config.clone();

        file_paths
            .par_iter()
            .map(|path| {
                let mut engine = Self::create_parallel_worker(&config)?;
                load_metta_file(
                    &mut engine.stdin_pipe,
                    &mut engine.stdout_pipe,
                    path,
                    &engine.config,
                )
            })
            .collect()
    }
}

impl Drop for PeTTaEngine {
    fn drop(&mut self) {
        self.shutdown();
    }
}
