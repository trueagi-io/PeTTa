//! PeTTa engine: persistent SWI-Prolog subprocess management
//!
//! The engine communicates with SWI-Prolog over a binary length-prefixed
//! protocol via stdin/stdout pipes. A single Prolog process lives for the
//! entire session, eliminating startup overhead.

mod client;
mod config;
pub(crate) mod errors;
mod server;
mod subprocess;
mod values;
mod version;

pub use config::{Backend, EngineConfig};
pub use errors::{BackendErrorKind, PeTTaError};
pub use values::{MettaResult, MettaValue};
pub use version::{MIN_SWIPL_VERSION, swipl_available};

use std::io::{BufReader, Write};
use std::path::Path;
#[cfg(feature = "parallel")]
use std::path::PathBuf;
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

        let manager = SubprocessManager::new(config.clone());
        let (child, stdin, stdout, stderr_output) = manager.spawn()?;

        let mut engine = Self {
            child: Some(child),
            stdin_pipe: Some(stdin),
            stdout_pipe: Some(stdout),
            stderr_output,
            config: config.clone(),
            restart_count: 0,
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
        self.stdin_pipe.is_some() && self.stdout_pipe.is_some()
    }

    /// Load and execute a MeTTa file
    pub fn load_metta_file(&mut self, file_path: &Path) -> Result<Vec<MettaResult>, PeTTaError> {
        let mut attempts = 0u32;
        loop {
            let r = load_metta_file(
                &mut self.stdin_pipe,
                &mut self.stdout_pipe,
                file_path,
                &self.config,
            );
            match r {
                Err(PeTTaError::ProtocolError(ref msg)) if msg.contains("child closed") => {
                    if attempts >= self.config.max_restarts {
                        return Err(PeTTaError::SubprocessCrashed {
                            restarts: self.restart_count,
                        });
                    }
                    attempts += 1;
                    self.restart_subprocess()?;
                    continue;
                }
                other => return other,
            }
        }
    }

    /// Load and execute multiple MeTTa files
    pub fn load_metta_files(
        &mut self,
        file_paths: &[&Path],
    ) -> Result<Vec<MettaResult>, PeTTaError> {
        let mut attempts = 0u32;
        loop {
            let r = load_metta_files(
                &mut self.stdin_pipe,
                &mut self.stdout_pipe,
                file_paths,
                &self.config,
            );
            match r {
                Err(PeTTaError::ProtocolError(ref msg)) if msg.contains("child closed") => {
                    if attempts >= self.config.max_restarts {
                        return Err(PeTTaError::SubprocessCrashed {
                            restarts: self.restart_count,
                        });
                    }
                    attempts += 1;
                    self.restart_subprocess()?;
                    continue;
                }
                other => return other,
            }
        }
    }

    /// Process a MeTTa string
    pub fn process_metta_string(&mut self, metta_code: &str) -> Result<Vec<MettaResult>, PeTTaError> {
        let mut attempts = 0u32;
        loop {
            let r = process_metta_string(
                &mut self.stdin_pipe,
                &mut self.stdout_pipe,
                metta_code,
                &self.config,
            );
            match r {
                Err(PeTTaError::ProtocolError(ref msg)) if msg.contains("child closed") => {
                    if attempts >= self.config.max_restarts {
                        return Err(PeTTaError::SubprocessCrashed {
                            restarts: self.restart_count,
                        });
                    }
                    attempts += 1;
                    self.restart_subprocess()?;
                    continue;
                }
                other => return other,
            }
        }
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
        let abs = file_path
            .canonicalize()
            .map_err(|e| PeTTaError::PathError(e.to_string()))?;
        if !abs.exists() {
            return Err(PeTTaError::FileNotFound(abs));
        }
        let input_size = abs.to_string_lossy().len();
        self.execute_profiled("load_metta_file", input_size, |engine| {
            load_metta_file(
                &mut engine.stdin_pipe,
                &mut engine.stdout_pipe,
                &abs,
                &engine.config,
            )
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
